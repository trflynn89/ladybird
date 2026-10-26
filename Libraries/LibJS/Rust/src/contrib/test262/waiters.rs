/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

//! test262 agents are processes. Route their waits to the test process's common LibCore registry, identifying each
//! location by the shared object ID and byte offset. Sockets carry requests and replies, not waiter
//! state in SharedArrayBuffer bytes. Closing an agent's socket cancels its pending wait; ending a test joins every
//! service thread before dropping its mappings.

use super::{ControlReader, send_with_descriptor};
use crate::futex::{AtomicWaitResult, core_atomic_notify, core_atomic_wait};
use crate::runtime::array_buffer::{SharedMemoryLocation, SharedMemoryOperations};
use core::cell::RefCell;
use core::ffi::c_void;
use core::ptr::NonNull;
use core::time::Duration;
use std::io::{self, Read, Write};
use std::net::Shutdown;
use std::os::fd::{AsFd, AsRawFd, OwnedFd};
use std::os::unix::net::UnixStream;
use std::thread::JoinHandle;

pub(crate) static OPERATIONS: SharedMemoryOperations = SharedMemoryOperations { wait, notify };

const WAIT: u8 = 0;
const NOTIFY: u8 = 1;
const REQUEST_SIZE: usize = 1 + 7 * size_of::<u64>();

thread_local! {
    static CLIENT: RefCell<Option<UnixStream>> = const { RefCell::new(None) };
}

pub(super) fn set_client(client: UnixStream) {
    CLIENT.set(Some(client));
}

pub(super) fn clear_client() {
    CLIENT.take();
}

fn request(location: SharedMemoryLocation<'_>, operation: u8, arguments: [u64; 4]) -> u64 {
    CLIENT.with_borrow_mut(|client| {
        let client =
            client.get_or_insert_with(|| super::connect_waiters().expect("connect to the test's waiter registry"));
        let mut message = vec![operation];
        for value in [
            location.object_id,
            location.byte_length as u64,
            location.byte_index as u64,
        ]
        .into_iter()
        .chain(arguments)
        {
            message.extend_from_slice(&value.to_le_bytes());
        }
        send_with_descriptor(client.as_fd(), &message, location.memory).expect("send a waiter request");
        let mut response = [0; 8];
        client.read_exact(&mut response).expect("receive a waiter response");
        u64::from_le_bytes(response)
    })
}

fn wait(location: SharedMemoryLocation<'_>, expected: u64, size: usize, timeout: Option<Duration>) -> AtomicWaitResult {
    let infinite = u64::from(timeout.is_none());
    let timeout = timeout.map_or(0, |timeout| {
        i64::try_from(timeout.as_nanos()).unwrap_or(i64::MAX) as u64
    });
    AtomicWaitResult::from_raw(request(location, WAIT, [size as u64, expected, timeout, infinite]) as u8)
}

fn notify(location: SharedMemoryLocation<'_>, max_count: usize) -> usize {
    request(location, NOTIFY, [4, 0, max_count as u64, 0]) as usize
}

#[derive(Default)]
pub(super) struct Server {
    connections: Vec<(UnixStream, JoinHandle<()>)>,
}

impl Server {
    pub(super) fn connect(&mut self) -> io::Result<UnixStream> {
        let (client, server) = UnixStream::pair()?;
        super::never_raise_sigpipe(&server)?;
        super::never_raise_sigpipe(&client)?;
        let shutdown = server.try_clone()?;
        let thread = std::thread::Builder::new()
            .name("Agent waiters".to_string())
            .spawn(move || {
                let _ = serve(server);
            })?;
        self.connections.push((shutdown, thread));
        Ok(client)
    }
}

impl Drop for Server {
    fn drop(&mut self) {
        for (socket, _) in &self.connections {
            let _ = socket.shutdown(Shutdown::Both);
        }
        for (_, thread) in self.connections.drain(..) {
            let _ = thread.join();
        }
    }
}

struct Mapping {
    data: NonNull<u8>,
    size: usize,
}

impl Mapping {
    fn new(descriptor: OwnedFd, size: usize) -> io::Result<Self> {
        let file = std::fs::File::from(descriptor);
        if size == 0 || file.metadata()?.len() < size as u64 {
            return Err(io::ErrorKind::InvalidData.into());
        }
        // SAFETY: The descriptor has at least size bytes and remains open for the duration of mmap. The mapping
        //         keeps the shared memory object alive independently of the descriptor after this call.
        let data = unsafe {
            libc::mmap(
                core::ptr::null_mut(),
                size,
                libc::PROT_READ | libc::PROT_WRITE,
                libc::MAP_SHARED,
                file.as_raw_fd(),
                0,
            )
        };
        if data == libc::MAP_FAILED {
            return Err(io::Error::last_os_error());
        }
        Ok(Self {
            data: NonNull::new(data.cast()).unwrap(),
            size,
        })
    }
}

impl Drop for Mapping {
    fn drop(&mut self) {
        // SAFETY: The request owns the mapping and its wait or notify has returned.
        unsafe { libc::munmap(self.data.as_ptr().cast(), self.size) };
    }
}

unsafe extern "C" fn disconnected(context: *mut c_void) -> bool {
    // SAFETY: serve() keeps the descriptor on its stack until core_atomic_wait returns.
    let descriptor = unsafe { *context.cast::<libc::c_int>() };
    let mut byte = 0u8;
    // SAFETY: MSG_PEEK leaves the protocol stream untouched and MSG_DONTWAIT makes this callback nonblocking.
    let result = unsafe {
        libc::recv(
            descriptor,
            (&raw mut byte).cast(),
            1,
            libc::MSG_PEEK | libc::MSG_DONTWAIT,
        )
    };
    result == 0
        || (result < 0
            && !matches!(
                io::Error::last_os_error().kind(),
                io::ErrorKind::WouldBlock | io::ErrorKind::Interrupted
            ))
}

fn serve(mut socket: UnixStream) -> io::Result<()> {
    let mut reader = ControlReader::new(OwnedFd::from(socket.try_clone()?));
    loop {
        let message = reader.read_bytes::<REQUEST_SIZE>()?;
        let mut fields = message[1..]
            .as_chunks::<8>()
            .0
            .iter()
            .map(|bytes| u64::from_le_bytes(*bytes));
        let [object_id, length, index, size, expected, argument, infinite] =
            core::array::from_fn(|_| fields.next().unwrap());
        let length = usize::try_from(length).map_err(|_| io::ErrorKind::InvalidData)?;
        let index = usize::try_from(index).map_err(|_| io::ErrorKind::InvalidData)?;
        let size = usize::try_from(size).map_err(|_| io::ErrorKind::InvalidData)?;
        if object_id == 0
            || !matches!(size, 4 | 8)
            || !index.is_multiple_of(size)
            || index.checked_add(size).is_none_or(|end| end > length)
        {
            return Err(io::ErrorKind::InvalidData.into());
        }
        let descriptor = reader
            .received_descriptors
            .pop_front()
            .ok_or(io::ErrorKind::InvalidData)?;
        let mapping = Mapping::new(descriptor, length)?;
        // SAFETY: The mapping stays alive for the whole operation and the offset and width were checked above.
        let address = unsafe { mapping.data.as_ptr().add(index) }.cast();
        let result = match message[0] {
            WAIT => {
                let timeout = if infinite != 0 {
                    -1
                } else {
                    i64::try_from(argument).unwrap_or(i64::MAX)
                };
                let mut descriptor = socket.as_raw_fd();
                // SAFETY: The checked word and cancellation context stay live throughout the wait. The callback
                //         only peeks at the socket, and cancellation removes an abandoned waiter from the common list.
                u64::from(unsafe {
                    core_atomic_wait(
                        address,
                        expected,
                        size,
                        timeout,
                        Some(disconnected),
                        (&raw mut descriptor).cast(),
                        object_id,
                        index,
                    )
                })
            }
            NOTIFY => {
                // SAFETY: The checked word stays mapped. Notification does not access its value.
                (unsafe {
                    core_atomic_notify(
                        address,
                        usize::try_from(argument).unwrap_or(usize::MAX),
                        object_id,
                        index,
                    )
                }) as u64
            }
            _ => return Err(io::ErrorKind::InvalidData.into()),
        };
        socket.write_all(&result.to_le_bytes())?;
    }
}
