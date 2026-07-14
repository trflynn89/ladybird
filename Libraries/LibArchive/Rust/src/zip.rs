/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

use std::ffi::OsString;
use std::fs::File;
use std::io::Read;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use zip::CompressionMethod;
use zip::ZipArchive;
use zip::ZipWriter;
use zip::read::ArchiveOffset;
use zip::read::Config;
use zip::result::ZipError;
use zip::write::SimpleFileOptions;

pub struct Patch<'a> {
    pub archive_path: &'a str,
    pub contents: &'a [u8],
}

fn open_archive(path: &Path) -> Result<ZipArchive<File>, String> {
    let file = File::open(path).map_err(|error| format!("Failed to open {}: {error}", path.display()))?;
    ZipArchive::with_config(
        Config {
            archive_offset: ArchiveOffset::Known(0),
        },
        file,
    )
    .map_err(|error| format!("Failed to read {}: {error}", path.display()))
}

fn patches_are_current(archive: &mut ZipArchive<File>, patches: &[Patch<'_>]) -> Result<bool, String> {
    for patch in patches {
        let mut entry = match archive.by_name(patch.archive_path) {
            Ok(entry) => entry,
            Err(ZipError::FileNotFound) => return Ok(false),
            Err(error) => return Err(format!("Failed to read {}: {error}", patch.archive_path)),
        };

        let mut contents = Vec::new();
        entry
            .read_to_end(&mut contents)
            .map_err(|error| format!("Failed to read {}: {error}", patch.archive_path))?;
        if contents != patch.contents {
            return Ok(false);
        }
    }

    Ok(true)
}

fn temporary_path_for(path: &Path) -> PathBuf {
    let mut temporary_path = OsString::from(path.as_os_str());
    temporary_path.push(".tmp");
    PathBuf::from(temporary_path)
}

fn replace_file(source: &Path, destination: &Path) -> Result<(), String> {
    match std::fs::rename(source, destination) {
        Ok(()) => Ok(()),
        #[cfg(target_os = "windows")]
        Err(_) => {
            std::fs::remove_file(destination)
                .map_err(|error| format!("Failed to remove {}: {error}", destination.display()))?;
            std::fs::rename(source, destination).map_err(|error| {
                format!(
                    "Failed to replace {} with {}: {error}",
                    destination.display(),
                    source.display()
                )
            })
        }
        #[cfg(not(target_os = "windows"))]
        Err(error) => Err(format!(
            "Failed to replace {} with {}: {error}",
            destination.display(),
            source.display()
        )),
    }
}

fn rebuild_with_patches(path: &Path, temporary_path: &Path, patches: &[Patch<'_>]) -> Result<(), String> {
    let mut archive = open_archive(path)?;
    let output = File::create(temporary_path)
        .map_err(|error| format!("Failed to create {}: {error}", temporary_path.display()))?;
    let mut writer = ZipWriter::new(output);

    for index in 0..archive.len() {
        let entry = archive
            .by_index_raw(index)
            .map_err(|error| format!("Failed to read ZIP entry {index}: {error}"))?;
        if patches.iter().any(|patch| entry.name() == patch.archive_path) {
            continue;
        }
        writer
            .raw_copy_file(entry)
            .map_err(|error| format!("Failed to copy ZIP entry {index}: {error}"))?;
    }

    let options = SimpleFileOptions::DEFAULT
        .compression_method(CompressionMethod::Deflated)
        .unix_permissions(0o644);
    for patch in patches {
        writer
            .start_file(patch.archive_path, options)
            .map_err(|error| format!("Failed to create {}: {error}", patch.archive_path))?;
        writer
            .write_all(patch.contents)
            .map_err(|error| format!("Failed to write {}: {error}", patch.archive_path))?;
    }

    writer
        .finish()
        .map_err(|error| format!("Failed to finish {}: {error}", temporary_path.display()))?;
    Ok(())
}

pub fn install_patches(path: &Path, patches: &[Patch<'_>]) -> Result<(), String> {
    if patches_are_current(&mut open_archive(path)?, patches)? {
        return Ok(());
    }

    let temporary_path = temporary_path_for(path);
    let result =
        rebuild_with_patches(path, &temporary_path, patches).and_then(|()| replace_file(&temporary_path, path));
    if result.is_err() {
        let _ = std::fs::remove_file(temporary_path);
    }
    result
}

pub fn read_member(path: &Path, member_path: &str) -> Result<Option<Vec<u8>>, String> {
    let mut archive = open_archive(path)?;
    let mut entry = match archive.by_name(member_path) {
        Ok(entry) => entry,
        Err(ZipError::FileNotFound) => return Ok(None),
        Err(error) => return Err(format!("Failed to read {member_path}: {error}")),
    };

    let mut contents = Vec::new();
    entry
        .read_to_end(&mut contents)
        .map_err(|error| format!("Failed to read {member_path}: {error}"))?;
    Ok(Some(contents))
}
