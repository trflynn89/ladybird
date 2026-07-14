set(SOURCES
    TarExtractor.cpp
)

ladybird_lib(LibArchive archive)

import_rust_crate(MANIFEST_PATH Rust/Cargo.toml CRATE_NAME libarchive_rust FEATURES allocator FFI_HEADER RustFFI.h)
target_link_libraries(LibArchive PRIVATE libarchive_rust LibCore)

if ((LINUX OR BSD) AND NOT BUILD_SHARED_LIBS)
    target_link_options(LibArchive INTERFACE LINKER:--allow-multiple-definition)
endif()
