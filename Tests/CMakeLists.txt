add_subdirectory(AK)
add_subdirectory(LibCompress)
add_subdirectory(LibCrypto)
add_subdirectory(LibDiff)
add_subdirectory(LibGC)
add_subdirectory(LibJS)
add_subdirectory(LibRegex)
add_subdirectory(LibTest)
add_subdirectory(LibTextCodec)
add_subdirectory(LibThreading)
add_subdirectory(LibTLS)
add_subdirectory(LibUnicode)
add_subdirectory(LibURL)
add_subdirectory(LibWasm)

if (ENABLE_GUI_TARGETS)
    add_subdirectory(LibGfx)
endif()

# FIXME: Increase support for building targets on Windows
if (WIN32 AND ENABLE_WINDOWS_CI)
    return()
endif()

add_subdirectory(LibCore)
add_subdirectory(LibDNS)
add_subdirectory(LibXML)

if (ENABLE_GUI_TARGETS)
    add_subdirectory(LibMedia)
    add_subdirectory(LibWeb)
    add_subdirectory(LibWebView)
endif()

if (ENABLE_CLANG_PLUGINS AND CMAKE_CXX_COMPILER_ID MATCHES "Clang$")
    add_subdirectory(ClangPlugins)
endif()
