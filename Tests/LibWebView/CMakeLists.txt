if (BUILD_TESTING AND NOT WIN32)
    if (APPLE OR LINUX)
        add_test(
            NAME TestWebDriverCrashReport
            COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test-webdriver-crash-report.py $<TARGET_FILE:WebDriver>
        )
        set_tests_properties(TestWebDriverCrashReport PROPERTIES TIMEOUT 120)
        add_test(
            NAME TestWebDriverBrowserCrashRecovery
            COMMAND ${Python3_EXECUTABLE}
                ${CMAKE_CURRENT_SOURCE_DIR}/test-webdriver-browser-crash-recovery.py
                $<TARGET_FILE:WebDriver>
        )
        set_tests_properties(TestWebDriverBrowserCrashRecovery PROPERTIES TIMEOUT 120)
        set(crash_report_helpers WebWorker RequestServer ImageDecoder Compositor)
        foreach(helper IN LISTS crash_report_helpers)
            add_test(
                NAME Test${helper}CrashReport
                COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test-webdriver-crash-report.py $<TARGET_FILE:WebDriver> --process ${helper}
            )
            set_tests_properties(Test${helper}CrashReport PROPERTIES TIMEOUT 120)
        endforeach()

set(TestWebViewStorage_SOURCES
    TestCookieJar.cpp
    TestDownloadStore.cpp
    TestFaviconStore.cpp
    TestHistoryStore.cpp
    TestHSTSStore.cpp
    TestStorageJar.cpp
    TestStorageAreaRegistry.cpp
)
ladybird_test("${TestWebViewStorage_SOURCES}" LibWebView NAME TestWebViewStorage LIBS LibDatabase LibWebView LibURL)


set(TestSessionHistory_SOURCES
    TestSessionHistory.cpp
    TestSessionHistorySnapshotStorage.cpp
    TestSessionHistoryTraversalQueue.cpp
    TestSessionStore.cpp
)
ladybird_test("${TestSessionHistory_SOURCES}" LibWebView NAME TestSessionHistory LIBS LibDatabase LibWebView LibURL)


set(TestAutocompleteSuggestions_SOURCES
    TestAutocompleteMuxer.cpp
    TestAutocompleteRanker.cpp
    TestOmnibox.cpp
    TestWebViewURL.cpp
)
ladybird_test("${TestAutocompleteSuggestions_SOURCES}" LibWebView NAME TestAutocompleteSuggestions LIBS LibDatabase LibWebView LibURL)

        add_test(
            NAME TestWebDriverWorkerTerminatedWhileStarting
            COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test-webdriver-worker-terminated-while-starting.py $<TARGET_FILE:WebDriver>
        )
        set_tests_properties(TestWebDriverWorkerTerminatedWhileStarting PROPERTIES TIMEOUT 120)
        add_test(
            NAME TestWebDriverCompositorReconnect
            COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test-webdriver-compositor-reconnect.py $<TARGET_FILE:WebDriver>
        )
        set_tests_properties(TestWebDriverCompositorReconnect PROPERTIES TIMEOUT 120)
        add_test(
            NAME TestDevTools
            COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test-devtools.py $<TARGET_FILE:ladybird>
        )
        set_tests_properties(TestDevTools PROPERTIES TIMEOUT 240)
        add_test(
            NAME TestWindowSizeReachesEveryPage
            COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test-window-size-reaches-every-page.py $<TARGET_FILE:ladybird>
        )
        set_tests_properties(TestWindowSizeReachesEveryPage PROPERTIES TIMEOUT 120)
        add_test(
            NAME TestUserAgentPresetReachesChildFrames
            COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test-user-agent-preset-reaches-child-frames.py $<TARGET_FILE:ladybird>
        )
        set_tests_properties(TestUserAgentPresetReachesChildFrames PROPERTIES TIMEOUT 120)
        add_test(
            NAME TestMessagePortCollection
            COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test-message-port-collection.py $<TARGET_FILE:WebDriver>
        )
        set_tests_properties(TestMessagePortCollection PROPERTIES TIMEOUT 120)
        add_test(
            NAME TestSharedStyleSheetMemory
            COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test-shared-stylesheet-memory.py $<TARGET_FILE:WebDriver>
        )
        set_tests_properties(TestSharedStyleSheetMemory PROPERTIES TIMEOUT 180)
        add_test(
            NAME TestBytecodeCacheBlobIsFileBacked
            COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test-bytecode-cache-blob-is-file-backed.py $<TARGET_FILE:WebDriver>
        )
        set_tests_properties(TestBytecodeCacheBlobIsFileBacked PROPERTIES TIMEOUT 120)
    endif()

    add_test(
        NAME TestWebDriverSession
        COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test-webdriver-session.py $<TARGET_FILE:WebDriver>
    )
    set_tests_properties(TestWebDriverSession PROPERTIES TIMEOUT 120)

    add_test(
        NAME TestWebDriverSettings
        COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test-webdriver-settings.py $<TARGET_FILE:WebDriver>
    )
    set_tests_properties(TestWebDriverSettings PROPERTIES TIMEOUT 300)

    add_test(
        NAME TestWebDriverSessionHistory
        COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test-webdriver-session-history.py $<TARGET_FILE:WebDriver>
    )
    set_tests_properties(TestWebDriverSessionHistory PROPERTIES TIMEOUT 120 ENVIRONMENT "XDG_DATA_HOME=${CMAKE_CURRENT_BINARY_DIR}/session-history-data")

    add_test(
        NAME TestWebDriverNavigationPreferences
        COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test-webdriver-navigation-preferences.py $<TARGET_FILE:WebDriver>
    )
    set_tests_properties(TestWebDriverNavigationPreferences PROPERTIES TIMEOUT 240)

    add_test(
        NAME TestWebDriverBiDi
        COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test-webdriver-bidi.py $<TARGET_FILE:WebDriver>
    )
    set_tests_properties(TestWebDriverBiDi PROPERTIES TIMEOUT 120)

endif()

if (BUILD_BROWSER_TESTS_ONLY)
    return()
endif()

set(TEST_SOURCES
    TestAccessibilityTreeManager.cpp
    TestAudibilityHold.cpp
    TestAudioServerAudibility.cpp
    TestBackgroundNetworking.cpp
    TestCanonicalBrowsingContextGroup.cpp
    TestDownloadSegmentation.cpp
    TestExternalURLHandler.cpp
    TestHelperProcessEnvironment.cpp
    TestNavigationLoader.cpp
    TestPictureInPictureWindow.cpp
    TestProfile.cpp
    TestSiteCompatibilityDataLoading.cpp
    TestStartupSettings.cpp
    TestTabPerformanceStats.cpp
    TestWebDriverBiDiNetwork.cpp
)

if (APPLE OR LINUX)
    list(APPEND TEST_SOURCES TestCrashReport.cpp TestFontService.cpp TestRendererFontService.cpp)
endif()

if (APPLE)
    list(APPEND TEST_SOURCES TestProcessReaper.cpp)
endif()

if (LINUX)
    list(APPEND TEST_SOURCES TestAudioDaemonSocket.cpp)
endif()

if (APPLE AND NOT IOS)
    ladybird_test(TestWebDriverMachBootstrap.cpp LibWebView LIBS LibGfx LibIPC LibURL LibWebCommon LibWebSocket OpenSSL::Crypto OpenSSL::SSL)
    target_sources(TestWebDriverMachBootstrap PRIVATE
        "${LADYBIRD_SOURCE_DIR}/Services/WebDriver/BiDiSession.cpp"
        "${LADYBIRD_SOURCE_DIR}/Services/WebDriver/BrowserConnection.cpp"
        "${LADYBIRD_SOURCE_DIR}/Services/WebDriver/Client.cpp"
        "${LADYBIRD_SOURCE_DIR}/Services/WebDriver/Session.cpp"
        "${LADYBIRD_SOURCE_DIR}/Services/WebDriver/WebSocket.cpp"
    )
    target_include_directories(TestWebDriverMachBootstrap PRIVATE "${LADYBIRD_SOURCE_DIR}/Services" "${CMAKE_BINARY_DIR}/Services")
    set_tests_properties(TestWebDriverMachBootstrap PROPERTIES TIMEOUT 30)
endif()

foreach(source IN LISTS TEST_SOURCES)
    set(test_libraries LibDatabase LibWebView LibURL)
    if (source STREQUAL "TestProfile.cpp" OR source STREQUAL "TestDownloadSegmentation.cpp")
        list(APPEND test_libraries LibHTTP)
    endif()
    if (source STREQUAL "TestAudioServerAudibility.cpp")
        list(APPEND test_libraries LibAudio LibIPC)
    endif()
    if (source STREQUAL "TestFontService.cpp")
        list(APPEND test_libraries LibFileSystem LibIPC LibGfx)
    endif()
    if (source STREQUAL "TestRendererFontService.cpp")
        list(APPEND test_libraries LibCompositing LibGfx LibIPC LibThreading)
    endif()
    ladybird_test("${source}" LibWebView LIBS ${test_libraries})
endforeach()

if (TARGET TestProcessReaper)
    add_dependencies(TestProcessReaper ProcessReaper)
endif()

if (TARGET TestAudioDaemonSocket)
    include(audio)
    if (LADYBIRD_AUDIO_BACKEND STREQUAL "PULSE")
        target_compile_definitions(TestAudioDaemonSocket PRIVATE HAVE_PULSEAUDIO=1)
    endif()
endif()

if (TARGET TestCrashReport AND cpptrace_FOUND AND LADYBIRD_ENABLE_CPPTRACE)
    target_compile_definitions(TestCrashReport PRIVATE TEST_HAS_CPPTRACE=1)
endif()

if (TARGET TestCrashReport AND LINUX AND CMAKE_SYSTEM_PROCESSOR STREQUAL "x86_64")
    add_library(CrashReportLateLibrary SHARED CrashReportLateLibrary.cpp)
    add_dependencies(TestCrashReport CrashReportLateLibrary)
    target_compile_definitions(TestCrashReport PRIVATE TEST_CRASH_REPORT_LATE_LIBRARY="$<TARGET_FILE:CrashReportLateLibrary>")
    target_link_libraries(TestCrashReport PRIVATE ${CMAKE_DL_LIBS})
endif()

if (APPLE OR LINUX)
    foreach(panic_strategy abort unwind)
        set(rust_test_library "${CMAKE_CURRENT_BINARY_DIR}/librust_crash_report_${panic_strategy}.a")
        set(ARG_CRATE_NAME "rust_crash_report_${panic_strategy}")
        add_custom_command(OUTPUT "${rust_test_library}"
            COMMAND ${CMAKE_COMMAND} -E env "CARGO_PKG_NAME=${ARG_CRATE_NAME}"
                ${RUST_RUSTC} --edition=2024 --crate-type=staticlib --crate-name=${ARG_CRATE_NAME}
                -Cpanic=${panic_strategy} -Cdebuginfo=1
                "${CMAKE_CURRENT_SOURCE_DIR}/RustCrashReport.rs" -o "${rust_test_library}"
            DEPENDS RustCrashReport.rs "${LADYBIRD_SOURCE_DIR}/Libraries/RustPanic.rs"
            VERBATIM)
        add_custom_target(${ARG_CRATE_NAME}_build DEPENDS "${rust_test_library}")
        ladybird_test(TestRustCrashReport.cpp LibWebView NAME TestRustCrashReport_${panic_strategy}
            LIBS LibWebView "${rust_test_library}")
        if (LINUX AND panic_strategy STREQUAL "unwind")
            # Keep the fixture's Rust standard library private to the test executable. Exported, it would take the
            # place of the copies in Ladybird's shared libraries, which then install their panic hooks into it.
            get_filename_component(rust_test_library_name "${rust_test_library}" NAME)
            target_link_options(TestRustCrashReport_${panic_strategy} PRIVATE "LINKER:--exclude-libs,${rust_test_library_name}")
        endif()
        set_tests_properties(TestRustCrashReport_${panic_strategy} PROPERTIES TIMEOUT 60)
        add_dependencies(TestRustCrashReport_${panic_strategy} ${ARG_CRATE_NAME}_build)
        if (panic_strategy STREQUAL "unwind")
            target_compile_definitions(TestRustCrashReport_${panic_strategy} PRIVATE TEST_RUST_UNWIND=1)
        endif()
        if (cpptrace_FOUND AND LADYBIRD_ENABLE_CPPTRACE)
            target_compile_definitions(TestRustCrashReport_${panic_strategy} PRIVATE TEST_HAS_CPPTRACE=1)
        endif()
    endforeach()
endif()

target_link_libraries(TestNavigationLoader PRIVATE LibRequests LibWeb)

if (NOT WIN32)
    ladybird_test(TestApplicationDestruction.cpp LibWebView LIBS LibCore LibDatabase LibFileSystem LibGfx LibIPC LibRequests LibURL LibWeb LibWebView)
    if (APPLE)
        target_compile_definitions(TestApplicationDestruction PRIVATE LADYBIRD_BINARY_PATH="$<TARGET_FILE_DIR:ladybird>")
    endif()

    set(headless_web_view_tests
        TestAutocomplete
        TestFavicon
        TestFileDownloader
        TestNavigationDownloads
        TestCrashOverlay
        TestBrowserHistoryTraversal
        TestBeforeUnload
        TestInputMethodState
        TestLinkHoverStatus
        TestUserActivation
        TestPictureInPictureUserActivation
        TestPictureInPictureWindowControls
        TestPictureInPictureWindowLifetime
        TestWebCompatReportPage
        TestShadowTreeTooltipsAndLinks
        TestCompositorInputRouting
        TestRendererCookieAccess
        TestRendererOriginClaims
        TestImageDecoderIsolation
        TestCrashReportSubmission
        TestWebCompatReport
        TestRequestServerClientSessions
        TestTabPerformanceMonitor
    )
    set(headless_test_sources HeadlessWebViewTestMain.cpp)
    foreach(test IN LISTS headless_web_view_tests)
        list(APPEND headless_test_sources "${test}.cpp")
    endforeach()
    add_executable(TestHeadlessWebView ${headless_test_sources})
    add_dependencies(TestHeadlessWebView ladybird_build_resource_files ${ladybird_helper_processes})
    target_link_libraries(TestHeadlessWebView PRIVATE
        AK
        LibCore
        LibCrypto
        LibDiff
        LibFileSystem
        LibGfx
        LibHTTP
        LibIPC
        LibImageDecoderClient
        LibImageDecoders
        LibJS
        LibMain
        LibRequests
        LibThreading
        LibURL
        LibWeb
        LibWebView
    )
    if (APPLE)
        target_compile_definitions(TestHeadlessWebView PRIVATE LADYBIRD_BINARY_PATH="$<TARGET_FILE_DIR:ladybird>")
    endif()
endif()

if (BUILD_TESTING AND NOT WIN32)
    add_test(NAME TestBuildInformation COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test-build-information.py)
    add_test(
        NAME TestWebDriverBiDiCleanup
        COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test-webdriver-bidi-cleanup.py
    )
    set_tests_properties(TestWebDriverBiDiCleanup PROPERTIES TIMEOUT 30)

    foreach(test IN LISTS headless_web_view_tests)
        add_test(NAME ${test} COMMAND $<TARGET_FILE:TestHeadlessWebView> ${test})
        set_tests_properties(${test} PROPERTIES TIMEOUT 60 ENVIRONMENT LADYBIRD_SOURCE_DIR=${LADYBIRD_SOURCE_DIR})
    endforeach()
    set_tests_properties(TestFavicon TestFileDownloader TestNavigationDownloads PROPERTIES TIMEOUT 120)
endif()

set(crash_report_storage_sources TestCrashReportDiagnostics.cpp)
if (APPLE OR LINUX)
    list(APPEND crash_report_storage_sources TestCrashReportReview.cpp TestCrashReportStore.cpp)
endif()
ladybird_test("${crash_report_storage_sources}" LibWebView NAME TestCrashReportStorage LIBS LibDatabase LibWebView LibURL)
