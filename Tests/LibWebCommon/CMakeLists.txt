set(TEST_SOURCES
    TestCSSPixels.cpp
    TestGamepadSharedState.cpp
    TestGamepadStateBuffer.cpp
    TestInsecureRequestsPolicy.cpp
    TestKeyEvent.cpp
    TestPermissionsPolicy.cpp
    TestSiteCompatibility.cpp
    TestStorageEndpoint.cpp
    TestStorageKey.cpp
    TestStrings.cpp
    TestWebDriverInputResponse.cpp
)

ladybird_test("${TEST_SOURCES}" LibWebCommon NAME TestLibWebCommon LIBS LibWebCommon LibHTTP LibIPC LibURL)
