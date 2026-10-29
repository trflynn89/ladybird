set(TEST_SOURCES
    TestElasticOverscroll.cpp
    TestOverlayFade.cpp
    TestOverlayGeometry.cpp
    TestPausedDebuggerOverlay.cpp
    TestPictureInPictureControls.cpp
    TestScrollRequest.cpp
    TestScrollState.cpp
    TestSmoothScrollAnimation.cpp
    TestTextureUpload.cpp
    TestUIOverlays.cpp
    TestVideoFrameHold.cpp
    TestWheelGestureIdentity.cpp
)

ladybird_test("${TEST_SOURCES}" LibCompositing NAME TestLibCompositing LIBS LibCompositing LibGfx LibIPC)
