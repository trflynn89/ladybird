set(TEST_SOURCES
    TestCaptureDevices.cpp
    TestPlaybackStreamMixer.cpp
    TestRemotePlaybackStream.cpp
    TestRemoteRecordStream.cpp
)

ladybird_test("${TEST_SOURCES}" AudioServer NAME TestAudioServer LIBS audioserverservice LibAudio LibCore LibIPC LibMedia LibThreading)

ladybird_test(TestPlatformPlaybackStream.cpp AudioServer LIBS audioserverservice LibAudio LibCore LibIPC LibMedia LibThreading)
