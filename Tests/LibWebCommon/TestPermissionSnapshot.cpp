/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/MemoryStream.h>
#include <LibIPC/Decoder.h>
#include <LibIPC/Encoder.h>
#include <LibTest/TestCase.h>
#include <LibURL/Parser.h>
#include <LibWebCommon/Permissions/Permissions.h>

using namespace Web::Permissions;
using Web::HTML::AutoplayPolicy;

static URL::Origin origin_of(StringView url)
{
    auto parsed_url = URL::Parser::basic_parse(url);
    VERIFY(parsed_url.has_value());
    return parsed_url->origin();
}

static PermissionSnapshot round_trip(PermissionSnapshot const& snapshot)
{
    IPC::MessageBuffer message;
    IPC::Encoder encoder { message };
    MUST(encoder.encode(snapshot));
    Queue<IPC::Attachment> attachments;
    for (auto& attachment : message.take_attachments())
        attachments.enqueue(move(attachment));
    FixedMemoryStream stream { message.data().span() };
    IPC::Decoder decoder { stream, attachments };
    return MUST(decoder.decode<PermissionSnapshot>());
}

TEST_CASE(snapshot_round_trips_through_ipc)
{
    PermissionSnapshot snapshot;
    snapshot.revision = 42;
    snapshot.autoplay_default = AutoplayPolicy::BlockAudio;
    snapshot.geolocation_default = EffectiveState::Denied;
    snapshot.microphone_default = EffectiveState::Prompt;
    snapshot.autoplay_entries.append({ origin_of("https://media.example.com/"sv), AutoplayPolicy::BlockAudioAndVideo });
    snapshot.entries.append({ origin_of("https://maps.example.com/"sv), PermissionName::Geolocation, EffectiveState::Granted });
    snapshot.entries.append({ origin_of("https://talk.example.org/"sv), PermissionName::Microphone, EffectiveState::Denied });

    auto decoded = round_trip(snapshot);

    EXPECT_EQ(decoded.revision, 42u);
    EXPECT(decoded.autoplay_default == AutoplayPolicy::BlockAudio);
    EXPECT(decoded.geolocation_default == EffectiveState::Denied);
    EXPECT(decoded.microphone_default == EffectiveState::Prompt);
    VERIFY(decoded.autoplay_entries.size() == 1);
    EXPECT(decoded.autoplay_entries[0].top_level_origin.is_same_origin(origin_of("https://media.example.com/"sv)));
    EXPECT(decoded.autoplay_entries[0].policy == AutoplayPolicy::BlockAudioAndVideo);
    VERIFY(decoded.entries.size() == 2);
    EXPECT(decoded.entries[0].top_level_origin.is_same_origin(origin_of("https://maps.example.com/"sv)));
    EXPECT(decoded.entries[0].name == PermissionName::Geolocation);
    EXPECT(decoded.entries[0].state == EffectiveState::Granted);
    EXPECT(decoded.entries[1].top_level_origin.is_same_origin(origin_of("https://talk.example.org/"sv)));
    EXPECT(decoded.entries[1].name == PermissionName::Microphone);
    EXPECT(decoded.entries[1].state == EffectiveState::Denied);
}

TEST_CASE(permission_names_round_trip)
{
    for (auto name : Web::Permissions::permission_names)
        EXPECT(permission_name_from_string(to_string(name)) == name);
    EXPECT(!permission_name_from_string("autoplay"sv).has_value());
    EXPECT(!permission_name_from_string("camera"sv).has_value());
    EXPECT(!permission_name_from_string("notifications"sv).has_value());
}
