/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/Array.h>
#include <AK/ByteString.h>
#include <AK/Hex.h>
#include <AK/NeverDestroyed.h>
#include <AK/ScopeGuard.h>
#include <LibArchive/TarExtractor.h>
#include <LibArchive/ZipArchive.h>
#include <LibCore/ConfigFile.h>
#include <LibCore/Directory.h>
#include <LibCore/File.h>
#include <LibCore/Process.h>
#include <LibCore/StandardPaths.h>
#include <LibCore/System.h>
#include <LibCrypto/Hash/SHA2.h>
#include <LibDevTools/Client/Runtime.h>
#include <LibFileSystem/FileSystem.h>
#include <LibURL/Parser.h>

namespace DevTools::Client {

static constexpr auto CONTROLLER_ARCHIVE_PATH = "chrome/devtools/content/framework/ladybird-controller.html"sv;
static constexpr auto CONTROLLER_CHROME_URL = "chrome://devtools/content/framework/ladybird-controller.html"sv;
static constexpr auto TOOLBOX_ARCHIVE_PATH = "chrome/devtools/content/framework/ladybird-toolbox.html"sv;

static constexpr auto CONTROLLER_HTML = R"~~~(
<!DOCTYPE html>
<html>
    <head>
        <meta charset="utf-8" />
        <title>Ladybird DevTools</title>
    </head>
    <body>
        <script>
            "use strict";

            const { interfaces: Ci } = Components;

            const openToolboxes = new Map();
            const pendingToolboxOpens = new Map();
            let gDevTools;
            let remoteId;
            let ladybirdActor;

            function toolboxForWindow(toolboxWindow) {
                return gDevTools.getToolboxes().find(toolbox => toolbox.topWindow === toolboxWindow);
            }

            function discardClosedToolboxes() {
                for (const [tabId, toolboxWindow] of openToolboxes) {
                    if (toolboxWindow.closed && !toolboxForWindow(toolboxWindow)) {
                        openToolboxes.delete(tabId);
                    }
                }
            }

            async function waitForToolboxDestruction(toolboxWindow) {
                const toolbox = toolboxForWindow(toolboxWindow);
                if (!toolbox) {
                    return;
                }

                const destroyed = new Promise(resolve => toolbox.once("destroyed", resolve));
                toolbox.destroy();
                await destroyed;
            }

            async function openToolbox(tabId) {
                const url = `chrome://devtools/content/framework/ladybird-toolbox.html?type=tab&id=${encodeURIComponent(tabId)}&remoteId=${remoteId}`;

                const existingToolbox = openToolboxes.get(tabId);
                if (existingToolbox && !existingToolbox.closed) {
                    existingToolbox.focus();
                    return;
                }

                if (existingToolbox) {
                    await waitForToolboxDestruction(existingToolbox);
                }
                openToolboxes.delete(tabId);

                const toolbox = Services.ww.openWindow(null, url, "_blank", "chrome,dialog=no,resizable,width=1280,height=850", null);
                if (!toolbox) {
                    throw new Error(`Unable to open a toolbox for tab ${tabId}`);
                }

                openToolboxes.set(tabId, toolbox);
            }

            function requestToolbox(tabId) {
                if (pendingToolboxOpens.has(tabId)) {
                    return pendingToolboxOpens.get(tabId);
                }

                const pendingOpen = openToolbox(tabId).finally(() => {
                    if (pendingToolboxOpens.get(tabId) === pendingOpen) {
                        pendingToolboxOpens.delete(tabId);
                    }
                });
                pendingToolboxOpens.set(tabId, pendingOpen);
                return pendingOpen;
            }

            async function toolboxForTab(tabId) {
                const toolboxWindow = openToolboxes.get(tabId);
                if (!toolboxWindow || toolboxWindow.closed) {
                    throw new Error(`There is no open toolbox for tab ${tabId}`);
                }

                for (let attempt = 0; attempt < 200; ++attempt) {
                    const toolbox = gDevTools.getToolboxes().find(candidate => candidate.topWindow === toolboxWindow);
                    if (toolbox && toolbox.isReady) {
                        return toolbox;
                    }

                    await new Promise(resolve => setTimeout(resolve, 25));
                }

                throw new Error(`The toolbox for tab ${tabId} did not become ready`);
            }

            async function inspectElement(tabId, nodeId) {
                const toolbox = await toolboxForTab(tabId);
                toolbox.topWindow.focus();

                const inspector = await toolbox.selectTool("inspector", "inspect_dom");
                const nodeFront = await inspector.inspectorFront.walker.getNodeFromActor(ladybirdActor, ["node", nodeId]);
                if (!nodeFront) {
                    return;
                }

                inspector.selection.setNodeFront(nodeFront, {
                    reason: "browser-context-menu",
                });
            }

            async function connect() {
                const port = Number.parseInt(new URLSearchParams(location.search).get("port"), 10);
                if (!Number.isInteger(port) || port < 1 || port > 65535) {
                    throw new Error("The DevTools server port is invalid");
                }

                const { require } = ChromeUtils.importESModule("resource://devtools/shared/loader/Loader.sys.mjs");
                const { DevToolsClient } = require("resource://devtools/client/devtools-client.js");
                const { remoteClientManager } = require("resource://devtools/client/shared/remote-debugging/remote-client-manager.js");
                ({ gDevTools } = require("resource://devtools/client/framework/devtools.js"));

                const transport = await DevToolsClient.socketConnect({
                    host: "localhost",
                    port,
                });
                const client = new DevToolsClient(transport);
                await client.connect();

                const root = await client.mainRoot.rootForm;
                ladybirdActor = root.ladybirdActor;
                if (!ladybirdActor) {
                    throw new Error("The DevTools server does not expose a Ladybird actor");
                }

                let deviceDescription = {};
                if (root.deviceActor) {
                    const response = await client.request({
                        to: root.deviceActor,
                        type: "getDescription",
                    });
                    deviceDescription = response.value || {};
                }

                const runtimeId = `localhost:${port}`;
                remoteClientManager.setClient(runtimeId, "network", client, {
                    icon: "chrome://devtools/skin/images/aboutdebugging-firefox-release.svg",
                    name: deviceDescription.name || "Ladybird",
                    os: deviceDescription.os || "Unknown",
                    type: "network",
                    version: deviceDescription.version || "Unknown",
                });
                remoteId = remoteClientManager.getRemoteId(runtimeId, "network");

                client.on("openToolbox", packet => {
                    if (packet.from !== ladybirdActor || !Number.isSafeInteger(packet.tabId)) {
                        return;
                    }

                    requestToolbox(packet.tabId).catch(console.error);
                });
                client.on("inspectElement", packet => {
                    if (packet.from !== ladybirdActor || !Number.isSafeInteger(packet.tabId) || typeof packet.nodeId !== "string") {
                        return;
                    }

                    inspectElement(packet.tabId, packet.nodeId).catch(console.error);
                });
                await client.request({ to: ladybirdActor, type: "connect" });

                discardClosedToolboxes();
                setInterval(discardClosedToolboxes, 1000);
            }

            function startHiddenController() {
                const parameters = new URLSearchParams(location.search);
                parameters.set("background", "1");

                const documentUrl = location.href.split("?")[0];
                const backgroundUrl = Services.io.newURI(`${documentUrl}?${parameters}`);

                Services.appShell.createTopLevelWindow(null, backgroundUrl, Ci.nsIWebBrowserChrome.CHROME_OPENAS_CHROME, 1, 1);
                window.close();
            }

            const parameters = new URLSearchParams(location.search);
            if (parameters.has("background")) {
                window.addEventListener("load", () => {
                    setTimeout(() => {
                        connect().catch(console.error);
                    });
                });
            } else {
                startHiddenController();
            }
        </script>
    </body>
</html>
)~~~"sv;

static constexpr auto TOOLBOX_HTML = R"~~~(
<!DOCTYPE html>
<html>
<head>
    <meta charset="utf-8"/>
    <title>DevTools</title>
    <style>
        html, body { margin: 0; padding: 0; width: 100%; height: 100%; }
        iframe { display: block; border: none; width: 100%; height: 100%; }
    </style>
</head>
<body>
    <script>
        // The iframe is only attached after the load event: the window is not shown until the initial document load
        // completes, which the toolbox frame would otherwise delay indefinitely.
        window.addEventListener("load", () => {
            setTimeout(() => {
                const frame = document.createElement("iframe");
                frame.src = "about:devtools-toolbox" + location.search;
                document.body.appendChild(frame);
            });
        });
    </script>
</body>
</html>
)~~~"sv;

static constexpr auto PROFILE_PREFS = R"~~~(
user_pref("app.update.auto", false);
user_pref("app.update.background.enabled", false);
user_pref("app.update.disabledForTesting", true);
user_pref("browser.dom.window.dump.enabled", true);
user_pref("browser.sessionstore.resume_from_crash", false);
user_pref("browser.shell.checkDefaultBrowser", false);
user_pref("datareporting.policy.dataSubmissionEnabled", false);
user_pref("devtools.chrome.enabled", true);
user_pref("devtools.console.stdout.chrome", true);
user_pref("devtools.debugger.chrome-debugging-host", "localhost");
user_pref("devtools.debugger.chrome-debugging-websocket", false);
user_pref("devtools.debugger.prompt-connection", false);
user_pref("devtools.debugger.remote-enabled", true);
user_pref("toolkit.startup.max_resumed_crashes", -1);
)~~~"sv;

static constexpr auto RUNTIME_POLICY = R"~~~(
{
    "policies": {
        "DisableAppUpdate": true
    }
}
)~~~"sv;

bool is_supported_platform()
{
#if defined(AK_OS_MACOS) || (defined(AK_OS_LINUX) && (ARCH(X86_64) || ARCH(AARCH64)))
    return true;
#else
    return false;
#endif
}

URL::URL const& runtime_download_url()
{
    static NeverDestroyed<URL::URL> download_url = []() {
        static constexpr auto BASE_URL = "https://ftp.mozilla.org/pub/firefox/releases"sv;

#if defined(AK_OS_MACOS)
        auto download_url = ByteString::formatted("{0}/{1}/mac/en-US/Firefox%20{1}.dmg", BASE_URL, FIREFOX_VERSION);
#elif defined(AK_OS_LINUX)
#    if ARCH(X86_64)
        static constexpr auto ARCH = "x86_64"sv;
#    elif ARCH(AARCH64)
        static constexpr auto ARCH = "aarch64"sv;
#    else
        VERIFY_NOT_REACHED();
#    endif

        auto download_url = ByteString::formatted("{0}/{1}/linux-{2}/en-US/firefox-{1}.tar.xz", BASE_URL, FIREFOX_VERSION, ARCH);
#else
        VERIFY_NOT_REACHED();
#endif

        return URL::Parser::basic_parse(download_url).release_value();
    }();

    return *download_url;
}

static constexpr StringView runtime_archive_sha256()
{
#if defined(AK_OS_MACOS)
    return "b8a46188850d2fb32f16ad3c0829b08cd689bc714b8ee18fd9b09bb60629004d"sv;
#elif defined(AK_OS_LINUX)
#    if ARCH(X86_64)
    return "748704b06ffaabc8bd2b3351c93c244db4e29bfe21af6ecb4d6c124c7bd4a234"sv;
#    elif ARCH(AARCH64)
    return "c44a986475745e9ae292e3d6561b2c0d366e6ff0cca06a1bbc6e166cfb0eb2f1"sv;
#    else
    VERIFY_NOT_REACHED();
#    endif
#else
    VERIFY_NOT_REACHED();
#endif
}

static LexicalPath const& base_directory()
{
    static NeverDestroyed<LexicalPath> path = LexicalPath::join(Core::StandardPaths::user_data_directory(), "Ladybird"sv, "DevTools"sv);
    return *path;
}

static LexicalPath const& runtime_directory()
{
    static NeverDestroyed<LexicalPath> path = base_directory().append("runtime"sv).append("firefox"sv);
    return *path;
}

static LexicalPath const& runtime_resources_directory()
{
#if defined(AK_OS_MACOS)
    static NeverDestroyed<LexicalPath> path = runtime_directory().append("Firefox.app"sv).append("Contents"sv).append("Resources"sv);
#else
    static NeverDestroyed<LexicalPath> path = runtime_directory();
#endif
    return *path;
}

static LexicalPath const& runtime_binary()
{
#if defined(AK_OS_MACOS)
    static NeverDestroyed<LexicalPath> path = runtime_directory().append("Firefox.app"sv).append("Contents"sv).append("MacOS"sv).append("firefox"sv);
#else
    static NeverDestroyed<LexicalPath> path = runtime_directory().append("firefox"sv);
#endif
    return *path;
}

static LexicalPath const& runtime_omnija()
{
    static NeverDestroyed<LexicalPath> path = runtime_resources_directory().append("browser"sv).append("omni.ja"sv);
    return *path;
}

static LexicalPath const& profile_directory()
{
    static NeverDestroyed<LexicalPath> path = base_directory().append("profile"sv);
    return *path;
}

static LexicalPath const& ready_marker_path()
{
    static NeverDestroyed<LexicalPath> path = runtime_directory().append(ByteString::formatted(".ladybird-provisioned-{}", FIREFOX_VERSION));
    return *path;
}

LexicalPath const& runtime_download_path()
{
#if defined(AK_OS_MACOS)
    static NeverDestroyed<LexicalPath> path = base_directory().append("devtools-runtime.dmg"sv);
#else
    static NeverDestroyed<LexicalPath> path = base_directory().append("devtools-runtime.tar.xz"sv);
#endif
    return *path;
}

static ErrorOr<void> mark_runtime_ready()
{
    (void)TRY(Core::File::open(ready_marker_path().string(), Core::File::OpenMode::Write));
    return {};
}

bool runtime_is_ready()
{
    if (!FileSystem::exists(runtime_binary().string()) || !FileSystem::exists(ready_marker_path().string()))
        return false;

    auto configuration = Core::ConfigFile::open(runtime_resources_directory().append("application.ini"sv).string());
    if (configuration.is_error())
        return false;

    return configuration.value()->read_entry("App", "Version") == FIREFOX_VERSION;
}

static ErrorOr<void> verify_archive_sha256(StringView archive_path, StringView expected_sha256)
{
    auto expected_digest = TRY(decode_hex(expected_sha256));
    VERIFY(expected_digest.size() == Crypto::Hash::SHA256::digest_size());

    auto archive = TRY(Core::File::open(archive_path, Core::File::OpenMode::Read));
    auto hasher = Crypto::Hash::SHA256::create();
    Array<u8, 64 * KiB> buffer;

    for (;;) {
        auto bytes = TRY(archive->read_some(buffer.span()));
        if (bytes.is_empty())
            break;
        hasher->update(bytes);
    }

    if (hasher->digest().bytes() != expected_digest.bytes())
        return Error::from_string_literal("Archive SHA-256 digest does not match");

    return {};
}

ErrorOr<void> extract_runtime()
{
    TRY(verify_archive_sha256(runtime_download_path().string(), runtime_archive_sha256()));

    auto const& runtime = runtime_directory();
    auto parent = runtime.parent();

    if (FileSystem::exists(runtime.string()))
        TRY(FileSystem::remove(runtime.string(), FileSystem::RecursionMode::Allowed));

#if defined(AK_OS_MACOS)
    static NeverDestroyed<ByteString> hdiutil = "/usr/bin/hdiutil"sv;

    auto mount_point = base_directory().append(ByteString::formatted("devtools-runtime-mount-{}", Core::System::getpid()));

    auto detach_disk_image = [&] {
        auto process = Core::Process::spawn({
            .executable = *hdiutil,
            .arguments = {
                "detach"sv,
                "-quiet"sv,
                mount_point.string(),
            },
        });
        if (!process.is_error())
            (void)process.value().wait_for_termination();
    };

    // Recover from an interrupted provisioning attempt that left this process's mount point behind.
    if (FileSystem::exists(mount_point.string())) {
        detach_disk_image();
        if (FileSystem::exists(mount_point.string()))
            TRY(Core::System::rmdir(mount_point.string()));
    }

    TRY(Core::Directory::create(mount_point, Core::Directory::CreateDirectories::Yes));
    TRY(Core::Directory::create(runtime, Core::Directory::CreateDirectories::Yes));

    ScopeGuard guard = [&]() {
        detach_disk_image();
        (void)Core::System::rmdir(mount_point.string());
    };

    auto attach_process = TRY(Core::Process::spawn({
        .executable = *hdiutil,
        .arguments = {
            "attach"sv,
            "-quiet"sv,
            "-nobrowse"sv,
            "-readonly"sv,
            "-mountpoint"sv,
            mount_point.string(),
            runtime_download_path().string(),
        },
    }));
    if (TRY(attach_process.wait_for_termination()) != 0)
        return Error::from_string_literal("Unable to mount the DevTools runtime disk image");

    auto source = mount_point.append("Firefox.app"sv);
    if (!FileSystem::exists(source.string()))
        return Error::from_string_literal("The DevTools runtime disk image does not contain Firefox.app");

    auto source_stat = TRY(Core::System::stat(source.string()));
    TRY(FileSystem::copy_directory(runtime.append("Firefox.app"sv).string(), source.string(), source_stat));
#else
    TRY(Core::Directory::create(parent, Core::Directory::CreateDirectories::Yes));
    TRY(Archive::TarExtractor::extract(runtime_download_path().string(), parent.string()));
#endif
    return {};
}

static ErrorOr<void> install_runtime_policy()
{
    auto distribution = runtime_resources_directory().append("distribution"sv);
    TRY(Core::Directory::create(distribution, Core::Directory::CreateDirectories::Yes));

    auto policy = TRY(Core::File::open(distribution.append("policies.json"sv).string(), Core::File::OpenMode::Write));
    TRY(policy->write_until_depleted(RUNTIME_POLICY));

    return {};
}

static ErrorOr<void> install_runtime_patches()
{
    static auto const RUNTIME_PATCHES = to_array<Archive::ZipArchive::Patch>({
        { CONTROLLER_ARCHIVE_PATH, CONTROLLER_HTML.bytes() },
        { TOOLBOX_ARCHIVE_PATH, TOOLBOX_HTML.bytes() },
    });

    TRY(Archive::ZipArchive::install_patches(runtime_omnija().string(), RUNTIME_PATCHES));
    return {};
}

ErrorOr<void> install_runtime()
{
    TRY(install_runtime_policy());
    TRY(install_runtime_patches());
    TRY(mark_runtime_ready());
    return {};
}

static ErrorOr<void> write_profile_prefs()
{
    auto const& profile = profile_directory();
    TRY(Core::Directory::create(profile, Core::Directory::CreateDirectories::Yes));

    auto file = TRY(Core::File::open(profile.append("user.js"sv).string(), Core::File::OpenMode::Write));
    TRY(file->write_until_depleted(PROFILE_PREFS));

    return {};
}

NonnullOwnPtr<Host> Host::create()
{
    return adopt_own(*new Host());
}

Host::~Host()
{
    if (is_running())
        (void)Core::Process::terminate_process(m_process->pid(), Core::Process::TerminationMode::Graceful);
}

bool Host::is_running() const
{
    return m_process.has_value() && !Core::System::kill(m_process->pid(), 0).is_error();
}

ErrorOr<void> Host::ensure_running(u16 port)
{
    if (is_running())
        return {};

    TRY(write_profile_prefs());

    m_process = TRY(Core::Process::spawn({
        .name = "devtools-client"sv,
        .executable = runtime_binary().string(),
        .arguments = {
            "-no-remote"sv,
#if defined(AK_OS_MACOS)
            "-foreground"sv,
#endif
            "-profile"sv,
            profile_directory().string(),
            "-purgecaches"sv,
            "-chrome"sv,
            ByteString::formatted("{}?port={}", CONTROLLER_CHROME_URL, port),
        },
    }));

    return {};
}

}
