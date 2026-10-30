import { registerDialogDeepLink } from "./dialog-deep-link.js";

const siteSettings = document.querySelector("#site-settings");
const siteSettingsAdd = document.querySelector("#site-settings-add");
const siteSettingsClose = document.querySelector("#site-settings-close");
const siteSettingsPolicy = document.querySelector("#site-settings-policy");
const siteSettingsList = document.querySelector("#site-settings-list");
const siteSettingsInput = document.querySelector("#site-settings-input");
const siteSettingsDecision = document.querySelector("#site-settings-decision");
const siteSettingsRemoveAll = document.querySelector("#site-settings-remove-all");
const siteSettingsTitle = document.querySelector("#site-settings-title");

const PERMISSION_POLICY_OPTIONS = [
    { value: "ask", label: "Ask before allowing" },
    { value: "block", label: "Block" },
];

const SITE_SETTINGS = {
    autoplay: { title: "Autoplay", hash: "autoplay", button: "#autoplay-settings" },
    geolocation: { title: "Location", hash: "location-permissions", button: "#location-permissions-settings" },
    microphone: { title: "Microphone", hash: "microphone-permissions", button: "#microphone-permissions-settings" },
};

const SITE_SETTING_POLICY_OPTIONS = {
    autoplay: [
        { value: "allow", label: "Allow audio and video" },
        { value: "allow-if-inaudible", label: "Block audio" },
        { value: "block", label: "Block audio and video" },
    ],
    geolocation: PERMISSION_POLICY_OPTIONS,
    microphone: PERMISSION_POLICY_OPTIONS,
};

let SETTINGS = {};
let currentSetting = null;

function loadSiteSettings(settings) {
    SETTINGS = { ...SETTINGS, ...settings };
    if (siteSettings.open) {
        renderSiteSettings();
    }
}

function forciblyEnableSiteSettings(settings) {
    settings.forEach(setting => {
        document.querySelector(`#${setting}-forcibly-enabled`).classList.remove("hidden");
        document.querySelector(`#${setting}-settings`).classList.add("hidden");
    });
}

function option(value, label) {
    const element = document.createElement("option");
    element.value = value;
    element.textContent = label;
    return element;
}

function setSiteDecision(origin, decision) {
    ladybird.sendMessage("setSitePermission", { permission: currentSetting, origin, decision });
}

function renderSiteSettings() {
    const settings = SETTINGS[currentSetting] ?? {};

    siteSettingsPolicy.replaceChildren(
        ...SITE_SETTING_POLICY_OPTIONS[currentSetting].map(({ value, label }) => option(value, label))
    );
    siteSettingsPolicy.value = settings.policy ?? SITE_SETTING_POLICY_OPTIONS[currentSetting][0].value;
    const decisionOptions =
        currentSetting === "autoplay"
            ? SITE_SETTING_POLICY_OPTIONS.autoplay
            : [
                  { value: "allow", label: "Allow" },
                  { value: "block", label: "Block" },
              ];
    const selectedDecision = siteSettingsDecision.value;
    siteSettingsDecision.replaceChildren(...decisionOptions.map(({ value, label }) => option(value, label)));
    siteSettingsDecision.value = selectedDecision;
    siteSettingsList.replaceChildren();

    // A deep link can open this dialog before the settings have loaded.
    const sites = SETTINGS.sitePermissions?.[currentSetting] ?? [];

    if (sites.length === 0) {
        const placeholder = document.createElement("div");
        placeholder.className = "dialog-list-item-placeholder";
        placeholder.textContent = "No sites added";
        siteSettingsList.append(placeholder);
    }

    sites.forEach(site => {
        const row = document.createElement("div");
        row.className = "dialog-list-item";
        row.dataset.origin = site.origin;

        const label = document.createElement("span");
        label.className = "dialog-list-item-label";
        label.textContent = site.origin;
        row.append(label);

        const select = document.createElement("select");
        select.setAttribute("aria-label", `Permission for ${site.origin}`);
        select.append(...decisionOptions.map(({ value, label }) => option(value, label)));
        if (site.decision === "allow-once") {
            select.append(option("allow-once", "Allow this time"));
        }
        select.value = site.decision;
        select.addEventListener("change", () => setSiteDecision(site.origin, select.value));
        row.append(select);

        const remove = document.createElement("button");
        remove.className = "dialog-button";
        remove.innerHTML = "&times;";
        remove.title = `Remove ${site.origin}`;
        remove.addEventListener("click", () => {
            setSiteDecision(site.origin, null);
        });
        row.append(remove);
        siteSettingsList.append(row);
    });
}

function showSiteSettings(setting) {
    if (currentSetting !== setting) {
        siteSettingsInput.value = "";
        siteSettingsInput.setCustomValidity("");
        siteSettingsDecision.value = "allow";
    }

    currentSetting = setting;
    siteSettingsTitle.textContent = SITE_SETTINGS[setting].title;

    renderSiteSettings();
    setTimeout(() => siteSettingsInput.focus());
}

function addSitePermission() {
    const filter = siteSettingsInput.value.trim();
    if (!filter) {
        return;
    }

    let url;
    try {
        url = new URL(filter);
    } catch {
        // Invalid and opaque URLs cannot identify a site permission.
    }

    if (!url || url.origin === "null") {
        siteSettingsInput.setCustomValidity("Enter a site URL, such as https://example.com.");
        siteSettingsInput.reportValidity();
        return;
    }

    setSiteDecision(url.origin, siteSettingsDecision.value);

    siteSettingsInput.classList.add("success");
    siteSettingsInput.value = "";
    setTimeout(() => siteSettingsInput.classList.remove("success"), 1000);
    siteSettingsInput.focus();
}

siteSettingsPolicy.addEventListener("change", () => {
    ladybird.sendMessage("setPermissionPolicy", { permission: currentSetting, policy: siteSettingsPolicy.value });
});
siteSettingsAdd.addEventListener("click", addSitePermission);
siteSettingsInput.addEventListener("input", () => siteSettingsInput.setCustomValidity(""));
siteSettingsInput.addEventListener("keydown", event => {
    if (event.key === "Enter") {
        addSitePermission();
    }
});
siteSettingsClose.addEventListener("click", () => siteSettings.close());
siteSettingsRemoveAll.addEventListener("click", () => {
    ladybird.sendMessage("removeAllSitePermissions", { permission: currentSetting });
});

for (const [setting, { hash, button }] of Object.entries(SITE_SETTINGS)) {
    registerDialogDeepLink({
        hash,
        tab: "permissions",
        dialog: siteSettings,
        onOpen: () => showSiteSettings(setting),
    });
    document.querySelector(button).addEventListener("click", () => {
        location.hash = hash;
    });
}

document.addEventListener("WebUILoaded", () => {
    ladybird.sendMessage("loadForciblyEnabledSiteSettings");
});

document.addEventListener("WebUIMessage", event => {
    if (event.detail.name === "loadSettings") {
        loadSiteSettings(event.detail.data);
    } else if (event.detail.name === "forciblyEnableSiteSettings") {
        forciblyEnableSiteSettings(event.detail.data);
    }
});
