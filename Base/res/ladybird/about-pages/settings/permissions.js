import { registerDialogDeepLink } from "./dialog-deep-link.js";

const siteSettings = document.querySelector("#site-settings");
const siteSettingsAdd = document.querySelector("#site-settings-add");
const siteSettingsClose = document.querySelector("#site-settings-close");
const siteSettingsPolicy = document.querySelector("#site-settings-policy");
const siteSettingsList = document.querySelector("#site-settings-list");
const siteSettingsListLabel = document.querySelector("#site-settings-list-label");
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
        { value: "allow-audio-and-video", label: "Allow audio and video" },
        { value: "block-audio", label: "Block audio" },
        { value: "block-audio-and-video", label: "Block audio and video" },
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
    const isPermission = currentSetting !== "autoplay";

    siteSettingsPolicy.replaceChildren(
        ...SITE_SETTING_POLICY_OPTIONS[currentSetting].map(({ value, label }) => option(value, label))
    );
    siteSettingsPolicy.value = settings.policy ?? SITE_SETTING_POLICY_OPTIONS[currentSetting][0].value;
    siteSettingsListLabel.textContent = isPermission ? "Site permissions" : "Allowlist";
    siteSettingsDecision.classList.toggle("hidden", !isPermission);
    siteSettingsInput.placeholder = isPermission ? "https://example.com" : "example.com";
    siteSettingsList.replaceChildren();

    // A deep link can open this dialog before the settings have loaded.
    const sites = isPermission
        ? (SETTINGS.sitePermissions?.[currentSetting] ?? [])
        : (settings.siteFilters ?? []).map(origin => ({ origin }));

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

        if (isPermission) {
            const select = document.createElement("select");
            select.setAttribute("aria-label", `Permission for ${site.origin}`);
            select.append(option("allow", "Allow"));
            if (site.decision === "allow-once") {
                select.append(option("allow-once", "Allow this time"));
            }
            select.append(option("block", "Block"));
            select.value = site.decision;
            select.addEventListener("change", () => setSiteDecision(site.origin, select.value));
            row.append(select);
        }

        const remove = document.createElement("button");
        remove.className = "dialog-button";
        remove.innerHTML = "&times;";
        remove.title = `Remove ${site.origin}`;
        remove.addEventListener("click", () => {
            if (isPermission) {
                setSiteDecision(site.origin, null);
            } else {
                ladybird.sendMessage("removeSiteSettingFilter", { setting: currentSetting, filter: site.origin });
            }
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

function addSiteSettingFilter() {
    const filter = siteSettingsInput.value.trim();
    if (!filter) {
        return;
    }

    if (currentSetting === "autoplay") {
        ladybird.sendMessage("addSiteSettingFilter", { setting: currentSetting, filter });
    } else {
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
    }

    siteSettingsInput.classList.add("success");
    siteSettingsInput.value = "";
    setTimeout(() => siteSettingsInput.classList.remove("success"), 1000);
    siteSettingsInput.focus();
}

siteSettingsPolicy.addEventListener("change", () => {
    ladybird.sendMessage("setSiteSettingPolicy", { setting: currentSetting, policy: siteSettingsPolicy.value });
});
siteSettingsAdd.addEventListener("click", addSiteSettingFilter);
siteSettingsInput.addEventListener("input", () => siteSettingsInput.setCustomValidity(""));
siteSettingsInput.addEventListener("keydown", event => {
    if (event.key === "Enter") {
        addSiteSettingFilter();
    }
});
siteSettingsClose.addEventListener("click", () => siteSettings.close());
siteSettingsRemoveAll.addEventListener("click", () => {
    ladybird.sendMessage("removeAllSiteSettingFilters", { setting: currentSetting });
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
