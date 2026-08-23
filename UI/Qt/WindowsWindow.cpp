/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <UI/Qt/WindowsWindow.h>

#include <AK/Optional.h>
#include <QAbstractNativeEventFilter>
#include <QCoreApplication>
#include <QWidget>

#include <AK/Windows.h>

namespace Ladybird {

// Marks the windows our WM_NCCALCSIZE handling applies to, so the filter can ignore every other
// native window in the process.
static constexpr wchar_t const* EXPANDED_CLIENT_AREA_PROPERTY = L"LadybirdExpandedClientArea";

// Qt paints its own title bar - window icon, window title, caption buttons - into a transparent
// child window registered under this class name, and keeps it parented to us for as long as
// Qt::ExpandedClientAreaHint is set.
static constexpr wchar_t const* QT_TITLE_BAR_CLASS_NAME = L"_q_titlebar";

static int resize_border_thickness(UINT dpi)
{
    return GetSystemMetricsForDpi(SM_CXSIZEFRAME, dpi) + GetSystemMetricsForDpi(SM_CXPADDEDBORDER, dpi);
}

// How far the window rectangle extends past the frame the compositor actually draws. This is the
// invisible grab area for resizing, and it is two device pixels narrower than the resize border
// metrics suggest - the same correction QWindowsWindow::calculateFullFrameMargins() applies to its
// own frame margins for an expanded client area.
//
// This has to be derived rather than measured. DWMWA_EXTENDED_FRAME_BOUNDS lags the window
// rectangle while a resize is in flight, and an inset that moves from one WM_NCCALCSIZE to the next
// leaves a sliver of unpainted frame flickering along the edge being dragged.
static int invisible_border_thickness(UINT dpi)
{
    auto scale = max(1, static_cast<int>(dpi) / 96);
    return max(resize_border_thickness(dpi) - 2 * scale, 0);
}

// Qt's own WM_NCCALCSIZE handling for an expanded client area takes the full resize border width
// off the left, right and bottom of the client area. That is wider than the invisible border, so
// the difference is non-client area that neither we nor the compositor ever paint, and it shows up
// as a bright line down three sides of the window. Claim those pixels for the client area instead.
class ExpandedClientAreaEventFilter final : public QAbstractNativeEventFilter {
public:
    virtual bool nativeEventFilter(QByteArray const& event_type, void* message, qintptr* result) override
    {
        if (event_type != QByteArrayLiteral("windows_generic_MSG"))
            return false;

        auto* msg = static_cast<MSG*>(message);
        if (msg->message != WM_NCCALCSIZE || msg->wParam == FALSE)
            return false;
        if (GetPropW(msg->hwnd, EXPANDED_CLIENT_AREA_PROPERTY) == nullptr)
            return false;

        auto dpi = GetDpiForWindow(msg->hwnd);

        // A maximized window is placed so that it hangs off every edge of the monitor by the full
        // resize border, and all four sides have to come back in by that much or the chrome is
        // drawn off-screen. Anywhere else the window only extends past the visible frame by the
        // invisible grab area, which is narrower.
        auto maximized = IsZoomed(msg->hwnd) != FALSE;
        auto inset = maximized ? resize_border_thickness(dpi) : invisible_border_thickness(dpi);

        auto* parameters = reinterpret_cast<NCCALCSIZE_PARAMS*>(msg->lParam);
        auto& client_area = parameters->rgrc[0];

        if (maximized)
            client_area.top += inset;
        client_area.left += inset;
        client_area.right -= inset;
        client_area.bottom -= inset;

        *result = 0;
        return true;
    }
};

static void install_expanded_client_area_event_filter()
{
    static bool installed = false;
    if (installed)
        return;

    auto* application = QCoreApplication::instance();
    if (!application)
        return;

    application->installNativeEventFilter(new ExpandedClientAreaEventFilter);
    installed = true;
}

// Qt::WindowTitleHint has to stay set: it is the only way to keep QWindowsWindow's hit testing on a
// code path that lets the window receive mouse events at all (see apply_expanded_client_area_flags).
// It also makes Qt paint the window icon and title straight over our tab strip. Hiding the child
// window Qt paints them into is what keeps them out of sight.
static void hide_qt_title_bar(HWND window)
{
    if (auto* title_bar = FindWindowExW(window, nullptr, QT_TITLE_BAR_CLASS_NAME, nullptr))
        ShowWindow(title_bar, SW_HIDE);
}

// Dropping the window button hints - which we must, or Qt hit-tests and paints caption buttons on
// top of our own - also drops the styles the rest of the shell keys off: Aero Snap needs
// WS_MAXIMIZEBOX alongside WS_THICKFRAME, and the taskbar's minimize and restore commands need
// WS_MINIMIZEBOX. Put them back directly.
static void restore_window_frame_styles(HWND window)
{
    auto style = GetWindowLongPtrW(window, GWL_STYLE);
    auto updated_style = style | WS_SYSMENU | WS_MINIMIZEBOX | WS_MAXIMIZEBOX;
    if (style == updated_style)
        return;

    SetWindowLongPtrW(window, GWL_STYLE, updated_style);
}

void apply_expanded_client_area_window_styles(QWidget& widget)
{
    auto* window = reinterpret_cast<HWND>(widget.winId());
    if (!window)
        return;

    install_expanded_client_area_event_filter();
    SetPropW(window, EXPANDED_CLIENT_AREA_PROPERTY, reinterpret_cast<HANDLE>(1));

    restore_window_frame_styles(window);
    hide_qt_title_bar(window);

    // Recalculate the frame so our WM_NCCALCSIZE handling takes effect for the current geometry.
    SetWindowPos(window, nullptr, 0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER | SWP_NOACTIVATE | SWP_FRAMECHANGED);
}

}
