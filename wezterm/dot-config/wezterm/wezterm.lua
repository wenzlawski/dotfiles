-- Pull in the wezterm API
local wezterm = require("wezterm")

-- This table will hold the configuration.
local config = {}

-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then
	config = wezterm.config_builder()
end

-- wezterm.gui is not available to the mux server, so take care to
-- do something reasonable when this config is evaluated by the mux
function get_appearance()
  if wezterm.gui then
    return wezterm.gui.get_appearance()
  end
  return 'Dark'
end

function scheme_for_appearance(appearance)
  if appearance:find 'Dark' then
    return 'Monokai Remastered'
  else
    return 'Equilibrium Light (base16)'
  end
end

-- This is where you actually apply your config choices

config.animation_fps = 100

-- For example, changing the color scheme:
config.color_scheme = scheme_for_appearance(get_appearance())
--config.color_scheme = "Mono Yellow (Gogh)"
config.font = wezterm.font_with_fallback {
  "Iosevka Comfy",
  "Apple Color Emoji"
}
config.enable_tab_bar = false
config.font_size = 16.0
config.cursor_thickness = 2
config.adjust_window_size_when_changing_font_size = false
--config.line_height = 0.9
config.window_decorations = "TITLE | RESIZE"
config.initial_rows = 25
config.initial_cols = 90

config.scrollback_lines = 5000

config.window_padding = {
    left = 2,
    right = 0,
    top = 0,
    bottom = 0,
}

config.window_frame = {
    -- The font used in the tab bar.
    -- Roboto Bold is the default; this font is bundled
    -- with wezterm.
    -- Whatever font is selected here, it will have the
    -- main font setting appended to it to pick up any
    -- fallback fonts you may have used there.
    font = wezterm.font({ family = "Fira Code", weight = "Bold" }),
    -- The size of the font in the tab bar.
    -- Default to 10. on Windows but 12.0 on other systems
    font_size = 12.0,
    -- The overall background color of the tab bar when
    -- the window is focused
    active_titlebar_bg = "#333333",
    -- The overall background color of the tab bar when
    -- the window is not focused
    inactive_titlebar_bg = "#333333",
    border_left_width = 2,
    border_right_width = 2,
    border_bottom_height = 2,
    border_top_height = 2,
}

-- window.colors = {
--   tab_bar = {
--     -- The color of the inactive tab bar edge/divider
--     inactive_tab_edge = '#575757',
--   },
-- }


-- and finally, return the configuration to wezterm
return config
