local logger = hs.logger.new("init.lua", "debug")

-- Set LibreWolf to smaller 800x600 window upon new window creation.
local wf = hs.window.filter

-- local wf_librewolf = wf.new('LibreWolf')
-- wf_librewolf:subscribe(wf.windowCreated, function(win, _, _)
--     win:setSize(1000, 750)
-- end)

------------------------

-- on system theme change event, change hammerspoon console to dark mode

local dm = require("darkmode")

dm.addHandler(function(dm2)
	print("darkmode changed to " .. tostring(dm2))
	if dm2 == true then
		hs.console.darkMode(true)
		hs.console.outputBackgroundColor({ white = 0 })
		hs.console.consoleCommandColor({ white = 1 })
	else
		hs.console.darkMode(false)
		hs.console.outputBackgroundColor({ white = 1 })
		hs.console.consoleCommandColor({ white = 0 })
	end
end)

-----------------------

-- make a shortcut to F16 that opens a dialog box to ask whether to put the computer to sleep, it can be cancelled with ESC, and accepted with <Enter>

hs.hotkey.bind({ "ctrl", "alt" }, "=", function()
	local res = hs.dialog.blockAlert("Are you sure you want to put the computer to sleep?", "", "Sleep", "Cancel")
	-- force focus on the dialog box
	--hs.application.frontmostApplication():activate(true)
	if res == "Sleep" then
		hs.caffeinate.systemSleep()
	end
end)

---------------------

function autoSetWallpaper()
	local darkmode, out, out2 =
		hs.execute("osascript -e 'tell application \"System Events\" to tell appearance preferences to get dark mode'")
	logger.d("darkmode: " .. tostring(darkmode))
	logger.d("darkmode-b: " .. tostring(darkmode == true))
	logger.d("out: " .. tostring(out))
	logger.d("out2: " .. out2)
	if darkmode then
		logger.d("dark mode")
		hs.execute(
			'osascript -e \'tell application "System Events" to tell every desktop to set picture to "/System/Library/Desktop Pictures/Solid Colors/Black.png" as POSIX file\''
		)
	else
		logger.d("light mode")
		hs.execute(
			'osascript -e \'tell application "System Events" to tell every desktop to set picture to "/System/Library/Desktop Pictures/Solid Colors/Silver.png" as POSIX file\''
		)
	end
end

-- on system color change, switch between light and dark wallpaper.
dm.addHandler(function(dm2)
	local wallpaper = "Silver.png"
	if dm2 == true then
		wallpaper = "Black.png"
	end
	logger.d("wallpaper: " .. wallpaper)
	hs.execute(
		'osascript -e \'tell application "System Events" to tell every desktop to set picture to "/System/Library/Desktop Pictures/Solid Colors/'
			.. wallpaper
			.. "\" as POSIX file'"
	)
end)

-- on system wake check if dark mode is enabled and set the wallpaper accordingly
hs.caffeinate.watcher
	.new(function(event)
		if event == hs.caffeinate.watcher.systemDidWake then
			autoSetWallpaper()
		end
	end)
	:start()

-- add a menubar item to change the wallpaper
hs.menubar.new(true, "wallpaper"):setClickCallback(autoSetWallpaper):setTitle("wp")

---------------------

-- on system color change, switch spotify colors with spicetify
dm.addHandler(function(dm2)
	if dm2 == true then
		logger.d("dark mode")
		hs.execute("spicetify config current_theme Ziro", true)
		hs.execute("spicetify config color_scheme gray-dark", true)
	else
		logger.d("light mode")
		hs.execute("spicetify config current_theme Ziro", true)
		hs.execute("spicetify config color_scheme gray-light", true)
	end
	hs.execute("spicetify apply", true)
end)

---------------------

-- make a gui for selecting a spiceify theme it will have one dropdown to select
-- the theme, another for selecting the color scheme, and a button to apply the
-- changes

function spicetify_change_theme()
	local theme_paths = hs.fnutils.split(hs.execute("ls -d ~/.config/spicetify/Themes/*/"), "\n")

	local choices = {}
	for k, v in pairs(theme_paths) do
		if string.len(v) > 0 then
			local last = string.match(v, "/(%w+)/$")
			table.insert(choices, { ["text"] = last, ["path"] = v })
		end
	end

	logger.d("text: " .. choices[1]["text"])
	local chooser = hs.chooser
		.new(function(event)
			if event == nil then
				return
			end
			logger.d(event["text"])
			local schemes, status = hs.execute("rg -N -r '\\$1' '^\\[(.+)\\]$' " .. event["path"] .. "color.ini", true)

			local scheme_l = hs.fnutils.split(schemes, "\n")

			local choices = {}
			for k, v in pairs(scheme_l) do
				if string.len(v) > 0 then
					table.insert(choices, { ["text"] = v })
				end
			end

			local chooser2 = hs.chooser
				.new(function(n)
					if n == nil then
						return
					end
					hs.execute("spicetify config current_theme " .. event["text"], true)
					hs.execute("spicetify config color_scheme " .. n["text"], true)
					local _, status3 = hs.execute("spicetify apply", true)
					if status3 == nil then
						hs.execute("spicetify update", true)
					end
					hs.execute("spicetify apply", true)
				end)
				:choices(choices)
				:show()
		end)
		:choices(choices)
		:show()
end

hs.menubar.new(true, "sp"):setClickCallback(spicetify_change_theme):setTitle("sp")

-- watch the directory ~/Dropbox/denote/vault/.import for new files and call the script ~/Dropbox/denote/.import/move.sh individually on every new file. New files are anything but .sh files.
-- file creation events are fired multiple times for the same file by the OS, so we need to filter them out. only fire on itemCreated flag

-- function zip(...)
--     local arrays, ans = { ... }, {}
--     local index = 0
--     return
--         function()
--             index = index + 1
--             for i, t in ipairs(arrays) do
--                 if type(t) == 'function' then ans[i] = t() else ans[i] = t[index] end
--                 if ans[i] == nil then return end
--             end
--             return table.unpack(ans)
--         end
-- end

-- local import_dir = os.getenv("HOME") .. "/Dropbox/denote/vault/.import"
-- local import_script = os.getenv("HOME") .. "/Dropbox/denote/.import/move.sh"

-- local import_watcher = hs.pathwatcher.new(import_dir, function(files, flags)
--     logger.d("files: " .. hs.inspect(files))
--     logger.d("flags: " .. hs.inspect(flags))
--     for i, file in pairs(files) do
--         -- check that the file is not a .sh file and that the itemCreated flag is set
--         if string.sub(file, -3) ~= ".sh" then
--             logger.d("file: " .. file)
--             logger.d("flag: " .. flags[i])
--             -- hs.execute(import_script .. " " .. file)
--         end
--     end
-- end)

---------------------

logger.d("SUCCESSFULLY RAN init.lua")
