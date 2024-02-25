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
			if
				hs.osascript.applescript(
					'tell application "System Events" to tell appearance preferences to get dark mode'
				) == "true"
			then
				hs.execute(
					'osascript -e \'tell application "System Events" to tell every desktop to set picture to "/System/Library/Desktop Pictures/Solid Colors/Black.png" as POSIX file\''
				)
			else
				hs.execute(
					'osascript -e \'tell application "System Events" to tell every desktop to set picture to "/System/Library/Desktop Pictures/Solid Colors/Silver.png" as POSIX file\''
				)
			end
		end
	end)
	:start()

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
