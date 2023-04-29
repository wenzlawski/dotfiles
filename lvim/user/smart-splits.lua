local status_ok, splits = pcall(require, "smart-splits")
if not status_ok then
  return
end
splits.setup({
  mappings = {
    ["<Up>"] = { function() require("smart-splits").resize_up(2) end, "Resize split up" },
    ["<Down>"] = { function() require("smart-splits").resize_down(2) end, "Resize split down" },
    ["<Left>"] = { function() require("smart-splits").resize_left(2) end, "Resize split left" },
    ["<Right>"] = { function() require("smart-splits").resize_right(2) end, "Resize split right" },
  }
})
