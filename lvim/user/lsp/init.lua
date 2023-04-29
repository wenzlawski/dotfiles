require "user.lsp.languages.rust"
require "user.lsp.languages.python"
require "user.lsp.languages.julia"
require "user.lsp.languages.sh"
require "user.lsp.languages.js-ts"
require "user.lsp.languages.css"
require "user.lsp.languages.emmet"

lvim.lsp.diagnostics.virtual_text = false

lvim.builtin.treesitter.ensure_installed = {
  "python",
}

local formatters = require "lvim.lsp.null-ls.formatters"
formatters.setup {
  { command = "stylua",   filetypes = { "lua" } },
  { command = "shfmt",    filetypes = { "sh", "zsh" } },
  { command = "prettier", filetypes = { "css" } },
}
