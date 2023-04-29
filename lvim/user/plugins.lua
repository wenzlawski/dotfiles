lvim.plugins = {
  {
    "RRethy/nvim-base16",
    priority = 1000,
  },
  {
    "notken12/base46-colors",
    priority = 1000

  },
  "roobert/tailwindcss-colorizer-cmp.nvim",
  "christianchiarulli/nvim-ts-autotag",
  "kylechui/nvim-surround",
  "NvChad/nvim-colorizer.lua",
  "folke/todo-comments.nvim",
  "windwp/nvim-spectre",
  "folke/zen-mode.nvim",
  "lvimuser/lsp-inlayhints.nvim",
  "ggandor/leap.nvim",
  "sindrets/diffview.nvim",
  "simrat39/rust-tools.nvim",
  "mfussenegger/nvim-dap-python",
  "jose-elias-alvarez/typescript.nvim",
  "mxsdev/nvim-dap-vscode-js",
  "mrjones2014/smart-splits.nvim",
  {
    "saecki/crates.nvim",
    version = "v0.3.0",
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
      require("crates").setup {
        null_ls = {
          enabled = true,
          name = "crates.nvim",
        },
      }
    end,
  },
  {
    "zbirenbaum/copilot.lua",
    -- cmd = "Copilot",
    event = "InsertEnter",
  },
  {
    "zbirenbaum/copilot-cmp",
    after = { "copilot.lua" },
    config = function()
      require("copilot_cmp").setup()
    end,
  },
}
