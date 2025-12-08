-- Neo-tree is a Neovim plugin to browse the file system
-- https://github.com/nvim-neo-tree/neo-tree.nvim

return {
  enabled = false, -- replaced
  "nvim-neo-tree/neo-tree.nvim",
  version = "*",
  dependencies = {
    "nvim-lua/plenary.nvim",
    "nvim-tree/nvim-web-devicons", -- not strictly required, but recommended
    "MunifTanjim/nui.nvim",
  },
  lazy = false,
  keys = {
    { "\\", ":Neotree reveal<CR>", desc = "NeoTree reveal", silent = true },
  },
  opts = {
    reveal_force_cwd = true,
    filesystem = {
      window = {
        mappings = {
          ["\\"] = "close_window",
        },
        position = "float",
      },
    },
    buffers = {
      show_unloaded = true,
      window = {
        mappings = {
          ["\\"] = "close_window",
        },
        position = "float",
      },
    },
  },
}
