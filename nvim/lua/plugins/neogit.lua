-- neogit
-- https://github.com/NeogitOrg/neogit

return {
  "NeogitOrg/neogit",
  cmd = "Neogit",
  keys = { { "<leader>gn", "<cmd>Neogit<CR>", mode = { "n" }, desc = "Open Neo[G]it" } },
  dependencies = {
    "nvim-lua/plenary.nvim", -- required
    "sindrets/diffview.nvim", -- optional - Diff integration
    "nvim-telescope/telescope.nvim",
  },
  integrations = {
    diffview = true,
  },
  config = function()
    require("neogit").setup({ kind = "tab" })
  end,
}
