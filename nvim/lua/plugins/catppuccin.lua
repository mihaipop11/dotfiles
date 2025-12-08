return {
  'catppuccin/nvim',
  name = 'catppuccin',
  priority = 1000,
  config = function()
    ---@diagnostic disable-next-line: missing-fields
    require('catppuccin').setup {
      flavour = 'mocha', -- latte, frappe, macchiato, mocha
      dim_inactive = {
        enabled = true, -- Dim inactive windows
        shade = 'dark', -- Shade color for inactive windows
        percentage = 0.10, -- Percentage of dimming
      },
      auto_integrations = true,
    }

    vim.cmd.colorscheme 'catppuccin'
  end,
}
