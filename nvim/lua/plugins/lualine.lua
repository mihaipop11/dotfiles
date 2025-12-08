return {
  'nvim-lualine/lualine.nvim',
  dependencies = {
    'nvim-tree/nvim-web-devicons',
    {
      'f-person/git-blame.nvim',
      config = function()
        vim.g.gitblame_display_virtual_text = 0
      end,
    },
  },

  config = function()
    require('lualine').setup {
      icons_enabled = true,
      options = {
        theme = 'powerline',
        component_separators = { left = '|', right = '|' },
        section_separators = {},
      },
      sections = {
        lualine_a = { 'mode' },
        lualine_b = { 'branch', 'diff', 'diagnostics' },
        lualine_c = { 'filename' },
        lualine_x = { 'copilot', 'encoding', 'fileformat', 'filetype' },
        lualine_y = { 'progress' },
        lualine_z = { 'location' },
      },
      inactive_sections = {
        lualine_a = {},
        lualine_b = {},
        lualine_c = { 'filename' },
        lualine_x = { 'location' },
        lualine_y = {},
        lualine_z = {},
      },
      tabline = {},
      extensions = {},
    }
  end,
}
