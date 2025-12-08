-- undotree
-- https://github.com/mbbill/undotree

return {
  'mbbill/undotree',
  cmd = 'UndotreeToggle',
  keys = { { '<leader>u', '<cmd>UndotreeToggle<CR>', mode = { 'n' }, desc = 'Toggle [U]ndo Tree' } },
  config = function()
    vim.g.undotree_WindowLayout = 2
    vim.g.undotree_SetFocusWhenToggle = 1
    vim.g.undotree_DiffpanelHeight = 20
    vim.g.undotree_DiffAutoOpen = 0
  end,
}
