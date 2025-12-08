-- undotree
-- https://github.com/tpope/vim-commentary

return {
  'tpope/vim-commentary',
  cmd = 'Commentary',
  -- keys = {
  --  {'x', 'gc'},
  --},
  setup = function()
    vim.g.commentary_format = 'default'
  end,
}
