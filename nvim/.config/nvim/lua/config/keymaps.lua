-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

-- Beautify JSON using jq
vim.keymap.set({ "n", "v" }, "<leader>jM", function()
  local mode = vim.fn.mode()
  if mode == "n" then
    vim.cmd("%!jq '.'")
  else
    vim.cmd("'<,'>!jq '.'")
  end
end, { desc = "Beautify JSON (jq)" })

-- Override ZQ and ZZ to quit all windows (including NeoTree)
vim.keymap.set("n", "ZQ", "<cmd>qa<cr>", { desc = "Quit all windows" })
vim.keymap.set("n", "ZZ", "<cmd>wqa<cr>", { desc = "Save all and quit all windows" })

-- NDJSON: pretty-print each line with jq
vim.keymap.set("n", "<leader>jM", "<cmd>%!jq -M .<cr>", { desc = "Pretty-print NDJSON with jq" })
