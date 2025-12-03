-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

-- Override ZQ and ZZ to quit all windows (including NeoTree)
vim.keymap.set("n", "ZQ", "<cmd>qa<cr>", { desc = "Quit all windows" })
vim.keymap.set("n", "ZZ", "<cmd>wqa<cr>", { desc = "Save all and quit all windows" })
