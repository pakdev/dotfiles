-- Autocmds are automatically loaded on the VeryLazy event
-- Default autocmds that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/autocmds.lua
--
-- Add any additional autocmds here
-- with `vim.api.nvim_create_autocmd`
--
-- Or remove existing autocmds by their group name (which is prefixed with `lazyvim_` for the defaults)
-- e.g. vim.api.nvim_del_augroup_by_name("lazyvim_wrap_spell")

-- Explicitly set gotmpl files to helm filetype for formatting
vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
  pattern = "*.gotmpl",
  callback = function()
    vim.bo.filetype = "helm"
  end,
})

-- Ensure flox manifest files use TOML formatter
vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
  pattern = "*.toml",
  callback = function()
    vim.bo.filetype = "toml"
  end,
})

-- Trim trailing whitespace in TOML files on save
local toml_trim_group = vim.api.nvim_create_augroup("TomlTrimWhitespace", { clear = true })
vim.api.nvim_create_autocmd("BufWritePre", {
  group = toml_trim_group,
  pattern = "*.toml",
  callback = function(args)
    local buf = args.buf
    local was_modifiable = vim.bo[buf].modifiable
    if not was_modifiable and not vim.bo[buf].readonly then
      vim.bo[buf].modifiable = true
    end

    if vim.bo[buf].modifiable then
      local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, true)
      local changed = false
      for i, line in ipairs(lines) do
        local trimmed = line:gsub("%s+$", "")
        if trimmed ~= line then
          lines[i] = trimmed
          changed = true
        end
      end
      if changed then
        vim.api.nvim_buf_set_lines(buf, 0, -1, true, lines)
      end
    end

    if vim.bo[buf].modifiable ~= was_modifiable then
      vim.bo[buf].modifiable = was_modifiable
    end
  end,
})

-- Set Dockerfile indentation to 4 spaces
vim.api.nvim_create_autocmd("FileType", {
  pattern = { "dockerfile", "docker-compose" },
  callback = function()
    vim.bo.tabstop = 4
    vim.bo.shiftwidth = 4
    vim.bo.softtabstop = 4
    vim.bo.expandtab = true
  end,
})
