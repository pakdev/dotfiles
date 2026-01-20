return {
  -- Add bash Tree-sitter parser for syntax highlighting
  {
    "nvim-treesitter/nvim-treesitter",
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "bash" })
      end
    end,
  },
  -- Configure formatter for bash/shell scripts
  {
    "stevearc/conform.nvim",
    optional = true,
    opts = function(_, opts)
      opts.formatters_by_ft = opts.formatters_by_ft or {}
      -- Configure formatters for bash and sh filetypes
      -- Try shfmt first, fallback to trim_whitespace
      opts.formatters_by_ft.bash = { "shfmt", "trim_whitespace", stop_after_first = true }
      opts.formatters_by_ft.sh = { "shfmt", "trim_whitespace", stop_after_first = true }

      opts.formatters = opts.formatters or {}
      -- Define trim_whitespace formatter as fallback
      opts.formatters.trim_whitespace = {
        command = "sed",
        args = { "-e", "s/[[:space:]]*$//" },
        stdin = true,
      }
    end,
  },
}
