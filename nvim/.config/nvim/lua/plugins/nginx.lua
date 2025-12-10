return {
  -- Add nginx Tree-sitter parser for syntax highlighting
  {
    "nvim-treesitter/nvim-treesitter",
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "nginx" })
      end
    end,
  },
  -- Add trailing whitespace removal formatter for nginx conf files
  {
    "stevearc/conform.nvim",
    optional = true,
    opts = function(_, opts)
      opts.formatters_by_ft = opts.formatters_by_ft or {}
      opts.formatters_by_ft.nginx = { "trim_whitespace" }

      opts.formatters = opts.formatters or {}
      opts.formatters.trim_whitespace = {
        command = "sed",
        args = { "-e", "s/[[:space:]]*$//" },
        stdin = true,
      }
    end,
  },
}
