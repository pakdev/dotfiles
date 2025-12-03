return {
  {
    "nvim-treesitter/nvim-treesitter",
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "go", "gomod", "gowork", "gosum" })
      end
    end,
  },
  {
    "stevearc/conform.nvim",
    opts = function(_, opts)
      -- Use trim_whitespace for helm files - removes trailing whitespace without breaking syntax
      opts.formatters_by_ft = opts.formatters_by_ft or {}
      opts.formatters_by_ft.helm = { "trim_whitespace" }
    end,
  },
}
