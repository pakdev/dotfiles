return {
  -- Add Nix Tree-sitter parser for syntax highlighting
  {
    "nvim-treesitter/nvim-treesitter",
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "nix" })
      end
    end,
  },
  -- Add nixfmt formatter for Nix
  {
    "stevearc/conform.nvim",
    optional = true,
    opts = {
      formatters_by_ft = {
        nix = { "nixfmt" },
      },
    },
  },
  -- Lazy-load nil_ls only for Nix files
  {
    "neovim/nvim-lspconfig",
    ft = { "nix" },
    opts = {
      servers = {
        nil_ls = {
          mason = false,
        },
      },
    },
  },
}
