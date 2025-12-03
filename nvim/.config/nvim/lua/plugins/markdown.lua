return {
  -- Disable markdownlint-cli2 linter entirely and rely only on Marksman LSP
  {
    "mfussenegger/nvim-lint",
    optional = true,
    opts = {
      linters_by_ft = {
        markdown = {},
      },
    },
  },
  -- Configure Prettier for markdown with no line wrapping
  -- Note: Prettier is disabled for markdown to avoid conflicts with vim-table-mode
  {
    "stevearc/conform.nvim",
    optional = true,
    opts = {
      formatters_by_ft = {
        markdown = { "prettier" },
        ["markdown.mdx"] = { "prettier" },
      },
      formatters = {
        prettier = {
          prepend_args = { "--prose-wrap", "never" },
        },
      },
    },
  },
  -- Markdown table formatting plugin
  {
    "dhruvasagar/vim-table-mode",
    ft = { "markdown" },
    config = function()
      vim.g.table_mode_corner = "|"
      vim.g.table_mode_corner_corner = "|"
      vim.g.table_mode_header_fillchar = "-"
      -- Disable automatic table mode, only use manual formatting
      vim.g.table_mode_auto_align = 0
    end,
    keys = {
      { "<leader>tm", "<cmd>TableModeToggle<cr>", desc = "Toggle Table Mode" },
      { "<leader>tr", "<cmd>TableModeRealign<cr>", desc = "Realign Table" },
    },
  },
}
