-- Workaround for vim treesitter query issues
return {
  {
    "folke/noice.nvim",
    opts = {
      presets = {
        bottom_search = true,
        command_palette = true,
        long_message_to_split = true,
      },
      -- Disable vim syntax highlighting in cmdline to avoid query issues
      cmdline = {
        view = "cmdline",
        format = {
          cmdline = { lang = "" }, -- Disable treesitter for cmdline
          search_down = { lang = "" },
          search_up = { lang = "" },
          filter = { lang = "" },
          lua = { lang = "" },
          help = { lang = "" },
        },
      },
    },
  },
}
