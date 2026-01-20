return {
  {
    "3ZsForInsomnia/token-count.nvim",
    config = function()
      require("token-count").setup({
        -- Show token count in command line when selecting text
        show_on_select = true,
        -- Enable token count in statusline (optional)
        statusline = false,
        -- Use chatgpt-5 model for token counting
        model = "chatgpt-5",
      })
    end,
  },
}
