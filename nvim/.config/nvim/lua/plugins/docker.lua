return {
  -- Set buffer options for Dockerfile to use 4 spaces
  {
    "neovim/nvim-lspconfig",
    opts = function(_, opts)
      -- Ensure setup_handlers exists
      opts.setup = opts.setup or {}

      -- Configure dockerls to use 4 spaces
      opts.setup.dockerls = function(_, server_opts)
        server_opts.on_attach = function(client, bufnr)
          -- Set buffer-local options for 4-space indentation
          vim.bo[bufnr].tabstop = 4
          vim.bo[bufnr].shiftwidth = 4
          vim.bo[bufnr].softtabstop = 4
          vim.bo[bufnr].expandtab = true
        end
        require("lspconfig").dockerls.setup(server_opts)
      end
    end,
  },
}
