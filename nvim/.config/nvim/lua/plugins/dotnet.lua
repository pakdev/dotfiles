-- Configure LazyVim for .NET/C# development with easy-dotnet.nvim
-- This plugin includes:
--   - Roslyn LSP (Microsoft's official C# language server)
--   - netcoredbg debugger
--   - Test runner, project management, NuGet tools, EF commands, and more
-- Install: dotnet tool install -g csharpier
-- Install: dotnet tool install -g EasyDotnet
return {
  {
    "GustavEikaas/easy-dotnet.nvim",
    dependencies = { "nvim-lua/plenary.nvim", "nvim-telescope/telescope.nvim" },
    ft = { "cs", "fsharp" },
    config = function()
      local dotnet = require("easy-dotnet")
      local dotnet_path = vim.fn.exepath("dotnet")
      if dotnet_path ~= "" then
        vim.env.DOTNET_HOST_PATH = dotnet_path
        vim.env.DOTNET_ROOT = vim.fn.fnamemodify(dotnet_path, ":h")
        vim.env.DOTNET_MULTILEVEL_LOOKUP = "0"
      end
      dotnet.setup({
        lsp = {
          enabled = true, -- Enable builtin roslyn lsp
          roslynator_enabled = true, -- Automatically enable roslynator analyzer
          easy_dotnet_analyzer_enabled = true, -- Enable roslyn analyzer from easy-dotnet-server
          auto_refresh_codelens = true,
          analyzer_assemblies = {}, -- Any additional roslyn analyzers
          config = {
            settings = {
              ["csharp|inlay_hints"] = {
                csharp_enable_inlay_hints_for_implicit_object_creation = true,
                csharp_enable_inlay_hints_for_implicit_variable_types = true,
                csharp_enable_inlay_hints_for_lambda_parameter_types = true,
                csharp_enable_inlay_hints_for_types = true,
                dotnet_enable_inlay_hints_for_indexer_parameters = true,
                dotnet_enable_inlay_hints_for_literal_parameters = true,
                dotnet_enable_inlay_hints_for_object_creation_parameters = true,
                dotnet_enable_inlay_hints_for_other_parameters = true,
                dotnet_enable_inlay_hints_for_parameters = true,
                dotnet_suppress_inlay_hints_for_parameters_that_differ_only_by_suffix = true,
                dotnet_suppress_inlay_hints_for_parameters_that_match_argument_name = true,
                dotnet_suppress_inlay_hints_for_parameters_that_match_method_intent = true,
              },
              ["csharp|code_lens"] = {
                dotnet_enable_references_code_lens = true,
              },
            },
          },
        },
        debugger = {
          bin_path = nil, -- Uses built-in netcoredbg
          apply_value_converters = true,
          auto_register_dap = true,
        },
        test_runner = {
          viewmode = "float", -- "split" | "vsplit" | "float" | "buf"
          enable_buffer_test_execution = true,
          noBuild = true,
        },
        csproj_mappings = true,
        fsproj_mappings = true,
        auto_bootstrap_namespace = {
          type = "block_scoped", -- "block_scoped" | "file_scoped"
          enabled = true,
        },
      })

      -- Useful keymaps
      vim.keymap.set("n", "<leader>dr", dotnet.run, { desc = "Dotnet run" })
      vim.keymap.set("n", "<leader>dR", dotnet.run_default, { desc = "Dotnet run default" })
      vim.keymap.set("n", "<leader>db", dotnet.build, { desc = "Dotnet build" })
      vim.keymap.set("n", "<leader>dt", dotnet.testrunner, { desc = "Dotnet test runner" })

      local conform_ok, conform = pcall(require, "conform")
      if conform_ok then
        vim.keymap.set("n", "<leader>dc", function()
          conform.format({ formatters = { "csharpier" } })
        end, { desc = "Format (csharpier)" })
        vim.keymap.set("n", "<leader>dd", function()
          conform.format({ formatters = { "dotnet_format" } })
        end, { desc = "Format (dotnet format)" })
      end
    end,
  },

  -- Configure conform.nvim for formatting using csharpier
  {
    "stevearc/conform.nvim",
    optional = true,
    opts = function(_, opts)
      -- Helper function to find the best formatter
      local function find_dotnet_formatter()
        -- Try csharpier first (preferred formatter)
        if vim.fn.executable("dotnet-csharpier") == 1 then
          return "csharpier"
        elseif vim.fn.executable("csharpier") == 1 then
          return "csharpier"
        -- Fall back to dotnet-format
        elseif vim.fn.executable("dotnet") == 1 then
          return "dotnet_format"
        end
        return nil
      end

      local formatter = find_dotnet_formatter()

      vim.schedule(function()
        vim.notify(
          "Conform C# formatter: " .. (formatter or "none"),
          vim.log.levels.INFO
        )
      end)

      opts.formatters_by_ft = opts.formatters_by_ft or {}
      opts.formatters_by_ft.cs = { formatter or "csharpier" }

      opts.formatters = opts.formatters or {}
      opts.timeout_ms = 120000
      opts.default_format_opts = vim.tbl_extend(
        "force",
        opts.default_format_opts or {},
        { timeout_ms = 120000 }
      )

      -- CSharpier formatter configuration
      opts.formatters.csharpier = {
        command = vim.fn.executable("dotnet-csharpier") == 1 and "dotnet-csharpier"
          or "csharpier",
        args = { "--write-stdout" },
        stdin = true,
      }

      -- dotnet-format configuration (fallback)
      opts.formatters.dotnet_format = {
        command = "dotnet",
        timeout_ms = 120000,
        args = function(self, ctx)
          return {
            "format",
            "--include",
            ctx.filename,
            vim.fn.fnamemodify(ctx.filename, ":h"),
          }
        end,
        stdin = false,
        cwd = require("conform.util").root_file({
          "*.sln",
          "*.csproj",
          ".editorconfig",
        }),
      }
    end,
  },

  -- Add treesitter support for C#
  {
    "nvim-treesitter/nvim-treesitter",
    opts = function(_, opts)
      opts.ensure_installed = opts.ensure_installed or {}
      vim.list_extend(opts.ensure_installed, { "c_sharp" })
    end,
  },
}
