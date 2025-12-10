-- Configure LazyVim to use ni-python-styleguide for Python linting and formatting
-- ni-python-styleguide should be installed in your project's virtual environment
-- For Poetry projects: poetry add --group dev ni-python-styleguide
return {
  -- Configure Python LSP
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        pyright = {},
      },
    },
  },

  -- Configure conform.nvim for formatting using ni-python-styleguide
  {
    "stevearc/conform.nvim",
    optional = true,
    opts = function(_, opts)
      -- Helper function to find ni-python-styleguide in the virtual environment
      local function find_ni_python_styleguide()
        -- Try poetry environment first
        local poetry_venv = vim.fn.trim(vim.fn.system("poetry env info --path 2>/dev/null"))
        if vim.v.shell_error == 0 and poetry_venv ~= "" then
          local poetry_cmd = poetry_venv .. "/bin/ni-python-styleguide"
          if vim.fn.executable(poetry_cmd) == 1 then
            return poetry_cmd
          end
        end

        -- Try activated virtual environment
        local venv = vim.env.VIRTUAL_ENV
        if venv then
          local venv_cmd = venv .. "/bin/ni-python-styleguide"
          if vim.fn.executable(venv_cmd) == 1 then
            return venv_cmd
          end
        end

        -- Fall back to global installation or poetry run
        if vim.fn.executable("ni-python-styleguide") == 1 then
          return "ni-python-styleguide"
        elseif vim.fn.executable("poetry") == 1 then
          return "poetry"
        end

        return nil
      end

      local ni_cmd = find_ni_python_styleguide()
      local is_poetry = ni_cmd == "poetry"

      opts.formatters_by_ft = opts.formatters_by_ft or {}
      opts.formatters_by_ft.python = { "ni_python_styleguide" }

      opts.formatters = opts.formatters or {}
      opts.formatters.ni_python_styleguide = {
        command = ni_cmd,
        args = function(self, ctx)
          local args = is_poetry and { "run", "ni-python-styleguide", "fix" } or { "fix" }
          table.insert(args, ctx.filename)
          return args
        end,
        stdin = false,
        cwd = require("conform.util").root_file({ "pyproject.toml", "setup.py" }),
      }
    end,
  },

  -- Configure nvim-lint for linting using ni-python-styleguide
  {
    "mfussenegger/nvim-lint",
    optional = true,
    opts = function(_, opts)
      -- Helper function to find ni-python-styleguide in the virtual environment
      local function find_ni_python_styleguide()
        -- Try poetry environment first
        local poetry_venv = vim.fn.trim(vim.fn.system("poetry env info --path 2>/dev/null"))
        if vim.v.shell_error == 0 and poetry_venv ~= "" then
          local poetry_cmd = poetry_venv .. "/bin/ni-python-styleguide"
          if vim.fn.executable(poetry_cmd) == 1 then
            return poetry_cmd, {}
          end
        end

        -- Try activated virtual environment
        local venv = vim.env.VIRTUAL_ENV
        if venv then
          local venv_cmd = venv .. "/bin/ni-python-styleguide"
          if vim.fn.executable(venv_cmd) == 1 then
            return venv_cmd, {}
          end
        end

        -- Fall back to global installation or poetry run
        if vim.fn.executable("ni-python-styleguide") == 1 then
          return "ni-python-styleguide", {}
        elseif vim.fn.executable("poetry") == 1 then
          return "poetry", { "run", "ni-python-styleguide" }
        end

        return nil, {}
      end

      local ni_cmd, prefix_args = find_ni_python_styleguide()

      opts.linters_by_ft = opts.linters_by_ft or {}
      opts.linters_by_ft.python = { "ni_python_styleguide" }

      opts.linters = opts.linters or {}
      opts.linters.ni_python_styleguide = {
        cmd = ni_cmd,
        args = vim.list_extend(vim.deepcopy(prefix_args), { "lint" }),
        stdin = false,
        stream = "stdout",
        ignore_exitcode = true,
        parser = require("lint.parser").from_errorformat("%f:%l:%c: %m", {
          source = "ni-python-styleguide",
        }),
      }
    end,
  },
}
