# dotfiles

## PowerShell
### Getting Started
```
$packages = (
    'get-childitemcolor',
    'posh-git',
    'oh-my-posh'
)

$packages | % { Install-Module $_ }
```
