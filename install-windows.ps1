#Requires -Version 5.1
[CmdletBinding()]
param(
    [ValidateSet('git', 'nvim', 'starship', 'alacritty', 'lazygit', 'lazydocker', 'zellij')]
    [string[]]$Packages = @('git', 'nvim', 'starship', 'alacritty', 'lazygit', 'lazydocker', 'zellij'),
    [string]$Target = [Environment]::GetFolderPath('UserProfile'),
    [switch]$SkipToolInstall,
    [switch]$Preview
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
if ($env:OS -ne 'Windows_NT') { throw 'Run this installer on Windows.' }
$repo = $PSScriptRoot
$Target = [IO.Path]::GetFullPath($Target)
if ($Target -eq $repo -or $Target.StartsWith($repo + '\', [StringComparison]::OrdinalIgnoreCase)) {
    throw 'The target must be outside the dotfiles repository.'
}
$Packages = @($Packages | Select-Object -Unique)
if ($Packages.Count -eq 0) { throw 'Select at least one package.' }
$paths = @{
    git = '.gitconfig'; nvim = '.config\nvim'; starship = '.config\starship.toml'
    alacritty = '.config\alacritty'; lazygit = '.config\lazygit'
    lazydocker = '.config\lazydocker'; zellij = '.config\zellij'
}
foreach ($package in $Packages) {
    if (-not (Test-Path -LiteralPath (Join-Path "$repo\$package" $paths[$package]))) {
        throw "Missing package configuration: $package"
    }
}

function Update-ProcessPath {
    $env:Path = [Environment]::GetEnvironmentVariable('Path', 'Machine') + ';' +
        [Environment]::GetEnvironmentVariable('Path', 'User') + ';' + $env:Path
    $env:Path = (@($env:Path -split ';' | Where-Object { $_ } | Select-Object -Unique) -join ';')
}

function Add-UserPath([string]$Directory) {
    $current = [Environment]::GetEnvironmentVariable('Path', 'User')
    $entries = @($current -split ';' | Where-Object { $_ -and $_ -ne $Directory })
    $updated = (@($Directory) + $entries) -join ';'
    if ($current -ne $updated) {
        [Environment]::SetEnvironmentVariable('Path', $updated, 'User')
    }
    $env:Path = $Directory + ';' + $env:Path
}

function Find-UsableTool([string]$Name) {
    foreach ($command in @(Get-Command $Name -CommandType Application -All -ErrorAction SilentlyContinue)) {
        try {
            $null = & $command.Source --version 2>&1
            if ($LASTEXITCODE -eq 0) { return $command.Source }
        } catch { continue }
    }
    return $null
}

function Install-WingetPackage([string]$Id) {
    if (-not (Get-Command winget -ErrorAction SilentlyContinue)) {
        throw 'WinGet is missing. Install/update App Installer from Microsoft Store, then rerun this script.'
    }
    Write-Host "Installing $Id with WinGet..."
    $scope = @()
    if ($Id -eq 'Starship.Starship') { $scope = @('--scope', 'user') }
    & winget install --id $Id --exact --source winget --silent --accept-source-agreements --accept-package-agreements --disable-interactivity @scope | Out-Host
    # WinGet can report an already-installed package with no applicable upgrade.
    if ($LASTEXITCODE -ne 0 -and $LASTEXITCODE -ne -1978335189) {
        throw "WinGet failed to install $Id (exit $LASTEXITCODE). See the error above; system installers may require elevated PowerShell."
    }
    Update-ProcessPath
}

function Ensure-Tool([string]$Name, [string]$Id) {
    $tool = Find-UsableTool $Name
    if (-not $tool) {
        if ($SkipToolInstall -or $Preview) {
            Write-Warning "Missing or unusable tool: $Name (WinGet: $Id)"
            return $null
        }
        $makeBinary = $null
        if ($Name -eq 'make') { $makeBinary = Find-UsableTool 'mingw32-make' }
        if (-not $makeBinary) { Install-WingetPackage $Id }
        if ($Name -eq 'make') {
            $makeBinary = Find-UsableTool 'mingw32-make'
            if (-not $makeBinary) { throw 'WinLibs is installed, but its bundled GNU Make is not usable.' }
            $aliasDirectory = Join-Path $env:LOCALAPPDATA 'Programs\Dotfiles\bin'
            $alias = Join-Path $aliasDirectory 'make.exe'
            if (Get-Item -Force -LiteralPath $alias -ErrorAction SilentlyContinue) { throw "An unusable Make alias already exists: $alias" }
            New-Item -ItemType Directory -Force -Path $aliasDirectory | Out-Null
            $previousMSYS = $env:MSYS
            try {
                $env:MSYS = 'winsymlinks:nativestrict'
                & $perl -e 'symlink($ARGV[0],$ARGV[1]) or die $!' $makeBinary $alias
                if ($LASTEXITCODE -ne 0) { throw 'Could not create the native make.exe alias.' }
            } finally { $env:MSYS = $previousMSYS }
            Add-UserPath $aliasDirectory
        }
        # Some Python installers do not add their executable directory to PATH.
        if ($Name -eq 'python') {
            foreach ($key in @(Get-Item 'HKCU:\SOFTWARE\Python\PythonCore\*\InstallPath', 'HKLM:\SOFTWARE\Python\PythonCore\*\InstallPath' -ErrorAction SilentlyContinue)) {
                $directory = $key.GetValue('')
                if ($directory -and (Test-Path (Join-Path $directory 'python.exe'))) { Add-UserPath $directory.TrimEnd('\') }
            }
        }
        $tool = Find-UsableTool $Name
        if (-not $tool) { throw "$Id is installed, but $Name is not usable. Check PATH and rerun this script." }
    }
    if (-not $Preview) {
        # Put working executables ahead of broken shims for future terminals too.
        Add-UserPath (Split-Path $tool -Parent)
    }
    Write-Host "Found ${Name}: $tool"
    return $tool
}

Update-ProcessPath
$git = Ensure-Tool 'git' 'Git.Git'
function Find-GitPerl {
    $roots = @()
    if ($git) { $roots += Split-Path (Split-Path $git -Parent) -Parent }
    foreach ($key in @('HKCU:\SOFTWARE\GitForWindows', 'HKLM:\SOFTWARE\GitForWindows')) {
        $item = Get-ItemProperty $key -ErrorAction SilentlyContinue
        if ($item -and $item.PSObject.Properties['InstallPath']) { $roots += $item.InstallPath }
    }
    $roots += @("$env:ProgramFiles\Git", "$env:LOCALAPPDATA\Programs\Git")
    foreach ($root in $roots) {
        $candidate = Join-Path $root 'usr\bin\perl.exe'
        if (Test-Path -LiteralPath $candidate) { return $candidate }
    }
    return $null
}
$perl = Find-GitPerl
if (-not $perl -and -not ($SkipToolInstall -or $Preview)) {
    Install-WingetPackage 'Git.Git'
    $perl = Find-GitPerl
}
if (-not $perl) { throw 'Git for Windows with bundled Perl is required for GNU Stow. Rerun without -SkipToolInstall/-Preview.' }

# Use physical drive paths: cygpath's /tmp alias breaks Stow's ownership checks.
function ConvertTo-StowPath([string]$Path) {
    $posix = $Path.Replace('\', '/')
    if ($posix -match '^([a-zA-Z]):/') { return '/' + $Matches[1].ToLowerInvariant() + $posix.Substring(2) }
    return $posix
}
$stowDir = ConvertTo-StowPath $repo
$stowTarget = ConvertTo-StowPath $Target

$stowRoot = Join-Path $env:LOCALAPPDATA 'Programs\GNUStow'
$stowRelease = Join-Path $stowRoot 'Stow-v2.4.1'
$stow = Join-Path $stowRoot 'stow.cmd'
$stowScript = Join-Path $stowRelease 'bin\stow'
$stowLibrary = Join-Path $stowRelease 'lib'
if (-not (Test-Path -LiteralPath $stowScript)) {
    if ($Preview -or $SkipToolInstall) { throw 'GNU Stow is missing. Rerun without -SkipToolInstall/-Preview to install it.' }
    New-Item -ItemType Directory -Force -Path $stowRoot | Out-Null
    $archive = Join-Path $stowRoot 'Stow-v2.4.1.tar.gz'
    try {
        [Net.ServicePointManager]::SecurityProtocol = [Net.ServicePointManager]::SecurityProtocol -bor [Net.SecurityProtocolType]::Tls12
        Invoke-WebRequest -UseBasicParsing -Uri 'https://cpan.metacpan.org/authors/id/A/AS/ASPIERS/Stow-v2.4.1.tar.gz' -OutFile $archive
        $expected = '2918947348c5bb58a379e9761b99c3d07976ce7a556766d9dbd32d4894ec1f52'
        $sha = [Security.Cryptography.SHA256]::Create()
        $stream = $null
        try {
            $stream = [IO.File]::OpenRead($archive)
            $actual = [BitConverter]::ToString($sha.ComputeHash($stream)).Replace('-', '').ToLowerInvariant()
        } finally {
            if ($stream) { $stream.Dispose() }
            $sha.Dispose()
        }
        if ($actual -ne $expected) { throw 'GNU Stow archive checksum mismatch.' }
        # Git's tar supports Unicode paths, but needs its bundled gzip on PATH.
        $gitBin = Split-Path $perl -Parent
        $previousPath = $env:Path
        try {
            $env:Path = $gitBin + ';' + $env:Path
            & (Join-Path $gitBin 'tar.exe') --force-local -xzf $archive -C $stowRoot
            if ($LASTEXITCODE -ne 0) { throw 'GNU Stow extraction failed.' }
        } finally { $env:Path = $previousPath }
    } finally {
        if (Test-Path -LiteralPath $archive) { Remove-Item -LiteralPath $archive -Force }
    }
}
$version = & $perl -I $stowLibrary $stowScript --version
if ($LASTEXITCODE -ne 0 -or ($version -join "`n") -notmatch 'GNU Stow') { throw 'GNU Stow is not usable.' }
Write-Host ($version -join "`n")
if (-not $Preview) {
    $launcherPerl = $perl
    foreach ($variable in @('LOCALAPPDATA', 'ProgramFiles', 'ProgramFiles(x86)')) {
        $base = [Environment]::GetEnvironmentVariable($variable)
        if ($base -and $perl.StartsWith($base + '\', [StringComparison]::OrdinalIgnoreCase)) {
            $launcherPerl = '%' + $variable + '%' + $perl.Substring($base.Length)
            break
        }
    }
    $launcher = @"
@echo off
setlocal
set "MSYS=winsymlinks:nativestrict"
"$launcherPerl" -I "%~dp0Stow-v2.4.1\lib" "%~dp0Stow-v2.4.1\bin\stow" %*
exit /b %ERRORLEVEL%
"@
    Set-Content -LiteralPath $stow -Value $launcher -Encoding Default
    Add-UserPath $stowRoot
}

# Probe the same Perl symlink implementation Stow uses before moving any configs.
if (-not $Preview) {
    $probe = Join-Path ([IO.Path]::GetTempPath()) ('dotfiles-symlink-' + [guid]::NewGuid().ToString('N'))
    $source = Join-Path $probe 'source'
    $link = Join-Path $probe 'link'
    $previousMSYS = $env:MSYS
    try {
        New-Item -ItemType Directory -Path $source -Force | Out-Null
        $env:MSYS = 'winsymlinks:nativestrict'
        & $perl -e 'symlink($ARGV[0],$ARGV[1]) or die $!' $source $link
        if ($LASTEXITCODE -ne 0 -or (Get-Item -Force -LiteralPath $link).LinkType -ne 'SymbolicLink') {
            throw 'Native symlinks failed. Enable Windows Developer Mode or run PowerShell as administrator.'
        }
    } finally {
        $env:MSYS = $previousMSYS
        if (Test-Path -LiteralPath $link) { [IO.Directory]::Delete($link) }
        if (Test-Path -LiteralPath $probe) { Remove-Item -LiteralPath $probe -Recurse -Force }
    }
}

$tools = @{}
if ($Packages -contains 'git' -or $Packages -contains 'lazygit') {
    $tools.delta = Ensure-Tool 'delta' 'dandavison.delta'
    $tools.difft = Ensure-Tool 'difft' 'Wilfred.difftastic'
}
if ($Packages -contains 'nvim') {
    foreach ($spec in @(
        @('nvim', 'Neovim.Neovim'), @('python', 'Python.Python.3.13'),
        @('rg', 'BurntSushi.ripgrep.MSVC'), @('fd', 'sharkdp.fd'),
        @('gcc', 'BrechtSanders.WinLibs.POSIX.UCRT'), @('make', 'BrechtSanders.WinLibs.POSIX.UCRT')
    )) { $tools[$spec[0]] = Ensure-Tool $spec[0] $spec[1] }
}
foreach ($spec in @(
    @('starship', 'Starship.Starship'), @('alacritty', 'Alacritty.Alacritty'),
    @('lazygit', 'JesseDuffield.lazygit'), @('lazydocker', 'JesseDuffield.Lazydocker')
)) {
    if ($Packages -contains $spec[0]) { $tools[$spec[0]] = Ensure-Tool $spec[0] $spec[1] }
}
if ($Packages -contains 'zellij') { Write-Warning 'Zellij is not a native Windows application; only its configuration is linked. Use it in WSL.' }

$config = Join-Path $Target '.config'
$configItem = Get-Item -Force -LiteralPath $config -ErrorAction SilentlyContinue
if ($configItem -and (-not $configItem.PSIsContainer -or $configItem.LinkType)) {
    throw "$config must be a real directory, not a file or link. No configurations were moved."
}
$conflicts = @()
foreach ($package in $Packages) {
    $destination = Join-Path $Target $paths[$package]
    $source = Join-Path "$repo\$package" $paths[$package]
    $existing = Get-Item -Force -LiteralPath $destination -ErrorAction SilentlyContinue
    if ($existing) {
        $matches = $false
        if ($existing.LinkType -eq 'SymbolicLink' -and $existing.Target) {
            $linked = $existing.Target[0]
            if (-not [IO.Path]::IsPathRooted($linked)) { $linked = Join-Path (Split-Path $destination -Parent) $linked }
            $matches = [IO.Path]::GetFullPath($linked) -eq [IO.Path]::GetFullPath($source)
        }
        if (-not $matches) { $conflicts += $destination }
    }
}
if ($Preview) {
    foreach ($conflict in $conflicts) { Write-Warning "Would back up: $conflict" }
    $previousMSYS = $env:MSYS
    try {
        $env:MSYS = 'winsymlinks:nativestrict'
        & $perl -I $stowLibrary $stowScript --dir=$stowDir --target=$stowTarget --simulate --verbose @Packages
        if ($LASTEXITCODE -ne 0) { throw 'Stow preview found conflicts; the normal run backs up the listed configurations.' }
    } finally { $env:MSYS = $previousMSYS }
    return
}

New-Item -ItemType Directory -Force -Path $config | Out-Null
$backups = @{}
try {
    foreach ($conflict in $conflicts) {
        $backup = $conflict + '.pre-stow-' + (Get-Date -Format 'yyyyMMdd-HHmmss-fff') + '-' + [guid]::NewGuid().ToString('N').Substring(0, 8)
        Move-Item -LiteralPath $conflict -Destination $backup
        $backups[$conflict] = $backup
        Write-Host "Backed up: $backup"
    }
    & $stow --dir=$stowDir --target=$stowTarget --simulate --verbose @Packages
    if ($LASTEXITCODE -ne 0) { throw 'Stow preview failed.' }
    & $stow --dir=$stowDir --target=$stowTarget --verbose @Packages
    if ($LASTEXITCODE -ne 0) { throw 'Stow failed to apply configurations.' }
} catch {
    # Never overwrite a link or file created during a partially completed operation.
    foreach ($original in $backups.Keys) {
        if (-not (Get-Item -Force -LiteralPath $original -ErrorAction SilentlyContinue)) {
            Move-Item -LiteralPath $backups[$original] -Destination $original
        }
    }
    throw
}
foreach ($package in $Packages) {
    $destination = Join-Path $Target $paths[$package]
    $item = Get-Item -Force -LiteralPath $destination
    if ($item.LinkType -ne 'SymbolicLink' -or -not (Test-Path -LiteralPath $destination)) {
        throw "Expected a working native symlink: $destination"
    }
    Write-Host "Verified link: $destination"
}
[Environment]::SetEnvironmentVariable('XDG_CONFIG_HOME', $config, 'User')
$env:XDG_CONFIG_HOME = $config
if ($tools.ContainsKey('nvim') -and $tools.nvim) {
    & $tools.nvim --clean --headless -c 'lua print(vim.fn.stdpath([[config]]))' -c qa
    if ($LASTEXITCODE -ne 0) { throw 'Neovim config-path check failed.' }
}
if ($tools.ContainsKey('lazygit') -and $tools.lazygit) {
    & $tools.lazygit --print-config-dir
    if ($LASTEXITCODE -ne 0) { throw 'LazyGit config-path check failed.' }
}
if ($backups.ContainsKey((Join-Path $Target '.gitconfig'))) {
    Write-Warning 'Git now uses the repository identity and settings. Previous credential helpers are in the backup, not the active configuration.'
}
if ($Packages -contains 'alacritty') { Write-Warning 'Alacritty imports an Omarchy theme and uses CaskaydiaMono Nerd Font; those resources are not installed by this script.' }
if ($Packages -contains 'lazygit') { Write-Warning 'LazyGit has a Ghostty/Zsh shortcut that is not portable to native Windows.' }
if ($Packages -contains 'lazydocker') { Write-Warning 'LazyDocker requires a separately configured Docker engine.' }
Write-Host "Applied: $($Packages -join ', '). Open a new terminal to pick up PATH and XDG_CONFIG_HOME."
