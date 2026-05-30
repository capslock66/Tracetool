# Build script for TraceTool
# Usage:
#   .\build.ps1                        # Release Win32 
#   .\build.ps1 -Config Debug          # Debug Win32
#   .\build.ps1 -Platform Win64        # Release Win64 (default)
#   .\build.ps1 -Config Debug -Platform Win64

param(
    [ValidateSet('Release','Debug')]
    [string]$Config   = 'Release',

    [ValidateSet('Win32','Win64')]
    [string]$Platform = 'Win64'
)

# --------------------------------------------------------------------------
# 1. Locate rsvars.bat (sets BDS, BDSCOMMONDIR, etc. for MSBuild)
# --------------------------------------------------------------------------
$rsvars = $null

# Prefer the BDS env var if already set
if ($env:BDS -and (Test-Path "$env:BDS\bin\rsvars.bat")) {
    $rsvars = "$env:BDS\bin\rsvars.bat"
}

# Otherwise probe known install locations (newest first)
if (-not $rsvars) {
    $candidates = @(
        'C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat',  # RAD Studio 12 Athens
        'C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat',  # RAD Studio 12
        'C:\Program Files (x86)\Embarcadero\Studio\22.0\bin\rsvars.bat',  # RAD Studio 11
        'C:\Program Files (x86)\Embarcadero\Studio\21.0\bin\rsvars.bat',  # RAD Studio 10.4
        'C:\Program Files (x86)\Embarcadero\Studio\20.0\bin\rsvars.bat'   # RAD Studio 10.3
    )
    foreach ($c in $candidates) {
        if (Test-Path $c) { $rsvars = $c; break }
    }
}

if (-not $rsvars) {
    Write-Error "rsvars.bat not found. Set the BDS environment variable to your RAD Studio installation root."
    exit 1
}

# --------------------------------------------------------------------------
# 2. Determine the BDS registry version from rsvars.bat content
#    rsvars.bat contains a line like: set BDS=C:\...\Studio\37.0
# --------------------------------------------------------------------------
$bdsVersion = $null
foreach ($line in (Get-Content $rsvars)) {
    if ($line -match 'set\s+BDS\s*=.*\\(\d+\.\d+)\s*$') {
        $bdsVersion = $Matches[1]
        break
    }
}

# --------------------------------------------------------------------------
# 3. Read IDE library search paths from the registry and pass to MSBuild
#    The IDE stores user-added paths (madExcept, JCL, VirtualTrees, etc.)
#    under HKCU\Software\Embarcadero\BDS\<ver>\Library\<Platform>
# --------------------------------------------------------------------------
$extraPaths = ''
if ($bdsVersion) {
    $regKey = "HKCU:\Software\Embarcadero\BDS\$bdsVersion\Library\$Platform"
    $regVal = (Get-ItemProperty $regKey -ErrorAction SilentlyContinue).'Search Path'
    if ($regVal) {
        $extraPaths = $regVal
        Write-Host "Library paths read from registry (BDS $bdsVersion / $Platform)"
    }
}

if (-not $extraPaths) {
    Write-Warning "Could not read library paths from registry. Third-party units (madExcept, JCL…) may not be found."
}

# --------------------------------------------------------------------------
# 4. Build
# --------------------------------------------------------------------------
$projectFile = Join-Path $PSScriptRoot 'TraceTool.dproj'

Write-Host "Config   : $Config"
Write-Host "Platform : $Platform"
Write-Host "rsvars   : $rsvars"
Write-Host "BDS ver  : $bdsVersion"
Write-Host "env:BDS  : $env:BDS"
Write-Host ""

# Escape semicolons in the path list so cmd.exe doesn't split on them
$escapedPaths = $extraPaths -replace ';', '^^^;'

$msbuildArgs = "/t:Build /p:Config=$Config /p:Platform=$Platform /nologo /v:minimal"
if ($extraPaths) {
    $msbuildArgs += " /p:DCC_UnitSearchPath=`"$escapedPaths`""
}

$cmd = "`"$rsvars`" && msbuild `"$projectFile`" $msbuildArgs"

cmd.exe /c $cmd

if ($LASTEXITCODE -eq 0) {
    Write-Host ""
    Write-Host "Build succeeded -> ..\Viewer\TraceTool.exe"
} else {
    Write-Host ""
    Write-Error "Build FAILED (exit code $LASTEXITCODE)"
    exit $LASTEXITCODE
}
