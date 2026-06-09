# Build script for TraceTool
# Usage:
#   .\build.ps1                        # Release Win32 
#   .\build.ps1 -Config Debug          # Debug Win32
#   .\build.ps1 -Platform Win64        # Release Win64 (default)
#   .\build.ps1 -Config Debug -Platform Win64

param(
    [string]$Config   = 'Release',
    [string]$Platform = 'Win64'
)

$validConfigs   = @('Release', 'Debug')
$validPlatforms = @('Win32', 'Win64')

if ($Config -notin $validConfigs) {
    Write-Error "Invalid -Config '$Config'. Allowed values: $($validConfigs -join ', ')."
    exit 1
}
if ($Platform -notin $validPlatforms) {
    Write-Error "Invalid -Platform '$Platform'. Allowed values: $($validPlatforms -join ', ')."
    exit 1
}

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
Write-Host "--- rsvars.bat content ---"
Get-Content $rsvars | Write-Host
Write-Host "--------------------------"
Write-Host ""

# Escape semicolons in the path list so cmd.exe doesn't split on them
$escapedPaths = $extraPaths -replace ';', '^^^;'

$msbuildArgs = "/t:Build /p:Config=$Config /p:Platform=$Platform /nologo /v:minimal"
if ($extraPaths) {
    $msbuildArgs += " /p:DCC_UnitSearchPath=`"$escapedPaths`""
}

$cmd = "`"$rsvars`" && msbuild `"$projectFile`" $msbuildArgs"

cmd.exe /c $cmd

if ($LASTEXITCODE -ne 0) {
    Write-Host ""
    Write-Error "Build FAILED (exit code $LASTEXITCODE)"
    exit $LASTEXITCODE
}

# --------------------------------------------------------------------------
# 5. Post-build: set file permissions (replaces afterBuild.bat / xcacls)
# --------------------------------------------------------------------------
$exe = Join-Path $PSScriptRoot '..\Viewer\TraceTool.exe'
Write-Host ""
Write-Host "Setting permissions on $exe"

$acl = New-Object System.Security.AccessControl.FileSecurity
$acl.SetAccessRuleProtection($true, $false)  # disable inheritance, remove inherited ACEs

foreach ($identity in @('SYSTEM', 'Everyone', 'Users', 'Administrators')) {
    $acl.AddAccessRule([System.Security.AccessControl.FileSystemAccessRule]::new(
        $identity,
        [System.Security.AccessControl.FileSystemRights]::FullControl,
        [System.Security.AccessControl.AccessControlType]::Allow
    ))
}

Set-Acl -Path $exe -AclObject $acl

$applied = (Get-Acl -Path $exe).Access
Write-Host ""
Write-Host "Permissions on $exe :"
$applied | Format-Table -AutoSize IdentityReference, FileSystemRights, AccessControlType, IsInherited

Write-Host "Build succeeded -> ..\Viewer\TraceTool.exe"

# --------------------------------------------------------------------------
# 6. Reset viewer position in TracetoolConfig.xml
# --------------------------------------------------------------------------
$configXml = Join-Path $PSScriptRoot '..\Viewer\TracetoolConfig.xml'
Write-Host ""
Write-Host "Resetting AppDisplay Left/Top to 200 in $configXml"

$content = Get-Content $configXml -Raw -Encoding UTF8
$content = $content -replace '(<Left\s+Value=")[^"]*(")', '${1}200${2}'
$content = $content -replace '(<Top\s+Value=")[^"]*(")', '${1}200${2}'
[System.IO.File]::WriteAllText($configXml, $content, [System.Text.UTF8Encoding]::new($false))

# --------------------------------------------------------------------------
# 7. Create Viewer ZIP for GitHub
# --------------------------------------------------------------------------
Add-Type -AssemblyName System.IO.Compression.FileSystem

$viewerDir  = Join-Path $PSScriptRoot '..\Viewer'
$githubDir  = Join-Path $PSScriptRoot '..\GithubFiles'
$zipName    = if ($Platform -eq 'Win64') { 'Viewer64.zip' } else { 'Viewer32.zip' }
$zipPath    = Join-Path $githubDir $zipName

Write-Host ""
Write-Host "Creating $zipName in GithubFiles\"

if (Test-Path $zipPath) { Remove-Item $zipPath -Force }

$zip = [System.IO.Compression.ZipFile]::Open($zipPath, [System.IO.Compression.ZipArchiveMode]::Create)

# Common flat files
$commonFiles = @(
    'clientaccesspolicy.xml',
    'crossdomain.xml',
    'TraceTool.exe',
    'tracetool.js',
    'tracetool.jmin.js',
    'TraceTool.map',
    'TraceTool.drc',
    'TracetoolConfig.xml'
)
foreach ($f in $commonFiles) {
    $src = Join-Path $viewerDir $f
    if (Test-Path $src) {
        [System.IO.Compression.ZipFileExtensions]::CreateEntryFromFile($zip, $src, $f) | Out-Null
    } else {
        Write-Warning "  Skipping missing file: $f"
    }
}

# Platform-specific DLLs
if ($Platform -eq 'Win64') {
    $platformFiles = @('DotNetWrapper64.dll', 'FastMM_FullDebugMode64.dll')
} else {
    $platformFiles = @('DotNetWrapper.dll', 'FastMM_FullDebugMode.dll')
}
foreach ($f in $platformFiles) {
    $src = Join-Path $viewerDir $f
    if (Test-Path $src) {
        [System.IO.Compression.ZipFileExtensions]::CreateEntryFromFile($zip, $src, $f) | Out-Null
    } else {
        Write-Warning "  Skipping missing file: $f"
    }
}

# WebSock\ folder (all files, preserving subfolder structure)
$webSockDir = (Resolve-Path (Join-Path $viewerDir 'WebSock')).Path.TrimEnd('\')
if (Test-Path $webSockDir) {
    Get-ChildItem $webSockDir -Recurse -File | ForEach-Object {
        $entryName = 'WebSock/' + $_.FullName.Substring($webSockDir.Length + 1).Replace('\', '/')
        [System.IO.Compression.ZipFileExtensions]::CreateEntryFromFile($zip, $_.FullName, $entryName) | Out-Null
    }
} else {
    Write-Warning "  WebSock folder not found — skipped."
}

$zip.Dispose()
Write-Host "$zipName created -> GithubFiles\"

# --------------------------------------------------------------------------
# 8. Package with Inno Setup 6
# --------------------------------------------------------------------------
$iscc = $null

$issoCandidates = @(
    'C:\Program Files (x86)\Inno Setup 6\ISCC.exe',
    'C:\Program Files\Inno Setup 6\ISCC.exe',
    "$env:LOCALAPPDATA\Programs\Inno Setup 6\ISCC.exe"
)
foreach ($c in $issoCandidates) {
    if (Test-Path $c) { $iscc = $c; break }
}

if (-not $iscc) {
    # Try registry
    $regPath = 'HKLM:\SOFTWARE\WOW6432Node\Microsoft\Windows\CurrentVersion\Uninstall'
    Get-ChildItem $regPath -ErrorAction SilentlyContinue | ForEach-Object {
        $disp = (Get-ItemProperty $_.PSPath -ErrorAction SilentlyContinue).DisplayName
        if ($disp -like 'Inno Setup 6*') {
            $loc = (Get-ItemProperty $_.PSPath -ErrorAction SilentlyContinue).InstallLocation
            if ($loc) {
                $candidate = Join-Path $loc 'ISCC.exe'
                if (Test-Path $candidate) { $iscc = $candidate }
            }
        }
    }
}

if (-not $iscc) {
    Write-Warning "Inno Setup 6 not found — skipping installer creation."
} else {
    if ($Platform -eq 'Win64') {
        $issFile = Join-Path $PSScriptRoot '..\Winget\tracetool64.iss'
    } else {
        $issFile = Join-Path $PSScriptRoot '..\Winget\tracetool32.iss'
    }

    Write-Host ""
    Write-Host "Running Inno Setup: $iscc"
    Write-Host "Script : $issFile"

    & $iscc $issFile
    if ($LASTEXITCODE -ne 0) {
        Write-Error "Inno Setup FAILED (exit code $LASTEXITCODE)"
        exit $LASTEXITCODE
    }
    Write-Host "Installer created -> ..\GithubFiles\"
}
