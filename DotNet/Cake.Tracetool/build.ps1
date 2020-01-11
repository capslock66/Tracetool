
[CmdletBinding()]
Param(
    [string]$Script = "build.cake",
    [string]$Target,
    [string]$Configuration,
    [ValidateSet("Quiet", "Minimal", "Normal", "Verbose", "Diagnostic")]
    [string]$Verbosity = "Diagnostic",
    
    [switch]$ShowDescription,
    [Alias("WhatIf", "Noop")]
    [switch]$DryRun,
    [switch]$SkipToolPackageRestore,
    [Parameter(Position=0,Mandatory=$false,ValueFromRemainingArguments=$true)]
    [string[]]$ScriptArgs
)

$CAKE_FOLDER = $env:CAKE_FOLDER
if(!$CAKE_FOLDER){
    $CAKE_FOLDER = "C:\github\Cake"   # Split-Path $MyInvocation.MyCommand.Path -Parent
    #Throw "Please Define a CAKE_FOLDER (C:\sources\Cake) environment var on local computer or (d:\Data\deployment\Cake) on build server"
}

$CAKE_TOOLS_DIR = Join-Path $CAKE_FOLDER "tools"
$env:NUGET_EXE = Join-Path $CAKE_TOOLS_DIR "nuget.exe"

$CAKE_EXE = Join-Path $CAKE_TOOLS_DIR "Cake/Cake.exe"
$CAKE_EXE_INVOCATION =  "`"$CAKE_EXE`""

 # Build an array (not a string) of Cake arguments to be joined later
$cakeArguments = @()

if ($Script) { $cakeArguments += "`"$Script`"" }
if ($Target) { $cakeArguments += "-target=`"$Target`"" }
if ($Configuration) { $cakeArguments += "-configuration=$Configuration" }
if ($Verbosity) { $cakeArguments += "-verbosity=$Verbosity" }
if ($ShowDescription) { $cakeArguments += "-showdescription" }
if ($DryRun) { $cakeArguments += "-dryrun" }
# prefer argument to environment var
$cakeArguments += "--paths_tools=`"$($CAKE_TOOLS_DIR)`""
$cakeArguments += "--paths_addins=`"$(Join-Path $CAKE_TOOLS_DIR "Addins")`""
$cakeArguments += "--paths_modules=`"$(Join-Path $CAKE_TOOLS_DIR "Module")`""

$cakeArguments += $ScriptArgs

Invoke-Expression "& $CAKE_EXE_INVOCATION $($cakeArguments -join " ")"
exit $LASTEXITCODE
