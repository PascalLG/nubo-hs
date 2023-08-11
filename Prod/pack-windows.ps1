# read version and trim trailing spaces
$VERSION = (Get-Content versioninfo.txt -Raw).trim()

Write-Host
Write-Host "================================================"
Write-Host "=== Packaging nubo client v$VERSION for Windows ==="
Write-Host "================================================"
Write-Host
Write-Host "Compiling..."
Write-Host

# patch the cabal file, build the client and retrieve the PATH to the binary
Push-Location ..\Client
(Get-Content nubo.cabal) -creplace '^\s*version:\s*[0-9.]+\s*$', "version:             $VERSION" | Set-Content -Path nubo.cabal
&stack build
$BUILDPATH=(&stack path --local-install-root) + '\bin\nubo-exe.exe'
Pop-Location

# Prepare the package content
Write-Host
Write-Host "Packaging..."
Write-Host

# patch the Wix file (regex must be case-sensitive to avoid replacing the wrong attribute!)
(Get-Content nubo.wxs) -creplace " Version='[0-9.]+' ", " Version='$VERSION' " | Set-Content -Path nubo.wxs

# prepare content
[void](New-Item -Name "tmp" -ItemType "directory" -Force)
[void](Copy-Item "$BUILDPATH" -Destination "tmp\nubo.exe" -Force)
[void](Copy-Item ([System.Environment]::SystemDirectory + "\sqlite3.dll") -Destination "tmp\sqlite3.dll" -Force)

# packaging
&candle nubo.wxs
&light nubo.wixobj

# cleanup
if (Test-Path nubo.wixobj) { Remove-Item nubo.wixobj }
if (Test-Path nubo.wixpdb) { Remove-Item nubo.wixpdb }
if (Test-Path tmp) { Remove-Item -LiteralPath "tmp" -Force -Recurse }

Write-Host
Write-Host "OK."
