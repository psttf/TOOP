try {
    Get-Command java | Select-Object Version
} catch {
    Write-Host ″You have java to be installed.″
    exit 1
}
New-Item -ItemType directory -Path "C:\Program Files\sigmac"
New-Item -ItemType directory -Path "C:\Program Files\sigmac\bin"
Copy-Item -Path "C:\ProgramData\chocolatey\lib\sigmac\tools\sigmac.jar"  -Force -Destination "C:\Program Files\sigmac\sigmac.jar"
Copy-Item -Path "C:\ProgramData\chocolatey\lib\sigmac\tools\sigmac.bat" -Force -Destination "C:\Program Files\sigmac\bin\sigmac.bat"
setx PATH "$env:path;C:\Program Files\sigmac\bin" -m

