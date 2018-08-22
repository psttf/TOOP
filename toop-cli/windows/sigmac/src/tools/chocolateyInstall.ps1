$packageName = 'sigmac'
$installerType = 'exe'
$url = './sigmac.exe' # download url
$silentArgs = 'q'
$validExitCodes = @(0)

Install-ChocolateyPackage "$packageName" "$installerType" "$silentArgs" "$url"  -validExitCodes $validExitCodes
