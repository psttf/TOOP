try {
    Remove-Item -Path "C:\Program Files\sigmac\sigmac.jar"
    Remove-Item -Path "C:\Program Files\sigmac\bin\sigmac.bat"
    Remove-Item -Path "C:\Program Files\sigmac\bin\"
    Remove-Item -Path "C:\Program Files\sigmac\"
} catch {
    exit 1
}

$newPath = $env:path
$strArr = $newPath.Split(";")
$resStr = ""
FOR ([int]$i = 0; $i -lt $strArr.Length-1; $i++){
    IF (-not ($strArr[$i] -eq "C:\Program Files\sigmac\bin")) {
        $resStr += $strArr[$i]
        $resStr += ";"
    }
}

setx PATH $resStr -m
