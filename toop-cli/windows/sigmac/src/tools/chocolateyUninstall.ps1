try {
    Remove-Item -Path "C:\Program Files\sigmac\sigmac.jar"
    Remove-Item -Path "C:\Program Files\sigmac\bin\sigmac.bat"
    Remove-Item -Path "C:\Program Files\sigmac\bin\"
    Remove-Item -Path "C:\Program Files\sigmac\"
} catch {
    exit 1
}

$path = [System.Environment]::GetEnvironmentVariable( 'PATH', 'Machine' ) 
$path = ($path.Split(';') | Where-Object { $_ -ne 'C:\Program Files\sigmac\bin' }) -join ';' 
[System.Environment]::SetEnvironmentVariable( 'PATH', $path, 'Machine' )
