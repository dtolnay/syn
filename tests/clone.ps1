# PowerShell v5.0 required
# Due to https://github.com/PowerShell/Microsoft.PowerShell.Archive/issues/32 this process will take 5 minutes

$Revision="521d78407471cb78e9bbf47160f6aa23047ac499"

cd "$PSScriptRoot"
if(-Not($(Get-Content "rust/COMMIT" -ErrorAction SilentlyContinue) -contains $Revision)) {
    Remove-Item rust -recurse -ErrorAction SilentlyContinue
    $download = New-TemporaryFile
    (New-Object System.Net.WebClient).DownloadFile("https://github.com/rust-lang/rust/archive/$Revision.zip", $download)
    $download | Rename-Item -NewName { $_ -replace 'tmp$', 'zip'} -PassThru | Expand-Archive -DestinationPath rusttemp
    Get-ChildItem rusttemp -Directory | ForEach-Object {
        Move-Item rusttemp\$_ -Destination "rust"
    }
    Remove-Item rusttemp
    echo $Revision > rust/COMMIT
}
