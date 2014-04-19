param([switch]$clean)

$JS_ZIP = "js185-1.0.0-win32.zip"
$JS_URL="http://www.atypical.net/apache"

$DISTDIR="${pwd}\.dists"
$JSLIBS="${pwd}\.libs\js"

#
# improved remove-item -recurse -force
# thanks to http://serverfault.com/questions/199921/powershell-remove-force
#
function rmrf($directory = $(throw "Required parameter missing")) {
    if ((test-path $directory) -and -not
            (gi $directory | ? { $_.PSIsContainer })) {
        throw ("rmrf called on non-directory.");
    }

    $finished = $false;
    $attemptsLeft = 3;

    do {
        if (test-path $directory) {
            rm $directory -recurse -force 2>&1 | out-null
        }
        if (test-path $directory) {
            Start-Sleep -Milliseconds 500
            $attemptsLeft = $attemptsLeft - 1
        } else {
            $finished = $true
        }
    } while (-not $finished -and $attemptsLeft -gt 0)

    if (test-path $directory) {
        throw ("Unable to fully remove directory " + $directory)
    }
}


#
# main
#
if ($clean -eq $true) {
    write-host "==> spidermonkey (clean)"
    rmrf($JSLIBS)
    rmrf($DISTDIR)
} else {
    write-host "==> spidermonkey (binary-download)"
    rmrf($JSLIBS)
    rmrf($DISTDIR)
    md $JSLIBS -ea silentlycontinue | out-null
    md $DISTDIR -ea silentlycontinue | out-null

    # download the zip
    $source = "${JS_URL}/${JS_ZIP}"
    $dest = "${DISTDIR}\${JS_ZIP}"
    if (-not (test-path $dest)) {
        write-host "==> Fetch ${JS_ZIP} to ${dest}"
        $wc = New-Object System.Net.WebClient
        $wc.DownloadFile($source, $dest)
    }

    # unpack the zip
    $shell = new-object -com shell.application
    $zip = $shell.NameSpace($dest)
    foreach($item in $zip.items())
    {
        $shell.Namespace($JSLIBS).copyhere($item)
    }
}
