#!/bin/bash

# read version and trim trailing spaces
VERSION=$(<versioninfo.txt)
VERSION="${VERSION%"${VERSION##*[![:space:]]}"}"

# Patch the server version number
sed -E -i "" -e "s/define\\(\"VERSION\", *\"[0-9.]+\"\\);/define(\"VERSION\", \"$VERSION\");/" ../Server/php/version.php

# Remove any previous temporary file.
rm tmp.zip > /dev/null 2>&1

# Zip the server source code, excluding files that should not be installed on the server.
echo "Zipping server source code..."
pushd ../Server > /dev/null
zip -r -9 ../Prod/tmp.zip . -x 'storage/*' '*.htaccess' '*.DS_Store'
popd > /dev/null

# Generate the installer header.
cat << 'EOF' > nubo.php
<?php
    //-----------------------------------------------------------------------------
    // Nubo Server Installer
    // Copyright (c) 2017, Pascal Levy
    //
    // Permission is hereby granted, free of charge, to any person obtaining a copy
    // of this software and associated documentation files (the "Software"), to deal
    // in the Software without restriction, including without limitation the rights
    // to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    // copies of the Software, and to permit persons to whom the Software is
    // furnished to do so, subject to the following conditions:
    //
    // The above copyright notice and this permission notice shall be included in
    // all copies or substantial portions of the Software.
    //
    // THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    // IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    // FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    // AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    // LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    // OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
    // THE SOFTWARE.
    //-----------------------------------------------------------------------------

    $content = <<<EOT
EOF

# Inline the base64 encoded server source code.
base64 < tmp.zip | fold -w 128 >> nubo.php
rm tmp.zip

# Generate the installer body.
cat << 'EOF' >> nubo.php
EOT;

    define('MIN_PHP_VERSION', 7);
    define('MIN_INT_SIZE', 4);
    define('REQUIRED_EXTENSIONS', 'sqlite3 zip');
    define('TMP_FILE', 'tmp.zip');

    function checkCompatibility() {
        $retval = '';
        $version = explode('.', phpversion());
        if (!isset($version[0]) || intval($version[0]) < MIN_PHP_VERSION) {
            $retval .= "<li>PHP " . MIN_PHP_VERSION . " or higher is required</li>";
        }
        if (PHP_INT_SIZE < MIN_INT_SIZE) {
            $retval .= "<li>A 64-bit version of PHP is required</li>";
        }
        if (!is_writable('.')) {
            $retval .= "<li>Cannot write in the current folder</li>";
        }
        foreach (explode(' ', REQUIRED_EXTENSIONS) as $ext) {
            if (!extension_loaded($ext)) {
                $retval .= "<li>Missing PHP $ext extension</li>";
            }
        }
        return $retval;
    }

    function unzipFiles() {
        global $content;
        $retval = false;
        if (file_put_contents(TMP_FILE, base64_decode($content)) > 0) {
            $zip = new ZipArchive;
            if ($zip->open(TMP_FILE) === true) {
                if ($zip->extractTo('.')) {
                    $retval = true;
                }
                $zip->close();
            }
            @unlink(TMP_FILE);
        }
        return $retval;
    }
?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="fr" lang="fr">
<head>
	<title>Nubo Installer</title>
	<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
</head>
<body>
<?php
    if (($errors = checkCompatibility()) != '') {
        echo "<p>Installation failed because of the following issues:</p>";
        echo "<ul>$errors</ul>";
    } elseif (!unzipFiles()) {
        echo "<p>Unexpected error while unzipping archive.</p>";
    } else {
        echo "<p>Installation successful. Click <a href=\"index.php\">here</a> to configure and start your nubo server.</p>";
        if (strpos(__FILE__, 'install.php') !== false) {
            @unlink(__FILE__);
        }
    }
?>
</body>
</html>
EOF

echo "Done."
