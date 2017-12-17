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
%content%
EOT;

    define('MIN_PHP_VERSION', 7);
    define('MIN_INT_SIZE', 8);
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
