#!/usr/bin/php
<?php
    define('SOURCE_PATH', '../Server');
    define('TMP_FILE', 'tmp.zip');

    function zipFolder($folder, $zip, $prefix) {
        $handle = opendir($folder);
        while (($f = readdir($handle)) !== false) { 
            if ($f[0] != '.' && $f != 'storage') {
                $path = "$folder/$f";
                $local = substr($path, $prefix); 
                if (is_file($path)) { 
                    $zip->addFile($path, $local);
                } else if (is_dir($path)) {
                    $zip->addEmptyDir($local);
                    zipFolder($path, $zip, $prefix); 
                }
            }
        }
        closedir($handle); 
    }
    
    @unlink(TMP_FILE);
    $z = new ZipArchive();
    $z->open(TMP_FILE, ZIPARCHIVE::CREATE);
    zipFolder(SOURCE_PATH, $z, strlen(SOURCE_PATH));
    $z->close();
    $content = chunk_split(base64_encode(file_get_contents(TMP_FILE)), 128, "\n");
    @unlink(TMP_FILE);

    $install = str_replace('%content%', rtrim($content), file_get_contents('template.php'));
    file_put_contents('install.php', $install);
?>
