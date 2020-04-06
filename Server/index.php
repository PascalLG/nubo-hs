<?php
    //-----------------------------------------------------------------------------
    // Nubo Server Application
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

    ini_set('error_reporting', E_ALL);
    ini_set('display_errors', 1);
    ini_set('log_errors', 1);

    require_once('php/sqlite.php');
    require_once('php/exception.php');
    require_once("php/helpers.php");
    require_once("php/version.php");
	session_start();

    if (strtolower($_SERVER['REQUEST_METHOD']) == 'post') {
        if (isset($_POST['password1']) && isset($_POST['password2']) && !file_exists($glo_dbname)) {
        	$pwd1 = trim($_POST['password1']);
        	$pwd2 = trim($_POST['password2']);
            if ($pwd1 != $pwd2) {
                $error = 'Passwords differ.';
            } else if (strlen($pwd1) < 4) {
                $error = 'Your password must be at least 4-character long.';
            } else {
                @mkdir($glo_dirname);
                @chmod($glo_dirname, 0770);
                @file_put_contents("./$glo_dirname/.htaccess", "deny from all\n");
                try {
                    $db = new NuboDatabase();
                    $db->execute('BEGIN TRANSACTION');
                    $db->execute('CREATE TABLE tbl_config (config_id INTEGER PRIMARY KEY AUTOINCREMENT, key TEXT NOT NULL, value BLOB)');
                    $db->execute('CREATE TABLE tbl_computer (computer_id INTEGER PRIMARY KEY AUTOINCREMENT, hostname TEXT, computer TEXT NOT NULL, selector TEXT, validator TEXT, atime INTEGER NOT NULL, lock INTEGER)');
                    $db->execute('CREATE TABLE tbl_file (file_id INTEGER PRIMARY KEY AUTOINCREMENT, filename TEXT NOT NULL, hash TEXT NOT NULL, mtime INTEGER NOT NULL)');
                    $db->execute('INSERT INTO tbl_config (key, value) VALUES ("password", :password)', ['password/blob' => password_hash($pwd1, PASSWORD_DEFAULT)]);
                    $db->execute('INSERT INTO tbl_config (key, value) VALUES ("salt", :salt)', ['salt/blob' => random_bytes(32)]);
                    $db->execute('INSERT INTO tbl_config (key, value) VALUES ("ctime", :ctime)', ['ctime/blob' => pack('L', time() - 946684800)]);
                    $db->execute('COMMIT TRANSACTION');
                } catch (NuboException $e) {
                    $db->close();
                    @unlink($glo_dbname);
                    $error = 'An error occured during creation of the database.';
                }
            }
        } else if (isset($_POST['password']) && !isset($_SESSION['login'])) {
            try {
                $db = new NuboDatabase();
                $pwd = trim($_POST['password']);
                $result = $db->select('SELECT value FROM tbl_config WHERE key="password" LIMIT 1');
                if (($row = reset($result)) !== false && password_verify($pwd, $row['value'])) {
                    $_SESSION['login'] = true;
                } else {
                    throw new NuboException(ERROR_FORBIDDEN);
                }
            } catch (NuboException $e) {
                $error = 'Invalid credentials.';
            }
        }
    } else if (isset($_GET['logout']) || !file_exists($glo_dbname)) {
        unset($_SESSION['login']);
    }

	header('Content-Type: text/html; charset=utf-8');
?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="fr" lang="fr">
<head>
	<title>Nubo</title>
	<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
    <link href="https://fonts.googleapis.com/css?family=Titillium+Web" rel="stylesheet"/>
	<link rel="stylesheet" type="text/css" href="style.css" media="screen"/>
<?php if (isset($_SESSION['login'])) echo "    <script src=\"actions.js\" type=\"text/javascript\"></script>\n"; ?>
</head>
<body>
    <div id="topbar">
        <div><h1>Nubo</h1></div>
        <div><?php if (isset($_SESSION['login'])) echo '<a href="index.php?logout">Log out</a>'; ?></div>
    </div>
    <div id="content">
<?php
    if (!file_exists($glo_dbname)) {
        echo <<<EOT
<div style="width: 500px;">
<h2>Welcome to Nubo!</h2>
<p>To finalize the installation of your storage space, you must define a password that will be used both to cipher your data and to restrict access of your cloud to authorised users. Chose it wisely! There is no way to recover if you forget it.</p>
EOT;
        if (isset($error)) {
            echo "<p class=\"error\">$error</p>";
        }
        echo <<<EOT
<form method="post" action="index.php"><fieldset>
<label>Password:</label><input class="pwd" type="password" name="password1"/>
<label>Confirm password:</label><input class="pwd" type="password" name="password2"/>
<input type="submit" value="Install"/>
</fieldset></form>
EOT;
        echo "\n";
        if (!isset($_SERVER['HTTPS']) || $_SERVER['HTTPS'] != 'on') {
            echo <<<EOT
<div class="warning">Your connection is not secured. To ensure your data privacy and safety, installing Nubo on a host that does not run SSL/TLS is strongly discouraged.</div>
EOT;
        }
    } else if (!isset($_SESSION['login'])) {
        echo <<<EOT
<div style="width: 500px;">
<p>Your storage space is installed and running. To download the nubo client application for your platform, or to get information about storage usage, please sign in below.</p>
EOT;
        if (isset($error)) {
            echo "<p class=\"error\">$error</p>";
        }
        echo <<<EOT
<form method="post" action="index.php"><fieldset>
<label>Password:</label><input class="pwd" type="password" name="password"/>
<input type="submit" value="Sign in"/>
</fieldset></form>
EOT;
    } else {
        try {
            $db = new NuboDatabase();
            $islocked = false;
            echo "<h2>Computers</h2>\n";
            $result = $db->select('SELECT computer_id, hostname, atime, validator, lock FROM tbl_computer ORDER BY hostname ASC, atime DESC');
            if (count($result)) {
                echo "<table>";
                echo "<tr><th>Name</th><th>Last synchronised</th><th>Status</th><th>Actions</th></tr>\n";
                foreach ($result as $row) {
                    $name = htmlspecialchars($row['hostname'], ENT_COMPAT, 'UTF-8');
                    $lock = !is_null($row['lock']);
                    $islocked |= $lock;
                    echo "<tr>";
                    echo "<td>$name</td>";
                    echo "<td>", formatDate($row['atime']), "</td>";
                    echo "<td>", ($row['validator'] ? ($lock ? "Syncing" : "OK") : "Auth failed"), "</td>";
                    echo "<td><a href=\"javascript:revokeComputer(${row['computer_id']}, '$name');\">Revoke</a></td>";
                    echo "</tr>\n";
                }
                echo "</table>\n";
                if ($islocked) {
                    echo "<p>The cloud is locked because a computer is currently syncing. If you have good reasons to think this information is outdated, you can <a href=\"javascript:releaseLock();\">force unlocking</a>.</p>\n";
                }
            } else {
                echo "<p>No computers are synchronised yet with this storage.</p>\n";
            }
            echo "<h2>Files</h2>\n";
            $result = $db->select('SELECT file_id, filename, hash, mtime FROM tbl_file ORDER BY filename ASC');
            $numfiles = count($result);
            if ($numfiles > 0) {
                echo "<table>";
                echo "<tr><th>Filename</th><th>Last modification</th><th>Actions</th></tr>\n";
                foreach ($result as $row) {
                    echo "<tr>";
                    echo "<td>", htmlspecialchars(abbreviatePath($row['filename'], 120), ENT_COMPAT, 'UTF-8'), "</td>";
                    echo "<td>", formatDate($row['mtime']), "</td>";
                    $fullname = htmlspecialchars($row['filename'], ENT_COMPAT, 'UTF-8');
                    echo "<td><a href=\"javascript:deleteFile(${row['file_id']}, '$fullname', $islocked);\">Delete</a></td>";
                    echo "</tr>\n";
                }
                $plural = ($numfiles != 1) ? "s" : "";
                echo "</table>\n<p>Total: $numfiles file$plural</p>\n";
            } else {
                echo "<p>This storage does not contain any file yet.</p>\n";
            }
            echo "<h2>Client application</h2>\n";
            $user_agent = $_SERVER['HTTP_USER_AGENT'];
            if (preg_match('/macintosh|mac os x/i', $user_agent)) {
                echo <<<EOT
<p>To install the latest <strong>nubo</strong> client application for macOS:</p>
<ul>
    <li><a href="https://aequans.com/download.php?file=7">Click here</a> to download the nubo package</li>
    <li>Double-click the downloaded file to start installation</li>
</ul>
EOT;
            } else if (preg_match('/linux|ubuntu/i', $user_agent)) {
                echo <<<EOT
<p>To install the latest <strong>nubo</strong> client application for Linux:</p>
<ul>
    <li><a href="https://aequans.com/download.php?file=8">Click here</a> to download the nubo package</li>
    <li>Open a terminal and change to the directory where the package has been downloaded</li>
    <li>Type: sudo dpkg -i nubo.deb</li>
</ul>
EOT;
            } else if (preg_match('/windows/i', $user_agent)) {
                echo <<<EOT
<p>To install the latest <strong>nubo</strong> client application for Windows:</p>
<ul>
    <li><a href="https://aequans.com/download.php?file=9">Click here</a> to download the nubo installer</li>
    <li>Double-click the downloaded file to start installation</li>
</ul>
EOT;
            } else {
                echo "<p>Sorry, there is no client application for your platform yet.</p>\n";
            }
            echo "<p class=\"copyright\">© 2017-2020, Æquans - Version ", VERSION, "</p>\n";
        } catch (NuboException $e) {
            echo "<p>Unexpected error.</p>\n";
        }
    }
?>
    </div>
</body>
</html>
