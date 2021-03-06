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
    ini_set('display_errors', 0);
    ini_set('log_errors', 1);

    require_once('php/exception.php');
    require_once('php/sqlite.php');
	session_start();

    if (!isset($_SESSION['login'])) {
        header($_SERVER['SERVER_PROTOCOL'] . ' 403 Forbidden', true, 403);
        die();
    }

    try {
        if (isset($_GET['revoke'])) {
            $db = new NuboDatabase();
            $db->execute('DELETE FROM tbl_computer WHERE computer_id=:id', ['id/int' => intval($_GET['revoke'])]);
            $db->close();
        } else if (isset($_GET['delete'])) {
            $db = new NuboDatabase();
            $db->execute('DELETE FROM tbl_file WHERE file_id=:id', ['id/int' => intval($_GET['delete'])]);
            $db->close();
        } else if (isset($_GET['unlock'])) {
            $db = new NuboDatabase();
            $db->execute('UPDATE tbl_computer SET lock=NULL');
            $db->close();
        } else {
            throw new NuboException(ERROR_BAD_COMMAND);
        }
    } catch (NuboException $e) {
        header($_SERVER['SERVER_PROTOCOL'] . ' 500 Internal Server Error', true, 500);
    }

    //-----------------------------------------------------------------------------
?>
