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

    // The authentication token is made of two components: a selector and a
    // validator. The selector identifies the computer and the validator acts as
    // a password that changes after every transaction.
    //
    // This function refreshes the token by:
    // - checking the selector/validator pair matches a registered computer
    // - generate a new random validator for that user
    // - return the new auth token
    //
    // If validation fails, both selector and validator are revoked, meaning
    // the user must authenticate again with its password.

    function refresh_token($db, $token) {
    	$n = strpos($token, ':');
    	if ($n !== false) {
    		$selector = substr($token, 0, $n);
    		$validator = substr($token, $n + 1);
            $result = $db->select('SELECT computer_id, validator FROM tbl_computer WHERE selector=:sel', ['sel/text' => $selector]);
            if (($row = reset($result)) !== false && $row['validator'] == hash('sha256', $validator)) {
                $validator = bin2hex(random_bytes(16));
                $db->execute('UPDATE tbl_computer SET validator=:val, atime=:atime WHERE computer_id=:id', ['id/int' => $row['computer_id'], 'val/text' => hash('sha256', $validator), 'atime/int' => time()]);
                return "$selector:$validator";
            } else {
    			$db->execute('UPDATE tbl_computer SET selector=NULL, validator=NULL WHERE computer_id=:id', ['id/int' => $row['computer_id']]);
            }
         }
         throw new NuboException(ERROR_FORBIDDEN);
    }

    // Build the name of the local file where an archive will be
    // stored, based on the row ID.

    function buildArchiveName($id) {
        global $glo_dirname;
        return sprintf("./$glo_dirname/file%09d.bin", $id);
    }

    // Init command. This command:
    // - authenticate the user with the password defined at installation
    // - generate the first authentication token
    // - return the master salt generated at installation that will be used
    //   by the client to cipher/uncipher archives.

    function cmd_init($db, $params) {
        if (!isset($params['password']) || !isset($params['computer']) || !isset($params['hostname']) || !isset($params['salt'])) {
            throw new NuboException(ERROR_MISSING_PARAMETER);
        }
        $db->execute('BEGIN TRANSACTION');
        $db->execute('DELETE FROM tbl_computer WHERE computer=:computer', ['computer/text' => $params['computer']]);
        $result = $db->select('SELECT value FROM tbl_config WHERE key="password" LIMIT 1');
        if (($row = reset($result)) !== false && password_verify($params['password'], $row['value'])) {
        	$selector = bin2hex(random_bytes(16));
        	$validator = bin2hex(random_bytes(16));
            $db->execute('INSERT INTO tbl_computer(computer, hostname, selector, validator, atime) VALUES (:computer, :hostname, :selector, :validator, :atime)', [
                'computer/text' => $params['computer'],
                'hostname/text' => $params['hostname'],
                'selector/text' => $selector,
                'validator/text' => hash('sha256', $validator),
                'atime/int' => time()
            ]);
            $ret = ["auth" => "$selector:$validator"];
            if ($params['salt']) {
                $result = $db->select('SELECT value FROM tbl_config WHERE key="salt" LIMIT 1');
                if ($row = reset($result)) {
                    $ret += ["salt" => $row['value']];
                }
            }
            $db->execute('COMMIT TRANSACTION');
            return $ret;
        } else {
            $db->execute('COMMIT TRANSACTION');
            throw new NuboException(ERROR_FORBIDDEN);
        }
    }

    // Directory command. Return the list of all archives on the server
    // with their hash.

    function cmd_directory($db, $params) {
        $result = $db->select('SELECT filename AS f, hash AS h FROM tbl_file');
        return ['result' => $result];
    }

    // Put command. Store an archive sent by the client application. The
    // filename and hash are stored in the database, while the archive
    // content is stored on disk. If the item is actually a directory,
    // the hash is empty and the content is missing.

    function cmd_put($db, $params) {
        if (!isset($params['name']) || !isset($params['hash']) || !isset($params['mtime']) || ($params['hash'] != '' && !isset($params['content']))) {
            throw new NuboException(ERROR_MISSING_PARAMETER);
        }
        $db->execute('BEGIN TRANSACTION');
        $result = $db->select('SELECT file_id FROM tbl_file WHERE filename=:name', ['name/text' => $params['name']]);
        $bindings = ['hash/text' => $params['hash'], 'mtime/int' => $params['mtime']];
        if (($row = reset($result)) !== false) {
            $id = $row['file_id'];
            $db->execute('UPDATE tbl_file SET hash=:hash, mtime=:mtime WHERE file_id=:id', ['id/int' => $id] + $bindings);
        } else {
            $db->execute('INSERT INTO tbl_file(filename, hash, mtime) VALUES (:name, :hash, :mtime)', ['name/text' => $params['name']] + $bindings);
            $id = $db->getLastInsertRowID();
        }
        if (isset($params['content'])) {
            if (file_put_contents(buildArchiveName($id), $params['content']) === false) {
                $db->execute('ROLLBACK TRANSACTION');
                throw new NuboException(ERROR_STORAGE);
            }
        }
        $db->execute('COMMIT TRANSACTION');
        return [];
    }

    // Get command. Return the content and the hash of an archive. In case the
    // filename refers to a directory, both the hash and content are empty.

    function cmd_get($db, $params) {
        if (!isset($params['name'])) {
            throw new NuboException(ERROR_MISSING_PARAMETER);
        }
        $result = $db->select('SELECT file_id, hash, mtime FROM tbl_file WHERE filename=:name LIMIT 1', ['name/text' => $params['name']]);
        if (($row = reset($result)) === false) {
            throw new NuboException(ERROR_STORAGE);
        }
        if ($row['hash'] != '') {
            $content = file_get_contents(buildArchiveName($row['file_id']));
            if ($content === false) {
                throw new NuboException(ERROR_STORAGE);
            }
        } else {
            $content = null;
        }
        return ['hash' => $row['hash'], 'mtime' => $row['mtime'], 'content' => $content];
    }

    // Delete command. Remove an archive or a directory from the database
    // and from the disk.

    function cmd_delete($db, $params) {
        if (!isset($params['name'])) {
            throw new NuboException(ERROR_MISSING_PARAMETER);
        }
        $result = $db->select('SELECT file_id FROM tbl_file WHERE filename=:name LIMIT 1', ['name/text' => $params['name']]);
        if (($row = reset($result)) !== false) {
            $id = $row['file_id'];
            $db->execute('DELETE FROM tbl_file WHERE file_id=:id', ['id/int' => $id]);
            @unlink(buildArchiveName($id));
        }
        return [];
    }

    //-----------------------------------------------------------------------------
?>
