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

    require_once('php/version.php');
    require_once('php/exception.php');
    require_once('php/sqlite.php');
    require_once('php/msgpack.php');
    require_once('php/api.php');

    // Webservice entry point. All queries are POST requests, with a msgpack
    // encoded body. The body contains a 'cmd' field that describe the command
    // to execute, an optional 'params' field, and authentication info.

    try {
        $response = [];
        $db = new NuboDatabase();

        $unpacker = new MsgUnpacker(file_get_contents('php://input'));
        $body = $unpacker->unpack();

        if (!is_array($body) || !isset($body['cmd'])) {
            throw new NuboException(ERROR_ILL_FORMED);
        }

        if (!isset($body['api']) || intval($body['api']) < APILEVEL) {
            throw new NuboException(ERROR_API_OLD_CLIENT);
        } else if (intval($body['api']) > APILEVEL) {
            throw new NuboException(ERROR_API_OLD_SERVER);
        }

        $query = 'cmd_' . $body['cmd'];
        $params = isset($body['params']) ? $body['params'] : null;

        if ($query == 'cmd_init') {
            $response += cmd_init($db, $params);
        } else if (is_callable($query)) {
            if (!isset($body['auth'])) {
                throw new NuboException(ERROR_FORBIDDEN);
            }
            list($newtoken, $computer) = refresh_token($db, $body['auth']);
            $response += ['auth' => $newtoken];
            $response += $query($db, $params, $computer);
        } else {
        	throw new NuboException(ERROR_BAD_COMMAND);
        }
    } catch (NuboException $e) {
        $response += $e->getResponse();
    }

    $db->close();
    $packer = new MsgPacker();
    $content = $packer->pack($response);

    header('Cache-Control: no-cache, must-revalidate');
    header('Expires: Sat, 26 Jul 1997 05:00:00 GMT');
    header('Content-Type: application/x-msgpack');
    header('Content-Length: ' . strlen($content));

    echo $content;

    //-----------------------------------------------------------------------------
?>
