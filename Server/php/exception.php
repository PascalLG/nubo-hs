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

    // Error codes. Must match error codes in the client application.

    define('ERROR_DATABASE',            1);
    define('ERROR_ILL_FORMED',          2);
    define('ERROR_BAD_COMMAND',         3);
    define('ERROR_MISSING_PARAMETER',   4);
    define('ERROR_FORBIDDEN',           5);
    define('ERROR_STORAGE',             6);
    define('ERROR_MSGPACK',             7);
    define('ERROR_INTERNAL',            8);
    define('ERROR_API_OLD_CLIENT',      9);
    define('ERROR_API_OLD_SERVER',     10);
    

    //-----------------------------------------------------------------------------
    // Custom exception.
    //-----------------------------------------------------------------------------

    class NuboException extends RuntimeException {
        private $errcode;
        private $backtrace;

        // Construct an exception with an error code.

        public function __construct($errcode) {
            $this->errcode = $errcode;
            $this->backtrace = [];
            foreach (debug_backtrace() as $call) {
                $this->backtrace[] = pathinfo($call['file'], PATHINFO_BASENAME) . ' ' . $call['line'];
            }
        }

        // Build the msgpack response corresponding to this exception.

        public function getResponse() {
            $msg = [
                ERROR_DATABASE =>           'database not found',
                ERROR_ILL_FORMED =>         'ill-formed query',
                ERROR_BAD_COMMAND =>        'bad command',
                ERROR_MISSING_PARAMETER =>  'missing parameter',
                ERROR_FORBIDDEN =>          'forbidden',
                ERROR_STORAGE =>            'storage consistency',
                ERROR_MSGPACK =>            'invalid msgpack',
                ERROR_INTERNAL =>           'internal server error',
                ERROR_API_OLD_CLIENT =>     'incompatible API, client is too old',
                ERROR_API_OLD_SERVER =>     'incompatible API, server is too old',
            ][$this->errcode];
            return ['error' => $this->errcode, 'message' => $msg . ' => ' . implode(', ', $this->backtrace)];
        }
    }

    //-----------------------------------------------------------------------------
?>
