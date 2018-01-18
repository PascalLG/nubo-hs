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

    $glo_dirname = "storage";
    $glo_dbname = "./$glo_dirname/nubo.rdb";

    //-----------------------------------------------------------------------------
    // Class to access the persistent storage.
    //-----------------------------------------------------------------------------

    class NuboDatabase {
        private $db;

        // Constructor.

        public function __construct() {
            global $glo_dbname;
            $this->db = new SQLite3($glo_dbname);
            if (!$this->db) {
                throw new NuboException(ERROR_DATABASE);
            }
        }
        
        // Destructor.

        public function __destruct() {
            $this->close();
        }

        // Close the database connection.

        public function close() {
            if ($this->db) {
                $this->db->close();
                $this->db = false;
            }
        }

        // Execute a SELECT query and return an array of rows.

        function select($query, $bindings = NULL) {
            $statement = $this->db->prepare($query);
            if ($statement === false) {
                throw new NuboException(ERROR_INTERNAL);
            }
            $this->bind($statement, $bindings);
            $result = $statement->execute();
            if ($result === false) {
                $statement->close();
                throw new NuboException(ERROR_INTERNAL);
            }
            $retval = array();
            while (($row = $result->fetchArray(SQLITE3_ASSOC)) !== false) {
                $retval[] = $row;
            }
            $result->finalize();
            $statement->close();
            return $retval;
        }

        // Execute a query that does not return results (INSERT, UPDATE, etc.)

        function execute($query, $bindings = NULL) {
            $statement = $this->db->prepare($query);
            if (!$statement) {
                throw new NuboException(ERROR_INTERNAL);
            }
            $this->bind($statement, $bindings);
            $result = $statement->execute();
            $statement->close();
            if ($result === false) {
                throw new NuboException(ERROR_INTERNAL);
            }
        }

        // Return the last inserted row ID.

        function getLastInsertRowID() {
            return $this->db->lastInsertRowID();
        }

        // Perform bindings. Bindings are specified as name/type array. For
        // example: 'name/text' or 'content/blob'. The type may be missing,
        // leaving SQLite determining a suitable one from the variable
        // type (not recommended).

        private function bind($statement, $bindings) {
            if (isset($bindings)) {
                if (!is_array($bindings)) {
                    throw new NuboException(ERROR_INTERNAL);
                }
                foreach ($bindings as $key => $value) {
                	$n = strpos($key, '/');
                	if ($n !== false) {
                		$key2 = substr($key, 0, $n);
                        switch (substr($key, $n + 1)) {
                            case 'int':     $type = SQLITE3_INTEGER;                    break;
                            case 'float':   $type = SQLITE3_FLOAT;                      break;
                            case 'text':    $type = SQLITE3_TEXT;                       break;
                            case 'blob':    $type = SQLITE3_BLOB;                       break;
                            default:        throw new NuboException(ERROR_INTERNAL);    break;
                        }
                        $statement->bindValue(":$key2", $value, $type);
                    } else {
                        $statement->bindValue(":$key", $value);
                    }
                }
            }
        }
    }

    //-----------------------------------------------------------------------------
?>
