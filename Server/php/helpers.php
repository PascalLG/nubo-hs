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

    // Format a date.

    function formatDate($date) {
        return date("Y-m-d H:i:s", $date);
    }
    
    // Abbreviate a path so it does not exceed a given number
    // of characters.

    function abbreviatePath($path, $limit) {
        if (strlen($path) <= $limit) {
            return $path;
        }
        $paths = explode('/', $path);
        $del = 1;
        while (true) {
            $middle = intval((count($paths) - $del) / 2);
            $tmp = array_merge(array_slice($paths, 0, $middle), [' … '] , array_slice($paths, $middle + $del));
            $path = implode('/', $tmp);
            if (strlen($path) <= $limit || $del >= count($paths) - 2) {
                return $path;
            }
            $del++;
        }
    }

    //-----------------------------------------------------------------------------
?>
