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

// Call the web service.

function callWebService(cmd) {
    var req = new XMLHttpRequest();
  	req.onreadystatechange = function() {
        if (req.readyState == 4) {
            if (req.status < 200 || req.status >= 300) {
                alert('An error occurred. (HTTP status = ' + status + ')');
            }
            document.location = 'index.php';
        }
    }
  	req.open('GET', 'actions.php?' + cmd, true);
  	req.send();
}

// Revoke access to a computer, given its name and ID.

function revokeComputer(id, name) {
    var go = confirm('Revoke access to computer "' + name + '"?');
    if (go) {
        callWebService('revoke=' + id);
    }
}

// Release all locks.

function releaseLock() {
    var go = confirm('Release lock? You may lose data if a computer is actually syncing.');
    if (go) {
        callWebService('unlock');
    }
}

// Delete a file, given its name and ID.

function deleteFile(id, name, lock) {
    if (lock) {
        alert('Cannot delete file ' + name + '. The cloud is currently syncing.');
    } else {
        var go = confirm('Delete file "' + name + '"?');
        if (go) {
            callWebService('delete=' + id);
        }
    }
}

//-----------------------------------------------------------------------------
