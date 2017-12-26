# Nubo

## Introduction

Nubo is a self-hosted file sharing application. It currently runs on macOS and Linux. Support for Windows is expected soon.

It consists of two applications:

* A cloud part, which is found in the Server directory. It is a small PHP application that must be installed on a server.
* A client part, which is found in the Client directory. It is a command line application that must be installed on every computer that want to synchronise with the cloud.

Basically nubo runs as any other file sharing solution. You drop files into a special folder and they auto-magically appear on all other computers of the same cloud. There are some significant differences though. (Actually I would not have written nubo if existing solutions exactly suited my needs!)

* Synchronisation between your local computer and the cloud is triggered by launching a command line application. Having an "on demand" instead of a continuous synchronisation avoids unnecessary network activity when you create or save files very often. (Think of working on and compiling a software project directly from your nubo drive, for example.) It also allows scripting, be it with cron, bash, Python, AppleScript or any other tool.

* Symlinks are fully resolved. To synchronise a file or a folder, no need to copy it on the nubo drive: just create an alias or a symbolic link to it. Note that this is a hazardous feature though: you can easily have nubo deleting content directly in your personal folder via a symlink, or entering an infinite loop if symlink resolution leads to a cycle.

* End-to-end encryption with AES 256 in CTR mode. The key is never stored on the server. Your data remains safe even if your server is compromised. (The counterpart is that this architecture forbids implementation of a web sharing feature à la Dropbox, but I can live with that.)

* You can add filter patterns (on a computer basis) to avoid synchronising specific files. For example, you may want to ignore all ``.o`` files if you happen to compile a C project directly from your nubo drive. Or you may want to share files between your personal and your business computers, except for that folder with very personal content.

* Unlike other similar applications, the nubo configuration is not stored globally but in a hidden file in each nubo drive. This means you can have as many nubo drives as you want on your computer, each synchronising with a different server. 

* Nubo silently ignores the following files and directories: ``.DS_Store``, ``Icon\\r``, ``.cabal-sandbox``, ``cabal.sandbox.config``, ``.stack-work``, ``.git``, ``desktop.ini`` and ``thumbs.db``. All these files are only relevant for a given computer (mostly because they store absolute paths) and there is no point in transfering them on another computer.

* Files with a non-portable name trigger an error. Non portable names include: characters that are not supported on all platforms, reserved keywords, reserved Windows peripheral names, and names that may conflict once transferred on a case insensitive file system. Files bigger than 300Mo also trigger an error.

*Warning: use nubo at your own risk. By nature this software may alter or delete content on your hard drive. It is provided "as is" without any warranty of any kind. In no event shall the authors be liable for any claim or damages. Refer to the license file for more information.*

## Installation

### Server

Nubo can be installed on any server that runs PHP 7 with SQLite and Zip extensions enabled. It is strongly advised that your server is reachable through a TLS connection: files are encrypted but metadata is not. Moreover, accessing your server in HTTP instead of HTTPS means that your password is sent as plain text on the internet during this installation phase.

To install your server:

* Download the ``nubo.php`` file by [clicking here](https://aequans.com/download.php?file=5).
* Upload this file on your server via FTP, SFTP or any other appropriate tool.
* Open it in your browser. For example, if your server URL is ``mydomain.com``, open ``https://mydomain.com/nubo.php``.
* If the installation failed, the page should read an error message that lists encountered issues. Fix them and try again.
* Otherwise, the page displays a success message. Follow the on screen instructions to define your cloud password and finalise the configuration.

Nubo does not have to reside at the root of your server. You can also install it in a subdirectory. You can even host several clouds on the same server in separate subdirectories.

### Client

The client is available as a macOS package for the macOS platform and as a Debian package for the Linux platform. To install it:

* Open your server URL in a browser.
* Enter your cloud password to log in.
* At the bottom of the page, a section should provide a link to download the package for your platform.
* Once downloaded, install that package the usual way. On most systems, that boils down to double-clicking the file.

You now have to create a nubo drive. This is the special folder that will synchronise with your cloud.

* Create an empty subfolder anywhere you wish below your home directory. For example, you could choose ``~/Nubo`` or ``~/Documents/NuboDrive``.
* Open a terminal and change the current directory to that folder. For example: ``cd ~/Nubo``.
* Type ``nubo init`` and hit enter.
* Enter your server URL (don't forget the ``https://`` part).
* Enter your cloud password.

Et voilà! You are ready to add content to your nubo folder and synchronise it with your cloud.

Note: on Windows 10, nubo may crash with an error message about certificate validation. Just open once your cloud URL with Edge to fix this issue.

## Usage

All operations are performed from the command line. Type ``nubo`` to display a full list of available commands. Type ``nubo help`` followed by a command name to get comprehensive information about that specific command.

The most frequently used commands are detailed below.

### Synchronisation

To trigger a synchronisation, open a terminal, change directory to your nubo drive and type ``nubo sync``. If any doubt, add the ``--dry`` option to check which file operations will be carried out. This option instructs nubo to print what it will do without actually doing it.

A conflict arises when the same file has been modified on several computers. In such a case, use either the ``--ours`` or the ``--theirs`` options to tell nubo which version of the file it should keep. Note that these options are only effective in case of a synchronisation conflict. You cannot use them to overwrite a modified local version of a file with a not modified remote version of that file. Actually, nubo is a file sharing application, not a version control system like SVN or Git.

You can also perform a partial synchronisation by specifying a list of files or directories to synchronise.

For more information about the sync command, type ``nubo help sync``.

### Ignoring files

The ``nubo ignore`` command allows you to add patterns to the list of files to ignore. During synchronisation, nubo simply ignores any file or directory that matches one of these patterns, be it on the server side or on the local side. For all these files, local modifications are no longer sent to the cloud and remote modifications are no longer reflected locally.

For more information about patterns and how to manage them, type ``nubo help ignore``.

### Authentication

Authentication is performed once with the ``nubo init`` command. Nubo then uses a temporary token that changes after each transaction to ensure subsequent authentications. However, that token may become invalid, either because it expires or because of an unexpected problem. In such a case, you must authenticate again with the ``nubo auth`` command. You will be asked for your cloud password.

Authentication issues are mostly related to network disconnections while synchronising. In such a case, the server processes a query, invalidates the current token and generates a new one, but the client is never aware of that new token because the connection is lost before it has a chance to receive the server response.

## Roadmap

Just a list of improvements, things to do and ideas to explore. No particular order. No deadlines.

* Ask for a confirmation in ``nubo init`` if it detects the connection is not secured with TLS
* Synchronise file permissions (currently all files are created with the default umask)
* Do not resolve symlinks that point to another location into the same drive
* Check if a new version of the application is available before each invocation (both client and server side)
* Fix/improve the progress bar when uploading/downloading large files
* Refactor using the more robust Path package for filename handling instead of System.Filepath
* Refactor the Archive module so it can be unit tested
* Create a macOS application to encapsulate the command line into a nice GUI
* Improve the Windows version and provide an MSI installer
* Improve the web installer (permissions problems sometimes arise when installing over an old version + there is currently no migration of the database in this case)
* Write a better documentation :-)

Suggestions are welcome. My goal is to keep this application as simple and minimalist as possible. "Make each program do one thing well" says the Unix philosophy!

You can also check the [wiki page](https://github.com/PascalLG/Nubo/wiki) if you are looking for more technical informations.
