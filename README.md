# Nubo

## Introduction

Nubo is a self-hosted file sharing application. It currently runs on macOS and Linux. Support for Windows should not require much work and is expected soon.

It consists of two applications:

* A cloud part, which is found in the Server directory. It is a small PHP application that must be installed on a server.
* A client part, which is found in the Client directory. It is a command line application that must be installed on every computer that synchronises with the cloud.

Basically nubo runs as any other file sharing solution. You drop files into a special folder and they auto-magically appear on all other computers of the same cloud. There are some significant differences though. (Actually I would not have written nubo if existing solutions exactly suited my needs!)

* Synchronisation between your local computer and the cloud is triggered by launching a command line application. Having an "on demand" instead of a continuous synchronisation avoids unnecessary network activity when you create or save files very often. (Think of working on and compiling a software project directly from your nubo drive, for example.) It also allows scripting, be it with cron, AppleScript or any other tool.

* Symlinks are fully resolved. To synchronise a file or a folder, no need to copy it on the nubo drive: just create an alias or a symbolic link to it. Note that this is a hazardous feature though: you can easily have nubo deleting content directly in your personal folder via a symlink, or entering an infinite loop if symlink resolution leads to a cycle.

* End-to-end encryption with AES 256 in CTR mode. The key is never stored on the server. Your data remains safe even if your server is compromised. The counterpart is that this architecture forbids implementation of a web sharing feature à la Dropbox.

* You can add filter patterns (on a computer basis) to avoid synchronising specific files. For example, you may want to ignore all ``.o`` files if you happen to compile a C project directly from your nubo drive. Or you may not want to download that very personal file on your business computer.

* Unlike other similar applications, the nubo configuration is not stored globally but in a hidden file in each nubo drive. This means you can have as many nubo drives as you want on your computer, each synchronising with a different server. 

* Nubo silently ignores the following files and directories: ``.DS_Store``, ``Icon\\r``, ``.cabal-sandbox``, ``cabal.sandbox.config``, ``.stack-work``, ``.git``, ``desktop.ini`` and ``thumbs.db``. All these files are only relevant for a given computer and there is no point in transfering them on another computer.

* Files with a non-portable name trigger an error. Non portable names include: characters that are not supported on all platforms, reserved keywords, reserved Windows peripheral names, and names that may conflict once transferred on a case insensitive file system. Files bigger than 300Mo also trigger an error.

Warning: use nubo at your own risk. By nature this software may alter or delete content on your hard drive. It is provided "as is" without any warranty of any kind. In no event shall the authors be liable for any claim or damages. Refer to the license file for more information.

## Installation

### Server

Nubo can be installed on any server that runs PHP 7 with SQLite and Zip extensions enabled. It is strongly advised that your server is reachable through a connection that is secured with TLS. Files are encrypted but metadata is not. Moreover, accessing your server in HTTP instead of HTTPS means that your password is sent as plain text on the internet during this installation phase.

To install your server:

* Download the ``install.php`` file by [clicking here](https://aequans.com/download.php?file=5).
* Upload this file on your server via FTP, SFTP or any other appropriate tool.
* Open it in your browser. For example, if your server URL is ``mydomain.com``, open ``https://mydomain.com/install.php``.
* If the installation failed, the page should read an error message listing encountered issues. Fix them and try again.
* Otherwise, the page displays a success message. Follow the on screen instructions to define your cloud password and finalise the configuration.

Nubo does not have to reside at the root of your server. You can also install it in a subdirectory. You can even host several clouds on the same server in separate subdirectories.

### Client

The client is available as a macOS package for the macOS platform, and as a Debian package for the Linux platform. To install it:

* Open your server URL in a browser.
* Enter your cloud password to log in.
* At the bottom of the page, a section should provide installation instructions and a link to download the client for your platform.
* Open a terminal.
* Type ``nubo`` and hit enter to check it runs properly.

You now have to create a nubo drive. This is the special folder that will be synchronised with your cloud.

* Create an empty subfolder anywhere you wish below your home directory. For example, you could choose ``~/Nubo`` or ``~/Documents/NuboDrive``.
* Open a terminal and change the current directory to that folder. For example: ``cd ~/Nubo``.
* Type ``nubo init`` and hit enter.
* Enter your server URL (don't forget the ``https://`` part).
* Enter your cloud password.

Et voilà! You are ready to add content to your nubo folder and synchronise it with your cloud.

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

Authentication issues are mostly related to disconnections while synchronising. In such a case, the server processes a query, invalidates the current token and generates a new one, but the client is never aware of that new token because the connection is lost before it has a chance to receive the server response.

## Compiling Nubo

If you want to contribute, or if you just want to play around with the source code, here is how to compile Nubo.

### Server

In the ``./Prod`` directory, the ``build-server.php`` script is used to generate an installer for the server application. This script zips all the required files, encodes the resulting archive in base64, and embeds that data into the ``template.php`` file to produce a self-extracting ``install.php`` file.

To run this script: open a terminal, change directory to ``./Prod`` and run ``./build-server.php``. If bash complains about permissions, ensure this script has its execute bit set by running ``chmod +x build-server.php``.

You'll need a PHP 5 or PHP 7 interpreter in ``/usr/bin``. This should not be a problem on most UNIX-like systems.

### Client

The client application is a regular Haskell project based on Stack and Cabal. If necessary, you can install the required tool chain on your computer by following the instructions on the [Stack](https://docs.haskellstack.org/en/stable/README/) homepage.

Once you have a running Haskell compiler, to build nubo: open a terminal, change directory to ``./Client`` and run ``stack build``. To run unit tests, type ``stack test``. The ``build-macos.sh`` and ``build-linux.sh`` scripts in the ``./Prod`` directory are then used to generate macOS and Debian packages.

Although I have not tested yet, I expect compiling on Windows requires a bit more work, mainly because some libraries that are common on UNIX-like systems are missing by default on Windows. More on that later.

Note that the source directory includes a hidden ``.ghci`` file. It defines command line options that are passed to GHCi but not to GHC, in other words options that apply when debugging in REPL mode but not when compiling a release build. This is used in ``./src/Config.hs`` to change the working directory to ``./Sandbox`` when experimenting interactively with the synchronisation algorithm.

## Roadmap

Just a list of improvements, things to do and ideas to explore. No particular order. No deadlines.

* Ask for a confirmation in ``nubo init`` if it detects the connection is not secured with TLS
* Synchronise file permissions (currently all files are created with the default umask)
* Do not resolve symlinks that point to another location into the same drive
* Check if a new version of the application is available before each invocation (both client and server side)
* Add an API version number to each frame to ensure the server and client are compatible
* Fix/improve the progress bar when uploading/downloading large files
* Refactor using the more robust Path package for filename handling instead of System.Filepath
* Refactor the Archive module so it can be unit tested
* Create a macOS application to encapsulate the command line into a nice GUI
* Build a Windows version (should work out-of-the-box but probably requires installation of a few DLLs on the user's computer)
* Write a better documentation :-)

Suggestions welcome. My goal is to keep this application as simple and minimalist as possible: "make each program do one thing well."
