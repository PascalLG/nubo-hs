#!/bin/bash

# Check this script is passed a version number.
if [[ $# -ne 1 ]]; then
    echo "Oops. Invalid number of arguments."
    exit 1
fi

# Check the version number has the right format.
if ! [[ $1 =~ ^[0-9]{1,}\.[0-9]{1,}\.[0-9]{1,}\.[0-9]{1,}$ ]]; then
    echo "Oops. '$1' does not look like a valid version number."
    exit 1
fi

# Patch the server version number
sed -E -i "" -e "s/define\\(\"VERSION\", *\"[0-9.]+\"\\);/define(\"VERSION\", \"$1\");/" ../Server/php/version.php

# Patch the client version number
sed -E -i "" -e "s/^ *version: *[0-9.]+ *$/version:             $1/" ../Client/nubo.cabal

# Patch the macOS and the Debian packagers
sed -E -i "" -e "s/^VERSION=\"[0-9.]+\" *$/VERSION=\"$1\"/" pack-macos.sh
sed -E -i "" -e "s/^VERSION=\"[0-9.]+\" *$/VERSION=\"$1\"/" pack-linux.sh
