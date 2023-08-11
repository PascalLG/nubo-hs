#!/bin/bash

# exit if any command fails
set -e

# uncomment to debug this script
# trap 'last_command=$current_command; current_command=$BASH_COMMAND' DEBUG
# trap 'echo "\"${last_command}\" command failed with exit code $?."' EXIT

# read version and trim trailing spaces
VERSION=$(<versioninfo.txt)
VERSION="${VERSION%"${VERSION##*[![:space:]]}"}"

echo
echo "==============================================="
echo "=== Packaging nubo client v$VERSION for Debian ==="
echo "==============================================="
echo
echo "Compiling..."
echo

# patch the cabal file, build the client and retrieve the PATH to the binary
# note: the windows packager leaves the cabal file with CRLF line endings, it must be
# converted back to LF before being passed to sed.
pushd ../Client > /dev/null 2>&1
tr -d '\r' < nubo.cabal | sed -E -e "s/^[[:space:]]*version:[[:space:]]*[0-9.]+[[:space:]]*$/version:             $VERSION/" > nubo.cabal.tmp
mv -f nubo.cabal.tmp nubo.cabal
stack build
BUILDPATH=$(stack path --local-install-root)/bin/nubo-exe
popd > /dev/null 2>&1

# Check build products actually exist.
if ! [ -f $BUILDPATH ]; then
    echo "Oops. Build products not found."
    exit 1
fi

# Remove any previous package
rm -f nubo.deb > /dev/null 2>&1

# Prepare the package content
echo
echo "Packaging..."
echo

mkdir -p "tmp/nubo/usr/local/bin"
cp $BUILDPATH tmp/nubo/usr/local/bin/nubo
strip tmp/nubo/usr/local/bin/nubo
chmod 755 tmp/nubo/usr/local/bin/nubo

# Prepare the package metadata
mkdir -p "tmp/nubo/DEBIAN"
cat << EOF > tmp/nubo/DEBIAN/control
Package: nubo
Version: $VERSION
Section: misc
Priority: optional
Architecture: amd64
Maintainer: pascal.levy@aequans.com
Description: Nubo client application
Homepage: https://github.com/PascalLG/nubo-hs
EOF

# Build the package
pushd tmp > /dev/null
dpkg-deb --build nubo
popd > /dev/null
mv tmp/nubo.deb .

# Remove temporary files
rm -rf tmp

echo
echo "OK."
echo
