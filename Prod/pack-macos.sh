#!/bin/bash

# exit if any command fails
set -e

# trap 'last_command=$current_command; current_command=$BASH_COMMAND' DEBUG
# trap 'echo "\"${last_command}\" command failed with exit code $?."' EXIT

# read version and trim trailing spaces
VERSION=$(<versioninfo.txt)
VERSION="${VERSION%"${VERSION##*[![:space:]]}"}"

echo
echo "=============================================="
echo "=== Packaging nubo client v$VERSION for macOS ==="
echo "=============================================="
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
rm -f nubo.pkg > /dev/null 2>&1
rm -f nubo_s.pkg > /dev/null 2>&1

# Prepare the package content
echo
echo "Packaging..."
echo

mkdir -p "tmp/usr/local/bin"
cp $BUILDPATH tmp/usr/local/bin/nubo
strip tmp/usr/local/bin/nubo
chmod 755 tmp/usr/local/bin/nubo

# Build the package
pkgbuild --root tmp --identifier com.aequans.nubo --version $VERSION --ownership recommended nubo.pkg

# Remove temporary files
rm -rf tmp > /dev/null 2>&1

# Sign the package
# productsign --sign "Developer ID Installer" nubo.pkg nubo_s.pkg
# if [ -f nubo_s.pkg ]; then
#     mv -f nubo_s.pkg nubo.pkg
# fi

echo
echo "OK."
