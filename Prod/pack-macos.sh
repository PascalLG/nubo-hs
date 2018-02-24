#!/bin/bash

VERSION="2.1.2"
BUILDPATH=../Client/.stack-work/install/x86_64-osx/lts-8.21/8.0.2/bin/nubo-exe

# Check we are root
if [[ $EUID -ne 0 ]]; then
   echo "Oops. This script must be run as root." 1>&2
   exit 1
fi

# Check build products actually exist.
if ! [ -f $BUILDPATH ]; then
    echo "Oops. Build products not found."
    echo "Run 'stack build' in the ../Client folder first."
    exit 1
fi

# Remove any previous package
rm nubo.pkg > /dev/null 2>&1
rm nubo_s.pkg > /dev/null 2>&1

# Prepare the package content
mkdir -p "nubo/usr/local/bin"
cp $BUILDPATH nubo/usr/local/bin/nubo
strip nubo/usr/local/bin/nubo
chmod 755 nubo/usr/local/bin/nubo
chown -R root:wheel nubo

# Build the package
pkgbuild --root nubo --identifier com.aequans.nubo --version $VERSION nubo.pkg

# Sign the package
productsign --sign "Developer ID Installer" nubo.pkg nubo_s.pkg
if [ -f nubo_s.pkg ]; then
    mv -f nubo_s.pkg nubo.pkg
fi

# Remove temporary files
rm -rf nubo
