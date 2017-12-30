#!/bin/bash

VERSION="2.0.0.4"
BUILDPATH=../Client/.stack-work/install/x86_64-linux/lts-8.21/8.0.2/bin/nubo-exe

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
rm nubo.deb > /dev/null 2>&1

# Prepare the package content
mkdir -p "/tmp/nubo/usr/local/bin"
cp $BUILDPATH /tmp/nubo/usr/local/bin/nubo
strip /tmp/nubo/usr/local/bin/nubo
chmod 755 /tmp/nubo/usr/local/bin/nubo
chown -R root:root /tmp/nubo

# Prepare the package metadata
mkdir -p "/tmp/nubo/DEBIAN"
cat << EOF > /tmp/nubo/DEBIAN/control
Package: nubo
Version: $VERSION
Section: base
Priority: optional
Architecture: all
Maintainer: pascal.levy@aequans.com
Description: Nubo client application
Homepage: https://github.com/PascalLG/Nubo
EOF

# Build the package
pushd /tmp > /dev/null
dpkg-deb --build nubo
popd > /dev/null
mv /tmp/nubo.deb .

# Remove temporary files
rm -rf /tmp/nubo
