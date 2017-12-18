#!/bin/bash

# Check we are root
if [[ $EUID -ne 0 ]]; then
   echo "This script must be run as root" 1>&2
   exit 1
fi

# Remove any previous package
rm nubo.deb > /dev/null 2>&1

# Prepare the package content
mkdir -p "nubo/usr/local/bin"
cp ../Client/.stack-work/install/x86_64-linux/lts-8.21/8.0.2/bin/nubo-exe nubo/usr/local/bin/nubo
strip nubo/usr/local/bin/nubo
chmod 755 nubo/usr/local/bin/nubo
chown -R root:root nubo

# Prepare the package metadata
mkdir -p "nubo/DEBIAN"
cat << 'EOF' > nubo/DEBIAN/control
Package: nubo
Version: 2.0.0.2
Section: base
Priority: optional
Architecture: all
Maintainer: pascal.levy@aequans.com
Description: Nubo client application
Homepage: https://github.com/PascalLG/Nubo
EOF

# Build the package
dpkg-deb --build nubo

# Remove temporary files
rm -rf nubo
