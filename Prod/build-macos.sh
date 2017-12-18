#!/bin/bash

# Check we are root
if [[ $EUID -ne 0 ]]; then
   echo "This script must be run as root" 1>&2
   exit 1
fi

# Remove any previous package
rm nubo.pkg > /dev/null 2>&1

# Prepare the package content
mkdir -p "nubo/usr/local/bin"
cp ../Client/.stack-work/install/x86_64-osx/lts-8.21/8.0.2/bin/nubo-exe nubo/usr/local/bin/nubo
strip nubo/usr/local/bin/nubo
chmod 755 nubo/usr/local/bin/nubo
chown -R root:wheel nubo

# Build the package
pkgbuild --root nubo --identifier com.aequans.nubo --version 2.0.0.2 nubo.pkg

# Remove temporary files
rm -rf nubo
