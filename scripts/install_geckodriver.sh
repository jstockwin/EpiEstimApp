#!/usr/bin/env bash
set -ex

VERSION=v0.22.0
FILENAME=geckodriver-$VERSION-linux64.tar.gz
if [ ! -f "$FILENAME" ]; then
    wget https://github.com/mozilla/geckodriver/releases/download/v0.22.0/$FILENAME
fi

tar -xvzf $FILENAME
chmod +x geckodriver
mv geckodriver /usr/bin/geckodriver