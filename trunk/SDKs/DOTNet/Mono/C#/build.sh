#!/bin/sh

xbuild $1
cp ./lib/* ./bin/Debug/
cp -R ./Resources ./bin/Debug