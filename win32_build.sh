#!/bin/bash
if ! svn update; then
    echo "Repository update failed ... sorry pal!"
    exit 1
fi

cd ./src

cp ./Makefile.win32 Makefile

if ! make depend; then
    echo "I could not make depend ..."
    exit 1
fi

if ! make poy; then
    echo "I could not make poy!!! ..."
    exit 1
fi

if !  scp -f poy.exe newlila2:poy_distro/distro_generation_scripts/; then
    echo "I could not copy the resulting executable in newlila2!"
    exit 1
fi
