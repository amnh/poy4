#!/bin/bash
export PATH=/home/andres/minglibs/bin:/cygdrive/c/ocamlmgw/3_10_0/bin:$PATH
source $HOME/.keychain/${HOSTNAME}-sh

if ! svn update; then
    echo "Repository update failed ... sorry pal!"
    exit 1
fi

cp ./config.win32 config

function compile_executable {

cd ./src

cp ./Makefile.win32 Makefile

if ! make clean; then
    echo "I could not clean up the distribution ..."
    exit 1
fi

if ! make depend; then
    echo "I could not make depend ..."
    exit 1
fi

if ! make poy; then
    echo "I could not make poy!!! ..."
    exit 1
fi

cd ../
}

compile_executable

if ! cp -f ./src/poy.exe /cygdrive/c/poy_distribution/bin/ncurses_poy.exe; then
    echo "I could not replace the poy executable in the distribution"
    exit 1
fi

sed -e "s/ncurses/html/" config.win32 > config
compile_executable
if ! cp -f ./src/poy.exe /cygdrive/c/poy_distribution/bin/seq_poy.exe; then
    echo "I could not replace the executable in the distribution"
    exit 1
fi
rm -f /cygdrive/c/POY_Installer.msi
./create_installers.bat
if ! scp /cygdrive/c/POY_Installer.msi samson:poy_distro/distro_generation_scripts/; then
    echo "I could not copy the resulting executable in newlila2!"
    exit 1
fi

