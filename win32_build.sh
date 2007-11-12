#!/bin/bash
# The environment MACHOST holds the URL of the macintosh host running the
# virtual machine.
export PATH=/home/andres/minglibs/bin:/cygdrive/c/ocamlmgw/3_10_0/bin:$PATH
source $HOME/.keychain/${HOSTNAME}-sh

if ! svn update; then
    echo "Repository update failed ... sorry pal!"
    exit 1
fi

function compile_executable {

echo "$1" >> config

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

cp ./config.win32 config
compile_executable "export USE_LONG_SEQUENCES = $1"

if ! cp -f ./src/poy.exe /cygdrive/c/poy_distribution/bin/ncurses_poy.exe; then
    echo "I could not replace the poy executable in the distribution"
    exit 1
fi

sed -e "s/ncurses/html/" config.win32 > config
compile_executable "export USE_LONG_SEQUENCES = $1"
if ! cp -f ./src/poy.exe /cygdrive/c/poy_distribution/bin/seq_poy.exe; then
    echo "I could not replace the executable in the distribution"
    exit 1
fi
rm -f /cygdrive/c/POY_Installer.msi
./create_installers.bat
if ! scp /cygdrive/c/POY_Installer.msi ${MACHOST}:poy_distro/distro_generation_scripts/; then
    echo "I could not copy the resulting executable in newlila2!"
    exit 1
fi

