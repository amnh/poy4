#!/bin/bash
# The environment MACHOST holds the URL of the macintosh host running the
# virtual machine. The only argument contains the desired extra flags for the
# configuration script (for example long sequence support). 
export PATH=/home/andres/minglibs/bin:/cygdrive/c/ocamlmgw/3_10_0/bin:$PATH
source $HOME/.keychain/${HOSTNAME}-sh

if ! svn update; then
    echo "Repository update failed ... sorry pal!"
    exit 1
fi

function compile_executable {

cd ./src

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

# We first compile the regular ncurses interface
./configure $1 --enable-xslt --with-extras="/home/andres/pdcurs28/*.o /home/andres/libxml2-2.6.30/*.o /home/andres/libxslt-1.1.22/libxslt/*.o" --enable-interface=ncurses 
if ! cp -f ./src/poy.exe /cygdrive/c/poy_distribution/bin/ncurses_poy.exe; then
    echo "I could not replace the poy executable in the distribution"
    exit 1
fi

# Now we compile the html interface
./configure $1 --enable-xslt --enable-interface=html
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

