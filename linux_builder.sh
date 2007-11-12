#!/bin/bash
# MACHOST holdst the URL of the macintosh computer hosting the virtual machine.
# Script to generate binaries for Linux
export PATH=/opt/ocaml-3.10.0/bin:$PATH
LINUX_DIRECTORY=linux
mkdir linux
rm -f linux/*
if ! ./configure --enable-interface=html CFLAGS="-static -static-libgcc -O3";then
    echo "Failure in html interface configuration"
    exit 1
fi
if ! make clean; then
    echo "Could not clean!"
    exit 1
fi
if ! make; then
    echo "Failure in make step"
    exit 1
fi
cp ./src/poy ./$LINUX_DIRECTORY/seq_poy.command

# Now we make the ncurses interface
if ! ./configure --enable-interface=ncurses CFLAGS="-static -static-libgcc -O3"; then
    echo "Failure in ncurses interface configuration"
    exit 1
fi
if ! make clean; then
    echo "Could not clean!"
    exit 1
fi
if ! make; then
    echo "Failure in make step"
    exit 1
fi
cp ./src/poy ./$LINUX_DIRECTORY/ncurses_poy
cat > ./${LINUX_DIRECTORY}/ncurses_poy.command <<EOF
#!/bin/bash
xterm -e /opt/poy4/Resources/ncurses_poy
EOF
chmod a+x ./${LINUX_DIRECTORY}/ncurses_poy.command
scp -r ./${LINUX_DIRECTORY} ${MACHOST}:poy_distro/distro_generation_scripts/
