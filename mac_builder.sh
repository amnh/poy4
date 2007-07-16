#!/bin/bash
# This script takes care of generating binaries for all mac plataforms.

# A function that takes four arguments: the SDK to use, the target architecture,
# the configuration options, and finally the target binary where poy will be
# placed.
function generate_binary {
    echo "Configurating for $1 - $2 - $3 for target $4" >> distro.log
    PATH=$5:$PATH CFLAGS="-O3 -I/usr/include/malloc -arch $2 -isysroot $1 $6" CC="$7" ./configure $3 | tee -a distro.log
    PATH=$5:$PATH make clean | tee -a distro.log
    PATH=$5:$PATH make ocamlmpi | tee -a distro.log
    PATH=$5:$PATH make depend | tee -a distro.log
    PATH=$5:$PATH make poy | tee -a distro.log
    mv -f src/poy $4
}

## Generating for Panther, we don't produce parallel version.
#generate_binary /Developer/SDKs/MacOSX10.3.9.sdk ppc "--enable-interface=html" \
#./panther/seq_poy.command /Developer/SDKs/MacOSX10.3.9.sdk/usr/bin "" gcc
#
#generate_binary /Developer/SDKs/MacOSX10.3.9.sdk ppc "--enable-interface=ncurses" \
#./panther/ncurses_poy.command /Developer/SDKs/MacOSX10.3.9.sdk/usr/bin "" gcc 
#
#
# Generating for universal binaries under Tiger.

 Generating for PowerPC
function generate_ppc {
    generate_binary /Developer/SDKs/MacOSX10.4u.sdk ppc "$1" \
    ./universal/$3 /opt/ocaml/ppc/3.09.3/bin/ "" "$2"
}
#
#generate_ppc "--enable-interface=html" gcc seq_poy_ppc
#generate_ppc "--enable-interface=ncurses" gcc ncurses_poy_ppc
#generate_ppc "--enable-interface=html --enable-mpi=mpich" /usr/local/poy4/mpich2-1.0.5p2/gforker/arch/bin/mpicc par_poy_pcc
#
#
function generate_intel {
    generate_binary /Developer/SDKs/MacOSX10.4u.sdk i386 "$1" \
    ./universal/$3 /opt/ocaml/3.09.3/bin/ "" "$2"
}

generate_intel "--enable-interface=html" gcc seq_poy_intel
generate_intel "--enable-interface=ncurses" gcc ncurses_poy_intel
generate_intel "--enable-interface=html --enable-mpi=mpich" /usr/local/poy4/mpich2-1.0.5p2/gforker/arch/bin/mpicc par_poy_intel

# Finally, generate the universal binaries
lipo -output ./universal/seq_poy.command -create ./universal/seq_poy_ppc \
./universal/seq_poy_intel

lipo -output ./universal/par_poy.command -create ./universal/par_poy_pcc \
./universal/par_poy_intel

lipo -output ./universal/ncurses_poy.command -create ./universal/ncurses_poy_ppc \
./universal/ncurses_poy_intel

