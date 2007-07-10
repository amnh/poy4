#!/bin/bash
# This script takes care of generating binaries for all mac plataforms.

# A function that takes four arguments: the SDK to use, the target architecture,
# the configuration options, and finally the target binary where poy will be
# placed.
function generate_binary {
    echo "Configurating for $1 - $2 - $3 for target $4" >> distro.log
    PATH=$5:$PATH CFLAGS="-O3 -I/usr/include/malloc -arch $2 -isysroot $1 $6" \
    $7 ./configure $3 | tee -a distro.log
    PATH=$5:$PATH make clean | tee -a distro.log
    PATH=$5:$PATH make depend | tee -a distro.log
    PATH=$5:$PATH make poy | tee -a distro.log
    mv -f src/poy $4
}

generate_binary /Developer/SDKs/MacOSX10.3.9.sdk ppc "" \
./panther/poy /Developer/SDKs/MacOSX10.3.9.sdk/usr/bin "" ""

generate_binary /Developer/SDKs/MacOSX10.4u.sdk ppc "" \
./universal_sequential/poy_ppc /opt/ocaml/ppc/3.09.3/bin/ "" ""

generate_binary /Developer/SDKs/MacOSX10.4u.sdk i386 "" \
./universal_sequential/poy_intel /opt/ocaml/3.09.3/bin/ "" ""

