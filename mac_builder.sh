#!/bin/bash
# This script takes care of generating binaries for all mac plataforms.

# A function that takes four arguments: the SDK to use, the target architecture,
# the configuration options, and finally the target binary where poy will be
# placed.
function generate_binary {
    echo "Configurating for $1 - $2 - $3 for target $4" >> distro.log
    echo "Configurating for $1 - $2 - $3 for target $4" 
    if ! PATH=$5:$PATH CFLAGS="-O3 -I/usr/include/malloc -arch $2 -isysroot $1 $6" CC="$7" ./configure $3 >> distro.log; then
        echo "Failed in the configuration step for $1 $2 $3 target $4"
        exit 1
    fi
    echo "Make Clean"
    if ! PATH=$5:$PATH make clean >> distro.log; then
        echo "Failed in the clean step for $1 $2 $3 target $4"
        exit 1
    fi
    echo "Make OcamlMPI"
    PATH=$5:$PATH make ocamlmpi >> distro.log
    echo "Make Depend"
    if ! PATH=$5:$PATH make depend >> distro.log; then
        echo "Failed in the depend step for $1 $2 $3 target $4"
        exit 1
    fi
    echo "Make POY"
    if ! PATH=$5:$PATH make poy >> distro.log; then
        echo "Failed in the make step for $1 $2 $3 target $4"
        exit 1
    fi
    mv -f src/poy $4
}

# Generating for Panther, we don't produce parallel version.
if ! generate_binary /Developer/SDKs/MacOSX10.3.9.sdk ppc "--enable-interface=html" \
    ./panther/seq_poy.command /Developer/SDKs/MacOSX10.3.9.sdk/usr/bin "" gcc; then
    exit 1
fi

if ! generate_binary /Developer/SDKs/MacOSX10.3.9.sdk ppc "--enable-interface=ncurses" \
./panther/ncurses_poy.command /Developer/SDKs/MacOSX10.3.9.sdk/usr/bin "" gcc; then
    exit 1
fi


# Generating for universal binaries under Tiger.

# Generating for PowerPC
function generate_ppc {
    generate_binary /Developer/SDKs/MacOSX10.4u.sdk ppc "$1" \
    ./universal/$3 /opt/ocaml/ppc/3.09.3/bin/ "" "$2"
}

if ! generate_ppc "--enable-interface=html" gcc seq_poy_ppc; then
    exit 1
fi
if ! generate_ppc "--enable-interface=ncurses" gcc ncurses_poy_ppc; then
    exit 1
fi
if ! generate_ppc "--enable-interface=html --enable-mpi=mpich" \
    /usr/local/poy4/mpich2-1.0.5p2/gforker/arch/bin/mpicc par_poy_pcc; then 
    exit 1
fi
function generate_intel {
    generate_binary /Developer/SDKs/MacOSX10.4u.sdk i386 "$1" \
    ./universal/$3 /opt/ocaml/3.09.3/bin/ "" "$2"
}

if ! generate_intel "--enable-interface=html" gcc seq_poy_intel; then
    exit 1
fi

if ! generate_intel "--enable-interface=ncurses" gcc ncurses_poy_intel; then
    exit 1
fi
if ! generate_intel "--enable-interface=html --enable-mpi=mpich" \
    /usr/local/poy4/mpich2-1.0.5p2/gforker/arch/bin/mpicc par_poy_intel; then
    exit 1
fi

# Finally, generate the universal binaries
if ! lipo -output ./universal/seq_poy.command -create ./universal/seq_poy_ppc \
    ./universal/seq_poy_intel; then
    echo "Failed in lipo sequential for universal binaries"
    exit 1
fi

if ! lipo -output ./universal/par_poy.command -create ./universal/par_poy_pcc \
    ./universal/par_poy_intel; then
    echo "Failed in lipo parallel for universal binaries"
    exit 1
fi

if ! lipo -output ./universal/ncurses_poy.command -create ./universal/ncurses_poy_ppc \
    ./universal/ncurses_poy_intel; then
    echo "Failed in lipo ncurses for universal binaries"
    exit 1
fi
