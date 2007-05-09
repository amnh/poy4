/* POY 4.0 Beta. A phylogenetic analysis program using Dynamic Homologies.    */
/* Copyright (C) 2007  Andrés Varón, Le Sy Vinh, Illya Bomash, Ward Wheeler,  */
/* and the American Museum of Natural History.                                */
/*                                                                            */
/* This program is free software; you can redistribute it and/or modify       */
/* it under the terms of the GNU General Public License as published by       */
/* the Free Software Foundation; either version 2 of the License, or          */
/* (at your option) any later version.                                        */
/*                                                                            */
/* This program is distributed in the hope that it will be useful,            */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of             */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              */
/* GNU General Public License for more details.                               */
/*                                                                            */
/* You should have received a copy of the GNU General Public License          */
/* along with this program; if not, write to the Free Software                */
/* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301   */
/* USA                                                                        */

#include <caml/custom.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include "seq.h"
#include "cm.h"

struct unionoff {
    seqt s;         /* A pointer to the sequence that holds the union */
    int *offsets;   /* A pointer to the array of offsets */
    int *begin;     /* The current position where the sequence starts */
    int *end;       /* The end of the offset array. This is after the 
                       first writtable memory position */
    int *ca_offsets;
    int *cb_offsets;
    int counter;    /* A convenient counter for the merging operations */
    int length;     /* The current number of items it holds */
    int position;   /* The location of the current position in some 
                       internal operations */
};

typedef struct unionoff *unionofft;

/* Take the counter prepend consecutive, numbers from counter to 0, 
 * to u->begin, then set counter to 0. */
void
union_prepend_counter (unionofft u);

void
union_merge (seqt a, seqt b, unionofft au, unionofft bu, unionofft c, cmt m);
