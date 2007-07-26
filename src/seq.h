/* POY 4.0 Beta. A phylogenetic analysis program using Dynamic Homologies.    */
/* Copyright (C) 2007  Andr�s Var�n, Le Sy Vinh, Illya Bomash, Ward Wheeler,  */
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

#ifndef SEQ_H

#define SEQ_H 1
#include <caml/custom.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include "array_pool.h"

/* Macro to retrieve and cast a pointer to a seq structure from the Ocaml custom
 * type. */
#define Seq_pointer(a) ((struct seq **) Data_custom_val(a))
#define Seq_custom_val(a) (*((struct seq **) Data_custom_val(a)))
#define Seq_struct(a) (Seq_custom_val(a))
#define SEQT unsigned char

/* Sequence structure to be used inside ocaml custom types. */
struct seq {
    int cap;        /* Capacity of the sequence memory structure. */
    int len;        /* Total length of the sequence stored. */
    SEQT *head;
    SEQT *begin;      /* Offset of the position where the first element of 
                       the sequence is actually stored. */
    SEQT *end;
    struct pool *my_pool;
};

typedef struct seq * seqt;

/* Gets the capacity of the sequence a. */
#ifdef _WIN32
__inline int
#else
inline int
#endif
seq_get_cap (const seqt a);

#ifdef _WIN32
__inline void
#else
inline void
#endif
seq_prepend (seqt a, SEQT v);

/* Gets the total length of the sequence a */
#ifdef _WIN32
__inline int
#else
inline int
#endif
seq_get_len (const seqt a);

/* Gets a pointer to the beginning of the sequence a */
#ifdef _WIN32
__inline SEQT *
#else
inline SEQT *
#endif
seq_get_begin (const seqt a);

/* Gets a pointer to the beginning of the array where the sequence is stored.
 * Note that begin != head. */
#ifdef _WIN32
__inline SEQT *
#else
inline SEQT *
#endif
seq_get_head (const seqt a);

/* Gets a pointer to the memory location where the last element of the sequence
 * a is stored. */
#ifdef _WIN32
__inline SEQT *
#else
inline SEQT *
#endif
seq_get_end (const seqt a);

/* Gets the value of the sequence a in the position p, a starting in position 0
 */
#ifdef _WIN32
__inline SEQT 
#else
inline SEQT 
#endif
seq_get (const seqt a, int p);

/* Stores the value v in the position p of sequence a. */
#ifdef _WIN32
__inline void
#else
inline void
#endif
seq_set (seqt a, int p, SEQT v);

#endif /* SEQ_H */
