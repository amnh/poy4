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

/* Alignment libray in C */

#include <stdio.h>
#include <malloc.h>
#include <stdio.h>
#include <limits.h>
#define NDEBUG 1
#include <assert.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include "zarr.h"
#include "matrices.h"
#include "array_pool.h"
#include "seq.h"
#include "cm.h"

#define HIGH_NUM 1000000
#define NPRINT_DM 1
#define NPRINT_CM 1
#define NDEBUG_BT 1

#include "matrices.c"
#include "cm.c"
#include "seq.c"
#include "array_pool.c"

#ifdef DEBUG_ALL_ASSERTIONS
int *_algn_max_matrix = NULL;
unsigned char *_algn_max_direction = NULL;
#endif
/******************************************************************************/
/*                        Pairwise Standard Alignment                         */
/******************************************************************************/
/*
 * As standard, all the caml binding functions are called algn_CAML_<function
 * name>
 */

/* Fill a row in a two dimentional alignment
 *
 * When pairwise alignment is performed, two sequences are compared over a
 * transformation cost matrix. Let's call them sequences x and y written over
 * some alphabet a of length |a|. Each base of x
 * is represented by a column in the transformation cost matrix and each base of
 * y by a row. However, note that the actual values that are added during the
 * alignment are produced by comparing every single base of x with only |a|
 * elements. Now, in order to to make these operations vectorizable, we perform
 * the comparisons with |a| precalculated vectors. This puts in context the
 * explanation of each parameter in the function.
 *
 * @param mm is the cost matrix row to be filled with values.
 * @param pm is the row above mm in the cost matrix being filled.
 * @param gap_row is the cost of aligning each base in x with a gap.
 * @param alg_row is the cost of aligning each base in x wit hthe base
 * represented by the base of the row of mm in y.
 * @param dm is the directional matrix for the backtrack
 * @param c is the cost of an insertion. As an insertion can only occur for one
 * particular base in the alphabet, corresponding to the base in y represented
 * by the row that is being filled.
 * @param st is the starting cell for the filling.
 * @param end is the final cell for the filling.
 * If you modify this code check algn_fill_cube as there is simmilar code there
 * used in the first plane of the alignment. It didn't use this function because
 * the direction codes are different for three dimensional alignments.
 */
#ifdef _WIN32
__inline void 
#else
inline void 
#endif
algn_fill_row (int *mm, const int *pm, const int *gap_row, \
        const int *alg_row, unsigned char *dm, int c, int st, int end) {
    int i, tmp1, tmp2, tmp3;

    for (i = st; i <= end; i++) {
        /* try align with substitution */
        tmp1 = pm[i] + c;
        tmp2 = mm[i - 1] + gap_row[i];
        tmp3 = pm[i - 1] + alg_row[i];
        /* check whether insertion is better */
        /* This option will allow all the possible optimal paths to be stored
         * concurrently on the same backtrack matrix. This is important for the
         * sequences being able to choose the appropriate direction while
         * keeping the algorithm that assumes that s2 is at most as long as s1.
         * */
        if (tmp1 < tmp3) { 
            if (tmp1 < tmp2) {
                mm[i] = tmp1;
                dm[i] = DELETE;
            }
            else if (tmp2 < tmp1) {
                mm[i] = tmp2;
                dm[i] = INSERT;
            }
            else {
                mm[i] = tmp2;
                dm[i] = (INSERT | DELETE);
            }
        }
        else if (tmp3 < tmp1) {
            if (tmp3 < tmp2) {
                mm[i] = tmp3;
                dm[i] = ALIGN;
            } 
            else if (tmp2 < tmp3) {
                mm[i] = tmp2;
                dm[i] = INSERT;
            }
            else {
                mm[i] = tmp2;
                dm[i] = (ALIGN | INSERT);
            }
        }
        else { /* tmp3 == tmp1 */
            if (tmp3 < tmp2) {
                mm[i] = tmp3;
                dm[i] = (ALIGN | DELETE);
            }
            else if (tmp2 < tmp3) {
                mm[i] = tmp2; 
                dm[i] = INSERT;
            }
            else {
                mm[i] = tmp2;
                dm[i] = (DELETE | INSERT | ALIGN);
            }
        }
        if (!NDEBUG && !NPRINT_DM) {
            /* Print the alignment matrix */
            if (INSERT & dm[i]) 
                printf ("I");
            if (DELETE & dm[i])
                printf ("D");
            if (ALIGN & dm[i])
                printf ("A");
            printf ("\t");
        }
        if (!NDEBUG &&!NPRINT_CM) {
            /* Print the cost matrix */
            printf ("%d\t", mm[i]);
            fflush (stdout);
        }
    }
    if (!NDEBUG && (!NPRINT_CM || !NPRINT_DM))  {
        printf ("\n");
        fflush (stdout);
    }
    return;
}

#ifdef _WIN32
__inline void 
#else
inline void 
#endif
algn_fill_ukk_right_cell (int *mm, const int *pm, const int *gap_row, \
        const int *alg_row, unsigned char *dm, int c, int pos) {
    int tmp2, tmp3;
    /* try align with substitution */
    tmp2 = mm[pos - 1] + gap_row[pos];
    tmp3 = pm[pos - 1] + alg_row[pos];
    /* check whether insertion is better */
    if (tmp2 < tmp3) {
        mm[pos] = tmp2;
        dm[pos] = INSERT;
    }
    else if (tmp3 < tmp2) {
        mm[pos] = tmp3;
        dm[pos] = ALIGN;
    }
    else {
        mm[pos] = tmp3;
        dm[pos] = INSERT | ALIGN;
    }
    if (!NDEBUG && !NPRINT_DM) {
        /* Print the alignment matrix */
        if (INSERT & dm[pos]) 
            printf ("I");
        if (DELETE & dm[pos])
            printf ("D");
        if (ALIGN & dm[pos])
            printf ("A");
        printf ("\t");
    }
    if (!NDEBUG &&!NPRINT_CM) {
        /* Print the cost matrix */
        printf ("%d\t", mm[pos]);
        fflush (stdout);
    }
    if (!NDEBUG && (!NPRINT_CM || !NPRINT_DM)) {
        printf ("\n");
        fflush (stdout);
    }
    return;
}

#ifdef _WIN32
__inline void 
#else
inline void 
#endif
algn_fill_ukk_left_cell (int *mm, const int *pm, const int *gap_row, \
        const int *alg_row, unsigned char *dm, int c, int pos) {
    int tmp1, tmp3;
    /* try align with substitution */
    tmp1 = pm[pos] + c;
    tmp3 = pm[pos - 1] + alg_row[pos];
        if (tmp1 < tmp3) {
            mm[pos] = tmp1;
            dm[pos] = DELETE;
        } 
        else if (tmp3 < tmp1) {
            mm[pos] = tmp3;
            dm[pos] = ALIGN;
        } 
        else {
            mm[pos] = tmp1;
            dm[pos] = ALIGN | DELETE;
        }
    if (!NDEBUG && !NPRINT_DM) {
        /* Print the alignment matrix */
        if (INSERT & dm[pos]) 
            printf ("I");
        if (DELETE & dm[pos])
            printf ("D");
        if (ALIGN & dm[pos])
            printf ("A");
        printf ("\t");
    }
    if (!NDEBUG &&!NPRINT_CM) {
        /* Print the cost matrix */
        printf ("%d\t", mm[pos]);
        fflush (stdout);
    }
    return;
}

#ifdef _WIN32
__inline void
#else
inline void
#endif
algn_fill_last_column (int *mm, const int *pm, int tlc, int l, unsigned char *dm) {
    int cst;
    if (l > 0) {
        cst = tlc + pm[l];
        if (cst < mm[l]) {
            mm[l] = cst;
            dm[l] = DELETE;
        } 
        else if (cst == mm[l])
            dm[l] = dm[l] | DELETE;
    }
    return;
}

#ifdef _WIN32
__inline void
#else
inline void
#endif
algn_fill_full_row (int *mm, const int *pm, const int *gap_row, \
        const int *alg_row, unsigned char *dm, int c, int tlc, int l) {
    /* first entry is delete */
    mm[0] = c + pm[0];
    dm[0] = DELETE;
    if ((!NDEBUG) && (!NPRINT_CM)) {
        printf ("%d\t", mm[0]);
        fflush (stdout);
    }
    if ((!NDEBUG) && (!NPRINT_DM))
        printf ("D\t");
    algn_fill_row (mm, pm, gap_row, alg_row, dm, c, 1, l - 1);
    algn_fill_last_column (mm, pm, tlc, l - 1, dm);
    return;
}

void
algn_fill_first_row (int *mm, unsigned char *dm, int len, int const *gap_row) {
    int i;
    /* We fill the first cell to start with */
    mm[0] = 0;
    dm[0] = ALIGN;
    /* Now the rest of the row */
    if ((!NDEBUG) && (!NPRINT_DM))
        printf ("A\t");
    if ((!NDEBUG) && (!NPRINT_CM)) {
        printf ("%d\t", mm[0]);
        fflush (stdout);
    }
    for (i = 1; i < len; i++) {
        mm[i] = mm[i - 1] + gap_row[i];
        dm[i] = INSERT;
        if ((!NDEBUG) && (!NPRINT_DM))
            printf ("I\t");
        if ((!NDEBUG) && (!NPRINT_CM)) {
            printf ("%d\t", mm[i]);
            fflush (stdout);
        }
    }
    return;
}

void
algn_fill_first_cell (int *mm, int pm, unsigned char *dm, int gap) {
    *mm = pm + gap;
    *dm = DELETE;
    if ((!NDEBUG) && (!NPRINT_DM))
        printf ("D\t");
    if ((!NDEBUG) && (!NPRINT_CM)) {
        printf ("%d\t", *mm);
        fflush (stdout);
    }
    return;
}

/* In the following three functions, we maintain the following invariants in
 * each loop:
 * 1. mm is a row that has not been filled and is the next to be.
 * 4. dm is the current row of the direction matrix
 * 2. pm is the previous row, located right above mm, which has been filled
 * already.
 * 3. i is the number of the row of mm in its containing matrix
 * 5. gap_row is the cost of aligning each base of s2 with a gap. This is
 * constant for all the loop.
 * 6. cur_s1 is the i'th base of s1
 * 7. const_val is the cost of cur_s1 aligned with a gap
 * 8. alg_row is the vector of costs of aligning s2 with cur_s1
 */

int *
algn_fill_extending_right (const seqt s1, int *prec, int s1_len, int s2_len,  \
        int *mm, int *pm, unsigned char *dm, const cmt c, int start_row, int end_row, int len) {
    int i;
    int *tmp, cur_s1, const_val;
    const int *gap_row, *alg_row;
    /** Invariants block 
     * len is the number of items in the row to be filled **/
    i = start_row;
    /* This is what we will perform conceptually, I will stop using the
     * cm_get_precal_row function to speed this up a little bit 
    gap_row = cm_get_precal_row (prec, cm_get_gap (c), s2_len);
    */
    gap_row = prec + (c->gap * s2_len);
    while (i < end_row) {
        /** Invariants block */
        cur_s1 = s1->begin[i];
        const_val = cm_calc_cost (c->cost, cur_s1, c->gap, c->lcm);
        /* This is conceptually what we do in the next line 
        alg_row = cm_get_precal_row (prec, cur_s1, s2_len);
        */
        alg_row = prec + (cur_s1 * s2_len);
        /* Align! */
        algn_fill_first_cell (mm, pm[0], dm, alg_row[0]);
        algn_fill_row (mm, pm, gap_row, alg_row, dm, const_val, 1, len - 2);
        algn_fill_ukk_right_cell (mm, pm, gap_row, alg_row, dm, const_val, \
                len - 1);
        /** Invariants block */
        tmp = mm;
        mm = pm;
        pm = tmp;
        i++;
        dm += s2_len;
        len++;
    }
    return (mm);
}

int *
algn_fill_extending_left_right (const seqt s1, int *prec, int s1_len, \
        int s2_len,  int *mm, int *pm, unsigned char *dm, const cmt c, int start_row, \
        int end_row, int start_column, int len) {
    int i;
    int *tmp, cur_s1, const_val;
    const int *gap_row, *alg_row;
    /** Invariants block 
     * len is the number of cells to fill in the current row minus 1
     * start_column is the first cell to fill in the row */
    i = start_row;
    /* Conceptually 
    gap_row = cm_get_precal_row (prec, cm_get_gap (c), s2_len);
    */
    gap_row = prec + (c->gap * s2_len);
    len--;
    while (i < end_row) {
        /** Invariants block */
        cur_s1 = s1->begin[i];
        const_val = cm_calc_cost (c->cost, cur_s1, c->gap, c->lcm);
        /* Conceptually 
        alg_row = cm_get_precal_row (prec, cur_s1, s2_len);
        */
        alg_row = prec + (cur_s1 * s2_len);
        /* Align! */
        algn_fill_ukk_left_cell (mm, pm, gap_row, alg_row, dm, const_val, \
                start_column);
        algn_fill_row (mm, pm, gap_row, alg_row, dm, const_val, \
                start_column + 1, start_column + (len - 2));
        algn_fill_ukk_right_cell (mm, pm, gap_row, alg_row, dm, const_val, \
                start_column + len - 1);
        /** Invariants block */
        tmp = mm;
        mm = pm;
        pm = tmp;
        i++;
        dm += s2_len;
        start_column++;
    }
    return (mm);
}

int *
algn_fill_extending_left (const seqt s1, int *prec, int s1_len, \
        int s2_len,  int *mm, int *pm, unsigned char *dm, const cmt c, int start_row, \
        int end_row, int start_column, int len) {
    int i;
    int *tmp, cur_s1, const_val, const_val_tail;
    const int *gap_row, *alg_row;
    /** Invariants block 
     * start_column is the first cell to fill in the row 
     * len is the number of cells to fill in the current row minus 1 */
    i = start_row;
    /* Conceptually 
    gap_row = cm_get_precal_row (prec, cm_get_gap (c), s2_len);
    */
    gap_row = prec + (c->gap * s2_len);
    while (i < end_row) {
        /** Invariants block */
        cur_s1 = s1->begin[i];
        const_val = cm_calc_cost (c->cost, cur_s1, c->gap, c->lcm);
        const_val_tail = (cm_get_tail_cost (c))[cur_s1];
        /* Conceptually 
        alg_row = cm_get_precal_row (prec, cur_s1, s2_len);
        */
        alg_row = prec + (cur_s1 * s2_len);
        /* Align! */
        algn_fill_ukk_left_cell (mm, pm, gap_row, alg_row, dm, const_val, \
                start_column);
        algn_fill_row (mm, pm, gap_row, alg_row, dm, const_val, \
                start_column + 1, start_column + len - 1);
        algn_fill_last_column (mm, pm, const_val_tail, start_column + len - 1, dm);
        /** Invariants block */
        tmp = mm;
        mm = pm;
        pm = tmp;
        i++;
        dm += s2_len;
        start_column++;
        len--;
    }
    if (!NDEBUG && !NPRINT_CM) {
        printf ("S2 gap cost\n");
        fflush (stdout);
        for (i = 0; i < s2_len; i++) {
            printf ("%d\t", gap_row[i]);
            fflush (stdout);
        }
        printf ("\n");
        printf ("The S1 - gap cost is %d\n", const_val);
        fflush (stdout);
    }

    return (mm);
}

int *
algn_fill_no_extending (const seqt s1, int *prec, int s1_len, \
        int s2_len,  int *mm, int *pm, unsigned char *dm, const cmt c, int start_row, \
        int end_row) {
    int i;
    int *tmp, cur_s1, const_val, const_val_tail;
    const int *gap_row, *alg_row;
    /** Invariants block */
    i = start_row;
    /* Conceptually 
    gap_row = cm_get_precal_row (prec, cm_get_gap (c), s2_len);
    */
    gap_row = prec + (c->gap * s2_len);
    while (i < end_row) {
        /** Invariants block */
        cur_s1 = s1->begin[i];
        const_val = cm_calc_cost (c->cost, cur_s1, c->gap, c->lcm);
        const_val_tail = (cm_get_tail_cost (c))[cur_s1];
        /* Conceptually 
        alg_row = cm_get_precal_row (prec, cur_s1, s2_len);
        */
        alg_row = prec + (cur_s1 * s2_len);
        /* Align! */
        algn_fill_first_cell (mm, pm[0], dm, alg_row[0]);
        algn_fill_row (mm, pm, gap_row, alg_row, dm, const_val, 1, s2_len - 1);
        algn_fill_last_column (mm, pm, const_val_tail, s2_len - 1, dm);
        /** Invariants block */
        tmp = mm;
        mm = pm;
        pm = tmp;
        i++;
        dm += s2_len;
    }
    return (mm);
}

/* Simmilar to the previous but when no barriers are set */
#ifdef _WIN32
__inline int
#else
inline int
#endif
algn_fill_plane (const seqt s1, int *prec, int s1_len, \
        int s2_len, int *mm, unsigned char *dm, const cmt c) {
    int i;
    const int *alg_row;
    int const_val, const_val_tail, *nm, *tmp;
    const int *gap_row, *first_gap_row;
    int gapcode;
    /* A precalculated cost of a gap aligned with each base in the array */
    gapcode = cm_get_gap (c);
    gap_row = cm_get_precal_row (prec, gapcode, s2_len);
    first_gap_row = cm_get_precal_row (prec, 0, s2_len);
    nm = mm;
    mm[0] = 0;
    dm[0] = ALIGN;
    if ((!NDEBUG) && (!NPRINT_CM))
        printf ("%d\t", mm[0]);
    if ((!NDEBUG) && (!NPRINT_DM))
        printf ("A\t");
    /* We fill the first row to start with */
    for (i = 1; i < s2_len; i++) {
        mm[i] = mm[i - 1] + first_gap_row[i];
        dm[i] = INSERT;
        if ((!NDEBUG) && (!NPRINT_CM))
            printf ("%d\t", mm[i]);
        if ((!NDEBUG) && (!NPRINT_DM))
            printf ("I\t");
    }
    if (!NDEBUG && (!NPRINT_DM || !NPRINT_CM))
        printf ("\n");
    mm += s2_len;
    /* Now we fill the rest of the matrix */
    for (i = 1, dm += s2_len; i < s1_len; i++, dm += s2_len) {
        const_val_tail = (cm_get_tail_cost (c))[seq_get(s1, i)];
        const_val = cm_calc_cost (c->cost, seq_get(s1, i), c->gap, c->lcm);
        alg_row = cm_get_precal_row (prec, seq_get (s1, i), s2_len);
        algn_fill_full_row (mm, nm, gap_row, alg_row, dm, const_val, \
                const_val_tail, s2_len);
        /* We swap mm and nm for the next round */
        tmp = mm;
        mm = nm;
        nm = tmp;
    }
    return (nm[s2_len - 1]);
}

#ifdef _WIN32
__inline int *
#else
inline int *
#endif
choose_other (int *compare, int *a, int *b) {
    if (a == compare) return b;
    else return a;
}

#ifdef _WIN32
__inline int
#else
inline int
#endif
algn_fill_plane_2 (const seqt s1, int *prec, int s1_len, int s2_len, int *mm, \
        unsigned char *dm, const cmt c, int width, int height, int dwidth_height) {
    int *next_row;
    int *next_pm;
    int *a, *b;
    int const *gap_row;
    int start_row, final_row, start_column, length;
    unsigned char *to_go_dm;
    width = width + dwidth_height;
    height = height + dwidth_height;
    a = mm;
    b = mm + s2_len;
    gap_row = cm_get_precal_row (prec, 0, s2_len); /* We want the horizontal row */
    /* We have to consider three cases in this new alignment procedure (much
     * cleaner than the previous): 
     * 
     * Case 1:
     * If s1 is much longer than s2, then there is no point on using the
     * barriers, we rather fill the full matrix in one shot */
    if (((float) s1_len) >= (((float) ((float) 3 / (float) 2) * (float) s2_len)))
        return (algn_fill_plane (s1, prec, s1_len, s2_len, mm, dm, c));
    /* Case 2:
     * There are no full rows to be filled, therefore we have to break the
     * procedure in three different subsets */
    else if ((2 * height) < s1_len) {
        algn_fill_first_row (a, dm, width, gap_row);
        start_row = 1;
        final_row = height;
        start_column = 0; 
        length = width + 1;
        to_go_dm = dm + (start_row * s2_len);
        /* Now we fill that space */
        next_row = algn_fill_extending_right (s1, prec, s1_len, s2_len, b, a, \
                to_go_dm, c, start_row, final_row, length);
        next_pm = choose_other (next_row, a, b);
        /* Next group */
        start_row = final_row;
        final_row = s1_len - (height - 1);
        start_column = 1;
        length = width + height;
        to_go_dm = dm + (start_row * s2_len);
        next_row = algn_fill_extending_left_right (s1, prec, s1_len, \
                s2_len, next_row, next_pm, to_go_dm, c, start_row, \
                final_row, start_column, length);
        next_pm = choose_other (next_row, a, b);
        /* The final group */
        start_row = final_row;
        final_row = s1_len;
        length = length - 2;
        start_column = s2_len - length;
        to_go_dm = dm + (start_row * s2_len);
        next_row = algn_fill_extending_left (s1, prec, s1_len, s2_len, \
                next_row, next_pm, to_go_dm, c, start_row, final_row, \
                start_column, length);
        next_pm = choose_other (next_row, a, b);
    }
    /* Case 3: (final case)
     * There is a block in the middle of with full rows that have to be filled
     * */
    else {
        /* We will simplify this case even further, if the size of the leftover
         * is too small, don't use the barriers at all, just fill it up all */
        if (8 >= (s1_len - height))
            return (algn_fill_plane (s1, prec, s1_len, s2_len, mm, dm, c));
        else {
            algn_fill_first_row (mm, dm, width, gap_row);
            start_row = 1;
            final_row = (s2_len - width) + 1;
            start_column = 0;
            length = width + 1;
            to_go_dm = dm + (s2_len * start_row);
            next_row = algn_fill_extending_right (s1, prec, s1_len, s2_len, b, a, \
                    to_go_dm, c, start_row, final_row, length);
            next_pm = choose_other (next_row, a, b);
            start_row = final_row;
            final_row = s1_len - (s2_len - width) + 1;
            length = s2_len;
            to_go_dm = dm + (s2_len * start_row);
            next_row = algn_fill_no_extending (s1, prec, s1_len, s2_len, \
                    next_row, next_pm, to_go_dm, c, start_row, \
                    final_row);
            next_pm = choose_other (next_row, a, b);
            start_row = final_row;
            final_row = s1_len;
            start_column = 1;
            length = s2_len - 1;
            to_go_dm = dm + (s2_len * start_row);
            next_row = algn_fill_extending_left (s1, prec, s1_len, s2_len, \
                    next_row, next_pm, to_go_dm, \
                    c, start_row, final_row, start_column, length);
            next_pm = choose_other (next_row, a, b);
        }
    }
    return (next_pm[s2_len - 1]);
}
/******************************************************************************/

/******************************************************************************/
/*                         Pairwise Affine Alignment                          */
/******************************************************************************/
/*
 *
 * WARNING! This is a copy of the pairwise standard alignment, modified slightly
 * for the affine case. This is for performance issues! so beware, any change
 * here must also go there.
 */

#define algn_assign_dm(dm,pos,v) dm[pos] = dm[pos] | v

#ifdef _WIN32
__inline void 
#else
inline void 
#endif
algn_fill_row_aff (int *mm, const int *pm, const int *gap_row, \
        const int *alg_row, unsigned char *dm, int c, int cprev, int st, \
        int end, int *dnmm, const int *pdnmm, int *htmm, int open_gap) {
    int i, tmp1, tmp2, tmp3, tmp4, tmp5;

    for (i = st; i <= end; i++) {
        /* try align with substitution */
#ifdef DEBUG_ALL_ASSERTIONS 
        assert ((mm + i) < _algn_max_matrix);
        assert ((pm + i) < _algn_max_matrix);
        assert ((dm + i) < _algn_max_direction);
        assert ((dnmm + i) < _algn_max_matrix);
        assert ((pdnmm + i) < _algn_max_matrix);
        assert ((htmm + i) < _algn_max_matrix);
#endif
        dm[i] = 0;
        { /* We deal with the difficultness of using an opening gap as 
             another unsigned character */
            if ((0 == cprev) && (0 != c)) {
                tmp1 = pdnmm[i] + open_gap + c;
                tmp4 = pm[i] + open_gap + c;
            }
            else if ((0 != cprev) && (0 == c)) {
                tmp1 = pdnmm[i] + open_gap + c;
                tmp4 = pm[i];
            }
            else {
                tmp1 = pdnmm[i] + c;
                tmp4 = pm[i] + open_gap + c;
            }

            if ((0 == gap_row[i - 1]) && (0 != gap_row[i])) {
                tmp5 = htmm[i - 1] + open_gap + gap_row[i];
                tmp2 = mm[i - 1] + open_gap + gap_row[i];
            } 
            else if ((0 != gap_row[i - 1]) && (0 == gap_row[i])) {
                tmp5 = htmm[i - 1] + open_gap + gap_row[i];
                tmp2 = mm[i - 1];
            }
            else {
                tmp2 = mm[i - 1] + open_gap + gap_row[i];
                tmp5 = htmm[i - 1] + gap_row[i];
            }

            if ((((0 == gap_row[i]) && (0 != c)) ||
                    ((0 != gap_row[i]) && (0 == c))) &&
                    ((0 == gap_row[i - 1]) || (0 == cprev)))
                tmp3 = pm[i - 1] + open_gap + alg_row[i];
            else
                tmp3 = pm[i - 1] + alg_row[i];
        }
        if (tmp1 < tmp4)
            algn_assign_dm(dm,i,DELETE_V);
        else {
            algn_assign_dm(dm,i,ALIGN_V);
            tmp1 = tmp4;
        }

        if (tmp2 <= tmp5) {
            algn_assign_dm(dm,i,ALIGN_H);
        }
        else {
            tmp2 = tmp5;
            algn_assign_dm(dm,i,INSERT_H);
        }
        dnmm[i] = tmp1;
        htmm[i] = tmp2;
        /* check whether insertion is better */
        /* This option will allow all the possible optimal paths to be stored
         * concurrently on the same backtrack matrix. This is important for the
         * sequences being able to choose the appropriate direction while
         * keeping the algorithm that assumes that s2 is at most as long as s1.
         * */
        if (tmp1 < tmp3) { 
            if (tmp1 < tmp2) {
                mm[i] = tmp1;
                algn_assign_dm(dm,i,DELETE);
            }
            else if (tmp2 < tmp1) {
                mm[i] = tmp2;
                algn_assign_dm(dm,i,INSERT);
            }
            else {
                mm[i] = tmp2;
                algn_assign_dm(dm,i,(DELETE | INSERT));
            }
        }
        else if (tmp3 < tmp1) {
            if (tmp3 < tmp2) {
                mm[i] = tmp3;
                algn_assign_dm(dm,i,(ALIGN));
            } 
            else if (tmp2 < tmp3) {
                mm[i] = tmp2;
                algn_assign_dm(dm,i,(INSERT));
            }
            else {
                mm[i] = tmp2;
                algn_assign_dm(dm,i,((ALIGN | INSERT)));
            }
        }
        else { /* tmp3 == tmp1 */
            if (tmp3 < tmp2) {
                mm[i] = tmp3;
                algn_assign_dm(dm,i,((ALIGN | DELETE)));
            }
            else if (tmp2 < tmp3) {
                mm[i] = tmp2; 
                algn_assign_dm(dm,i,(INSERT));
            }
            else {
                mm[i] = tmp2;
                algn_assign_dm(dm,i,((DELETE | INSERT | ALIGN)));
            }
        }
        if (!NDEBUG && !NPRINT_DM) {
            /* Print the alignment matrix */
            if (INSERT & dm[i]) 
                printf ("I");
            if (DELETE & dm[i])
                printf ("D");
            if (ALIGN & dm[i])
                printf ("A");
            printf ("\t");
        }
        if (!NDEBUG &&!NPRINT_CM) {
            /* Print the cost matrix */
            printf ("(%d,%d,%d)\t", mm[i], htmm[i], dnmm[i]);
            fflush (stdout);
        }
    }
    if (!NDEBUG && !NPRINT_DM) {
        printf ("\n");
        fflush (stdout);
    }
    return;
}

#ifdef _WIN32
__inline void 
#else
inline void 
#endif
algn_fill_ukk_right_cell_aff (int *mm, const int *pm, const int *gap_row, \
        const int *alg_row, unsigned char *dm, int c, int cprev, int pos, int *dnmm, int *htmm, \
        int open_gap) {
    int tmp2, tmp3, tmp4;
    /* try align with substitution */
#ifdef DEBUG_ALL_ASSERTIONS 
        assert ((mm + pos) < _algn_max_matrix);
        assert ((pm + pos) < _algn_max_matrix);
        assert ((dm + pos) < _algn_max_direction);
        assert ((htmm + pos) < _algn_max_matrix);
#endif
    dm[pos] = 0;
    { /* Affine gap difficultness */
        if ((0 != gap_row[pos - 1]) && (0 == gap_row[pos]))
            tmp2 = mm[pos - 1];
        else
            tmp2 = mm[pos - 1] + open_gap + gap_row[pos];

        if (((0 == gap_row[pos - 1]) && (0 != gap_row[pos])) ||
            ((0 != gap_row[pos - 1]) && (0 == gap_row[pos])))
            tmp4 = htmm[pos - 1] + open_gap + gap_row[pos];
        else
            tmp4 = htmm[pos - 1] + gap_row[pos];

        if ((((0 == gap_row[pos]) && (0 != c)) ||
                ((0 != gap_row[pos]) && (0 == c))) &&
                ((0 == gap_row[pos - 1]) || (0 == cprev)))
            tmp3 = pm[pos - 1] + open_gap + alg_row[pos];
        else
            tmp3 = pm[pos - 1] + alg_row[pos];
    }
    if (tmp2 <= tmp4) 
        algn_assign_dm(dm,pos,ALIGN_H);
    else {
        tmp2 = tmp4;
        algn_assign_dm(dm,pos,INSERT_H);
    }
    htmm[pos] = tmp2;
    dnmm[pos] = HIGH_NUM;
    /* check whether insertion is better */
    if (tmp2 < tmp3) {
        mm[pos] = tmp2;
        algn_assign_dm(dm,pos,(INSERT));
    }
    else if (tmp3 < tmp2) {
        mm[pos] = tmp3;
        algn_assign_dm(dm,pos,(ALIGN));
    }
    else {
        mm[pos] = tmp3;
        algn_assign_dm(dm,pos,(INSERT | ALIGN));
    }
    if (!NDEBUG && !NPRINT_DM) {
        /* Print the alignment matrix */
        if (INSERT & dm[pos]) 
            printf ("I");
        if (DELETE & dm[pos])
            printf ("D");
        if (ALIGN & dm[pos])
            printf ("A");
        printf ("\t");
    }
    if (!NDEBUG &&!NPRINT_CM) {
        /* Print the cost matrix */
        printf ("(%d,%d)\t", mm[pos], htmm[pos]);
    }
    if (!NDEBUG && (!NPRINT_DM || !NPRINT_CM))
        printf ("\n");
    return;
}

#ifdef _WIN32
__inline void 
#else
inline void 
#endif
algn_fill_ukk_left_cell_aff (int *mm, const int *pm, const int *gap_row, \
        const int *alg_row, unsigned char *dm, int c, int cprev, int pos, int *dnmm, \
        int *pdnmm, int *htmm, int open_gap) {
    int tmp1, tmp3, tmp5;
    /* try align with substitution */
#ifdef DEBUG_ALL_ASSERTIONS 
        assert ((mm + pos) < _algn_max_matrix);
        assert ((pm + pos) < _algn_max_matrix);
        assert ((dm + pos) < _algn_max_direction);
        assert ((dnmm + pos) < _algn_max_matrix);
        assert ((pdnmm + pos) < _algn_max_matrix);
#endif
    dm[pos] = 0;
    { /* Affine gap difficultness */
        if ((0 != cprev) && (0 == c))
            tmp1 = pm[pos];
        else
            tmp1 = pm[pos] + open_gap + c;

        if (((0 == cprev) && (0 != c)) ||
            ((0 != cprev) && (0 == c)))
            tmp5 = pdnmm[pos] + open_gap + c;
        else
            tmp5 = pdnmm[pos] + c;

        if ((((0 == gap_row[pos]) && (0 != c)) ||
                ((0 != gap_row[pos]) && (0 == c))) &&
                    ((0 == gap_row[pos - 1]) || (0 == cprev)))
            tmp3 = pm[pos - 1] + open_gap + alg_row[pos];
        else
            tmp3 = pm[pos - 1] + alg_row[pos];
    }
    if (tmp1 <= tmp5) 
        algn_assign_dm(dm,pos,ALIGN_V);
    if (tmp5 < tmp1) {
        algn_assign_dm(dm,pos,DELETE_V);
        tmp1 = tmp5;
    }
    dnmm[pos] = tmp1;
    htmm[pos] = HIGH_NUM;
        if (tmp1 < tmp3) {
            mm[pos] = tmp1;
            algn_assign_dm(dm,pos,(DELETE));
        } 
        else if (tmp3 < tmp1) {
            mm[pos] = tmp3;
            algn_assign_dm(dm,pos,(ALIGN));
        } 
        else {
            mm[pos] = tmp1;
            algn_assign_dm(dm,pos,(ALIGN | DELETE));
        }
    if (!NDEBUG && !NPRINT_DM) {
        /* Print the alignment matrix */
        if (INSERT & dm[pos]) 
            printf ("I");
        if (DELETE & dm[pos])
            printf ("D");
        if (ALIGN & dm[pos])
            printf ("A");
        printf ("\t");
    }
    if (!NDEBUG &&!NPRINT_CM) {
        /* Print the cost matrix */
        printf ("(%d,,%d)\t", mm[pos], dnmm[pos]);
    }
    return;
}

#ifdef _WIN32
__inline void
#else
inline void
#endif
algn_fill_last_column_aff (int *mm, const int *pm, int tlc, int tlcprev, \
        int l, unsigned char *dm, int *dnmm, const int *pdnmm, int open_gap) {
    int cst, tmp2;
#ifdef DEBUG_ALL_ASSERTIONS 
        assert ((mm + l) < _algn_max_matrix);
        assert ((pm + l) < _algn_max_matrix);
        assert ((dm + l) < _algn_max_direction);
        assert ((dnmm + l) < _algn_max_matrix);
        assert ((pdnmm + l) < _algn_max_matrix);
#endif
    tmp2 = pm[l] + tlc + open_gap;
    { /* Affine gap difficultness */
        if (((0 == tlcprev) && (0 != tlc)) ||
            ((0 != tlcprev) && (0 == tlc)))
            cst = pdnmm[l] + open_gap + tlc;
        else
            cst = pdnmm[l] + tlc;
    }
    if (cst < tmp2)
        algn_assign_dm(dm,l,DELETE_V);
    else {
        cst = tmp2;
        algn_assign_dm(dm,l,ALIGN_V);
    }
    dnmm[l] = cst;
    if (cst < mm[l]) {
        mm[l] = cst;
        algn_assign_dm(dm,l,(DELETE));
    } 
    else if (cst == mm[l])
        algn_assign_dm(dm,l,DELETE);
    return;
}

#ifdef _WIN32
__inline void
#else
inline void
#endif
algn_fill_full_row_aff (int *mm, const int *pm, const int *gap_row, \
        const int *alg_row, unsigned char *dm, int c, int cprev, int tlc, \
        int tlcprev, int l, int *dnmm, const int *pdnmm, int *htmm, \
        int open_gap) {
    /* first entry is delete */
    htmm[0] = HIGH_NUM;
    mm[0] += c;
    dm[0] = DELETE | DELETE_V;
    dnmm[0] = c + pdnmm[0];
    if ((!NDEBUG) && (!NPRINT_CM))
        printf ("%d\t", mm[0]);
    if ((!NDEBUG) && (!NPRINT_DM))
        printf ("D\t");
    algn_fill_row_aff (mm, pm, gap_row, alg_row, dm, c, cprev, 1, l - 1, \
            dnmm, pdnmm, htmm, open_gap);
    algn_fill_last_column_aff (mm, pm, tlc, tlcprev, l - 1, dm, dnmm, pdnmm, \
            open_gap);
    return;
}

void
algn_fill_first_row_aff (int *mm, unsigned char *dm, int len, int const *gap_row, \
        int *dnmm, int *htmm, int open_gap) {
    int i;
    /* We fill the first cell to start with */
    mm[0] = open_gap;
    dnmm[0] = htmm[0] = HIGH_NUM;
    dm[0] = ALIGN | ALIGN_V | ALIGN_H;
    /* Now the rest of the row */
    if ((!NDEBUG) && (!NPRINT_DM))
        printf ("A\t");
    if ((!NDEBUG) && (!NPRINT_CM))
        printf ("%d\t", mm[0]);
    for (i = 1; i < len; i++) {
        dnmm[i] = HIGH_NUM;
        mm[i] = mm[i - 1] + gap_row[i];
        dm[i] = INSERT | (INSERT_H);
        if ((!NDEBUG) && (!NPRINT_DM))
            printf ("I\t");
        if ((!NDEBUG) && (!NPRINT_CM))
            printf ("%d\t", mm[i]);
    }
    return;
}

void
algn_fill_first_cell_aff (int *mm, int pm, unsigned char *dm, int gap, int *dnmm, \
        int *pdnmm, int *htmm) {
    htmm[0] = HIGH_NUM;
    mm[0] += gap;
    *dm = DELETE | DELETE_V;
    dnmm[0] = gap + pdnmm[0];
    if ((!NDEBUG) && (!NPRINT_DM))
        printf ("D\t");
    if ((!NDEBUG) && (!NPRINT_CM))
        printf ("%d\t", *mm);
    return;
}

/* In the following three functions, we maintain the following invariants in
 * each loop:
 * 1. mm is a row that has not been filled and is the next to be.
 * 4. dm is the current row of the direction matrix
 * 2. pm is the previous row, located right above mm, which has been filled
 * already.
 * 3. i is the number of the row of mm in its containing matrix
 * 5. gap_row is the cost of aligning each base of s2 with a gap. This is
 * constant for all the loop.
 * 6. cur_s1 is the i'th base of s1
 * 7. const_val is the cost of cur_s1 aligned with a gap
 * 8. alg_row is the vector of costs of aligning s2 with cur_s1
 */

int *
algn_fill_extending_right_aff (const seqt s1, int *prec, int s1_len, \
        int s2_len, int *mm, int *pm, unsigned char *dm, const cmt c, int start_row, \
        int end_row, int len, int *dnmm, int *pdnmm, int *htmm, int open_gap) {
    int i;
    int *tmp, *tmp1, cur_s1, const_val, prev_s1, prev_const_val;
    const int *gap_row, *alg_row;
    /** Invariants block 
     * len is the number of items in the row to be filled **/
    i = start_row;
    /* This is what we will perform conceptually, I will stop using the
     * cm_get_precal_row function to speed this up a little bit 
    gap_row = cm_get_precal_row (prec, cm_get_gap (c), s2_len);
    */
    gap_row = prec + (c->gap * s2_len);
    while (i < end_row) {
        /** Invariants block */
        assert (i > 0);
        prev_s1 = s1->begin[i - 1];
        cur_s1 = s1->begin[i];
        const_val = cm_calc_cost (c->cost, cur_s1, c->gap, c->lcm);
        prev_const_val = cm_calc_cost (c->cost, prev_s1, c->gap, c->lcm);
        /* This is conceptually what we do in the next line 
        alg_row = cm_get_precal_row (prec, cur_s1, s2_len);
        */
        alg_row = prec + (cur_s1 * s2_len);
        /* Align! */
        algn_fill_first_cell_aff (mm, pm[0], dm, alg_row[0], dnmm, pdnmm, htmm);
        algn_fill_row_aff (mm, pm, gap_row, alg_row, dm, const_val, \
                prev_const_val, 1, len - 2, dnmm, pdnmm, htmm, open_gap);
        algn_fill_ukk_right_cell_aff (mm, pm, gap_row, alg_row, dm, \
                const_val, prev_const_val, len - 1, dnmm, htmm, open_gap);
        /** Invariants block */
        tmp = mm;
        tmp1 = dnmm;
        mm = pm;
        dnmm = pdnmm;
        pm = tmp;
        pdnmm = tmp1;
        i++;
        dm += s2_len;
        len++;
        mm[0] = pm[0];
    }
    return (mm);
}

int *
algn_fill_extending_left_right_aff (const seqt s1, int *prec, int s1_len, \
        int s2_len,  int *mm, int *pm, unsigned char *dm, const cmt c, int start_row, \
        int end_row, int start_column, int len, int *dnmm, int *pdnmm, \
        int *htmm, int open_gap) {
    int i;
    int *tmp, *tmp1, cur_s1, const_val, prev_s1, prev_const_val;
    const int *gap_row, *alg_row;
    /** Invariants block 
     * len is the number of cells to fill in the current row minus 1
     * start_column is the first cell to fill in the row */
    i = start_row;
    /* Conceptually 
    gap_row = cm_get_precal_row (prec, cm_get_gap (c), s2_len);
    */
    gap_row = prec + (c->gap * s2_len);
    len--;
    while (i < end_row) {
        /** Invariants block */
        assert (i > 0);
        prev_s1 = s1->begin[i - 1];
        cur_s1 = s1->begin[i];
        const_val = cm_calc_cost (c->cost, cur_s1, c->gap, c->lcm);
        prev_const_val = cm_calc_cost (c->cost, prev_s1, c->gap, c->lcm);
        /* Conceptually 
        alg_row = cm_get_precal_row (prec, cur_s1, s2_len);
        */
        alg_row = prec + (cur_s1 * s2_len);
        /* Align! */
        algn_fill_ukk_left_cell_aff (mm, pm, gap_row, alg_row, dm, const_val, \
                prev_const_val, start_column, dnmm, pdnmm, htmm, open_gap);
        algn_fill_row_aff (mm, pm, gap_row, alg_row, dm, const_val, \
                prev_const_val, start_column + 1, start_column + (len - 2), \
                dnmm, pdnmm, htmm, open_gap);
        algn_fill_ukk_right_cell_aff (mm, pm, gap_row, alg_row, dm, const_val, \
                prev_const_val, start_column + len - 1, dnmm, htmm, open_gap);
        /** Invariants block */
        tmp = mm;
        tmp1 = dnmm;
        mm = pm;
        dnmm = pdnmm;
        pm = tmp;
        pdnmm = tmp1;
        i++;
        dm += s2_len;
        start_column++;
    }
    return (mm);
}

int *
algn_fill_extending_left_aff (const seqt s1, int *prec, int s1_len, \
        int s2_len,  int *mm, int *pm, unsigned char *dm, const cmt c, int start_row, \
        int end_row, int start_column, int len, int *dnmm, int *pdnmm, \
        int *htmm, int open_gap) {
    int i;
    int *tmp, *tmp1, cur_s1, const_val, prev_s1, prev_const_val, \
        const_val_tail, prev_const_val_tail;
    const int *gap_row, *alg_row;
    /** Invariants block 
     * start_column is the first cell to fill in the row 
     * len is the number of cells to fill in the current row minus 1 */
    i = start_row;
    /* Conceptually 
    gap_row = cm_get_precal_row (prec, cm_get_gap (c), s2_len);
    */
    gap_row = prec + (c->gap * s2_len);
    while (i < end_row) {
        /** Invariants block */ 
        assert (i > 0);
        prev_s1 = s1->begin[i - 1];
        cur_s1 = s1->begin[i];
        prev_const_val = cm_calc_cost (c->cost, prev_s1, c->gap, c->lcm);
        const_val = cm_calc_cost (c->cost, cur_s1, c->gap, c->lcm);
        const_val_tail = (cm_get_tail_cost (c))[cur_s1];
        prev_const_val_tail = (cm_get_tail_cost (c))[prev_s1];
        /* Conceptually 
        alg_row = cm_get_precal_row (prec, cur_s1, s2_len);
        */
        alg_row = prec + (cur_s1 * s2_len);
        /* Align! */
        algn_fill_ukk_left_cell_aff (mm, pm, gap_row, alg_row, dm, const_val, \
                prev_const_val, start_column, dnmm, pdnmm, htmm, open_gap);
        algn_fill_row_aff (mm, pm, gap_row, alg_row, dm, const_val, prev_const_val, \
                start_column + 1, start_column + len - 1, dnmm, pdnmm, htmm, \
                open_gap);
        algn_fill_last_column_aff (mm, pm, const_val_tail, prev_const_val_tail, \
                start_column + len - 1, dm, dnmm, pdnmm, open_gap);
        /** Invariants block */
        tmp = mm;
        tmp1 = dnmm;
        mm = pm;
        dnmm = pdnmm;
        pm = tmp;
        pdnmm = tmp1;
        i++;
        dm += s2_len;
        start_column++;
        len--;
    }
    if (!NDEBUG && !NPRINT_CM) {
        printf ("S2 gap cost\n");
        fflush (stdout);
        for (i = 0; i < s2_len; i++) {
            printf ("%d\t", gap_row[i]);
            fflush (stdout);
        }
        fflush (stdout);
    }

    return (mm);
}

int *
algn_fill_no_extending_aff (const seqt s1, int *prec, int s1_len, \
        int s2_len,  int *mm, int *pm, unsigned char *dm, const cmt c, int start_row, \
        int end_row, int *dnmm, int *pdnmm, int *htmm, int open_gap) {
    int i;
    int *tmp, cur_s1, const_val, const_val_tail, prev_s1, prev_const_val, \
        prev_const_val_tail, *tmp1;
    const int *gap_row, *alg_row;
    /** Invariants block */
    i = start_row;
    /* Conceptually 
    gap_row = cm_get_precal_row (prec, cm_get_gap (c), s2_len);
    */
    gap_row = prec + (c->gap * s2_len);
    while (i < end_row) {
        /** Invariants block */
        assert (i > 0);
        prev_s1 = s1->begin[i - 1];
        cur_s1 = s1->begin[i];
        const_val = cm_calc_cost (c->cost, cur_s1, c->gap, c->lcm);
        prev_const_val = cm_calc_cost (c->cost, prev_s1, c->gap, c->lcm);
        const_val_tail = (cm_get_tail_cost (c))[cur_s1];
        prev_const_val_tail = (cm_get_tail_cost (c))[prev_s1];
        /* Conceptually 
        alg_row = cm_get_precal_row (prec, cur_s1, s2_len);
        */
        alg_row = prec + (cur_s1 * s2_len);
        /* Align! */
        algn_fill_first_cell_aff (mm, pm[0], dm, open_gap, dnmm, pdnmm, htmm);
        algn_fill_row_aff (mm, pm, gap_row, alg_row, dm, const_val, \
                prev_const_val, 1, s2_len - 1, dnmm, pdnmm, htmm, open_gap);
        algn_fill_last_column_aff (mm, pm, const_val_tail, \
                prev_const_val_tail, s2_len - 1, dm, dnmm, \
                pdnmm, open_gap);
        /** Invariants block */
        tmp = mm;
        tmp1 = dnmm;
        mm = pm;
        dnmm = pdnmm;
        pm = tmp;
        pdnmm = tmp1;
        i++;
        dm += s2_len;
    }
    return (mm);
}

/* Simmilar to the previous but when no barriers are set */
#ifdef _WIN32
__inline int
#else
inline int
#endif
algn_fill_plane_aff (const seqt s1, int *prec, int s1_len, \
        int s2_len, int *mm, unsigned char *dm, const cmt c, int *dnmm, int *htmm, \
        int open_gap) {
    int i;
    const int *alg_row;
    int const_val, const_val_tail, prev_const_val, prev_const_val_tail, \
        *nm, *tmp, *tmp1, *pdnmm;
    const int *gap_row, *first_gap_row;
    int gapcode;
    /* A precalculated cost of a gap aligned with each base in the array */
    gapcode = cm_get_gap (c);
    gap_row = cm_get_precal_row (prec, gapcode, s2_len);
    first_gap_row = cm_get_precal_row (prec, 0, s2_len);
    nm = mm;
    pdnmm = dnmm;
    mm[0] = open_gap;
    dm[0] = ALIGN | ALIGN_H | ALIGN_V;
    htmm[0] = HIGH_NUM;
    dnmm[0] = HIGH_NUM;
    if ((!NDEBUG) && (!NPRINT_CM))
        printf ("%d\t", mm[0]);
    if ((!NDEBUG) && (!NPRINT_DM))
        printf ("A\t");
    /* We fill the first row to start with */
    for (i = 1; i < s2_len; i++) {
        dnmm[i] = HIGH_NUM;
        mm[i] = mm[i - 1] + first_gap_row[i];
        dm[i] = INSERT | INSERT_H;
        if ((!NDEBUG) && (!NPRINT_CM)) {
            printf ("%d\t", mm[i]);
            fflush (stdout);
        }
        if ((!NDEBUG) && (!NPRINT_DM))
            printf ("I\t");
    }
    mm += s2_len;
    mm[0] = nm[0];
    nm[0] = 0;
    if (!NDEBUG && (!NPRINT_DM || !NPRINT_CM)) {
        printf ("\n");
        fflush (stdout);
    }
    dnmm += s2_len;
    /* Now we fill the rest of the matrix */
    for (i = 1, dm += s2_len; i < s1_len; i++, dm += s2_len) {
        prev_const_val_tail = (cm_get_tail_cost (c))[seq_get(s1, i - 1)];
        prev_const_val = cm_calc_cost (c->cost, seq_get(s1, i - 1), c->gap, c->lcm);
        const_val_tail = (cm_get_tail_cost (c))[seq_get(s1, i)];
        const_val = cm_calc_cost (c->cost, seq_get(s1, i), c->gap, c->lcm);
        alg_row = cm_get_precal_row (prec, seq_get (s1, i), s2_len);
        algn_fill_full_row_aff (mm, nm, gap_row, alg_row, dm, const_val, \
                prev_const_val, const_val_tail, prev_const_val_tail, s2_len, \
                dnmm, pdnmm, htmm, open_gap);
        if (!NDEBUG && !NPRINT_CM) {
            printf ("\n");
            fflush (stdout);
        }
        /* We swap mm and nm for the next round */
        tmp = mm;
        tmp1 = dnmm;
        mm = nm;
        dnmm = pdnmm;
        nm = tmp;
        pdnmm = tmp1;
        mm[0] = nm[0];
    }
    return (nm[s2_len - 1]);
}

#ifdef _WIN32
__inline void
#else
inline void
#endif
algn_choose_aff_other (int *next_row, int *mm, int **next_dnmm, \
        int **next_pdnmm, int *dnmm, int *pdnmm) {
    if (next_row == mm) {
        *next_dnmm = dnmm;
        *next_pdnmm = pdnmm;
    }
    else {
        *next_dnmm = pdnmm;
        *next_pdnmm = dnmm;
    }
    return;
}

#ifdef _WIN32
__inline int
#else
inline int
#endif
algn_fill_plane_2_aff (const seqt s1, int *prec, int s1_len, int s2_len, int *mm, \
        unsigned char *dm, const cmt c, int width, int height, int dwidth_height, \
        int *dnmm, int *htmm) {
    int *next_row, *next_pm, *next_dnmm, *next_pdnmm;
    int *a, *b, *d, *e, open_gap;
    int const *gap_row;
    int start_row, final_row, start_column, length;
    unsigned char *to_go_dm;
    open_gap = cm_get_gap_opening_parameter (c);
    width = width + dwidth_height;
    height = height + dwidth_height;
    a = mm;
    b = mm + s2_len;
    d = dnmm;
    e = dnmm + s2_len;
    gap_row = cm_get_precal_row (prec, 0, s2_len); /* We want the horizontal row */
    /* We have to consider three cases in this new alignment procedure (much
     * cleaner than the previous): 
     * 
     * Case 1:
     * If s1 is much longer than s2, then there is no point on using the
     * barriers, we rather fill the full matrix in one shot */
    if (((float) s1_len) >= (((float) ((float) 3 / (float) 2) * (float) s2_len)))
        return (algn_fill_plane_aff (s1, prec, s1_len, s2_len, mm, dm, c, d, \
                    htmm, open_gap));
    /* Case 2:
     * There are no full rows to be filled, therefore we have to break the
     * procedure in three different subsets */
    else if ((2 * height) < s1_len) {
        algn_fill_first_row_aff (a, dm, width, gap_row, d, htmm, open_gap);
        b[0] = a[0];
        a[0] = 0;
        start_row = 1;
        final_row = height;
        start_column = 0; 
        length = width + 1;
        to_go_dm = dm + (start_row * s2_len);
        /* Now we fill that space */
        next_row = algn_fill_extending_right_aff (s1, prec, s1_len, s2_len, b, a, \
                to_go_dm, c, start_row, final_row, length, e, d, htmm, open_gap);
        next_pm = choose_other (next_row, a, b);
        algn_choose_aff_other (next_row, mm, &next_dnmm, &next_pdnmm, d, e);
        /* Next group */
        start_row = final_row;
        final_row = s1_len - (height - 1);
        start_column = 1;
        length = width + height;
        to_go_dm = dm + (start_row * s2_len);
        next_row = 
            algn_fill_extending_left_right_aff (s1, prec, s1_len, \
                    s2_len, next_row, next_pm, to_go_dm, c, start_row, \
                    final_row, start_column, length, next_dnmm, next_pdnmm, \
                    htmm, open_gap);
        next_pm = choose_other (next_row, a, b);
        algn_choose_aff_other (next_row, mm, &next_dnmm, &next_pdnmm, d, e);
        /* The final group */
        start_row = final_row;
        final_row = s1_len;
        length = length - 2;
        start_column = s2_len - length;
        to_go_dm = dm + (start_row * s2_len);
        next_row = algn_fill_extending_left_aff (s1, prec, s1_len, s2_len, \
                next_row, next_pm, to_go_dm, c, start_row, final_row, \
                start_column, length, next_dnmm, next_pdnmm, htmm, open_gap);
        next_pm = choose_other (next_row, a, b);
        algn_choose_aff_other (next_row, mm, &next_dnmm, &next_pdnmm, d, e);
    }
    /* Case 3: (final case)
     * There is a block in the middle of with full rows that have to be filled
     * */
    else {
        /* We will simplify this case even further, if the size of the leftover
         * is too small, don't use the barriers at all, just fill it up all */
        if (8 >= (s1_len - height))
            return (algn_fill_plane_aff (s1, prec, s1_len, s2_len, mm, dm, c, \
                        d, htmm, open_gap));
        else {
            algn_fill_first_row_aff (mm, dm, width, gap_row, dnmm, htmm, open_gap);
            b[0] = mm[0];
            mm[0] = 0;
            start_row = 1;
            final_row = (s2_len - width) + 1;
            start_column = 0;
            length = width + 1;
            to_go_dm = dm + (s2_len * start_row);
            next_row = algn_fill_extending_right_aff (s1, prec, s1_len, s2_len, \
                    b, a, to_go_dm, c, start_row, final_row, length, e, d, htmm, \
                    open_gap);
            next_pm = choose_other (next_row, a, b);
            algn_choose_aff_other (next_row, mm, &next_dnmm, &next_pdnmm, d, e);
            start_row = final_row;
            final_row = s1_len - (s2_len - width) + 1;
            length = s2_len;
            to_go_dm = dm + (s2_len * start_row);
            next_row = algn_fill_no_extending_aff (s1, prec, s1_len, s2_len, \
                    next_row, next_pm, to_go_dm, c, start_row, \
                    final_row, next_dnmm, next_pdnmm, htmm, open_gap);
            next_pm = choose_other (next_row, a, b);
            algn_choose_aff_other (next_row, mm, &next_dnmm, &next_pdnmm, d, e);
            start_row = final_row;
            final_row = s1_len;
            start_column = 1;
            length = s2_len - 1;
            to_go_dm = dm + (s2_len * start_row);
            next_row = 
                algn_fill_extending_left_aff (s1, prec, s1_len, s2_len, \
                    next_row, next_pm, to_go_dm, c, start_row, final_row, \
                    start_column, length, next_dnmm, next_pdnmm, htmm, \
                    open_gap);
            next_pm = choose_other (next_row, a, b);
        }
    }
    return (next_pm[s2_len - 1]);
}
/******************************************************************************/

/** Fill parallel must have been called before */
#ifdef _WIN32
static __inline void
#else
static inline void
#endif
fill_moved (int s3_len, const int *prev_m, const int *upper_m, \
        const int *diag_m, const int *s1gs3, const int *gs2s3, \
        const int *s1s2s3, int *mm, unsigned char *dm) {
    int k, tmp0, tmp1, tmp;
    for (k = 1; k < s3_len; k++) {
        tmp0 = upper_m[k] + s1gs3[k];
        if (tmp0 < mm[k]) {
            mm[k] = tmp0;
            dm[k] = S3;
        }
        tmp = prev_m[k] + gs2s3[k];
        if (tmp < mm[k]) {
            mm[k] = tmp;
            dm[k] = S1;
        }
        tmp1 = diag_m[k] + s1s2s3[k];
        if (tmp1 < mm[k]) {
            mm[k] = tmp1;
            dm[k] = S2;
        }
    }
}

#ifdef _WIN32
__inline void
#else
inline void
#endif
fill_parallel (int s3_len, const int *prev_m, const int *upper_m, \
        const int *diag_m, int s1gg, int gs2g, int s1s2g, int *mm, \
        unsigned char *dm) {
    int k, tmp1, tmp;
    for (k = 0; k < s3_len; k++) {
        mm[k] = upper_m[k] + s1gg;
        dm[k] = P3;
        tmp = prev_m[k] + gs2g;
        if (tmp < mm[k]) {
            mm[k] = tmp;
            dm[k] = P1;
        }
        tmp1 = diag_m[k] + s1s2g;
        if (tmp1 < mm[k]) {
            mm[k] = tmp1;
            dm[k] = P2;
        }
    }
}

/*
 * s1 is a pointer to the sequence s1 (vertical), and s2 (horizontal 1) defined
 * in the same way. prec is a pointer to the three dimensional matrix that holds
 * the alignment values for all the combinations of the alphabet of sequences
 * s1, s2 and s3, with the sequence s3 (see cm_precalc_4algn_3d for more
 * information). s1_len, s2_len and s3_len is the length of the three sequences
 * to be aligned, and *mm is the first element of the alignment cube that will
 * cold the matrix of the dynamic programming algorithm, while dm does the same
 * job, holding the direction information for the backtrack. uk is the value of
 * the Ukkonen barriers (not used in this version of the program 
 * conside all combinations
 * s1, g, g -> const for plane
 * g, s2, g -> const const per row
 * s1, s2, g -> const const per row
 * g, s2, s3 -> vector (changes on each row)
 * s1, g, s3 -> vector (change per plane)
 * s1, s2, s3 -> vector (changes on each row)
 * g, g, s3 -> vector (the last one to be done, not parallelizable
 *
 */
#ifdef _WIN32
__inline int
#else
inline int
#endif
algn_fill_cube (const seqt s1, const seqt s2, const int *prec, \
        int s1_len, int s2_len, int s3_len, int *mm, unsigned char *dm, int uk, \
        int gap, int a_sz) {
    int *s1p, *s2p;
    /* Each of the following arrays hold some precalculated value for the
     * sequence s3 which is not passed as argument. */
    const int *gs2s3;     /** Align a gap and the current base of s2 with s3 */
    const int *s1gs3;     /** Align the current base of s1 and a gap with s3 */
    const int *s1s2s3;    /** Align the current bases of s1 and s2 with s3 */
    const int *ggs3;      /** Align two gaps with s3 */
    /* Each of the following arrays hold the arrays of the three dimensional
     * matrix that are located around the row that is filled on each iteration.
     * These rows are already filled in some previous iteration and used in 
     * the dynamic programming matrix. */
    int *upper_m;       /** The row in the upper plane of the cube */
    int *prev_m;        /** The row behind the current row in the same plane */
    int *diag_m;        /** The upper_m relative to prev_m */
    int *tmp_mm;        /** A temporary pointer to the row that is being filled 
                        currently */
    unsigned char *tmp_dm;       /** Same as previous for dm */
    int i, j, k, tmp;

    s1p = seq_get_begin (s1);
    s2p = seq_get_begin (s2);
    tmp_dm = dm;
    tmp_mm = mm;
    upper_m = mm + s3_len;
    diag_m = mm;
    if (!NDEBUG) 
        printf ("Three dimensional sequence alignment matrix.\n");
    /* Fill the first plane only at the beginning, this is special */
    {
        mm[0] = 0;              /* Fill the first cell, of course to 0 */
        dm[0] = S2;

        /* Fill first row based on precalculated row.
         * The first row consists of aligning s3 with gaps, we have that
         * precalculated, so all we really need is to add up that vector.*/
        ggs3 = cm_get_row_precalc_3d (prec, s3_len, a_sz, gap, gap);
        for (i = 1; i < s3_len; i++) {
            mm[i] = mm[i - 1] + ggs3[i];
            dm[i] = SS;
        }

        prev_m = mm;
        mm += s3_len;
        dm += s3_len;

        /* Finish filling the first plane.
         * In this plane filling, all we really need to deal with is aligning s2
         * and s3, as the first plane holds the inital gap of s1. */
        for (i = 1; i < s2_len; i++, prev_m += s3_len, mm += s3_len, dm += s3_len) {
            gs2s3 = cm_get_row_precalc_3d (prec, s3_len, a_sz, gap, s2p[i]);

            /** Fill the first cell with the cost of extending the gap from the
             * previous row */
            mm[0] = prev_m[0] + gs2s3[0];
            dm[0] = P1;
            /* Everyone else requires the three comparisons we used when filling
             * rows as usual in a two dimensional alignment. Note that this code
             * is almost the same as algn_fill_row, so if something is modified
             * there, there will be need of modifying it in the same way here. */
            /* TODO: Add the ukkonen barriers */
            for (j = 1; j < s3_len; j++) {
                mm[j] = prev_m[j] + gs2s3[0];
                dm[j] = P1;
                tmp = prev_m[j - 1] + gs2s3[j];
                if (tmp < mm[j]) {
                    mm[j] = tmp;
                    dm[j] = S1;
                }
                tmp = mm[j - 1] + ggs3[j];
                if (tmp < mm[j]) {
                    mm[j] = tmp;
                    dm[j] = SS;
                }
            }
        }
        if (!NDEBUG && !NPRINT_CM) {  /* Printing the cost matrix */
            int *tmp_mm_debug;
            tmp_mm_debug = tmp_mm;
            printf ("\n");
            for (i = 0; i < s2_len; i++) {
                for (j = 0; j < s3_len; j++)
                    printf ("%d\t", tmp_mm_debug[j]);
                tmp_mm_debug += s2_len;
                printf ("\n");
            }
            printf ("\n");
        }
    } /* Finished the first plane filling */

    /* Fill plane by plane */
    mm = tmp_mm + (s3_len * s2_len);
    dm = tmp_dm + (s3_len * s2_len);
    diag_m = tmp_mm;
    upper_m = tmp_mm + s3_len;
    prev_m = mm - s3_len;
    for (i = 1; i < s1_len; i++) { /* For each plane */
        int s1_it; /* The element in s1 represented by this plane */
        s1_it = s1p[i];
        s1gs3 = cm_get_row_precalc_3d (prec, s3_len, a_sz, s1_it, gap);
        /* Filling the first row of the current plane. */
        {   
            /* This requires only three operations, equivalent to the
             2-dimensional alignment (the three for loops) */
            mm[0] = diag_m[0] + s1gs3[0]; /* diag is upper in this step */
            dm[0] = P3;
            if (!NDEBUG && !NPRINT_CM)
                printf ("%d\t", mm[0]);
            for (j = 1, k = 0; j < s3_len; j++, k++) {
                mm[j] = diag_m[j] + s1gs3[0];
                dm[j] = P3;
                tmp = diag_m[k] + s1gs3[j];
                if (tmp < mm[j]) {
                    mm[j] = tmp;
                    dm[j] = S3;
                }
                tmp = ggs3[j] + mm[k];
                if (tmp < mm[j]) {
                    mm[j] = tmp;
                    dm[j] = SS;
                }
                if (!NDEBUG && !NPRINT_CM) 
                    printf ("%d\t", mm[j]);
            }
            if (!NDEBUG)
                printf ("\n");
            /* Now we should move to the next row to continue filling the matrix
             * */
            dm += s3_len;
            mm += s3_len;
        }
        /* Now, go on with the rest of the rows.  On each row, mm is the row
         * being constructed, prev_m is the previous row in the same horizontal
         * plane, upper_m is the previous row in the vertical plane and diag_m
         * is the row in the previous planes (vertical and horizontal).
         */
        for (j = 1; j < s2_len; j++, diag_m += s3_len, 
                upper_m += s3_len, prev_m += s3_len, mm += s3_len, 
                dm += s3_len) {
            /* We first set the vectors that are needed */
            int s2_it;
            s2_it = s2p[j];
            gs2s3 = cm_get_row_precalc_3d (prec, s3_len, a_sz, gap, s2_it);
            s1s2s3 = cm_get_row_precalc_3d (prec, s3_len, a_sz, s1_it, s2_it);
            fill_parallel (s3_len, prev_m, upper_m, diag_m, s1gs3[0], gs2s3[0], 
                    s1s2s3[0], mm, dm);
            fill_moved (s3_len, prev_m - 1, upper_m - 1, diag_m - 1, s1gs3, \
                    gs2s3, s1s2s3, mm, dm);
            /* In the final step we run over the array filling the self check.
             * */
            if (!NDEBUG)
                printf ("%d\t", mm[0]);
            for (k = 1; k < s3_len; k++) {
                tmp = mm[k - 1] + ggs3[k];
                if (tmp < mm[k]) {
                    mm[k] = tmp;
                    dm[k] = SS;
                }
                if (!NDEBUG)
                    printf ("%d\t", mm[k]);
            }
            if (!NDEBUG)
                printf ("\n");
        }
        if (!NDEBUG)
            printf ("\n");
    }
    return (mm[-1]); /** We return the last item in the previous row */
}

/* Same as the previous function but with Ukkonen barriers turned on. The
 * additional parameters are:
 * @param w is the maximum width to be filled.
 * @param d is the maximum depth to be filled
 * @param h is the maximum height to be filled.
 * TODO: Finish this implementation, I will stop now to test the sequence
 * analysis procedures in the rest of POY.
 * */
#ifdef _WIN32
__inline int
#else
inline int
#endif
algn_fill_cube_ukk (const seqt s1, const seqt s2, const int *prec, \
        int s1_len, int s2_len, int s3_len, int *mm, unsigned char *dm, int uk, \
        int gap, int a_sz, int w, int d, int h) {
    int *s1p, *s2p;
    /* Each of the following arrays hold some precalculated value for the
     * sequence s3 which is not passed as argument. */
    const int *gs2s3;     /** Align a gap and the current base of s2 with s3 */
    const int *s1gs3;     /** Align the current base of s1 and a gap with s3 */
    const int *s1s2s3;    /** Align the current bases of s1 and s2 with s3 */
    const int *ggs3;      /** Align two gaps with s3 */
    /* Each of the following arrays hold the arrays of the three dimensional
     * matrix that are located around the row that is filled on each iteration.
     * These rows are already filled in some previous iteration and used in 
     * the dynamic programming matrix. */
    int *upper_m;       /** The row in the upper plane of the cube */
    int *prev_m;        /** The row behind the current row in the same plane */
    int *diag_m;        /** The upper_m relative to prev_m */
    int *tmp_mm;        /** A temporary pointer to the row that is being filled 
                        currently */
    unsigned char *tmp_dm;       /** Same as previous for dm */
    int i, j, k, tmp;

    s1p = seq_get_begin (s1);
    s2p = seq_get_begin (s2);
    tmp_dm = dm;
    tmp_mm = mm;
    upper_m = mm + s3_len;
    diag_m = mm;
    if (!NDEBUG) 
        printf ("Three dimensional sequence alignment matrix.\n");
    /* Fill the first plane only at the beginning, this is special */
    {
        mm[0] = 0;              /* Fill the first cell, of course to 0 */
        dm[0] = S2;

        /* Fill first row based on precalculated row.
         * The first row consists of aligning s3 with gaps, we have that
         * precalculated, so all we really need is to add up that vector.*/
        ggs3 = cm_get_row_precalc_3d (prec, s3_len, a_sz, gap, gap);
        for (i = 1; i < s3_len; i++) {
            mm[i] = mm[i - 1] + ggs3[i];
            dm[i] = SS;
        }

        prev_m = mm;
        mm += s3_len;
        dm += s3_len;

        /* Finish filling the first plane.
         * In this plane filling, all we really need to deal with is aligning s2
         * and s3, as the first plane holds the inital gap of s1. */
        for (i = 1; i < s2_len; i++, prev_m += s3_len, mm += s3_len, dm += s3_len) {
            gs2s3 = cm_get_row_precalc_3d (prec, s3_len, a_sz, gap, s2p[i]);

            /** Fill the first cell with the cost of extending the gap from the
             * previous row */
            mm[0] = prev_m[0] + gs2s3[0];
            dm[0] = P1;
            /* Everyone else requires the three comparisons we used when filling
             * rows as usual in a two dimensional alignment. Note that this code
             * is almost the same as algn_fill_row, so if something is modified
             * there, there will be need of modifying it in the same way here. */
            /* TODO: Add the ukkonen barriers */
            for (j = 1; j < s3_len; j++) {
                mm[j] = prev_m[j] + gs2s3[0];
                dm[j] = P1;
                tmp = prev_m[j - 1] + gs2s3[j];
                if (tmp < mm[j]) {
                    mm[j] = tmp;
                    dm[j] = S1;
                }
                tmp = mm[j - 1] + ggs3[j];
                if (tmp < mm[j]) {
                    mm[j] = tmp;
                    dm[j] = SS;
                }
            }
        }
        if (!NDEBUG) {  /* Printing the cost matrix */
            int *tmp_mm_debug;
            tmp_mm_debug = tmp_mm;
            for (i = 0; i < s2_len; i++) {
                for (j = 0; j < s3_len; j++)
                    printf ("%d\t", tmp_mm_debug[j]);
                tmp_mm_debug += s2_len;
                printf ("\n");
            }
            printf ("\n");
        }
    } /* Finished the first plane filling */

    /* Fill plane by plane */
    mm = tmp_mm + (s3_len * s2_len);
    dm = tmp_dm + (s3_len * s2_len);
    diag_m = tmp_mm;
    upper_m = tmp_mm + s3_len;
    prev_m = mm - s3_len;
    for (i = 1; i < s1_len; i++) { /* For each plane */
        int s1_it; /* The element in s1 represented by this plane */
        s1_it = s1p[i];
        s1gs3 = cm_get_row_precalc_3d (prec, s3_len, a_sz, s1_it, gap);
        /* Filling the first row of the current plane. */
        {   
            /* This requires only three operations, equivalent to the
             2-dimensional alignment (the three for loops) */
            mm[0] = diag_m[0] + s1gs3[0]; /* diag is upper in this step */
            dm[0] = P3;
            if (!NDEBUG)
                printf ("%d\t", mm[0]);
            for (j = 1, k = 0; j < s3_len; j++, k++) {
                mm[j] = diag_m[j] + s1gs3[0];
                dm[j] = P3;
                tmp = diag_m[k] + s1gs3[j];
                if (tmp < mm[j]) {
                    mm[j] = tmp;
                    dm[j] = S3;
                }
                tmp = ggs3[j] + mm[k];
                if (tmp < mm[j]) {
                    mm[j] = tmp;
                    dm[j] = SS;
                }
                if (!NDEBUG) 
                    printf ("%d\t", mm[j]);
            }
            if (!NDEBUG)
                printf ("\n");
            /* Now we should move to the next row to continue filling the matrix
             * */
            dm += s3_len;
            mm += s3_len;
        }
        /* Now, go on with the rest of the rows.  On each row, mm is the row
         * being constructed, prev_m is the previous row in the same horizontal
         * plane, upper_m is the previous row in the vertical plane and diag_m
         * is the row in the previous planes (vertical and horizontal).
         */
        for (j = 1; j < s2_len; j++, diag_m += s3_len, 
                upper_m += s3_len, prev_m += s3_len, mm += s3_len, 
                dm += s3_len) {
            /* We first set the vectors that are needed */
            int s2_it;
            s2_it = s2p[j];
            gs2s3 = cm_get_row_precalc_3d (prec, s3_len, a_sz, gap, s2_it);
            s1s2s3 = cm_get_row_precalc_3d (prec, s3_len, a_sz, s1_it, s2_it);
            fill_parallel (s3_len, prev_m, upper_m, diag_m, s1gs3[0], gs2s3[0], 
                    s1s2s3[0], mm, dm);
            fill_moved (s3_len, prev_m - 1, upper_m - 1, diag_m - 1, s1gs3, \
                    gs2s3, s1s2s3, mm, dm);
            /* In the final step we run over the array filling the self check.
             * */
            if (!NDEBUG)
                printf ("%d\t", mm[0]);
            for (k = 1; k < s3_len; k++) {
                tmp = mm[k - 1] + ggs3[k];
                if (tmp < mm[k]) {
                    mm[k] = tmp;
                    dm[k] = SS;
                }
                if (!NDEBUG)
                    printf ("%d\t", mm[k]);
            }
            if (!NDEBUG)
                printf ("\n");
        }
        if (!NDEBUG)
            printf ("\n");
    }
    return (mm[-1]); /** We return the last item in the previous row */
}

#ifdef _WIN32
__inline int
#else
inline int
#endif
algn_nw_limit (const seqt s1, const seqt s2, const cmt c, \
        matricest m, int deltawh, int st_s1, int len_s1, \
        int st_s2, int len_s2) {
    const int *ss1, *ss2;
    int *mm, *prec, s1_len, s2_len;
    unsigned char *dm;
    ss1 = seq_get_begin (s1);
    ss2 = seq_get_begin (s2);
    mm = mat_get_2d_matrix (m);
    dm = mat_get_2d_direct (m);
    s1_len = seq_get_len (s1);
    s2_len = seq_get_len (s2);
    prec = mat_get_2d_prec (m);
    cm_precalc_4algn (c, m, s2);
    if (cm_get_affine_flag (c))
        return 
            (algn_fill_plane_2_aff (s1, prec, s1_len, s2_len, mm, dm, c, 3, \
                (len_s1 - len_s2) + 3, deltawh, mm + (2 * s2_len), \
                mm + (4 * s2_len)));
    else
    return (algn_fill_plane_2 (s1, prec, s1_len, s2_len, mm, dm, c, 3, 
                (len_s1 - len_s2) + 3, deltawh));
}


#ifdef _WIN32
__inline int
#else
inline int
#endif
algn_nw (const seqt s1, const seqt s2, const cmt c, \
        matricest m, int deltawh) {
    int s1_len, s2_len;
    s1_len = seq_get_len (s1);
    s2_len = seq_get_len (s2);
    assert (s1_len >= s2_len);
    return (algn_nw_limit (s1, s2, c, m, deltawh, 0, s1_len, 0, s2_len));
}

#ifdef _WIN32
__inline int
#else
inline int
#endif
algn_nw_3d (const seqt s1, const seqt s2, const seqt s3,
        const cm_3dt c, matricest m, int w) {
    const int *ss1, *ss2, *ss3;
    int *mm, *prec, s1_len, s2_len, s3_len, gap, res;
    unsigned char *dm;
    ss1 = seq_get_begin (s1);
    ss2 = seq_get_begin (s2);
    ss3 = seq_get_begin (s3);
    mat_setup_size (m, seq_get_len (s2), seq_get_len (s3), seq_get_len (s1), \
            w, c->a_sz);
    mm = mat_get_3d_matrix (m);
    dm = mat_get_3d_direct (m);
    prec = mat_get_3d_prec (m);
    s1_len = seq_get_len (s1);
    s2_len = seq_get_len (s2);
    s3_len = seq_get_len (s3);
    gap = cm_get_gap_3d (c);
    cm_precalc_4algn_3d (c, prec, s3);
    /* TODO Check how is this ukkonen barrier affecting this fill cube, the w
     * was called uk */
    res = algn_fill_cube (s1, s2, prec, s1_len, s2_len, s3_len, mm, dm, w, \
            gap, c->a_sz);
    return res;
}

int
algn_worst_2 (seqt s1, seqt s2, cmt c) {
    int i, res = 0;
    assert ((seq_get_len (s1)) == (seq_get_len (s2)));
    for (i = 0; i < seq_get_len (s1); i++) {
        res += (cm_calc_cost (c->worst, seq_get (s1, i), seq_get (s2, i), c->lcm));
    }
    return (res);
}

value
algn_CAML_worst_2 (value s1, value s2, value c) {
    CAMLparam3(s1, s2, c);
    cmt tc;
    int res;
    seqt s1p, s2p;
    s1p = Seq_struct(s1);
    s2p = Seq_struct(s2);
    tc = Cost_matrix_struct(c);
    res = algn_worst_2 (s1p, s2p, tc);
    CAMLreturn(Val_int(res));
}

value 
algn_CAML_simple_2 (value s1, value s2, value c, value a, value deltawh) {
    CAMLparam5(s1, s2, c, a, deltawh);
    seqt s1p, s2p;
    int res;
    cmt tc;
    matricest ta;
    tc = Cost_matrix_struct(c);
    ta = Matrices_struct(a);
    s1p = Seq_struct(s1);
    s2p = Seq_struct(s2);
    mat_setup_size (ta, seq_get_len(s1p), seq_get_len(s2p), 0, 0, \
            cm_get_alphabet_size(tc));
#ifdef DEBUG_ALL_ASSERTIONS
    _algn_max_matrix = ta->matrix + ta->len_eff;
    _algn_max_direction = ta->matrix_d + ta->len;
#endif
    res = algn_nw (s1p, s2p, tc, ta, Int_val(deltawh));
    CAMLreturn(Val_int(res));
}

value 
algn_CAML_limit_2 (value s1, value s2, value c, value a, value w, value h, \
        value s1_st, value s2_st, value s1_len, value s2_len) {
    CAMLparam5(s1, s2, c, a, w);
    CAMLxparam5(h, s1_st, s2_st, s1_len, s2_len);
    seqt s1p, s2p;
    int res, cw;
    cmt tc;
    matricest ta;
    cw = Int_val(w);
    tc = Cost_matrix_struct(c);
    ta = Matrices_struct(a);
    s1p = Seq_struct(s1);
    s2p = Seq_struct(s2);
    mat_setup_size (ta, seq_get_len(s1p), seq_get_len(s2p), 0, 0, \
            cm_get_alphabet_size(tc));
    /* TODO: Fix this deltaw binding */
    res = algn_nw_limit (s1p, s2p, tc, ta, Int_val(w), 
            Int_val(s1_st), Int_val(s1_len), Int_val(s2_st), Int_val(s2_len));
    CAMLreturn(Val_int(res));
}

value 
algn_CAML_limit_2_bc (value *argv, int argn) {
    return (algn_CAML_limit_2 (argv[0], argv[1], argv[2], argv[3], argv[4], \
                argv[5], argv[6], argv[7], argv[8], argv[9]));
}

value
algn_CAML_simple_3 (value s1, value s2, value s3, value c, value a, value uk) {
    CAMLparam5(s1, s2, s3, c, a);
    CAMLxparam1(uk);
    seqt s1p, s2p, s3p;
    int res;
    cm_3dt tc;
    matricest ta;
    tc = Cost_matrix_struct_3d(c);
    ta = Matrices_struct(a);
    s1p = Seq_struct(s1);
    s2p = Seq_struct(s2);
    s3p = Seq_struct(s3);
    res = algn_nw_3d (s1p, s2p, s3p, tc, ta, Int_val(uk));
    CAMLreturn(Val_int(res));
}

value
algn_CAML_simple_3_bc (value *argv, int argn) {
    return (algn_CAML_simple_3 (argv[0], argv[1], argv[2], argv[3], argv[4], \
                argv[5]));
}

void
print_bcktrck (const seqt s1, const seqt s2, \
        const matricest m) {
    int i, j;
    unsigned char *d;
    d = mat_get_2d_direct (m);
    printf ("\n");
    for (i = 0; i < seq_get_len (s1); i++) {
        for (j = 0; j < seq_get_len (s2); j++) {
            printf ("%d", (int) *(d + j));
            fflush (stdout);
        }
        d += j;
        printf ("\n");
        fflush (stdout);
    }
    printf ("\n\n");
    fflush (stdout);
    return;
}

value
algn_CAML_print_bcktrck (value s1, value s2, value matrix) {
    CAMLparam3(s1, s2, matrix);
    seqt s1c, s2c;
    matricest m;
    s1c = Seq_struct(s1);
    s2c = Seq_struct(s2);
    m = Matrices_struct(matrix);
    print_bcktrck (s1c, s2c, m);
    CAMLreturn (Val_unit);
}

void
print_dynmtrx (const seqt s1, const seqt s2, \
        matricest m) {
    int i, j;
    int *d;
    d = mat_get_2d_matrix (m);
    for (i = 0; i < seq_get_len (s1); i++) {
        for (j = 0; j < seq_get_len (s2); j++) 
            printf ("%d", (int) *(d + j));
        d += j;
        printf ("\n");
    }
    return;
}

void
algn_string_of_2d_direction (unsigned char v) {
    if (v & ALIGN) printf ("A");
    if (v & DELETE) printf ("D");
    if (v & INSERT) printf ("I");
    if (v & ALIGN_V) printf ("VA");
    if (v & DELETE_V) printf ("VD");
    if (v & ALIGN_H) printf ("HA");
    if (v & INSERT_H) printf ("HI");
    return;
}

#define my_prepend(a,b) assert (a->cap > a->len); (a)->begin = (((a)->begin) - 1); ((a)->len = 1 + (a)->len); *((a)->begin) = b

#define my_get(a,b) ((a)->begin)[b]

#ifdef _WIN32
__inline void
#else
inline void
#endif
backtrack_2d (const seqt s1, const seqt s2, seqt r1, \
        seqt r2, const matricest m, const cmt c, int st_s1, \
        int st_s2, int algn_s1, int algn_s2, int swaped, \
        value a, value b) {
    int l, l1, l2;
    unsigned char *beg, *end;
    int new_item_for_r1 = 0;
    int new_item_for_r2 = 0;
    int shifter = 0;
    l1 = seq_get_len (s1);
    l2 = seq_get_len (s2);
    l = l1 * l2;
    beg = st_s2 + mat_get_2d_direct (m);
    /* Stitching goes to hell now 
    end = beg + (l2 * algn_s1) + algn_s2 - 1;
    */
    end = beg + (l1 * l2) - 1;
    l = l2;

    if (!NDEBUG && !NPRINT_DM) {
        unsigned char *beg_debug;
        int i, j;
        beg_debug = beg;
        printf ("Printing a two dimensional direction matrix.\n");
        for (i = 0; i < algn_s1; i++, beg_debug += l2) {
            for (j  = 0; j < algn_s2; j++) {
                algn_string_of_2d_direction (beg_debug[j]);
                printf ("\t");
                fflush (stdout);
                end = beg_debug + j;
            }
            printf ("\n");
            fflush (stdout);
        }
        printf ("\n");
        fflush (stdout);
    }

    end = beg + (l2 * (algn_s1 - 1)) + algn_s2 - 1;

    algn_s1 = algn_s1 + st_s1;
    algn_s2 = algn_s2 + st_s2;
    /* The following pair of while loops are the same lines of code, each 
     * has swaped INSERT and DELETE procedures, so that depending on the ordering 
     * of the two sequences (swap) either INSERTING or DELETING will be preferred. 
     * During the downpass and all the optimization procedures, keeping the same
     * ordering for the medians output is important to keep consistency in the
     * diagnosis at every step. In other words, if a join is performed starting
     * in any position of the tree, it is necessary to make sure that the very
     * same median would be produced if the calculation started in any of it's
     * children. 
     * Note that this two lines could be defined as macros, but we (I?) have
     * decided not to do so to keep it readable. Besides, once correct, there is
     * nothing (or very few things) to do here. */
    if (!(cm_get_affine_flag (c))) {
        if (swaped) {
            while (end >= beg) {
                if (*end & ALIGN) {
                    algn_s1--;
                    new_item_for_r1 = my_get(s1,algn_s1);
                    my_prepend(r1,new_item_for_r1);
                    algn_s2--;
                    new_item_for_r2 = my_get(s2,algn_s2);
                    my_prepend(r2,new_item_for_r2);
                    end -= l + 1;
                } 
                else if (*end & INSERT) {
                    new_item_for_r1 = cm_get_gap (c);
                    my_prepend(r1,new_item_for_r1);
                    algn_s2--;
                    new_item_for_r2 = my_get(s2,algn_s2);
                    my_prepend(r2,new_item_for_r2);
                    end -= 1;
                } 
                else {
                    assert (*end & DELETE);
                    algn_s1--;
                    new_item_for_r1 = my_get (s1,algn_s1);
                    my_prepend(r1,new_item_for_r1);
                    new_item_for_r2 = cm_get_gap (c);
                    my_prepend(r2,new_item_for_r2);
                    end -= l;
                }
            }
        }
        else {
            while (end >= beg) {
                if (*end & ALIGN) {
                    algn_s1--;
                    new_item_for_r1 = my_get(s1,algn_s1);
                    my_prepend(r1,new_item_for_r1);
                    algn_s2--;
                    new_item_for_r2 = my_get(s2,algn_s2);
                    my_prepend(r2,new_item_for_r2);
                    end -= l + 1;
                } 
                else if (*end & DELETE) {
                    algn_s1--;
                    new_item_for_r1 = my_get(s1,algn_s1);
                    my_prepend(r1, new_item_for_r1);
                    new_item_for_r2 = cm_get_gap (c);
                    my_prepend(r2,new_item_for_r2);
                    end -= l;
                } 
                else {
                    assert (*end & INSERT);
                    new_item_for_r1 = cm_get_gap (c);
                    my_prepend(r1,new_item_for_r1);
                    algn_s2--;
                    new_item_for_r2 = my_get(s2,algn_s2);
                    my_prepend(r2,new_item_for_r2);
                    end -= 1;
                }
            }
        }
    }
    else {
        if (swaped) {
            while (end >= beg) {
                if (*end & (ALIGN << shifter)) {
                    if (0 == shifter) {
                        if (!NDEBUG_BT) printf ("1\t");
                        algn_s1--;
                        new_item_for_r1 = my_get(s1,algn_s1);
                        my_prepend(r1,new_item_for_r1);
                        algn_s2--;
                        new_item_for_r2 = my_get(s2,algn_s2);
                        my_prepend(r2,new_item_for_r2);
                        end -= l + 1;
                    } else if (SHIFT_V == shifter) {
                        if (!NDEBUG_BT) printf ("2\t");
                        algn_s1--;
                        new_item_for_r1 = my_get (s1,algn_s1);
                        my_prepend(r1,new_item_for_r1);
                        new_item_for_r2 = cm_get_gap (c);
                        my_prepend(r2,new_item_for_r2);
                        end -= l;
                        shifter = 0;
                    } else {
                        if (!NDEBUG_BT) printf ("3\t");
                        assert (SHIFT_H == shifter);
                        new_item_for_r1 = cm_get_gap (c);
                        my_prepend(r1,new_item_for_r1);
                        algn_s2--;
                        new_item_for_r2 = my_get(s2,algn_s2);
                        my_prepend(r2,new_item_for_r2);
                        end -= 1;
                        shifter = 0;
                    }
                } 
                else if (*end & (INSERT << shifter)) {
                    if (0 == shifter) {
                        if (!NDEBUG_BT) printf ("4\t");
                        shifter = SHIFT_H;
                    }
                    else if (SHIFT_H == shifter) {
                        if (!NDEBUG_BT) printf ("5\t");
                        new_item_for_r1 = cm_get_gap (c);
                        my_prepend(r1,new_item_for_r1);
                        algn_s2--;
                        new_item_for_r2 = my_get(s2,algn_s2);
                        my_prepend(r2,new_item_for_r2);
                        end -= 1;
                    }
                    else {
                        if (!NDEBUG_BT) printf ("6\t");
                        assert (0);
                    }
                } 
                else {
                    assert (*end & (DELETE << shifter));
                    if (0 == shifter) {
                        if (!NDEBUG_BT) printf ("7\t");
                        shifter = SHIFT_V;
                    }
                    else if (SHIFT_V == shifter) {
                        if (!NDEBUG_BT) printf ("8\t");
                        algn_s1--;
                        new_item_for_r1 = my_get (s1,algn_s1);
                        my_prepend(r1,new_item_for_r1);
                        new_item_for_r2 = cm_get_gap (c);
                        my_prepend(r2,new_item_for_r2);
                        end -= l;
                    } 
                    else {
                        if (!NDEBUG_BT) printf ("9\t");
                        assert (0);
                    }
                }
            }
        }
        else {
            while (end >= beg) {
                if (*end & (ALIGN << shifter)) {
                    if (0 == shifter) {
                        algn_s1--;
                        new_item_for_r1 = my_get(s1,algn_s1);
                        my_prepend(r1,new_item_for_r1);
                        algn_s2--;
                        new_item_for_r2 = my_get(s2,algn_s2);
                        my_prepend(r2,new_item_for_r2);
                        end -= l + 1;
                    } else if (SHIFT_V == shifter) {
                        algn_s1--;
                        new_item_for_r1 = my_get (s1,algn_s1);
                        my_prepend(r1,new_item_for_r1);
                        new_item_for_r2 = cm_get_gap (c);
                        my_prepend(r2,new_item_for_r2);
                        end -= l;
                        shifter = 0;
                    } else {
                        assert (SHIFT_H == shifter);
                        new_item_for_r1 = cm_get_gap (c);
                        my_prepend(r1,new_item_for_r1);
                        algn_s2--;
                        new_item_for_r2 = my_get(s2,algn_s2);
                        my_prepend(r2,new_item_for_r2);
                        end -= 1;
                        shifter = 0;
                    }
                } 
                else if (*end & (DELETE << shifter)) {
                    if (0 == shifter) 
                        shifter = SHIFT_V;
                    else if (SHIFT_V == shifter) {
                        algn_s1--;
                        new_item_for_r1 = my_get (s1,algn_s1);
                        my_prepend(r1,new_item_for_r1);
                        new_item_for_r2 = cm_get_gap (c);
                        my_prepend(r2,new_item_for_r2);
                        end -= l;
                    } 
                    else {
                        assert (0);
                    }
                }
                else {
                    assert (*end & (INSERT << shifter));
                    if (0 == shifter) 
                        shifter = SHIFT_H;
                    else if (SHIFT_H == shifter) {
                        new_item_for_r1 = cm_get_gap (c);
                        my_prepend(r1,new_item_for_r1);
                        algn_s2--;
                        new_item_for_r2 = my_get(s2,algn_s2);
                        my_prepend(r2,new_item_for_r2);
                        end -= 1;
                    }
                    else {
                        assert (0);
                    }
                } 
            }
        }
    }
    return;
}

char *
algn_string_of_3d_direction (char v) {
    if (v & S2) return "S2";
    else if (v & S3) return "S3";
    else if (v & S1) return "S1";
    else if (v & P3) return "P3";
    else if (v & SS) return "SS";
    else if (v & P1) return "P1";
    else if (v & P2) return "P2";
    else {
        assert (0);
    }
    return "Empty";
}

#ifdef _WIN32
__inline void
#else
inline void
#endif
backtrack_3d (const seqt s1, const seqt s2, seqt s3, \
        seqt r1, seqt r2, seqt r3, matricest m, \
        const cm_3dt c) {
    int l, l1, l2, l3;
    int a_plane, a_line, a_cell = 1;
    unsigned char *beg, *end;
    l1 = seq_get_len (s1);
    l2 = seq_get_len (s2);
    l3 = seq_get_len (s3);
    l = l1 * l2 * l3;
    a_plane = l2 * l3;
    a_line = l3;
    beg = mat_get_3d_direct (m);
    if (!NDEBUG) {
        char *toprint;
        unsigned char *beg_debug;
        int i, j, k;
        beg_debug = beg;
        printf ("Printing a three dimensional direction matrix.\n");
        for (i = 0; i < l1; i++) {
            for (j  = 0; j < l2; j++) {
                for (k = 0 ; k < l3; k++, beg_debug++) {
                    toprint = algn_string_of_3d_direction (*beg_debug);
                    printf ("%s\t", toprint);
                }
                printf ("\n");
            }
            printf ("\n");
        }
    }
    end = beg + l - 1;
    l1--;
    l2--;
    l3--;
    while (end > beg) {
        if (*end & S2) {        /* A plane, line, and cell */
            seq_prepend (r1, seq_get (s1, l1--));
            seq_prepend (r2, seq_get (s2, l2--));
            seq_prepend (r3, seq_get (s3, l3--));
            end -= a_plane + a_line + a_cell;
        } else if (*end & S3) { /* A plane and cell */
            seq_prepend (r1, seq_get (s1, l1--));
            seq_prepend (r2, cm_get_gap_3d (c));
            seq_prepend (r3, seq_get (s3, l3--));
            end -= a_plane + a_cell;
        } else if (*end & S1) { /* A line and cell */
            seq_prepend (r1, cm_get_gap_3d (c));
            seq_prepend (r2, seq_get (s2, l2--));
            seq_prepend (r3, seq_get (s3, l3--));
            end -= a_line + a_cell;
        } else if (*end & P3) { /* A plane */
            seq_prepend (r1, seq_get (s1, l1--));
            seq_prepend (r2, cm_get_gap_3d (c));
            seq_prepend (r3, cm_get_gap_3d (c));
            end -= a_plane;
        } else if (*end & SS) { /* A cell */
            seq_prepend (r1, cm_get_gap_3d (c));
            seq_prepend (r2, cm_get_gap_3d (c));
            seq_prepend (r3, seq_get (s3, l3--));
            end -= a_cell;
        } else if (*end & P1) { /* A line */
            seq_prepend (r1, cm_get_gap_3d (c));
            seq_prepend (r2, seq_get (s2, l2--));
            seq_prepend (r3, cm_get_gap_3d (c));
            end -= a_line;
        } else if (*end & P2) { /* A plane and line */
            seq_prepend (r1, seq_get (s1, l1--));
            seq_prepend (r2, seq_get (s2, l2--));
            seq_prepend (r3, cm_get_gap_3d (c));
            end -= a_plane + a_line;
        }
        else {
            assert (0);
        }
    }
    return;
}

value
algn_CAML_backtrack_2d (value s1, value s2, value s1p, value s2p, value a, \
        value c, value swap) {
    CAMLparam5(s1, s2, s1p, s2p, a);
    CAMLxparam2(c, swap);
    seqt ss1, ss2, ss1p, ss2p;
    matricest ta;
    cmt cc;
    ta = Matrices_struct(a);
    ss1 = Seq_struct(s1);
    ss2 = Seq_struct(s2);
    ss1p = Seq_struct(s1p);
    ss2p = Seq_struct(s2p);
    cc = Cost_matrix_struct(c);
    backtrack_2d (ss1, ss2, ss1p, ss2p, ta, cc, 0, 0, seq_get_len(ss1), \
            seq_get_len(ss2), Bool_val(swap), s1, s2);
    CAMLreturn(Val_unit);
}

value 
algn_CAML_backtrack_2d_bc (value *argv, int argn) {
    return (algn_CAML_backtrack_2d (argv[0], argv[1], argv[2], argv[3], \
                argv[4], argv[5], argv[6]));
}

value 
algn_CAML_backtrack_2d_limit (value s1, value s2, value s1p, \
        value s2p, value a, value c, value st_s1, value st_s2, \
        value algn_s1, value algn_s2, value swaped) {
    CAMLparam5 (s1, s2, s1p, s2p, a);
    CAMLxparam5 (c, st_s1, st_s2, algn_s1, algn_s2);
    CAMLxparam1 (swaped);
    seqt ss1, ss2, ss1p, ss2p;
    matricest ta;
    cmt cc;
    ta = Matrices_struct(a);
    ss1 = Seq_struct(s1);
    ss2 = Seq_struct(s2);
    ss1p = Seq_struct(s1p);
    ss2p = Seq_struct(s2p);
    cc = Cost_matrix_struct(c);
    backtrack_2d (ss1, ss2, ss1p, ss2p, ta, cc, Int_val(st_s1), \
            Int_val(st_s2), Int_val(algn_s1), Int_val(algn_s2), 
            Bool_val(swaped), s1, s2);
    CAMLreturn (Val_unit);
}

value 
algn_CAML_backtrack_2d_limit_bc (value *argv, int argn) {
    return (algn_CAML_backtrack_2d_limit (argv[0], argv[1], argv[2], argv[3], \
                argv[4], argv[5], argv[6], argv[7], argv[8], argv[9], argv[10]));
}

value
algn_CAML_backtrack_3d (value s1, value s2, value s3, value s1p, value s2p, \
        value s3p, value a, value c) {
    CAMLparam5(s1, s2, s1p, s2p, a);
    CAMLxparam2(s3, s3p);
    seqt ss1, ss2, ss3, ss1p, ss2p, ss3p;
    matricest ta;
    cm_3dt tc;
    ta = Matrices_struct(a);
    ss1 = Seq_struct(s1);
    ss2 = Seq_struct(s2);
    ss3 = Seq_struct(s3);
    ss1p = Seq_struct(s1p);
    ss2p = Seq_struct(s2p);
    ss3p = Seq_struct(s3p);
    tc = Cost_matrix_struct_3d(c);
    backtrack_3d (ss1, ss2, ss3, ss1p, ss2p, ss3p, ta, tc);
    CAMLreturn(Val_unit);
}

value
algn_CAML_backtrack_3d_bc (value *argv, int argc) {
    return (algn_CAML_backtrack_3d (argv[0], argv[1], argv[2], argv[3], \
                argv[4], argv[5], argv[6], argv[7]));
}

value 
algn_CAML_align_2d (value s1, value s2, value c, value a, value s1p, \
        value s2p, value deltawh, value swaped)
{
    CAMLlocal1(res);
    res = algn_CAML_simple_2 (s1, s2, c, a, deltawh);
    algn_CAML_backtrack_2d (s1, s2, s1p, s2p, a, c, swaped);
    return (res);
}

value
algn_CAML_align_2d_bc (value *argv, int argn) {
    return (algn_CAML_align_2d (argv[0], argv[1], argv[2], argv[3], argv[4], \
                argv[5], argv[6], argv[7])); 
}

value 
algn_CAML_align_3d (value s1, value s2, value s3, value c, value a, \
        value s1p, value s2p, value s3p, value uk) {
    CAMLlocal1(res);
    res = algn_CAML_simple_3 (s1, s2, s3, c, a, uk);
    algn_CAML_backtrack_3d (s1, s2, s3, s1p, s2p, s3p, a, c);
    return (res);
}

value
algn_CAML_align_3d_bc (value *argv, int argn) {
    return (algn_CAML_align_3d (argv[0], argv[1], argv[2], argv[3], argv[4], \
                argv[5], argv[6], argv[7], argv[8])); 
}

#ifdef _WIN32
__inline void
#else
inline void
#endif
algn_get_median_2d_with_gaps (seqt s1, seqt s2, cmt m, seqt sm) {
    int *begin1, *begin2;
    int interm;
    int i;
    begin1 = seq_get_begin (s1);
    begin2 = seq_get_begin (s2);
    for (i = seq_get_len (s1) - 1; i >= 0; i--) {
        interm = cm_get_median (m, begin1[i], begin2[i]);
        seq_prepend (sm, interm);
    }
    return;
}

#ifdef _WIN32
__inline void
#else
inline void
#endif
algn_get_median_2d_no_gaps (seqt s1, seqt s2, cmt m, seqt sm) {
    int *begin1, *begin2;
    int interm;
    int i;
    begin1 = seq_get_begin (s1);
    begin2 = seq_get_begin (s2);
    for (i = seq_get_len (s1) - 1; i >= 0; i--) {
        interm = cm_get_median (m, begin1[i], begin2[i]);
        if (interm != cm_get_gap (m))
            seq_prepend (sm, interm);
    }
    seq_prepend (sm, cm_get_gap (m));
    return;
}

void
algn_remove_gaps (int gap, seqt s) {
    int i, len;
    len = seq_get_len (s);
    int *source, *destination, newlen = 0; 
    source = destination = s->end;
    for (i = len - 1; i >= 0; i--) {
        if (gap != *source) {
            *destination = *source;
            destination--;
            newlen++;
        }
        source--;
    }
    s->len = newlen;
    s->begin = destination + 1;
    /* We restore the leading gap */
    seq_prepend (s, gap);
    return;
}

void
algn_correct_blocks_affine (int gap, seqt s, seqt a, seqt b) {
    int i, len, in_gap_block = 0, ab, bb, sb, extending_gap, 
        inside_block = 0, prev_block = 0;
    len = seq_get_len (s);
    extending_gap = 0;
    inside_block = 0;
    for (i = 0; i < len; i++) {
        ab = seq_get (a, i);
        bb = seq_get (b, i);
        sb = seq_get (s, i);
        if (i > 2) {

        }
        if (!inside_block && (!(ab & gap) || !(bb & gap)))
            inside_block = 0;
        else if (inside_block && ((!(ab & gap)) || (!(bb & gap))))
            inside_block = 0;
        else if (((ab & gap) || (bb & gap)) && ((ab != gap) || (bb != gap)))
            inside_block = 1;
        else 
            inside_block = 0;
        if (((gap & ab) || (gap & bb)) && (!(sb & gap)) && !extending_gap) {
            prev_block = inside_block;
            extending_gap = 1;
        }
        else if ((gap & ab) && (gap & bb) && (sb & gap) && (sb != gap) && 
                extending_gap && inside_block && !prev_block) {
            sb = (~gap) & sb;
            prev_block = 0;
        }
        else if ((gap & ab) && (gap & bb) && (1 == extending_gap)) {
            prev_block = inside_block;
            extending_gap = 0;
        }

        seq_set (s, i, sb);
    }
    algn_remove_gaps (gap, s);
    return;
}

#ifdef _WIN32
__inline void
#else
inline void
#endif
algn_ancestor_2 (seqt s1, seqt s2, cmt m, seqt sm ) {
    int *begin1, *begin2;
    int interm;
    int i, gap, is_combinations, cost_model, on_gap_block;
    begin1 = seq_get_begin (s1);
    begin2 = seq_get_begin (s2);
    gap = cm_get_gap (m);
    is_combinations = m->combinations;
    cost_model = m->cost_model_type;
    for (i = seq_get_len (s1) - 1; i >= 0; i--) {
        interm = cm_get_median (m, begin1[i], begin2[i]);
        if ((!is_combinations) || (1 != cost_model)) {
            if (interm != gap) seq_prepend (sm, interm);
        }
        else seq_prepend (sm, interm);
    }
    if ((!is_combinations) || ((1 != cost_model) && (gap != seq_get (sm, 0))))
        seq_prepend (sm, gap);
    else if (is_combinations)
        algn_correct_blocks_affine (gap, sm, s1, s2);
    return;
}



/* 
 * Given three aligned sequences s1, s2, and s3, the median between them is
 * returned in the sequence sm, using the cost matrix stored in m.
 */
#ifdef _WIN32
__inline void
#else
inline void
#endif
algn_get_median_3d (seqt s1, seqt s2, seqt s3, \
        cm_3dt m, seqt sm) {
    int *end1, *end2, *end3;
    int interm;
    int i;
    end1 = seq_get_end (s1);
    end2 = seq_get_end (s2);
    end3 = seq_get_end (s3);
    for (i = seq_get_len (s1) - 1; i >= 0; i--) {
        interm = cm_get_median_3d (m, *end1, *end2, *end3);
        seq_prepend (sm, interm);
    }
    return;
}

void
algn_union (seqt s1, seqt s2, seqt su) {
    assert (seq_get_len (s1) == seq_get_len (s2));
    assert (seq_get_cap (s1) >= seq_get_len (s2));
    int len, i;
    len = seq_get_len (s1);
    for (i = len - 1; i >= 0; i--)
        seq_prepend (su, (seq_get (s1, i) | seq_get (s2, i)));
    return;
}

value 
algn_CAML_union (value s1, value s2, value su) {
    CAMLparam3(s1, s2, su);
    seqt ss1, ss2, ssu;
    ss1 = Seq_struct(s1);
    ss2 = Seq_struct(s2);
    ssu = Seq_struct(su);
    algn_union (ss1, ss2, ssu);
    CAMLreturn(Val_unit);
}

value
algn_CAML_median_2_no_gaps (value s1, value s2, value m, value sm) {
    CAMLparam4(s1, s2, m, sm);
    seqt ss1, ss2, ssm;
    cmt tm;
    ss1 = Seq_struct(s1);
    ss2 = Seq_struct(s2);
    ssm = Seq_struct(sm);
    tm = Cost_matrix_struct(m);
    algn_get_median_2d_no_gaps (ss1, ss2, tm, ssm);
    CAMLreturn(Val_unit);
}

value
algn_CAML_median_2_with_gaps (value s1, value s2, value m, value sm) {
    CAMLparam4(s1, s2, m, sm);
    seqt ss1, ss2, ssm;
    cmt tm;
    ss1 = Seq_struct(s1);
    ss2 = Seq_struct(s2);
    ssm = Seq_struct(sm);
    tm = Cost_matrix_struct(m);
    algn_get_median_2d_with_gaps (ss1, ss2, tm, ssm);
    CAMLreturn(Val_unit);
}

value 
algn_CAML_median_3 (value s1, value s2, value s3, value m, value sm) {
    CAMLparam5(s1, s2, s3, m, sm);
    seqt ss1, ss2, ss3, ssm;
    cm_3dt tm;
    ss1 = Seq_struct(s1);
    ss2 = Seq_struct(s2);
    ss3 = Seq_struct(s3);
    ssm = Seq_struct(sm);
    tm = Cost_matrix_struct_3d(m);
    algn_get_median_3d (ss1, ss2, ss3, tm, ssm);
    CAMLreturn(Val_unit);
}

/* Alignment following the algorithm of Myers , 1986. */
static zarrt v = NULL;

int
algn_myers (zarrt v, seqt a, seqt b, int max) {
    int d, kp1, km1, la, lb, k, x, y;
    la = seq_get_len (a) - 1;
    lb = seq_get_len (b) - 1;
    if (zarr_clear (v, max)) {
        for (d = 0; d <= max; d++) {
            for (k = -d; k <= d; k+=2) {
                zarr_get (v, k + 1, &kp1);
                zarr_get (v, k - 1, &km1);
                if ((k == -d) || ((k != d) && (km1 < kp1))) x = kp1;
                else x = km1 + 1;
                y = x - k;

                while ((x < la) && (y < lb) &&
                        (seq_get (a, x + 1) == seq_get (b, y + 1))) {
                    x++;
                    y++;
                }
                zarr_set (v, k, x);
                if ((x >= la) && (y >= lb)) return d;
            }
        }
    }
    return -1;
}


value
algn_CAML_myers (value sa, value sb) {
    CAMLparam0();
    seqt a, b;
    int res, max;
    a = Seq_custom_val(sa);
    b = Seq_custom_val(sb);
    max = seq_get_len (a) + seq_get_len (b); 
    if (max > zarr_length (v)) {
        if ((v != NULL) && (zarr_realloc (v, max))) failwith ("Allocation error.");
        else {
            v = zarr_alloc (max);
            if (v == NULL) failwith ("Allocation error.");
        }
    }
    res = algn_myers (v, a, b, max);
    CAMLreturn (Val_int (res));
}

value 
algn_CAML_ancestor_2 (value sa, value sb, value cm, value sab) {
    CAMLparam4(sa, sb, cm, sab);
    seqt a, b, ab;
    cmt tm;

    a = Seq_custom_val(sa);
    b = Seq_custom_val(sb);
    ab = Seq_custom_val(sab);
    tm = Cost_matrix_struct(cm);
    algn_ancestor_2 (a, b, tm, ab);
    CAMLreturn (Val_unit);
}

void
algn_copy_backtrack (int s1, int s2, const matricest m, value res) {
    value row;
    int i, j;
    unsigned char *d;
    d = mat_get_2d_direct (m);
    for (i = 0; i < s1; i++) {
        row = Field(res, i);
        for (j = 0; j < s2; j++) {
            Store_field(row, j, Val_int (*(d + j)));
        }
        d += j;
    }
    return;
}

value
algn_CAML_create_backtrack (value lsa, value lsb, value scm, value sres) {
    CAMLparam4(lsa, lsb, scm, sres);
    int la, lb;
    matricest m;

    la = Int_val(lsa);
    lb = Int_val(lsb);
    m = Matrices_struct(scm);
    algn_copy_backtrack (la, lb, m, sres);
    CAMLreturn(Val_unit);
}

#include "union.c"
