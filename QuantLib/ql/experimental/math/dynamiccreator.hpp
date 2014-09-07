/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2014 Peter Caspers

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it
 under the terms of the QuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

/* This is a wrapper for the original C code "Dynamic Creator of Mersenne
 * Twisters Ver. 0.6 (2009/12/15)"
 * http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/DC/dc.html
*/

/*
  Copyright (C) 2001-2009 Makoto Matsumoto and Takuji Nishimura.
  Copyright (C) 2009 Mutsuo Saito
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are
  met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

/*! \file dynamiccreator.hpp
    \brief dynamic creator library
*/

#ifndef quantlib_dynamic_creator_hpp
#define quantlib_dynamic_creator_hpp

#include <ql/methods/montecarlo/sample.hpp>
#include <boost/cstdint.hpp>

namespace QuantLib {

// original code is placed in namespace mt_detail
// macros N and M were renamed to dcmt_N and dcmt_M respectively
// macros can not be hidden in namespaces, we still leave the
// others defined in this header unchanged
// (NOT_REJECTED, REJECTED, REDU, IRRED, NONREDU)
// we commented out the "old" interface since we do not use it
// in the wrapper anyway as well as code activated in debug mode
// only

namespace mt_detail {
// dc.h
typedef struct {
    uint32_t aaa;
    int mm, nn, rr, ww;
    uint32_t wmask, umask, lmask;
    int shift0, shift1, shiftB, shiftC;
    uint32_t maskB, maskC;
    int i;
    uint32_t *state;
} mt_struct;
} // namespace mt_detail

// to store a precomputed mt instance
struct MersenneTwisterDynamicRngDescription {
    int w, p;
    uint32_t aaa;
    int mm, nn, rr, ww;
    uint32_t wmask, umask, lmask;
    int shift0, shift1, shiftB, shiftC;
    uint32_t maskB, maskC;
    int i;
};

class MersenneTwisterDynamicRng {

  public:
    typedef Sample<Real> sample_type;

    // if given seed is 0 then a clock based seed is used

    // create a mt instance with word size w (31, 32) and period 2^p-1
    // this can take very long for a larger p
    // the id is incorporated into the rng such that rng with different
    // ids are highly independent
    // usable p for periods 2^p-1 are 521   607  1279  2203
    // 2281  3217  4253  4423 9689  9941 11213 19937 21701
    // 23209 44497

    MersenneTwisterDynamicRng(const int w = 32, const int p = 521,
                              const uint32_t creatorSeed = 42,
                              const uint16_t id = 0, const uint32_t seed = 0);

    // create mt from saved description

    MersenneTwisterDynamicRng(const MersenneTwisterDynamicRngDescription &d,
                              const uint32_t seed = 0);

    ~MersenneTwisterDynamicRng();

    void resetSeed(const uint32_t seed);
    sample_type next();
    Real nextReal();
    unsigned long nextInt32();
    MersenneTwisterDynamicRngDescription description() const;

  private:
    // hide copy and assignment constructors
    MersenneTwisterDynamicRng(const MersenneTwisterDynamicRng &o);
    MersenneTwisterDynamicRng &operator=(const MersenneTwisterDynamicRng &o);
    const int w_, p_;
    mt_detail::mt_struct *m_;

};

// precomputed instances
// 8 instances with p=19937, w=32 created with creator-seed 42
// and ids 4138, 4139, 4140, 4142, 4143, 4144, 4145, 4146

const static struct MersenneTwisterDynamicRngDescription mtdesc_0_8_19937[8] = {
    {32, 19937, 2760052778UL, 312, 624, 31, 32, 4294967295UL, 2147483648UL,
     2147483647UL, 12, 18, 7, 15, 861204096UL, 3721887744UL, 624},
    {32, 19937, 2708803627UL, 312, 624, 31, 32, 4294967295UL, 2147483648UL,
     2147483647UL, 12, 18, 7, 15, 995441536UL, 4158029824UL, 624},
    {32, 19937, 2902986796UL, 312, 624, 31, 32, 4294967295UL, 2147483648UL,
     2147483647UL, 12, 18, 7, 15, 2607987584UL, 4149641216UL, 624},
    {32, 19937, 3431665710UL, 312, 624, 31, 32, 4294967295UL, 2147483648UL,
     2147483647UL, 12, 18, 7, 15, 827782784UL, 3621027840UL, 624},
    {32, 19937, 3975614511UL, 312, 624, 31, 32, 4294967295UL, 2147483648UL,
     2147483647UL, 12, 18, 7, 15, 3142380672UL, 4023877632UL, 624},
    {32, 19937, 3342274608UL, 312, 624, 31, 32, 4294967295UL, 2147483648UL,
     2147483647UL, 12, 18, 7, 15, 1860665216UL, 4117987328UL, 624},
    {32, 19937, 2711425073UL, 312, 624, 31, 32, 4294967295UL, 2147483648UL,
     2147483647UL, 12, 18, 7, 15, 4007967616UL, 2008121344UL, 624},
    {32, 19937, 3279884338UL, 312, 624, 31, 32, 4294967295UL, 2147483648UL,
     2147483647UL, 12, 18, 7, 15, 3151424896UL, 3749019648UL, 624}};

namespace mt_detail {

// dc.h

// we have this above
// typedef struct {
//     uint32_t aaa;
//     int mm, nn, rr, ww;
//     uint32_t wmask, umask, lmask;
//     int shift0, shift1, shiftB, shiftC;
//     uint32_t maskB, maskC;
//     int i;
//     uint32_t *state;
// } mt_struct;

/* old interface */

// we disable the old interface, because not used in our wrapper anyway

// void init_dc(uint32_t seed);
// mt_struct *get_mt_parameter(int w, int p);
// mt_struct *get_mt_parameter_id(int w, int p, int id);
// mt_struct **get_mt_parameters(int w, int p, int max_id, int *count);

/* new interface */
mt_struct *get_mt_parameter_st(int w, int p, uint32_t seed);
mt_struct *get_mt_parameter_id_st(int w, int p, int id, uint32_t seed);
mt_struct **get_mt_parameters_st(int w, int p, int start_id, int max_id,
                                 uint32_t seed, int *count);
/* common */
void free_mt_struct(mt_struct *mts);
void free_mt_struct_array(mt_struct **mtss, int count);
void sgenrand_mt(uint32_t seed, mt_struct *mts);
uint32_t genrand_mt(mt_struct *mts);

// mt19337.h

#define dcmt_N 624

typedef struct _ORG_STATE {
    uint32_t mt[dcmt_N];
    int mti;
} _org_state;

void _sgenrand_dc(_org_state *st, uint32_t seed);
uint32_t _genrand_dc(_org_state *st);

// dci.h

#define NOT_REJECTED 1
#define REJECTED 0
#define REDU 0
#define IRRED 1
#define NONREDU 1

extern _org_state global_mt19937;
typedef struct {int *x; int deg;} Polynomial;

typedef struct PRESCR_T {
    int sizeofA; /* parameter size */
    uint32_t **modlist;
    Polynomial **preModPolys;
} prescr_t;

typedef struct CHECK32_T {
    uint32_t upper_mask;
    uint32_t lower_mask;
    uint32_t word_mask;
} check32_t;

typedef struct EQDEG_T {
    uint32_t bitmask[32];
    uint32_t mask_b;
    uint32_t mask_c;
    uint32_t upper_v_bits;
    int shift_0;
    int shift_1;
    int shift_s;
    int shift_t;
    int mmm;
    int nnn;
    int rrr;
    int www;
    uint32_t aaa[2];
    uint32_t gupper_mask;   /** most significant  (WWW - RRR) bits **/
    uint32_t glower_mask;	/** least significant RRR bits **/
    uint32_t greal_mask;	/** upper WWW bitmask **/
    int ggap; /** difference between machine wordsize and dest wordsize **/
    int gcur_maxlengs[32];	/** for optimize_v_hard **/
    uint32_t gmax_b, gmax_c;
} eqdeg_t;

int _prescreening_dc(prescr_t *pre, uint32_t aaa);
void _InitPrescreening_dc(prescr_t *pre, int m, int n, int r, int w);
void _EndPrescreening_dc(prescr_t *pre);
int _CheckPeriod_dc(check32_t *ck, _org_state *st,
            uint32_t a, int m, int n, int r, int w);
void _get_tempering_parameter_dc(mt_struct *mts);
void _get_tempering_parameter_hard_dc(mt_struct *mts);
void _InitCheck32_dc(check32_t *ck, int r, int w);

} // namesapce mt_detail
} // namespace QuantLib

#endif
