/* oaddn.h: Declarations for oaddn.c

   Copyright 2003 Bjoern Butscher, Hendrik Weimer

   This file is part of libquantum

   libquantum is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3 of the License,
   or (at your option) any later version.

   libquantum is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with libquantum; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
   MA 02110-1301, USA

*/

#ifndef __OADDN_H

#define __OADDN_H

#include "qureg.h"

extern void test_sum(int, int, quantum_reg *);

extern void muxfa(int, int, int, int, int, int, int, quantum_reg *);

extern void muxfa_inv(int, int, int, int, int, int, int, quantum_reg *);

extern void muxha(int, int, int, int, int, int, quantum_reg *);

extern void muxha_inv(int, int, int, int, int, int, quantum_reg *);

extern void madd(int, int, int, quantum_reg *);

extern void madd_inv(int, int, int,quantum_reg *);

extern void addn(int,int,int, quantum_reg *);

extern void addn_inv(int, int, int, quantum_reg *);

extern void add_mod_n(int, int, int, quantum_reg *);

#endif
