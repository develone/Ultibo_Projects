////////////////////////////////////////////////////////////////////////////////
//
// Filename: 	lifting.h
//
// Project:	XuLA2-LX25 SoC based upon the ZipCPU
//
// Purpose:	This header file just defines the interfaces associated with
//		singlelift() (a single forward WVT transformation, to be applied
//	to a square 2D image), ilift (the inverse of singlelift), and lifting
//	(a composition of the two--as currently written).
//
// Creator:	Dan Gisselquist, Ph.D.
//		Gisselquist Technology, LLC
//
////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2015-2016, Gisselquist Technology, LLC
//
// This program is free software (firmware): you can redistribute it and/or
// modify it under the terms of  the GNU General Public License as published
// by the Free Software Foundation, either version 3 of the License, or (at
// your option) any later version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTIBILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.
//
// You should have received a copy of the GNU General Public License along
// with this program.  (It's in the $(ROOT)/doc directory, run make with no
// target there if the PDF file isn't present.)  If not, see
// <http://www.gnu.org/licenses/> for a copy.
//
// License:	GPL, v3, as defined and found on www.gnu.org,
//		http://www.gnu.org/licenses/gpl.html
//
//
////////////////////////////////////////////////////////////////////////////////
//
//
#ifndef	LIFTING_H
#define	LIFTING_H

extern	void	singlelift(int rb, int w, int * const ibuf, int * const obuf);
extern	void	ilift(int rb, int w, int * const ibuf, int * const obuf);
extern	void	lifting(int w, int *ibuf, int *tmpbuf);

#endif

