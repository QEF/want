! Input/Output Tool Kit (IOTK)
! Copyright (C) 2004,2005 Giovanni Bussi
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation; either
! version 2.1 of the License, or (at your option) any later version.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this library; if not, write to the Free Software
! Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 2 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_AUXMACROS
#define __IOTK_AUXMACROS

! The macros are defined with -D option or inside iotk_config.h
! The default values are set here
! Maximum rank of an array
#ifndef __IOTK_MAXRANK
#  define __IOTK_MAXRANK 7
#endif
! Minimum value used in iotk_free_unit
#ifndef __IOTK_UNITMIN
#  define __IOTK_UNITMIN 90000
#endif
! Maximum value used in iotk_free_unit
#ifndef __IOTK_UNITMAX
#  define __IOTK_UNITMAX 99999
#endif
! Kind for header in binary files
#ifndef __IOTK_HEADER_KIND
#  define __IOTK_HEADER_KIND selected_int_kind(8)
#endif
! Character (or eventually string) for newline
! It may be adjusted for particular systems
! Unix    achar(10)
! Mac-OS  achar(13)
! Windows ? (now it should be a single byte)
#ifndef __IOTK_NEWLINE
#  define __IOTK_NEWLINE achar(10)
#endif
! Character for EOS
#ifndef __IOTK_EOS
#  define __IOTK_EOS achar(0)
#endif
! These are the default kinds, which depend on the options used
! during the library compilation
! Only default characters are implemented
#define __IOTK_CHARACTER1 iotk_defkind_character
! For logical, integer and real types, the c precompiler
! looks for defined kinds. If no kind is found, the default
! is used as __IOTK_type1
# 46 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_LOGICAL1
# 46 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_LOGICAL2
# 46 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_LOGICAL3
# 46 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_LOGICAL4
# 48 "../include/iotk_auxmacros.spp"
#define __IOTK_LOGICAL1 iotk_defkind_LOGICAL
# 51 "../include/iotk_auxmacros.spp"
#endif
# 51 "../include/iotk_auxmacros.spp"
#endif
# 51 "../include/iotk_auxmacros.spp"
#endif
# 51 "../include/iotk_auxmacros.spp"
#endif
# 46 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_INTEGER1
# 46 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_INTEGER2
# 46 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_INTEGER3
# 46 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_INTEGER4
# 48 "../include/iotk_auxmacros.spp"
#define __IOTK_INTEGER1 iotk_defkind_INTEGER
# 51 "../include/iotk_auxmacros.spp"
#endif
# 51 "../include/iotk_auxmacros.spp"
#endif
# 51 "../include/iotk_auxmacros.spp"
#endif
# 51 "../include/iotk_auxmacros.spp"
#endif
# 46 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_REAL1
# 46 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_REAL2
# 46 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_REAL3
# 46 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_REAL4
# 48 "../include/iotk_auxmacros.spp"
#define __IOTK_REAL1 iotk_defkind_REAL
# 51 "../include/iotk_auxmacros.spp"
#endif
# 51 "../include/iotk_auxmacros.spp"
#endif
# 51 "../include/iotk_auxmacros.spp"
#endif
# 51 "../include/iotk_auxmacros.spp"
#endif
# 54 "../include/iotk_auxmacros.spp"
! Complex are treated indentically to reals
! These lines map the definitions.
# 57 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_REAL1
#define __IOTK_COMPLEX1 __IOTK_REAL1
#else
#undef __IOTK_COMPLEX1
#endif
# 57 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_REAL2
#define __IOTK_COMPLEX2 __IOTK_REAL2
#else
#undef __IOTK_COMPLEX2
#endif
# 57 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_REAL3
#define __IOTK_COMPLEX3 __IOTK_REAL3
#else
#undef __IOTK_COMPLEX3
#endif
# 57 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_REAL4
#define __IOTK_COMPLEX4 __IOTK_REAL4
#else
#undef __IOTK_COMPLEX4
#endif
# 63 "../include/iotk_auxmacros.spp"
! If the binary format is not defined, use *
#ifndef __IOTK_BINARY_FORMAT
#define __IOTK_BINARY_FORMAT "*"
#endif

! Some check 
#if __IOTK_MAXRANK > 7
#  error
#endif
#if __IOTK_MAXRANK < 1
#  error
#endif

#endif

# 30 "iotk_misc_interf.spp"

module iotk_misc_interf
use iotk_base
implicit none
private

public :: iotk_copy_tag
public :: iotk_parse_dat
public :: iotk_set_options
public :: iotk_get_options
public :: iotk_copy_dat_aux
public :: iotk_copy_dat
public :: iotk_print_kinds
public :: iotk_check_iotk_attr
public :: iotk_index
public :: iotk_check_name
public :: iotk_tag_parse
public :: iotk_complete_filepath



! This module contains the interfaces to all iotk routines

interface iotk_copy_tag
subroutine iotk_copy_tag_x(source,dest,maxsize,ierr)
  implicit none
  integer,           intent(in)  :: source
  integer,           intent(in)  :: dest
  integer, optional, intent(in)  :: maxsize
  integer, optional, intent(out) :: ierr
end subroutine iotk_copy_tag_x
end interface

interface iotk_parse_dat
subroutine iotk_parse_dat_x(attr,type,kind,isize,len,fmt,ierr)
  implicit none
  character(*), intent(in)  :: attr
  character(*), intent(out) :: type
  integer,      intent(out) :: kind
  integer,      intent(out) :: isize
  integer,      intent(out) :: len
  character(*), intent(out) :: fmt
  integer,      intent(out) :: ierr
end subroutine iotk_parse_dat_x
end interface

interface iotk_set_options
subroutine iotk_set_options_x(unitmin,unitmax,getline_buffer,error_warn_overflow,ierr)
  implicit none
  integer, optional, intent(in) :: unitmin
  integer, optional, intent(in) :: unitmax
  integer, optional, intent(in) :: getline_buffer
  logical, optional, intent(in) :: error_warn_overflow
  integer, optional, intent(out):: ierr
end subroutine iotk_set_options_x
end interface

interface iotk_get_options
subroutine iotk_get_options_x(unitmin,unitmax,getline_buffer,error_warn_overflow)
  implicit none
  integer, optional, intent(out):: unitmin
  integer, optional, intent(out):: unitmax
  logical, optional, intent(out):: error_warn_overflow
  integer, optional, intent(out):: getline_buffer
end subroutine iotk_get_options_x
end interface

interface iotk_copy_dat_aux
subroutine iotk_copy_dat_aux_x(source,dest,source_binary,dest_binary,name,type,ikind,isize,len,fmt,ierr)
  implicit none
  integer,      intent(in)  :: source
  integer,      intent(in)  :: dest
  logical,      intent(in)  :: source_binary
  logical,      intent(in)  :: dest_binary
  character(*), intent(in)  :: name
  character(*), intent(in)  :: type
  integer,      intent(in)  :: ikind
  integer,      intent(in)  :: isize
  integer,      intent(in)  :: len
  character(*), intent(in)  :: fmt
  integer,      intent(out) :: ierr
end subroutine iotk_copy_dat_aux_x
end interface

interface iotk_copy_dat
subroutine iotk_copy_dat_x(source,dest,source_binary,dest_binary,name,attr,maxsize,ierr)
  implicit none
  integer,      intent(in)  :: source
  integer,      intent(in)  :: dest
  logical,      intent(in)  :: source_binary
  logical,      intent(in)  :: dest_binary
  character(*), intent(in)  :: name
  character(*), intent(in)  :: attr
  integer,      intent(in)  :: maxsize
  integer,      intent(out) :: ierr
end subroutine iotk_copy_dat_x
end interface

interface iotk_print_kinds
subroutine iotk_print_kinds_x
end subroutine iotk_print_kinds_x
end interface

interface iotk_check_iotk_attr
subroutine iotk_check_iotk_attr_x(unit,attr,ierr)
  use iotk_base
  implicit none
  integer,                 intent(in)  :: unit
  character(iotk_attlenx), intent(in)  :: attr
  integer,                 intent(out) :: ierr
  character(iotk_vallenx) :: version
end subroutine iotk_check_iotk_attr_x
end interface


interface iotk_index
function iotk_index_scal(index)
  integer,           intent(in) :: index
  character(len=range(index)+3) :: iotk_index_scal
end function iotk_index_scal
function iotk_index_vec(index)
  integer,                         intent(in) :: index(:)
  character(len=(range(index)+3)*size(index)) :: iotk_index_vec
end function iotk_index_vec
end interface

interface iotk_tag_parse
subroutine iotk_tag_parse_x(tag,name,attr,ierr)
  use iotk_base
  implicit none
  character(iotk_taglenx), intent(in)  :: tag
  character(iotk_namlenx), intent(out) :: name
  character(iotk_attlenx), intent(out) :: attr
  integer,                 intent(out) :: ierr
end subroutine iotk_tag_parse_x
end interface

interface iotk_complete_filepath
function iotk_complete_filepath_x(newfile,oldfile)
  character(len=*), intent(in) :: newfile
  character(len=*), intent(in) :: oldfile
  character(len=len(newfile)+len(oldfile)) :: iotk_complete_filepath_x
  character(len=len(oldfile)) :: prefix
end function iotk_complete_filepath_x
end interface

interface iotk_check_name
function iotk_check_name_x(name)
  character(len=*), intent(in) :: name
  logical                      :: iotk_check_name_x
end function iotk_check_name_x
end interface

end module iotk_misc_interf
