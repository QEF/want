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

# 30 "iotk_files_interf.spp"

module iotk_files_interf
use iotk_base
implicit none
private

public :: iotk_copyfile
public :: iotk_link
public :: iotk_open_write
public :: iotk_close_write
public :: iotk_open_read
public :: iotk_close_read
public :: iotk_magic

interface iotk_copyfile
subroutine iotk_copyfile_x(source,dest,source_unit,dest_unit,ierr)
  implicit none
  character(len=*), optional, intent(in) :: source
  character(len=*), optional, intent(in) :: dest
  integer,          optional, intent(in) :: source_unit
  integer,          optional, intent(in) :: dest_unit
  integer,          optional, intent(out):: ierr
end subroutine iotk_copyfile_x
end interface

interface iotk_link
subroutine iotk_link_x(unit,name,file,binary,raw,create,ierr)
  implicit none
  integer,                    intent(in)  :: unit
  character(len=*),           intent(in)  :: name
  character(len=*),           intent(in)  :: file
  logical,          optional, intent(in)  :: binary
  logical,          optional, intent(in)  :: raw
  logical,          optional, intent(in)  :: create
  integer,          optional, intent(out) :: ierr
end subroutine iotk_link_x
end interface

interface iotk_open_write
subroutine iotk_open_write_x(unit,file,attr,binary,new,raw,root,skip_root,skip_head,ierr)
  implicit none
  integer,                    intent(in)  :: unit
  character(len=*), optional, intent(in)  :: file
  character(len=*), optional, intent(in)  :: attr
  logical,          optional, intent(in)  :: binary
  logical,          optional, intent(in)  :: new
  logical,          optional, intent(in)  :: raw
  character(len=*), optional, intent(in)  :: root
  logical,          optional, intent(in)  :: skip_root
  logical,          optional, intent(in)  :: skip_head
  integer,          optional, intent(out) :: ierr
end subroutine iotk_open_write_x
end interface

interface iotk_close_write
subroutine iotk_close_write_x(unit,ierr)
  implicit none
  integer,                intent(in)  :: unit
  integer,      optional, intent(out) :: ierr
end subroutine iotk_close_write_x
end interface

interface iotk_open_read
subroutine iotk_open_read_x(unit,file,attr,binary,raw,root,ierr)
  implicit none
  integer,                    intent(in)  :: unit
  character(len=*), optional, intent(in)  :: file
  logical,          optional, intent(in)  :: binary 
  logical,          optional, intent(in)  :: raw
  character(len=*), optional, intent(out) :: attr
  character(len=*), optional, intent(out) :: root
  integer,          optional, intent(out) :: ierr
end subroutine iotk_open_read_x
end interface

interface iotk_close_read
subroutine iotk_close_read_x(unit,ierr)
  implicit none
  integer,                intent(in)  :: unit
  integer,      optional, intent(out) :: ierr
end subroutine iotk_close_read_x
end interface

interface iotk_magic
subroutine iotk_magic_x(file,binary)
  implicit none
  character(len=*), intent(in)  :: file
  logical,          intent(out) :: binary
end subroutine iotk_magic_x
end interface

end module iotk_files_interf
