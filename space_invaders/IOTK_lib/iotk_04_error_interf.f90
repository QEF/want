# 1 "iotk_error_interf.spp"
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
! Unit for errors
#ifndef __IOTK_ERROR_UNIT
#  define __IOTK_ERROR_UNIT 0
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
# 50 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_LOGICAL1
# 50 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_LOGICAL2
# 50 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_LOGICAL3
# 50 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_LOGICAL4
# 52 "../include/iotk_auxmacros.spp"
#define __IOTK_LOGICAL1 iotk_defkind_LOGICAL
# 55 "../include/iotk_auxmacros.spp"
#endif
# 55 "../include/iotk_auxmacros.spp"
#endif
# 55 "../include/iotk_auxmacros.spp"
#endif
# 55 "../include/iotk_auxmacros.spp"
#endif
# 50 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_INTEGER1
# 50 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_INTEGER2
# 50 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_INTEGER3
# 50 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_INTEGER4
# 52 "../include/iotk_auxmacros.spp"
#define __IOTK_INTEGER1 iotk_defkind_INTEGER
# 55 "../include/iotk_auxmacros.spp"
#endif
# 55 "../include/iotk_auxmacros.spp"
#endif
# 55 "../include/iotk_auxmacros.spp"
#endif
# 55 "../include/iotk_auxmacros.spp"
#endif
# 50 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_REAL1
# 50 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_REAL2
# 50 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_REAL3
# 50 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_REAL4
# 52 "../include/iotk_auxmacros.spp"
#define __IOTK_REAL1 iotk_defkind_REAL
# 55 "../include/iotk_auxmacros.spp"
#endif
# 55 "../include/iotk_auxmacros.spp"
#endif
# 55 "../include/iotk_auxmacros.spp"
#endif
# 55 "../include/iotk_auxmacros.spp"
#endif
# 58 "../include/iotk_auxmacros.spp"

! Some useful check follow
#if __IOTK_MAXRANK > 7
#  error
#endif
#if __IOTK_MAXRANK < 1
#  error
#endif
# 68 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_LOGICAL5
#  error
#endif
# 68 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_LOGICAL6
#  error
#endif
# 68 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_LOGICAL7
#  error
#endif
# 68 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_LOGICAL8
#  error
#endif
# 68 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_LOGICAL9
#  error
#endif
# 68 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_LOGICAL10
#  error
#endif
# 68 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_INTEGER5
#  error
#endif
# 68 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_INTEGER6
#  error
#endif
# 68 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_INTEGER7
#  error
#endif
# 68 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_INTEGER8
#  error
#endif
# 68 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_INTEGER9
#  error
#endif
# 68 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_INTEGER10
#  error
#endif
# 68 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_REAL5
#  error
#endif
# 68 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_REAL6
#  error
#endif
# 68 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_REAL7
#  error
#endif
# 68 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_REAL8
#  error
#endif
# 68 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_REAL9
#  error
#endif
# 68 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_REAL10
#  error
#endif
# 73 "../include/iotk_auxmacros.spp"
#endif

! Complex are treated indentically to reals
! These lines map the definitions.
# 78 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_REAL1
#  define __IOTK_COMPLEX1 __IOTK_REAL1
#else
#  undef __IOTK_COMPLEX1
#endif
# 78 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_REAL2
#  define __IOTK_COMPLEX2 __IOTK_REAL2
#else
#  undef __IOTK_COMPLEX2
#endif
# 78 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_REAL3
#  define __IOTK_COMPLEX3 __IOTK_REAL3
#else
#  undef __IOTK_COMPLEX3
#endif
# 78 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_REAL4
#  define __IOTK_COMPLEX4 __IOTK_REAL4
#else
#  undef __IOTK_COMPLEX4
#endif
# 84 "../include/iotk_auxmacros.spp"


# 30 "iotk_error_interf.spp"

module iotk_error_interf
implicit none
private

public :: iotk_error_init
public :: iotk_error_clear
public :: iotk_error_append
public :: iotk_error_add
public :: iotk_error_print
public :: iotk_error_issue
public :: iotk_error_check
public :: iotk_error_msg
public :: iotk_error_write
public :: iotk_error_scan
public :: iotk_error_handler
public :: iotk_error_pool_pending

interface iotk_error_init
subroutine iotk_error_init_e(error)
  use iotk_base
  implicit none
  type(iotk_error), intent(out) :: error
end subroutine iotk_error_init_e
subroutine iotk_error_init_i(ierr)
  use iotk_base
  implicit none
  integer, intent(out) :: ierr
end subroutine iotk_error_init_i
end interface

interface iotk_error_clear
subroutine iotk_error_clear_e(error)
  use iotk_base
  implicit none
  type(iotk_error), intent(inout) :: error
end subroutine iotk_error_clear_e
subroutine iotk_error_clear_i(ierr)
  use iotk_base
  implicit none
  integer, intent(inout) :: ierr
end subroutine iotk_error_clear_i
end interface

interface iotk_error_add
function iotk_error_add_x()
  use iotk_base
  implicit none
  integer :: iotk_error_add_x
end function iotk_error_add_x
end interface

interface iotk_error_append
subroutine iotk_error_append_e(error,str)
  use iotk_base
  implicit none
  type(iotk_error), intent(inout) :: error
  character(len=*), intent(in)    :: str
end subroutine iotk_error_append_e
subroutine iotk_error_append_i(ierr,str)
  use iotk_base
  implicit none
  integer, intent(inout) :: ierr
  character(len=*), intent(in)    :: str
end subroutine iotk_error_append_i
end interface

interface iotk_error_print
subroutine iotk_error_print_e(error,unit)
  use iotk_base
  implicit none
  type(iotk_error), intent(in) :: error
  integer,          intent(in) :: unit
end subroutine iotk_error_print_e
subroutine iotk_error_print_i(ierr,unit)
  use iotk_base
  implicit none
  integer, intent(in) :: ierr
  integer, intent(in) :: unit
end subroutine iotk_error_print_i
end interface

interface iotk_error_issue
subroutine iotk_error_issue_e(error,sub,file,line)
  use iotk_base
  implicit none
  type(iotk_error), intent(inout) :: error
  character(len=*), intent(in)    :: sub
  character(len=*), intent(in)    :: file
  integer,          intent(in)    :: line
end subroutine iotk_error_issue_e
subroutine iotk_error_issue_i(ierr,sub,file,line)
  use iotk_base
  implicit none
  integer,          intent(inout) :: ierr
  character(len=*), intent(in)    :: sub
  character(len=*), intent(in)    :: file
  integer,          intent(in)    :: line
end subroutine iotk_error_issue_i
end interface

interface iotk_error_msg
subroutine iotk_error_msg_e(error,msg)
  use iotk_base
  implicit none
  type(iotk_error), intent(inout) :: error
  character(len=*), intent(in)    :: msg
end subroutine iotk_error_msg_e
subroutine iotk_error_msg_i(ierr,msg)
  use iotk_base
  implicit none
  integer,          intent(inout) :: ierr
  character(len=*), intent(in)    :: msg
end subroutine iotk_error_msg_i
end interface

interface iotk_error_check
function iotk_error_check_e(error)
  use iotk_base
  implicit none
  type(iotk_error), intent(in) :: error
  logical :: iotk_error_check_e
end function iotk_error_check_e
function iotk_error_check_i(ierr)
  use iotk_base
  implicit none
  integer, intent(in) :: ierr
  logical :: iotk_error_check_i
end function iotk_error_check_i
end interface

interface iotk_error_write
subroutine iotk_error_write_character_e(error,name,val)
  use iotk_base
  implicit none
  type(iotk_error), intent(inout) :: error
  character(len=*), intent(in)    :: name
  character(len=*), intent(in)    :: val
end subroutine iotk_error_write_character_e
subroutine iotk_error_write_character_i(ierr,name,val)
  use iotk_base
  implicit none
  integer, intent(inout) :: ierr
  character(len=*), intent(in)    :: name
  character(len=*), intent(in)    :: val
end subroutine iotk_error_write_character_i
subroutine iotk_error_write_logical_e(error,name,val)
  use iotk_base
  implicit none
  type(iotk_error), intent(inout) :: error
  character(len=*), intent(in)    :: name
  logical,          intent(in)    :: val
end subroutine iotk_error_write_logical_e
subroutine iotk_error_write_logical_i(ierr,name,val)
  use iotk_base
  implicit none
  integer, intent(inout) :: ierr
  character(len=*), intent(in)    :: name
  logical,          intent(in)    :: val
end subroutine iotk_error_write_logical_i 
subroutine iotk_error_write_integer_e(error,name,val)
  use iotk_base
  implicit none
  type(iotk_error), intent(inout) :: error
  character(len=*), intent(in)    :: name
  integer,          intent(in)    :: val
end subroutine iotk_error_write_integer_e
subroutine iotk_error_write_integer_i(ierr,name,val)
  use iotk_base
  implicit none
  integer, intent(inout) :: ierr
  character(len=*), intent(in)    :: name
  integer,          intent(in)    :: val
end subroutine iotk_error_write_integer_i
end interface

interface iotk_error_scan
subroutine iotk_error_scan_character_e(error,name,val)
  use iotk_base
  implicit none
  type(iotk_error), intent(in) :: error
  character(len=*), intent(in) :: name
  character(len=*), intent(out):: val
end subroutine iotk_error_scan_character_e
subroutine iotk_error_scan_character_i(ierr,name,val)
  use iotk_base
  implicit none
  integer, intent(in) :: ierr
  character(len=*), intent(in) :: name
  character(len=*), intent(out):: val
end subroutine iotk_error_scan_character_i
subroutine iotk_error_scan_logical_e(error,name,val)
  use iotk_base
  implicit none
  type(iotk_error), intent(in) :: error
  character(len=*), intent(in) :: name
  logical,          intent(out):: val
end subroutine iotk_error_scan_logical_e
subroutine iotk_error_scan_logical_i(ierr,name,val)
  use iotk_base
  implicit none
  integer, intent(in) :: ierr
  character(len=*), intent(in) :: name
  logical,          intent(out):: val
end subroutine iotk_error_scan_logical_i
subroutine iotk_error_scan_integer_e(error,name,val)
  use iotk_base
  implicit none
  type(iotk_error), intent(in) :: error
  character(len=*), intent(in) :: name
  integer,          intent(out):: val
end subroutine iotk_error_scan_integer_e
subroutine iotk_error_scan_integer_i(ierr,name,val)
  use iotk_base
  implicit none
  integer, intent(in) :: ierr
  character(len=*), intent(in) :: name
  integer,          intent(out):: val
end subroutine iotk_error_scan_integer_i
end interface

interface iotk_error_pool_pending
function iotk_error_pool_pending_x()
  use iotk_base
  implicit none
  integer :: iotk_error_pool_pending_x
end function iotk_error_pool_pending_x
end interface

interface iotk_error_handler
subroutine iotk_error_handler_x(ierr)
  use iotk_base
  implicit none
  integer, intent(in) :: ierr
end subroutine iotk_error_handler_x
end interface

end module iotk_error_interf
