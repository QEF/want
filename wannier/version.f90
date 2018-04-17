! 
! Copyright (C) 2004 WanT Group
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 
   MODULE version_module
#    include "version.h"
     character(LEN=4), PARAMETER   :: version_name=  __VERSION_NAME 
     character(LEN=1), PARAMETER   :: version_major= __VERSION_MAJOR
     character(LEN=1), PARAMETER   :: version_minor= __VERSION_MINOR
     character(LEN=1), PARAMETER   :: version_patch= __VERSION_PATCH
     character(LEN=10),PARAMETER   :: version_number=  version_name//"-"//   &
                                                       version_major//"."//  &
                                                       version_minor//"."//  &
                                                       version_patch
   END MODULE version_module
