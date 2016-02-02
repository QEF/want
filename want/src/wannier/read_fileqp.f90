!
! Copyright (C) 2015 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!********************************************************
    SUBROUTINE read_fileqp( fileqp, dim1, dim2, dim3, eig)
    !********************************************************
    !
    ! eigs are updated
    !
    USE kinds,        ONLY : dbl
    USE iotk_module,  ONLY : iotk_free_unit
    IMPLICIT NONE
    !
    ! input vars
    !
    CHARACTER(*), INTENT(IN) :: fileqp
    INTEGER,      INTENT(IN) :: dim1, dim2, dim3
    REAL(dbl),    INTENT(INOUT) :: eig(dim1,dim2,dim3)
    !
    ! local vars
    !
    CHARACTER(11)  :: subname="read_fileqp"
    CHARACTER(256) :: line
    INTEGER   :: ierr, iunit,nline
    INTEGER   :: ib, ik, isp
    REAL(dbl) :: rb, rk, rsp
    REAL(dbl) :: E0, DE_QP, sgmc

!
!=========================
!
    !
    ! parse the QP file
    !
    CALL iotk_free_unit(iunit)
    !
    OPEN( iunit, FILE=fileqp, STATUS="old", IOSTAT=ierr)
    IF ( ierr/=0 ) CALL errore(subname,"opening "//TRIM(fileqp), ABS(ierr))
    !
    nline=0
    !
    loop_scan:&
    DO WHILE (.TRUE.)
        !
        line=""
        READ(iunit, "(A)", IOSTAT=ierr) line
        IF ( ierr/=0 ) EXIT loop_scan
        nline=nline+1
        !
        IF ( line(1:1) == "#" .OR. LEN_TRIM(line) ==0 ) CYCLE 
        !
        READ(line,*,IOSTAT=ierr) rk, rb, E0, DE_QP, sgmc, rsp
        IF ( ierr/=0 ) CALL errore(subname,"invalid line fmt",nline)
        !
        ik=NINT(rk)
        ib=NINT(rb)
        isp=NINT(rsp)
        IF ( isp == -1 ) isp=2
        !
        IF ( ib < 0 .OR. ib > dim1 ) CALL errore(subname,"invalid nbnd",ib)
        IF ( ik < 0 .OR. ik > dim2 ) CALL errore(subname,"invalid nkpts",ik)
        IF ( isp < 0 .OR. isp > dim3 ) CALL errore(subname,"invalid nspin",isp)
        !
        !eig(ib,ik,isp) = eig(ib,ik,isp) + DE_QP
        eig(ib,ik,isp) = E0 + DE_QP
        !
    ENDDO loop_scan
    !
    RETURN
    ! 
END SUBROUTINE read_fileqp

