!
! Copyright (C) 2015 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!********************************************************
    SUBROUTINE read_fileqp( fileqp, ndim1, ndim2, ndim3, eig)
    !********************************************************
    !
    ! eigs are updated
    !
    USE kinds,         ONLY : dbl
    USE iotk_module,   ONLY : iotk_free_unit
    USE parser_module, ONLY : matches
    IMPLICIT NONE
    !
    ! input vars
    !
    CHARACTER(*), INTENT(IN) :: fileqp
    INTEGER,      INTENT(IN) :: ndim1, ndim2, ndim3
    REAL(dbl),    INTENT(INOUT) :: eig(ndim1,ndim2,ndim3)
    !
    ! local vars
    !
    CHARACTER(11)  :: subname="read_fileqp"
    CHARACTER(256) :: line
    INTEGER   :: ierr, iunit, nline, NF
    INTEGER   :: ib, ik, isp, i
    LOGICAL   :: file_is_HF,file_is_QP
    LOGICAL   :: detect_NF
    REAL(dbl) :: rb, rk, rsp, rtmp
    REAL(dbl) :: E0, E_QP, DE_QP, sgmc

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
    ! detect extension
    !
    file_is_QP=matches(".qp_@__@",trim(fileqp)//"_@__@")
    file_is_HF=matches(".hf_@__@",trim(fileqp)//"_@__@")
    !
    if (.not.file_is_HF .and. .not. file_is_QP) &
       CALL errore(subname,"unknown extension of fileqp = "//TRIM(fileqp),10)
    !
    detect_NF=.TRUE.
    NF=0
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
        ! at first detect the number of columns
        !
        IF ( detect_NF ) THEN
           !
           NF=0
           !
           line_scan:&
           DO WHILE (.TRUE.)
              NF=NF+1
              READ(line,*,IOSTAT=ierr) (rtmp, i=1,NF)  
              IF (ierr/=0) EXIT line_scan
           ENDDO line_scan
           !
           NF=NF-1
           !
           nline=0
           detect_NF=.FALSE.
           REWIND(iunit)
           !
        ENDIF
        !
        ! actual read-in
        !
        IF ( file_is_QP .and. NF == 6 ) THEN
           READ(line,*,IOSTAT=ierr) rk, rb, E0, DE_QP, sgmc, rsp
           IF ( ierr/=0 ) CALL errore(subname,"invalid line fmt I",nline)
        ELSE IF ( file_is_QP .and. NF == 5 ) THEN
           READ(line,*,IOSTAT=ierr) rk, rb, E0, DE_QP, sgmc
           IF ( ierr/=0 ) CALL errore(subname,"invalid line fmt II",nline)
           rsp=1
        ELSE IF ( file_is_HF .and. NF == 6 ) THEN
           READ(line,*,IOSTAT=ierr) rk, rb, E0, E_QP, rtmp, rtmp
           IF ( ierr/=0 ) CALL errore(subname,"invalid line fmt III",nline)
           rsp=1
           DE_QP=E_QP-E0
        ELSE 
           CALL errore(subname,"fmt not supported",10)
        ENDIF
        !
        ik=NINT(rk)
        ib=NINT(rb)
        isp=NINT(rsp)
        IF ( isp == -1 ) isp=2
        !
        IF ( ib < 0 .OR. ib > ndim1 ) CALL errore(subname,"invalid nbnd",ib)
        IF ( ik < 0 .OR. ik > ndim2 ) CALL errore(subname,"invalid nkpts",ik)
        IF ( isp < 0 .OR. isp > ndim3 ) CALL errore(subname,"invalid nspin",isp)
        !
        eig(ib,ik,isp) = E0 + DE_QP
        !
    ENDDO loop_scan
    !
    RETURN
    ! 
END SUBROUTINE read_fileqp

