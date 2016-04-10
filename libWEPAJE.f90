!                                                          // -*- Fortran90 -*-

!/*****************************************************************************/
!/*                                                                           */
!/* (c) Copyright 2005                                                        */
!/*     Numerica Ltda                                                         */
!/*     All rights reserved.                                                  */
!/*                                                                           */
!/*     See http://numerica.com.co/ for further details.                      */
!/*                                                                           */

!/*****************************************************************************/
!/
!/ Elkin Arroyo-Negrete, Sun 05/15/2005, 14:00:00
! 
! Wraper for Well Models. LIB WEPAJE is a trademark by Elkin Arroyo-Negrete

! WEPAJE: WEll Production Analisys Just Easy
! 

! precision of 32-, 64-, and 128-bit reals can usually be obtained with 
! the following constants
! integer, parameter :: sp = selected_real_kind(6, 37)
! integer, parameter :: dp = selected_real_kind(15, 307)
! integer, parameter :: qp = selected_real_kind(33, 4931)

!!#if defined(__GFORTRAN__) ... #elif defined(__INTEL_COMPILER)


!--------------------------------------------------------
#if defined(__G95__)
FUNCTION qDNFBDLL(t,R) Result(qD) 
#else
F_STDCALL FUNCTION qDNFBDLL(t,R) Result(qD) 
#endif
!--------------------------------------------------------
!GCC$ ATTRIBUTES STDCALL :: qDNFBDLL
USE well_models
! USE MSWIN
IMPLICIT NONE
INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(15, 307)
INTEGER, PARAMETER :: sp = SELECTED_REAL_KIND(6, 37)


real(sp), intent(in) :: t
real(sp), intent(in) :: R
real(sp)  qD

real(dp) :: tt
real(dp) :: RR
real(dp) :: qDqD


! Open comunication file in current directory
! Make sure DLL is in the same directory as the 
! Excel file from where to call it !
! G95 Syntaxis for current directory
! INTEGER FUNCTION getcwd(name)
! CHARACTER(LEN=*), INTENT(OUT) :: name
! END FUNCTION
! Returns the current working directory in name. Returns nonzero if there is an error.

CHARACTER (LEN=256) FILENAME
CHARACTER (LEN=256)::CURDIR
INTEGER            ::erro
#if defined(__G95__)
erro=getcwd(CURDIR)
FILENAME = TRIM(CURDIR)//'\qDNFBDLL.txt'
#else
FILENAME = TRIM(CURDIR@())//'\qDNFBDLL.txt'
#endif

open (unit = 101, file = FILENAME)
read(101,*) tt
read(101,*) RR
close(101)


! Finally Call DimensioneLess Solution
qDqD = qDNFB(tt,RR)

! Check if it returns an NaN
!IF(isnan(qDqD)) THEN
!  qD=0.d0
!  return
!END IF
IF (qDqD .NE. qDqD) THEN 
  qDqD=0.d0
END IF

qD = qDqD

#if defined(__G95__)
erro=getcwd(CURDIR)
FILENAME = TRIM(CURDIR)//'\qDNFBDLL_Output.txt'
#else
FILENAME = TRIM(CURDIR@())//'\qDNFBDLL_Output.txt'
#endif
open (unit = 101, file = FILENAME)
write(101,*) qDqD
close(101)

END FUNCTION qDNFBDLL

!--------------------------------------------------------
#if defined(__G95__)
FUNCTION qDFRACNFBDLL(t,reD,xD) Result(qD) 
#else
F_STDCALL FUNCTION qDFRACNFBDLL(t,reD,xD) Result(qD) 
#endif
!--------------------------------------------------------
!GCC$ ATTRIBUTES STDCALL :: qDFRACNFBDLL
USE well_models
IMPLICIT NONE
INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(15, 307)
INTEGER, PARAMETER :: sp = SELECTED_REAL_KIND(6, 37)


real(sp), intent(in) :: t
real(sp), intent(in) :: reD
real(sp), intent(in) :: xD
real(sp)  qD

real(dp) :: tt
real(dp) :: RR
real(dp) :: xDxD
real(dp) :: qDqD

CHARACTER (LEN=256) FILENAME
CHARACTER (LEN=256)::CURDIR
INTEGER            ::erro

#if defined(__G95__)
erro=getcwd(CURDIR)
FILENAME = TRIM(CURDIR)//'\qDFRACNFBDLL.txt'
#else
FILENAME = TRIM(CURDIR@())//'\qDFRACNFBDLL.txt'
#endif

open (unit = 101, file = FILENAME)

read(101,*) tt
read(101,*) RR
read(101,*) xDxD

close(101)


! Finally Call DimensioneLess Solution
! qDqD = qDFracNFB(tt,RR,xDxD)
! FUNCTION qDFracNFB(t,reD,xD,alpha_opt, tol_opt,M_opt) result(qD)
qDqD = qDFracNFB(tt,RR,xDxD)


! Check if it returns an NaN
!IF(isnan(qDqD)) THEN
!  qD=0.d0
!  return
!END IF

IF (qDqD .NE. qDqD) THEN 
  qDqD=0.d0
END IF

qD = qDqD

! Excel is crashing when using the returned valued
! read the response from file
#if defined(__G95__)
erro=getcwd(CURDIR)
FILENAME = TRIM(CURDIR)//'\qDFRACNFBDLL_Output.txt'
#else
FILENAME = TRIM(CURDIR@())//'\qDFRACNFBDLL_Output.txt'
#endif
open (unit = 101, file = FILENAME)
write(101,*) qDqD
close(101)

END FUNCTION qDFRACNFBDLL


!--------------------------------------------------------
#if defined(__G95__)
FUNCTION cumDFRACNFBDLL(t,reD,xD) Result(qD) 
#else
F_STDCALL FUNCTION cumDFRACNFBDLL(t,reD,xD) Result(qD) 
#endif
!--------------------------------------------------------
!GCC$ ATTRIBUTES STDCALL :: volDFRACNFBDLL
USE well_models
IMPLICIT NONE
INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(15, 307)
INTEGER, PARAMETER :: sp = SELECTED_REAL_KIND(6, 37)

real(sp), intent(in) :: t
real(sp), intent(in) :: reD
real(sp), intent(in) :: xD
real(sp)  qD

real(dp) :: tt
real(dp) :: RR
real(dp) :: xDxD
real(dp) :: qDqD

CHARACTER (LEN=256) FILENAME
CHARACTER (LEN=256)::CURDIR
INTEGER            ::erro

#if defined(__G95__)
erro=getcwd(CURDIR)
FILENAME = TRIM(CURDIR)//'\qDFRACNFBDLL.txt'
#else
FILENAME = TRIM(CURDIR@())//'\qDFRACNFBDLL.txt'
#endif

open (unit = 101, file = FILENAME)
read(101,*) tt
read(101,*) RR
read(101,*) xDxD

close(101)

! Finally Call DimensioneLess Solution
!      cumDFracNFB(t,reD,xD,alpha_opt, tol_opt,M_opt)
qDqD = cumDFracNFB(tt,RR,xDxD)

! Check if it returns an NaN
IF (qDqD .NE. qDqD) THEN 
  qDqD=0.d0
END IF

qD = qDqD

! Excel is crashing when using the returned valued
! read the response from file
#if defined(__G95__)
erro=getcwd(CURDIR)
FILENAME = TRIM(CURDIR)//'\qDFRACNFBDLL_Output.txt'
#else
FILENAME = TRIM(CURDIR@())//'\qDFRACNFBDLL_Output.txt'
#endif
open (unit = 101, file = FILENAME)
write(101,*)  qDqD
close(101)

END FUNCTION cumDFRACNFBDLL


!--------------------------------------------------------
#if defined(__G95__)
FUNCTION qDFRACNFBDPDLL(t,reD,xD,w,lamda) Result(qD) 
#else
F_STDCALL FUNCTION qDFRACNFBDPDLL(t,reD,xD,w,lamda) Result(qD) 
#endif
!--------------------------------------------------------
!GCC$ ATTRIBUTES STDCALL :: qDFRACNFBDLL
USE well_models
IMPLICIT NONE
INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(15, 307)
INTEGER, PARAMETER :: sp = SELECTED_REAL_KIND(6, 37)


real(sp), intent(in) :: t
real(sp), intent(in) :: reD
real(sp), intent(in) :: xD
real(sp), intent(in) :: w
real(sp), intent(in) :: lamda
real(sp) :: qD


real(dp) :: tt
real(dp) :: RR
real(dp) :: xDxD
real(dp) :: qDqD

real(dp) :: w_

real(dp) :: lamda_

CHARACTER (LEN=256) FILENAME
CHARACTER (LEN=256)::CURDIR
INTEGER            ::erro

#if defined(__G95__)
erro=getcwd(CURDIR)
FILENAME = TRIM(CURDIR)//'\qDFRACNFBDPDLL.txt'
#else
FILENAME = TRIM(CURDIR@())//'\qDFRACNFBDPDLL.txt'
#endif

open (unit = 101, file = FILENAME)

read(101,*) tt
read(101,*) RR
read(101,*) xDxD
read(101,*) w_
read(101,*) lamda_

close(101)


! Finally Call DimensioneLess Solution
qDqD = qDFracNFBDP(tt,RR,xDxD,w_,lamda_)

! qDqD = qDFracNFBDP(0.415052684931297D0, 182.618618091373D0, 0.732D0, 0.5D0, 3.2D-9)

!open (unit = 101, file = "X:\FractureWell\HOOG\qDFracNFBDP.log")
!write(101,*) 'qD = qDFracNFBDP(tD,rD,xD,w,lamda)'
!write(101,*) 'qD=', qDqD 
!write(101,*) 'tD,rD,xD,w,lamda'
!write(101,*) tt,RR,xDxD,w_,lamda_
!close(101)


! Check if it returns an NaN
!IF(isnan(qDqD)) THEN
!  qD=0.d0
!  return
!END IF

!IF (qDqD .NE. qDqD) THEN 
!  qDqD=0.d0
!END IF

qD = qDqD

! Excel is crashing when using the returned valued
! read the response from file
#if defined(__G95__)
erro=getcwd(CURDIR)
FILENAME = TRIM(CURDIR)//'\qDFRACNFBDPDLL_Output.txt'
#else
FILENAME = TRIM(CURDIR@())//'\qDFRACNFBDPDLL_Output.txt'
#endif
open (unit = 101, file = FILENAME)
write(101,*) qDqD
!write(101,*) 'tD,rD,xD,w,lamda'
!write(101,*) tt,RR,xDxD,w_,lamda_
close(101)

END FUNCTION qDFRACNFBDPDLL

!--------------------------------------------------------
! FUNCTION pDVSqNFBDLL(t_,xeD_,yeD_,xwD_,ywD_) Result(pD_) 
FUNCTION pDVSqNFBDLL() Result(pD_) 

!--------------------------------------------------------
USE well_models
IMPLICIT NONE
INTEGER, PARAMETER :: sp = SELECTED_REAL_KIND(6, 37)
INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(15, 307)

! real(sp), intent(in) :: t_
! real(sp), intent(in) :: xeD_
! real(sp), intent(in) :: yeD_
! real(sp), intent(in) :: xwD_
! real(sp), intent(in) :: ywD_
real(dp) :: pD
real(sp) :: pD_
real(dp) :: t,xD,xeD,yeD,xwD,ywD,w,lamda,Cd,skin

CHARACTER (LEN=256) FILENAME
CHARACTER (LEN=256) ERRFILENAME
CHARACTER (LEN=256)::CURDIR
INTEGER            ::ioerr
INTEGER            ::erro
INTEGER            ::io
erro=getcwd(CURDIR)

ERRFILENAME = TRIM(CURDIR)//'\pDVSqNFBDLL_Err.txt'
open (unit = 911, file = ERRFILENAME,IOSTAT=ioerr)
write(911,*) 'At beginig of file ', ERRFILENAME
write(911,*) 'Reported IOSTAT=',ioerr
close(911)

FILENAME = TRIM(CURDIR)//'\pDVSqNFBDLL.txt'
OPEN (UNIT = 101, FILE = FILENAME, IOSTAT=ioerr, ERR=100)
read(101,*,IOSTAT=io) t
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
END IF
read(101,*,IOSTAT=io) xeD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
END IF

read(101,*,IOSTAT=io) yeD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
END IF

read(101,*,IOSTAT=io) xwD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
END IF

read(101,*,IOSTAT=io) ywD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
END IF

read(101,*,IOSTAT=io) xD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
END IF

read(101,*,IOSTAT=io) w
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
END IF

read(101,*,IOSTAT=io) lamda
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
END IF

read(101,*,IOSTAT=io) Cd
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
END IF

read(101,*,IOSTAT=io) skin
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
END IF

close(101)

!ERRFILENAME = TRIM(CURDIR)//'\pDVSqNFBDLL_Err_No.txt'
!open (unit = 911, file = ERRFILENAME,IOSTAT=ioerr)
!write(911,*) 'After reading file file ', ERRFILENAME
!write(911,*) 'Reported IOSTAT=',ioerr
!write(911,*) t
!write(911,*) xeD
!write(911,*) yeD
!write(911,*) xwD
!write(911,*) ywD
!write(911,*) xD
!write(911,*) w
!write(911,*) lamda
!write(911,*) Cd
!write(911,*) skin
!close(911)


! Finally Call DimensioneLess Solution
! xD   = 0.732d0
! w    = 1.0d0
! lamda= 1.0d0
! Cd  = 0.0d0
! skin= 0.0d0 
pD  = pDVSqNFB(t,xD,xeD,yeD,xwD,ywD,w,lamda,Cd,skin)
pD_ = pD
! pD = pDVSqNFB(t,xeD,yeD,xwD,ywD)
FILENAME = TRIM(CURDIR)//'\pDVSqNFBDLL_Output.txt'
open (unit = 101, file = FILENAME)
write(101,*) pD
close(101)
return

100 ERRFILENAME = TRIM(CURDIR)//'\pDVSqNFBDLL_Err.txt'
    close(101)
    open (unit = 911, file = ERRFILENAME)
    IF (ioerr .NE. 0)  THEN
       write(911,*) 'Error opening file ', FILENAME
       write(911,*) 'Reported IOSTAT=',ioerr
    END IF
    IF (io .NE. 0)  THEN
       write(911,*) 'Error reading file ', FILENAME
       write(911,*) 'Reported IOSTAT=',io
    END IF    
    close(911)
END FUNCTION pDVSqNFBDLL

!        QDVSQNFBDLL(t, xD, xed, yed, xwd, ywd, w, lamda, Cd, Skin)
!---------------------------------------------------------------------------------
FUNCTION qDVSqNFBDLL(t_,xD_,xeD_,yeD_,xwD_,ywD_,w_,lamda_,Cd_,skin_) Result(pD_) 
!---------------------------------------------------------------------------------
USE well_models
IMPLICIT NONE
INTEGER, PARAMETER :: sp = SELECTED_REAL_KIND(6, 37)
INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(15, 307)

real(sp), intent(in) :: t_
real(sp), intent(in) :: xD_
real(sp), intent(in) :: xeD_
real(sp), intent(in) :: yeD_
real(sp), intent(in) :: xwD_
real(sp), intent(in) :: ywD_
real(sp), intent(in) :: w_
real(sp), intent(in) :: lamda_
real(sp), intent(in) :: Cd_
real(sp), intent(in) :: skin_
real(dp) :: pD
real(sp) :: pD_
real(dp) :: t,xD,xeD,yeD,xwD,ywD,w,lamda,Cd,skin

CHARACTER (LEN=256) FILENAME
CHARACTER (LEN=256) ERRFILENAME
CHARACTER (LEN=256)::CURDIR
INTEGER            ::ioerr
INTEGER            ::erro
INTEGER            ::io
erro=getcwd(CURDIR)


FILENAME = TRIM(CURDIR)//'\qDVSqNFBDLL.txt'
OPEN (UNIT = 101, FILE = FILENAME, IOSTAT=ioerr, ERR=100)

read(101,*,IOSTAT=io) t
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) xeD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) yeD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) xwD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) ywD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) xD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) w
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) lamda
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) Cd
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) skin
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
close(101)

! Finally Call DimensioneLess Solution
pD  = qDVSqNFB(t,xD,xeD,yeD,xwD,ywD,w,lamda,Cd,skin)
pD_ = pD

FILENAME = TRIM(CURDIR)//'\qDVSqNFBDLL_Output.txt'
open (unit = 101, file = FILENAME)
write(101,*) pD
close(101)
return

100 ERRFILENAME = TRIM(CURDIR)//'\qDVSqNFBDLL_Err.txt'
    close(101)
    open (unit = 911, file = ERRFILENAME)
    IF (ioerr .NE. 0)  THEN
       write(911,*) 'Error opening file ', FILENAME
       write(911,*) 'Reported IOSTAT=',ioerr
    END IF
    IF (io .NE. 0)  THEN
       write(911,*) 'Error reading file ', FILENAME
       write(911,*) 'Reported IOSTAT=',io
    END IF    
    close(911)
END FUNCTION qDVSqNFBDLL

!---------------------------------------------------------------------------------
FUNCTION CumqDVSqNFBDLL(t_,xD_,xeD_,yeD_,xwD_,ywD_,w_,lamda_,Cd_,skin_) Result(pD_) 
!---------------------------------------------------------------------------------
USE well_models
IMPLICIT NONE
INTEGER, PARAMETER :: sp = SELECTED_REAL_KIND(6, 37)
INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(15, 307)

real(sp), intent(in) :: t_
real(sp), intent(in) :: xD_
real(sp), intent(in) :: xeD_
real(sp), intent(in) :: yeD_
real(sp), intent(in) :: xwD_
real(sp), intent(in) :: ywD_
real(sp), intent(in) :: w_
real(sp), intent(in) :: lamda_
real(sp), intent(in) :: Cd_
real(sp), intent(in) :: skin_
real(dp) :: pD
real(sp) :: pD_
real(dp) :: t,xD,xeD,yeD,xwD,ywD,w,lamda,Cd,skin

CHARACTER (LEN=256) FILENAME
CHARACTER (LEN=256) ERRFILENAME
CHARACTER (LEN=256)::CURDIR
INTEGER            ::ioerr
INTEGER            ::erro
INTEGER            ::io
erro=getcwd(CURDIR)


FILENAME = TRIM(CURDIR)//'\qDVSqNFBDLL.txt'
OPEN (UNIT = 101, FILE = FILENAME, IOSTAT=ioerr, ERR=100)

read(101,*,IOSTAT=io) t
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) xeD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) yeD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) xwD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) ywD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) xD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) w
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) lamda
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) Cd
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) skin
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
close(101)

! Finally Call DimensioneLess Solution
pD  = CumqDVSqNFB(t,xD,xeD,yeD,xwD,ywD,w,lamda,Cd,skin)
pD_ = pD

FILENAME = TRIM(CURDIR)//'\qDVSqNFBDLL_Output.txt'
open (unit = 101, file = FILENAME)
write(101,*) pD
close(101)
return

100 ERRFILENAME = TRIM(CURDIR)//'\qDVSqNFBDLL_Err.txt'
    close(101)
    open (unit = 911, file = ERRFILENAME)
    IF (ioerr .NE. 0)  THEN
       write(911,*) 'Error opening file ', FILENAME
       write(911,*) 'Reported IOSTAT=',ioerr
    END IF
    IF (io .NE. 0)  THEN
       write(911,*) 'Error reading file ', FILENAME
       write(911,*) 'Reported IOSTAT=',io
    END IF    
    close(911)
END FUNCTION CumqDVSqNFBDLL

!---------------------------------------------------------------------------------------------------
FUNCTION pDHorzNFBDLL(tD_,xD_,xeD_,xwD_,yD_,yeD_,ywD_,zD_,zwD_,LD_,w_,lambda_,Cd_,skin_) Result(pD_) 
!---------------------------------------------------------------------------------------------------
USE well_models
IMPLICIT NONE
INTEGER, PARAMETER :: sp = SELECTED_REAL_KIND(6, 37)
INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(15, 307)

real(sp), intent(in) :: tD_
real(sp), intent(in) :: xD_
real(sp), intent(in) :: xeD_
real(sp), intent(in) :: xwD_
real(sp), intent(in) :: yD_
real(sp), intent(in) :: yeD_
real(sp), intent(in) :: ywD_
real(sp), intent(in) :: zD_
real(sp), intent(in) :: zwD_
real(sp), intent(in) :: LD_
real(sp), intent(in) :: w_
real(sp), intent(in) :: lambda_
real(sp), intent(in) :: Cd_
real(sp), intent(in) :: skin_
real(dp) :: pD
real(sp) :: pD_
real(dp) :: tD,xD,xeD,xwD,yD,yeD,ywD,zD,zwD,LD,w,lambda,Cd,skin

CHARACTER (LEN=256) FILENAME
CHARACTER (LEN=256) ERRFILENAME
CHARACTER (LEN=256)::CURDIR
INTEGER            ::ioerr
INTEGER            ::erro
INTEGER            ::io
erro=getcwd(CURDIR)

! ERRFILENAME = TRIM(CURDIR)//'\pDHorzNFBDLL_Err.txt'
! OPEN (UNIT = 102, FILE = ERRFILENAME, ACCESS='append', IOSTAT=ioerr, ERR=100)
! OPEN (UNIT = 102, FILE = ERRFILENAME, IOSTAT=ioerr, ERR=100)
! WRITE(102,*) 'Begin Function'

FILENAME = TRIM(CURDIR)//'\pDHorzNFBDLL.txt'
OPEN (UNIT = 101, FILE = FILENAME, IOSTAT=ioerr, ERR=100)

read(101,*,IOSTAT=io) tD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) xD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) xeD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) xwD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) yD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) yeD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) ywD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) zD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) zwD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) LD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) w
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) lambda
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) Cd
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) skin
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
close(101)

! WRITE(102,*) 'After Reading Function'
! WRITE(102,*) ! 'tD=',tD,'xD=',xD,'xeD=',xeD,'xwD=',xwD,'yD=',yD,'yeD=',yeD,'ywD=',ywD,'zD=',zD,'zwD=',zwD
! close(102) ! Function call will add stuff to file so close it temporally

! Finally Call DimensioneLess Solution
pD  = pDHorzNFB(tD,xD,xeD,xwD,yD,yeD,ywD,zD,zwD,LD,w,lambda,Cd,skin)
! pD_ = pD
pD_ = 0.d0

! OPEN (UNIT = 102, FILE = ERRFILENAME, ACCESS='append', IOSTAT=ioerr, ERR=100)
! WRITE(102,*) 'After Function Call'
! WRITE(102,*) 'pD=',pD
! close(102)

FILENAME = TRIM(CURDIR)//'\pDHorzNFBDLL_Output.txt'
open (unit = 101, file = FILENAME, IOSTAT=ioerr, ERR=100)
write(101,*) pD
close(101)
return

100 ERRFILENAME = TRIM(CURDIR)//'\pDHorzNFBDLL_Err.txt'
    close(101)
    close(102)
    open (unit = 911, file = ERRFILENAME)
    IF (ioerr .NE. 0)  THEN
       write(911,*) 'Error OPENING file ', FILENAME
       write(911,*) 'Reported IOSTAT=',ioerr
    END IF
    IF (io .NE. 0)  THEN
       write(911,*) 'Error READING file ', FILENAME
       write(911,*) 'Reported IOSTAT=',io
    END IF    
    close(911)

END FUNCTION pDHorzNFBDLL

!---------------------------------------------------------------------------------------------------
FUNCTION qDHorzNFBDLL(tD_,xD_,xeD_,xwD_,yD_,yeD_,ywD_,zD_,zwD_,LD_,w_,lambda_,Cd_,skin_) Result(pD_) 
!---------------------------------------------------------------------------------------------------
USE well_models
IMPLICIT NONE
INTEGER, PARAMETER :: sp = SELECTED_REAL_KIND(6, 37)
INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(15, 307)

real(sp), intent(in) :: tD_
real(sp), intent(in) :: xD_
real(sp), intent(in) :: xeD_
real(sp), intent(in) :: xwD_
real(sp), intent(in) :: yD_
real(sp), intent(in) :: yeD_
real(sp), intent(in) :: ywD_
real(sp), intent(in) :: zD_
real(sp), intent(in) :: zwD_
real(sp), intent(in) :: LD_
real(sp), intent(in) :: w_
real(sp), intent(in) :: lambda_
real(sp), intent(in) :: Cd_
real(sp), intent(in) :: skin_
real(dp) :: pD
real(sp) :: pD_
real(dp) :: tD,xD,xeD,xwD,yD,yeD,ywD,zD,zwD,LD,w,lambda,Cd,skin

CHARACTER (LEN=256) FILENAME
CHARACTER (LEN=256) ERRFILENAME
CHARACTER (LEN=256)::CURDIR
INTEGER            ::ioerr
INTEGER            ::erro
INTEGER            ::io
erro=getcwd(CURDIR)

! ERRFILENAME = TRIM(CURDIR)//'\pDHorzNFBDLL_Err.txt'
! OPEN (UNIT = 102, FILE = ERRFILENAME, ACCESS='append', IOSTAT=ioerr, ERR=100)
! OPEN (UNIT = 102, FILE = ERRFILENAME, IOSTAT=ioerr, ERR=100)
! WRITE(102,*) 'Begin Function'

FILENAME = TRIM(CURDIR)//'\pDHorzNFBDLL.txt'
OPEN (UNIT = 101, FILE = FILENAME, IOSTAT=ioerr, ERR=100)

read(101,*,IOSTAT=io) tD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) xD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) xeD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) xwD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) yD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) yeD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) ywD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) zD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) zwD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) LD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) w
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) lambda
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) Cd
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) skin
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
close(101)

! WRITE(102,*) 'After Reading Function'
! WRITE(102,*) ! 'tD=',tD,'xD=',xD,'xeD=',xeD,'xwD=',xwD,'yD=',yD,'yeD=',yeD,'ywD=',ywD,'zD=',zD,'zwD=',zwD
! close(102) ! Function call will add stuff to file so close it temporally

! Finally Call DimensioneLess Solution
pD  = qDHorzNFB(tD,xD,xeD,xwD,yD,yeD,ywD,zD,zwD,LD,w,lambda,Cd,skin)
! pD_ = pD
pD_ = 0.d0

! OPEN (UNIT = 102, FILE = ERRFILENAME, ACCESS='append', IOSTAT=ioerr, ERR=100)
! WRITE(102,*) 'After Function Call'
! WRITE(102,*) 'pD=',pD
! close(102)

FILENAME = TRIM(CURDIR)//'\pDHorzNFBDLL_Output.txt'
open (unit = 101, file = FILENAME, IOSTAT=ioerr, ERR=100)
write(101,*) pD
close(101)
return

100 ERRFILENAME = TRIM(CURDIR)//'\pDHorzNFBDLL_Err.txt'
    close(101)
    close(102)
    open (unit = 911, file = ERRFILENAME)
    IF (ioerr .NE. 0)  THEN
       write(911,*) 'Error OPENING file ', FILENAME
       write(911,*) 'Reported IOSTAT=',ioerr
    END IF
    IF (io .NE. 0)  THEN
       write(911,*) 'Error READING file ', FILENAME
       write(911,*) 'Reported IOSTAT=',io
    END IF    
    close(911)

END FUNCTION qDHorzNFBDLL

!---------------------------------------------------------------------------------------------------
FUNCTION CumqDHorzNFBDLL(tD_,xD_,xeD_,xwD_,yD_,yeD_,ywD_,zD_,zwD_,LD_,w_,lambda_,Cd_,skin_) Result(pD_) 
!---------------------------------------------------------------------------------------------------
USE well_models
IMPLICIT NONE
INTEGER, PARAMETER :: sp = SELECTED_REAL_KIND(6, 37)
INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(15, 307)

real(sp), intent(in) :: tD_
real(sp), intent(in) :: xD_
real(sp), intent(in) :: xeD_
real(sp), intent(in) :: xwD_
real(sp), intent(in) :: yD_
real(sp), intent(in) :: yeD_
real(sp), intent(in) :: ywD_
real(sp), intent(in) :: zD_
real(sp), intent(in) :: zwD_
real(sp), intent(in) :: LD_
real(sp), intent(in) :: w_
real(sp), intent(in) :: lambda_
real(sp), intent(in) :: Cd_
real(sp), intent(in) :: skin_
real(dp) :: pD
real(sp) :: pD_
real(dp) :: tD,xD,xeD,xwD,yD,yeD,ywD,zD,zwD,LD,w,lambda,Cd,skin

CHARACTER (LEN=256) FILENAME
CHARACTER (LEN=256) ERRFILENAME
CHARACTER (LEN=256)::CURDIR
INTEGER            ::ioerr
INTEGER            ::erro
INTEGER            ::io
erro=getcwd(CURDIR)

! ERRFILENAME = TRIM(CURDIR)//'\pDHorzNFBDLL_Err.txt'
! OPEN (UNIT = 102, FILE = ERRFILENAME, ACCESS='append', IOSTAT=ioerr, ERR=100)
! OPEN (UNIT = 102, FILE = ERRFILENAME, IOSTAT=ioerr, ERR=100)
! WRITE(102,*) 'Begin Function'

FILENAME = TRIM(CURDIR)//'\pDHorzNFBDLL.txt'
OPEN (UNIT = 101, FILE = FILENAME, IOSTAT=ioerr, ERR=100)

read(101,*,IOSTAT=io) tD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) xD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) xeD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) xwD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) yD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) yeD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) ywD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) zD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) zwD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) LD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) w
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) lambda
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) Cd
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) skin
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
close(101)

! WRITE(102,*) 'After Reading Function'
! WRITE(102,*) 'tD=',tD,'xD=',xD,'xeD=',xeD,'xwD=',xwD,'yD=',yD,'yeD=',yeD,'ywD=',ywD,'zD=',zD,'zwD=',zwD
! close(102) ! Function call will add stuff to file so close it temporally

! Finally Call DimensioneLess Solution
pD  = CumqDHorzNFB(tD,xD,xeD,xwD,yD,yeD,ywD,zD,zwD,LD,w,lambda,Cd,skin)
! pD_ = pD
pD_ = 0.d0

! OPEN (UNIT = 102, FILE = ERRFILENAME, ACCESS='append', IOSTAT=ioerr, ERR=100)
! WRITE(102,*) 'After Function Call'
! WRITE(102,*) 'pD=',pD
! close(102)

FILENAME = TRIM(CURDIR)//'\pDHorzNFBDLL_Output.txt'
open (unit = 101, file = FILENAME, IOSTAT=ioerr, ERR=100)
write(101,*) pD
close(101)
return

100 ERRFILENAME = TRIM(CURDIR)//'\pDHorzNFBDLL_Err.txt'
    close(101)
!    close(102)
    open (unit = 911, file = ERRFILENAME)
    IF (ioerr .NE. 0)  THEN
       write(911,*) 'Error OPENING file ', FILENAME
       write(911,*) 'Reported IOSTAT=',ioerr
    END IF
    IF (io .NE. 0)  THEN
       write(911,*) 'Error READING file ', FILENAME
       write(911,*) 'Reported IOSTAT=',io
    END IF    
    close(911)

END FUNCTION CumqDHorzNFBDLL

!---------------------------------------------------------------------------------------------------
FUNCTION pDHorzMultFracDLL() Result(pD_) 
!---------------------------------------------------------------------------------------------------
USE well_models
IMPLICIT NONE
INTEGER, PARAMETER :: sp = SELECTED_REAL_KIND(6, 37)
INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(15, 307)

! real(sp), intent(in) :: tD_,xeD_,yD_,yeD_,etaOD_,etaFD_,CRD_,CFD_,w_,lambda_,Cd_,skin_
real(dp)             :: pD,dpD,tD,xeD,yD,yeD,etaOD,etaFD,CRD,CFD,w,lambda,Cd,skin
! CHARACTER (LEN=16), intent(in) :: dpm_
CHARACTER (LEN=16)             :: dpm

real(sp) :: pD_

CHARACTER (LEN=256) FILENAME
CHARACTER (LEN=256) ERRFILENAME
CHARACTER (LEN=256)::CURDIR
INTEGER            ::ioerr
INTEGER            ::erro
INTEGER            ::io
erro=getcwd(CURDIR)

ERRFILENAME = TRIM(CURDIR)//'\pDHorzMultFracDLL_Err.txt'
OPEN (UNIT = 102, FILE = ERRFILENAME, ACCESS='append', IOSTAT=ioerr, ERR=100)
OPEN (UNIT = 102, FILE = ERRFILENAME, IOSTAT=ioerr, ERR=100)
WRITE(102,*) 'Begin Function'

FILENAME = TRIM(CURDIR)//'\pDHorzMultFracDLL.txt'
OPEN (UNIT = 101, FILE = FILENAME, IOSTAT=ioerr, ERR=100)

read(101,*,IOSTAT=io) tD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) xeD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) yD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) yeD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) etaOD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) etaFD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) CRD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) CFD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) dpm
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) w
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) lambda
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) Cd
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) skin
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
close(101)

WRITE(102,*) 'After Reading Function'
WRITE(102,*) 'tD=',tD,'xeD=',xeD,'yeD=',yeD,'wFD=',yD*2
WRITE(102,*) 'etaOD=',etaOD,'etaFD=',etaFD,'CRD=',CRD,'CFD=',CFD
WRITE(102,*) 'dpm=',dpm,'w=',w,'lambda=',lambda,'Cd=',Cd,'skin=',skin
! xeD,yD,yeD,etaOD,etaFD,CRD,CFD,w,lambda,Cd,skin
close(102) ! Function call will add stuff to file so close it temporally

! Finally Call DimensioneLess Solution
call pDHorzMultFracTL(pD,dpD,tD,xeD,yD,yeD,etaOD,etaFD,CRD,CFD,dpm,w,lambda,Cd,skin)
pD_ = 0.d0

! OPEN (UNIT = 102, FILE = ERRFILENAME, ACCESS='append', IOSTAT=ioerr, ERR=100)
! WRITE(102,*) 'After Function Call'
! WRITE(102,*) 'pD=',pD
! close(102)

FILENAME = TRIM(CURDIR)//'\pDHorzMultFracDLL_Output.txt'
open (unit = 101, file = FILENAME, IOSTAT=ioerr, ERR=100)
write(101,*) pD
write(101,*) dpD
close(101)
return

100 ERRFILENAME = TRIM(CURDIR)//'\pDHorzNFBDLL_Err.txt'
    close(101)
!   close(102)
    open (unit = 911, file = ERRFILENAME)
    IF (ioerr .NE. 0)  THEN
       write(911,*) 'Error OPENING file ', FILENAME
       write(911,*) 'Reported IOSTAT=',ioerr
    END IF
    IF (io .NE. 0)  THEN
       write(911,*) 'Error READING file ', FILENAME
       write(911,*) 'Reported IOSTAT=',io
    END IF    
    close(911)

END FUNCTION pDHorzMultFracDLL


!---------------------------------------------------------------------------------------------------
FUNCTION qDHorzMultFracDLL() Result(pD_) 
!---------------------------------------------------------------------------------------------------
USE well_models
IMPLICIT NONE
INTEGER, PARAMETER :: sp = SELECTED_REAL_KIND(6, 37)
INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(15, 307)

! real(sp), intent(in) :: tD_,xeD_,yD_,yeD_,etaOD_,etaFD_,CRD_,CFD_,w_,lambda_,Cd_,skin_
real(dp)             :: pD,dpD,tD,xeD,yD,yeD,etaOD,etaFD,CRD,CFD,w,lambda,Cd,skin
! CHARACTER (LEN=16), intent(in) :: dpm_
CHARACTER (LEN=16)             :: dpm

real(sp) :: pD_

CHARACTER (LEN=256) FILENAME
CHARACTER (LEN=256) ERRFILENAME
CHARACTER (LEN=256)::CURDIR
INTEGER            ::ioerr
INTEGER            ::erro
INTEGER            ::io
erro=getcwd(CURDIR)

!ERRFILENAME = TRIM(CURDIR)//'\qDHorzMultFracDLL_Err.txt'
!OPEN (UNIT = 102, FILE = ERRFILENAME, ACCESS='append', IOSTAT=ioerr, ERR=100)
!OPEN (UNIT = 102, FILE = ERRFILENAME, IOSTAT=ioerr, ERR=100)
!WRITE(102,*) 'Begin Function'

FILENAME = TRIM(CURDIR)//'\qDHorzMultFracDLL.txt'
OPEN (UNIT = 101, FILE = FILENAME, IOSTAT=ioerr, ERR=100)

read(101,*,IOSTAT=io) tD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) xeD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) yD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) yeD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) etaOD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) etaFD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) CRD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) CFD
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) dpm
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) w
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) lambda
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) Cd
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
read(101,*,IOSTAT=io) skin
IF (io .NE. 0)  THEN
   ! ... something wrong ...
   CLOSE(101)
   GOTO 100
END IF
close(101)

!WRITE(102,*) 'After Reading Function'
!WRITE(102,*) 'tD=',tD,'xeD=',xeD,'yeD=',yeD,'wFD=',yD*2
!WRITE(102,*) 'etaOD=',etaOD,'etaFD=',etaFD,'CRD=',CRD,'CFD=',CFD
!WRITE(102,*) 'dpm=',dpm,'w=',w,'lambda=',lambda,'Cd=',Cd,'skin=',skin
! xeD,yD,yeD,etaOD,etaFD,CRD,CFD,w,lambda,Cd,skin
!close(102) ! Function call will add stuff to file so close it temporally

! Finally Call DimensioneLess Solution
call qDHorzMultFracTL(pD,dpD,tD,xeD,yD,yeD,etaOD,etaFD,CRD,CFD,dpm,w,lambda,Cd,skin)
pD_ = 0.d0

! OPEN (UNIT = 102, FILE = ERRFILENAME, ACCESS='append', IOSTAT=ioerr, ERR=100)
! WRITE(102,*) 'After Function Call'
! WRITE(102,*) 'pD=',pD
! close(102)

FILENAME = TRIM(CURDIR)//'\qDHorzMultFracDLL_Output.txt'
open (unit = 101, file = FILENAME, IOSTAT=ioerr, ERR=100)
write(101,*) pD
write(101,*) dpD
close(101)
return

100 ERRFILENAME = TRIM(CURDIR)//'\qDHorzNFBDLL_Err.txt'
    close(101)
!   close(102)
    open (unit = 911, file = ERRFILENAME)
    IF (ioerr .NE. 0)  THEN
       write(911,*) 'Error OPENING file ', FILENAME
       write(911,*) 'Reported IOSTAT=',ioerr
    END IF
    IF (io .NE. 0)  THEN
       write(911,*) 'Error READING file ', FILENAME
       write(911,*) 'Reported IOSTAT=',io
    END IF    
    close(911)

END FUNCTION qDHorzMultFracDLL
