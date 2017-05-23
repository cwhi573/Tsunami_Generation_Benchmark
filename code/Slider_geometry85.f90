!**************************************************************************

      MODULE MovingBottom

!**************************************************************************
! ***  Routines to accomodate moving bottom
! ***  Solid slider testcases from Langford Sue
!**************************************************************************

      real*8, save :: xslide, zmin,x1, x2, x3, t1, t2, t3, a0, uterm
      integer, parameter :: np0 = 1500
      real*8, parameter :: xmaj = 0.25D0       !half of the slider length
      real*8, parameter :: ymaj = 0.026D0      !half of the slider height
      real*8, parameter :: xmaj2 = 1.D0/(xmaj*xmaj)

      END MODULE

!**************************************************************************

      program driver
      
      USE MovingBottom
      
      implicit none
      
      integer :: j,jj,np,numexp
      real*8 :: time, xdist(np0),bottom(np0)
      
! *** define experiment with acceleration and terminal velocity.
!**************************************************************
! *** experiment 1
      numexp = 4  !1
      np = 1463
      a0 = 0.5D0
      uterm = 0.926D0 !0.232D0
      zmin = -0.350D0    !depth
!**************************************************************
       
! *** define x axis
      do j=1,np
        xdist(j)= -5.0D0 + 0.01D0*dble(j-1)
      enddo
     
      t1 = uterm/a0
      x1 =  0.5D0*a0*t1*t1
      t2 = t1 + 2.D0
      x2 = x1 + uterm*(t2-t1)
      t3 = t2 + t1
      x3 = x2 + x1
      write(*,'(a17,i5,3(f7.3))') ' n,a0,uterm,zmin=',numexp,a0,uterm,zmin
      write(*,'(a19,3(1x,f7.3),3(1x,f8.3))') ' t1,t2,t3,x1,x2,x3=', t1,t2,t3,x1,x2,x3    
! *** open output file
      open(23,file='bed_elevation85.dat')
      
      do jj=1,601  !test time loop
      
        time = 0.D0 + 0.01D0*dble(jj-1)
      
        call CalcBottomElevation(time,xdist,bottom,np)
      
!  *** write data in your format here. This is for Tecplot.
        write(23,*) 'ZONE'
        write(23,*) 'SOLUTIONTIME=' , time
        do j=1,np
          write(23,'(f10.2,1x,f10.5)') xdist(j),bottom(j)
        enddo
        
      enddo
      
      end

!**************************************************************************

      SUBROUTINE CalcBottomElevation(time,xdist,bottom,np)
      
      USE MovingBottom

      implicit none
      
! *** passed variables
      integer, intent(in) :: np      !number of points in x
      real*8 ,intent(in)  :: time    !time in seconds from start
      real*8 ,intent(in)  :: xdist(np)   !distance(m) from x=0 at intersection of slope and initial water surface
      real*8 ,intent(out) :: bottom(np)  !bottom elevation(m) measured from free surface

      integer :: jn
      real*8 :: zbot,xdif,arg

! *** Case 85 UCant Flume (flat bottom) with elliptical slider

! *** find location of cg
      if(time.le.t1) then
        xslide = 0.5D0*a0*time*time
      elseif(time.le.t2) then
        xslide = x1 + uterm*(time-t1)
      elseif(time.le.t3) then
        xslide = x2 + uterm*(time-t2) - 0.5D0*a0*(time-t2)*(time-t2)
      endif

      do jn=1,np
! *** slide across bottom
        xdif = xdist(jn) - xslide
        zbot = zmin
        if(abs(xdif).le.(xmaj)) then
          arg = 1.D0 - xmaj2*xdif*xdif
          if(arg.ge.0.) then
            zbot = zmin + ymaj*sqrt(arg)
          else
            write(*,*) ' sqrt out of range'
            read(*,*)
          endif
        endif
        bottom(jn) = max(zmin,zbot)
      enddo
                                
      END SUBROUTINE

!******************************************************************************
!******************************************************************************
