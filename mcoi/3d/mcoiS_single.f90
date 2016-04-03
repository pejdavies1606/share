PROGRAM sine_lines ! mcoi = moving circle optical illusion
  IMPLICIT NONE
  REAL :: sf, w, t, tlim, dt, dr, sfaxes, viewx, viewz, aspect_ratio, aspect_spec, aspect_x, aspect_y, margin
  REAL, PARAMETER :: pi = ACOS(-1.0)
  INTEGER :: delay, rev, num, flim, i, f, ps, j, spin, size_x, size_y, frame
  CHARACTER(LEN=32) :: dataname, gifname, specname, plotname, debugname, bgnd
  LOGICAL :: debug = .FALSE., hidden3d = .FALSE.

  TYPE dot
    REAL :: p, r, th, phi, x, y, z
  END TYPE dot

  TYPE svar ! start, finish, difference, number
    REAL :: s, f, d
    INTEGER :: n
  END TYPE svar

  TYPE(dot), DIMENSION(:,:), ALLOCATABLE :: dots
  TYPE(svar) :: p, th, phi

  dataname = 'mcoiS_frames.dat'
  gifname = 'mcoiS.gif'
  specname = 'mcoiS_spec.gnu'
  plotname = 'mcoiS_plot.gnu'
  debugname = 'mcoiS_debug.log'

  aspect_spec = 0.25
  aspect_x = 500
  aspect_y = 500
  margin = 0.05
  bgnd = '"#000000"'
  
  IF (debug) THEN
    aspect_spec = 1.0
    aspect_x = 640
    aspect_y = 480
    bgnd = '"#FFFFFF"'
  END IF
  
  size_x = aspect_spec*aspect_x
  size_y = aspect_spec*aspect_y
  aspect_ratio = REAL(size_y)/REAL(size_x)
  
  WRITE(6,'(a,f5.1)') 'r=', aspect_ratio
  WRITE(6,'(a,i4)') 'x=', size_x
  WRITE(6,'(a,i4)') 'y=', size_y
  
  spin = 0 ! -1, 0, 1
  delay = 5 ! smoothness of animation deciseconds (1/100), for gnuplot frames
  dt = 0.5 ! controls number of data points, and speed of rotation
  rev = 1 ! keep at 1, gifs repeat anyway
  dr = 1.0 ! 1.75 ! dr > 1 for hole in centre
  sf = 2.0 ! 0.25! amplitude of sineline
  sfaxes = 1.0 ! 2.0 ! scale factor for canvas border
  ps = 1 ! point size, line thickness
  w = 1.0 ! 2.0 ! speed of oscillation
  viewx = 0.0 ! 60.0
  viewz = 360.0 ! total angle to rotate through over the whole animation
  th%s = 0.0
  phi%s = 0.0
  p%s = 0.0
  th%f = 180.0
  phi%f = 360.0
  p%f = 360.0
  th%n = 48
  phi%n = 12
  p%n = 2 ! for th or phi dependence, see DO loop below
  WRITE(6,'(a,i0)') 'phi/p=', phi%n/p%n
  WRITE(6,'(a,i0)') 'th/p=', th%n/p%n
  
  num = th%n*phi%n
  CALL calc_d(th); CALL calc_d(phi); CALL calc_d(p)
  WRITE(6,'(a)') 'dth, dphi, dp, num'
  WRITE(6,'(3f8.1,1x,i0)') th%d, phi%d, p%d, num

  OPEN(UNIT=10, FILE=dataname)
  ALLOCATE(dots(0:phi%n,0:th%n)) ! dots(phi, th)
  tlim = rev*360.0
  WRITE(6,'(a,f10.2)') 'tlim ', tlim
  
  !WRITE(6,'(a)') 'Press enter to continue ... '
  !READ(5,*)

  ! Spherical polar ==> Cartesian variables
  ! x = r*sin(th)*cos(phi)
  ! y = r*sin(th)*sin(phi)
  ! z = r*cos(th)
  ! rho = r*sin(th) cylindrical radius

  ! Spherical polar ==> Cartesian unit vectors
  ! (i)   ( sin(th)*cos(phi)  cos(th)*sin(phi) -sin(phi) )(r  )
  ! (j) = ( sin(th)*sin(phi)  cos(th)*sin(phi)  cos(phi) )(th )
  ! (k)   ( cos(th)          -sin(th)           0        )(phi)
!https://surreylearn.surrey.ac.uk/d2l/le/content/118114/viewContent/681905/View?ou=118114

  OPEN(UNIT=9, FILE=debugname)

  WRITE(9,'(a)') 'i j p th phi'
  DO j=0,th%n ! cycle through dots
   DO i=0,phi%n
      dots(i,j)%th = MOD(th%s + j*th%d, th%f - th%s + th%d) ! set th orientation
      dots(i,j)%phi = MOD(phi%s + i*phi%d, phi%f - phi%s + phi%d) ! set phi orientation (rotation about z axis)
      
      IF ( dots(i,j)%th == th%s .OR. dots(i,j)%th == th%f ) THEN
        dots(i,j)%p = p%s
      ELSE
        dots(i,j)%p = MOD(p%s + i*p%d, p%f - p%s )
        ! set phase shift
        ! i for phi dependence, j for th dependence
        ! (i+j)/2 for both
      END IF
      
      WRITE(9,'(2i3,3f8.1)') i, j, dots(i,j)%p, dots(i,j)%th, dots(i,j)%phi
    END DO
  END DO
  
  CLOSE(9)

  flim = INT(tlim/dt)
  WRITE(6,'(a,i0)') 'Number of frames = ', flim
  
  frame = flim/2
  WRITE(6,'(a,i0)') 'Frame ', frame
  !READ(5,*)

  DO f = 1, flim ! cycle through time frames
    t = REAL(f-1)*dt
    !WRITE(6,'(a,i0,a,f8.2)') 'f ', f, ' t ', t

    DO j=0,th%n ! cycle through dots
      DO i=0,phi%n
        ! change spherical polar coordinates to Cartesian
        !IF ( j == 0 .OR. j == th%n ) THEN
        !  dots(i,j)%r = dr
        !ELSE
          dots(i,j)%r = dr + sf*COS((w*t + dots(i,j)%p)*pi/180.0) ! sine line = sf*COS(w*t + p)
        !END IF
        
        CALL polar2cartesian(dots(i,j))
        WRITE(10,'(3f10.4)') dots(i,j)%x, dots(i,j)%y, dots(i,j)%z
        !WRITE(10,'(3f10.4)') 0.0, 0.0, 0.0
      END DO
      WRITE(10,*)
    END DO

    WRITE(10,*); WRITE(10,*) ! new index for gnuplot
    WRITE(10,'(3f10.4)') 0.0, 0.0, -(sfaxes) ! draw z axis
    WRITE(10,'(3f10.4)') 0.0, 0.0, sfaxes

    !IF (f >= flim) EXIT

    WRITE(10,*); WRITE(10,*) ! new index for gnuplot
    !f = f + 1
  END DO

  CLOSE(10)

  OPEN(UNIT=11, FILE=TRIM(specname))
  WRITE(11,'(a)') 'reset'
  WRITE(11,'(2a,3(a,i0))') 'set term gif opt background ',bgnd,' delay ', delay, ' size ',size_x,',',size_y
  WRITE(11,'(a)') 'set output "'//TRIM(gifname)//'"'
  WRITE(11,'(a)') 'set view equal' ! 3d version of "set size square"
  WRITE(11,'(a,2(f10.4,a))') 'set xrange[',-(sfaxes),':',sfaxes,']'
  WRITE(11,'(a,2(f10.4,a))') 'set yrange[',-(sfaxes),':',sfaxes,']'
  WRITE(11,'(a,2(f10.4,a))') 'set zrange[',-(sfaxes),':',sfaxes,']'
  WRITE(11,'(a,2(f10.4,a))') 'set cbrange[',-(sfaxes),':',sfaxes,']'
  WRITE(11,'(a)') 'unset tics'
  WRITE(11,'(a)') 'unset border'
  WRITE(11,'(a)') 'unset zeroaxis'
  IF (.NOT.debug) THEN
    WRITE(11,'(a,f6.2)') 'set lmargin at screen ', margin
    WRITE(11,'(a,f6.2)') 'set rmargin at screen ', 1.0 - margin
    WRITE(11,'(a,f6.2)') 'set bmargin at screen ', margin
    WRITE(11,'(a,f6.2)') 'set tmargin at screen ', 1.0 - margin
  END IF
  WRITE(11,'(a)') 'show margin'
  !WRITE(11,'(a)') 'set palette cubehelix start 0.0 cycles 2.0'
  WRITE(11,'(a)') 'set palette rgbformulae 0,17,8'
  WRITE(11,'(a)') 'show palette rgbformulae'
  WRITE(11,'(a)') 'set pm3d'
  IF (hidden3d) WRITE(11,'(a)') 'set hidden3d'
  IF (debug) WRITE(11,'(a)') 'set key samplen 1 at screen 0.7, screen 0.86'
  IF (.NOT.debug) WRITE(11,'(a)') 'unset key'
  IF (.NOT.debug) WRITE(11,'(a)') 'unset colorbox'
  IF (debug) WRITE(11,'(a)') 'set title "Spherical Coordinates"'
  
  IF (debug) WRITE(11,100) 'set label "num=',num,', w=',w,', p=',p%n,', th=',th%n,', phi=',phi%n,', dr=',dr,', sf=',sf,&
  &'" at screen 0.2, screen 0.15'
  !', viewx=',viewx,', viewz=',viewz,'"'
  100 FORMAT(a,1(i0,a),1(f5.1,a),3(i0,a),2(f5.2,a))
  
  IF (debug) WRITE(11,101) 'set label "flim=',flim,', delay=',delay,', dt=',dt,', sfaxes=',sfaxes,'" at screen 0.2, screen 0.1'
  101 FORMAT(a,2(i0,a),2(f5.1,a),a)
  
  WRITE(11,'(a,i0)') 'n=',flim
  WRITE(11,'(a,i0)') 'i=',frame
  
  SELECT CASE (spin)
    CASE (0:1)
      WRITE(11,'(2(a,f9.3),a,i1)') 'set view ',viewx,', ',viewz,'/n*(i-1)*', ABS(spin)
    CASE (-1)
      WRITE(11,'(2(a,f9.3),a,i1)') 'set view ',viewx,', ',viewz,'/n*(n-(i-1))*', ABS(spin)
  END SELECT
  
  WRITE(11,102) 'splot "'//TRIM(dataname)//'" index 2*(i-1) u 1:2:3 w l lw ',ps,' lc 0 title sprintf("f=%i",i)'
  102 FORMAT(a,i0,a)
  
  WRITE(11,'(a)') 'set output'
  CLOSE(11)

  !OPEN(UNIT=12, FILE=TRIM(plotname))
  !WRITE(12,200) 'set label "viewx=',viewx,', viewz=',viewz,'*',rotspeed,'/n*(i-1)" at screen 0.2, screen 0.2'
  !200 FORMAT(3(a,f9.3),a)
  
  !SELECT CASE (spin)
  !  CASE (0:1)
  !    WRITE(12,'(2(a,f9.3),a,i1)') 'set view ',viewx,', ',viewz,'/n*(i-1)*', ABS(spin)
  !  CASE (-1)
  !    WRITE(12,'(2(a,f9.3),a,i1)') 'set view ',viewx,', ',viewz,'/n*(n-(i-1))*', ABS(spin)
   !END SELECT
  
  
  ! points and z axis
  !WRITE(12,102) 'splot "'//TRIM(dataname)//'" index 2*(i-1) u 1:2:3 w p pt 7 ps ',ps,' lc 3 title sprintf("f=%i",i), "'&
  !&//TRIM(dataname)//'" index 2*i u 1:2:3 w l title ""'
  ! lines and z axis
  !WRITE(12,102) 'splot "'//TRIM(dataname)//'" index 2*(i-1) u 1:2:3 w l lw ',ps,' lc 3 title sprintf("f=%i",i), "'&
  !&//TRIM(dataname)//'" index 2*i u 1:2:3 w l title ""'
  ! points only
  !WRITE(12,102) 'splot "'//TRIM(dataname)//'" index 2*(i-1) u 1:2:3 w p pt 7 ps ',ps,' lc 3 title sprintf("f=%i",i)'
  ! lines only
  !WRITE(12,102) 'splot "'//TRIM(dataname)//'" index 2*(i-1) u 1:2:3 w l lw ',ps,' lc 0 title sprintf("f=%i",i)'
  !102 FORMAT(a,i0,a)
  !WRITE(12,'(a)') 'i=i+1'
  !WRITE(12,'(a)') 'if (i < n+1) reread'
  !CLOSE(12)

  CALL SYSTEM('gnuplot "'//TRIM(specname)//'"')
  CALL SYSTEM('eog '//TRIM(gifname)//' &')

  CONTAINS

    SUBROUTINE calc_d(angle)
      IMPLICIT NONE
      TYPE(svar), INTENT(INOUT) :: angle
      angle%d = (angle%f - angle%s)/angle%n
      RETURN
    END SUBROUTINE
  
    SUBROUTINE polar2cartesian(d)
      IMPLICIT NONE
      TYPE(dot), INTENT(INOUT) :: d
      
        d%x = d%r*SIN(d%th*pi/180.0)*COS(d%phi*pi/180.0) ! x = r*sin(th)*cos(phi)
        d%y = d%r*SIN(d%th*pi/180.0)*SIN(d%phi*pi/180.0) ! y = r*sin(th)*sin(phi)
        d%z = d%r*COS(d%th*pi/180.0) ! z = r*cos(th)
      
      RETURN
    END SUBROUTINE

END PROGRAM sine_lines
