PROGRAM sine_lines ! mcoi = moving circle optical illusion
  IMPLICIT NONE
  REAL :: sf, w, t, tlim, dt, dr, sfaxes, viewx, viewz, rotspeed
  REAL, PARAMETER :: pi = ACOS(-1.0)
  INTEGER :: delay, rev, num, flim, config, i, f, ps, j
  LOGICAL :: trace
  CHARACTER(LEN=32) :: dataname, gifname, specname, plotname

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

  delay = 5 ! smoothness of animation deciseconds (1/100), for gnuplot frames
  dt = 2.0 ! controls number of data points
  rev = 1 ! keep at 1, gifs repeat anyway
  dr = 1.0 ! dr > 1 for hole in centre
  sf = 0.5 ! scale factor for plot variables
  sfaxes = 2.0 ! scale factor for canvas border
  ps = 1 ! point size
  w = 3.0 ! speed of animation
  trace = .FALSE. !unused
  config = 1
  viewx = 60.0
  viewz = 360.0
  rotspeed = 0.1
  p%s = 0.0
  th%s = 0.0
  phi%s = 0.0
  p%f = 360.0
  th%f = 180.0
  phi%f = 360.0
  p%n = 1
  th%n = 12
  phi%n = 12
  num = p%n*th%n*phi%n
  CALL calc_d(p); CALL calc_d(th); CALL calc_d(phi)
  WRITE(6,'(a)') 'dp, dth, dphi, num'
  WRITE(6,'(3f8.1,i4)') p%d, th%d, phi%d, num

  SELECT CASE (config)
    CASE (1) ! default, circle
  END SELECT

  OPEN(UNIT=10, FILE=dataname)
  ALLOCATE(dots(0:phi%n,0:th%n)) ! dots(phi, th)
  tlim = rev*360.0/w
  WRITE(6,'(a,f10.2)') 'tlim ', tlim
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

  !WRITE(6,'(a)') 'i j p th phi'
  DO j=0,th%n ! cycle through dots
   DO i=0,phi%n
      dots(i,j)%p = MOD(p%s + i*p%d, 360.0 + p%d) ! set phase shift
      dots(i,j)%phi = MOD(phi%s + i*phi%d, 360.0 + phi%d) ! set phi orientation (rotation about z axis)
      dots(i,j)%th = MOD(th%s + j*th%d, 180.0 + th%d) ! set th orientation
      !WRITE(6,'(2i3,3f8.1)') i, j, dots(i,j)%p, dots(i,j)%th, dots(i,j)%phi
    END DO
  END DO

  flim = INT(tlim/dt*w) + 1
  WRITE(6,'(a,i0)') 'Number of frames = ', flim
  !READ(5,*)

  f = 0 !; t = 0
  DO !f=0,flim ! cycle through time frames
    t = REAL(f)*dt
    !WRITE(6,'(a,i0,a,f8.2)') 'f ', f, ' t ', t

    DO j=0,th%n ! cycle through dots
      DO i=0,phi%n
        ! change spherical polar coordinates to Cartesian
        dots(i,j)%r = dr + sf*SIN((w*t + dots(i,j)%p)*pi/180.0)! sine line = sf*(COS(w*t) + p)
        dots(i,j)%x = dots(i,j)%r*SIN(dots(i,j)%th*pi/180.0)*COS(dots(i,j)%phi*pi/180.0) ! x = r*sin(th)*cos(phi)
        dots(i,j)%y = dots(i,j)%r*SIN(dots(i,j)%th*pi/180.0)*SIN(dots(i,j)%phi*pi/180.0) ! y = r*sin(th)*sin(phi)
        dots(i,j)%z = dots(i,j)%r*COS(dots(i,j)%th*pi/180.0) ! z = r*cos(th)
        !WRITE(10,'(3f10.4)') 0.0, 0.0, 0.0
        WRITE(10,'(3f10.4)') dots(i,j)%x, dots(i,j)%y, dots(i,j)%z
      END DO
    END DO

    WRITE(10,*); WRITE(10,*) ! new index for gnuplot
    WRITE(10,'(3f10.4)') 0.0, 0.0, -(sfaxes)
    WRITE(10,'(3f10.4)') 0.0, 0.0, sfaxes

    IF (t >= tlim) EXIT ! w*t = tlim = rev*360.0 ==> t/rev = 360.0/w
    !IF (f = flim) EXIT

    WRITE(10,*); WRITE(10,*) ! new index for gnuplot
    !t = t + dt
    f = f + 1
  END DO

  CLOSE(10)

  OPEN(UNIT=11, FILE=TRIM(specname))
  WRITE(11,'(a)') 'reset'
  WRITE(11,'(a,i0)') 'set term gif animate delay ', delay
  WRITE(11,'(a)') 'set output "'//TRIM(gifname)//'"'
  WRITE(11,'(a)') 'set view equal' ! 3d version of "set size square"
  WRITE(11,'(a,2(f10.4,a))') 'set xrange[',-(sfaxes),':',sfaxes,']'
  WRITE(11,'(a,2(f10.4,a))') 'set yrange[',-(sfaxes),':',sfaxes,']'
  WRITE(11,'(a,2(f10.4,a))') 'set zrange[',-(sfaxes),':',sfaxes,']'
  WRITE(11,'(a)') 'unset xtics'
  WRITE(11,'(a)') 'unset ytics'
  WRITE(11,'(a)') 'unset ztics'
  WRITE(11,'(a)') 'unset border'
  WRITE(11,'(a)') 'unset zeroaxis'
  !WRITE(11,'(2(a,f5.1))') 'set view ',viewx,', ',viewz
  WRITE(11,'(a)') 'set key samplen 1 at screen 0.7, screen 0.86'
  !WRITE(11,'(a)') 'set key out'
  WRITE(11,100) 'set title "num=',num,', w=',w,', dp=',p%d,', dth=',th%d,' dphi=',phi%d,', dr=',dr!,&
  !&', viewx=',viewx,', viewz=',viewz,'"'
  100 FORMAT(a,1(i0,a),5(f5.1,a))
  !WRITE(11,101) 'set xlabel "flim=',flim,', delay=',delay,', dt=',dt,', sf=',sf,', sfaxes=',sfaxes,'"'
  !101 FORMAT(a,2(i0,a),3(f5.1,a))
  WRITE(11,'(a,i0)') 'n=',flim
  WRITE(11,'(a)') 'i=1'
  WRITE(11,'(a)') 'load "'//TRIM(plotname)//'"'
  WRITE(11,'(a)') 'set output'
  CLOSE(11)

  OPEN(UNIT=12, FILE=TRIM(plotname))
  WRITE(12,'(3(a,f9.1))') 'set view ',viewx,', ',viewz,'/n*i*',rotspeed
  WRITE(12,102) 'splot "'//TRIM(dataname)//'" index i-1 u 1:2:3 w p pt 7 ps ',ps,' lc 3 title sprintf("f=%i",i), "'&
  &//TRIM(dataname)//'" index i u 1:2:3 w l title ""'
  !WRITE(12,102) 'splot "'//TRIM(dataname)//'" index i-1 u 1:2:3 w l lw ',ps,' lc 3 title sprintf("f=%i",i), "'&
  !&//TRIM(dataname)//'" index i u 1:2:3 w l title ""'
  102 FORMAT(a,i0,a)
  WRITE(12,'(a)') 'i=i+2'
  WRITE(12,'(a)') 'if (i < n+1) reread'
  CLOSE(12)

  CALL SYSTEM('gnuplot "'//TRIM(specname)//'"')
  CALL SYSTEM('eog '//TRIM(gifname)//' &')

  CONTAINS

    SUBROUTINE calc_d(angle)
      IMPLICIT NONE
      TYPE(svar), INTENT(INOUT) :: angle
      angle%d = (angle%f - angle%s)/angle%n
      RETURN
  END SUBROUTINE

END PROGRAM sine_lines
