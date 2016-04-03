PROGRAM sine_lines ! mcoi = moving circle optical illusion
  IMPLICIT NONE
  REAL :: sf, w, dp, dth, t, tlim, dt, val1, val2, dr, sfaxes
  REAL, PARAMETER :: pi = ACOS(-1.0)
  INTEGER :: delay, rev, num, flim, config, i, f, ps
  LOGICAL :: trace
  CHARACTER :: dataname*32, gifname*32

  TYPE dot
    REAL :: p, th, r, x, y
  END TYPE dot

  TYPE(dot), DIMENSION(:), ALLOCATABLE :: dots

  dataname = 'mcoi_frames.dat'
  gifname = 'mcoi.gif'

  delay = 5 ! smoothness of animation deciseconds (1/100), for gnuplot frames
  dt = 2.0 ! controls number of data points
  rev = 1 ! keep at 1, gifs repeat anyway
  num = INT(48.0*1.0) ! default
  sf = 1.0 ! scale factor for plot variables
  sfaxes = 4.0 ! scale factor for canvas border
  ps = 1 ! point size
  w = 1.0 ! speed of animation
  trace = .FALSE. !unused
  config = 7
  val1 = 6.0 ! a variable that is used to customise a configuration
  val2 = 60.0
  dr = 2.0 ! dr > 1 for hole in centre

  SELECT CASE (config)
    CASE (1) ! default, circle
      dp = 180.0/REAL(num)
      dth = 180.0/REAL(num)
    CASE (2) ! custom (dp,dth) = (90,45), (30,15), (15,30), (15,75)
      dp = 90.0
      dth = 45.0
    CASE (3) ! complementary
      dp = 2.0
      dth = 90.0 - dp
    CASE (4) ! supplementary
      dp = 2.0
      dth = 180.0 - dp
    CASE (5)
      dp = 2.0
      dth = 360.0 - dp
    CASE (6)
      dp = 10.0
      dth = 30.0 - dp
    CASE (7) ! sphere effect
      num = INT(val2*val1)
      dp = val2/val1
      dth = dp*(1.0 + 0.1)
  END SELECT

  OPEN(UNIT=10, FILE=dataname)
  ALLOCATE(dots(0:num - 1))
  tlim = rev*360.0/w
  !WRITE(6,'(a,f10.2)') 'tlim ', tlim

  !WRITE(6,'(a)') 'i p th'
  DO i=0,num-1 ! cycle through dots
    dots(i)%p = i*dp ! set phase shift
    dots(i)%th = i*dth ! set orientation
    !WRITE(6,'(i3,2f6.2)') i, dots(i)%p, dots(i)%th
  END DO

  flim = INT(tlim/dt) + 1
  !WRITE(6,'(a,i0)') 'Number of frames = ', flim
  !READ(5,*)

  f = 0 !; t = 0
  DO !f=0,flim ! cycle through time frames
    t = REAL(f)*dt
    !WRITE(6,'(a,i0,a,f8.2)') 'f ', f, ' t ', t

    DO i=0,num-1 ! cycle through dots
      dots(i)%r = sf*COS((w*t + dots(i)%p)*pi/180.0) + dr*sf ! sine line = sf*(COS(w*t) + p)
      dots(i)%x = dots(i)%r*COS(dots(i)%th*pi/180.0) ! x = r*cos(th)
      dots(i)%y = dots(i)%r*SIN(dots(i)%th*pi/180.0) ! y = r*sin(th)
      WRITE(10,'(2f10.4)') dots(i)%x, dots(i)%y
    END DO

    IF (t >= tlim) EXIT ! w*t = tlim = rev*360.0 ==> t/rev = 360.0/w
    !IF (f = flim) EXIT

    WRITE(10,*); WRITE(10,*) ! new index for gnuplot = new frame in gif
    !t = t + dt
    f = f + 1
  END DO

  CLOSE(10)

  OPEN(UNIT=11, FILE='mcoi_spec.gnu')
  WRITE(11,'(a)') 'reset'
  WRITE(11,'(a,i0)') 'set term gif animate delay ', delay
  WRITE(11,'(a)') 'set output "'//TRIM(gifname)//'"'
  WRITE(11,'(a)') 'set size square'
  WRITE(11,'(a,2(f10.4,a))') 'set xrange[',-(sf*sfaxes),':',sf*sfaxes,']'
  WRITE(11,'(a,2(f10.4,a))') 'set yrange[',-(sf*sfaxes),':',sf*sfaxes,']'
  WRITE(11,'(a)') 'unset xtics'
  WRITE(11,'(a)') 'unset ytics'
  WRITE(11,'(a)') 'set key samplen 1'
  WRITE(11,100) 'set title "num=',num,', w=',w,', dp=',dp,', dth=',dth,', dr=',dr,'"'
  100 FORMAT(a,1(i0,a),4(f5.1,a))
  WRITE(11,101) 'set xlabel "flim=',flim,', delay=',delay,', dt=',dt,', sf=',sf,', sfaxes=',sfaxes,'"'
  101 FORMAT(a,2(i0,a),3(f5.1,a))
  WRITE(11,'(a,i0)') 'n=',flim
  WRITE(11,'(a)') 'i=1'
  WRITE(11,'(a)') 'load "mcoi_plot.gnu"'
  WRITE(11,'(a)') 'set output'
  CLOSE(11)

  OPEN(UNIT=12, FILE='mcoi_plot.gnu')
  WRITE(12,102) 'plot "'//TRIM(dataname)//'" index i-1 w p pt 7 ps ',ps,' lc 3 title sprintf("f=%i",i)'
  102 FORMAT(a,i0,a)
  WRITE(12,'(a)') 'i=i+1'
  WRITE(12,'(a)') 'if (i < n+1) reread'
  CLOSE(12)

  CALL SYSTEM('gnuplot "mcoi_spec.gnu"')
  CALL SYSTEM('eog '//gifname)

END PROGRAM sine_lines
