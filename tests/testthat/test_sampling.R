test_that(
  'sampling errors',{
    expect_error(runif_square(-1))
    expect_error(runif_disk(-1))
    expect_error(runif_annulus(-1))
    expect_error(runif_cube(-1))
    expect_error(runif_ball_3D(-1))
    expect_error(runif_shell_3D(-1))
    expect_error(runif_square(10, xmin=1, max=0))
    expect_error(runif_square(10, ymin=1, ymax=0))
    expect_error(runif_disk(10,-1))
    expect_error(runif_annulus(10, -1, -2))
    expect_error(runif_annulus(10, rmax = 1, rmin= 2))
    expect_error(runif_cube(10, xmin=1, xmax=0))
    expect_error(runif_cube(10, ymin=1, ymax=0))
    expect_error(runif_cube(10, zmin=1, zmax=0))
    expect_error(runif_ball_3D(10, -1))
    expect_error(runif_shell_3D(10, rmax=-1, rmin=-2))
    expect_error(runif_shell_3D(10, rmax=1, rmin=2))
  }
)

test_that(
  '2D sampling square',{
    #given
    n=5
    set.seed(20159)
    #when
    points = cbind(runif(n), runif(n))
    set.seed(20159)
    #then
    expect_equal(runif_square(n), points)
  }
)

test_that(
  '2D sampling circle',{
    #given
    n=5
    set.seed(20159)
    #when
    rad = runif(n)
    angle = runif(n, 0, 2*pi)
    points = cbind(sqrt(rad)*cos(angle), sqrt(rad)*sin(angle))
    set.seed(20159)
    #then
    expect_equal(runif_disk(n), points)
  }
)

test_that(
  '2D sampling annulus',{
   #given
    n=5
    set.seed(20159)
    #when
    rad = runif(n, 0.25, 1)
    angle = runif(n, 0, 2*pi)
    points = cbind(sqrt(rad)*cos(angle), sqrt(rad)*sin(angle))
    set.seed(20159)
    #then
    expect_equal(runif_annulus(n), points)
  }
)

test_that(
  '3D sampling cube',{
    #given
    n=5
    set.seed(20159)
    #when
    points = cbind(runif(n), runif(n), runif(n))
    set.seed(20159)
    #then
    expect_equal(runif_cube(n), points)
  }
)

test_that(
  '3D sampling sphere',{
    #given
    n=5
    set.seed(20159)
    #when
    rad = runif(n)
    angle1 = runif(n, 0, 2*pi)
    angle2 = acos(1 - 2*runif(n,0,1))
    points = cbind(pracma::nthroot(rad,3)*cos(angle1)*sin(angle2), 
                   pracma::nthroot(rad,3)*sin(angle1)*sin(angle2),
                   pracma::nthroot(rad,3)*cos(angle2))
    set.seed(20159)
    #then
    expect_equal(runif_ball_3D(n), points)
  }
)

test_that(
  '3D sampling shell',{
    #given
    n=5
    set.seed(20159)
    #when
    rad = runif(n, 0.125,1)
    angle1 = runif(n, 0, 2*pi)
    angle2 = acos(1 - 2*runif(n,0,1))
    points = cbind(pracma::nthroot(rad,3)*cos(angle1)*sin(angle2), 
                   pracma::nthroot(rad,3)*sin(angle1)*sin(angle2),
                   pracma::nthroot(rad,3)*cos(angle2))
    set.seed(20159)
    #then
    expect_equal(runif_shell_3D(n), points)
  }
)