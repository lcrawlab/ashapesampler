test_that("2D sampling square", {
  #given
  set.seed(20159)
  points1 = runif_square(787)
  points2 = runif_square(787)
  a1 = alphahull::ashape(points1, alpha=0.24)
  a2 = alphahull::ashape(points2, alpha=0.24)
  alist = list(a1, a2)
  #when
  set.seed(20159)
  test1 = sampling2Dashape(N=1,afixed=TRUE,nconnect=FALSE,nhomology=TRUE)
  set.seed(20159)
  test2 = sampling2Dashape(N=2,afixed=TRUE,nconnect=FALSE,nhomology=TRUE)
  #then
  expect_equal(list(a1), test1)
  expect_equal(alist, test2)
})

test_that("2D sampling circle", {
  #given
  set.seed(20159)
  alpha1 = truncnorm::rtruncnorm(1, a=0.1, b=0.5, mean=0.24, sd=0.05)
  points1 = runif_disk(20)
  a1 = alphahull::ashape(points1, alpha=alpha1)
  #when
  set.seed(20159)
  test1 = sampling2Dashape(N=1, bound="circle")
  #then
  expect_equal(list(a1), test1)
})

test_that("2D sampling annulus", {
  #given
  set.seed(20159)
  alpha1 = truncnorm::rtruncnorm(1, a=0.0625, b=0.125, mean=0.12, sd=0.05)
  n_plus = stats::rpois(1,lambda=3)
  points1 = runif_annulus(20+n_plus, rmax=1, rmin=0.25)
  a1 = alphahull::ashape(points1, alpha=alpha1)
  #when
  set.seed(20159)
  test1 = sampling2Dashape(N=1, n.noise=TRUE, mu=0.12, bound="annulus", n.dependent=FALSE)
  #then
  expect_equal(list(a1), test1)
})

test_that("3D sampling cube", {
  #given
  set.seed(20159)
  points1 = runif_cube(13429)
  points2 = runif_cube(13429)
  a1 = alphashape3d::ashape3d(points1, alpha=0.24)
  a2 = alphashape3d::ashape3d(points2, alpha=0.24)
  alist = list(a1, a2)
  #when
  set.seed(20159)
  test1 = sampling3Dashape(N=1,afixed=TRUE,nconnect=FALSE,nhomology=TRUE)
  set.seed(20159)
  test2 = sampling3Dashape(N=2,afixed=TRUE,nconnect=FALSE,nhomology=TRUE)
  #then
  expect_equal(list(a1), test1)
  expect_equal(alist, test2)
})

test_that("3D sampling sphere", {
  #given
  set.seed(20159)
  alpha1 = truncnorm::rtruncnorm(1, a=0.1, b=0.5, mean=0.24, sd=0.05)
  points1 = runif_ball_3D(39)
  a1 = alphashape3d::ashape3d(points1, alpha=alpha1)
  #when
  set.seed(20159)
  test1 = sampling3Dashape(N=1, bound="sphere")
  #then
  expect_equal(list(a1), test1)
})

test_that("3D sampling shell", {
  #given
  set.seed(20159)
  alpha1 = truncnorm::rtruncnorm(1, a=0.0625, b=0.125, mean=0.12, sd=0.05)
  n_plus = stats::rpois(1,lambda=3)
  points1 = runif_shell_3D(20+n_plus, rmax=1, rmin=0.25)
  a1 = alphashape3d::ashape3d(points1, alpha=alpha1)
  #when
  set.seed(20159)
  test1 = sampling3Dashape(N=1, n.noise=TRUE, mu=0.12, bound="shell", n.dependent=FALSE)
  #then
  expect_equal(list(a1), test1)
})

test_that("sampling errors", {
  expect_error(sampling2Dashape(-1))
  expect_error(sampling2Dashape(0.25))
  expect_warning(sampling2Dashape(10, afixed=TRUE, alpha=10))
  expect_warning(sampling2Dashape(10, mu=10))
  expect_warning(sampling2Dashape(10, nconnect=TRUE, nhomology=TRUE))
  expect_error(sampling2Dashape(10, n.dependent = TRUE, nconnect = FALSE, nhomology=FALSE))
  expect_error(sampling2Dashape(10, bound="triangle"))
  expect_error(sampling3Dashape(-1))
  expect_error(sampling3Dashape(0.25))
  expect_warning(sampling3Dashape(10, afixed=TRUE, alpha=10))
  expect_warning(sampling3Dashape(10, nconnect=TRUE, nhomology=TRUE))
  expect_warning(sampling3Dashape(10, mu=10))
  expect_error(sampling3Dashape(10, n.dependent = TRUE, nconnect = FALSE, nhomology=FALSE))
  expect_error(sampling3Dashape(10, bound="triangle"))
})

test_that("get area, volume functions", {
  expect_equal(1, get_area(r=1, rmin=0, bound="square"))
  expect_equal(pi, get_area(r=1, rmin=0, bound="circle"))
  expect_equal(pi*(1-0.25^2), get_area(r=1, rmin=0.25, bound="annulus"))
  expect_equal(1, get_volume(r=1, rmin=0, bound="cube"))
  expect_equal((4/3)*pi, get_volume(r=1, rmin=0, bound="sphere"))
  expect_equal((4/3)*pi*(1-0.25^3), get_volume(r=1, rmin=0.25, bound="shell"))
  expect_error(get_area(r=1, rmin=0, bound= "triangle"))
  expect_error(get_volume(r=1, rmin=0, bound= "triangle"))
}
)
