test_that("mcmc errors", {
  #given
  N=2
  points2 = cbind(stats::runif(20), stats::runif(20))
  points3 = cbind(points2, stats::runif(20))
  tau=1
  #then
  expect_error(generate_ashape2d(points3, N, tau))
  expect_error(generate_ashape3d(points2, N, tau))
  expect_error(generate_ashape2d(points2, N, tau, sig=-1))
  expect_error(generate_ashape3d(points3, N, tau, sig=-1))
  expect_error(generate_ashape2d(points2, N, tau, bound="triangle"))
  expect_error(generate_ashape3d(points3, N, tau, bound="triangle"))
  expect_error(generate_ashape2d(points2, N=-1, tau))
  expect_error(generate_ashape3d(points3, N=-1, tau))
  expect_error(generate_ashape2d(points2, N, tau=-0.7))
  expect_error(generate_ashape3d(points3, N, tau=-0.7))
  expect_warning(generate_ashape2d(points2, N, tau, mu=6))
  expect_warning(generate_ashape3d(points3, N, tau, mu=6))
})

test_that("2D mcmc smooth runs", {
  #given
  tau=1
  N=2
  points2 = cbind(stats::runif(20), stats::runif(20))
  #then
  expect_no_error(generate_ashape2d(points2, N, tau))
  expect_no_error(generate_ashape2d(points2, N, tau, bound="square"))
  expect_no_error(generate_ashape2d(points2, N, tau, bound = "annulus"))
})

test_that("3D mcmc smooth runs", {
  #given
  tau=1
  N=2
  points3 = cbind(stats::runif(10,0.01,0.1), stats::runif(10,0.01,0.1),
                  stats::runif(10,0.01,0.1))
  #then
  expect_no_error(generate_ashape3d(points3, N, tau))
  expect_no_error(generate_ashape3d(points3, N, tau, bound="cube"))
  expect_no_error(generate_ashape3d(points3, N, tau, bound = "shell"))
})

