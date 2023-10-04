test_that("mcmc errors", {
  #given
  N=2
  points2 = cbind(stats::runif(20), stats::runif(20))
  points3 = cbind(stats::runif(500), stats::runif(500), stats::runif(500))
  tau=1
  #then
  expect_error(generate_ashape2d(points3, N, tau))
  expect_error(generate_ashape3d(points2, N, tau))
  expect_error(generate_ashape2d(points2, N, tau, afixed=FALSE, sig=-1))
  expect_error(generate_ashape3d(points3, N, tau, afixed=FALSE, sig=-1))
  expect_error(generate_ashape2d(points2, J=-1, tau))
  expect_error(generate_ashape3d(points3, J=-1, tau))
  expect_error(generate_ashape2d(points2, N, tau=-0.7))
  expect_error(generate_ashape3d(points3, N, tau=-0.7))
  expect_error(generate_ashape3d(points3, N, tau=0.0001))
  expect_error(generate_ashape2d(points2, N, tau=0.0001))
  expect_warning(generate_ashape2d(points2, N, tau, afixed=FALSE, mu=6))
  expect_warning(generate_ashape3d(points3, N, tau,afixed=FALSE, mu=6))
  #expect_warning(generate_ashape2d(points2, J=2, tau=5, cores=120))
  #expect_warning(generate_ashape3d(points3, J=2, tau=5, cores=120))
  expect_error(generate_ashape2d(points2, J=2, tau=5, k_min=100))
  expect_error(generate_ashape3d(points3, J=2, tau=6, k_min=1000))
  expect_error(generate_ashape2d(points2, N, tau, sample_rad=-1))
  expect_error(generate_ashape2d(points2, N, tau, acc_rad=-1))
  expect_error(generate_ashape3d(points3, N, tau, sample_rad=-1))
  expect_error(generate_ashape3d(points3, N, tau, acc_rad=-1))
})

test_that("2D mcmc smooth runs", {
  #given
  tau=1
  J=2
  points2 = cbind(stats::runif(20), stats::runif(20))
  #then
  expect_no_error(generate_ashape2d(points2, J, tau))
})

test_that("3D mcmc smooth runs", {
  #given
  set.seed(201727)
  tau=1
  J=2
  points3 = cbind(stats::runif(500,0.8,1), stats::runif(500,0.8,1),
                  stats::runif(500,0.8,1))
  #then
  expect_no_error(generate_ashape3d(points3, J, tau))
})

