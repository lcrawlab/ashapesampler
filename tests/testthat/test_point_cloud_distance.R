test_that(
  'point cloud errors',{
    #given
    NA_mat = matrix(rep(NA,3), nrow=1, ncol=3)
    empty_mat2 = matrix(0, nrow=0, ncol=2)
    empty_mat3 = matrix(0, nrow=0, ncol=3)
    NA_point = c(NA, 1)
    point2 = c(0,0)
    point3 = c(0,0,0)
    points2 <- rbind(c(0,1), c(1,0), c(1,1), c(0,1))
    points3 <- rbind(c(0,1,0), c(1,0,1), c(1,1,0), c(0,1,1))
    #then
    expect_error(euclid_dists_point_cloud_2D(NA_point, points2))
    expect_error(euclid_dists_point_cloud_2D(point2, NA_mat))
    expect_error(euclid_dists_point_cloud_3D(point3, NA_mat))
    expect_error(euclid_dists_point_cloud_3D(NA_point, points3))
    expect_error(euclid_dists_point_cloud_2D(point2, empty_mat2))
    expect_error(eulcid_dists_point_cloud_3D(point3, empty_mat3))
    expect_error(euclid_dists_point_cloud_3D(point2, points3))
    expect_error(euclid_dists_point_cloud_2D(point3, points2))
  }
)

test_that(
  'point cloud 2D',{
  #given
  point = c(0,0)
  #when
  points = rbind(c(0,1), c(1,0), c(0,1))
  #then
  expect_equal(euclid_dists_point_cloud_2D(point,points), c(1,1,1))
  }
)

test_that(
  'point cloud 3D', {
  #given
  point = c(0,0,0)
  #when
  points <- rbind(c(0,1,0), c(1,0,1), c(1,1,0), c(0,1,1))
  test = c(1,sqrt(2), sqrt(2), sqrt(2))
  #then
  expect_equal(euclid_dists_point_cloud_3D(point, points),test)
  }
)