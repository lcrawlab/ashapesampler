test_that( 'tau bound error', {
  #given
  points2 <- rbind(c(0,0), c(0,1), c(1,0))
  points3 <- rbind(c(0,1,0), c(1,0,1), c(1,1,0), c(0,1,1))
  empty_complex = list()
  complex_edges_2 = append(as.list(1:3), list(c(1,2), c(2,3), c(1,3)))
  #when
  points_null1 <- matrix(nrow=0, ncol=3)
  points_null2 <- matrix(nrow=3, ncol=0)
  vNA = rbind(c(1,NA,0), c(0,1,1))
  vNull = matrix(0, nrow=0, ncol=3)
  eNA = rbind(c(1,NA), c(1,2))
  #then
  expect_error(tau_bound(vNA, complex_edges_2))
  expect_error(tau_bound(rbind(c(1,1,1,1), c(2,2,2,2)), complex_edges_2))
  expect_error(tau_bound(vNull, empty_complex))
  expect_error(tau_bound(rbind(c(1,1,1)), list(1)))
  expect_error(tau_bound(points2, complex_edges_2, extremes = 1:10))
  expect_error(tau_bound(points2, empty_complex))
  expect_error(tau_bound(points3, complex_edges_2))
}
)

test_that( 'tau bound', {
  #given
  points2 <- rbind(c(0,0), c(0,1), c(1,0))
  points3 <- rbind(c(0,1,0), c(1,0,1), c(1,1,0), c(0,1,1))
  complex_iso_2 = as.list(1:3)
  complex_iso_3 = as.list(1:4)
  complex_1_iso = append(as.list(1:3), list(c(1,2)))
  complex_edges_2 = append(as.list(1:3), list(c(1,2), c(2,3), c(1,3)))
  complex_face_2 = append(as.list(1:3), list(c(1,2), c(2,3), c(1,3), c(1,2,3)))
  complex_edges_3 = append(as.list(1:4), list(c(1,2), c(1,3), c(1,4), c(2,3), 
                                              c(2,4), c(3,4)))
  complex_face_3 = append(as.list(1:4), list(c(1,2), c(1,3), c(1,4), c(2,3), 
                                             c(2,4), c(3,4), c(1,2,3), c(1,3,4),
                                             c(1,2,4), c(2,3,4)))
  complex_tet_3 = append(as.list(1:4), list(c(1,2), c(1,3), c(1,4), c(2,3), 
                                            c(2,4), c(3,4), c(1,2,3), c(1,3,4),
                                            c(1,2,4), c(2,3,4), c(1,2,3,4)))
  #then
  expect_equal(tau_bound(points2, complex_iso_2), 1)
  expect_equal(tau_bound(points3, complex_iso_3), 1)
  expect_equal(tau_bound(points2, complex_1_iso), 1)
  expect_equal(tau_bound(points2, complex_edges_2), 1)
  expect_equal(tau_bound(points3, complex_edges_3), sqrt(2))
  expect_equal(tau_bound(points2, complex_face_2), sqrt(2))
  expect_equal(tau_bound(points3, complex_face_3), sqrt(3))
  expect_equal(tau_bound(points3, complex_tet_3), sqrt(3))
  expect_equal(count_neighbors(points2, complex_edges_2), c(2,2,2))
}
)