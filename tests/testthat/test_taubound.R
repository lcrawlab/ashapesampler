test_that( 'tau bound error', {
  #given
  points <- rbind(c(0,1,0), c(1,0,1), c(1,1,0), c(0,1,1))
  edge_df = data.frame("ed1" = c(3,4,3,4,4,2), "ed2" = c(1,1,2,2,3,1))
  #when
  points_null1 <- matrix(nrow=0, ncol=3)
  points_null2 <- matrix(nrow=3, ncol=0)
  vNA = rbind(c(1,NA,0), c(0,1,1))
  vNull = matrix(0, nrow=0, ncol=3)
  eNA = rbind(c(1,NA), c(1,2))
  #then
  expect_error(tau_bound(points, points_null1))
  expect_error(tau_bound(points, points_null2))
  expect_error(tau_bound(vNA, edge_df))
  expect_error(tau_bound(points, eNA))
  expect_error(tau_bound(vNull, edge_df))
}
)

test_that( 'tau bound', {
  #given
  points <- rbind(c(0,1,0), c(1,0,1), c(1,1,0), c(0,1,1))
  edge_df = data.frame("ed1" = c(3,4,3,4,4,2), "ed2" = c(1,1,2,2,3,1))
  edge_df_empty = data.frame("ed1"=0, "ed2"=0)
  #then
  expect_equal(tau_bound(points, edge_df), sqrt(2))
  expect_equal(tau_bound(points, edge_df_empty), 1)
}
)