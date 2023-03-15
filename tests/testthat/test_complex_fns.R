test_that(
  "get alpha complex errors", {
    #given
    points <- rbind(c(0,1,0), c(1,0,1), c(1,1,0), c(0,1,1))
    acomplex <- TDA::alphaComplexFiltration(points)$cmplx
    #when
    points_null1 <- matrix(nrow=0, ncol=3)
    points_null2 <- matrix(nrow=3, ncol=0)
    alpha_neg <- -1
    alpha_pos <- 1
    #then
    expect_error(get_alpha_complex(points, alpha_neg))
    expect_error(get_alpha_complex(points_null1, alpha_pos))
    expect_error(get_alpha_complex(points_null2, alpha_pos))
    expect_error(extract_complex_edges(acomplex, n_vert=100))
    expect_error(extract_complex_faces(acomplex, n_vert=100))
    expect_error(extract_complex_tet(acomplex, n_vert=100))
  }
)

test_that(
  "Alpha Complex Results", {
    #given
    alpha0 <- 0
    alpha1 <- 0.72
    alpha2 <- 1
    points <- rbind(c(0,1,0), c(1,0,1), c(1,1,0), c(0,1,1))
    #when
    alpha0complex <- get_alpha_complex(points, alpha0)
    alpha1complex <- get_alpha_complex(points, alpha1)
    alpha2complex <- get_alpha_complex(points, alpha2)
    fullalpha <- TDA::alphaComplexFiltration(points)$cmplx
    alpha1true <- list(1,2,3,4, c(3,1), c(4,1), c(3,2), c(4,2), c(4,3), c(4,3,1))
    #then
    expect_equal(alpha0complex, list(1,2,3,4))
    expect_equal(alpha1complex, alpha1true)
    expect_equal(alpha2complex, fullalpha)
  }
)

test_that(
  "Getting simplex lists", {
    #given
    points <- rbind(c(0,1,0), c(1,0,1), c(1,1,0), c(0,1,1))
    empty_complex = list(1,2,3,4)
    full_complex = TDA::alphaComplexFiltration(points)$cmplx
    edge_df = data.frame("ed1" = c(3,4,3,4,4,2), "ed2" = c(1,1,2,2,3,1))
    face_df = data.frame("f1" = c(4,4,3,4), "f2" = c(3,3,2,2), "f3" = c(1,2,1,1))
    tet_df = data.frame("t1" = 4, "t2"=3, "t3" = 2, "t4" = 1)
    #when
    emptyedge1 = extract_complex_edges(empty_complex,4)
    emptyedge2 = extract_complex_edges(empty_complex)
    emptyface1 = extract_complex_faces(empty_complex, 4)
    emptyface2 = extract_complex_faces(empty_complex)
    emptytet1 = extract_complex_tet(empty_complex, 4)
    emptytet2 = extract_complex_tet(empty_complex)
    fulledge1 = extract_complex_edges(full_complex,4)
    fulledge2 = extract_complex_edges(full_complex)
    fulledge3 = extract_complex_edges(full_complex, -1)
    fullface1 = extract_complex_faces(full_complex, 4)
    fullface2 = extract_complex_faces(full_complex)
    fullface3 = extract_complex_faces(full_complex, -1)
    fulltet1 = extract_complex_tet(full_complex, 4)
    fulltet2 = extract_complex_tet(full_complex)
    fulltet3 = extract_complex_tet(full_complex, -2)
    #then
    expect_equal(emptyedge1, NULL)
    expect_equal(emptyedge2, NULL)
    expect_equal(emptyface1, NULL)
    expect_equal(emptyface2, NULL)
    expect_equal(emptytet1, NULL)
    expect_equal(emptytet2, NULL)
    expect_equal(fulledge1, edge_df)
    expect_equal(fulledge2, edge_df)
    expect_equal(fulledge3, edge_df)
    expect_equal(fullface1, face_df)
    expect_equal(fullface2, face_df)
    expect_equal(fullface3, face_df)
    expect_equal(fulltet1, tet_df)
    expect_equal(fulltet2, tet_df)
    expect_equal(fulltet3, tet_df)
  }
)
