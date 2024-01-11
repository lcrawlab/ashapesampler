
#' Get alpha complex
#'
#' Generates alpha complex for a set of points and parameter alpha
#'
#' @param points point cloud for alpha complex, in form of 2 column of 3 column
#'                           matrix with nonzero number of rows
#' @param alpha  alpha parameter for building the alpha complex
#'
#' @return complex list of vertices, edges, faces, and tetrahedra.
#'
#' @export
#' @importFrom TDA alphaComplexFiltration
get_alpha_complex <- function(points, alpha){
  if (alpha < 0){
    stop("alpha must be nonnegative.")
  }
  alpha = alpha^2
  if(dim(points)[1]==0 || dim(points)[2]==0){
    stop("Dimensions of points matrix incorrect, must be larger than 0.")
  }
  filtration <- TDA::alphaComplexFiltration(points)
  bound <- length(which(filtration$values <= alpha))
  complex <- filtration$cmplx[1:bound]
  return(complex)
}

#' Returns the edges of complex.
#' @param complex complex object from TDA packages
#'
#' @param n_vert number of vertices in complex; default is 0, specifying
#'               this parameter speeds up the function
#'
#' @return edge_list data frame or if empty NULL
#' @export
extract_complex_edges <- function(complex, n_vert=0){
  if (n_vert < 0){
    n_vert=0
  }
  m = length(complex)
  if(n_vert > m){
    stop("Complex length and vertices don't match")
  }
  if(n_vert == m){
    return(NULL)
  }
  edge_list = matrix(NA, nrow=(m-n_vert), ncol=2)
  #First n_vert entries are the vertices, no need to check
  for (k in (n_vert+1):m){
    if(length(as.vector(complex[[k]]))==2){
      temp <- sort(complex[[k]])
      edge_list[(k-n_vert),] <- temp
    }
  }
  edge_list = as.data.frame(na.omit(edge_list))
  if (nrow(edge_list)==0){
    return(NULL)
  } else {
    colnames(edge_list)=c("ed1", "ed2")
    return(edge_list)
  }
}

#' Returns faces of complex.
#' @param complex complex object from TDA package
#'
#' @param n_vert number of vertices in the complex; default is 0, specifying
#'                this parameter speeds up function
#'
#' @return face_list data frame of points forming faces in complex
#' @export
extract_complex_faces <- function(complex, n_vert=0){
  if (n_vert < 0){
    n_vert=0
  }
  m = length(complex)
  if(n_vert > m){
    stop("Complex length and vertices don't match")
  }
  if(n_vert == m){
    return(NULL)
  }
  face_list = matrix(NA, nrow=(m-n_vert), ncol=3)
  #First n_vert entries are the vertices, no need to check
  for (k in (n_vert+1):m){
    if(length(as.vector(complex[[k]]))==3){
      temp <- sort(complex[[k]])
      face_list[(k-n_vert),] <- temp
    }
  }
  face_list = as.data.frame(na.omit(face_list))
  if (nrow(face_list)==0){
    return(NULL)
  } else {
    colnames(face_list)=c("f1", "f2", "f3")
    return(face_list)
  }
}

#' Returns tetrahedra of complex (3 dimensions)
#' @param complex complex object from TDA package
#' @param n_vert number of vertices in the complex; default is 0, specifying this
#'               parameter speeds up function
#' @return tet_list data frame of points forming tetrahedra in complex
#' @export
extract_complex_tet <- function(complex, n_vert=0){
  if (n_vert < 0){
    n_vert=0
  }
  m = length(complex)
  if(n_vert > m){
    stop("Complex length and vertices don't match")
  }
  if(n_vert == m){
    return(NULL)
  }
  tet_list = matrix(NA, nrow=(m-n_vert), ncol=4)
  #First n_vert entries are the vertices, no need to check
  for (k in (n_vert+1):m){
    if(length(as.vector(complex[[k]]))==4){
      temp <- sort(complex[[k]])
      tet_list[(k-n_vert),] <- temp
    }
  }
  tet_list = as.data.frame(na.omit(tet_list))
  if (nrow(tet_list)==0){
    return(NULL)
  } else {
    colnames(tet_list)=c("t1", "t2", "t3", "t4")
    return(tet_list)
  }
}
