
#' tau_bound
#' 
#' This function finds the bound of tau, which is the maximum length of the 
#' fiber bundle off of a shape for determining the density of points necessary
#' to recover the homology from the open cover. See Niyogi et al 2008. Function
#' checks length of edges and distances to barycenters from each vertex before
#' checking against the rest of the point cloud and finds the minimum length.
#'
#' @param v_list matrix or data frame of cartesian coordinates of vertices in 
#'               in point cloud
#' @param complex list of each vertex, edge, face, and (in 3D) tetrahedron in 
#'                a simplicial complex; same form as complex object in TDA package
#' @param extremes matrix or data frame of cartesian coordinates of vertices on
#'                 the boundary of the data frame. If no list given, function will
#'                 assume all points are extreme and check them all. Inclusion of
#'                 this parameter speeds up the process both within this function
#'                 and when calculating alpha because you will get a bigger (but 
#'                 still valid) tau bound.
#' @return tau, real nonnegative number. 
#' @export
#' @importFrom stats na.omit
tau_bound <- function(v_list, complex, extremes=NULL){
  dimension = dim(v_list)[2]
  n = dim(v_list)[1]
  tau_vec = vector("numeric", n)
  if(sum(is.na(v_list))>0){
    stop("NA values in input vertex matrix.")
  }
  if(dimension!=2 & dimension !=3){
    stop("Vertex matrix must be 2 or 3 columns; if higher, please project to 2 or 3 dimensions.")
  }
  if(n==0){
    stop("Vertex matrix empty")
  }
  if(n==1){
    stop("Only one vertex; tau is infinite.")
  }
  if (length(extremes)>n){
    stop("Can't have more extreme points than vertices.")
  }
  #Check complex
  if(length(complex)<n){
    stop("Improper format for complex; must be list that includes vertices, and where applicable, edges, faces, and tetrahedra.")
  }
  if(length(unlist(complex[1:n]))!=n){
    stop("Vertices must be listed at start of complex list and number of vertices
         in complex must match number of vertices in vertex matrix.")
  }
  m = length(extremes)
  if (m == 0){
    extremes = 1:n
    m=n
  }
  dist_matrix = as.matrix(stats::dist(v_list))
  e_list = extract_complex_edges(complex,m)
  if(is.null(e_list)){
    return(min(dist_matrix[dist_matrix>0]))
  }
  f_list = extract_complex_faces(complex,m)
  f_bary = barycenter_face(v_list, f_list)
  t_list = NULL
  t_bary = NULL
  if(dimension>2){
    t_list = extract_complex_tet(complex,m)
    t_bary = barycenter_tet(v_list, t_list)
  }
  tau_keep = 100 
  for (k in 1:m){
    i = extremes[k]
    edge_list_zoom = c(which(e_list$ed1==i), which(e_list$ed2==i))
    edge_list_zoom = c(e_list[edge_list_zoom,1], e_list[edge_list_zoom,2])
    edge_list_zoom = edge_list_zoom[which(edge_list_zoom != i)]
    face_list_zoom = NULL
    tet_list_zoom = NULL
    if(!is.null(f_list)){
      face_list_zoom = c(which(f_list$f1==i), which(f_list$f2==i), 
                         which(f_list$f3==i))
    }
    if(!is.null(t_list)){
      tet_list_zoom = c(which(t_list$t1==i), which(t_list$t2==i),
                        which(t_list$t3==i), which(t_list$t4==i))
    }
    #Get distance vector
    dist_vec_point = as.matrix(dist_matrix[,i])
    #Find smallest distance from point that is longer than edges, face bc, or tet bc
    test_tau=0
    if (length(edge_list_zoom)==0){
      test_tau = min(dist_vec_point[dist_vec_point>0])  #case where isolated point
    } else {
      dist_vec = dist_vec_point[edge_list_zoom]
      dist_vec_b = c()
      if (dimension == 2){
        if(!is.null(face_list_zoom)){
          points = matrix(f_bary[face_list_zoom,], ncol=2)
          dist_vec_b = c(dist_vec_b, 2*euclid_dists_point_cloud_2D(v_list[i,],
                                                              points ))
        }
      } else {
        if(!is.null(face_list_zoom)){
          points = matrix(f_bary[face_list_zoom,], ncol=3)
          dist_vec_b = 2*euclid_dists_point_cloud_3D(v_list[i,],points)
          if(!is.null(tet_list_zoom)){
          points = matrix(t_bary[tet_list_zoom,], ncol=3)
          dist_vec_b = c(dist_vec_b, 2*euclid_dists_point_cloud_3D(v_list[i,], points))
          }
        }
      }
      dist_vec = max(c(dist_vec, dist_vec_b))
      if(length(dist_vec_point[dist_vec_point>dist_vec])==0){
        test_tau = dist_vec
      } else {
        test_tau = min(dist_vec_point[dist_vec_point>dist_vec])
      }
      if (test_tau < tau_keep){
        tau_keep = test_tau
      }
    }
  }
  #tau_keep = min(tau_vec[tau_vec>0])
  return(tau_keep)
}


#' Barycenter Face
#'
#' This function finds the barycenters of the faces of a simplicial complex given the 
#' list of vertex coordinates and the set of faces.
#'
#' @param v_list matrix of vertex coordinates
#' @param f_list matrix with 3 columns with face information.
#'
#' @return bary_mat, matrix of coordinates of barycenters of faces.
barycenter_face <- function(v_list, f_list){
  if(is.null(f_list)){
    return(NULL)
  }
  bary_mat = (1/3)*(v_list[f_list$f1,]+v_list[f_list$f2,]+v_list[f_list$f3,])
  bary_mat = matrix(bary_mat, nrow=dim(f_list)[1], ncol = dim(v_list)[2])
  return(bary_mat)
}

#' Barycenter Tetrahedra
#' 
#' This function finds the barycenters of the tetrahedra/3-simplices of a simplicial
#' complex given the list of vertex coordinates and the set of tetrahedra.
#'
#' @param v_list matrix of vertex coordinates
#' @param t_list matrix of 4 columns with tetrahedra
#'
#' @return bary_mat, matrix of coordinates of barycenters of teterahedra
barycenter_tet <- function(v_list, t_list){
  if(is.null(t_list)){
    return(NULL)
  }
  bary_mat = (1/4)*(v_list[t_list$t1,]+v_list[t_list$t2,]+v_list[t_list$t3,]
                    +v_list[t_list$t4,])
  bary_mat = matrix(bary_mat, nrow=dim(t_list)[1], ncol = dim(v_list)[2])
  return(bary_mat)
}


