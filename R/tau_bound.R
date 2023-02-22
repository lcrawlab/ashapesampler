
#' tau_bound
#' 
#' This function finds the bound of tau for one shape, which is the maximum length of the 
#' fiber bundle off of a shape for determining the density of points necessary
#' to recover the homology from the open cover. See Niyogi et al 2008. Function
#' checks length of edges and distances to circumcenters from each vertex before
#' checking against the rest of the point cloud and finds the minimum length.
#' We then keep the largest tau to account for the possibility of nonuniformity 
#' among points. 
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
#' @param cores number of cores for parallelizing. Default 1.
#' @return tau, real nonnegative number. 
#' @export
#' @importFrom stats na.omit
#' @import doParallel
#' @import foreach
tau_bound <- function(v_list, complex, extremes=NULL, cores = 1){
  ### Determine the number of Cores for Parallelization ###
  if(cores > 1){
    if(cores>detectCores()){
      warning("The number of cores you're setting is larger than available cores!")
      cores <- max(1L, detectCores(), na.rm = TRUE)}
  }
  
  ### Register those Cores ###
  registerDoParallel(cores=cores)
  
  dimension = dim(v_list)[2]
  n = dim(v_list)[1]
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
  f_circ = circumcenter_face(v_list, f_list)
  t_list = NULL
  t_circ = NULL
  if(dimension>2){
    t_list = extract_complex_tet(complex,m)
    t_circ = circumcenter_tet(v_list, t_list)
  }
  tau_vec=vector("numeric", m)
  tau_vec = foreach(k=1:m, .combine=cbind,
                     .export = c("euclid_dists_point_cloud_2D",
                                 "euclid_dists_point_cloud_3D"))%dopar%{
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
    if (length(edge_list_zoom)==0){
      tau = min(dist_vec_point[dist_vec_point>0])  #case where isolated point
    } else {
      dist_vec = dist_vec_point[edge_list_zoom]
      dist_vec_b = c()
      if (dimension == 2){
        if(!is.null(face_list_zoom)){
          points = matrix(f_circ[face_list_zoom,], ncol=2)
          dist_vec_b = c(dist_vec_b, 2*euclid_dists_point_cloud_2D(v_list[i,],
                                                              points ))
        }
      } else {
        if(!is.null(face_list_zoom)){
          points = matrix(f_circ[face_list_zoom,], ncol=3)
          dist_vec_b = 2*euclid_dists_point_cloud_3D(v_list[i,],points)
          if(!is.null(tet_list_zoom)){
          points = matrix(t_circ[tet_list_zoom,], ncol=3)
          dist_vec_b = c(dist_vec_b, 2*euclid_dists_point_cloud_3D(v_list[i,], points))
          }
        }
      }
      dist_vec = max(c(dist_vec, dist_vec_b))
      if(length(dist_vec_point[dist_vec_point>dist_vec])==0){
        tau = dist_vec
      } else {
        tau = min(dist_vec_point[dist_vec_point>dist_vec])
      }

    }
    tau
  }
  tau_keep = min(tau_vec[tau_vec>0])
  return(tau_keep)
}

#' circumcenter Face
#'
#' This function finds the circumcenters of the faces of a simplicial complex given the 
#' list of vertex coordinates and the set of faces.
#'
#' @param v_list matrix of vertex coordinates
#' @param f_list matrix with 3 columns with face information.
#'
#' @return circ_mat, matrix of coordinates of circumcenters of faces.
circumcenter_face <- function(v_list, f_list){
  if(is.null(f_list)){
    return(NULL)
  }
  dimension = dim(v_list)[2]
  nface = dim(f_list)[1]
  circ_mat = matrix(NA, nrow=nface, ncol = dimension)
  if(dimension==2){
    for (i in 1:nface){
      points = rbind(v_list[f_list$f1[i],], v_list[f_list$f2[i],], v_list[f_list$f3[i],])
      circ_mat[i,] <- circ_face_2D(points)
    }
  } else {
    for (i in 1:nface){
    points = rbind(v_list[f_list$f1[i],], v_list[f_list$f2[i],], v_list[f_list$f3[i],])
    circ_mat[i,] <- circ_face_3D(points)
    } 
  }
  return(circ_mat)
}

#' Circumcenter face - three points in 2D
#' Given 3 sets of coordinates, calculates the circumcenter
#' @param points, 3x2 matrix
#'
#' @return 1x2 vector, coordinates of circumcenter
circ_face_2D <- function(points){
  if(dim(points)[2]!=2 || dim(points)[1]!=3){
    stop("Input must be 3 by 2 matrix")
  }
  a = points[1,1]
  b = points[1,2]
  c = points[2,1]
  d = points[2,2]
  f = points[3,1]
  g = points[3,2]
  R = a*(d-g)+c*(g-b)+f*(b-d)
  J = -a^2*(d-g)-b^2*(d-g)-c^2*(g-b)-d^2*(g-b)-f^2*(b-d)-g^2*(b-d)
  K = a^2*(c-f)+b^2*(c-f)+c^2*(f-a)+d^2*(f-a)+f^2*(a-c)+g^2*(a-c)
  return(-c(J,K)/(2*R))
}

#' Circumcenter face - three points in 3D
#' Given 3 sets of coordinates, calculates the circumcenter
#' @param points, 3x3 matrix
#'
#' @return 1x3 vector, coordinates of circumcenter
circ_face_3D <- function(points){
  if(dim(points)[2]!=3 || dim(points)[1]!=3){
    stop("Input must be 3 by 3 matrix")
  }
  a = points[1,1]
  b = points[1,2]
  c = points[1,3]
  d = points[2,1]
  f = points[2,2]
  g = points[2,3]
  i = points[3,1]
  j = points[3,2]
  k = points[3,3]
  A <- rbind(c(d-a, f-b, g-c), c(i-d, j-f, k-g), c((b*g-b*k-c*f+c*j+f*k-g*j),
                                                   (a*k-a*g+c*d-c*i-d*k+g*i),
                                                   (a*f-a*j-b*d+b*i+d*j-i*f)))
  B <- c(1/2*(d^2-a^2+f^2-b^2+g^2-c^2), 1/2*(i^2-d^2+j^2-f^2+k^2-g^2), 
         (a*(f*k-g*i)-b*(d*k-g*i)+c*(d*j-f*i)))
  my_vec <- solve(A,B)
  return(my_vec)
}

#' circumcenter Tetrahedra
#' 
#' This function finds the circumcenters of the tetrahedra/3-simplices of a simplicial
#' complex given the list of vertex coordinates and the set of tetrahedra.
#'
#' @param v_list matrix of vertex coordinates
#' @param t_list matrix of 4 columns with tetrahedra
#'
#' @return circ_mat, matrix of coordinates of circumcenters of teterahedra
circumcenter_tet <- function(v_list, t_list){
  if(is.null(t_list)){
    return(NULL)
  }
  ntet = dim(t_list)[1]
  circ_mat = matrix(NA, nrow=ntet, ncol = 3)
  for(i in 1:ntet){
    points = rbind(v_list[t_list$t1[i],], v_list[t_list$t2[i],], v_list[t_list$t3[i],], v_list[t_list$t4[i],])
    circ_mat[i,] <- circ_tet_3D(points)
  }
  return(circ_mat)
}

#' Circumcenter tetrahedron - 4 points in 3D
#' Given 3D coordinates of 4 points, calculates circumcenter
#' @param points, 4x3 matrix
#'
#' @return 1x3 vector, coordinates of circumcenter
circ_tet_3D <- function(points){
  if(dim(points)[2]!=3 || dim(points)[1]!=4){
    stop("Input must be 4 by 3 matrix")
  }
  points = cbind(points[,1]^2+points[,2]^2 + points[,3]^2, points, c(1,1,1,1))
  Q = det(points[, 2:5])
  X = -1*det(cbind(points[,1], points[,3:5]))
  Y = det(cbind(points[,1:2], points[,4:5]))
  Z = -1*det(cbind(points[,1:3], points[,5]))
  return(-c(X,Y,Z)/(2*Q))
}