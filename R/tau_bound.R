
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
#' @param extremes matrix or data frame of cartesian coordinates of vertices on
#'                 the boundary of the data frame. If no list given, function will
#'                 assume all points are extreme and check them all. Inclusion of
#'                 this parameter speeds up the process.
#' @param e_list data frame or matrix of edges, listed as pairs of vertices
#' @param f_list data frame or matrix of faces, listed as sets of 3 vertices
#' @param t_list data frame or matrix of faces, listed as sets of 4 vertices
#'
#' @return tau, real nonnegative number. 
#' @export
#'
tau_bound <- function(v_list, e_list, extremes=NULL, f_list=NULL, t_list=NULL){
  if(sum(is.na(v_list))>0 || sum(is.na(e_list))>0){
    stop("NA values in input vertex or edge lists.")
  }
  n = dim(v_list)[1]
  if(n==0){
    stop("Vertex matrix empty")
  }
  m = length(extremes)
  if (m == 0){
    extremes = 1:n
    m=n
  }
  dist_matrix = as.matrix(dist(v_list))
  e_list = as.data.frame(e_list)
  colnames(e_list) = c("ed1", "ed2") #To keep consistent
  tau_keep = 100 #intentionally make too big
  for (k in 1:m){
    i = extremes[k]
    edge_list_zoom = c(dplyr::filter(e_list, ed2==i)[,1], 
                       dplyr::filter(e_list, ed1==i)[,2])
    #Get distance vector
    dist_vec = as.matrix(dist_matrix[,i])
    #Find smallest distance from point that is longer than edges
    test_tau=0
    if (length(edge_list_zoom)>0){
      test_tau = min(dist_vec[dist_vec>=max(dist_vec[edge_list_zoom])])
    } else {
      test_tau = min(dist_vec[dist_vec>0])  #case where isolated point
    }
    if (test_tau < tau_keep){
      tau_keep = test_tau
    }
  }
  return(tau_keep)
}