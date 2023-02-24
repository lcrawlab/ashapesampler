#mcmc sampling

#' Generate 3D alpha shape
#'
#' @param point_cloud 3 column matrix of all points from all shapes in initial 
#'                    data set
#' @param N number of shapes in initial data set
#' @param tau tau bound
#' @param delta probability of not preserving homology; default is 0.05
#' @param bound manifold bound being used to sample points
#' @param afixed boolean, whether to sample alpha or leave fixed based on tau. Default FALSE
#' @param mu mean of truncated distribution from which alpha sampled; default tau/3
#' @param sig standard deviation of truncated distribution from which alpha 
#'              sampled; default tau/12
#' @param k_min number of points needed in radius 2 alpha of point cloud to accept a sample
#' @param eps amount to subtract from tau/2 to give alpha. Defaul 1e-4.
#' @param cores number of cores for parallelizing. Default 1.
#'
#' @return new_ashape three dimensional alpha shape object from alphashape3d library
#' @export
#' @importFrom stats runif
#' @import doParallel
#' @import foreach
generate_ashape3d <- function(point_cloud, N, tau, delta=0.05,
                              afixed = TRUE, mu=NULL, sig = NULL, k_min=3, eps=1e-4,
                              cores=1){
  ### Determine the number of Cores for Parallelization ###
   if(cores > 1){
     if(cores>parallel::detectCores()){
       warning("The number of cores you're setting is larger than available cores!")
       cores <- max(1L, parallel::detectCores(), na.rm = TRUE)}
   }
  registerDoParallel(cores=cores)
  #Check: 3 columns on vertex list
  if(dim(point_cloud)[2]!=3){
    stop("Point cloud does not have correct number of columns.")
  }
  n_vert = dim(point_cloud)[1]
  if(N<=0 || floor(N) !=N){
    stop("N must be positive integer.")
  }
  if(tau<=0){
    stop("Tau must be positive real number.")
  }
  #Sample alpha
  my_alpha <- 0
  if(afixed==FALSE){
    if(is.null(mu)){
      mu=tau/3
    }
    if(is.null(sig)){
      sig=tau/12
    } else if (sig <0){
      stop("sig must be nonnegative value.")
    }
    if(mu>tau/2 || mu<0){
      warning("Mean of alpha outside of truncated distribution range for alpha")
    }
    my_alpha <- truncnorm::rtruncnorm(1, a=0, b=tau/2, mean=mu, sd=sig)
  } else {
    my_alpha <- tau/2-eps
  }

  #Sample and reject points
  my_points = matrix(NA, nrow=0, ncol=3)
  #Initialize by taking point from point cloud.
  m = n_bound_homology_3D((4/3)*pi*(my_alpha/4)^3, epsilon = my_alpha, tau=tau)
  
  my_points = foreach(
    i = 1:dim(point_cloud)[1],
    .combine = rbind,
    .export = c("runif_ball_3D", "euclid_dists_point_cloud_3D")
  ) %dopar% {
  #for (i in 1:n_vert){
    new_points = runif_ball_3D(m, r = my_alpha/4) + rep(point_cloud[i, ], each=m)
    keep_pts = matrix(NA, nrow=0, ncol=3)
    for (j in 1:m) {
      dist_list = euclid_dists_point_cloud_3D(new_points[j, ], point_cloud)
      dist_near = dist_list[dist_list < 2 * my_alpha]
      knn = length(dist_near)
      if (knn >= N) {
        keep_pts = rbind(keep_pts, new_points[j, ])
      } else if (knn > k_min) {
        a_prob = 1 - exp(-(knn - k_min) * 2 / N)
        if (runif(1) < a_prob) {
          keep_pts = rbind(keep_pts, new_points[j, ])
        }
      }
    }
    keep_pts
  }

  if(dim(my_points)[1]<5){
    stop("Not enough points accepted in MCMC walk to make a shape. Need at least 5.")
  }
  new_ashape <- alphashape3d::ashape3d(my_points, alpha=my_alpha)
  return(new_ashape)
}

#' Generate 2D alpha shape
#'
#' @param point_cloud 2 column matrix of all points from all shapes in initial 
#'                    data set
#' @param N number of shapes in initial data set
#' @param tau tau bound
#' @param delta probability of not preserving homology; default is 0.05
#' @param afixed boolean, whether to sample alpha or leave fixed based on tau. Default FALSE
#' @param mu mean of truncated distribution from which alpha sampled; default tau/3
#' @param sig standard deviation of truncated distribution from which alpha 
#'              sampled; default tau/12
#' @param k_min number of points needed in radius 2 alpha of point cloud to accept a sample
#' @param eps amount to subtract from tau/2 to give alpha. Defaul 1e-4.
#' @param cores number of computer cores for parallelizing. Default 1.
#'
#' @return new_ashape two dimensional alpha shape object from alphahull library
#' @export
#' @importFrom stats runif
#' @import doParallel
#' @import foreach
generate_ashape2d <- function(point_cloud, N, tau, delta=0.05,
                              afixed=TRUE, mu=NULL, sig = NULL, k_min=3, eps=1e-4,
                              cores=1){
  ### Determine the number of Cores for Parallelization ###
  if(cores > 1){
    if(cores>parallel::detectCores()){
      warning("The number of cores you're setting is larger than available cores!")
      cores <- max(1L, parallel::detectCores(), na.rm = TRUE)}
  }
  registerDoParallel(cores=cores)
  #Check: 2 columns on vertex list
  if(dim(point_cloud)[2]!=2){
    stop("Point cloud does not have correct number of columns.")
  }
  n_vert = dim(point_cloud)[1]
  if(N<=0 || floor(N) !=N){
    stop("N must be positive integer.")
  }
  if(tau<=0){
    stop("Tau must be positive real number.")
  }
  #Sample alpha
  my_alpha=0
  if(afixed==FALSE){
    if(is.null(mu)){
      mu=tau/3
    }
    if(is.null(sig)){
      sig=tau/12
    } else if (sig <0){
      stop("sig must be nonnegative value.")
    }
    if(mu>tau/2 || mu<0){
      warning("Mean of alpha outside of truncated distribution range for alpha")
    }
    my_alpha <- truncnorm::rtruncnorm(1, a=0, b=tau/2, mean=mu, sd=sig)
  } else {
    my_alpha <- tau/2-eps
  }
  #Sample and reject points
  my_points = matrix(NA, nrow=0, ncol=2)
  #Initialize by taking point from point cloud.
  m = n_bound_homology_2D(pi*(my_alpha/4)^2, epsilon = my_alpha, tau=tau)
  
 my_points = foreach(i=1:n_vert, .combine=rbind, 
          .export = c("runif_disk", "euclid_dists_point_cloud_2D"))%dopar%{
     
  #for(i in 1:n_vert){
    new_points = runif_disk(m, my_alpha/4)+rep(point_cloud[i,], each=m)
    keep_pts = matrix(NA, nrow=0, ncol=2)
     for (j in 1:m){
       dist_list = euclid_dists_point_cloud_2D(new_points[j,], point_cloud)
       dist_near = dist_list[dist_list < 2*my_alpha] 
       knn = length(dist_near) 
       if (knn >= N){
        keep_pts = rbind(keep_pts, new_points[j,])
      } else if (knn > k_min) {   
        a_prob = 1-exp(-(knn-k_min)*2/N)
        if (runif(1)<a_prob){
          keep_pts = rbind(keep_pts, new_points[j,])
        }
     }
     }
    keep_pts
          }
  if(dim(my_points)[1]<3){
    stop("Not enough points accepted in MCMC walk to make a shape. Need at least 3.")
  }
  new_ashape <- alphahull::ashape(my_points, alpha=my_alpha)
  return(new_ashape)
}

#' Check Bounds 2D
#' 
#' Checks the bound parameters for a sampled point during the MCMC algorithm
#'
#' @param point 1 x 2 vector or matrix holding point being checked
#' @param bounds bounds list from mcmc function above
#'
#' @return boolean TRUE/FALSE
check_bound2d <- function(point, bounds){
  if (bounds$bound == "square"){
    if(point[1]<bounds$limits[2] & point[1]>bounds$limits[1] & 
       point[2]<bounds$limits[4] & point[2]> bounds$limits[3]){
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else if (bounds$bound == "circle"){
    r_temp = sqrt(point[1]^2 + point[2]^2)
    if(r_temp < bounds$limits[1]){
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    r_temp = sqrt(point[1]^2 + point[2]^2)
    if(r_temp < bounds$limits[1] & r_temp > bounds$limits[2]){
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

#' Check Bounds 3D
#' 
#' Checks the bound parameters for a sampled point during the MCMC algorithm
#'
#' @param point 1 x 3 vector or matrix of the point being checked
#' @param bounds bounds list from above mcmc sampler
#'
#' @return boolean TRUE/FALSE
check_bound3d <- function(point, bounds){
  if (bounds$bound == "cube"){
    if(point[1]<bounds$limits[2] & point[1]>bounds$limits[1] & 
       point[2]<bounds$limits[4] & point[2]> bounds$limits[3] & 
       point[3]>bounds$limits[5] & point[3]<bounds$limits[6]){
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else if (bounds$bound == "sphere"){
    r_temp = sqrt(point[1]^2 + point[2]^2 + point[3]^2)
    if(r_temp < bounds$limits[1]){
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    r_temp = sqrt(point[1]^2 + point[2]^2 + point[3]^2)
    if(r_temp < bounds$limits[1] & r_temp > bounds$limits[2]){
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}