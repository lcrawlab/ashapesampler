#mcmc sampling

#' Generate 3D alpha shape
#'
#' @param point_cloud 3 column matrix of all points from all shapes in initial 
#'                    data set
#' @param N number of shapes in initial data set
#' @param tau tau bound
#' @param delta probability of not preserving homology; default is 0.05
#' @param bound manifold bound being used to sample points
#' @param mu mean of truncated distribution from which alpha sampled; default tau/3
#' @param sig standard deviation of truncated distribution from which alpha 
#'              sampled; default tau/12
#'
#' @return new_ashape three dimensional alpha shape object from alphashape3d library
#' @export
#'
generate_ashape3d <- function(point_cloud, N, tau, delta=0.05, bound="sphere", 
                              mu=NULL, sig = NULL){
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
  bound = tolower(bound)
  #Sample alpha
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
  my_alpha <- truncnorm::rtruncnorm(1,a=0,b=tau/2, mean=mu,sd=sig)
  
  #Get volume, number of points needed, bounds for sampling
  vol=0; xmin=0; xmax=0; ymin=0; ymax=0; zmin=0; zmax=0; rmax=0; rmin=0
  bounds=list()
  if(bound=="cube"){
    xmin = min(point_cloud[,1]-tau)
    xmax = max(point_cloud[,1]+tau)
    ymin = min(point_cloud[,2]-tau)
    ymax = max(point_cloud[,2]+tau)
    zmin = min(point_cloud[,3]-tau)
    zmax = max(point_cloud[,3]+tau)
    bounds <- list("bound"=bound, "limits"=c(xmin, xmax, ymin, ymax, zmin, zmax))
    vol <- (xmax-xmin)*(ymax-ymin)*(zmax-zmin)
  } else if (bound=="sphere"){
    radii = sqrt(point_cloud[,1]^2 + point_cloud[,2]^2 + point_cloud[,3]^2)
    rmax = max(radii)+tau
    bounds <- list("bound"=bound, "limits" = rmax)
    vol <- (4/3)*pi*rmax^3
  } else if (bound == "shell"){
    radii = sqrt(point_cloud[,1]^2 + point_cloud[,2]^2 + point_cloud[,3]^2)
    rmax = max(radii)+tau
    rmin = max(min(radii-tau),0)
    bounds <- list("bound"=bound, "limits" = c(rmax, rmin))
    vol <- (4/3)*pi*(rmax^3 - rmin^3)
  } else {
    stop("Not a valid bound. Please enter bound = sphere, shell, or cube. Default is sphere.")
  }
  
  n <- n_bound_homology_3D(volume=vol, tau=tau, epsilon=my_alpha, delta=delta)
  
  #Sample and reject points
  my_points = matrix(NA, nrow=0, ncol=3)
  #Initialize by taking point from point cloud.
  temp = purrr::rdunif(1, n_vert)
  curr_point = point_cloud[temp, ]

  for (i in 1:n){
    new_point = runif_ball_3D(1,tau)+curr_point
    if (check_bound3d(new_point, bounds)){
      dist_list = euclid_dists_point_cloud_3D(new_point, point_cloud)
      dist_near = dist_list[dist_list < 2*my_alpha ] 
      knn = length(dist_near) 
      if (knn >= N){
        my_points = rbind(my_points, new_point)
        curr_point=new_point
      } else if (knn > 3) {   
        a_prob = 1-exp(-knn*2/N)
        if (runif(1)<a_prob){
          my_points = rbind(my_points, new_point)
          curr_point=new_point
        }
      }
    }
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
#' @param bound manifold bound being used to sample points
#' @param mu mean of truncated distribution from which alpha sampled; default tau/3
#' @param sig standard deviation of truncated distribution from which alpha 
#'              sampled; default tau/12
#'
#' @return new_ashape two dimensional alpha shape object from alphahull library
#' @export
#'
generate_ashape2d <- function(point_cloud, N, tau, delta=0.05, bound="circle", 
                              mu=NULL, sig = NULL){
  #Check: 3 columns on vertex list
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
  bound = tolower(bound)
  #Sample alpha
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
  my_alpha <- truncnorm::rtruncnorm(1,a=0,b=tau/2, mean=mu,sd=sig)
  
  #Get volume, number of points needed, bounds for sampling
  area=0; xmin=0; xmax=0; ymin=0; ymax=0; rmax=0; rmin=0
  bounds=list()
  if(bound=="square"){
    xmin = min(point_cloud[,1]-tau)
    xmax = max(point_cloud[,1]+tau)
    ymin = min(point_cloud[,2]-tau)
    ymax = max(point_cloud[,2]+tau)
    bounds <- list("bound"=bound, "limits"=c(xmin, xmax, ymin, ymax))
    area <- (xmax-xmin)*(ymax-ymin)
  } else if (bound=="circle"){
    radii = sqrt(point_cloud[,1]^2 + point_cloud[,2]^2)
    rmax = max(radii)+tau
    bounds <- list("bound"=bound, "limits" = rmax)
    area <- pi*rmax^2
  } else if (bound == "annulus"){
    radii = sqrt(point_cloud[,1]^2 + point_cloud[,2]^2)
    rmax = max(radii)+tau
    rmin = max(min(radii-tau),0)
    bounds <- list("bound"=bound, "limits" = c(rmax, rmin))
    area <- pi*(rmax^2 - rmin^2)
  } else {
    stop("Not a valid bound. Please enter bound = circle, square, or annulus. Default is circle.")
  }
  
  n <- n_bound_homology_2D(area=area, tau=tau, epsilon=my_alpha, delta=delta)
  
  #Sample and reject points
  my_points = matrix(NA, nrow=0, ncol=2)
  #Initialize by taking point from point cloud.
  temp = purrr::rdunif(1, n_vert)
  curr_point = point_cloud[temp, ]
  
  for (i in 1:n){
    new_point = runif_disk(1,tau)+curr_point
    if (check_bound2d(new_point, bounds)){
      dist_list = euclid_dists_point_cloud_2D(new_point, point_cloud)
      dist_near = dist_list[dist_list < 2*my_alpha ] 
      knn = length(dist_near) 
      if (knn >= N){
        my_points = rbind(my_points, new_point)
        curr_point=new_point
      } else if (knn > 3) {   
        a_prob = 1-exp(-knn*2/N)
        if (runif(1)<a_prob){
          my_points = rbind(my_points, new_point)
          curr_point=new_point
        }
      }
    }
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