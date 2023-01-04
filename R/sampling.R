# sampling functions

#' Sampling 2D alpha shapes
#'
#' This function takes parameter input from user and returns list of two dimensional
#' alpha shape objects from the ahull package.
#'
#' @param N number of alpha shapes to sample
#' @param tau bound on alpha distribution
#' @param n.dependent boolean, whether the number of points n are dependent on alpha
#' @param nconnect boolean, whether user wants shapes to have one connected component 
#'                          with high probability
#' @param nhomology boolean, whether user wants shapes to preserve homology of
#'                          underlying manifold with high probability
#' @param n.noise boolean, whether to add noise variable to number of points n
#'                         for more variety in shapes
#' @param afixed boolean, whether alpha is fixed for all shapes sampled
#' @param mu mean value of truncated normal from which alpha is sampled
#' @param sigma standard deviation of truncated normal distribution from which 
#'              alpha is sampled
#' @param delta probability of getting disconnected shape or not preserving homology
#' @param n minimum number of points to be sampled for each alpha shape
#' @param alpha chosen fixed alpha; only used if afixed = TRUE
#' @param lambda parameter for adding noise to n; only used if n.noise=TRUE
#' @param r length of radius of circle, side length of square, or outer radius of annulus
#' @param rmin inner radius of annulus
#' @param bound compact manifold to be sampled from; either square, circle, or annulus
#'
#' @return list of alpha shapes of length N
#' @export
#'
sampling2Dashape <- function(N, tau=1, n.dependent=TRUE, nconnect=TRUE, 
                             nhomology=FALSE, n.noise =FALSE,
                             afixed=FALSE, mu=0.3, sigma=0.05, delta=0.05, 
                             n = 20, alpha=0.3, lambda=3, r=1, rmin=0.25, 
                             bound="square"){
  shape_list = list()
  bound = tolower(bound) 
  
  #Check for errors, warnings
  if(nconnect == TRUE & nhomology == TRUE){
    warning("Both nhomology and nconnect are true, default to nhomology for choosing n.")
  }
  if(N<=0 || floor(N)!=N){
    stop("N must be a positive integer.")
  }
  if(afixed==TRUE & alpha > tau/2){
    stop("Invalid alpha, tau values.")
  }
  if(afixed==FALSE & (mu>tau/2 || mu<0)){
    warning("Mean of alpha outside of truncated distribution range for alpha")
  }
  #Initialize variables 
  alpha_vec = rep(0,N)
  n_vec = rep(n, N)
  #Get alphas
  if (afixed==TRUE){
    alpha_vec = rep(alpha, N)
  } else {
    alpha_vec = truncnorm::rtruncnorm(n=N, a=0, b=tau/2, mean=mu, sd=sigma)
  }
  #Get n vector (get minimums first, then add noise if applicable.)
  if(n.dependent==FALSE){
    n_vec = rep(n, N)
  } else {
    if(nhomology == TRUE){
      area = get_area(r, rmin, bound)
      for(i in 1:N){
        n_vec[i] = n_bound_homology_2D(area, alpha_vec[i], tau=tau, delta=delta)
      }
    } else if (nconnect == TRUE){
      for (i in 1:N){
        n_vec[i] = n_bound_connect_2D(alpha_vec[i], delta=delta, r=r, rmin=rmin, bound=bound)
      }
    } else {
      stop("Invalid conditions; if n.dependent=TRUE, that is, n is dependent on alpha,
           then need either nhomology = TRUE or nconnect = TRUE.")
    }
  }
  
  if(n.noise == TRUE){
    n_vec = n_vec + stats::rpois(N, lambda=lambda)
  }
  
  #Enter loop for alpha shapes
  for (k in 1:N){
    #Get points
    points = matrix()
    if(bound=="square"){
      points = runif_square(n_vec[k], xmax=r, ymax=r)
    } else if (bound=="circle"){
      points = runif_disk(n_vec[k],r)
    } else {
      points = runif_annulus(n_vec[k], rmax=r, rmin=rmin)
    }
    #Get shape
    my_shape = alphahull::ashape(points, alpha=alpha_vec[k])
    shape_list = append(shape_list, list(my_shape))
  }
  return(shape_list)
} 

#' Sample 3D alpha shapes
#'
#' This function takes parameter input from user and returns list of three dimensional
#' alpha shape objects from the ahull package.
#'
#' @param N number of alpha shapes to sample
#' @param tau bound on alpha distribution
#' @param n.dependent boolean, whether the number of points n are dependent on alpha
#' @param nconnect boolean, whether user wants shapes to have one connected component 
#'                          with high probability
#' @param nhomology boolean, whether user wants shapes to preserve homology of
#'                          underlying manifold with high probability
#' @param n.noise boolean, whether to add noise variable to number of points n
#'                         for more variety in shapes
#' @param afixed boolean, whether alpha is fixed for all shapes sampled
#' @param mu mean value of truncated normal from which alpha is sampled
#' @param sigma standard deviation of truncated normal distribution from which 
#'              alpha is sampled
#' @param delta probability of getting disconnected shape or not preserving homology
#' @param n minimum number of points to be sampled for each alpha shape
#' @param alpha chosen fixed alpha; only used if afixed = TRUE
#' @param lambda parameter for adding noise to n; only used if n.noise=TRUE
#' @param r length of radius of circle, side length of square, or outer radius of annulus
#' @param rmin inner radius of annulus
#' @param bound compact manifold to be sampled from; either cube, sphere, or shell 
#'
#' @return list of alpha shapes of length N
#' @export
#'
sampling3Dashape <- function(N, tau=1, n.dependent=TRUE, nconnect=TRUE, 
                             nhomology=FALSE, n.noise =FALSE,
                             afixed=FALSE, mu=0.3, sigma=0.05, delta=0.05, 
                             n = 20, alpha=0.3, lambda=3, r=1, rmin=0.25, 
                             bound="cube"){
  shape_list = list()
  bound = tolower(bound)
  
  #Check for errors, warnings
  if(nconnect == TRUE & nhomology == TRUE){
    warning("Both nhomology and nconnect are true, default to nhomology for choosing n.")
  }
  if(N<=0 || floor(N)!=N){
    stop("N must be a positive integer.")
  }
  if(afixed==TRUE & alpha > tau/2){
    stop("Invalid alpha, tau values.")
  }
  if(afixed==FALSE & (mu>tau/2 || mu<0)){
    warning("Mean of alpha outside of truncated distribution range for alpha")
  }
  #Initialize variables 
  alpha_vec = rep(0,N)
  n_vec = rep(n, N)
  #Get alphas
  if (afixed==TRUE){
    alpha_vec = rep(alpha, N)
  } else {
    alpha_vec = truncnorm::rtruncnorm(n=N, a=0, b=tau/2, mean=mu, sd=sigma)
  }
  #Get n vector (get minimums first, then add noise if applicable.)
  if(n.dependent==FALSE){
    n_vec = rep(n, N)
  } else {
    if(nhomology == TRUE){
      volume = get_volume(r, rmin, bound)
      for(i in 1:N){
        n_vec[i] = n_bound_homology_3D(volume, alpha_vec[i], tau=tau, delta=delta)
      }
    } else if (nconnect == TRUE){
      for (i in 1:N){
        n_vec[i] = n_bound_connect_3D(alpha_vec[i], delta=delta, r=r, rmin=rmin, bound=bound)
      }
    } else {
      stop("Invalid conditions; if n.dependent=TRUE, that is, n is dependent on alpha,
           then need either nhomology = TRUE or nconnect = TRUE.")
    }
  }
  
  if(n.noise == TRUE){
    n_vec = n_vec + stats::rpois(N, lambda=lambda)
  }
  
  #Enter loop for alpha shapes
  for (k in 1:N){
    #Get points
    points = matrix()
    if(bound=="cube"){
      points = runif_cube(n_vec[k], xmax=r, ymax=r, zmax=r)
    } else if (bound=="sphere"){
      points = runif_ball_3D(n_vec[k],r)
    } else {
      points = runif_shell_3D(n_vec[k], rmax=r, rmin=rmin)
    } 
    #Get shape
    my_shape = alphashape3d::ashape3d(points, alpha=alpha_vec[k])
    shape_list = append(shape_list, list(my_shape))
  }
  return(shape_list)
}

#' Get area
#' 
#' Quickly calculate which area needed for a homology bound; here to clean up 
#' code above
#'
#' @param r side length (square) or radius (circle, annulus)
#' @param rmin radius of inner circle for annulus
#' @param bound square, circle, or annulus
#'
#' @return area, number
get_area <- function(r, rmin, bound){
  if(bound=="square"){
    return(r^2)
  } else if(bound=="circle"){
    return(pi*r^2)
  } else if (bound=="annulus"){
    return(pi*(r^2-rmin^2))
  } else {
    stop("Not a valid bound.")
  }
}

#' Get volume
#' 
#' Quickly calculate which volume needed for a homology bound; here to clean up 
#' code above
#'
#' @param r side length (cube) or radius (sphere, shell)
#' @param rmin radius of inner sphere for shell
#' @param bound cube, sphere, shell
#'
#' @return volume, number
get_volume <- function(r, rmin, bound){
  if(bound=="cube"){
    return(r^3)
  } else if(bound=="sphere"){
    return((4/3)*pi*r^3)
  } else if (bound=="shell"){
    return((4/3)*pi*(r^3-rmin^3))
  } else {
    stop("Not a valid bound.")
  }
}