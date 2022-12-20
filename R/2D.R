#' Circle Overlap Centered on Circumference
#'
#' Circle overlap cc is subfunction for repeated code in calc_overlap_2D
#' Returns the area of two overlapping circles where one is centered on the other's
#' Circumference. (cc = centered on circumference )
#'
#' @param alpha radius 1
#' @param r radius 2
#'
#' @return area of overlap
circle_overlap_cc <- function(alpha, r = 1) {
  theta_1 = 2 * (acos(alpha ^ 2 / (2 * r * alpha)))
  theta_2 = 2 * (acos((2 * r ^ 2 - alpha ^ 2) / (2 * r ^ 2)))
  a_1 = alpha^2 * 0.5 * (theta_1 - sin(theta_1))
  a_2 = 0
  if (alpha < (r*sqrt(2))){
    a_2 = r^2 * 0.5 * (theta_2 - sin(theta_2))
  } else {
    a_2 = r^2 * 0.5 * (theta_2 + sin((2 * pi - theta_2)))
  }
  return(a_1 + a_2)
}

#' Circle Overlap Inner Annulus
#'
#' Circle overlap ia (inner annulus) calculates area needed to subtract 
#' when calculating area of overlap of annulus and circle.
#'
#' @param alpha radius of circle
#' @param R outer radius of annulus
#' @param r inner radius of annulus
#'
#' @return area of overlap
circle_overlap_ia <- function(alpha, R, r){
  if (R+r<=alpha || r+alpha <= R || alpha+R <= r){
    stop("Impossible triangle, check alpha, R, and r.")
  }
  theta_1 = 2*acos((alpha^2+R^2-r^2)/(2*alpha*R))
  theta_2 = 2*acos((R^2+r^2-alpha^2)/(2*r*R))
  a_1 = alpha^2*0.5*(theta_1 - sin(theta_1))
  a_2 = 0
  if (theta_2< pi){
    a_2 = r^2*0.5*(theta_2 - sin(theta_2))
  } else {
    a_2 = r^2*0.5*(theta_2 + sin((2*pi-theta_2)))
  }
  return(a_1 + a_2)
}

#' Calculate Overlap 2D
#'
#' This function calculates the minimum coverage percentage of an alpha ball over the bounded
#' area being considered. 0 is no coverage, 1 means complete coverage.
#' For the square, r is the length of the side. For circle, r is the radius. For 
#' the annulus, r and min_r are the two radii.
#' @param alpha radius of alpha ball
#' @param r length of square, radius of circle, or outer radius of annulus
#' @param rmin inner radius of annulus
#' @param bound manifold shape, options are "square", "circle", or "annulus"
#'
#' @return area of overlap
#' @export
calc_overlap_2D <- function(alpha, r=1, rmin=0.01, bound="square"){
  if (alpha <=0 || r<=0 || rmin<=0){
    stop("Negative numbers and 0 not valid.")
  }
  if(rmin>=r){
    stop("rmin must be less than r")
  }
  bound = tolower(bound) #in case of capitalization
  alpha=2*alpha     #Why is this here? If there is a point within 2*alpha, then there will be a circle of radius alpha that can connect the two points
  if (bound=="square"){
    if (alpha< r){
      return((pi*alpha^2/4)/r^2)
    } else if (alpha < sqrt(2)*r){
      theta = acos(r/alpha)
      a = 0.5*(theta - sin(theta))*alpha^2
      return(((pi*alpha^2/4)-a)/r^2)
    } else {
      return(1) #scenario where ball covers area no matter what.
    }
  } else if (bound == "circle") {
    if (alpha < 2*r){
      area = circle_overlap_cc(alpha, r)
      return(area/(pi*r^2))
    } else {
      return(1)
    }
  } else if (bound=="annulus"){
    if(rmin>=r){
      stop("rmin must be less than r")
    }
    #Four scenarios: alpha less than r-rmin, alpha bigger but not r, bigger than r, bigger than R
    if (alpha <= r - rmin){
      area = circle_overlap_cc(alpha, r)
      return(area/(pi*(r^2-rmin^2)))
    } else if (alpha < r+rmin) {
      area = circle_overlap_cc(alpha, r)-circle_overlap_ia(alpha, R=r, r=rmin)
      return(area/(pi*(r^2-rmin^2)))
    } else if (alpha < 2*r){
      area = circle_overlap_cc(alpha, r)-(pi*rmin^2)
      return(area/(pi*(r^2-rmin^2)))
    } else {
      return(1)
    }
  } else {
    stop("Not a valid bound for the manifold. Please enter 'square', 'circle', or 
          'annulus'. Default is 'square'.")
  }
}

#' n Bound Connect 2D
#'
#' This is the bound for connectivity based on samples.
#'
#' @param alpha alpha parameter for alpha shape
#' @param delta probability of isolated point
#' @param r length of square, radius of circle, or outer radius of annulus
#' @param rmin inner radius of annulus
#' @param bound manifold shape, options are "square", "circle", or "annulus"
#'
#' @return minimum number of points to meet probability threshold.
#' @export
n_bound_connect_2D <- function(alpha, delta=0.05, r=1, rmin=0.01, bound="square"){
  if (alpha <=0 || r<= 0 || rmin <= 0){
    stop("Cannot have a negative or 0 value for alpha, r, or rmin.")
  }
  if (delta <=0 || delta >= 1){
    stop("Invalid number for delta; must be a value greater than 0 and less than 1.")
  }
  min_ball = calc_overlap_2D(alpha, r, rmin=rmin, bound=bound)
  temp = 1-min_ball
  temp = log(delta)/log(temp)
  return(1 + ceiling(temp))
}

#from Niyogi et al 2008
#' n Bound Homology 2D
#' 
#' #' Function returns the minimum number of points to preserve the homology with 
#' an open cover of radius alpha.
#'
#' @param area area of manifold from which points being sampled
#' @param epsilon size of balls of cover
#' @param tau number bound 
#' @param delta probability of not recovering homology
#'
#' @return n, number of points needed
#' @export
n_bound_homology_2D <- function(area, epsilon, tau=1, delta=0.05){
  if (epsilon <=0 || area <= 0 || tau <= 0){
    stop("Cannot have a negative or 0 value for epsilon, area, or tau")
  }
  if (delta <=0 || delta >= 1){
    stop("Invalid number for delta; must be a value greater than 0 and less than 1.")
  }
  if (2*epsilon >= tau){
    stop("Must have epsilon/2 < tau.")
  }
  theta_1 = asin(epsilon/(8*tau))
  theta_2 = asin(epsilon/(16*tau))
  beta_1 = area/(cos(theta_1)^2 * (pi*(epsilon/4)^2))
  beta_2 = area/(cos(theta_2)^2 * (pi*(epsilon/8)^2))
  n_bound = beta_1*(log(beta_2)+log(1/delta))
  return(ceiling(n_bound))
}

#' Euclidean Distance Point Cloud 2D
#'
#' Calculates the distance matrix of a point from the point cloud.
#'
#' @param point cartesian coordinates of 2D point
#' @param point_cloud 3 column matrix with cartesian coordinates of 2D point cloud
#'
#' @return vector of distances from the point to each point in the point cloud
#' @export
euclid_dists_point_cloud_2D <- function(point, point_cloud){
  if(sum(is.na(point))>0 || sum(is.na(point_cloud))>0){
    stop("NA values in input.")
  }
  if(length(point) !=2 || dim(point_cloud)[2] !=2){
    stop("Dimensions of input incorrect.")
  }
  m = nrow(point_cloud)
  if (m==0){
    stop("Empty point cloud")
  }
  dist_vec = vector("numeric",m)
  for (j in 1:m){
    sqr_dist = (point[1]-point_cloud[j,1])^2+(point[2]-point_cloud[j,2])^2
    dist_vec[j] = sqrt(sqr_dist)
  }
  return(dist_vec)
}

#' Uniform Sampling from Square
#'
#' Returns points uniformly sampled from square or rectangle in plane.
#'
#' @param n number of points
#' @param xmin minimum x coordinate
#' @param xmax maximum x coordinate
#' @param ymin minimum y coordinate
#' @param ymax maximum y coordinate
#'
#' @return n by 2 matrix of points
#' @export
#'
#' @examples
#' # Sample 100 points from unit square
#' runif_square(100)
#' # Sample 100 points from unit square centered at origin
#' runif_square(100, 0.5, 0.5, 0.5, 0.5)
runif_square <- function(n, xmin=0, xmax=1, ymin=0, ymax=1){
  if(xmax<xmin || ymax<ymin){
    stop("Invalid bounds")
  }
  if(n<=0 || floor(n)!=n){
    stop("n must be positive integer.")
  }
  points = matrix(data=NA, nrow=n, ncol=2)
  points[,1] = runif(n, min=xmin, max=xmax)
  points[,2] = runif(n, min=ymin, max=ymax)
  return(points)
}

#' Uniform sampling from disk
#' 
#' Returns points uniformly sampled from disk of radius r in plane
#'
#' @param n number of points to sample
#' @param r radius of disk
#'
#' @return n by 2 matrix of points sampled
#' @export
#'
#' @examples
#' # Sample 100 points from unit disk
#' runif_disk(100)
#' # Sample 100 points from disk of radius 0.7
#' runif_disk(100, 0.7)
runif_disk <- function(n, r=1){
  if(n<=0 || floor(n) !=n || r<=0){
    stop("n must be positive integer, and r must be a positive real number.")
  }
  points = matrix(data=NA, nrow=n, ncol=2)
  radius = runif(n, min=0, max=r^2)
  theta = runif(n, min=0, max=2*pi)
  points[,1] = sqrt(radius)*cos(theta)
  points[,2] = sqrt(radius)*sin(theta)
  return(points)
}

#' Uniform Sampling from Annulus
#' 
#' Returns points uniformly sampled from annulus in plane
#'
#' @param n number of points to sample
#' @param rmax radius of outer circle of annulus
#' @param rmin raidus of inner circle of annulus
#'
#' @return n by 2 matrix of points sampled
#' @export
#'
#' @examples
#' # Sample 100 points from annulus with rmax=1 and rmin=0.5
#' runif_annulus(100)
#' # Sample 100 points from annulus with rmax=0.75 and rmin=0.25
#' runif_annulus(100, 0.75, 0.25)
runif_annulus <- function(n, rmax=1, rmin=0.5){
  if(n<=0 || floor(n) !=n || rmax<=0 || rmin<=0){
    stop("n must be positive integer, and rmax, rmin must be a positive real numbers.")
  }
  if(rmin >= rmax){
    stop("Invalid values for rmin and rmax.")
  }
  points = matrix(data=NA, nrow =n, ncol=2)
  radius = runif(n, min=rmin^2, max=rmax^2)
  theta = runif(n, min=0, max=2*pi)
  points[,1] = sqrt(radius)*cos(theta)
  points[,2] = sqrt(radius)*sin(theta)
  return(points)
}
