#' Spherical cap
#'
#' Calculates the volume of a sphere cap given radius r and height of cap h
#' @param r  radius
#' @param h  height of cap
#'
#' @return v_c volume of spherical cap
spherical_cap <- function(r, h){
  v_c = (1/3)*pi*h^2*(3*r-h)
  return(v_c)
}

#' Intersection of spheres
#'
#' Called for sphere overlaps with alpha > r*sqrt(2). Integral precalculated and  
#' numbers plugged in.
#' 
#' @param alpha radius 1
#' @param r  radius 2
#'
#' @return volume of intersection of spheres.
cap_intersect_vol <- function(alpha, r){
  a = sqrt(alpha^2-r^2)
  b = sqrt(a^2 - r^2)
  a_vol = (1/6)*(2*b*a*sqrt(alpha^2-b^2-a^2) 
                 + 3*alpha*b^2*atan((b*a)/(alpha*sqrt(alpha^2-b^2-a^2))))
  - a*(-3*alpha^2 + 3*b^2 + a^2)*atan(b/sqrt(alpha^2-b^2-a^2))
  + b*(3*alpha^2-4*b^2)*atan(a/sqrt(alpha^2-b^2-a^2))
  - 2*alpha^3*atan((b*a)/(alpha*sqrt(alpha^2-b^2-a^2)))
  r_vol = (1/6)*(2*b*r*sqrt(alpha^2-b^2-r^2) 
                 + 3*alpha*b^2*atan((b*r)/(alpha*sqrt(alpha^2-b^2-r^2))))
  - r*(-3*alpha^2 + 3*b^2 + r^2)*atan(b/sqrt(alpha^2-b^2-r^2))
  + b*(3*alpha^2-4*b^2)*atan(r/sqrt(alpha^2-b^2-r^2))
  - 2*alpha^3*atan((b*r)/(alpha*sqrt(alpha^2-b^2-r^2)))
  return(a_vol-r_vol)
}

#' sphere overlap when one is centered on circumference of the other
#'
#' Sphere overlap cs is subfunction for repeated code in calc_overlap_3D
#' Returns the area of two overlapping spheres where one is centered on the 
#' other's surface (cs = centered on surface) 
#'
#' @param alpha radius 1
#' @param r radius 2
#'
#' @return volume of intersection
sphere_overlap_cs <- function(alpha, r){
  theta_1 = acos((alpha^2)/(2*r*alpha))
  h2 = alpha*cos(theta_1)
  h1 = alpha-h2
  V1 = (1/3)*pi*h1^2*(3*alpha-h1)
  V2 = 0
  if (h2 <= r){
    V2 = (1/3)*pi*h2^2*(3*r-h2)
  } else {
    h2 = 2*r-h2
    V2 = (4/3)*pi*r^3 - (1/3)*pi*h2^2*(3*r-h2)
  }
  return(V1+V2)
}

#' sphere overlap inner shell
#'
#' Sphere overlap is (inner shell) calculates area needed to subtract 
#' when calculating volume of overlap of shell and sphere.
#'
#' @param alpha radius of sphere
#' @param rmax  outer radius of shell
#' @param rmin  inner radius of shell
#'
#' @return volume of intersection
sphere_overlap_is <- function(alpha, rmax, rmin){
  if(rmax+rmin<=alpha || rmin+alpha <= rmax || alpha+rmax <= rmin){
    stop("Impossible values, try again.")
  }
  theta_1 = acos((alpha^2+rmax^2-rmin^2)/(2*alpha*rmax))
  h2 = alpha*cos(theta_1)-(rmax-rmin)
  h1 = alpha-h2-(rmax-rmin)
  V1 = (1/3)*pi*h1^2*(3*alpha-h1)
  V2 = 0
  if (h2 <= rmin){
    V2 = (1/3)*pi*h2^2*(3*rmin-h2)
  } else {
    h2 = 2*rmin-h2
    V2 = (4/3)*pi*rmin^3 - (1/3)*pi*h2^2*(3*rmin-h2)
  }
  return(V1+V2)
}


#' calculate overlap in three dimensions (calc_overlap_3D)
#'
#' Calculates the volume of intersection divided by the volume of the manifold.
#' For the cube, r is the length of the side. For sphere, r is the radius. For 
#' the annulus, r and min_r are the two radii.
#' 
#' @param alpha radius of one sphere
#' @param r  radius of second sphere or outer radius of shell or length of
#'              cube side
#' @param rmin  inner radius of shell, only needed if bound=shell
#' @param bound manifold type, options are "cube", "shell", and "sphere"
#'
#' @return volume of overlap 
#' @export
#'
calc_overlap_3D <- function(alpha, r=1, rmin = 0.01, bound = "cube"){
  if (alpha <=0 || r<=0 || rmin<=0){
    stop("Negative numbers and 0 not valid.")
  }
  if(rmin>=r){
    stop("rmin must be less than r")
  }
  bound=tolower(bound)
  alpha=2*alpha     #Why is this here? If there is a point within 2*alpha, then there will be a circle of radius alpha that can connect the two points
  if(bound == "cube"){
    if (alpha < r){
      vol = (pi/6)*alpha^3
      return(vol/r^3)
    }else if(alpha < r*sqrt(2)){
      vol = pi/6*(alpha)^3
      caps = (3/4)*spherical_cap(alpha, alpha-r)
      return((vol-caps)/r^3)
    }else if(alpha < r*sqrt(3)){
      vol = pi/6*(alpha)^3
      caps = (3/4)*spherical_cap(alpha, alpha-r)
      cap_intersect = 3*cap_intersect_vol(alpha, r)
      total = vol-caps+cap_intersect
      return(total/r^3)
    }else{
      return(1)
    }
  } else if (bound == "sphere") {
    if(alpha>2*r){
      return(1)
    } else {
      vol = sphere_overlap_cs(alpha, r)
      return(vol/((4/3)*pi*r^3))
    }
  } else if (bound == "shell") {
    total_vol = (4/3)*pi*(r^3-rmin^3)
    if(alpha<r-rmin){
      vol = sphere_overlap_cs(alpha, r)
      return(vol/total_vol)
    } else if (alpha < r + rmin){
      vol = sphere_overlap_cs(alpha, r) - sphere_overlap_is(alpha, rmax=r, rmin=rmin)
      return(vol/total_vol)
    } else if (alpha < 2*r){
      vol = sphere_overlap_cs(alpha, r) - (4/3)*pi*rmin^3
      return(vol/total_vol)
    } else {
      return(1)
    }
  } else {
    stop("Not a valid bound for the manifold. Please enter 'cube', 'sphere', or
          'shell'. Default is 'cube'.")
  }
}

#' N Bound Connect 3D
#'
#' Function returns the minimum number of points to preserve the homology with 
#' an open cover of radius alpha.
#'
#' @param alpha radius of open balls around points
#' @param delta probability of isolated point
#' @param r radius of sphere, outer radius of shell, or length of cube side
#' @param rmin inner radius of shell 
#' @param bound manifold from which points sampled. Options are sphere, shell, cube
#'
#' @return integer of minimum number of points needed
#'
#' @examples 
#' # For a cube with probability 0.05 of isolated points
#' n_bound_connect_3D(0.2, 0.05,0.9)
#' # For a sphere with probability 0.01 of isolated points
#' n_bound_connect_3D(0.2, 0.01, 1, bound="sphere")
#' # For a shell with probability 0.1 isolated points.
#' n_bound_connect_3D(0.2, 0,1, 1, 0.25, bound="shell")
#' @export
n_bound_connect_3D <- function(alpha, delta=0.05, r=1, rmin=0.01, bound="cube"){
  if (alpha <=0 || r<= 0 || rmin < 0){
    stop("Cannot have a negative or 0 value for alpha, r, or rmin.")
  }
  if (delta <=0 || delta >= 1){
    stop("Invalid number for delta; must be a value greater than 0 and less than 1.")
  }
  min_ball = calc_overlap_3D(alpha, r, rmin, bound)
  temp = 1-min_ball
  temp = log(delta)/log(temp)
  return(1 + ceiling(temp))
}

#' n Bound Homology 3D
#'
#' Calculates number of points needed to be samped from manifold for open 
#' ball cover to have same homology as original manifold. See Niyogi et al 2008
#'
#' @param volume volume of manifold from which points being sampled
#' @param epsilon size of balls of cover
#' @param tau number bound
#' @param delta probability of not recovering homology
#'
#' @return n, number of points needed
#' @export
n_bound_homology_3D <- function(volume, epsilon, tau=1, delta=0.05){
  if (epsilon <=0 || volume <= 0 || tau <= 0){
    stop("Cannot have a negative or 0 value for epsilon, volume, or tau")
  }
  if (delta <=0 || delta >= 1){
    stop("Invalid number for delta; must be a value greater than 0 and less than 1.")
  }
  if (2*epsilon >= tau){
    stop("Must have epsilon/2 < tau.")
  }
  theta_1 = asin(epsilon/(8*tau))
  theta_2 = asin(epsilon/(16*tau))
  beta_1 = volume/(cos(theta_1)^3 * ((4/3)*pi*(epsilon/4)^3))
  beta_2 = volume/(cos(theta_2)^3 * ((4/3)*pi*(epsilon/8)^3))
  n_bound = beta_1*(log(beta_2)+log(1/delta))
  return(ceiling(n_bound))
}

#' Euclidean Distance Point Cloud 3D
#'
#' Calculates the distance matrix of a point from the point cloud.
#'
#' @param point cartesian coordinates of 3D point
#' @param point_cloud 3 column matrix with cartesian coordinates of 3D point cloud
#'
#' @return vector of distances from the point to each point in the point cloud
#' @export
#'
euclid_dists_point_cloud_3D <- function(point, point_cloud){
  if(sum(is.na(point))>0 || sum(is.na(point_cloud))>0){
    stop("NA values in input.")
  }
  if(length(point) !=3 || dim(point_cloud)[2] !=3){
    stop("Dimensions of input incorrect.")
  }
  m = nrow(point_cloud)
  if (m==0){
    stop("Empty point cloud")
  }
  dist_vec = vector("numeric",m)
  for (j in 1:m){
    sqr_dist = (point[1]-point_cloud[j,1])^2+(point[2]-point_cloud[j,2])^2+(point[3]-point_cloud[j,3])^2
    dist_vec[j] = sqrt(sqr_dist)
  }
  return(dist_vec)
}


#' r Uniform Cube
#' 
#' Returns points uniformly sampled from cube or rectangular prism in space.
#'
#' @param n number of points to be sampled
#' @param xmin miniumum x coordinate
#' @param xmax maximum x coordinate
#' @param ymin minimum y coordinate
#' @param ymax maximum y coordinate
#' @param zmin minimum z coordinate
#' @param zmax maximum z coordinate
#'
#' @return n by 3 matrix of points
#'
#' @examples
#' # Sample 100 points from unit cube
#' runif_cube(100)
#' # Sample 100 points from unit cube centered on origin
#' runif_cube(100, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)
#' @export
runif_cube <- function(n, xmin=0, xmax=1, ymin=0, ymax=1, zmin=0, zmax = 1){
  if(xmax<xmin || ymax<ymin || zmax < zmin){
    stop("Invalid bounds")
  }
  if(n<=0 || floor(n)!=n){
    stop("n must be positive integer.")
  }
  points = matrix(data=NA, nrow=n, ncol=3)
  points[,1] = stats::runif(n, min=xmin, max=xmax)
  points[,2] = stats::runif(n, min=ymin, max=ymax)
  points[,3] = stats::runif(n, min=zmin, max=zmax)
  return(points)
}

#' Uniform Ball 3D
#'
#' Returns points uniformly centered from closed ball of radius r in 3D space
#'
#' @param n number of points
#' @param r radius of ball, default r=1
#'
#' @return n by 3 matrix of points
#'
#' @examples
#' # Sample 100 points from unit ball
#' runif_ball_3D(100)
#' # Sample 100 points from ball of radius 0.5
#' runif_ball_3D(100, r=0.5)
#' @export
runif_ball_3D <- function(n, r=1){
  if(n<=0 || floor(n) !=n || r<=0){
    stop("n must be positive integer, and r must be a positive real number.")
  }
  points = matrix(data=NA, nrow =n, ncol=3)
  radius = stats::runif(n, min=0, max=2*r^3)
  theta = stats::runif(n, min=0, max=2*pi) #horizontal angle from x axis
  phi = acos(1 - 2*runif(n,0,1))   #vertical angle from z axis
  points[,1] = pracma::nthroot(radius/2,3)*cos(theta)*sin(phi)
  points[,2] = pracma::nthroot(radius/2,3)*sin(theta)*sin(phi)
  points[,3] = pracma::nthroot(radius/2,3)*cos(phi)
  return(points)
}

#' Uniform Shell 3D
#'
#' Returns points uniformly sampled from spherical shell in 3D
#'
#' @param n number of points
#' @param rmax radius of outer sphere
#' @param rmin radius of inner sphere
#'
#' @return n by 3 matrix of points
#'
#' @examples
#' # Sample 100 points with defaults rmax=1, rmin=0.5
#' runif_shell_3D(100)
#' # Sample 100 points with rmax=0.75, rmin=0.25
#' runif_shell_3D(100, 0.75, 0.25)
#' @export
runif_shell_3D <- function(n, rmax=1, rmin=0.5){
  if(n<=0 || floor(n) !=n || rmax<=0 || rmin<=0){
    stop("n must be positive integer, and rmax, rmin must be a positive real numbers.")
  }
  if(rmin >= rmax){
    stop("Invalid values for rmin and rmax.")
  }
  points = matrix(data=NA, nrow =n, ncol=3)
  radius = stats::runif(n, min=2*rmin^3, max=2*rmax^3)
  theta = stats::runif(n, min=0, max=2*pi) #horizontal angle from x axis
  phi = acos(1 - 2*runif(n,0,1))  #vertical angle from z axis
  points[,1] = pracma::nthroot(radius/2,3)*cos(theta)*sin(phi)
  points[,2] = pracma::nthroot(radius/2,3)*sin(theta)*sin(phi)
  points[,3] = pracma::nthroot(radius/2,3)*cos(phi)
  return(points)
}
