#mcmc sampling

generate_ashape3d <- function(point_cloud, N, tau, delta, bound="sphere"){
  #Check: 3 columns on vertex list, 
  n_vert = dim(point_cloud)[1]
  
  #Get volume, number of points needed
  #Sample alpha, based on posterior distribution?
  my_alpha <- rtruncnorm(1,a=0,b=tau/2, mean=mu,sd=sig)
  cube_vol <- (xmax-xmin)*(ymax-ymin)*(zmax-zmin)
  n <- n_bound_homology_3D(volume=cube_vol, tau=tau, epsilon=my_alpha, delta=0.01)
  
  #Sample and reject points
  my_points = matrix(NA, nrow=0, ncol=3)
  #Initialize by taking point from point cloud.
  temp = rdunif(1, dim(point_cloud)[1])
  curr_point = point_cloud[temp, ]

  new_point = runif_ball_3D(1,tau)+curr_point
  
  if (new_point[1]<xmax & new_point[1]>xmin & new_point[2]<ymax &
      new_point[2]>ymin & new_point[3]>zmin & new_point[3]<zmax){
    dist_list = euclid_dists_point_cloud_3D(new_point, point_cloud)
    dist_near = dist_list[dist_list < 2*my_alpha ] #Really this is fraction of what tau should be
    knn = length(dist_near) 
    if (knn >= N){
      my_points = rbind(my_points, new_point)
      curr_point=new_point
    } else if (knn > 3) {                         #How many points in range
      a_prob = 1-exp(-knn*2/N)
      if (runif(1)<a_prob){
        my_points = rbind(my_points, new_point)
        curr_point=new_point
      }
    }
  }
  
  #return(my_points)
}