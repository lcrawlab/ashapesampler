#GGplot code for plotting 2D alpha shape

#' Plot 2D alpha shape in ggplot2
#'
#' @param my_ashape two dimensional alpha shape
#' @param color1 (optional) color of interior, default gray
#' @param color2 (optional) color of boundary edges/points, default blue
#'
#' @return nothing; plots shape.
#' @import ggplot2
#' @export
plot_2D_ashape <- function(my_ashape, color1="gray", color2="blue"){
  X1 <- X2 <- x <- y <- id <- x1 <- y1 <- y2 <- x2 <- V1 <- V2 <- NULL #to pass checks.
  points = my_ashape$x
  my_alpha = my_ashape$alpha
  tri_keep = my_ashape$delvor.obj$tri.obj$trlist[which(my_ashape$delvor.obj$tri.obj$cclist[,3]<my_alpha), 1:3]
  dim_tri = dim(tri_keep)[1]
  tri_keep = as.vector(t(tri_keep))
  triangles = data.frame("id"=sort(rep(1:dim_tri, 3)), "x"=points[tri_keep, 1], "y"=points[tri_keep,2])
  extremes = as.data.frame(points[my_ashape$alpha.extremes,])
  
  edges = as.data.frame(my_ashape$edges[,3:6])
  
  ggplot(data.frame(points), aes(x=X1, y=X2)) +
    geom_polygon(data=triangles, aes(x=x, y=y, group=id), fill=color1) +
    geom_segment(data=edges, aes(x=x1, y=y1, xend=x2, yend=y2), color=color2)+
    geom_point(data=extremes, aes(x=V1, y=V2), size=1.5)+
    theme_classic()#+
  #xlim(c(-1,1))+
  #ylim(c(-1,1))
}