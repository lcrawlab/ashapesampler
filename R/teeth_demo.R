#' Microcebus Teeth as objects for ashapesampler
#' 
#' This data set is of 2 of the Microcebus mandibular molars characterized as vertices
#' and the resulting simplicial complex, and as mesh3d objects, as used in
#' Winn-Nunez, et. al. (2023). These were imported from off files with 
#' function readOFF from the ashapesampler package, and via vcgImport in the Rvcg 
#' package.
#'
#' @format
#' A named list containing Microcebus molars as mesh3d objects of the Rvcg package 
#' \describe{
#'   \item{List Object 1}{First list with complex information of two teeth}
#'   \item{ToothComplex1}{List with the vertices and complex for tooth 1}
#'   \item{ToothComplex1$Vertices}{3 x n matrix containing n vertices as Euclidean coordinates of tooth 1}
#'   \item{ToothComplex1$cmplx}{list of vertices, edges, and faces forming simplicial complex of tooth 1}
#'   \item{ToothComplex2}{List with Vertices and complex for tooth 2}
#'   \item{ToothComplex2$Vertices}{3 x n matrix containing n vertices as Euclidean coordinates of tooth 2}
#'   \item{ToothComplex2$cmplx}{list of vertices, edges, and faces forming simplicial complex of tooth 2}
#'   \item{List Object 2}{Second list with two teeth as mesh3d objects}
#'   \item{ToothMesh1}{mesh3d object of tooth 1}
#'   \item{ToothMesh2}{mesh3d object of tooth 2}
#' }
#' @source <https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/K9A0EG&faces-redirect=true>
"teeth_demo"