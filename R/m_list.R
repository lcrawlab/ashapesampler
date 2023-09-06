#' Microcebus Teeth as objects for ashapesampler
#' 
#' This data set is of 8 of Microcebus mandibular molars characterized as vertices
#' and the resulting simplicial complex, as used in
#' Winn-Nunez, et. al. (2023). These were imported from off files with 
#' function readOFF from the ashapesampler package.
#'
#' @format
#' A named list containing Microcebus molars as mesh3d objects of the Rvcg package 
#' \describe{
#'   \item{Vertices}{3 x n matrix containing n vertices as Euclidean coordinates}
#'   \item{cmplx}{list of vertices, edges, and faces forming simplicial complex}
#' }
#' @source <https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/K9A0EG&faces-redirect=true>
"m_list"