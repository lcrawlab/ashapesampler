#' Tarsius Teeth as mesh3d objects
#' 
#' This data set is of 7 mesh3d representations of Tarsius mandibular molars
#' use in Winn-Nunez, et. al. (2023). These were imported from off files using 
#' the vcgImport function from package Rvcg.
#' See <https://cran.r-project.org/web/packages/Rvcg/Rvcg.pdf>
#' for more information about the mesh3d object and the vcgImport function.
#'
#' @format
#' A named list containing Microcebus molars as mesh3d objects of the Rvcg package 
#' \describe{
#'   \item{vb}{4 x n matrix containing n vertices as homolougous coordinates}
#'   \item{it}{3 x m matrix containing vertex indices forming triangular faces}
#'   \item{normals}{4 x n matrix containing vertex normals (homologous coordinates)}
#' }
#' @source <https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/K9A0EG&faces-redirect=true>
"t_mesh"