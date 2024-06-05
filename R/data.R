#' Triangle Centers and Normals
#'
#' This is an example an imported STL file.
#'
#' @format A data frame with 16,847 rows and 7 variables:
#' \describe{
#'   \item{ID}{A numeric vector of consequtive vertex IDs}
#'   \item{x}{A numeric vector of vertex x coordinates}
#'   \item{y}{A numeric vector of vertex y coordinates}
#'   \item{z}{A numeric vector of vertex z coordinates}
#'   \item{norm.x}{A numeric vector of vertex  normal x components}
#'   \item{norm.y}{A numeric vector of vertex  normal y components}
#'   \item{norm.z}{A numeric vector of vertex  normal z components}
#' }
#' @source Generated using \code{compoundvision3D::STL_triangles()}
"tri_centers_normals"

#' Local Heights
#'
#' This is an example a tibble containing information on local heights of each
#' vertex.
#'
#' @format A data frame with 16,847 rows and 8 variables:
#' \describe{
#'   \item{ID}{A numeric vector of consequtive vertex IDs}
#'   \item{x}{A numeric vector of vertex x coordinates}
#'   \item{y}{A numeric vector of vertex y coordinates}
#'   \item{z}{A numeric vector of vertex z coordinates}
#'   \item{norm.x}{A numeric vector of vertex  normal x components}
#'   \item{norm.y}{A numeric vector of vertex  normal y components}
#'   \item{norm.z}{A numeric vector of vertex  normal z components}
#'   \item{local_height}{A numeric vector of the vertex local height}
#' }
#' @source Generated using \code{compoundvision3D::get_local_heights()} on
#' \code{compoundvision3D::Drosohila_eye_tri_centers_normals}
"local_heights"
