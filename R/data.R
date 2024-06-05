#' Triangle Centers and Normals
#'
#' This is an example an imported STL file.
#'
#' @format A data frame with 22,464 rows and 7 variables:
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
#' @format A data frame with 22,464 rows and 8 variables:
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

#' Landmarks
#'
#' This is an example tibble containing landmark coordinates.
#'
#' @format A data frame with 9 rows and 4 variables:
#' \describe{
#'   \item{LM}{A character vector of landmark names}
#'   \item{x}{A numeric vector of landmark x coordinates}
#'   \item{y}{A numeric vector of landmark y coordinates}
#'   \item{z}{A numeric vector of landmark z coordinates}
#' }
#' @source Generated using the software 3D SLicer.
"LMs_df"

#' Facet Positions
#'
#' This is an example tibble containing 735 facet coordinates.
#'
#' @format A data frame with 735 rows and 4 variables:
#' \describe{
#'   \item{ID}{A numeric vector of consequtive facet IDs}
#'   \item{x}{A numeric vector of facet x coordinates}
#'   \item{y}{A numeric vector of facet y coordinates}
#'   \item{z}{A numeric vector of facet z coordinates}
#' }
#' @source Generated using the software Blender and the Blender Python
#'  script `4_export_corrected_facets.py`.
"facet_positions"

#' Facet Positions Referenced
#'
#' This is an example tibble with 735 facet coordinates that where already 
#' referenced to the `local_heights` tibble.
#'
#' @format A data frame with 735 rows and 8 variables:
#' \describe{
#'   \item{ID}{A numeric vector of consequtive facet IDs}
#'   \item{x}{A numeric vector of facet x coordinates}
#'   \item{y}{A numeric vector of facet y coordinates}
#'   \item{z}{A numeric vector of facet z coordinates}
#'   \item{norm.x}{A numeric vector of vertex  facet x components}
#'   \item{norm.y}{A numeric vector of vertex  facet y components}
#'   \item{norm.z}{A numeric vector of vertex  facet z components}
#'   \item{local_height}{A numeric vector of the facet local height}
#' }
#' @source Generated using \code{compoundvision3D::reference_point_clouds()}.
"facet_positions_new"
