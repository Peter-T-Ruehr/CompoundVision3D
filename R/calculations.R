#' Calculates the angle between 2 vectors
#'
#' Rotates 3D point cloud according to one defined vector so that this vector
#' is aligned to one of the global coordinate system axes.
#'
#' @param df A tibble containing coordinates in columns `x, y, z`.
#' @param line_points A 2x3 tibble containing coordinates of line to align the 
#' point cloud to. Must contain one row per point and columns `x, y, z`.
#' @param axis A character string defining the global axis to align to. Must be 
#' `x`, `y`, or `z`.
#' @return Returns a tibble with the aligned coordinates in columns `x, y, z`.
#'
#' @export
#' @examples
#' # xxx: add example and change above descsriptionand parameters
#'
# calculate angle between 2 vectors (types: d = degrees, r = radians, both = c(regrees, radians))
calc_delta.phi <- function(curr.lens, curr.partner, type){
  #message('- ', curr.lens)
  #message('p = ', curr.partner)
  curr.x_norm_lens <- curr.lens[1]
  curr.y_norm_lens <- curr.lens[2]
  curr.z_norm_lens <- curr.lens[3]
  
  curr.x_norm_partner <- curr.partner[1]
  curr.y_norm_partner <- curr.partner[2]
  curr.z_norm_partner <- curr.partner[3]
  
  curr_delta_phi.rad <- acos(((curr.x_norm_lens * curr.x_norm_partner)
                              + (curr.y_norm_lens * curr.y_norm_partner)
                              + (curr.z_norm_lens * curr.z_norm_partner))
                             /
                               (sqrt(curr.x_norm_lens**2
                                     + curr.y_norm_lens**2
                                     + curr.z_norm_lens**2) *
                                  sqrt(curr.x_norm_partner**2
                                       + curr.y_norm_partner**2
                                       + curr.z_norm_partner**2)))
  curr_delta_phi.deg <- curr_delta_phi.rad*180/pi
  # message(paste0('-> ', curr_delta_phi.deg, '?'))
  if(type == "d"){
    return <- curr_delta_phi.deg
  } else if(type == "r"){
    return <- curr_delta_phi.rad
  } else if(type == "b"){
    return <- c(curr_delta_phi.deg, curr_delta_phi.rad)
  }
}