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
#' xxx: add example and change above descsriptionand parameters
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


#' Generates a random line in 3D
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
#' xxx: add example and change above descsriptionand parameters
#'
generate_random_line <- function() {
  point <- runif(3, min = -10, max = 10)
  direction <- runif(3, min = -1, max = 1)
  direction <- direction / sqrt(sum(direction^2))  # Normalize direction vector
  list(point = point, direction = direction)
}

#' Finds the point closest to multiple lines
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
#' xxx: add example and change above descsriptionand parameters
#'
closest_point_to_lines <- function(lines) {
  require(MASS)
  n <- length(lines)
  
  # Construct the matrices A and b
  A <- matrix(0, nrow = 3, ncol = 3)
  b <- rep(0, 3)
  
  for (line in lines) {
    p <- line$point
    d <- line$direction
    
    # Project p onto the plane orthogonal to d
    P <- diag(3) - outer(d, d)
    A <- A + t(P) %*% P
    b <- b + t(P) %*% P %*% p
  }
  
  # Solve for the closest point
  closest_point <- solve(A, b)
  return(closest_point)
}

#' Translates coordinates according to a ROI
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
#' xxx: add example and change above descsriptionand parameters
#'
translate_ROIs <- function(df,
                           ROI_coordinates,
                           eye){
  # # testing
  # df = local_heights
  # # df = facet_positions_new
  # ROI_coordinates = curr_ROI_coordinates
  # eye = 2
  
  df_translated <- df %>% 
    mutate(x = x + (ROI_coordinates %>% 
                      filter(ROI == paste0("eye", eye)) %>% 
                      pull(x_coord) * px_size_eyes) -
             (ROI_coordinates %>% 
                filter(ROI == "head") %>% 
                pull(x_coord) * px_size_eyes),
           y = y + (ROI_coordinates %>% 
                      filter(ROI == paste0("eye", eye)) %>% 
                      pull(y_coord) * px_size_eyes) -
             (ROI_coordinates %>% 
                filter(ROI == "head") %>% 
                pull(y_coord) * px_size_eyes),
           z = z + (ROI_coordinates %>% 
                      filter(ROI == paste0("eye", eye)) %>% 
                      pull(z1_coord) * px_size_eyes) -
             (ROI_coordinates %>% 
                filter(ROI == "head") %>% 
                pull(z1_coord) * px_size_eyes))
  
  
  return(df_translated)
}

#' Extract ROI coordinates from crop_log file
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
#' @importFrom dplyr add_row
#'
#' @export
#' @examples
#' xxx: add example and change above descsriptionand parameters
#'
get_ROI_coordinates <- function(ROIs,
                                crop_log_data){
  results = tibble(ROI = character(),
                   x_coord = numeric(), y_coord = numeric(), 
                   x_length = numeric(), y_length = numeric(), 
                   z1_coord = numeric(), z2_coord = numeric())
  i=1
  for(i in 1:length(ROIs)){
    curr_ROI_name <- ROIs [i]
    curr_ROI <- crop_log_data$val[which(grepl(paste0("ROI_", curr_ROI_name), crop_log_data$var))]
    ROI_coords <- strsplit(gsub("\\);", "",
                                strsplit(curr_ROI, "\\(")[[1]][2]), ", ")[[1]]
    x_coord <- as.numeric(ROI_coords[1])
    y_coord <- as.numeric(ROI_coords[2])
    x_length <- as.numeric(ROI_coords[3])
    y_length <- as.numeric(ROI_coords[4])
    
    z1_coord <- as.numeric(crop_log_data$val[which(grepl(paste0("z_first_", curr_ROI_name), crop_log_data$var))])
    z2_coord <- as.numeric(crop_log_data$val[which(grepl(paste0("z_last_", curr_ROI_name), crop_log_data$var))])
    
    results = results %>%  
      add_row(ROI = curr_ROI_name,
              x_coord = x_coord, y_coord = y_coord, 
              x_length = x_length, y_length = y_length, 
              z1_coord = z1_coord, z2_coord = z2_coord)
  }
  return(results)
}