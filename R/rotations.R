#' Align 3D Point Cloud to Global Axis
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
#' # xxx: add example
#'
align_to_global_axis = function(df,
                                # line_points_x_ind,
                                line_points_y_ind,
                                axis){

  line_points_y <- which(all_coords_combined$ID %in% c(ant_landmark, post_landmark))

  # # testing
  # df = all_coords_combined
  # line_points_y_ind = line_points_y
  # axis = "y"

  # select only xyz columns
  df_xyz <- df %>%
    select(x,y,z)

  # line_points are the points to which it should be rotated
  line_points_x <- df_xyz %>%
    slice(line_points_y_ind)

  # define axis to rotate
  x = line_points_x %>%
    slice(2) %>%
    unlist(., use.names=FALSE) -
    line_points_x %>%
    slice(1) %>%
    unlist(., use.names=FALSE)
  # x

  # define target (global) axis
  if(axis == "x"){
    y <- c(1,0,0)
  } else if(axis == "y"){
    y <- c(0,1,0)
  } else if(axis == "z"){
    y <- c(0,0,1)
  }
  # y

  u=x/sqrt(sum(x^2))

  v=y-sum(u*y)*u
  v=v/sqrt(sum(v^2))

  cost=sum(x*y)/sqrt(sum(x^2))/sqrt(sum(y^2))

  sint=sqrt(1-cost^2);

  R <- diag(length(x)) - u %*% t(u) - v %*% t(v) +
    cbind(u,v) %*% matrix(c(cost,-sint,sint,cost), 2) %*% t(cbind(u,v))

  df_xyz_rot <- as_tibble(as.matrix(df_xyz)%*% R) %>%
    round(., 8)

  colnames(df_xyz_rot) <- c("x", "y", "z")

  df_xyz_rot$ID <- df$ID
  df_xyz_rot <- df_xyz_rot %>%
    left_join(df %>%
                select(-c(x,y,z)), by="ID")


  # # extract single dfs
  # LMS_df_rot <- df_xyz_rot %>%
  #   filter(is.na(norm.x))
  #
  # facet_positions_translated_rot <- df_xyz_rot %>%
  #   filter(!is.na(norm.x))
  #
  # plot3d(LMS_df_rot %>%
  #          select(x, y, z),
  #        col="red", size=10, alpha = 1,
  #        aspect = "iso")
  #
  # text3d(LMS_df_rot %>%
  #          select(x, y, z),
  #        texts = LMS_df_rot$ID)

  return(df_xyz_rot)
}

#' Align 3D Point Cloud to Global Axis
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
#' # xxx: add example
#'
angle_to_global_axis <- function(vector, global_axis) {
  # # testing
  # vector=left_right_vector
  # global_axis <- c(1,0,0)
  
  # Normalize vectors
  vector <- vector / sqrt(sum(vector^2))
  global_axis <- global_axis / sqrt(sum(global_axis^2))
  
  # Calculate dot product
  dot_product <- sum(vector * global_axis)
  
  # Calculate the angle (in radians)
  angle_rad <- acos(dot_product)
  
  # Convert angle to degrees
  angle_deg <- angle_rad * 180 / pi
  
  print(angle_deg)
  return(angle_deg)
}

#' Align 3D Point Cloud to Global Axis
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
#' # xxx: add example
#'
rotate_point_cloud <- function(point_cloud, angles) {
  # # testing
  # point_cloud <- all_coords_combined_aligned_y
  # angles <- c(0, 78.29898 , 0)
  
  point_cloud_xyz <- point_cloud %>% 
    select(x,y,z)
  
  # Convert angles to radians
  angles_rad <- angles * pi / 180
  
  # Rotation matrices around x, y, and z axes
  rot_x <- matrix(c(1, 0, 0,
                    0, cos(angles_rad[1]), -sin(angles_rad[1]),
                    0, sin(angles_rad[1]), cos(angles_rad[1])), 3, 3, byrow = TRUE)
  
  rot_y <- matrix(c(cos(angles_rad[2]), 0, sin(angles_rad[2]),
                    0, 1, 0,
                    -sin(angles_rad[2]), 0, cos(angles_rad[2])), 3, 3, byrow = TRUE)
  
  rot_z <- matrix(c(cos(angles_rad[3]), -sin(angles_rad[3]), 0,
                    sin(angles_rad[3]), cos(angles_rad[3]), 0,
                    0, 0, 1), 3, 3, byrow = TRUE)
  
  # Combine rotation matrices
  rot_matrix <- rot_x %*% rot_y %*% rot_z
  
  # Apply rotation matrix to each point in the cloud
  rotated_cloud_xyz <- as_tibble(t(rot_matrix %*% t(point_cloud_xyz)))
  colnames(rotated_cloud_xyz) <- c("x", "y", "z")
  
  rotated_cloud_xyz$ID <- point_cloud$ID
  rotated_cloud_xyz <- rotated_cloud_xyz %>% 
    left_join(point_cloud %>% 
                select(-c(x,y,z)), by="ID")
  
  # extract single dfs
  LMS_df_rot_fin <- rotated_cloud_xyz %>% 
    filter(is.na(norm.x))
  
  facet_positions_translated_rot_fin <- rotated_cloud_xyz %>% 
    filter(!is.na(norm.x))
  
  # plot facet positions
  spheres3d(facet_positions_translated_rot_fin %>% 
              select(x, y, z), 
            col="red", radius=20, alpha = 1)
  
  
  return(rotated_cloud_xyz)
}