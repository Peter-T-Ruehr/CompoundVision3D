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
#' xxx: add example and change above descsriptionand parameters
#'
align_to_global_axis = function(df,
                                line_points_y_inds,
                                line_points_x_inds){
  
  # # testing
  # df = all_coords_combined
  # line_points_y_inds = line_points_y
  # line_points_x_inds = line_points_x
  ## has_normals = TRUE
  
  # rotate_points_tibble <- function(points) {
  require(tibble)
  require(dplyr)
  
  # Ensure the tibble has the required columns
  if (!all(c("x", "y", "z") %in% colnames(df))) {
    stop("Input tibble must have columns: x, y, z")
  }
  # if(has_normals == TRUE){
  #   if (!all(c("norm.x", "norm.y", "norm.z") %in% colnames(df))) {
  #     stop("Input tibble must have columns: x, y, z")
  #   }
  # }
  
  # Helper functions
  normalize <- function(v) {
    magnitude <- sqrt(sum(v^2))
    if (magnitude == 0) stop("Cannot normalize a zero-length vector.")
    v / magnitude
  }
  
  cross_product <- function(u, v) c(
    u[2] * v[3] - u[3] * v[2],
    u[3] * v[1] - u[1] * v[3],
    u[1] * v[2] - u[2] * v[1]
  )
  
  rotate_matrix <- function(df, axis, angle) {
    ux <- axis[1]; uy <- axis[2]; uz <- axis[3]
    cos_a <- cos(angle); sin_a <- sin(angle)
    R <- matrix(c(
      cos_a + ux^2 * (1 - cos_a),      ux * uy * (1 - cos_a) - uz * sin_a, ux * uz * (1 - cos_a) + uy * sin_a,
      uy * ux * (1 - cos_a) + uz * sin_a, cos_a + uy^2 * (1 - cos_a),      uy * uz * (1 - cos_a) - ux * sin_a,
      uz * ux * (1 - cos_a) - uy * sin_a, uz * uy * (1 - cos_a) + ux * sin_a, cos_a + uz^2 * (1 - cos_a)
    ), nrow = 3, byrow = TRUE)
    t(apply(df, 1, function(p) R %*% p))
  }
  
  # Extract df
  A <- df %>%
    slice(line_points_y_inds[1]) %>%
    select(x,y,z) %>%
    as.numeric() # as.numeric(df[1, c("x", "y", "z")])
  B <- df %>%
    slice(line_points_y_inds[2]) %>%
    select(x,y,z) %>%
    as.numeric() # as.numeric(df[2, c("x", "y", "z")])
  C <- df %>%
    slice(line_points_x_inds[1]) %>%
    select(x,y,z) %>%
    as.numeric() # as.numeric(df[3, c("x", "y", "z")])
  D <-  df %>%
    slice(line_points_x_inds[2]) %>%
    select(x,y,z) %>%
    as.numeric() # as.numeric(df[4, c("x", "y", "z")])
  
  # Step 1: Translate all df so A is at the origin
  translate <- function(point, origin) point - origin
  
  translated_df <- as.matrix(df[, c("x", "y", "z")]) %>%
    apply(1, translate, origin = A) %>%
    t()
  # if(has_normals == TRUE) {
  #   translated_normals_df <- as.matrix(df[, c("norm.x", "norm.y", "norm.z")]) %>%
  #     apply(1, translate, origin = A) %>%
  #     t()
  # }
  
  # plot3d(translated_df)
  # plot3d(translated_normals_df)
  
  # Step 2: Rotate so AB aligns with the Y-axis
  AB <- normalize(translated_df[line_points_y_inds[2],])  # Vector AB
  # if(has_normals == TRUE) {
  #   AB.norm <- normalize(translated_normals_df[line_points_y_inds[2],])  # Vector AB
  # }
  
  y_axis <- c(0, 1, 0)
  
  if (!all(abs(AB - y_axis) < 1e-8)) {
    # Find the axis of rotation (cross product of AB and Y-axis)
    rotation_axis_1 <- normalize(cross_product(AB, y_axis))
    angle_1 <- acos(sum(AB * y_axis))
    
    # Apply rotation to align AB with the Y-axis
    translated_df <- rotate_matrix(translated_df, rotation_axis_1, angle_1)
  }
  
  # Step 3: Rotate around the Y-axis so C and D have the same z-coordinate
  C <- translated_df[line_points_x_inds[1],]
  D <- translated_df[line_points_x_inds[2],]
  delta_z <- C[3] - D[3]  # Difference in z-coordinates
  delta_x <- D[1] - C[1]  # Difference in x-coordinates
  
  angle_2 <- atan2(delta_z, delta_x)  # Angle to align z-coordinates of C and D
  
  # Rotate around the Y-axis
  rotation_axis_2 <- c(0, 1, 0)
  final_df <- rotate_matrix(translated_df, rotation_axis_2, -angle_2)
  
  # Convert back to tibble format
  final_df_tibble <- as_tibble(final_df, .name_repair = "unique")
  colnames(final_df_tibble) <- c("x", "y", "z")
  
  # points3d(final_df_tibble)
  
  if(final_df_tibble %>%
     slice(line_points_x[1]) %>%
     pull(x) < final_df_tibble %>%
     slice(line_points_x[2]) %>%
     pull(x)) {
    final_df_tibble <- rotate_point_cloud(final_df_tibble, angles = c(0,180,0))
  }
  
  return(final_df_tibble)
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
#' xxx: add example
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
#' xxx: add example
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
  
  # rotated_cloud_xyz$ID <- point_cloud$ID
  # rotated_cloud_xyz <- rotated_cloud_xyz %>% 
  #   left_join(point_cloud %>% 
  #               select(-c(x,y,z)), by="ID")
  
  # # extract single dfs
  # LMS_df_rot_fin <- rotated_cloud_xyz %>% 
  #   filter(is.na(norm.x))
  # 
  # facet_positions_translated_rot_fin <- rotated_cloud_xyz %>% 
  #   filter(!is.na(norm.x))
  # 
  # # # plot facet positions
  # # spheres3d(facet_positions_translated_rot_fin %>% 
  # #             select(x, y, z), 
  # #           col="red", radius=20, alpha = 1)
  
  return(rotated_cloud_xyz)
}



#' Calculate normal of triangle in 3D
#'
#' xxx: add description
#'
#' @param A A `vector` with the `numeric` x, y, and z coordinges of point 1.
#' @param B A `vector` with the `numeric` x, y, and z coordinges of point 2.
#' @param C A `vector` with the `numeric` x, y, and z coordinges of point 3.
#' @return Returns a `vector` with three `numeric` data points describing the 
#' normal vector of the triangle.
#'
#' @export
#' @examples
#' xxx: add example
#'
calculate_normal <- function(A, B, C, normalize = TRUE) {
  # Ensure the points are numeric vectors of length 3
  if (length(A) != 3 || length(B) != 3 || length(C) != 3) {
    stop("All points must be numeric vectors of length 3.")
  }
  
  # Calculate the vectors AB and AC
  AB <- B - A
  AC <- C - A
  
  # Compute the cross product AB x AC
  normal <- c(
    AB[2] * AC[3] - AB[3] * AC[2],
    AB[3] * AC[1] - AB[1] * AC[3],
    AB[1] * AC[2] - AB[2] * AC[1]
  )
  
  # Calculate the magnitude of the normal vector
  magnitude <- sqrt(sum(normal^2))
  
  # Normalize the normal vector to get a unit normal vector
  if(normalize == TRUE){
    if (magnitude != 0) {
      normal <- normal / magnitude
    }
  }
  
  return(normal)
}


#' Calculate angle between two vectors in 3D
#'
#' xxx: add description
#'
#' @param a A `vector` with three `numeric` data points describing vector 1.
#' @param b A `vector` with three `numeric` data points describing vector b.
#' @return Returns a `numeric` value with the angle in degree (°).
#'
#' @export
#' @examples
#' xxx: add example
#'
angle_between_vectors <- function(a, b) {
  # # testing
  # a = curr_normal
  # b = center_vector
  
  # # plot vectors starting at 0
  # # plot3d(x=NULL)
  # lines3d(x = c(0, curr_normal[1]),
  #         y = c(0, curr_normal[2]),
  #         z = c(0, curr_normal[3]),
  #         col = "cyan")
  # lines3d(x = c(0, center_vector[1]),
  #         y = c(0, center_vector[2]),
  #         z = c(0, center_vector[3]),
  #         col = "red")
  
  # Compute the dot product
  dot_product <- sum(a * b)
  
  # Compute the magnitudes of the vectors
  magnitude_a <- sqrt(sum(a^2))
  magnitude_b <- sqrt(sum(b^2))
  
  # Compute the cosine of the angle between the vectors
  cos_theta <- dot_product / (magnitude_a * magnitude_b)
  angle_deg <- cos_theta*180/pi
  return(angle_deg)
}