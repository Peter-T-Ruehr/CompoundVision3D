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
                                line_points_y_ind,
                                line_points_x_ind,
                                eye_landmarks){
  
  # # testing
  # df = all_coords_combined
  # line_points_y_ind = line_points_y
  # line_points_x_ind = line_points_x
  # eye_landmarks = eye_landmarks
  
  # select only xyz columns
  df_xyz <- df %>%
    select(x,y,z)
  
  # select only xyz normal columns
  df_xyz_norms <- df %>%
    select(norm.x,norm.y,norm.z)
  
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
  # if(axis == "x"){
  #   y <- c(1,0,0)
  # } else if(axis == "y"){
  #   y <- c(0,1,0)
  # } else if(axis == "z"){
  #   y <- c(0,0,1)
  # }
  # # y
  y <- c(0,1,0)
  
  u=x/sqrt(sum(x^2))
  
  v=y-sum(u*y)*u
  if(v[1] == 0 & v[2] == 0 & v[3] == 0){
    print("already aligned to global y axis.")
    df_xyz_aligned_y <- df_xyz
    df_xyz_norms_aligned_y <- df_xyz_norms
  } else {
    v=v/sqrt(sum(v^2))
    
    cost=sum(x*y)/sqrt(sum(x^2))/sqrt(sum(y^2))
    
    sint=sqrt(1-cost^2);
    
    R <- diag(length(x)) - u %*% t(u) - v %*% t(v) +
      cbind(u,v) %*% matrix(c(cost,-sint,sint,cost), 2) %*% t(cbind(u,v))
    
    df_xyz_aligned_y <- as_tibble(as.matrix(df_xyz)%*% R) %>%
      round(., 8)
    df_xyz_norms_aligned_y <- as_tibble(as.matrix(df_xyz_norms)%*% R) %>%
      round(., 8)
  }
  
  # define column names
  colnames(df_xyz_aligned_y) <- c("x", "y", "z")
  colnames(df_xyz_norms_aligned_y) <- c("norm.x", "norm.y", "norm.z")
  
  df_xyz_aligned_y$ID <- df$ID
  df_xyz_norms_aligned_y$ID <- df$ID
  df_xyz_aligned_y <- df_xyz_aligned_y %>%
    left_join(df %>%
                select(-c(x,y,z,norm.x,norm.y,norm.z)), by="ID") %>%
    left_join(df_xyz_norms_aligned_y, by="ID")
  
  
  # extract single dfs for plotting
  LMS_df_xyz_aligned_y <- df_xyz_aligned_y %>%
    filter(is.na(norm.x))
  
  facet_positions_df_xyz_aligned_y <- df_xyz_aligned_y %>%
    filter(!is.na(norm.x))
  
  
  # plot3d(facet_positions_df_xyz_aligned_y %>%
  #          select(x, y, z),
  #        col="red", size=10, alpha = 1,
  #        aspect = "iso")
  # 
  # 
  # points3d(LMS_df_xyz_aligned_y %>%
  #          select(x, y, z),
  #          size=10, col="blue")
  # 
  # text3d(LMS_df_xyz_aligned_y %>%
  #          select(x, y, z),
  #        texts = LMS_df_xyz_aligned_y$ID)
  # 
  # # draw vectors
  # vec.mult <- .5
  # for(curr_facet in 1:nrow(facet_positions_df_xyz_aligned_y)){ # nrow(facet_positions_df_xyz_aligned_y)
  #   normal_vectors_df_subset <- facet_positions_df_xyz_aligned_y %>%
  #     slice(curr_facet) %>%
  #     select(norm.x, norm.y, norm.z)
  #   curr_facet_coordinates <- facet_positions_df_xyz_aligned_y %>%
  #     slice(curr_facet) %>%
  #     select(x,y,z)
  # 
  #   # find mean point of normalized normal vector ends
  #   norm.x <- normal_vectors_df_subset$norm.x
  #   norm.y <- normal_vectors_df_subset$norm.y
  #   norm.z <- normal_vectors_df_subset$norm.z
  # 
  #   lines3d(x = c(curr_facet_coordinates %>% pull(x), curr_facet_coordinates %>% pull(x) + norm.x*vec.mult),
  #           y = c(curr_facet_coordinates %>% pull(y), curr_facet_coordinates %>% pull(y) + norm.y*vec.mult),
  #           z = c(curr_facet_coordinates %>% pull(z), curr_facet_coordinates %>% pull(z) + norm.z*vec.mult),
  #           col = "green")
  # }
  
  
  # rotate around y to align left and right landmarks in x axis
  # get angle between left-right axis of head to global x axis
  left_coord <- df_xyz_aligned_y %>% 
    slice(line_points_x_ind[1]) %>% 
    select(x,y,z) %>% 
    unlist()
  
  right_coord <- df_xyz_aligned_y %>% 
    slice(line_points_x_ind[2]) %>% 
    select(x,y,z) %>% 
    unlist()
  
  left_right_vector <-  right_coord-left_coord
  
  # # add line to existing plot
  # lines3d(rbind(left_coord, left_coord+left_right_vector))
  
  print("Rotating data...")
  global_axis <- c(1, 0, 0)
  angle_x <- angle_to_global_axis(left_right_vector, global_axis)
  # print(angle_x)
  if(left_right_vector[1] > 0){ #  & mirror == 0
    angle_x <- -1*angle_x
  }
  # print(angle_x)
  
  # define all angles
  angles <- c(0, angle_x , 0)
  
  # plot3d(df_xyz_aligned_y %>%
  #          select(x,y,z))
  
  # Rotate the point cloud: forst facet positions, then facet normals
  df_xyz_aligned_yx <- rotate_point_cloud(df_xyz_aligned_y, 
                                          angles)
  
  df_xyz_aligned_y_facet_normals <- df_xyz_aligned_y %>% 
    filter(!is.na(norm.x)) %>% 
    select(ID,norm.x,norm.y,norm.z)
  colnames(df_xyz_aligned_y_facet_normals) <- c("ID", "x","y","z")
  df_xyz_aligned_yx_facet_normals <- rotate_point_cloud(df_xyz_aligned_y_facet_normals, 
                                                        angles)
  
  colnames(df_xyz_aligned_yx_facet_normals) <- c("norm.x","norm.y","norm.z","ID")
  
  # get rotated normals into df_xyz_aligned_yx
  df_xyz_aligned_yx <- df_xyz_aligned_yx %>% 
    select(-c(norm.x, norm.y, norm.z)) %>% 
    left_join(df_xyz_aligned_yx_facet_normals, by = "ID")
  
  # # extract single dfs
  # LMS_df_xyz_aligned_yx <- df_xyz_aligned_yx %>%
  #   filter(is.na(norm.x))
  # 
  # facet_positions_df_xyz_aligned_yx <- df_xyz_aligned_yx %>%
  #   filter(!is.na(norm.x))
  # 
  # # mirroring data
  # LMS_df_xyz_aligned_yx$x <- -1*LMS_df_xyz_aligned_yx$x
  # LMS_df_xyz_aligned_yx$z <- -1*LMS_df_xyz_aligned_yx$z
  # 
  # # all_coords_combined_rot_fin_mir <- rbind(all_coords_combined_rot_fin,
  # #                                          LMS_df_rot_fin)
  # 
  # # mirroring data
  # facet_positions_df_xyz_aligned_yx$x <- -1*facet_positions_df_xyz_aligned_yx$x
  # facet_positions_df_xyz_aligned_yx$z <- -1*facet_positions_df_xyz_aligned_yx$z
  
  
  # mirroring data
  df_xyz_aligned_yx_fin <- df_xyz_aligned_yx
  df_xyz_aligned_yx_fin$x <- -1*df_xyz_aligned_yx_fin$x
  df_xyz_aligned_yx_fin$z <- -1*df_xyz_aligned_yx_fin$z
  df_xyz_aligned_yx_fin$norm.x <- -1*df_xyz_aligned_yx_fin$norm.x
  df_xyz_aligned_yx_fin$norm.z <- -1*df_xyz_aligned_yx_fin$norm.z
  
  # plot3d(df_xyz_aligned_yx_fin, aspect = "iso")
  # # draw vectors
  # vec.mult <- .5
  # for(curr_facet in 1:nrow(df_xyz_aligned_yx_fin)){
  #   normal_vectors_df_subset <- df_xyz_aligned_yx_fin %>%
  #     slice(curr_facet) %>%
  #     select(norm.x, norm.y, norm.z)
  #   curr_facet_coordinates <- df_xyz_aligned_yx_fin %>%
  #     slice(curr_facet) %>%
  #     select(x,y,z)
  # 
  #   # find mean point of normalized normal vector ends
  #   norm.x <- normal_vectors_df_subset$norm.x
  #   norm.y <- normal_vectors_df_subset$norm.y
  #   norm.z <- normal_vectors_df_subset$norm.z
  # 
  #   lines3d(x = c(curr_facet_coordinates %>% pull(x), curr_facet_coordinates %>% pull(x) + norm.x*vec.mult),
  #           y = c(curr_facet_coordinates %>% pull(y), curr_facet_coordinates %>% pull(y) + norm.y*vec.mult),
  #           z = c(curr_facet_coordinates %>% pull(z), curr_facet_coordinates %>% pull(z) + norm.z*vec.mult),
  #           col = "green")
  # }
  
  # translate everything so that clypeolabral suture = x0
  clypeop_labral_suture_pos_x <- df_xyz_aligned_yx_fin %>% 
    filter(ID == "clypeolabral_suture") %>% 
    pull(x)
  
  # move point cloud along x axis
  df_xyz_aligned_yx_fin_trans_x <- df_xyz_aligned_yx_fin %>% 
    mutate(x = x-clypeop_labral_suture_pos_x)
  
  # plot3d(df_xyz_aligned_yx_fin_trans_x, aspect = "iso")
  
  facet_positions_df_xyz_aligned_yx_fin_trans_x <- df_xyz_aligned_yx_fin_trans_x %>% 
    slice(eye_landmarks)
  
  LMs_df_xyz_aligned_yx_fin_trans_x <- df_xyz_aligned_yx_fin_trans_x %>% 
    slice(-eye_landmarks)
  
  # add right eye
  facet_positions_df_xyz_aligned_yx_fin_trans_x_LR <- facet_positions_df_xyz_aligned_yx_fin_trans_x %>% 
    mutate(ID = paste("L", ID, sep = "_"),
           point_col = "green") %>% 
    add_row(facet_positions_df_xyz_aligned_yx_fin_trans_x %>% 
              mutate(ID = paste("R", ID, sep = "_"),
                     x = -1*x,
                     norm.x = -1*norm.x,
                     point_col = "red")) 
  
  # plot3d(facet_positions_df_xyz_aligned_yx_fin_trans_x_LR, aspect = "iso")
  
  
  # print("Plotting rotated data...")
  # plot3d(facet_positions_df_xyz_aligned_yx_fin_trans_x_LR %>%
  #          filter(!is.na(norm.x)) %>%
  #          select(x, y, z),
  #        col=facet_positions_df_xyz_aligned_yx_fin_trans_x_LR$point_col, size=10, alpha = 1, type = "p",
  #        aspect = "iso")
  # 
  # points3d(LMs_df_xyz_aligned_yx_fin_trans_x %>%
  #            select(x, y, z),
  #          col = "blue", size=10)
  # 
  # text3d(LMs_df_xyz_aligned_yx_fin_trans_x %>%
  #          select(x, y, z),
  #        texts = LMs_df_xyz_aligned_yx_fin_trans_x$ID)
  # # draw vectors
  # vec.mult <- .5
  # curr_facet <- 1
  # for(curr_facet in seq(1,nrow(facet_positions_df_xyz_aligned_yx_fin_trans_x_LR),4)){
  #   normal_vectors_df_subset <- facet_positions_df_xyz_aligned_yx_fin_trans_x_LR %>%
  #     slice(curr_facet) %>%
  #     select(norm.x, norm.y, norm.z)
  #   curr_facet_coordinates <- facet_positions_df_xyz_aligned_yx_fin_trans_x_LR %>%
  #     slice(curr_facet) %>%
  #     select(x,y,z)
  # 
  #   # find mean point of normalized normal vector ends
  #   norm.x <- normal_vectors_df_subset$norm.x
  #   norm.y <- normal_vectors_df_subset$norm.y
  #   norm.z <- normal_vectors_df_subset$norm.z
  # 
  #   lines3d(x = c(curr_facet_coordinates %>% pull(x), curr_facet_coordinates %>% pull(x) + norm.x*vec.mult),
  #           y = c(curr_facet_coordinates %>% pull(y), curr_facet_coordinates %>% pull(y) + norm.y*vec.mult),
  #           z = c(curr_facet_coordinates %>% pull(z), curr_facet_coordinates %>% pull(z) + norm.z*vec.mult),
  #           col = "blue")
  # }
  
  # get midpoint between eyes
  eyes_mid <- c(mean(facet_positions_df_xyz_aligned_yx_fin_trans_x_LR$x),
                mean(facet_positions_df_xyz_aligned_yx_fin_trans_x_LR$y),
                mean(facet_positions_df_xyz_aligned_yx_fin_trans_x_LR$z))
  
  # combine dfs again
  all_combined_yx_fin_trans_x_LR <- rbind(facet_positions_df_xyz_aligned_yx_fin_trans_x_LR,
                                          LMs_df_xyz_aligned_yx_fin_trans_x %>% 
                                            mutate(point_col = NA)) 
  
  # # translate everything in y and z so that midpoint between eyes is at 0,0,0
  all_combined_yx_fin_trans_xyz_LR <- all_combined_yx_fin_trans_x_LR %>%
    mutate(y = y-eyes_mid[2],
           z = z-eyes_mid[3])
  
  # plot3d(all_combined_yx_fin_trans_xyz_LR, aspect = "iso")
  # text3d(all_combined_yx_fin_trans_xyz_LR %>%
  #          select(x, y, z),
  #        texts = all_combined_yx_fin_trans_xyz_LR$ID)
  
  # rotate around y if upside down
  z_dist <- all_combined_yx_fin_trans_xyz_LR %>% 
    filter(ID == "innermost_eye_L") %>% 
    pull(z) -
    all_combined_yx_fin_trans_xyz_LR %>% 
    filter(ID == "clypeolabral_suture") %>% 
    pull(z) 
  if(z_dist < 0){
    all_combined_yx_fin_trans_xyz_LR_rot_y <- rotate_point_cloud(point_cloud = all_combined_yx_fin_trans_xyz_LR,
                                                                 angles = c(0,180,0))
    
    # rotate normals
    all_combined_yx_fin_trans_xyz_LR_rot_y_facet_normals <- all_combined_yx_fin_trans_xyz_LR_rot_y %>% 
      filter(!is.na(norm.x)) %>% 
      select(ID,norm.x,norm.y,norm.z)
    colnames(all_combined_yx_fin_trans_xyz_LR_rot_y_facet_normals) <- c("ID", "x","y","z")
    all_combined_yx_fin_trans_xyz_LR_rot_y_facet_normals <- rotate_point_cloud(all_combined_yx_fin_trans_xyz_LR_rot_y_facet_normals, 
                                                                               angles = c(0,180,0))
    colnames(all_combined_yx_fin_trans_xyz_LR_rot_y_facet_normals) <- c("norm.x","norm.y","norm.z","ID")
    
    # get rotated normals into all_combined_yx_fin_trans_xyz_LR_rot_y
    all_combined_yx_fin_trans_xyz_LR_rot_y <- all_combined_yx_fin_trans_xyz_LR_rot_y %>% 
      select(-c(norm.x, norm.y, norm.z)) %>% 
      left_join(all_combined_yx_fin_trans_xyz_LR_rot_y_facet_normals, by = "ID")
  } else{
    all_combined_yx_fin_trans_xyz_LR_rot_y <- all_combined_yx_fin_trans_xyz_LR
  }
  
  # plot3d(all_combined_yx_fin_trans_xyz_LR_rot_y, aspect = "iso")
  # text3d(all_combined_yx_fin_trans_xyz_LR_rot_y %>%
  #          select(x, y, z),
  #        texts = all_combined_yx_fin_trans_xyz_LR_rot_y$ID)
  
  # rotate around z if front is back
  y_dist <- all_combined_yx_fin_trans_xyz_LR %>% 
    filter(ID == "clypeolabral_suture") %>% 
    pull(y) -
    all_combined_yx_fin_trans_xyz_LR %>% 
    filter(ID == "foramen_dorsal") %>% 
    pull(y) 
  if(y_dist > 0){
    all_combined_yx_fin_trans_xyz_LR_rot_yz <- rotate_point_cloud(point_cloud = all_combined_yx_fin_trans_xyz_LR_rot_y,
                                                                  angles = c(0,0,180))
    
    # rotate normals
    all_combined_yx_fin_trans_xyz_LR_rot_yz_facet_normals <- all_combined_yx_fin_trans_xyz_LR_rot_yz %>% 
      filter(!is.na(norm.x)) %>% 
      select(ID,norm.x,norm.y,norm.z)
    colnames(all_combined_yx_fin_trans_xyz_LR_rot_yz_facet_normals) <- c("ID", "x","y","z")
    all_combined_yx_fin_trans_xyz_LR_rot_yz_facet_normals <- rotate_point_cloud(all_combined_yx_fin_trans_xyz_LR_rot_yz_facet_normals, 
                                                                                angles = c(0,0,180))
    colnames(all_combined_yx_fin_trans_xyz_LR_rot_yz_facet_normals) <- c("norm.x","norm.y","norm.z","ID")
    
    # get rotated normals into all_combined_yx_fin_trans_xyz_LR_rot_y
    all_combined_yx_fin_trans_xyz_LR_rot_y <- all_combined_yx_fin_trans_xyz_LR_rot_y %>% 
      select(-c(norm.x, norm.y, norm.z)) %>% 
      left_join(all_combined_yx_fin_trans_xyz_LR_rot_yz_facet_normals, by = "ID")
    
  } else{
    all_combined_yx_fin_trans_xyz_LR_rot_yz <- all_combined_yx_fin_trans_xyz_LR_rot_y
  }
  
  # plot3d(all_combined_yx_fin_trans_xyz_LR_rot_yz, aspect = "iso")
  # # text3d(all_combined_yx_fin_trans_xyz_LR_rot_yz %>%
  # #          select(x, y, z),
  # # texts = all_combined_yx_fin_trans_xyz_LR_rot_yz$ID)
  # # draw vectors
  # vec.mult <- .5
  # curr_facet <- 1
  # for(curr_facet in seq(1,nrow(facet_positions_df_xyz_aligned_yx_fin_trans_x_LR),4)){
  #   normal_vectors_df_subset <- all_combined_yx_fin_trans_xyz_LR_rot_yz %>%
  #     slice(curr_facet) %>%
  #     select(norm.x, norm.y, norm.z)
  #   curr_facet_coordinates <- all_combined_yx_fin_trans_xyz_LR_rot_yz %>%
  #     slice(curr_facet) %>%
  #     select(x,y,z)
  # 
  #   # find mean point of normalized normal vector ends
  #   norm.x <- normal_vectors_df_subset$norm.x
  #   norm.y <- normal_vectors_df_subset$norm.y
  #   norm.z <- normal_vectors_df_subset$norm.z
  # 
  #   lines3d(x = c(curr_facet_coordinates %>% pull(x), curr_facet_coordinates %>% pull(x) + norm.x*vec.mult),
  #           y = c(curr_facet_coordinates %>% pull(y), curr_facet_coordinates %>% pull(y) + norm.y*vec.mult),
  #           z = c(curr_facet_coordinates %>% pull(z), curr_facet_coordinates %>% pull(z) + norm.z*vec.mult),
  #           col = "blue")
  # }
  
  # # rotate around y if left is right
  # y_dist <- all_combined_yx_fin_trans_xyz_LR %>% 
  #   filter(ID == "clypeolabral_suture") %>% 
  #   pull(y) -
  #   all_combined_yx_fin_trans_xyz_LR %>% 
  #   filter(ID == "foramen_dorsal") %>% 
  #   pull(y) 
  # if(y_dist > 0){
  #   all_combined_yx_fin_trans_xyz_LR_rot_yz <- rotate_point_cloud(point_cloud = all_combined_yx_fin_trans_xyz_LR_rot_y,
  #                                                                 angles = c(0,0,180))
  # } else{
  #   all_combined_yx_fin_trans_xyz_LR_rot_yz <- all_combined_yx_fin_trans_xyz_LR_rot_y
  # }
  # # plot3d(all_combined_yx_fin_trans_xyz_LR_rot_yz, aspect = "iso")
  
  return(all_combined_yx_fin_trans_xyz_LR_rot_yz)
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
  
  rotated_cloud_xyz$ID <- point_cloud$ID
  rotated_cloud_xyz <- rotated_cloud_xyz %>% 
    left_join(point_cloud %>% 
                select(-c(x,y,z)), by="ID")
  
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
