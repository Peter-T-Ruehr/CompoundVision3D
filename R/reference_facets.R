#' References Coordinates to Exisiting Point Cloud.
#'
#' Takes a point cloud of coordinates, e.g. facet positions, and finds the 
#' closest points (including their data) in a second point cloud of coordinates.
#'
#' @param coordinates A dataframe containing at least the columns `x,y,z`.
#' @param reference_coordinates A dataframe containing at least the columns `x,y,z`.
#' @param search_diameter A numeric value to define the size of the box that is 
#' used around each coordinate to find its closest reference coordinate. 
#' Defailt: `1`.
#' @return Returns a tibble with points in `reference_coordinates` that are 
#' closest to the points in `coordinates`, containing the same columns as 
#' `reference_coordinates`.
#'
#' @export
#' @examples
#' # xxx: add example
#'
reference_point_clouds <- function(coordinates,
                             reference_coordinates,
                             search_diameter = 10,
                             cores = 1){
  # # testing
  # coordinates = facet_positions
  # reference_coordinates = local_heights
  # cores = 12
  
  # load multi-core package
  require(doParallel)
  
  # go through all facet positions imported from blender and get closest local heights data
  registerDoParallel(cores)
  i=1
  
  facet_positions_new <- foreach(i = 1:nrow(coordinates), # nrow(coordinates)
                                 .combine=rbind, .packages=c("dplyr", 'reshape2')) %dopar% {
                                   
                                   
                                   curr_x <- coordinates$x[i]
                                   curr_y <- coordinates$y[i]
                                   curr_z <- coordinates$z[i]
                                   
                                   # get window around current poin in reference_coordinates
                                   curr_window <- reference_coordinates %>% 
                                     filter(x >= curr_x - search_diameter,
                                            x <= curr_x + search_diameter,
                                            y >= curr_y - search_diameter,
                                            y <= curr_y + search_diameter,
                                            z >= curr_z - search_diameter,
                                            z <= curr_z + search_diameter)
                                   
                                   # add curr. facet position to window with ID = 0
                                   curr_window <- curr_window %>% 
                                     add_row(ID = 0, x=curr_x, y=curr_y, z=curr_z) %>% 
                                     arrange(ID)
                                   
                                   # # plot for testing
                                   # par(mfrow=c(1,2))
                                   # plot(curr_window %>% select(x,y), pch=16)
                                   # points(curr_window %>% select(x,y) %>% slice(1), col="red", cex = 2)
                                   # plot(curr_window %>% select(x,z), pch=16)
                                   # points(curr_window %>% select(x,z) %>% slice(1), col="red", cex = 2)
                                   # par(mfrow=c(1,1))
                                   
                                   # create distance matrix of all clusters to each other
                                   dist_matrix <- dist(curr_window %>% 
                                                         select(x,y,z))
                                   
                                   # melt the distance matrix into a three-column tibble
                                   dist_matrix_tbl <- reshape2::melt(as.matrix((dist_matrix))) %>% as_tibble() %>% filter(Var1 < Var2)
                                   
                                   # get closest point to current facet (is now number 1, because after arranging it was at position 1 in curr_window)
                                   closest_point_df <- dist_matrix_tbl %>% 
                                     filter(Var1 == 1 | Var2 == 1) %>% 
                                     arrange(value) %>% 
                                     slice(1)
                                   
                                   closest_point_position <- max(closest_point_df %>%
                                                                   select(Var1, Var2))
                                   
                                   tmp <- curr_window %>% 
                                     slice(closest_point_position) # closest_point_data
                                   
                                   # get data of that point as new row into facet_positions_new
                                   
                                   # facet_positions_new <- facet_positions_new %>% 
                                   #   add_row(closest_point_data)
                                 }
  stopImplicitCluster()
  
  return(facet_positions_new)
}