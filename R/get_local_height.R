#' Find Local Vertex Heights
#'
#' Calculate distance of vertices from local plane.
#'
#' @param df A tibble containing triangle center coordinates in columns `x, y, z`.
#' @param search_diam A numerical value defining the size of the search diameter 
#' of defining the local plane.
#' @param cores A numerical value of how many cores to use. Default: `1`.
#'
#' @return Tibble `df` with additional column `local_height`.
#'
#' @export
#' @examples
#' # xxx: add example
#'
get_local_height <- function(df,
                             search_diam,
                             cores = 1,
                             log_scale = FALSE){
  
  # load package for multi-core
  require(doParallel)
  # # testing
  # tri_centers_normals <- readr::read_csv("X:/Pub/2021/_Ruehr_AntVision/data/3_triangle_centers_and_normals//AV00001_Camponotus_hyatti_eye1_surface.csv",
  #                                        show_col_types = FALSE)
  # df = tri_centers_normals[1:100, ]
  # search_diam = 55
  # cores = 12
  # log_scale = TRUE
  # #/ testing
  
  # dplyr NULLs
  ID <- x <- y <- z <- value <- value.1 <- value.2 <- row_number <- norm.x <- 
    norm.y <- norm.z <- i <- search_diam_local_height <- local_height <- NULL
  
  # define function to normalize vector
  normalize_vector <- function(v){
    v/sqrt(sum(v^2))
  }
  
  # define function to normalize data (from forceR)
  rescale_to_range <- function(data, from, to) {
    data <- data - min(data)
    data <- data / max(data)
    data <- data * (to - from)
    data + from
    return(data)
  }
  
  # calculate distance of all vertices to local plane within search_diam
  start_time <- Sys.time()
  registerDoParallel(cores)
  
  print("Starting analyses on cluster...")
  print("Calculatinjg local heigths...")
  local_heights <- foreach(i = 1:nrow(df),
                           .combine=rbind, .packages=c('dplyr', 'geometry')) %dopar% {
                             
                             curr.facet.x.y.z <- df %>%
                               dplyr::filter(ID == i) %>%
                               select(x, y, z) %>%
                               as.numeric()
                             
                             search_diam_local_height <- round(1/8*search_diam,2)
                             
                             curr.facets.df <- df %>%
                               dplyr::filter(x  >= curr.facet.x.y.z[1] - search_diam_local_height &
                                               y  >= curr.facet.x.y.z[2] - search_diam_local_height &
                                               z  >= curr.facet.x.y.z[3] - search_diam_local_height &
                                               x  <= curr.facet.x.y.z[1] + search_diam_local_height &
                                               y  <= curr.facet.x.y.z[2] + search_diam_local_height &
                                               z  <= curr.facet.x.y.z[3] + search_diam_local_height )
                             # print(nrow(curr.facets.df))
                             
                             # calculate current average facet normal
                             curr.facets.av.normal <- c(mean(curr.facets.df$norm.x), 
                                                        mean(curr.facets.df$norm.y), 
                                                        mean(curr.facets.df$norm.z))
                             
                             # calculate current facet center
                             curr.facets.center <- c(mean(curr.facets.df$x), 
                                                     mean(curr.facets.df$y), 
                                                     mean(curr.facets.df$z))
                             
                             
                             # create unit normal vector of plane normal
                             curr.facets.av.normal.normailzed <- normalize_vector(curr.facets.av.normal)
                             
                             # find vector of current point to arbitrary other point on plane (here: plane center)
                             vector_point_facet_center <- curr.facet.x.y.z-curr.facets.center
                             
                             
                             curr_local_height <- dot(vector_point_facet_center,curr.facets.av.normal.normailzed, 
                                                      d=TRUE)
                             
                             # plot3d(curr.facets.df[2:4], aspect = "iso")
                             # par3d("windowRect"= c(2300,200,3400,1000))
                             # lines3d(x=c(curr.facets.center[1], curr.facets.center[1]+curr.facets.av.normal.normailzed[1]),
                             #         y=c(curr.facets.center[2], curr.facets.center[2]+curr.facets.av.normal.normailzed[2]),
                             #         z=c(curr.facets.center[3], curr.facets.center[3]+curr.facets.av.normal.normailzed[3]))
                             
                             # df$local_height[n] <- curr_local_height
                             
                             tmp <- curr_local_height
                           }
  
  # add distances to local planes to df tibble
  df$local_height <- as.numeric(local_heights)
  
  # normalize local_height values within search radius
  print("Normalizing local heights...")
  local_heights_norm <- foreach(i = 1:nrow(df), # nrow(df)
                           .combine=rbind, .packages=c('dplyr')) %dopar% {
                             
                             curr.facet.x.y.z <- df %>%
                               dplyr::filter(ID == i) %>%
                               select(x, y, z) %>%
                               as.numeric()
                             
                             search_diam_normalize <- search_diam/2
                             
                             curr.facets.df <- df %>%
                               dplyr::filter(x  >= curr.facet.x.y.z[1] - search_diam_normalize &
                                               y  >= curr.facet.x.y.z[2] - search_diam_normalize &
                                               z  >= curr.facet.x.y.z[3] - search_diam_normalize &
                                               x  <= curr.facet.x.y.z[1] + search_diam_normalize &
                                               y  <= curr.facet.x.y.z[2] + search_diam_normalize &
                                               z  <= curr.facet.x.y.z[3] + search_diam_normalize )
                             # print(nrow(curr.facets.df))
                             
                             curr_local_height_norm <- curr.facets.df %>% 
                               mutate(local_height_norm = rescale_to_range(local_height, 0, 99)) %>% 
                               filter(x == curr.facet.x.y.z[1], 
                                      y == curr.facet.x.y.z[2], 
                                      z == curr.facet.x.y.z[3])
                             
                             tmp <- curr_local_height_norm
                           }
  
  stopImplicitCluster()
  print("Cluster analysis finished.")
  local_heights_norm
  
  # add colors
  local_heights_norm <- local_heights_norm %>% 
    arrange(local_height_norm) %>% 
    mutate(local_height_col = grey.colors(nrow(local_heights_norm), start=0.0)) %>% 
    arrange(ID)
  
  end_time <- Sys.time()
  print(end_time - start_time)
  
  # plot eye in 'SEM colors'
  plot3d(local_heights_norm[, 2:4], 
         col = local_heights_norm$local_height_col, 
         aspect = "iso",
         size=7)
  
  print("done!")
  
  return(local_heights_norm)
}
