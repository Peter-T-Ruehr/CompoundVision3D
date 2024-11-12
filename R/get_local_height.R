#' Find Local Vertex Heights
#'
#' Calculate distance of vertices from local plane.
#'
#' @param df A tibble containing triangle center coordinates in columns `x, y, z`.
#' @param search_diam A numerical value defining the size of the search diameter 
#' of defining the local plane.
#' @param cores A numerical value of how many cores to use. Default: `1`.
# @param normalize A numerical value to specifiy if local distances should be
# normalized within a the given radius. If `NULL`, normalizing will be skipped.
# Default: `NULL`. Recommended: `serach_daim/2`.
#'
#' @return Tibble `df` with additional column `local_height`.
#'
#' @export
#' @examples
#' # xxx: add example
#'
get_local_heights <- function(df,
                              search_diam,
                              cores = 1,
                              log_scale = TRUE,
                              verbose = FALSE){
  
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
  x <- y <- z <- value <- value.1 <- value.2 <- row_number <- norm.x <- 
    norm.y <- norm.z <- i <- search_diam_local_height <- local_height <- NULL
  
  # define function to normalize vector
  normalize_vector <- function(v){
    v/sqrt(sum(v^2))
  }
  
  if(verbose == TRUE){
    print("Starting analyses on cluster...")
    start_time <- Sys.time()
  }
  
  # calculate distance of all vertices to local plane within search_diam
  registerDoParallel(cores)
  
  
  if(verbose == TRUE){
    print(paste0("Calculating local heights for all ", nrow(df), " vertices..."))
  }
  local_heights <- foreach(i = 1:nrow(df),
                           .combine=rbind, .packages=c('dplyr', 'geometry')) %dopar% {
                             
                             curr.facet.x.y.z <- df %>%
                               dplyr::slice(i) %>%
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
  # save(df, file = "./data/AV00003_df.Rdata")
  
  stopImplicitCluster()
  
  # save(df, file = "./data/AV00003_df_NORM.Rdata")
  # load(file = "./data/AV00003_df_NORM.Rdata")
  
  
  if(verbose == TRUE){
    print("Cluster analysis finished.")
    end_time <- Sys.time()
    print(end_time - start_time)
  }
  
  if(verbose == TRUE){
    print("done!")
  }
  
  return(df)
}

#' Get colour values !HERE: Change description
#'
#' Calculate distance of vertices from local plane.
#'
#' @param df A tibble containing triangle center coordinates in columns `x, y, z`.
#' @param search_diam A numerical value defining the size of the search diameter 
#' of defining the local plane.
#' @param cores A numerical value of how many cores to use. Default: `1`.
# @param normalize A numerical value to specifiy if local distances should be
# normalized within a the given radius. If `NULL`, normalizing will be skipped.
# Default: `NULL`. Recommended: `serach_daim/2`.
#' @importFrom forceR rescale_to_range
#'
#' @return Tibble `df` with additional column `local_height`.
#'
#' @export
#' @examples
#' # xxx: add example
#'
get_height_colors <- function(heights,
                              lower_quantile = NULL,
                              upper_quantile = NULL,
                              verbose = FALSE){
  
  # # testing
  # lower_quantile = NULL
  # upper_quantile = NULL
  # heights = local_heights$local_height
  # heights = 10^local_heights$local_height
  # range(heights)
  # lower_quantile = 0.1
  # upper_quantile = 0.9
  
  
  if(!is.null(upper_quantile) & !is.null(lower_quantile)){
    # set upper and lower boundary for local heights
    heights[heights >= 
              quantile(heights, upper_quantile)] <-
      quantile(heights, upper_quantile)
    
    if(min(heights) < 0){
      if(verbose == TRUE){
      print("non-logarigthmic")
      }
      heights[heights <= 
                quantile(heights, lower_quantile)] <-
        quantile(heights, lower_quantile)
    } else{
      if(verbose == TRUE){
      print("logarigthmic")
      }
      heights[heights <= 
                quantile(heights, lower_quantile)] <-
        quantile(heights, lower_quantile)
    }
  }
  
  if(verbose == TRUE){
  message("Adding colours for height values...")
  }
  # create color vector
  color_df <- tibble(local_height = round(seq(0, 100000-1, length.out = 100000)), 
                     local_height_col = grey.colors(100000, start=0.0)) %>% 
    distinct(local_height, .keep_all = TRUE)
  
  # add colors
  local_height_cols <- tibble(local_height = heights) %>% 
    mutate(local_height = round(rescale_to_range(local_height, 1, 100000))) %>% 
    left_join(color_df, by = "local_height") %>% 
    pull(local_height_col)
  
  return(local_height_cols)
}