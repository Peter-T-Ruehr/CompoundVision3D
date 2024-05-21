#' Plot eye in 3D
#'
#' Imports triangle centers and triangle normals of STL file as tibble. We use 
#' the word 'triangle' here to refer to the facets of an STL mesh to avoid 
#' confusion with the facets of compound eyes.
#'
#' @param file_name File name of STL to import.
#'
#' @return A tibble containing triangle centers and triangle normals of STL file.
#'
#' @export
#' @importFrom doParallel registerDoParallel stopImplicitCluster
#' @importFrom dplyr filter pull select mutate arrange slice group_by ungroup left_join summarize distinct
#' first lead lag case_when bind_cols tibble as_tibble desc progress_estimated bind_rows all_of rename n 
#' mutate_all
#' @importFrom foreach foreach '%dopar%'
#' @importFrom magrittr '%>%'
#' @importFrom geometry dot
#' @importFrom graphics locator par abline hist
#' @importFrom grDevices grey.colors
#' @importFrom readr write_csv
#' @importFrom reshape2 melt
#' @importFrom rgl plot3d spheres3d selectpoints3d points3d text3d
#' @importFrom tidyr separate
#' @importFrom stats dist hclust setNames cutree median
#' @examples
#' # xxx: add example and change above parameter description
#' 
plot_eye <- function(facets_coords,
                     facet_colours = "red",
                     facet_sizes = 10,
                     facet_type = "p",
                     LM_coords = NULL,
                     LM_colours = "blue",
                     LM_sizes = 10,
                     LM_type = "p",
                     text_coords = NULL,
                     text_labels = NULL){
  
  # # testing
  # facets_coords = facets %>% 
  #   select(x, y, z)
  # facet_colours = facets$point_col
  # # facet_colours = "red"
  # facet_sizes = 60
  # facet_type = "p"
  # # facet_type = "s"
  # LM_coords = LMs %>% 
  #   select(x, y, z)
  # LM_colours = "blue"
  # # LM_colours = "red"
  # LM_sizes = 80
  # LM_type = "p"
  # # LM_type = "s"
  # text_coords = LMs %>% 
  #   select(x, y, z)
  # text_labels = LMs$ID
  
  # # close rgl window
  # while (rgl.cur() > 0) { close3d }
  
  if(facet_type == "s"){
    facet_radius = facet_sizes # /2
    plot3d(facets_coords, 
           col=facet_colours, 
           radius=facet_radius, 
           type = facet_type,
           aspect = "iso")
  } else if(facet_type == "p"){
    plot3d(facets_coords, 
           col=facet_colours, 
           size=facet_sizes, 
           type = facet_type,
           aspect = "iso")
  }
  
  if(!is.null(LM_coords)){
    if(LM_type == "s"){
      LM_radius = LM_sizes # /2
      spheres3d(LMs %>% 
                  select(x, y, z),
                col = "blue", radius=LM_radius)
    } else if(LM_type == "p"){
      points3d(LMs %>% 
                 select(x, y, z),
               col = "blue", size=LM_sizes)
    }
  }
  
  if(!is.null(LM_coords)){
    text3d(text_coords,
           texts = text_labels)
  }
}


#' # create a continuous color ramp
#'
#' Imports triangle centers and triangle normals of STL file as tibble. We use 
#' the word 'triangle' here to refer to the facets of an STL mesh to avoid 
#' confusion with the facets of compound eyes.
#'
#' @param file_name File name of STL to import.
#'
#' @return A tibble containing triangle centers and triangle normals of STL file.
#'
#' @export
#' @importFrom doParallel registerDoParallel stopImplicitCluster
#' @importFrom dplyr filter pull select mutate arrange slice group_by ungroup left_join summarize distinct
#' first lead lag case_when bind_cols tibble as_tibble desc progress_estimated bind_rows all_of rename n 
#' mutate_all
#' @importFrom foreach foreach '%dopar%'
#' @importFrom magrittr '%>%'
#' @importFrom geometry dot
#' @importFrom graphics locator par abline hist
#' @importFrom grDevices grey.colors
#' @importFrom readr write_csv
#' @importFrom reshape2 melt
#' @importFrom rgl plot3d spheres3d selectpoints3d points3d text3d
#' @importFrom tidyr separate
#' @importFrom stats dist hclust setNames cutree median
#' @examples
#' # xxx: add example and change above parameter description
#' 
continuous_color_ramp <- function(values, colors) {
  v <- (values - min(values))/diff(range(values))
  x <- colorRamp(colors)(v)
  rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
}