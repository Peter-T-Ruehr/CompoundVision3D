#' Rough clustering towards Facet Peaks
#'
#' Very roughly find clusters around facet peaks.
#'
#' @param df A tibble containing triangle center coordinates of vertices that 
#' lie above threshold in columns `x, y, z` and local height in `local_height`. 
#' Typically, this is the resulting tibble of the `get_local_height()` function.
#' @param local_height_threshold Threshold to filter out vertices: Vertices with
#' `local_height >= local_height_threshold * mean(local_height)` will be kept. 
#' Default: `2.5`.
#' @param clust_melt_rad A numerical value of search radius for agglomerative 
#' clustering.
#' @param iterations A numerical value of how many clustering iterations should 
#' be run. Default: `1`.
#' @param cores A numerical value of how many cores to use. Default: `1`.
#' @return Tibble `df` with ideally one vertex per facet, located on the facet 
#' peak.
#'
#' @export
#' @examples
#' # xxx: add example
#' 
find_facet_peaks_rough <- function(df,
                                   local_height_threshold = 2.5,
                                   clust_melt_rad,
                                   iterations = 1,
                                   cores = 1){
  # dplyr NULLs
  ID <- x <- y <- z <- i <- local_height <- NULL
  
  # remove all coordinates with a local height of less than 2.5x mean local height
  df <- df %>% 
    filter(local_height >= mean(local_height)*2.5) 
  
  
  # store filtered df for final results
  df_final <- df
  
  # select relevant columns for analyses within this function
  df <- df %>% 
    select(x, y, z)
  
  # # plot filtered tibble in 'SEM colours'
  # plot3d(df,
  #        aspect = "iso")
  
  registerDoParallel(cores)
  for(k in 1:iterations){
    print("iteration #:")
    print(k)
    # define number of iterations of preliminary agglomerative clustering
    local.clust.verts.means <- foreach(i = 1:nrow(df),
                                       .combine=rbind, .packages=c('dplyr')) %dopar% { # .packages=c('dplyr', 'geometry')
                                         
                                         # get last columns of df because those are the ones containing the results of the last iteration
                                         curr.df <- df[,(ncol(df)-2):ncol(df)]
                                         colnames(curr.df) <- c("x", "y", "z")
                                         
                                         local.clust.verts <- curr.df %>%
                                           filter(x>(df$x[i]-clust_melt_rad) & x<(df$x[i]+clust_melt_rad) &
                                                    y>(df$y[i]-clust_melt_rad) & y<(df$y[i]+clust_melt_rad) &
                                                    z>(df$z[i]-clust_melt_rad) & z<(df$z[i]+clust_melt_rad))
                                         # plot(local.clust.verts[,2:3])
                                         tmp <- local.clust.verts %>% # local.clust.verts.mean
                                           summarize(median_x = median(x),
                                                     median_y = median(y),
                                                     median_z = median(z))
                                         # points(local.clust.verts.mean[,2:3], pch=16,col="red")
                                       }
    
    colnames(local.clust.verts.means) <- as.vector(outer(c("local_meds_x", "local_meds_y", "local_meds_z"), 
                                                         k, 
                                                         paste, sep="_"))
    df_final <- as_tibble(cbind(df_final, local.clust.verts.means))
    
    # dist.matr <- dist(local.clust.verts.means)
    # dist.matr.tbl <- melt(as.matrix((dist.matr))) %>% as_tibble() %>% filter(Var1 < Var2) %>% 
    #   filter(value <= 50)
    # png(paste0(stl.folder, "/hist_below_50_",k,".png"))
    # hist(dist.matr.tbl$value)
    # dev.off()
    
  }
  stopImplicitCluster()
  
  
  print("done!")
  return(df_final)
}




#' Fine clustering towards Facet Peaks
#'
#' Find facet peaks.
#'
#' @param df A tibble containing triangle center coordinates of vertices that 
#' lie above threshold in columns `x, y, z` . 
#' Typically, this is the resulting tibble of the `find_facet_peaks_rough()` 
#' function.
#' @param cols_to_use A numerical value of which columns store x,y,z coordinates 
#' of rough clustering.
#' @return Tibble `df` with ideally one vertex per facet, located on the facet 
#' peak.
#'
#' @export
#' @examples
#' # xxx: add example
#' 
find_facet_peaks_fine <- function(df,
                                  cols_to_use,
                                  h_min = NULL,
                                  h_max = NULL){
  
  # # testing
  # df = rough_peaks
  # cols_to_use = (ncol(rough_peaks)-2):ncol(rough_peaks)
  # h_min = NULL
  # h_max = NULL
  
  # dplyr NULLs
  ID <- x <- y <- z <- cluster <- Var1 <- Var2 <- '.' <- NULL
  
  # Agglomerative clustering
  # Dissimilarity matrix
  d <- dist(df[,cols_to_use], method = "euclidean")
  # Hierarchical clustering using Complete Linkage
  hc1 <- hclust(d, method = "complete" )
  
  if(is.null(h_min)|is.null(h_max)){
    # Plot the obtained dendrogram
    plot(hc1, cex = 0.6, hang = -1)
    
    message("select minimum and maximum cut-off points on y axis for first trial.")
    h.cutoff.df <- locator(type = "n", n=2)
    h_min = h.cutoff.df$y[1]
    h_max = h.cutoff.df$y[2]
  }
  print(paste0("Min.: ", h_min, "; max.: ", h_max))
  n_steps = 200
  names = c("h", "ommatidia.no")
  ommatidia.no.df = as_tibble(setNames(data.frame(matrix(nrow = 0, 
                                                         ncol = length(names))), 
                                       names))
  
  for(h in seq(h_min, h_max, length.out = n_steps)){
    clusters.indeces <- cutree(hc1, h = h)
    ommatidia.no <- length(unique(clusters.indeces))
    ommatidia.no.df <- rbind(ommatidia.no.df, cbind(h, ommatidia.no))
    # show.progress(h, h_max)
  }
  
  ommatidia.no.df <- ommatidia.no.df %>%
    mutate(ommatidia.no.diff = ommatidia.no - lag(ommatidia.no, default = ommatidia.no[2]))
  
  par(mfrow=c(2,1))
  plot(ommatidia.no.df$h, ommatidia.no.df$ommatidia.no) # , ylim = c(0,max(ommatidia.no.df$ommatidia.no))
  plot(ommatidia.no.df$h, ommatidia.no.df$ommatidia.no.diff, type="l")
  abline(a=0, b=0, col="red", lty=2)
  par(mfrow=c(1,1))
  
  message("select cut-off point on y axis.")
  h.cutoff <- locator(type = "n", n=1)
  
  # save a vector (clusters.fin) that stores to which cluster each coordinate belongs
  clusters.fin <- cutree(hc1, h = h.cutoff$x[length(h.cutoff$x)])
  # print(paste0("Found ", length(unique(clusters.fin)), " potential facets."))
  
  # add cluster-memberships (clusters.fin) to the cluster coordinates (df)
  # reduce clusters to their mean coordinates
  df.fin <- df %>% 
    select(x,y,z) %>% 
    mutate(cluster = clusters.fin) %>% 
    group_by(cluster) %>% 
    summarize(x = median(x),
              y = median(y),
              z = median(z)) %>% 
    select(-cluster)
  
  # # plot the cluster centers
  # plot3d(tri_centers_normals_use[,2:4],
  #        col = tri_centers_normals_use$local_height_cols, aspect = "iso")
  # points3d(df.fin, col="red", size=10, alpha = 0.9)
  # text3d(df.fin, texts = 1:nrow(df.fin), pos = 3, col = "blue", cex=0.5)
  
  # create distance matrix of all clusters to each other
  dist.clusters <- dist(df.fin)
  
  # melt the distance matrix into a three-column tibble
  dist.clusters.tbl <- melt(as.matrix((dist.clusters))) %>% as_tibble() %>% filter(Var1 < Var2)
  
  # plot a histogram of all distances
  hist(dist.clusters.tbl$value)
  clust.dist.med <- median(dist.clusters.tbl$value)
  
  
  # filter the clusters that remain
  df.fin.clean <- df.fin %>% 
    mutate(ID = 1:nrow(.))
  
  
  # plot the cluster centers
  plot3d(df %>% 
           select(x, y, z),
         col = df$local_height_cols, aspect = "iso")
  points3d(df.fin.clean %>% 
             select(x, y, z), 
           col="red", size=10, alpha = 0.9)
  text3d(df.fin.clean %>% 
           select(x, y, z), 
         texts = df.fin.clean$ID, 
         pos = 3, col = "blue", cex=1)
  
  print(paste0("Found ", nrow(df.fin.clean), " facet center candiates. 
               Check 3D plot device."))
  return(df.fin.clean)
}