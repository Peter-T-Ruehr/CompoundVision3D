#' Read json File with Landmark Data from 3D Slicer
#'
#' Reads a *.mrk.json file containing landmarks created in 3D slicer.
#'
#' @param LM_file_location A string containing the full path of the json file.
#' @return Returns a tibble with the landmark data in columns 
#' `LM,x, y, z`.
#'
#' @export
#' @examples
#' # xxx: add example
#'
read_3DSlicer_landmarks <- function(LM_file_location){
  # # testing
  # LM_file_location <- file.path(LM_folder, "landmarks.mrk.json")
  
  LM_file <- fromJSON(file = LM_file_location)
  
  LM_file_df <- tibble(var = names(unlist(LM_file$markups[[1]]$controlPoints)),
                       val = unlist(LM_file$markups[[1]]$controlPoints))
  
  LMs_df <- tibble(LM = character(),
                   x = numeric(),
                   y = numeric(),
                   z = numeric())
  
  i=2
  while(i <= nrow(LM_file_df)){
    curr_var = LM_file_df$var[i]
    if(curr_var == "label"){
      curr_LM <- LM_file_df$val[i]
      curr_LM_x <- as.numeric(LM_file_df$val[i+3])
      curr_LM_y <- as.numeric(LM_file_df$val[i+4])
      curr_LM_z <- as.numeric(LM_file_df$val[i+5])
      
      LMs_df <- LMs_df %>% 
        add_row(LM = curr_LM,
                x = curr_LM_x,
                y = curr_LM_y,
                z = curr_LM_z)
    }
    i=i+1
  }
  
  return(LMs_df)
}