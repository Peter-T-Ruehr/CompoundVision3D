#' Read multiple csv files and store their file name in column
#'
#' @param file_list A vector containing `character` strings of csv file names.
#' @param show_col_types A `logic` value to define if column types should be 
#' shown while importing. `Default: FALSE`.
#' @param axis A character string defining the global axis to align to. Must be 
#' `x`, `y`, or `z`.
#' @return Returns a tibble with the columns as they appear in all csv files.
#' @export
#' @examples
#' xxx: add example and change above descsriptionand parameters
#'
read_plus <- function(file_list,
                      show_col_types = FALSE) {
  # # testing
  # file_list = file_list
  # filename <- file_list[1]
  # show_col_types = FALSE
  
  df_fin <- tibble()
  for(filename in file_list){
    print(filename)
    curr_df <- read_csv(filename,
                        show_col_types = show_col_types) %>% 
      mutate(filename = gsub("\\.csv", "", basename(filename)))
    df_fin <- rbind(df_fin,curr_df)
  }
  return(df_fin)
}