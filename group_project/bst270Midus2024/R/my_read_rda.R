#' Read .rda files
#' 
#' @param file_path A string.
#' @examples
#' read_rda("data/my_data.rda")
read_rda <- function(file_path) { # function to read .rda files
  print("Reading file")
  load(file_path)
}