
#' setdir
#' 
#' Change the working directory
#' 
#' @param dir Project root directory
#' @details Change the working directory to the specified one
#' @export
setdir <- function(dir = ""){
  if(dir.exists(dir) == FALSE){
    message("Add a valid directory!")
  }else{
    setwd(dir = '~/')
    setwd(dir)
  }
}

#' load_data
#'
#' Load database and return a data.frame.
#'
#' @param file Data file name
#' @param h Boolean if there is a header in the file
#' @param separator Column separator used from file
#' @param decimal Separator for decimal values in the file
#' @return Object type data.frame
#' @details Use the read.table function to load the data and return a table with it.
#' @importFrom utils read.table
#' @importFrom utils head
#' @export
load_data <- function(file, h, separator, decimal){
  dgeo = read.table(file = file, header = h, sep = separator, dec = decimal)
  return(dgeo)
}




