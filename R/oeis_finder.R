#' @title 
#' Searches for numeric sequences
#' 
#' @details
#' Searches through the oeis data base for sequences 
#' that match the first few numbers in a sequence (inputted by the user)
#'
#' @param x a numeric vector with the first few numbers of the desired sequence 
#' 
#'
#' @return a tibble with all the sequences that begin with the inputted numbers,
#' and their corresponding name in the oeis data base
#' @export
#'
#' @examples
#' oeis_finder(c(0,1,1,2,3,5,8,13,21))
oeis_finder<-function(x){
  stopifnot(is.numeric(x))
  data<-oeis_load()
  if(is.null(data)){
    return(NULL)
  }
  x<-stringr::str_c(as.character(x),collapse=",")
  x<-stringr::str_c("^",x)
  data[stringr::str_detect(data$sequence, x),]
}




