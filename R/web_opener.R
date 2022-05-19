#'
#' @title Open oeis web page on a given sequence
#' 
#' @details 
#' It opens the oeis web page on the sequence that matches
#' the name provided by the user. 
#'  
#'
#' @param x a string with the name of the sequence 
#' in the oeis data base 
#' (which can be fond using the oeis_finder() function).
#'
#' @return the webpage is opened on the default browser
#' @export
#'
#' @examples
#' oeis_browse("A000045")
oeis_browse<- function(x){
  stopifnot(is.character(x))
  utils::browseURL(url = stringr::str_c("https://oeis.org/",x))
}