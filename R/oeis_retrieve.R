#' @title
#' Retrieve oeis sequence
#' 
#' @details 
#' Searches through the oeis data base for a sequence that
#' matches the name inputted by the user
#' and returns it as a numeric vector
#'
#' @param x a string with the name of the desired sequence 
#' in the oeis data base
#'
#' @return a numeric vector with the elements of 
#' the desired sequence
#' @export
#'
#' @examples
#' oeis_retrieve("A000045")
oeis_retrieve<-function(x){
  stopifnot(is.character(x))
  stopifnot(length(x)==1)
  
  data<-oeis_load()
  if(is.null(data)){
    return(NULL)
  }
  data[stringr::str_detect(data$name, x),][2]->auxiliar
  auxiliar$sequence<-stringr::str_remove(auxiliar$sequence,",$")
  sequence<-stringr::str_split(auxiliar$sequence,",")
  sequence<-as.numeric(sequence[[1]])
  return(sequence)
}