#' 
#' @title
#' Plots oeis numeric sequence
#' 
#' @details
#' Plots the oeis sequence that matches the name given by the user
#'
#' @param x  a string with the name of the desired sequence 
#' on the oeis data base
#' 
#' @param n an integer indicating the number of elements of
#' the sequence that should be plotted. 
#' If null, then all the complete oeis sequences is plotted
#' 
#' @param title a string indicating what the title of the plot shoudl be
#'
#' @return a line plot displaying the sequence 
#' against the corresponding indices
#' @export
#'
#' @examples
#' oeis_plot("A005132")
oeis_plot <- function(x, n = NULL, title = x){
  stopifnot(is.character(x))
  stopifnot(length(x)==1)
  data<-oeis_load()
  if(is.null(data)){
    return(NULL)
  }

  sequence<-oeis_retrieve(x)
  
  if (is.null(n)) {
    n <- length(sequence)
  }
  stopifnot(is.numeric(n), length(n) == 1)
  stopifnot(n <= length(sequence))
  
  graphics::plot(x = seq_len(n),
                 y = sequence[seq_len(n)], 
                 type = "l", 
                 xlab = "Index",
                 ylab = "Number",
                 main = title)
}



#' @title
#' Plot two oeis numeric sequences
#' 
#' @details
#' Plots the oeis sequences that match the names provided by the user
#'
#' @param A  a string with the name of the first desired sequence
#' on the oeis data base 
#' 
#' @param B a string with the name of the second desired sequence
#' on the oeis data base
#' 
#' @param title a string indicating what the title of the plot shoudl be
#'
#' @return a line plot displaying the two sequences 
#' against the corresponding indices  
#' @export
#'
#' @examples
#' oeis_plot2("A297346","A297347")
oeis_plot2<- function(A, B, title = paste(A,"vs",B)){
  stopifnot(is.character(A))
  stopifnot(length(A)==1)
  stopifnot(is.character(B))
  stopifnot(length(B)==1)
  
  data<-oeis_load()
  if(is.null(data)){
    return(NULL)
  }
  
  sequenceA<-oeis_retrieve(A)
  sequenceB<-oeis_retrieve(B)
  
  numplot <- min(length(sequenceA), length(sequenceB))
  
  graphics::plot(sequenceA[seq_len(numplot)], 
                 sequenceB[seq_len(numplot)], 
                 type = "l",
                 xlab = A,
                 ylab = B,
                 main = title)
  
}