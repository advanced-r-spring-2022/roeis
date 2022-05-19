###########
## Functions to play noise
###########

#' Play music from the OEIS
#' 
#' Uses the tuneR package to encode each integer as a note. Solution
#' based on this StackOverflow: \url{https://stackoverflow.com/questions/32857065/is-it-possible-to-code-music-in-r-and-play-it-back-mac-os-x}
#' 
#' @param x The OEIS sequence name.
#' @param low Lowest frequency.
#' @param high Highest frequency.
#' @param wav The default play for Wave files. If you are using mac, then 
#'    this is likely "afplay" or the location of "afplay". If you are
#'    using Windows, it's probably OK to leave this as NULL. If you are using
#'    Ubuntu, I got this to work using "aplay".
#' 
#' @author David Gerard
#' 
#' @export
#' 
#' @examples 
#' \dontrun{
#'   ## Using wav = NULL works on Windows:
#'   oeis_play("A108618")
#'   oeis_play("A007318")
#'   oeis_play("A025480")
#'   oeis_play("A056239")
#'   oeis_play("A117153")
#'   oeis_play("A000005")
#'   oeis_play("A003602")
#'   oeis_play("A004718")
#'   oeis_play("A117154")
#'   oeis_play("A000010")
#'   oeis_play("A064413")
#'   oeis_play("A006577")
#'   oeis_play("A005132")
#'   oeis_play("A001223")
#'   oeis_play("A119953")
#'   oeis_play("A123456")
#'   
#'   ## On Ubuntu, I got it to play music using aplay
#'   oeis_play("A123456", wav = "aplay")
#'   oeis_play("A117153", wav = "aplay")
#' }
oeis_play <- function(x, low = 100, high = 4000, wav = NULL) {
  
  tuneR::setWavPlayer(wav)
  
  stopifnot(is.character(x))
  stopifnot(length(x) == 1)
  data <- oeis_load()
  if(is.null(data)){
    return(NULL)
  }
  seqvec <- oeis_retrieve(x)
  seqvec <- seqvec %% nrow(notes)
  
  notes <- notes[notes$frequency >= low & notes$frequency <= high, , drop = FALSE]
  
  s <- unlist(notes$wave[seqvec])
  sr <- 8000
  bits <- 16
  u <- tuneR::Wave(s, samp.rate=sr, bit=bits)
  tuneR::play(u)
}
