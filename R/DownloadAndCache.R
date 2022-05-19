#' Downloads and caches OEIS data
#'
#' @details
#'When called, this function downloads the oeis data directly from the oeis
#'website and saves it in the cache memory of the user's computer for easy
#'and rapid access.
#'
#' @return
#' A message indicating how to view the downloaded data
#' 
#' @export
#'
#' @examples
#' oeis_cache()
oeis_cache <- function() {
  
  # Set key
  key <- list("current_data")
  
  # Download data if not already saved
  cat("Downloading data... ")
  
  # Read URL
  data <- suppressWarnings(data.table::fread("https://oeis.org/stripped.gz"))
  
  # Remove unecessary columns
  data <- data[ , -c(3:5)]
  
  # Rename columns
  names(data)[1] <- "name"
  names(data)[2] <- "sequence"
  
  # Remove extra commas
  data$sequence <- stringr::str_remove(data$sequence, "^,")
  data<-tibble::as_tibble(data)
  
  # # Read .csv file from provided URL
  # data <- suppressWarnings(readr::read_csv("https://oeis.org/stripped.gz",show_col_types = FALSE, skip = 3))
  # names(data)<-"column"
  # data$name <- stringr::str_sub(data$column,start = 1,end=7)
  # data$sequence <- stringr::str_remove(data$column,"^A......\\s,")
  # data$column<-NULL
  
  
  cat("Success! \n")
  
  # Use saveCache() to commit the data to a cache location
  R.cache::saveCache(data, key = key, comment = "downloaded")
  print("Run `oeis_load()` to view data")
}



#' Displays oeis data
#'
#' @details
#' Allows the user to access the oeis data saved in the cache memory.
#' If the data has not been downloaded, it tells the user he/she has to 
#' run the oeis_cache()function first.
#'
#' @return the oeis data
#' @export
#'
#' @examples
#' oeis_load()
oeis_load <- function() {
  
  key <- list("current_data")
  
  data <- R.cache::loadCache(key)
  
  # Check if data is not empty
  if (!is.null(data)) {
    
    return(tibble::as_tibble(data))
    
  } else {
    message("Data are not found in cache memory. Use oeis_cache() to download and cache data.")
    return(NULL)
  }
  
}