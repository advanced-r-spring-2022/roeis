#' @title Download and parses the OEIS webpage of a sequence
#'
#' 
#' @details This function uses the rvest package to download references, various links, and comments from the webpage of a sequence on the OEIS website. If the OEIS sequence page does not follow a standard structure, Users can access the second, third, or fourth sections which include important information about the sequence.
#'
#' @param x This argument is the id for a mathematical sequence
#' 
#' @param portion This argument specifies what data the user wants to see. This could be just the references, links, or the entire webpage. Alternatively, users can select second, third, or fourth to download those respective sections. 1st is generally offset, and second refers to the second section header in the information section of a sequence entry.
#' 
#' @return All of the text such as references, various links, and comments is returned depending on what the user selects.
#' 
#' @export
#' 
#' @examples
#' oeis_download_webpage("A000045", "all")

oeis_download_webpage <- function(x, portion = c("all", "references", "links", "comments", "formula", "examples", "second", "third", "fourth")){
  portion <- match.arg(portion)
  stopifnot(is.character(x))
  html_obj <- rvest::read_html(stringr::str_c("https://oeis.org/",x))
  if(portion == "all"){
    elements_obj <- rvest::html_nodes(html_obj, css = "p tt")
    text_vec <- rvest::html_text(elements_obj)
    return(text_vec)
  }
  if(portion == "comments"){
    header_obj <- rvest::html_nodes(html_obj, css = "td td tr:nth-child(2) font")
    header_vec <- rvest::html_text(header_obj)
    if(stringr::str_detect(header_vec, "COMMENTS")){
      elements_obj <- rvest::html_nodes(html_obj, css = "tr:nth-child(2) td~ td+ td")
      text_vec <- rvest::html_text(elements_obj)
      text_vec <- cat("Comments associated with the Sequence...", "\n", text_vec)
      return(text_vec)
    } 
    else {
      stop("No comments section detected, consider changing the portion argument to all or second, third, or fourth to view data. Alternatively, check for spelling.")
    }
  }
  if(portion == "references"){
    header_obj <- rvest::html_nodes(html_obj, css = "td td tr:nth-child(3) font")
    header_vec <- rvest::html_text(header_obj)
    if(stringr::str_detect(header_vec, "REFERENCES")){
      elements_obj <- rvest::html_nodes(html_obj, css = "td td tr:nth-child(3) td~ td+ td")
      text_vec <- rvest::html_text(elements_obj)
      text_vec <- cat("References associated with the Sequence...", "\n", text_vec)
      return(text_vec)
    } else {
      stop("No references section detected, consider changing the portion argument to all or second, third, or fourth to view data. Alternatively, check for spelling.")
    }
  }
  if(portion == "links"){
    header_obj <- rvest::html_nodes(html_obj, css = "td td tr:nth-child(4) font")
    header_vec <- rvest::html_text(header_obj)
    if(stringr::str_detect(header_vec, "LINKS")){
      elements_obj <- rvest::html_nodes(html_obj, css = "tr:nth-child(4) td~ td+ td")
      text_vec <- rvest::html_text(elements_obj)
      text_vec <- cat("Links associated with the Sequence. To access these links, use the web_opener() function to click on the hyperlinks", "\n", text_vec)
      return(text_vec)
    } else {
      stop("No links section detected, consider changing the portion argument to all or second, third, or fourth to view data. Alternatively, check for spelling.")
    }
  }
  if(portion == "formula"){
    header_obj <- rvest::html_nodes(html_obj, css = "td td tr:nth-child(5) font")
    header_vec <- rvest::html_text(header_obj)
    elements_obj <- rvest::html_nodes(html_obj, css = "td td tr:nth-child(5) td~ td+ td")
    if(stringr::str_detect(header_vec, "FORMULA")){
      text_vec <- rvest::html_text(elements_obj)
      text_vec <- cat("Formulas associated with the Sequence...", "\n", text_vec)
      return(text_vec)
    } else{
      stop("No links section detected, consider changing the portion argument to all or second, third, or fourth to download the data. Alternatively, check for spelling.")
    }
  }
  if(portion == "examples"){
    header_obj <- rvest::html_nodes(html_obj, css = "tr:nth-child(6) font")
    header_vec <- rvest::html_text(header_obj)
    if(stringr::str_detect(header_vec, "EXAMPLE")){
      elements_obj <- rvest::html_nodes(html_obj, css = "tr:nth-child(6) td~ td+ td")
      text_vec <- rvest::html_text(elements_obj)
      text_vec <- cat("Examples associated with the Sequence...", "\n", text_vec)
      return(text_vec)
    } else {
      stop("No examples section detected, consider changing the portion argument to all or second, third, or fourth to download the data. Alternatively, check for spelling.")
    }
  }
  if(portion == "second"){
    elements_obj <- rvest::html_nodes(html_obj, css = "tr:nth-child(2) td~ td+ td")
    text_vec <- rvest::html_text(elements_obj)
    text_vec <- cat("View 2nd section about the sequence from OEIS webpage", "\n", text_vec)
    return(text_vec)
  } else {
    print("Please use `oeis_browse()` to view the webpage or consider changing the portion argument to all to download the data. Alternatively, check for spelling.")
  }
  if(portion == "third"){
    elements_obj <- rvest::html_nodes(html_obj, css = "td td tr:nth-child(3) td~ td+ td")
    text_vec <- rvest::html_text(elements_obj)
    text_vec <- cat("View 3rd section about the sequence from OEIS webpage", "\n", text_vec)
    return(text_vec)
  } else {
    print("Please use `oeis_browse()` to view the webpage or consider changing the portion argument to all to download the data. Alternatively, check for spelling.")
  }
  if(portion == "fourth"){
    elements_obj <- rvest::html_nodes(html_obj, css = "tr:nth-child(4) td~ td+ td")
    text_vec <- rvest::html_text(elements_obj)
    text_vec <- cat("View 4th section about the sequence from OEIS webpage", "\n", text_vec)
    return(text_vec)
  } else {
    print("Please use `oeis_browse()` to view the webpage or consider changing the portion argument to all to download the data. Alternatively, check for spelling.")
  }
}