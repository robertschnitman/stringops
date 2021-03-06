#' Join elements into a single string
#' 
#' @description Join (collapse) elements into a single string or vector.
#' 
#' @usage string_join(s, collapse)
#'
#' @param s A string (character) vector.
#' @param collapse A string by which to separate the elements of a vector.
#' 
#' @return Character vector.
#' 
#' @details
#' \code{string_join()} joins vector elements into a single string. Synonyms are \code{s_join()} and \code{join()}.
#'
#' @examples
#' string_join(rownames(mtcars), ", ")
#' @seealso \url{https://github.com/robertschnitman/stringops}


#' @rdname string_join
string_join <- function(s, collapse = "") {
  
  stopifnot(is.character(s))
  
  output <- paste0(s, collapse = collapse)
  
  if (collapse != "") {
    
    # Remove last separator.
    output <- string_remove(output, collapse %&% "$")
    
  }
  
  output
  
}
s_join      <- string_join
join        <- string_join