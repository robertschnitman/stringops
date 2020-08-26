#' Join elements into a single string or vector.
#' 
#' @description Join (collapse) elements into a single string or vector.
#' 
#' @usage string_join(s, collapse)
#' string_flatten(l)
#'
#' @param s A string (character) vector.
#' @param l A list.
#' @param collapse A string by which to separate the elements of a vector.
#' 
#' @return Character vector.
#' 
#' @details
#' \code{string_join()} joins vector elements into a single string.
#' 
#' \code{string_flatten()} joins list elements into a vector.
#' 
#' The synonym pattern of these functions are \code{s_*()} and \code{*()} (replace asterisks with join and flatten).
#'
#' @examples
#' string_join(rownames(mtcars), ", ")
#' string_flatten(list('a', 'b', 'c'))
#' @seealso \url{https://github.com/robertschnitman/stringops}


#' @rdname string_join
string_join <- function(s, collapse = "") {
  
  output <- paste0(s, collapse = collapse)
  
  if (collapse != "") {
    
    # Remove last separator.
    output <- string_remove(output, collapse %&% "$")
    
  }
  
  output
  
}
s_join      <- string_join
join        <- string_join

#' @rdname string_flatten
string_flatten <- function(l) {
  
  # Type-check
  if (typeof(l) != 'list') {
    
    stop('Input detected is of type ' %&% typeof(l) %&% " instead of list. Please use a list.")
    
  }
  
  # Output
  output <- paste0(l, sep = "")
  
  output
  
}
s_flatten <- string_flatten
flatten   <- string_flatten
