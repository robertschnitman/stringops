#' Modify cases for strings.
#' 
#' @description Modify the case of the string.
#' 
#' @usage string_upper(s)
#' s_upper(s) 
#' upper(s) 
#' string_lower(s)
#' s_lower(s)
#' lower(s)
#' string_swapcase(s)
#' s_swapcase(s)
#' swapcase(s)
#'
#' @param s A string (character) vector.
#' 
#' @return Character vector.
#' 
#' @details The \code{upper} functions make all characters in a string vector in their capitalized case. The \code{lower} functions makes the same vector in their lower case. The \code{swapcase} functions switches the case for each character in a string vector.
#'
#' @examples
#' upper(rownames(mtcars))
#' lower(rownames(mtcars))
#' swapcase(rownames(mtcars))
#' 
#' @seealso \url{https://github.com/robertschnitman/stringops}

#' @rdname string_cases

#' @rdname string_upper
string_upper <- function(s) toupper(s)
s_upper      <- string_upper
upper        <- string_upper

#' @rdname string_lower
string_lower <- function(s) tolower(s)
s_lower      <- string_lower
lower        <- string_lower

#string_title <- function(s) {
  
 # splits <- lapply(strsplit(s, " "))
  
#}
#s_title
#title

#' @rdname string_swapcase
string_swapcase <- function(s) {
  
  case_test <- function(x) ifelse(x %in% letters, toupper(x), tolower(x))
  
  splits <- lapply(strsplit(s, ""), case_test)
  
  output <- sapply(splits, string_join)
  
  output
  
}
s_swapcase <- string_swapcase
swapcase   <- string_swapcase