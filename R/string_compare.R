#' Compare string vectors.
#' 
#' @description Compare string vectors.
#' 
#' @usage string_isblank(s)
#' %like%
#'
#' @param s A string (character) vector.
#' 
#' @return Character vector for \code{string_isblank}; Boolean vector for \code{%like%}.
#' 
#' @details The function \code{string_isblank} tests whether a vector consists of only blank characters. The \code{%like%} operator tests whether a pattern match is found in a string.
#' 
#' @examples
#' string_isblank("         ")
#' string_isblank("    string     ")
#' 
#' rownames(mtcars) %like% "^M"
#' 
#' @seealso \url{https://github.com/robertschnitman/stringops}

#' @rdname string_compare

#' @rdname string_isblank
string_isblank <- function(s) trimws(s) == ""
s_isblank      <- string_isblank
isblank        <- string_isblank

#' @rdname like
`%like%` <- function(s, pattern) grepl(pattern, s)
