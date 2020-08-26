#' Cull/extract patterns from strings
#' 
#' @description Cull (extract) patterns from strings.
#' 
#' @usage string_cull(s, pattern, ...)
#' string_left(s, n)
#' string_right(s, n)
#' string_mid(s, start, n)
#' 
#' @param s A string (character) vector.
#' @param pattern A regular expression pattern.
#' @param start The position in a string at which to start.
#' @param n The number of characters.
#' @param ... Inputs get passed to \code{gregexpr()}.
#' 
#' @return Character vector.
#' 
#' @details The function \code{string_cull()} culls (or extracts) a pattern from a string vector--if no pattern is found, \code{NA} is returned.
#' 
#' The functions \code{string_left()}, \code{string_right()}, \code{string_mid()} act the same as Excel's \code{LEFT()}, \code{RIGHT}, \code{MID()} functions, respectively.
#' 
#' The synonym pattern of these functions are \code{s_*()} and \code{*()} (replace asterisks with cull, left, right, and mid).
#'
#' @examples
#' string_cull(rownames(mtcars), '^M.*')
#' string_left(rownames(mtcars), 3)
#' string_right(rownames(mtcars), 3)
#' string_mid(rownames(mtcars), 2, 2)
#' 
#' @seealso \url{https://github.com/robertschnitman/stringops}

#' @rdname string_cull
string_cull <- function(s, pattern, ...) {
  
  # Find patterns
  greg <- gregexpr(pattern, s, ...)
  
  # Get matches
  regm <- regmatches(s, greg)
  
  # Flatten matched patterns
  flat <- sapply(regm, string_join)
  
  # Output should be missing if no matches are found.
  output <- ifelse(flat == "", NA_character_, flat)
  
  output
  
}
s_cull         <- string_cull
cull           <- string_cull

#' @rdname string_left
string_left <- function(s, n) substr(s, 1, n)
s_left      <- string_left
left        <- string_left

#' @rdname string_right
string_right <- function(s, n) substr(s, nchar(s) - n + 1, nchar(s))
s_right      <- string_right
right        <- string_right

#' @rdname string_mid
string_mid <- function(s, start, n) substr(s, start, start + n)
s_mid      <- string_mid
mid        <- string_mid