#' Cull/extract patterns from strings
#' 
#' @description Cull (extract) patterns from strings.
#' 
#' @usage string_cull(s, pattern, get_all = FALSE, collapse = ',', ...)
#' string_left(s, n)
#' string_right(s, n)
#' string_mid(s, start, n)
#' string_positions(s, positions, join = FALSE)
#' 
#' @param s A string (character) vector.
#' @param pattern A regular expression pattern.
#' @param get_all Whether to get all matches (TRUE) or not (FALSE).
#' @param collapse A separator in the case of multiple matches.
#' @param ... Inputs get passed to \code{gregexpr()}.
#' @param start The position in a string at which to start.
#' @param n The number of characters.
#' @param positions An integer vector of character positions to extract from a string.
#' @param join Determines whether to have columns for each position (\code{FALSE}) or join the extracted characters together (\code{TRUE}).
#' 
#' @return Character vector.
#' 
#' @details The function \code{string_cull()} culls (or extracts) a pattern from a string vector--if no pattern is found, \code{NA} is returned. Multiple pattern matches are separated by the \code{collapse()} input if \code{get_all = TRUE}.
#' 
#' The functions \code{string_left()}, \code{string_right()}, \code{string_mid()} act the same as Excel's \code{LEFT()}, \code{RIGHT}, \code{MID()} functions, respectively: they extract a number of characters at a specified starting point (from the beginning for \code{string_left()}, from the right for \code{string_right()}, and from a specific position for \code{string_mid()}).
#' 
#' The function \code{string_positions()} pulls the character(s) at specified positions. If \code{join = FALSE}, then the extracted characters are split into columns; otherwise, they are concatenated together into a single vector.
#' 
#' The synonym pattern of these functions are \code{s_*()} and \code{*()} (replace asterisks with cull, left, right, mid, and positions).
#'
#' @examples
#' string_cull(rownames(mtcars), '^M|a')
#' string_cull(rownames(mtcars), '^M|a', get_all = TRUE)
#' string_left(rownames(mtcars), 3)
#' string_right(rownames(mtcars), 3)
#' string_mid(rownames(mtcars), 2, 2)
#' string_positions(rownames(mtcars), c(1, 3, 5))
#' string_positions(rownames(mtcars), c(1, 3, 5), join = TRUE)
#' 
#' @seealso \url{https://github.com/robertschnitman/stringops}

#' @rdname string_cull
string_cull <- function(s, pattern, get_all = FALSE, collapse = ',', ...) {
  
  stopifnot(is.character(s))
  
  # Find patterns
  greg <- gregexpr(pattern, s, ...)
  
  # Get matches
  regm <- regmatches(s, greg)
  
  # Flatten matched patterns
  flat <- sapply(regm, string_join, collapse = collapse)
  
  # Get only first match, or all?
  if (get_all == FALSE) {
    
    flat <- string_remove(flat, collapse %&% ".*")
    
  }
  
  # Output should be missing if no matches are found.
  output <- ifelse(flat == "", NA_character_, flat)
  
  output
  
}
s_cull <- string_cull
cull   <- string_cull

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

#' @rdname string_positions
string_positions <- function(s, positions, join = FALSE) {
  
  stopifnot(is.character(s) | is.numeric(positions))
  
  output <- sapply(positions, function(x) string_mid(s, x, 0))
  
  if (join == FALSE) {
    
    colnames(output) <- positions
    
  } else {
    
    output <- apply(output, 1, string_join)
    
  }
  
  output
  
}
s_positions <- string_positions
positions   <- string_positions