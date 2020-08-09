#' Concatenate strings together
#' 
#' @description Concatenate strings together.
#' 
#' @usage %&%
#' string_concat(a, b)
#' string_prefix(s, prefix)
#' s_prefix(s, prefix)
#' prefix(s, prefix)
#' string_suffix(s, suffix)
#' s_suffix(s, suffix)
#' suffix(s, suffix)
#' string_flag(s, flag, width)
#' s_flag(s, flag, width)
#' flag(s, flag, width, which = c('left', 'right', 'both'))
#' string_flagv(s, ...)
#' s_flagv(s, ...)
#' flagv(s, ...)
#' 
#'
#' @param s A string (character) vector.
#' @param a A string (character) vector.
#' @param b A string (character) vector.
#' @param collapse A string by which to separate the elements of a vector.
#' @param flag Character or number to append to a string.
#' @param width Number of characters a string should be.
#' @param which Side to flag a string.
#' @param ... Parameters passed to \code{string_flag()}.
#' 
#' @return Character vector.
#' 
#' @details The \code{\%&\%} operator acts similar to BASIC's \code{&}, concatenating two elements together.
#' 
#' \code{string_prefix()} and its synonyms prefix a string to a vector, while \code{string_suffix()} and its synonyms suffix a string to a vector.
#' 
#' \code{string_flag()} is a scalar function that appends a character or number to a string if it does not meet a specified width.
#' 
#' \code{string_flagv()} is a vectorized version of \code{string_flag()}.
#'
#' @examples
#' "a" %&% "b"
#' string_prefix(rownames(mtcars), "A")
#' string_suffix(rownames(mtcars), "Z")
#' string_flag('123456789', '0', 10)
#' 
#' @seealso \url{https://github.com/robertschnitman/stringops}

#' @rdname string_concat
string_concat <- function(a, b) paste0(a, b)

#' @rdname string_concat2
`%&%` <- string_concat

#' @rdname string_prefix
string_prefix <- function(s, prefix) prefix %&% s
s_prefix      <- string_prefix
prefix        <- string_prefix

#' @rdname string_suffix
string_suffix <- function(s, suffix) s %&% suffix
s_suffix      <- string_suffix
suffix        <- string_suffix

#' @rdname string_flag
string_flag <- function(s, flag, width, which = c('left', 'right', 'both')) {
  
  # string_flag() is a scalar function
  
  # Check which input
  which <- match.arg(which)
  
  # Repeat flag a specified number of times.
  diff      <- width - nchar(s)
  diff_adj  <- ifelse(diff <= 0, 1, diff)
  
  flags <- string_join(rep(flag, diff_adj))
  
  flags <- ifelse(nchar(s) > width,
                  string_remove(flags, flags),
                  flags)
  
  # Special case for "both".
  if (which == 'both') {
    
    flags <- list(l = string_join(rep(flag, diff_adj/2)), 
                  r = string_join(rep(flag, diff_adj/2)))
    
  }
  
  # Output should be a string vector based on the "which" input.
  output <- switch(which,
                   left = flags %&% s,
                   right = s %&% flags,
                   both = flags$l %&% s %&% flags$r)
  
  output
  
}

#' @rdname string_flagv
string_flagv <- function(s, ...) {
  
  output <- sapply(s, function(x) string_flag(x, ...))
  
  names(output) <- NULL
  
  output
  
}
s_flagv <- string_flagv
flagv   <- string_flagv