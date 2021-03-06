#' Remove (empty) characters from a string as inspired by Ruby
#' 
#' @description Remove (empty) characters from a string as inspired by Ruby.
#' 
#' @usage string_chomp(s)
#' string_chop(s)
#' string_trim(s, which, whitespace)
#' string_cut(s, which)
#'
#' @param s A string (character) vector.
#' @param which Denotes how to trim/cut a vector (both, left only, or right only).
#' @param whitespace Denotes which whitespace characters to remove.
#' 
#' @details As inspired by Ruby, \code{string_chomp()} removes all whitespace characters in a string. 
#' 
#' The function \code{string_chop()} removes the last character from a string. 
#' 
#' The function \code{string_trim()} acts the same as \code{trimws} but its source code is more readable (in my opinion!).
#' 
#' \code{string_cut()} removes characters from either the left, right, or both ends of a string.
#' 
#' The synonym pattern of these functions are \code{s_*()} and \code{*()} (replace asterisks with upper, chomp, chop, and trim. \code{s_cut()} is the only synonym for \code{string_cut()}).
#' 
#' @return Character vector.
#'
#' @examples
#' string_chomp(rownames(mtcars))
#' string_chop(rownames(mtcars))
#' string_trim("   s   ")
#' string_cut("cut this please", which = 'both')
#' 
#' @seealso \url{https://github.com/robertschnitman/stringops}

#' @rdname string_chomp

string_chomp <- function(s) gsub(" |\n|\t|\r", "", s)
s_chomp      <- string_chomp
chomp        <- string_chomp

#' @rdname string_chop
string_chop <- function(s) substr(s, 1, nchar(s) - 1)
s_chop      <- string_chop
chop        <- string_chop

#' @rdname string_cut
string_cut <- function(s, which = c('both', 'left', 'right')) {
  
  stopifnot(is.character(s))
  
  # Check "which" inputs.
  which <- match.arg(which)
  
  # Different operation for each "which" input.
  if (which == 'left') {
    
    output <- substr(s, 2, nchar(s))
    
  } else if (which == 'right') {
    
    output <- string_chop(s)
    
  } else if (which == 'both') {
    
    output <- substr(s, 2, nchar(s) - 1)
    
  } 
  
  output
  
}
s_cut <- string_cut

#' @rdname string_trim
string_trim <- function(s, which = c('both', 'left', 'right'), whitespace = '[ \t\r\n]') {
  
  stopifnot(is.character(s))
  
  # Make sure arguments match "which".
  which <- match.arg(which)
  
  # Define functions for easier referencing.
  ## Remove specific patterns
  sub_out    <- function(x, r) gsub(r, '', x, perl = TRUE)
  
  ## vectorize switch so that we can apply the substitutions in a vectorized manner. 
  switch_out <- function(x, ...) sapply(x, switch, ...)
  
  # Conditions for each "which" input.
  cond_beg  <- '^' %&% whitespace %&% '+'
  cond_end  <- whitespace %&% '+$'
  cond_both <- "^" %&% whitespace %&% "*|" %&% whitespace %&% "*+$"
  
  # Output should be based on user input for "which".
  output <- switch_out(which,
                       left  = sub_out(s, cond_beg),
                       right = sub_out(s, cond_end),
                       both  = sub_out(s, cond_both))
  
  # Initial output is a matrix, so flatten it to a vector so that 
  #   users can use string_trim() on dataframe columns, for example.
  output <- as.vector(output)
  
  output
  
}
s_trim      <- string_trim
trim        <- string_trim