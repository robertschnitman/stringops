string_chomp <- function(s) gsub(" |\n|\t|\r", "", s)
s_chomp      <- string_chomp
chomp        <- string_chomp

string_chop <- function(s) substr(s, 1, nchar(s) - 1)
s_chop      <- string_chop
chop        <- string_chop

string_trim <- function(s, which = c('both', 'left', 'right'), whitespace = '[ \t\r\n]') {
  
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
  cond_both <- "^" %&% whitespace %&% "|" %&% whitespace %&% "+$"
  
  # Output should be based on user input for "which".
  output <- switch_out(which,
                       left  = sub_out(s, cond_beg),
                       right = sub_out(s, cond_end),
                       both  = sub_out(s, cond_both))
  
  # Initial output is a matrix, so flatten it to a vector so that 
  # users can use string_trim() on dataframe columns, for example.
  output <- as.vector(output)
  
  output
  
}
s_trim      <- string_trim
trim        <- string_trim