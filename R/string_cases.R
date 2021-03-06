#' Modify cases for strings
#' 
#' @description Modify the case of the string.
#' 
#' @usage string_upper(s)
#' string_lower(s)
#' string_swapcase(s)
#' string_titlecase(s)
#' @param s A string (character) vector.
#' 
#' @return Character vector.
#' 
#' @details The \code{upper()} functions make all characters in a string vector in their capitalized case. 
#' 
#' The \code{string_lower()} function makes the same vector in their lower case. 
#' 
#' The \code{string_swapcase()} function switches the case for each character in a string vector.
#' 
#' The \code{string_titlecase()} function converts the first character of each word to uppercase.
#' 
#' The synonym pattern of these functions are \code{s_*()} and \code{*()} (replace asterisks with upper, lower, swapcase, or titlecase).
#'
#' @examples
#' string_upper(rownames(mtcars))
#' string_lower(rownames(mtcars))
#' string_swapcase(rownames(mtcars))
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

string_titlecase <- function(s) {
  
  # Split by word
  splits1 <- strsplit(tolower(s), " ")
  
  # Split by letter
  splits2 <- lapply(splits1, function(x) strsplit(x, ""))
  
  # Replace first letter of each word with the upper-case version.
  splits3 <- lapply(splits2, function(x) lapply(x, function(y) replace(y, 1, toupper(y[1]))))
  
  # Join the letters to reform the words.
  splits4 <- lapply(splits3, function(x) lapply(x, string_join))
  
  # Join the words, separated by a space.
  splits5 <- lapply(splits4, function(x) string_join(x, " "))
  
  # Output should remove the last separator.
  output  <- paste(splits5, sep = '')
  
  output  <- string_remove(output, ' $')
  
  output

}
s_titlecase <- string_titlecase
titlecase   <- string_titlecase

#' @rdname string_swapcase
string_swapcase <- function(s) {
  
  # Function to test the case of the letters.
  case_test <- function(x) ifelse(x %in% letters, toupper(x), tolower(x))
  
  # For each letter, switch their case.
  splits <- lapply(strsplit(s, ""), case_test)
  
  # Reform the words.
  output <- sapply(splits, string_join)
  
  output
  
}
s_swapcase <- string_swapcase
swapcase   <- string_swapcase
