#' Miscellaneous string functions
#' 
#' @description Miscellaneous string functions that don't quite fit into the other categories.
#' 
#' @usage string_dup(s, times)
#' string_insert(s, insert, position)
#' string_split(s, split, ...)
#' string_len(s)
#' string_reverse(s)
#' string_unique(s)
#' 
#' @param s A string (character) vector.
#' @param times The number of times to duplicate a string.
#' @param insert The string to insert.
#' @param position The position index to which to insert the string.
#' @param split The character by which to split a string vector.
#' @param ... Parameters passed to \code{strsplit()}.
#' 
#' @details \code{string_dup()} acts the same as strrep(): it repeats a string n number of times. 
#' 
#' \code{string_insert()} inserts a string at a specified position.
#'  
#' \code{string_len()} acts the same as nchar(): it counts the number of characters per vector element.
#' 
#' \code{string_reverse()} reverses the characters in a string.
#' 
#' \code{string_split()} acts the same as \code{strsplit()}: it splits a string by a specified character.
#' 
#' \code{string_unique()} obtains the distinct characters for each element in a vector.
#'
#' @examples
#' string_dup(rownames(mtcars), 3)
#' string_insert("abcd", "Z", 3)
#' string_len(rownames(mtcars))
#' string_reverse(rownames(mtcars))
#' string_split(rownames(mtcars), " ")
#' string_unique(rownames(mtcars))
#' 
#' @seealso \url{https://github.com/robertschnitman/stringops}

#' @rdname string_misc

#' @rdname string_dup
string_dup <- function(s, times) strrep(s, times)
s_dup      <- string_dup
dup        <- string_dup

#' @rdname string_insert
string_insert <- function(s, insert, position) {
  
  # Function that inserts a character at a specified position
  concat_insert <- function(x) {
    
    x[position] <- insert %&% x[position]
    
    x
    
  }
  
  # For each element, insert a string at the specified position.
  splits <- lapply(strsplit(s, ""), concat_insert)
  
  # Reform the vector.
  output <- sapply(splits, string_join)
  
  output
  
}

#' @rdname string_split
string_split <- function(s, split, ...) strsplit(s, split, ...)
s_split      <- string_split

#' @rdname string_len
string_len <- nchar
s_len      <- string_len
len        <- string_len

#' @rdname string_reverse
string_reverse <- function(s) {
  
  # Split and reverse letters
  splits <- lapply(strsplit(s, ""), rev)
  
  # Combine
  output <- sapply(splits, string_join)
  
  output
  
}
s_reverse <- string_reverse
reverse   <- string_reverse

#' @rdname string_unique
string_unique <- function(s) {
  
  # Split by each character.
  splits <- string_split(s, "")
  
  # Get unique characters per element.
  splits_unique <- lapply(splits, unique)
  
  # Get a single string for each list element.
  splits_join <- lapply(splits_unique, string_join)
  
  # Flatten list to a vector
  output <- string_flatten(splits_join)
  
  # Output should be a vector.
  output
  
}
s_unique <- string_unique