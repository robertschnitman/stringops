#' Miscellaneous stirng functions
#' 
#' @description Miscellaneous string functions due for a full explanation. Explore at your own risk!
#' 
#' @usage string_dup(s, times)
#' string_insert(s, insert, position)
#' string_split(s, split, ...)
#' string_len(s)
#' string_reverse(s)
#'
#' @examples
#' string_dup(rownames(mtcars), 3)
#' string_insert("abcd", "Z", 3)
#' string_len(rownames(mtcars))
#' string_reverse(rownames(mtcars))
#' 
#' @seealso \url{https://github.com/robertschnitman/stringops}

#' @rdname string_misc

#' @rdname string_dup
string_dup <- function(s, times) strrep(s, times)
s_dup      <- string_dup
dup        <- string_dup

#' @rdname string_insert
string_insert <- function(s, insert, position) {
  
  concat_insert <- function(x) {
    
    x[position] <- insert %&% x[position]
    
    x
    
  }
  
  splits <- lapply(strsplit(s, ""), concat_insert)
  
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
  
  splits <- lapply(strsplit(s, ""), rev)
  
  output <- sapply(splits, string_join)
  
  output
  
}
s_reverse <- string_reverse
reverse   <- string_reverse