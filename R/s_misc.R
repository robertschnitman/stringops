string_dup <- strrep
s_dup      <- string_dup
dup        <- string_dup

string_insert <- function(s, insert, position) {
  
  concat_insert <- function(x) {
    
    x[position] <- insert %&% x[position]
    
    x
    
  }
  
  splits <- lapply(strsplit(s, ""), concat_insert)
  
  output <- sapply(splits, string_join)
  
  output
  
}

string_split <- strsplit
s_split      <- string_split

string_len <- nchar
s_len      <- string_len
len        <- string_len

string_reverse <- function(s) {
  
  splits <- lapply(strsplit(s, ""), rev)
  
  output <- sapply(splits, string_join)
  
  output
  
}
s_reverse <- string_reverse
reverse   <- string_reverse