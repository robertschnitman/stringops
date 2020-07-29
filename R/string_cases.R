string_upper <- function(s) toupper(s)
s_upper      <- string_upper
upper        <- string_upper

string_lower <- function(s) tolower(s)
s_lower      <- string_lower
lower        <- string_lower

#string_title <- function(s) {
  
 # splits <- lapply(strsplit(s, " "))
  
#}
#s_title
#title

string_swapcase <- function(s) {
  
  case_test <- function(x) ifelse(x %in% letters, toupper(x), tolower(x))
  
  splits <- lapply(strsplit(s, ""), case_test)
  
  output <- sapply(splits, string_join)
  
  output
  
}
s_swapcase <- string_swapcase
swapcase   <- string_swapcase