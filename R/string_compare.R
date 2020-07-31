string_isblank <- function(s) trimws(s) == ""
s_isblank      <- string_isblank
isblank        <- string_isblank

`%like%` <- function(s, pattern) grepl(pattern, s)
