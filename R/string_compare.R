string_isblank <- function(s) ifelse(s == "" | grepl(" *", s), TRUE, FALSE)
s_isblank      <- string_isblank
isblank        <- string_isblank
