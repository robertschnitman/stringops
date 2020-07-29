`%&%` <- function(a, b) paste0(a, b)

string_prefix <- function(x, prefix) prefix %&% x
s_prefix      <- string_prefix
prefix        <- string_prefix

string_suffix <- function(x, suffix) x %&% suffix
s_suffix      <- string_suffix
suffix        <- string_suffix