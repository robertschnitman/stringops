string_cull <- function(s, pattern) {
  
  
  
}
s_cull <- string_cull
cull   <- string_cull

string_left <- function(s, n) substr(s, 1, n)
s_left      <- string_left
left        <- string_left

string_right <- function(s, n) substr(s, nchar(s) - n + 1, nchar(s))
s_right      <- string_right
right        <- string_right

string_mid <- substr
s_mid      <- string_mid
mid        <- string_mid