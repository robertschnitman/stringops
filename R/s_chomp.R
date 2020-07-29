string_chomp <- function(s) gsub(" |\n|\t", "", s)
s_chomp      <- string_chomp
chomp        <- string_chomp

string_chop <- function(s) substr(s, 1, nchar(s) - 1)
s_chop      <- string_chop
chop        <- string_chop