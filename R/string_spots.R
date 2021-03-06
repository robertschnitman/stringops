#' Spot patterns in strings
#' 
#' @description Spot patterns in strings.
#' 
#' @usage string_spot(s, pattern, value = TRUE, ...)
#' string_spoti(s, pattern, ...)
#' string_spotl(s, pattern, ...)
#' string_spotm(s, pattern, invert = FALSE, ...)
#' string_replace(s, search, replace, ...)
#' string_remove(s, remove, ...)
#' string_countm(s, pattern, ...)
#' string_locate(s, pattern, invert = FALSE, ...)
#'
#' @param s A string (character) vector.
#' @param pattern A regular expression pattern.
#' @param value Boolean value (\code{TRUE/FALSE}) for whether to return the actual values (the former) or the indices (the latter).
#' @param invert Boolean value (\code{TRUE/FALSE}) for whether to find actual matches (\code{FALSE}; default) or non-matches (\code{TRUE}).
#' @param search A pattern to search.
#' @param replace The string to replace the searched pattern.
#' @param remove The string to remove.
#' @param ... Parameters to pass to \code{grep()}, \code{grepl()}, \code{gsub()}, or \code{gregexpr()}.
#' 
#' @return Character vector for all functions except the \code{string_spotl()}, \code{string_spoti()}, and \code{string_countm()} functions, which produce Boolean, numeric, and numeric vectors respectively.
#' 
#' @details The function \code{string_spot()} subsets a vector to the values matching a given pattern. Optional inputs get passed to \code{grep()}. Synonyms are \code{s_spot()} and \code{spot()}.
#' 
#' The function \code{string_spoti()} subsets a vector to the indices matching a given pattern. Optional inputs get passed to \code{grep()}. Synonyms are \code{s_spoti()} and \code{spoti()}.
#' 
#' The function \code{string_spotl()} detects whether a pattern exists in a vector, outputting a Boolean value (\code{TRUE/FALSE}). Optional inputs get passed to \code{grepl()}. Synonyms are \code{s_spotl()} and \code{spotl()}.
#' 
#' The \code{string_spotm()} function spots pattern matches and returns \code{NA} if none are found. Optional inputs get passed to \code{grepl()}. Synonyms are \code{s_spotm()} and \code{spotm()}.
#' 
#' The \code{string_replace()} function acts the same as \code{gsub()} with the inputs ordered differently. Optional inputs get passed to \code{gsub()}. Synonyms are \code{s_replace()}, \code{search_replace()}, \code{sr()}, \code{find_replace()}, and \code{fr()}.
#' 
#' The \code{string_remove()} function blanks out a matching pattern. Optional inputs get passed to \code{gsub()}. Synonym is \code{s_remove()}.
#' 
#' The \code{string_countm()} function counts the number of matches in a string. Optional inputs get passed to \code{gregexpr()}. Synonyms are \code{s_countm()} and \code{countm()}.
#' 
#' The \code{string_locate()} functions produces the character positions at where the pattern match are found--non-matches are produced if \code{invert = TRUE}. Optional inputs are passed to \code{gregexpr()}. Synonyms are \code{s_locate()} and \code{locate()}.
#'
#' @examples
#' rn <- rownames(mtcars)
#' 
#' string_spot(rn, "^M")
#' string_spot(rn, '^M', invert = TRUE)
#' string_spoti(rn, "^M")
#' string_spotl(rn, "^M")
#' string_spotm(rn, "^M")
#' 
#' string_replace(rn, "^M", "Z")
#' string_remove(rn, "^M")
#' string_countm(rn, 'a')
#' 
#' string_locate(rn, 'a')
#' string_locate(rn, 'a', invert = TRUE)
#' 
#' 
#' @seealso \url{https://github.com/robertschnitman/stringops}

#' @rdname string_spots

#' @rdname string_spot
string_spot  <- function(s, pattern, value = TRUE, ...) {
  
  # Input must be a vector
  stopifnot(is.character(s))
  
  # Spot should behave similarly as grep.
  output <- grep(pattern, s, value = value, ...)
  
  output
  
}
s_spot       <- string_spot
spot         <- string_spot

#' @rdname string_spoti
string_spoti <- function(s, pattern, ...) string_spot(s, pattern, value = FALSE, ...)
s_spoti      <- string_spoti
spoti        <- string_spoti

#' @rdname string_spotl
string_spotl <- function(s, pattern, ...) {
  
  output <- grepl(pattern, s, ...)
  
  output
  
}
s_spotl      <- string_spotl
spotl        <- string_spotl

#' @rdname string_spotm
string_spotm <- function(s, pattern, invert = FALSE, ...) {
  
  stopifnot(is.character(s))
  
  if (invert == FALSE) {
    
    output <- ifelse(grepl(pattern, s), s, NA_character_)
    
  } else {
    
    output <- ifelse(!grepl(pattern, s), s, NA_character_)
    
  }
  
  output
  
}
s_spotm      <- string_spotm
spotm        <- string_spotm

#' @rdname string_replace
string_replace <- function(s, search, replace, ...) gsub(search, replace, s, ...)
s_replace      <- string_replace
find_replace   <- string_replace
fr             <- string_replace
search_replace <- string_replace
sr             <- string_replace

#' @rdname string_remove
string_remove <- function(s, remove, ...) {
  
  stopifnot(is.character(s))
  
  gsub(remove, "", s, ...)
  
}
s_remove      <- string_remove

#' @rdname string_count
string_countm <- function(s, pattern, ...) {
  
  stopifnot(is.character(s))
  
  # https://stringr.tidyverse.org/articles/from-base.html#overall-differences-1
  locations <- gregexpr(pattern = pattern, text = s, ...)
  
  matches   <- lapply(locations, function(x) attr(x, "match.length"))
  
  # -1 for no matches
  output <- sapply(matches, function(x) sum(ifelse(x == -1, 0, 1))) 
  
  output
  
}
s_countm <- string_countm
countm   <- string_countm

#' @rdname string_locate
string_locate <- function(s, pattern, invert = FALSE, ...) {
  
  # type-check
  stopifnot(is.character(s))
  
  # Get locations
  locations <- gregexpr(pattern, s, ...)
  
  locations2 <- lapply(locations, function(x) x[seq_along(x)])
  
  # Output contingent on invert input.
  
  if (invert == FALSE) {
    
    output <- lapply(locations2, function(x) ifelse(x == -1, NA_character_, x))
    
  } else {
    
    num_char <- string_len(s)
    
    positions <- sapply(num_char, function(x) 1:x)
    
    output <- Map(function(x, y) x[!x %in% y], positions, locations2)
    
  }
  
  output
  
}

s_locate <- string_locate
locate   <- string_locate