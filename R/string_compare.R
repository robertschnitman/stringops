#' Compare string vectors.
#' 
#' @description Compare string vectors.
#' 
#' @usage %like%
#' string_isblank(s)
#' string_isupper(s)
#' string_islower(s)
#' 
#' @param s A string (character) vector.
#' 
#' @return Boolean vector.
#' 
#' @details The function \code{string_isblank()} tests whether a vector consists of only blank characters. Synonyms are \code{s_isblank()}, \code{isblank()}, and \code{is.blank()}.
#' 
#' The \code{\%like\%} operator tests whether a pattern match is found in a string. 
#' 
#' The functions \code{string_isupper()} and \code{string_islower()} test whether each element in a string vector is all uppercase or lowercase, respectively. Synonyms are \code{s_fun()}, \code{isfun()}, and \code{is.fun()} (replace "fun" with "upper" or "lower").
#' 
#' @examples
#' string_isblank("         ")
#' string_isblank("    string     ")
#' 
#' rownames(mtcars) %like% "^M"
#' 
#' string_isupper(c('TEST', 'test', 'tEsT'))
#' string_islower(c('TEST', 'test', 'tEsT'))
#' 
#' @seealso \url{https://github.com/robertschnitman/stringops}

#' @rdname string_compare

#' @rdname string_isblank
string_isblank <- function(s) trimws(s) == ""
s_isblank      <- string_isblank
isblank        <- string_isblank
is.blank       <- string_isblank

#' @rdname like
`%like%` <- function(s, pattern) grepl(pattern, s)

#' @rdname is.upper
split_apply <- function(f, x, apply_type) {
  
  ### GOAL: Split --> Apply Function --> Combine
  ## The "Split-Apply-Combine" strategy by Hadley Wickham.
  # https://www.jstatsoft.org/article/view/v040i01/v40i01.pdf
  
  # 0. Type check.
  stopifnot(class(x) == 'character')
  
  # 1. Split.
  splits <- sapply(x, strsplit, split = NULL)
  
  # 2. Apply & Combine.
  
  if (apply_type == 'lapply') {
    
    apps <- lapply(splits, f)
    
    names(apps) <- NULL # Juxtaposition of original attributes and applied vector is confusing.
    
    output <- sapply(apps, paste0, sep = '', collapse = '')
    
  } else if (apply_type == 'mapply') { # if sub-block above converts boolean vectors to string.
    
    output <- mapply(f, splits)
    
    names(output) <- NULL # Juxtaposition of original attributes and applied vector is confusing.
    
    
  } else {
    
    stop('Invalid apply function: use either lapply or mapply')
    
  }
  
  output
  
}

is.upper <- function(x) split_apply(function(x) all(x %in% LETTERS), x, apply_type = 'mapply')
is.lower <- function(x) split_apply(function(x) all(x %in% letters), x, apply_type = 'mapply')

string_isupper <- is.upper
s_isupper      <- is.upper
isupper        <- is.upper

string_islower <- is.lower
s_islower      <- is.lower
islower        <- is.lower

