#' Concatenate strings together
#' 
#' @description Concatenate strings together.
#' 
#' @usage %&%
#' string_prefix(s, prefix)
#' s_prefix(s, prefix)
#' prefix(s, prefix)
#' string_suffix(s, suffix)
#' s_suffix(s, suffix)
#' suffix(s, suffix)
#'
#' @param s A string (character) vector.
#' @param collapse A string by which to separate the elements of a vector.
#' 
#' @return The \code{%&%} operator acts similar to BASIC's \code{&}, concatenating two elements together.
#' 
#' \code{string_prefix} and its synonyms prefix a string to a vector, while \code{string_suffix} and its synonyms suffix a string to a vector.
#'
#' @examples
#' "a" %&% "b"
#' string_prefix(rownames(mtcars), "A")
#' string_suffix(rownames(mtcars), "Z")
#' 
#' @seealso \url{https://github.com/robertschnitman/stringops}

#' @rdname string_concat
string_concat <- function(a, b) paste0(a, b)
`%&%` <- string_concat

#' @rdname string_prefix
string_prefix <- function(s, prefix) prefix %&% s
s_prefix      <- string_prefix
prefix        <- string_prefix

#' @rdname string_suffix
string_suffix <- function(s, suffix) s %&% suffix
s_suffix      <- string_suffix
suffix        <- string_suffix