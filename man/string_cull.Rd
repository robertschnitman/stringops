% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/string_cull.R
\name{string_cull}
\alias{string_cull}
\title{Cull/extract patterns from strings}
\usage{
string_cull(s, pattern, get_all = FALSE, collapse = ',', ...)
string_left(s, n)
string_right(s, n)
string_mid(s, start, n)
string_positions(s, positions, join = FALSE)
}
\arguments{
\item{s}{A string (character) vector.}

\item{pattern}{A regular expression pattern.}

\item{get_all}{Whether to get all matches (TRUE) or not (FALSE).}

\item{collapse}{A separator in the case of multiple matches.}

\item{...}{Inputs get passed to \code{gregexpr()}.}

\item{start}{The position in a string at which to start.}

\item{n}{The number of characters.}

\item{positions}{An integer vector of character positions to extract from a string.}

\item{join}{Determines whether to have columns for each position (\code{FALSE}) or join the extracted characters together (\code{TRUE}).}
}
\value{
Character vector.
}
\description{
Cull (extract) patterns from strings.
}
\details{
The function \code{string_cull()} culls (or extracts) a pattern from a string vector--if no pattern is found, \code{NA} is returned. Multiple pattern matches are separated by the \code{collapse()} input if \code{get_all = TRUE}.

The functions \code{string_left()}, \code{string_right()}, \code{string_mid()} act the same as Excel's \code{LEFT()}, \code{RIGHT}, \code{MID()} functions, respectively: they extract a number of characters at a specified starting point (from the beginning for \code{string_left()}, from the right for \code{string_right()}, and from a specific position for \code{string_mid()}).

The function \code{string_positions()} pulls the character(s) at specified positions. If \code{join = FALSE}, then the extracted characters are split into columns; otherwise, they are concatenated together into a single vector.

The synonym pattern of these functions are \code{s_*()} and \code{*()} (replace asterisks with cull, left, right, mid, and positions).
}
\examples{
string_cull(rownames(mtcars), '^M|a')
string_cull(rownames(mtcars), '^M|a', get_all = TRUE)
string_left(rownames(mtcars), 3)
string_right(rownames(mtcars), 3)
string_mid(rownames(mtcars), 2, 2)
string_positions(rownames(mtcars), c(1, 3, 5))
string_positions(rownames(mtcars), c(1, 3, 5), join = TRUE)

}
\seealso{
\url{https://github.com/robertschnitman/stringops}
}
