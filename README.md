# `stringops`: String-processing Tools for R

When creating syntax, one has to ask themselves about the naming scheme:
should I make the functions short for typing efficiency, or long for
increased readability? Ruby has the former benefit, but sometimes the
methods can be difficult to remember (e.g. is it `len` or `length`? Is
it `swapcase` or `swap_case`?), as there isn’t a consistent naming
scheme–however, some functions have synonyms to help those from other
programming languages learn Ruby faster (e.g. `reduce` and `inject` do
the same thing). On the other hand, the `stringr` library has a
consisent naming scheme for its functions, but does not have synonyms,
so you are forced to learn the `stringr` way. Thirdly, R does not have
concatenation operator (only functions) like in Ruby and BASIC, which is
odd, as many situations require concatenation; so using the
`paste/paste0()` functions can make code less readable. As such, I am
introducing a new package to take these considerations into account:
`stringops`, a library consisting of tools for processing strings in R.

What this package brings are (1) a consistent naming-scheme for
functions, (2) synonyms for said functions, and (3) a concatenation
operator. The first item benefits users of all skill levels, as it makes
certain functions easier to remember while making use of RStudio’s
predictive text. The second item is useful when one tires of typing
`string_cull()`, for example, and wishes to use a shorthand to simplify
the code (in this case, the shorthand would be `cull()`). The third
item’s benefit is more readable code by avoiding the function syntax of
`paste/paste0()`. Ultimately, these items will hopefully make processing
strings in R more fun for the user!

# Installation

This package currently is only available on GitHub–there are no plans to
submit this package to CRAN at this time. As such, please use the
`devtools` library to install `stringops`.

``` r
# install.packages('devtools')

devtools::install_github('robertschnitman/stringops')
```

``` r
library(stringops)
```
