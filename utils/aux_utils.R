#### Packages ####
library(kableExtra)
library(reshape2)
library(dplyr)
library(ggplot2)
library(beepr)
library(cli)


#### Auxiliary Functions to Label Variables ####
z.lab <- function(variable) paste0(zlabs, '_', variable) # e.g. z.lab('c') returns c('Z1_c', 'Z2_c')
zk.lab <- function(variable) {
  unlist(lapply(zlabs, function(z) paste0(z, '_', variable, '-', 1:K)))
} # e.g. returns industry-level variables
zk.sum <- function(vec) {
  sapply(seq_len(N), function(i) {
    sum(vec[
      seq(1, by = K, length.out = N)[i]:(seq(1, by = K, length.out = N) +
        K -
        1)[i]
    ])
  })
}
# zk.sum <- function (vec) array(sapply(mapply(seq, cumsum(c(0, K[-length(K)])) + 1, cumsum(K), SIMPLIFY = FALSE), function (x) sum(vec[x])), dim = N, dimnames = list(zlabs)) # sums industry-level variables into country-level (i.e. from KN to N)
# zk.mean <- function (vec) array(sapply(mapply(seq, cumsum(c(0, K[-length(K)])) + 1, cumsum(K), SIMPLIFY = FALSE), function (x) mean(vec[x])), dim = N, dimnames = list(zlabs)) # sums industry-level variables into country-level (i.e. from KN to N)
zk.mean <- function(vec) {
  sapply(seq_len(N), function(i) {
    mean(vec[
      (seq(1, by = K, length.out = N))[i]:(seq(1, by = K, length.out = N) +
        K -
        1)[i]
    ])
  })
} # sums industry-level variables into country-level (i.e. from KN to N)
rev.zk.lab <- function(variable) {
  unlist(lapply(rev(zlabs), function(z) paste0(z, '_', variable, '-', 1:K)))
} # e.g. returns reverse industry-level variables
# rev.zk.lab <- function (variable) unlist(lapply(rev(zlabs), function (z) paste0(z, '_', variable, '-', 1 : K[z]))) # e.g. returns reverse industry-level variables

insert.line.break <- function(x) {
  if (nchar(x) > 50) {
    # Find the approximate middle of the string
    middle <- nchar(x) %/% 2
    # Find the nearest space to the middle
    space_position <- regexpr(" ", x, fixed = TRUE)[1]
    if (space_position < 0) {
      return(x)
    } # Return the original string if no space is found

    # Find the closest space to the middle of the string
    closest_space <- which.min(abs(gregexpr(" ", x)[[1]] - middle))
    space_index <- gregexpr(" ", x)[[1]][closest_space]

    # Insert line break at the space closest to the middle
    x <- sub(paste0("^(.{", space_index - 1, "})( )"), "\\1\n", x)
  }
  return(x)
}
