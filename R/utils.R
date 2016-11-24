#' @title Create asset id
#' @description Creates a unique if from the tipo, emisora, and serie of an asset. Input may be vector but must all have the same length;
#' if they are not, it will not neccesarily yield an error but undesired results may occur.
#' @param tipo character vector of tipo
#' @param emisora character vector of emisora
#' @param serie character vector of serie
#' @return the unique id of the asset
#' @examples
#' asset_id("M", "BONOS", "191211")
#' @export
asset_id <- function(tipo, emisora, serie) {
  tipo[nchar(tipo) == 1] <- paste0(tipo[nchar(tipo) == 1], "_")
  id <- tolower(paste0(tipo, emisora, serie))
  id <- gsub("\\*|\\+|\\-", "_", id)
  id
}

#' @title Map a variable
#' @description Sometimes we need to map one vector into another. For inhouse data it can be useful when defining categories for
#' carteras modelo, id's or emisoras. This function maps \code{origin -> target} for every value of x.
#' @param x The vector that will be transformed; x must have values in \code{origin}.
#' @param origin A vector
#' @param target A vector of the same length as origin
#' @param replace_missing Indicates whether the values of \code{x} that are not found \code{origin} should be changed.
#' If \code{replace_missing} is given a value other than \code{FALSE} then this value will be used as replacement for the
#' values of \code{x} not found in \code{origin}.
#' @return A transformed vector
#' @examples
#' map(1:6, c(2,3), c(-1, -2))
#' map(1:6, c(2,3), c(-1, -2), replace_missing = NA)
#' @export
map <- function(x, origin, target, replace_missing = FALSE) {
  if (length(origin) != length(target)) stop("origin and target must have the same length")
  matching <- match(x, origin)
  x_new <- x
  x_new[!is.na(matching)] <- target[matching[which(!is.na(matching))]]
  if (!identical(replace_missing, FALSE)) {
    x_new[is.na(matching)] <- replace_missing
  }
  x_new
}

