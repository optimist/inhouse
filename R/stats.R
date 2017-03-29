# OPTIMIST STATISTICAL FUNCTIONS

#' @title Halflife-weighted mean
#' @details Uses geometric decay weighting where the last observations has weight \code{1/halflife}, with
#' geometric decay. The last \code{halflife} observations have weight \code{(1/halflife)^(size - 1)}.
#' @param size The length of the array
#' @param halflife The parameter for geometric time-weighting
#' @return A vector of length \code{size} with weights with total sum \code{1},
#' decaying geometrically with a factor of \code{1/halflife} and equal weight at the tail.
#' @export
optimist_wt <- function(size, halflife){
  if(size < halflife){
    wt <- rep(1/size, size)
  } else{
    wt <- sapply(
      size:1, function(x) ifelse(
        x > (size-halflife),
        (1/halflife)*((halflife-1)/halflife)^(size-halflife),
        (1/halflife)*((halflife-1)/halflife)^(x-1)
      )
    )
  }
  return(wt)
}

#' @name OptimistFunctions
#' @rdname OptimistFunctions
#' @title Statistical functions with halflife for financial analysis
#' @param object x
#' @param halflife the value for time-weighting. See \link{optimist_wt}.
#' @return mean, variance and convariance matrix of \code{x} with halflife \code{h}.
NULL


#' @rdname OptimistFunctions
#' @export
optimist_mean <- function(x, halflife = length(x), type=c("arithmetic", "geometric")) {
  type <- type[1]
  wt <- optimist_wt(length(x), halflife)
  stopifnot(type[1] %in% c("arithmetic", "geometric"))
  if (type[1] == "arithmetic") {
    return(sum(wt*x))
  }
  if (type[1] == "geometric") {
    return(exp(sum(wt*log(x + 1))) - 1)
  }
}

#' @rdname OptimistFunctions
#' @export
optimist_var <- function(x, halflife = length(x)){
  mean <- optimist_mean(x, halflife)
  sq <- x^2
  wt <- optimist_wt(length(x), halflife)
  var <- (sum(wt*sq)-mean^2)/(1-sum(wt^2)) # el coeficiente es para arreglar el sesgo
  return(var)
}


#' @rdname OptimistFunctions
#' @export
optimist_cov <- function(x, halflife = nrow(as.matrix(x)), cor = FALSE){
  x <- as.matrix(x)
  col_means <- apply(x, 2, optimist_mean, halflife)
  x <- t(apply(x, 1, function(x) x - col_means))
  wt <- optimist_wt(nrow(x), halflife)
  x2 <- apply(x, 2, function(x) x * wt)
  mat <- (t(x) %*% x ) / (1-sum(wt^2)) # el coeficiente es para arreglar el sesgo
  if (cor) {
    col_sd <- sqrt(diag(mat))
    mat <- t(apply(mat, 1, function(x) x/col_sd))
    mat <- apply(mat, 2, function(x) x/col_sd)
  }
  return(mat)
}
