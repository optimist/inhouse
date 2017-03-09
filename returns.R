#' @import tidyquant
#' @import tidyr
#'
#' @export
magrittr::`%>%`

#' @title Yahoo Data Wide
#' @description Get yahoo price data in wide form for a vector of stocks.
#' @param x A single character string, a character vector or data frame
#' representing a single or multiple stock index, stock symbol, metal symbol, currency combination
#' @param type a single \code{character} element from the following options
#' \code{"open", "close", "high", "low", "volume", "adjusted"}. Defaults to "close".
#' @param ... Additional parameters passed to the appropriate quantmod function. Common optional parameters include:
#' \itemize{
#'     \item \code{from} A character string representing a start date in YYYY-MM-DD format
#'     \item \code{to} A character string representing a end date in YYYY-MM-DD format.
#' }
#' @seealso \link{tq_get}, \link{tq_spread}
#' @examples
#' tq_get_wide(c("SPY", "SHV"))
#' tq_get_wide(c("SPY", "SHV"), type = "volume", from = "2015-12-31")
#' @return A data frame with the first column as the date and each other column representing a stock and each cell its price.
#' @export
tq_get_wide <- function(x, type="close", ...) {

  if (!(type %in% c("open", "close", "high", "low", "volume", "adjusted"))) {
    stop('type must be one of "open", "close", "high", "low", "volume", "adjusted"')
  }
  if (length(x) == 0 | !is.character(x)) {
    stop('x must be a character vector of length greater than 1')
  }
  x <- spl(x)
  price <- x %>%
    tidyquant::tq_get(get = "stock.prices", ...)
  if (length(x) > 1) {
    price <- price %>%
      select_(.dots = c("date", "symbol", type)) %>%
      `names<-`(c("date", "symbol", "price")) %>%
      tidyr::spread(symbol, price) %>%
      as.data.frame(check.names = FALSE)
  } else {
    price <- price %>%
      select_(.dots = c("date", type)) %>%
      as.data.frame() %>%
      `names<-`(c("date", x))
  }
  price
}

#' @title Spread tq_get prices
#' @description A method to use with \link{tq_get} from the \link{tidyquant} package.
#' @param tbl The results from using \code{tq_get} with the option \code{get = "stock.prices"}
#' @param type a single \code{character} element from the following options
#' \code{"open", "close", "high", "low", "volume", "adjusted"}. Defaults to "close".
#' @param ... Additional parameters passed to the appropriate quantmod function. Common optional parameters include:
#' \itemize{
#'     \item \code{from} A character string representing a start date in YYYY-MM-DD format
#'     \item \code{to} A character string representing a end date in YYYY-MM-DD format.
#' }
#' @seealso \link{tq_get}, \link{tq_get_wide}
#' @examples
#' tq_get(c("SPY", "SHV")) %>% tq_spread()
#' tq_get(c("SPY", "SHV"), type = "volume", from = "2015-12-31") %>% tq_spread()
#' @details To use only after \code{tq_get}.
#' @return A data frame with the first column as the date and each other column representing a stock and each cell its price.
#' @export
tq_spread <- function(tbl, type="close", ...) {
  if (!(type %in% c("open", "close", "high", "low", "volume", "adjusted"))) {
    stop('type must be one of "open", "close", "high", "low", "volume", "adjusted"')
  }
  price <- tbl
  if (names(tbl)[1] == "symbol") {
    price <- price %>%
      select_(.dots = c("date", "symbol", type)) %>%
      tidyr::spread(symbol, close) %>%
      as.data.frame(check.names = FALSE)
  } else {
    price <- price %>%
      select_(.dots = c("date", type)) %>%
      as.data.frame(check.names = FALSE)
  }
  price
}


#' @title Daily returns for wide-form price data
#' @description Daily returns for wide-form price data
#' @param df A data frame or xts with price data. If a data frame is given then the first column must include
#' the dates
#' @param halflife To be implemented
#' @param rolling To be implemented
#' @examples
#' tq_get(c("SPY", "SHV")) %>% tq_ret()
#' @return An object of the same class of df with the returns
#' @export
tq_dly_ret <- function(tbl, type="close", ...) {
  if (!(type %in% c("open", "close", "high", "low", "volume", "adjusted"))) {
    stop('type must be one of "open", "close", "high", "low", "volume", "adjusted"')
  }
  price <- tbl
  if (names(tbl)[1] == "symbol") {
    price <- price %>%
      select_(.dots = c("date", "symbol", type)) %>%
      tidyr::spread(symbol, close) %>%
      as.data.frame(check.names = FALSE)
  } else {
    price <- price %>%
      select_(.dots = c("date", type)) %>%
      as.data.frame(check.names = FALSE)
  }
  price
}
