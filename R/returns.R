#' @import tidyquant
#' @importFrom tidyr spread
#' @import xts
#' @importFrom tibble as_data_frame
#' @export
magrittr::`%>%`

#' @export
tidyquant::tq_get

#' @title Spread tq_get prices
#' @description A method to use with \link{tq_get} from the \link{tidyquant} package.
#' @param tbl The results from using \code{tq_get} with the option \code{get = "stock.prices"}
#' @param what a single \code{character} element from the following options
#' \code{"open", "close", "high", "low", "volume", "adjusted"}. Defaults to "close".
#' @param ... Additional parameters passed to the appropriate quantmod function. Common optional parameters include:
#' \itemize{
#'     \item \code{from} A character string representing a start date in YYYY-MM-DD format
#'     \item \code{to} A character string representing a end date in YYYY-MM-DD format.
#' }
#' @seealso \link{tq_get}, \link{tq_get_wide}
#' @details To use only after \code{tq_get}.
#' @return A data frame with the first column as the date and each other column representing a stock and each cell its price.
#' @export
tq_spread <- function(tbl, as_xts = FALSE, what = "close", ...) {
  if (!(what %in% c("open", "close", "high", "low", "volume", "adjusted"))) {
    stop('type must be one of "open", "close", "high", "low", "volume", "adjusted"')
  }
  price <- as_data_frame(tbl)
  if (names(tbl)[1] == "symbol") {
    price <- price %>%
      select_(.dots = c("date", "symbol", what)) %>%
      rename_(what = what) %>%
      spread(symbol, what)
  } else {
    price <- price %>%
      select_(.dots = c("date", what))
  }
  if (as_xts) {
    return(tidyquant::as_xts(price, date_col = date, check.names = FALSE))
  } else {
    return(data.frame(price, check.names = FALSE))
  }
  price
}



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
#' tq_get_wide(c("SPY", "SHV"), what = "volume", from = "2015-12-31")
#' @return A data frame with the first column as the date and each other column representing a stock and each cell its price.
#' @export
tq_get_wide <- function(x, what="close", ...) {

  if (!(what %in% c("open", "close", "high", "low", "volume", "adjusted"))) {
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
      select_(.dots = c("date", "symbol", what)) %>%
      `names<-`(c("date", "symbol", "price")) %>%
      spread(symbol, price) %>%
      as.data.frame(check.names = FALSE)
  } else {
    price <- price %>%
      select_(.dots = c("date", what)) %>%
      as.data.frame() %>%
      `names<-`(c("date", x))
  }

  price
}



#' @title Daily returns for wide-form price data
#' @description Daily returns for wide-form price data
#' @param df A data frame or xts with price data. If a data frame is given then the first column of date type will as
#' the index unless it is manually set in date_col
#' @param roll Rolling returns, "1 = no rolling"
#' @param type To be implemented
#' @param date_col To be implemented
#' @examples
#' tq_get_wide(c("SPY", "SHV")) %>% dly_ret()
#' @return An object of the same class of df with the returns.
#' @export
dly_ret <- function(df, roll = 1, accumulate = FALSE, date_col = NULL) {
  is_xts_df <- is.xts(df)
  if (!is_xts_df) {
    date_col <- ifelse(is.null(date_col), which(sapply(df, class) == "Date")[1],  date_col)
    if (is.na(date_col)) stop("No date column")
    col_names <- names(df)
    date_col_name <- col_names[date_col]
    df <- xts(df[ ,-date_col, drop = FALSE], order.by = df[ ,date_col])
    names(df) <- col_names[-date_col]
  }
  dly_ret <- df/xts::lag.xts(df)-1 # retornos
  dly_ret[is.na(dly_ret)] <- 0 # rellenar ceros
  if(roll > 1) dly_ret <- (rollProd(dly_ret, roll)+1)^(1/roll)-1
  if (accumulate) coredata(dly_ret) <- apply(dly_ret, 2, function(x) cumprod(x + 1) - 1)
  if (!is_xts_df) {
    dly_ret <- data.frame(fecha = index(dly_ret), dly_ret, check.names = FALSE, row.names = NULL)
    names(dly_ret)[1] <- date_col_name
    dly_ret <- dly_ret[ ,col_names]
  }
  return(dly_ret)
}









