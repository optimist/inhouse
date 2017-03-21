#' @import xts

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

#' @title Split string  by symbol
#' @description Splits a string by a symbol, trims whitespace and unlists
#' @param x single string
#' @param sep symbol to split, defaults to ','
#' @return x splitted and unlisted into vector
#' @seealso \link{str_split}
#' @examples
#' spl("SPY,SHV")
#' @export
spl <- function(x, sep =",") {
  if (length(x) != 1) stop("x must have length 1")
  gsub("[ ]+", "", unlist(stringr::str_split(x, pattern = sep)))
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
#' keymapping(1:6, c(2,3), c(-1, -2))
#' keymapping(1:6, c(2,3), c(-1, -2), replace_missing = NA)
#' @export
keymapping <- function(x, origin, target, replace_missing = FALSE) {
  if (length(origin) != length(target)) stop("origin and target must have the same length")
  matching <- match(x, origin)
  x_new <- x
  x_new[!is.na(matching)] <- target[matching[which(!is.na(matching))]]
  if (!identical(replace_missing, FALSE)) {
    x_new[is.na(matching)] <- replace_missing
  }
  x_new
}


#' @title Convenient date sequences
#' @description A wrapper for seq.Date in more convenient form
#' @param period A character of the form 'date1/date2', where the date1 and date2 are converted do Date objects using \code{lubridate}.
#' @param by an integer specifying the space between dates in the sequence
#' @details A convenient wrapper for seq.date that does not requite tu input Dates and accepts other date formats using \code{ymd} from \code{lubridate}.
#' The syntax is inspired on the indexing format of xts objects.
#' @return A sequence of dates
#' @examples
#' date_seq("20151231/")
#' date_seq("20151231/20161130", by = 2)
#' @export
date_seq <- function(period, by = 1) {
  fromto <- unlist(strsplit(period, "/"))
  if(length(fromto) == 1) {
    fromto[2] <- as.character(Sys.Date())
  }
  seq.Date(from = lubridate::ymd(fromto[1]), to = lubridate::ymd(fromto[2]), by = by)
}


#' @title Fill data frame or xts
#' @description Fills missing data in convenient form for Finance
#' @param df data.frame or xts with data to fill
#' @param fill_missing_init A boolean indicating whether starting missing values should be copied to first actual value
#' @return Filled data frame
#' @examples
#' fill_prices(tq_get_wide("SPY,MXN=X"))
#' @export
fill_prices <- function(df, fill_missing_init = TRUE, date_col = NULL) {
  if (is.xts(df)) {
    dat <- data.frame(df)
  } else {
    date_col <- ifelse(is.null(date_col), which(sapply(df, class) == "Date")[1],  date_col)
    dat <- df[order(df[ ,date_col]), ]
  }
  filled_dat <- tidyr::fill_(dat, names(dat), .direction = "down")
  print(filled_dat)
  if (fill_missing_init) {
    filled_dat <- tidyr::fill_(filled_dat, names(filled_dat), .direction = "up")
  }
  if (is.xts(df)) {
    filled_df <- df
    coredata(filled_df) <- as.matrix(filled_dat)
  } else {
    filled_df <- filled_dat
  }
  filled_df
}

#' @title Rolling product for xts
#' @description Wrapper of RcppRoll::roll_prod
#' @param x what to roll
#' @param roll rolling window
#' @return rolled product of x
rollProd <- function(x, roll=1) {
  rollprod <- apply(x+1,2, RcppRoll::roll_prod,n=roll)-1
  date <- index(x)
  rollprod <- xts(rollprod, order.by=date[roll:length(date)])
  return(rollprod)
}


#' @title rollapply xts as it should be
#' @description Overwrites rollapply.xts and uses rollapply.zoo for xts data
#' @param data the data to be used (representing a series of observations).
#' @param  ... arguments to pass to \code{\link{rollapply.zoo}}
#' @details The function transforms
#' @return Corces xts to zoo, then applies \code{\link{rollapply.zoo}} of \code{\link{zoo}} package and transforms back to xts
#' @export
rollapply.xts <- function(data, ...) {
  data_names <- names(data)
  out <- zoo::rollapply(zoo(data), ...)
  out <- xts(out)
  names(out) <- data_names
  out
}



