#' @import dplyr
#'
#' @export
magrittr::`%>%`

#' @title Fetch holdings data
#' @description Construct a mysql query for the selected dates and contrato and sends it to the sql database posiciones
#' @details This function will return a data.frame with holdings information from one or several dates
#' When no contrato is given, it will fetch the information of every contrato
#' @param fecha A vector of dates that can be of class character or Date but is expected to be in the format aaaa-mm-dd
#' @param contrato A character or integer vector with the number of contratos
#' @return A data.frame with the holding of the given contratos in the selected dates. If no contrato is given, it will return the information of every contrato.
#' @examples
#' get_position("2015-12-31")
#' get_position("2015-12-31", contrato = "25774")
#' @export
get_position <- function(fecha, contrato = NULL){
  con <- DBI::dbConnect(RMySQL::MySQL(), host='CISM21', username="cism", password="cism", dbname="portafolio")

  query <- paste("select ",
                 "fecha, carteramodelo, contrato, tot, reporto, tipo, emisora, serie, precio, tit, mon ",
                 "from posiciones where ",
                 "fecha in ('",
                 paste(lubridate::ymd(fecha), collapse = "','"),
                 "') ",
                 sep="")

  if(!is.null(contrato)) {
    query <- paste(query,
                   "and contrato in ('",
                   paste(contrato, collapse ="','"),
                   "') ",
                   sep="")
  }
  pos <- DBI::dbGetQuery(con, query)
  DBI::dbDisconnect(con)
  pos <- pos %>%
    #as date
    mutate(fecha = as.Date(fecha)) %>%
    #repo
    mutate(tipo = replace(tipo, reporto == "R", "")) %>%
    mutate(emisora = replace(emisora, reporto == "R", "REPORTO")) %>%
    mutate(serie = replace(serie, reporto == "R", "")) %>%
    mutate(precio = replace(precio, reporto == "R", 1)) %>%
    mutate(tit = replace(tit, reporto == "R", 0)) %>%
    #id
    mutate(id = asset_id(tipo, emisora, serie)) %>%
    #group
    group_by(fecha, carteramodelo, contrato, tot, id, reporto, tipo, emisora, serie, precio) %>%
    summarise(tit = sum(tit), mon = sum(mon)) %>%
    mutate(perc = mon/tot) %>%
    as.data.frame()
  pos$tit[pos$emisora=="EFECTIVO"] <- pos$mon[pos$emisora=="EFECTIVO"] # causaba incorrecta suma del total de valuacion
  pos$tit[pos$emisora=="REPORTO"] <- pos$mon[pos$emisora=="REPORTO"] # causaba incorrecta suma del total de valuacion
  pos[pos$tit != 0, ]
}

#' @title Fetch blocked holdings data
#' @description Construct a mysql query for the selected dates and contrato and sends it to the sql database 'posbloq'
#' @details Blocked holdings are those cannot be rebalanced since they belong to client decisions outsided our investment strategies.
#' This function will return a data.frame with blocked holdings information from one or several dates
#' When no contrato is given, it will fetch the information of every contrato
#' @param date A vector of dates that can be of class character or Date but is expected to be in the format aaaa-mm-dd
#' @param contrato A character or integer vector with the number of contratos
#' @return A data.frame with the holding of the given contratos in the selected dates. If no contrato is given, it will return
#' the information of every contrato.
#' @examples
#' get_blocked_position("2015-12-31")
#' @export
get_blocked_position <- function(fecha, contrato = NULL){
  con <- DBI::dbConnect(RMySQL::MySQL(), host='CISM21', username="cism", password="cism", dbname="portafolio")
  query <- paste("select ",
                 "fecha, carteramodelo, contrato, tipo, emisora, serie, tit, mon ",
                 "from posicionbloqueada where fecha in ('",
                 paste(fecha, collapse = "','"),
                 "')", sep="")
  if(!is.null(contrato)) {
    query <- paste(query,
                   "and contrato in ('",
                   paste(contrato, collapse ="','"),
                   "') ",
                   sep="")
  }
  bloq <- DBI::dbGetQuery(con, query)

  bloq <- bloq %>%
    #as date
    mutate(fecha = as.Date(fecha)) %>%
    #id
    mutate(id = asset_id(tipo, emisora, serie)) %>%
    as.data.frame()

  # filter by contrato
  if(!is.null(contrato)) {
    bloq <- bloq %>% filter(contrato == contrato)
  }

  bloq
}

#' @title Remove blocked holdings
#' @description Remove all the holdings that are blocked from holdings data
#' @details This function will match the entries of a data frame representing blocked holdings and remove the corresponding titles
#' and valuation from another data frame containing the full position
#' @param position A data.frame containing the full holdings information.
#' It is assumed to be generated with the function \code{\link{get_position}}
#' @param blocked A data.frame containing the full holdings information
#' It is assumed to be generated with the function \code{\link{get_blocked_position}}
#' @return A data.frame with the holdings that are not blocked (those that belong to the strategies and not
#' the client decisions and thus can be rebalanced)
#' @examples
#' date <- "2016-08-31"
#' pos <- get_position(date)
#' bl <- get_blocked_position(date)
#' unbl <- remove_blocked(pos, bl)
#' head(unbl)
#' @export
remove_blocked <- function(position, blocked) {
  position_key <- paste(position$fecha, position$contrato, position$id)
  blocked_key <- paste(blocked$fecha, blocked$contrato, blocked$id)
  blocked_idx <- match(position_key, blocked_key)

  tit <- -blocked$tit[blocked_idx]
  tit[is.na(tit)] <- 0

  position$tit <- position$tit + tit
  position <- position[position$tit > 0, ]
  position$mon <- position$precio * position$tit

  position %>%
    group_by(contrato) %>%
    mutate(tot=sum(mon)) %>%
    mutate(perc = mon/tot) %>%
    as.data.frame()
}

#' @title Find uncommon holdings
#' @description Finds the holdings that have median below cut (defaults to zero) among clientes with the same strategy
#' @details The function computes the median percentage of the total holdings for each strategy and the filters the full
#' holdings information to return whose median is below the cut
#' @param position A data.frame object of holdings expected to be obtained from \code{\link{get_position}}
#' @param cut A numeric value indicating a cut value to filter using the median percentage of the total
#' @return A data.frame object containing the holdings that have median percentage of the total within clients of the same
#' strategy below the selected cut value
#' @examples
#' find_rare_position(get_position("2015-12-31"))
#' @export
find_rare_position <- function(position, cut = 0) {
  position %>%
    #complete
    group_by(fecha, carteramodelo) %>%
    tidyr::complete(tidyr::nesting(contrato, tot),
                    tidyr::nesting(id, reporto, tipo, emisora, serie),
             fill = list(tit=0, mon = 0)) %>%
    #filter
    mutate(perc = mon/tot) %>%
    group_by(fecha, carteramodelo, id) %>%
    mutate(sum_mon = sum(mon), perc_median = median(perc)) %>%
    filter(perc_median <= cut) %>%
    filter(!is.na(precio)) %>%
    as.data.frame()
}


#' @title Remove uncommon holdings from position data
#' @description Computes the median percentage from total within each strategy and removes holdings from position data that have median zero.
#' @details Computes the median percentage from total within each strategy and removes holdings from position data that have median zero.
#' It is the opposite of \code{\link{find_rare_position}}. Sometimes even after removing blocked holdings using \code{\link{remove_blocked}} there
#' are holdings that belong to client decision and not our investment strategies (the position has not yet been blocked). This method
#' can be used to further clean the data by removing elements that are uncommon within a strategy
#' @param position A data.frame object containing holdings information. It is assumed to be generated using \code{\link{get_position}}
#' @return A data.frame with the holdings after removing id's that have zero median within a strategy
#' @seealso \code{\link{find_rare_position}}
#' @examples
#' clean_position(get_position("2015-12-31"))
#' @export
clean_position <- function(position) {
  position %>%
    #complete
    group_by(fecha, carteramodelo) %>%
    tidyr::complete(tidyr::nesting(contrato, tot),
                    tidyr::nesting(id, reporto, tipo, emisora, serie),
             fill = list(tit=0, mon = 0)) %>%
    #filter
    mutate(perc = mon/tot) %>%
    group_by(fecha, carteramodelo, id) %>%
    mutate(sum_mon = sum(mon), perc_median = median(perc)) %>%
    filter(perc_median > 0) %>%
    as.data.frame()
}





#' @title Summary statistics for holdings data
#' @description Compute basic summary statistics for holdings data for each strategy, date and id
#' @param position A data.frame object containing holdings information. It is assumed to be generated using \code{\link{get_position}}
#' @param groups  character vector of the names of the variables that will be used with grouping and computing statistics
#' Variable names must be quoted (an inconvenience used to avoid confusion with the commands passed to \code{\link{summarize}}).
#' If no grouping is desired then it can be set to NULL
#' @param ... Commands to be passed to \code{\link{summarize}} from \code{\link{dplyr}} for computing statistics within groups
#' #' @details The function will group by investment strategy (carteramodelo), date and asset id and compute
#' basic summary statistics and any other command specified by the user.
#' @return A data.frame object with the summary statistics. By default it returns a data.frame indicating the group and columns
#' \itemize{
#'   \item{\strong{sum_mon}:} {The sum of the valuation of the group}
#'   \item{\strong{perc_median}:} {The median of the percentage of the total valuation}
#'   \item{\strong{perc_mean}:} {The mean of the percentage of the total valuation}
#'   \item{\strong{perc_sd}:} {The standard deviation of the percentage of the total valuation}
#'   \item{}{The result of additional commands supplied by the user}
#' }
#' @examples
#' position <- get_position(fecha = c("2016-07-28", "2016-07-29"))
#' summarize_position(position, groups = c("fecha", "carteramodelo", "id"), count = length(contrato))
#' summarize_position(position, groups = NULL)
#' @export
summarize_position <- function(position, groups = NULL, ...) {
  position <- position %>%
    tidyr::complete(
      tidyr::nesting(fecha, contrato, tot),
      tidyr:: nesting(id, reporto, tipo, emisora, serie),
      fill = list(tit=0, mon = 0)
    ) %>%
    as.data.frame()
  if (!is.null(groups)) {
    position <- position %>%
      group_by_(.dots = lapply(paste0("~", groups), as.formula))
  }
  position %>%
    summarize(...)  %>%
    as.data.frame()
}


#' @title returns of the total valuation per contrato
#' @description Create return data per contract for each contrato
#' @details This function takes a position data.frame and uses it to compute the return of the contrato by taking the percent change
#' of each date's valuation and its previous valuation indicated by the column named \code{tot}.
#' @param position A position data.frame object assumed to be generated using \code{\link{get_position}}
#' @param cumulative A boolean indicated whether returns are cumulative or daily. Defaults to FALSE
#' @return A \code{\link{xts}} (time series) object indexed by the date and with each column representing the daily return of the contrato.
#' Returns are give in the form of valuation ratio (final_price/initial_price)
#' @examples
#' pos <- get_position(c("2015-12-31", "2016-01-04", "2016-01-05"), contrato = c("25774", "25773"))
#' get_position_returns(pos)
#' @export
get_contrato_returns <- function(position, cumulative = FALSE) {
  tot_df <- position %>%
    select(fecha, contrato, tot) %>%
    distinct() %>%
    tidyr::spread(contrato, tot)
  tot_df <- xts::xts(tot_df[ ,-1], order.by = as.Date(tot_df$fecha))
  ret_df <- tot_df/xts::lag.xts(tot_df)
  if (cumulative) {
    coredata(ret_df) <- apply(coredata(ret_df), 2, function(row) row <- row / ret_df[1, ])
  }
}
