#' @import dplyr

#' @title Fetch holdings data
#' @description Construct a mysql query for the selected dates and contrato and sends it to the sql database posiciones
#' @details This function will return a data.frame with holdings information from one or several dates.
#' When no contrato is given, it will fetch the information of every contrato.
#' @param date A vector of dates that can be of class character or Date but is expected to be in the format aaaa-mm-dd
#' @param contrato A character or integer vector with the number of contratos
#' @return A data.frame with the holding of the given contratos in the selected dates. If no contrato is given, it will return the information of every contrato.
#' @examples
#' get_position("2015-12-31")
#' get_position("2015-12-31", contrato = "25774")
#' @export
get_position <- function(date, contrato = NULL){
  con <- DBI::dbConnect(RMySQL::MySQL(), host='CISM21', username="cism", password="cism", dbname="portafolio")

  query <- paste("select ",
                 "fecha, carteramodelo, contrato, tot, reporto, tipo, emisora, serie, precio, tit, mon ",
                 "from posiciones where ",
                 "fecha in ('",
                 paste(date, collapse = "','"),
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
    mutate(emisora = replace(emisora, reporto == "R", "EFECTIVO")) %>%
    mutate(serie = replace(serie, reporto == "R", "")) %>%
    mutate(precio = replace(precio, reporto == "R", 1)) %>%
    mutate(tit = replace(tit, reporto == "R", 0)) %>%
    #id
    mutate(id = asset_id(tipo, emisora, serie)) %>%
    #group
    group_by(fecha, carteramodelo, contrato, tot, id, reporto, tipo, emisora, serie, precio) %>%
    summarise(tit = sum(tit), mon = sum(mon)) %>%
    as.data.frame()
  pos$tit[pos$emisora=="EFECTIVO"] <- pos$mon[pos$emisora=="EFECTIVO"]
  pos[pos$tit > 0, ]
}

#' @title Fetch blocked holdings data
#' @description Construct a mysql query for the selected dates and contrato and sends it to the sql database 'posbloq'
#' @details Blocked holdings are those cannot be rebalanced since they belong to client decisions outsided our investment strategies.
#' This function will return a data.frame with blocked holdings information from one or several dates.
#' When no contrato is given, it will fetch the information of every contrato.
#' @param date A vector of dates that can be of class character or Date but is expected to be in the format aaaa-mm-dd
#' @param contrato A character or integer vector with the number of contratos
#' @return A data.frame with the holding of the given contratos in the selected dates. If no contrato is given, it will return
#' the information of every contrato.
#' @examples
#' get_blocked_position("2015-12-31")
#' @export
get_blocked_position <- function(date, contrato = NULL){
  con <- DBI::dbConnect(RMySQL::MySQL(), host='CISM21', username="cism", password="cism", dbname="portafolio")
  query <- paste("select ",
                 "fecha, carteramodelo, contrato, tipo, emisora, serie, tit, mon ",
                 "from posicionbloqueada where fecha in ('",
                 paste(date, collapse = "','"),
                 "')", sep="")
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
#' and valuation from another data frame containing the full position.
#' @param position A data.frame containing the full holdings information.
#' It is assumed to be generated with the function \code{\link{get_position}}
#' @param blocked A data.frame containing the full holdings information.
#' It is assumed to be generated with the function \code{\link{get_blocked_position}}
#' @return A data.frame with the holdings that are not blocked (those that belong to the strategies and not
#' the client decisions and thus can be rebalanced)
#' @examples
#' date <- "2016-08-31"
#' pos <- get_position(date)
#' bl <- get_blocked_position(date)
#' unbl <- rm_blocked(pos, bl)
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
  as.data.frame()
}

#' @title Find uncommon holdings
#' @description Finds the holdings that have median below cut (defaults to zero) among clientes with the same strategy
#' @details The function computes the median percentage of the total holdings for each strategy and the filters the full
#' holdings information to return whose median is below the cut.
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

#
#
# clean.pos <- function(pos) {
#   pos %>%
#     #complete
#     group_by(fecha, carteramodelo) %>%
#     complete(nesting(contrato, tot),
#              nesting(id, reporto, tipo, emisora, serie),
#              fill = list(tit=0, mon = 0)) %>%
#     #filter
#     mutate(perc = mon/tot) %>%
#     group_by(fecha, carteramodelo, id) %>%
#     mutate(sum_mon = sum(mon), perc_median = median(perc)) %>%
#     filter(!perc_median == 0) %>%
#     as.data.frame()
# }
#
# asset_stats.pos <- function(pos) {
#   pos %>%
#     group_by(fecha, carteramodelo, id) %>%
#     summarize(sum_mon = sum(mon), perc_median = median(perc), perc_mean = mean(perc), perc_sd =sd(perc))  %>%
#     as.data.frame()
# }
#
#
#
#
#
#
#
#
#
#
