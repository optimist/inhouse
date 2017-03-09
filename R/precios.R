#' @import dplyr
#'
#' @export
magrittr::`%>%`

#' @title Fetch price data from Amazon
#' @description Construct a mysql query for the selected dates and ids from the Amazon prices database
#' @details This function will return a data.frame with a column per id and date rows
#' @param fecha A vector of dates that can be of class character or Date but in the format aaaa-mm-dd
#' @param id A character vector with id list to query
#' @return A data.frame with the prices of the given ids in the selected dates.
#' @examples
#' get_prices(date_seq("2015-12-31/"), c("1ispy_", "1ishv_"))
#' @export
get_prices <- function(fecha = NULL, id = NULL){
  con <- DBI::dbConnect(
    drv=RMySQL::MySQL(),
    host="portafolio.c07ss4f9aoi8.us-east-1.rds.amazonaws.com",
    username="cism", password="cism", port=3306, dbname="precios")
  query <- paste("SELECT fecha, id, precio FROM precios")
  if(!is.null(fecha) | !is.null(id)) {
    query <- paste(query, "WHERE")
    if(!is.null(fecha)) {
      query <- paste0(query, " fecha IN ('", paste(fecha, collapse = "','"), "')")
      if (!is.null(id)) {
        query <- paste(query, "AND")
      }
    }
    if(!is.null(id)) {
      query <- paste0(query, " id IN ('", paste(id, collapse="','"), "')")
    }
  }
  cat(sprintf("fetching query: %s\n", query))
  prices <- DBI::dbGetQuery(con, query)
  DBI::dbDisconnect(con)
  prices %>%
    #as date
    mutate(fecha = as.Date(fecha)) %>%
    #repo
    tidyr::spread(id, precio) %>%
    data.frame(check.names = FALSE)
}

#' @title Id list
#' @description Generate the full list of available ids from Amazon
#' @param fecha In case they must only be selected from a date (faster)
#' @return A character vector the full list of unique ids
#' @examples
#' price_ids()
#' price_ids("2015-12-31")
#' @export
price_ids <- function(fecha = NULL){
  con <- DBI::dbConnect(
    drv=RMySQL::MySQL(),
    host="portafolio.c07ss4f9aoi8.us-east-1.rds.amazonaws.com",
    username="cism", password="cism", port=3306, dbname="precios")
  query <- "SELECT DISTINCT id FROM precios"
  if (!is.null(fecha)) {
    query <- paste0(query, " WHERE fecha IN ('", paste(fecha, collapse="','"), "')")
  }
  id_list <- DBI::dbGetQuery(con, query)
  DBI::dbDisconnect(con)
  as.character(id_list[[1]])
}

