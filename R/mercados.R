#' @import dplyr
#'
#' @export
magrittr::`%>%`

#' @title Fetch asset information data from Amazon
#' @description Construct a mysql query for the selected dates and ids from the Amazon mercados database, table valores
#' @param fecha A vector of dates that can be of class character or Date but in the format aaaa-mm-dd
#' @param id A character vector with id list to query
#' @return A data.frame with the prices of the given ids in the selected dates.
#' @examples
#' get_prices(date_seq("2015-12-31/"), c("1ispy_", "1ishv_"))
#' @export
get_mercados <- function(id = NULL){
  con <- DBI::dbConnect(
    drv=RMySQL::MySQL(),
    host="portafolio.c07ss4f9aoi8.us-east-1.rds.amazonaws.com",
    username="cism", password="cism", port=3306, dbname="mercados")
  col_names <- paste("priceid as id, tipovalor as tipo, emisora, serie, vencimiento, moneda",
                     "periodicidad_de_pago_dias, flotante, ",
                     "periodicidad_revision_dias, emisor, tasa_de_cupon",
                     "valor_nominal, preciohoy as last_price, fechahoy as last_update")
  query <- paste(sprintf("SELECT %s FROM valores", col_names))
  if (!is.null(id)) {
    query <- paste0(query, " WHERE priceid IN ('", paste(id, collapse="','"), "')")
  }
  valores <- DBI::dbGetQuery(con, query) %>%
    mutate(vencimiento = as.Date(vencimiento)) %>%
    mutate(last_update = as.Date(last_update))
  DBI::dbDisconnect(con)
  valores
}

