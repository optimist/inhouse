#' @import dplyr
#'
#' @export
magrittr::`%>%`

#' @title Fetch cartera modelo
#' @description Construct a mysql query for the selected dates it to the sql database 'carteramodelo'
#' @details This function will return a data.frame with cartera modelo (strategy objetive composition) information from one or several dates
#' @param fecha A vector of dates that can be of class character or Date but is expected to be in the format aaaa-mm-dd
#' @return A data.frame with the cartera modelo (composition) of the given contratos in the selected dates.
#' @examples
#' fecha <- seq(as.Date("2015-12-31"), as.Date("2016-01-31"), by = 1)
#' get_carteramodelo(fecha)
#' get_carteramodelo(fecha)  %>%
#'    filter(grupo == "CG-CM-TESTIGO-RV-LOCAL")  %>%
#'    select(fecha, id, participacion) %>%
#'    tidyr::spread(id, participacion) %>%
#'    head()
#' @export

get_carteramodelo <- function(fecha){
  con <- DBI::dbConnect(RMySQL::MySQL(), host='CISM21', username="cism", password="cism", dbname="portafolio")

  query <- paste("select * from carteramodelo where ",
                 "fecha in ('",
                 paste(fecha, collapse = "','"),
                 "') ",
                 sep="")

  cm <- DBI::dbGetQuery(con, query)
  DBI::dbDisconnect(con)
  cm <- cm %>%
    as.data.frame() %>%
    mutate(tipo = sapply(strsplit(instrumento, "/"), function(x) x[[1]])) %>%
    mutate(emisora = sapply(strsplit(instrumento, "/"), function(x) x[[2]])) %>%
    mutate(serie = sapply(strsplit(instrumento, "/"), function(x) x[[3]])) %>%
    mutate(id = asset_id(tipo, emisora, serie)) %>%
    #repo
    mutate(tipo = replace(tipo, id == "repguber_", "")) %>%
    mutate(emisora = replace(emisora, id == "repguber_", "reporto")) %>%
    mutate(serie = replace(serie, id == "repguber_", "")) %>%
    mutate(id = replace(id, id == "repguber_", "reporto")) %>%
    as.data.frame()
}
