create_pos <- function(date, contrato = NULL){
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

create_bloq <- function(date){
  con <- DBI::dbConnect(RMySQL::MySQL(), host='CISM21', username="cism", password="cism", dbname="portafolio")
  query <- paste("select ",
                 "fecha, carteramodelo, contrato, tipo, emisora, serie, tit, mon ",
                 "from posicionbloqueada where fecha in ('",
                 paste(date, collapse = "','"),
                 "')", sep="")
  bloq <- DBI::dbGetQuery(con, query)

  bloq %>%
    #as date
    mutate(fecha = as.Date(fecha)) %>%
    #id
    mutate(id = asset_id(tipo, emisora, serie)) %>%
    as.data.frame()
}

rm_bloq.pos <- function(pos, bloq) {
  pos_key <- paste(pos$fecha, pos$contrato, pos$id)
  bloq_key <- paste(bloq$fecha, bloq$contrato, bloq$id)
  bloq_idx <- match(pos_key, bloq_key)

  tit <- -bloq$tit[bloq_idx]
  tit[is.na(tit)] <- 0

  pos$tit <- pos$tit + tit
  pos <- pos[pos$tit > 0, ]
  pos$mon <- pos$precio * pos$tit

  pos %>%
    group_by(contrato) %>%
    mutate(tot=sum(mon)) %>%
  as.data.frame()
}

outliers.pos <- function(pos) {
  pos %>%
    #complete
    group_by(fecha, carteramodelo) %>%
    complete(nesting(contrato, tot),
             nesting(id, reporto, tipo, emisora, serie),
             fill = list(tit=0, mon = 0)) %>%
    #filter
    mutate(perc = mon/tot) %>%
    group_by(fecha, carteramodelo, id) %>%
    mutate(sum_mon = sum(mon), perc_median = median(perc)) %>%
    filter(perc_median == 0) %>%
    filter(!is.na(precio)) %>%
    as.data.frame()
}


clean.pos <- function(pos) {
  pos %>%
    #complete
    group_by(fecha, carteramodelo) %>%
    complete(nesting(contrato, tot),
             nesting(id, reporto, tipo, emisora, serie),
             fill = list(tit=0, mon = 0)) %>%
    #filter
    mutate(perc = mon/tot) %>%
    group_by(fecha, carteramodelo, id) %>%
    mutate(sum_mon = sum(mon), perc_median = median(perc)) %>%
    filter(!perc_median == 0) %>%
    as.data.frame()
}

asset_stats.pos <- function(pos) {
  pos %>%
    group_by(fecha, carteramodelo, id) %>%
    summarize(sum_mon = sum(mon), perc_median = median(perc), perc_mean = mean(perc), perc_sd =sd(perc))  %>%
    as.data.frame()
}










