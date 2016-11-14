asset_id <- function(tipo, emisora, serie) {
  tipo[nchar(tipo) == 1] <- paste0(tipo[nchar(tipo) == 1], "_")
  id <- tolower(paste0(tipo, emisora, serie))
  id <- gsub("\\*|\\+|\\-", "_", id)
  id
}
