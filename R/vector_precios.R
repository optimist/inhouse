#' @importFrom readr read_delim
vector_precios <- function(file_path) {
  vector <- read_delim(
    file = file_path,
    delim = "|",
    col_types = cols(
      .default = col_character(),
      FECHA = col_date(format = "%Y%m%d"),
      `PRECIO SUCIO` = col_double(),
      `PRECIO LIMPIO` = col_double(),
      `INTERESES ACUMULADOS` = col_double(),
      `CUPON ACTUAL` = col_double(),
      SOBRETASA = col_double(),
      `MONTO EMITIDO` = col_double(),
      `MONTO EN CIRCULACION` = col_double(),
      `FECHA EMISION` =col_date(format = "%d/%m/%Y"),
      `PLAZO EMISION` = col_integer(),
      `FECHA VCTO` =col_date(format = "%d/%m/%Y"),
      `VALOR NOMINAL` = col_double(),
      `ST COLOCACION` = col_double(),
      `TASA CUPON` = col_double(),
      `CUPONES EMISION` = col_integer(),
      `CUPONES X COBRAR` = col_integer(),
      `DIAS TRANSC. CPN` = col_integer(),
      VOLATILIDAD = col_double(),
      `VOLATILIDAD 2` = col_double(),
      DURACION = col_double(),
      CONVEXIDAD = col_double(),
      VAR = col_double(),
      `DESVIACION STAND` = col_double(),
      `TASA DE RENDIMIENTO` = col_double(),
      `TASA ANT CORTE CUPON` = col_double(),
      `TASA SIG CORTE CUPON` = col_double(),
      `FECHA ANT CORTE CUPON` = col_date(format = "%d/%m/%Y"),
      `FECHA SIG CORTE CUPON` = col_date(format = "%d/%m/%Y"),
      `PRECIO MAX 12M` = col_double(),
      `PRECIO MIN 12M` = col_double(),
      `FECHA PRECIO MAXIMO` = col_date(format = "%d/%m/%Y"),
      `FECHA PRECIO MINIMO` = col_date(format = "%d/%m/%Y"),
      `SENSIBILIDAD` = col_double(),
      `DURACION MACAULAY` = col_double(),
      `DURACION EFECTIVA` = col_double(),
      `DIAS LIQUIDACION` = col_integer()
    )
  )
  for (col in names(vector)) {
    if (is.character(vector[[col]])) {
      vector[[col]] <- gsub('"|-', '', vector[[col]])
    }
  }
  vector <- vector %>% mutate(id = asset_id(tipo, emisora, serie), precio = precio)
  vector
}



