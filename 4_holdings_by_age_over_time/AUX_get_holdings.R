get_holdings <- function(yyyy, mm) {
  # URL to download data from the beginning until 03-2016
  URL11 <- "http://www.spensiones.cl/inf_estadistica/aficot/trimestral/"
  URL12 <- "/04A.html"
  # URL to download data thereafter
  URL21 <- paste0("http://www.spensiones.cl/apps/loadEstadisticas/genEstadAfiliados",
                  "Cotizantes.php?id=inf_estadistica/aficot/trimestral/")
  URL22 <- "/04A.html&p=T&menu=sci&menuN1=afil&menuN2=sdomovcci&orden=10&ext=.html"
  out <- tryCatch(
    { 
      # Try part
      if (yyyy<=2015) {
        source <- paste0(URL11, yyyy, "/", mm, URL12)
      } else if (yyyy==2016) {
        if (mm %in% c("03", "06")) {
          source <- paste0(URL11, yyyy, "/", mm, URL12)
        } else {
          source <- paste0(URL21, yyyy, "/", mm, URL22)
        }} else {
          source <- paste0(URL21, yyyy, "/", mm, URL22)
        }
      aux <- readHTMLTable(source)[[1]]  
      names <-  aux[, 1]
      rownames(aux) <-  make.names(names, unique=TRUE)
      subset <- c(paste0("X", 46:70))
      aux <- aux[subset, c(1, 6, 7 , 9)]
      rownames(aux) <- NULL
      names(aux) <- c("age", "avg_holdings_males", "avg_holdings_fem", "avg_holdings")
     # age              <- data.frame(age = c("Hasta 45", 46:70, "Más de 70"))
     # levels(age)      <- c("Hasta 45", c(46:70), "Más de 70")
     # aux$age <- 45:71
      aux <- map_df(aux, ~ str_remove_all(.x, "\\.")) %>% 
        map_df(as.numeric)  
     # aux <- bind_cols(age, aux)
      aux$date <- as.Date(rep(paste0(yyyy, "-", mm,"-01"), each = nrow(aux)))
      aux$age <- as.numeric(aux$age)
      aux
    },
    # Catch part
    error=function(cond) {
      message(paste0("No data available for", " ", mm, "-", yyyy))
      # Choose a return value in case of error
      data.frame()
    }
  )    
  return(out)
}