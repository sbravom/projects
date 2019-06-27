get_data_by_opt <- function(yyyy, mm) {
  # URL to download data from the beginning until 10-2016
  URL11 <- "http://www.spensiones.cl/inf_estadistica/aficot/mensual/"
  URL12 <- "/02A.html"
  # URL to download data thereafter
  URL21 <- paste0("http://www.spensiones.cl/apps/loadEstadisticas/",
                  "genEstadAfiliadosCotizantes.php?id=inf_Cstadistica",
                  "/aficot/mensual/")
  URL22 <- "/02A.html&p=M&menu=sci&menuN1=afil&menuN2=tipfon&orden=10&ext=.html"
  out <- tryCatch(
    { 
      # Try part
      if (yyyy<=2015) {
        source <- paste0(URL11, yyyy, "/", mm, URL12)
      } else if (yyyy==2016) {
        if (mm %in% c("01", "02", "03", "04", "05", "06",
                      "07", "08", "09", "10")) {
          source <- paste0(URL11, yyyy, "/", mm, URL12)
        } else {
          source <- paste0(URL21, yyyy, "/", mm, URL22)
        }} else {
          source <- paste0(URL21, yyyy, "/", mm, URL22)
        }
      
      aux <- readHTMLTable(source)[[1]]  
      names <-  aux[,1]
      rownames(aux) <-  make.names(names, unique=TRUE)
      aux <- aux[c("AFILIADOS.QUE.OPTAN", "AFILIADOS.ASIGNADOS"),] %>% .[, -1]
      rownames(aux) <- NULL
      names(aux) <- c("A", "B", "C", "D", "E")
      aux <- map_df(aux, ~ str_remove_all(.x, "\\.")) %>% 
        map_df(as.numeric)
      
      aux$status <- c("opted", "assigned")
      aux <- aux %>% mutate(date = as.Date(paste0(yyyy, "-", mm,"-01"))) %>% 
        gather(key = "fund", value = "opt", -date, -status) %>% 
        spread(key = "status", value = "opt")
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