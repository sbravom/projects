
# This function cleans and tidy each data frame contained in funds_lst
clean_data                  <- function(x) {                                  # the first row will be the header
  colnames(x)        <- x[1, ]  
  row_int            <- c(1:(nrow(x)-3), nrow(x))
  col_int            <- c((ncol(x)-1), ncol(x))
  x                  <- x[-row_int, -col_int]                          # removing the first row and two last columns (no relevant data)
  colnames(x)[1]     <- "keep"                                      # keeping this column when gathering 
  x                  <- x %>% mutate_all(as.character)
  x                  <- gather(x, key = "Age", value = "N", -keep)  # tidy data
  x                  <- spread(x, keep, N)                          # tidy data
  colnames(x)        <- c("Age", "Female", "Male")                           # set N as double
  x$Age              <- c( "<20", "20-25", "65-70", "25-30", "30-35", "35-40", 
                           "40-45", "45-50", "50-55", "55-60", 
                           "60-65", ">70" )
  x$Age              <- as.factor(x$Age)                            # setting levels to appear correctly at plots
  x$Age              <- factor(x$Age, levels(x$Age)[c(1, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 2)])
  x$Male             <- str_remove(x$Male, "\\.")
  x$Female             <- str_remove(x$Female, "\\.")
  x$Female           <- as.numeric(x$Female)                        
  x$Male             <- as.numeric(x$Male)  
  x$All              <- x$Male + x$Female
  x
}




get_data_funds_age_q <- function(yyyy, mm) {
  URLS <- list()   
  # URL to download data from 2003Q1 to Q62016
  URLS[1] <- "http://www.spensiones.cl/inf_estadistica/aficot/trimestral/"
  URLS[2] <- "/03A.html"
  
  # URL To download data from Q92016 to present
  URLS[3] <- "http://www.spensiones.cl/apps/loadEstadisticas/genEstadAfiliadosCotizantes.php?id=inf_estadistica/aficot/trimestral/"
  URLS[4] <- "/03A.html&p=T&menu=sci&menuN1=afil&menuN2=tipfon&orden=30&ext=.html"
  out <- tryCatch(
    { 
      # Try part
      if (yyyy<=2015) {
        source <- paste0(URLS[1], yyyy, "/", mm, URLS[2])
        funds_lst <- readHTMLTable(source, which = c(3, 5, 7, 9, 11))
      } else if (yyyy==2016) {
        if (mm=="03" | mm=="06") {
          source <- paste0(URLS[1], yyyy, "/", mm, URLS[2])
          funds_lst <- readHTMLTable(source, which = c(3, 5, 7, 9, 11))
        } else {
          source <- paste0(URLS[3], yyyy, "/", mm, URLS[4])
          funds_lst <- readHTMLTable(source)
        }} else {
          source <- paste0(URLS[3], yyyy, "/", mm, URLS[4])
          funds_lst <- readHTMLTable(source)
        }
      
      # Cleaning data
      funds_lst <- map(funds_lst, clean_data)   # apply clean_data function over funds' data frames A to E
      for (k in 1:5) {funds_lst[[k]][["Fund"]] <- LETTERS[k]}   # column with fund ID
      funds_tidy   <- bind_rows(funds_lst) # merge 5 funds dfs in a single data frame
      funds_tidy$Date <- as.Date(paste0(yyyy, "-", mm, "-01"))
      funds_tidy
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