
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

