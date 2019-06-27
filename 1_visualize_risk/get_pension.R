if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, scales, ggridges, reshape2)

# Anchored to a 100% stock portfolio with 6% return and 16% vol.
portstats <- function(risk_level, er = 6, vol=16){
  er <- er * risk_level
  vol <- vol * risk_level
  return(c(er = er, vol = vol))
}


# A workhorse function to produce the results.
cumulative_results <- function(risk_level, ptiles = c(0.20, 0.5, 0.80), years = 30, nobs = 150000, annual_savings = 720000){
  ps <- portstats(risk_level)
  # Generate a matrix of returns
  raw_returns <- matrix(rnorm(n = nobs, mean = ps['er'], sd = ps['vol'])/100,
                        nrow=years)
  
# Cummulative returns for investment on each period from t to the end
  
  returns_cumul <- matrix(ncol = nobs/years, nrow = years)
  
  for (i in 1:years) {
    for (j in 1:(nobs/years)) {
    returns_cumul[i, j] <- (cumprod(raw_returns[i:years, j]+1) %>% .[length(.)])*annual_savings
    }
    }
  
  # Find wealth accumulated at the end of the investment period
  returns_cumul_end <- colSums(returns_cumul)
  
  # Find the pdf of cumulative returns
  returns_cumul_dens <- density(returns_cumul_end)
  
  # Store info from pdf
  cumul_densities <- data.frame(year = as.numeric(years), 
                                    risk = risk_level,
                                    loc  = returns_cumul_dens$x/10^6,
                                    dens = returns_cumul_dens$y)
    
  # Find the focus percentiles
  cumul_quantiles <-  data.frame(t(quantile(returns_cumul_end, probs = ptiles)))
  cumul_quantiles$year = as.numeric(years)
  cumul_quantiles <- gather(cumul_quantiles, key = "percentile", value = "value", -year) %>% 
                       mutate(value = value/10^6)
  cumul_quantiles$percentile <- as.numeric(gsub("[^0-9\\]", "", cumul_quantiles$percentile))/100
  cumul_quantiles$risk = risk_level
  
  return(list(cumul_densities = cumul_densities,
              cumul_quantiles = cumul_quantiles))
}



ggplot_vertical_dist <- function(df, group_var, ylims = NULL, quantile_data = NULL){
  df[,group_var] <- as.factor(df[,group_var])
  df[,group_var] <- factor(df[,group_var], levels=rev(levels(df[,group_var])))
  
  tp <-  ggplot(data = df, aes_string(y = group_var, 
                                      x = "loc", 
                                      height ="dens")) 
  
  tp <- tp + geom_density_ridges(scale = 0.9, stat = "identity",
                                 fill = adjustcolor("dark blue", alpha.f = 0.2), 
                                 color = "dark blue") 
  
  tp <- tp + geom_vline(xintercept = 0, color = "grey") +
    theme_classic() + theme(axis.text.y = element_text(colour = "black")) 
  
  if(!is.null(ylims)){
    tp <- tp + coord_flip(xlim=ylims)
  } else {
    tp <- tp + coord_flip()
  }
    
    quantile_data[,"percentile"] <- as.factor(quantile_data[,"percentile"])
    quantile_data[,group_var] <- as.factor(quantile_data[,group_var])
    quantile_data$dens <- NA
    tp <-  tp + geom_point(data = quantile_data, aes_string(x = "value", y = group_var, shape="percentile"), size=2, color = "dark red")
  
  tp
}