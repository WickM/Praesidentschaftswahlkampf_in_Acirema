
#'//////////////////////////////////////////////////////////////////////////////
#' AUTHOR: Manuel Wick-Eckl
#' CREATED: 09 Oktober 2020
#' MODIFIED: 09 Oktober 2020
#' PURPOSE: Funktionen für die Simulation 
#' Status: 
#' Comments:
#'//////////////////////////////////////////////////////////////////////////////
#' GLOBAL OPTIONS:

#'Libraries:
library(tidyverse)

###----

create_stats <- function(){
  
  adaptierung_stats <- function(stats=stats[3], schwelle= 50, malus=50){
    stats <- ifelse(stats < schwelle, stats, stats -malus)
    return(stats)
  }
  
  stats <- sample(seq(1 , 100) ,size = 5, replace = TRUE)
  
  stats[3] <- adaptierung_stats(stats = stats[3], schwelle = 50, malus = 50)
  stats[4] <- adaptierung_stats(stats = stats[4], schwelle = 50, malus = 50)
  stats[5] <- adaptierung_stats(stats = stats[5], schwelle = 75, malus = 25)
  
  stats <- stats / sum(stats)
  stats <- round( stats * 100)
  
  ind <- which.max(stats)
  stats_sum <- sum(stats)
  
  if (stats_sum > 100) {stats[ind] <- stats[ind] - (stats_sum - 100)}
  if (stats_sum < 100) {stats[ind] <- stats[ind] + (100 - stats_sum)}
  
  sum(stats)
  
  return(stats)
}



get_strategie <- function(burgen){
  pos_strategie <- combn(13, burgen, function(x) (list(x)))
  pos_strategie$sum <- map_int(pos_strategie, sum)
  pos_strategie <- pos_strategie[which(pos_strategie$sum == 46)]
  return (pos_strategie)
}

strategien <- map(seq(4, 7), get_strategie) %>% flatten()
strategie_matrix <- matrix(NA, ncol = 13, nrow =  length(strategien))

for (ii in seq(1, length(strategien))){
  vec <- pluck(strategien, ii)
  
  for (aa in vec){
    strategie_matrix[ii, aa] <- 1
  }
}