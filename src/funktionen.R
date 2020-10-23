
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
library(janitor)

###----alle möglichen strategien

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

strategien <- map(seq(1:13), get_strategie) %>% flatten()


strategie_matrix <- matrix(NA, ncol = 13, nrow =  length(strategien))
strategie_names <- matrix(NA, ncol = 2, nrow =  length(strategien))

for (ii in seq(1, length(strategien))){
  vec <- pluck(strategien, ii)
  
  for (aa in vec){
    strategie_matrix[ii, aa] <- 1
  }
  strategie_names[ii, 1] <- paste(vec, collapse = "")
  strategie_names[ii, 2] <- length(vec)
}

strategie_matrix <- as.data.frame(strategie_matrix)
colnames(strategie_matrix) <- c(paste("land", seq(1,13)))
strategie_names <- as.data.frame(strategie_names)
colnames(strategie_names) <- c("strategie", "anz")

ind <- which(strategie_names$anz == 5)
strategie_matrix_5_laender <- strategie_matrix[ind,]
strategie_matrix <- bind_cols(strategie_names, strategie_matrix)

#Beste Strategie waehlen
strategie <- strategie_matrix %>% 
  pivot_longer(cols = starts_with("land"), names_to = "land", values_to = "val", values_drop_na = TRUE) %>% 
  group_by(anz, land) %>% 
  summarise(summe = n()) %>% 
  filter(anz == 5) %>% 
  arrange(summe) %>% 
  pull(summe)

stats <- as.matrix( t(strategie / sum(strategie)))
for (ii in seq (1, nrow(strategie_matrix_5_laender))){
  strategie_matrix_5_laender[ii,] <- stats * strategie_matrix_5_laender[ii,]
} 

strategie_matrix_5_laender$gefahr <-  rowSums(strategie_matrix_5_laender, na.rm = TRUE)
strategie <- strategie_matrix_5_laender

#strategie <- strategie_matrix_5_laender[which(strategie_matrix_5_laender$gefahr == min(strategie_matrix_5_laender$gefahr)),]
strategie <- strategie[1:13]

for (ii in seq(1, nrow(strategie))) {
  strategie[ii, ] <- strategie[ii, ] / sum(strategie[ii, ], na.rm = TRUE)
  strategie[ii, ] <- round( strategie[ii, ] * 100)
}




