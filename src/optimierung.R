#'//////////////////////////////////////////////////////////////////////////////
#' AUTHOR: Manuel Wick-Eckl
#' CREATED: 23 Oktober 2020
#' MODIFIED: 23 Oktober 2020
#' PURPOSE: Riddler optimierung
#' Status: 
#' Comments:
#'//////////////////////////////////////////////////////////////////////////////
#' GLOBAL OPTIONS:

#Abkuelung
s_curve_amplitude <- 4000
s_curve_center <- 4000
s_curve_width <- 7000
survived <- 0

number_of_survived <- 100000
iteration <- 0


#'Libraries:
library(tidyverse)
library(crayon)
###----

strategien <- read.csv2("riddlersieger.csv")

gegner <- strategien
meine_strategie <- strategien[11,]

tournament <- function(strategie, gegner) {
  
  victory <- which(strategie[1, ] > gegner[1, ])
  v <- sum (victory)
  
  tie <- which(strategie[1, ] == gegner[1, ])
  t <- sum (tie)
  
  lost <- which(strategie[1, ] < gegner[1, ])
  l <- sum(lost)
  
  won <- ifelse( (v + t / 2) >= 46, 1, 0)
  lost <- ifelse( (l + t / 2) >= 46, 1, 0)
  tie <- ifelse( ((v + t / 2) < 46) & ((l + t / 2) < 46)  , 1, 0)
  
  return(list(won = won, 
              lost = lost,
              tie =tie) )
}

fit_function <- function(strategie, gegner) {
  purrr::map_dfr(.x = seq(1, nrow(gegner)), ~ tournament(strategie = strategie[1, ], gegner = gegner[.x, ]) ) %>% 
  pull(won) %>% 
  sum()
}

disturbance <- function(strategie) {
  samp_p <- sample(seq(1,13), 1)
  strategie[,samp_p] <- strategie[,samp_p]+1
  
  samp_n <- sample( which( seq(1,13)!= samp_p & strategie[,] != 0)   , 1)
  strategie[,samp_n] <- strategie[,samp_n]-1
  return(strategie)
}

current_temperature = function(iter, s_curve_amplitude, s_curve_center, s_curve_width) {
  s_curve_amplitude * s_curve(iter, s_curve_center, s_curve_width)
}

s_curve = function(x, center, width) {
  1 / (1 + exp((x - center) / width))
}

parent_strategie <- meine_strategie
parent_fit <- fit_function(strategie = parent_strategie, gegner = gegner)

repeat{
  
  iteration <- iteration + 1
  temp <- current_temperature(iteration, s_curve_amplitude, s_curve_center, s_curve_width)
  
  child_strategie <- disturbance(parent_strategie)
  
  child_fit <- fit_function(strategie = child_strategie, gegner = gegner)
  
  ratio <- exp((parent_fit - child_fit) / temp)
  
  if (is.nan (ratio)) {ratio <- 0}
  
  if (runif(1) < ratio) {
    parent_strategie <- child_strategie
    parent_fit <- child_fit
    
    if (ratio != 1) { survived <- 0 }
  } else {survived <- survived + 1 }
  
  #console output 
  
  cat("current_fit=",red(parent_fit), "current_temperature=", temp, "ratio=", ratio , "iteration=", iteration, "nr_survived =",survived,"\n") 
  flush.console()
  
  if (survived >= 100000 | parent_fit == 10){break}
}


