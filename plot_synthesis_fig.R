#PURPOSE: Create figure for Lenfest metrics paper
#AUTHOR: Kelly Robinson
#DATE CREATED: 14 Feb 2019

#DATE LAST MODIFIED: 14 Feb 2019

#libraries
library(plyr)
library(dplyr)
library(ggplot2)

#load files
load("scenario_outputs/synthesis/GoMex_prod_matrices.Rdata")

ps.files <- list.files("scenario_outputs/GoMex_prod_matrices/", pattern = "ps", full.names = T)

gom <- adply(ps.files, 1, function(x){

  y <- read.csv(ps.files[1], header = T)
  return(y)

}, .progress = T)
