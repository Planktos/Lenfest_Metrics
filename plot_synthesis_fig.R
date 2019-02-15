#PURPOSE: Create figure for Lenfest metrics paper
#AUTHOR: Kelly Robinson
#DATE CREATED: 14 Feb 2019

#DATE LAST MODIFIED: 14 Feb 2019

#libraries
library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)

source("f_delta_stats.R")


#GoMex --------------
load("scenario_outputs/synthesis/GoMex_prod_matrices.Rdata")

ps.files <- list.files("scenario_outputs/GoMex_prod_matrices/", pattern = "ps", full.names = T)
gom <- adply(ps.files, 1, function(x){

  y <- read.table(x, sep = ",", header = T, stringsAsFactors = F)
  return(y)

})
gom <- gom[,-1]

gom.men <- melt(gom[gom$agg_grp == "gulf menhaden",] ,id.vars = c("scale_factor", "agg_grp"))
gom.men <- rename(gom.men, scenario_prod = value)
gom.men <- rename(gom.men, sim = variable)

base <- read.table("scenario_outputs/GoMex_prod_matrices/prod_base.csv", sep = ",", header = T, stringsAsFactors = F)
base.men <- melt(base[base$agg_grp == "gulf menhaden",], id.vars = c("scale_factor", "agg_grp"))
f_stats <- f_delta_stat(base.men$value)
base_mean <- f_stats$mean

gom.men$prod.diff <- ((gom.men$scenario_prod-base_mean)/base_mean)*100
gom.men <- arrange(gom.men, scale_factor)

plot(x=gom.men$scale_factor, y = gom.men$prod.diff)


# Peru ----------
load("scenario_outputs/synthesis/peru_prod_matrices.Rdata")

ps.files <- list.files("scenario_outputs/Peru_prod_matrices/", pattern = "ps", full.names = T)
peru <- adply(ps.files, 1, function(x){

  y <- read.table(x, sep = ",", header = T, stringsAsFactors = F)
  return(y)

})
peru <- peru[,-1]

peru.forage <- melt(peru[peru$agg_grp == "FOR",] ,id.vars = c("scale_factor", "agg_grp"))
peru.forage <- rename(peru.forage, scenario_prod = value)
peru.forage <- rename(peru.forage, sim = variable)

base <- read.table("scenario_outputs/Peru_prod_matrices/prod_base.csv", sep = ",", header = T, stringsAsFactors = F)
base.forage <- melt(base[base$agg_grp == "FOR",], id.vars = c("scale_factor", "agg_grp"))
f_stats <- f_delta_stat(base.forage$value)
base_mean <- f_stats$mean

peru.forage$prod.diff <- ((peru.forage$scenario_prod-base_mean)/base_mean)*100
peru.forage <- arrange(peru.forage, scale_factor)

plot(x=peru.forage$scale_factor, y = peru.forage$prod.diff)


# NCC ----------
load("scenario_outputs/synthesis/NCC_prod_matrices.Rdata")

ps.files <- list.files("scenario_outputs/NCC_prod_matrices/", pattern = "ps", full.names = T)
ncc <- adply(ps.files, 1, function(x){

  y <- read.table(x, sep = ",", header = T, stringsAsFactors = F)
  return(y)

})
ncc <- ncc[,-1]

ncc.forage <- melt(ncc[ncc$agg_grp == "forage fish",] ,id.vars = c("scale_factor", "agg_grp"))
ncc.forage <- rename(ncc.forage, scenario_prod = value)
ncc.forage <- rename(ncc.forage, sim = variable)

base <- read.table("scenario_outputs/NCC_prod_matrices/NCC_prod_base.csv", sep = ",", header = T, stringsAsFactors = F)
base.forage <- melt(base[base$agg_grp == "forage fish",], id.vars = c("scale_factor", "agg_grp"))
f_stats <- f_delta_stat(base.forage$value)
base_mean <- f_stats$mean

ncc.forage$prod.diff <- ((ncc.forage$scenario_prod-base_mean)/base_mean)*100
ncc.forage <- arrange(ncc.forage, scale_factor)

plot(x=ncc.forage$scale_factor, y = ncc.forage$prod.diff)


# EBS ----------
load("scenario_outputs/synthesis/EBS_prod_matrices.Rdata")

ps.files <- list.files("scenario_outputs/EBS_prod_matrices/", pattern = "ps", full.names = T)
ebs <- adply(ps.files, 1, function(x){

  y <- read.table(x, sep = ",", header = T, stringsAsFactors = F)
  return(y)

})
ebs <- ebs[,-1]

ebs.forage <- melt(ebs[ebs$agg_grp == "forage fish",] ,id.vars = c("scale_factor", "agg_grp"))
ebs.forage <- rename(ebs.forage, scenario_prod = value)
ebs.forage <- rename(ebs.forage, sim = variable)

base <- read.table("scenario_outputs/EBS_prod_matrices/EBS_prod_base.csv", sep = ",", header = T, stringsAsFactors = F)
base.forage <- melt(base[base$agg_grp == "forage fish",], id.vars = c("scale_factor", "agg_grp"))
f_stats <- f_delta_stat(base.forage$value)
base_mean <- f_stats$mean

ebs.forage$prod.diff <- ((ebs.forage$scenario_prod-base_mean)/base_mean)*100
ebs.forage <- arrange(ebs.forage, scale_factor)

plot(x=ebs.forage$scale_factor, y = ebs.forage$prod.diff)

