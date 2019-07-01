# Import data

# in.dir <- list.files(path = "Jelly Forage Post-hoc Model/data/",
#                      full.names = T, pattern = "_matrices")

in.dir <- list.files(path = "data/",
                     full.names = T, pattern = "_matrices")

suppressWarnings(dir.create("data/synthesis_kr"))

# tiff("data/synthesis/synthesis_kr.tiff")

#par(mfrow = c(2,2))

for (qq in 1:length(in.dir)) {
  
system.name <- basename(in.dir[qq])
  
base <- read.csv(paste0(in.dir[qq],"/", dir(path = in.dir[qq], pattern = "prod_base")), header = T)
ext.ind <- c(which(as.character(base[,2]) == "forage fish"),which(as.character(base[,2]) == "large jellyfish"))
if (qq == 4) {
  ext.ind <- c(which(as.character(base[,2]) == "FOR"),which(as.character(base[,2]) == "JEL"))
}

mean.for <- as.matrix(base[ext.ind,3:dim(base)[2]])[1,]
mean.jel <- as.matrix(base[ext.ind,3:dim(base)[2]])[2,]

scen.csv <- paste0(in.dir[qq],"/",dir(path = in.dir[qq], pattern = "ps_sf"))

perc.inc <- c()
real.mat <- matrix(NA, ncol = length(scen.csv), nrow = 1000)
for (j in 1:length(scen.csv)) {
  perc.inc[j] <- as.numeric(strsplit(strsplit(scen.csv[j],"_")[[1]][5],".csv")[[1]])
  mat. <- read.csv(scen.csv[j], header = T)

  for (k in 3:dim(mat.)[2]) { real.mat[k-2,j] <- mat.[ext.ind[1],k] }

}

real.mat <- real.mat[,order(perc.inc)]
real.mat <- (real.mat - mean.for)/mean.for
perc.inc <- perc.inc[order(perc.inc)]

# for (j in 1:dim(real.mat)[2]) {
#    hist(real.mat[,j], 30, xlim = c(-1,0), main = perc.inc[j])
#    readline(prompt="Press [enter] to continue")
#  }

real.mat.res <- real.mat
for (j in 1:dim(real.mat.res)[2]) {
  real.mat.res[,j] <- sample(real.mat[,j], size = 1000, replace = T) }

x. <- perc.inc
y. <- real.mat.res[1,]

# plot(x., y., type = 'n', xlab = '', ylab = 'Proportion decrease in forage fish biomass', xaxt = 'n',
#      ylim = range(real.mat.res))
# legend("bottomleft", legend = in.dir[qq])
# axis(side = 3, at = perc.inc)
# mtext(side = 3, text = 'Proportion incrase in jellyfish biomass' , line = 2.5)


# for (j in 1:dim(real.mat.res)[1]) {
#   points(jitter(x.), real.mat.res[j,], pch = 21, bg = "white", cex = 0.5)
# }

red.mat <- matrix(NA, nrow = dim(real.mat.res)[2], ncol = 5)
for (j in 1:dim(real.mat.res)[2]) {
  red.mat[j,1:5] <- sort(real.mat.res[,j])[c(0.5,0.025,0.05,0.95,0.975)*1000]
}

# lines(perc.inc, red.mat[,1], lwd = 2, col = "red")
# lines(perc.inc, red.mat[,3], lwd = 2, lty = 2, col = "red")
# lines(perc.inc, red.mat[,4], lwd = 2, lty = 2, col = "red")

red.mat <- as.data.frame(red.mat)
names(red.mat) <- paste0("level_",c(0.5,0.025,0.05,0.95,0.975))

red.mat$system <- system.name #kr addition
red.mat$perc.in <- perc.inc #kr addition

save(red.mat, file = paste0("data/synthesis_kr/",substr(in.dir[qq],6,nchar(in.dir[qq])),"_perc-inc.RData"))

#}

#dev.off()


#ggplot verson of figure Kelly Robinson addition ---- 

  #put real.mat.res values into a data frame

  d <- list()

  for (j in 1:dim(real.mat.res)[1]) {
    y. <- real.mat.res[j,]
    x. <- perc.inc
    system.name.rep <- rep(x = system.name, length(y.))
    data <- data.frame(x., y., system.name.rep)
    colnames(data) <- c("perc_inc", "real_mat_res", "system.name.rep")
    
    d[[j]] <- data
  }

  system.results <- do.call("rbind", d)
  
  save(system.results, file = paste0("data/synthesis_kr/",substr(in.dir[qq],6,nchar(in.dir[qq])),"_system_results.RData"))

}

# RESULTS LOOP END ----

# PLOT RESULTS ----
library(dplyr)
library(plyr)
library(ggplot2)

#read in results--
in.dir <- list.files(path = "data/synthesis_kr/",
                     full.names = T, pattern = "_system_")

results <- adply(in.dir, 1, function(y){
  
  load(y)
  return(system.results)
  
})
results <- results[,-1]
results <- dplyr::rename(results, system = system.name.rep)

in.dir <- list.files(path = "data/synthesis_kr/",
                     full.names = T, pattern = "_perc")

perc <- adply(in.dir, 1, function(y){
  
  load(y)
  return(red.mat)
  
})
perc <- perc[,-1]


ebs <- ggplot(data = results[results$system=="EBS_prod_matrices",], aes(x = perc_inc, y = real_mat_res*100)) + 
  geom_point(colour = "grey30", size = 1, shape = 21) +
  geom_ribbon(data = perc[perc$system=="EBS_prod_matrices",], 
              aes(x=perc.inc, ymin=level_0.05*100, ymax=level_0.95*100), inherit.aes = F, fill = "steelblue", alpha = 0.2) +
    geom_line(data = perc[perc$system=="EBS_prod_matrices",], 
            aes(y = level_0.5*100, x = perc.inc), color = "firebrick4", size = 1, linetype="solid") +
  geom_line(data = perc[perc$system=="EBS_prod_matrices",], 
            aes(y = level_0.05*100, x = perc.inc), color = "firebrick4", size = 1, linetype="dashed") +
  geom_line(data = perc[perc$system=="EBS_prod_matrices",], 
            aes(y = level_0.95*100, x = perc.inc), color = "firebrick4", size = 1, linetype="dashed") +
  scale_y_continuous(name = "% decrease in forage fish biomass", breaks = seq(-100,0,10), limits = c(-100,0),
                     expand = c(0,0)) +
  scale_x_continuous(name = "% increase in jellyfish biomass", breaks = seq(0,150,10), limits = c(0,150),
                     expand = c(0,0), position = "bottom") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(size = rel(1.5), 
                                  face = "bold", vjust = 1.5),
        axis.title = element_text(face = "bold"),
        axis.ticks.length = unit(0.2, "cm"),
        axis.ticks.y = element_line(size=1),
        axis.title.y = element_text(size=12, face = "bold", vjust= 1.8),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size=12, color = "black"),
        axis.text.y = element_text(size=12, color = "black"),
        panel.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black", size = 0.75),
        plot.background=element_rect(fill="white"),
        plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
    ggtitle(label = "E. Bering Sea")
  

ncc <- ggplot(data = results[results$system=="NCC_prod_matrices",], aes(x = perc_inc, y = real_mat_res*100)) + 
  geom_point(colour = "grey30", size = 1, shape = 21) +
  geom_ribbon(data = perc[perc$system=="NCC_prod_matrices",], 
              aes(x=perc.inc, ymin=level_0.05*100, ymax=level_0.95*100), inherit.aes = F, fill = "steelblue", alpha = 0.2) +
  geom_line(data = perc[perc$system=="NCC_prod_matrices",], 
            aes(y = level_0.5*100, x = perc.inc), color = "firebrick4", size = 1, linetype="solid") +
  geom_line(data = perc[perc$system=="NCC_prod_matrices",], 
            aes(y = level_0.05*100, x = perc.inc), color = "firebrick4", size = 1, linetype="dashed") +
  geom_line(data = perc[perc$system=="NCC_prod_matrices",], 
            aes(y = level_0.95*100, x = perc.inc), color = "firebrick4", size = 1, linetype="dashed") +
  scale_y_continuous(name = "", breaks = seq(-100,0,10), limits = c(-100,0),
                     expand = c(0,0)) +
  scale_x_continuous(name = "% increase in jellyfish biomass", breaks = seq(0,150,10), limits = c(0,150),
                     expand = c(0,0), position = "bottom") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(size = rel(1.5), 
                                  face = "bold", vjust = 1.5),
        axis.title = element_text(face = "bold"),
        axis.ticks.length = unit(0.2, "cm"),
        axis.ticks.y = element_line(size=1),
        axis.title.y = element_text(size=14, face = "bold", vjust= 1.8),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size=12, color = "black"),
        axis.text.y = element_text(size=12, color = "black"),
        panel.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black", size = 0.75),
        plot.background=element_rect(fill="white"),
        plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
    ggtitle(label = "N. California Current")

gom <- ggplot(data = results[results$system=="GoMex_prod_matrices",], aes(x = perc_inc, y = real_mat_res*100)) + 
  geom_point(colour = "grey30", size = 1, shape = 21) +
  geom_ribbon(data = perc[perc$system=="GoMex_prod_matrices",], 
              aes(x=perc.inc, ymin=level_0.05*100, ymax=level_0.95*100), inherit.aes = F, fill = "steelblue", alpha = 0.2) +
  geom_line(data = perc[perc$system=="GoMex_prod_matrices",], 
            aes(y = level_0.5*100, x = perc.inc), color = "firebrick4", size = 1, linetype="solid") +
  geom_line(data = perc[perc$system=="GoMex_prod_matrices",], 
            aes(y = level_0.05*100, x = perc.inc), color = "firebrick4", size = 1, linetype="dashed") +
  geom_line(data = perc[perc$system=="GoMex_prod_matrices",], 
            aes(y = level_0.95*100, x = perc.inc), color = "firebrick4", size = 1, linetype="dashed") +
  scale_y_continuous(name = "% decrease in forage fish biomass", breaks = seq(-100,0,10), limits = c(-100,0),
                     expand = c(0,0)) +
  scale_x_continuous(name = "% increase in jellyfish biomass", breaks = seq(0,150,10), limits = c(0,150),
                     expand = c(0,0), position = "bottom") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(size = rel(1.5), 
                                  face = "bold", vjust = 1.5),
        axis.title = element_text(face = "bold"),
        axis.ticks.length = unit(0.2, "cm"),
        axis.ticks.y = element_line(size=1),
        axis.title.y = element_text(size=12, face = "bold", vjust= 1.8),
        axis.text.x = element_text(size=12, color = "black"),
        axis.text.y = element_text(size=12, color = "black"),
        panel.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black", size = 0.75),
        plot.background=element_rect(fill="white"),
        plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
  ggtitle(label = "N. Gulf of Mexico")


peru <- ggplot(data = results[results$system=="Peru_prod_matrices",], aes(x = perc_inc, y = real_mat_res*100)) + 
  geom_point(colour = "grey30", size = 1, shape = 21) +
  geom_ribbon(data = perc[perc$system=="Peru_prod_matrices",], 
              aes(x=perc.inc, ymin=level_0.05*100, ymax=level_0.95*100), inherit.aes = F, fill = "steelblue", alpha = 0.2) +
  geom_line(data = perc[perc$system=="Peru_prod_matrices",], 
            aes(y = level_0.5*100, x = perc.inc), color = "firebrick4", size = 1, linetype="solid") +
  geom_line(data = perc[perc$system=="Peru_prod_matrices",], 
            aes(y = level_0.05*100, x = perc.inc), color = "firebrick4", size = 1, linetype="dashed") +
  geom_line(data = perc[perc$system=="Peru_prod_matrices",], 
            aes(y = level_0.95*100, x = perc.inc), color = "firebrick4", size = 1, linetype="dashed") +
  scale_y_continuous(name = "", breaks = seq(-100,0,10), limits = c(-100,0),
                     expand = c(0,0)) +
  scale_x_continuous(name = "% increase in jellyfish biomass", breaks = seq(0,150,10), limits = c(0,150),
                     expand = c(0,0), position = "bottom") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(size = rel(1.5), 
                                  face = "bold", vjust = 1.5),
        axis.title = element_text(face = "bold"),
        axis.ticks.length = unit(0.2, "cm"),
        axis.ticks.y = element_line(size=1),
        axis.title.y = element_text(size=12, face = "bold", vjust= 1.8),
        axis.text.x = element_text(size=12, color = "black"),
        axis.text.y = element_text(size=12, color = "black"),
        panel.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black", size = 0.75),
        plot.background=element_rect(fill="white"),
        plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
  ggtitle(label = "N. Humboldt Current")

library(gridExtra)
library(grid)

p <- grid.arrange(ebs,ncc,gom,peru, nrow=2, ncol=2)

#file = paste0(output.directory1, "Fig4_plankton_pl_peak_mode_area_yr.png")
file = "Fig2_synthesis.png"
png(file = file, width = 14, height = 9, units = "in", res = 300)
plot(p)
dev.off()

# END PLOT Figure ------------

# Test for significant declines each system -----
library(plyr)

in.dir <- list.files(path = "data/synthesis_kr/",
                     full.names = T, pattern = "_perc")

perc <- adply(in.dir, 1, function(y){
  
  load(y)
  return(red.mat)
  
})
perc <- perc[,-1]

perc$system_fac <- factor(perc$system, levels = c("EBS_prod_matrices", "GoMex_prod_matrices", 
                                                      "NCC_prod_matrices","Peru_prod_matrices"), 
                            labels = c("EBS", "GoMex", "NCC","Peru"))

d.mean <- perc[,c("level_0.5","system_fac","perc.in")]


library(lattice)
histogram(~ level_0.5 | system_fac,
          data=d.mean,
          layout=c(1,4))


library(emmeans)
model.s <- lm(level_0.5*100 ~ perc.in*system_fac, data = d.mean)
anova(model.s)
summary(model.s)

#Obtain slopes --
model.s$coefficients
m.lst <- lstrends(model.s, "system_fac", var="perc.in")

#NCC, NHC, and GOM mean slope ---
slopes <- c(-0.0782, -0.0960, -0.1730)
mean.slope <- mean(slopes)
print(mean.slope)

#NCC, NHC, EBS and GOM mean slope ---
slopes <- c(-0.0782, -0.0960, -0.1730, -0.3588)
mean.slope <- mean(slopes)
print(mean.slope)

#Compare slopes
pairs(m.lst, adjust = "tukey")

library(multcomp)
cld(m.lst,
    alpha=0.05, 
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey")

#GoMEx ---
library(car)
model.g <- lm(level_0.5*100 ~ perc.in, data = d.mean[d.mean$system_fac=="GoMex",])
Anova(model.g, type = 2)
summary(model.g)
summary(model.g)$r.squared

library(rcompanion)
nagelkerke(model.g)

max.ff <- d.mean[d.mean$system_fac=="GoMex",]


#EBS ---
library(car)
model.b <- lm(level_0.5*100 ~ perc.in, data = d.mean[d.mean$system_fac=="EBS",])
Anova(model.b, type = 2)
summary(model.b)
summary(model.b)$r.squared

library(rcompanion)
nagelkerke(model.b)

max.ff <- d.mean[d.mean$system_fac=="EBS",]


#Peru ---
library(car)
model.p <- lm(level_0.5*100 ~ perc.in, data = d.mean[d.mean$system_fac=="Peru",])
Anova(model.p, type = 2)
summary(model.p)
summary(model.p)$r.squared

library(rcompanion)
nagelkerke(model.p)

max.ff <- perc[perc$system_fac=="Peru",]

#NCC ---
library(car)
model.c <- lm(level_0.5*100 ~ perc.in, data = d.mean[d.mean$system_fac=="NCC",])
Anova(model.c, type = 2)
summary(model.c)
summary(model.c)$r.squared

library(rcompanion)
nagelkerke(model.c)

max.ff <- d.mean[d.mean$system_fac=="NCC",]