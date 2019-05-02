# Import data

in.dir <- list.files(path = "data", full.names = T, pattern = "matrices")
dir.create("data/synthesis")

tiff("data/synthesis/synthesis.tiff")

par(mfrow = c(2,2))

for (qq in 1:length(in.dir)) {

base <- read.csv(paste0(in.dir[qq],"/",dir(path = in.dir[qq], pattern = "prod_base")), header = T)
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

plot(x., y., type = 'n', xlab = '', ylab = 'Proportion decrease in forage fish biomass', xaxt = 'n',
     ylim = range(real.mat.res))
legend("bottomleft", legend = in.dir[qq])
axis(side = 3, at = perc.inc)
mtext(side = 3, text = 'Proportion incrase in jellyfish biomass' , line = 2.5)


for (j in 1:dim(real.mat.res)[1]) {
  points(jitter(x.), real.mat.res[j,], pch = 21, bg = "white", cex = 0.5)
}

red.mat <- matrix(NA, nrow = dim(real.mat.res)[2], ncol = 5)
for (j in 1:dim(real.mat.res)[2]) {
  red.mat[j,1:5] <- sort(real.mat.res[,j])[c(0.5,0.025,0.05,0.95,0.975)*1000]
}

lines(perc.inc, red.mat[,1], lwd = 2, col = "red")
lines(perc.inc, red.mat[,3], lwd = 2, lty = 2, col = "red")
lines(perc.inc, red.mat[,4], lwd = 2, lty = 2, col = "red")

red.mat <- as.data.frame(red.mat)
names(red.mat) <- paste0("level_",c(0.5,0.025,0.05,0.95,0.975))

save(red.mat, file = paste0("data/synthesis/",substr(in.dir[qq],6,nchar(in.dir[qq])),".RData"))

}

dev.off()
