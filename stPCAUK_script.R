library(stpca)
library(stringdist)
library(sp)
library(geonames)
library(reshape2)
library(dplyr)
library(expm)
library(rgl)
library(mgcv)
library(ggplot2)
library(tidyr)

# knitr::opts_chunk$set(fig.pos = "!H", out.extra = "")

dat <- read.csv("DASHBOARD_27_2_21.csv")[,c(1,4:8)]

names(dat) <- c("date","country","beds","hosp","cases","deaths")

dat.comp <- dat[complete.cases(dat[,3:6]),]

dat.comp$country <- replace(dat.comp$country, dat.comp$country=="Scotland","S")
dat.comp$country <- replace(dat.comp$country, dat.comp$country=="England","E")
dat.comp$country <- replace(dat.comp$country, dat.comp$country=="Wales","W")
dat.comp$country <- replace(dat.comp$country, dat.comp$country=="Northern Ireland","NI")

dat_wide <- dat.comp %>% pivot_wider(names_from = "country",values_from=c("hosp","cases","deaths","beds"))

dat_wide <- data.frame(dat_wide)

dat_wide <- dat_wide[complete.cases(dat_wide),]

#### CONSTRUCT TEMPORAL WEIGHT MATRIX ######


nt <- dim(dat_wide)[1]
cormd <- NULL
for(j in 1:(dim(dat_wide)[2]-1)){
  x <- dat_wide[,j+1]
  gamfit <- try(gam(x~s(as.numeric(row.names(dat_wide))),method="REML"))
  if(class(gamfit)[1]=="gam"){
    cormd <- c(cormd,cor(gamfit$residuals[1:(nt-1)],gamfit$residuals[2:nt]))
  }else{
    cormd <- c(cormd,NA)
  }
  
}
tempwtd <- createWeightT(dim(dat_wide)[1], median(cormd,na.rm = T), corr.form = "AR1")

smodsu = stpca(dat_wide[,-1],pca.mode = "Smode",scale=T,scree=F)
smodtu = stpca(dat_wide[,-1],pca.mode = "Tmode",scale=T,scree=F)

smodst = stpca(dat_wide[,-1],pca.mode = "Smode",temporal.wt = tempwtd, pca.wt ="temporal",scale=T,scree=F)
smodtt = stpca(dat_wide[,-1],pca.mode = "Tmode",temporal.wt = tempwtd, pca.wt="temporal",scale=T,scree=F)


par(mfrow=c(2,3))
for(i in 1:3) plot(as.Date(dat_wide[,1]),smodsu$unweighted$scores[,i],pch=20,ylab=paste0("PC",i," score"),xlab="Date",main=paste0("%var = ",round(smodsu$unweighted$var.prop[i]*100,3)))
for(i in 1:3) plot(as.Date(dat_wide[,1]),smodst$temporal$scores[,i],pch=20,ylab=paste0("PC",i," score"),xlab="Date",main=paste0("%var = ",round(smodst$temporal$var.prop[i]*100,3)))

par(mfrow=c(1,2))
plot(as.factor(names(dat_wide[,-1])),smodtu$unweighted$scores[,1],pch=20,ylab="PC1 score",xlab="", xaxt = "n",main=paste0("%var = ",round(smodtu$unweighted$var.prop[1]*100,3)))
axis(side = 1, labels = FALSE)

text(x = 1:length(names(dat_wide[,-1])),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 0.80,
     ## Use names from the data list.
     labels = names(dat_wide[,-1]),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 90,
     ## Adjust the labels to almost 100% right-justified.
     adj = 0.965,
     ## Increase label size.
     cex = 0.8)


plot(as.factor(names(dat_wide[,-1])),smodtt$temporal$scores[,1],pch=20,ylab="PC1 score",xlab="", xaxt = "n",main=paste0("%var = ",round(smodtt$temporal$var.prop[1]*100,3)))
axis(side = 1, labels = FALSE)

text(x = 1:length(names(dat_wide[,-1])),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 0.80,
     ## Use names from the data list.
     labels = names(dat_wide[,-1]),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 90,
     ## Adjust the labels to almost 100% right-justified.
     adj = 0.965,
     ## Increase label size.
     cex = 0.8)





dat <- read.csv("DASHBOARD_27_2_21.csv")[,c(1,4,7:8)]

names(dat) <- c("date","country","cases","deaths")

dat.comp <- dat[complete.cases(dat[,3:4]),]

dat.comp$country <- replace(dat.comp$country, dat.comp$country=="Scotland","S")
dat.comp$country <- replace(dat.comp$country, dat.comp$country=="England","E")
dat.comp$country <- replace(dat.comp$country, dat.comp$country=="Wales","W")
dat.comp$country <- replace(dat.comp$country, dat.comp$country=="Northern Ireland","NI")

dat_wide <- dat.comp %>% pivot_wider(names_from = "country",values_from=c("cases","deaths"))

dat_wide <- data.frame(dat_wide)

dat_wide <- dat_wide[complete.cases(dat_wide),]

#### CONSTRUCT TEMPORAL WEIGHT MATRIX ######


nt <- dim(dat_wide)[1]
cormd <- NULL
for(j in 1:(dim(dat_wide)[2]-1)){
  x <- dat_wide[,j+1]
  gamfit <- try(gam(x~s(as.numeric(row.names(dat_wide))),method="REML"))
  if(class(gamfit)[1]=="gam"){
    cormd <- c(cormd,cor(gamfit$residuals[1:(nt-1)],gamfit$residuals[2:nt]))
  }else{
    cormd <- c(cormd,NA)
  }
  
}
tempwtd <- createWeightT(dim(dat_wide)[1], median(cormd,na.rm = T), corr.form = "AR1")

smodsu = stpca(dat_wide[,-1],pca.mode = "Smode",scale=T,scree = F)
smodtu = stpca(dat_wide[,-1],pca.mode = "Tmode",scale=T,scree = F)

smodst = stpca(dat_wide[,-1],pca.mode = "Smode",temporal.wt = tempwtd, pca.wt ="temporal",scale=T,scree = F)
smodtt = stpca(dat_wide[,-1],pca.mode = "Tmode",temporal.wt = tempwtd, pca.wt="temporal",scale=T,scree = F)


par(mfrow=c(2,3))
for(i in 1:3) plot(as.Date(dat_wide[,1]),smodsu$unweighted$scores[,i],pch=20,ylab=paste0("PC",i," score"),xlab="Date",main=paste0("%var = ",round(smodsu$unweighted$var.prop[i]*100,3)))
for(i in 1:3) plot(as.Date(dat_wide[,1]),smodst$temporal$scores[,i],pch=20,ylab=paste0("PC",i," score"),xlab="Date",main=paste0("%var = ",round(smodst$temporal$var.prop[i]*100,3)))


par(mfrow=c(1,2))
plot(as.factor(names(dat_wide[,-1])),smodtu$unweighted$scores[,1],pch=20,ylab="PC1 score",xlab="", xaxt = "n",main=paste0("%var = ",round(smodtu$unweighted$var.prop[1]*100,3)))
axis(side = 1, labels = FALSE)

text(x = 1:length(names(dat_wide[,-1])),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 1.20,
     ## Use names from the data list.
     labels = names(dat_wide[,-1]),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 90,
     ## Adjust the labels to almost 100% right-justified.
     adj = 0.965,
     ## Increase label size.
     cex = 0.8)

plot(as.factor(names(dat_wide[,-1])),smodtt$temporal$scores[,1],pch=20,ylab="PC1 score",xlab="", xaxt = "n",main=paste0("%var = ",round(smodtt$temporal$var.prop[1]*100,3)))
axis(side = 1, labels = FALSE)

text(x = 1:length(names(dat_wide[,-1])),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 0.80,
     ## Use names from the data list.
     labels = names(dat_wide[,-1]),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 90,
     ## Adjust the labels to almost 100% right-justified.
     adj = 0.965,
     ## Increase label size.
     cex = 0.8)


#### NATION SPECIFIC


dat <- read.csv("DASHBOARD_27_2_21.csv")[,c(1,4:8)]

names(dat) <- c("date","country","beds","hosp","cases","deaths")

dat$country <- replace(dat$country, dat$country=="Scotland","S")
dat$country <- replace(dat$country, dat$country=="England","E")
dat$country <- replace(dat$country, dat$country=="Wales","W")
dat$country <- replace(dat$country, dat$country=="Northern Ireland","NI")

dat_wideS <- data.frame(dat %>% filter(country == "S"))
dat_wideW <- data.frame(dat %>% filter(country == "W"))
dat_wideE <- data.frame(dat %>% filter(country == "E"))
dat_wideNI <- data.frame(dat %>% filter(country == "NI"))

dat_wideS <- dat_wideS[complete.cases(dat_wideS[,-1]),-2]
dat_wideW <- dat_wideW[complete.cases(dat_wideW[,-1]),-2]
dat_wideE <- dat_wideE[complete.cases(dat_wideE[,-1]),-2]
dat_wideNI <- dat_wideNI[complete.cases(dat_wideNI[,-1]),-2]

#### CONSTRUCT TEMPORAL WEIGHT MATRIX ######


nt <- dim(dat_wideS)[1]
cormd <- NULL
for(j in 1:(dim(dat_wideS)[2]-1)){
  x <- dat_wideS[,j+1]
  gamfit <- try(gam(x~s(as.numeric(row.names(dat_wideS))),method="REML"))
  if(class(gamfit)[1]=="gam"){
    cormd <- c(cormd,cor(gamfit$residuals[1:(nt-1)],gamfit$residuals[2:nt]))
  }else{
    cormd <- c(cormd,NA)
  }
  
}
tempwtd <- createWeightT(dim(dat_wideS)[1], median(cormd,na.rm = T), corr.form = "AR1")

smodsuS = stpca(dat_wideS[,-1],pca.mode = "Smode",scale=T,scree=F)
smodtuS = stpca(dat_wideS[,-1],pca.mode = "Tmode",scale=T,scree=F)

smodstS = stpca(dat_wideS[,-1],pca.mode = "Smode",temporal.wt = tempwtd, pca.wt ="temporal",scale=T,scree=F)
smodttS = stpca(dat_wideS[,-1],pca.mode = "Tmode",temporal.wt = tempwtd, pca.wt="temporal",scale=T,scree=F)

nt <- dim(dat_wideW)[1]
cormd <- NULL
for(j in 1:(dim(dat_wideW)[2]-1)){
  x <- dat_wideW[,j+1]
  gamfit <- try(gam(x~s(as.numeric(row.names(dat_wideW))),method="REML"))
  if(class(gamfit)[1]=="gam"){
    cormd <- c(cormd,cor(gamfit$residuals[1:(nt-1)],gamfit$residuals[2:nt]))
  }else{
    cormd <- c(cormd,NA)
  }
  
}
tempwtd <- createWeightT(dim(dat_wideW)[1], median(cormd,na.rm = T), corr.form = "AR1")

smodsuW = stpca(dat_wideW[,-1],pca.mode = "Smode",scale=T,scree=F)
smodtuW = stpca(dat_wideW[,-1],pca.mode = "Tmode",scale=T,scree=F)

smodstW = stpca(dat_wideW[,-1],pca.mode = "Smode",temporal.wt = tempwtd, pca.wt ="temporal",scale=T,scree=F)
smodttW = stpca(dat_wideW[,-1],pca.mode = "Tmode",temporal.wt = tempwtd, pca.wt="temporal",scale=T,scree=F)


nt <- dim(dat_wideE)[1]
cormd <- NULL
for(j in 1:(dim(dat_wideE)[2]-1)){
  x <- dat_wideE[,j+1]
  gamfit <- try(gam(x~s(as.numeric(row.names(dat_wideE))),method="REML"))
  if(class(gamfit)[1]=="gam"){
    cormd <- c(cormd,cor(gamfit$residuals[1:(nt-1)],gamfit$residuals[2:nt]))
  }else{
    cormd <- c(cormd,NA)
  }
  
}
tempwtd <- createWeightT(dim(dat_wideE)[1], median(cormd,na.rm = T), corr.form = "AR1")

smodsuE = stpca(dat_wideE[,-1],pca.mode = "Smode",scale=T,scree=F)
smodtuE = stpca(dat_wideE[,-1],pca.mode = "Tmode",scale=T,scree=F)

smodstE = stpca(dat_wideE[,-1],pca.mode = "Smode",temporal.wt = tempwtd, pca.wt ="temporal",scale=T,scree=F)
smodttE = stpca(dat_wideE[,-1],pca.mode = "Tmode",temporal.wt = tempwtd, pca.wt="temporal",scale=T,scree=F)


nt <- dim(dat_wideNI)[1]
cormd <- NULL
for(j in 1:(dim(dat_wideNI)[2]-1)){
  x <- dat_wideNI[,j+1]
  gamfit <- try(gam(x~s(as.numeric(row.names(dat_wideNI))),method="REML"))
  if(class(gamfit)[1]=="gam"){
    cormd <- c(cormd,cor(gamfit$residuals[1:(nt-1)],gamfit$residuals[2:nt]))
  }else{
    cormd <- c(cormd,NA)
  }
  
}
tempwtd <- createWeightT(dim(dat_wideNI)[1], median(cormd,na.rm = T), corr.form = "AR1")

smodsuNI = stpca(dat_wideNI[,-1],pca.mode = "Smode",scale=T,scree=F)
smodtuNI = stpca(dat_wideNI[,-1],pca.mode = "Tmode",scale=T,scree=F)

smodstNI = stpca(dat_wideNI[,-1],pca.mode = "Smode",temporal.wt = tempwtd, pca.wt ="temporal",scale=T,scree=F)
smodttNI = stpca(dat_wideNI[,-1],pca.mode = "Tmode",temporal.wt = tempwtd, pca.wt="temporal",scale=T,scree=F)

par(mfrow=c(2,3))
for(i in 1:3) plot(as.Date(dat_wideS[,1]),smodsuS$unweighted$scores[,i],pch=20,ylab=paste0("PC",i," score"),xlab="Date",main=paste0("%var = ",round(smodsuS$unweighted$var.prop[i]*100,3)))
for(i in 1:3) plot(as.Date(dat_wideS[,1]),smodstS$temporal$scores[,i],pch=20,ylab=paste0("PC",i," score"),xlab="Date",main=paste0("%var = ",round(smodstS$temporal$var.prop[i]*100,3)))

par(mfrow=c(2,3))
for(i in 1:3) plot(as.Date(dat_wideW[,1]),smodsuW$unweighted$scores[,i],pch=20,ylab=paste0("PC",i," score"),xlab="Date",main=paste0("%var = ",round(smodsuW$unweighted$var.prop[i]*100,3)))
for(i in 1:3) plot(as.Date(dat_wideW[,1]),smodstW$temporal$scores[,i],pch=20,ylab=paste0("PC",i," score"),xlab="Date",main=paste0("%var = ",round(smodstW$temporal$var.prop[i]*100,3)))

par(mfrow=c(2,3))
for(i in 1:3) plot(as.Date(dat_wideE[,1]),smodsuE$unweighted$scores[,i],pch=20,ylab=paste0("PC",i," score"),xlab="Date",main=paste0("%var = ",round(smodsu$unweighted$var.prop[i]*100,3)))
for(i in 1:3) plot(as.Date(dat_wideE[,1]),smodstE$temporal$scores[,i],pch=20,ylab=paste0("PC",i," score"),xlab="Date",main=paste0("%var = ",round(smodstE$temporal$var.prop[i]*100,3)))


par(mfrow=c(2,3))
for(i in 1:3) plot(as.Date(dat_wideNI[,1]),smodsuNI$unweighted$scores[,i],pch=20,ylab=paste0("PC",i," score"),xlab="Date",main=paste0("%var = ",round(smodsuNI$unweighted$var.prop[i]*100,3)))
for(i in 1:3) plot(as.Date(dat_wideNI[,1]),smodstNI$temporal$scores[,i],pch=20,ylab=paste0("PC",i," score"),xlab="Date",main=paste0("%var = ",round(smodstNI$temporal$var.prop[i]*100,3)))

par(mfrow=c(2,2))
plot(as.factor(names(dat_wideE[,-1])),smodtuE$unweighted$scores[,1],pch=20,ylab="PC1 score",xlab="", xaxt = "n",main=paste0("a) %var = ",round(smodtuE$unweighted$var.prop[1]*100,3)))
axis(side = 1, labels = FALSE)

text(x = 1:length(names(dat_wideE[,-1])),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 0.80,
     ## Use names from the data list.
     labels = names(dat_wideE[,-1]),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 90,
     ## Adjust the labels to almost 100% right-justified.
     adj = 0.965,
     ## Increase label size.
     cex = 0.8)

plot(as.factor(names(dat_wideS[,-1])),smodtuS$unweighted$scores[,1],pch=20,ylab="PC1 score",xlab="", xaxt = "n",main=paste0("b) %var = ",round(smodtuS$unweighted$var.prop[1]*100,3)))
axis(side = 1, labels = FALSE)

text(x = 1:length(names(dat_wideS[,-1])),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 0.80,
     ## Use names from the data list.
     labels = names(dat_wideS[,-1]),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 90,
     ## Adjust the labels to almost 100% right-justified.
     adj = 0.965,
     ## Increase label size.
     cex = 0.8)

plot(as.factor(names(dat_wideW[,-1])),smodtuW$unweighted$scores[,1],pch=20,ylab="PC1 score",xlab="", xaxt = "n",main=paste0("c) %var = ",round(smodtuW$unweighted$var.prop[1]*100,3)))
axis(side = 1, labels = FALSE)

text(x = 1:length(names(dat_wideW[,-1])),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 0.80,
     ## Use names from the data list.
     labels = names(dat_wideW[,-1]),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 90,
     ## Adjust the labels to almost 100% right-justified.
     adj = 0.965,
     ## Increase label size.
     cex = 0.8)

plot(as.factor(names(dat_wideNI[,-1])),smodtuNI$unweighted$scores[,1],pch=20,ylab="PC1 score",xlab="", xaxt = "n",main=paste0("d) %var = ",round(smodtuNI$unweighted$var.prop[1]*100,3)))
axis(side = 1, labels = FALSE)

text(x = 1:length(names(dat_wideNI[,-1])),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 0.80,
     ## Use names from the data list.
     labels = names(dat_wideNI[,-1]),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 90,
     ## Adjust the labels to almost 100% right-justified.
     adj = 0.965,
     ## Increase label size.
     cex = 0.8)


par(mfrow=c(2,2))

plot(as.factor(names(dat_wideE[,-1])),smodttE$temporal$scores[,1],pch=20,ylab="PC1 score",xlab="", xaxt = "n",main=paste0("a) %var = ",round(smodttE$temporal$var.prop[1]*100,3)))
axis(side = 1, labels = FALSE)

text(x = 1:length(names(dat_wideE[,-1])),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 0.80,
     ## Use names from the data list.
     labels = names(dat_wideE[,-1]),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 90,
     ## Adjust the labels to almost 100% right-justified.
     adj = 0.965,
     ## Increase label size.
     cex = 0.8)

plot(as.factor(names(dat_wideS[,-1])),smodttS$temporal$scores[,1],pch=20,ylab="PC2 score",xlab="", xaxt = "n",main=paste0("b) %var = ",round(smodttS$temporal$var.prop[1]*100,3)))
axis(side = 1, labels = FALSE)

text(x = 1:length(names(dat_wideS[,-1])),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 0.80,
     ## Use names from the data list.
     labels = names(dat_wideS[,-1]),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 90,
     ## Adjust the labels to almost 100% right-justified.
     adj = 0.965,
     ## Increase label size.
     cex = 0.8)

plot(as.factor(names(dat_wideW[,-1])),smodttW$temporal$scores[,1],pch=20,ylab="PC2 score",xlab="", xaxt = "n",main=paste0("c) %var = ",round(smodttW$temporal$var.prop[1]*100,3)))
axis(side = 1, labels = FALSE)

text(x = 1:length(names(dat_wideW[,-1])),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 0.80,
     ## Use names from the data list.
     labels = names(dat_wideW[,-1]),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 90,
     ## Adjust the labels to almost 100% right-justified.
     adj = 0.965,
     ## Increase label size.
     cex = 0.8)

plot(as.factor(names(dat_wideNI[,-1])),smodttNI$temporal$scores[,1],pch=20,ylab="PC2 score",xlab="", xaxt = "n",main=paste0("d) % var = ",round(smodttNI$temporal$var.prop[1]*100,3)))
axis(side = 1, labels = FALSE)

text(x = 1:length(names(dat_wideNI[,-1])),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 0.80,
     ## Use names from the data list.
     labels = names(dat_wideS[,-1]),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 90,
     ## Adjust the labels to almost 100% right-justified.
     adj = 0.965,
     ## Increase label size.
     cex = 0.8)



###### WAVE 1


dat <- read.csv("DASHBOARD_27_2_21.csv")[,c(1,4:8)]

names(dat) <- c("date","country","beds","hosp","cases","deaths")

dat.comp <- dat[complete.cases(dat[,3:6]),]
dat.comp<-dat.comp[as.Date(dat.comp[,1])<"2020-05-31",]
dat.comp$country <- replace(dat.comp$country, dat.comp$country=="Scotland","S")
dat.comp$country <- replace(dat.comp$country, dat.comp$country=="England","E")
dat.comp$country <- replace(dat.comp$country, dat.comp$country=="Wales","W")
dat.comp$country <- replace(dat.comp$country, dat.comp$country=="Northern Ireland","NI")

dat_wide <- dat.comp %>% pivot_wider(names_from = "country",values_from=c("hosp","cases","deaths","beds"))

dat_wide <- data.frame(dat_wide)

dat_wide <- dat_wide[complete.cases(dat_wide),]

#### CONSTRUCT TEMPORAL WEIGHT MATRIX ######


nt <- dim(dat_wide)[1]
cormd <- NULL
for(j in 1:(dim(dat_wide)[2]-1)){
  x <- dat_wide[,j+1]
  gamfit <- try(gam(x~s(as.numeric(row.names(dat_wide))),method="REML"))
  if(class(gamfit)[1]=="gam"){
    cormd <- c(cormd,cor(gamfit$residuals[1:(nt-1)],gamfit$residuals[2:nt]))
  }else{
    cormd <- c(cormd,NA)
  }
  
}
tempwtd <- createWeightT(dim(dat_wide)[1], median(cormd,na.rm = T), corr.form = "AR1")

smodsu = stpca(dat_wide[,-1],pca.mode = "Smode",scale=T,scree=F)
smodtu = stpca(dat_wide[,-1],pca.mode = "Tmode",scale=T,scree=F)

smodst = stpca(dat_wide[,-1],pca.mode = "Smode",temporal.wt = tempwtd, pca.wt ="temporal",scale=T,scree=F)
smodtt = stpca(dat_wide[,-1],pca.mode = "Tmode",temporal.wt = tempwtd, pca.wt="temporal",scale=T,scree=F)

par(mfrow=c(2,3))
for(i in 1:3) plot(as.Date(dat_wide[,1]),smodsu$unweighted$scores[,i],pch=20,ylab=paste0("PC",i," score"),xlab="Date",main=paste0("%var = ",round(smodsu$unweighted$var.prop[i]*100,3)))
for(i in 1:3) plot(as.Date(dat_wide[,1]),smodst$temporal$scores[,i],pch=20,ylab=paste0("PC",i," score"),xlab="Date",main=paste0("%var = ",round(smodst$temporal$var.prop[i]*100,3)))


##### WAVE 2


dat <- read.csv("DASHBOARD_27_2_21.csv")[,c(1,4:8)]

names(dat) <- c("date","country","beds","hosp","cases","deaths")

dat.comp <- dat[complete.cases(dat[,3:6]),]
dat.comp<-dat.comp[as.Date(dat.comp[,1])>="2020-09-01",]
dat.comp$country <- replace(dat.comp$country, dat.comp$country=="Scotland","S")
dat.comp$country <- replace(dat.comp$country, dat.comp$country=="England","E")
dat.comp$country <- replace(dat.comp$country, dat.comp$country=="Wales","W")
dat.comp$country <- replace(dat.comp$country, dat.comp$country=="Northern Ireland","NI")

dat_wide <- dat.comp %>% pivot_wider(names_from = "country",values_from=c("hosp","cases","deaths","beds"))

dat_wide <- data.frame(dat_wide)

dat_wide <- dat_wide[complete.cases(dat_wide),]

#### CONSTRUCT TEMPORAL WEIGHT MATRIX ######


nt <- dim(dat_wide)[1]
cormd <- NULL
for(j in 1:(dim(dat_wide)[2]-1)){
  x <- dat_wide[,j+1]
  gamfit <- try(gam(x~s(as.numeric(row.names(dat_wide))),method="REML"))
  if(class(gamfit)[1]=="gam"){
    cormd <- c(cormd,cor(gamfit$residuals[1:(nt-1)],gamfit$residuals[2:nt]))
  }else{
    cormd <- c(cormd,NA)
  }
  
}
tempwtd <- createWeightT(dim(dat_wide)[1], median(cormd,na.rm = T), corr.form = "AR1")

smodsu = stpca(dat_wide[,-1],pca.mode = "Smode",scale=T,scree=F)
smodtu = stpca(dat_wide[,-1],pca.mode = "Tmode",scale=T,scree=F)

smodst = stpca(dat_wide[,-1],pca.mode = "Smode",temporal.wt = tempwtd, pca.wt ="temporal",scale=T,scree=F)
smodtt = stpca(dat_wide[,-1],pca.mode = "Tmode",temporal.wt = tempwtd, pca.wt="temporal",scale=T,scree=F)

par(mfrow=c(2,3))
for(i in 1:3) plot(as.Date(dat_wide[,1]),smodsu$unweighted$scores[,i],pch=20,ylab=paste0("PC",i," score"),xlab="Date",main=paste0("%var = ",round(smodsu$unweighted$var.prop[i]*100,3)))
for(i in 1:3) plot(as.Date(dat_wide[,1]),smodst$temporal$scores[,i],pch=20,ylab=paste0("PC",i," score"),xlab="Date",main=paste0("%var = ",round(smodst$temporal$var.prop[i]*100,3)))

