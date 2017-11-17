curWd <- getwd()	;setwd("..")
FB <- getFlagBank()
FB.f <- getFlagBank("./zoidHistory/ZH_Final.csv")
setwd(curWd)

source("20171116_A_H.R")
source("20171116_A_H_cliper.R")

zh <- as.matrix(FB$zh)


