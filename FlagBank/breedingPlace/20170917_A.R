# 
library(plyr)
source("20170911_A_H.R")
source("20170911_A_auxH.R")

source("20170917_A_H.R")

# 한글 깨지지는 않겠지?

curWd <- getwd()
setwd("..")
FB <- getFlagBank()
setwd(curWd)

k1 <- getSeqProbMapObj( pFlag=sample(1:3 ,500 ,replace=T ,prob=c(2,1,1) ) )

stdSeqObj <- getStdSeqProbMapObj( pTestNum=100 ,pSeqLogMax=300 )