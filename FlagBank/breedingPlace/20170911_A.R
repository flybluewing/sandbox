# 
source("20170911_A_H.R")

# 한글 깨지지는 않겠지?

curWd <- getwd()
setwd("..")
FB <- getFlagBank()

fLine <- FB$zh[,1]

hSpan <- 100:nrow(FB$zh)
divBase <- 8
flag <- fLine %% 8


for( regIdx in 0:(divBase-1) ){ # regIdx <- 1
	bFlag <- flag==regIdx
	rObj <- getFlagStatSample( bFlag ,hSpan )
}

predCol <- c("mean","seqHaunt")	# ,"mean.last20","mean.diff"
statDf <- data.frame(rObj$sampleMtx)
statDf[,"mean.diff"] <- statDf[,"mean"] - statDf[,"mean.last20"]
statDf.scale <- statDf
statDf.scale[,predCol] <- scale(statDf[,predCol])

lm.out <- lm( output~mean+seqHaunt,data=statDf.scale )	# +mean.last20+mean.diff 
lm.pred <- predict( lm.out ,statDf.scale[,predCol] ,interval="confidence")

pred.diff <- lm.pred[,"fit"] - statDf[,"mean"] 
pred.out <- ifelse( pred.diff>0 ,1 ,0 )
pred.hit <- ifelse(pred.out==statDf[,"output"] ,1 ,0 )
pred.col <- rep("black",length(pred.hit))
pred.col[ statDf[,"output"]==1 ] <- "red"

plot( pred.diff ,jitter(pred.hit) ,col=pred.col ,pch=".")


out.col <- ifelse(statDf[,"output"]==1,"red","black")
plot( statDf[,"mean.last20"] ,statDf[,"output"])


