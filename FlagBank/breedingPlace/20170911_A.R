# 
library(plyr)
source("20170911_A_H.R")

# 한글 깨지지는 않겠지?

curWd <- getwd()
setwd("..")
FB <- getFlagBank()
setwd(curWd)

fLine <- FB$zh[,1]

hSpan <- 100:nrow(FB$zh)
divBase <- 8
flag <- fLine %% divBase



log.txt <- "./log/sampleMtx.txt"
k.FLogStr("",pFile=log.txt)

cName <- c("output","mean","mean.diff","seqHaunt")
statDf <- data.frame( output=integer(0) ,mean=numeric(0) ,mean.diff=numeric(0) ,seqHaunt=integer(0) )

for( cSize in 1:(divBase-1) ){
	combMtx <- combinations( divBase ,cSize )
	combMtx <- combMtx-1	# flag는 나머지들의 값이므로.

	for( combIdx in seq_len(nrow(combMtx)) ){
		bFlag <- flag %in% combMtx[combIdx,]
		rObj <- getFlagStatSample( bFlag ,hSpan )
		k.FLogStr(sprintf("\\n cSize:%d combIdx:%d (mean:%03.f%%)----------------"
					,cSize ,combIdx ,100*mean(rObj$sampleMtx[,"mean"])
				)
				,pConsole=T ,pFile=log.txt
			)
		statDf <- rbind( statDf ,rObj$sampleMtx )
	}	# for(combIdx)
} # for(cSize)


glm.out <- glm( output~poly(mean,4)+poly(mean.diff,4)+poly(seqHaunt,4) 
					,data=statDf ,family=binomial
				)
seqHaunt <- sort(unique(statDf$seqHaunt))
seqHaunt.col <- terrain.colors(length(seqHaunt))

xRange <- range( statDf$mean )
for( shIdx in seq_len(length(seqHaunt)) ){

	rDataFlag <- statDf$seqHaunt==seqHaunt[shIdx]

	predDf <- data.frame( mean=seq( xRange[1] ,xRange[2] ,0.01 ) )
	predDf$mean.diff	<- rep( mean(statDf$mean.diff[rDataFlag]) ,nrow(predDf) )
	predDf$seqHaunt		<- rep( seqHaunt[shIdx] ,nrow(predDf) )
	glm.pred <- predict( glm.out ,predDf ,type="response" )
	
	if( shIdx == 1 ){
		plot( predDf$mean ,glm.pred
				,xlim=xRange ,ylim=c(-0.1,1.1)
				,pch="+" ,col=seqHaunt.col[shIdx] ,type="l" )
	} else {
		lines( predDf$mean ,glm.pred ,col=seqHaunt.col[shIdx] )
	}
	
}


