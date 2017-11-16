curWd <- getwd()	;setwd("..")
FB <- getFlagBank()	;setwd(curWd)

source("20171116_A_H.R")

zh <- as.matrix(FB$zh)

rebMtx <- matrix( 0 ,nrow=nrow(zh) ,ncol=ncol(zh) )
rebMtx[1,] <- NA    # n번째에 나왔음을 의미. 즉 바로 이전에 나왔으면 1
for( hIdx in 2:nrow(zh) ){
    ml <- getReboundLst( zh[hIdx,] ,zh[1:(hIdx-1),,drop=F] ,pSearchFirst=T )
    rebMtx[hIdx,] <- sapply( ml ,function(p){ifelse(0==length(p$fIdx),NA,hIdx-p$fIdx[1])} )
} # for(hIdx)



