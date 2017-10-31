# 한글한글
source("20170917_A_H.R")
source("20171029_A_H.R")

curWd <- getwd()
setwd("..")
FB <- getFlagBank()
setwd(curWd)


pZh <- as.matrix(FB$zh)

makeSpan <- 1:5
creFunSet <- getCreateFunSet( pZh )
eleSet <- getNewElementSet( creFunSet ,pZh[makeSpan,] )

for( msIdx in makeSpan ){

	bornEleLst <- list()
	eleSet$rowSpan <- if( 1==msIdx ) integer(0) else 1:(msIdx-1)

	# 일단 현재 Zoid에 대한 element value, 즉 bornEleLst를 계산.
	#	bornEleLst는 나중에 유전 알고리즘에서의 평가자료로도 사용될 것이다.
	for( eleIdx in seq_len(length(creFunSet)) ){
		colVal <- sapply( creFunSet[[eleIdx]] ,function( p ){
						p$output( eleSet ,pZh[msIdx,] ,bornEleLst )
						# p$ioAddr[["outLst"]]
					})
		bornEleLst[[eleIdx]] <- colVal
	} # for(eleIdx)

	# 현재는 zoid history에 대한 element value생성 중 이므로
	#	bornEleLst를 eleSet에 저장.
	for( eleIdx in seq_len(length(creFunSet)) ){
		eleSet$eleLst[[eleIdx]]$mtx[msIdx,] <- bornEleLst[[eleIdx]]
	} # for(eleIdx)

} # for(msIdx)


creFunSet[[3]]

> inColIdx
[1] 31 32 33 34 35 36
> eleSet$eleLst[[1]]$mtx[,c(31 ,32 ,33 ,34 ,35 ,36)]
     [,1] [,2] [,3] [,4] [,5] [,6]
[1,]    0    3    4    3    2    0
[2,]    4    3    1    0    2    2
[3,]    1    1    4    1    2    1
[4,]    4    2    0    1    0    2
[5,]    1    4    4    0    1    2

