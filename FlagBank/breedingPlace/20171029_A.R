# 기초 데이터 로딩
curWd <- getwd()	;setwd("..")
FB <- getFlagBank()	;setwd(curWd)
# 헤더 파일
source("20170917_A_H.R")
source("20171029_A_H.R")

pZh <- as.matrix(FB$zh)

makeSpan <- 1:nrow(pZh)
creFunSet <- getCreateFunSet( pZh )
eleSet <- getNewElementSet( creFunSet ,pZh=pZh[makeSpan,] )

tStamp <- Sys.time()
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

stmpDiff <- Sys.time() - tStamp
k.FLogStr(sprintf("eleSet is made. cost:%.1f%s",stmpDiff,units(stmpDiff)),pConsole=T)

hAnaSet <- analyzeSeq( eleSet )
stmpDiff <- Sys.time() - tStamp
k.FLogStr(sprintf("eleSet is made. cost:%.1f%s",stmpDiff,units(stmpDiff)),pConsole=T)






