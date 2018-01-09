# 20180109_A.R 마지막 시도가 되길..
source("../breedingPlace/20171116_A_H.R")
source("../breedingPlace/20171116_A_H_cliper.R")
source("../breedingPlace/20171116_B_H.R")
source("../breedingPlace/20171116_C_H.R")
source("../breedingPlace/20171116_D_H.R")
source("20180109_A_H.R")

curWd <- getwd()	;setwd("..")
FB <- getFlagBank()
FB.f <- getFlagBank("./zoidHistory/ZH_Final.csv")
setwd(curWd)
zh	<- as.matrix( FB$zh )
zhF	<- as.matrix( FB.f$zh )	;rownames(zhF) <- 1:nrow(zhF)
lastZoid <- zhF[nrow(zhF),]
filtLst <- list()
getFiltHist <- function( pFiltId ,pTStmp ,pAllZoidMtx ,pFlag=NULL ){
		rObj <- list( filtId=pFiltId ,tCost=(Sys.time()-pTStmp) 
						,zoidSize=nrow(pAllZoidMtx)
						,flag=pFlag 
					)
		return( rObj )
	}
#allZoidMtx <- getAllZoid() # 38sec
allZoidMtx <- zhF	# 일단 필터링 결과를 보기위해..

# zhF ,allZoidMtx
testSpan <- 400:nrow(zhF)

filtFuncLst <- list()
filtFuncLst[[1+length(filtFuncLst)]] <- filt_A0010
filtFuncLst[[1+length(filtFuncLst)]] <- filt_A0020


fRstLst <- list() # 각 hIdx에서 걸린 필터들의 ID
for( hIdx in testSpan ){

	gEnv <- list( allZoidMtx = allZoidMtx 
					,zhF = zhF[1:(nrow(zhF)-1),]
					,logFile = "./log/envLog.txt"
					,doLog = TRUE
				)
	gEnv$log <- function( pMsg ){ if(gEnv$doLog) k.FLog(pMsg ,pFile=gEnv$logFile) }
	gEnv$logStr <- function( pMsg ){ if(gEnv$doLog) k.FLogStr( pMsg ,pFile=gEnv$logFile) }

	remLst <- list()
	for( fIdx in 1:length(filtFuncLst) ){
		rstObj <- filtFuncLst[[fIdx]]( gEnv )	# 소요시간 rstObj$tCost
		remLst[[rstObj$filtId]] <- which( !rstObj$flag )
	} # fIdx

	remFlag <- sapply( remLst ,function(p){ hIdx%in%p })
	fRstLst[[1+length(fRstLst)]] <- attributes(remLst)$names[remFlag]

} # hIdx

filedCnt <- sapply( fRstLst ,length )
