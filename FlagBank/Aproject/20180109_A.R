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
filtFuncLst[[1+length(filtFuncLst)]] <- filt_A0030

filtFuncLst[[1+length(filtFuncLst)]] <- filt_A0100.A
filtFuncLst[[1+length(filtFuncLst)]] <- filt_A0110.A
filtFuncLst[[1+length(filtFuncLst)]] <- filt_AK000.A
filtFuncLst[[1+length(filtFuncLst)]] <- filt_AK000.B
filtFuncLst[[1+length(filtFuncLst)]] <- filt_AK000.C
filtFuncLst[[1+length(filtFuncLst)]] <- filt_AK000.D

filtFuncLst[[1+length(filtFuncLst)]] <- filt_AL000.A
filtFuncLst[[1+length(filtFuncLst)]] <- filt_AP000.A
filtFuncLst[[1+length(filtFuncLst)]] <- filt_AP000.B
filtFuncLst[[1+length(filtFuncLst)]] <- filt_AP000.C
filtFuncLst[[1+length(filtFuncLst)]] <- filt_AP000.D
filtFuncLst[[1+length(filtFuncLst)]] <- filt_AP000.E
filtFuncLst[[1+length(filtFuncLst)]] <- filt_AQ000.A
filtFuncLst[[1+length(filtFuncLst)]] <- filt_AR000.A
filtFuncLst[[1+length(filtFuncLst)]] <- filt_AR000.B
filtFuncLst[[1+length(filtFuncLst)]] <- filt_AS000.A

filtFuncLst[[1+length(filtFuncLst)]] <- filt_C0000.A
filtFuncLst[[1+length(filtFuncLst)]] <- filt_C1000.A

# =====================================================================================
# 실제 Zoid History들의 필터링 테스트.
fRstLst <- list() # 각 hIdx에서 걸린 필터들의 ID
for( hIdx in testSpan ){ # 35분 정도 소요.(388 ZH, 21 Filt)

	gEnv <- list( allZoidMtx = allZoidMtx 
					,zhF = zhF[1:(nrow(zhF)-1),]
					,logFile = "./log/gEnv.log"
					,doLog = TRUE
				)
	gEnv$log <- function( pMsg ){ if(gEnv$doLog) k.FLog(pMsg ,pFile=gEnv$logFile,pTime=F) }
	gEnv$logStr <- function( pMsg ){ if(gEnv$doLog) k.FLogStr( pMsg ,pFile=gEnv$logFile,pTime=F) }

	remLst <- list()
	for( fIdx in 1:length(filtFuncLst) ){
		rstObj <- filtFuncLst[[fIdx]]( gEnv )	# 소요시간 rstObj$tCost
		remLst[[rstObj$filtId]] <- which( !rstObj$flag )
	} # fIdx

	remFlag <- sapply( remLst ,function(p){ hIdx%in%p })
	fRstLst[[1+length(fRstLst)]] <- attributes(remLst)$names[remFlag]

	k.FLogStr(sprintf("current test : %d",hIdx))

} # hIdx

filtedCnt <- sapply( fRstLst ,length )
fRstMtx <- do.call( rbind ,fRstLst[filtedCnt==5] )
rownames(fRstMtx) <- testSpan[which( filtedCnt==5 )]
# -------------------------------------------------------------------
# fRstMtx 에 대해.. 최대 가뭄은 15번.
#	- 2번 까지는 동일반복 없음
	matMtx <- scanSameRow( fRstMtx )
# 	- ptn$nextVal 일치검사. 19개 기존 패턴 중 5개 일치 없음.(4개는 2번)
	inspSpan <- 10:nrow(fRstMtx)
	flag <- rep( -1 ,nrow(fRstMtx) )
	for( iIdx in inspSpan ){
		ptn <- getPtnReb( fRstMtx[1:(iIdx-1),] )
		if( !is.null(ptn) ){
			flag[iIdx] <- sum(fRstMtx[iIdx,]==ptn$nextRow)
		}
	}




#		
# save( fRstLst ,file="Obj_fRstLst.save" )






# =====================================================================================
# 실제 AllZoidMtx 대상
gEnv <- list( allZoidMtx = getAllZoid()
				,zhF = zhF
				,logFile = "./log/allZoidMtx.log"
				,doLog = TRUE
			)
gEnv$log <- function( pMsg ){ if(gEnv$doLog) k.FLog(pMsg ,pFile=gEnv$logFile) }
gEnv$logStr <- function( pMsg ){ if(gEnv$doLog) k.FLogStr( pMsg ,pFile=gEnv$logFile) }

tStmp <- Sys.time()
remLst <- list()
for( fIdx in 1:length(filtFuncLst) ){
	rstObj <- filtFuncLst[[fIdx]]( gEnv )	# 소요시간 rstObj$tCost
	remLst[[rstObj$filtId]] <- which( !rstObj$flag )
	tDiff <- Sys.time() - tStmp
	k.FLogStr(sprintf("current filt:%s  time:%.1f%s",rstObj$filtId,tDiff,units(tDiff)))
	if( 0==(fIdx%%2) ){
		k.FLogStr(sprintf("   memory:%.1f",memory.size()))
		gc()
	}
} # fIdx

save( remLst ,file="Obj_remLst.save" )



