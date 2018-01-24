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
saveId <- gsub(".*-([0-9]*)-([0-9]*) ([0-9]*).*","\\1\\2_\\3"
				,sprintf("%s",Sys.time())
			)
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

filtFuncLst <- getFiltLst.base()

k.FLogStr(sprintf("Start filt (filt number:%d ,saveId:%s)"
			,length(filtFuncLst),saveId)
		)

# =====================================================================================
# 실제 Zoid History들의 필터링 테스트.
logFile <- sprintf("./log/gEnv%s.log",saveId)
fRstLst <- list() # 각 hIdx에서 걸린 필터들의 ID
for( hIdx in testSpan ){ # 35분 정도 소요.(388 ZH, 21 Filt)

	gEnv <- list( allZoidMtx = allZoidMtx 
					,zhF = zhF[1:(hIdx-1),]
					,logFile = logFile
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

	if( 0==(hIdx%%100) ){
		k.FLogStr(sprintf("current test : %d",hIdx))
	}

} # hIdx

save( fRstLst ,file=sprintf("./save/Obj_fRstLst%s.save",saveId) )
# load("Obj_fRstLst.save")
k.FLogStr(sprintf("fRstLst is created.(logfile:%s)",logFile))

# =====================================================================================
# 실제 AllZoidMtx 대상
logFile <- sprintf("./log/allZoidMtx%s.log",saveId)
gEnv <- list( allZoidMtx = getAllZoid()
				,zhF = zhF
				,logFile = logFile
				,doLog = TRUE
			)
gEnv$log <- function( pMsg ){ if(gEnv$doLog) k.FLog(pMsg ,pFile=gEnv$logFile) }
gEnv$logStr <- function( pMsg ){ if(gEnv$doLog) k.FLogStr( pMsg ,pFile=gEnv$logFile) }
save( gEnv ,file=sprintf("./save/Obj_gEnv%s.save",saveId) )
k.FLogStr(sprintf("gEnv is created.(logfile:%s)",logFile))

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
tDiff <- Sys.time() - tStmp
save( remLst ,file=sprintf("./save/Obj_remLst%s.save",saveId) )
# load("Obj_remLst.save")
k.FLogStr(sprintf("remLst is created.(logfile:%s)",logFile))
