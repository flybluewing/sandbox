headerFiles <- c( "../breedingPlace/20171116_A_H.R" 
                ,"../breedingPlace/20171116_A_H_cliper.R"
                ,"../breedingPlace/20171116_B_H.R"
                ,"../breedingPlace/20171116_C_H.R"
                ,"../breedingPlace/20171116_D_H.R"
                ,"20180109_A_H.R"
                ,"20180109_C_H.R" ,"20180109_C_HRad.R" ,"20180109_C_HUnit.R"
                ,"20180109_D_H.R"
                ,"./lib/fCutU_H.R"
                ,"./lib/u0_H.R" ,"./lib/u1_H.R"
                ,"./lib/ff0_H.R"
                ,"./lib/refine0_H.R" ,"./lib/refine1_H.R" ,"./lib/refine2_H.R" ,"./lib/refine3_H.R" ,"./lib/refine4_H.R"
)
for( idx in 1:length(headerFiles) ) source(headerFiles[idx])

prllNum <- 3     # 실수가 잦아서 그냥 오류 코드로 놔둔다.
prllLog <- k.getFlogObj( "./log/parallel_log.txt" )
prll.initHeader <- function( ){
    k <- sfLapply(1:prllNum,function(prllId){
        curWd <- getwd()
        setwd("..")
        source("hCommon.R")

        setwd( curWd )
        for( idx in 1:length(headerFiles) ) source(headerFiles[idx])
    })
}

sfInit( parallel=T, cpus=prllNum )
sfExport("prllLog") ;sfExport("headerFiles")
prll.initHeader( )
prllLog$fLogStr("parallel init", pTime=T ,pAppend=F )
cat(sprintf("* Parallel ready... see log : %s \n",prllLog$fileName))







curWd <- getwd()	;setwd("..")
FB.f <- getFlagBank("./zoidHistory/ZH_Final.csv")
setwd(curWd)
zhF	<- as.matrix( FB.f$zh )	;rownames(zhF) <- 1:nrow(zhF)
lastZoid <- zhF[nrow(zhF),]
saveId <- sprintf( "Z%d" ,nrow(zhF) )


testSpan <- 300:nrow(zhF)
filtFuncLst <- getFiltLst.base()
k.FLogStr(sprintf("Start filt (filt number:%d ,saveId:%s)",length(filtFuncLst),saveId)	)


# =====================================================================================
# fRstLst - 이전 Zoid History들의 필터링 테스트.
logFile <- sprintf("./log/gEnv%s.log",saveId)
fRstLst <- list() # 각 hIdx에서 걸린 필터들의 ID
sfExport("zhF") ;sfExport("logFile")    ;sfExport("filtFuncLst")
resultLst <- sfLapply( testSpan ,function( hIdx ){
	gEnv <- list( allZoidMtx = zhF	    ,zhF = zhF[1:(hIdx-1),]
					,logFile = logFile	,doLog = TRUE
				)
	gEnv$log <- function( pMsg ){ if(gEnv$doLog) k.FLog(pMsg ,pFile=gEnv$logFile,pTime=F) }
	gEnv$logStr <- function( pMsg ){ if(gEnv$doLog) k.FLogStr( pMsg ,pFile=gEnv$logFile,pTime=F) }

	remLst <- list()
	for( fIdx in 1:length(filtFuncLst) ){
		rstObj <- filtFuncLst[[fIdx]]( gEnv )	# 소요시간 rstObj$tCost
		remLst[[rstObj$filtId]] <- which( !rstObj$flag )
	} # fIdx

	remFlag <- sapply( remLst ,function(p){ hIdx%in%p })
	fRst <- attributes(remLst)$names[remFlag]

    gc()
    prllLog$fLogStr(sprintf("current test : %d",hIdx),pTime=T)
    return( list(hIdx=hIdx ,fRst=fRst) )
})
hIdx <- sapply( resultLst ,function(p){p$hIdx})
hIdx.ord <- order(hIdx)
fRstLst <- lapply( resultLst[hIdx.ord] ,function(p){p$fRst})
names(fRstLst) <- hIdx[hIdx.ord]
resultLst <- NULL


save( fRstLst ,file=sprintf("./save/Obj_fRstLst%s.save",saveId) )
k.FLogStr(sprintf("fRstLst is created.(logfile:%s)",logFile))


# =====================================================================================
# gEnv, remLst -실제 AllZoidMtx 대상
logFile <- sprintf("./log/allZoidMtx%s.log",saveId)
gEnv <- list( allZoidMtx = getAllZoid() ,zhF = zhF
				,logFile = logFile		,doLog = TRUE
			)
gEnv$log <- function( pMsg ){ if(gEnv$doLog) k.FLog(pMsg ,pFile=gEnv$logFile) }
gEnv$logStr <- function( pMsg ){ if(gEnv$doLog) k.FLogStr( pMsg ,pFile=gEnv$logFile) }
save( gEnv ,file=sprintf("./save/Obj_gEnv%s.save",saveId) )
k.FLogStr(sprintf("gEnv is created.(logfile:%s)",logFile))

# for test Data
#   selIdx <- sample( 1:nrow( gEnv$allZoidMtx ) ,10000 )    ;selIdx <- sort(selIdx) ;gEnv$allZoidMtx <- gEnv$allZoidMtx[selIdx,]
tStmp <- Sys.time()
sfExport("gEnv")
resultLst <- sfLapply( filtFuncLst ,function( filtFunc ){
    tStmp <- Sys.time()
    rstObj <- filtFunc( gEnv )	# 소요시간 rstObj$tCost
    remIdx <- which( !rstObj$flag )

    gc()
    tDiff <- Sys.time() - tStmp
    prllLog$fLogStr(sprintf("current filt:%s  time:%.1f%s",rstObj$filtId,tDiff,units(tDiff)),pTime=T)
    return( list( fIdx=rstObj$filtId ,remIdx=remIdx ) )
})
remLst <- lapply( resultLst ,function(p){p$remIdx})
names(remLst) <- sapply( resultLst ,function(p){p$fIdx})

tDiff <- Sys.time() - tStmp
save( remLst ,file=sprintf("./save/Obj_remLst%s.save",saveId) )
# load("Obj_remLst.save")
k.FLogStr(sprintf("remLst is created.(logfile:%s)",logFile))

