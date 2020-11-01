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

prllNum <- 4     # 실수가 잦아서 그냥 오류 코드로 놔둔다.
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
saveId <- sprintf( "Z%d" ,nrow(zhF) )           ;saveId


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




# =====================================================================================
# stdFiltedCnt
stdFiltedCnt <- sapply( fRstLst ,length )   ;names(stdFiltedCnt) <- ( nrow(gEnv$zhF)-length(stdFiltedCnt)+1 ):nrow(gEnv$zhF)
stdFilted.tbl <- table(stdFiltedCnt)
stdFilted.per <- sprintf( "%.1f" ,100*stdFilted.tbl / length(fRstLst) )
names(stdFilted.per) <- names(stdFilted.tbl)



# =====================================================================================
# stdFiltedCnt
allZoid.lst <- vector("list",nrow(gEnv$allZoidMtx))
for( nIdx in attributes(remLst)$name ){
    for( aIdx in remLst[[nIdx]] ){
        allZoid.lst[[aIdx]][1+length(allZoid.lst[[aIdx]])] <- nIdx
    }
}

allZoid.fltCnt <- getAllZoidIdx.FltCnt( gEnv ,remLst )
# allZoidMtx <- gEnv$allZoidMtx[(allZoid.fltCnt==0),]

lastZoid <- gEnv$zhF[nrow(gEnv$zhF),]
# 가정 : allZoid.fltCnt 는 1~3이다.
allZoid.idx0 <- which(allZoid.fltCnt==0)
allZoid.idx1 <- which(allZoid.fltCnt==1)
allZoid.idx2 <- which(allZoid.fltCnt==2)
if( TRUE ){ # allZoid.idx1, allZoid.idx2 동일 필터링 재발 가능성은 제외
    stdFiltedCnt.idx1 <- which(stdFiltedCnt==1)
    lastFlt <- fRstLst[[ stdFiltedCnt.idx1[[length(stdFiltedCnt.idx1)]] ]]
    fName <- do.call( c ,allZoid.lst[allZoid.idx1] )
    allZoid.idx1 <- allZoid.idx1[fName!=lastFlt]

    neverFnd <- setdiff( attributes(remLst)$name ,unique(do.call(c,fRstLst[stdFiltedCnt==1])) )
    fName <- do.call( c ,allZoid.lst[allZoid.idx1] )
    flag <- sapply( allZoid.lst[allZoid.idx1] ,function(p){ p%in%neverFnd })
    allZoid.idx1 <- allZoid.idx1[!flag]

    # 가정 : allZoid.fltCnt의 2영역에서, 바로이전 필터링 결과는 포함되지 않는다.
    #   단, 과거에 일어난 필터링 조합만 사용한다.
    stdFiltedCnt.idx2 <- which(stdFiltedCnt==2)
    lastFlt <- fRstLst[[ stdFiltedCnt.idx2[[length(stdFiltedCnt.idx2)]] ]]
    lastFlt.name <- paste(sort(lastFlt),collapse="_")
    flag <- sapply( allZoid.lst[allZoid.idx2] ,function(p){
                        lastFlt.name != paste(sort(p),collapse="_")
                    })
    allZoid.idx2 <- allZoid.idx2[flag]

    hnt.name <- unique(sapply(fRstLst[stdFiltedCnt.idx2] ,function(p){ paste(sort(p),collapse="_") } ))
    flag <- sapply( allZoid.lst[allZoid.idx2] ,function(p){
                        return( paste(sort(p),collapse="_") %in% hnt.name )
                    })
    allZoid.idx2 <- allZoid.idx2[flag]
}

# =====================================================================================
# allIdxLst
rebCnt <- sapply( 2:nrow(gEnv$zhF) ,function(hIdx){ sum(gEnv$zhF[hIdx,] %in% gEnv$zhF[(hIdx-1),]) } )
rebCnt <- c( 0 ,rebCnt )    ;names(rebCnt) <- 1:nrow(gEnv$zhF)

allIdxLst <- list( allZoid.idx0=allZoid.idx0 ,allZoid.idx1=allZoid.idx1 ,allZoid.idx2=allZoid.idx2 )
allIdxLst$saveId <- saveId
allIdxLst$stdFiltedCnt <- stdFiltedCnt
allIdxLst$stdFiltedCnt.n0 <- names(stdFiltedCnt)[stdFiltedCnt==0]
allIdxLst$stdFiltedCnt.n1 <- names(stdFiltedCnt)[stdFiltedCnt==1]
allIdxLst$infoMtx <- cbind( stdFiltedCnt ,rebCnt[names(stdFiltedCnt)] )
colnames( allIdxLst$infoMtx ) <- c("stdFiltedCnt","rebCnt")

save( allIdxLst, file=sprintf("Obj_allIdxLst%s.save",saveId) )

# Review ---------------------------------------------------------------------------------------
{
    reviewSpan <- 19:0
    hSpan <- nrow(gEnv$zhF) - reviewSpan
    lst <- fRstLst[length(fRstLst)-reviewSpan]
    flt <- do.call( c ,lst )
    tbl <- table(flt)
    tbl[order(tbl,decreasing=T)]

    for( idx in 1:length(hSpan) ){
        rptStr <- sprintf( "H%d(%d)    %s\n",hSpan[idx] ,stdFiltedCnt[as.character(hSpan[idx])] ,paste(lst[[idx]],collapse=" ") )
        cat( rptStr )
    }
}

