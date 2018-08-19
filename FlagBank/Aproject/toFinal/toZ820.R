# Z820
source("./toFinal/toZ820_H.R")

saveId <- "Z819"	;rpt=TRUE
load( sprintf("Obj_allIdxLst%s.save",saveId) )
load(sprintf("./save/Obj_gEnv%s.save",saveId))
allZoidGrpName <-"allZoid.idx0"	# 
allIdx <- allIdxLst[[allZoidGrpName]]	# 908523, 1703788, 1465803
stdZoid <- NULL

# simMode start ----------------------------------------------------
	aZoid <- stdZoid <- c(10,21,22,30,35,42) # ZH 820 채워넣을 것.
	allIdxF <- allIdx <- stdIdx <- which(apply(gEnv$allZoidMtx,1,function(zoid){all(zoid==stdZoid)}))
# simMode end   ----------------------------------------------------

finalCut <- function( gEnv ,allIdx ,allZoidGrpName ){
    # cutEadge.getBanPtnColVal() 에서 1~2개 발생 탈락값들에 대한 검토 권장.
	allIdxFObj <- list()
	# 참고 자료 --------------------------------------------------------------------	
	fCutU.rptColValSeqNext( gEnv ,allIdxF ,sprintf("toZ%d",nrow(gEnv$zhF)+1) )
	# aQuoTblLst <- fCutU.getQuoTblLst( gEnv$allZoidMtx[allIdxF,] )
	# aQuoTblStr <- sapply( aQuoTblLst ,function(quoTbl){quoTbl$valStr})	;table(aQuoTblStr)

    allIdxF <- allIdx
	stdMI <- fCutU.getMtxInfo( gEnv$zhF )	#	rptObj<-anaMtx( stdMI$rawTail )	# u0.zoidMtx_ana( stdMI$rawTail )
	# mtxLen  lastZoid    rem quo10   cStep   fStep   rawTail cStepTail   quoTail quoRebPtn



	tStmp <- Sys.time()
	# 기본제거 --------------------------------------------------------------------	
	allIdxF <- fCut.default( gEnv ,allIdxF )
	allIdxF <- fCut.basic( gEnv ,allIdxF )
	cat(sprintf("allIdxF %d\n",length(allIdxF)))
	allIdxFObj$allIdxF.fCut <- allIdxF

	# ------------------------------------------------------------------
	# fCutCnt.**
	#		각 파트에서 2 이상씩은 잘라낸 후,
	#		전체 파트에서 하나도 안 걸린 것들은 제외시키자.
	flgCnt <- fCutCnt.default( gEnv ,allIdxF )
	flag <- flgCnt<2	;table(flag)
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	flgCnt <- fCutCnt.basic( gEnv ,allIdxF )$flgCnt
	flag <- flgCnt<2	;table(flag)
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	flgCnt <- fCutCnt.colValSeqNext( gEnv ,allIdxF )
	flag <- flgCnt<2	;table(flag)
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	flgCnt <- fCutCnt.colValSeqNext.cStep( gEnv ,allIdxF )
	flag <- flgCnt<2	;table(flag)
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	flgCnt <- fCutCnt.nextZW( gEnv ,allIdxF )$flgCnt
	flag <- flgCnt<2	;table(flag)
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	flgCnt <- fCutCnt.nextQuo10( gEnv ,allIdxF )$flgCnt
	flag <- flgCnt<2	;table(flag)
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	flgCnt <- fCutCnt.nextBin( gEnv ,allIdxF )$flgCnt
	flag <- flgCnt<2	;table(flag)
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	flgCnt <- fCutCnt.nextRebNum( gEnv ,allIdxF )$flgCnt
	flag <- flgCnt<2	;table(flag)
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	flgCnt <- fCutCnt.nextCStepBin( gEnv ,allIdxF )$flgCnt
	flag <- flgCnt<2	;table(flag)
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	flgCnt <- fCutCnt.nextFStepBin( gEnv ,allIdxF )$flgCnt
	flag <- flgCnt<2	;table(flag)
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	flgCnt <- fCutCnt.nextColVal_1( gEnv ,allIdxF )$flgCnt
	flag <- flgCnt<2	;table(flag)
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	flgCnt <- fCutCnt.nextColVal_2( gEnv ,allIdxF )$flgCnt
	flag <- flgCnt<2	;table(flag)
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	flgCnt <- fCutCnt.nextColVal_3( gEnv ,allIdxF )$flgCnt
	flag <- flgCnt<2	;table(flag)
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	flgCnt <- fCutCnt.nextColVal_4( gEnv ,allIdxF )$flgCnt
	flag <- flgCnt<2	;table(flag)
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	flgCnt <- fCutCnt.nextColVal_5( gEnv ,allIdxF )$flgCnt
	flag <- flgCnt<2	;table(flag)
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	flgCnt <- fCutCnt.nextColVal_6( gEnv ,allIdxF )$flgCnt
	flag <- flgCnt<2	;table(flag)
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))


	tDiff <- Sys.time() - tStmp	
	allIdxFObj$allIdxF.fCutCnt <- allIdxF

	#=<Final Approach>=======================================================
	#	원래는 맨 마지막이어야 하나, table(flgCnt) 동향파악을 위해 앞으로 옮긴다.
	# allIdxF <- fCut.finalApproach( gEnv ,allIdxF )
	# cat(sprintf("allIdxF %d\n",length(allIdxF)))
	
	# ------------------------------------------------------------------
	# multiple fCutCnt.**
	# flgCnt <- flgCnt + fCutCnt.**( gEnv ,allIdxF )
	#	allIdxF <- allIdxFObj$allIdxF.fCutCnt
	flgCnt <- rep( 0 ,length(allIdxF) )
	flgCnt <- flgCnt + fCutCnt.default( gEnv ,allIdxF )
	flgCnt <- flgCnt + fCutCnt.basic( gEnv ,allIdxF )$flgCnt
	flgCnt <- flgCnt + fCutCnt.colValSeqNext( gEnv ,allIdxF )
	flgCnt <- flgCnt + fCutCnt.colValSeqNext.cStep( gEnv ,allIdxF )
	flgCnt <- flgCnt + fCutCnt.nextZW( gEnv ,allIdxF )$flgCnt
	flgCnt <- flgCnt + fCutCnt.nextQuo10( gEnv ,allIdxF )$flgCnt
	flgCnt <- flgCnt + fCutCnt.nextBin( gEnv ,allIdxF )$flgCnt
	flgCnt <- flgCnt + fCutCnt.nextRebNum( gEnv ,allIdxF )$flgCnt
	flgCnt <- flgCnt + fCutCnt.nextCStepBin( gEnv ,allIdxF )$flgCnt
	flgCnt <- flgCnt + fCutCnt.nextFStepBin( gEnv ,allIdxF )$flgCnt
	flgCnt <- flgCnt + fCutCnt.nextColVal_1( gEnv ,allIdxF )$flgCnt
	flgCnt <- flgCnt + fCutCnt.nextColVal_2( gEnv ,allIdxF )$flgCnt
	flgCnt <- flgCnt + fCutCnt.nextColVal_3( gEnv ,allIdxF )$flgCnt
	flgCnt <- flgCnt + fCutCnt.nextColVal_4( gEnv ,allIdxF )$flgCnt
	flgCnt <- flgCnt + fCutCnt.nextColVal_5( gEnv ,allIdxF )$flgCnt
	flgCnt <- flgCnt + fCutCnt.nextColVal_6( gEnv ,allIdxF )$flgCnt

	allIdxFObj$flgCnt <- flgCnt
	table(flgCnt)
	# length( allIdxFObj$flgCnt )


	flag <- (0<flgCnt)&(flgCnt<3)	# 하나도 안 걸릴 수는 없겠지.
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	allIdxFObj$allIdxF.fCutCnt.m <- allIdxF

	# ------------------------------------------------------------------
	# fCutCnt.**  ... cntMtx, auxCntMtx
	cntMtxLst <- list()
	cntMtxLst[["fCutCnt.basic"]] <- fCutCnt.basic( gEnv ,allIdxF )$cntMtx
	cntMtxLst[["fCutCnt.nextZW"]] <- fCutCnt.nextZW( gEnv ,allIdxF )$cntMtx
	cntMtxLst[["fCutCnt.nextQuo10"]] <- fCutCnt.nextQuo10( gEnv ,allIdxF )$cntMtx
	cntMtxLst[["fCutCnt.nextBin"]] <- fCutCnt.nextBin( gEnv ,allIdxF )$cntMtx
	cntMtxLst[["fCutCnt.nextRebNum"]] <- fCutCnt.nextRebNum( gEnv ,allIdxF )$cntMtx
	cntMtxLst[["fCutCnt.nextCStepBin"]] <- fCutCnt.nextCStepBin( gEnv ,allIdxF )$cntMtx
	cntMtxLst[["fCutCnt.nextFStepBin"]] <- fCutCnt.nextFStepBin( gEnv ,allIdxF )$cntMtx
	cntMtxLst[["fCutCnt.nextColVal_1"]] <- fCutCnt.nextColVal_1( gEnv ,allIdxF )$cntMtx
	cntMtxLst[["fCutCnt.nextColVal_2"]] <- fCutCnt.nextColVal_2( gEnv ,allIdxF )$cntMtx
	cntMtxLst[["fCutCnt.nextColVal_3"]] <- fCutCnt.nextColVal_3( gEnv ,allIdxF )$cntMtx
	cntMtxLst[["fCutCnt.nextColVal_4"]] <- fCutCnt.nextColVal_4( gEnv ,allIdxF )$cntMtx
	cntMtxLst[["fCutCnt.nextColVal_5"]] <- fCutCnt.nextColVal_5( gEnv ,allIdxF )$cntMtx
	cntMtxLst[["fCutCnt.nextColVal_6"]] <- fCutCnt.nextColVal_6( gEnv ,allIdxF )$cntMtx

	cntMtx <- do.call( cbind ,lapply( cntMtxLst ,function(mtx){mtx[,"raw"]}) )
	cntSum <- apply( cntMtx ,1 ,sum )
	# cut 기준을 어떻게 잡아야 할 까...

	auxCntMtxLst <- list()
	auxCntMtxLst[["fCutCnt.basic"]] <- fCutCnt.basic( gEnv ,allIdxF )$auxCntMtx
	auxCntMtxLst[["fCutCnt.nextZW"]] <- fCutCnt.nextZW( gEnv ,allIdxF )$auxCntMtx
	auxCntMtxLst[["fCutCnt.nextQuo10"]] <- fCutCnt.nextQuo10( gEnv ,allIdxF )$auxCntMtx
	auxCntMtxLst[["fCutCnt.nextBin"]] <- fCutCnt.nextBin( gEnv ,allIdxF )$auxCntMtx
	auxCntMtxLst[["fCutCnt.nextRebNum"]] <- fCutCnt.nextRebNum( gEnv ,allIdxF )$auxCntMtx
	auxCntMtxLst[["fCutCnt.nextCStepBin"]] <- fCutCnt.nextCStepBin( gEnv ,allIdxF )$auxCntMtx
	auxCntMtxLst[["fCutCnt.nextFStepBin"]] <- fCutCnt.nextFStepBin( gEnv ,allIdxF )$auxCntMtx
	auxCntMtxLst[["fCutCnt.nextColVal_1"]] <- fCutCnt.nextColVal_1( gEnv ,allIdxF )$auxCntMtx
	auxCntMtxLst[["fCutCnt.nextColVal_2"]] <- fCutCnt.nextColVal_2( gEnv ,allIdxF )$auxCntMtx
	auxCntMtxLst[["fCutCnt.nextColVal_3"]] <- fCutCnt.nextColVal_3( gEnv ,allIdxF )$auxCntMtx
	auxCntMtxLst[["fCutCnt.nextColVal_4"]] <- fCutCnt.nextColVal_4( gEnv ,allIdxF )$auxCntMtx
	auxCntMtxLst[["fCutCnt.nextColVal_5"]] <- fCutCnt.nextColVal_5( gEnv ,allIdxF )$auxCntMtx
	auxCntMtxLst[["fCutCnt.nextColVal_6"]] <- fCutCnt.nextColVal_6( gEnv ,allIdxF )$auxCntMtx

	cntMtx <- do.call( cbind ,lapply( auxCntMtxLst ,function(mtx){mtx[,"auxQuo"]}) )
	cntSum <- apply( cntMtx ,1 ,sum )
	# cut 기준을 어떻게 잡아야 할 까...

	tDiff <- Sys.time() - tStmp
	allIdxFObj$timeCost <- tDiff
	save( allIdxFObj ,file=sprintf("Obj_allIdxFObj_%s.save",allZoidGrpName) )

	allIdxF.bak <- allIdxF

	selIdx <- allIdxFObj$allIdxF.fCutCnt[ allIdxFObj$flgCnt==0 ]
	fCutU.logAllZoidMtx( gEnv$allZoidMtx[selIdx,,drop=F] 
					,logId=sprintf("finalZoid20180728_0") 
				)
	selIdx <- allIdxFObj$allIdxF.fCutCnt[ allIdxFObj$flgCnt==1 ]
	fCutU.logAllZoidMtx( gEnv$allZoidMtx[selIdx,,drop=F] 
					,logId=sprintf("finalZoid20180728_1") 
				)

	#---------------------------------------------------------------------------------------
	# colValLst
	colValLst <- apply( gEnv$allZoidMtx[allIdxF,] ,2 ,function(p){sort(unique(p))})
	colVal <- gEnv$allZoidMtx[allIdxF,1]	;table(colVal)
	colValTblLst.raw <- apply( gEnv$allZoidMtx[allIdxF,] ,2 ,function(p){table(p)})
	colValTblLst.rem <- apply( gEnv$allZoidMtx[allIdxF,] ,2 ,function(p){table(p%%10)})
	cStepMtx <- t(apply(gEnv$allZoidMtx[allIdxF,] ,1 ,function(zoid){zoid[2:6]-zoid[1:5]}))
	colValTblLst.cStep <- apply( cStepMtx ,2 ,function(p){table(p)})
	zw <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(zoid){zoid[6]-zoid[1]})	;sort(table(zw))

	# quoTbl
	quoTblLst <- fCutU.getQuoTblLst( gEnv$allZoidMtx[allIdxF,] )
	quoTblStr <- sapply( quoTblLst ,function(quoTbl){quoTbl$valStr})	# sort( table(quoTblStr) )

	# rebNum
	rebNum <- sapply( 2:nrow(gEnv$zhF) ,function(hIdx){ sum(gEnv$zhF[(hIdx-1),] %in% gEnv$zhF[hIdx,]) })
	rebNum <- c( 0 ,rebNum )
	# rebNumTbl
	rebNumTbl <- table(apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){ sum(lastZoid%in%aZoid) }))



    return( rObj )

} # finalCut()

finalCut.test <- function(){
	# 1 2 3 / 1 4 1	<- lastZoid quo
	quoLst <- apply( gEnv$zhF ,1 ,fCutU.getQuoObj )
	quoFlag <- sapply( quoLst ,function( quoObj ){
					return( quoObj$sameTbl(c(1,4,1)) )
				})
	qIdx <- which(quoFlag)
	qIdx <- setdiff( qIdx ,length(quoLst) )
	preTbl <- sapply( quoLst[qIdx-1] ,function(quoObj){ paste(quoObj$tbl,collapse=" " )})
	postTbl <- sapply( quoLst[qIdx+1] ,function(quoObj){ paste(quoObj$tbl,collapse=" " )})
	cbind( preTbl ,postTbl )
} # finalCut.test()

