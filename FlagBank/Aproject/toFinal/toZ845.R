# Z844
workH <- 844	;rpt=TRUE
source(sprintf("./toFinal/toZ%d_H.R",workH))

load( sprintf("Obj_allIdxLstZ%d.save",workH-1) )
load(sprintf("./save/Obj_gEnvZ%d.save",workH-1))
allZoidGrpName <-"allZoid.idx0"						#	
allIdx <- allIdxLst[[allZoidGrpName]]	#
allIdxF <- 1000:1010		;stdZoid <- NULL
# 참고 자료 --------------------------------------------------------------------	
fCutU.rptColValSeqNext( gEnv ,allIdxF ,sprintf("toZ%d",workH) )


# simMode start ----------------------------------------------------
	aZoid <- stdZoid <- c(  7, 8,13,15,33,45 ) # ZH 843 채워넣을 것.
	allIdxF <- allIdx <- stdIdx <- 4908396
		# which(apply(gEnv$allZoidMtx,1,function(zoid){all(zoid==stdZoid)}))
	u0.saveStdZoidFltRst( workH )
# simMode end   ----------------------------------------------------

finalCut <- function( gEnv ,allIdx ,allZoidGrpName ){
    # cutEadge.getBanPtnColVal() 에서 1~2개 발생 탈락값들에 대한 검토 권장.
	allIdxFObj <- list()
	# aQuoTblLst <- fCutU.getQuoTblLst( gEnv$allZoidMtx[allIdxF,] )
	# aQuoTblStr <- sapply( aQuoTblLst ,function(quoTbl){quoTbl$valStr})	;table(aQuoTblStr)

    allIdxF <- allIdx
	stdMI <- fCutU.getMtxInfo( gEnv$zhF )	#	rptObj<-anaMtx( stdMI$rawTail )	# u0.zoidMtx_ana( stdMI$rawTail )


	tStmp <- Sys.time()
	# 기본제거 --------------------------------------------------------------------	
	allIdxF <- fCut.default( gEnv ,allIdxF )		;allIdxF.bak <- allIdxF

	# colValSeqNext ------------------------------------------------------
	flgCnt <- fCutCnt.colValSeqNext( gEnv ,allIdxF )
	flgCnt <- flgCnt + fCutCnt.colValSeqNext.cStep( gEnv ,allIdxF )
	flgCnt <- flgCnt + fCutCnt.default( gEnv ,allIdxF )	# 효율이 의문시된다.
	flag <- flgCnt<2	;table(flag)
    allIdxF <- allIdxF[flag]
	allIdxF.bak <- allIdxF
	allIdxFObj$allIdxF.colValSeqNext <- allIdxF
	cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# fCut.basic( gEnv ,allIdxF ) -----------------------------------------
	allIdxF <- fCut.basic( gEnv ,allIdxF )
	allIdxFObj$allIdxF.fCut <- allIdxF
	tDiff <- Sys.time() - tStmp	

	# ff0.filtByOnePhase( gEnv ,allIdxF ) ---------------------------------
	allIdxF <- ff0.filtByOnePhase( gEnv ,allIdxF )
	cat(sprintf("allIdxF %d\n",length(allIdxF)))
	allIdxFObj$filtByOnePhase <- allIdxF

	# u0.getPhObjLst( gEnv ,allIdxF ) -------------------------------------
	ccObjLst <- u0.getPhObjLst( gEnv ,allIdxF )
	save( ccObjLst ,file="Obj_ccObjLst.save" )
	tDiff <- Sys.time() - tStmp

	if( TRUE ){	# cntMtx[,c("cStep","fStep")] <-- w1,w2 제거
		for( phIdx in attributes(ccObjLst)$names ){
			cntMtx <- ccObjLst[[nIdx]]$cntMtx
			cStep <- cntMtx[,"cStep"] - cntMtx[,"cStep.w1"] - cntMtx[,"cStep.w2"]
			ccObjLst[[nIdx]]$cntMtx[,"cStep"] <- ifelse( cStep>0 ,cStep ,0 )
			fStep <- cntMtx[,"fStep"] - cntMtx[,"fStep.w1"] - cntMtx[,"fStep.w2"]
			ccObjLst[[nIdx]]$cntMtx[,"fStep"] <- ifelse( fStep>0 ,fStep ,0 )
		}
	}
	flagCntMtx		<- fltCntMtx(		ccObjLst	,allIdxF )
	flagScoreMtx	<- fltScoreMtx(		ccObjLst	,allIdxF )
	flagCStepValMtx	<- fltCStepValMtx(	ccObjLst	,allIdxF )

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

