# Z818
source("./toFinal/toZ818_H.R")

saveId <- "Z817"	;rpt=TRUE
load( sprintf("Obj_allIdxLst%s.save",saveId) )
load(sprintf("./save/Obj_gEnv%s.save",saveId))
allZoidGrpName <-"allZoid.idx0"	# 800151 ,1696555 ,1348295
allIdx <- allIdxLst[[allZoidGrpName]]

simulIdx <- 818	# c(14,15,25,28,29,30)
stdZoid <- NULL
aZoid <- stdZoid <- gEnv$zhF[simulIdx,]
allIdxF <- allIdx <- stdIdx <- which(apply(gEnv$allZoidMtx,1,function(zoid){all(zoid==stdZoid)}))

# gEnv$zhF <- gEnv$zhF[1:(simulIdx-1),]

finalCut <- function( gEnv ,allIdx ,allZoidGrpName ){
    # cutEadge.getBanPtnColVal() 에서 1~2개 발생 탈락값들에 대한 검토 권장.
	allIdxFObj <- list()
	# 참고 자료 --------------------------------------------------------------------	
	fCutU.rptColValSeqNext( gEnv ,allIdxF ,"z817")
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
	flgCnt <- fCutCnt.basic( gEnv ,allIdxF )
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
	flgCnt <- fCutCnt.nextZW( gEnv ,allIdxF )
	flag <- flgCnt<2	;table(flag)
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	flgCnt <- fCutCnt.nextQuo10( gEnv ,allIdxF )
	flag <- flgCnt<2	;table(flag)
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	flgCnt <- fCutCnt.nextBin( gEnv ,allIdxF )
	flag <- flgCnt<2	;table(flag)
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	flgCnt <- fCutCnt.nextRebNum( gEnv ,allIdxF )		# fltCnt 2
	flag <- flgCnt<2	;table(flag)
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	flgCnt <- fCutCnt.nextCStepBin( gEnv ,allIdxF )		# fltCnt 1
	flag <- flgCnt<2	;table(flag)
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	for( cutCol.idx in c(1,3,6) ){	# fltCnt 1,12, 1
		cutCol.val.span <- sort(unique(gEnv$allZoidMtx[allIdxF,cutCol.idx]))
		cutCol.val.span <- setdiff( cutCol.val.span ,stdMI$lastZoid[cutCol.idx] )
		for( cutCol.val in cutCol.val.span ){
			flgCnt <- fCutCnt.colValStd( gEnv ,allIdxF ,cutCol.idx ,cutCol.val )
			flag <- flgCnt<2	;table(flag)
			allIdxF <- allIdxF[flag]
			cat(sprintf("colVal %d(col %d)  allIdxF %d\n",cutCol.val,cutCol.idx,length(allIdxF)))
		}
	}
	zWidth.span <- sort(unique(gEnv$allZoidMtx[allIdxF,6]-gEnv$allZoidMtx[allIdxF,1]))
	zWidth.span <- setdiff( zWidth.span ,stdMI$lastZoid[6]-stdMI$lastZoid[1])
	for( zWidth in zWidth.span ){	# fltCnt 1
		flgCnt <- fCutCnt.zWidthStd( gEnv ,allIdxF ,zWidth )
		flag <- flgCnt<2	;table(flag)
		allIdxF <- allIdxF[flag]
		cat(sprintf("zWidth %d  allIdxF %d\n",zWidth,length(allIdxF)))
	}

	aQuoTblObjLst <- fCutU.getQuoTblLst( gEnv$allZoidMtx[allIdxF,,drop=F] )	# tbl valStr quoStr idStr
	aQuoTblStr <- sapply( aQuoTblObjLst ,function(quoTbl){quoTbl$valStr})
	aQuoTblSpan <- sort(unique(aQuoTblStr))	#	table(aQuoTblStr)
	aQuoTblSpan <- aQuoTblSpan[aQuoTblSpan!=stdMI$quo10$valStr]
	for( tblStr in aQuoTblSpan ){	# fltCnt 1
		flgCnt <- fCutCnt.quoTblStd( gEnv ,allIdxF ,tblStr )
		flag <- flgCnt<2	;table(flag)
		allIdxF <- allIdxF[flag]
		cat(sprintf("tblStr %s  allIdxF %d\n",tblStr,length(allIdxF)))
	}

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
	flgCnt <- flgCnt + fCutCnt.basic( gEnv ,allIdxF )
	flgCnt <- flgCnt + fCutCnt.colValSeqNext( gEnv ,allIdxF )
	flgCnt <- flgCnt + fCutCnt.colValSeqNext.cStep( gEnv ,allIdxF )
	flgCnt <- flgCnt + fCutCnt.nextZW( gEnv ,allIdxF )
	flgCnt <- flgCnt + fCutCnt.nextQuo10( gEnv ,allIdxF )
	flgCnt <- flgCnt + fCutCnt.nextBin( gEnv ,allIdxF )
	flgCnt <- flgCnt + fCutCnt.nextRebNum( gEnv ,allIdxF )
	flgCnt <- flgCnt + fCutCnt.nextCStepBin( gEnv ,allIdxF )

	for( cutCol.idx in c(1,3,6) ){
		cutCol.val.span <- sort(unique(gEnv$allZoidMtx[allIdxF,cutCol.idx]))
		cutCol.val.span <- setdiff( cutCol.val.span ,stdMI$lastZoid[cutCol.idx] )
		for( cutCol.val in cutCol.val.span ){
			flgCnt <- flgCnt + fCutCnt.colValStd( gEnv ,allIdxF ,cutCol.idx ,cutCol.val )
		}
	}
	zWidth.span <- sort(unique(gEnv$allZoidMtx[allIdxF,6]-gEnv$allZoidMtx[allIdxF,1]))
	zWidth.span <- setdiff( zWidth.span ,stdMI$lastZoid[6]-stdMI$lastZoid[1])
	for( zWidth in zWidth.span ){
		flgCnt <- flgCnt + fCutCnt.zWidthStd( gEnv ,allIdxF ,zWidth )
		cat(sprintf("zWidth:%d\n",zWidth))
	}

	aQuoTblObjLst <- fCutU.getQuoTblLst( gEnv$allZoidMtx[allIdxF,,drop=F] )	# tbl valStr quoStr idStr
	aQuoTblStr <- sapply( aQuoTblObjLst ,function(quoTbl){quoTbl$valStr})
	aQuoTblSpan <- sort(unique(aQuoTblStr))	#	table(aQuoTblStr)
	aQuoTblSpan <- aQuoTblSpan[aQuoTblSpan!=stdMI$quo10$valStr]
	for( tblStr in aQuoTblSpan ){
		flgCnt <- flgCnt + fCutCnt.quoTblStd( gEnv ,allIdxF ,tblStr )
		cat(sprintf("tblStr:%s\n",tblStr))
	}

	# flgCnt <- flgCnt + fCutCnt.colVal_1_x( gEnv ,allIdxF )
	# flgCnt <- flgCnt + fCutCnt.colVal_3_x( gEnv ,allIdxF )
	# flgCnt <- flgCnt + fCutCnt.colVal_5_x( gEnv ,allIdxF )
	# flgCnt <- flgCnt + fCutCnt.colVal_6_x( gEnv ,allIdxF )
	# flgCnt <- flgCnt + fCutCnt.zWidth( gEnv ,allIdxF )
	# flgCnt <- flgCnt + fCutCnt.quoTbl( gEnv ,allIdxF )
	
	allIdxFObj$flgCnt <- flgCnt
	table(flgCnt)
	# length( allIdxFObj$flgCnt )
	

	flag <- (0<flgCnt)&(flgCnt<3)	# 하나도 안 걸릴 수는 없겠지.
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	allIdxFObj$allIdxF.fCutCnt.m <- allIdxF


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

