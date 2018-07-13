# to20180714.R 최종접근
source("./toFinal/to20180714_H.R")

saveId <- "Z814"
load( sprintf("Obj_allIdxLst%s.save",saveId) )
load(sprintf("./save/Obj_gEnv%s.save",saveId))

allIdx <- allIdxLst$allZoid.idx0	# 525364

finalCut <- function( gEnv ,allIdx ){
    # cutEadge.getBanPtnColVal() 에서 1~2개 발생 탈락값들에 대한 검토 권장.

    allIdxF <- allIdx
	allIdxFObj <- list()
	lastZoid <- gEnv$zhF[nrow(gEnv$zhF),]
	cStep <- lastZoid[2:6] - lastZoid[1:5]
	fStep <- lastZoid - gEnv$zhF[nrow(gEnv$zhF)-1,]

	# 참고 자료 --------------------------------------------------------------------
    rebCnt <- sapply( 2:nrow(gEnv$zhF) ,function(idx){
                    cnt <- sum( gEnv$zhF[idx-1,] %in% gEnv$zhF[idx,] )
                    return(cnt)
                })
    azColValLst <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,2 ,function(p){sort(unique(p))})
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# 	801 17 25 28 37 43 44
	# 	802 10 11 12 18 24 42
	# 	803  5  9 14 26 30 43
	# 	804  1 10 13 26 32 36
	#   805  3 12 13 18 31 32
	#   806 14,20,23,31,37,38
	#   807  6,10,18,25,34,35
	#   808 15 21 31 32 41 43
	#	809  6 11 15 17 23 40
	#   810  5 10 13 21 39 43
	#	811  8 11 19 21 36 45
    #   812  1  3 12 14 16 43
	#	813 11 30 34 35 42 44
	#	814  2 21 28 38 42 45

	tStmp <- Sys.time()
	allIdxF <- fCut.customStatic( gEnv ,allIdxF )
	allIdxF <- fCut.colValSeqNext( gEnv ,allIdxF )
	allIdxF <- fCut.colValSeqNext.cStep( gEnv ,allIdxF )
	allIdxF <- fCut.cust.nextZW( gEnv ,allIdxF )
	allIdxF <- fCut.cust.NextQuo10( gEnv ,allIdxF )
	allIdxFObj$allIdxF.fCut <- allIdxF

	# colValLst
	colValLst <- apply( gEnv$allZoidMtx[allIdxF,] ,2 ,function(p){sort(unique(p))})
	# rebNum
	rebNum <- sapply( 2:nrow(gEnv$zhF) ,function(hIdx){ sum(gEnv$zhF[(hIdx-1),] %in% gEnv$zhF[hIdx,]) })
	rebNum <- c( 0 ,rebNum )
	# rebNumTbl
	rebNumTbl <- table(apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){ sum(lastZoid%in%aZoid) }))

	# ------------------------------------------------------------------
	# fCutCnt.**
	#		각 파트에서 2 이상씩은 잘라낸 후,
	#		전체 파트에서 하나도 안 걸린 것들은 제외시키자.
	flgCnt <- fCutCnt.customCnt( gEnv ,allIdxF )
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

	flgCnt <- fCutCnt.cust.nextZW( gEnv ,allIdxF )
	flag <- flgCnt<2	;table(flag)
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	flgCnt <- fCutCnt.cust.NextQuo10( gEnv ,allIdxF )
	flag <- flgCnt<2	;table(flag)
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	flgCnt <- fCutCnt.cust.getNextBin( gEnv ,allIdxF )
	flag <- flgCnt<2	;table(flag)
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	flgCnt <- fCutCnt.cust.colval1_03( gEnv ,allIdxF )
	flag <- flgCnt<2	;table(flag)
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	flgCnt <- fCutCnt.cust.colval1_05( gEnv ,allIdxF )
	flag <- flgCnt<2	;table(flag)
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	flgCnt <- fCutCnt.cust.colval1_07( gEnv ,allIdxF )
	flag <- flgCnt<2	;table(flag)
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	flgCnt <- fCutCnt.cust.colval1_09( gEnv ,allIdxF )
	flag <- flgCnt<2	;table(flag)
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	cutCol.idx <- 3
	cutCol.val.span <- sort(unique(gEnv$allZoidMtx[allIdxF,cutCol.idx]))
	cutCol.val.span <- setdiff( cutCol.val.span ,lastZoid[cutCol.idx] )
	for( cutCol.val in cutCol.val.span ){
		flgCnt <- fCutCnt.colValStd( gEnv ,allIdxF ,cutCol.idx ,cutCol.val )
		flag <- flgCnt<2	;table(flag)
		allIdxF <- allIdxF[flag]
		cat(sprintf("colVal %d(col %d)  allIdxF %d\n",cutCol.val,cutCol.idx,length(allIdxF)))
	}

	cutCol.idx <- 6
	cutCol.val.span <- sort(unique(gEnv$allZoidMtx[allIdxF,cutCol.idx]))
	cutCol.val.span <- setdiff( cutCol.val.span ,lastZoid[cutCol.idx] )
	for( cutCol.val in cutCol.val.span ){
		flgCnt <- fCutCnt.colValStd( gEnv ,allIdxF ,cutCol.idx ,cutCol.val )
		flag <- flgCnt<2	;table(flag)
		allIdxF <- allIdxF[flag]
		cat(sprintf("colVal %d(col %d)  allIdxF %d\n",cutCol.val,cutCol.idx,length(allIdxF)))
	}

	zWidth.span <- sort(unique(gEnv$allZoidMtx[allIdxF,cutCol.idx]))
	zWidth.span <- setdiff( zWidth.span ,lastZoid[6]-lastZoid[1])
	for( zWidth in zWidth.span ){
		flgCnt <- fCutCnt.zWidthStd( gEnv ,allIdxF ,zWidth )
		flag <- flgCnt<2	;table(flag)
		allIdxF <- allIdxF[flag]
		cat(sprintf("zWidth %d  allIdxF %d\n",zWidth,length(allIdxF)))
	}


	allIdxFObj$allIdxF.fCutCnt <- allIdxF
	
	# multiple fCutCnt.**
	# flgCnt <- flgCnt + fCutCnt.**( gEnv ,allIdxF )
	#	allIdxF <- allIdxFObj$allIdxF.fCutCnt
	flgCnt <- rep( 0 ,length(allIdxF) )
	flgCnt <- flgCnt + fCutCnt.customCnt( gEnv ,allIdxF )
	flgCnt <- flgCnt + fCutCnt.colValSeqNext( gEnv ,allIdxF )
	flgCnt <- flgCnt + fCutCnt.colValSeqNext.cStep( gEnv ,allIdxF )
	flgCnt <- flgCnt + fCutCnt.cust.nextZW( gEnv ,allIdxF )
	flgCnt <- flgCnt + fCutCnt.cust.NextQuo10( gEnv ,allIdxF )
	flgCnt <- flgCnt + fCutCnt.cust.getNextBin( gEnv ,allIdxF )
	flgCnt <- flgCnt + fCutCnt.cust.colval1_03( gEnv ,allIdxF )
	flgCnt <- flgCnt + fCutCnt.cust.colval1_05( gEnv ,allIdxF )
	flgCnt <- flgCnt + fCutCnt.cust.colval1_07( gEnv ,allIdxF )
	flgCnt <- flgCnt + fCutCnt.cust.colval1_09( gEnv ,allIdxF )

	cutCol.idx <- 3
	cutCol.val.span <- sort(unique(gEnv$allZoidMtx[allIdxF,cutCol.idx]))
	cutCol.val.span <- setdiff( cutCol.val.span ,lastZoid[cutCol.idx] )
	for( cutCol.val in cutCol.val.span ){
		flgCnt <- flgCnt + fCutCnt.colValStd( gEnv ,allIdxF ,cutCol.idx ,cutCol.val )
	}
	cutCol.idx <- 6
	cutCol.val.span <- sort(unique(gEnv$allZoidMtx[allIdxF,cutCol.idx]))
	cutCol.val.span <- setdiff( cutCol.val.span ,lastZoid[cutCol.idx] )
	for( cutCol.val in cutCol.val.span ){
		flgCnt <- flgCnt + fCutCnt.colValStd( gEnv ,allIdxF ,cutCol.idx ,cutCol.val )
	}
	zWidth.span <- sort(unique(gEnv$allZoidMtx[allIdxF,cutCol.idx]))
	zWidth.span <- setdiff( zWidth.span ,lastZoid[6]-lastZoid[1])
	for( zWidth in zWidth.span ){
		flgCnt <- flgCnt + fCutCnt.zWidthStd( gEnv ,allIdxF ,zWidth )
		cat(sprintf("zWidth:%d\n",zWidth))
	}

	table(flgCnt)
	flag <- (0<flgCnt)&(flgCnt<3)	# 하나도 안 걸릴 수는 없겠지.
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	allIdxFObj$allIdxF.fCutCnt.m <- allIdxF

	mCnt <- fCut.fltQuoTbl( gEnv ,allIdxF )
	flag <- mCnt < 3
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	tDiff <- Sys.time() - tStmp
	allIdxFObj$timeCost <- tDiff
	save( allIdxFObj ,file="Obj_allIdxFObj.save" )

	QQE

    return( rObj )

} # finalCut()









