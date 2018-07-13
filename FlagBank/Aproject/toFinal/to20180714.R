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

	tDiff <- Sys.time() - tStmp
	allIdxFObj$timeCost <- tDiff
	save( allIdxFObj ,file="Obj_allIdxFObj.save" )

	QQE

    return( rObj )

} # finalCut()


fCut.fltQuoTbl <- function( gEnv ,allIdxF ){

	getQVal <- function( zoid ){
		quo <- zoid%/%10
		quoVal <- sort(unique(quo))
		valLst <- list()
		for( qIdx in quoVal ){
			valLst[[as.character(qIdx)]] <- zoid[quo==qIdx]
		}
		return( valLst )
	}

	zQuoTblLst <- fCutU.getQuoTblLst( gEnv$zhF )
	zQuoStr <- sapply( zQuoTblLst ,function(zQuo){zQuo$idStr})

	aQuoTblLst <- fCutU.getQuoTblLst( gEnv$allZoidMtx[allIdxF,] )
	aQuoStr <- sapply( aQuoTblLst ,function(aQuo){aQuo$idStr})
	aQuoStr <- sort(unique(aQuoStr))

	# 과거 동향 확인.
	mtxLst <- list()
	for( idx in 1:length(aQuoStr) ){
		indices <- which(zQuoStr==aQuoStr[idx])
		if( 0==length(indices) ){
			mtxLst[[1+length(mtxLst)]] <- matrix(0,nrow=0,ncol=6)
		} else {
			mtxLst[[1+length(mtxLst)]] <- gEnv$zhF[indices,,drop=F]
		}
	}

	chkNRow <- sapply( mtxLst ,nrow )

	# 과거 동향으로부터 ban 정보 정리.
	banLst <- list()
	for( idx in 1:length(aQuoStr) ){
		rowLen <- nrow(mtxLst[[idx]])
		mtx <- mtxLst[[idx]]
		if( 0==rowLen ) next

		lastZoid=mtx[rowLen,]
		obj <- list( idStr=aQuoStr[idx] ,lastZoid=lastZoid )
		obj$tbl <- table(lastZoid%/%10)
		obj$qVal <- getQVal( lastZoid )

		banLst[[ aQuoStr[idx] ]] <- obj
	}

	# level 1 : 거의 불가.
	# 크기 3개 이상 블럭의 값이 일치하거나.
	# 값이 일치하는 블럭의 길이 총 합이 3이상이거나.
	# hIdx-1, hIdx-3 에서 2개 이상 일치.

	# level 2 : 이따금 발생.
	# 크기 3개 이상 블럭 일치값 2개.
	# 일치하는 블럭의 길이 총합이 3 이상.(길이 1 블럭 2개 일치나, 길이 2 블럭 하나 일치.)

	assRst <- rep( 0 ,length(aQuoTblLst) )
	for( aIdx in 1:length(aQuoTblLst) ){
		banObj <- banLst[[ aQuoTblLst[[aIdx]]$idStr ]]
		aZoid <- gEnv$allZoidMtx[allIdxF,][aIdx,]
		aQVal <- getQVal( aZoid )

		matchFlg <- sapply( 1:length(aQVal) ,function(qIdx){ all(aQVal[[qIdx]]==banObj$qVal[[qIdx]]) })
		if( 0==sum(matchFlg) ) next

		assRst[aIdx] <- sum(matchFlg)
	}

	assRst1.idx <- which( assRst==1 )
	assRst2.idx <- which( assRst==2 )

} # fCut.fltQuoTbl()








