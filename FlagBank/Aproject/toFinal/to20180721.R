# to20180714.R 최종접근
source("./toFinal/to20180721_H.R")

saveId <- "Z815"	;rpt=TRUE
load( sprintf("Obj_allIdxLst%s.save",saveId) )
load(sprintf("./save/Obj_gEnv%s.save",saveId))

allZoidGrpName <-"allZoid.idx0"	# 660671 ,1531865
allIdx <- allIdxLst[[allZoidGrpName]]

	# save( allIdxF ,file="Obj_allIdxF.save" )	# 임시성 저장.
	# load("Obj_allIdxFObj.save")	;allIdxF <- allIdxFObj$allIdxF.fCutCnt.m

	# QQE
	# flter count 0인 경우에 대한 추가

finalCut <- function( gEnv ,allIdx ,allZoidGrpName ){
    # cutEadge.getBanPtnColVal() 에서 1~2개 발생 탈락값들에 대한 검토 권장.
	allIdxFObj <- list()

    allIdxF <- allIdx
	stdMI <- fCutU.getMtxInfo( gEnv$zhF )	# matrix info
		# mtxLen lastZoid rem quo10 cStep fStep rawTail cStepTail

	# 참고 자료 --------------------------------------------------------------------	
	fCutU.rptColValSeqNext( gEnv ,allIdxF ,"to20180721")

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
	flgCnt <- fCutCnt.nextRebNum( gEnv ,allIdxF )
	flag <- flgCnt<2	;table(flag)
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	flgCnt <- fCutCnt.nextCStepBin( gEnv ,allIdxF )
	flag <- flgCnt<2	;table(flag)
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	for( cutCol.idx in c(1,3,6) ){
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
	for( zWidth in zWidth.span ){
		flgCnt <- fCutCnt.zWidthStd( gEnv ,allIdxF ,zWidth )
		flag <- flgCnt<2	;table(flag)
		allIdxF <- allIdxF[flag]
		cat(sprintf("zWidth %d  allIdxF %d\n",zWidth,length(allIdxF)))
	}

	allIdxFObj$allIdxF.fCutCnt <- allIdxF
	
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

	table(flgCnt)
	flag <- (0<flgCnt)&(flgCnt<3)	# 하나도 안 걸릴 수는 없겠지.
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	allIdxFObj$allIdxF.fCutCnt.m <- allIdxF

	tDiff <- Sys.time() - tStmp
	allIdxFObj$timeCost <- tDiff
	save( allIdxFObj ,file="Obj_allIdxFObj.save" )

	allIdxF.bak <- allIdxF


	================================================================================
	QQE ----------------------------------------------------------------------------

	# ------------------------------------------------------------------
	# fCutCnt.**
	#		각 파트에서 2 이상씩은 잘라낸 후,
	#		전체 파트에서 하나도 안 걸린 것들은 제외시키자.

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

	zWidth.span <- sort(unique(gEnv$allZoidMtx[allIdxF,6]-gEnv$allZoidMtx[allIdxF,1]))
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
	flgCnt <- flgCnt + fCutCnt.cust.getNextRebNum( gEnv ,allIdxF )

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
	zWidth.span <- sort(unique(gEnv$allZoidMtx[allIdxF,6]-gEnv$allZoidMtx[allIdxF,1]))
	zWidth.span <- setdiff( zWidth.span ,lastZoid[6]-lastZoid[1])
	for( zWidth in zWidth.span ){
		flgCnt <- flgCnt + fCutCnt.zWidthStd( gEnv ,allIdxF ,zWidth )
		cat(sprintf("zWidth:%d\n",zWidth))
	}

	aQuoTblLst <- fCutU.getQuoTblLst( gEnv$allZoidMtx[allIdxF,] )
	flag <- sapply( aQuoTblLst ,function( aQuoTbl ){	# "tbl"    "valStr" "quoStr" "idStr" 
					cnt <- 0
					quoLen <- ifelse( is.na(aQuoTbl$tbl["0"]) ,0 ,aQuoTbl$tbl["0"] )
					if( quoLen %in% c(0  ) ) cnt <- cnt+1	# 0,0,?
					quoLen <- ifelse( is.na(aQuoTbl$tbl["1"]) ,0 ,aQuoTbl$tbl["1"] )
					if( quoLen %in% c(1  ) ) cnt <- cnt+1	# 3,2,?
					quoLen <- ifelse( is.na(aQuoTbl$tbl["2"]) ,0 ,aQuoTbl$tbl["2"] )
					if( quoLen %in% c(2,0) ) cnt <- cnt+1	# 
					quoLen <- ifelse( is.na(aQuoTbl$tbl["3"]) ,0 ,aQuoTbl$tbl["3"] )
					if( quoLen %in% c(3,1) ) cnt <- cnt+1	# 
					quoLen <- ifelse( is.na(aQuoTbl$tbl["4"]) ,0 ,aQuoTbl$tbl["4"] )
					if( quoLen %in% c(2  ) ) cnt <- cnt+1	# 
	 				return( cnt<2 )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1


	table(flgCnt)
	flag <- (0<flgCnt)&(flgCnt<3)	# 하나도 안 걸릴 수는 없겠지.
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	allIdxFObj$allIdxF.fCutCnt.m <- allIdxF

	tDiff <- Sys.time() - tStmp
	allIdxFObj$timeCost <- tDiff
	save( allIdxFObj ,file="Obj_allIdxFObj.save" )

	allIdxF.bak <- allIdxF

	#        0 1 2 3 4
	# [809,] 1 3 1 0 1
	# [810,] 1 2 1 1 1
	# [811,] 1 2 1 1 1
	# [812,] 2 3 0 0 1
	# [813,] 0 1 0 3 2
	# [814,] 1 0 2 1 2
	      #  0 1 2 3 2
		  #      0 1 2
		  # 	 0 1
		  # 	 2
	aQuoTblLst <- fCutU.getQuoTblLst( gEnv$allZoidMtx[allIdxF,] )
	flag <- sapply( aQuoTblLst ,function( aQuoTbl ){	# "tbl"    "valStr" "quoStr" "idStr" 
					cnt <- 0
					quoLen <- ifelse( is.na(aQuoTbl$tbl["2"]) ,0 ,aQuoTbl$tbl["2"] )
					if( quoLen %in% c(2,0) ) cnt <- cnt+1	# 
					quoLen <- ifelse( is.na(aQuoTbl$tbl["3"]) ,0 ,aQuoTbl$tbl["3"] )
					if( quoLen %in% c(1) ) cnt <- cnt+1	# 
					quoLen <- ifelse( is.na(aQuoTbl$tbl["4"]) ,0 ,aQuoTbl$tbl["4"] )
					if( quoLen %in% c(2  ) ) cnt <- cnt+1	# 
	 				return( cnt==0 )
				})	;kIdx<-anaFlagFnd(!flag)
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	


	# 								lastZoid				cStep				quoTbl		/names(quoTbl)
	# fCut.customStatic				 2 21 28 38 42 45		19  7 10  4  3		1 2 1 2 	/0 2 3 4 
	# fCut.cust.nextZW				 3  8 19 27 30 41(*)	 5 11  8  3 11		2 1 1 1 1 	/0 1 2 3 4 
	# fCut.cust.NextQuo10			15 21 31 32 41 43		 6 10  1  9  2		1 1 2 2 	/1 2 3 4 
	# fCutCnt.cust.getNextBin		 2  5  6 13 28 44		 3  1  7 15 16		3 1 1 1 	/0 1 2 4 
	# fCutCnt.cust.getNextRebNum	 3  8 19 27 30 41(*)	 5 11  8  3 11		2 1 1 1 1 	/0 1 2 3 4 

	# fCutCnt.cust.colval1_0N		 3 12 13 18 31 32		 9  1  5 13  1		1 3 2 		/0 1 3 
	# 								 5 10 13 21 39 43		 5  3  8 18  4		1 2 1 1 1 	/0 1 2 3 4 
	# 								 7 27 29 30 38 44		20  2  1  8  6		1 2 2 1 	/0 2 3 4 
	# 								 9 30 34 35 39 41		21  4  1  4  2		1 4 1 		/0 3 4 

	# banVal 
	# 	[1]	 2 , 3		
	# 	[2]	21 , 8
	# 	[3]	19
	# 	[4]	27
	# 	[5]	30
	# 	[6]	41		(9)41 ,(7)44 ,(5)43

	# lastZoid 중첩.
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function( aZoid ){
				if(	aZoid[1] %in% c( 2, 3) )	return( FALSE )
				if(	aZoid[2] %in% c(21, 8) )	return( FALSE )
				if(	aZoid[3] %in% c(19   ) )	return( FALSE )
				if(	aZoid[4] %in% c(27   ) )	return( FALSE )
				if(	aZoid[5] %in% c(30   ) )	return( FALSE )
				if(	aZoid[6] %in% c(41   ) )	return( FALSE )

				if(	all(aZoid[c(1,6)]==c(9,41)) )	return( FALSE )
				if(	all(aZoid[c(1,6)]==c(7,44)) )	return( FALSE )
				if(	all(aZoid[c(1,6)]==c(5,43)) )	return( FALSE )
				return( TRUE )
			})	;kIdx<-anaFlagFnd(!flag)
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	aQuoTblLst <- fCutU.getQuoTblLst( gEnv$allZoidMtx[allIdxF,] )
	flag <- sapply( aQuoTblLst ,function( aQuoTbl ){	# "tbl"    "valStr" "quoStr" "idStr" 
					cnt <- 0
					quoLen <- ifelse( is.na(aQuoTbl$tbl["1"]) ,0 ,aQuoTbl$tbl["1"] )
					if( quoLen %in% c(1) ) cnt <- cnt+1	# 
	 				return( cnt==0 )
				})	;kIdx<-anaFlagFnd(!flag)
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	allIdxFObj$finalIdx <- allIdxF
	save( allIdxFObj ,file="Obj_allIdxFObj.save" )

	logAllZoidMtx( gEnv$allZoidMtx[allIdxFObj$finalIdx,] 
					,logId=sprintf("finalZoid_%d",length(allIdx)) 
				)

	#---------------------------------------------------------------------------------------
	# colValLst
	colValLst <- apply( gEnv$allZoidMtx[allIdxF,] ,2 ,function(p){sort(unique(p))})
	colVal <- gEnv$allZoidMtx[allIdxF,1]	;table(colVal)
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

