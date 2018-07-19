# to20180714.R 최종접근
source("./toFinal/to20180721_H.R")

saveId <- "Z815"	;rpt=TRUE
load( sprintf("Obj_allIdxLst%s.save",saveId) )
load(sprintf("./save/Obj_gEnv%s.save",saveId))

allZoidGrpName <-"allZoid.idx0"	# 660671 ,1531865
allIdx <- allIdxLst[[allZoidGrpName]]

	# save( allIdxF ,file="Obj_allIdxF.save" )	# 임시성 저장.
	# load(sprintf("Obj_allIdxFObj_%s.save",allZoidGrpName))	;allIdxF <- allIdxFObj$allIdxF.fCutCnt.m

	# QQE
	# flter count 0인 경우에 대한 추가

finalCut <- function( gEnv ,allIdx ,allZoidGrpName ){
    # cutEadge.getBanPtnColVal() 에서 1~2개 발생 탈락값들에 대한 검토 권장.
	allIdxFObj <- list()
	# 참고 자료 --------------------------------------------------------------------	
	fCutU.rptColValSeqNext( gEnv ,allIdxF ,"to20180721")
	# aQuoTblLst <- fCutU.getQuoTblLst( gEnv$allZoidMtx[allIdxF,] )
	# aQuoTblStr <- sapply( aQuoTblLst ,function(quoTbl){quoTbl$valStr})	;table(aQuoTblStr)

    allIdxF <- allIdx
	stdMI <- fCutU.getMtxInfo( gEnv$zhF )	# matrix info
		# mtxLen lastZoid rem quo10 cStep fStep rawTail cStepTail

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

	aQuoTblObjLst <- fCutU.getQuoTblLst( gEnv$allZoidMtx[allIdxF,,drop=F] )	# tbl valStr quoStr idStr
	aQuoTblStr <- sapply( aQuoTblObjLst ,function(quoTbl){quoTbl$valStr})
	aQuoTblSpan <- sort(unique(aQuoTblStr))	#	table(aQuoTblStr)
	aQuoTblSpan <- aQuoTblSpan[aQuoTblSpan!=stdMI$quo10$valStr]
	for( tblStr in aQuoTblSpan ){
		flgCnt <- fCutCnt.quoTblStd( gEnv ,allIdxF ,tblStr )
		flag <- flgCnt<2	;table(flag)
		allIdxF <- allIdxF[flag]
		cat(sprintf("tblStr %s  allIdxF %d\n",tblStr,length(allIdxF)))
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

	aQuoTblObjLst <- fCutU.getQuoTblLst( gEnv$allZoidMtx[allIdxF,,drop=F] )	# tbl valStr quoStr idStr
	aQuoTblStr <- sapply( aQuoTblObjLst ,function(quoTbl){quoTbl$valStr})
	aQuoTblSpan <- sort(unique(aQuoTblStr))	#	table(aQuoTblStr)
	aQuoTblSpan <- aQuoTblSpan[aQuoTblSpan!=stdMI$quo10$valStr]
	for( tblStr in aQuoTblSpan ){
		flgCnt <- flgCnt + fCutCnt.quoTblStd( gEnv ,allIdxF ,tblStr )
		cat(sprintf("tblStr:%s\n",tblStr))
	}

	flgCnt <- flgCnt + fCutCnt.colVal_1_x( gEnv ,allIdxF )

	table(flgCnt)
	flag <- (0<flgCnt)&(flgCnt<3)	# 하나도 안 걸릴 수는 없겠지.
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	allIdxFObj$allIdxF.fCutCnt.m <- allIdxF

	#=<Final Approach>=======================================================
	# 	<Total Ban Value>
	# 		[1] 3(1/0)~5(/5)~3(/0)
	# 		[2] 21(1/0),30(1/0),14(*/0)~17(/1),20(/1)~2(/4),15(/0)~15(/0)~16(/65)
	# 		[3] 32(/6)~26(/29),34(/0)~16(/73)~
	# 		[4] 20(/4),28(*/115),34(/29),35(/18)~39(/12),11(/0),38(/12)~12(/4),41(/0)
	# 		[5] 37(*/25)~41(/0)~35(/0),43(/0/)
	# 		[6] 37(*/5),22(/0)~45(/0),34(/52),33(/37)~43(/0),38(/21)
	# 		ptn : (37,39)~(42,43)(37,40)~(16,19)~(37,38),(32,37)~(37,41),(32,41 <3,4>)

	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c( 3, 5 ) )	cnt <- cnt+1						#  3(2)
					if( aZoid[2]%in%c(21,30,14,17,20, 2,15,16) )	cnt <- cnt+1	# 15(2)
					if( aZoid[3]%in%c(32,26,34,16) )	cnt <- cnt+1
					if( aZoid[4]%in%c(20,28,34,35,39,11,38,12,41) )	cnt <- cnt+1
					if( aZoid[5]%in%c(37,41,35,43) )	cnt <- cnt+1
					if( aZoid[6]%in%c(37,22,45,34,33,43,38) )	cnt <- cnt+1

					if( fCutU.hasPtn( c(37,39) ,aZoid ) )	cnt <- cnt+1
					if( fCutU.hasPtn( c(42,43) ,aZoid ) )	cnt <- cnt+1
					if( fCutU.hasPtn( c(37,40) ,aZoid ) )	cnt <- cnt+1
					if( fCutU.hasPtn( c(16,19) ,aZoid ) )	cnt <- cnt+1
					if( fCutU.hasPtn( c(37,38) ,aZoid ) )	cnt <- cnt+1
					if( fCutU.hasPtn( c(32,37) ,aZoid ) )	cnt <- cnt+1
					if( fCutU.hasPtn( c(37,41) ,aZoid ) )	cnt <- cnt+1
					if( all(aZoid[c(3,4)]==c(32,41)) )	cnt <- cnt+1

					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    allIdxF <- allIdxF[flag]


	tDiff <- Sys.time() - tStmp
	allIdxFObj$timeCost <- tDiff
	save( allIdxFObj ,file=sprintf("Obj_allIdxFObj_%s.save",allZoidGrpName) )

	allIdxF.bak <- allIdxF



	logAllZoidMtx( gEnv$allZoidMtx[allIdxFObj$finalIdx,] 
					,logId=sprintf("finalZoid_%d",length(allIdx)) 
				)

	#---------------------------------------------------------------------------------------
	# colValLst
	colValLst <- apply( gEnv$allZoidMtx[allIdxF,] ,2 ,function(p){sort(unique(p))})
	colVal <- gEnv$allZoidMtx[allIdxF,1]	;table(colVal)
	colValTblLst.raw <- apply( gEnv$allZoidMtx[allIdxF,] ,2 ,function(p){table(p)})
	colValTblLst.rem <- apply( gEnv$allZoidMtx[allIdxF,] ,2 ,function(p){table(p%%10)})
	cStepMtx <- t(apply(gEnv$allZoidMtx[allIdxF,] ,1 ,function(zoid){zoid[2:6]-zoid[1:5]}))
	colValTblLst.cStep <- apply( cStepMtx ,2 ,function(p){table(p)})

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

