# Z823
source("./toFinal/toZ823_H.R")

saveId <- "Z822"	;rpt=TRUE
load( sprintf("Obj_allIdxLst%s.save",saveId) )
load(sprintf("./save/Obj_gEnv%s.save",saveId))
allZoidGrpName <-"allZoid.idx0"	# 
allIdx <- allIdxLst[[allZoidGrpName]]	#   817018   1514562
stdZoid <- NULL

# simMode start ----------------------------------------------------
	aZoid <- stdZoid <- c( ,,,,, ) # ZH 823 채워넣을 것.
	allIdxF <- allIdx <- stdIdx <- which(apply(gEnv$allZoidMtx,1,function(zoid){all(zoid==stdZoid)}))
# simMode end   ----------------------------------------------------

cutCC <- function( ccObj ,allIdxF ){
	#  cntMtx	auxCntMtx	cccMtx

	surFlag <- rep( TRUE ,length(allIdxF) )

	flag <- apply( ccObj$auxCntMtx, 1, function( cntVal ){
					# auxZW auxQuo
					if( all(cntVal>0) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,F)
	surFlag[ !flag ] <- FALSE

	# flag <- apply( ccObj$cccMtx, 1, function( cntVal ){
	# 				# reb nbor spanM quoAll quoPtn zw remH0 remH1 cStep2 cStep3
	# 			})	;kIdx<-anaFlagFnd(!flag,F)
	# surFlag[ !flag ] <- FALSE

	ccc <- apply( ccObj$cccMtx ,1 ,function(cccVal){ sum(cccVal>0) })
	cntMtx <- cbind( ccc, ccObj$cntMtx )
	cName <- c( "ccc", "raw", "rawFV", "rem", "cStep", "fStep" )
	thld <- c( 2, 2, 2, 3, 2, 2 )	;names(thld) <- cName
	flag <- apply( cntMtx, 1, function( cntVal ){
					# ccc   raw rawFV rem cStep fStep
					if( 1 < sum(cntVal>=thld) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,F)
	surFlag[ !flag ] <- FALSE

	return( surFlag )

} # cutCC()

finalCut <- function( gEnv ,allIdx ,allZoidGrpName ){
    # cutEadge.getBanPtnColVal() 에서 1~2개 발생 탈락값들에 대한 검토 권장.
	allIdxFObj <- list()
	# 참고 자료 --------------------------------------------------------------------	
	fCutU.rptColValSeqNext( gEnv ,allIdxF ,sprintf("toZ%d",nrow(gEnv$zhF)+1) )
	# aQuoTblLst <- fCutU.getQuoTblLst( gEnv$allZoidMtx[allIdxF,] )
	# aQuoTblStr <- sapply( aQuoTblLst ,function(quoTbl){quoTbl$valStr})	;table(aQuoTblStr)

    allIdxF <- allIdx
	stdMI <- fCutU.getMtxInfo( gEnv$zhF )	#	rptObj<-anaMtx( stdMI$rawTail )	# u0.zoidMtx_ana( stdMI$rawTail )


	tStmp <- Sys.time()
	# 기본제거 --------------------------------------------------------------------	
	allIdxF <- fCut.default( gEnv ,allIdxF )
	allIdxF <- fCut.basic( gEnv ,allIdxF )
	cat(sprintf("allIdxF %d\n",length(allIdxF)))
	allIdxFObj$allIdxF.fCut <- allIdxF
	tDiff <- Sys.time() - tStmp	

	# ------------------------------------------------------------------
	# fCutCnt.**
	#		각 파트에서 2 이상씩은 잘라낸 후,
	#		전체 파트에서 하나도 안 걸린 것들은 제외시키자.
	# ccObj <- fCutCnt.default( gEnv ,allIdxF )
	ccObj <- fCutCnt.basic( gEnv ,allIdxF )
	allIdxF <- allIdxF[ cutCC( ccObj ,allIdxF ) ]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	ccObj <- fCutCnt.nextZW( gEnv ,allIdxF )
	allIdxF <- allIdxF[ cutCC( ccObj ,allIdxF ) ]
	cat(sprintf("allIdxF %d\n",length(allIdxF)))
	ccObj <- fCutCnt.nextQuo10( gEnv ,allIdxF )
	allIdxF <- allIdxF[ cutCC( ccObj ,allIdxF ) ]
	cat(sprintf("allIdxF %d\n",length(allIdxF)))
	ccObj <- fCutCnt.nextBin( gEnv ,allIdxF )
	allIdxF <- allIdxF[ cutCC( ccObj ,allIdxF ) ]
	cat(sprintf("allIdxF %d\n",length(allIdxF)))
	ccObj <- fCutCnt.nextRebNum( gEnv ,allIdxF )
	allIdxF <- allIdxF[ cutCC( ccObj ,allIdxF ) ]
	cat(sprintf("allIdxF %d\n",length(allIdxF)))
	ccObj <- fCutCnt.nextCStepBin( gEnv ,allIdxF )
	allIdxF <- allIdxF[ cutCC( ccObj ,allIdxF ) ]
	cat(sprintf("allIdxF %d\n",length(allIdxF)))
	ccObj <- fCutCnt.nextFStepBin( gEnv ,allIdxF )
	allIdxF <- allIdxF[ cutCC( ccObj ,allIdxF ) ]
	cat(sprintf("allIdxF %d\n",length(allIdxF)))
	ccObj <- fCutCnt.nextColVal_1( gEnv ,allIdxF )
	allIdxF <- allIdxF[ cutCC( ccObj ,allIdxF ) ]
	cat(sprintf("allIdxF %d\n",length(allIdxF)))
	ccObj <- fCutCnt.nextColVal_2( gEnv ,allIdxF )
	allIdxF <- allIdxF[ cutCC( ccObj ,allIdxF ) ]
	cat(sprintf("allIdxF %d\n",length(allIdxF)))
	ccObj <- fCutCnt.nextColVal_3( gEnv ,allIdxF )
	allIdxF <- allIdxF[ cutCC( ccObj ,allIdxF ) ]
	cat(sprintf("allIdxF %d\n",length(allIdxF)))
	ccObj <- fCutCnt.nextColVal_4( gEnv ,allIdxF )
	allIdxF <- allIdxF[ cutCC( ccObj ,allIdxF ) ]
	cat(sprintf("allIdxF %d\n",length(allIdxF)))
	ccObj <- fCutCnt.nextColVal_5( gEnv ,allIdxF )
	allIdxF <- allIdxF[ cutCC( ccObj ,allIdxF ) ]
	cat(sprintf("allIdxF %d\n",length(allIdxF)))
	ccObj <- fCutCnt.nextColVal_6( gEnv ,allIdxF )
	allIdxF <- allIdxF[ cutCC( ccObj ,allIdxF ) ]
	cat(sprintf("allIdxF %d\n",length(allIdxF)))
	save( allIdxF ,file="Obj_allIdxF.save" )

	tDiff <- Sys.time() - tStmp	
	allIdxFObj$allIdxF.fCutCnt <- allIdxF

	ccObjLst <- list()
	ccObjLst[["basic"		]] <- fCutCnt.basic( 		gEnv ,allIdxF )
	ccObjLst[["nextZW"		]] <- fCutCnt.nextZW( 		gEnv ,allIdxF )
	ccObjLst[["nextQuo10"	]] <- fCutCnt.nextQuo10( 	gEnv ,allIdxF )
	ccObjLst[["nextBin"		]] <- fCutCnt.nextBin( 		gEnv ,allIdxF )
	ccObjLst[["nextRebNum"	]] <- fCutCnt.nextRebNum( 	gEnv ,allIdxF )
	ccObjLst[["nextCStepBin"]] <- fCutCnt.nextCStepBin( gEnv ,allIdxF )
	ccObjLst[["nextFStepBin"]] <- fCutCnt.nextFStepBin( gEnv ,allIdxF )
	ccObjLst[["nextColVal_1"]] <- fCutCnt.nextColVal_1( gEnv ,allIdxF )
	ccObjLst[["nextColVal_2"]] <- fCutCnt.nextColVal_2( gEnv ,allIdxF )
	ccObjLst[["nextColVal_3"]] <- fCutCnt.nextColVal_3( gEnv ,allIdxF )
	ccObjLst[["nextColVal_4"]] <- fCutCnt.nextColVal_4( gEnv ,allIdxF )
	ccObjLst[["nextColVal_5"]] <- fCutCnt.nextColVal_5( gEnv ,allIdxF )
	ccObjLst[["nextColVal_6"]] <- fCutCnt.nextColVal_6( gEnv ,allIdxF )

	fName <- attributes(ccObjLst)$names

	tStmp2 <- Sys.time()
	# surFlag
	surFlag <- rep( TRUE ,length(allIdxF) )
	cName <- c( "ccc", "auxZW", "auxQuo", "raw", "rawFV", "rem", "cStep", "fStep" )
	scoreMtx <- matrix( 0 ,nrow=length(ccObjLst) ,ncol=length(cName) )
	rownames(scoreMtx) <- fName		;colnames(scoreMtx) <- cName
	cccReb <- rep( 0 ,length(fName) )	;names(cccReb) <- fName
	for( aIdx in 1:length(allIdxF) ){
		cccReb[] <- 0	;scoreMtx[,] <- 0
		for( nIdx in fName ){
			cccReb[ nIdx ] <- ccObjLst[[nIdx]]$cccMtx[aIdx,"reb"]

			scoreMtx[nIdx,"ccc"] <- sum( 0<ccObjLst[[nIdx]]$cccMtx[aIdx,] )
			scoreMtx[nIdx,c("auxZW", "auxQuo")] <- ccObjLst[[nIdx]]$auxCntMtx[aIdx,c("auxZW", "auxQuo")]
			scoreMtx[nIdx,c("raw", "rawFV", "rem", "cStep", "fStep")] <-
				ccObjLst[[nIdx]]$cntMtx[aIdx,c("raw", "rawFV", "rem", "cStep", "fStep")]
		}

		#=[ccc]========================================================================================
		# # <reb> - reb (late)
		# chkFN <- c( "basic", "nextZW", "nextQuo10", "nextRebNum" )
		# if( 2 <= sum(0<cccReb[chkFN]) ) surFlag[aIdx] <- FALSE
		# # <reb> - reb (gold)
		# chkFN <- c( "nextRebNum", "nextFStepBin"
		# 				, "nextColVal_1", "nextColVal_3" ,"nextColVal_4" ,"nextColVal_5" ,"nextColVal_6")
		# if( 2 <= sum(0<cccReb[chkFN]) ) surFlag[aIdx] <- FALSE

		# # <reb> cnt - 2이상 값 발생위치 재발이 2개 이상. (late+gold)
		# chkFN <- c( "nextZW", "nextColVal_3" )
		# if( 2 <= sum(2<=scoreMtx[chkFN,"ccc"]) ) surFlag[aIdx] <- FALSE
		# # <reb> cnt - 3이상은 최대 1 개.
		# if( 2 <= sum(3<=scoreMtx[,"ccc"]) ) surFlag[aIdx] <- FALSE

		# <min>
		# if( 7 > sum(scoreMtx[,"ccc"]) ) surFlag[aIdx] <- FALSE

		#=[raw]========================================================================================
		# # <reb> - late : 1이상 값의 재발은 1개 이하.
		# chkFN <- c( "nextZW", "nextColVal_2" )
		# if( 2 <= sum(0<scoreMtx[chkFN,"raw"]) ) surFlag[aIdx] <- FALSE
		# # <reb> - gold : 1이상 값의 재발은 1개 이하.
		# chkFN <- c( "nextCStepBin", "nextColVal_2", "nextColVal_5" )
		# if( 2 <= sum(0<scoreMtx[chkFN,"raw"]) ) surFlag[aIdx] <- FALSE
		# <min-event>
		if( 2 <= sum(2<=scoreMtx[,"raw"]) ) surFlag[aIdx] <- FALSE
		# <max-happen>
		if( 8 >= sum(0<=scoreMtx[,"raw"]) ) surFlag[aIdx] <- FALSE

		#=[rawFV]======================================================================================
		if( 2 <= sum(1<=scoreMtx[,"rawFV"]) ) surFlag[aIdx] <- FALSE

		#=[rem]========================================================================================
		#=[cStep]======================================================================================
		#=[fStep]======================================================================================


	} # aIdx
	tDiff2 <- Sys.time() - tStmp2

	# table(surFlag)	;kIdx <- head(which(!surFlag))

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

