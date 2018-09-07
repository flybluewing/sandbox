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

	cccMtx <- ccObj$cccMtx
	cccMtx <- cccMtx[,-which(colnames(cccMtx)=="reb")]
	ccc <- apply( cccMtx ,1 ,function(cccVal){ sum(cccVal>0) })
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


	# surFlag(scoreMtx) ------------------------------------------------------
	surFlag <- rep( TRUE ,length(allIdxF) )

	cName <- c( "ccc", "auxZW", "auxQuo", "raw", "rawFV", "rem", "cStep", "fStep" )
	thld <- c( 2, 2, 2, 2, 2, 3, 2, 2 )	;names(thld) <- cName

	fName <- attributes(ccObjLst)$names
	scoreMtx <- matrix( 0 ,nrow=length(ccObjLst) ,ncol=length(thld) )
	rownames(scoreMtx) <- fName		;colnames(scoreMtx) <- names(thld)
	cName <- c( "reb",  "nbor", "spanM", "quoAll", "quoPtn", "zw",  "remH0", "remH1", "cStep2", "cStep3" )
	cccMtx <- matrix( 0 ,nrow=length(ccObjLst) ,ncol=length(cName) )
	rownames(cccMtx) <- fName		;colnames(cccMtx) <- cName
	cccReb <- rep( 0 ,length(fName) )	;names(cccReb) <- fName
	evtSum <- rep( 0, length(fName) )	;names(evtSum) <- fName
	for( aIdx in 1:length(allIdxF) ){
		cccReb[] <- 0	;scoreMtx[,] <- 0	;cccMtx[,] <- 0		;evtSum[] <- 0
		for( nIdx in fName ){
			cccVal <- ccObjLst[[nIdx]]$cccMtx[aIdx,]
			cccReb[ nIdx ] <- cccVal["reb"]

			cccMtx[nIdx,] <- ccObjLst[[nIdx]]$cccMtx[aIdx,]

			scoreMtx[nIdx,"ccc"] <- sum( 0 < cccVal )
			scoreMtx[nIdx,c("auxZW", "auxQuo")] <- ccObjLst[[nIdx]]$auxCntMtx[aIdx,c("auxZW", "auxQuo")]
			scoreMtx[nIdx,c("raw", "rawFV", "rem", "cStep", "fStep")] <-
				ccObjLst[[nIdx]]$cntMtx[aIdx,c("raw", "rawFV", "rem", "cStep", "fStep")]

			
			if( thld["ccc"] <= sum(cccVal[names(cccVal)!="reb"]) ){
				evtSum[nIdx] <- evtSum[nIdx] + 1
			}
			cntVal <- ccObjLst[[nIdx]]$cntMtx[aIdx,c("raw", "rawFV", "rem", "cStep", "fStep")]
			evtSum[nIdx] <- evtSum[nIdx] + 
					sum( cntVal>=thld[c("raw", "rawFV", "rem", "cStep", "fStep")] )

		} # for(nIdx)

		#=[ccc]========================================================================================
		if( TRUE ){ # 코드접기용.
			scoreCut <- fCutU.cutScore( scoreMtx[,"ccc"]
										,pMinMaxSum=c( 7,12) ,pMinMaxHpn=c( 5,11) ,pMinMaxEvent=c(2,0,4) 
									)
			if( 0 < sum(scoreCut,na.rm=T) ) surFlag[aIdx] <- FALSE
			scoreCut <- fCutU.cutScore( cccMtx[,"reb"]
										,pMinMaxSum=c(NA,NA) ,pMinMaxHpn=c( 2, 8) ,pMinMaxEvent=c(2,NA,NA) 
									)
			if( 0 < sum(scoreCut,na.rm=T) ) surFlag[aIdx] <- FALSE
			scoreCut <- fCutU.cutScore( cccMtx[,"nbor"]
										,pMinMaxSum=c(NA,NA) ,pMinMaxHpn=c(NA, 3) ,pMinMaxEvent=c(2,NA,NA) 
									)
			scoreCut <- fCutU.cutScore( cccMtx[,"spanM"]
										,pMinMaxSum=c(NA,NA) ,pMinMaxHpn=c(NA, 4) ,pMinMaxEvent=c(2,NA,NA) 
									)
			scoreCut <- fCutU.cutScore( cccMtx[,"quoAll"]
										,pMinMaxSum=c(NA,NA) ,pMinMaxHpn=c(NA, 3) ,pMinMaxEvent=c(2,NA,NA) 
									)
			scoreCut <- fCutU.cutScore( cccMtx[,"quoPtn"]
										,pMinMaxSum=c(NA,NA) ,pMinMaxHpn=c(NA, 4) ,pMinMaxEvent=c(2,NA,NA) 
									)
			scoreCut <- fCutU.cutScore( cccMtx[,"zw"]
										,pMinMaxSum=c(NA,NA) ,pMinMaxHpn=c(NA, 3) ,pMinMaxEvent=c(2,NA,NA) 
									)
			scoreCut <- fCutU.cutScore( cccMtx[,"remH0"]
										,pMinMaxSum=c(NA,NA) ,pMinMaxHpn=c(NA, 2) ,pMinMaxEvent=c(2,NA,NA) 
									)
			scoreCut <- fCutU.cutScore( cccMtx[,"remH1"]
										,pMinMaxSum=c(NA,NA) ,pMinMaxHpn=c(NA, 2) ,pMinMaxEvent=c(2,NA,NA) 
									)
			scoreCut <- fCutU.cutScore( cccMtx[,"cStep2"]
										,pMinMaxSum=c(NA,NA) ,pMinMaxHpn=c(NA, 3) ,pMinMaxEvent=c(2,NA,NA) 
									)
			scoreCut <- fCutU.cutScore( cccMtx[,"cStep3"]
										,pMinMaxSum=c(NA,NA) ,pMinMaxHpn=c(NA, 3) ,pMinMaxEvent=c(2,NA,NA) 
									)

			if( 0 < sum(scoreCut,na.rm=T) ) surFlag[aIdx] <- FALSE

			# <reb> - late : reb
			chkFN <- c( "basic", "nextZW", "nextQuo10", "nextRebNum" )
			if( 3 <= sum(0<cccReb[chkFN]) ) surFlag[aIdx] <- FALSE
			# <reb> - gold : reb
			chkFN <- c( "nextRebNum", "nextFStepBin"
							, "nextColVal_1", "nextColVal_3" ,"nextColVal_4" 
							,"nextColVal_5" ,"nextColVal_6")
			if( 3 <= sum(0<cccReb[chkFN]) ) surFlag[aIdx] <- FALSE

			# <reb> cnt - 2이상 값 재발연속은 최대 1개 (late+gold)
			chkFN <- c( "nextZW", "nextColVal_3" )
			if( 2 <= sum(2<=scoreMtx[chkFN,"ccc"]) ) surFlag[aIdx] <- FALSE
			# <reb> cnt - 3이상 값 재발연속은 최대 1 개.
			if( 2 <= sum(3<=scoreMtx[,"ccc"]) ) surFlag[aIdx] <- FALSE

		}

		#=[raw]========================================================================================
		if( TRUE ){ # 코드 접기용.
			scoreCut <- fCutU.cutScore( scoreMtx[,"raw"]
										,pMinMaxSum=c(NA, 8) ,pMinMaxHpn=c(NA, 8) ,pMinMaxEvent=c(2,NA,2) 
									)
			if( 0 < sum(scoreCut,na.rm=T) ) surFlag[aIdx] <- FALSE

			# <reb> - late : Event 재발연속은 1개 이하.
			chkFN <- c( "nextZW", "nextColVal_2" )
			if( 2 <= sum(0<scoreMtx[chkFN,"raw"]) ) surFlag[aIdx] <- FALSE
			# <reb> - gold : Event 재발연속은 1개 이하.
			chkFN <- c( "nextCStepBin", "nextColVal_2", "nextColVal_5" )
			if( 2 <= sum(0<scoreMtx[chkFN,"raw"]) ) surFlag[aIdx] <- FALSE

		}

		#=[rawFV]======================================================================================
		if( TRUE ){ # 코드 접기용.
			scoreCut <- fCutU.cutScore( scoreMtx[,"rawFV"]
										,pMinMaxSum=c(NA, 3) ,pMinMaxHpn=c(NA, 2) ,pMinMaxEvent=c(2,NA,2) 
									)
			if( 0 < sum(scoreCut,na.rm=T) ) surFlag[aIdx] <- FALSE
		}

		#=[rem]========================================================================================
		if( TRUE ){ # 코드 접기용.
			scoreCut <- fCutU.cutScore( scoreMtx[,"rem"]
										,pMinMaxSum=c( 8,15) ,pMinMaxHpn=c(6,11) ,pMinMaxEvent=c(2,1,5) 
									)
			if( 0 < sum(scoreCut,na.rm=T) ) surFlag[aIdx] <- FALSE

			# <reb> - late : event 재발연속은 1개 이하
			chkFN <- c( "nextZW" ,"nextColVal_2" )
			if( 2 <= sum(1<scoreMtx[chkFN,"rem"]) ) surFlag[aIdx] <- FALSE
			# <reb> - gold : event 재발연속은 1개 이하
			chkFN <- c( "nextQuo10" ,"nextFStepBin" ,"nextColVal_5" )
			if( 2 <= sum(1<scoreMtx[chkFN,"rem"]) ) surFlag[aIdx] <- FALSE
		}

		#=[cStep]======================================================================================
		if( TRUE ){ # 코드 접기용.
			scoreCut <- fCutU.cutScore( scoreMtx[,"cStep"]
										,pMinMaxSum=c( 4,14) ,pMinMaxHpn=c(4,10) ,pMinMaxEvent=c(2,NA,5) 
									)
			if( 0 < sum(scoreCut,na.rm=T) ) surFlag[aIdx] <- FALSE

			# <reb> - late : event 재발연속은 1개 이하
			chkFN <- c( "nextBin" ,"nextFStepBin" ,"nextColVal_1" ,"nextColVal_2" )
			if( 2 <= sum(1<scoreMtx[chkFN,"cStep"]) ) surFlag[aIdx] <- FALSE

		}

		#=[fStep]======================================================================================
		if( TRUE ){ # 코드 접기용.
			scoreCut <- fCutU.cutScore( scoreMtx[,"fStep"]
										,pMinMaxSum=c( 0, 4) ,pMinMaxHpn=c( 0, 3) ,pMinMaxEvent=c(2,NA,2) 
									)
			if( 0 < sum(scoreCut,na.rm=T) ) surFlag[aIdx] <- FALSE

			# <reb> - (late+gold) : 재발연속은 1개 이하
			chkFN <- c( "nextBin" ,"nextColVal_4" )
			if( 2 <= sum(0<scoreMtx[chkFN,"fStep"]) ) surFlag[aIdx] <- FALSE
		}


		#=[evtSum : total score]========================================================================
		if( TRUE ){ # 코드 접기용.
			# - event 발생 총합 : evtSum 활용.
			# gold 영역에서는 총 합이 2 이하였다. late 에서는 2~6
			scoreCut <- fCutU.cutScore( evtSum
										,pMinMaxSum=c( 0, 3) ,pMinMaxHpn=c(NA,NA) ,pMinMaxEvent=c(2,NA,NA) 
									)
			if( 0 < sum(scoreCut,na.rm=T) ) surFlag[aIdx] <- FALSE

			# - event 연속 발생 (이전 history와의 비교.)
			# <reb> - late : reb
			chkFN <- c( "nextZW", "nextBin", "nextFStepBin", "nextColVal_1", "nextColVal_2" ,"nextColVal_4" )
			if( 2 <= sum(0<evtSum[chkFN]) ) surFlag[aIdx] <- FALSE
			# <reb> - gold : reb
			chkFN <- c( "nextColVal_3" )
			if( 1 <= sum(0<evtSum[chkFN]) ) surFlag[aIdx] <- FALSE

			# - event 연속 발생(scoreMtx 내 다음 row)
			ccc.reb.idx <- which( colnames(cccMtx)=="reb" )
			for( rIdx in 2:nrow(scoreMtx) ){
				score.pre	<- scoreMtx[(rIdx-1),]
				score.next	<- scoreMtx[rIdx	,]
				score.pre["ccc"]	<- sum( cccMtx[(rIdx-1)	, -ccc.reb.idx] )
				score.next["ccc"]	<- sum( cccMtx[rIdx		, -ccc.reb.idx] )
				seqSum <- sum( (score.pre>=thld) & (score.next>=thld) )
				if( 1 <= seqSum ){	# gold 기준.
					surFlag[aIdx] <- FALSE
					break
				}
			} # for

		}

	}  # aIdx

	table(surFlag)	;kIdx <- head(which(!surFlag))
	allIdxF <- allIdxF[ surFlag ]
	cat(sprintf("allIdxF %d\n",length(allIdxF)))
	# save( allIdxF ,file="Obj_allIdxF.save" )

	# colValSeqNext ------------------------------------------------------
	flgCnt <- fCutCnt.colValSeqNext( gEnv ,allIdxF ) 
	flgCnt <- flgCnt + fCutCnt.colValSeqNext.cStep( gEnv ,allIdxF )
	flag <- flgCnt<2	;table(flag)
    allIdxF <- allIdxF[flag]
	cat(sprintf("allIdxF %d\n",length(allIdxF)))

	


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

