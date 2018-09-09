# Z823
source("./toFinal/toZ823_H.R")

saveId <- "Z822"	;rpt=TRUE
load( sprintf("Obj_allIdxLst%s.save",saveId) )
load(sprintf("./save/Obj_gEnv%s.save",saveId))
allZoidGrpName <-"allZoid.idx0"	# 
allIdx <- allIdxLst[[allZoidGrpName]]	#   817018   1514562
stdZoid <- NULL

# simMode start ----------------------------------------------------
	aZoid <- stdZoid <- c( 12,18,24,26,39,40 ) # ZH 823 채워넣을 것.
	allIdxF <- allIdx <- stdIdx <- 6949788
		# which(apply(gEnv$allZoidMtx,1,function(zoid){all(zoid==stdZoid)}))
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

			ccc.reb.idx <- which( colnames(cccMtx)=="reb" )
			# - event 연속 발생(scoreMtx 내 다음 row)
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

			# - event 연속 발생(cccMtx 내 다음 row)
			for( rIdx in 2:nrow(cccMtx) ){
				ccc.pre		<- cccMtx[(rIdx-1),-ccc.reb.idx]
				ccc.next	<- cccMtx[rIdx	  ,-ccc.reb.idx]
				seqSum <- sum( (ccc.pre>=1) & (ccc.next>=1) )
				if( 2 <= seqSum ){	# gold 기준.
					surFlag[aIdx] <- FALSE
					break
				}
			} # for
		}

	}  # aIdx

	table(surFlag)	;kIdx <- head(which(!surFlag))
	allIdxF <- allIdxF[ surFlag ]
	allIdxFObj$allIdxF.surFlag <- allIdxF
	cat(sprintf("allIdxF %d\n",length(allIdxF)))
	# save( allIdxF ,file="Obj_allIdxF.save" )

	# colValSeqNext ------------------------------------------------------
	flgCnt <- fCutCnt.colValSeqNext( gEnv ,allIdxF ) 
	flgCnt <- flgCnt + fCutCnt.colValSeqNext.cStep( gEnv ,allIdxF )
	flgCnt <- flgCnt + fCutCnt.default( gEnv ,allIdxF )	# 효율이 의문시된다.
	flag <- flgCnt<2	;table(flag)
    allIdxF <- allIdxF[flag]
	allIdxF.bak <- allIdxF
	allIdxFObj$allIdxF.colValSeqNext <- allIdxF
	cat(sprintf("allIdxF %d\n",length(allIdxF)))

	tDiff <- Sys.time() - tStmp
	allIdxFObj$timeCost <- tDiff
	allIdxFObj$allIdxF.final <- allIdxF

	flgCnt <- cutRawFV3( gEnv ,allIdxF )	# 개쓸모 없...

	save( allIdxFObj ,file=sprintf("Obj_allIdxFObj_%s.save",allZoidGrpName) )


	fCutU.logAllZoidMtx( gEnv$allZoidMtx[ allIdxFObj$allIdxF.final ,,drop=F] 
					,logId=sprintf("final_nextOf%s_%s",saveId,allZoidGrpName) 
				)


    return( rObj )

} # finalCut()

cutRawFV3 <- function( gEnv ,allIdxF ,rpt=FALSE ){
	#	3개 이상에 대한 패턴은 걸리는 게 없는 것 같아 별도 필터로 모은다.
	#	"rawFV" 에 대한 복사가 아닌, 전면 재작성할 것. (이전 부분은 2개 이상은 의미가 없어 잘려나가는 값들이 많다.)

	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					# < 9>
					if( fCutU.hasPtn(c( 9,24,27,23),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# <12>
					# <13>
					if( fCutU.hasPtn(c(12,13,23),aZoid) ) cnt<-cnt+1
					# <24>
					if( fCutU.hasPtn(c(17,NA,NA,24,25,28),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
					# <25>
					# <29>
					if( fCutU.hasPtn(c( 9, 1,20,29),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
					# <30>
					if( fCutU.hasPtn(c(14,15,30),aZoid) ) cnt<-cnt+1

					# <12>
					if( fCutU.hasPtn(c( 12,NA,26,44),aZoid) ) cnt<-cnt+1
					# <15>
					if( fCutU.hasPtn(c(15,NA,33,43),aZoid) ) cnt<-cnt+1
					# <18>
					if( fCutU.hasPtn(c(10, 9,18,20),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
					# <24>
					if( fCutU.hasPtn(c(20,12,17,24,45),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
					# <25>
					if( fCutU.hasPtn(c( 2, 3,25),aZoid) ) cnt<-cnt+1
					# <39>
					if( fCutU.hasPtn(c( 1,10,39),aZoid) ) cnt<-cnt+1
					# <42>
					if( fCutU.hasPtn(c( 8, 7, 6, 8,14,42),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1
					# <44>

					# < 2>
					if( fCutU.hasPtn(c( 2,15,13,26,45),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# < 6>
					if( fCutU.hasPtn(c( 6, 8,NA,10,27,37),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# <14>					# <15>
					# <16>
					# <17>
					# <19>
					# <38>
					# <41>
					if( fCutU.hasPtn(c(18,22,11,30,41),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
					# <45>
					if( fCutU.hasPtn(c(22,14,NA,NA,35,45),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1

					#	fCutCnt.nextBin
					# <15>
					# <17>
					if( fCutU.hasPtn(c( 9,17,31,27,39),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
					# <25>
					# <28>
					if( fCutU.hasPtn(c(25,25,28,39),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
					# <32>
					# <39>
					if( fCutU.hasPtn(c( 9,17,31,27,39),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
					# <40>

					# fCutCnt.nextRebNum
					# < 3>
					if( fCutU.hasPtn(c( 3,20,10,16,34,20),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# < 5>
					# <13>
					if( fCutU.hasPtn(c( 7, 8,13,24),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
					# <16>
					if( fCutU.hasPtn(c( 3,16,19),aZoid) ) cnt<-cnt+1
					# <21>					# <27>					# <30>
					# <44>
					if( fCutU.hasPtn(c(11,NA,NA,40,NA,44),aZoid) ) cnt<-cnt+1

					#	fCutCnt.nextCStepBin
					cnt <- 0
					# < 3>
					if( fCutU.hasPtn(c( 3,11,13),aZoid) ) cnt<-cnt+1
					# < 6>
					if( fCutU.hasPtn(c( 6,NA,14,15,35,44),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# <15>
					# <16>
					# <32>
					if( fCutU.hasPtn(c(11,29,32),aZoid) ) cnt<-cnt+1

					#	fCutCnt.nextFStepBin
					# < 5>
					# <12>
					if( fCutU.hasPtn(c( 3,12,20,31,32),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
					# <13>
					# <20>
					if( fCutU.hasPtn(c( 3,12,20,31,32),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
					# <26>
					if( fCutU.hasPtn(c(19,26),aZoid) ) cnt<-cnt+1
					# <27>
					if( fCutU.hasPtn(c(25,NA,27,40,NA,42),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
					# <28>
					if( fCutU.hasPtn(c(15, 9, 3,28,45),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
					# <29>
					if( fCutU.hasPtn(c(11, 7,13,24,29,36),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
					# <38>
					if( fCutU.hasPtn(c(14,NA,28,NA,38),aZoid) ) cnt<-cnt+1
					# <43>
					if( fCutU.hasPtn(c(11, 8,10,20,32,43),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1

					# fCutCnt.nextColVal_1
					# < 8>
					# <10>
					if( fCutU.hasPtn(c(10,33,42,42),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# <11>
					if( fCutU.hasPtn(c( 6,11,18,16),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
					# <13>
					if( fCutU.hasPtn(c( 6,13,35,NA,45),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
					# <15>
					# <20>
					if( fCutU.hasPtn(c(18,20,35),aZoid) ) cnt<-cnt+1
					# <27>
					if( fCutU.hasPtn(c( 8,24,27),aZoid) ) cnt<-cnt+1
					# <31>
					if( fCutU.hasPtn(c( 8,13,31,NA,41),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
					# <42>
					if( fCutU.hasPtn(c(20,18,35,37,42),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
					# <45>

					# fCutCnt.nextColVal_2
					# < 3>
					if( fCutU.hasPtn(c( 3,14,15,NA,25),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# < 4>
					if( fCutU.hasPtn(c( 4, 9,25,17,27),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# < 7>
					if( fCutU.hasPtn(c( 7,23,26,NA,45),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# < 9>
					if( fCutU.hasPtn(c( 2, 9,NA,17),aZoid) ) cnt<-cnt+1
					# <13>
					if( fCutU.hasPtn(c( 9,NA,13,19),aZoid) ) cnt<-cnt+1
					# <16>
					# <25>
					if( fCutU.hasPtn(c( 3,14,15,NA,25),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
					# <33>
					# <38>
					if( fCutU.hasPtn(c(21,14,14,NA,38),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1

					# fCutCnt.nextColVal_3
					cnt <- 0
					# < 4>
					# <11>
					if( fCutU.hasPtn(c(11,12,NA,23,38),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# <17>
					# <18>
					if( fCutU.hasPtn(c(18,26,39,28,35),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# <19>
					if( fCutU.hasPtn(c(19,28,31),aZoid) ) cnt<-cnt+1
					# <26>
					if( fCutU.hasPtn(c(18,26,39,28,35),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# <27>
					# <38>
					if( fCutU.hasPtn(c(   25,26,38),aZoid) ) cnt<-cnt+1
					# <41>
					if( fCutU.hasPtn(c(16,12,21,34,41),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1

					# fCutCnt.nextColVal_4
					# <15>
					if( fCutU.hasPtn(c(15,33,33,45),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# <17>
					# <21>
					# <24>
					if( fCutU.hasPtn(c(16,NA,23,24,25,41),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
					# <26>
					if( fCutU.hasPtn(c( 7, 9,18,NA,26),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
					# <27>
					if( fCutU.hasPtn(c( 19,21,NA,27),aZoid) ) cnt<-cnt+1
					# <36>
					if( fCutU.hasPtn(c( 5,22,14,20,24,36),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1

					# fCutCnt.nextColVal_5
					# <11>
					# <18>
					if( fCutU.hasPtn(c( 8,18,19,34,23),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
					# <19>
					if( fCutU.hasPtn(c(14,18,19,23,23),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
					# <24>					# <33>
					# <35>
					if( fCutU.hasPtn(c( 5,NA,38),aZoid) ) cnt<-cnt+1
					# <39>
					if( fCutU.hasPtn(c(22,26,27,NA,27,39),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1

					# fCutCnt.nextColVal_6
					# < 3>
					if( fCutU.hasPtn(c( 3,16,14,NA,19),aZoid) ) cnt<-cnt+1
					# <10>
					if( fCutU.hasPtn(c(10,16),aZoid) ) cnt<-cnt+1
					# <12>
					if( fCutU.hasPtn(c(12,23,20,27,30),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# <18>
					# <31>
					if( fCutU.hasPtn(c(21,24,25,NA,31),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
					# <42>
					if( fCutU.hasPtn(c(23,31,38,NA,42),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
					# <43>
					if( fCutU.hasPtn(c(19,27,34,NA,34,43),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)

	return( cntMtx[,"rawFV"]>0 )
}

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

