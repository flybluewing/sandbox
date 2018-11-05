# Z832
workH <- 832	;rpt=TRUE
source(sprintf("./toFinal/toZ%d_H.R",workH))

load( sprintf("Obj_allIdxLstZ%d.save",workH-1) )
load(sprintf("./save/Obj_gEnvZ%d.save",workH-1))
allZoidGrpName <-"allZoid.idx0"	# 
allIdx <- allIdxLst[[allZoidGrpName]]	#   
allIdxF <- 1000:1010		;stdZoid <- NULL

# simMode start ----------------------------------------------------
	aZoid <- stdZoid <- c( ,,,,, ) # ZH 832 채워넣을 것.
	allIdxF <- allIdx <- stdIdx <- 
	u0.saveStdZoidFltRst( workH )
		# which(apply(gEnv$allZoidMtx,1,function(zoid){all(zoid==stdZoid)}))
# simMode end   ----------------------------------------------------

finalCut <- function( gEnv ,allIdx ,allZoidGrpName ){
    # cutEadge.getBanPtnColVal() 에서 1~2개 발생 탈락값들에 대한 검토 권장.
	allIdxFObj <- list()
	# 참고 자료 --------------------------------------------------------------------	
	fCutU.rptColValSeqNext( gEnv ,allIdxF ,sprintf("toZ%d",workH) )
	# aQuoTblLst <- fCutU.getQuoTblLst( gEnv$allZoidMtx[allIdxF,] )
	# aQuoTblStr <- sapply( aQuoTblLst ,function(quoTbl){quoTbl$valStr})	;table(aQuoTblStr)

    allIdxF <- allIdx
	stdMI <- fCutU.getMtxInfo( gEnv$zhF )	#	rptObj<-anaMtx( stdMI$rawTail )	# u0.zoidMtx_ana( stdMI$rawTail )


	tStmp <- Sys.time()
	# 기본제거 --------------------------------------------------------------------	
	allIdxF <- fCut.default( gEnv ,allIdxF )

	# colValSeqNext ------------------------------------------------------
	flgCnt <- fCutCnt.colValSeqNext( gEnv ,allIdxF ) 
	flgCnt <- flgCnt + fCutCnt.colValSeqNext.cStep( gEnv ,allIdxF )
	flgCnt <- flgCnt + fCutCnt.default( gEnv ,allIdxF )	# 효율이 의문시된다.
	flag <- flgCnt<2	;table(flag)
    allIdxF <- allIdxF[flag]
	allIdxF.bak <- allIdxF
	allIdxFObj$allIdxF.colValSeqNext <- allIdxF
	cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# allIdxF <- fCut.rawFV3(  gEnv ,allIdxF  )
	# cat(sprintf("allIdxF %d\n",length(allIdxF)))

	allIdxF <- fCut.basic( gEnv ,allIdxF )
	cat(sprintf("allIdxF %d\n",length(allIdxF)))
	allIdxFObj$allIdxF.fCut <- allIdxF
	tDiff <- Sys.time() - tStmp	

	# ------------------------------------------------------------------
	# fCutCnt.**
	#		각 파트에서 2 이상씩은 잘라낸 후,
	#		전체 파트에서 하나도 안 걸린 것들은 제외시키자.
	# ccObj <- fCutCnt.default( gEnv ,allIdxF )

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

	# basic과 nextZW는 fCut.basic()에서 이미 필터링 되는지라..
	ccObj <- fCutCnt.basic( gEnv ,allIdxF )
	allIdxF <- allIdxF[ cutCC( ccObj ,allIdxF ) ]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	ccObj <- fCutCnt.nextZW( gEnv ,allIdxF )
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
	save( ccObjLst ,file="Obj_ccObjLst.save" )

	# surFlag(scoreMtx) ------------------------------------------------------
	surFlag <- rep( TRUE ,length(allIdxF) )
	fName <- attributes(ccObjLst)$names

	cName <- c( "ccc", "auxZW", "auxQuo", "raw", "rawFV", "rem", "cStep", "fStep" )
	thld <- c( 2, 2, 2, 2, 2, 3, 2, 2 )	;names(thld) <- cName

	scoreMtx <- matrix( 0 ,nrow=length(ccObjLst) ,ncol=length(thld) )
	rownames(scoreMtx) <- fName		;colnames(scoreMtx) <- names(thld)

	cName <- c( "reb",  "nbor", "spanM", "quoAll", "quoPtn", "zw",  "remH0", "remH1", "cStep2", "cStep3" )	
	cccMtx <- matrix( 0 ,nrow=length(ccObjLst) ,ncol=length(cName) )	
	rownames(cccMtx) <- fName		;colnames(cccMtx) <- cName
	cccMtx.rCol <- cName[ cName!="reb" ]

	cName <- c( "c31","c32","c33","c34","c21","c22","c23","c24","c25","max2","min2" )	# cccObj$cStepValMtx 
	cStepValMtx = matrix( 0 ,nrow=length(ccObjLst) ,ncol=length(cName) )
	rownames(cStepValMtx) <- fName		;colnames(cStepValMtx) <- cName

	cnt4Spy <- rep( TRUE ,length(allIdxF) )
	cName <- c("reb","spanM","quoPtn")
	spyMtx <- matrix( 0, ncol=length(cName), nrow=length(allIdxF) )	;colnames(spyMtx)<-cName
	for( aIdx in 1:length(allIdxF) ){
		scoreMtx[,] <- 0	;cccMtx[,] <- 0		;cStepValMtx[,] <- 0
		for( nIdx in fName ){
			cccVal <- ccObjLst[[nIdx]]$cccMtx[aIdx,]
			cccMtx[nIdx,] <- ccObjLst[[nIdx]]$cccMtx[aIdx,]

			scoreMtx[nIdx,"ccc"] <- sum( 0 < cccVal[cccMtx.rCol] )
			scoreMtx[nIdx,c("auxZW", "auxQuo")] <- ccObjLst[[nIdx]]$auxCntMtx[aIdx,c("auxZW", "auxQuo")]
			scoreMtx[nIdx,c("raw", "rawFV", "rem", "cStep", "fStep")] <-
				ccObjLst[[nIdx]]$cntMtx[aIdx,c("raw", "rawFV", "rem", "cStep", "fStep")]

			cStepValMtx[nIdx,] <- ccObjLst[[nIdx]]$cStepValMtx[aIdx,]

		} # for(nIdx)

		# scoreMtx : ccc auxZW auxQuo raw rawFV rem cStep fStep
		# cccMtx : reb nbor spanM quoAll quoPtn zw remH0 remH1 cStep2 cStep3
		# cStepValMtx : c31 c32 c33 c34 c21 c22 c23 c24 c25 max2 min2

		cutCnt <- 0
		cnt4Spy[aIdx] <- sum(cccMtx[,c("reb","spanM","quoPtn")])
		spyMtx[aIdx,] <- apply( cccMtx[,c("reb","spanM","quoPtn")] ,2 ,function(p){sum(p>0)} )

		# 이벤트 발생
		eventFlag <- apply(scoreMtx ,1 ,function(score){ sum(score>=thld) })
		if( TRUE ){	# eventFlag
			# 2.1 one dimPlane - gold : event h방향 연속발생 없음.
			#	하지만 4개는 너무 많다... 고로 sumThld 적용
			checkName <- c("nextZW","nextBin","nextColVal_3","nextColVal_6")
			sumThld <- ifelse( 3>=length(checkName) ,0 ,length(checkName)-3 )
			if( sumThld < sum(0<eventFlag[checkName]) ){
				surFlag[aIdx] <- FALSE
				next
			}
			checkName <- c("nextZW","nextQuo10","nextFStepBin","nextColVal_3")
			if( 2 < sum( 0<eventFlag[checkName] ) ){
				# 2.1 one dimPlane - late : event h방향 연속발생 1~2
				surFlag[aIdx] <- FALSE
				next
			}

			hpnCnt <- apply( scoreMtx ,1 ,function(p){sum(p>0)} )
			# 2.1 one dimPlane - common cnt 0~4 (0, 4는 h,dp 모든 방향에서 연속발생 없음)
			#	단 4개 이상은... (gold와 late를 분리할까...)
			checkName <- c("basic","nextBin","nextRebNum","nextColVal_1")
			sumThld <- ifelse( 3>=length(checkName) ,0 ,length(checkName)-3 )
			if( sumThld < sum(0==hpnCnt[checkName]) ){
				surFlag[aIdx] <- FALSE
				next
			}
			# 2.1 one dimPlane - common cnt 0~4 (0, 4는 h,dp 모든 방향에서 연속발생 없음)
			if( any(4<=hpnCnt[c("nextColVal_2","nextFStepBin")]) ){
				surFlag[aIdx] <- FALSE
				next
			}
			if( any(hpnCnt>4) ){
				# 2.1 one dimPlane - cnt 0~4
				surFlag[aIdx] <- FALSE
				next
			}
			hpnCnt.sum <- sum(hpnCnt)
			if( (hpnCnt.sum<20) || (31<hpnCnt.sum) ){
				# 2.2 sum:through dimPlane - common
				surFlag[aIdx] <- FALSE
				next
			}
			eventCnt <- sum( eventFlag )
			if( (eventCnt<1) || (2<eventCnt) ){ 
				# 2.2 sum:through dimPlane - gold : event 1~2(OL:,4)
				surFlag[aIdx] <- FALSE
				next
			}
		}

		# QQE : working
		# common : filt for gold & late
		flagCnt.gold <- finalFilt.common( scoreMtx ,cccMtx ,cStepValMtx ,thld ,cccMtx.rCol )
		if( 5<flagCnt.gold ){
			surFlag[aIdx] <- FALSE
			next
		}

		# wildF : cStep
		wildFMtx <- fCut.wildF_cStep( gEnv$allZoidMtx[allIdxF[aIdx],] )
		if(TRUE){
			#	*2 : gold 기준
			if( 0 < sum(wildFMtx[,"*2"]>0) ){
				surFlag[aIdx] <- FALSE
				next
			}

			#	*1 범위
			wildF1.sum <- sum(wildFMtx[,"*1"]>0)
			if( 3<wildF1.sum ){
				surFlag[aIdx] <- FALSE
				next
			}

			#	*1 과거 패턴 재발 없음.
			pastHpnLst <-list() 
			pastHpnLst[["toZ816"]]	<- c("nextRebNum","nextColVal_1","nextColVal_3")
			pastHpnLst[["toZ819"]]	<- c("nextColVal_5","nextColVal_6")
			pastHpnLst[["toZ820"]]	<- c("basic")
			pastHpnLst[["toZ821"]]	<- c("nextColVal_1","nextColVal_2")
			pastHpnLst[["toZ822"]]	<- c("nextColVal_6")
			pastHpnLst[["toZ823"]]	<- c("nextBin")
			for( nIdx in attributes(pastHpnLst)$names ){
				if( sum(wildFMtx[,"*1"]) != length(pastHpnLst[[nIdx]]) ) next

				if( all(wildFMtx[ pastHpnLst[[nIdx]] ,"*1"]>0) ){
					surFlag[aIdx] <- FALSE
					next
				}
			}

		}

		# QQE : wildF : fStep

		# Filt range : ccc auxZW auxQuo raw rawFV rem cStep fStep
		if(TRUE){
			# ccc
			scoreCut <- fCutU.cutScore( scoreMtx[,"ccc"]
										,pMinMaxSum=c( 0, 9) ,pMinMaxHpn=c( 0, 9) ,pMinMaxEvent=c(2,NA,3) 
									)
			if( 0 < sum(scoreCut,na.rm=T) ) surFlag[aIdx] <- FALSE
			if( any(2<=scoreMtx[c("nextZW"),"ccc"]) )	 surFlag[aIdx] <- FALSE		# gold event rebind
			fltName <- c("nextZW","nextQuo10","nextRebNum","nextFStepBin","nextColVal_2","nextColVal_5","nextColVal_6")
			sumThld <- 2 + ifelse( 3<length(fltName) ,0 ,1 )	# length(fltName)이 3일 때 2가 발생한 적은 있으나, OL로 판단.
			if( sumThld < sum(0<scoreMtx[fltName,"ccc"]) )	 surFlag[aIdx] <- FALSE		# gold hpn rebind

			# auxZW
			scoreCut <- fCutU.cutScore( scoreMtx[,"auxZW"]
										,pMinMaxSum=c(NA, 3) ,pMinMaxHpn=c(NA, 3) ,pMinMaxEvent=c(1,NA,3) 
									)
			if( 0 < sum(scoreCut,na.rm=T) ) surFlag[aIdx] <- FALSE
			if( all(1<=scoreMtx[c("nextRebNum","nextColVal_1"),"auxZW"]) ) surFlag[aIdx] <- FALSE	# gold past hpn rebind

			# auxQuo
			scoreCut <- fCutU.cutScore( scoreMtx[,"auxQuo"]
										,pMinMaxSum=c(NA,2) ,pMinMaxHpn=c(NA,2) ,pMinMaxEvent=c(2,NA,2)
									)
			if( 0 < sum(scoreCut,na.rm=T) ) surFlag[aIdx] <- FALSE
			if( all(1<=scoreMtx[c("nextZW"),"auxQuo"]) ) surFlag[aIdx] <- FALSE	# gold hpn rebind

			# raw
			scoreCut <- fCutU.cutScore( scoreMtx[,"raw"]
										,pMinMaxSum=c(NA,6) ,pMinMaxHpn=c(NA,5) ,pMinMaxEvent=c(2,NA,2) 
									)
			if( 0 < sum(scoreCut,na.rm=T) ) surFlag[aIdx] <- FALSE
			if( all(2<=scoreMtx[c("nextRebNum"),"raw"]) ) surFlag[aIdx] <- FALSE	# gold past event rebind
			
			# rawFV
			scoreCut <- fCutU.cutScore( scoreMtx[,"rawFV"]
										,pMinMaxSum=c(NA,3) ,pMinMaxHpn=c(NA,2) ,pMinMaxEvent=c(2,NA,2) 
									)
			if( 0 < sum(scoreCut,na.rm=T) ) surFlag[aIdx] <- FALSE
			if( all(2<=scoreMtx[c("nextColVal_3"),"rawFV"]) ) surFlag[aIdx] <- FALSE	# gold past event rebind

			# rem
			#	gold에서 event가 없기 한데.. 너무 과한 듯 해서 1까지는 허용.
			scoreCut <- fCutU.cutScore( scoreMtx[,"rem"]
										,pMinMaxSum=c(9,15) ,pMinMaxHpn=c(6,11) ,pMinMaxEvent=c(3,NA,2) 
									)
			if( 0 < sum(scoreCut,na.rm=T) ) surFlag[aIdx] <- FALSE
			if( all(3<=scoreMtx[c("nextColVal_5"),"rem"]) ) surFlag[aIdx] <- FALSE	# late hpn rebind
			# 2는 1번 이상 연속 없음.
			if( 1 < sum(2<=scoreMtx[c("nextFStepBin","nextColVal_2"),"rem"]) ) surFlag[aIdx] <- FALSE	# gold
			if( 1 < sum(2<=scoreMtx[c("nextQuo10","nextFStepBin","nextColVal_3"),"rem"]) ) surFlag[aIdx] <- FALSE	# late
			#	violation이 생기긴 했는데, OL로 판단.

			# cStep
			scoreCut <- fCutU.cutScore( scoreMtx[,"cStep"]
										,pMinMaxSum=c(4,13) ,pMinMaxHpn=c(4,10) ,pMinMaxEvent=c(2,NA,4) 
									)
			if( 0 < sum(scoreCut,na.rm=T) ) surFlag[aIdx] <- FALSE
			if( all(2<=scoreMtx[c("nextBin","nextColVal_3","nextColVal_6"),"cStep"]) ) surFlag[aIdx] <- FALSE	# gold hpn rebind

			# fStep
			scoreCut <- fCutU.cutScore( scoreMtx[,"fStep"]
										,pMinMaxSum=c(0,4) ,pMinMaxHpn=c(0,3) ,pMinMaxEvent=c(2,NA,2) 
									)
			if( 0 < sum(scoreCut,na.rm=T) ) surFlag[aIdx] <- FALSE
			if( all(2<=scoreMtx[c("nextCStepBin"),"fStep"]) ) surFlag[aIdx] <- FALSE	# gold hpn rebind

		}

	}  # aIdx

	table(surFlag)	;kIdx <- head(which(!surFlag))
	allIdxF <- allIdxF[ surFlag ]
	allIdxFObj$allIdxF.surFlag <- allIdxF
	cat(sprintf("allIdxF %d\n",length(allIdxF)))
	# save( allIdxF ,file="Obj_allIdxF.save" )


	tDiff <- Sys.time() - tStmp
	allIdxFObj$timeCost <- tDiff
	allIdxFObj$allIdxF.final <- allIdxF
	save( allIdxFObj ,file=sprintf("Obj_allIdxFObj_%s.save",allZoidGrpName) )


	fCutU.logAllZoidMtx( gEnv$allZoidMtx[ allIdxFObj$allIdxF.final ,,drop=F] 
					,logId=sprintf("finalFor%s_%s",workH,allZoidGrpName) 
				)


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

