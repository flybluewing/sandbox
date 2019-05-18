
ref0.lastPtn_cntMtx <- function( ccObjLst ,allIdxF ,mtxLstPh ,mtxLstFlt ){
    #   mtxLstPh=mtxLst.byPh[["cntMtxLst"]]       ;mtxLstFlt=mtxLst.byFlt[["cntMtxLst"]]
	cntMtxLst <- lapply( ccObjLst ,function(p){ cbind(p$cntMtx,p$auxCntMtx) })

	allIdxF.len <- length(allIdxF)
	
	fltLst <- vector( "list",allIdxF.len )		;names(fltLst) <- paste("a",allIdxF)

    # byPhase ========================================================================================================
	phNames <- attributes(cntMtxLst)$names
        #   basic nextZW nextQuo10 nextBin nextRebNum nextCStepBin nextFStepBin 
        #	nextColVal_1 nextColVal_2 nextColVal_3 nextColVal_4 nextColVal_5 nextColVal_6 

	phName <- "basic"
    stdMtx <- mtxLstPh[[phName]]
	aMtx <- cntMtxLst[[phName]]

	workCol <- c("raw","rem","cStep","fStep","auxZW")
	evtThld <- c( 2,3,2,2,1 )	;names(evtThld) <- workCol
	stdMtx.evt <- apply(stdMtx[,workCol],1,function(rowD){ 
					flag <- rowD>=evtThld	;flag[!flag] <- NA
					return(flag)
				})
	stdMtx.evt <- t(stdMtx.evt)
	stdMtx.hpn <- stdMtx[,workCol]
	stdMtx.hpn[stdMtx.hpn==0] <- NA
	for( aIdx in 1:allIdxF.len ){
		#	Event
		flag <- aMtx[aIdx,workCol]>=evtThld
		flag[!flag] <- NA
		rst <- fCutU.checkOverlap4Mtx( stdMtx.evt ,flag ,maxDepth=10 )	# valCnt ,lstCnt ,lstMtch ,lstMtch.hpn ,maxMtch 
		if( rst["lstCnt"]>0 )	fltLst[[aIdx]] <- sprintf("E.byPhase evt %s lstCnt",phName)
		if( rst["maxMtch"]>1 )	fltLst[[aIdx]] <- sprintf("E.byPhase evt %s maxMtch",phName)

		#	Happen
		flag <- aMtx[aIdx,workCol]
		flag[flag==0] <- NA
		rst <- fCutU.checkOverlap4Mtx( stdMtx.hpn ,flag ,maxDepth=10 )	# valCnt ,lstCnt ,lstMtch ,lstMtch.hpn ,maxMtch 
		if( rst["lstCnt"]>0 )	fltLst[[aIdx]] <- sprintf("E.byPhase hpn %s lstCnt",phName)
		if( rst["maxMtch"]>1 )	fltLst[[aIdx]] <- sprintf("E.byPhase hpn %s maxMtch",phName)
	}
	# table(do.call(c,fltLst,))	;fltCnt <- sapply(fltLst,length)	;table(fltCnt)	;dbgIdx <- which(fltCnt>0)
	# dbgIdx<-which(sapply(fltLst,function(p){any(p=="E.byPhase evt basic maxMtch")}))		;aIdx <- dbgIdx[1]

	#	raw rawFV rem cStep fStep raw.w1 cStep.w1 cStep.w2 fStep.w1 fStep.w2 auxZW auxQuo
	# QQE 기능확인.


	rawMtx <- do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"raw"] } ) )	#	rownames(rawMtx) <- allIdxF
	rawFVMtx <- do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"rawFV"] } ) )	#	rownames(rawFVMtx) <- allIdxF
	remMtx <-  do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"rem"] } ) )

	cStepMtx <-  do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"cStep"] } ) )	# w1, w2는 이전에서 정산되어 있어야 한다.
	cStepMtx.w1 <-  do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"cStep.w1"] } ) )
	cStepMtx.w2 <-  do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"cStep.w2"] } ) )

	fStepMtx <-  do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"fStep"] } ) )	# w1, w2는 이전에서 정산되어 있어야 한다.
	fStepMtx.w1 <-  do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"fStep.w1"] } ) )
	fStepMtx.w2 <-  do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"fStep.w2"] } ) )


	# cnt <- rep( 0 ,nrow(cntMtxLst[[1]]) )	;names(cnt) <- allIdxF

	for( aIdx in seq_len(allIdxF.len) ){
	}
	# debug
	# table(do.call(c,fltLst,))	;fltCnt <- sapply(fltLst,length)	;table(fltCnt)	;dbgIdx <- which(fltCnt>0)
	# dbgIdx<-which(sapply(fltLst,function(p){any(p=="fStep021")}))		;aIdx <- dbgIdx[1]

	fltCnt <- sapply(fltLst,length)
	allIdxF <- allIdxF[fltCnt==0]
	# fltFlag <- rep( FALSE ,nrow(cntMtxLst[[1]]) )	;names(fltFlag) <- allIdxF
	return( list( allIdxF=allIdxF ,fltCnt=fltCnt ) )

}   # ref0.lastPtn_cntMtx()

