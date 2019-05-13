
ref0.lastPtn_cntMtx <- function( ccObjLst ,allIdxF ,mtxLstPh ,mtxLstFlt ){
    #   mtxLstPh=mtxLst.byPh[["cntMtxLst"]]       ;mtxLstFlt=mtxLst.byFlt[["cntMtxLst"]]
	cntMtxLst <- lapply( ccObjLst ,function(p){ p$cntMtx })

	allIdxF.len <- length(allIdxF)
	fltLst <- vector( "list",allIdxF.len )		;names(fltLst) <- paste("a",allIdxF)


    # byPhase ========================================================================================================
	phName <- attributes(cntMtxLst)$names
        #   basic nextZW nextQuo10 nextBin nextRebNum nextCStepBin nextFStepBin 
        #	nextColVal_1 nextColVal_2 nextColVal_3 nextColVal_4 nextColVal_5 nextColVal_6 

    mtxPh <- mtxLstPh[["basic"]]



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

