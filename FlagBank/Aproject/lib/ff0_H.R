# ff0_H.R unit model zero
#   final filtering ÇÔ¼öµé


ff0.filtByOnePhase <- function( gEnv ,allIdxF ){

	ccObj <- fCutCnt.basic( 		gEnv ,allIdxF )
	allIdxF <- allIdxF[ !ff0.byOnePhase(ccObj,phName="basic")$filtedFlg ]
    cat( sprintf(" filtByOnePhase:basic %d \n",length(allIdxF)) )
	ccObj <- fCutCnt.nextZW( 		gEnv ,allIdxF )
	allIdxF <- allIdxF[ !ff0.byOnePhase(ccObj,phName="nextZW")$filtedFlg ]
    cat( sprintf(" filtByOnePhase:nextZW %d \n",length(allIdxF)) )
	ccObj <- fCutCnt.nextQuo10( 	gEnv ,allIdxF )
	allIdxF <- allIdxF[ !ff0.byOnePhase(ccObj,phName="nextQuo10")$filtedFlg ]
    cat( sprintf(" filtByOnePhase:nextQuo10 %d \n",length(allIdxF)) )
	ccObj <- fCutCnt.nextBin( 		gEnv ,allIdxF )
	allIdxF <- allIdxF[ !ff0.byOnePhase(ccObj,phName="nextBin")$filtedFlg ]
    cat( sprintf(" filtByOnePhase:nextBin %d \n",length(allIdxF)) )
	ccObj <- fCutCnt.nextRebNum( 	gEnv ,allIdxF )
	allIdxF <- allIdxF[ !ff0.byOnePhase(ccObj,phName="nextRebNum")$filtedFlg ]
    cat( sprintf(" filtByOnePhase:nextRebNum %d \n",length(allIdxF)) )
	ccObj <- fCutCnt.nextCStepBin(  gEnv ,allIdxF )
	allIdxF <- allIdxF[ !ff0.byOnePhase(ccObj,phName="nextCStepBin")$filtedFlg ]
    cat( sprintf(" filtByOnePhase:nextCStepBin %d \n",length(allIdxF)) )
	ccObj <- fCutCnt.nextFStepBin(  gEnv ,allIdxF )
	allIdxF <- allIdxF[ !ff0.byOnePhase(ccObj,phName="nextFStepBin")$filtedFlg ]
    cat( sprintf(" filtByOnePhase:nextFStepBin %d \n",length(allIdxF)) )
	ccObj <- fCutCnt.nextColVal_1(  gEnv ,allIdxF )
	allIdxF <- allIdxF[ !ff0.byOnePhase(ccObj,phName="nextColVal_1")$filtedFlg ]
    cat( sprintf(" filtByOnePhase:nextColVal_1 %d \n",length(allIdxF)) )
	ccObj <- fCutCnt.nextColVal_2(  gEnv ,allIdxF )
	allIdxF <- allIdxF[ !ff0.byOnePhase(ccObj,phName="nextColVal_2")$filtedFlg ]
    cat( sprintf(" filtByOnePhase:nextColVal_2 %d \n",length(allIdxF)) )
	ccObj <- fCutCnt.nextColVal_3(  gEnv ,allIdxF )
	allIdxF <- allIdxF[ !ff0.byOnePhase(ccObj,phName="nextColVal_3")$filtedFlg ]
    cat( sprintf(" filtByOnePhase:nextColVal_3 %d \n",length(allIdxF)) )
	ccObj <- fCutCnt.nextColVal_4(  gEnv ,allIdxF )
	allIdxF <- allIdxF[ !ff0.byOnePhase(ccObj,phName="nextColVal_4")$filtedFlg ]
    cat( sprintf(" filtByOnePhase:nextColVal_4 %d \n",length(allIdxF)) )
	ccObj <- fCutCnt.nextColVal_5(  gEnv ,allIdxF )
	allIdxF <- allIdxF[ !ff0.byOnePhase(ccObj,phName="nextColVal_5")$filtedFlg ]
    cat( sprintf(" filtByOnePhase:nextColVal_5 %d \n",length(allIdxF)) )
	ccObj <- fCutCnt.nextColVal_6(  gEnv ,allIdxF )
	allIdxF <- allIdxF[ !ff0.byOnePhase(ccObj,phName="nextColVal_6")$filtedFlg ]
    cat( sprintf(" filtByOnePhase:nextColVal_6 %d \n",length(allIdxF)) )

    return( allIdxF )
} # ff0.filtByOnePhase( )



ff0.byOnePhase <- function( ccObj ,phName=NULL ){
    #   ccObj <- fCutCnt.nextColVal_3( gEnv ,allIdxF )
    cntMtx <- ff0.byOnePhase.getCntMtx( ccObj )

    filtedLst <- vector("list",nrow(cntMtx) )
    filtedFlg <- rep( FALSE ,nrow(cntMtx) )

    for( rIdx in 1:nrow(cntMtx) ){
        score <- cntMtx[rIdx,c("raw","rawFV","rem","cStep","fStep","cStep.w1","cStep.w2","fStep.w1","fStep.w2","auxZW","auxQuo")]

        flag <- score[c("raw","rawFV","rem","cStep","fStep","auxZW","auxQuo")] >= c( 2, 1, 3, 2, 2, 1, 1 )
        if( any(flag[c("raw","rawFV","auxQuo")]) ){
            filtedLst[[rIdx]] <- c(filtedLst[[rIdx]],"cntMtx.evt01.out")
            filtedFlg[ rIdx ] <- TRUE
        }

    }

    return( list(filtedLst=filtedLst ,filtedFlg=filtedFlg) )

}   # ff0.byOnePhase( )

ff0.byOnePhase.getCntMtx <- function( ccObj ){
    cntMtx <- cbind( ccObj$cntMtx ,ccObj$auxCntMtx )
    cntMtx[,"cStep"] <- cntMtx[,"cStep"]-(cntMtx[,"cStep.w1"]+cntMtx[,"cStep.w2"])
    cntMtx[,"fStep"] <- cntMtx[,"fStep"]-(cntMtx[,"fStep.w1"]+cntMtx[,"fStep.w2"])

    scoreMtx <- ccObj$cccObj$scoreMtx
    cntMtx[,"cStep.w1"] <- cntMtx[,"cStep.w1"] - scoreMtx[,"w1CStep.cnt"]
    cntMtx[,"fStep.w1"] <- cntMtx[,"fStep.w1"] - scoreMtx[,"w1FStep.cnt"]
    return( cntMtx )
} # ff0.byOnePhase.getCntMtx()


