# ff0_H.R unit model zero
#   final filtering ÇÔ¼öµé


filtByOnePhase <- function( gEnv ,allIdxF ){

	ccObj <- fCutCnt.basic( 		gEnv ,allIdxF )
	allIdxF <- allIdxF[ !byOnePhase(ccObj,phName="basic")$filtedFlg ]
    cat( sprintf(" filtByOnePhase:basic %d \n",length(allIdxF)) )
	ccObj <- fCutCnt.nextZW( 		gEnv ,allIdxF )
	allIdxF <- allIdxF[ !byOnePhase(ccObj,phName="nextZW")$filtedFlg ]
    cat( sprintf(" filtByOnePhase:nextZW %d \n",length(allIdxF)) )
	ccObj <- fCutCnt.nextQuo10( 	gEnv ,allIdxF )
	allIdxF <- allIdxF[ !byOnePhase(ccObj,phName="nextQuo10")$filtedFlg ]
    cat( sprintf(" filtByOnePhase:nextQuo10 %d \n",length(allIdxF)) )
	ccObj <- fCutCnt.nextBin( 		gEnv ,allIdxF )
	allIdxF <- allIdxF[ !byOnePhase(ccObj,phName="nextBin")$filtedFlg ]
    cat( sprintf(" filtByOnePhase:nextBin %d \n",length(allIdxF)) )
	ccObj <- fCutCnt.nextRebNum( 	gEnv ,allIdxF )
	allIdxF <- allIdxF[ !byOnePhase(ccObj,phName="nextRebNum")$filtedFlg ]
    cat( sprintf(" filtByOnePhase:nextRebNum %d \n",length(allIdxF)) )
	ccObj <- fCutCnt.nextCStepBin(  gEnv ,allIdxF )
	allIdxF <- allIdxF[ !byOnePhase(ccObj,phName="nextCStepBin")$filtedFlg ]
    cat( sprintf(" filtByOnePhase:nextCStepBin %d \n",length(allIdxF)) )
	ccObj <- fCutCnt.nextFStepBin(  gEnv ,allIdxF )
	allIdxF <- allIdxF[ !byOnePhase(ccObj,phName="nextFStepBin")$filtedFlg ]
    cat( sprintf(" filtByOnePhase:nextFStepBin %d \n",length(allIdxF)) )
	ccObj <- fCutCnt.nextColVal_1(  gEnv ,allIdxF )
	allIdxF <- allIdxF[ !byOnePhase(ccObj,phName="nextColVal_1")$filtedFlg ]
    cat( sprintf(" filtByOnePhase:nextColVal_1 %d \n",length(allIdxF)) )
	ccObj <- fCutCnt.nextColVal_2(  gEnv ,allIdxF )
	allIdxF <- allIdxF[ !byOnePhase(ccObj,phName="nextColVal_2")$filtedFlg ]
    cat( sprintf(" filtByOnePhase:nextColVal_2 %d \n",length(allIdxF)) )
	ccObj <- fCutCnt.nextColVal_3(  gEnv ,allIdxF )
	allIdxF <- allIdxF[ !byOnePhase(ccObj,phName="nextColVal_3")$filtedFlg ]
    cat( sprintf(" filtByOnePhase:nextColVal_3 %d \n",length(allIdxF)) )
	ccObj <- fCutCnt.nextColVal_4(  gEnv ,allIdxF )
	allIdxF <- allIdxF[ !byOnePhase(ccObj,phName="nextColVal_4")$filtedFlg ]
    cat( sprintf(" filtByOnePhase:nextColVal_4 %d \n",length(allIdxF)) )
	ccObj <- fCutCnt.nextColVal_5(  gEnv ,allIdxF )
	allIdxF <- allIdxF[ !byOnePhase(ccObj,phName="nextColVal_5")$filtedFlg ]
    cat( sprintf(" filtByOnePhase:nextColVal_5 %d \n",length(allIdxF)) )
	ccObj <- fCutCnt.nextColVal_6(  gEnv ,allIdxF )
	allIdxF <- allIdxF[ !byOnePhase(ccObj,phName="nextColVal_6")$filtedFlg ]
    cat( sprintf(" filtByOnePhase:nextColVal_6 %d \n",length(allIdxF)) )

    return( allIdxF )
} # filtByOnePhase( )



byOnePhase <- function( ccObj ,phName=NULL ){
    #   ccObj <- fCutCnt.nextColVal_3( gEnv ,allIdxF )
    cntMtx <- byOnePhase.getCntMtx( ccObj )
    

    filtedLst <- vector("list",nrow(cntMtx) )
    filtedFlg <- rep( FALSE ,nrow(cntMtx) )

    for( rIdx in 1:nrow(cntMtx) ){
        score <- cntMtx[rIdx,c("raw","rawFV","rem","cStep","fStep","cStep.w1","cStep.w2","fStep.w1","fStep.w2","auxZW","auxQuo")]

        
        if( !is.null(phName) ){
            if( any(phName%in%c("basic")) ){
                flag <- score[c("raw","rawFV","rem","cStep","fStep","auxZW","auxQuo")] >= c( 2, 1, 3, 2, 2, 1, 1 )
                if( any(flag[c("raw","rawFV","rem","cStep","fStep")]) ){
                    filtedFlg[ rIdx ] <- TRUE
                }
            }
            if( any(phName%in%c("nextZW","nextQuo10")) ){
                flag <- score[c("raw","rawFV","rem","cStep","fStep","auxZW","auxQuo")] >= c( 2, 2, 3, 2, 2, 1, 1 )
                if( 1<sum(flag) ){
                    filtedFlg[ rIdx ] <- TRUE
                }
            }
        }

        flag <- score[c("raw","rawFV","rem","cStep","fStep","auxZW","auxQuo")] >= c( 2, 2, 3, 2, 2, 1, 1 )
        if( 2< sum(flag) ){
            filtedLst[[rIdx]] <- c(filtedLst[[rIdx]],"cntMtx.evt01.out")
            filtedFlg[ rIdx ] <- TRUE
        } else if( 2==sum(flag) ){
            filtedLst[[rIdx]] <- c(filtedLst[[rIdx]],"cntMtx.evt01")
        }
        if( 1< sum(flag[c("rawFV","auxZW","auxQuo")]) ){
            filtedLst[[rIdx]] <- c(filtedLst[[rIdx]],"cntMtx.evt02.out")
            filtedFlg[ rIdx ] <- TRUE
        }
        if( 2< score["rawFV"] ){
            filtedLst[[rIdx]] <- c(filtedLst[[rIdx]],"cntMtx.evt03.out")
            filtedFlg[ rIdx ] <- TRUE
        } else if( 1<=score["rawFV"] ){
            filtedLst[[rIdx]] <- c(filtedLst[[rIdx]],"cntMtx.evt03")
        }
        if( 1< sum(flag[c("auxZW","auxQuo")]) ){
            filtedLst[[rIdx]] <- c(filtedLst[[rIdx]],"cntMtx.evt04.out")
            filtedFlg[ rIdx ] <- TRUE
        }

        flag <- score[c("cStep.w1","cStep.w2","fStep.w1","fStep.w2")] > 0
        if( 2< sum(flag) ){
            filtedLst[[rIdx]] <- c(filtedLst[[rIdx]],"cntMtx.evt11.out")
            filtedFlg[ rIdx ] <- TRUE
        } else if( 2==sum(flag) ){
            filtedLst[[rIdx]] <- c(filtedLst[[rIdx]],"cntMtx.evt11")
        }
        flag <- score[c("cStep.w1","cStep.w2","fStep.w1","fStep.w2")] > 1
        if( 0< sum(flag) ){
            filtedLst[[rIdx]] <- c(filtedLst[[rIdx]],"cntMtx.evt12.out")
            filtedFlg[ rIdx ] <- TRUE
        }

        # --------------------------------------------------------------------
        if( 2<=length(filtedLst[[rIdx]]) ){
            filtedLst[[rIdx]] <- c(filtedLst[[rIdx]],"cntMtx.evtZZ.out")
            filtedFlg[ rIdx ] <- TRUE
        }

    }

    for( rIdx in 1:nrow() ){

    }


    return( list(filtedLst=filtedLst ,filtedFlg=filtedFlg) )

}   # byOnePhase( )

byOnePhase.getCntMtx <- function( ccObj ){
    cntMtx <- cbind( ccObj$cntMtx ,ccObj$auxCntMtx )
    cntMtx[,"cStep"] <- cntMtx[,"cStep"]-(cntMtx[,"cStep.w1"]+cntMtx[,"cStep.w2"])
    cntMtx[,"fStep"] <- cntMtx[,"fStep"]-(cntMtx[,"fStep.w1"]+cntMtx[,"fStep.w2"])
    return( cntMtx )
} # byOnePhase.getCntMtx()


