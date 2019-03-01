# ff0_H.R unit model zero
#   final filtering 함수들


ff0.filtByOnePhase <- function( gEnv ,allIdxF ){

	ccObj <- fCutCnt.basic( 		gEnv ,allIdxF )
	allIdxF <- allIdxF[ !ff0.byOnePhase.cut(ccObj,phName="basic")$filtedFlg ]
    cat( sprintf(" filtByOnePhase:basic %d \n",length(allIdxF)) )
	ccObj <- fCutCnt.nextZW( 		gEnv ,allIdxF )
	allIdxF <- allIdxF[ !ff0.byOnePhase.cut(ccObj,phName="nextZW")$filtedFlg ]
    cat( sprintf(" filtByOnePhase:nextZW %d \n",length(allIdxF)) )
	ccObj <- fCutCnt.nextQuo10( 	gEnv ,allIdxF )
	allIdxF <- allIdxF[ !ff0.byOnePhase.cut(ccObj,phName="nextQuo10")$filtedFlg ]
    cat( sprintf(" filtByOnePhase:nextQuo10 %d \n",length(allIdxF)) )
	ccObj <- fCutCnt.nextBin( 		gEnv ,allIdxF )
	allIdxF <- allIdxF[ !ff0.byOnePhase.cut(ccObj,phName="nextBin")$filtedFlg ]
    cat( sprintf(" filtByOnePhase:nextBin %d \n",length(allIdxF)) )
	ccObj <- fCutCnt.nextRebNum( 	gEnv ,allIdxF )
	allIdxF <- allIdxF[ !ff0.byOnePhase.cut(ccObj,phName="nextRebNum")$filtedFlg ]
    cat( sprintf(" filtByOnePhase:nextRebNum %d \n",length(allIdxF)) )
	ccObj <- fCutCnt.nextCStepBin(  gEnv ,allIdxF )
	allIdxF <- allIdxF[ !ff0.byOnePhase.cut(ccObj,phName="nextCStepBin")$filtedFlg ]
    cat( sprintf(" filtByOnePhase:nextCStepBin %d \n",length(allIdxF)) )
	ccObj <- fCutCnt.nextFStepBin(  gEnv ,allIdxF )
	allIdxF <- allIdxF[ !ff0.byOnePhase.cut(ccObj,phName="nextFStepBin")$filtedFlg ]
    cat( sprintf(" filtByOnePhase:nextFStepBin %d \n",length(allIdxF)) )
	ccObj <- fCutCnt.nextColVal_1(  gEnv ,allIdxF )
	allIdxF <- allIdxF[ !ff0.byOnePhase.cut(ccObj,phName="nextColVal_1")$filtedFlg ]
    cat( sprintf(" filtByOnePhase:nextColVal_1 %d \n",length(allIdxF)) )
	ccObj <- fCutCnt.nextColVal_2(  gEnv ,allIdxF )
	allIdxF <- allIdxF[ !ff0.byOnePhase.cut(ccObj,phName="nextColVal_2")$filtedFlg ]
    cat( sprintf(" filtByOnePhase:nextColVal_2 %d \n",length(allIdxF)) )
	ccObj <- fCutCnt.nextColVal_3(  gEnv ,allIdxF )
	allIdxF <- allIdxF[ !ff0.byOnePhase.cut(ccObj,phName="nextColVal_3")$filtedFlg ]
    cat( sprintf(" filtByOnePhase:nextColVal_3 %d \n",length(allIdxF)) )
	ccObj <- fCutCnt.nextColVal_4(  gEnv ,allIdxF )
	allIdxF <- allIdxF[ !ff0.byOnePhase.cut(ccObj,phName="nextColVal_4")$filtedFlg ]
    cat( sprintf(" filtByOnePhase:nextColVal_4 %d \n",length(allIdxF)) )
	ccObj <- fCutCnt.nextColVal_5(  gEnv ,allIdxF )
	allIdxF <- allIdxF[ !ff0.byOnePhase.cut(ccObj,phName="nextColVal_5")$filtedFlg ]
    cat( sprintf(" filtByOnePhase:nextColVal_5 %d \n",length(allIdxF)) )
	ccObj <- fCutCnt.nextColVal_6(  gEnv ,allIdxF )
	allIdxF <- allIdxF[ !ff0.byOnePhase.cut(ccObj,phName="nextColVal_6")$filtedFlg ]
    cat( sprintf(" filtByOnePhase:nextColVal_6 %d \n",length(allIdxF)) )

    return( allIdxF )
} # ff0.filtByOnePhase( )



ff0.byOnePhase.cut <- function( ccObj ,phName=NULL ){
    #   ccObj <- fCutCnt.nextColVal_3( gEnv ,allIdxF )
    cntMtx <- ff0.byOnePhase.getCntMtx( ccObj )
    scoreMtx <- ff0.byOnePhase.scoreMtx( ccObj )
    scoreMtx2 <- ff0.byOnePhase.scoreMtx2( ccObj )
    cStepValMtx <- ff0.byOnePhase.cStepValMtx( ccObj )
    filtedLst <- vector("list",nrow(cntMtx) )
    filtedFlg <- rep( FALSE ,nrow(cntMtx) )

    for( rIdx in 1:nrow(cntMtx) ){  # cntMtx
        score <- cntMtx[rIdx,c("raw","rawFV","rem","cStep","fStep","cStep.w1","cStep.w2","fStep.w1","fStep.w2","auxZW","auxQuo")]

        flag <- score[c("raw","rawFV","rem","cStep","fStep","auxZW","auxQuo")] >= c( 2, 1, 3, 2, 2, 1, 1 )
        if( any(flag[c("rawFV","auxQuo")]) ){
            filtedLst[[rIdx]] <- c(filtedLst[[rIdx]],"cntMtx.evt01.out")    ;filtedFlg[ rIdx ] <- TRUE
        }
        if( 1<sum(flag[c("raw","rem","cStep","fStep")]) ){
            filtedLst[[rIdx]] <- c(filtedLst[[rIdx]],"cntMtx.evt02.out")    ;filtedFlg[ rIdx ] <- TRUE
        }

        flag <- score[c("cStep.w1","cStep.w2","fStep.w1","fStep.w2")] > 1
        if( any(flag) ){    filtedLst[[rIdx]] <- c(filtedLst[[rIdx]],"cntMtx.evt10.out")    ;filtedFlg[ rIdx ] <- TRUE
        }
        flagScore <- c( sum(score[c("cStep.w1","cStep.w2")]) ,sum(score[c("fStep.w1","fStep.w2")]) )
        if( any(flagScore>1)  ){    filtedLst[[rIdx]] <- c(filtedLst[[rIdx]],"cntMtx.evt11.out")    ;filtedFlg[ rIdx ] <- TRUE
        }

    }   # table(filtedFlg)  ;dbgIdx <- head(which(filtedFlg))

    for( rIdx in 1:nrow(cntMtx) ){  # scoreMtx
        score <- scoreMtx[rIdx,]
        flag <- score[c("w1CStep.matLen","w1FStep.matLen")] > 2
        if( any(flag) ){    filtedLst[[rIdx]] <- c(filtedLst[[rIdx]],"cntMtx.evtA1.out")    ;filtedFlg[ rIdx ] <- TRUE
        }
        flag <- score[c("w1CStep.cnt","w1FStep.cnt")] > 1
        if( any(flag) ){    filtedLst[[rIdx]] <- c(filtedLst[[rIdx]],"cntMtx.evtA2.out")    ;filtedFlg[ rIdx ] <- TRUE
        }
        flag <- score[c("quoPtn","zw","remH0","remH1","cStep2","cStep3","w1CStep.cnt","w1FStep.cnt")] > 0
        if( 2<sum(flag) ){  filtedLst[[rIdx]] <- c(filtedLst[[rIdx]],"cntMtx.evtA3.out")    ;filtedFlg[ rIdx ] <- TRUE
        }
        flag <- score[c("reb","nbor","spanM","quoAll")] > 0
        if( 2<sum(flag) ){  filtedLst[[rIdx]] <- c(filtedLst[[rIdx]],"cntMtx.evtA4.out")    ;filtedFlg[ rIdx ] <- TRUE
        }

        flagGrp1 <- score[c("quoPtn","zw","remH0","remH1","cStep2","cStep3","w1CStep.cnt","w1FStep.cnt")]
        flagGrp2 <- score[c("reb","nbor","spanM","quoAll")] > 0
        if( 2 < sum(c(flagGrp1,flagGrp2)) ){
              filtedLst[[rIdx]] <- c(filtedLst[[rIdx]],"cntMtx.evtA5.out")    ;filtedFlg[ rIdx ] <- TRUE
        }
    }   # table(filtedFlg)  ;dbgIdx <- head(which(filtedFlg))

    for( rIdx in 1:nrow(cntMtx) ){  # scoreMtx2
        score <- scoreMtx2[rIdx,]
        # rebV rebC rebL rebR rebL.cnt rebR.cnt inc.raw inc.cStep
        flag <- score[c("rebV","rebC","rebL","rebR","inc.raw", "inc.cStep")] >= c( 4, 3, 2, 2, 3, 2 )
        if( any(flag) ){    filtedLst[[rIdx]] <- c(filtedLst[[rIdx]],"scoreMtx2.evtA01.out")    ;filtedFlg[ rIdx ] <- TRUE
        }

    }   # table(filtedFlg)  ;dbgIdx <- head(which(filtedFlg))


    for( rIdx in 1:nrow(cntMtx) ){  # cStepValMtx
        score <- cStepValMtx[rIdx,]

        flag <- score[c("max2","min2")] > 0
        if( any(flag) ){  filtedLst[[rIdx]] <- c(filtedLst[[rIdx]],"cntMtx.ev_01.out")    ;filtedFlg[ rIdx ] <- TRUE
        }
        flag <- score[c("c21","c22","c23","c24","c25")] > 0
        if( 2<sum(flag) ){  filtedLst[[rIdx]] <- c(filtedLst[[rIdx]],"cntMtx.ev_02.out")    ;filtedFlg[ rIdx ] <- TRUE
        }
        flag <- score[c("c31","c32","c33","c34")] > 0
        if( 2<sum(flag) ){  filtedLst[[rIdx]] <- c(filtedLst[[rIdx]],"cntMtx.ev_03.out")    ;filtedFlg[ rIdx ] <- TRUE
        }

        flagGrp1 <- score[c("c21","c22","c23","c24","c25")] > 0
        flagGrp2 <- score[c("c31","c32","c33","c34")] > 0
        if( 2<sum(sum(flagGrp1,flagGrp2)) ){  filtedLst[[rIdx]] <- c(filtedLst[[rIdx]],"cntMtx.ev_04.out")    ;filtedFlg[ rIdx ] <- TRUE
        }

    }   # table(filtedFlg)  ;dbgIdx <- head(which(filtedFlg))   ;rIdx<-dbgIdx[1]



    return( list(filtedLst=filtedLst ,filtedFlg=filtedFlg) )

}   # ff0.byOnePhase.cut( )

ff0.byOnePhase.getCntMtx <- function( ccObj ){
    cntMtx <- cbind( ccObj$cntMtx ,ccObj$auxCntMtx )
    cntMtx[,"cStep"] <- cntMtx[,"cStep"]-(cntMtx[,"cStep.w1"]+cntMtx[,"cStep.w2"])
    cntMtx[,"fStep"] <- cntMtx[,"fStep"]-(cntMtx[,"fStep.w1"]+cntMtx[,"fStep.w2"])

    scoreMtx <- ccObj$cccObj$scoreMtx
    cntMtx[,"cStep.w1"] <- cntMtx[,"cStep.w1"] - scoreMtx[,"w1CStep.cnt"]
    cntMtx[,"fStep.w1"] <- cntMtx[,"fStep.w1"] - scoreMtx[,"w1FStep.cnt"]
    return( cntMtx )
} # ff0.byOnePhase.getCntMtx()

ff0.byOnePhase.scoreMtx <- function( ccObj ){
    return( ccObj$cccObj$scoreMtx )
} # ff0.byOnePhase.scoreMtx()

ff0.byOnePhase.scoreMtx2 <- function( ccObj ){
    return( ccObj$cccObj$scoreMtx2 )
} # ff0.byOnePhase.scoreMtx()


ff0.byOnePhase.cStepValMtx <- function( ccObj ){
    return( ccObj$cccObj$cStepValMtx )
} # ff0.byOnePhase.cStepValMtx()


ff0.cut.thrwPhase <- function( gEnv ,allIdxF ){

    filtedLst <- vector("list",length(allIdxF) )
    filtedFlg <- rep( FALSE ,length(allIdxF) )
    ccObjLst <- u0.getPhObjLst( gEnv ,allIdxF )
    phName <- attributes(ccObjLst)$names

    cntMtxLst <- lapply(ccObjLst,function(ccObj){ ff0.byOnePhase.getCntMtx(ccObj) })
    for( idx in seq_len(length(allIdxF)) ){
        cntMtx <- do.call( rbind ,lapply(cntMtxLst,function(mtx){mtx[idx,]}) )  #   row : phName     col : flt name

        evtCnt <- sum(cntMtx[,"raw"]>1)
        if( evtCnt>1 ){  filtedFlg[[idx]] <- TRUE
        }

        evtCnt <- c( sum(cntMtx[,"raw"]>1) ,sum(cntMtx[,"rem"]>2) ,sum(cntMtx[,"cStep"]>1) ,sum(cntMtx[,"fStep"]>1) )
        names(evtCnt) <- c("raw","rem","cStep","fStep")
        if( 4<sum(evtCnt) ){  filtedFlg[[idx]] <- TRUE
        }

        evtCnt <- apply( cntMtx[,c("cStep.w1","cStep.w2","fStep.w1","fStep.w2")] ,1 ,function(wDat){ sum(wDat>0) } )
        if( 2<sum(evtCnt>1) ){  filtedFlg[[idx]] <- TRUE    # 2번 이상 발생 ph가 2개 초과 불가.
        }


    }   # table(filtedFlg)  ;dbgIdx <- head(which(filtedFlg))   ;idx<-dbgIdx[1]

    # test -0---------------------------------------------------------------------------------
    filtedLst <- vector("list",length(allIdxF) )
    filtedFlg <- rep( FALSE ,length(allIdxF) )
    cntMtxLst <- lapply(ccObjLst,function(ccObj){ ff0.byOnePhase.getCntMtx(ccObj) })
    for( idx in seq_len(length(allIdxF)) ){
        cntMtx <- do.call( rbind ,lapply(cntMtxLst,function(mtx){mtx[idx,]}) )  #   row : phName     col : flt name

        evtCnt <- apply( cntMtx[,c("raw","rem","cStep","fStep","cStep.w1","cStep.w2","fStep.w1","fStep.w2")] ,1 
                            ,function(wDat){ sum(wDat>c(1,2,1,1,0,0,0,0)) 
                        } )
        if( 2<sum(evtCnt>1) ){  filtedFlg[[idx]] <- TRUE    # 이벤트가 한번도 발생하지 않은 적은 없겠지.
        }

    }   ; table(filtedFlg)  ;dbgIdx <- head(which(filtedFlg))   ;idx<-dbgIdx[1]

    return( allIdxF )

}   # ff0.cut.thrwPhase

#     ccObjLst <- u0.getPhObjLst( gEnv ,allIdxF )
#     cntMtxLst <- list() ;scoreMtxLst <- list()  ;cStepValMtxLst  <- list()
#     for( nIdx in attributes(ccObjLst)$names ){
#         cntMtxLst[[     nIdx]] <- ff0.byOnePhase.getCntMtx(     ccObjLst[[nIdx]] )
#         scoreMtxLst[[   nIdx]] <- ff0.byOnePhase.scoreMtx(      ccObjLst[[nIdx]] )
#         cStepValMtxLst[[nIdx]] <- ff0.byOnePhase.cStepValMtx(   ccObjLst[[nIdx]] )
#     }



ff0.thrwPhase.cntMtx <- function( cntMtxLst ){

    phName <- attributes(cntMtxLst)$names
    zoidNum <- nrow(cntMtxLst[[1]])

    zoidEvtLst <- list()    # zoidNum
    for( zIdx in 1:zoidNum ){
        evtLst <- list()



        zoidEvtLst[[1+length(zoidEvtLst)]] <- evtLst
    }

    return( zoidEvtLst )

} # ff0.thrwPhase.cntMtx()



