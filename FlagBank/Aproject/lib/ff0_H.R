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
    scoreMtx3 <- ff0.byOnePhase.scoreMtx3( ccObj )
    scoreMtx4 <- ff0.byOnePhase.scoreMtx4( ccObj )
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

        flag <- score[c("cStep.w1","cStep.w2","fStep.w1")] > 1  # ,"fStep.w2"
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
        flag <- score[c("rebV","rebC","rebC2","rebL","rebR","inc.raw", "inc.cStep")] >= c( 4, 3, 3, 2, 2, 3, 2 )
        if( any(flag) ){    filtedLst[[rIdx]] <- c(filtedLst[[rIdx]],"scoreMtx2.evtA01.out")    ;filtedFlg[ rIdx ] <- TRUE
        }
        if( 1<sum(score[c("rebC","rebC2")==2]) ){    
            filtedLst[[rIdx]] <- c(filtedLst[[rIdx]],"scoreMtx2.evtA02.out")    ;filtedFlg[ rIdx ] <- TRUE
        }

    }   # table(filtedFlg)  ;dbgIdx <- head(which(filtedFlg))

    for( rIdx in 1:nrow(cntMtx) ){  # scoreMtx3
        score <- scoreMtx3[rIdx,]
        # rebPtn.1 rebPtn.n rebC.C1 rebC.F1 rebC.C2 rebC.F2
        flag <- score[c("rebPtn.1","rebPtn.n","rebC.C1","rebC.F1","rebC.C2","rebC.F2")] >= c( 2 ,1 ,3 ,3 ,3 ,3 )
        if( any(flag) ){    filtedLst[[rIdx]] <- c(filtedLst[[rIdx]],"scoreMtx3.evtA01.out")    ;filtedFlg[ rIdx ] <- TRUE
        }
        if( 2<sum(score[c("rebC.C1","rebC.F1","rebC.C2","rebC.F2")==2]) ){    
            filtedLst[[rIdx]] <- c(filtedLst[[rIdx]],"scoreMtx3.evtA02.out")    ;filtedFlg[ rIdx ] <- TRUE
        }

    }   # table(filtedFlg)  ;dbgIdx <- head(which(filtedFlg))

    for( rIdx in 1:nrow(cntMtx) ){  # scoreMtx4
        score <- scoreMtx4[rIdx,]
        #  incRaw3 incC3 incF3 incRaw2 incC2 incF2 (1,6)
        flag <- score[c("incRaw3" ,"incC3" ,"incF3" ,"incRaw2" ,"incC2" ,"incF2" ,"(1,6)" ,"nextVal.r" ,"nextVal.c" ,"nextVal.f")] >= c( 1, 1, 1, 2, 2, 2, 2, 3, 3, 3 )
        if( any(flag) ){    filtedLst[[rIdx]] <- c(filtedLst[[rIdx]],"scoreMtx4.evtA01.out")    ;filtedFlg[ rIdx ] <- TRUE
        }
        if( 2<sum(score[c("incRaw2" ,"incC2" ,"incF2")]) ){    
            filtedLst[[rIdx]] <- c(filtedLst[[rIdx]],"scoreMtx4.evtA02.out")    ;filtedFlg[ rIdx ] <- TRUE
        }
        if( 1<sum(1<score[c("nextVal.r" ,"nextVal.c" ,"nextVal.f")]) ){    
            filtedLst[[rIdx]] <- c(filtedLst[[rIdx]],"scoreMtx4.evtA03.out")    ;filtedFlg[ rIdx ] <- TRUE
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

ff0.byOnePhase.scoreMtx3 <- function( ccObj ){
    return( ccObj$cccObj$scoreMtx3 )
} # ff0.byOnePhase.scoreMtx()

ff0.byOnePhase.scoreMtx4 <- function( ccObj ){
    return( ccObj$cccObj$scoreMtx4 )
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




#	static과 custom 분리필요
ff0.fltCntMtx		<- function( ccObjLst ,allIdxF ){
	# 	# w1, w2는 이전에서 정산되어 있어야 한다.

	cntMtxLst <- lapply( ccObjLst ,function(p){ p$cntMtx })
	phName <- attributes(cntMtxLst)$names

	# cnt <- rep( 0 ,nrow(cntMtxLst[[1]]) )	;names(cnt) <- allIdxF
	rawMtx <- do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"raw"] } ) )	#	rownames(rawMtx) <- allIdxF
	rawFVMtx <- do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"rawFV"] } ) )	#	rownames(rawFVMtx) <- allIdxF
	remMtx <-  do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"rem"] } ) )
	
	cStepMtx <-  do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"cStep"] } ) )	# w1, w2는 이전에서 정산되어 있어야 한다.
	cStepMtx.w1 <-  do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"cStep.w1"] } ) )
	cStepMtx.w2 <-  do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"cStep.w2"] } ) )

	fStepMtx <-  do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"fStep"] } ) )	# w1, w2는 이전에서 정산되어 있어야 한다.
	fStepMtx.w1 <-  do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"fStep.w1"] } ) )
	fStepMtx.w2 <-  do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"fStep.w2"] } ) )


    #   basic nextZW nextQuo10 nextBin nextRebNum nextCStepBin nextFStepBin 
	#	nextColVal_1 nextColVal_2 nextColVal_3 nextColVal_4 nextColVal_5 nextColVal_6 

	allIdxF.len <- length(allIdxF)
	fltLst <- vector( "list",allIdxF.len )		;names(fltLst) <- paste("a",allIdxF)
	for( aIdx in seq_len(allIdxF.len) ){

		# # -[rawMtx]-----------------------------------------------------------
			fndIdx <- which(rawMtx[aIdx,]>2)
			if(0<length(fndIdx)) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"raw001" )
			fndIdx <- which(rawMtx[aIdx,]==2)
			if(0<length(fndIdx)){
				if( 2< length(fndIdx) )	fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"raw011" )
				if( 2==length(fndIdx) ){
					# if( all(c("nextQuo10","nextColVal_6")==names(fndIdx)) )		fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"raw012" )
					fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"raw012" )
				}
				if( 1==length(fndIdx) ){
					banCol <- c("nextRebNum")
					if( banCol %in% names(fndIdx) )		fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"raw013" )
				}
			}
			fndIdx <- which(rawMtx[aIdx,]==1)
			if( 5<length(fndIdx) ) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"raw021" )
		# # -[remMtx]-----------------------------------------------------------
			fndIdx <- which(remMtx[aIdx,]>3)
			if(0<length(fndIdx)){
				if(1<length(fndIdx)) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"rem001" )
				if( any(names(fndIdx)%in%c("nextColVal_2"))  ) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"rem002" )
			}
			fndIdx <- which(remMtx[aIdx,]==3)
			if(0<length(fndIdx)){
				if( 2<length(fndIdx) ) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"rem011" )
				if( 1==length(fndIdx) ){
					banCol <- c("nextColVal_3","nextQuo10","nextColVal_6")
					if( names(fndIdx) %in% banCol ) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"rem012" )
				}
			}
			fndIdx <- which(remMtx[aIdx,]==2)
			if( 4<length(fndIdx) || length(fndIdx)<2 ) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"rem021" )
			fndIdx <- which(remMtx[aIdx,]==1)
			if( 8<length(fndIdx) || length(fndIdx)<2 ) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"rem031" )
			fndIdx <- which( 0<remMtx[aIdx,] & remMtx[aIdx,]<3 )
			if( 10<length(fndIdx) || length(fndIdx)<4 ) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"rem041" )
		# # -[cStepMtx]-----------------------------------------------------------
			fndIdx <- which(cStepMtx[aIdx,]>2)
			if(0<length(fndIdx)){
				if(1<length(fndIdx)) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"cStep001" )
				banCol <- c("nextColVal_4")
				if( any(names(fndIdx)%in%banCol)  ) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"cStep002" )
			}
			fndIdx <- which(cStepMtx[aIdx,]==2)
			if(0<length(fndIdx)){
				if( 3<length(fndIdx) ) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"cStep011" )
			}
			fndIdx <- which(cStepMtx[aIdx,]==1)
			if( 8<length(fndIdx) || length(fndIdx)<2 ) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"cStep021" )
		# # -[fStepMtx]-----------------------------------------------------------
			fndIdx <- which(fStepMtx[aIdx,]>2)
			if(0<length(fndIdx)) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"fStep001" )
			fndIdx <- which(fStepMtx[aIdx,]==2)
			if(1<length(fndIdx)) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"fStep011" )
			fndIdx <- which(fStepMtx[aIdx,]==1)
			if(3<length(fndIdx)) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"fStep021" )

		# # -[cStepMtx.w1]-----------------------------------------------------------
			fndIdx <- which(cStepMtx.w1[aIdx,]>1)
			if(0<length(fndIdx)) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"cStepW1:001" )
			fndIdx <- which(cStepMtx.w1[aIdx,]==1)
			if(4<length(fndIdx)) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"cStepW1:011" )
		# # -[cStepMtx.w2]-----------------------------------------------------------
			fndIdx <- which(cStepMtx.w2[aIdx,]>1)
			if(0<length(fndIdx)) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"cStepW2:001" )
			fndIdx <- which(cStepMtx.w2[aIdx,]==1)
			if(1<length(fndIdx)) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"cStepW2:011" )
			if( 1==length(fndIdx) ) {
				banCol <- c("nextRebNum","nextQuo10","nextFStepBin","nextColVal_1","nextColVal_6")
				if( any(banCol%in%names(fndIdx)) ) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"cStepW2:021" )
			}

		# # -[fStepMtx.w1]-----------------------------------------------------------
			fndIdx <- which(fStepMtx.w1[aIdx,]>1)
			if(0<length(fndIdx)) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"fStepW1:001" )
			fndIdx <- which(fStepMtx.w1[aIdx,]==1)
			if(2<length(fndIdx)) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"fStepW1:011" )
			if(1==length(fndIdx)) {
				banCol <- c("nextRebNum","basic","nextColVal_5")
				if( any(banCol%in%names(fndIdx)) ) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"fStepW1:021" )
			}
		# # -[fStepMtx.w2]-----------------------------------------------------------
			# fndIdx <- which(fStepMtx.w1[aIdx,]>1)
			# if(0<length(fndIdx)) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"fStepW2:001" )
			fndIdx <- which(fStepMtx.w1[aIdx,]==1)
			if(3<length(fndIdx)) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"fStepW2:011" )
			if(1==length(fndIdx)) {
				banCol <- c("nextZW","nextColVal_2","nextQuo10","nextFStepBin")
				if( any(banCol%in%names(fndIdx)) ) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"fStepW2:021" )
			}


		# # -[raw.w1.w2]-----------------------------------------------------------
			cntVal <- rawMtx[aIdx,] + cStepMtx.w1[aIdx,] + cStepMtx.w2[aIdx,] + fStepMtx.w1[aIdx,] + fStepMtx.w2[aIdx,]
			if(0==sum(cntVal)) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"raw.w1.w2:000" )
	}
	# debug
	# table(do.call(c,fltLst,))	;fltCnt <- sapply(fltLst,length)	;table(fltCnt)	;dbgIdx <- which(fltCnt>0)
	# dbgIdx<-which(sapply(fltLst,function(p){any(p=="fStep021")}))		;aIdx <- dbgIdx[1]

	fltCnt <- sapply(fltLst,length)
	allIdxF <- allIdxF[fltCnt==0]
	# fltFlag <- rep( FALSE ,nrow(cntMtxLst[[1]]) )	;names(fltFlag) <- allIdxF
	return( list( allIdxF=allIdxF ,fltCnt=fltCnt ) )

} # ff0.fltCntMtx()

#	static과 custom 분리필요
ff0.fltScoreMtx.static		<- function( ccObjLst ,allIdxF ){
	allIdxF.len <- length( allIdxF )
	scoreMtxLst <- lapply( ccObjLst ,function(p){ p$cccObj$scoreMtx })
	phName <- attributes(scoreMtxLst)$names
	colName <- colnames(scoreMtxLst[[1]])

	scoreMtx <- matrix( NA, nrow=length(phName), ncol=length(colName) )
	rownames(scoreMtx) <- phName	;colnames(scoreMtx)=colName
	scoreMtx.evt <- scoreMtx

	fltLst <- vector( "list",allIdxF.len )
	for( aIdx in seq_len(allIdxF.len) ){
		scoreMtx[,] <- 0		;scoreMtx.evt[,] <- 0
		for( phIdx in phName ){
			scoreMtx[phIdx,] <- scoreMtxLst[[phIdx]][aIdx,]
			#	scoreMtx.evt[phIdx,] <- scoreMtx[phIdx,] >= c( 1,1,1,  1,1,1,  1, 2,2,2 )	# 
			#	reb nbor spanM quoAll quoPtn zw remH0 remH1 cStep2 cStep3 w1CStep.cnt w1FStep.cnt w1CStep.matLen w1FStep.matLen
		}

		evtCnt <- apply( scoreMtx.evt ,1 ,sum )

		colSum <- apply( scoreMtx ,2 ,sum )
		workCol <- c("nbor","spanM","quoAll","quoPtn","zw","remH0","remH1","cStep2","cStep3","w1CStep.cnt","w1FStep.cnt")
		if( 0==sum(0<colSum[workCol]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"voidCol.00") }
		workCol <- c("nbor","spanM","quoAll","quoPtn","zw","remH0","remH1","cStep2","cStep3","w1CStep.cnt","w1FStep.cnt")
		if( 4<sum(1==colSum[workCol]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"voidCol.01") }
		workCol <- c("nbor","spanM","quoAll","quoPtn","zw")
		if( 3<sum(1==colSum[workCol]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"voidCol.01.a") }
		workCol <- c("remH0","remH1","cStep2","cStep3","w1CStep.cnt","w1FStep.cnt")
		if( 3<sum(1==colSum[workCol]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"voidCol.01.b") }


		if( 4<colSum["nbor"]	){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"nbor.00") }
		if( 4<colSum["spanM"]	){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"nbor.00") }
		if( 5<colSum["quoAll"]	){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"quoAll.00") }
		if( 3<colSum["quoPtn"]	){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"quoPtn.00") }
		if( 2<colSum["zw"]		){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"zw.00") }
		if( 1<colSum["remH0"]	){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"remH0.00") }
		if( 1<colSum["remH1"]	){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"remH1.00") }
		if( 1<sum(colSum[c("remH0","remH1")]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"remHn.00") }
		if( 2<colSum["cStep2"]	){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"cStep2.00") }
		if( 1<colSum["cStep3"]	){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"cStep3.00") }
		if( 2<colSum["w1CStep.cnt"] ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"w1CStep.cnt.00") }
		if( 4<colSum["w1FStep.cnt"] ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"w1FStep.cnt.00") }

		ceilThld <- c( 4,4, 4,3, 2, 1,1, 2,1, 2,4 )
		names(ceilThld) <- c("nbor","spanM","quoAll","quoPtn","zw","remH0","remH1","cStep2","cStep3","w1CStep.cnt","w1FStep.cnt")
		if( 3<sum(ceilThld<=colSum[names(ceilThld)]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"ceilThld.00") }

	}
	# debug
	# table(do.call(c,fltLst,))	;fltCnt <- sapply(fltLst,length)	;table(fltCnt)	;dbgIdx <- which(fltCnt>0)
	# dbgIdx<-which(sapply(fltLst,function(p){any(p=="remHn.00")}))		;aIdx <- dbgIdx[1]

	fltCnt <- sapply(fltLst,length)
	allIdxF <- allIdxF[fltCnt==0]
	rObj <- list( allIdxF=allIdxF ,fltCnt=fltCnt ,fltLst=fltLst )

	return( rObj )

} # ff0.fltScoreMtx.static()

#	static과 custom 분리필요
ff0.fltScoreMtx2.static	<- function( ccObjLst ,allIdxF ){
	
	allIdxF.len <- length( allIdxF )
	scoreMtx2Lst <- lapply( ccObjLst ,function(p){ p$cccObj$scoreMtx2 })
	phName <- attributes(scoreMtx2Lst)$names
	colName <- colnames(scoreMtx2Lst[[1]])

	scoreMtx2 <- matrix( NA, nrow=length(phName), ncol=length(colName) )
	rownames(scoreMtx2) <- phName	;colnames(scoreMtx2)=colName
	scoreMtx2.evt <- scoreMtx2

	fltLst <- vector( "list",allIdxF.len )
	for( aIdx in seq_len(allIdxF.len) ){
		scoreMtx2[,] <- 0		;scoreMtx2.evt[,] <- 0
		for( phIdx in phName ){
			scoreMtx2[phIdx,] <- scoreMtx2Lst[[phIdx]][aIdx,]
			scoreMtx2.evt[phIdx,] <- scoreMtx2[phIdx,] >= c( 3, 2, 2, 1, 1, 100,100, 2, 2 )	# rebL.cnt,rebR.cnt은 100으로
			#	"rebV" "rebC" "rebC2" "rebL" "rebR" "rebL.cnt" "rebR.cnt" "inc.raw" "inc.cStep"
		}

		evtCnt <- apply( scoreMtx2.evt ,1 ,sum )
		if( any(3<=evtCnt) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0010") }
		if( 5<sum(evtCnt) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0020") }

		colSum <- apply( scoreMtx2 ,2 ,sum )
		workCol <- c("inc.raw", "inc.cStep")
		if( 1<sum(0==colSum[workCol]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"voidCol.00") }

		colSum <- apply( scoreMtx2 ,2 ,function(cv){any(cv>1)} )
		workCol <- c("rebC","rebC2","inc.raw", "inc.cStep")
		if( 2<sum(colSum[workCol]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"evtCol.00") }

		colSum <- apply( scoreMtx2 ,2 ,function(cv){any(cv>1)} )
		evtCnt <- rep( 0, 6 )
			evtCnt[1] <- sum(scoreMtx2[,"rebV"]>1)
			if( any(scoreMtx2[,"rebV"]>=3) ) evtCnt[1] <- 100
			evtCnt[2] <- sum(scoreMtx2[,"rebC"]>1)
			evtCnt[3] <- sum(scoreMtx2[,"rebC2"]>1)
			evtCnt[4] <- sum(scoreMtx2[,c("rebL","rebR")]>0)
			evtCnt[5] <- sum(scoreMtx2[,"inc.raw"]>1)
			evtCnt[6] <- sum(scoreMtx2[,"inc.cStep"]>1)
		if( 3<sum(evtCnt>=c(3,1,1,1,1,1)) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"evtCol.01") }
		if( 0==sum(evtCnt>=c(3,1,1,1,1,1)) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"evtCol.01.a") }


		# # -[rebV]--------------------------------------------------------------
		if( 7>sum(scoreMtx2[,"rebV"]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0030") }
		flg <- scoreMtx2[,"rebV"] >= 3
			if( 2 < sum(flg) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0040") }
			# banArea <- c("basic","nextZW","nextQuo10","nextBin","nextRebNum","nextCStepBin","nextFStepBin")
			# if( any(flg[banArea]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0050") }
		flg <- scoreMtx2[,"rebV"] == 2
			if( 5 < sum(flg) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0060") }

		# # -[rebC]--------------------------------------------------------------
		if( 0==sum(scoreMtx2[,"rebC"]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0070") }
		flg <- scoreMtx2[,"rebC"] >= 2
			if( 2 < sum(flg) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0080") }
			# banArea <- c("basic","nextZW","nextQuo10","nextBin","nextRebNum","nextCStepBin","nextFStepBin") # 검토필요.
			# if( any(flg[banArea]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0090") }
		# # -[rebC]--------------------------------------------------------------
		if( 0==sum(scoreMtx2[,"rebC2"]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0070.2") }
		flg <- scoreMtx2[,"rebC2"] >= 2
			if( 2 < sum(flg) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0080.2") }
			# banArea <- c("basic","nextZW","nextQuo10","nextBin","nextRebNum","nextCStepBin","nextFStepBin")
			# if( any(flg[banArea]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0090.2") }

		# # -[rebL]--------------------------------------------------------------
		if( 1<sum(scoreMtx2[,"rebL"]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0100") }

		# # -[rebR]--------------------------------------------------------------
		if( 1<sum(scoreMtx2[,"rebR"]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0110") }

		# # -[inc.raw]-----------------------------------------------------------
		if( 6<sum(scoreMtx2[,"inc.raw"]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0120") }
		flg <- scoreMtx2[,"inc.raw"] >= 2
			banArea <- c("basic","nextZW","nextQuo10","nextBin","nextRebNum","nextCStepBin","nextFStepBin")
			if( any(flg[banArea]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0130") }

		# # -[inc.cStep]---------------------------------------------------------
		if( 6<sum(scoreMtx2[,"inc.cStep"]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0140") }

	}
	# debug
	# table(do.call(c,fltLst,))	;fltCnt <- sapply(fltLst,length)	;table(fltCnt)	;dbgIdx <- which(fltCnt>0)
	# dbgIdx<-which(sapply(fltLst,function(p){any(p=="0130")}))		;aIdx <- dbgIdx[1]

	fltCnt <- sapply(fltLst,length)
	allIdxF <- allIdxF[fltCnt==0]
	rObj <- list( allIdxF=allIdxF ,fltCnt=fltCnt ,fltLst=fltLst )

	return( rObj )

} # ff0.fltScoreMtx2.static()

ff0.fltScoreMtx3.static	<- function( ccObjLst ,allIdxF ){

	allIdxF.len <- length( allIdxF )
	scoreMtx3Lst <- lapply( ccObjLst ,function(p){ p$cccObj$scoreMtx3 })
	phName <- attributes(scoreMtx3Lst)$names
	colName <- colnames(scoreMtx3Lst[[1]])

	scoreMtx3 <- matrix( NA, nrow=length(phName), ncol=length(colName) )
	rownames(scoreMtx3) <- phName	;colnames(scoreMtx3)=colName
	scoreMtx3.evt <- scoreMtx3

	fltLst <- vector( "list",allIdxF.len )
	for( aIdx in seq_len(allIdxF.len) ){
		scoreMtx3[,] <- 0		;scoreMtx3.evt[,] <- 0
		for( phIdx in phName ){
			scoreMtx3[phIdx,] <- scoreMtx3Lst[[phIdx]][aIdx,]
			scoreMtx3.evt[phIdx,] <- scoreMtx3[phIdx,] >= c( 1, 1, 2, 2, 2, 2 )	# 
			#	rebPtn.1 rebPtn.n rebC.C1 rebC.F1 rebC.C2 rebC.F2
		}

		evtCnt <- apply( scoreMtx3.evt ,1 ,sum )
		# if( any(3<=evtCnt) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0010") }
		# if( 5<sum(evtCnt) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0020") }

		colSum <- apply( scoreMtx3 ,2 ,sum )
		workCol <- c("rebPtn.1","rebC.C1","rebC.F1","rebC.C2","rebC.F2")
		if( 2<sum(0==colSum[workCol]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"voidCol.00") }

		colSum <- apply( scoreMtx3 ,2 ,function(cv){any(cv>1)} )
		workCol <- c("rebC.C1","rebC.F1","rebC.C2","rebC.F2")
		if( 2<sum(colSum[workCol]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"evtCol.00") }

		colSum <- apply( scoreMtx3 ,2 ,function(cv){sum(cv==1)} )
		workCol <- c("rebC.C1","rebC.F1","rebC.C2","rebC.F2")
		if(  6>sum(colSum[workCol]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"evtCol.01") }
		if(  2>sum(colSum[c("rebC.C1","rebC.F1")]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"evtCol.01.a") }
		if(  2>sum(colSum[c("rebC.C2","rebC.F2")]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"evtCol.01.b") }
		if( 17<sum(colSum[workCol]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"evtCol.02") }

		colSum <- apply( scoreMtx3 ,2 ,function(cv){sum(cv>=2)} )
		workCol <- c("rebC.C1","rebC.F1","rebC.C2","rebC.F2")
		if(  3<sum(colSum[workCol]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"evtCol.10") }

		# -[rebPtn.1]--------------------------------------------------------------
		if( 2<sum(scoreMtx3[,"rebPtn.1"]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"rebPtn.1.00") }

		# # -[rebC.X1 : rebC.C1, rebC.F1]--------------------------------------------------------------
		workCol <- c("rebC.C1","rebC.F1")
		if( 2>sum(scoreMtx3[,"rebC.C1"]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"rebC.X1.00") }

		# # -[rebC.X2 : rebC.C2, rebC.F2]--------------------------------------------------------------
		workCol <- c("rebC.C2","rebC.F2")
		if( 2>sum(scoreMtx3[,"rebC.C2"]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"rebC.X2.00") }

	}
	# debug
	# table(do.call(c,fltLst,))	;fltCnt <- sapply(fltLst,length)	;table(fltCnt)	;dbgIdx <- which(fltCnt>0)
	# dbgIdx<-which(sapply(fltLst,function(p){any(p=="0130")}))		;aIdx <- dbgIdx[1]

	fltCnt <- sapply(fltLst,length)
	allIdxF <- allIdxF[fltCnt==0]
	rObj <- list( allIdxF=allIdxF ,fltCnt=fltCnt ,fltLst=fltLst )

	return( rObj )

} # ff0.fltScoreMtx3.static()

ff0.fltScoreMtx4.static	<- function( ccObjLst ,allIdxF ){

	allIdxF.len <- length( allIdxF )
	scoreMtx4Lst <- lapply( ccObjLst ,function(p){ p$cccObj$scoreMtx4 })
	phName <- attributes(scoreMtx4Lst)$names
	colName <- colnames(scoreMtx4Lst[[1]])

	scoreMtx4 <- matrix( NA, nrow=length(phName), ncol=length(colName) )
	rownames(scoreMtx4) <- phName	;colnames(scoreMtx4)=colName
	scoreMtx4.evt <- scoreMtx4

	fltLst <- vector( "list",allIdxF.len )
	for( aIdx in seq_len(allIdxF.len) ){
		scoreMtx4[,] <- 0		;scoreMtx4.evt[,] <- 0
		for( phIdx in phName ){
			scoreMtx4[phIdx,] <- scoreMtx4Lst[[phIdx]][aIdx,]
			scoreMtx4.evt[phIdx,] <- scoreMtx4[phIdx,] >= c( 1,1,1,  1,1,1,  1, 2,2,2 )	# 
			#	incRaw3     incC3     incF3   incRaw2     incC2     incF2     (1,6) nextVal.r nextVal.c nextVal.f 
		}

		evtCnt <- apply( scoreMtx4.evt ,1 ,sum )

		colSum <- apply( scoreMtx4 ,2 ,sum )
		workCol <- c("incRaw2","incC2","incF2","nextVal.r","nextVal.c","nextVal.f")
		if( 2<sum(0==colSum[workCol]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"voidCol.00") }

		colSum <- apply( scoreMtx4 ,2 ,sum )
		workCol <- c("incC2","incF2","(1,6)","nextVal.r","nextVal.c","nextVal.f")
		if( 2<sum(1==colSum[workCol]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"voidCol.01") }

		colSum <- apply( scoreMtx4 ,2 ,sum )
		workCol <- c("incRaw2","incC2","incF2","(1,6)","nextVal.r","nextVal.c","nextVal.f")
		if( 1>sum(1==colSum[workCol]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"voidCol.01a") }

		colSum <- apply( scoreMtx4 ,2 ,function(cv){any(cv>1)} )
		workCol <- c("nextVal.r","nextVal.c","nextVal.f")
		if( 2<sum(colSum[workCol]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"evtCol.10") }

		colSum <- apply( scoreMtx4 ,2 ,function(cv){sum(cv>=2)} )
		workCol <- c("nextVal.r","nextVal.c","nextVal.f")
		if( 3<sum(colSum[workCol]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"evtCol.11.gt") }

		colSum <- apply( scoreMtx4 ,2 ,function(cv){sum(cv==1)} )
		workCol <- c("nextVal.r","nextVal.c","nextVal.f")
		if(  4>sum(colSum[workCol]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"evtCol.11.lt") }
		if( 17<sum(colSum[workCol]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"evtCol.11.gt") }


		# # -[incX2 : incRaw2 ,incC2 ,incF2]--------------------------------------------------------------
		workCol <- c("incRaw2","incC2","incF2")
		if( 1>sum(scoreMtx4[,workCol]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"incX2.00") }

	}
	# debug
	# table(do.call(c,fltLst,))	;fltCnt <- sapply(fltLst,length)	;table(fltCnt)	;dbgIdx <- which(fltCnt>0)
	# dbgIdx<-which(sapply(fltLst,function(p){any(p=="voidCol.01")}))		;aIdx <- dbgIdx[1]

	fltCnt <- sapply(fltLst,length)
	allIdxF <- allIdxF[fltCnt==0]
	rObj <- list( allIdxF=allIdxF ,fltCnt=fltCnt ,fltLst=fltLst )

	return( rObj )

} # ff0.fltScoreMtx4.static()

ff0.fltCStepValMtx.static	<- function( ccObjLst ,allIdxF ){

	allIdxF.len <- length( allIdxF )
	cStepValMtxLst <- lapply( ccObjLst ,function(p){ p$cccObj$cStepValMtx })
	phName <- attributes(cStepValMtxLst)$names
	colName <- colnames(cStepValMtxLst[[1]])

	cStepValMtx <- matrix( NA, nrow=length(phName), ncol=length(colName) )
	rownames(cStepValMtx) <- phName	;colnames(cStepValMtx)=colName
	cStepValMtx.evt <- cStepValMtx

	fltLst <- vector( "list",allIdxF.len )
	for( aIdx in seq_len(allIdxF.len) ){
		cStepValMtx[,] <- 0		;cStepValMtx.evt[,] <- 0
		for( phIdx in phName ){
			cStepValMtx[phIdx,] <- cStepValMtxLst[[phIdx]][aIdx,]
			# cStepValMtx.evt[phIdx,] <- cStepValMtx[phIdx,] >= c( 1,1,1,  1,1,1,  1, 2,2,2 )	# 
			#	c31 c32 c33 c34 	c21 c22 c23 c24 c25 	max2 min2
		}

		evtCnt <- apply( cStepValMtx.evt ,1 ,sum )

		colSum <- apply( cStepValMtx ,2 ,sum )
		workCol <- c("c21","c22","c23","c24","c25")
		# if( 2<sum(0==colSum[workCol]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"voidCol.00") }

		colSum <- apply( cStepValMtx ,2 ,sum )
		workCol <- c("c21","c22","c23","c24","c25")
		if( 2<sum(1==colSum[workCol]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"voidCol.01") }

		colSum <- apply( cStepValMtx ,2 ,function(cv){any(cv>1)} )
		workCol <- c("c21","c22","c23","c24","c25")
		if( 3<sum(0<colSum[workCol]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"evtCol.00") }

	}
	# debug
	# table(do.call(c,fltLst,))	;fltCnt <- sapply(fltLst,length)	;table(fltCnt)	;dbgIdx <- which(fltCnt>0)
	# dbgIdx<-which(sapply(fltLst,function(p){any(p=="voidCol.01")}))		;aIdx <- dbgIdx[1]

	fltCnt <- sapply(fltLst,length)
	allIdxF <- allIdxF[fltCnt==0]
	rObj <- list( allIdxF=allIdxF ,fltCnt=fltCnt ,fltLst=fltLst )

	return( rObj )

} # ff0.fltCStepValMtx.static()


ff0.noPastRebound.byFilter <- function( ccObjLst, allIdxF, rstObj ){
	#	load("./save/stdZoidFltRst/Obj_rstObj.goldRstSpan.save")
	mtxLst <- rstObj$getMtx.byFilter()	# mtxLst[["cntMtxLst"]]$basic

	allIdxF.len <- length(allIdxF)

	redLst		<- vector("list",allIdxF.len)
	yellowLst	<- vector("list",allIdxF.len)

	# cntMtx --------------------------------------
	tableName <- "cntMtx"	;lstName <- sprintf("%sLst",tableName)
	fltNames <- c("raw","rem","cStep","fStep","raw.w1","cStep.w1","cStep.w2","fStep.w1","fStep.w2")
	colEH    <- c(    2,    3,      2,      2,       1,         2,         2,         2,        2 )
	names(colEH) <- fltNames
	for( fIdx in fltNames ){
		lst <- lapply( ccObjLst,function(p){p$cntMtx[,fIdx]})
		aMtx <- do.call( cbind ,lst )

		# event
		flagMtx <- mtxLst[[lstName]][[fIdx]] >= colEH[fIdx]

		# QQE work		
	}

	# scoreMtx --------------------------------------
	# scoreMtx2 --------------------------------------
	# scoreMtx3 --------------------------------------
	# scoreMtx4 --------------------------------------
	# cStepValMtx --------------------------------------


	rObj <- list( redCnt=sapply(redLst,length) ,yellowCnt=sapply(yellowLst,length) )
	return( rObj )

} # ff0.noPastRebound.byFilter()

if( FALSE ){	#  "./save/stdZoidFltRst/Obj_rstObj.goldRstSpan.save"

	load("./save/stdZoidFltRst/Obj_rstObj.goldRstSpan.save")
	mtxLst <- rstObj$getMtx.byPhase()	# mtxLst[["cntMtxLst"]]$basic


}	# Obj_rstObj.goldRstSpan.save
