#   경고! 코딩만 하고 동작테스트는 아직 안해봤다. ㅋㅋㅋㅋ
bN.get_testData.grp <- function( testSpan ,gEnv ,allIdxLst ,fRstLst ,tgt.scMtx=tgt.scMtx){
    #   testData.grp <- B.get_testData.grp( testSpan ,gEnv ,allIdxLst ,fRstLst ,tgt.scMtx=tgt.scMtx) 

    tStmp <- Sys.time()
    sfExport("tgt.scMtx")   ;sfExport("get.scoreMtx.grp")
    sfExport("prllLog")
    sfExport("gEnv")    ;sfExport("fRstLst")    ;sfExport("allIdxLst")

    resultLst <- sfLapply(testSpan,function(curHIdx){
        tStmp.prll <- Sys.time()

        wLastH <-curHIdx-1
        wLastSpan <- 1:which(names(fRstLst)==wLastH)

        # ------------------------------------------------------------------------
        # curHMtxLst.grp
        gEnv.w <- gEnv              ;gEnv.w$zhF <- gEnv$zhF[1:wLastH,]
        allIdxLst.w <- allIdxLst    ;allIdxLst.w$stdFiltedCnt <- allIdxLst$stdFiltedCnt[wLastSpan]
                                    allIdxLst.w$infoMtx <- allIdxLst$infoMtx[wLastSpan,]
        fRstLst.w <- fRstLst[wLastSpan]

        curHMtxLst_bN <- B.makeHMtxLst( gEnv.w, allIdxLst.w, fRstLst.w, tgt.scMtx )

        # ------------------------------------------------------------------------
        # stdIdx.grp
        stdZoid <- gEnv$zhF[curHIdx,]
        stdIdx <- k.getIdx_AllZoidMtx( gEnv, stdZoid )

        tDiff <- Sys.time() - tStmp.prll
        prllLog$fLogStr(sprintf("    bN.get_testData.grp - hIdx:%d finished %.1f%s",curHIdx,tDiff,units(tDiff)))

        rObj <- list( hIdx=curHIdx ,stdIdx=stdIdx ,hMtxLst_bN=curHMtxLst_bN ) 

        if( get.scoreMtx.grp ){
            stdMI.grp   <- bN.getStdMILst( gEnv.w ,fRstLst.w )
            filter.grp  <- bN.getFilter.grp( stdMI.grp ,tgt.scMtx=tgt.scMtx )
            rObj$scoreMtx.grp <- bN.getScoreMtx.grp( matrix(stdZoid,nrow=1) ,filter.grp ,tgt.scMtx=tgt.scMtx )
        }

        return( rObj )
    })
    names(resultLst) <- sapply(resultLst,function(p){ p$hIdx })

    curHMtxLst.grp <- lapply(resultLst,function(p){ p$hMtxLst })
    curHMtxLst_bS.grp <- lapply(resultLst,function(p){ p$hMtxLst_bS })
    stdIdx.grp <- lapply(resultLst,function(p){ p$stdIdx })

    tDiff <- Sys.time() - tStmp
    cat(sprintf("time : %.1f,%s   \n",tDiff,units(tDiff)))

    rLst <- list(curHMtxLst.grp=curHMtxLst.grp ,stdIdx.grp=stdIdx.grp ,curHMtxLst_bS.grp=curHMtxLst_bS.grp )
    if( get.scoreMtx.grp ){
        rLst$scoreMtxLst.grp <- lapply(resultLst,function(p){p$scoreMtx.grp})
    }

    return( rLst )

}

bN.getFilter.grp <- function( stdMI.grp ,tgt.scMtx=NULL ){

	getMtxObjLst <- function( stdMIObj ){
		mtxObjLst <- list()
		if( is.null(tgt.scMtx) || ("score1" %in%tgt.scMtx ) ){
			mtxObjLst[[1+length(mtxObjLst)]] <- bFMtx.score1( stdMIObj )
		}
		if( is.null(tgt.scMtx) || ("score2" %in%tgt.scMtx ) ){
			mtxObjLst[[1+length(mtxObjLst)]] <- bFMtx.score2( stdMIObj )
		}
		if( is.null(tgt.scMtx) || ("score3" %in%tgt.scMtx ) ){
			mtxObjLst[[1+length(mtxObjLst)]] <- bFMtx.score3( stdMIObj )
		}
		if( is.null(tgt.scMtx) || ("score4" %in%tgt.scMtx ) ){
			mtxObjLst[[1+length(mtxObjLst)]] <- bFMtx.score4( stdMIObj )
		}
		if( is.null(tgt.scMtx) || ("score5" %in%tgt.scMtx ) ){
			mtxObjLst[[1+length(mtxObjLst)]] <- bFMtx.score5( stdMIObj )
		}
		if( is.null(tgt.scMtx) || ("score6" %in%tgt.scMtx ) ){
			mtxObjLst[[1+length(mtxObjLst)]] <- bFMtx.score6( stdMIObj )
		}
		if( is.null(tgt.scMtx) || ("score7" %in%tgt.scMtx ) ){
			mtxObjLst[[1+length(mtxObjLst)]] <- bFMtx.score7( stdMIObj )
		}
		if( is.null(tgt.scMtx) || ("score8" %in%tgt.scMtx ) ){
			mtxObjLst[[1+length(mtxObjLst)]] <- bFMtx.score8( stdMIObj )
		}
		if( is.null(tgt.scMtx) || ("score9" %in%tgt.scMtx ) ){
			mtxObjLst[[1+length(mtxObjLst)]] <- bFMtx.score9( stdMIObj )
		}
		if( is.null(tgt.scMtx) || ("scoreA" %in%tgt.scMtx ) ){
			mtxObjLst[[1+length(mtxObjLst)]] <- bFMtx.scoreA( stdMIObj )
		}
		if( is.null(tgt.scMtx) || ("scoreB" %in%tgt.scMtx ) ){
			mtxObjLst[[1+length(mtxObjLst)]] <- bFMtx.scoreB( stdMIObj )
		}
		if( is.null(tgt.scMtx) || ("scoreC" %in%tgt.scMtx ) ){
			mtxObjLst[[1+length(mtxObjLst)]] <- bFMtx.scoreC( stdMIObj )
		}
		if( is.null(tgt.scMtx) || ("scoreD" %in%tgt.scMtx ) ){
			mtxObjLst[[1+length(mtxObjLst)]] <- bFMtx.scoreD( stdMIObj )
		}
		if( is.null(tgt.scMtx) || ("scoreE" %in%tgt.scMtx ) ){
			mtxObjLst[[1+length(mtxObjLst)]] <- bFMtx.scoreE( stdMIObj )
		}
		if( is.null(tgt.scMtx) || ("scoreF" %in%tgt.scMtx ) ){
			mtxObjLst[[1+length(mtxObjLst)]] <- bFMtx.scoreF( stdMIObj )
		}

		if( is.null(tgt.scMtx) || ("scoreLAr13" %in%tgt.scMtx ) ){
			mtxObjLst[[1+length(mtxObjLst)]] <- bFMtx.scoreLAr13( stdMIObj )
		}
        if( is.null(tgt.scMtx) || ("scoreLAr24" %in%tgt.scMtx ) ){
			mtxObjLst[[1+length(mtxObjLst)]] <- bFMtx.scoreLAr24( stdMIObj )
		}
        if( is.null(tgt.scMtx) || ("scoreLVr13" %in%tgt.scMtx ) ){
			mtxObjLst[[1+length(mtxObjLst)]] <- bFMtx.scoreLVr13( stdMIObj )
		}
        if( is.null(tgt.scMtx) || ("scoreLVr24" %in%tgt.scMtx ) ){
			mtxObjLst[[1+length(mtxObjLst)]] <- bFMtx.scoreLVr24( stdMIObj )
		}
        if( is.null(tgt.scMtx) || ("scoreLAe13" %in%tgt.scMtx ) ){
			mtxObjLst[[1+length(mtxObjLst)]] <- bFMtx.scoreLAe13( stdMIObj )
		}
        if( is.null(tgt.scMtx) || ("scoreLAe24" %in%tgt.scMtx ) ){
			mtxObjLst[[1+length(mtxObjLst)]] <- bFMtx.scoreLAe24( stdMIObj )
		}
        if( is.null(tgt.scMtx) || ("scoreLVe13" %in%tgt.scMtx ) ){
			mtxObjLst[[1+length(mtxObjLst)]] <- bFMtx.scoreLVe13( stdMIObj )
		}
        if( is.null(tgt.scMtx) || ("scoreLVe24" %in%tgt.scMtx ) ){
			mtxObjLst[[1+length(mtxObjLst)]] <- bFMtx.scoreLVe24( stdMIObj )
		}
        if( is.null(tgt.scMtx) || ("scoreLAc13" %in%tgt.scMtx ) ){
			mtxObjLst[[1+length(mtxObjLst)]] <- bFMtx.scoreLAc13( stdMIObj )
		}
        if( is.null(tgt.scMtx) || ("scoreLAc24" %in%tgt.scMtx ) ){
			mtxObjLst[[1+length(mtxObjLst)]] <- bFMtx.scoreLAc24( stdMIObj )
		}
        if( is.null(tgt.scMtx) || ("scoreLVc13" %in%tgt.scMtx ) ){
			mtxObjLst[[1+length(mtxObjLst)]] <- bFMtx.scoreLVc13( stdMIObj )
		}
        if( is.null(tgt.scMtx) || ("scoreLVc24" %in%tgt.scMtx ) ){
			mtxObjLst[[1+length(mtxObjLst)]] <- bFMtx.scoreLVc24( stdMIObj )
		}
        if( is.null(tgt.scMtx) || ("scoreLAf13" %in%tgt.scMtx ) ){
			mtxObjLst[[1+length(mtxObjLst)]] <- bFMtx.scoreLAf13( stdMIObj )
		}
        if( is.null(tgt.scMtx) || ("scoreLAf24" %in%tgt.scMtx ) ){
			mtxObjLst[[1+length(mtxObjLst)]] <- bFMtx.scoreLAf24( stdMIObj )
		}
        if( is.null(tgt.scMtx) || ("scoreLVf13" %in%tgt.scMtx ) ){
			mtxObjLst[[1+length(mtxObjLst)]] <- bFMtx.scoreLVf13( stdMIObj )
		}
        if( is.null(tgt.scMtx) || ("scoreLVf24" %in%tgt.scMtx ) ){
			mtxObjLst[[1+length(mtxObjLst)]] <- bFMtx.scoreLVf24( stdMIObj )
		}

		names(mtxObjLst) <- sapply(mtxObjLst,function(p){p$idStr})
		return( mtxObjLst )
	}

	rObj <- list()
	rObj$basic <- lapply( stdMI.grp$basic ,getMtxObjLst )
	rObj$bDup <- list()         ;rObj$mf <- list()

	return( rObj )
}

bN.getScoreMtx.grp <- function( aZoidMtx ,filter.grp ,makeInfoStr=F ,tgt.scMtx=NULL ){
	#  aZoidMtx <- matrix(stdZoid,nrow=1) ,tgt.scMtx=NULL

	rObj <- list( basic=list() ,bDup=list() ,mf=list() )

	for( nIdx in names(filter.grp$basic) ){
		scoreMtxLst <- list()
		scMtxName <- names(filter.grp$basic[[nIdx]])
		if( !is.null(tgt.scMtx) ){
			scMtxName <- intersect( tgt.scMtx ,scMtxName )
		}

		for( nIdx.s in scMtxName ){
			filterObj <- filter.grp$basic[[nIdx]][[nIdx.s]]
			scoreMtxObj <- filterObj$fMtxObj( aZoidMtx ,makeInfoStr=makeInfoStr )

			scoreMtxLst[[nIdx.s]] <- scoreMtxObj
		}
		rObj$basic[[nIdx]] <- scoreMtxLst
	}

	scMtxName <- names(filter.grp$bDup)
	if( !is.null(tgt.scMtx) ){
		scMtxName <- intersect( tgt.scMtx ,scMtxName )
	}
	for( nIdx.s in scMtxName ){
		filterObj <- filter.grp$bDup[[nIdx.s]]
		scoreMtxObj <- filterObj$fMtxObj( aZoidMtx ,makeInfoStr=is.null(cutter.grp) )

		rObj$bDup[[nIdx.s]] <- scoreMtxObj
	}

	scMtxName <- names(filter.grp$mf)
	if( !is.null(tgt.scMtx) ){
		scMtxName <- intersect( tgt.scMtx ,scMtxName )
	}
	for( nIdx.s in scMtxName ){
		filterObj <- filter.grp$mf[[nIdx.s]]
		scoreMtxObj <- filterObj$fMtxObj( aZoidMtx ,makeInfoStr=is.null(cutter.grp) )
		rObj$mf[[nIdx.s]] <- scoreMtxObj
	}

	return(rObj)

} # getScoreMtx.grp()

bN.getFCustGrp <- function( hMtxLst ,tgt.scMtx ){
    # 참고코드 : cut.grp <- bFCust.getFCustGrp( hMtxLst ,tgt.scMtx )
    # bScr은 없으나 기존 코드 호환을 위해 남겨둔다.

    rObj <- list(   sfcHLst = hMtxLst$sfcHLst
                    ,mtxInfoLst = hMtxLst$mtxInfoLst
					,mtxInfoLst.bScr = hMtxLst$mtxInfoLst.bScr
                    ,phaseName = hMtxLst$phaseName
    )

    if( !is.null(tgt.scMtx) ){
        availMtx <- intersect( tgt.scMtx ,names(rObj$mtxInfoLst) )
        rObj$mtxInfoLst <- rObj$mtxInfoLst[availMtx]

        availMtx <- intersect( tgt.scMtx ,names(rObj$mtxInfoLst.bScr) )
        rObj$mtxInfoLst.bScr <- rObj$mtxInfoLst.bScr[availMtx]
    }

    # 가용 bNMtxLstMFltLst 추출.
    #   tgt.scMtx가 일부 mName만 갖고있을 수 있으므로.
	cutterLst <- list()         ;cutterExtLst <- list()         ;cutterExtMLst <- list()
	cutterLst.bScr <- list()    ;cutterExtLst.bScr <- list()    # ;cutterExtMLst.bScr <- list()
	for( hName in names(rObj$sfcHLst) ){	    # hName <- names(rObj$sfcHLst)[1]

        # cutterLst, cutterExtLst -----------------------------------------------------
        mLst <- list()  ;mExtLst <- list()  #   cutterLst
        for( mName in names(rObj$mtxInfoLst) ){	# mName <- names(rObj$mtxInfoLst)[1]
            # <stdCut>
            stdCut <- list()    ;stdCutExt <- list()
            for( pName in rObj$phaseName ){     # pName <- rObj$phaseName[1]
                scoreMtxObj <- B.HMtxLst_getMtxLst( hMtxLst , hName ,mName ,pName )
                # stdCut[[pName]] <- FCust_stdCut.rawRow( hName ,mName ,pName ,scoreMtxObj$scoreMtx )
                stdCut[[pName]] <- bN.stdCut_rawRow( hName ,mName ,pName ,scoreMtxObj$scoreMtx )

                if( is.null(bNMtxExtFltLst[[mName]]) ){   stdCutExt[[pName]] <- list()
                } else {
                    fltLst <- list()
                    for( nIdx in names(bNMtxExtFltLst[[mName]]) ){
                        # bNMtxExtFltLst[[mName]][[nIdx]] 는 bN.stdCutExt_rawRow() 내부에서 이용됨.
                        # fltLst[[nIdx]] <- FCust_stdCutExt.rawRow( hName ,mName ,pName ,scoreMtxObj$scoreMtx ,fltName=nIdx )
                        fltLst[[nIdx]] <- bN.stdCutExt_rawRow( hName ,mName ,pName ,scoreMtxObj$scoreMtx ,fltName=nIdx )
                    }
                    stdCutExt[[pName]] <- fltLst
                }
            }

            # <fColCut>
            fColCut <- list()   # preserve

            # <hIdxCut>
			hIdxObj <- B.getHMtxLst_byHIdx( hMtxLst )
            # hIdxCut <- FCust_stdCut.hIdx( hName ,mName ,mtxLst=hIdxObj[[hName]][[mName]] )
            hIdxCut <- bN.stdCut_hIdx( hName ,mName ,mtxLst=hIdxObj[[hName]][[mName]] )

            mLst[[mName]] <- list( stdCut=stdCut ,fColCut=fColCut ,hIdxCut=hIdxCut )
            mExtLst[[mName]] <- list( stdCut=stdCutExt )
        }
        cutterLst[[hName]] <- mLst
        cutterExtLst[[hName]] <- mExtLst

        # cutterExtMLst ---------------------------------------------------------------
        if( TRUE ){
            pLst <- list()  # mName  단위가 없으므로 pName 단위가 됨.
            mScoreMtxLst <- list()
            for( pName in rObj$phaseName ){     # pName <- rObj$phaseName[1]
                curPCutLst <- list()            # cur Phase Cutter List
                mtxLst <- list()
                for( nIdx in names(bNMtxMFltLst) ){ # nIdx <- names(bFMtxMFltLst)[1]
                    mtxMaker <- bNMtxMFltLst[[nIdx]]( tgt.scMtx )
                    if( !mtxMaker$available )   next

                    scoreMtxLst <- hMtxLst$scoreMtxLst[[hName]][[pName]]
                    mtxLst[[nIdx]] <- mtxMaker$getScoreMtx( scoreMtxLst )
                    # curPCutLst[[nIdx]] <- FCust_stdCut.rawRow( hName ,mName=nIdx ,pName ,mtxLst[[nIdx]] )
                    curPCutLst[[nIdx]] <- bN.stdCut_rawRow( hName ,mName=nIdx ,pName ,mtxLst[[nIdx]] )
                }

                mScoreMtxLst[[pName]] <- mtxLst
                pLst[[pName]] <- curPCutLst
            }

            hIdxCut <- list()
            if( 0<length(mScoreMtxLst[[1]]) ){
                phNames <- names(mScoreMtxLst)
                hIdxName <- rownames(mScoreMtxLst[[1]][[1]])
                datSize <- length(hIdxName)

                for( mfName in names(mScoreMtxLst[[1]]) ){  # mfName <- names(mScoreMtxLst$basic)[1]
                    fColName <- colnames(mScoreMtxLst[[1]][[mfName]])
                    mtx <- matrix( 0 ,nrow=length(fColName) ,ncol=length(phNames) ,dimnames=list(fColName,phNames) )

                    mtxLst <- list( )
                    for( aIdx in seq_len(datSize) ){
                        mtx[,] <- 0
                        for( pName in phNames ){
                            mtx[,pName] <- mScoreMtxLst[[pName]][[mfName]][aIdx,]
                        }
                        mtxLst[[1+length(mtxLst)]] <- mtx
                    }
                    names(mtxLst) <- hIdxName

                    # hIdxCut[[mfName]] <- FCust_stdCut.hIdx( hName ,mfName ,mtxLst=mtxLst )
                    hIdxCut[[mfName]] <- bN.stdCut_hIdx( hName ,mfName ,mtxLst=mtxLst )
                }

            }
            cutterExtMLst[[hName]] <- list( stdCut=pLst ,hIdxCut=hIdxCut )
        }

        # cutterLst.bScr, cutterExtLst.bScr -------------------------------------------
		mLst <- list()  ;mExtLst <- list()  #   bN에서는 bScr을 사용치 않음. 이전 코드 호환성만을 위해 남겨 둠.
        
		cutterLst.bScr[[hName]] <- mLst
        cutterExtLst.bScr[[hName]] <- mExtLst

    }

    rObj$cutterLst          <- cutterLst
    rObj$cutterLst.bScr     <- cutterLst.bScr
    rObj$cutterExtLst       <- cutterExtLst
    rObj$cutterExtLst.bScr  <- cutterExtLst.bScr
    rObj$cutterExtMLst      <- cutterExtMLst

    return( rObj )

}

bN.stdCut_rawRow    <- function( hName ,mName ,pName ,scoreMtxH ){
    # hName ,mName ,pName ,scoreMtxObj$scoreMtx

    rObj <- list( defId=c(hName=hName,mName=mName,pName=pName) )

    hLen <- nrow(scoreMtxH)
    rObj$lastScore <- scoreMtxH[hLen,]
    rObj$available <- TRUE

    if( rObj$available ){
        cfg <- bNScoreMtxCfg[[mName]]
        if( !is.null(cfg) ){
            rObj$isHard <- if( is.null(cfg$isHard) ) bFCust.defaultHardFlag else cfg$isHard  # 일단 기존 코드 유지.

            rObj$lastEvt <- bFCust.getEvt( rObj$lastScore ,cfg$fCol )
            if( hLen>1 ){
                evt2 <- bFCust.getEvt( scoreMtxH[hLen-1 ,] ,cfg$fCol )
                rObj$evtReb <- bFCust.evtComp( evt2["lev",] ,rObj$lastEvt["lev",] ) # ,levMin=2
                if( all(is.na(rObj$evtReb$levDup)) )    rObj$evtReb <- NULL
            }

        } else {
            rObj$available <- FALSE
        }   # cfg

    }

    rObj$cut <- function( scoreMtx ,alreadyDead=NULL ,anaMode=TRUE ){

        val.len <- nrow(scoreMtx)
        if( is.null(alreadyDead) )	alreadyDead <- rep( F, val.len )

        cutLst <- list()    # anaMode=T일때만 필요. F이면 surFlag만 사용된다.
        if( !rObj$available ) return( list(cutLst=cutLst,surFlag=!alreadyDead) )

        hardFlag <- rObj$isHard( rObj$defId["pName"] )$flag["p"]
        cfg <- bNScoreMtxCfg[[ rObj$defId["mName"] ]]

        # each fCol --------------------------------------------
        cutLst.fCol <- list()
        for( fcName in names(cfg$fCol) ){
            for( aIdx in seq_len(val.len) ){
                if( !anaMode && alreadyDead[aIdx] ) next

                surWin <- cfg$fCol[[fcName]]$rng[ ,ifelse(hardFlag,"lev1","lev2")]
                val <- scoreMtx[aIdx,fcName]
                if( !bUtil.in(val,surWin) ){
                    alreadyDead[aIdx] <- TRUE

                    infoStr <- sprintf("%s(%d)",fcName,val )
                    cObj <- cutLst.fCol[[as.character(aIdx)]]
                    if( is.null(cObj) ){
                        cutLst.fCol[[as.character(aIdx)]] <- list( idx=aIdx ,info=infoStr )
                    } else {
                        cutLst.fCol[[as.character(aIdx)]]$info <- paste( cObj$info, infoStr, collapse=", " )
                    }

                }

            }

        }

        # sm row: evtCnt  --------------------------------------------
        cutLst.rowE <- list()
        for( aIdx in seq_len(val.len) ){
            if( !anaMode && alreadyDead[aIdx] ) next

            evt.sm <- bFCust.getEvt( scoreMtx[aIdx ,] ,cfg$fCol )
            evtMin <- cfg$evtMax[ifelse(hardFlag,"lev1","lev2"),]
            evtCnt <- sum( evt.sm["lev" ,]>=evtMin["minLev"] ,na.rm=T )
            evtCntH <- sum( evt.sm["lev" ,]>=evtMin["minLevH"] ,na.rm=T )
            if( evtCnt>evtMin["maxHpn"] || evtCntH>evtMin["maxHpnH"] ){
                alreadyDead[aIdx] <- TRUE

                infoStr=""
                if( anaMode ){
                    flag <- evt.sm["lev",]>=evtMin["minLev"]
                    flag[is.na(flag)] <- F

                    infoStr <- sprintf("evtCnt:%d(%s)",evtCnt,paste(evt.sm["lev" ,flag],collapse=","))
                }
                cutLst.rowE[[as.character(aIdx)]] <- list( idx=aIdx ,info=infoStr )
            }
        }

        # sm row: rebound --------------------------------------------
        cutLst.reb <- list()
        ctrObj <- bUtil.getRowRebCutter( rObj ,cfg )
        for( aIdx in seq_len(val.len) ){
            if( !anaMode && alreadyDead[aIdx] ) next

            smRow <- scoreMtx[aIdx ,]
            evt.sm <- bFCust.getEvt(smRow,cfg$fCol)

            rebCut <- ctrObj$cut( aIdx ,smRow ,evt.sm )
            if( !is.null(rebCut) ){
                alreadyDead[aIdx] <- TRUE
                cutLst.reb[[as.character(aIdx)]] <- rebCut
            }
        }

        if( anaMode ){  # build cutLst. anaMode일때만 필요. (aZoid생존여부는 alreadyDead에서 세팅되므로.)
            idxFCol <- if( length(cutLst.fCol)==0 ) integer(0) else sapply( cutLst.fCol  ,function(p){p$idx} )
            idxReb  <- if( length(cutLst.reb)==0 ) integer(0) else sapply( cutLst.reb   ,function(p){p$idx} )
            idxRowE <- if( length(cutLst.rowE)==0 ) integer(0) else sapply( cutLst.rowE  ,function(p){p$idx} )

            idxAll <- union(idxFCol,idxReb)
            idxAll <- sort(union(idxAll,idxRowE))

            cutLst <- list()
            names(cutLst.fCol)  <- idxFCol
            names(cutLst.reb)   <- idxReb
            names(idxRowE)      <- idxRowE

            for( aIdx in idxAll ){
                idStr <- as.character(aIdx)

                cLst <- list()
                if( !is.null(cutLst.fCol[[idStr]]) ){
                    cLst[["rawFCol"]] <- cutLst.fCol[[idStr]]
                    cLst[["rawFCol"]]$idObjDesc <- c( typ="rawFCol" ,rObj$defId )
                }
                if( !is.null(cutLst.reb[[idStr]]) ){
                    cLst[["rawReb"]] <- cutLst.reb[[idStr]]
                    cLst[["rawReb"]]$idObjDesc <- c( typ="rawReb" ,rObj$defId )
                }
                if( !is.null(cutLst.rowE[[idStr]]) ){
                    cLst[["rowE"]] <- cutLst.rowE[[idStr]]
                    cLst[["rowE"]]$idObjDesc <- c( typ="rowE" ,rObj$defId )
                }

                cutLst[[idStr]] <- list( idx=aIdx ,cLst=cLst )
            }
        }

        return( list(cutLst=cutLst,surFlag=!alreadyDead) )

    }

    return( rObj )

}
bN.stdCutExt_rawRow <- function( hName ,mName ,pName ,scoreMtxH ,fltName ){
    # hName ,mName ,pName ,scoreMtxH=scoreMtxObj$scoreMtx ,fltName=nIdx

    rObj <- list( defId=c(hName=hName,mName=mName,pName=pName,fltName=fltName) )

    extFilter <- bNMtxExtFltLst[[mName]][[fltName]]
    scrExtMtxH <- extFilter$getScoreMtx( scoreMtxH )

    hLen <- nrow(scrExtMtxH)
    rObj$lastScore <- scrExtMtxH[hLen,]
    rObj$available <- TRUE


    if( rObj$available ){

        cfg <- bNScrExtMtxCfg[[mName]][[fltName]]
        if( !is.null(cfg) ){
            rObj$isHard <- if( is.null(cfg$isHard) ) bFCust.defaultHardFlag else cfg$isHard

            rObj$lastEvt <- bFCust.getEvt( rObj$lastScore ,cfg$fCol )
            if( hLen>1 ){
                evt2 <- bFCust.getEvt( scrExtMtxH[hLen-1 ,] ,cfg$fCol )
                rObj$evtReb <- bFCust.evtComp( evt2["lev",] ,rObj$lastEvt["lev",] ) # ,levMin=2
                if( all(is.na(rObj$evtReb$levDup)) )    rObj$evtReb <- NULL
            }

        } else {
            rObj$available <- FALSE
        }   # cfg

    }

    rObj$cut <- function( scoreMtx ,alreadyDead=NULL ,anaMode=TRUE ){

        val.len <- nrow(scoreMtx)
        if( is.null(alreadyDead) )	alreadyDead <- rep( F, val.len )

        cutLst <- list()
        if( !rObj$available ) return( list(cutLst=cutLst,surFlag=!alreadyDead) )

        hardFlag <- rObj$isHard( rObj$defId["pName"] )$flag["p"]

        extFilter <- bNMtxExtFltLst[[ rObj$defId["mName"] ]][[ rObj$defId["fltName"] ]]
        scrExtMtx <- extFilter$getScoreMtx( scoreMtx )
        cfg <- bNScrExtMtxCfg[[ rObj$defId["mName"] ]][[ rObj$defId["fltName"] ]]

        # each fCol --------------------------------------------
        cutLst.fCol <- list()
        for( fcName in names(cfg$fCol) ){
            for( aIdx in seq_len(val.len) ){
                if( !anaMode && alreadyDead[aIdx] ) next

                surWin <- cfg$fCol[[fcName]]$rng[ ,ifelse(hardFlag,"lev1","lev2")]
                val <- scrExtMtx[aIdx,fcName]
                if( !bUtil.in(val,surWin) ){
                    alreadyDead[aIdx] <- TRUE

                    infoStr <- sprintf("%s(%d)",fcName,val )
                    cObj <- cutLst.fCol[[as.character(aIdx)]]
                    if( is.null(cObj) ){
                        cutLst.fCol[[as.character(aIdx)]] <- list( idx=aIdx ,info=infoStr )
                    } else {
                        cutLst.fCol[[as.character(aIdx)]]$info <- paste( cObj$info, infoStr, collapse=", " )
                    }

                }

            }

        }

        # sm row: evtCnt  --------------------------------------------
        cutLst.rowE <- list()
        for( aIdx in seq_len(val.len) ){
            if( !anaMode && alreadyDead[aIdx] ) next

            evt.sm <- bFCust.getEvt( scrExtMtx[aIdx ,] ,cfg$fCol )
            evtMin <- cfg$evtMax[ifelse(hardFlag,"lev1","lev2"),]
            evtCnt <- sum( evt.sm["lev" ,]>=evtMin["minLev"] ,na.rm=T )
            evtCntH <- sum( evt.sm["lev" ,]>=evtMin["minLevH"] ,na.rm=T )
            if( evtCnt>evtMin["maxHpn"] || evtCntH>evtMin["maxHpnH"] ){
                alreadyDead[aIdx] <- TRUE

                infoStr=""
                if( anaMode ){
                    flag <- evt.sm["lev",]>=evtMin["minLev"]
                    flag[is.na(flag)] <- F

                    infoStr <- sprintf("evtCnt:%d(%s)",evtCnt,paste(evt.sm["lev" ,flag],collapse=","))
                }
                cutLst.rowE[[as.character(aIdx)]] <- list( idx=aIdx ,info=infoStr )
            }
        }

        # sm row: rebound --------------------------------------------
        cutLst.reb <- list()
        ctrObj <- bUtil.getRowRebCutter( rObj ,cfg )
        for( aIdx in seq_len(val.len) ){
            if( !anaMode && alreadyDead[aIdx] ) next

            smRow <- scrExtMtx[aIdx ,]
            evt.sm <- bFCust.getEvt(smRow,cfg$fCol)

            rebCut <- ctrObj$cut( aIdx ,smRow ,evt.sm )
            if( !is.null(rebCut) ){
                alreadyDead[aIdx] <- TRUE
                cutLst.reb[[as.character(aIdx)]] <- rebCut
            }
        }

        if( anaMode ){  # build cutLst 
            idxFCol <- if( length(cutLst.fCol)==0 ) integer(0) else sapply( cutLst.fCol  ,function(p){p$idx} )
            idxReb  <- if( length(cutLst.reb)==0 ) integer(0) else sapply( cutLst.reb   ,function(p){p$idx} )
            idxRowE <- if( length(cutLst.rowE)==0 ) integer(0) else sapply( cutLst.rowE  ,function(p){p$idx} )

            idxAll <- union(idxFCol,idxReb)
            idxAll <- sort(union(idxAll,idxRowE))

            cutLst <- list()
            names(cutLst.fCol)  <- idxFCol
            names(cutLst.reb)   <- idxReb
            names(idxRowE)      <- idxRowE

            for( aIdx in idxAll ){
                idStr <- as.character(aIdx)

                cLst <- list()
                if( !is.null(cutLst.fCol[[idStr]]) ){
                    cLst[["rawFCol"]] <- cutLst.fCol[[idStr]]
                    cLst[["rawFCol"]]$idObjDesc <- c( typ="rawFCol" ,rObj$defId )
                }
                if( !is.null(cutLst.reb[[idStr]]) ){
                    cLst[["rawReb"]] <- cutLst.reb[[idStr]]
                    cLst[["rawReb"]]$idObjDesc <- c( typ="rawReb" ,rObj$defId )
                }
                if( !is.null(cutLst.rowE[[idStr]]) ){
                    cLst[["rowE"]] <- cutLst.rowE[[idStr]]
                    cLst[["rowE"]]$idObjDesc <- c( typ="rowE" ,rObj$defId )
                }

                cutLst[[idStr]] <- list( idx=aIdx ,cLst=cLst )
            }
        }

        return( list(cutLst=cutLst,surFlag=!alreadyDead) )
    }

    return( rObj )

}
bN.stdCut_hIdx      <- function(  hName ,mName ,mtxLst ){
    # hName ,mName ,mtxLst=hIdxObj[[hName]][[mName]]

    rObj <- list( defId=c(hName=hName,mName=mName) )

    hLen <- length(mtxLst)
    rObj$lastMtx <- mtxLst[[hLen]]
    rObj$available <- TRUE

    if( rObj$available ){
        cfg <- bNScoreMtxCfg[[mName]]
        if( !is.null(cfg) ){

            # standard Event check
            stdEvt.H2 <- NULL
            if( 1<length(mtxLst) ){
            	stdEvt.H2 <- bFCust.getEvt_byHIdx( mtxLst[[length(mtxLst)-1]] ,cfg ,NULL )
            }
            rObj$stdEvt.H1 <- bFCust.getEvt_byHIdx( mtxLst[[length(mtxLst)]] ,cfg ,lastEvt=stdEvt.H2 )

            # rebound check(skip zero)
            # bFCust.getSkipZero_byHIdx.ass
            szObj.H2 <- NULL
            if( 1<length(mtxLst) ){
                mtxLst.H2 <- mtxLst[1:(length(mtxLst)-1)]
                szObj.H2 <- bFCust.getSkipZero_byHIdx( mtxLst.H2 ,cfg )
            }
            rObj$szObj <- bFCust.getSkipZero_byHIdx( mtxLst ,cfg ,lastSZ=szObj.H2 )

        } else {
            rObj$available <- FALSE
        }   # cfg

    }


    rObj$getRawScore <- function( rawMtx ){

        if( !rObj$available ) return( NULL )

        cfg <- bNScoreMtxCfg[[ rObj$defId["mName"] ]]
        evtObj <- bFCust.getEvtMtx( rawMtx ,cfg )

        curEvt <- bFCust.getEvt_byHIdx( rawMtx ,cfg ,lastEvt=rObj$stdEvt.H1 )
        fColEvt <- bFCust.getEvt_byFCol( evtObj ,cfg )
        rebInfo <- bFCust.getSkipZero_byHIdx.ass( rObj$szObj ,rawMtx ,evtObj$eValMtx )

        return( list( cfg=cfg ,evtObj=evtObj ,curEvt=curEvt ,fColEvt=fColEvt ,rebInfo=rebInfo ) )
    }

    rObj$getRaw4Ass <- function( rawObj ){
        #   mName 단위가 아닌, 전체 mName 범위로 평가하기 위한 데이터 추출.
        #   bUtil.getCut1Score( ) 함수 참고.

        r4Ass <- list()

        # phaseHpnCnt는 현재 aZoid가 아닌, 이전 lastZoid에 관한 값임
        phaseHpnCnt <- rbind( rObj$stdEvt.H1$hpnInfo$phase ,rObj$stdEvt.H1$evtInfo$phase )
        rownames( phaseHpnCnt ) <- c("raw","evt")

        # phaseHpnCnt는 현재 aZoid가 아닌, 이전 lastZoid에 관한 값임
        phaseRebCnt <- rbind( rObj$stdEvt.H1$hpnInfo$phaseReb["reb",] ,rObj$stdEvt.H1$evtInfo$phaseReb["reb",] )
        rownames( phaseRebCnt ) <- c("raw","evt")

        # r4Ass$H1.phHpnCnt <- phaseHpnCnt    # 혼동가능성 때문에 이름 변경.
        # r4Ass$H1.phRebCnt <- phaseRebCnt    #       초기 이름은 phaseHpnCnt, phaseRebCnt 이었음.(검토 후 폐기.)

        r4Ass$rebMtx.ph <- rawObj$curEvt$rebInfo$rebMtx.ph

        evtHpnLevMtx <- NULL
        cName <- c("lev1","lev2","lev3")
        eLevMtx <- rawObj$evtObj$eLevMtx
        evtHpnLevMtx <- matrix( 0 ,nrow=length(cName) ,ncol=ncol(eLevMtx) ,dimnames=list(cName,colnames(eLevMtx)) )
        evtHpnLevMtx["lev1" ,] <- apply( eLevMtx ,2 ,function(cDat){sum(cDat==1,na.rm=T)} )
        evtHpnLevMtx["lev2" ,] <- apply( eLevMtx ,2 ,function(cDat){sum(cDat==2,na.rm=T)} )
        evtHpnLevMtx["lev3" ,] <- apply( eLevMtx ,2 ,function(cDat){sum(cDat>=3,na.rm=T)} )
        r4Ass$evtHpnLevMtx <- evtHpnLevMtx

        phaseReb.raw <- rawObj$curEvt$hpnInfo$phaseReb[c("reb","hpn"),] ;rownames(phaseReb.raw) <- c("rebFlag.raw","hpn.raw")
        phaseReb.evt <- rawObj$curEvt$evtInfo$phaseReb[c("reb","hpn"),] ;rownames(phaseReb.evt) <- c("rebFlag.evt","hpn.evt")
        r4Ass$phaseReb  <- rbind( phaseReb.raw ,phaseReb.evt )

        return( r4Ass )

    }

    rObj$getSummScore <- function( rawObj ){
        scoreObj <- list( )

        if( is.null(rawObj) ){
            rName <- c("raw","evt")
			cName <- c("all","ph","fCol","phReb","xyCnt.fCol","xyCnt.phase")
			summMtx <- matrix( 0 ,nrow=length(rName) ,ncol=length(cName) )
			rownames(summMtx) <- rName	;colnames(summMtx) <- cName

            scoreObj$summMtx        <- summMtx
            scoreObj$summMtx.reb    <- summMtx # 내부 구조는 같다.

            cName <- c("r.ph","r.fCol","r.dblHpnFlg" ,"e.ph","e.fCol","e.dblHpnFlg")
            rName <- c("rebCnt","rebDup")   # 반복 수, H1에서의 재현이 반복되었는지? ,발생 수
            scMtx.sz <- matrix( 0 ,ncol=length(cName) ,nrow=length(rName) ,dimnames=list(rName,cName) )
            scoreObj$scMtx.sz
            return( scoreObj )
        }
        cfg     <- rawObj$cfg
        evtObj  <- rawObj$evtObj
        curEvt  <- rawObj$curEvt
        fColEvt <- rawObj$fColEvt
        rebInfo <- rawObj$rebInfo   # bFCust.getSkipZero_byHIdx.ass() 값임을 유의

        # fColEvt 평가 적용.
        #   cfg의 evtMax.fCol이 이미 적용되었음(close max 값이므로)
        scoreObj$fColEvt <- fColEvt

        #   summMtx,summMtx.reb / stdEvt.H1 --------------------------------------------------------
        # curEvt <- bFCust.getEvt_byHIdx( rawMtx ,cfg ,lastEvt=rObj$stdEvt.H1 )
        scoreObj$summMtx <- curEvt$rebInfo$summMtx
        scoreObj$summMtx.reb <- NULL
        if( !is.null(rObj$stdEvt.H1$rebInfo) ){
            summMtx.reb <- scoreObj$summMtx
            summMtx.reb[,] <- 0     # 하나라도 0 이 아니면 Cut...

            rebInfo.H1 <- rObj$stdEvt.H1$rebInfo

            summMtx.reb["raw","all"] <- scoreObj$summMtx["raw","all"]>0 && curEvt$rebInfo$summMtx["raw","all"]>0
            summMtx.reb["evt","all"] <- scoreObj$summMtx["evt","all"]>0 && curEvt$rebInfo$summMtx["evt","all"]>0

            summMtx.reb["raw","ph"] <- sum( rebInfo.H1$rebMtx.ph["rebFlag.raw",]>0 & curEvt$rebInfo$rebMtx.ph["rebFlag.raw",]>0 )
            summMtx.reb["evt","ph"] <- sum( rebInfo.H1$rebMtx.ph["rebFlag.evt",]>0 & curEvt$rebInfo$rebMtx.ph["rebFlag.evt",]>0 )

            summMtx.reb["raw","fCol"] <- sum( rebInfo.H1$rebMtx.fCol["rebFlag.raw",]>0 & curEvt$rebInfo$rebMtx.fCol["rebFlag.raw",]>0 )
            summMtx.reb["evt","fCol"] <- sum( rebInfo.H1$rebMtx.fCol["rebFlag.evt",]>0 & curEvt$rebInfo$rebMtx.fCol["rebFlag.evt",]>0 )

            summMtx.reb["raw","phReb"] <- sum( rebInfo.H1$rebMtx.phReb["raw",]>0 & curEvt$rebInfo$rebMtx.phReb["raw",]>0 )
            summMtx.reb["evt","phReb"] <- sum( rebInfo.H1$rebMtx.phReb["evt",]>0 & curEvt$rebInfo$rebMtx.phReb["evt",]>0 )

            summMtx.reb["raw","xyCnt.fCol"] <- rebInfo.H1$rebMtx.xyCnt["raw","fCol.allMat"]>0 && curEvt$rebInfo$rebMtx.xyCnt["raw","fCol.allMat"]
            summMtx.reb["evt","xyCnt.fCol"] <- rebInfo.H1$rebMtx.xyCnt["evt","fCol.allMat"]>0 && curEvt$rebInfo$rebMtx.xyCnt["evt","fCol.allMat"]

            summMtx.reb["raw","xyCnt.phase"] <- rebInfo.H1$rebMtx.xyCnt["raw","phase.allMat"]>0 && curEvt$rebInfo$rebMtx.xyCnt["raw","phase.allMat"]
            summMtx.reb["evt","xyCnt.phase"] <- rebInfo.H1$rebMtx.xyCnt["evt","phase.allMat"]>0 && curEvt$rebInfo$rebMtx.xyCnt["evt","phase.allMat"]


            scoreObj$summMtx.reb <- summMtx.reb
        }

        #   scMtx.sz / szObj ------------------------------------------------------------
        # rebInfo <- bFCust.getSkipZero_byHIdx.ass( rObj$szObj ,rawMtx ,evtObj$eValMtx )
        cName <- c("r.ph","r.fCol","r.dblHpnFlg" ,"e.ph","e.fCol","e.dblHpnFlg")
        rName <- c("rebCnt","rebDup")   # 반복 수, H1에서의 재현이 반복되었는지? ,발생 수
        scMtx.sz <- matrix( 0 ,ncol=length(cName) ,nrow=length(rName) ,dimnames=list(rName,cName) )
        if( TRUE ){
            scMtx.sz["rebCnt","r.ph"] <- sum(rebInfo$matRaw$ph["mat",])
            scMtx.sz["rebCnt","r.fCol"] <- sum(rebInfo$matRaw$fCol["mat",])
            scMtx.sz["rebCnt","r.dblHpnFlg"] <- rebInfo$matRaw$dblHpn["mat"]
            scMtx.sz["rebCnt","e.ph"] <- sum(rebInfo$matEvt$ph["mat",])
            scMtx.sz["rebCnt","e.fCol"] <- sum(rebInfo$matEvt$fCol["mat",])
            scMtx.sz["rebCnt","e.dblHpnFlg"] <- rebInfo$matEvt$dblHpn["mat"]

            if( !is.null(rObj$szObj$rebInfo) ){ # reb Dup
                matFlag <- (rObj$szObj$rebInfo$matRaw$ph["mat",]>0) & (rebInfo$matRaw$ph["mat",]>0)
                scMtx.sz["rebDup","r.ph"] <- sum( matFlag )
                matFlag <- (rObj$szObj$rebInfo$matRaw$fCol["mat",]>0) & (rebInfo$matRaw$fCol["mat",]>0)
                scMtx.sz["rebDup","r.fCol"] <- sum( matFlag )
                scMtx.sz["rebDup","r.dblHpnFlg"] <- (rObj$szObj$rebInfo$matRaw$dblHpn["mat"]>0) && (rebInfo$matRaw$dblHpn["mat"]>0)

                matFlag <- (rObj$szObj$rebInfo$matEvt$ph["mat",]>0) & (rebInfo$matEvt$ph["mat",]>0)
                scMtx.sz["rebDup","e.ph"] <- sum( matFlag )
                matFlag <- (rObj$szObj$rebInfo$matEvt$fCol["mat",]>0) & (rebInfo$matEvt$fCol["mat",]>0)
                scMtx.sz["rebDup","e.fCol"] <- sum( matFlag )
                scMtx.sz["rebDup","e.dblHpnFlg"] <- (rObj$szObj$rebInfo$matEvt$dblHpn["mat"]>0) && (rebInfo$matEvt$dblHpn["mat"]>0)
            }            
        }
        scoreObj$scMtx.sz <- scMtx.sz

        return( scoreObj )
    }

    rObj$cut <- function( scoreMtx ,anaMode=TRUE ){   # 하나씩 오므로, alreadyDead 처리.

        cLst <- list( )     #   cutLst[[1]]$cLst
                            #   만약 생존했으면 cLst의 길이는 0
        if( !rObj$available ) return( cLst )

        rawObj <- rObj$getRawScore( scoreMtx )
        scObj <- rObj$getSummScore( rawObj )
        cfg <- bNScoreMtxCfg[[ rObj$defId["mName"] ]]

        survive <- TRUE
        # if( survive ){  # fCol Evt Cnt 
        if( TRUE ){  # fCol Evt Cnt 
            closeMaxDistVal <- scObj$fColEvt$closeMaxDistVal
            fEvtMtx <- scObj$fColEvt$fEvtMtx
            fClMMtx <- scObj$fColEvt$fClMMtx

            # fCol 별로 전체 ph에서의 evt 발생 수 한계 제약.
            #   cfg$fCol[[n]]$evtMaxFColTot
            if( any(fClMMtx[,"lev1ClM"]>=closeMaxDistVal) ){
                survive <- F
                cLst[["fCol EvtCnt4AllPh(lev1ClM)"]] <- "fCol EvtCnt4AllPh(lev1ClM)"
                if( anaMode ){
                    flag <- fClMMtx[,"lev1ClM"]>=closeMaxDistVal
                    str <- paste( names( fClMMtx[,"lev1ClM"] )[flag] ,fEvtMtx[flag,"lev1Cnt"] ,sep=":" )
                    infoStr <- sprintf("fCol EvtCnt4AllPh(lev1ClM) %s",paste(str,collapse=", "))
                    cLst[["fCol EvtCnt4AllPh(lev1ClM)"]] <- infoStr
                }
            }
            if( any(fClMMtx[,"lev2ClM"]>=closeMaxDistVal) ){
                survive <- F
                cLst[["fCol EvtCnt4AllPh(lev2ClM)"]] <- "fCol EvtCnt4AllPh(lev2ClM)"
                if( anaMode ){
                    flag <- fClMMtx[,"lev2ClM"]>=closeMaxDistVal
                    str <- paste( names( fClMMtx[,"lev2ClM"] )[flag] ,fEvtMtx[flag,"lev2Cnt"] ,sep=":" )
                    infoStr <- sprintf("fCol EvtCnt4AllPh(lev2ClM) %s",paste(str,collapse=", "))
                    cLst[["fCol EvtCnt4AllPh(lev2ClM)"]] <- infoStr
                }
            }
            if( any(fClMMtx[,"lev3ClM"]>=closeMaxDistVal) ){
                survive <- F
                cLst[["fCol EvtCnt4AllPh(lev3ClM)"]] <- "fCol EvtCnt4AllPh(lev3ClM)"
                if( anaMode ){
                    flag <- fClMMtx[,"lev3ClM"]>=closeMaxDistVal
                    str <- paste( names( fClMMtx[,"lev3ClM"] )[flag] ,fEvtMtx[flag,"lev3Cnt"] ,sep=":" )
                    infoStr <- sprintf("fCol EvtCnt4AllPh(lev3ClM) %s",paste(str,collapse=", "))
                    cLst[["fCol EvtCnt4AllPh(lev3ClM)"]] <- infoStr
                }
            }

            # 이제 오버매치가 발생한 fCol 들의 갯수를 제한하자.
            #   evtMaxFColTot  <- c( lev1Max=1 ,lev2Max=1 ,lev3Max=1 )
            botThld <- closeMaxDistVal - closeMaxDistVal    # 0
            eSumLev1 <- sum(scObj$fColEvt$fClMMtx[,"lev1ClM"] > botThld )
            if( eSumLev1>=cfg$evtMaxFColTot["lev1Max"] ){
                survive <- F
                cLst[["fCol evtMaxFColTot(lev1ClM)"]] <- "fCol evtMaxFColTot(lev1ClM)"
                if( anaMode ){
                    flag <- scObj$fColEvt$fClMMtx[,"lev1ClM"] > botThld
                    str <- paste( names( scObj$fColEvt$fClMMtx[,"lev1ClM"] )[flag] ,scObj$fColEvt$fEvtMtx[flag,"lev1Cnt"] ,sep=":" )
                    infoStr <- sprintf("fCol evtMaxFColTot(lev1ClM) %s",paste(str,collapse=", "))
                    cLst[["fCol evtMaxFColTot(lev1ClM)"]] <- infoStr
                }
            }
            botThld <- closeMaxDistVal - 2
            eSumLev2 <- sum(scObj$fColEvt$fClMMtx[,"lev2ClM"] > botThld )
            if( eSumLev2>=cfg$evtMaxFColTot["lev2Max"] ){
                survive <- F
                cLst[["fCol evtMaxFColTot(lev2ClM)"]] <- "fCol evtMaxFColTot(lev2ClM)"
                if( anaMode ){
                    flag <- scObj$fColEvt$fClMMtx[,"lev2ClM"] > botThld
                    str <- paste( names( scObj$fColEvt$fClMMtx[,"lev2ClM"] )[flag] ,scObj$fColEvt$fEvtMtx[flag,"lev2Cnt"] ,sep=":" )
                    infoStr <- sprintf("fCol evtMaxFColTot(lev2ClM) %s",paste(str,collapse=", "))
                    cLst[["fCol evtMaxFColTot(lev2ClM)"]] <- infoStr
                }
            }
            botThld <- closeMaxDistVal - 2
            eSumLev3 <- sum(scObj$fColEvt$fClMMtx[,"lev3ClM"] > botThld )
            if( eSumLev3>=cfg$evtMaxFColTot["lev3Max"] ){
                survive <- F
                cLst[["fCol evtMaxFColTot(lev3ClM)"]] <- "fCol evtMaxFColTot(lev3ClM)"
                if( anaMode ){
                    flag <- scObj$fColEvt$fClMMtx[,"lev3ClM"] > botThld
                    str <- paste( names( scObj$fColEvt$fClMMtx[,"lev3ClM"] )[flag] ,scObj$fColEvt$fEvtMtx[flag,"lev3Cnt"] ,sep=":" )
                    infoStr <- sprintf("fCol evtMaxFColTot(lev3ClM) %s",paste(str,collapse=", "))
                    cLst[["fCol evtMaxFColTot(lev3ClM)"]] <- infoStr
                }
            }

        }

        if( survive ){ #   summMtx.cut
            infoStr <- ""
            #     $summMtx    all ph fCol phReb xyCnt.fCol xyCnt.phase
            #             raw   0  0    0     0          0           0
            #             evt   0  0    0     0          0           0
            summMtx.cut <- scObj$summMtx >= cfg$summMtx
            if( any(summMtx.cut["raw",]) ){
                survive <- F
                cLst[["summMtx.cut raw"]] <- "summMtx.cut raw"
                if( anaMode ){  # infoStr
                    flag <- summMtx.cut["raw",]
                    str <- paste( names(scObj$summMtx["raw",])[flag] ,scObj$summMtx["raw",flag] ,sep=":" )
                    infoStr <- sprintf("summMtx.cut - raw %s",paste(str,collapse=", ") )
                    cLst[["summMtx.cut raw"]] <- infoStr
                }
            }
            if( any(summMtx.cut["evt",]) ){
                survive <- F
                cLst[["summMtx.cut evt"]] <- "summMtx.cut evt"
                if( anaMode ){  # infoStr
                    flag <- summMtx.cut["evt",]
                    str <- paste( names(scObj$summMtx["evt",])[flag] ,scObj$summMtx["evt",flag] ,sep=":" )
                    infoStr <- sprintf("summMtx.cut - evt %s",paste(str,collapse=", ") )
                    cLst[["summMtx.cut evt"]] <- infoStr
                }
            }
            if( cfg$summMtx.sum["raw"]<=sum(scObj$summMtx["raw",]) ){
                survive <- F
                cLst[["summMtx.cut raw.sum"]] <- "summMtx.cut raw.sum"
                if( anaMode ){  # infoStr
                    flag <- scObj$summMtx["raw",]>0
                    str <- paste( names(scObj$summMtx["raw",])[flag] ,scObj$summMtx["raw",flag] ,sep=":" )
                    infoStr <- sprintf("summMtx.cut - raw.sum %s",paste(str,collapse=", ") )
                    cLst[["summMtx.cut raw.sum"]] <- infoStr
                }
            }
            if( cfg$summMtx.sum["evt"]<=sum(scObj$summMtx["evt",]) ){
                survive <- F
                cLst[["summMtx.cut evt.sum"]] <- "summMtx.cut evt.sum"
                if( anaMode ){  # infoStr
                    flag <- scObj$summMtx["evt",]>0
                    str <- paste( names(scObj$summMtx["evt",])[flag] ,scObj$summMtx["evt",flag] ,sep=":" )
                    infoStr <- sprintf("summMtx.cut - evt.sum %s",paste(str,collapse=", ") )
                    cLst[["summMtx.cut evt.sum"]] <- infoStr
                }
            }
        }

        if( survive ){ #   summMtx.reb.cut
            #     $summMtx.reb  all ph fCol phReb xyCnt.fCol xyCnt.phase
            #               raw   0  0    0     0          0           0
            #               evt   0  0    0     0          0           0
            summMtx.reb.cut <- scObj$summMtx.reb >= cfg$summMtx.reb
            if( any(summMtx.reb.cut["raw",]) ){
                survive <- F
                cLst[["summMtx.reb.cut raw"]] <- "summMtx.reb.cut raw"
                if( anaMode ){  # infoStr
                    flag <- summMtx.reb.cut["raw",]
                    str <- paste( names(scObj$summMtx.reb["raw",])[flag] ,scObj$summMtx.reb["raw",flag] ,sep=":" )
                    infoStr <- sprintf("summMtx.reb.cut - raw %s",paste(str,collapse=", ") )
                    cLst[["summMtx.reb.cut raw"]] <- infoStr
                }
            }
            if( any(summMtx.reb.cut["evt",]) ){
                survive <- F
                cLst[["summMtx.reb.cut evt"]] <- "summMtx.reb.cut evt"
                if( anaMode ){  # infoStr
                    flag <- summMtx.reb.cut["evt",]
                    str <- paste( names(scObj$summMtx.reb["evt",])[flag] ,scObj$summMtx.reb["evt",flag] ,sep=":" )
                    infoStr <- sprintf("summMtx.reb.cut - evt %s",paste(str,collapse=", ") )
                    cLst[["summMtx.reb.cut evt"]] <- infoStr
                }
            }
            # sum 체크를 해야 할 일은 없을 듯 하다.
        }

        if( survive ){ #   scMtx.sz.cut
            #     $scMtx.sz   r.ph r.fCol r.dblHpnFlg e.ph e.fCol e.dblHpnFlg
            #             rebCnt    0      0           0    0      0           0
            #             rebDup    0      0           0    0      0           0
            scMtx.sz.cut <- scObj$scMtx.sz >= cfg$scMtx.sz
            if( any(scMtx.sz.cut["rebCnt",]) ){ #   rebCnt
                survive <- F
                cLst[["scMtx.sz.cut rebCnt"]] <- "scMtx.sz.cut rebCnt"
                if( anaMode ){  # infoStr
                    flag <- scMtx.sz.cut["rebCnt",]
                    str <- paste( names(scObj$scMtx.sz["rebCnt",])[flag] ,scObj$scMtx.sz["rebCnt",flag] ,sep=":" )
                    infoStr <- sprintf("scMtx.sz.cut - rebCnt %s",paste(str,collapse=", ") )
                    cLst[["scMtx.sz.cut rebCnt"]] <- infoStr
                }
            }
            if( any(scMtx.sz.cut["rebDup",]) ){ #   rebDup
                survive <- F
                cLst[["scMtx.sz.cut rebDup"]] <- "scMtx.sz.cut rebDup"
                if( anaMode ){  # infoStr
                    flag <- scMtx.sz.cut["rebDup",]
                    str <- paste( names(scObj$scMtx.sz["rebDup",])[flag] ,scObj$scMtx.sz["rebDup",flag] ,sep=":" )
                    infoStr <- sprintf("scMtx.sz.cut - rebDup %s",paste(str,collapse=", ") )
                    cLst[["scMtx.sz.cut rebDup"]] <- infoStr
                }
            }

            sumCol <- c("r.ph","r.fCol","r.dblHpnFlg")
            if( cfg$scMtx.sz.sum["rebCnt.r"]<=sum(scObj$scMtx.sz["rebCnt",sumCol]) ){
                survive <- F
                cLst[["scMtx.sz.cut rebCnt.r.sum"]] <- "scMtx.sz.cut rebCnt.r.sum"
                if( anaMode ){  # infoStr
                    str <- paste( sumCol ,scObj$scMtx.sz["rebCnt",sumCol] ,sep=":" )
                    infoStr <- sprintf("scMtx.sz.cut - rebCnt.r.sum %s",paste(str,collapse=", ") )
                    cLst[["scMtx.sz.cut rebCnt.r.sum"]] <- infoStr
                }
            }
            sumCol <- c("e.ph","e.fCol","e.dblHpnFlg")
            if( cfg$scMtx.sz.sum["rebCnt.e"]<=sum(scObj$scMtx.sz["rebCnt",sumCol]) ){
                survive <- F
                cLst[["scMtx.sz.cut rebCnt.e.sum"]] <- "scMtx.sz.cut rebCnt.e.sum"
                if( anaMode ){  # infoStr
                    str <- paste( sumCol ,scObj$scMtx.sz["rebCnt",sumCol] ,sep=":" )
                    infoStr <- sprintf("scMtx.sz.cut - rebCnt.e.sum %s",paste(str,collapse=", ") )
                    cLst[["scMtx.sz.cut rebCnt.e.sum"]] <- infoStr
                }
            }

            # cfg$scMtx.sz.sum["rebDup",] 의 총합을 체크해야 할 일은 없을 듯.

        }

        if( TRUE ){ # bUtil.closeMax() QQE 차후 적용 필요.
            windMtxMin <- cfg$scMtx.sz
            windMtxMin[,] <- 0
            cm_scMtx.sz <- bUtil.closeMax_Mtx( scObj$scMtx.sz ,windMtxMin=NULL ,windMtxMax=cfg$scMtx.sz )
        }

        return( list( cLst=cLst ,scObj=scObj ) )
    }

    return( rObj )

}


bN.cut1 <- function( scoreMtx.grp ,cut.grp ,fHName ,tgt.scMtx=NULL ,anaOnly=F ,logger=NULL ){
    #   anaOnly=T : scoreMtx[1,] 만 분석하며, 그 대신 cutting 정보를 추가한다.
	#	logger <- k.getFlogObj( "./log/cutLog.txt" )

	reportStatus <- function( tStmp ,strWhere ,surFlag ,logger ){
		#	strWhere <- sprintf("[%s,%s] stdLst",hName,mName)
		if( is.null(logger) )	return( NULL )
		tDiff <- Sys.time() - tStmp
		rptStr <- sprintf("    %s  %d/%d  %.1f%s ",strWhere,sum(surFlag),length(surFlag),tDiff,units(tDiff) )
		logger$fLogStr( rptStr )
	}

    scMtxName <- names(cut.grp$mtxInfoLst)
	if( !is.null(tgt.scMtx) )	scMtxName <- intersect( scMtxName ,tgt.scMtx )

	bScrMtxName <- names(cut.grp$mtxInfoLst.bScr)   # bN에선 bScr이 없다. 그냥 기존 코드 호환을 위해 남겨놓음.
	if( !is.null(tgt.scMtx) )	bScrMtxName <- intersect( bScrMtxName ,tgt.scMtx )

    datLen <- 1
	cutInfoLst <- list()
	if( !anaOnly ){
		cutInfoLst <- NULL

		if( 0<length(scMtxName) ){
			datLen <- nrow(scoreMtx.grp$basic[[1]][[ scMtxName[1] ]]$scoreMtx)
		} else if( 0<length(bScrMtxName) ){
			datLen <- nrow(scoreMtx.grp$mf[[ bScrMtxName[1] ]]$scoreMtx)
		}
	}

	tStmp <- Sys.time()
	if( !is.null(logger) ) logger$fLogStr("Start", pTime=T ,pAppend=F )

    surFlag <- rep( T ,datLen )
	auxInfoLst <- list( basic=list() ,mf=list() )
	mtxGrp <- NULL
	if( 0<length(scMtxName) ){
		mtxGrp <- getScoreMtx.grp_byHIdx( scoreMtx.grp )
	}
    for( hName in fHName ){ # hName <- fHName[1]
        for( mName in scMtxName ){ # mName <- scMtxName[1]
            #   "stdCut" -------------------------------------------
            for( pName in cut.grp$phaseName ){   # pName <- cut.grp$phaseName[1]
				scoreMtx <- scoreMtx.grp$basic[[pName]][[mName]]$scoreMtx

                cutObj <- cut.grp$cutterLst[[hName]][[mName]]$stdCut[[pName]]
                cRst <- cutObj$cut( scoreMtx ,alreadyDead=!surFlag ,anaMode=anaOnly )
				if( !anaOnly ){	surFlag <- surFlag & cRst$surFlag
				} else {
					if( 0<length(cRst$cutLst) ){
						cutInfoLst <- append( cutInfoLst 
											,lapply( cRst$cutLst[[1]]$cLst ,function(p){ c(p$idObjDesc ,info=p$info) } ) 
										)
					}
				}

				for( extFltName in names(cut.grp$cutterExtLst[[hName]][[mName]]$stdCut[[pName]]) ){
					cutExtObj <- cut.grp$cutterExtLst[[hName]][[mName]]$stdCut[[pName]][[extFltName]]
					cRst <- cutExtObj$cut( scoreMtx ,alreadyDead=!surFlag ,anaMode=anaOnly )
					if( !anaOnly ){	surFlag <- surFlag & cRst$surFlag
					} else {
						if( 0<length(cRst$cutLst) ){
							cutInfoLst <- append( cutInfoLst 
												,lapply( cRst$cutLst[[1]]$cLst ,function(p){ c(p$idObjDesc ,info=p$info) } ) 
											)
						}
					}
				}

				reportStatus( tStmp ,sprintf("     %s",pName) ,surFlag ,logger )
            }
			reportStatus( tStmp ,sprintf("[%s,%s] stdLst",hName,mName) ,surFlag ,logger )

			#   "hIdxLst" ------------------------------------------
			hIdxCut <- cut.grp$cutterLst[[hName]][[mName]]$hIdxCut
			for( aIdx in seq_len(datLen) ){
				if( !anaOnly && !surFlag[aIdx] )	next

				cutInfo <- hIdxCut$cut( mtxGrp[[mName]][[aIdx]] ,anaMode=anaOnly )
				if( 0<length(cutInfo$cLst) ){
					if( !anaOnly ){	surFlag[aIdx] <- FALSE
					} else {
						for( idx in seq_len(length(cutInfo$cLst)) ){
							idxName <- sprintf("hIdxCut_%dth",1+length(cutInfoLst))
							cutInfoLst[[idxName]] <- c( typ=names(cutInfo$cLst)[idx] ,hIdxCut$defId ,pName="ALL" ,info=cutInfo$cLst[[idx]] )
						}
					}
				}

				if( anaOnly && ("sfcLate"==hName) ){	# anaOnly상태이면 aIdx는 항상 1이라는 가정.
					auxInfoLst$basic[[mName]] <- cutInfo$scObj
				}
			}

			reportStatus( tStmp ,sprintf("[%s,%s] hIdxLst",hName,mName) ,surFlag ,logger )

        }

		# bNMtxMulti ----------------------------------------------------------------
		availMFName <- names(cut.grp$cutterExtMLst[[hName]]$stdCut[[1]])
		for( mfName in availMFName ){
			mtxMaker <- bNMtxMFltLst[[mfName]]( tgt.scMtx )
			if( !mtxMaker$available )   next

			mtxLst <- list()
			for( pName in cut.grp$phaseName ){
				scoreMtxLst <- scoreMtx.grp$basic[[pName]]
				mtxLst[[pName]] <- mtxMaker$getScoreMtx( scoreMtxLst )

                cutObj <- cut.grp$cutterExtMLst[[hName]]$stdCut[[pName]][[mfName]]
                cRst <- cutObj$cut( mtxLst[[pName]] ,alreadyDead=!surFlag ,anaMode=anaOnly )
				if( !anaOnly ){	surFlag <- surFlag & cRst$surFlag
				} else {
					if( 0<length(cRst$cutLst) ){
						cutInfoLst <- append( cutInfoLst 
											,lapply( cRst$cutLst[[1]]$cLst ,function(p){ c(p$idObjDesc ,info=p$info) } ) 
										)
					}
				}
			}

			#   "hIdxLst" ------------------------------------------
			# mtxGrp$score1[[1]]
			mtx <- matrix( 0 ,nrow=length(mtxMaker$mInfo$cName) ,ncol=length(cut.grp$phaseName) 
						,dimnames=list( mtxMaker$mInfo$cName ,cut.grp$phaseName )
			)
			for( aIdx in seq_len(datLen) ){
				if( !anaOnly && !surFlag[aIdx] )	next

				for( pName in cut.grp$phaseName ){
					mtx[,pName] <- mtxLst[[pName]][aIdx,]
				}

				hIdxCut <- cut.grp$cutterExtMLst[[hName]]$hIdxCut[[mfName]]
				cutInfo <- hIdxCut$cut( mtx ,anaMode=anaOnly )
				if( 0<length(cutInfo$cLst) ){
					if( !anaOnly ){	surFlag[aIdx] <- FALSE
					} else {
						for( idx in seq_len(length(cutInfo$cLst)) ){
							idxName <- sprintf("hIdxCut_%dth",1+length(cutInfoLst))
							cutInfoLst[[idxName]] <- c( typ=names(cutInfo$cLst)[idx] ,hIdxCut$defId ,pName="ALL" ,info=cutInfo$cLst[[idx]] )
						}
					}
				}

			}

		}

		# for( mName in bScrMtxName ){		}       # bF와 달리, bN에서 bScr 부분은 없다.

    }

    return( list( surFlag=surFlag ,cutInfoLst=cutInfoLst ,auxInfoLst=auxInfoLst ) )

} # bUtil.cut1()

bN.getCut1Score <- function( scoreMtx.grp ,cut.grp ,fHName ,tgt.scMtx=NULL ,logger=NULL ,deepInfo=F ){

	reportStatus <- function( tStmp ,strWhere ,surFlag ,logger ){
		#	strWhere <- sprintf("[%s,%s] stdLst",hName,mName)
		if( is.null(logger) )	return( NULL )
		tDiff <- Sys.time() - tStmp
		rptStr <- sprintf("    %s  %d/%d  %.1f%s ",strWhere,sum(surFlag),length(surFlag),tDiff,units(tDiff) )
		logger$fLogStr( rptStr )
	}

    scMtxName <- names(cut.grp$mtxInfoLst)
	if( !is.null(tgt.scMtx) )	scMtxName <- intersect( scMtxName ,tgt.scMtx )
	bScrMtxName <- names(cut.grp$mtxInfoLst.bScr)
	if( !is.null(tgt.scMtx) )	bScrMtxName <- intersect( bScrMtxName ,tgt.scMtx )

	datLen <- nrow(scoreMtx.grp$basic[[1]][[ scMtxName[1] ]]$scoreMtx)
	if( is.null(datLen) ){
		datLen <- nrow(scoreMtx.grp$mf[[ bScrMtxName[1] ]]$scoreMtx)
	}

	tStmp <- Sys.time()
	if( !is.null(logger) ) logger$fLogStr("Start", pTime=T ,pAppend=F )

	mtxGrp <- getScoreMtx.grp_byHIdx( scoreMtx.grp ,tgt.scMtx=scMtxName )
	aLst <- list()
	for( aIdx in seq_len(datLen) ){
		hLst <- list()
		for( hName in fHName ){ # hName <- fHName[1]
			basicLst <- list()
			for( mName in scMtxName ){ # mName <- scMtxName[1]
				#   "hIdxLst" ------------------------------------------
				hIdxCut <- cut.grp$cutterLst[[hName]][[mName]]$hIdxCut
				rawObj <- hIdxCut$getRawScore( mtxGrp[[mName]][[aIdx]] )
				raw4Ass <- hIdxCut$getRaw4Ass( rawObj )
				summObj <- hIdxCut$getSummScore( rawObj )

				basicLst[[mName]] <- list(raw=raw4Ass ,summ=summObj)
				if( deepInfo){
					# rawObj$rebInfo 는 bFCust.getSkipZero_byHIdx.ass() 로부터 나왔다.
					basicLst[[mName]]$rawSz <- list( ph=rawObj$rebInfo$matRaw$ph ,fCol=rawObj$rebInfo$matRaw$fCol )
				}

				# reportStatus( tStmp ,sprintf("[%s,%s] hIdxLst",hName,mName) ,surFlag ,logger )
			}

			bScrLst <- list()	#  bf와 달리, bN에서는 bScrLst가 없음.
			bScrMtxName <- character(0)

			hLst[[hName]] <- list( basic=basicLst ,bScr=bScrLst )
		}

		aLst[[as.character(aIdx)]] <- hLst
	}	# for(aIdx)

	cut1ScoreObj <- list( aLst=aLst )
	cut1ScoreObj$metaInfo <- list( datLen=datLen ,scMtxName=scMtxName ,bScrMtxName=bScrMtxName )

	return( cut1ScoreObj )
} # bUtil.cut1Score()


bN.getStdMILst <- function( gEnv ,fRstLst ){

    stdMILst.basic <- list()
    if( TRUE ){
		zMtx <- bN.phCStepColVal( gEnv ,tgtCol=1 )$zMtx
		stdMILst.basic[["cStepCV1"]] <- list( stdMI=fCutU.getMtxInfo(zMtx) ,zMtx=zMtx )
		zMtx <- bN.phCStepColVal( gEnv ,tgtCol=2 )$zMtx
		stdMILst.basic[["cStepCV2"]] <- list( stdMI=fCutU.getMtxInfo(zMtx) ,zMtx=zMtx )
		zMtx <- bN.phCStepColVal( gEnv ,tgtCol=3 )$zMtx
		stdMILst.basic[["cStepCV3"]] <- list( stdMI=fCutU.getMtxInfo(zMtx) ,zMtx=zMtx )
		zMtx <- bN.phCStepColVal( gEnv ,tgtCol=4 )$zMtx
		stdMILst.basic[["cStepCV4"]] <- list( stdMI=fCutU.getMtxInfo(zMtx) ,zMtx=zMtx )
		zMtx <- bN.phCStepColVal( gEnv ,tgtCol=5 )$zMtx
		stdMILst.basic[["cStepCV5"]] <- list( stdMI=fCutU.getMtxInfo(zMtx) ,zMtx=zMtx )

		zMtx <- bN.phFStepColVal( gEnv ,tgtCol=1 )$zMtx
		stdMILst.basic[["fStepCV1"]] <- list( stdMI=fCutU.getMtxInfo(zMtx) ,zMtx=zMtx )
		zMtx <- bN.phFStepColVal( gEnv ,tgtCol=2 )$zMtx
		stdMILst.basic[["fStepCV2"]] <- list( stdMI=fCutU.getMtxInfo(zMtx) ,zMtx=zMtx )
		zMtx <- bN.phFStepColVal( gEnv ,tgtCol=3 )$zMtx
		stdMILst.basic[["fStepCV3"]] <- list( stdMI=fCutU.getMtxInfo(zMtx) ,zMtx=zMtx )
		zMtx <- bN.phFStepColVal( gEnv ,tgtCol=4 )$zMtx
		stdMILst.basic[["fStepCV4"]] <- list( stdMI=fCutU.getMtxInfo(zMtx) ,zMtx=zMtx )
		zMtx <- bN.phFStepColVal( gEnv ,tgtCol=5 )$zMtx
		stdMILst.basic[["fStepCV5"]] <- list( stdMI=fCutU.getMtxInfo(zMtx) ,zMtx=zMtx )
		zMtx <- bN.phFStepColVal( gEnv ,tgtCol=6 )$zMtx
		stdMILst.basic[["fStepCV6"]] <- list( stdMI=fCutU.getMtxInfo(zMtx) ,zMtx=zMtx )
    }

    stdMI.bDup <- list()    ;stdMI.mf <- list()     # 기존 코드 호환성을 위해 남겨놓음.

	rObj <- list( basic=stdMILst.basic ,bDup=stdMI.bDup ,mf=stdMI.mf )
	rObj$anyWarn <- function(){
		rptStr <- character(0)

		chkStr <- character(0)
		for( nIdx in names(rObj$basic) ){
			rCnt <- nrow(rObj$basic[[nIdx]]$stdMI$rawTail)
			if( rCnt<6 ) chkStr <- c( chkStr ,sprintf("%s:%d",nIdx,rCnt) )
		}
		if( 0<length(chkStr) ){
			rptStr <- c( rptStr ,"stdMI.basic")
			rptStr <- c( rptStr ,sprintf("    warn!! %s",paste(chkStr,collapse=" ")) )
		}

		chkStr <- character(0)
		for( nIdx in names(rObj$bDup) ){
			rCnt <- nrow(rObj$bDup[[nIdx]]$stdMI$rawTail)
			if( rCnt<6 ) chkStr <- c( chkStr ,sprintf("%s:%d",nIdx,rCnt) )
		}
		if( 0<length(chkStr) ){
			rptStr <- c( rptStr ,"stdMI.bDup")
			rptStr <- c( rptStr ,sprintf("    warn!! %s",paste(chkStr,collapse=" ")) )
		}

		chkStr <- character(0)
		for( nIdx in names(rObj$mf) ){
			rCnt <- nrow(rObj$mf[[nIdx]]$stdMI$rawTail)
			if( rCnt<6 ) chkStr <- c( chkStr ,sprintf("%s:%d",nIdx,rCnt) )
		}
		if( 0<length(chkStr) ){
			rptStr <- c( rptStr ,"stdMI.mf")
			rptStr <- c( rptStr ,sprintf("    warn!! %s",paste(chkStr,collapse=" ")) )
		}

		rptStr <- paste( rptStr ,collapse="\n" )
		return( sprintf("%s\n",rptStr) )
	} # rObj$rpt( )

    return( rObj )

}

bN.phCStepColVal <- function( gEnv ,tgtCol ){

    hLen <- nrow(gEnv$zhF)
    cStepH <- gEnv$zhF[,2:6]-gEnv$zhF[,1:5]

    cStepVal <- cStepH[hLen,tgtCol]
    cStepVal.idx <- which( cStepH[1:(hLen-1),tgtCol]==cStepVal )+1      # integer(0)이면 +1 해도 길이는 0!

	rObj <- list( zMtx=gEnv$zhF[cStepVal.idx,,drop=F] ,cStepVal=cStepVal )
	return( rObj )
}

bN.phFStepColVal <- function( gEnv ,tgtCol ){

    hLen <- nrow(gEnv$zhF)
    fStepH <- gEnv$zhF[2:hLen ,] - gEnv$zhF[1:(hLen-1) ,]
    fStepH <- rbind( rep(40,ncol(fStepH)) ,fStepH ) # 40이라는 값은 fStep에서 나올 수 없겠지.

    fStepVal <- fStepH[hLen,tgtCol]
    fStepVal.idx <- which( fStepH[1:(hLen-1),tgtCol]==fStepVal )+1      # integer(0)이면 +1 해도 길이는 0!

	rObj <- list( zMtx=gEnv$zhF[fStepVal.idx,,drop=F] ,fStepVal=fStepVal )
	return( rObj )

}

bN.makeHMtxLst <- function( gEnv, allIdxLst, fRstLst ,tgt.scMtx=NULL ,lastH=NULL ){

    hStr <- names(allIdxLst$stdFiltedCnt)
    names(fRstLst) <- hStr

    tStmp <- Sys.time()
    # ----------------------------------------------------
    firstH <- as.integer(hStr[1])
    if( is.null(lastH) ){
        lastH <-as.integer(hStr[length(hStr)])
    }
    fRstLst <- fRstLst[as.character(firstH:lastH)]

    fRstLst.hSpan <- as.integer(names(fRstLst))

    # repace : bUtil.getSfcHLst( stdFiltedCnt ,baseSpan ,fRstLst )
    baseSpan <- 700:lastH
    sfcHLst <- bUtil.getSfcHLst( stdFiltedCnt=allIdxLst$stdFiltedCnt[as.character(baseSpan)] ,baseSpan ,fRstLst )

    scoreMtxLst <- list()
    bScrMtxLst <- list()
    for( sfcIdx in names(sfcHLst) ){    # sfcIdx <- names(sfcHLst)[2]

        scoreMtx.grp.lst <- list( )
        for( hIdx in sfcHLst[[sfcIdx]] ){   # hIdx <- sfcHLst[[sfcIdx]][1]
            stdZoid <- gEnv$zhF[hIdx ,]
            wEnv <- gEnv
            wEnv$zhF <- gEnv$zhF[1:(hIdx-1),]

            fRstLst.w <- fRstLst[as.character(fRstLst.hSpan[fRstLst.hSpan<hIdx])]

            stdMI.grp <- bN.getStdMILst( wEnv ,fRstLst.w )
            filter.grp <- bN.getFilter.grp( stdMI.grp ,tgt.scMtx )

            scoreMtx.grp <- bN.getScoreMtx.grp( matrix(stdZoid,nrow=1) ,filter.grp ,makeInfoStr=T )
            scoreMtx.grp.lst[[sprintf("hIdx:%d",hIdx)]] <- scoreMtx.grp
        }

        # basicHMtxLst : scoreMtx.grp.lst의 데이터를 히스토리 MTX 형태로 변환해서 저장.
        basicHMtxLst <- list()
        scoreMtxNames <- names(scoreMtx.grp.lst[[1]]$basic[[1]]) # 필터링이 아닌 H를 보려하는 것이므로 basic만 다룬다.
        for( nIdx in names(scoreMtx.grp.lst[[1]]$basic) ){ # nIdx<-names(scoreMtx.grp.lst[[1]]$basic)[1]
            mtxLst <- list()
            for( smnIdx in scoreMtxNames ){ # smnIdx <-scoreMtxNames[1]
                scoreMtx <- NULL    ;infoMtx<-NULL
                for( rIdx in seq_len(length(scoreMtx.grp.lst)) ){
                    scoreObj <- scoreMtx.grp.lst[[rIdx]]$basic[[nIdx]][[smnIdx]]
                    scoreMtx <- rbind( scoreMtx ,scoreObj$scoreMtx[1,] )
                    if( any(is.na(scoreObj$scoreMtx[1,])) ){
                        hStr <- sfcHLst[[sfcIdx]][rIdx]
                        colStr <- paste( names(scoreObj$scoreMtx[1,])[which(is.na(scoreObj$scoreMtx[1,]))],collapse=",")
                        k.FLogStr(sprintf("WARN : NA - %s, %s, %s(%s), %s",sfcIdx,nIdx,smnIdx,colStr,hStr)
                                    ,pConsole=T
                                )
                    }
                    if( !is.null(scoreObj$infoMtx) ){
                        infoMtx <- rbind( infoMtx ,scoreObj$infoMtx[1,] )
                    }
                }

                if( !is.null(scoreMtx) )    rownames(scoreMtx) <- sfcHLst[[sfcIdx]]

                if( !is.null(infoMtx) ) rownames(infoMtx) <- sfcHLst[[sfcIdx]]

                mtxLst[[smnIdx]] <- list( scoreMtx=scoreMtx ,infoMtx=infoMtx )
            }
            basicHMtxLst[[nIdx]] <- mtxLst
        }

        bScrHMtxLst <- list()   # bN에서는 bScr이 없음. 그냥 기존 코드 호환성을 위해 남겨놓은 것.

        scoreMtxLst[[sfcIdx]] <- basicHMtxLst
        bScrMtxLst[[sfcIdx]] <- bScrHMtxLst
    }

    mtxInfoLst <- lapply( scoreMtxLst[[1]][[1]] ,function( pLst ){
                        colnames(pLst$scoreMtx)
                    })
    mtxInfoLst.bScr <- lapply( bScrMtxLst[[1]] ,function(pLst){ colnames(pLst$scoreMtx) })
    phaseName <- names(scoreMtxLst[[1]])

    rObj <- list( sfcHLst=sfcHLst ,lastH=lastH
                    ,mtxInfoLst=mtxInfoLst  ,mtxInfoLst.bScr=mtxInfoLst.bScr
                    ,phaseName=phaseName
                    ,scoreMtxLst=scoreMtxLst 
                    ,mfMtxLst=bScrMtxLst
                )

    cnt <- sapply(sfcHLst,length)
    tDiff <- Sys.time() - tStmp
    cat(sprintf("       %d time %.1f%s(tgt.scMtx:%s)   %s\n", lastH
            ,tDiff  ,units(tDiff)   ,ifelse( is.null(tgt.scMtx),"*",paste(tgt.scMtx,collapse=",") )
            ,paste(paste(names(cnt),cnt,sep=":") ,collapse="   " ) 
    ))

    return( rObj )
} # bN.makeHMtxLst()

