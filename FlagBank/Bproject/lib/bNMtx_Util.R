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

    # 가용 bFMtxLstMFltLst 추출.
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

                # bFMtxExtFltLst 자체는 bf, bN 호환될테니까..(pName만 다를 뿐, mName은 동일하므로.)
                if( is.null(bFMtxExtFltLst[[mName]]) ){   stdCutExt[[pName]] <- list()
                } else {
                    fltLst <- list()
                    for( nIdx in names(bFMtxExtFltLst[[mName]]) ){
                        # bFMtxExtFltLst[[mName]][[nIdx]] 는 FCust_stdCutExt.rawRow() 내부에서 이용됨.
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
                for( nIdx in names(bFMtxMFltLst) ){ # nIdx <- names(bFMtxMFltLst)[1]
                    mtxMaker <- bFMtxMFltLst[[nIdx]]( tgt.scMtx )
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
}
bN.stdCutExt_rawRow <- function( hName ,mName ,pName ,scoreMtxH ,fltName ){
    # hName ,mName ,pName ,scoreMtxObj$scoreMtx ,fltName=nIdx
}
bN.stdCut_hIdx      <- function(  hName ,mName ,mtxLst ){
    # hName ,mName ,mtxLst=hIdxObj[[hName]][[mName]]
}



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

