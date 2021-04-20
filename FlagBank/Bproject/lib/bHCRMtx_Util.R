
HCR.getMName <- function( workMName ,warn=T ){
    # bFMtx에 해당하는 mName과 bSMtx에 해당하는 mName 분류

    bfMNames <- grep("score*",workMName,value=T)
    bSMNames <- grep("sScore*",workMName,value=T)
    # bfMNames <- intersect( workMName ,names(crScrH$std.grp[[1]]$sfcLate$basic) )
    # bSMNames <- intersect( workMName ,names(crScrH$bS.grp[[1]]$sfcLate$basic) )

    if( warn ){
        haveFlag <- (workMName %in% bfMNames) | (workMName %in% bSMNames)
        if( any(!haveFlag) ){
            missingM <- workMName[!haveFlag]
            cat(sprintf("    Warning!! check mName : %s \n",paste(missingM,collapse=",")))
        }
    }

    return( list(bf=bfMNames ,bS=bSMNames) )
}

HCR.getHCRNames <- function( tgt.scMtx=NULL ){

    tgt.HCRMtx <- character(0)
    if( !is.null(tgt.scMtx) ){
        for( mName in names(bHCRMtxLst) ){
            flag_bf <- all( bHCRMtxLst[[mName]]$wMLst$bf %in% tgt.scMtx )
            flag_bS <- all( bHCRMtxLst[[mName]]$wMLst$bS %in% tgt.scMtx )

            if( flag_bf && flag_bS ){
                tgt.HCRMtx <- c( tgt.HCRMtx ,mName )
            }
        }
    } else {
        tgt.HCRMtx <- names(bHCRMtxLst)
    }

    return( tgt.HCRMtx )
}

HCR.getFilter.grp <- function( tgt.scMtx=NULL ,crScrH ){   # tgt.scMtx
    #   bFMtx.R getFilter.grp() 참고.

    tgt.HCRMtx <- HCR.getHCRNames( tgt.scMtx )

    filterLst <- list()
    for( mName in tgt.HCRMtx ){
        # bHCRMtxLst[tgt.HCRMtx]
        filterLst[[mName]] <- bHCRMtxLst[[mName]]$getFilter( crScrH )
    }

    return( filterLst )

}

HCR.makeHCRMtxLst <- function( crScrH ,allIdxLst ,fRstLst ,lastH=NULL ,tgt.scMtx=NULL ){
    # 파라미터
    #   crScrH <- crScrHTool$getData()
    # 참고
    #   B.makeHMtxLst( gEnv, allIdxLst, fRstLst, lastH=lastH, tgt.scMtx ) 참고

    baseSpan <- as.integer(names(crScrH$stdIdx))
    baseSpan.len <- length(baseSpan)
    if( any(baseSpan[1:(baseSpan.len-1)]!=(baseSpan[2:baseSpan.len]-1)) ){
        return( "Error. check crScrH. missing step found." )
    }

    if( is.null(lastH) ){   lastH <- baseSpan[baseSpan.len]
    } else {
        fndIdx <- which(baseSpan==lastH)
        if( 0<length(fndIdx) ){
            baseSpan <- baseSpan[1:fndIdx]
        } else {    return( "Error. check crScrH range for lastH" ) }
    }

    fRstLst.hSpan <- as.integer(names(fRstLst)[1]):lastH
    fRstLst <- fRstLst[as.character(fRstLst.hSpan)]

    sfcHLst <- bUtil.getSfcHLst( stdFiltedCnt=allIdxLst$stdFiltedCnt[as.character(baseSpan)] ,baseSpan ,fRstLst )
    # filterLst <- HCR.getFilter.grp()  # HCR.getScoreMtx.grp() 내부에서 호출된다.

    crScrH.min <- min(as.integer(names(crScrH$stdIdx))) # crScrH 기록이 존재하는 범위 내로 sfcHLst를 한정한다.
    for( sfcIdx in names(sfcHLst) ){
        availFlag <- sfcHLst[[sfcIdx]] >= crScrH.min
        sfcHLst[[sfcIdx]] <- sfcHLst[[sfcIdx]][availFlag]
    }


    basicLst.empty <- list()    # sfcHLst 에서 데이터가 없는 경우를 위한 디폴트 값 준비.
    filterLst <- HCR.getFilter.grp( tgt.scMtx ,crScrH )
    for( mName in names(filterLst) ){
        cName <- filterLst[[mName]]$cName
        basicLst.empty[[mName]] <- matrix( 0 ,nrow=0 ,ncol=length(cName) ,dimnames=list(NULL,cName) )
    }

    scoreMtxLst <- list()
    for( sfcIdx in names(sfcHLst) ){

        basicLst <- basicLst.empty
        for( hIdx in sfcHLst[[sfcIdx]] ){
            workH <- hIdx-1
            crScrW <- crScrHTool$bySpan(crScrH,workH)
            filterLst <- HCR.getFilter.grp( tgt.scMtx ,crScrW )

            hIdxStr <- as.character(hIdx)
            crScrA <- list( stdIdx=crScrH$stdIdx[hIdxStr] ,std.grp=crScrH$std.grp[hIdxStr] ,bS.grp=crScrH$bS.grp[hIdxStr] )
            mtxGrp <- HCR.getScoreMtx.grp( crScrA ,filterLst ,tgt.scMtx=tgt.scMtx )
            for( mName in names(mtxGrp$basic) ){
                basicLst[[mName]] <- rbind( basicLst[[mName]] ,mtxGrp$basic[[mName]] )
            }
            # scoreLst[[as.character(hIdx)]] <- HCR.getScoreMtx.grp( crScrA ,filterLst ,tgt.scMtx=tgt.scMtx )
        }

        scoreMtxLst[[sfcIdx]] <- list( basic=basicLst )
    }

    rObj <- list( scoreMtxLst=scoreMtxLst ,mInfo=list(hName=names(scoreMtxLst)) )
    rObj$mInfo$mName <- names(scoreMtxLst[[1]]$basic)

    return( rObj )
}

HCR.getScoreMtx.grp <- function( crScrA ,filterLst ,tgt.scMtx=NULL ){
    #   ph 단계가 없음을 유의하자.

    rObj <- list()

    scoreMtxLst <- list()
    for( mName in names(filterLst) ){
        scoreMtxLst[[mName]] <- filterLst[[mName]]$fMtxObj( crScrA )
    }

    rObj$basic <- scoreMtxLst

    return( rObj )
}

HCR.getCutterGrp <- function( hMtxLst_HCR ,fHName ,tgt.scMtx ){
    # bFCust.getFCustGrp( hMtxLst ,tgt.scMtx ) 참고.

    rObj <- list( mInfo=list(hName=fHName) )
    rObj$mInfo$mName <- HCR.getHCRNames( tgt.scMtx )

    cutterLst <- list()
    for( hName in rObj$mInfo$hName ){
        #   hName<-rObj$mInfo$hName[2]  ;mName<-rObj$mInfo$mName[1]

        # <stdCut>
        stdCut <- list()
        for( mName in rObj$mInfo$mName ){
            scoreMtxH <- HCR.HMtxLst_getMtx( hMtxLst_HCR ,hName ,mName )
            stdCut[[mName]] <- HCR.stdCut_rawRow( hName ,mName ,scoreMtxH )
        }

        # <hIdxCut>

        cutterLst[[hName]] <- list( stdCut=stdCut )
    }

    rObj$cutterLst          <- cutterLst
    return( rObj )

}

HCR.HMtxLst_getMtx <- function( hMtxLst_HCR ,hName ,mName ){
    mtx <- NULL
    mtx <- hMtxLst_HCR$scoreMtxLst[[hName]]$basic[[mName]]
    return( mtx )
}

HCR.stdCut_rawRow <- function( hName ,mName ,scoreMtxH ){
    # FCust_stdCut.rawRow  참고.

    rObj <- list( defId=c(hName=hName,mName=mName,pName="N/A(HCR)") )

    hLen <- nrow(scoreMtxH)
    rObj$lastScore <- if(hLen>0) scoreMtxH[hLen,] else NULL
    rObj$available <- TRUE

    if( rObj$available && !is.null(rObj$lastScore) ){
        cfg <- HCRMtxCfg[[mName]]
        if( !is.null(cfg) ){
            rObj$isHard <- cfg$isHard   # 별 의미 없어지는 듯.

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

        cutLst <- list()
        if( !rObj$available ) return( list(cutLst=cutLst,surFlag=!alreadyDead) )

        hardFlag <- FALSE   # 일단은 사용 안함.
        cfg <- HCRMtxCfg[[ rObj$defId["mName"] ]]

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

HCR.cut1 <- function( scoreMtx.grp ,cut.grp ,anaOnly=T ,logger=NULL ){
    # fHName 이 필요한가? cut.grp에 다 있는데..
    # tgt.scMtx이 필요한가? cut.grp 생성 시 이미 tgt.scMtx 제한이 반영되어 있다.

	reportStatus <- function( tStmp ,strWhere ,surFlag ,logger ){
		#	strWhere <- sprintf("[%s,%s] stdLst",hName,mName)
		if( is.null(logger) )	return( NULL )
		tDiff <- Sys.time() - tStmp
		rptStr <- sprintf("    %s  %d/%d  %.1f%s ",strWhere,sum(surFlag),length(surFlag),tDiff,units(tDiff) )
		logger$fLogStr( rptStr )
	}

    scMtxName <- cut.grp$mInfo$mName        # scMtxName

	cutInfoLst <- list()
    datLen <- 1
	if( !anaOnly ){
		cutInfoLst <- NULL

		if( 0<length(scMtxName) ){
			datLen <- nrow(scoreMtx.grp$basic[[1]])
		}
	}


	tStmp <- Sys.time()
	if( !is.null(logger) ) logger$fLogStr("Start", pTime=T ,pAppend=F )

    surFlag <- rep( T ,datLen )
	auxInfoLst <- list( basic=list() ,mf=list() )
    for( hName in cut.grp$mInfo$hName ){ # 

        for( mName in scMtxName ){
            # hName<-cut.grp$mInfo$hName[1]    ;mName <- scMtxName[1]

            scoreMtx <- scoreMtx.grp$basic[[mName]]
            cutObj <- cut.grp$cutterLst[[hName]]$stdCut[[mName]]
            cRst <- cutObj$cut( scoreMtx ,alreadyDead=!surFlag ,anaMode=anaOnly )
            if( !anaOnly ){	surFlag <- surFlag & cRst$surFlag
            } else {
                if( 0<length(cRst$cutLst) ){    # anaOnly일 때는 어차피 datLen이 1이므로 cRst$cutLst 순서는 의미없다.
                    cutInfoLst <- append( cutInfoLst 
                                        ,lapply( cRst$cutLst[[1]]$cLst ,function(p){ c(p$idObjDesc ,info=p$info) } ) 
                                    )
                }
            }
        } # for(mName)

    }

    return( list( surFlag=surFlag ,cutInfoLst=cutInfoLst ,auxInfoLst=auxInfoLst ) )

}

HCR.get_testData.grp <- function( testSpan ,crScrH ,allIdxLst ,fRstLst ,lastH=NULL ,tgt.scMtx=NULL ){
    # gEnv, allIdxLst, fRstLst ,tgt.scMtx=NULL ,lastH=NULL

    tStmp <- Sys.time()
    sfExport("tgt.scMtx")                       ;sfExport("prllLog")
    sfExport("crScrH")  ;sfExport("crScrHTool")
    sfExport("gEnv")    ;sfExport("fRstLst")    ;sfExport("allIdxLst")

    resultLst <- sfLapply(testSpan,function(curHIdx){
        tStmp.prll <- Sys.time()

        wLastH <- curHIdx-1
        wLastSpan <- 1:which(names(fRstLst)==wLastH)
        gEnv.w <- gEnv              ;gEnv.w$zhF <- gEnv$zhF[1:wLastH,]
        allIdxLst.w <- allIdxLst    ;allIdxLst.w$stdFiltedCnt <- allIdxLst$stdFiltedCnt[wLastSpan]
                                    allIdxLst.w$infoMtx <- allIdxLst$infoMtx[wLastSpan,]
        fRstLst.w <- fRstLst[wLastSpan]

        stdIdx <- crScrH$stdIdx[ as.character(curHIdx) ]
        crScrW <- crScrHTool$bySpan(crScrH,wLastH)
        hMtxLst_HCR <- HCR.makeHCRMtxLst( crScrW ,allIdxLst.w ,fRstLst.w ,lastH=wLastH ,tgt.scMtx=tgt.scMtx)

        tDiff <- Sys.time() - tStmp.prll
        prllLog$fLogStr(sprintf("    B.get_testData.grp - hIdx:%d finished %.1f%s",curHIdx,tDiff,units(tDiff)))

        rObj <- list( hIdx=curHIdx ,stdIdx=stdIdx ,hMtxLst_HCR=hMtxLst_HCR )

        return( rObj )
    })
    names(resultLst) <- sapply(resultLst,function(p){ p$hIdx })

    curHMtxLst_HCR.grp <- lapply(resultLst,function(p){ p$hMtxLst_HCR })
    stdIdx.grp <- lapply(resultLst,function(p){ p$stdIdx })

    tDiff <- Sys.time() - tStmp
    cat(sprintf("time : %.1f,%s   \n",tDiff,units(tDiff)))

    rLst <- list(curHMtxLst_HCR.grp=curHMtxLst_HCR.grp ,stdIdx.grp=stdIdx.grp )

    return( rLst )

}



# -- Template for bHCRMtx -----------------------------------------------------------------------------------
HCR.MtxTmpl_szReb <- function( mName ,wMLst ,szColName ,szRowName ){
    #   HCR.getMName(tgt.scMtx)     # bFMtx, bSMtx에 따라 분리추출된 mName
    #   swColName : r.ph r.fCol r.dblHpnFlg e.ph e.fCol e.dblHpnFlg
    #   swRowName : rebCnt / rebDup

	rObj <- list( 	mInfo=c("mName"=mName ,"szRowName"=szRowName ,"szColName"=szColName  ) ,wMLst=wMLst
				)

    rObj$cName <- c( wMLst$bf ,wMLst$bS )

    rObj$fMtxObj <- function( crScr ){
        # crScr : cutRst List. bFMtx/bSMtx 모두 포함된 리스트
        #   std.grp ,bS.grp

        datLen <- length(crScr$std.grp) ;datNam <- names(crScr$std.grp) # datNam은 NULL일 수도 있다.

        scrMtx <- matrix( 0 ,nrow=datLen ,ncol=length(rObj$cName) ,dimnames=list(datNam,rObj$cName) )
        for( rIdx in seq_len(datLen) ){
            std.grp <- crScr$std.grp[[rIdx]]$sfcLate$basic
            bS.grp <- crScr$bS.grp[[rIdx]]$sfcLate$basic

            for( wmName in rObj$wMLst$bf ){
                scMtx.sz <- std.grp[[wmName]]$summ$scMtx.sz
                scrMtx[rIdx,wmName] <- scMtx.sz[ rObj$mInfo["szRowName"] ,rObj$mInfo["szColName"] ]
            }
            for( wmName in rObj$wMLst$bS ){
                scMtx.sz <- bS.grp[[wmName]]$summ$scMtx.sz
                scrMtx[rIdx,wmName] <- scMtx.sz[ rObj$mInfo["szRowName"] ,rObj$mInfo["szColName"] ]
            }
        }

        return( scrMtx )
    }

    return( rObj )
}

HCR.MtxTmpl_rawReb <- function( mName ,wMLst ,colName ,rowName="raw" ){
	rObj <- list( 	mInfo=c("mName"=mName ,"rowName"=rowName ) ,"colName"=colName ,wMLst=wMLst )

    rObj$cName <- c( wMLst$bf ,wMLst$bS )

    rObj$fMtxObj <- function( crScrA ){
        # crScr : cutRst List. bFMtx/bSMtx 모두 포함된 리스트
        #   std.grp ,bS.grp

        datLen <- length(crScrA$std.grp) ;datNam <- names(crScrA$std.grp) # datNam은 NULL일 수도 있다.

        scrMtx <- matrix( 0 ,nrow=datLen ,ncol=length(rObj$cName) ,dimnames=list(datNam,rObj$cName) )
        for( rIdx in seq_len(datLen) ){
            std.grp <- crScrA$std.grp[[rIdx]]$sfcLate$basic
            bS.grp <- crScrA$bS.grp[[rIdx]]$sfcLate$basic

            for( wmName in rObj$wMLst$bf ){
                summMtx <- std.grp[[wmName]]$summ$summMtx
                scrMtx[rIdx,wmName] <- sum( summMtx[ rObj$mInfo["rowName"] ,rObj$colName ] )
            }
            for( wmName in rObj$wMLst$bS ){
                summMtx <- bS.grp[[wmName]]$summ$summMtx
                scrMtx[rIdx,wmName] <- sum( summMtx[ rObj$mInfo["rowName"] ,rObj$colName ] )
            }
        }

        return( scrMtx )
    }

    return( rObj )
}

HCR.MtxTmpl_rebSz <- function( mName=mName ,wMLst=rObj$wMLst ,crScrH ,szCol ){
    #   szCol : r.ph r.fCol r.dblHpnFlg        e.ph e.fCol e.dblHpnFlg

	rObj <- list( 	mInfo=c("mName"=mName ,"szRowName"="rebCnt" ) ,wMLst=wMLst ,szCol=szCol
				)

    rObj$cName <- c( wMLst$bf ,wMLst$bS )

    rObj$szLst <- NULL
    crScrH.len <- length(crScrH$std.grp)
    if( 0<crScrH.len ){
        lastHStr <- names(crScrH$std.grp)[crScrH.len]
        rObj$mInfo["lastH"] <- lastHStr
        
        szLst_bf <- lapply( crScrH$std.grp[[lastHStr]]$sfcLate$basic[ rObj$wMLst$bf ] ,function( crObj ){
            crObj$summ$scMtx.sz[rObj$mInfo["szRowName"],szCol]
        })
        szLst_bS <- lapply( crScrH$bS.grp[[lastHStr]]$sfcLate$basic[ rObj$wMLst$bS ] ,function( crObj ){
            crObj$summ$scMtx.sz[rObj$mInfo["szRowName"],szCol]
        })
        rObj$szLst <- append( szLst_bf ,szLst_bS )

        allZeroFlag <- sapply(rObj$szLst,function(pObj){ all(pObj==0) })
        if( all(allZeroFlag) ){
            rObj$szLst <- NULL
        }
    }

    rObj$fMtxObj <- function( crScrA ){
        datLen <- length(crScrA$std.grp) ;datNam <- names(crScrA$std.grp) # datNam은 NULL일 수도 있다.

        scrMtx <- matrix( 0 ,nrow=datLen ,ncol=length(rObj$cName) ,dimnames=list(datNam,rObj$cName) )
        if( is.null(rObj$szLst) ){
            return( scrMtx )
        }

        for( rIdx in seq_len(datLen) ){
            std.grp <- crScrA$std.grp[[rIdx]]$sfcLate$basic
            bS.grp <- crScrA$bS.grp[[rIdx]]$sfcLate$basic

            for( wmName in rObj$wMLst$bf ){
                szA <- std.grp[[wmName]]$summ$scMtx.sz[rObj$mInfo["szRowName"] ,rObj$szCol]
                matFlag <- szA == rObj$szLst[[wmName]]
                if( all(matFlag) ){
                    matFlag <- matFlag & (szA>0)
                    scrMtx[rIdx,wmName] <- sum( szA[matFlag] )
                }
            }
            for( wmName in rObj$wMLst$bS ){
                szA <- bS.grp[[wmName]]$summ$scMtx.sz[rObj$mInfo["szRowName"] ,rObj$szCol]
                matFlag <- szA == rObj$szLst[[wmName]]
                if( all(matFlag) ){
                    matFlag <- matFlag & (szA>0)
                    scrMtx[rIdx,wmName] <- sum( szA[matFlag] )
                }
            }
        }

        return( scrMtx )
    }

    return( rObj )

}

HCR.MtxTmpl_rebRaw <- function( mName=mName ,wMLst=rObj$wMLst ,crScrH ,compCol ,rowName="raw" ){
    #   rowName : raw / evt
    #   compCol : all ph fCol phReb xyCnt.fCol xyCnt.phase

	rObj <- list( 	mInfo=c("mName"=mName ,"rowName"=rowName ) ,wMLst=wMLst ,compCol=compCol
				)
    rObj$cName <- c( wMLst$bf ,wMLst$bS )

    rObj$rawLst <- NULL
    crScrH.len <- length(crScrH$std.grp)
    if( 0<crScrH.len ){
        lastHStr <- names(crScrH$std.grp)[crScrH.len]
        rObj$mInfo["lastH"] <- lastHStr

        rawLst_bf <- lapply( crScrH$std.grp[[lastHStr]]$sfcLate$basic[ rObj$wMLst$bf ] ,function( crObj ){
            crObj$summ$summMtx[rObj$mInfo["rowName"],compCol]
        })
        rawLst_bS <- lapply( crScrH$bS.grp[[lastHStr]]$sfcLate$basic[ rObj$wMLst$bS ] ,function( crObj ){
            crObj$summ$summMtx[rObj$mInfo["rowName"],compCol]
        })
        rObj$rawLst <- append( rawLst_bf ,rawLst_bS )
        
        allZeroFlag <- sapply(rObj$rawLst,function(pObj){ all(pObj==0) })
        if( all(allZeroFlag) ){
            rObj$rawLst <- NULL
        }
    }

    rObj$fMtxObj <- function( crScrA ){
        datLen <- length(crScrA$std.grp) ;datNam <- names(crScrA$std.grp) # datNam은 NULL일 수도 있다.

        scrMtx <- matrix( 0 ,nrow=datLen ,ncol=length(rObj$cName) ,dimnames=list(datNam,rObj$cName) )
        if( is.null(rObj$rawLst) ){
            return( scrMtx )
        }

        for( rIdx in seq_len(datLen) ){
            std.grp <- crScrA$std.grp[[rIdx]]$sfcLate$basic
            bS.grp <- crScrA$bS.grp[[rIdx]]$sfcLate$basic

            for( wmName in rObj$wMLst$bf ){
                szA <- std.grp[[wmName]]$summ$summMtx[rObj$mInfo["rowName"] ,rObj$compCol]
                matFlag <- szA == rObj$rawLst[[wmName]]
                if( all(matFlag) ){
                    matFlag <- matFlag & (szA>0)
                    scrMtx[rIdx,wmName] <- sum( szA[matFlag] )
                }
            }
            for( wmName in rObj$wMLst$bS ){
                szA <- bS.grp[[wmName]]$summ$summMtx[rObj$mInfo["rowName"] ,rObj$compCol]
                matFlag <- szA == rObj$rawLst[[wmName]]
                if( all(matFlag) ){
                    matFlag <- matFlag & (szA>0)
                    scrMtx[rIdx,wmName] <- sum( szA[matFlag] )
                }
            }
        }

        return( scrMtx )
    }

    return( rObj )

}

#   ph별 reb 체크.
HCR.MtxTmpl_phRebCnt_raw <- function( mName ,wMName ,crScrH ,mGrp ){
    # mGrp : std.grp bS.grp

        # rebMtx.ph                   colVal1 colVal3 colVal6 remPair zw cSCVal1 cSCVal2 cSCVal3 cSCVal4 cSCVal5
        #                 rebFlag.raw       0       0       0       0  0       0       0       0       0       0
        #                 hpn.raw           0       1       0       0  0       0       0       0       0       3

	rObj <- list( mInfo=c("mName"=mName ,"mGrp"=mGrp ) ,wMName=wMName )

    rObj$getRebMtx <- function( crScr ,wMName ,cName ){
        # wMName<-rObj$wMName    ;cName<-rObj$cName
        rMtx <- matrix( 0 ,nrow=length(wMName) ,ncol=length(cName) ,dimnames=list(wMName,cName) )
        for( phIdx in cName ){
            rMtx[,phIdx] <- sapply( crScr$sfcLate$basic[wMName] ,function(mObj){
                if( 0==mObj$raw$rebMtx.ph["rebFlag.raw",phIdx] ){
                    return( 0 )
                } else {
                    return( mObj$raw$rebMtx.ph["rebFlag.raw",phIdx] )
                    #   return( mObj$raw$rebMtx.ph["hpn.raw",phIdx] )
                }
                # return( mObj$raw$rebMtx.ph["rebFlag.raw",phIdx] )
            })
        }
        return( rMtx )
    }

    rObj$cName <- crScrH$mInfo[[mGrp]]$pName
    rObj$available <- FALSE
    if( 0<length(crScrH[[mGrp]]) ){
        rObj$lastMtx <- rObj$getRebMtx( crScrH[[mGrp]][[length(crScrH$stdIdx)]] ,wMName=rObj$wMName ,cName=rObj$cName )
        rObj$availPh <- !apply(rObj$lastMtx ,2 ,function(rDat){ all(0==rDat) })   # 모두 0인 ph는 비교작업이 필요 없으니까..
        rObj$available <- any(rObj$availPh)

        # lastMtx : reb 발생 여부.(reb발생한 pName의 hpn수를 따지면 너무 경우의 수가 적을 듯 하다.)
        # fMtxObj() 에서는 lastMtx에서 reb 일치가 일어난 mName 수를 score로 계산.
    }

    rObj$fMtxObj <- function( crScrA ){
        #   debug  crScrA <- crScrH
        #       crScrA 에서 mInfo를 찾으려하지 말 것.(데이터가 없는 crScrA도 있을 수 있으므로.)
        datLen <- length(crScrA$std.grp) ;datNam <- names(crScrA$std.grp) # datNam은 NULL일 수도 있다.

        scrMtx.colNames <- rObj$cName
        scrMtx <- matrix( 0 ,nrow=datLen ,ncol=length(rObj$cName) ,dimnames=list(datNam,rObj$cName) )
        if( !rObj$available ){
            return( scrMtx )
        }

        for( rIdx in seq_len(datLen) ){
            # std.grp <- crScrA$std.grp[[rIdx]]$sfcLate$basic
            # bS.grp <- crScrA$bS.grp[[rIdx]]$sfcLate$basic

            crScr <- crScrA[[ rObj$mInfo["mGrp"] ]][[rIdx]]
            mtxA <- rObj$getRebMtx( crScr ,wMName=rObj$wMName ,cName=rObj$cName )

            for( phIdx in rObj$cName ){
                if( !rObj$availPh[phIdx] ) next

                matchF <- rObj$lastMtx[,phIdx]==mtxA[,phIdx]
                if( all(matchF) ){
                    scrMtx[rIdx,phIdx] <- sum( mtxA[,phIdx]>0 )   # hpnCnt로 할까... hpnMax로 할까.
                }
            }
        }

        return( scrMtx )
    }

    return( rObj )

}

HCR.MtxTmpl_phRebCnt_sz <- function( mName ,wMName ,crScrH ,mGrp ){
    # $rawSz$ph
    #         basic nextZW nextQuo10 nextBin nextRebNum nextCStepBin 
    #     mat     0      0         0       0          0            0 
    #     hpn     4      2         2       2          1            2 

	rObj <- list( mInfo=c("mName"=mName ,"mGrp"=mGrp ) ,wMName=wMName )

    rObj$getRebMtx <- function( crScr ,wMName ,cName ){
        # wMName<-rObj$wMName    ;cName<-rObj$cName
        rMtx <- matrix( 0 ,nrow=length(wMName) ,ncol=length(cName) ,dimnames=list(wMName,cName) )
        for( phIdx in cName ){
            rMtx[,phIdx] <- sapply( crScr$sfcLate$basic[wMName] ,function(mObj){
                if( 0==mObj$rawSz$ph["mat",phIdx] ){
                    return( 0 )
                } else {
                    return( mObj$rawSz$ph["mat",phIdx] )
                    #   return( mObj$rawSz$ph["hpn",phIdx] )
                }
                # return( mObj$rawSz$ph["mat",phIdx] )
            })
        }
        return( rMtx )
    }

    rObj$cName <- crScrH$mInfo[[mGrp]]$pName
    rObj$available <- FALSE
    if( 0<length(crScrH[[mGrp]]) ){
        rObj$lastMtx <- rObj$getRebMtx( crScrH[[mGrp]][[length(crScrH$stdIdx)]] ,wMName=rObj$wMName ,cName=rObj$cName )
        rObj$availPh <- !apply(rObj$lastMtx ,2 ,function(rDat){ all(0==rDat) })   # 모두 0인 ph는 비교작업이 필요 없으니까..
        rObj$available <- any(rObj$availPh)

        # lastMtx : reb 발생 여부.(reb발생한 pName의 hpn수를 따지면 너무 경우의 수가 적을 듯 하다.)
        # fMtxObj() 에서는 lastMtx에서 reb 일치가 일어난 mName 수를 score로 계산.
    }

    rObj$fMtxObj <- function( crScrA ){
        #   debug  crScrA <- crScrH
        #       crScrA 에서 mInfo를 찾으려하지 말 것.(데이터가 없는 crScrA도 있을 수 있으므로.)
        datLen <- length(crScrA$std.grp) ;datNam <- names(crScrA$std.grp) # datNam은 NULL일 수도 있다.

        scrMtx.colNames <- rObj$cName
        scrMtx <- matrix( 0 ,nrow=datLen ,ncol=length(rObj$cName) ,dimnames=list(datNam,rObj$cName) )
        if( !rObj$available ){
            return( scrMtx )
        }

        for( rIdx in seq_len(datLen) ){
            # std.grp <- crScrA$std.grp[[rIdx]]$sfcLate$basic
            # bS.grp <- crScrA$bS.grp[[rIdx]]$sfcLate$basic

            crScr <- crScrA[[ rObj$mInfo["mGrp"] ]][[rIdx]]
            mtxA <- rObj$getRebMtx( crScr ,wMName=rObj$wMName ,cName=rObj$cName )

            for( phIdx in rObj$cName ){
                if( !rObj$availPh[phIdx] ) next

                matchF <- rObj$lastMtx[,phIdx]==mtxA[,phIdx]
                if( all(matchF) ){
                    scrMtx[rIdx,phIdx] <- sum( mtxA[,phIdx]>0 )   # hpnCnt로 할까... hpnMax로 할까.
                }
            }
        }

        return( scrMtx )
    }

    return( rObj )

}



HCR.MtxTmpl_crScrNnx <- function( mName ,wMName ,rawF=T ){ # crScrN01R/crScrN01E
    #   rawF : raw or evt

	rObj <- list( 	mInfo=c("mName"=mName  ) ,wMLst=wMLst   ,rawF=rawF
				)

    rObj$cName <- c(	 "hpn0"             # 모든 ph에서 hpn이 없는 mName 수.
                        ,"ph"   ,"fCol" ,"phReb"    # hpn Cnt, ph/fCol reb from last H, ph reb from left ph
                        ,"xyCnt.fCol" ,"xyCnt.phase"
                        ,"ph_Reb"   ,"fCol_Reb" ,"phReb_Reb"    ,"xyCnt.fCol_Reb"   ,"xyCnt.phase_Reb"    # from summMtx.Reb
                        ,"ph_sz" ,"fCol_sz" ,"dblHpnFlg_sz" ,"ph_szDup" ,"fCol_szDup" ,"dblHpnFlg_szDup"    # from scMtx.sz
    )

    rObj$getScore <- function( crLst ){
        score <- rep( 0 ,length(rObj$cName) )   ;names(score)<-rObj$cName

        rName <- ifelse( rObj$rawF ,"hpn.raw" ,"hpn.evt" )
        hpnCnt <- sapply( crLst ,function(crObj){ sum(crObj$raw$rebMtx.ph[rName,]>0) })
        score["hpn0"] <- sum(hpnCnt==0)

        # -----------------------------------------------------------------------------------------
        rName <- ifelse( rObj$rawF ,"raw" ,"evt" )
        rebCnt <- sapply( crLst ,function(crObj){ sum(crObj$summ$summMtx[rName,"ph"]>0) })
        score["ph"] <- sum(rebCnt)
        rebCnt <- sapply( crLst ,function(crObj){ sum(crObj$summ$summMtx[rName,"fCol"]>0) })
        score["fCol"] <- sum(rebCnt)
        rebCnt <- sapply( crLst ,function(crObj){ sum(crObj$summ$summMtx[rName,"phReb"]>0) })
        score["phReb"] <- sum(rebCnt)
        rebCnt <- sapply( crLst ,function(crObj){ sum(crObj$summ$summMtx[rName,"xyCnt.fCol"]>0) })
        score["xyCnt.fCol"] <- sum(rebCnt)
        rebCnt <- sapply( crLst ,function(crObj){ sum(crObj$summ$summMtx[rName,"xyCnt.phase"]>0) })
        score["xyCnt.phase"] <- sum(rebCnt)

        rebCnt <- sapply( crLst ,function(crObj){ sum(crObj$summ$summMtx.reb[rName,"ph"]>0) })
        score["ph_Reb"] <- sum(rebCnt)
        rebCnt <- sapply( crLst ,function(crObj){ sum(crObj$summ$summMtx.reb[rName,"fCol"]>0) })
        score["fCol_Reb"] <- sum(rebCnt)
        rebCnt <- sapply( crLst ,function(crObj){ sum(crObj$summ$summMtx.reb[rName,"phReb"]>0) })
        score["phReb_Reb"] <- sum(rebCnt)
        rebCnt <- sapply( crLst ,function(crObj){ sum(crObj$summ$summMtx.reb[rName,"xyCnt.fCol"]>0) })
        score["xyCnt.fCol_Reb"] <- sum(rebCnt)
        rebCnt <- sapply( crLst ,function(crObj){ sum(crObj$summ$summMtx.reb[rName,"xyCnt.phase"]>0) })
        score["xyCnt.phase_Reb"] <- sum(rebCnt)

        # -----------------------------------------------------------------------------------------
        cName <- ifelse( rObj$rawF ,"r.ph" ,"e.ph" )
        rebCnt <- sapply( crLst ,function(crObj){ sum(crObj$summ$scMtx.sz["rebCnt",cName]>0) })
        score["ph_sz"] <- sum(rebCnt)

        cName <- ifelse( rObj$rawF ,"r.fCol" ,"e.fCol" )
        rebCnt <- sapply( crLst ,function(crObj){ sum(crObj$summ$scMtx.sz["rebCnt",cName]>0) })
        score["fCol_sz"] <- sum(rebCnt)

        cName <- ifelse( rObj$rawF ,"r.dblHpnFlg" ,"e.dblHpnFlg" )
        rebCnt <- sapply( crLst ,function(crObj){ sum(crObj$summ$scMtx.sz["rebCnt",cName]>0) })
        score["dblHpnFlg_sz"] <- sum(rebCnt)

        cName <- ifelse( rObj$rawF ,"r.ph" ,"e.ph" )
        rebCnt <- sapply( crLst ,function(crObj){ sum(crObj$summ$scMtx.sz["rebDup",cName]>0) })
        score["ph_szDup"] <- sum(rebCnt)

        cName <- ifelse( rObj$rawF ,"r.fCol" ,"e.fCol" )
        rebCnt <- sapply( crLst ,function(crObj){ sum(crObj$summ$scMtx.sz["rebDup",cName]>0) })
        score["fCol_szDup"] <- sum(rebCnt)

        cName <- ifelse( rObj$rawF ,"r.dblHpnFlg" ,"e.dblHpnFlg" )
        rebCnt <- sapply( crLst ,function(crObj){ sum(crObj$summ$scMtx.sz["rebDup",cName]>0) })
        score["dblHpnFlg_szDup"] <- sum(rebCnt)

        return( score )
    }

    rObj$fMtxObj <- function( crScr ){
        # crScr(crScrA) : cutRst List. bFMtx/bSMtx 모두 포함된 리스트
        #   std.grp ,bS.grp

        datLen <- length(crScr$std.grp) ;datNam <- names(crScr$std.grp) # datNam은 NULL일 수도 있다.

        scrMtx <- matrix( 0 ,nrow=datLen ,ncol=length(rObj$cName) ,dimnames=list(datNam,rObj$cName) )
        for( rIdx in seq_len(datLen) ){
            std.grp <- crScr$std.grp[[rIdx]]$sfcLate$basic
            bS.grp <- crScr$bS.grp[[rIdx]]$sfcLate$basic

            if( 0<length(rObj$wMLst$bf) ){
                scrMtx[rIdx,] <- scrMtx[rIdx,] + rObj$getScore( crLst=std.grp[rObj$wMLst$bf] )
            }            
            if( 0<length(rObj$wMLst$bS) ){
                scrMtx[rIdx,] <- scrMtx[rIdx,] + rObj$getScore( crLst=bS.grp[rObj$wMLst$bS] )
            }
        }

        return( scrMtx )
    }

    return( rObj )
}


HCR.MtxTmpl_crScrNnPhEvt <- function( mName ,wMName ){
    # QQE : tempMtx에 대해서... 
    #   evt lev1, lev2 갯수가 흔한 건 자르고 있는데, reb 체크를 위해서는 자르지 않는 게 효율적일 듯.
    #   결과 데이터 확인 후 결정.

	rObj <- list( 	mInfo=c("mName"=mName  ) ,wMLst=wMLst
				)

    rObj$cName <- c(  "e3Max" ,"e3MCnt" ,"e2Max" ,"e2MCnt" ,"e1Max" ,"e1MCnt"   # Cnt는 evt가 2번 이상 발생한 pName 수
                        ,"rebRawMax" ,"rebRawMCnt" ,"rebEvtMax" ,"rebEvtMCnt"
    )

    rObj$getScore <- function( crLst ){

        score <- rep( 0 ,length(rObj$cName) )   ;names(score)<-rObj$cName

        phNameAll <- colnames(crLst[[1]]$raw$rebMtx.ph)
        #   중간 계산 및 디버깅용 데이터 매트릭스
        tempCName <- c("e1","e2","e3" ,"rebRCnt" ,"rebECnt" )
        tempMtx <- matrix( 0 ,nrow=length(tempCName) ,ncol=length(phNameAll) ,dimnames=list(tempCName,phNameAll) )

        for( mIdx in names(crLst) ){
            evtHpnLevMtx <- crLst[[mIdx]]$raw$evtHpnLevMtx
                #      basic ZW Quo10 Bin RebNum CBin FBin cv1 cv2 cv3 cv4 cv5 cv6
                # lev1     0  0     1   0      0    0    0   0   0   0   0   0   0
                # lev2     0  0     1   0      0    0    0   0   0   0   0   0   0
                # lev3     0  0     0   0      0    0    0   0   0   1   1   0   0
            
            # evt lev1은 너무 흔해서, 하나의 phase 내에서 lev1 2개 이하는 자르자.
            lev1 <- evtHpnLevMtx["lev1",]
            evtHpnLevMtx["lev1",] <- ifelse(lev1>2,lev1-2,0)

            # evt lev2도 1개 이하는 자르자.
            lev2 <- evtHpnLevMtx["lev2",]
            evtHpnLevMtx["lev2",] <- ifelse(lev2>1,lev2-1,0)

            for( pIdx in phNameAll ){  # nIdx == pName
                tempMtx["e1",pIdx] <- tempMtx["e1",pIdx] + evtHpnLevMtx["lev1",pIdx]
                tempMtx["e2",pIdx] <- tempMtx["e2",pIdx] + evtHpnLevMtx["lev2",pIdx]
                tempMtx["e3",pIdx] <- tempMtx["e3",pIdx] + evtHpnLevMtx["lev3",pIdx]

                rebMtx.ph <- crLst[[mIdx]]$raw$rebMtx.ph
                tempMtx["rebRCnt",pIdx] <- tempMtx["rebRCnt",pIdx] + rebMtx.ph["rebFlag.raw",pIdx]
                tempMtx["rebECnt",pIdx] <- tempMtx["rebECnt",pIdx] + rebMtx.ph["rebFlag.evt",pIdx]
            }
        }

        score["e1Max"]  <- max(tempMtx["e1",])
        score["e1MCnt"] <- sum(tempMtx["e1",]>1)
        score["e2Max"]  <- max(tempMtx["e2",])
        score["e2MCnt"] <- sum(tempMtx["e2",]>1)
        score["e3Max"]  <- max(tempMtx["e3",])
        score["e3MCnt"] <- sum(tempMtx["e3",]>1)

        score["rebRawMax"]  <- max(tempMtx["rebRCnt",])
        score["rebRawMCnt"] <- sum(tempMtx["rebRCnt",]>1)
        score["rebEvtMax"]  <- max(tempMtx["rebECnt",])
        score["rebEvtMCnt"] <- sum(tempMtx["rebECnt",]>1)

        # MCnt가 1이면 Max 컬럼값과 중복의미가 되므로 0으로 없앤다.    tempMtx에서의  트리밍과 중복되는 거 같은데..?
        cName <- c("e1MCnt","e2MCnt","e3MCnt","rebRawMCnt","rebEvtMCnt")
        score[cName] <- ifelse( 2>score[cName] ,0 ,score[cName] )

        return( score )
    }

    rObj$fMtxObj <- function( crScr ){
        # crScr(crScrA) : cutRst List. bFMtx/bSMtx 모두 포함된 리스트
        #   std.grp ,bS.grp

        datLen <- length(crScr$std.grp) ;datNam <- names(crScr$std.grp) # datNam은 NULL일 수도 있다.

        scrMtx <- matrix( 0 ,nrow=datLen ,ncol=length(rObj$cName) ,dimnames=list(datNam,rObj$cName) )
        for( rIdx in seq_len(datLen) ){
            std.grp <- crScr$std.grp[[rIdx]]$sfcLate$basic
            bS.grp <- crScr$bS.grp[[rIdx]]$sfcLate$basic

            if( 0<length(rObj$wMLst$bf) ){
                scrMtx[rIdx,] <- scrMtx[rIdx,] + rObj$getScore( crLst=std.grp[rObj$wMLst$bf] )
            }            
            if( 0<length(rObj$wMLst$bS) ){
                scrMtx[rIdx,] <- scrMtx[rIdx,] + rObj$getScore( crLst=bS.grp[rObj$wMLst$bS] )
            }
        }

        return( scrMtx )
    }

    return( rObj )
}


HCR.MtxTmpl_crScrNnSum <- function( mName ,wMName ){

	rObj <- list( 	mInfo=c("mName"=mName  ) ,wMLst=wMLst
				)

    rObj$cName <- c(	 "summSumRaw"   ,"summSumEvt"       ,"summSumOthRaw"    ,"summSumOthEvt"
                        ,"summSumRebRaw","summSumRebEvt"    ,"summSumRebOthRaw" ,"summSumRebOthEvt"
                        ,"szSumRebCnt"  ,"szSumRebDup"
    )

    rObj$getScore <- function( crLst ){

        score <- rep( 0 ,length(rObj$cName) )   ;names(score)<-rObj$cName

        summMtx <- NULL ;summMtx.reb <- NULL    ;scMtx.sz <- NULL
        for( mName in names(crLst) ){
            rawObj <- crLst[[mName]]$raw
            summObj <- crLst[[mName]]$summ

            if( is.null(summMtx) ){    summMtx <- summObj$summMtx
            } else {    summMtx <- summMtx + summObj$summMtx                }
            if( is.null(summMtx.reb) ){    summMtx.reb <- summObj$summMtx.reb
            } else {    summMtx.reb <- summMtx.reb + summObj$summMtx.reb    }

            if( is.null(scMtx.sz) ){    scMtx.sz <- summObj$scMtx.sz
            } else {    scMtx.sz <- scMtx.sz + summObj$scMtx.sz    }
        }

        score["summSumRaw"]     <- sum( summMtx["raw",c("ph","fCol")] )
        score["summSumEvt"]     <- sum( summMtx["evt",c("ph","fCol")] )
        score["summSumOthRaw"]  <- sum( summMtx["raw",c("phReb","xyCnt.fCol","xyCnt.phase")] )
        score["summSumOthEvt"]  <- sum( summMtx["evt",c("phReb","xyCnt.fCol","xyCnt.phase")] )

        score["summSumRebRaw"]      <- sum( summMtx.reb["raw",c("ph","fCol")] )
        score["summSumRebEvt"]      <- sum( summMtx.reb["evt",c("ph","fCol")] )
        score["summSumRebOthRaw"]   <- sum( summMtx.reb["raw",c("phReb","xyCnt.fCol","xyCnt.phase")] )
        score["summSumRebOthEvt"]   <- sum( summMtx.reb["evt",c("phReb","xyCnt.fCol","xyCnt.phase")] )

        score["szSumRebCnt"]    <- sum( scMtx.sz["rebCnt",c("r.ph","r.fCol","e.ph","e.fCol")] )
        score["szSumRebDup"]    <- sum( scMtx.sz["rebDup",c("r.ph","r.fCol","e.ph","e.fCol")] )

        return( score )
    }

    rObj$fMtxObj <- function( crScr ){
        # crScr(crScrA) : cutRst List. bFMtx/bSMtx 모두 포함된 리스트
        #   std.grp ,bS.grp

        datLen <- length(crScr$std.grp) ;datNam <- names(crScr$std.grp) # datNam은 NULL일 수도 있다.

        scrMtx <- matrix( 0 ,nrow=datLen ,ncol=length(rObj$cName) ,dimnames=list(datNam,rObj$cName) )
        for( rIdx in seq_len(datLen) ){
            std.grp <- crScr$std.grp[[rIdx]]$sfcLate$basic
            bS.grp <- crScr$bS.grp[[rIdx]]$sfcLate$basic

            if( 0<length(rObj$wMLst$bf) ){
                scrMtx[rIdx,] <- scrMtx[rIdx,] + rObj$getScore( crLst=std.grp[rObj$wMLst$bf] )
            }            
            if( 0<length(rObj$wMLst$bS) ){
                scrMtx[rIdx,] <- scrMtx[rIdx,] + rObj$getScore( crLst=bS.grp[rObj$wMLst$bS] )
            }
        }

        return( scrMtx )
    }

    return( rObj )
}

HCR.MtxTmpl_crScrNnSum <- function( mName ,wMName ){

	rObj <- list( 	mInfo=c("mName"=mName  ) ,wMLst=wMLst
				)

    rObj$cName <- c(  "sumTotHpn" ,"sumTot1" ,"sumTot2" ,"sumTot3"
    )

    rObj$getScore <- function( crLst ){

        score <- rep( 0 ,length(rObj$cName) )   ;names(score)<-rObj$cName

        clM <- bUtil.getClM_cutRst1Score( cutRst1=cutRst1Score$aLst[[aIdx]] ,cfgLst=scoreMtxCfg ,mNameGrp=rObj$mName ,fHName )
        crScrMtx[aIdx,"sumTotHpn"]  <- sum(clM$sumTot>0)
        crScrMtx[aIdx,"sumTot1"]  <- sum(clM$sumTot==1)
        crScrMtx[aIdx,"sumTot2"]  <- sum(clM$sumTot==2)
        crScrMtx[aIdx,"sumTot3"]  <- sum(clM$sumTot==3)


        return( score )
    }

    rObj$fMtxObj <- function( crScr ){
        # crScr(crScrA) : cutRst List. bFMtx/bSMtx 모두 포함된 리스트
        #   std.grp ,bS.grp

        datLen <- length(crScr$std.grp) ;datNam <- names(crScr$std.grp) # datNam은 NULL일 수도 있다.
        fHName <- names(crScr$std.grp[[1]])

        scrMtx <- matrix( 0 ,nrow=datLen ,ncol=length(rObj$cName) ,dimnames=list(datNam,rObj$cName) )
        for( rIdx in seq_len(datLen) ){
            # std.grp <- crScr$std.grp[[rIdx]]$sfcLate$basic
            # bS.grp <- crScr$bS.grp[[rIdx]]$sfcLate$basic

            if( 0<length(rObj$wMLst$bf) ){
                # scrMtx[rIdx,] <- scrMtx[rIdx,] + rObj$getScore( crLst=std.grp[rObj$wMLst$bf] )
                clM <- bUtil.getClM_cutRst1Score( cutRst1=crScr$std.grp[[rIdx]] ,cfgLst=scoreMtxCfg ,mNameGrp=rObj$wMLst$bf ,fHName=fHName )
                scrMtx[rIdx,"sumTotHpn"]<- scrMtx[rIdx,"sumTotHpn"] + sum(clM$sumTot>0)
                scrMtx[rIdx,"sumTot1"]  <- scrMtx[rIdx,"sumTot1"]   + sum(clM$sumTot==1)
                scrMtx[rIdx,"sumTot2"]  <- scrMtx[rIdx,"sumTot2"]   + sum(clM$sumTot==2)
                scrMtx[rIdx,"sumTot3"]  <- scrMtx[rIdx,"sumTot3"]   + sum(clM$sumTot==3)
            }            
            if( 0<length(rObj$wMLst$bS) ){
                # scrMtx[rIdx,] <- scrMtx[rIdx,] + rObj$getScore( crLst=bS.grp[rObj$wMLst$bS] )
                clM <- bUtil.getClM_cutRst1Score( cutRst1=crScr$bS.grp[[rIdx]] ,cfgLst=bsScoreMtxCfg ,mNameGrp=rObj$wMLst$bS ,fHName=fHName )
                scrMtx[rIdx,"sumTotHpn"]<- scrMtx[rIdx,"sumTotHpn"] + sum(clM$sumTot>0)
                scrMtx[rIdx,"sumTot1"]  <- scrMtx[rIdx,"sumTot1"]   + sum(clM$sumTot==1)
                scrMtx[rIdx,"sumTot2"]  <- scrMtx[rIdx,"sumTot2"]   + sum(clM$sumTot==2)
                scrMtx[rIdx,"sumTot3"]  <- scrMtx[rIdx,"sumTot3"]   + sum(clM$sumTot==3)
            }
        }

        return( scrMtx )
    }

    return( rObj )
}


