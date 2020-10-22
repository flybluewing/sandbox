
B.makeHMtxLst <- function( gEnv, allIdxLst, fRstLst ,tgt.scMtx=NULL ,lastH=NULL ){

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

    baseSpan <- 700:lastH
    stdFiltedCnt <- allIdxLst$stdFiltedCnt[as.character(baseSpan)]

    # ----------------------------------------------------
    sfcHLst <- list(    sfcLate= baseSpan[length(baseSpan)] - 20:0
                        ,sfc0=as.integer(names(stdFiltedCnt)[stdFiltedCnt==0])
                        ,sfc1=as.integer(names(stdFiltedCnt)[stdFiltedCnt==1])
                        ,sfc2=as.integer(names(stdFiltedCnt)[stdFiltedCnt==2])
                    )

    stdFilter <- c("D0000.A","A0100.A","AP000.E")   # "AR000.B","AL000A","C1000.A"
    for( sfnIdx in stdFilter ){
        #   sfnIdx <- "D0000.A"
        hSpan <- baseSpan[sapply( fRstLst[as.character(baseSpan)] ,function(p){ sfnIdx %in% p } )]
        hSpan.NG <- hSpan+1
        hSpan.NG <- hSpan.NG[hSpan.NG<=lastH]
        sfcHLst[[sprintf("NG%s",sfnIdx)]] <- hSpan.NG
    }
    lenMax <- 20
    for( hName in names(sfcHLst) ){ # hLst 범위는 20 이내로 하자.
        hLen <- length(sfcHLst[[hName]])
        if( lenMax < hLen ){
            sfcHLst[[hName]] <- sfcHLst[[hName]][ (hLen-lenMax+1):hLen ]
        }
    }

    scoreMtxLst <- list()
    bScrMtxLst <- list()
    for( sfcIdx in names(sfcHLst) ){    # sfcIdx <- names(sfcHLst)[2]

        scoreMtx.grp.lst <- list( )
        for( hIdx in sfcHLst[[sfcIdx]] ){   # hIdx <- sfcHLst[[sfcIdx]][1]
            stdZoid <- gEnv$zhF[hIdx ,]
            wEnv <- gEnv
            wEnv$zhF <- gEnv$zhF[1:(hIdx-1),]

            fRstLst.w <- fRstLst[as.character(fRstLst.hSpan[fRstLst.hSpan<hIdx])]

            stdMI.grp <- bUtil.getStdMILst( wEnv ,fRstLst.w )
            filter.grp <- getFilter.grp( stdMI.grp ,tgt.scMtx )

            scoreMtx.grp <- getScoreMtx.grp.4H( stdZoid ,filter.grp )
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

        # bScrHMtxLst : scoreMtx.grp.lst의 데이터를 히스토리 MTX 형태로 변환해서 저장.
        bScrHMtxLst <- list()
        bScrMtxName <- names(scoreMtx.grp.lst[[1]]$mf) # 필터링이 아닌 H를 보려하는 것이므로 basic만 다룬다.
        for( mName in bScrMtxName ){    # mName <- bScrMtxName[1]
            scoreMtx <- NULL    ;infoMtx<-NULL
            for( rIdx in seq_len(length(scoreMtx.grp.lst)) ){
                scoreObj <- scoreMtx.grp.lst[[rIdx]]$mf[[mName]]
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

            if( !is.null(infoMtx) )     rownames(infoMtx) <- sfcHLst[[sfcIdx]]

            bScrHMtxLst[[mName]] <- list( scoreMtx=scoreMtx ,infoMtx=infoMtx )
        }

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

    # rObj$getScoreMtxObj <- function( hName, mName, pName ){
    #       # 폐지. rObj 를 붙잡고 있어서 메모리를 괴물처럼 잡아먹는다.
    #       #   B.HMtxLst_getMtxLst() 사용으로 대치
    #     return( rObj$scoreMtxLst[[hName]][[pName]][[mName]] )
    # }

    cnt <- sapply(sfcHLst,length)
    tDiff <- Sys.time() - tStmp
    cat(sprintf("       %d time %.1f%s(tgt.scMtx:%s)   %s\n", lastH
            ,tDiff  ,units(tDiff)   ,ifelse( is.null(tgt.scMtx),"*",paste(tgt.scMtx,collapse=",") )
            ,paste(paste(names(cnt),cnt,sep=":") ,collapse="   " ) 
    ))

    return( rObj )
} # B.makeHMtxLst()

B.HMtxLst_getMtxLst <- function( hMtxLst ,hName, mName, pName, tgt="scoreMtxLst" ){
    mtxLst <- NULL
    if( tgt=="scoreMtxLst" ){
        mtxLst <- hMtxLst[[tgt]][[hName]][[pName]][[mName]]
    } else if( tgt=="mfMtxLst" ){
        mtxLst <- hMtxLst[[tgt]][[hName]][[mName]]
    }

    return( mtxLst )
}

B.getHMtxLst_byFCol <- function( hMtxLst ){ # scoreMtxLst <- hMtxLst$scoreMtxLst

    rLst <- list()

    scoreMtxLst <- hMtxLst$scoreMtxLst
    mtxInfoLst <- hMtxLst$mtxInfoLst
    phaseName <- hMtxLst$phaseName

    for( hnIdx in names(scoreMtxLst) ){ # hnIdx <- names(scoreMtxLst)[1]
        scmLst <- list()
        if( 0==length(scoreMtxLst[[hnIdx]][[1]]) ) break

        rowSize <- nrow(scoreMtxLst[[hnIdx]][[1]][[1]]$scoreMtx)
        for( scmIdx in names(mtxInfoLst) ){ # scmIdx <- names(mtxInfoLst)[1]
            byColMtxLst <- list()
            for( cnIdx in mtxInfoLst[[scmIdx]] ){   # cnIdx <- mtxInfoLst[[scmIdx]][1]
                mtx <- matrix( 0, nrow=rowSize, ncol=length(phaseName) )
                colnames(mtx) <- phaseName
                rownames(mtx) <- rownames(scoreMtxLst[[hnIdx]][[1]][[scmIdx]]$scoreMtx)
                for( pnIdx in phaseName ){  # pnIdx <- phaseName[1]
                    mtx[,pnIdx] <- scoreMtxLst[[hnIdx]][[pnIdx]][[scmIdx]]$scoreMtx[,cnIdx]
                }
                byColMtxLst[[cnIdx]] <- mtx
            }
            scmLst[[scmIdx]] <- byColMtxLst
        }
        rLst[[hnIdx]] <- scmLst
    }

    return( rLst )
} # B.getHMtxLst_byFCol()

B.getHMtxLst_byHIdx <- function( hMtxLst ){ # scoreMtxLst <- hMtxLst$scoreMtxLst
    #   phase * FCol for each HIdx
    rLst <- list()

    scoreMtxLst <- hMtxLst$scoreMtxLst
    sfcHLst <- hMtxLst$sfcHLst
    mtxInfoLst <- hMtxLst$mtxInfoLst
    phaseName <- hMtxLst$phaseName

    for( hnIdx in names(scoreMtxLst) ){ # hnIdx <- names(scoreMtxLst)[1]
        scmLst <- list()
        for( scmIdx in names(mtxInfoLst) ){ # scmIdx <- names(mtxInfoLst)[1]
            mtx <- matrix( 0, ncol=length(phaseName) ,nrow=length(mtxInfoLst[[scmIdx]]) )
            rownames(mtx) <- mtxInfoLst[[scmIdx]]
            colnames(mtx) <- phaseName
            byHLst <- list()
            for( hIdx in as.character(sfcHLst[[hnIdx]]) ){ # hIdx <- as.character(sfcHLst[[hnIdx]])[1]
                mtx[,] <- 0
                for( pnIdx in phaseName ){ # pnIdx <- phaseName[1]
                    scoreMtx <- scoreMtxLst[[hnIdx]][[pnIdx]][[scmIdx]]$scoreMtx
                    mtx[,pnIdx] <- scoreMtx[hIdx,]
                }
                byHLst[[hIdx]] <- mtx
            }
            scmLst[[scmIdx]] <- byHLst
        }
        rLst[[hnIdx]] <- scmLst
    }

    return( rLst )
} # B.getHMtxLst_byHIdx()


B.rptHMtxLst <- function( hMtxLst ){

    getShortPhaseName <- function( phaseName ){
        phaseName <- gsub("^next","",phaseName)
        phaseName <- gsub("^ColVal_","cv",phaseName)
        phaseName <- gsub("StepBin","Bin",phaseName)
        return( phaseName )
    }

    log.meta <- k.getFlogObj( sprintf("./report/HMtxLst/%d_metaInfo.txt",hMtxLst$lastH) )
    log.meta$fLogStr("start", pTime=T ,pAppend=F )
    log.meta$fLogStr( sprintf("lastH : %d",hMtxLst$lastH) )
    log.meta$fLogStr( sprintf("phase : %s",paste(hMtxLst$phaseName,collapse=" ") ) )
    log.meta$fLogStr( "scoreMtx" )
    for( nIdx in names(hMtxLst$mtxInfoLst) ){
        log.meta$fLogStr( sprintf("    %s : %s",nIdx,paste(hMtxLst$mtxInfoLst[[nIdx]],collapse=" ")) )
    }
    log.meta$fLogStr( "sfcHLst" )
    for( nIdx in names(hMtxLst$sfcHLst) ){
        hStr <- paste(hMtxLst$sfcHLst[[nIdx]],collapse=" ")
        log.meta$fLogStr( sprintf("    %s - %s",nIdx,hStr) )
    }

    hNames <- names(hMtxLst$scoreMtxLst)
    mtxInfoLst <- hMtxLst$mtxInfoLst
    mtxName <- names(mtxInfoLst)
    phaseName <- hMtxLst$phaseName

    # [hMtxLst$scoreMtxLst] -----------------------------
    #   h * fCol for each phase
    for( hnIdx in hNames ){ # hnIdx <- hNames[1]
        lLst <- lapply( mtxName ,function( mName ){
                            s <- k.getFlogObj( sprintf("./report/HMtxLst/%d_%s_%s_scoreMtx.txt",hMtxLst$lastH,hnIdx,mName) )
                            i <- k.getFlogObj( sprintf("./report/HMtxLst/%d_%s_%s_infoMtx.txt",hMtxLst$lastH,hnIdx,mName) )
                            s$fLogStr( sprintf("%s(hnIdx:%s)",mName,hnIdx) , pTime=T ,pAppend=F )
                            i$fLogStr( sprintf("%s(hnIdx:%s)",mName,hnIdx) , pTime=T ,pAppend=F )
                            return( list(s=s,i=i) )
                        })
        names(lLst) <- mtxName

        for( pnIdx in phaseName ){  # pnIdx <- phaseName[1]
            for( mnIdx in mtxName ){ # mnIdx <- mtxName[1]
                scoreMtx <- hMtxLst$scoreMtx[[hnIdx]][[pnIdx]][[mnIdx]]$scoreMtx
                lLst[[mnIdx]]$s$fLogStr(sprintf("%s----------------------",pnIdx))
                lLst[[mnIdx]]$s$fLogMtx( scoreMtx )

                infoMtx <- hMtxLst$scoreMtx[[hnIdx]][[pnIdx]][[mnIdx]]$infoMtx
                if( !is.null(infoMtx) ){
                    lLst[[mnIdx]]$i$fLogStr(sprintf("%s----------------------",pnIdx))
                    lLst[[mnIdx]]$i$fLogMtx( infoMtx )
                }
            } # mnIdx
        }
    }

    # [byFCol ] -----------------------------
    #   h * phase for each scoreMtx col
    byFCol <- B.getHMtxLst_byFCol( hMtxLst )
    for( hnIdx in hNames ){ # hnIdx <- hNames[1]
        for( mnIdx in mtxName ){ # mnIdx <- mtxName[1]
            wLog <- k.getFlogObj( sprintf("./report/HMtxLst/%d_%s_%s_scoreMtx.byFCol.txt",hMtxLst$lastH,hnIdx,mnIdx) )
            wLog$fLogStr( sprintf("start %s (hnIdx:%s)",mnIdx,hnIdx) ,pTime=T ,pAppend=F )
            colNames <- mtxInfoLst[[mnIdx]]
            for( cIdx in seq_len(length(colNames)) ){ # cIdx <- 1
                cnIdx <- colNames[cIdx]
                wLog$fLogStr( sprintf("<%s of %s> ------------------------",cnIdx,mnIdx) )
                mtx <- byFCol[[hnIdx]][[mnIdx]][[cnIdx]]
                colnames(mtx) <- getShortPhaseName( colnames(mtx) )
                wLog$fLogMtx( mtx )
            }
        }

    }

    # [byH ] -----------------------------
    #   fCol * phase for each hIdx
    byHIdx <- B.getHMtxLst_byHIdx( hMtxLst )
    for( hnIdx in hNames ){ # hnIdx <- hNames[1]
        for( mnIdx in mtxName ){ # mnIdx <- mtxName[1]
            wLog <- k.getFlogObj( sprintf("./report/HMtxLst/%d_%s_%s_scoreMtx.byHIdx.txt",hMtxLst$lastH,hnIdx,mnIdx) )
            wLog$fLogStr( sprintf("start %s (hnIdx:%s)",mnIdx,hnIdx) ,pTime=T ,pAppend=F )
            for( hIdx in names(byHIdx[[hnIdx]][[mnIdx]]) ){ # hIdx <- names(byHIdx[[hnIdx]][[mnIdx]])[1]
                wLog$fLogStr( sprintf("<%s at H %s> ------------------------",mnIdx,hIdx) )
                mtx <- byHIdx[[hnIdx]][[mnIdx]][[hIdx]]
                colnames(mtx) <- getShortPhaseName( colnames(mtx) )
                wLog$fLogMtx( mtx )
            }
        }
    }

    # [mf(BScrMtx) ] -----------------------------
    for( hName in names(hMtxLst$mfMtxLst) ){
        mfMtxLst <- hMtxLst$mfMtxLst[[hName]]
        lLst <- lapply( names(mfMtxLst) ,function( mName ){
                            s <- k.getFlogObj( sprintf("./report/HMtxLst/%d_%s_%s_scoreMtx.txt",hMtxLst$lastH,hName,mName) )
                            i <- k.getFlogObj( sprintf("./report/HMtxLst/%d_%s_%s_infoMtx.txt",hMtxLst$lastH,hName,mName) )
                            s$fLogStr( sprintf("%s(hnIdx:%s)",mName,hName) , pTime=T ,pAppend=F )
                            i$fLogStr( sprintf("%s(hnIdx:%s)",mName,hName) , pTime=T ,pAppend=F )
                            return( list(s=s,i=i) )
                        })
        names(lLst) <- names(mfMtxLst)

        for( mName in names(mfMtxLst) ){
            scoreMtx <- mfMtxLst[[mName]]$scoreMtx
            lLst[[mName]]$s$fLogMtx( scoreMtx )

            infoMtx <- mfMtxLst[[mName]]$infoMtx
            if( !is.null(infoMtx) ){
                lLst[[mName]]$i$fLogMtx( infoMtx )
            }
        }
    }

} # B.rptHMtxLst()

B.rptStdMI.grp <- function( stdMI.grp ,file="stdMI.grp" ){

    log.meta <- k.getFlogObj( sprintf("./report/workRpt/%s.txt",file) )
    log.meta$fLogStr("start", pTime=T ,pAppend=F )

    # basic
    log.meta$fLogStr("<Basic phase> ---------------------------------------------------")
    for( pName in names(stdMI.grp$basic) ){ # pName <- names(stdMI.grp$basic)[1]
        stdMI <- stdMI.grp$basic[[pName]]$stdMI
        log.meta$fLogStr( sprintf("*** %s ***",pName) )
        # log.meta$fLogStr( sprintf("    rem : %s",paste(stdMI$rem,collapse=",") ) )
        # log.meta$fLogStr( sprintf("    rawTail") )
        # log.meta$fLogMtx( stdMI$rawTail ,pIndent="        " )
        # log.meta$fLogStr( sprintf("    cStepTail") )
        # log.meta$fLogMtx( stdMI$cStepTail ,pIndent="        " )
        # log.meta$fLogStr( sprintf("    fStepTail") )
        # log.meta$fLogMtx( stdMI$fStepTail ,pIndent="        " )
        dfStr <- capture.output( anaMtx(stdMI$rawTail,NULL) )
        dfStr <- paste( dfStr ,collapse="\n" )
        log.meta$fLogStr( sprintf("%s \n",dfStr) )
    }

    # bDup
    log.meta$fLogStr("<bDup phase> ---------------------------------------------------")
    log.meta$fLogStr("    working")

    # mf
    log.meta$fLogStr("<mf phase> ---------------------------------------------------")
    log.meta$fLogStr("    working")

} # B.rptStdMI.grp()

B.rptScoreMtx.grp <- function( scoreMtx.grp ,rIdx=1 ,file="scoreMtx.grp" ){

    getShortPhaseName <- function( phaseName ){
        phaseName <- gsub("^next","",phaseName)
        phaseName <- gsub("^ColVal_","cv",phaseName)
        phaseName <- gsub("StepBin","Bin",phaseName)
        return( phaseName )
    }

    log.meta <- k.getFlogObj( sprintf("./report/workRpt/%s.txt",file) )
    log.meta$fLogStr("start", pTime=T ,pAppend=F )

    # basic
    log.meta$fLogStr("<Basic phase> ---------------------------------------------------")
    for( pName in names(scoreMtx.grp$basic) ){ # pName <- names(scoreMtx.grp$basic)[1]
        for( mName in names(scoreMtx.grp$basic[[pName]]) ){ # mName <- names(scoreMtx.grp$basic[[pName]])[1]
            scoreMtx <- scoreMtx.grp$basic[[pName]][[mName]]$scoreMtx[rIdx,,drop=F]
            rownames(scoreMtx) <- paste(rIdx,"st",sep="")

            log.meta$fLogStr( sprintf("  %s / %s",pName,mName) )
            log.meta$fLogMtx( scoreMtx ,pIndent="    " )
        }
    }
    log.meta$fLogStr("<Basic phase> FCol------------------------------------------------")
    mtxGrp <- getScoreMtx.grp_byFCol( scoreMtx.grp )
    for( mName in names(mtxGrp) ){  # mName <- names(mtxGrp)[1]
        for( fColName in names(mtxGrp[[mName]]) ){  # fColName <- names(mtxGrp[[mName]])[1]
            mtx <- mtxGrp[[mName]][[fColName]][rIdx,,drop=F]
            colnames( mtx ) <- getShortPhaseName( colnames( mtx ) )
            log.meta$fLogStr( sprintf("  %s[,\"%s\"]",mName,fColName) )
            log.meta$fLogMtx( mtx ,pIndent="    " )
        }
    }
    log.meta$fLogStr("<Basic phase> hIdx------------------------------------------------")
    mtxGrp <- getScoreMtx.grp_byHIdx( scoreMtx.grp )
    for( mName in names(mtxGrp) ){  # mName <- names(mtxGrp)[1]
        for( aIdx in rIdx ){    # aIdx <- rIdx[1]
            mtx <- mtxGrp[[mName]][[rIdx]]
            colnames( mtx ) <- getShortPhaseName( colnames( mtx ) )
            log.meta$fLogStr( sprintf("  %s for %dth aZoid ",mName,aIdx) )
            log.meta$fLogMtx( mtx ,pIndent="    " )
        }
    }

    # bDup
    log.meta$fLogStr("<bDup phase> ---------------------------------------------------")
    log.meta$fLogStr("    working")

    # mf
    log.meta$fLogStr("<mf phase> ---------------------------------------------------")
    log.meta$fLogStr("    working")

} # B.rptScoreMtx.grp

B.rptCut.grp <- function( cut.grp ,file="cut.grp" ){

    log.meta <- k.getFlogObj( sprintf("./report/workRpt/%s.txt",file) )
    log.meta$fLogStr("start", pTime=T ,pAppend=F )

    for( hName in names(cut.grp$sfcHLst) ){ # hName <- names(cut.grp$sfcHLst)[1]
        for( mName in names(cut.grp$mtxInfoLst) ){  # mName <- names(cut.grp$mtxInfoLst)[1]
            #   "stdLst"  "fCol"    "hIdxLst"
            stdLst <- cut.grp$cutterLst[[hName]][[mName]]$stdLst
            for( pName in names(stdLst) ){  # pName <- names(stdLst)[1]
                log.meta$fLogStr( sprintf("%s - stdLst:%s/%s",hName,mName,pName) )
                for( cutName in names(stdLst[[pName]]) ){   # cutName <- names(stdLst[[pName]])[1]
                    log.meta$fLogStr( sprintf("    <%s>\t%s",cutName,stdLst[[pName]][[cutName]]$description) )
                }
            }

            fCol <- cut.grp$cutterLst[[hName]][[mName]]$fCol
            log.meta$fLogStr( sprintf("%s - fCol:%s/",hName,mName) )

            hIdxLst <- cut.grp$cutterLst[[hName]][[mName]]$hIdxLst
            log.meta$fLogStr( sprintf("%s - hIdxLst:%s/",hName,mName) )
        }
    }

} # B.rptCut.grp

B.rptCutRst <- function( cutRst ,file="cutRst" ){

    log.meta <- k.getFlogObj( sprintf("./report/workRpt/%s.txt",file) )
    log.meta$fLogStr("start", pTime=T ,pAppend=F )

    cutInfoLst <- cutRst$cutInfoLst

    for( idx in seq_len(length(cutInfoLst)) ){
        log.meta$fLogStr( sprintf("%3dth   %s  (%s)",idx
            ,paste(cutInfoLst[[idx]],collapse="/")
            ,paste(names(cutInfoLst[[idx]]),collapse="/")
        ) )
    }

} # B.rptCutRst()


B.rptCutRstLst <- function( cutRstLst ,file="cutRstLst" ,rptBanTyp=NULL ,rptBanM=NULL ){

    getPortion <- function( fieldName ,cutRstLst ,headLen=NULL ){
        # fieldName <- "typ"
        cutRstLst.len <- length(cutRstLst)

        valLst <- list()
        for( nIdx in names(cutRstLst) ){
            cutRst <- cutRstLst[[nIdx]]
            vals <- ""
            for( lIdx in seq_len(length(cutRst$cutInfoLst)) ){
                cutInfo <- cutRst$cutInfoLst[[lIdx]]
                if( fieldName %in% names(cutInfo) ){
                    vals <- c(vals,cutInfo[fieldName])
                }
            }
            valLst[[nIdx]] <- vals
        }
        names(valLst) <- names(cutRstLst)

        allVal <- do.call( c ,valLst )
        if( 0==length(allVal) ){
            return( NULL )
        }

        val.uniq <- unique(allVal)
        val.uniq <- val.uniq[sapply(val.uniq,nchar)>0]
        
        valCnt <- rep( 0, length(val.uniq) )    ;names(valCnt) <- val.uniq
        for( vIdx in val.uniq ){
            for( nIdx in names(valLst) ){
                if( vIdx %in% valLst[[nIdx]] ){
                    valCnt[vIdx] <- valCnt[vIdx] + 1
                }
            }
        }
        valCnt <- valCnt[order(valCnt,decreasing=T)]
        if( !is.null(headLen) && (headLen<length(valCnt)) ){
            valCnt <- valCnt[1:headLen]
        }

        portion <- (valCnt*100)/cutRstLst.len

        rptStrPer <- sprintf("%3.1f%%",portion)
        rptStrCnt <- sprintf("(%d/%d)",valCnt,cutRstLst.len)
        rptStr <- paste( sprintf("%s:",names(portion)),rptStrPer,rptStrCnt,sep="" )
        rptStr <- paste( rptStr ,collapse="  " )

        return( list(rptStr=rptStr,portion=portion,valCnt=valCnt,totCnt=cutRstLst.len) )
    } # getPortion()

    log.meta <- k.getFlogObj( sprintf("./report/workRpt/%s.txt",file) )
    log.meta$fLogStr("start", pTime=T ,pAppend=F )

    log.meta$fLogStr("[Portion]============================================")
    log.meta$fLogStr("  <typ>")
    porObj <- getPortion( "typ" ,cutRstLst ,headLen=5 )
    if( !is.null(porObj) ){
        log.meta$fLogStr( sprintf("    %s",porObj$rptStr) )
    }
    log.meta$fLogStr("  <hName>")
    porObj <- getPortion( "hName" ,cutRstLst ,headLen=5 )
    if( !is.null(porObj) ){
        log.meta$fLogStr( sprintf("    %s",porObj$rptStr) )
    }
    log.meta$fLogStr("  <mName>")
    porObj <- getPortion( "mName" ,cutRstLst ,headLen=5 )
    if( !is.null(porObj) ){
        log.meta$fLogStr( sprintf("    %s",porObj$rptStr) )
    }
    log.meta$fLogStr("  <pName>")
    porObj <- getPortion( "pName" ,cutRstLst ,headLen=5 )
    if( !is.null(porObj) ){
        log.meta$fLogStr( sprintf("    %s",porObj$rptStr) )
    }
    log.meta$fLogStr("  <fcName>")
    porObj <- getPortion( "fcName" ,cutRstLst ,headLen=5 )
    if( !is.null(porObj) ){
        log.meta$fLogStr( sprintf("    %s",porObj$rptStr) )
    }

    cutCnt <- sapply( cutRstLst ,function(p){length(p$cutInfoLst)})
    cntTbl <- table( cutCnt )
    log.meta$fLogStr(sprintf("[cutCnt(total %d)]============================================",sum(cntTbl)))
    # perStr <- kLog.getPerStr( cntTbl ,sum(cntTbl) ) #  ,pLong=T
    # log.meta$fLogStr( sprintf("\t%s",paste(perStr,collapse=" ")) )
    cntStr <- paste(sprintf("cnt%s",names(cntTbl)),cntTbl,sep=": ")
    cntStr <- paste( cntStr ,sprintf("(%.1f%%)",(cntTbl*100)/sum(cntTbl)) ,sep="" )
    log.meta$fLogStr( sprintf("\t%s",paste(cntStr,collapse="   ")) )

    # ====================================================================
    log.meta$fLogStr("\n\n")
    rptHidden <- ""
    if( 0 < length(rptBanTyp) ){
        rptHidden <- c( rptHidden ,sprintf("  skipped type:%s",paste(rptBanTyp,collapse=",")) )
    }
    if( 0 < length(rptBanM) ){
        rptHidden <- c( rptHidden ,sprintf("  skipped mName:%s",paste(rptBanM,collapse=",")) )
    }

    for( nIdx in names(cutRstLst) ){   # nIdx <- names(cutRstLst)[1]
        cutRst <- cutRstLst[[nIdx]]
        log.meta$fLogStr( sprintf("<%s>",nIdx) )

        cutLen <- length(cutRst$cutInfoLst)
        if( 0 == cutLen )    next

        reportSkip <- c("typ"=0 ,"mName"=0)
        for( idx in 1:cutLen ){
            cutInfo <- cutRst$cutInfoLst[[idx]]
            grep( "v", c("nv","vv","3v","n4v","nn") )

            banFlag <- F
            for( banIdx in rptBanTyp ){
                if( grepl(banIdx,cutInfo["typ"]) ){
                    banFlag <- T
                    reportSkip["typ"] <- 1 + reportSkip["typ"]
                    break
                }
            }
            for( banIdx in rptBanM ){
                if( grepl(banIdx,cutInfo["mName"]) ){
                    banFlag <-T
                    reportSkip["mName"] <- 1 + reportSkip["mName"]
                    break
                }
            }
            if( banFlag ){
                next    # report skip
            }

            log.meta$fLogStr( sprintf("  %4d %s",idx,paste(names(cutInfo),collapse="\t") ) )
            log.meta$fLogStr( sprintf("       %s",    paste(cutInfo,collapse="\t") ) )
        }
        if( any(0<reportSkip) ){
            skipStr <- paste( names(reportSkip) ,reportSkip ,sep=":" )
            log.meta$fLogStr( sprintf("  ** report skip - %s", paste(skipStr,collapse=" ") ) )
            log.meta$fLogStr( sprintf("            %s ", rptHidden ) )
        }

    }

} # B.rptCutRstLst()

B.rptCutRst1Score <- function( resultLst ,file="CutRst1Score" ){

    getShortPhaseName <- function( phaseName ){
        phaseName <- gsub("^next","",phaseName)
        phaseName <- gsub("^ColVal_","cv",phaseName)
        phaseName <- gsub("StepBin","Bin",phaseName)
        return( phaseName )
    }

    rpt <- function( logger ,hIdx ,hName ,rebMtx.ph ,phaseReb ,summMtx ,summMtx.reb ,scMtx.sz ){
        logger$fLogStr( sprintf("\n[hIdx:%s - %s] ------------------------------------------------",hIdx,hName) )
        
        logger$fLogStr( "  <rebMtx.ph>" ) ;mtx <- rebMtx.ph[c("rebFlag.raw","rebFlag.evt") ,]
        colnames(mtx) <- getShortPhaseName(colnames(mtx))
        logger$fLogMtx( mtx ,pIndent="    ")

        logger$fLogStr( "  <phaseReb>" )  ;mtx <- phaseReb[c("rebFlag.raw","rebFlag.evt") ,]
        colnames(mtx) <- getShortPhaseName(colnames(mtx))
        logger$fLogMtx( mtx ,pIndent="    ")

        logger$fLogStr( "  <summMtx>" ) ;mtx <- summMtx
        colnames(mtx) <- getShortPhaseName(colnames(mtx))
        logger$fLogMtx( mtx ,pIndent="    ")
        logger$fLogStr( "  <summMtx.reb>" ) ;mtx <- summMtx.reb
        colnames(mtx) <- getShortPhaseName(colnames(mtx))
        logger$fLogMtx( mtx ,pIndent="    ")

        logger$fLogStr( "  <scMtx.sz>" ) ;mtx <- scMtx.sz
        colnames(mtx) <- getShortPhaseName(colnames(mtx))
        logger$fLogMtx( mtx ,pIndent="    ")
    }

    logger <- k.getFlogObj( sprintf("./report/workRpt/%s.txt",file) )
    logger$fLogStr("Start",pTime=T,pAppend=F)

    for( hIdx in names(resultLst) ){
        if( is.null(resultLst[[hIdx]]$cutRst1Score) ) next

        cutRst1Score <- resultLst[[hIdx]]$cutRst1Score$aLst[[1]]
        for( hName in names(cutRst1Score) ){
            rebMtx.ph <- NULL   ;phaseReb <- NULL
            summMtx <- NULL     ;summMtx.reb <- NULL    ;scMtx.sz <- NULL
            for( mName in names(cutRst1Score[[hName]]$basic) ){
                rawObj <- cutRst1Score[[hName]]$basic[[mName]]$raw      # "rebMtx.ph"    "evtHpnLevMtx" "phaseReb"    
                summObj <- cutRst1Score[[hName]]$basic[[mName]]$summ    # "fColEvt"     "summMtx"     "summMtx.reb" "scMtx.sz"  

                if( is.null(rebMtx.ph) ){   rebMtx.ph <- rawObj$rebMtx.ph
                } else {    rebMtx.ph <- rebMtx.ph + rawObj$rebMtx.ph           }
                if( is.null(phaseReb) ){    phaseReb <- rawObj$phaseReb
                } else {    phaseReb <- phaseReb + rawObj$phaseReb              }

                if( is.null(summMtx) ){     summMtx <- summObj$summMtx
                } else {    summMtx <- summMtx + summObj$summMtx                }
                if( is.null(summMtx.reb) ){ summMtx.reb <- summObj$summMtx.reb
                } else {    summMtx.reb <- summMtx.reb + summObj$summMtx.reb    }
                if( is.null(scMtx.sz) ){    scMtx.sz <- summObj$scMtx.sz
                } else {    scMtx.sz <- scMtx.sz + summObj$scMtx.sz             }
            }

            rpt( logger ,hIdx ,hName ,rebMtx.ph ,phaseReb ,summMtx ,summMtx.reb ,scMtx.sz )
        }
    }

    logger$fLogStr("End of Report",pTime=T)

}

B.rptCutRst1Score_byMtx <- function( resultLst ,mName ,file="cutRst1Score"){

    removeZeroRow <- function( mtx ){
        flag <- apply( mtx ,1 ,function(rDat){all(rDat==0)})
        mtx <- mtx[!flag,]
        rownames(mtx) <- rownames(mtx)[flag]
        return( mtx )
    }

    logSummMtx <- k.getFlogObj( sprintf("./report/workRpt/%s_%s_summMtx.txt",file,mName) )
    logSummMtxReb <- k.getFlogObj( sprintf("./report/workRpt/%s_%s_summMtxReb.txt",file,mName) )
    logScMtxSz <- k.getFlogObj( sprintf("./report/workRpt/%s_%s_scMtxSz.txt",file,mName) )

    logSummMtx$fLogStr("Start by B.rptCutRst1Score_byMtx()",pTime=T,pAppend=F)
    logSummMtxReb$fLogStr("Start by B.rptCutRst1Score_byMtx()",pTime=T,pAppend=F)
    logScMtxSz$fLogStr("Start by B.rptCutRst1Score_byMtx()",pTime=T,pAppend=F)

    summMtxLst <- list()
    for( hIdx in names(resultLst) ){
        if( is.null(resultLst[[hIdx]]$cutRst1Score) ) next

        rptStr <- sprintf("hIdx:%s",hIdx)
        logSummMtx$fLogStr(     rptStr)
        logSummMtxReb$fLogStr(  rptStr)
        logScMtxSz$fLogStr(     rptStr)


        cutRst1Score <- resultLst[[hIdx]]$cutRst1Score$aLst[[1]]

        hNameSet <- names(cutRst1Score)
        mNameSet <- names(cutRst1Score[[ hNameSet[1] ]]$basic)
        if( !(mName %in% mNameSet) )    next

        # summMtx
        rawMtx <- NULL  ;evtMtx <- NULL
        for( hName in hNameSet ){
            summObj <- cutRst1Score[[ hName ]]$basic[[mName]]$summ
            rawMtx <- rbind( rawMtx ,summObj$summMtx["raw",] )
            evtMtx <- rbind( evtMtx ,summObj$summMtx["evt",] )
        }
        rownames(rawMtx) <- hNameSet        ;rownames(evtMtx) <- hNameSet
        logSummMtx$fLogMtx( removeZeroRow(rawMtx) ,pIndent="    ")
        logSummMtx$fLogMtx( removeZeroRow(evtMtx) ,pIndent="    ")

        # summMtx.reb
        rawMtx <- NULL  ;evtMtx <- NULL
        for( hName in hNameSet ){
            summObj <- cutRst1Score[[ hName ]]$basic[[mName]]$summ
            rawMtx <- rbind( rawMtx ,summObj$summMtx.reb["raw",] )
            evtMtx <- rbind( evtMtx ,summObj$summMtx.reb["evt",] )
        }
        rownames(rawMtx) <- hNameSet        ;rownames(evtMtx) <- hNameSet
        logSummMtxReb$fLogMtx( removeZeroRow(rawMtx) ,pIndent="    ")
        logSummMtxReb$fLogMtx( removeZeroRow(evtMtx) ,pIndent="    ")

        # scMtx.sz
        rawMtx <- NULL  ;evtMtx <- NULL
        for( hName in hNameSet ){
            summObj <- cutRst1Score[[ hName ]]$basic[[mName]]$summ
            rawMtx <- rbind( rawMtx ,summObj$scMtx.sz["rebCnt",] )
            evtMtx <- rbind( evtMtx ,summObj$scMtx.sz["rebDup",] )
        }
        rownames(rawMtx) <- hNameSet        ;rownames(evtMtx) <- hNameSet
        logScMtxSz$fLogMtx( removeZeroRow(rawMtx) ,pIndent="    ")
        logScMtxSz$fLogMtx( removeZeroRow(evtMtx) ,pIndent="    ")

    }

    logSummMtx$fLogStr("End",pTime=T)
    logSummMtxReb$fLogStr("End",pTime=T)
    logScMtxSz$fLogStr("End",pTime=T)

}

B.rptCutRst1Score_bS <- function( resultLst ,file="CutRst1Score_bS" ){

    getShortPhaseName <- function( phaseName ){
        phaseName <- gsub("^next","",phaseName)
        phaseName <- gsub("^ColVal_","cv",phaseName)
        phaseName <- gsub("StepBin","Bin",phaseName)
        return( phaseName )
    }

    rpt <- function( logger ,hIdx ,hName ,rebMtx.ph ,phaseReb ,summMtx ,summMtx.reb ,scMtx.sz ){
        logger$fLogStr( sprintf("\n[hIdx:%s - %s] ------------------------------------------------",hIdx,hName) )
        
        logger$fLogStr( "  <rebMtx.ph>" ) ;mtx <- rebMtx.ph[c("rebFlag.raw","rebFlag.evt") ,]
        colnames(mtx) <- getShortPhaseName(colnames(mtx))
        logger$fLogMtx( mtx ,pIndent="    ")

        logger$fLogStr( "  <phaseReb>" )  ;mtx <- phaseReb[c("rebFlag.raw","rebFlag.evt") ,]
        colnames(mtx) <- getShortPhaseName(colnames(mtx))
        logger$fLogMtx( mtx ,pIndent="    ")

        logger$fLogStr( "  <summMtx>" ) ;mtx <- summMtx
        colnames(mtx) <- getShortPhaseName(colnames(mtx))
        logger$fLogMtx( mtx ,pIndent="    ")
        logger$fLogStr( "  <summMtx.reb>" ) ;mtx <- summMtx.reb
        colnames(mtx) <- getShortPhaseName(colnames(mtx))
        logger$fLogMtx( mtx ,pIndent="    ")

        logger$fLogStr( "  <scMtx.sz>" ) ;mtx <- scMtx.sz
        colnames(mtx) <- getShortPhaseName(colnames(mtx))
        logger$fLogMtx( mtx ,pIndent="    ")
    }

    logger <- k.getFlogObj( sprintf("./report/workRpt/%s.txt",file) )
    logger$fLogStr("Start",pTime=T,pAppend=F)

    for( hIdx in names(resultLst) ){
        if( 0==length(resultLst[[hIdx]]$cutRst1Score_bS$aLst) ) next

        cutRst1Score <- resultLst[[hIdx]]$cutRst1Score_bS$aLst[[1]]

        for( hName in names(cutRst1Score) ){
            rebMtx.ph <- NULL   ;phaseReb <- NULL
            summMtx <- NULL     ;summMtx.reb <- NULL    ;scMtx.sz <- NULL
            for( mName in names(cutRst1Score[[hName]]$basic) ){
                rawObj <- cutRst1Score[[hName]]$basic[[mName]]$raw      # "rebMtx.ph"    "evtHpnLevMtx" "phaseReb"    
                summObj <- cutRst1Score[[hName]]$basic[[mName]]$summ    # "fColEvt"     "summMtx"     "summMtx.reb" "scMtx.sz"  

                if( is.null(rebMtx.ph) ){   rebMtx.ph <- rawObj$rebMtx.ph
                } else {    rebMtx.ph <- rebMtx.ph + rawObj$rebMtx.ph           }
                if( is.null(phaseReb) ){    phaseReb <- rawObj$phaseReb
                } else {    phaseReb <- phaseReb + rawObj$phaseReb              }

                if( is.null(summMtx) ){     summMtx <- summObj$summMtx
                } else {    summMtx <- summMtx + summObj$summMtx                }
                if( is.null(summMtx.reb) ){ summMtx.reb <- summObj$summMtx.reb
                } else {    summMtx.reb <- summMtx.reb + summObj$summMtx.reb    }
                if( is.null(scMtx.sz) ){    scMtx.sz <- summObj$scMtx.sz
                } else {    scMtx.sz <- scMtx.sz + summObj$scMtx.sz             }
            }

            rpt( logger ,hIdx ,hName ,rebMtx.ph ,phaseReb ,summMtx ,summMtx.reb ,scMtx.sz )
        }
    }

    logger$fLogStr("End of Report",pTime=T)

}


B.get_testData.grp.old <- function( testSpan ,gEnv ,allIdxLst ,fRstLst ,tgt.scMtx=NULL ,get.scoreMtx.grp=FALSE ){

    curHMtxLst.grp <- list( )
    stdIdx.grp <- list( )
    scoreMtxLst.grp <- list( )

    tStmp <- Sys.time()
    for( curHIdx in testSpan ){    # curHIdx <- testSpan[1] # 842

        wLastH <-curHIdx-1
        wLastSpan <- 1:which(names(fRstLst)==wLastH)

        # ------------------------------------------------------------------------
        # curHMtxLst.grp
        gEnv.w <- gEnv              ;gEnv.w$zhF <- gEnv$zhF[1:wLastH,]
        allIdxLst.w <- allIdxLst    ;allIdxLst.w$stdFiltedCnt <- allIdxLst$stdFiltedCnt[wLastSpan]
                                    allIdxLst.w$infoMtx <- allIdxLst$infoMtx[wLastSpan,]
        fRstLst.w <- fRstLst[wLastSpan]

        curHMtxLst <- B.makeHMtxLst( gEnv.w, allIdxLst.w, fRstLst.w, tgt.scMtx )   

        curHMtxLst.grp[[as.character(curHIdx)]] <- curHMtxLst

        # ------------------------------------------------------------------------
        # stdIdx.grp
        stdZoid <- gEnv$zhF[curHIdx,]
        stdIdx <- k.getIdx_AllZoidMtx( gEnv, stdZoid )
        stdIdx.grp[[as.character(curHIdx)]] <- stdIdx

        if( get.scoreMtx.grp ){
            stdMI.grp <- bUtil.getStdMILst( gEnv.w ,fRstLst.w )
            filter.grp <- getFilter.grp( stdMI.grp ,tgt.scMtx=tgt.scMtx )
            scoreMtx.grp <- getScoreMtx.grp.4H( stdZoid ,filter.grp )
            scoreMtxLst.grp[[as.character(curHIdx)]] <- scoreMtx.grp
        }

    }
    tDiff <- Sys.time() - tStmp
    cat(sprintf("time : %.1f,%s   \n",tDiff,units(tDiff)))

    rLst <- list(curHMtxLst.grp=curHMtxLst.grp ,stdIdx.grp=stdIdx.grp)
    if( get.scoreMtx.grp ) rLst$scoreMtxLst.grp <- scoreMtxLst.grp

    return( rLst )
}

B.get_testData.grp <- function( testSpan ,gEnv ,allIdxLst ,fRstLst ,tgt.scMtx=NULL ,get.scoreMtx.grp=FALSE ){

    tStmp <- Sys.time()
    sfExport("tgt.scMtx")   ;sfExport("get.scoreMtx.grp")
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

        curHMtxLst <- B.makeHMtxLst( gEnv.w, allIdxLst.w, fRstLst.w, tgt.scMtx )
        curHMtxLst_bS <- bS.makeHMtxLst( gEnv.w, allIdxLst.w, fRstLst.w ,tgt.scMtx )

        # ------------------------------------------------------------------------
        # stdIdx.grp
        stdZoid <- gEnv$zhF[curHIdx,]
        stdIdx <- k.getIdx_AllZoidMtx( gEnv, stdZoid )

        tDiff <- Sys.time() - tStmp.prll
        prllLog$fLogStr(sprintf("    B.get_testData.grp - hIdx:%d finished %.1f%s",curHIdx,tDiff,units(tDiff)))

        rObj <- list( hIdx=curHIdx ,stdIdx=stdIdx ,hMtxLst=curHMtxLst ,hMtxLst_bS=curHMtxLst_bS ) 

        if( get.scoreMtx.grp ){
            stdMI.grp <- bUtil.getStdMILst( gEnv.w ,fRstLst.w )
            filter.grp <- getFilter.grp( stdMI.grp ,tgt.scMtx=tgt.scMtx )
            rObj$scoreMtx.grp <- getScoreMtx.grp.4H( stdZoid ,filter.grp )
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

B.get_cutRst1.grp <- function( testData.grp ,gEnv ,allIdxLst ,fRstLst ){
    #   주의 : (1) testData.grp 의 hIdx는 건너뜀없이 연속적이어야 한다. 
    #
    #   bUtil.cutRst1( )

    testSpan <- as.integer(names(testData.grp$curHMtxLst.grp))
    tgt.scMtx <- c( names(testData.grp$curHMtxLst[[1]]$mtxInfoLst) 
                    ,names(testData.grp$curHMtxLst[[1]]$mtxInfoLst.bScr) 
                )

    resultLst <- sfLapply( testSpan ,function( curHIdx ){
        wLastH <-curHIdx-1
        wLastSpan <- 1:which(names(fRstLst)==wLastH)

        # ------------------------------------------------------------------------
        # cut.grp : cutter grp 을 얻어내자.
        gEnv.w <- gEnv              ;gEnv.w$zhF <- gEnv$zhF[1:wLastH,]
        allIdxLst.w <- allIdxLst    ;allIdxLst.w$stdFiltedCnt <- allIdxLst$stdFiltedCnt[wLastSpan]
                                    allIdxLst.w$infoMtx <- allIdxLst$infoMtx[wLastSpan,]
        fRstLst.w <- fRstLst[wLastSpan]

        curHMtxLst <- testData.grp$curHMtxLst.grp[[as.character(curHIdx)]]
            # B.makeHMtxLst() 의 lastH는 allIdxLst.w$stdFiltedCnt에 의존한다.
            # curHIdx-1 시점까지의 scoreMtx가 curHMtxLst에 담겨있다.

        cut.grp <- bFCust.getFCustGrp( curHMtxLst ,tgt.scMtx )  #   레포팅 : B.rptCut.grp( cut.grp ) 

        # ------------------------------------------------------------------------
        # 이제, 현재 stdZoid의 특성(sfcHLst, scoreMtx)을 얻자.
        stdZoid <- gEnv$zhF[curHIdx,]
        stdIdx <- testData.grp$stdIdx[[as.character(curHIdx)]]
        curStdFilted <- fRstLst[[as.character(curHIdx)]]    #   평가가 아닌 실제에선, remLst 으로부터 가져올 것.
        fHName <- bUtil.getSfcLstName( fRstLst.w[[length(fRstLst.w)]] ,curStdFiltedCnt=length(curStdFilted) ,cut.grp )

        stdMI.grp <- bUtil.getStdMILst( gEnv.w ,fRstLst.w )
        filter.grp <- getFilter.grp( stdMI.grp ,tgt.scMtx=tgt.scMtx )
        scoreMtx.grp <- getScoreMtx.grp( matrix(stdZoid,nrow=1) ,filter.grp ,makeInfoStr=T )

        cutRst1 <- bUtil.getCut1Score( scoreMtx.grp ,cut.grp ,fHName ,tgt.scMtx=tgt.scMtx )
        cutRst1$metaInfo$curHIdx <- curHIdx

        return( cutRst1 )
    })
    cutRst1Lst.grp <- resultLst
    names( cutRst1Lst.grp ) <- sapply( cutRst1Lst.grp ,function(p){p$metaInfo$curHIdx})
    lNames <- names(cutRst1Lst.grp)
    names(cutRst1Lst.grp) <- paste("H",lNames,"_",allIdxLst$stdFiltedCnt[lNames],sep="")

    # cutRst1 <- cutRst1Lst.grp[[1]]$aLst[[1]]
    # metaInfo <- cutRst1Lst.grp[[1]]$metaInfo

    # cutRstScrSet <- bUtil.cutRst1_scoreMtx(cutRst1Lst.grp[[1]]$aLst[[1]])

    return( cutRst1Lst.grp )

}   # B.get_cutRst1.grp



