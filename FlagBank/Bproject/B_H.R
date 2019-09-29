
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

    scoreMtxLst <- list()
    for( sfcIdx in names(sfcHLst) ){    # sfcIdx <- names(sfcHLst)[2]

        scoreMtx.grp.lst <- list()
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

        #   필터링이 아닌 H를 보려하는 것이므로 basic만 다룬다.
        #       mf나 bDup는 sfcHLst를 통해 H 파악.(아마 basic내 basic만 의미있겠지..)
        basicHMtxLst <- list()
        scoreMtxNames <- names(scoreMtx.grp.lst[[1]]$basic[[1]])
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

        scoreMtxLst[[sfcIdx]] <- basicHMtxLst
    }

    mtxInfoLst <- lapply( scoreMtxLst[[1]][[1]] ,function( pLst ){
                        colnames(pLst$scoreMtx)
                    })
    phaseName <- names(scoreMtxLst[[1]])

    rObj <- list( sfcHLst=sfcHLst ,lastH=lastH
                    ,mtxInfoLst=mtxInfoLst  ,phaseName=phaseName
                    ,scoreMtxLst=scoreMtxLst 
                )
    rObj$getScoreMtxObj <- function( hName, mName, pName ){
        return( rObj$scoreMtxLst[[hName]][[pName]][[mName]] )
    }

    cnt <- sapply(sfcHLst,length)
    tDiff <- Sys.time() - tStmp
    cat(sprintf("       time %.1f%s(tgt.scMtx:%s)   %s\n"
            ,tDiff  ,units(tDiff)   ,ifelse( is.null(tgt.scMtx),"*",paste(tgt.scMtx,collapse=",") )
            ,paste(paste(names(cnt),cnt,sep=":") ,collapse="   " ) 
    ))

    return( rObj )
} # B.makeHMtxLst()

B.getHMtxLst_byFCol <- function( hMtxLst ){ # scoreMtxLst <- hMtxLst$scoreMtxLst

    rLst <- list()

    scoreMtxLst <- hMtxLst$scoreMtxLst
    mtxInfoLst <- hMtxLst$mtxInfoLst
    phaseName <- hMtxLst$phaseName

    for( hnIdx in names(scoreMtxLst) ){ # hnIdx <- names(scoreMtxLst)[1]
        scmLst <- list()
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


B.rptCutRstLst <- function( cutRstLst ,file="cutRstLst" ){

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
    for( nIdx in names(cutRstLst) ){   # nIdx <- names(cutRstLst)[1]
        cutRst <- cutRstLst[[nIdx]]
        log.meta$fLogStr( sprintf("<%s>",nIdx) )

        cutLen <- length(cutRst$cutInfoLst)
        if( 0 == cutLen )    next

        for( idx in 1:cutLen ){
            cutInfo <- cutRst$cutInfoLst[[idx]]
            log.meta$fLogStr( sprintf("  %4d %s",idx,paste(names(cutInfo),collapse="\t") ) )
            log.meta$fLogStr( sprintf("       %s",    paste(cutInfo,collapse="\t") ) )
        }

    }

} # B.rptCutRstLst()


