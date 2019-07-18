

B.makeHMtxLst <- function( gEnv, allIdxLst, fRstLst, lastH=NULL ){

    hStr <- names(allIdxLst$stdFiltedCnt)
    names(fRstLst) <- hStr

    # ----------------------------------------------------
    firstH <- as.integer(hStr[1])
    if( is.null(lastH) ){
        lastH <-as.integer(hStr[length(hStr)])
    }
    fRstLst <- fRstLst[as.character(firstH:lastH)]

    fRstLst.hSpan <- as.integer(names(fRstLst))

    baseSpan <- 800:lastH
    stdFiltedCnt <- allIdxLst$stdFiltedCnt[as.character(baseSpan)]

    # ----------------------------------------------------
    sfcHLst <- list(    sfcLate= baseSpan[length(baseSpan)] - 10:0
                        #   Q_RBF
                        # ,sfc0=as.integer(names(stdFiltedCnt)[stdFiltedCnt==0])
                        # ,sfc1=as.integer(names(stdFiltedCnt)[stdFiltedCnt==1])
                        # ,sfc2=as.integer(names(stdFiltedCnt)[stdFiltedCnt==2])
                    )

    #   Q_RBF
    #       stdFilter <- c("D0000.A","A0100.A","AP000.E")
    stdFilter <- c("D0000.A")
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
        for( hIdx in sfcHLst[[sfcIdx]] ){   # hIdx <- hSpan[1]
            stdZoid <- gEnv$zhF[hIdx ,]
            wEnv <- gEnv
            wEnv$zhF <- gEnv$zhF[1:(hIdx-1),]

            fRstLst.w <- fRstLst[as.character(fRstLst.hSpan[fRstLst.hSpan<hIdx])]

            stdMI.grp <- bUtil.getStdMILst( wEnv ,fRstLst.w )
            filter.grp <- getFilter.grp( stdMI.grp )

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

