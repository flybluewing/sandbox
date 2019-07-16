

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

    baseSpan <- 650:lastH
    stdFiltedCnt <- allIdxLst$stdFiltedCnt[as.character(baseSpan)]

    # ----------------------------------------------------
    sfcHLst <- list(    sfcLate= baseSpan[length(baseSpan)] - 10:0
                        ,sfc0=as.integer(names(stdFiltedCnt)[stdFiltedCnt==0])
                        ,sfc1=as.integer(names(stdFiltedCnt)[stdFiltedCnt==1])
                        ,sfc2=as.integer(names(stdFiltedCnt)[stdFiltedCnt==2])
                    )
    stdFilter <- c("D0000.A","A0100.A","AP000.E")
    for( sfnIdx in stdFilter ){
        #   sfnIdx <- "D0000.A"
        hSpan <- baseSpan[sapply( fRstLst[as.character(baseSpan)] ,function(p){ sfnIdx %in% p } )]
        hSpan.NG <- hSpan+1
        hSpan.NG <- hSpan.NG[hSpan.NG<=lastH]
        sfcHLst[[sprintf("NG:%s",sfnIdx)]] <- hSpan.NG
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

    rObj <- list( sfcHLst=sfcHLst ,lastH=lastH ,scoreMtxLst=scoreMtxLst )
    return( rObj )

} # B.makeHMtxLst()

