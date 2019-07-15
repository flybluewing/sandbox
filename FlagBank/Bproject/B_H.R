

B.makeHMtx <- function( gEnv, allIdxLst, fRstLst, lastH=NULL ){

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
                        ,sfc0=as.integer(names(stdFiltedCnt)[stdFiltedCnt==0])
                        ,sfc1=as.integer(names(stdFiltedCnt)[stdFiltedCnt==1])
                        ,sfc2=as.integer(names(stdFiltedCnt)[stdFiltedCnt==2])
                    )

    for( sfnIdx in c("D0000.A","A0100.A","AP000.E") ){
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

            # QQE Todo  sfcIdx<-"sfcLate"   ;hIdx <- 857
            # Error in if (2 > stdMI$mtxLen) NULL else stdMI$rawTail[nrow(stdMI$rawTail) -  : 
            #   인자의 길이가 0

            scoreMtx.grp <- getScoreMtx.grp.4H( stdZoid ,filter.grp )
            scoreMtx.grp.lst[[sprintf("hIdx:%d",hIdx)]] <- scoreMtx.grp
        }

        scoreMtxLst[[sfcIdx]] <- scoreMtx.grp.lst
    }


} # B.makeHMtx()