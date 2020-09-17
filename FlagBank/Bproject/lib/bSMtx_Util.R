
bS.getPhVPGrp <- function( gEnv ,aZoidMtx ){    # bUtil.getStdMILst( ) 와 비슷
    phVPLst <- list()

    phVPLst[[1+length(phVPLst)]] <- bS.vp_ColVal( gEnv, aZoidMtx, fixCol=1 )
    phVPLst[[1+length(phVPLst)]] <- bS.vp_ColVal( gEnv, aZoidMtx, fixCol=3 )
    phVPLst[[1+length(phVPLst)]] <- bS.vp_ColVal( gEnv, aZoidMtx, fixCol=6 )

    names(phVPLst) <- sapply( phVPLst ,function(p){p$idStr})

    phVPGrp <- list( phVPLst=phVPLst )
    phVPGrp$anyWarn <- function(){
        # TODO
    }

    return( phVPGrp )
}

bS.makeHMtxLst <- function( gEnv, allIdxLst, fRstLst ,tgt.scMtx=NULL ,lastH=NULL ){

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
    for( sfcIdx in names(sfcHLst) ){    # sfcIdx <- names(sfcHLst)[2]

        scoreMtx.grp.lst <- list( )
        for( hIdx in sfcHLst[[sfcIdx]] ){   # hIdx <- sfcHLst[[sfcIdx]][1]
            stdZoid <- gEnv$zhF[hIdx ,]
            wEnv <- gEnv
            wEnv$zhF <- gEnv$zhF[1:(hIdx-1),]

            fRstLst.w <- fRstLst[as.character(fRstLst.hSpan[fRstLst.hSpan<hIdx])]

            aZoidMtx <- matrix(stdZoid ,nrow=1)
            phVP.grp <- bS.getPhVPGrp( wEnv ,aZoidMtx )
            # stdMI.grp <- bUtil.getStdMILst( wEnv ,fRstLst.w )
            # filter.grp <- getFilter.grp( stdMI.grp ,tgt.scMtx )

            # scoreMtx.grp <- getScoreMtx.grp.4H( stdZoid ,filter.grp )
            # scoreMtx.grp.lst[[sprintf("hIdx:%d",hIdx)]] <- scoreMtx.grp
        }

        basicHMtxLst <- list()
        scoreMtxNames <- names(scoreMtx.grp.lst[[1]]$basic[[1]]) # 필터링이 아닌 H를 보려하는 것이므로 basic만 다룬다.
        for( nIdx in names(scoreMtx.grp.lst[[1]]$basic) ){ # nIdx<-names(scoreMtx.grp.lst[[1]]$basic)[1]
            # mtxLst <- list()
            # for( smnIdx in scoreMtxNames ){ # smnIdx <-scoreMtxNames[1]
            #     scoreMtx <- NULL    ;infoMtx<-NULL
            #     for( rIdx in seq_len(length(scoreMtx.grp.lst)) ){
            #         scoreObj <- scoreMtx.grp.lst[[rIdx]]$basic[[nIdx]][[smnIdx]]
            #         scoreMtx <- rbind( scoreMtx ,scoreObj$scoreMtx[1,] )
            #         if( any(is.na(scoreObj$scoreMtx[1,])) ){
            #             hStr <- sfcHLst[[sfcIdx]][rIdx]
            #             colStr <- paste( names(scoreObj$scoreMtx[1,])[which(is.na(scoreObj$scoreMtx[1,]))],collapse=",")
            #             k.FLogStr(sprintf("WARN : NA - %s, %s, %s(%s), %s",sfcIdx,nIdx,smnIdx,colStr,hStr)
            #                         ,pConsole=T
            #                     )
            #         }
            #         if( !is.null(scoreObj$infoMtx) ){
            #             infoMtx <- rbind( infoMtx ,scoreObj$infoMtx[1,] )
            #         }
            #     }

            #     if( !is.null(scoreMtx) )    rownames(scoreMtx) <- sfcHLst[[sfcIdx]]

            #     if( !is.null(infoMtx) ) rownames(infoMtx) <- sfcHLst[[sfcIdx]]

            #     mtxLst[[smnIdx]] <- list( scoreMtx=scoreMtx ,infoMtx=infoMtx )
            # }
            # basicHMtxLst[[nIdx]] <- mtxLst
        }

        scoreMtxLst[[sfcIdx]] <- basicHMtxLst
    }


    mtxInfoLst <- lapply( scoreMtxLst[[1]][[1]] ,function( pLst ){
                        colnames(pLst$scoreMtx)
                    })
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

}


bS.getFilter.grp <- function( phVP.grp ,tgt.scMtx=NULL ){
    # phVP.grp <- bS.getPhVPGrp( wEnv ,aZoidMtx )

    #   bFMtx.R 의 getFilter.grp() 참고
}
