
cutH.InitialCut <- function( gEnv ,allIdxF ,blk ,filter.grp ,cut.grp ,fHName=NULL ,exeCfg=NULL ,logger=NULL ){
    #   logger <- k.getFlogObj( "./log/commonLog.txt" )

    logMsg <- function( msgStr ,tStmp=NULL ){
        if( is.null(logger) )   return()
        
        if( !is.null(tStmp) ){
            tDiff <- Sys.time() - tStmp
            msgStr <- sprintf("%s   cost:%5.1f%s",msgStr,tDiff,units(tDiff))
        }
        logger$fLogStr( msgStr )
    }

    if( is.null(exeCfg) ){  
        # 적용 대기. "FCust_stdCut.rawRow"들만을 먼저 돌리는 게 속도향상이 될까?
        #   scoreMtx.grp의 중복계산이 될 수 있음...
        exeCfg <- c( "stdCut.rawRow"=T ,"stdCut.hIdx"=T )
    }
    hName <- "sfcLate"  # fHName 을 sfcLate 로만 고정.(근데 별 쓸모없다...)
    if( !is.null(fHName) ){
        hName <- fHName
    }

    flagName <- blk["start"]:blk["end"]
    surFlag <- rep( T ,length(flagName) )   ;names(surFlag) <- flagName

    blkSpan <- blk["start"]:blk["end"]

    timeCost <- c( "score1"=26 ,"score2"=23 ,"score3"=19 ,"score4"=59 ,"score5"=94 ,"score6"=54 ,"score7"=61 ,"score8"=21 ,"score9"=30 )
    timeCost <- c( timeCost ,"bScr01"=1 ,"bScr02"=1 )
    timeCost <- c( timeCost ,"scoreA"=29 ,"scoreB"=24 ,"scoreC"=24 ,"scoreD"=26 )
    timeCost <- c( timeCost ,"scoreE"=100 ,"scoreF"=100 )  # 측정필요.
    if( FALSE ){ # aux info
        # mName 별 소요시간 참고(aIdx 1000개). bScr도 나중에 추가할 것.
        #         score1 is done.(cut  925/10000)   cost:  2.6mins
        #         score2 is done.(cut 1152/10000)   cost:  2.3mins
        #         score3 is done.(cut  836/10000)   cost:  1.9mins
        #         score4 is done.(cut  209/10000)   cost:  5.9mins
        #         score5 is done.(cut 2608/10000)   cost:  9.4mins
        #         score6 is done.(cut 1150/10000)   cost:  5.4mins
        #         score7 is done.(cut   66/10000)   cost:  6.1mins
        #         score8 is done.(cut 4904/10000)   cost:  2.1mins
        #         score9 is done.(cut 1827/10000)   cost:  3.0mins
        #         bScr01 is done.(cut 1296/10000)   cost:  3.3secs
        #         bScr02 is done.(cut  508/10000)   cost:  6.8secs
        #         scoreA is done.(cut 2705/10000)   cost:  2.9mins
        #         scoreB is done.(cut 1397/10000)   cost:  2.4mins
        #         scoreC is done.(cut 1143/10000)   cost:  2.4mins
        #         scoreD is done.(cut  474/10000)   cost:  2.6mins
    }

    mtxNames <- names(timeCost)[order(timeCost)]    # 가장 소요시작 작은 것 부터 mtxNames 등록
    for( mName in mtxNames ){   # mName <- mtxNames[1]
        tStmp <- Sys.time()

        surIdxSpan <- which(surFlag)
        aIdxSpan <- allIdxF[ blkSpan[surIdxSpan] ]
        scoreMtx.grp <- getScoreMtx.grp( gEnv$allZoidMtx[aIdxSpan,,drop=F] ,filter.grp ,tgt.scMtx=mName )

        cutRst <- bUtil.cut1( scoreMtx.grp ,cut.grp ,hName ,tgt.scMtx=mName ,anaOnly=F )
        surFlag[ surIdxSpan[!cutRst$surFlag] ] <- FALSE

        logMsg( sprintf("    %s is done.(cut %d/%d)",mName,sum(!cutRst$surFlag),length(cutRst$surFlag)) ,tStmp )

        if( all(!surFlag) ) break

        # logMsg( sprintf("    %s is done.",mName) ,tStmp )
    }

    return( list( surFlag=surFlag ) )

    if( FALSE ){    # time Cost 측정 테스트 코드
        # mtxNames <- c("score1" ,"score2" ,"score3" ,"score4" ,"score5" ,"score6" ,"score7" ,"score8" ,"score9" ,"bScr01" ,"bScr02" )

        # for( mName in mtxNames ){   # mName <- mtxNames[1]
        #     tStmp <- Sys.time()

        #     surIdxSpan <- which(surFlag)
        #     aIdxSpan <- allIdxF[ blkSpan[surIdxSpan] ]
        #     scoreMtx.grp <- getScoreMtx.grp( gEnv$allZoidMtx[aIdxSpan,,drop=F] ,filter.grp ,tgt.scMtx=mName )

        #     cutRst <- bUtil.cut1( scoreMtx.grp ,cut.grp ,hName ,tgt.scMtx=mName ,anaOnly=F )
        #     # surFlag[ surIdxSpan[!cutRst$surFlag] ] <- FALSE

        #     logMsg( sprintf("    %s is done.(cut %d/%d)",mName,sum(!cutRst$surFlag),length(cutRst$surFlag)) ,tStmp )
        # }
    }

}


#   Working
cutH.MultiMtxCut <- function( gEnv ,allIdxF ,blk ,filter.grp ,cut.grp ,fHName ,tgt.scMtx ,logger=NULL ){
    logMsg <- function( msgStr ,tStmp=NULL ){
        if( is.null(logger) )   return()
        
        if( !is.null(tStmp) ){
            tDiff <- Sys.time() - tStmp
            msgStr <- sprintf("%s   cost:%5.1f%s",msgStr,tDiff,units(tDiff))
        }
        logger$fLogStr( msgStr )
    }

    blkSpan <- blk["start"]:blk["end"]
    scoreMtx.grp <- getScoreMtx.grp( gEnv$allZoidMtx[allIdxF[blkSpan],,drop=F] ,filter.grp ,tgt.scMtx=tgt.scMtx )

    cutRst <- bUtil.cut1( scoreMtx.grp ,cut.grp ,hName ,tgt.scMtx=tgt.scMtx ,anaOnly=F )

    return( list( surFlag=cutRst$surFlag ) )
}

#   need cut test
cutH.bC.Cut <- function( gEnv ,allIdxF ,blk ,filter.grp ,tgt.scMtx ,logger=NULL ){

    logMsg <- function( msgStr ,tStmp=NULL ){
        if( is.null(logger) )   return()
        
        if( !is.null(tStmp) ){
            tDiff <- Sys.time() - tStmp
            msgStr <- sprintf("%s   cost:%5.1f%s",msgStr,tDiff,units(tDiff))
        }
        logger$fLogStr( msgStr )
    }

    blkSpan <- blk["start"]:blk["end"]
    scoreMtx.grp <- getScoreMtx.grp( gEnv$allZoidMtx[allIdxF[blkSpan],,drop=F] ,filter.grp ,tgt.scMtx=tgt.scMtx )

    crCutRst <- bC.cut( crMName ,scoreMtx.grp ,cut.grp ,anaOnly=F )

    return( list( surFlag=crCutRst$surFlag ) )

}


cutH.bS.Cut <- function( gEnv ,allIdxF ,hMtxLst_bS ,fHName ,tgt.scMtx=NULL ){
    #  gEnv ,stdZoid ,hMtxLst_bS ,fHName 

    tStmp <- Sys.time()

    aZoidMtx <- gEnv$allZoidMtx[allIdxF ,,drop=F]
    phVP.grp <- bS.getPhVPGrp( gEnv ,aZoidMtx )

    cut.grp <- bS.getCutGrp( hMtxLst_bS ,tgt.scMtx=tgt.scMtx )  # curHMtxLst 적용 추가 필요.

    scoreMtx.grp <- bS.getScoreMtx.grp( phVP.grp ,aZoidMtx ,tgt.scMtx=tgt.scMtx )

    cutRst.bS <- bS.cut( scoreMtx.grp ,cut.grp ,fHName ,tgt.scMtx=tgt.scMtx ,anaOnly=F ) 

    cutRst.bS$tDiff <- Sys.time() - tStmp

    return( cutRst.bS )
}



CRpt.cutRst1Score <- function( aZoidMtx ,filter.grp ,cut.grp ,fHName ,logFile="CRpt_CutRst1Score" ){
    #   aZoidMtx <- gEnv$allZoidMtx[sort(allIdxF),,drop=F]

    logger <- k.getFlogObj( sprintf("./report/workRpt/%s.txt",logFile) )
    logger$fLogStr("Start",pTime=T,pAppend=F)

    tgt.scMtx <- c( "score1","score2","score3","score4","score5","score6","score7","score8","score9" )

    tStmp <- Sys.time()
    scoreMtx.grp <- getScoreMtx.grp( aZoidMtx ,filter.grp )
    cutRst1Score <- bUtil.getCut1Score( scoreMtx.grp ,cut.grp ,fHName ,tgt.scMtx=tgt.scMtx )
    tDiff <- Sys.time() - tStmp

    aLen <- length(cutRst1Score$aLst)
    cName <- c("r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg")
    scMtx_rebCnt <- matrix( 0 ,nrow=aLen ,ncol=length(cName) 
                    ,dimnames=list(names(cutRst1Score$aLst),cName)
    )
    for( idx in seq_len(aLen) ){
        for( hName in "sfcLate" ){    # names(cutRst1Score$aLst[[idx]])
            scMtx.sz <- NULL
            for( mName in names(cutRst1Score$aLst[[idx]][[hName]]$basic) ){
                rawObj <- cutRst1Score$aLst[[idx]][[hName]]$basic[[mName]]$raw
                summObj <- cutRst1Score$aLst[[idx]][[hName]]$basic[[mName]]$summ

                if( is.null(scMtx.sz) ){    scMtx.sz <- summObj$scMtx.sz
                } else {    scMtx.sz <- scMtx.sz + summObj$scMtx.sz    }
            }
            scMtx_rebCnt[idx,] <- scMtx.sz["rebCnt",]   #  일단 sfcLate에 대해서만 관찰하자.
        }
    }

}



# Code BackUp
if( FALSE ){    # cutH.InitialCut() 사용 이후로 필요 없어진 듯 하다.
    # surFlag <- rep( T ,length(allIdxF) )
    # bLst <- k.blockLst( length(allIdxF) ,100*ifelse(testMode,2,1000) )

    # sfExport("fHName")  ;sfExport("allIdxF")
    # resultLst <- sfLapply( bLst ,function( blk ){
    #     tStmp <- Sys.time()
    #     span1nd <- blk["start"]:blk["end"]
    #     scoreMtx.grp <- getScoreMtx.grp( gEnv$allZoidMtx[allIdxF[span1nd],,drop=F] ,filter.grp )

    #     cutRst <- bUtil.cut1( scoreMtx.grp ,cut.grp ,fHName ,tgt.scMtx=tgt.scMtx ,anaOnly=F )

    #     tDiff <- Sys.time() - tStmp
    #     logStr <- sprintf("  block finished for cut1. %d/%d  %5.1f%s for %d~%d "
    #                         ,sum(!cutRst$surFlag),length(cutRst$surFlag)
    #                         ,tDiff  ,units(tDiff)
    #                         ,blk["start"] ,blk["end"]
    #                 )
    #     prllLog$fLogStr( logStr )
        
    #     return( list( surFlag=cutRst$surFlag ,blk=blk ) )
    # })
    # for( idx in seq_len(length(resultLst)) ){
    #     blk <- resultLst[[idx]]$blk
    #     surFlag[ blk["start"]:blk["end"] ] <- resultLst[[idx]]$surFlag
    # }
    # allIdxF <- allIdxF[surFlag]
    # logger$fLogStr(sprintf("   - bUtil.cut1()   final size :%7d",length(allIdxF)),pTime=T)
    # save( allIdxF ,file=sprintf("Obj_allIdxF%d_cut1.save",sfcIdx) )
}



