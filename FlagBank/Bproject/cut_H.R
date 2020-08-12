
cutH.InitialCut <- function( gEnv ,allIdxF ,blk ,filter.grp ,fHName=NULL ,exeCfg=NULL ,logger=NULL ){
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

cutH.bC.Cut <- function( gEnv ,allIdxF ,blk ,filter.grp ,tgt.scMtx ,logger=NULL ){

    logMsg <- function( msgStr ,tStmp=NULL ){
        if( is.null(logger) )   return()
        
        if( !is.null(tStmp) ){
            tDiff <- Sys.time() - tStmp
            msgStr <- sprintf("%s   cost:%5.1f%s",msgStr,tDiff,units(tDiff))
        }
        logger$fLogStr( msgStr )
    }

    flagName <- blk["start"]:blk["end"]
    surFlag <- rep( T ,length(flagName) )   ;names(surFlag) <- flagName

    blkSpan <- blk["start"]:blk["end"]
    scoreMtx.grp <- getScoreMtx.grp( gEnv$allZoidMtx[allIdxF[blkSpan],,drop=F] ,filter.grp ,tgt.scMtx=mName )

    crCutRst <- bC.cut( crMName ,scoreMtx.grp ,cut.grp ,anaOnly=F )

    return( list( surFlag=crCutRst$surFlag ) )

}

