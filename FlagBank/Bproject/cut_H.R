
cutH.InitialCut <- function( gEnv ,allIdxF ,blk ,filter.grp ,fHName=NULL ,logger=NULL ){
    #   logger <- k.getFlogObj( "./log/commonLog.txt" )

    logMsg <- function( msgStr ,tStmp=NULL ){
        if( is.null(logger) )   return()
        
        if( !is.null(tStmp) ){
            tDiff <- Sys.time() - tStmp
            msgStr <- sprintf("%s   cost:%5.1f%s",msgStr,tDiff,units(tDiff))
        }
        logger$fLogStr( msgStr )
    }

    hName <- "sfcLate"  # fHName 을 sfcLate 로만 고정.(근데 별 쓸모없다...)
    if( !is.null(fHName) ){
        hName <- fHName
    }

    flagName <- blk["start"]:blk["end"]
    surFlag <- rep( T ,length(flagName) )   ;names(surFlag) <- flagName

    blkSpan <- blk["start"]:blk["end"]

    # mName 별 소요시간 참고(aIdx 1000개). bScr도 나중에 추가할 것.
    #     score1 : 19.4secs (cut  85/1000)
    #     score2 : 16.4secs (cut 232/1000)
    #     score3 : 14.9secs (cut 155/1000)
    #     score4 :  1.0mins (cut  30/1000)
    #     score5 :  1.9mins (cut 324/1000)
    #     score6 : 59.2secs (cut  31/1000)
    #     score7 :  1.1mins (cut  14/1000)
    #     score8 : 22.8secs (cut  43/1000)
    #     score9 : 30.9secs (cut 130/1000)
    #     bScr01 :  0.5secs (cut 175/1000)
    #     bScr02 :  1.1secs (cut  86/1000) <--- 확인 요.

    timeCost <- c( "score1"=19 ,"score2"=16 ,"score3"=15 ,"score4"=60 ,"score5"=114 ,"score6"=59 ,"score7"=66 ,"score8"=23 ,"score9"=30 )
    timeCost <- c( timeCost ,"bScr01"=0 ,"bScr02"=1 )

    mtxNames <- names(timeCost)[order(timeCost)]    # 가장 소요시작 작은 것 부터 mtxNames 등록
    for( mName in mtxNames ){   # mName <- mtxNames[1]
        tStmp <- Sys.time()

        surIdxSpan <- which(surFlag)
        aIdxSpan <- allIdxF[ blkSpan[surIdxSpan] ]
        scoreMtx.grp <- getScoreMtx.grp( gEnv$allZoidMtx[aIdxSpan,,drop=F] ,filter.grp ,tgt.scMtx=mName )

        cutRst <- bUtil.cut1( scoreMtx.grp ,cut.grp ,hName ,tgt.scMtx=mName ,anaOnly=F )
        surFlag[ surIdxSpan[!cutRst$surFlag] ] <- FALSE

        logMsg( sprintf("    %s is done.(cut %d/%d)",mName,sum(!cutRst$surFlag),length(cutRst$surFlag)) ,tStmp )
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



