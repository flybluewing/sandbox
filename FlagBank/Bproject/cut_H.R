
#   (속도향상을 위한) 초기 cutting. 'sfcLate'에 대해서만 cutting 작업을 진행한다.
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

    hName <- "sfcLate"  # fHName 을 sfcLate 로만 고정.
    if( !is.null(fHName) ){
        hName <- fHName
    }

    flagName <- blk["start"]:blk["end"]
    surFlag <- rep( T ,length(flagName) )   ;names(surFlag) <- flagName

    blkSpan <- blk["start"]:blk["end"]

    # mName 별 소요시간 참고(aIdx 1000개). bScr도 나중에 추가할 것.
    #     score1 :   0.8secs
    #     score2 :   0.7secs
    #     score3 :   5.4secs
    #     score4 :  43.4secs
    #     score5 :   1.6mins
    #     score6 :  41.0secs
    #     score7 :  45.8secs
    #     score8 :   7.4secs
    #     score9 :  13.8secs

    timeCost <- c("score1"=1 ,"score2"=1 ,"score3"=5 ,"score4"=43 ,"score5"=96 ,"score6"=41 ,"score7"=45 ,"score8"=7 ,"score9"=13 )
    timeCost <- c( timeCost )   # 차후 bScr 부분 추가

    mtxNames <- names(timeCost)[order(timeCost)]    # 가장 소요시작 작은 것 부터 mtxNames 등록
    for( mName in mtxNames ){   # mName <- mtxNames[1]
        tStmp <- Sys.time()

        surIdxSpan <- which(surFlag)
        aIdxSpan <- allIdxF[ blkSpan[surIdxSpan] ]
        scoreMtx.grp <- getScoreMtx.grp( gEnv$allZoidMtx[aIdxSpan,,drop=F] ,filter.grp ,tgt.scMtx=mName )

        cutRst <- bUtil.cut1( scoreMtx.grp ,cut.grp ,hName ,tgt.scMtx=mName ,anaOnly=F )
        surFlag[ surIdxSpan[!cutRst$surFlag] ] <- FALSE

        # logMsg( sprintf("    %s is done.(%d->%d)",mName,sum(!cutRst$surFlag),sum(surFlag)) ,tStmp )
        logMsg( sprintf("    %s is done.",mName) ,tStmp )
    }

    return( list( surFlag=surFlag ) )

}



