

#   score를 생성함과 동시에 생존 여부 확인하여 필터링.
#       그냥 scoreMtx를 만들면 getScoreMtx.grp() 함수가 부하량을 못 견딘다.
#       return : surFlag
C.primaryCut.bySC <- function( allIdxF ,gEnv ,filter.grp ,cut.grp ,logFile="primaryCut.bySC.txt" ){
    #   aZoidMtx=gEnv$allZoidMtx[allIdxF,,drop=F]  ;scName="score2"    ;logFile="primaryCut.bySC.txt"
    logger=k.getFlogObj( sprintf("./log/%s",logFile) )
    logger$fLogStr("start", pTime=T ,pAppend=F )

    aZoidLen <- length(allIdxF)
    surFlag <- rep( F, aZoidLen )

    aZoidLen <- 37
    blockSize <- 10
    blockLst <- list()
    for( idx in 0:(aZoidLen%/%blockSize) ){
        if( aZoidLen <= blockSize*idx ){
            blockLst[[1+length(blockLst)]] <- 
        } else {
            blockLst[[1+length(blockLst)]] <- 
        }
    }

    #   mName<-names(filter.grp$basic$basic)[1] ;pName<-cut.grp$phaseName[1]
    for( mName in names(filter.grp$basic$basic) ){  # mName <- names(filter.grp$basic$basic)
        for( pName in cut.grp$phaseName ){  # pName<-cut.grp$phaseName[1]
            filterObj <- filter.grp$basic[[pName]][[mName]]
            scoreMtxObj <- filterObj$fMtxObj( aZoidMtx ,makeInfoStr=F )
        }
    }


    return( surFlag )

} # C.primaryCut.bySC( )
