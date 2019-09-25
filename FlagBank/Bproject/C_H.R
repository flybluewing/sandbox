





#   score를 생성함과 동시에 생존 여부 확인하여 필터링.
#       그냥 scoreMtx를 만들면 getScoreMtx.grp() 함수가 부하량을 못 견딘다.
#       return : surFlag
C.primaryCut.bySC <- function( allIdxF ,gEnv ,filter.grp ,fHName ,cut.grp ,logFile="primaryCut.bySC.txt" ){
    #   ;logFile="primaryCut.bySC.txt"
    logger=k.getFlogObj( sprintf("./log/%s",logFile) )
    logger$fLogStr("start", pTime=T ,pAppend=F )

    # bLst <- k.blockLst( length(allIdxF) ,20*10000 )
    bLst <- k.blockLst( length(allIdxF) ,20 )   ;qqe:rbf

    #   mName<-names(filter.grp$basic$basic)[1] ;pName<-cut.grp$phaseName[1]    ;hName<-fHName[1]
    for( mName in names(filter.grp$basic$basic) ){  # mName <- names(filter.grp$basic$basic)
        for( pName in cut.grp$phaseName ){  # pName<-cut.grp$phaseName[1]
            filterObj <- filter.grp$basic[[pName]][[mName]]

            for( bName in names(bLst) ){
                span <- bLst[[bName]]["start"]:bLst[[bName]]["end"] 
                scoreMtxObj <- filterObj$fMtxObj( gEnv$allZoidMtx[,,drop=F] ,makeInfoStr=F )
            }

        }
    }


    return( surFlag )

} # C.primaryCut.bySC( )
