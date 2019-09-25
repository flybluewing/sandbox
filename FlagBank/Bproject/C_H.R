





#   score를 생성함과 동시에 생존 여부 확인하여 필터링.
#       그냥 scoreMtx를 만들면 getScoreMtx.grp() 함수가 부하량을 못 견딘다.
#       return : surFlag
C.primaryCut.bySC <- function( allIdxF ,gEnv ,filter.grp ,fHName ,cut.grp ,logFile="primaryCut.bySC.txt" ){
    #   ;logFile="primaryCut.bySC.txt"
    cutting <- function( scoreMtx ,cut.grp ,mName ,pName ,fHName ){
        surFlag <- rep( T ,nrow(scoreMtx) )

        for( hName in fHName ){
            cutLst <- cut.grp$cutterLst[[hName]][[mName]]$stdLst[[pName]]
            for( cnIdx in names(stdLst) ){    # cnIdx <- names(stdLst)[1]
                cuttedLst <- cutLst[[cnIdx]]$cut( scoreMtx ,!surFlag )
                cut_aIdx <- sapply( cuttedLst ,function(p){p$idx} )
                surFlag[cut_aIdx] <- FALSE
            }
        }

        return( surFlag )
    } # cutting()


    logger=k.getFlogObj( sprintf("./log/%s",logFile) )
    logger$fLogStr("start", pTime=T ,pAppend=F )
    makeLog <- function( logger, mName, pName, str, tStmp=NULL ){
        if( !is.null(tStmp) ){
            tDiff <- Sys.time() - tStmp
            str <- sprintf("(%.1f%s) %s",tDiff,units(tdiff),str)
        }

        logger$fLogStr("    %s,%s - %s",mName,pName,str)
    } # makeLog( )

    #   mName<-names(filter.grp$basic$basic)[1] ;pName<-cut.grp$phaseName[1]    ;hName <- fHName[1]
    for( mName in names(filter.grp$basic$basic) ){  # mName <- names(filter.grp$basic$basic)
        for( pName in cut.grp$phaseName ){  # pName<-cut.grp$phaseName[1]
            filterObj <- filter.grp$basic[[pName]][[mName]]

            surFlag <- rep( T ,length(allIdxF) )
            # bLst <- k.blockLst( length(allIdxF) ,20*10000 )
            bLst <- k.blockLst( length(allIdxF) ,20 )   ;qqe:rbf

            tStmp <- Sys.time()
            for( bName in names(bLst) ){
                span <- bLst[[bName]]["start"]:bLst[[bName]]["end"] 
                scoreMtxObj <- filterObj$fMtxObj( gEnv$allZoidMtx[,,drop=F] ,makeInfoStr=F )
                surFlag.blk <- cutting( scoreMtxObj$scoreMtx ,cut.grp ,mName ,pName ,fHName )

                logStr <- sprintf("  %s block finished.(%d/%d)",bName,sum(surFlag.blk),length(surFlag.blk))
                makeLog( logger, mName, pName, logStr, tStmp )
            }

            allIdxF <- allIdxF[surFlag]
            makeLog( logger, mName, pName, sprintf("survive %d from %d",sum(surFlag),length(surFlag)), tStmp )
        }
    }


    return( surFlag )

} # C.primaryCut.bySC( )
