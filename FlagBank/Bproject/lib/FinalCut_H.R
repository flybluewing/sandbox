
FC.primaryCut.static <- function( allIdxF ,gEnv ){

    # mtx <- apply( gEnv$zhF ,1 ,function(zoid){ c(zoid[1],zoid[6]-zoid[1],zoid[6]) })
    # mtx <- t(mtx)   ;colnames(mtx) <- c("c1","zw","c6")
    lastZoid <- gEnv$zhF[nrow(gEnv$zhF),]

    aZoidMtx <- gEnv$allZoidMtx[allIdxF,]
    flag <- apply( aZoidMtx ,1 ,function(aZoid){3<=sum(aZoid==lastZoid)})
    allIdxF <- allIdxF[ !flag ]

    aZoidMtx <- gEnv$allZoidMtx[allIdxF,]
    flag <- apply( aZoidMtx ,1 ,function(aZoid){4<=sum(aZoid%in%lastZoid)})
    allIdxF <- allIdxF[ !flag ]

    # ==============================================================
        # table( mtx[,"c1"]>15 )  #   66/860
        # table( mtx[,"c6"]<30 )  #   51/860
        # table( mtx[,"zw"]<20 )  #   37/860
        # table( mtx[,"zw"]>42 )  #   38/860
    aZoidMtx <- gEnv$allZoidMtx[allIdxF,]
    allIdxF <- allIdxF[aZoidMtx[,1]<15]

    if( lastZoid[1]>=10 ){
        aZoidMtx <- gEnv$allZoidMtx[allIdxF,]
        allIdxF <- allIdxF[ aZoidMtx[,1]!=lastZoid[1] ]
    }

    if( lastZoid[6] <30 ){
        aZoidMtx <- gEnv$allZoidMtx[allIdxF,]
        allIdxF <- allIdxF[ aZoidMtx[,6]!=lastZoid[6] ]
    }

    l.zw <- lastZoid[6]-lastZoid[1]
    if( l.zw<20 || 42<l.zw ){
        aZoidMtx <- gEnv$allZoidMtx[allIdxF,]
        allIdxF <- allIdxF[ l.zw != (aZoidMtx[,6]-aZoidMtx[,1]) ]
    }

    # ==============================================================

    #   rebCnt
    l.rebCnt <- sum(lastZoid %in% gEnv$zhF[nrow(gEnv$zhF)-1,])

    a.rebCnt <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){ sum(aZoid %in% lastZoid) })
    allIdxF <- allIdxF[ a.rebCnt<4 ]

    if( l.rebCnt==3 ){
        a.rebCnt <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){ sum(aZoid %in% lastZoid) })
        allIdxF <- allIdxF[ a.rebCnt<3 ]
    }

    #   rem
    l.rem <- lastZoid %% 10
    flag <- apply( gEnv$allZoidMtx[allIdxF,]%%10 ,1 ,function(a.rem){  
                3 <= sum(l.rem==a.rem)
    })
    allIdxF <- allIdxF[ !flag ]

    if( any(l.rem[1:5]==l.rem[2:6]) ){
        flag <- apply( gEnv$allZoidMtx[allIdxF,]%%10 ,1 ,function(a.rem){  
                    any( a.rem[1:5]==a.rem[2:6] )
        })
        allIdxF <- allIdxF[ !flag ]
    }
    if( 3<=max(table(l.rem)) ){
        flag <- apply( gEnv$allZoidMtx[allIdxF,]%%10 ,1 ,function(a.rem){  
                    3<=max(table(l.rem))
        })
        allIdxF <- allIdxF[ !flag ]
    }

    #   cStep
    lCStep <- lastZoid[2:6]-lastZoid[1:5]
    cnt <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
        aCStep <- aZoid[2:6]-aZoid[1:5]
        sum(lCStep==aCStep)
    })
    allIdxF <- allIdxF[ 3>cnt ]

    #   fStep
    lFStep <- lastZoid - gEnv$zhF[nrow(gEnv$zhF)-1,]
    cnt <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
        aFStep <- aZoid - lastZoid
        sum(lFStep==aFStep)
    })
    allIdxF <- allIdxF[ 3>cnt ]

    return( allIdxF )
}


#   score를 생성함과 동시에 생존 여부 확인하여 필터링.
#       그냥 scoreMtx를 만들면 getScoreMtx.grp() 함수가 부하량을 못 견딘다.
#       return : allIdxF (trimmed)
FC.primaryCut.bySC <- function( allIdxF ,gEnv ,filter.grp ,fHName ,cut.grp ,logFile="primaryCut.bySC.txt" ){
    #   ;logFile="primaryCut.bySC.txt"
    cutting <- function( scoreMtx ,cut.grp ,mName ,pName ,fHName ){
        surFlag <- rep( T ,nrow(scoreMtx) )

        for( hName in fHName ){
            stdLst <- cut.grp$cutterLst[[hName]][[mName]]$stdLst[[pName]]
            for( cnIdx in names(stdLst) ){    # cnIdx <- names(stdLst)[1]
                cuttedLst <- cutLst[[cnIdx]]$cut( scoreMtx ,!surFlag )
                if( 0==length(cuttedLst) ) next

                cut_aIdx <- sapply( cuttedLst ,function(p){p$idx} )
                surFlag[cut_aIdx] <- FALSE
            }
        }

        return( surFlag )
    } # cutting()


    logger=k.getFlogObj( sprintf("./log/%s",logFile) )
    logger$fLogStr("start", pTime=T ,pAppend=F )
    makeLog <- function( logger, mName, pName, logStr, tStmp=NULL ){
        if( !is.null(tStmp) ){
            tDiff <- Sys.time() - tStmp
            logStr <- sprintf("(%4.1f%s) %s",tDiff,units(tDiff),logStr)
        }

        logger$fLogStr( sprintf("    %s,%s - %s",mName,pName,logStr) )
    } # makeLog( )

    #   mName<-names(filter.grp$basic$basic)[1] ;pName<-cut.grp$phaseName[1]    ;hName <- fHName[1]
    for( mName in names(filter.grp$basic$basic) ){  # mName <- names(filter.grp$basic$basic)
        for( pName in cut.grp$phaseName ){  # pName<-cut.grp$phaseName[1]
            filterObj <- filter.grp$basic[[pName]][[mName]]

            surFlag <- rep( T ,length(allIdxF) )
            bLst <- k.blockLst( length(allIdxF) ,10*10000 )
            # if( TRUE ){ ;qqe:rbf
            #     allIdxF <- allIdxF[sample(1:length(allIdxF),3500)]
            #     surFlag <- rep( T ,length(allIdxF) )
            #     bLst <- k.blockLst( length(allIdxF) ,3000 )
            # }

            tStmp <- Sys.time()
            for( bName in names(bLst) ){    # bName <- names(bLst)[1]
                span <- bLst[[bName]]["start"]:bLst[[bName]]["end"] 
                scoreMtxObj <- filterObj$fMtxObj( gEnv$allZoidMtx[span,,drop=F] ,makeInfoStr=F )
                surFlag.blk <- cutting( scoreMtxObj$scoreMtx ,cut.grp ,mName ,pName ,fHName )
                surFlag[span] <- surFlag.blk

                logStr <- sprintf("  %s block finished.(remove:%d/%d)",bName,sum(!surFlag.blk),length(surFlag.blk))
                makeLog( logger, mName, pName, logStr, tStmp )
            }

            allIdxF <- allIdxF[surFlag]
            makeLog( logger, mName, pName, sprintf("survive %d from %d",sum(surFlag),length(surFlag)), tStmp )
        }
    }


    return( allIdxF )

} # FC.primaryCut.bySC( )


