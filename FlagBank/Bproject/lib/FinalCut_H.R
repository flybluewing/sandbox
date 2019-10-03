
FC.primaryCut.static <- function( allIdxF ,gEnv ,filtTest=F ){

    checkExit <- function( allIdxF ){

        if( 1>=length(allIdxF) )    return( TRUE )
        return( FALSE )
    }

    filtedId <- integer(0)
    zhF <- gEnv$zhF                 #   zhF <- gEnv.w$zhF   ;allZoidMtx <- gEnv.w$allZoidMtx
    allZoidMtx <- gEnv$allZoidMtx

    # mtx <- apply( gEnv$zhF ,1 ,function(zoid){ c(zoid[1],zoid[6]-zoid[1],zoid[6]) })
    # mtx <- t(mtx)   ;colnames(mtx) <- c("c1","zw","c6")
    lastZoid  <- zhF[nrow(zhF)  ,]
    lastZoid2 <- zhF[nrow(zhF)-1,]


    aZoidMtx <- allZoidMtx[allIdxF,]
    flag <- apply( aZoidMtx ,1 ,function(aZoid){3<=sum(aZoid==lastZoid)})
    if( filtTest ){
        if( flag[1] ) filtedId <- c(filtedId,"01.0")
    } else {    allIdxF <- allIdxF[ !flag ]   }
    if( checkExit(allIdxF) ) return( NA )

    aZoidMtx <- allZoidMtx[allIdxF,]
    flag <- apply( aZoidMtx ,1 ,function(aZoid){4<=sum(aZoid%in%lastZoid)})
    if( filtTest ){
        if( flag[1] ) filtedId <- c(filtedId,"01.1")
    } else {    allIdxF <- allIdxF[ !flag ]   }
    if( checkExit(allIdxF) ) return( NA )

    # ==============================================================
        # table( mtx[,"c1"]>15 )  #   66/860
        # table( mtx[,"c6"]<30 )  #   51/860
        # table( mtx[,"zw"]<20 )  #   37/860
        # table( mtx[,"zw"]>42 )  #   38/860
    aZoidMtx <- allZoidMtx[allIdxF,]
    thld <- 20
    if( filtTest ){
        if( aZoidMtx[1,1]>=thld ) filtedId <- c(filtedId,"02.0")
    } else {    allIdxF <- allIdxF[aZoidMtx[1,1]<thld]   }
    if( checkExit(allIdxF) ) return( NA )

    if( lastZoid[1]>=10 ){
        aZoidMtx <- allZoidMtx[allIdxF,]
        if( filtTest ){
            if( aZoidMtx[1,1]==lastZoid[1] ) filtedId <- c(filtedId,"02.1")
        } else {    allIdxF <- allIdxF[ aZoidMtx[,1]!=lastZoid[1] ]   }
    }
    if( checkExit(allIdxF) ) return( NA )

    if( lastZoid[6] <30 ){
        aZoidMtx <- allZoidMtx[allIdxF,]
        if( filtTest ){
            if( aZoidMtx[1,6]==lastZoid[6] ) filtedId <- c(filtedId,"02.2")
        } else {    allIdxF <- allIdxF[ aZoidMtx[,6]!=lastZoid[6] ]   }
    }
    if( checkExit(allIdxF) ) return( NA )

    l.zw <- lastZoid[6]-lastZoid[1]
    if( l.zw<20 || 42<l.zw ){
        aZoidMtx <- allZoidMtx[allIdxF,]
        if( filtTest ){
            if( l.zw == (aZoidMtx[1,6]-aZoidMtx[1,1]) ) filtedId <- c(filtedId,"02.3")
        } else {    allIdxF <- allIdxF[ l.zw != (aZoidMtx[,6]-aZoidMtx[,1]) ]   }
    }
    if( checkExit(allIdxF) ) return( NA )

    # ==============================================================

    #   rebCnt
    l.rebCnt <- sum(lastZoid %in% lastZoid2)

    a.rebCnt <- apply( allZoidMtx[allIdxF,] ,1 ,function(aZoid){ sum(aZoid %in% lastZoid) })
    if( filtTest ){
        if( a.rebCnt[1]>=4 ) filtedId <- c(filtedId,"03.1")
    } else {    allIdxF <- allIdxF[ a.rebCnt<4 ]   }
    if( checkExit(allIdxF) ) return( NA )


    if( l.rebCnt==3 ){
        a.rebCnt <- apply( allZoidMtx[allIdxF,] ,1 ,function(aZoid){ sum(aZoid %in% lastZoid) })
        if( filtTest ){
            if( a.rebCnt[1]>=3 ) filtedId <- c(filtedId,"03.2")
        } else {    allIdxF <- allIdxF[ a.rebCnt<3 ]   }
    }
    if( checkExit(allIdxF) ) return( NA )

    #   rem
    l.rem <- lastZoid %% 10 ;l2.rem <- lastZoid2 %% 10

    flag <- apply( allZoidMtx[allIdxF,]%%10 ,1 ,function(a.rem){  
                4 <= sum(l.rem==a.rem)
    })
    if( filtTest ){
        if( flag[1] ) filtedId <- c(filtedId,"03.3")
    } else {    allIdxF <- allIdxF[ !flag ]   }
    if( checkExit(allIdxF) ) return( NA )

    if( 2<= sum(l.rem==l2.rem) ){
        flag <- apply( allZoidMtx[allIdxF,]%%10 ,1 ,function(a.rem){  
                    3 <= sum(l.rem==a.rem)
        })
        if( filtTest ){
            if( flag[1] ) filtedId <- c(filtedId,"03.3.a")
        } else {    allIdxF <- allIdxF[ !flag ]   }
    }
    if( checkExit(allIdxF) ) return( NA )

    if( any(l.rem[1:5]==l.rem[2:6]) ){
        fIdx <- which( l.rem[1:5]==l.rem[2:6] )
        flag <- apply( allZoidMtx[allIdxF,]%%10 ,1 ,function(a.rem){
                    any(a.rem[fIdx]==a.rem[fIdx+1])
        })
        if( filtTest ){
            if( flag[1] ) filtedId <- c(filtedId,"03.4")
        } else {    allIdxF <- allIdxF[ !flag ]   }
        if( checkExit(allIdxF) ) return( NA )
    }
    if( 3<=max(table(l.rem)) ){
        flag <- apply( allZoidMtx[allIdxF,]%%10 ,1 ,function(a.rem){  
                    3<=max(table(a.rem))
        })
        if( filtTest ){
            if( flag[1] ) filtedId <- c(filtedId,"03.5")
        } else {    allIdxF <- allIdxF[ !flag ]   }
        if( checkExit(allIdxF) ) return( NA )
    }
    if( 2<=max(table(l.rem)) ){
        tbl <- table(l.rem)
        tbl <- tbl[tbl>1]
        for( val in as.integer(names(tbl)) ){
            vGrpIdx <- which(l.rem==val)

            cFlag <- apply( allZoidMtx[allIdxF,]%%10 ,1 ,function(a.rem){
                # 1 < max(table(a.rem[vGrpIdx]))
                all( l.rem[vGrpIdx]==a.rem[vGrpIdx] )
            })
            if( filtTest ){
                if( cFlag[1] ) filtedId <- c(filtedId,"03.6.n")
            } else {    allIdxF <- allIdxF[ !cFlag ]   }
        }
        if( checkExit(allIdxF) ) return( NA )
    }


    #   cStep
    lCStep <- lastZoid[2:6]-lastZoid[1:5]
    cnt <- apply( allZoidMtx[allIdxF,] ,1 ,function(aZoid){
        aCStep <- aZoid[2:6]-aZoid[1:5]
        sum(lCStep==aCStep)
    })
    flag <- 3<=cnt
    if( filtTest ){
        if( flag[1] ) filtedId <- c(filtedId,"04.1")
    } else {    allIdxF <- allIdxF[ !flag ]   }
    if( checkExit(allIdxF) ) return( NA )

    if( 3<=max(table(lCStep)) ){
        cFlag <- apply( allZoidMtx[allIdxF,] ,1 ,function(aZoid){
            aCStep <- aZoid[2:6]-aZoid[1:5]
            3 <= max(table(aCStep))
        })
        if( filtTest ){
            if( cFlag[1] ) filtedId <- c(filtedId,"04.2")
        } else {    allIdxF <- allIdxF[ !cFlag ]   }
        if( checkExit(allIdxF) ) return( NA )
    }

    if( 2<=max(table(lCStep)) ){    # 동일 cStep 값이 동일위치 재현
        tbl <- table(lCStep)
        tbl <- tbl[tbl>1]

        for( val in as.integer(names(tbl)) ){
            vGrpIdx <- which(lCStep==val)

            cFlag <- apply( allZoidMtx[allIdxF,] ,1 ,function(aZoid){
                aCode <- aZoid[2:6]-aZoid[1:5]
                all(aCode[vGrpIdx]==lCStep[vGrpIdx])
            })
            if( filtTest ){
                if( cFlag[1] ) filtedId <- c(filtedId,"04.3.n")
            } else {    allIdxF <- allIdxF[ !cFlag ]   }
            if( checkExit(allIdxF) ) return( NA )
        }
    }

    #   fStep
    lFStep <- lastZoid - lastZoid2
    cnt <- apply( allZoidMtx[allIdxF,] ,1 ,function(aZoid){
        aFStep <- aZoid - lastZoid
        sum(lFStep==aFStep)
    })
    cFlag <- 3<=cnt
    if( filtTest ){
        if( cFlag[1] ) filtedId <- c(filtedId,"05.0")
    } else {    allIdxF <- allIdxF[ !cFlag ]   }
    if( checkExit(allIdxF) ) return( NA )

    if( 3<=max(table(lFStep)) ){
        cFlag <- apply( allZoidMtx[allIdxF,] ,1 ,function(aZoid){
            aFStep <- aZoid-lastZoid
            3 <= max(table(aFStep))
        })
        if( filtTest ){
            if( cFlag[1] ) filtedId <- c(filtedId,"05.1")
        } else {    allIdxF <- allIdxF[ !cFlag ]   }
        if( checkExit(allIdxF) ) return( NA )
    }

    if( 2<=max(table(lFStep)) ){
        tbl <- table(lFStep)
        tbl <- tbl[tbl>1]
        for( val in as.integer(names(tbl)) ){
            vGrpIdx <- which(lFStep==val)

            cFlag <- apply( allZoidMtx[allIdxF,] ,1 ,function(aZoid){
                aFStep <- aZoid - lastZoid
                all( aFStep[vGrpIdx]==lFStep[vGrpIdx] )
            })
            if( filtTest ){
                if( cFlag[1] ) filtedId <- c(filtedId,"05.2.n")
            } else {    allIdxF <- allIdxF[ !cFlag ]   }
            if( checkExit(allIdxF) ) return( NA )
        }
    }



    # <return> ------------------------------------------------------------------------
    if( filtTest ){ return( filtedId )
    } else {    return( allIdxF )   }

}   # FC.primaryCut.static( )


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
                cuttedLst <- stdLst[[cnIdx]]$cut( scoreMtx ,!surFlag )
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

            tStmp <- Sys.time()
            for( bName in names(bLst) ){    # bName <- names(bLst)[1]
                span <- bLst[[bName]]["start"]:bLst[[bName]]["end"]
                scoreMtxObj <- filterObj$fMtxObj( gEnv$allZoidMtx[allIdxF[span],,drop=F] ,makeInfoStr=F )
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


