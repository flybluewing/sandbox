

bS_stdCut.rawRow <- function( hName ,mName ,pName ,scoreMtxH ){

    rObj <- list( defId=c(hName=hName,mName=mName,pName=pName) )

    hLen <- nrow(scoreMtxH)
    rObj$lastScore <- scoreMtxH[hLen,]
    rObj$available <- TRUE

    if( rObj$available ){
        cfg <- bsScoreMtxCfg[[mName]]
        if( !is.null(cfg) ){

            rObj$lastEvt <- bFCust.getEvt( rObj$lastScore ,cfg$fCol )
            if( hLen>1 ){
                evt2 <- bFCust.getEvt( scoreMtxH[hLen-1 ,] ,cfg$fCol )
                rObj$evtReb <- bFCust.evtComp( evt2["lev",] ,rObj$lastEvt["lev",] ) # ,levMin=2
                if( all(is.na(rObj$evtReb$levDup)) )    rObj$evtReb <- NULL
            }

        } else {
            rObj$available <- FALSE
        }

    }


    rObj$cut <- function( scoreMtx ,alreadyDead=NULL ,anaMode=TRUE ){

        val.len <- nrow(scoreMtx)
        if( is.null(alreadyDead) )	alreadyDead <- rep( F, val.len )

        cutLst <- list()
        if( !rObj$available ) return( list(cutLst=cutLst,surFlag=!alreadyDead) )

        hardFlag <- FALSE    # hardFlag <- rObj$isHard( rObj$defId["pName"] )$flag["p"]  혹시 나중에 필요할지도..
        cfg <- bsScoreMtxCfg[[ rObj$defId["mName"] ]]


        # each fCol --------------------------------------------
        cutLst.fCol <- list()
        for( fcName in names(cfg$fCol) ){
            for( aIdx in seq_len(val.len) ){
                if( !anaMode && alreadyDead[aIdx] ) next

                surWin <- cfg$fCol[[fcName]]$rng[ ,ifelse(hardFlag,"lev1","lev2")]
                val <- scoreMtx[aIdx,fcName]
                if( !bUtil.in(val,surWin) ){
                    alreadyDead[aIdx] <- TRUE

                    infoStr <- sprintf("%s(%d)",fcName,val )
                    cObj <- cutLst.fCol[[as.character(aIdx)]]
                    if( is.null(cObj) ){
                        cutLst.fCol[[as.character(aIdx)]] <- list( idx=aIdx ,info=infoStr )
                    } else {
                        cutLst.fCol[[as.character(aIdx)]]$info <- paste( cObj$info, infoStr, collapse=", " )
                    }

                }

            }

        }

        # sm row: evtCnt  --------------------------------------------
        cutLst.rowE <- list()
        for( aIdx in seq_len(val.len) ){
            if( !anaMode && alreadyDead[aIdx] ) next

            evt.sm <- bFCust.getEvt( scoreMtx[aIdx ,] ,cfg$fCol )
            evtMin <- cfg$evtMax[ifelse(hardFlag,"lev1","lev2"),]
            evtCnt <- sum( evt.sm["lev" ,]>=evtMin["minLev"] ,na.rm=T )
            evtCntH <- sum( evt.sm["lev" ,]>=evtMin["minLevH"] ,na.rm=T )
            if( evtCnt>evtMin["maxHpn"] || evtCntH>evtMin["maxHpnH"] ){
                alreadyDead[aIdx] <- TRUE

                infoStr=""
                if( anaMode ){
                    flag <- evt.sm["lev",]>=evtMin["minLev"]
                    flag[is.na(flag)] <- F

                    infoStr <- sprintf("evtCnt:%d(%s)",evtCnt,paste(evt.sm["lev" ,flag],collapse=","))
                }
                cutLst.rowE[[as.character(aIdx)]] <- list( idx=aIdx ,info=infoStr )
            }
        }

        # sm row: rebound --------------------------------------------
        cutLst.reb <- list()
        checkRawReb.cnt <- sum(rObj$lastScore>0)
        checkRawReb.flag <- checkRawReb.cnt >= cfg$rowReb["rawMin"]
        checkEvtReb.flag <- FALSE
        if( 0<sum(rObj$lastEvt["lev"],na.rm=T) ){
            checkEvtReb.cnt <- c( lowE=sum(rObj$lastEvt["lev",]>0,na.rm=T) ,rareE=sum(rObj$lastEvt["lev",]>1,na.rm=T) )
            checkEvtReb.flag <- any( checkEvtReb.cnt >= cfg$rowReb[c("lowE","rareE")] )
            checkEvtReb.str <- paste(names(checkEvtReb.cnt),checkEvtReb.cnt,sep=".")
            checkEvtReb.str <- paste( checkEvtReb.str ,collapse="," )
        }
        checkEvtReb.Dbl.flag <- !is.null(rObj$evtReb)
        if( checkEvtReb.Dbl.flag ){
            levDup <- rObj$evtReb$levDup[!is.na(rObj$evtReb$levDup)]
            checkEvtReb.Dbl.levDup <- levDup
        }
        for( aIdx in seq_len(val.len) ){
            smRow <- scoreMtx[aIdx ,]
            evt.sm <- bFCust.getEvt(smRow,cfg$fCol)

            # raw Reb
            if( checkRawReb.flag ){
                if( !anaMode && alreadyDead[aIdx] ) next

                if( all(rObj$lastScore==smRow) ){
                    alreadyDead[aIdx] <- TRUE

                    infoStr <- sprintf("rReb(last:%d)",checkRawReb.cnt )
                    cutLst.reb[[as.character(aIdx)]] <- list( idx=aIdx ,info=infoStr )
                }
            }

            # evt Reb
            if( checkEvtReb.flag ){
                if( !anaMode && alreadyDead[aIdx] ) next

                evtComp <- bFCust.evtComp( evt.sm["lev",] ,rObj$lastEvt["lev",] )
                if( 1<sum(!is.na(evtComp$levDup)) ){    # evtComp$allMat으로 해야 하려나?
                    alreadyDead[aIdx] <- TRUE

                    infoStr <- sprintf("rebE(last:%s)",checkEvtReb.str)
                    cObj <- cutLst.reb[[as.character(aIdx)]]
                    if( is.null(cObj) ){
                        cutLst.reb[[as.character(aIdx)]] <- list( idx=aIdx ,info=infoStr )
                    } else {
                        cutLst.reb[[as.character(aIdx)]]$info <- paste( cObj$info, infoStr, collapse=", " )
                    }
                }
            }

            # evt Reb Dup
            if( checkEvtReb.Dbl.flag && (sum(evt.sm["lev",]>0,na.rm=T)>0) ){
                if( !anaMode && alreadyDead[aIdx] ) next

                levDupName <- names(checkEvtReb.Dbl.levDup)
                evtComp <- bFCust.evtComp( evt.sm["lev",levDupName] ,rObj$lastEvt["lev",levDupName] )
                names(evtComp$levDup) <- levDupName # 값이 1개일때는 컬럼명이 따라오지 않아서..

                # cfg$rowReb.dup <- c( lowE=1 ,rareE=1 )

                eRebDup.cnt <- c( lowE=sum(evtComp$levDup>0,na.rm=T) ,rareE=sum(evtComp$levDup>1,na.rm=T) )
                eRebDup.flag <- any( eRebDup.cnt >= cfg$rowRebDup[c("lowE","rareE")] )
                if( eRebDup.flag ){ 
                    alreadyDead[aIdx] <- TRUE

                    levDup <- evtComp$levDup[!is.na(evtComp$levDup)]
                    str <- paste( paste( levDupName[!is.na(evtComp$levDup)] ,levDup ,sep=":" ) ,collapse=", " )
                    infoStr <- sprintf("rebE.dup(last:%s)",str)
                    cObj <- cutLst.reb[[as.character(aIdx)]]
                    if( is.null(cObj) ){
                        cutLst.reb[[as.character(aIdx)]] <- list( idx=aIdx ,info=infoStr )
                    } else {
                        cutLst.reb[[as.character(aIdx)]]$info <- paste( cObj$info, infoStr, collapse=", " )
                    }

                }

            }

        }

        if( anaMode ){  # build cutLst 
            idxFCol <- if( length(cutLst.fCol)==0 ) integer(0) else sapply( cutLst.fCol  ,function(p){p$idx} )
            idxReb  <- if( length(cutLst.reb)==0 ) integer(0) else sapply( cutLst.reb   ,function(p){p$idx} )
            idxRowE <- if( length(cutLst.rowE)==0 ) integer(0) else sapply( cutLst.rowE  ,function(p){p$idx} )

            idxAll <- union(idxFCol,idxReb)
            idxAll <- sort(union(idxAll,idxRowE))

            cutLst <- list()
            names(cutLst.fCol)  <- idxFCol
            names(cutLst.reb)   <- idxReb
            names(idxRowE)      <- idxRowE

            for( aIdx in idxAll ){
                idStr <- as.character(aIdx)

                cLst <- list()
                if( !is.null(cutLst.fCol[[idStr]]) ){
                    cLst[["rawFCol"]] <- cutLst.fCol[[idStr]]
                    cLst[["rawFCol"]]$idObjDesc <- c( typ="rawFCol" ,rObj$defId )
                }
                if( !is.null(cutLst.reb[[idStr]]) ){
                    cLst[["rawReb"]] <- cutLst.reb[[idStr]]
                    cLst[["rawReb"]]$idObjDesc <- c( typ="rawReb" ,rObj$defId )
                }
                if( !is.null(cutLst.rowE[[idStr]]) ){
                    cLst[["rowE"]] <- cutLst.rowE[[idStr]]
                    cLst[["rowE"]]$idObjDesc <- c( typ="rowE" ,rObj$defId )
                }

                cutLst[[idStr]] <- list( idx=aIdx ,cLst=cLst )
            }
        }

        return( list(cutLst=cutLst,surFlag=!alreadyDead) )

    }

    return( rObj )

}


bS_stdCutExt.rawRow <- function( hName ,mName ,pName ,scoreMtxH ,fltName ){  

    rObj <- list( defId=c(hName=hName,mName=mName,pName=pName,fltName=fltName) )

    extFilter <- bSMtxExtFltLst[[mName]][[fltName]]
    scrExtMtxH <- extFilter$getScoreMtx( scoreMtxH )

    hLen <- nrow(scrExtMtxH)
    rObj$lastScore <- scrExtMtxH[hLen,]
    rObj$available <- TRUE

    if( rObj$available ){

        cfg <- bsScrExtMtxCfg[[mName]][[fltName]]
        if( !is.null(cfg) ){
            rObj$isHard <- if( is.null(cfg$isHard) ) bFCust.defaultHardFlag else cfg$isHard

            rObj$lastEvt <- bFCust.getEvt( rObj$lastScore ,cfg$fCol )
            if( hLen>1 ){
                evt2 <- bFCust.getEvt( scrExtMtxH[hLen-1 ,] ,cfg$fCol )
                rObj$evtReb <- bFCust.evtComp( evt2["lev",] ,rObj$lastEvt["lev",] ) # ,levMin=2
                if( all(is.na(rObj$evtReb$levDup)) )    rObj$evtReb <- NULL
            }

        } else {
            rObj$available <- FALSE
        }   # cfg

    }


    rObj$cut <- function( scoreMtx ,alreadyDead=NULL ,anaMode=TRUE ){

        val.len <- nrow(scoreMtx)
        if( is.null(alreadyDead) )	alreadyDead <- rep( F, val.len )

        cutLst <- list()
        if( !rObj$available ) return( list(cutLst=cutLst,surFlag=!alreadyDead) )

        # hardFlag <- rObj$isHard( rObj$defId["pName"] )$flag["p"]
        hardFlag <- FALSE

        extFilter <- bSMtxExtFltLst[[ rObj$defId["mName"] ]][[ rObj$defId["fltName"] ]]
        scrExtMtx <- extFilter$getScoreMtx( scoreMtx )
        cfg <- bsScrExtMtxCfg[[ rObj$defId["mName"] ]][[ rObj$defId["fltName"] ]]

        # each fCol --------------------------------------------
        cutLst.fCol <- list()
        for( fcName in names(cfg$fCol) ){
            for( aIdx in seq_len(val.len) ){
                if( !anaMode && alreadyDead[aIdx] ) next

                surWin <- cfg$fCol[[fcName]]$rng[ ,ifelse(hardFlag,"lev1","lev2")]
                val <- scrExtMtx[aIdx,fcName]
                if( !bUtil.in(val,surWin) ){
                    alreadyDead[aIdx] <- TRUE

                    infoStr <- sprintf("%s(%d)",fcName,val )
                    cObj <- cutLst.fCol[[as.character(aIdx)]]
                    if( is.null(cObj) ){
                        cutLst.fCol[[as.character(aIdx)]] <- list( idx=aIdx ,info=infoStr )
                    } else {
                        cutLst.fCol[[as.character(aIdx)]]$info <- paste( cObj$info, infoStr, collapse=", " )
                    }

                    # infoStr <- sprintf("val:%d",val )
                    # idObjDesc <- c( typ="rawFCol" ,rObj$defId ,fcName=fcName )
                    # cutLst[[1+length(cutLst)]] <- list( idx=aIdx ,idObjDesc=idObjDesc ,info=infoStr )
                }

            }

        }

        # sm row: evtCnt  --------------------------------------------
        cutLst.rowE <- list()
        for( aIdx in seq_len(val.len) ){
            if( !anaMode && alreadyDead[aIdx] ) next

            evt.sm <- bFCust.getEvt( scrExtMtx[aIdx ,] ,cfg$fCol )
            evtMin <- cfg$evtMax[ifelse(hardFlag,"lev1","lev2"),]
            evtCnt <- sum( evt.sm["lev" ,]>=evtMin["minLev"] ,na.rm=T )
            evtCntH <- sum( evt.sm["lev" ,]>=evtMin["minLevH"] ,na.rm=T )
            if( evtCnt>evtMin["maxHpn"] || evtCntH>evtMin["maxHpnH"] ){
                alreadyDead[aIdx] <- TRUE

                infoStr=""
                if( anaMode ){
                    flag <- evt.sm["lev",]>=evtMin["minLev"]
                    flag[is.na(flag)] <- F

                    infoStr <- sprintf("evtCnt:%d(%s)",evtCnt,paste(evt.sm["lev" ,flag],collapse=","))
                }
                cutLst.rowE[[as.character(aIdx)]] <- list( idx=aIdx ,info=infoStr )
            }
        }

        # sm row: rebound --------------------------------------------
        cutLst.reb <- list()
        checkRawReb.cnt <- sum(rObj$lastScore>0)
        checkRawReb.flag <- checkRawReb.cnt >= cfg$rowReb["rawMin"]
        checkEvtReb.flag <- FALSE
        if( 0<sum(rObj$lastEvt["lev"],na.rm=T) ){
            checkEvtReb.cnt <- c( lowE=sum(rObj$lastEvt["lev",]>0,na.rm=T) ,rareE=sum(rObj$lastEvt["lev",]>1,na.rm=T) )
            checkEvtReb.flag <- any( checkEvtReb.cnt >= cfg$rowReb[c("lowE","rareE")] )
            checkEvtReb.str <- paste(names(checkEvtReb.cnt),checkEvtReb.cnt,sep=".")
            checkEvtReb.str <- paste( checkEvtReb.str ,collapse="," )
        }
        checkEvtReb.Dbl.flag <- !is.null(rObj$evtReb)
        if( checkEvtReb.Dbl.flag ){
            levDup <- rObj$evtReb$levDup[!is.na(rObj$evtReb$levDup)]
            # str <- paste( names(levDup) ,levDup ,sep=":")
            # checkEvtReb.Dbl.str <- paste( str ,collapse=", ")
            checkEvtReb.Dbl.levDup <- levDup
        }
        for( aIdx in seq_len(val.len) ){
            smRow <- scrExtMtx[aIdx ,]
            evt.sm <- bFCust.getEvt(smRow,cfg$fCol)

            # raw Reb
            if( checkRawReb.flag ){
                if( !anaMode && alreadyDead[aIdx] ) next

                if( all(rObj$lastScore==smRow) ){
                    alreadyDead[aIdx] <- TRUE

                    infoStr <- sprintf("rReb(last:%d)",checkRawReb.cnt )
                    cutLst.reb[[as.character(aIdx)]] <- list( idx=aIdx ,info=infoStr )
                }
            }

            # evt Reb
            if( checkEvtReb.flag ){
                if( !anaMode && alreadyDead[aIdx] ) next

                evtComp <- bFCust.evtComp( evt.sm["lev",] ,rObj$lastEvt["lev",] )
                if( 1<sum(!is.na(evtComp$levDup)) ){    # evtComp$allMat으로 해야 하려나?
                    alreadyDead[aIdx] <- TRUE

                    infoStr <- sprintf("rebE(last:%s)",checkEvtReb.str)
                    cObj <- cutLst.reb[[as.character(aIdx)]]
                    if( is.null(cObj) ){
                        cutLst.reb[[as.character(aIdx)]] <- list( idx=aIdx ,info=infoStr )
                    } else {
                        cutLst.reb[[as.character(aIdx)]]$info <- paste( cObj$info, infoStr, collapse=", " )
                    }
                }
            }

            # evt Reb Dup
            if( checkEvtReb.Dbl.flag && (sum(evt.sm["lev",]>0,na.rm=T)>0) ){
                if( !anaMode && alreadyDead[aIdx] ) next

                levDupName <- names(checkEvtReb.Dbl.levDup)
                evtComp <- bFCust.evtComp( evt.sm["lev",levDupName] ,rObj$lastEvt["lev",levDupName] )
                names(evtComp$levDup) <- levDupName # 값이 1개일때는 컬럼명이 따라오지 않아서..

                # cfg$rowReb.dup <- c( lowE=1 ,rareE=1 )

                eRebDup.cnt <- c( lowE=sum(evtComp$levDup>0,na.rm=T) ,rareE=sum(evtComp$levDup>1,na.rm=T) )
                eRebDup.flag <- any( eRebDup.cnt >= cfg$rowRebDup[c("lowE","rareE")] )
                if( eRebDup.flag ){ 
                    alreadyDead[aIdx] <- TRUE

                    levDup <- evtComp$levDup[!is.na(evtComp$levDup)]
                    str <- paste( paste( levDupName[!is.na(evtComp$levDup)] ,levDup ,sep=":" ) ,collapse=", " )
                    infoStr <- sprintf("rebE.dup(last:%s)",str)
                    cObj <- cutLst.reb[[as.character(aIdx)]]
                    if( is.null(cObj) ){
                        cutLst.reb[[as.character(aIdx)]] <- list( idx=aIdx ,info=infoStr )
                    } else {
                        cutLst.reb[[as.character(aIdx)]]$info <- paste( cObj$info, infoStr, collapse=", " )
                    }

                }

            }

        }

        if( anaMode ){  # build cutLst 
            idxFCol <- if( length(cutLst.fCol)==0 ) integer(0) else sapply( cutLst.fCol  ,function(p){p$idx} )
            idxReb  <- if( length(cutLst.reb)==0 ) integer(0) else sapply( cutLst.reb   ,function(p){p$idx} )
            idxRowE <- if( length(cutLst.rowE)==0 ) integer(0) else sapply( cutLst.rowE  ,function(p){p$idx} )

            idxAll <- union(idxFCol,idxReb)
            idxAll <- sort(union(idxAll,idxRowE))

            cutLst <- list()
            names(cutLst.fCol)  <- idxFCol
            names(cutLst.reb)   <- idxReb
            names(idxRowE)      <- idxRowE

            for( aIdx in idxAll ){
                idStr <- as.character(aIdx)

                cLst <- list()
                if( !is.null(cutLst.fCol[[idStr]]) ){
                    cLst[["rawFCol"]] <- cutLst.fCol[[idStr]]
                    cLst[["rawFCol"]]$idObjDesc <- c( typ="rawFCol" ,rObj$defId )
                }
                if( !is.null(cutLst.reb[[idStr]]) ){
                    cLst[["rawReb"]] <- cutLst.reb[[idStr]]
                    cLst[["rawReb"]]$idObjDesc <- c( typ="rawReb" ,rObj$defId )
                }
                if( !is.null(cutLst.rowE[[idStr]]) ){
                    cLst[["rowE"]] <- cutLst.rowE[[idStr]]
                    cLst[["rowE"]]$idObjDesc <- c( typ="rowE" ,rObj$defId )
                }

                cutLst[[idStr]] <- list( idx=aIdx ,cLst=cLst )
            }
        }

        return( list(cutLst=cutLst,surFlag=!alreadyDead) )

    }

    return( rObj )

}



bS_stdCut.hIdx <- function( hName ,mName ,mtxLst ){


    rObj <- list( defId=c(hName=hName,mName=mName) )

    hLen <- length(mtxLst)
    rObj$lastMtx <- mtxLst[[hLen]]
    rObj$available <- TRUE

    if( rObj$available ){
        cfg <- bsScoreMtxCfg[[mName]]
        if( !is.null(cfg) ){

            stdEvt.H2 <- NULL
            if( 1<length(mtxLst) ){
            	stdEvt.H2 <- bFCust.getEvt_byHIdx( mtxLst[[length(mtxLst)-1]] ,cfg ,NULL )
            }
            rObj$stdEvt.H1 <- bFCust.getEvt_byHIdx( mtxLst[[length(mtxLst)]] ,cfg ,lastEvt=stdEvt.H2 )

            szObj.H2 <- NULL
            if( 1<length(mtxLst) ){
                mtxLst.H2 <- mtxLst[1:(length(mtxLst)-1)]
                szObj.H2 <- bFCust.getSkipZero_byHIdx( mtxLst.H2 ,cfg )
            }
            rObj$szObj <- bFCust.getSkipZero_byHIdx( mtxLst ,cfg ,lastSZ=szObj.H2 )

        } else {
            rObj$available <- FALSE
        }   # cfg

    }


    rObj$getRawScore <- function( rawMtx ){ 

        if( !rObj$available ) return( NULL )

        cfg <- bsScoreMtxCfg[[ rObj$defId["mName"] ]]
        evtObj <- bFCust.getEvtMtx( rawMtx ,cfg )

        curEvt <- bFCust.getEvt_byHIdx( rawMtx ,cfg ,lastEvt=rObj$stdEvt.H1 )
        fColEvt <- bFCust.getEvt_byFCol( evtObj ,cfg )
        rebInfo <- bFCust.getSkipZero_byHIdx.ass( rObj$szObj ,rawMtx ,evtObj$eValMtx )

        return( list( cfg=cfg ,evtObj=evtObj ,curEvt=curEvt ,fColEvt=fColEvt ,rebInfo=rebInfo ) )

    }

    rObj$getRaw4Ass <- function( rawObj ){
        #   mName 단위가 아닌, 전체 mName 범위로 평가하기 위한 데이터 추출.
        #   bUtil.getCut1Score( ) 함수 참고.

        r4Ass <- list()

        # phaseHpnCnt는 현재 aZoid가 아닌, 이전 lastZoid에 관한 값임
        phaseHpnCnt <- rbind( rObj$stdEvt.H1$hpnInfo$phase ,rObj$stdEvt.H1$evtInfo$phase )
        rownames( phaseHpnCnt ) <- c("raw","evt")

        # phaseHpnCnt는 현재 aZoid가 아닌, 이전 lastZoid에 관한 값임
        phaseRebCnt <- rbind( rObj$stdEvt.H1$hpnInfo$phaseReb["reb",] ,rObj$stdEvt.H1$evtInfo$phaseReb["reb",] )
        rownames( phaseRebCnt ) <- c("raw","evt")

        # r4Ass$H1.phHpnCnt <- phaseHpnCnt    # 혼동가능성 때문에 이름 변경.
        # r4Ass$H1.phRebCnt <- phaseRebCnt    #       초기 이름은 phaseHpnCnt, phaseRebCnt 이었음.(검토 후 폐기.)

        r4Ass$rebMtx.ph <- rawObj$curEvt$rebInfo$rebMtx.ph

        evtHpnLevMtx <- NULL
        cName <- c("lev1","lev2","lev3")
        eLevMtx <- rawObj$evtObj$eLevMtx
        evtHpnLevMtx <- matrix( 0 ,nrow=length(cName) ,ncol=ncol(eLevMtx) ,dimnames=list(cName,colnames(eLevMtx)) )
        evtHpnLevMtx["lev1" ,] <- apply( eLevMtx ,2 ,function(cDat){sum(cDat==1,na.rm=T)} )
        evtHpnLevMtx["lev2" ,] <- apply( eLevMtx ,2 ,function(cDat){sum(cDat==2,na.rm=T)} )
        evtHpnLevMtx["lev3" ,] <- apply( eLevMtx ,2 ,function(cDat){sum(cDat>=3,na.rm=T)} )
        r4Ass$evtHpnLevMtx <- evtHpnLevMtx

        phaseReb.raw <- rawObj$curEvt$hpnInfo$phaseReb[c("reb","hpn"),] ;rownames(phaseReb.raw) <- c("rebFlag.raw","hpn.raw")
        phaseReb.evt <- rawObj$curEvt$evtInfo$phaseReb[c("reb","hpn"),] ;rownames(phaseReb.evt) <- c("rebFlag.evt","hpn.evt")
        r4Ass$phaseReb  <- rbind( phaseReb.raw ,phaseReb.evt )

        return( r4Ass )

    }

    rObj$getSummScore <- function( rawObj ){
        scoreObj <- list( )

        # cfg <- scoreMtxCfg[[ rObj$defId["mName"] ]]
        # evtObj <- bFCust.getEvtMtx( rawMtx ,cfg )
        # rawObj <- rObj$getRawScore( rawMtx )
        if( is.null(rawObj) ){
            rName <- c("raw","evt")
			cName <- c("all","ph","fCol","phReb","xyCnt.fCol","xyCnt.phase")
			summMtx <- matrix( 0 ,nrow=length(rName) ,ncol=length(cName) )
			rownames(summMtx) <- rName	;colnames(summMtx) <- cName

            scoreObj$summMtx        <- summMtx
            scoreObj$summMtx.reb    <- summMtx # 내부 구조는 같다.

            cName <- c("r.ph","r.fCol","r.dblHpnFlg" ,"e.ph","e.fCol","e.dblHpnFlg")
            rName <- c("rebCnt","rebDup")   # 반복 수, H1에서의 재현이 반복되었는지? ,발생 수
            scMtx.sz <- matrix( 0 ,ncol=length(cName) ,nrow=length(rName) ,dimnames=list(rName,cName) )
            scoreObj$scMtx.sz
            return( scoreObj )
        }
        cfg     <- rawObj$cfg
        evtObj  <- rawObj$evtObj
        curEvt  <- rawObj$curEvt
        fColEvt <- rawObj$fColEvt
        rebInfo <- rawObj$rebInfo   # bFCust.getSkipZero_byHIdx.ass() 값임을 유의

        # fColEvt 평가 적용.
        #   cfg의 evtMax.fCol이 이미 적용되었음(close max 값이므로)
        scoreObj$fColEvt <- fColEvt

        #   summMtx,summMtx.reb / stdEvt.H1 --------------------------------------------------------
        # curEvt <- bFCust.getEvt_byHIdx( rawMtx ,cfg ,lastEvt=rObj$stdEvt.H1 )
        scoreObj$summMtx <- curEvt$rebInfo$summMtx
        scoreObj$summMtx.reb <- NULL
        if( !is.null(rObj$stdEvt.H1$rebInfo) ){
            summMtx.reb <- scoreObj$summMtx
            summMtx.reb[,] <- 0     # 하나라도 0 이 아니면 Cut...

            rebInfo.H1 <- rObj$stdEvt.H1$rebInfo

            summMtx.reb["raw","all"] <- scoreObj$summMtx["raw","all"]>0 && curEvt$rebInfo$summMtx["raw","all"]>0
            summMtx.reb["evt","all"] <- scoreObj$summMtx["evt","all"]>0 && curEvt$rebInfo$summMtx["evt","all"]>0

            summMtx.reb["raw","ph"] <- sum( rebInfo.H1$rebMtx.ph["rebFlag.raw",]>0 & curEvt$rebInfo$rebMtx.ph["rebFlag.raw",]>0 )
            summMtx.reb["evt","ph"] <- sum( rebInfo.H1$rebMtx.ph["rebFlag.evt",]>0 & curEvt$rebInfo$rebMtx.ph["rebFlag.evt",]>0 )

            summMtx.reb["raw","fCol"] <- sum( rebInfo.H1$rebMtx.fCol["rebFlag.raw",]>0 & curEvt$rebInfo$rebMtx.fCol["rebFlag.raw",]>0 )
            summMtx.reb["evt","fCol"] <- sum( rebInfo.H1$rebMtx.fCol["rebFlag.evt",]>0 & curEvt$rebInfo$rebMtx.fCol["rebFlag.evt",]>0 )

            summMtx.reb["raw","phReb"] <- sum( rebInfo.H1$rebMtx.phReb["raw",]>0 & curEvt$rebInfo$rebMtx.phReb["raw",]>0 )
            summMtx.reb["evt","phReb"] <- sum( rebInfo.H1$rebMtx.phReb["evt",]>0 & curEvt$rebInfo$rebMtx.phReb["evt",]>0 )

            summMtx.reb["raw","xyCnt.fCol"] <- rebInfo.H1$rebMtx.xyCnt["raw","fCol.allMat"]>0 && curEvt$rebInfo$rebMtx.xyCnt["raw","fCol.allMat"]
            summMtx.reb["evt","xyCnt.fCol"] <- rebInfo.H1$rebMtx.xyCnt["evt","fCol.allMat"]>0 && curEvt$rebInfo$rebMtx.xyCnt["evt","fCol.allMat"]

            summMtx.reb["raw","xyCnt.phase"] <- rebInfo.H1$rebMtx.xyCnt["raw","phase.allMat"]>0 && curEvt$rebInfo$rebMtx.xyCnt["raw","phase.allMat"]
            summMtx.reb["evt","xyCnt.phase"] <- rebInfo.H1$rebMtx.xyCnt["evt","phase.allMat"]>0 && curEvt$rebInfo$rebMtx.xyCnt["evt","phase.allMat"]


            scoreObj$summMtx.reb <- summMtx.reb
        }

        #   scMtx.sz / szObj ------------------------------------------------------------
        # rebInfo <- bFCust.getSkipZero_byHIdx.ass( rObj$szObj ,rawMtx ,evtObj$eValMtx )
        cName <- c("r.ph","r.fCol","r.dblHpnFlg" ,"e.ph","e.fCol","e.dblHpnFlg")
        rName <- c("rebCnt","rebDup")   # 반복 수, H1에서의 재현이 반복되었는지? ,발생 수
        scMtx.sz <- matrix( 0 ,ncol=length(cName) ,nrow=length(rName) ,dimnames=list(rName,cName) )
        if( TRUE ){
            scMtx.sz["rebCnt","r.ph"] <- sum(rebInfo$matRaw$ph["mat",])
            scMtx.sz["rebCnt","r.fCol"] <- sum(rebInfo$matRaw$fCol["mat",])
            scMtx.sz["rebCnt","r.dblHpnFlg"] <- rebInfo$matRaw$dblHpn["mat"]
            scMtx.sz["rebCnt","e.ph"] <- sum(rebInfo$matEvt$ph["mat",])
            scMtx.sz["rebCnt","e.fCol"] <- sum(rebInfo$matEvt$fCol["mat",])
            scMtx.sz["rebCnt","e.dblHpnFlg"] <- rebInfo$matEvt$dblHpn["mat"]

            if( !is.null(rObj$szObj$rebInfo) ){
                matFlag <- (rObj$szObj$rebInfo$matRaw$ph["mat",]>0) & (rebInfo$matRaw$ph["mat",]>0)
                scMtx.sz["rebDup","r.ph"] <- sum( matFlag )
                matFlag <- (rObj$szObj$rebInfo$matRaw$fCol["mat",]>0) & (rebInfo$matRaw$fCol["mat",]>0)
                scMtx.sz["rebDup","r.fCol"] <- sum( matFlag )
                scMtx.sz["rebDup","r.dblHpnFlg"] <- (rObj$szObj$rebInfo$matRaw$dblHpn["mat"]>0) && (rebInfo$matRaw$dblHpn["mat"]>0)

                matFlag <- (rObj$szObj$rebInfo$matEvt$ph["mat",]>0) & (rebInfo$matEvt$ph["mat",]>0)
                scMtx.sz["rebDup","e.ph"] <- sum( matFlag )
                matFlag <- (rObj$szObj$rebInfo$matEvt$fCol["mat",]>0) & (rebInfo$matEvt$fCol["mat",]>0)
                scMtx.sz["rebDup","e.fCol"] <- sum( matFlag )
                scMtx.sz["rebDup","e.dblHpnFlg"] <- (rObj$szObj$rebInfo$matEvt$dblHpn["mat"]>0) && (rebInfo$matEvt$dblHpn["mat"]>0)
            }            
        }
        scoreObj$scMtx.sz <- scMtx.sz

        return( scoreObj )

    }

    rObj$cut <- function( scoreMtx ,anaMode=TRUE ){   # 하나씩 오므로, alreadyDead 처리.

        cLst <- list( )     #   cutLst[[1]]$cLst
                            #   만약 생존했으면 cLst의 길이는 0
        if( !rObj$available ) return( cLst )

        rawObj <- rObj$getRawScore( scoreMtx )
        scObj <- rObj$getSummScore( rawObj )
        cfg <- bsScoreMtxCfg[[ rObj$defId["mName"] ]]

        survive <- TRUE
        # if( survive ){  # fCol Evt Cnt 
        if( TRUE ){  # fCol Evt Cnt 
            closeMaxDistVal <- scObj$fColEvt$closeMaxDistVal
            fEvtMtx <- scObj$fColEvt$fEvtMtx
            fClMMtx <- scObj$fColEvt$fClMMtx

            # fCol 별로 전체 ph에서의 evt 발생 수 한계 제약.
            if( any(fClMMtx[,"lev1ClM"]>=closeMaxDistVal) ){
                survive <- F
                cLst[["fCol EvtCnt4AllPh(lev1ClM)"]] <- "fCol EvtCnt4AllPh(lev1ClM)"
                if( anaMode ){
                    flag <- fClMMtx[,"lev1ClM"]>=closeMaxDistVal
                    str <- paste( names( fClMMtx[,"lev1ClM"] )[flag] ,fEvtMtx[flag,"lev1Cnt"] ,sep=":" )
                    infoStr <- sprintf("fCol EvtCnt4AllPh(lev1ClM) %s",paste(str,collapse=", "))
                    cLst[["fCol EvtCnt4AllPh(lev1ClM)"]] <- infoStr
                }
            }
            if( any(fClMMtx[,"lev2ClM"]>=closeMaxDistVal) ){
                survive <- F
                cLst[["fCol EvtCnt4AllPh(lev2ClM)"]] <- "fCol EvtCnt4AllPh(lev2ClM)"
                if( anaMode ){
                    flag <- fClMMtx[,"lev2ClM"]>=closeMaxDistVal
                    str <- paste( names( fClMMtx[,"lev2ClM"] )[flag] ,fEvtMtx[flag,"lev2Cnt"] ,sep=":" )
                    infoStr <- sprintf("fCol EvtCnt4AllPh(lev2ClM) %s",paste(str,collapse=", "))
                    cLst[["fCol EvtCnt4AllPh(lev2ClM)"]] <- infoStr
                }
            }
            if( any(fClMMtx[,"lev3ClM"]>=closeMaxDistVal) ){
                survive <- F
                cLst[["fCol EvtCnt4AllPh(lev3ClM)"]] <- "fCol EvtCnt4AllPh(lev3ClM)"
                if( anaMode ){
                    flag <- fClMMtx[,"lev3ClM"]>=closeMaxDistVal
                    str <- paste( names( fClMMtx[,"lev3ClM"] )[flag] ,fEvtMtx[flag,"lev3Cnt"] ,sep=":" )
                    infoStr <- sprintf("fCol EvtCnt4AllPh(lev3ClM) %s",paste(str,collapse=", "))
                    cLst[["fCol EvtCnt4AllPh(lev3ClM)"]] <- infoStr
                }
            }

            # 이제 오버매치가 발생한 fCol 들의 갯수를 제한하자.
            #   evtMaxFColTot  <- c( lev1Max=1 ,lev2Max=1 ,lev3Max=1 )
            botThld <- closeMaxDistVal - closeMaxDistVal    # 0
            eSumLev1 <- sum(scObj$fColEvt$fClMMtx[,"lev1ClM"] > botThld )
            if( eSumLev1>=cfg$evtMaxFColTot["lev1Max"] ){
                survive <- F
                cLst[["fCol evtMaxFColTot(lev1ClM)"]] <- "fCol evtMaxFColTot(lev1ClM)"
                if( anaMode ){
                    flag <- scObj$fColEvt$fClMMtx[,"lev1ClM"] > botThld
                    str <- paste( names( scObj$fColEvt$fClMMtx[,"lev1ClM"] )[flag] ,scObj$fColEvt$fEvtMtx[flag,"lev1Cnt"] ,sep=":" )
                    infoStr <- sprintf("fCol evtMaxFColTot(lev1ClM) %s",paste(str,collapse=", "))
                    cLst[["fCol evtMaxFColTot(lev1ClM)"]] <- infoStr
                }
            }
            botThld <- closeMaxDistVal - 2
            eSumLev2 <- sum(scObj$fColEvt$fClMMtx[,"lev2ClM"] > botThld )
            if( eSumLev2>=cfg$evtMaxFColTot["lev2Max"] ){
                survive <- F
                cLst[["fCol evtMaxFColTot(lev2ClM)"]] <- "fCol evtMaxFColTot(lev2ClM)"
                if( anaMode ){
                    flag <- scObj$fColEvt$fClMMtx[,"lev2ClM"] > botThld
                    str <- paste( names( scObj$fColEvt$fClMMtx[,"lev2ClM"] )[flag] ,scObj$fColEvt$fEvtMtx[flag,"lev2Cnt"] ,sep=":" )
                    infoStr <- sprintf("fCol evtMaxFColTot(lev2ClM) %s",paste(str,collapse=", "))
                    cLst[["fCol evtMaxFColTot(lev2ClM)"]] <- infoStr
                }
            }
            botThld <- closeMaxDistVal - 2
            eSumLev3 <- sum(scObj$fColEvt$fClMMtx[,"lev3ClM"] > botThld )
            if( eSumLev3>=cfg$evtMaxFColTot["lev3Max"] ){
                survive <- F
                cLst[["fCol evtMaxFColTot(lev3ClM)"]] <- "fCol evtMaxFColTot(lev3ClM)"
                if( anaMode ){
                    flag <- scObj$fColEvt$fClMMtx[,"lev3ClM"] > botThld
                    str <- paste( names( scObj$fColEvt$fClMMtx[,"lev3ClM"] )[flag] ,scObj$fColEvt$fEvtMtx[flag,"lev3Cnt"] ,sep=":" )
                    infoStr <- sprintf("fCol evtMaxFColTot(lev3ClM) %s",paste(str,collapse=", "))
                    cLst[["fCol evtMaxFColTot(lev3ClM)"]] <- infoStr
                }
            }

        }

        if( survive ){ #   summMtx.cut
            infoStr <- ""
            #     $summMtx    all ph fCol phReb xyCnt.fCol xyCnt.phase
            #             raw   0  0    0     0          0           0
            #             evt   0  0    0     0          0           0
            summMtx.cut <- scObj$summMtx >= cfg$summMtx
            if( any(summMtx.cut["raw",]) ){
                survive <- F
                cLst[["summMtx.cut raw"]] <- "summMtx.cut raw"
                if( anaMode ){  # infoStr
                    flag <- summMtx.cut["raw",]
                    str <- paste( names(scObj$summMtx["raw",])[flag] ,scObj$summMtx["raw",flag] ,sep=":" )
                    infoStr <- sprintf("summMtx.cut - raw %s",paste(str,collapse=", ") )
                    cLst[["summMtx.cut raw"]] <- infoStr
                }
            }
            if( any(summMtx.cut["evt",]) ){
                survive <- F
                cLst[["summMtx.cut evt"]] <- "summMtx.cut evt"
                if( anaMode ){  # infoStr
                    flag <- summMtx.cut["evt",]
                    str <- paste( names(scObj$summMtx["evt",])[flag] ,scObj$summMtx["evt",flag] ,sep=":" )
                    infoStr <- sprintf("summMtx.cut - evt %s",paste(str,collapse=", ") )
                    cLst[["summMtx.cut evt"]] <- infoStr
                }
            }
            if( cfg$summMtx.sum["raw"]<=sum(scObj$summMtx["raw",]) ){
                survive <- F
                cLst[["summMtx.cut raw.sum"]] <- "summMtx.cut raw.sum"
                if( anaMode ){  # infoStr
                    flag <- scObj$summMtx["raw",]>0
                    str <- paste( names(scObj$summMtx["raw",])[flag] ,scObj$summMtx["raw",flag] ,sep=":" )
                    infoStr <- sprintf("summMtx.cut - raw.sum %s",paste(str,collapse=", ") )
                    cLst[["summMtx.cut raw.sum"]] <- infoStr
                }
            }
            if( cfg$summMtx.sum["evt"]<=sum(scObj$summMtx["evt",]) ){
                survive <- F
                cLst[["summMtx.cut evt.sum"]] <- "summMtx.cut evt.sum"
                if( anaMode ){  # infoStr
                    flag <- scObj$summMtx["evt",]>0
                    str <- paste( names(scObj$summMtx["evt",])[flag] ,scObj$summMtx["evt",flag] ,sep=":" )
                    infoStr <- sprintf("summMtx.cut - evt.sum %s",paste(str,collapse=", ") )
                    cLst[["summMtx.cut evt.sum"]] <- infoStr
                }
            }
        }

        if( survive ){ #   summMtx.reb.cut
            #     $summMtx.reb  all ph fCol phReb xyCnt.fCol xyCnt.phase
            #               raw   0  0    0     0          0           0
            #               evt   0  0    0     0          0           0
            summMtx.reb.cut <- scObj$summMtx.reb >= cfg$summMtx.reb
            if( any(summMtx.reb.cut["raw",]) ){
                survive <- F
                cLst[["summMtx.reb.cut raw"]] <- "summMtx.reb.cut raw"
                if( anaMode ){  # infoStr
                    flag <- summMtx.reb.cut["raw",]
                    str <- paste( names(scObj$summMtx.reb["raw",])[flag] ,scObj$summMtx.reb["raw",flag] ,sep=":" )
                    infoStr <- sprintf("summMtx.reb.cut - raw %s",paste(str,collapse=", ") )
                    cLst[["summMtx.reb.cut raw"]] <- infoStr
                }
            }
            if( any(summMtx.reb.cut["evt",]) ){
                survive <- F
                cLst[["summMtx.reb.cut evt"]] <- "summMtx.reb.cut evt"
                if( anaMode ){  # infoStr
                    flag <- summMtx.reb.cut["evt",]
                    str <- paste( names(scObj$summMtx.reb["evt",])[flag] ,scObj$summMtx.reb["evt",flag] ,sep=":" )
                    infoStr <- sprintf("summMtx.reb.cut - evt %s",paste(str,collapse=", ") )
                    cLst[["summMtx.reb.cut evt"]] <- infoStr
                }
            }
            # sum 체크를 해야 할 일은 없을 듯 하다.
        }

        if( survive ){ #   scMtx.sz.cut
            #     $scMtx.sz   r.ph r.fCol r.dblHpnFlg e.ph e.fCol e.dblHpnFlg
            #             rebCnt    0      0           0    0      0           0
            #             rebDup    0      0           0    0      0           0
            scMtx.sz.cut <- scObj$scMtx.sz >= cfg$scMtx.sz
            if( any(scMtx.sz.cut["rebCnt",]) ){ #   rebCnt
                survive <- F
                cLst[["scMtx.sz.cut rebCnt"]] <- "scMtx.sz.cut rebCnt"
                if( anaMode ){  # infoStr
                    flag <- scMtx.sz.cut["rebCnt",]
                    str <- paste( names(scObj$scMtx.sz["rebCnt",])[flag] ,scObj$scMtx.sz["rebCnt",flag] ,sep=":" )
                    infoStr <- sprintf("scMtx.sz.cut - rebCnt %s",paste(str,collapse=", ") )
                    cLst[["scMtx.sz.cut rebCnt"]] <- infoStr
                }
            }
            if( any(scMtx.sz.cut["rebDup",]) ){ #   rebDup
                survive <- F
                cLst[["scMtx.sz.cut rebDup"]] <- "scMtx.sz.cut rebDup"
                if( anaMode ){  # infoStr
                    flag <- scMtx.sz.cut["rebDup",]
                    str <- paste( names(scObj$scMtx.sz["rebDup",])[flag] ,scObj$scMtx.sz["rebDup",flag] ,sep=":" )
                    infoStr <- sprintf("scMtx.sz.cut - rebDup %s",paste(str,collapse=", ") )
                    cLst[["scMtx.sz.cut rebDup"]] <- infoStr
                }
            }

            sumCol <- c("r.ph","r.fCol","r.dblHpnFlg")
            if( cfg$scMtx.sz.sum["rebCnt.r"]<=sum(scObj$scMtx.sz["rebCnt",sumCol]) ){
                survive <- F
                cLst[["scMtx.sz.cut rebCnt.r.sum"]] <- "scMtx.sz.cut rebCnt.r.sum"
                if( anaMode ){  # infoStr
                    str <- paste( sumCol ,scObj$scMtx.sz["rebCnt",sumCol] ,sep=":" )
                    infoStr <- sprintf("scMtx.sz.cut - rebCnt.r.sum %s",paste(str,collapse=", ") )
                    cLst[["scMtx.sz.cut rebCnt.r.sum"]] <- infoStr
                }
            }
            sumCol <- c("e.ph","e.fCol","e.dblHpnFlg")
            if( cfg$scMtx.sz.sum["rebCnt.e"]<=sum(scObj$scMtx.sz["rebCnt",sumCol]) ){
                survive <- F
                cLst[["scMtx.sz.cut rebCnt.e.sum"]] <- "scMtx.sz.cut rebCnt.e.sum"
                if( anaMode ){  # infoStr
                    str <- paste( sumCol ,scObj$scMtx.sz["rebCnt",sumCol] ,sep=":" )
                    infoStr <- sprintf("scMtx.sz.cut - rebCnt.e.sum %s",paste(str,collapse=", ") )
                    cLst[["scMtx.sz.cut rebCnt.e.sum"]] <- infoStr
                }
            }

            # cfg$scMtx.sz.sum["rebDup",] 의 총합을 체크해야 할 일은 없을 듯.

        }

        if( TRUE ){ # bUtil.closeMax() QQE 차후 적용 필요.
            # bUtil.closeMax_Mtx <- function( scoreMtx ,windMtxMin=0 ,windMtxMax ,distVal=3 )
            windMtxMin <- cfg$scMtx.sz
            windMtxMin[,] <- 0
            cm_scMtx.sz <- bUtil.closeMax_Mtx( scObj$scMtx.sz ,windMtxMin=NULL ,windMtxMax=cfg$scMtx.sz )
                # close max for scObj$scMtx.sz by cfg$scMtx.sz
                #   예 시 (distVal=3 일때) : 
                #         scObj$scMtx.sz
                #                        r.ph r.fCol r.dblHpnFlg e.ph e.fCol e.dblHpnFlg
                #                 rebCnt    0      1           0    0      1           0
                #                 rebDup    0      1           0    0      1           0
                #         cfg$scMtx.sz
                #                        r.ph r.fCol r.dblHpnFlg e.ph e.fCol e.dblHpnFlg
                #                 rebCnt    2      2           1    2      2           1
                #                 rebDup    1      1           1    1      1           1
                #         cm_scMtx.sz
                #                        r.ph r.fCol r.dblHpnFlg e.ph e.fCol e.dblHpnFlg
                #                 rebCnt    0      2           0    0      2           0
                #                 rebDup    0      3           0    0      3           0
        }

        return( list( cLst=cLst ,scObj=scObj ) )

    }

    return( rObj )

}



