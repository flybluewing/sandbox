lastH <- 855
# fullRstSpan, goldRstSpan ----------------------------------------------------------
fullRstSpan <- 826:lastH
loadObjNm <- load( sprintf("Obj_allIdxLstZ%d.save",lastH) )
stdFiltedCnt <- allIdxLst$stdFiltedCnt[as.character(809:lastH)]
goldRstSpan <- as.integer(names( stdFiltedCnt[stdFiltedCnt<=1] ))
stdFiltedCnt <- allIdxLst$stdFiltedCnt[as.character(819:lastH)]
grp2RstSpan <- as.integer(names( stdFiltedCnt[stdFiltedCnt==2] ))
rm(allIdxLst)

build.u0.saveStdZoidFltRst <- function( pHSpan ){
    #   pHSpan <- goldRstSpan

    for( workH in pHSpan ){
        cat(sprintf("workH %d ----------------------------------\n",workH))
        source(sprintf("./toFinal/toZ%d_H.R",workH))	# working
        u0.saveStdZoidFltRst( workH )
    }
} # build.u0.saveStdZoidFltRst

lab.getMtxLst <- function( pHSpan ,pIdStr ){

    # rptLst ----------------------------------------------------------------------------
    rptLst <- list()
    for( hIdx in pHSpan ){
        fileName <- sprintf("./save/stdZoidFltRst/z%d.save",hIdx)
        cat( paste(fileName,"\n",collapse="") )
        myObj <- load(fileName)    # rptObj
        rptLst[[sprintf("z%d_%d",hIdx,rptObj$stdFiltedCnt)]] <- rptObj
    }

    hSpan <- sapply( rptLst ,function(rpt){rpt$h})
    zhName <- attributes(rptLst)$names
    phName <- attributes(rptLst[[1]]$ccObjLst)$names
    name.cntMtx <- colnames(rptLst[[1]]$ccObjLst[[1]]$cntMtx)
    name.auxCntMtx <- colnames(rptLst[[1]]$ccObjLst[[1]]$auxCntMtx)
    name.cccObj.scoreMtx <- colnames(rptLst[[1]]$ccObjLst[[1]]$cccObj$scoreMtx)
    name.cccObj.scoreMtx2 <- colnames(rptLst[[1]]$ccObjLst[[1]]$cccObj$scoreMtx2)
    name.cccObj.scoreMtx3 <- colnames(rptLst[[1]]$ccObjLst[[1]]$cccObj$scoreMtx3)
    name.cccObj.scoreMtx4 <- colnames(rptLst[[1]]$ccObjLst[[1]]$cccObj$scoreMtx4)
    name.cccObj.cStepValMtx <- colnames(rptLst[[1]]$ccObjLst[[1]]$cccObj$cStepValMtx)

    # mtxInfoLst ------------------------------------------------------------------------
    hpnCntMtx <- matrix( 0 ,nrow=length(phName) ,ncol=length(zhName) )
    colnames(hpnCntMtx)<-zhName        ;rownames(hpnCntMtx)<-phName

    log.cntMtx      <- k.getFlogObj( "./report/cntMtx.txt" )        ;log.cntMtx$fLogStr("start", pTime=T ,pAppend=F )
    log.scoreMtx    <- k.getFlogObj( "./report/scoreMtx.txt" )      ;log.scoreMtx$fLogStr("start", pTime=T ,pAppend=F )
    log.scoreMtx2   <- k.getFlogObj( "./report/scoreMtx2.txt" )     ;log.scoreMtx2$fLogStr("start", pTime=T ,pAppend=F )
    log.scoreMtx3   <- k.getFlogObj( "./report/scoreMtx3.txt" )     ;log.scoreMtx2$fLogStr("start", pTime=T ,pAppend=F )
    log.scoreMtx4   <- k.getFlogObj( "./report/scoreMtx4.txt" )     ;log.scoreMtx2$fLogStr("start", pTime=T ,pAppend=F )
    log.cStepValMtx <- k.getFlogObj( "./report/cStepValMtx.txt" )   ;log.cStepValMtx$fLogStr("start", pTime=T ,pAppend=F )

    mtxInfoLst <- list()
    for( zhIdx in zhName ){

        hCntMtx <- sapply( rptLst[[zhIdx]]$ccObjLst ,function(ccObj){ ccObj$cntMtx } )
        hCntMtx <- t(hCntMtx)       ;colnames(hCntMtx) <- name.cntMtx
        hAuxCntMtx <- sapply( rptLst[[zhIdx]]$ccObjLst ,function(ccObj){ ccObj$auxCntMtx } )
        hAuxCntMtx <- t(hAuxCntMtx) ;colnames( hAuxCntMtx ) <- name.auxCntMtx

        hCntMtx <- cbind( hCntMtx ,hAuxCntMtx )
        hScoreMtx <- sapply( rptLst[[zhIdx]]$ccObjLst ,function(ccObj){ ccObj$cccObj$scoreMtx })
        hScoreMtx <- t(hScoreMtx)   ;colnames( hScoreMtx ) <- name.cccObj.scoreMtx
        hScoreMtx2 <- sapply( rptLst[[zhIdx]]$ccObjLst ,function(ccObj){ ccObj$cccObj$scoreMtx2 })
        hScoreMtx2 <- t(hScoreMtx2)   ;colnames( hScoreMtx2 ) <- name.cccObj.scoreMtx2
        hScoreMtx3 <- sapply( rptLst[[zhIdx]]$ccObjLst ,function(ccObj){ ccObj$cccObj$scoreMtx3 })
        hScoreMtx3 <- t(hScoreMtx3)   ;colnames( hScoreMtx3 ) <- name.cccObj.scoreMtx3
        hScoreMtx4 <- sapply( rptLst[[zhIdx]]$ccObjLst ,function(ccObj){ ccObj$cccObj$scoreMtx4 })
        hScoreMtx4 <- t(hScoreMtx4)   ;colnames( hScoreMtx4 ) <- name.cccObj.scoreMtx4
        hCStepValMtx <- sapply( rptLst[[zhIdx]]$ccObjLst ,function(ccObj){ ccObj$cccObj$cStepValMtx })
        hCStepValMtx <- t(hCStepValMtx) ;colnames( hCStepValMtx ) <- name.cccObj.cStepValMtx 

        hCntMtx[,"cStep"] <- hCntMtx[,"cStep"] - hCntMtx[,"cStep.w1"] - hCntMtx[,"cStep.w2"]
        hCntMtx[,"fStep"] <- hCntMtx[,"fStep"] - hCntMtx[,"fStep.w1"] - hCntMtx[,"fStep.w2"]
        hCntMtx[,"cStep.w1"] <- hCntMtx[,"cStep.w1"] - hScoreMtx[,"w1CStep.cnt"]

        log.cntMtx$fLogStr( sprintf("<%s> %d ---------------------------------",zhIdx,rptLst[[zhIdx]]$stdFiltedCnt) )
        log.cntMtx$fLogMtx(hCntMtx,pIndent="  ")

        log.scoreMtx$fLogStr(sprintf("<%s> %d ---------------------------------",zhIdx,rptLst[[zhIdx]]$stdFiltedCnt))
        log.scoreMtx$fLogMtx(hScoreMtx)

        log.scoreMtx2$fLogStr(sprintf("<%s> %d ---------------------------------",zhIdx,rptLst[[zhIdx]]$stdFiltedCnt))
        log.scoreMtx2$fLogMtx(hScoreMtx2)

        log.scoreMtx3$fLogStr(sprintf("<%s> %d ---------------------------------",zhIdx,rptLst[[zhIdx]]$stdFiltedCnt))
        log.scoreMtx3$fLogMtx(hScoreMtx3)

        log.scoreMtx4$fLogStr(sprintf("<%s> %d ---------------------------------",zhIdx,rptLst[[zhIdx]]$stdFiltedCnt))
        log.scoreMtx4$fLogMtx(hScoreMtx4)

        log.cStepValMtx$fLogStr(sprintf("<%s> %d ---------------------------------",zhIdx,rptLst[[zhIdx]]$stdFiltedCnt))
        log.cStepValMtx$fLogMtx(hCStepValMtx)

        mtxInfoLst[[zhIdx]] <- list( cntMtx=hCntMtx ,scoreMtx=hScoreMtx 
                                        ,scoreMtx2=hScoreMtx2 ,scoreMtx3=hScoreMtx3 ,scoreMtx4=hScoreMtx4 
                                        ,cStepValMtx=hCStepValMtx )

    } # for(zhIdx) -- mtxInfoLst

    rObj <- list( idStr=pIdStr
                    ,mtxInfoLst=mtxInfoLst ,hSpan=hSpan ,zhName=zhName ,phName=phName
                    ,name.cntMtx=name.cntMtx
                    ,name.auxCntMtx=name.auxCntMtx
                    ,name.cccObj.scoreMtx=name.cccObj.scoreMtx
                    ,name.cccObj.scoreMtx2=name.cccObj.scoreMtx2
                    ,name.cccObj.scoreMtx3=name.cccObj.scoreMtx3
                    ,name.cccObj.scoreMtx4=name.cccObj.scoreMtx4
                    ,name.cccObj.cStepValMtx=name.cccObj.cStepValMtx
                 )

    rObj$getMtx.byPhase <- function( ){
        # ( hist, filter )

        mtxObj <- list()
        # cntMtx/auxCntMtx --------------------------------------
        mtxLst <- list()    # phase
        for( phIdx in rObj$phName ){
            rowLst <- lapply( rObj$mtxInfoLst ,function( mtxInfo ){ mtxInfo$cntMtx[phIdx,] })
            mtxLst[[phIdx]] <-  do.call(rbind,rowLst)
        }
        mtxObj$cntMtxLst <- mtxLst

        # scoreMtx --------------------------------------
        mtxLst <- list()    # phase
        for( phIdx in rObj$phName ){
            rowLst <- lapply( rObj$mtxInfoLst ,function( mtxInfo ){ mtxInfo$scoreMtx[phIdx,] })
            mtxLst[[phIdx]] <-  do.call(rbind,rowLst)
        }
        mtxObj$scoreMtxLst <- mtxLst

        # scoreMtx2 --------------------------------------
        mtxLst <- list()    # phase
        for( phIdx in rObj$phName ){
            rowLst <- lapply( rObj$mtxInfoLst ,function( mtxInfo ){ mtxInfo$scoreMtx2[phIdx,] })
            mtxLst[[phIdx]] <-  do.call(rbind,rowLst)
        }
        mtxObj$scoreMtx2Lst <- mtxLst

        # scoreMtx3 --------------------------------------
        mtxLst <- list()    # phase
        for( phIdx in rObj$phName ){
            rowLst <- lapply( rObj$mtxInfoLst ,function( mtxInfo ){ mtxInfo$scoreMtx3[phIdx,] })
            mtxLst[[phIdx]] <-  do.call(rbind,rowLst)
        }
        mtxObj$scoreMtx3Lst <- mtxLst

        # scoreMtx4 --------------------------------------
        mtxLst <- list()    # phase
        for( phIdx in rObj$phName ){
            rowLst <- lapply( rObj$mtxInfoLst ,function( mtxInfo ){ mtxInfo$scoreMtx4[phIdx,] })
            mtxLst[[phIdx]] <-  do.call(rbind,rowLst)
        }
        mtxObj$scoreMtx4Lst <- mtxLst

        # cStepValMtx --------------------------------------
        mtxLst <- list()    # phase
        for( phIdx in rObj$phName ){
            rowLst <- lapply( rObj$mtxInfoLst ,function( mtxInfo ){ mtxInfo$cStepValMtx[phIdx,] })
            mtxLst[[phIdx]] <-  do.call(rbind,rowLst)
        }
        mtxObj$cStepValMtxLst <- mtxLst

        return( mtxObj )
    }   # rObj$getMtx.byPh()

    rObj$getMtx.byFilter <- function( ){
        # ( hist, phase )
        mtxObj <- list()
        # cntMtx/auxCntMtx --------------------------------------
        mtxLst <- list()    # filter
        for( fIdx in rObj$name.cntMtx ){
            rowLst <- lapply( rObj$mtxInfoLst ,function( mtxInfo ){ mtxInfo$cntMtx[,fIdx] })
            mtxLst[[fIdx]] <-  do.call(rbind,rowLst)
        }
        mtxObj$cntMtxLst <- mtxLst

        # scoreMtx --------------------------------------
        mtxLst <- list()    # filter
        for( fIdx in rObj$name.cccObj.scoreMtx ){
            rowLst <- lapply( rObj$mtxInfoLst ,function( mtxInfo ){ mtxInfo$scoreMtx[,fIdx] })
            mtxLst[[fIdx]] <-  do.call(rbind,rowLst)
        }
        mtxObj$scoreMtxLst <- mtxLst

        # scoreMtx2 --------------------------------------
        mtxLst <- list()    # filter
        for( fIdx in rObj$name.cccObj.scoreMtx2 ){
            rowLst <- lapply( rObj$mtxInfoLst ,function( mtxInfo ){ mtxInfo$scoreMtx2[,fIdx] })
            mtxLst[[fIdx]] <-  do.call(rbind,rowLst)
        }
        mtxObj$scoreMtx2Lst <- mtxLst

        # scoreMtx3 --------------------------------------
        mtxLst <- list()    # filter
        for( fIdx in rObj$name.cccObj.scoreMtx3 ){
            rowLst <- lapply( rObj$mtxInfoLst ,function( mtxInfo ){ mtxInfo$scoreMtx3[,fIdx] })
            mtxLst[[fIdx]] <-  do.call(rbind,rowLst)
        }
        mtxObj$scoreMtx3Lst <- mtxLst

        # scoreMtx4 --------------------------------------
        mtxLst <- list()    # filter
        for( fIdx in rObj$name.cccObj.scoreMtx4 ){
            rowLst <- lapply( rObj$mtxInfoLst ,function( mtxInfo ){ mtxInfo$scoreMtx4[,fIdx] })
            mtxLst[[fIdx]] <-  do.call(rbind,rowLst)
        }
        mtxObj$scoreMtx4Lst <- mtxLst

        # cStepValMtx --------------------------------------
        mtxLst <- list()    # filter
        for( fIdx in rObj$name.cccObj.cStepValMtx ){
            rowLst <- lapply( rObj$mtxInfoLst ,function( mtxInfo ){ mtxInfo$cStepValMtx[,fIdx] })
            mtxLst[[fIdx]] <-  do.call(rbind,rowLst)
        }
        mtxObj$cStepValMtxLst <- mtxLst

        return( mtxObj )

    }   # rObj$getMtx.byPh()

    rObj$checkLastDup <- function( flagMtx ,show=T ){

        # flagMtx <- rstObj$getMtx.byPhase()$cntMtx$basic
        # flagMtx[flagMtx==0] <- NA

        rowLen <- nrow(flagMtx) ;colLen <- ncol(flagMtx)
        rName <- rownames(flagMtx)
        if( 2>rowLen ){
            return(" not enough data")
        }

        rptStr <- NULL
        for( rIdx in 2:rowLen ){
            matCnt <- sum(flagMtx[rIdx,]==flagMtx[rIdx-1,] ,na.rm=T)
            totCnt.cur <- colLen - sum( is.na(flagMtx[rIdx,]) )
            totCnt.lst <- colLen - sum( is.na(flagMtx[rIdx-1,]) )
            allMatch <- (totCnt.cur>0) && (totCnt.cur==totCnt.lst) && (totCnt.cur==matCnt)
            rstStr <- sprintf("%7dth row  mat %d in (%d->%d) %s %s (dist 1)"
                            ,rIdx, matCnt, totCnt.lst, totCnt.cur
                            , ifelse(allMatch,"*"," ") ,ifelse( matCnt>0 && !is.null(rName),rName[rIdx] ,"    " )
                        )
            rptStr <- c( rptStr ,rstStr )
            if( show )
                cat(sprintf("%s \n",rstStr))
        }

        return( rptStr )

    } # rObj$checkLastDup()

    rObj$checkPastDup <- function( flagMtx ,show=T ){

        rowLen <- nrow(flagMtx) ;colLen <- ncol(flagMtx)
        rName <- rownames(flagMtx)
        if( 2>rowLen ){
            return(" not enough data")
        }

        rptStr <- NULL
        for( rIdx in 2:rowLen ){
            rstStr <- sprintf("%7dth row %s",rIdx ,ifelse(!is.null(rName),rName[rIdx],"    ") )
            rptStr <- c( rptStr ,rstStr )
            if( show )
                cat(sprintf("%s \n",rstStr))

            for( tIdx in 1:(rIdx-1) ){
                matCnt <- sum(flagMtx[rIdx,]==flagMtx[tIdx,] ,na.rm=T)
                if( 0==matCnt )
                    next

                totCnt.cur <- colLen - sum( is.na(flagMtx[rIdx,]) )
                totCnt.tgt <- colLen - sum( is.na(flagMtx[tIdx,]) )
                allMatch <- (totCnt.cur>0) && (totCnt.cur==totCnt.tgt) && (totCnt.cur==matCnt)
                rstStr <- sprintf("    %7dth mat %d in %d->%d %s %s (dist %d)"
                                ,tIdx, matCnt, totCnt.tgt, totCnt.cur
                                ,ifelse(allMatch,"*"," ") ,ifelse( matCnt>0 && !is.null(rName),rName[tIdx] ,"    " )
                                ,rIdx-tIdx
                            )
                rptStr <- c( rptStr ,rstStr )
                if( show )
                    cat(sprintf("%s \n",rstStr))

            }
        }

        return( rptStr )

    } # rObj$checkPastDup()

    return( rObj )

}   # lab.getMtxLst( )

idStr <- "goldRstSpan"
rstObj <- lab.getMtxLst( goldRstSpan ,pIdStr=idStr )
save( rstObj ,file=sprintf("./save/stdZoidFltRst/Obj_rstObj.%s.save",idStr) )
#   rstObj <- lab.getMtxLst( grp2RstSpan ,pIdStr="grp2RstSpan" )
#   rstObj <- lab.getMtxLst( fullRstSpan ,pIdStr="fullRstSpan" )
if( FALSE ){ 
    # cStep, fStep 에서 w1,w2 포함 제외 -> lab.getMtxLst() 에서 처리.
    # for( nIdx in rstObj$zhName ){
    #     cntMtx <- rstObj$mtxInfoLst[[nIdx]]$cntMtx
    #     scoreMtx <- rstObj$mtxInfoLst[[nIdx]]$scoreMtx
    #     rstObj$mtxInfoLst[[nIdx]]$cntMtx[,"cStep"] <- cntMtx[,"cStep"] - cntMtx[,"cStep.w1"] - cntMtx[,"cStep.w2"]
    #     rstObj$mtxInfoLst[[nIdx]]$cntMtx[,"fStep"] <- cntMtx[,"fStep"] - cntMtx[,"fStep.w1"] - cntMtx[,"fStep.w2"]
    #     rstObj$mtxInfoLst[[nIdx]]$cntMtx[,"cStep.w1"] <- cntMtx[,"cStep.w1"] - scoreMtx[,"w1CStep.cnt"]
    #     rstObj$mtxInfoLst[[nIdx]]$cntMtx[,"fStep.w1"] <- cntMtx[,"fStep.w1"] - scoreMtx[,"w1FStep.cnt"]
    # }
}


checkLastDup <- function( flagMtx ,show=T ){

    # flagMtx <- rstObj$getMtx.byPhase()$cntMtx$basic
    # flagMtx[flagMtx==0] <- NA

    rowLen <- nrow(flagMtx) ;colLen <- ncol(flagMtx)
    rName <- rownames(flagMtx)
    if( 2>rowLen ){
        return(" not enough data")
    }

    rptStr <- NULL
    for( rIdx in 2:rowLen ){
        matCnt <- sum(flagMtx[rIdx,]==flagMtx[rIdx-1,] ,na.rm=T)
        totCnt.cur <- colLen - sum( is.na(flagMtx[rIdx,]) )
        totCnt.lst <- colLen - sum( is.na(flagMtx[rIdx-1,]) )
        allMatch <- (totCnt>0) && (totCnt.cur==totCnt.lst) && (totCnt.cur==matCnt)
        rstStr <- sprintf("%7dth row  mat %d in (%d->%d) %s %s (dist 1)"
                        ,rIdx, matCnt, totCnt.lst, totCnt.cur
                        , ifelse(allMatch,"*"," ") ,ifelse( matCnt>0 && !is.null(rName),rName[rIdx] ,"    " )
                    )
        rptStr <- c( rptStr ,rstStr )
        if( show )
            cat(sprintf("%s \n",rstStr))
    }

    return( rptStr )

} # checkLastDup()


checkPastDup <- function( flagMtx ,show=T ){

    rowLen <- nrow(flagMtx) ;colLen <- ncol(flagMtx)
    rName <- rownames(flagMtx)
    if( 2>rowLen ){
        return(" not enough data")
    }

    rptStr <- NULL
    for( rIdx in 2:rowLen ){
        rstStr <- sprintf("%7dth row %s",rIdx ,ifelse(!is.null(rName),rName[rIdx],"    ") )
        if( show )
            cat(sprintf("%s \n",rstStr))

        for( tIdx in 1:(rIdx-1) ){
            matCnt <- sum(flagMtx[rIdx,]==flagMtx[tIdx,] ,na.rm=T)
            if( 0==matCnt )
                next

            totCnt.cur <- colLen - sum( is.na(flagMtx[rIdx,]) )
            totCnt.tgt <- colLen - sum( is.na(flagMtx[tIdx,]) )
            allMatch <- (totCnt>0) && (totCnt.cur==totCnt.tgt) && (totCnt.cur==matCnt)
            rstStr <- sprintf("    %7dth mat %d in %d->%d %s %s (dist %d)"
                            ,tIdx, matCnt, totCnt.tgt, totCnt.cur
                            ,ifelse(allMatch,"*"," ") ,ifelse( matCnt>0 && !is.null(rName),rName[tIdx] ,"    " )
                            ,rIdx-tIdx
                        )
            rptStr <- c( rptStr ,rstStr )
            if( show )
                cat(sprintf("%s \n",rstStr))

        }
    }

    return( rptStr )

} # checkPastDup()


# testMtx ---------------------------------------------------------------------------
logObj <- k.getFlogObj( "./report/logObj.txt" )        ;logObj$fLogStr("start", pTime=T ,pAppend=F )
phName.short <- gsub("^next","",rstObj$phName)
phName.short <- gsub("^ColVal","CV",phName.short)
phName.short <- gsub("StepBin","Step",phName.short)

logObj$fLogStr(sprintf("<< cntMtx >> ------------------------"))
hpnMtx <- NULL  ;sumMtx <- NULL ;evtMtx <- NULL
for( phIdx in rstObj$phName ){
    # cntMtx( No w filter in cStep, fStep ) -----------------------------------------
    testMtx <- NULL
    for( zhIdx in rstObj$zhName ){
        mtxObj <- rstObj$mtxInfoLst[[zhIdx]]
        testMtx <- rbind( testMtx ,mtxObj$cntMtx[ phIdx,,drop=F] )
    } # for(zhIdx)
    rownames(testMtx) <- rstObj$zhName
    # cStep.score <-  testMtx[,"cStep"] - testMtx[,"cStep.w1"] - testMtx[,"cStep.w2"]
    # testMtx[,"cStep"] <- ifelse( cStep.score>0 ,cStep.score ,0 )
    # fStep.score <- testMtx[,"fStep"] - testMtx[,"fStep.w1"] - testMtx[,"fStep.w2"]
    # testMtx[,"fStep"] <- ifelse( fStep.score>0 ,fStep.score ,0 )

    testMtx.evt <- testMtx > 1  ;testMtx.evt[,"rem"] <- testMtx[,"rem"] > 2
    rowObj <- u1.anaMtx.cnt( pMtx=testMtx ,pMtx.evt=testMtx.evt ,pEvtName="evt" ,pRow=T )
    colObj <- u1.anaMtx.cnt( pMtx=testMtx ,pMtx.evt=testMtx.evt ,pEvtName="evt" ,pRow=F )

    hpnMtx <- cbind( hpnMtx ,rowObj$anaMtx[,"hpn"] )
    sumMtx <- cbind( sumMtx ,rowObj$anaMtx[,"sum"] )
    evtMtx <- cbind( evtMtx ,rowObj$anaMtx[,"evt"] )

    logObj$fLogStr(sprintf("    [%s]",phIdx))
    logObj$fLogMtxLst( list(rowObj$anaMtx,testMtx) ,pIndent="    " ,pSep=" |"  )
}
if( TRUE ){
    colnames(hpnMtx) <- phName.short  ;colnames(sumMtx) <- phName.short     ;colnames(evtMtx) <- phName.short
    logObj$fLogStr(sprintf("  ** [hpnMtx]"))
    rowObj <- u1.anaMtx.cnt( pMtx=hpnMtx ,pMtx.evt=hpnMtx>3 ,pEvtName="e>3" ,pRow=T )
    logObj$fLogMtxLst( list(rowObj$anaMtx,hpnMtx) ,pIndent="    " ,pSep=" :")
    logObj$fLogStr(sprintf("  ** [sumMtx]"))
    rowObj <- u1.anaMtx.cnt( pMtx=sumMtx ,pMtx.evt=sumMtx>5 ,pEvtName="e>5" ,pRow=T )
    logObj$fLogMtxLst( list(rowObj$anaMtx,sumMtx) ,pIndent="    " ,pSep=" :")
    logObj$fLogStr(sprintf("  ** [evtMtx]"))
    rowObj <- u1.anaMtx.cnt( pMtx=evtMtx ,pMtx.evt=evtMtx>1 ,pEvtName="e>1" ,pRow=T )
    logObj$fLogMtxLst( list(rowObj$anaMtx,evtMtx) ,pIndent="    " ,pSep=" :")
}

logObj$fLogStr(sprintf("<< scoreMtx >> ------------------------"))
hpnMtx <- NULL  ;sumMtx <- NULL ;evtMtx <- NULL
for( phIdx in rstObj$phName ){
    # scoreMtx -----------------------------------------
    testMtx <- NULL
    for( zhIdx in rstObj$zhName ){
        mtxObj <- rstObj$mtxInfoLst[[zhIdx]]
        testMtx <- rbind( testMtx ,mtxObj$scoreMtx[ phIdx,,drop=F] )
    } # for(zhIdx)
    rownames(testMtx) <- rstObj$zhName
    cName <- setdiff( colnames(testMtx) ,c("w1CStep.matLen","w1FStep.matLen") )
    testMtx <- testMtx[,cName]

    testMtx.evt <- testMtx > 0
    rowObj <- u1.anaMtx.cnt( pMtx=testMtx ,pMtx.evt=testMtx.evt ,pEvtName="evt" ,pRow=T )
    colObj <- u1.anaMtx.cnt( pMtx=testMtx ,pMtx.evt=testMtx.evt ,pEvtName="evt" ,pRow=F )

    hpnMtx <- cbind( hpnMtx ,rowObj$anaMtx[,"hpn"] )
    sumMtx <- cbind( sumMtx ,rowObj$anaMtx[,"sum"] )
    evtMtx <- cbind( evtMtx ,rowObj$anaMtx[,"evt"] )

    logObj$fLogStr(sprintf("    [%s]",phIdx))
    logObj$fLogMtxLst( list(rowObj$anaMtx,testMtx) ,pIndent="      " ,pSep=" |"  )

}
if( TRUE ){
    colnames(hpnMtx) <- phName.short  ;colnames(sumMtx) <- phName.short     ;colnames(evtMtx) <- phName.short
    logObj$fLogStr(sprintf("  ** [hpnMtx]"))
    rowObj <- u1.anaMtx.cnt( pMtx=hpnMtx ,pMtx.evt=hpnMtx>2 ,pEvtName="e>2" ,pRow=T )
    logObj$fLogMtxLst( list(rowObj$anaMtx,hpnMtx) ,pIndent="    " ,pSep=" :")
    logObj$fLogStr(sprintf("  ** [sumMtx]"))
    rowObj <- u1.anaMtx.cnt( pMtx=sumMtx ,pMtx.evt=sumMtx>3 ,pEvtName="e>3" ,pRow=T )
    logObj$fLogMtxLst( list(rowObj$anaMtx,sumMtx) ,pIndent="    " ,pSep=" :")
    logObj$fLogStr(sprintf("  ** [evtMtx]"))
    rowObj <- u1.anaMtx.cnt( pMtx=evtMtx ,pMtx.evt=evtMtx>2 ,pEvtName="e>2" ,pRow=T )
    logObj$fLogMtxLst( list(rowObj$anaMtx,evtMtx) ,pIndent="    " ,pSep=" :")
}

logObj$fLogStr(sprintf("<< scoreMtx2 >> ------------------------"))
hpnMtx <- NULL  ;sumMtx <- NULL ;evtMtx <- NULL
for( phIdx in rstObj$phName ){
    # scoreMtx2 -----------------------------------------
    testMtx <- NULL
    for( zhIdx in rstObj$zhName ){
        mtxObj <- rstObj$mtxInfoLst[[zhIdx]]
        testMtx <- rbind( testMtx ,mtxObj$scoreMtx2[ phIdx,,drop=F] )
    } # for(zhIdx)
    rownames(testMtx) <- rstObj$zhName
    cName <- colnames(testMtx)
    testMtx <- testMtx[,cName]

    testMtx.evt <- testMtx > 0
    rowObj <- u1.anaMtx.cnt( pMtx=testMtx ,pMtx.evt=testMtx.evt ,pEvtName="evt" ,pRow=T )
    colObj <- u1.anaMtx.cnt( pMtx=testMtx ,pMtx.evt=testMtx.evt ,pEvtName="evt" ,pRow=F )

    hpnMtx <- cbind( hpnMtx ,rowObj$anaMtx[,"hpn"] )
    sumMtx <- cbind( sumMtx ,rowObj$anaMtx[,"sum"] )
    evtMtx <- cbind( evtMtx ,rowObj$anaMtx[,"evt"] )

    logObj$fLogStr(sprintf("    [%s]",phIdx))
    logObj$fLogMtxLst( list(rowObj$anaMtx,testMtx) ,pIndent="      " ,pSep=" |"  )

}
if( TRUE ){
    colnames(hpnMtx) <- phName.short  ;colnames(sumMtx) <- phName.short     ;colnames(evtMtx) <- phName.short
    logObj$fLogStr(sprintf("  ** [hpnMtx]"))
    rowObj <- u1.anaMtx.cnt( pMtx=hpnMtx ,pMtx.evt=hpnMtx>2 ,pEvtName="e>2" ,pRow=T )
    logObj$fLogMtxLst( list(rowObj$anaMtx,hpnMtx) ,pIndent="    " ,pSep=" :")
    logObj$fLogStr(sprintf("  ** [sumMtx]"))
    rowObj <- u1.anaMtx.cnt( pMtx=sumMtx ,pMtx.evt=sumMtx>3 ,pEvtName="e>3" ,pRow=T )
    logObj$fLogMtxLst( list(rowObj$anaMtx,sumMtx) ,pIndent="    " ,pSep=" :")
    logObj$fLogStr(sprintf("  ** [evtMtx]"))
    rowObj <- u1.anaMtx.cnt( pMtx=evtMtx ,pMtx.evt=evtMtx>2 ,pEvtName="e>2" ,pRow=T )
    logObj$fLogMtxLst( list(rowObj$anaMtx,evtMtx) ,pIndent="    " ,pSep=" :")
}

logObj$fLogStr(sprintf("<< scoreMtx3 >> ------------------------"))
hpnMtx <- NULL  ;sumMtx <- NULL ;evtMtx <- NULL
for( phIdx in rstObj$phName ){
    # scoreMtx3 -----------------------------------------
    testMtx <- NULL
    for( zhIdx in rstObj$zhName ){
        mtxObj <- rstObj$mtxInfoLst[[zhIdx]]
        testMtx <- rbind( testMtx ,mtxObj$scoreMtx3[ phIdx,,drop=F] )
    } # for(zhIdx)
    rownames(testMtx) <- rstObj$zhName
    cName <- colnames(testMtx)
    testMtx <- testMtx[,cName]

    testMtx.evt <- testMtx > 0
    rowObj <- u1.anaMtx.cnt( pMtx=testMtx ,pMtx.evt=testMtx.evt ,pEvtName="evt" ,pRow=T )
    colObj <- u1.anaMtx.cnt( pMtx=testMtx ,pMtx.evt=testMtx.evt ,pEvtName="evt" ,pRow=F )

    hpnMtx <- cbind( hpnMtx ,rowObj$anaMtx[,"hpn"] )
    sumMtx <- cbind( sumMtx ,rowObj$anaMtx[,"sum"] )
    evtMtx <- cbind( evtMtx ,rowObj$anaMtx[,"evt"] )

    logObj$fLogStr(sprintf("    [%s]",phIdx))
    logObj$fLogMtxLst( list(rowObj$anaMtx,testMtx) ,pIndent="      " ,pSep=" |"  )

}
if( TRUE ){
    colnames(hpnMtx) <- phName.short  ;colnames(sumMtx) <- phName.short     ;colnames(evtMtx) <- phName.short
    logObj$fLogStr(sprintf("  ** [hpnMtx]"))
    rowObj <- u1.anaMtx.cnt( pMtx=hpnMtx ,pMtx.evt=hpnMtx>2 ,pEvtName="e>2" ,pRow=T )
    logObj$fLogMtxLst( list(rowObj$anaMtx,hpnMtx) ,pIndent="    " ,pSep=" :")
    logObj$fLogStr(sprintf("  ** [sumMtx]"))
    rowObj <- u1.anaMtx.cnt( pMtx=sumMtx ,pMtx.evt=sumMtx>3 ,pEvtName="e>3" ,pRow=T )
    logObj$fLogMtxLst( list(rowObj$anaMtx,sumMtx) ,pIndent="    " ,pSep=" :")
    logObj$fLogStr(sprintf("  ** [evtMtx]"))
    rowObj <- u1.anaMtx.cnt( pMtx=evtMtx ,pMtx.evt=evtMtx>2 ,pEvtName="e>2" ,pRow=T )
    logObj$fLogMtxLst( list(rowObj$anaMtx,evtMtx) ,pIndent="    " ,pSep=" :")
}


logObj$fLogStr(sprintf("<< scoreMtx4 >> ------------------------"))
hpnMtx <- NULL  ;sumMtx <- NULL ;evtMtx <- NULL
for( phIdx in rstObj$phName ){
    # scoreMtx4 -----------------------------------------
    testMtx <- NULL
    for( zhIdx in rstObj$zhName ){
        mtxObj <- rstObj$mtxInfoLst[[zhIdx]]
        testMtx <- rbind( testMtx ,mtxObj$scoreMtx4[ phIdx,,drop=F] )
    } # for(zhIdx)
    rownames(testMtx) <- rstObj$zhName
    cName <- colnames(testMtx)
    testMtx <- testMtx[,cName]

    testMtx.evt <- testMtx > 0
    rowObj <- u1.anaMtx.cnt( pMtx=testMtx ,pMtx.evt=testMtx.evt ,pEvtName="evt" ,pRow=T )
    colObj <- u1.anaMtx.cnt( pMtx=testMtx ,pMtx.evt=testMtx.evt ,pEvtName="evt" ,pRow=F )

    hpnMtx <- cbind( hpnMtx ,rowObj$anaMtx[,"hpn"] )
    sumMtx <- cbind( sumMtx ,rowObj$anaMtx[,"sum"] )
    evtMtx <- cbind( evtMtx ,rowObj$anaMtx[,"evt"] )

    logObj$fLogStr(sprintf("    [%s]",phIdx))
    logObj$fLogMtxLst( list(rowObj$anaMtx,testMtx) ,pIndent="      " ,pSep=" |"  )

}
if( TRUE ){
    colnames(hpnMtx) <- phName.short  ;colnames(sumMtx) <- phName.short     ;colnames(evtMtx) <- phName.short
    logObj$fLogStr(sprintf("  ** [hpnMtx]"))
    rowObj <- u1.anaMtx.cnt( pMtx=hpnMtx ,pMtx.evt=hpnMtx>2 ,pEvtName="e>2" ,pRow=T )
    logObj$fLogMtxLst( list(rowObj$anaMtx,hpnMtx) ,pIndent="    " ,pSep=" :")
    logObj$fLogStr(sprintf("  ** [sumMtx]"))
    rowObj <- u1.anaMtx.cnt( pMtx=sumMtx ,pMtx.evt=sumMtx>3 ,pEvtName="e>3" ,pRow=T )
    logObj$fLogMtxLst( list(rowObj$anaMtx,sumMtx) ,pIndent="    " ,pSep=" :")
    logObj$fLogStr(sprintf("  ** [evtMtx]"))
    rowObj <- u1.anaMtx.cnt( pMtx=evtMtx ,pMtx.evt=evtMtx>2 ,pEvtName="e>2" ,pRow=T )
    logObj$fLogMtxLst( list(rowObj$anaMtx,evtMtx) ,pIndent="    " ,pSep=" :")
}

logObj$fLogStr(sprintf("<< cStepValMtx >> ------------------------"))
hpnMtx <- NULL  ;sumMtx <- NULL ;evtMtx <- NULL
for( phIdx in rstObj$phName ){
    # scoreMtx -----------------------------------------
    testMtx <- NULL
    for( zhIdx in rstObj$zhName ){
        mtxObj <- rstObj$mtxInfoLst[[zhIdx]]
        testMtx <- rbind( testMtx ,mtxObj$cStepValMtx[ phIdx,,drop=F] )
    } # for(zhIdx)
    rownames(testMtx) <- rstObj$zhName

    testMtx.evt <- testMtx > 0
    rowObj <- u1.anaMtx.cnt( pMtx=testMtx ,pMtx.evt=testMtx.evt ,pEvtName="evt" ,pRow=T )
    colObj <- u1.anaMtx.cnt( pMtx=testMtx ,pMtx.evt=testMtx.evt ,pEvtName="evt" ,pRow=F )

    c3 <- apply( testMtx[,c("c31","c32","c33","c34")] ,1 ,sum )
    c2 <- apply( testMtx[,c("c21","c22","c23","c24","c25")] ,1 ,sum )
    grpMtx <- cbind( c3 ,c2 ,testMtx[,c("max2","min2")] )
    rowObj.sum <- u1.anaMtx.cnt( pMtx=grpMtx ,pMtx.evt=grpMtx>0 ,pEvtName="evt" ,pRow=T )
    colObj.sum <- u1.anaMtx.cnt( pMtx=grpMtx ,pMtx.evt=grpMtx>0 ,pEvtName="evt" ,pRow=F )

    hpnMtx <- cbind( hpnMtx ,rowObj.sum$anaMtx[,"hpn"] )
    sumMtx <- cbind( sumMtx ,rowObj.sum$anaMtx[,"sum"] )
    evtMtx <- cbind( evtMtx ,rowObj.sum$anaMtx[,"evt"] )

    logObj$fLogStr(sprintf("    [%s]",phIdx))
    logObj$fLogMtxLst( list(rowObj$anaMtx,testMtx,rowObj.sum$anaMtx,grpMtx) ,pIndent="      " ,pSep=" |"  )

}
if( TRUE ){
    colnames(hpnMtx) <- phName.short  ;colnames(sumMtx) <- phName.short     ;colnames(evtMtx) <- phName.short
    logObj$fLogStr(sprintf("  ** [hpnMtx]"))
    rowObj <- u1.anaMtx.cnt( pMtx=hpnMtx ,pMtx.evt=hpnMtx>1 ,pEvtName="e>3" ,pRow=T )
    logObj$fLogMtxLst( list(rowObj$anaMtx,hpnMtx) ,pIndent="    " ,pSep=" :")
    logObj$fLogStr(sprintf("  ** [sumMtx]"))
    rowObj <- u1.anaMtx.cnt( pMtx=sumMtx ,pMtx.evt=sumMtx>2 ,pEvtName="e>2" ,pRow=T )
    logObj$fLogMtxLst( list(rowObj$anaMtx,sumMtx) ,pIndent="    " ,pSep=" :")
    logObj$fLogStr(sprintf("  ** [evtMtx]"))
    rowObj <- u1.anaMtx.cnt( pMtx=evtMtx ,pMtx.evt=evtMtx>1 ,pEvtName="e>1" ,pRow=T )
    logObj$fLogMtxLst( list(rowObj$anaMtx,evtMtx) ,pIndent="    " ,pSep=" :")
}

# testMtx by mtx---------------------------------------------------------------------------
logObj <- k.getFlogObj( "./report/logObj_cntMtx.txt" )        ;logObj$fLogStr("start", pTime=T ,pAppend=F )
for( cnIdx in c(rstObj$name.cntMtx,rstObj$name.auxCntMtx) ){
    logObj$fLogStr(sprintf("<cntMtx: %s >",cnIdx))
    testMtx <- NULL
    for( hnIdx in rstObj$zhName ){
        testMtx <- rbind( testMtx ,rstObj$mtxInfoLst[[hnIdx]]$cntMtx[,cnIdx] )
    } # for( hnIdx )
    rownames( testMtx ) <- rstObj$zhName

    sumMtx <- NULL
    sumMtx <- cbind( sumMtx ,apply( testMtx ,1 ,function(dRow){sum(dRow)} ) )   # sum
    sumMtx <- cbind( sumMtx ,apply( testMtx ,1 ,function(dRow){sum(dRow>0)} ) ) # hpn
    if( cnIdx %in% c("raw","rawFV","cStep","fStep") ){  
        sumMtx <- cbind( sumMtx ,apply( testMtx ,1 ,function(dRow){sum(dRow>1)} ) ) # evt > 0
    } else if( cnIdx %in% c("rem") ) {
        sumMtx <- cbind( sumMtx ,apply( testMtx ,1 ,function(dRow){sum(dRow>2)} ) ) # evt > 0
    } else {    # "raw.w1"   "cStep.w1" "cStep.w2" "fStep.w1" "fStep.w2"
        sumMtx <- cbind( sumMtx ,apply( testMtx ,1 ,function(dRow){sum(dRow>0)} ) ) # evt > 0
    }
    colnames(sumMtx) <- c("sum","hpn","evt")

    shortName <- gsub("^next" ,"" ,colnames(testMtx) )
    shortName <- gsub("^ColVal","CV",shortName)
    colnames(testMtx) <- shortName
    logObj$fLogMtxLst( list(sumMtx,testMtx) ,pIndent="      " ,pSep=" |"  )
    logObj$fLogStr("\n")

} # for(cnIdx)

logObj <- k.getFlogObj( "./report/logObj_scoreMtx.txt" )        ;logObj$fLogStr("start", pTime=T ,pAppend=F )
for( cnIdx in setdiff(rstObj$name.cccObj.scoreMtx,c("w1CStep.matLen","w1FStep.matLen")) ){
    logObj$fLogStr(sprintf("<scoreMtx: %s >",cnIdx))
    testMtx <- NULL
    for( hnIdx in rstObj$zhName ){
        testMtx <- rbind( testMtx ,rstObj$mtxInfoLst[[hnIdx]]$scoreMtx[,cnIdx] )
    } # for( hnIdx )
    rownames( testMtx ) <- rstObj$zhName

    sumMtx <- NULL
    sumMtx <- cbind( sumMtx ,apply( testMtx ,1 ,function(dRow){sum(dRow)} ) )   # sum
    sumMtx <- cbind( sumMtx ,apply( testMtx ,1 ,function(dRow){sum(dRow>0)} ) ) # hpn
    if( cnIdx %in% c("QQE") ){  
    } else {    # "reb" "nbor" "spanM" "quoAll" "quoPtn" "zw" "remH0" "remH1" "cStep2" "cStep3" "w1CStep.cnt" "w1FStep.cnt"
        sumMtx <- cbind( sumMtx ,apply( testMtx ,1 ,function(dRow){sum(dRow>0)} ) ) # evt > 0
    }
    colnames(sumMtx) <- c("sum","hpn","evt")

    shortName <- gsub("^next" ,"" ,colnames(testMtx) )
    shortName <- gsub("^ColVal","CV",shortName)
    colnames(testMtx) <- shortName
    logObj$fLogMtxLst( list(sumMtx,testMtx) ,pIndent="      " ,pSep=" |"  )
    logObj$fLogStr("\n")

} # for(cnIdx)

logObj <- k.getFlogObj( "./report/logObj_scoreMtx2.txt" )        ;logObj$fLogStr("start", pTime=T ,pAppend=F )
for( cnIdx in rstObj$name.cccObj.scoreMtx2 ){
    logObj$fLogStr(sprintf("<scoreMtx2: %s >",cnIdx))
    testMtx <- NULL
    for( hnIdx in rstObj$zhName ){
        testMtx <- rbind( testMtx ,rstObj$mtxInfoLst[[hnIdx]]$scoreMtx2[,cnIdx] )
    } # for( hnIdx )
    rownames( testMtx ) <- rstObj$zhName

    sumMtx <- NULL
    sumMtx <- cbind( sumMtx ,apply( testMtx ,1 ,function(dRow){sum(dRow)} ) )   # sum
    sumMtx <- cbind( sumMtx ,apply( testMtx ,1 ,function(dRow){sum(dRow>0)} ) ) # hpn
    if( cnIdx %in% c("QQE") ){  
    } else {    # "reb" "nbor" "spanM" "quoAll" "quoPtn" "zw" "remH0" "remH1" "cStep2" "cStep3" "w1CStep.cnt" "w1FStep.cnt"
        sumMtx <- cbind( sumMtx ,apply( testMtx ,1 ,function(dRow){sum(dRow>0)} ) ) # evt > 0
    }
    colnames(sumMtx) <- c("sum","hpn","evt")

    shortName <- gsub("^next" ,"" ,colnames(testMtx) )
    shortName <- gsub("^ColVal","CV",shortName)
    colnames(testMtx) <- shortName
    logObj$fLogMtxLst( list(sumMtx,testMtx) ,pIndent="      " ,pSep=" |"  )
    logObj$fLogStr("\n")

} # for(cnIdx)

logObj <- k.getFlogObj( "./report/logObj_scoreMtx3.txt" )        ;logObj$fLogStr("start", pTime=T ,pAppend=F )
for( cnIdx in rstObj$name.cccObj.scoreMtx3 ){
    logObj$fLogStr(sprintf("<scoreMtx3: %s >",cnIdx))
    testMtx <- NULL
    for( hnIdx in rstObj$zhName ){
        testMtx <- rbind( testMtx ,rstObj$mtxInfoLst[[hnIdx]]$scoreMtx3[,cnIdx] )
    } # for( hnIdx )
    rownames( testMtx ) <- rstObj$zhName

    sumMtx <- NULL
    sumMtx <- cbind( sumMtx ,apply( testMtx ,1 ,function(dRow){sum(dRow)} ) )   # sum
    sumMtx <- cbind( sumMtx ,apply( testMtx ,1 ,function(dRow){sum(dRow>0)} ) ) # hpn
    if( cnIdx %in% c("QQE") ){  
    } else {    # "reb" "nbor" "spanM" "quoAll" "quoPtn" "zw" "remH0" "remH1" "cStep2" "cStep3" "w1CStep.cnt" "w1FStep.cnt"
        sumMtx <- cbind( sumMtx ,apply( testMtx ,1 ,function(dRow){sum(dRow>0)} ) ) # evt > 0
    }
    colnames(sumMtx) <- c("sum","hpn","evt")

    shortName <- gsub("^next" ,"" ,colnames(testMtx) )
    shortName <- gsub("^ColVal","CV",shortName)
    colnames(testMtx) <- shortName
    logObj$fLogMtxLst( list(sumMtx,testMtx) ,pIndent="      " ,pSep=" |"  )
    logObj$fLogStr("\n")

} # for(cnIdx)

logObj <- k.getFlogObj( "./report/logObj_scoreMtx4.txt" )        ;logObj$fLogStr("start", pTime=T ,pAppend=F )
for( cnIdx in rstObj$name.cccObj.scoreMtx4 ){
    logObj$fLogStr(sprintf("<scoreMtx4: %s >",cnIdx))
    testMtx <- NULL
    for( hnIdx in rstObj$zhName ){
        testMtx <- rbind( testMtx ,rstObj$mtxInfoLst[[hnIdx]]$scoreMtx4[,cnIdx] )
    } # for( hnIdx )
    rownames( testMtx ) <- rstObj$zhName

    sumMtx <- NULL
    sumMtx <- cbind( sumMtx ,apply( testMtx ,1 ,function(dRow){sum(dRow)} ) )   # sum
    sumMtx <- cbind( sumMtx ,apply( testMtx ,1 ,function(dRow){sum(dRow>0)} ) ) # hpn
    if( cnIdx %in% c("QQE") ){  
    } else {    # "reb" "nbor" "spanM" "quoAll" "quoPtn" "zw" "remH0" "remH1" "cStep2" "cStep3" "w1CStep.cnt" "w1FStep.cnt"
        sumMtx <- cbind( sumMtx ,apply( testMtx ,1 ,function(dRow){sum(dRow>0)} ) ) # evt > 0
    }
    colnames(sumMtx) <- c("sum","hpn","evt")

    shortName <- gsub("^next" ,"" ,colnames(testMtx) )
    shortName <- gsub("^ColVal","CV",shortName)
    colnames(testMtx) <- shortName
    logObj$fLogMtxLst( list(sumMtx,testMtx) ,pIndent="      " ,pSep=" |"  )
    logObj$fLogStr("\n")

} # for(cnIdx)

logObj <- k.getFlogObj( "./report/logObj_cStepValMtx.txt" )        ;logObj$fLogStr("start", pTime=T ,pAppend=F )
for( cnIdx in rstObj$name.cccObj.cStepValMtx ){
    logObj$fLogStr(sprintf("<cStepValMtx: %s >",cnIdx))
    testMtx <- NULL
    for( hnIdx in rstObj$zhName ){
        testMtx <- rbind( testMtx ,rstObj$mtxInfoLst[[hnIdx]]$cStepValMtx[,cnIdx] )
    } # for( hnIdx )
    rownames( testMtx ) <- rstObj$zhName

    sumMtx <- NULL
    sumMtx <- cbind( sumMtx ,apply( testMtx ,1 ,function(dRow){sum(dRow  )} ) )   # sum
    sumMtx <- cbind( sumMtx ,apply( testMtx ,1 ,function(dRow){sum(dRow>0)} ) ) # hpn
    if( cnIdx %in% c("QQE") ){  
    } else {    # "c31"  "c32"  "c33"  "c34"  "c21"  "c22"  "c23"  "c24"  "c25"  "max2" "min2"
        sumMtx <- cbind( sumMtx ,apply( testMtx ,1 ,function(dRow){sum(dRow>0)} ) ) # evt > 0
    }
    colnames(sumMtx) <- c("sum","hpn","evt")

    shortName <- gsub("^next" ,"" ,colnames(testMtx) )
    shortName <- gsub("^ColVal","CV",shortName)
    colnames(testMtx) <- shortName
    logObj$fLogMtxLst( list(sumMtx,testMtx) ,pIndent="      " ,pSep=" |"  )
    logObj$fLogStr("\n")

} # for(cnIdx)




#- [getMtx.byPhase()] ------------------------------------------------------------------------------
logObj <- k.getFlogObj( "./report/testRst_byPhase.txt" )        ;logObj$fLogStr("start", pTime=T ,pAppend=F )
mtxLst <- rstObj$getMtx.byPhase()	# mtxLst[["cntMtxLst"]]$basic

mtxLstName <- "cntMtxLst"
logObj$fLogStr(sprintf("< %s > ---------------------------------------------------",mtxLstName))
for( phIdx in rstObj$phName ){
    #   raw rawFV rem cStep fStep raw.w1 cStep.w1 cStep.w2 fStep.w1 fStep.w2 auxZW auxQuo
    logObj$fLogStr(sprintf("phName:%s",phIdx))
    mtx <- mtxLst[[mtxLstName]][[phIdx]]

    colSet <- c("raw","rem","cStep","fStep","auxZW")
    {   colEH  <- c( 2, 3, 2, 2, 1 )   # event horizen
        logObj$fLogStr(sprintf("* colSet00 Event: %s(%s)",paste(colSet,collapse=","),paste(colEH,collapse=",")))
        flagMtx <- t(apply( mtx[,colSet] ,1 ,function(rData){rData>=colEH}))
        flagMtx[!flagMtx] <- NA
        logObj$fLogStr("rstObj$checkLastDup()")
        rptStr <- rstObj$checkLastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
        logObj$fLogStr("rstObj$checkPastDup()")
        rptStr <- rstObj$checkPastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))

        logObj$fLogStr(sprintf("* colSet00 Happen: %s",paste(colSet,collapse=",")))
        flagMtx <- mtx[,colSet]    ;flagMtx[flagMtx==0] <- NA
        logObj$fLogStr("rstObj$checkLastDup()")
        rptStr <- rstObj$checkLastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
        logObj$fLogStr("rstObj$checkPastDup()")
        rptStr <- rstObj$checkPastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
    }

    colSet <- c("cStep.w1","cStep.w2","fStep.w1","fStep.w2","auxZW")
    {   colEH  <- c( 2, 2, 2, 2, 1 )   # event horizen
        logObj$fLogStr(sprintf("* colSet01 Event: %s(%s)",paste(colSet,collapse=","),paste(colEH,collapse=",")))
        flagMtx <- t(apply( mtx[,colSet] ,1 ,function(rData){rData>=colEH}))
        flagMtx[!flagMtx] <- NA
        logObj$fLogStr("rstObj$checkLastDup()")
        rptStr <- rstObj$checkLastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
        logObj$fLogStr("rstObj$checkPastDup()")
        rptStr <- rstObj$checkPastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))

        logObj$fLogStr(sprintf("* colSet01 Happen: %s",paste(colSet,collapse=",")))
        flagMtx <- mtx[,colSet]    ;flagMtx[flagMtx==0] <- NA
        logObj$fLogStr("rstObj$checkLastDup()")
        rptStr <- rstObj$checkLastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
        logObj$fLogStr("rstObj$checkPastDup()")
        rptStr <- rstObj$checkPastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
    }
}

mtxLstName <- "scoreMtxLst"
logObj$fLogStr(sprintf("< %s > ---------------------------------------------------",mtxLstName))
for( phIdx in rstObj$phName ){
    #   reb nbor spanM quoAll quoPtn zw remH0 remH1 cStep2 cStep3 w1CStep.cnt w1FStep.cnt w1CStep.matLen w1FStep.matLen
    logObj$fLogStr(sprintf("phName:%s",phIdx))
    mtx <- mtxLst[[mtxLstName]][[phIdx]]

    colSet <- c("nbor","spanM","quoAll","quoPtn","zw","remH0","remH1","cStep2","cStep3","w1CStep.cnt","w1FStep.cnt")
    {   colEH  <- c( 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 )   # event horizen
        logObj$fLogStr(sprintf("* colSet00 Event: %s(%s)",paste(colSet,collapse=","),paste(colEH,collapse=",")))
        flagMtx <- t(apply( mtx[,colSet] ,1 ,function(rData){rData>=colEH}))
        flagMtx[!flagMtx] <- NA
        logObj$fLogStr("rstObj$checkLastDup()")
        rptStr <- rstObj$checkLastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
        logObj$fLogStr("rstObj$checkPastDup()")
        rptStr <- rstObj$checkPastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))

        logObj$fLogStr(sprintf("* colSet00 Happen: %s",paste(colSet,collapse=",")))
        flagMtx <- mtx[,colSet]    ;flagMtx[flagMtx==0] <- NA
        logObj$fLogStr("rstObj$checkLastDup()")
        rptStr <- rstObj$checkLastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
        logObj$fLogStr("rstObj$checkPastDup()")
        rptStr <- rstObj$checkPastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
    }

}

mtxLstName <- "scoreMtx2Lst"
logObj$fLogStr(sprintf("< %s > ---------------------------------------------------",mtxLstName))
for( phIdx in rstObj$phName ){
    #   rebV rebC rebC2 rebL rebR rebL.cnt rebR.cnt inc.raw inc.cStep
    logObj$fLogStr(sprintf("phName:%s",phIdx))
    mtx <- mtxLst[[mtxLstName]][[phIdx]]

    colSet <- c("rebV","rebC","rebC2","rebL","rebR","inc.raw","inc.cStep")
    {   colEH  <- c( 3, 2, 2, 1, 1, 1, 1 )   # event horizen
        logObj$fLogStr(sprintf("* colSet00 Event: %s(%s)",paste(colSet,collapse=","),paste(colEH,collapse=",")))
        flagMtx <- t(apply( mtx[,colSet] ,1 ,function(rData){rData>=colEH}))
        flagMtx[!flagMtx] <- NA
        logObj$fLogStr("rstObj$checkLastDup()")
        rptStr <- rstObj$checkLastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
        logObj$fLogStr("rstObj$checkPastDup()")
        rptStr <- rstObj$checkPastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))

        logObj$fLogStr(sprintf("* colSet00 Happen: %s",paste(colSet,collapse=",")))
        flagMtx <- mtx[,colSet]    ;flagMtx[flagMtx==0] <- NA
        logObj$fLogStr("rstObj$checkLastDup()")
        rptStr <- rstObj$checkLastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
        logObj$fLogStr("rstObj$checkPastDup()")
        rptStr <- rstObj$checkPastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
    }

}

mtxLstName <- "scoreMtx3Lst"
logObj$fLogStr(sprintf("< %s > ---------------------------------------------------",mtxLstName))
for( phIdx in rstObj$phName ){
    #   rebPtn.1 rebPtn.n rebC.C1 rebC.F1 rebC.C2 rebC.F2
    logObj$fLogStr(sprintf("phName:%s",phIdx))
    mtx <- mtxLst[[mtxLstName]][[phIdx]]

    colSet <- c("rebPtn.1","rebC.C1","rebC.F1","rebC.C2","rebC.F2")  # rebPtn.n 는 무의미한 듯 하여 제외.
    {   colEH  <- c(     1,        2,        2,        2,        2)   # event horizen
        logObj$fLogStr(sprintf("* colSet00 Event: %s(%s)",paste(colSet,collapse=","),paste(colEH,collapse=",")))
        flagMtx <- t(apply( mtx[,colSet] ,1 ,function(rData){rData>=colEH}))
        flagMtx[!flagMtx] <- NA
        logObj$fLogStr("rstObj$checkLastDup()")
        rptStr <- rstObj$checkLastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
        logObj$fLogStr("rstObj$checkPastDup()")
        rptStr <- rstObj$checkPastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))

        logObj$fLogStr(sprintf("* colSet00 Happen: %s",paste(colSet,collapse=",")))
        flagMtx <- mtx[,colSet]    ;flagMtx[flagMtx==0] <- NA
        logObj$fLogStr("rstObj$checkLastDup()")
        rptStr <- rstObj$checkLastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
        logObj$fLogStr("rstObj$checkPastDup()")
        rptStr <- rstObj$checkPastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
    }

}

mtxLstName <- "scoreMtx4Lst"
logObj$fLogStr(sprintf("< %s > ---------------------------------------------------",mtxLstName))
for( phIdx in rstObj$phName ){
    #   incRaw3 incC3 incF3 incRaw2 incC2 incF2 (1,6) nextVal.r nextVal.c nextVal.f
    logObj$fLogStr(sprintf("phName:%s",phIdx))
    mtx <- mtxLst[[mtxLstName]][[phIdx]]

    colSet <- c("incRaw3","incC3","incF3","incRaw2","incC2","incF2","(1,6)","nextVal.r","nextVal.c","nextVal.f")
    {   colEH  <- c( 2,2,2,2,2, 2,2,2,2,2 )   # event horizen
        logObj$fLogStr(sprintf("* colSet00 Event: %s(%s)",paste(colSet,collapse=","),paste(colEH,collapse=",")))
        flagMtx <- t(apply( mtx[,colSet] ,1 ,function(rData){rData>=colEH}))
        flagMtx[!flagMtx] <- NA
        logObj$fLogStr("rstObj$checkLastDup()")
        rptStr <- rstObj$checkLastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
        logObj$fLogStr("rstObj$checkPastDup()")
        rptStr <- rstObj$checkPastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))

        logObj$fLogStr(sprintf("* colSet00 Happen: %s",paste(colSet,collapse=",")))
        flagMtx <- mtx[,colSet]    ;flagMtx[flagMtx==0] <- NA
        logObj$fLogStr("rstObj$checkLastDup()")
        rptStr <- rstObj$checkLastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
        logObj$fLogStr("rstObj$checkPastDup()")
        rptStr <- rstObj$checkPastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
    }

    colSet <- c("incRaw3","incC3","incF3","incRaw2","incC2","incF2")
    {   colEH  <- c( 2,2,2 ,2,2,2 )   # event horizen
        logObj$fLogStr(sprintf("* colSet00 Event: %s(%s)",paste(colSet,collapse=","),paste(colEH,collapse=",")))
        flagMtx <- t(apply( mtx[,colSet] ,1 ,function(rData){rData>=colEH}))
        flagMtx[!flagMtx] <- NA
        logObj$fLogStr("rstObj$checkLastDup()")
        rptStr <- rstObj$checkLastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
        logObj$fLogStr("rstObj$checkPastDup()")
        rptStr <- rstObj$checkPastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))

        logObj$fLogStr(sprintf("* colSet00 Happen: %s",paste(colSet,collapse=",")))
        flagMtx <- mtx[,colSet]    ;flagMtx[flagMtx==0] <- NA
        logObj$fLogStr("rstObj$checkLastDup()")
        rptStr <- rstObj$checkLastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
        logObj$fLogStr("rstObj$checkPastDup()")
        rptStr <- rstObj$checkPastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
    }

    colSet <- c("(1,6)","nextVal.r","nextVal.c","nextVal.f")
    {   colEH  <- c( 2,2,2,2 )   # event horizen
        logObj$fLogStr(sprintf("* colSet00 Event: %s(%s)",paste(colSet,collapse=","),paste(colEH,collapse=",")))
        flagMtx <- t(apply( mtx[,colSet] ,1 ,function(rData){rData>=colEH}))
        flagMtx[!flagMtx] <- NA
        logObj$fLogStr("rstObj$checkLastDup()")
        rptStr <- rstObj$checkLastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
        logObj$fLogStr("rstObj$checkPastDup()")
        rptStr <- rstObj$checkPastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))

        logObj$fLogStr(sprintf("* colSet00 Happen: %s",paste(colSet,collapse=",")))
        flagMtx <- mtx[,colSet]    ;flagMtx[flagMtx==0] <- NA
        logObj$fLogStr("rstObj$checkLastDup()")
        rptStr <- rstObj$checkLastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
        logObj$fLogStr("rstObj$checkPastDup()")
        rptStr <- rstObj$checkPastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
    }

}

mtxLstName <- "cStepValMtxLst"
logObj$fLogStr(sprintf("< %s > ---------------------------------------------------",mtxLstName))
for( phIdx in rstObj$phName ){
    #   c31 c32 c33 c34 c21 c22 c23 c24 c25 max2 min2
    logObj$fLogStr(sprintf("phName:%s",phIdx))
    mtx <- mtxLst[[mtxLstName]][[phIdx]]

    colSet <- c("c31","c32","c33","c34" ,"c21","c22","c23","c24","c25", "max2","min2" )
    {
        logObj$fLogStr(sprintf("* colSet00 Happen: %s",paste(colSet,collapse=",")))
        flagMtx <- mtx[,colSet]    ;flagMtx[flagMtx==0] <- NA
        logObj$fLogStr("rstObj$checkLastDup()")
        rptStr <- rstObj$checkLastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
        logObj$fLogStr("rstObj$checkPastDup()")
        rptStr <- rstObj$checkPastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
    }

    colSet <- c("c21","c22","c23","c24","c25" )
    {
        logObj$fLogStr(sprintf("* colSet00 Happen: %s",paste(colSet,collapse=",")))
        flagMtx <- mtx[,colSet]    ;flagMtx[flagMtx==0] <- NA
        logObj$fLogStr("rstObj$checkLastDup()")
        rptStr <- rstObj$checkLastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
        logObj$fLogStr("rstObj$checkPastDup()")
        rptStr <- rstObj$checkPastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
    }

}


#- [getMtx.byFilter()] ------------------------------------------------------------------------------
logObj <- k.getFlogObj( "./report/testRst_byFilter.txt" )        ;logObj$fLogStr("start", pTime=T ,pAppend=F )
mtxLst <- rstObj$getMtx.byFilter()	# mtxLst[["cntMtxLst"]]$raw

mtxLstName <- "cntMtxLst"
fltNames <- c("raw","rem","cStep","fStep","raw.w1","cStep.w1","cStep.w2","fStep.w1","fStep.w2")
colEH    <- c(    2,    3,      2,      2,       1,         2,         2,         2,        2 )
names(colEH) <- fltNames
logObj$fLogStr(sprintf("< %s > ---------------------------------------------------",mtxLstName))
for( fIdx in fltNames ){

    logObj$fLogStr(sprintf("filter:%s",fIdx))
    mtx <- mtxLst[[mtxLstName]][[fIdx]]

    {   logObj$fLogStr(sprintf("* Event (threshold:%d)",colEH[fIdx]))
        flagMtx <- t(apply( mtx ,1 ,function(rData){rData>=colEH[fIdx]}))
        flagMtx[!flagMtx] <- NA
        logObj$fLogStr("rstObj$checkLastDup()")
        rptStr <- rstObj$checkLastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
        logObj$fLogStr("rstObj$checkPastDup()")
        rptStr <- rstObj$checkPastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))

        logObj$fLogStr(sprintf("* Happen  "))
        flagMtx[flagMtx==0] <- NA
        logObj$fLogStr("rstObj$checkLastDup()")
        rptStr <- rstObj$checkLastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
        logObj$fLogStr("rstObj$checkPastDup()")
        rptStr <- rstObj$checkPastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
    }

}
fIdx <- "rem"  # rebV 가 2인 경우들에 대한 별도 체크.
logObj$fLogStr(sprintf("filter:%s 별도 추가체크 ",fIdx))
mtx <- mtxLst[[mtxLstName]][[fIdx]]
{   logObj$fLogStr(sprintf("* Event (threshold:%d)", 2 ))
    flagMtx <- t(apply( mtx ,1 ,function(rData){rData==2 }))
    flagMtx[!flagMtx] <- NA
    logObj$fLogStr("rstObj$checkLastDup()")
    rptStr <- rstObj$checkLastDup( flagMtx ,show=F )
    logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
    logObj$fLogStr("rstObj$checkPastDup()")
    rptStr <- rstObj$checkPastDup( flagMtx ,show=F )
    logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
}


mtxLstName <- "scoreMtxLst"
fltNames <- c("reb","nbor","spanM","quoAll","quoPtn","zw","remH0","remH1","cStep2","cStep3","w1CStep.cnt","w1FStep.cnt")
colEH    <- c(    1,     2,      2,       1,       1,   1,      1,      1,       1,      1,             2,            2)
names(colEH) <- fltNames
logObj$fLogStr(sprintf("< %s > ---------------------------------------------------",mtxLstName))
for( fIdx in fltNames ){

    logObj$fLogStr(sprintf("filter:%s",fIdx))
    mtx <- mtxLst[[mtxLstName]][[fIdx]]

    {   logObj$fLogStr(sprintf("* Event (threshold:%d)",colEH[fIdx]))
        flagMtx <- t(apply( mtx ,1 ,function(rData){rData>=colEH[fIdx]}))
        flagMtx[!flagMtx] <- NA
        logObj$fLogStr("rstObj$checkLastDup()")
        rptStr <- rstObj$checkLastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
        logObj$fLogStr("rstObj$checkPastDup()")
        rptStr <- rstObj$checkPastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))

        logObj$fLogStr(sprintf("* Happen  "))
        flagMtx[flagMtx==0] <- NA
        logObj$fLogStr("rstObj$checkLastDup()")
        rptStr <- rstObj$checkLastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
        logObj$fLogStr("rstObj$checkPastDup()")
        rptStr <- rstObj$checkPastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
    }

}

mtxLstName <- "scoreMtx2Lst"
fltNames <- c( "rebV","rebC","rebC2","rebL","rebR","inc.raw","inc.cStep")
colEH    <- c(      3,     2,      2,     1,     1,        2,          2)
names(colEH) <- fltNames
logObj$fLogStr(sprintf("< %s > ---------------------------------------------------",mtxLstName))
for( fIdx in fltNames ){
    #   basic nextZW nextQuo10 nextBin nextRebNum nextCStepBin nextFStepBin 
    #   nextColVal_1 nextColVal_2 nextColVal_3 nextColVal_4 nextColVal_5 nextColVal_6

    logObj$fLogStr(sprintf("filter:%s",fIdx))
    mtx <- mtxLst[[mtxLstName]][[fIdx]]

    {   logObj$fLogStr(sprintf("* Event (threshold:%d)",colEH[fIdx]))
        flagMtx <- t(apply( mtx ,1 ,function(rData){rData>=colEH[fIdx]}))
        flagMtx[!flagMtx] <- NA
        logObj$fLogStr("rstObj$checkLastDup()")
        rptStr <- rstObj$checkLastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
        logObj$fLogStr("rstObj$checkPastDup()")
        rptStr <- rstObj$checkPastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))

        logObj$fLogStr(sprintf("* Happen  "))
        flagMtx[flagMtx==0] <- NA
        logObj$fLogStr("rstObj$checkLastDup()")
        rptStr <- rstObj$checkLastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
        logObj$fLogStr("rstObj$checkPastDup()")
        rptStr <- rstObj$checkPastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
    }
}
fIdx <- "rebV"  # rebV 가 2인 경우들에 대한 별도 체크.
logObj$fLogStr(sprintf("filter:%s 별도 추가체크 ",fIdx))
mtx <- mtxLst[[mtxLstName]][[fIdx]]
{   logObj$fLogStr(sprintf("* Event (threshold:%d)", 2 ))
    flagMtx <- t(apply( mtx ,1 ,function(rData){rData==2 }))
    flagMtx[!flagMtx] <- NA
    logObj$fLogStr("rstObj$checkLastDup()")
    rptStr <- rstObj$checkLastDup( flagMtx ,show=F )
    logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
    logObj$fLogStr("rstObj$checkPastDup()")
    rptStr <- rstObj$checkPastDup( flagMtx ,show=F )
    logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
}


mtxLstName <- "scoreMtx3Lst"
fltNames <- c( "rebPtn.1", "rebPtn.n", "rebC.C1", "rebC.F1", "rebC.C2", "rebC.F2" )
colEH    <- c(          1,          1,         2,         2,         2,         2 )
names(colEH) <- fltNames
logObj$fLogStr(sprintf("< %s > ---------------------------------------------------",mtxLstName))
for( fIdx in fltNames ){

    logObj$fLogStr(sprintf("filter:%s",fIdx))
    mtx <- mtxLst[[mtxLstName]][[fIdx]]

    {   logObj$fLogStr(sprintf("* Event (threshold:%d)",colEH[fIdx]))
        flagMtx <- t(apply( mtx ,1 ,function(rData){rData>=colEH[fIdx]}))
        flagMtx[!flagMtx] <- NA
        logObj$fLogStr("rstObj$checkLastDup()")
        rptStr <- rstObj$checkLastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
        logObj$fLogStr("rstObj$checkPastDup()")
        rptStr <- rstObj$checkPastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))

        logObj$fLogStr(sprintf("* Happen  "))
        flagMtx[flagMtx==0] <- NA
        logObj$fLogStr("rstObj$checkLastDup()")
        rptStr <- rstObj$checkLastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
        logObj$fLogStr("rstObj$checkPastDup()")
        rptStr <- rstObj$checkPastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
    }

}

mtxLstName <- "scoreMtx4Lst"
fltNames <- c("incRaw3","incC3","incF3","incRaw2","incC2","incF2","(1,6)","nextVal.r","nextVal.c","nextVal.f")
colEH    <- c(        1,      1,      1,        1,      1,      1,     2,           2,          2,          2)
names(colEH) <- fltNames
logObj$fLogStr(sprintf("< %s > ---------------------------------------------------",mtxLstName))
for( fIdx in fltNames ){

    logObj$fLogStr(sprintf("filter:%s",fIdx))
    mtx <- mtxLst[[mtxLstName]][[fIdx]]

    {   logObj$fLogStr(sprintf("* Event (threshold:%d)",colEH[fIdx]))
        flagMtx <- t(apply( mtx ,1 ,function(rData){rData>=colEH[fIdx]}))
        flagMtx[!flagMtx] <- NA
        logObj$fLogStr("rstObj$checkLastDup()")
        rptStr <- rstObj$checkLastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
        logObj$fLogStr("rstObj$checkPastDup()")
        rptStr <- rstObj$checkPastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))

        logObj$fLogStr(sprintf("* Happen  "))
        flagMtx[flagMtx==0] <- NA
        logObj$fLogStr("rstObj$checkLastDup()")
        rptStr <- rstObj$checkLastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
        logObj$fLogStr("rstObj$checkPastDup()")
        rptStr <- rstObj$checkPastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
    }

}


mtxLstName <- "cStepValMtxLst"
fltNames <- c("c31","c32","c33","c34","c21","c22","c23","c24","c25","max2","min2")
logObj$fLogStr(sprintf("< %s > ---------------------------------------------------",mtxLstName))
for( fIdx in fltNames ){
    #   basic nextZW nextQuo10 nextBin nextRebNum nextCStepBin nextFStepBin 
    #   nextColVal_1 nextColVal_2 nextColVal_3 nextColVal_4 nextColVal_5 nextColVal_6

    logObj$fLogStr(sprintf("filter:%s",fIdx))
    mtx <- mtxLst[[mtxLstName]][[fIdx]]

    {   logObj$fLogStr(sprintf("* Event : skip...",colEH[fIdx]))

        logObj$fLogStr(sprintf("* Happen  "))
        flagMtx[flagMtx==0] <- NA
        logObj$fLogStr("rstObj$checkLastDup()")
        rptStr <- rstObj$checkLastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
        logObj$fLogStr("rstObj$checkPastDup()")
        rptStr <- rstObj$checkPastDup( flagMtx ,show=F )
        logObj$fLogStr(sprintf("%s\n",paste(rptStr,collapse="\n")))
    }

}

