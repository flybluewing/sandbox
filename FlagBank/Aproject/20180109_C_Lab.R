lastH <- 838
# fullRstSpan, goldRstSpan ----------------------------------------------------------
fullRstSpan <- 826:lastH
loadObjNm <- load( sprintf("Obj_allIdxLstZ%d.save",lastH) )
stdFiltedCnt <- allIdxLst$stdFiltedCnt[as.character(809:lastH)]
goldRstSpan <- as.integer(names( stdFiltedCnt[stdFiltedCnt<=1] ))
stdFiltedCnt <- allIdxLst$stdFiltedCnt[as.character(819:lastH)]
grp2RstSpan <- as.integer(names( stdFiltedCnt[stdFiltedCnt==2] ))
rm(allIdxLst)

lab.getMtxLst <- function( hSpan ){

    # rptLst ----------------------------------------------------------------------------
    rptLst <- list()
    for( hIdx in hSpan ){
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
    name.cccObj.cStepValMtx <- colnames(rptLst[[1]]$ccObjLst[[1]]$cccObj$cStepValMtx)

    # mtxInfoLst ------------------------------------------------------------------------
    hpnCntMtx <- matrix( 0 ,nrow=length(phName) ,ncol=length(zhName) )
    colnames(hpnCntMtx)<-zhName        ;rownames(hpnCntMtx)<-phName

    log.cntMtx      <- k.getFlogObj( "./report/cntMtx.txt" )        ;log.cntMtx$fLogStr("start", pTime=T ,pAppend=F )
    log.scoreMtx    <- k.getFlogObj( "./report/scoreMtx.txt" )      ;log.scoreMtx$fLogStr("start", pTime=T ,pAppend=F )
    log.cStepValMtx <- k.getFlogObj( "./report/cStepValMtx.txt" )   ;log.cStepValMtx$fLogStr("start", pTime=T ,pAppend=F )

    mtxInfoLst <- list()
    for( zhIdx in zhName ){

        hCntMtx <- sapply( rptLst[[zhIdx]]$ccObjLst ,function(ccObj){ ccObj$cntMtx } )
        hCntMtx <- t(hCntMtx)       ;colnames(hCntMtx) <- name.cntMtx
        hAuxCntMtx <- sapply( rptLst[[zhIdx]]$ccObjLst ,function(ccObj){ ccObj$auxCntMtx } )
        hAuxCntMtx <- t(hAuxCntMtx) ;colnames( hAuxCntMtx ) <- name.auxCntMtx

        hCntMtx <- cbind( hCntMtx ,hAuxCntMtx )
        log.cntMtx$fLogStr( sprintf("<%s> %d ---------------------------------",zhIdx,rptLst[[zhIdx]]$stdFiltedCnt) )
        log.cntMtx$fLogMtx(hCntMtx,pIndent="  ")

        hScoreMtx <- sapply( rptLst[[zhIdx]]$ccObjLst ,function(ccObj){ ccObj$cccObj$scoreMtx })
        hScoreMtx <- t(hScoreMtx)   ;colnames( hScoreMtx ) <- name.cccObj.scoreMtx
        log.scoreMtx$fLogStr(sprintf("<%s> %d ---------------------------------",zhIdx,rptLst[[zhIdx]]$stdFiltedCnt))
        log.scoreMtx$fLogMtx(hScoreMtx)

        hCStepValMtx <- sapply( rptLst[[zhIdx]]$ccObjLst ,function(ccObj){ ccObj$cccObj$cStepValMtx })
        hCStepValMtx <- t(hCStepValMtx) ;colnames( hCStepValMtx ) <- name.cccObj.cStepValMtx 
        log.cStepValMtx$fLogStr(sprintf("<%s> %d ---------------------------------",zhIdx,rptLst[[zhIdx]]$stdFiltedCnt))
        log.cStepValMtx$fLogMtx(hCStepValMtx)

        mtxInfoLst[[zhIdx]] <- list( cntMtx=hCntMtx ,scoreMtx=hScoreMtx ,cStepValMtx=hCStepValMtx )

    } # for(zhIdx) -- mtxInfoLst

    rObj <- list( mtxInfoLst=mtxInfoLst ,hSpan=hSpan ,zhName=zhName ,phName=phName
                    ,name.cntMtx=name.cntMtx
                    ,name.auxCntMtx=name.auxCntMtx
                    ,name.cccObj.scoreMtx=name.cccObj.scoreMtx
                    ,name.cccObj.cStepValMtx=name.cccObj.cStepValMtx
                 )

    return( rObj )

}   # lab.getMtxLst( )

rstObj <- lab.getMtxLst( goldRstSpan )
#   rstObj <- lab.getMtxLst( grp2RstSpan )
#   rstObj <- lab.getMtxLst( fullRstSpan )
if( TRUE ){ # cStep, fStep 에서 w1,w2 포함 제외
    for( nIdx in rstObj$zhName ){
        cntMtx <- rstObj$mtxInfoLst[[nIdx]]$cntMtx
        scoreMtx <- rstObj$mtxInfoLst[[nIdx]]$scoreMtx
        rstObj$mtxInfoLst[[nIdx]]$cntMtx[,"cStep"] <- cntMtx[,"cStep"] - cntMtx[,"cStep.w1"] - cntMtx[,"cStep.w2"]
        rstObj$mtxInfoLst[[nIdx]]$cntMtx[,"fStep"] <- cntMtx[,"fStep"] - cntMtx[,"fStep.w1"] - cntMtx[,"fStep.w2"]
        rstObj$mtxInfoLst[[nIdx]]$cntMtx[,"cStep.w1"] <- cntMtx[,"cStep.w1"] - scoreMtx[,"w1CStep.cnt"]
        rstObj$mtxInfoLst[[nIdx]]$cntMtx[,"fStep.w1"] <- cntMtx[,"fStep.w1"] - scoreMtx[,"w1FStep.cnt"]
    }
}


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


# -----------------------------------------------------------------------------------------------------------------------------------
ff0.phName <- c(    "basic","nextZW","nextQuo10" ,"nextBin" ,"nextRebNum" ,"nextCStepBin" ,"nextFStepBin"
                    ,"nextColVal_1","nextColVal_2","nextColVal_3","nextColVal_4","nextColVal_5","nextColVal_6"
                )

ff0.filtCntMtx.1ph <- function( phName ,pCntMtx ){
    #   phName="basic"  ;pCntMtx=ccObj$cntMtx
    #   1개 필터차원 단위의 생존평가.
    getEvtThldMtx <- function( phName ){
                        rMtx <- NULL
                        rMtx <- rbind( rMtx ,c( NA,3, NA,5, NA,1 ) )    # "basic"
                        rMtx <- rbind( rMtx ,c( NA,3, NA,5, NA,1 ) )    # "nextZW"
                        rMtx <- rbind( rMtx ,c( NA,3, NA,5, NA,1 ) )    # "nextQuo10"
                        rMtx <- rbind( rMtx ,c( NA,3, NA,5, NA,1 ) )    # "nextBin"
                        rMtx <- rbind( rMtx ,c( NA,3, NA,5, NA,1 ) )    # "nextRebNum"
                        rMtx <- rbind( rMtx ,c( NA,3, NA,5, NA,1 ) )    # "nextCStepBin"
                        rMtx <- rbind( rMtx ,c( NA,3, NA,5, NA,1 ) )    # "nextFStepBin"
                        rMtx <- rbind( rMtx ,c( NA,3, NA,5, NA,2 ) )    # "nextColVal_1"
                        rMtx <- rbind( rMtx ,c( NA,3, NA,5, NA,2 ) )    # "nextColVal_2"
                        rMtx <- rbind( rMtx ,c( NA,3, NA,5, NA,2 ) )    # "nextColVal_3"
                        rMtx <- rbind( rMtx ,c( NA,3, NA,5, NA,2 ) )    # "nextColVal_4"
                        rMtx <- rbind( rMtx ,c( NA,3, NA,5, NA,2 ) )    # "nextColVal_5"
                        rMtx <- rbind( rMtx ,c( NA,3, NA,5, NA,2 ) )    # "nextColVal_6"
                        rownames(rMtx) <- ff0.phName
                        colnames(rMtx) <- c("hpnMin","hpnMax","sumMin","sumMax""evtMin","evtMax")
                        return( rMtx[phName,] )
    } # getEvtThldMtx()

    cntMtx <- pCntMtx

    cStep.score <-  cntMtx[,"cStep"] - cntMtx[,"cStep.w1"] - cntMtx[,"cStep.w2"]
    cntMtx[,"cStep"] <- ifelse( cStep.score>0 ,cStep.score ,0 )
    fStep.score <- cntMtx[,"fStep"] - cntMtx[,"fStep.w1"] - cntMtx[,"fStep.w2"]
    cntMtx[,"fStep"] <- ifelse( fStep.score>0 ,fStep.score ,0 )

    thld <- getEvtThldMtx( phName )

    # evtMtx
    cntMtx.evt <- cntMtx > 1  ;cntMtx.evt[,"rem"] <- cntMtx[,"rem"] > 2
    flagMtx <- apply( cntMtx.evt ,1 ,function( evt ){ eSum <- sum(evt)
                        evtFlag <- FALSE    ;cutFlag <- FALSE
                        if( !is.na(thld["evtMin"]) ){
                            if( eSum< thld["evtMin"] ){ evtFlag<-cutFlag<-TRUE }
                            if( eSum==thld["evtMin"] ){ evtFlag<-TRUE }
                        }
                        if( !is.na(thld["evtMax"]) ){
                            if( eSum> thld["evtMax"] ){ evtFlag<-cutFlag<-TRUE }
                            if( eSum==thld["evtMax"] ){ evtFlag<-TRUE }
                        }
                        return( c(evtFlag,cutFlag,eSum) )
                } )
    flagMtx <- t(flagMtx)   ;colnames(flagMtx)<-c("evt","cut","sumVal")


    evtCut <- ifelse( )
    evtEvt

    test <- c( -1, 0, 1, 2 )
    ifelse( thld[] )

    rObj <- list()

    return( rObj )

} # ff0.filtCntMtx.1ph( )


log.cntMtx$fLogMtx( workMtx.evt )

# 분석
# 레포트
