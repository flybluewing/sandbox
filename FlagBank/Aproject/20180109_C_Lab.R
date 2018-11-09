
rptLst <- list()
for( hIdx in 826:831 ){
    fileName <- sprintf("./save/stdZoidFltRst/z%d.save",hIdx)
    cat( paste(fileName,"\n",collapse="") )
    myObj <- load(fileName)    # rptObj
    rptLst[[sprintf("z%d",hIdx)]] <- rptObj
}


hSpan <- sapply( rptLst ,function(rpt){rpt$h})
zhName <- attributes(rptLst)$names
phName <- attributes(rptLst[[1]]$ccObjLst)$names
name.cntMtx <- colnames(rptLst[[1]]$ccObjLst[[1]]$cntMtx)
name.auxCntMtx <- colnames(rptLst[[1]]$ccObjLst[[1]]$auxCntMtx)
name.cccObj.scoreMtx <- colnames(rptLst[[1]]$ccObjLst[[1]]$cccObj$scoreMtx)
name.cccObj.cStepValMtx <- colnames(rptLst[[1]]$ccObjLst[[1]]$cccObj$cStepValMtx)

# 매트릭스 작성
hpnCntMtx <- matrix( 0 ,nrow=length(phName) ,ncol=length(zhName) )
colnames(hpnCntMtx)<-zhName        ;rownames(hpnCntMtx)<-phName

log.cntMtx      <- k.getFlogObj( "./report/cntMtx.txt" )        ;log.cntMtx$fLogStr("start", pTime=T ,pAppend=F )
log.scoreMtx    <- k.getFlogObj( "./report/scoreMtx.txt" )      ;log.scoreMtx$fLogStr("start", pTime=T ,pAppend=F )
log.cStepValMtx <- k.getFlogObj( "./report/cStepValMtx.txt" )   ;log.cStepValMtx$fLogStr("start", pTime=T ,pAppend=F )

mtxInfoLst <- list()
for( zhIdx in zhName ){

    log.cntMtx$fLogStr(sprintf("<%s> ---------------------------------",zhIdx))
    hCntMtx <- sapply( rptLst[[zhIdx]]$ccObjLst ,function(ccObj){ ccObj$cntMtx } )
    hCntMtx <- t(hCntMtx)       ;colnames(hCntMtx) <- name.cntMtx
    hAuxCntMtx <- sapply( rptLst[[zhIdx]]$ccObjLst ,function(ccObj){ ccObj$auxCntMtx } )
    hAuxCntMtx <- t(hAuxCntMtx) ;colnames( hAuxCntMtx ) <- name.auxCntMtx
    # log.cntMtx$fLogMtxLst( list(hAuxCntMtx,hCntMtx) ,pSep=" | " ,pIndent="   ")
    hCntMtx <- cbind( hCntMtx ,hAuxCntMtx )
    log.cntMtx$fLogMtx(hCntMtx,pIndent="  ")

    log.scoreMtx$fLogStr(sprintf("<%s> ---------------------------------",zhIdx))
    hScoreMtx <- sapply( rptLst[[zhIdx]]$ccObjLst ,function(ccObj){ ccObj$cccObj$scoreMtx })
    hScoreMtx <- t(hScoreMtx)   ;colnames( hScoreMtx ) <- name.cccObj.scoreMtx
    log.scoreMtx$fLogMtx(hScoreMtx)

    


    log.cStepValMtx$fLogStr(sprintf("<%s> ---------------------------------",zhIdx))
    hCStepValMtx <- sapply( rptLst[[zhIdx]]$ccObjLst ,function(ccObj){ ccObj$cccObj$cStepValMtx })
    hCStepValMtx <- t(hCStepValMtx) ;colnames( hCStepValMtx ) <- name.cccObj.cStepValMtx 
    log.cStepValMtx$fLogMtx(hCStepValMtx)

    mtxInfoLst[[zhIdx]] <- list( cntMtx=hCntMtx ,scoreMtx=hScoreMtx ,cStepValMtx=hCStepValMtx )
}

fLogMtx <- function( mtx ,pIndent="" ){
    # pIndent
    dfStr <- capture.output( mtx )
    cat(sprintf("%s%s\n",pIndent,dfStr))
}

# 분석
# 레포트
