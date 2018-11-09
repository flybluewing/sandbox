
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

mtxInfoLst <- list()
for( zhIdx in zhName ){

    hCntMtx <- sapply( rptLst[[zhIdx]]$ccObjLst ,function(ccObj){ ccObj$cntMtx } )
    hCntMtx <- t(hCntMtx)       ;colnames(hCntMtx) <- name.cntMtx
    hAuxCntMtx <- sapply( rptLst[[zhIdx]]$ccObjLst ,function(ccObj){ ccObj$auxCntMtx } )
    hAuxCntMtx <- t(hAuxCntMtx) ;colnames( hAuxCntMtx ) <- name.auxCntMtx
    hCntMtx <- cbind( hCntMtx ,hAuxCntMtx )

    hScoreMtx <- sapply( rptLst[[zhIdx]]$ccObjLst ,function(ccObj){ ccObj$cccObj$scoreMtx })
    hScoreMtx <- t(hScoreMtx)   ;colnames( hScoreMtx ) <- name.cccObj.scoreMtx

    hCStepValMtx <- sapply( rptLst[[zhIdx]]$ccObjLst ,function(ccObj){ ccObj$cccObj$cStepValMtx })
    hCStepValMtx <- t(hCStepValMtx) ;colnames( hCStepValMtx ) <- name.cccObj.cStepValMtx 

    mtxInfoLst[[zhIdx]] <- list( cntMtx=hCntMtx ,scoreMtx=hScoreMtx ,cStepValMtx=hCStepValMtx )
}

# 분석
# 레포트
