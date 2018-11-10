
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
    hCntMtx <- sapply( rptLst[[zhIdx]]$ccObjLst ,function(ccObj){ ccObj$cntMtx } )
    hCntMtx <- t(hCntMtx)       ;colnames(hCntMtx) <- name.cntMtx
    hAuxCntMtx <- sapply( rptLst[[zhIdx]]$ccObjLst ,function(ccObj){ ccObj$auxCntMtx } )
    hAuxCntMtx <- t(hAuxCntMtx) ;colnames( hAuxCntMtx ) <- name.auxCntMtx
    # log.cntMtx$fLogMtxLst( list(hAuxCntMtx,hCntMtx) ,pSep=" | " ,pIndent="   ")
    hCntMtx <- cbind( hCntMtx ,hAuxCntMtx )
    log.cntMtx$fLogMtx(hCntMtx,pIndent="  ")

    hScoreMtx <- sapply( rptLst[[zhIdx]]$ccObjLst ,function(ccObj){ ccObj$cccObj$scoreMtx })
    hScoreMtx <- t(hScoreMtx)   ;colnames( hScoreMtx ) <- name.cccObj.scoreMtx
    log.scoreMtx$fLogMtx(hScoreMtx)

    hCStepValMtx <- sapply( rptLst[[zhIdx]]$ccObjLst ,function(ccObj){ ccObj$cccObj$cStepValMtx })
    hCStepValMtx <- t(hCStepValMtx) ;colnames( hCStepValMtx ) <- name.cccObj.cStepValMtx 
    log.cStepValMtx$fLogMtx(hCStepValMtx)

    mtxInfoLst[[zhIdx]] <- list( cntMtx=hCntMtx ,scoreMtx=hScoreMtx ,cStepValMtx=hCStepValMtx )

} # for(zhIdx) -- mtxInfoLst

for( zhIdx in zhName ){
    mtxObj <- mtxInfoLst[[zhIdx]]
    # log.cntMtx$fLogStr(sprintf("<%s> ---------------------------------",zhIdx))
    # log.scoreMtx$fLogStr(sprintf("<%s> ---------------------------------",zhIdx))
    # log.cStepValMtx$fLogStr(sprintf("<%s> ---------------------------------",zhIdx))


} # for(zhIdx)

mtx <- mtxObj$cntMtx

pMtx <- mtx ;pMtx.evt <- mtx > 1    ;pEvtName="Evt"

anaMtx <- function( pMtx ,pMtx.evt=NULL ,pEvtName="evt" ){

    cName <- if( is.null(pMtx.evt) ) c("hpn","sum") else c("hpn","sum",pEvtName)
    cntMtx <- matrix( 0 ,ncol=length(cName) ,nrow=nrow(pMtx) )
    rownames(cntMtx) <- rownames(pMtx)      ;colnames(cntMtx)<-cName

    # row 방향 분석
        # count

        # rebCnt 1, 3, 5, 전체 (1는 사실상 연속.)

    # col 방향 분석
        # count

        # rebCnt 1, 3, 5, 전체 (1는 사실상 연속.)


}

anaMtx.cnt <- function( pMtx ,pMtx.evt=NULL ,pEvtName="evt" ,pReb=c(2,4,6) ,pRow=T ){
    #   pRow : T 이면 row 방향에 대한 count, F이면 col방향으로의 count
    workMtx <- if( pRow ) pMtx else t(pMtx)
    rObj <- list(   hpn = apply( workMtx ,1 ,function(row){ sum(row>0) } )
                    ,sum = apply( workMtx ,1 ,sum )
                )
    workMtx.evt <- NULL
    if( !is.null(pMtx.evt) ){
        workMtx.evt <- if( pRow ) pMtx.evt else t(pMtx.evt)
        rObj[[pEvtName]] <- apply( workMtx.evt ,1 ,sum )

        # reb count for event
        rebSpan <- c( pReb[pReb<ncol(workMtx)] ,ncol(workMtx) )
        valSpanLst <- list()
        for( rebIdx in rebSpan ){
            # search index Span 생성.
            # span 내에서 min,max값 검색.
            valSpanLst[[sprintf("reb%02d",rebIdx)]] <- c( 0, 3 )    # min max
        }

    } # if( !is.null(pMtx.evt) )

    return( rObj )
}

# 분석
# 레포트
