
rptLst <- list()
for( hIdx in 826:832 ){
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


for( zhIdx in zhName ){
    mtxObj <- mtxInfoLst[[zhIdx]]
    # log.cntMtx$fLogStr(sprintf("<%s> ---------------------------------",zhIdx))
    # log.scoreMtx$fLogStr(sprintf("<%s> ---------------------------------",zhIdx))
    # log.cStepValMtx$fLogStr(sprintf("<%s> ---------------------------------",zhIdx))

} # for(zhIdx)







u1.anaMtx.cnt <- function( pMtx ,pMtx.evt=NULL ,pEvtName="evt" ,pReb=c(2,4,6) ,pRow=T ){
    # test : pMtx.evt=pMtx>0 ;pEvtName="evt" ;pReb=c(2,4,6) ;pRow=T
    #       rowObj <- u1.anaMtx.cnt( pMtx=pMtx ,pMtx.evt=pMtx.evt ,pEvtName=pEvtName ,pRow=T )
    #       colObj <- u1.anaMtx.cnt( pMtx=pMtx ,pMtx.evt=pMtx.evt ,pEvtName=pEvtName ,pRow=F )

    #   pRow : T 이면 row 방향에 대한 count, F이면 col방향으로의 count
    workMtx <- if( pRow ) pMtx else t(pMtx)

    rObj <- list( direction=ifelse(pRow,"row","col") )
    anaMtx <- rbind( apply( workMtx ,1 ,function(row){ sum(row>0) } )
                    ,apply( workMtx ,1 ,sum )
                )
    rownames( anaMtx ) <- c("hpn","sum")

    evtRebPtnMtx <- NULL
    if( !is.null(pMtx.evt) ){
        workMtx.evt <- if( pRow ) pMtx.evt else t(pMtx.evt)
        anaMtx <- rbind( anaMtx ,apply( workMtx.evt ,1 ,sum ) )
        rownames( anaMtx ) <- c("hpn","sum",pEvtName)

        # reb count for event
        rebSpan <- c( pReb[pReb<ncol(workMtx)] ,ncol(workMtx) )
        rebMatchNumLst <- list()
        for( rebIdx in rebSpan ){
            # search index Span 생성.
            searchSpanLst <- list()
            for( sIdx in 1:(ncol(workMtx)-rebIdx+1) ){
                searchSpanLst[[length(searchSpanLst)+1]] <- sIdx:(sIdx+rebIdx-1)
            }

            # span 내에서 min,max값 검색.
            matchCnt <- integer(0)
            spanLen <- length(searchSpanLst[[1]])
            for( lIdx in 1:length(searchSpanLst) ){
                idxSpan <- searchSpanLst[[lIdx]]
                for( baseIdx in 1:(spanLen-1) ){
                    for( tIdx in (baseIdx+1):spanLen ){
                        matchSum <- sum( workMtx.evt[,idxSpan[baseIdx]] & workMtx.evt[,idxSpan[tIdx]] )
                        matchCnt <- c( matchCnt ,matchSum )
                    }
                }
            }
            rebMatchNumLst[[sprintf("reb%02d",rebIdx)]] <- c( min(matchCnt), max(matchCnt) )    # min max
        }
        evtRebPtnMtx <- do.call( rbind ,rebMatchNumLst )
        colnames(evtRebPtnMtx) <- c("min","max")
    } # if( !is.null(pMtx.evt) )

    rObj$anaMtx <- if( pRow ) t(anaMtx) else anaMtx
    rObj$evtRebPtnMtx <- evtRebPtnMtx

    return( rObj )
} # u1.anaMtx.cnt()


log.cntMtx$fLogMtx( workMtx.evt )

# 분석
# 레포트
