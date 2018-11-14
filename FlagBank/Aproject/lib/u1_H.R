# u1_H.R unit model zero
#   주로 최종 결과에 대한 필터링 함수 관리용.


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


