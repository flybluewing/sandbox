



Bprll.resultLst <- function( resultLst ,tgt.scMtx ){

    mtx <- matrix( " ", nrow=length(resultLst) ,ncol=length(tgt.scMtx) 
            ,dimnames=list( names(resultLst) ,tgt.scMtx )
    )
    auxTestLst <- lapply( resultLst ,function(p){p$auxTest})
    for( idx in 1:length(resultLst) ){
        zeroM <- resultLst[[idx]]$auxTest$zeroM
        mtx[ idx, intersect( colnames(mtx) ,zeroM ) ] <- "T"
    }



    #   cutRstLst[[1]]$auxInfoLst$basic$score1
    #       "fColEvt"     "summMtx"     "summMtx.reb" "scMtx.sz"   
    names( resultLst ) <- sapply( resultLst ,function(p){p$hIdx})
    cutRstLst <- lapply( resultLst ,function(p){p$cutRst})
    names(cutRstLst) <- paste("H",testSpan,sep="")
    names(cutRstLst) <- paste( names(cutRstLst) ,allIdxLst$stdFiltedCnt[as.character(testSpan)] ,sep="_" )

    if( is.null(tgt.scMtx) ){
        tgt.scMtx <- names( cutRstLst[[1]]$auxInfoLst$basic )
    }

    hSummMtx <- matrix( 0 ,nrow=length(cutRstLst) ,ncol=length(tgt.scMtx) )
    rownames(hSummMtx) <- names(cutRstLst)   ;colnames(hSummMtx) <- tgt.scMtx
    for( idx in 1:length(cutRstLst) ){
        for( mName in tgt.scMtx ){
            rawObj <- 
            summMtx <- cutRstLst[[idx]]$auxInfoLst$basic[[mName]]$summMtx
            #     all ph fCol phReb xyCnt.fCol xyCnt.phase
            # raw   0  0    0     0          0           0
            # evt   0  0    0     0          0           0

            hSummMtx[ idx, mName ] <- summMtx["raw","ph"]

        }
    }



}

