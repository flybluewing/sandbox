



Bprll.cutRstLst <- function( cutRstLst ,tgt.scMtx ){

#   cutRstLst[[1]]$auxInfoLst$basic$score1
#       "fColEvt"     "summMtx"     "summMtx.reb" "scMtx.sz"   

    if( is.null(tgt.scMtx) ){
        tgt.scMtx <- names( cutRstLst[[1]]$auxInfoLst$basic )
    }

    hSummMtx <- matrix( 0 ,nrow=length(cutRstLst) ,ncol=length(tgt.scMtx) )
    rownames(hSummMtx) <- names(cutRstLst)   ;colnames(hSummMtx) <- tgt.scMtx
    for( idx in 1:length(cutRstLst) ){
        for( mName in tgt.scMtx ){
            summMtx <- cutRstLst[[idx]]$auxInfoLst$basic[[mName]]$summMtx
            #     all ph fCol phReb xyCnt.fCol xyCnt.phase
            # raw   0  0    0     0          0           0
            # evt   0  0    0     0          0           0

            hSummMtx[ idx, mName ] <- summMtx["raw","ph"]

        }
    }



}

