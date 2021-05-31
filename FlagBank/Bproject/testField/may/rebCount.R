
cutRstGrp <- may.loadSaves()

rebGrp <- list()
for( nIdx in names(cutRstGrp) ){
    cutRstObj <- cutRstGrp[[nIdx]]

    rebLst <- list()
    for( hIdx in names(cutRstObj$cutRstLst) ){
        rebLst[[hIdx]] <- 0

        cutInfoLst <- cutRstObj$cutRstLst[[hIdx]]$cutInfoLst
        if( 0<length(cutInfoLst) ){
            rebFlag <- sapply( cutInfoLst ,function(ci){ 
                            grepl("rReb\\(hpn",ci["info"] )
            })
            rebLst[[hIdx]] <- rebLst[[hIdx]] + sum(rebFlag)
        }

        cutInfoLstHCR <- cutRstObj$cutRstLstHCR[[hIdx]]$cutInfoLst
        if( 0<length(cutInfoLstHCR) ){
            rebFlag <- sapply( cutInfoLstHCR ,function(ci){ 
                            grepl("rReb\\(hpn",ci["info"] )
            })
            rebLst[[hIdx]] <- rebLst[[hIdx]] + sum(rebFlag)
        }
    }

    rebGrp[[nIdx]] <- sapply(rebLst,function(p){p})
}

