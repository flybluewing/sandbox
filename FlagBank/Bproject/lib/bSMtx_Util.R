
bS.getPhVPLst <- function( gEnv ,aZoidMtx ){
    phVPLst <- list()

    phVPLst[[1+length(phVPLst)]] <- bS.vp_ColVal( gEnv, aZoidMtx, fixCol=1 )
    phVPLst[[1+length(phVPLst)]] <- bS.vp_ColVal( gEnv, aZoidMtx, fixCol=3 )
    phVPLst[[1+length(phVPLst)]] <- bS.vp_ColVal( gEnv, aZoidMtx, fixCol=6 )

    names(phVPLst) <- sapply( phVPLst ,function(p){p$idStr})

    return( phVPLst )
}



