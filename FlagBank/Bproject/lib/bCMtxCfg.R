
bCMtxCfg <- list()
if( FALSE ){    # template
    # bCMtxCfg[[mName]] <- list(  mName = mName
    #     ,fCol = list(
    #         "aaa" = list( rng=c(min=0,max=1) )
    #         ,"aaa" = list( rng=c(min=0,max=1) )
    #     )
    # )
}

crMName <- "crScrN01"
if( TRUE ){
    bCMtxCfg[[crMName]] <- list(  mName = crMName
        ,fCol = list(
            "rHpnE0Cnt" = list( rng=c(min=10,max=12) )
            ,"eHpnl1Cnt" = list( rng=c(min=10,max=12) )
            ,"phRebCntR" = list( rng=c(min=10,max=12) )
            ,"phRebCntE" = list( rng=c(min=10,max=12) )
        )
    )
}



for( idx in seq_len(length(bCMtxCfg)) ){

}
