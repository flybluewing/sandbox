
bCMtxCfg <- list()
if( FALSE ){    # template
    # bCMtxCfg[[mName]] <- list(  mName = mName
    #     ,fCol = list(
    #         "aaa" = list( rng=c(min=0,max=1) )
    #         ,"aaa" = list( rng=c(min=0,max=1) )
    #     )
    # )
}

mName <- "crScrN01"
if( TRUE ){
    bCMtxCfg[[mName]] <- list(  mName = mName
        ,fCol = list(
            "rHpnE0Cnt" = list( rng=c(min=0,max=2) )
            ,"eHpnl1Cnt" = list( rng=c(min=0,max=2) )
            ,"phRebCntR" = list( rng=c(min=0,max=2) )
            ,"phRebCntE" = list( rng=c(min=0,max=2) )
        )
    )
}



for( idx in seq_len(length(bCMtxCfg)) ){

}
