
bCMtxCfg <- list()
if( FALSE ){    # template
    # bCMtxCfg[[mName]] <- list(  mName = mName
    #     ,fCol = list(
    #         "aaa" = list( rng=c(min=0,max=1) )
    #         ,"aaa" = list( rng=c(min=0,max=1) )
    #     )
    # )
}

crMName <- "crScrN01R"
if( TRUE ){
    bCMtxCfg[[crMName]] <- list(  mName = crMName
        ,fCol = list(
              "hpn"  = list( rng=c(min=0,max=1) )
             ,"ph"   = list( rng=c(min=0,max=1) )
             ,"fCol" = list( rng=c(min=0,max=1) )
             ,"phReb"    = list( rng=c(min=0,max=1) )
             ,"xyCnt.fCol"   = list( rng=c(min=0,max=1) )
             ,"xyCnt.phase"  = list( rng=c(min=0,max=1) )
             ,"ph_Reb"       = list( rng=c(min=0,max=1) )
             ,"fCol_Reb"     = list( rng=c(min=0,max=1) )
             ,"phReb_Reb"    = list( rng=c(min=0,max=1) )
             ,"xyCnt.fCol_Reb"   = list( rng=c(min=0,max=1) )
             ,"xyCnt.phase_Reb"  = list( rng=c(min=0,max=1) )
             ,"ph_sz"        = list( rng=c(min=0,max=1) )
             ,"fCol_sz"      = list( rng=c(min=0,max=1) )
             ,"dblHpnFlg_sz" = list( rng=c(min=0,max=1) )
             ,"ph_szDup"     = list( rng=c(min=0,max=1) )
             ,"fCol_szDup"   = list( rng=c(min=0,max=1) )
             ,"dblHpnFlg_szDup"  = list( rng=c(min=0,max=1) )
        )
    )
}




for( idx in seq_len(length(bCMtxCfg)) ){

}
