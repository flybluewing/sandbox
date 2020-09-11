
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
              "hpn"  = list( rng=c(min=0,max=2) )
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
             ,"ph_sz"        = list( rng=c(min=0,max=2) )
             ,"fCol_sz"      = list( rng=c(min=0,max=2) )
             ,"dblHpnFlg_sz" = list( rng=c(min=0,max=1) )
             ,"ph_szDup"     = list( rng=c(min=0,max=1) )
             ,"fCol_szDup"   = list( rng=c(min=0,max=1) )
             ,"dblHpnFlg_szDup"  = list( rng=c(min=0,max=1) )
        )
    )
}

crMName <- "crScrN01E"
if( TRUE ){
    bCMtxCfg[[crMName]] <- list(  mName = crMName
        ,fCol = list(
              "hpn"  = list( rng=c(min=0,max=2) )
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
             ,"ph_sz"        = list( rng=c(min=0,max=2) )
             ,"fCol_sz"      = list( rng=c(min=0,max=2) )
             ,"dblHpnFlg_sz" = list( rng=c(min=0,max=1) )
             ,"ph_szDup"     = list( rng=c(min=0,max=1) )
             ,"fCol_szDup"   = list( rng=c(min=0,max=1) )
             ,"dblHpnFlg_szDup"  = list( rng=c(min=0,max=1) )
        )
    )
}

crMName <- "crScrN01PhEvt"
if( TRUE ){
    bCMtxCfg[[crMName]] <- list(  mName = crMName
        ,fCol = list(
              "e3Max"   = list( rng=c(min=0,max=1) )
             ,"e3MCnt"  = list( rng=c(min=0,max=1) )
             ,"e2Max"   = list( rng=c(min=0,max=2) )
             ,"e2MCnt"  = list( rng=c(min=0,max=1) )
             ,"e1Max"   = list( rng=c(min=0,max=2) )
             ,"e1MCnt"  = list( rng=c(min=0,max=3) )
             ,"rebRawMax"   = list( rng=c(min=0,max=1) )
             ,"rebRawMCnt"  = list( rng=c(min=0,max=1) )
             ,"rebEvtMax"   = list( rng=c(min=0,max=1) )
             ,"rebEvtMCnt"  = list( rng=c(min=0,max=1) )
        )
    )
}

crMName <- "crScrN02R"
if( TRUE ){
    bCMtxCfg[[crMName]] <- list(  mName = crMName
        ,fCol = list(
              "hpn"  = list( rng=c(min=1,max=5) )
             ,"ph"   = list( rng=c(min=0,max=2) )
             ,"fCol" = list( rng=c(min=0,max=2) )
             ,"phReb"    = list( rng=c(min=0,max=2) )
             ,"xyCnt.fCol"   = list( rng=c(min=0,max=2) )
             ,"xyCnt.phase"  = list( rng=c(min=0,max=2) )
             ,"ph_Reb"       = list( rng=c(min=0,max=2) )
             ,"fCol_Reb"     = list( rng=c(min=0,max=2) )
             ,"phReb_Reb"    = list( rng=c(min=0,max=2) )
             ,"xyCnt.fCol_Reb"   = list( rng=c(min=0,max=2) )
             ,"xyCnt.phase_Reb"  = list( rng=c(min=0,max=2) )
             ,"ph_sz"        = list( rng=c(min=0,max=3) )
             ,"fCol_sz"      = list( rng=c(min=0,max=3) )
             ,"dblHpnFlg_sz" = list( rng=c(min=0,max=3) )
             ,"ph_szDup"     = list( rng=c(min=0,max=1) )
             ,"fCol_szDup"   = list( rng=c(min=0,max=1) )
             ,"dblHpnFlg_szDup"  = list( rng=c(min=0,max=1) )
        )
    )
}

crMName <- "crScrN02E"
if( TRUE ){
    bCMtxCfg[[crMName]] <- list(  mName = crMName
        ,fCol = list(
              "hpn"  = list( rng=c(min=1,max=7) )
             ,"ph"   = list( rng=c(min=0,max=2) )
             ,"fCol" = list( rng=c(min=0,max=2) )
             ,"phReb"    = list( rng=c(min=0,max=2) )
             ,"xyCnt.fCol"   = list( rng=c(min=0,max=2) )
             ,"xyCnt.phase"  = list( rng=c(min=0,max=2) )
             ,"ph_Reb"       = list( rng=c(min=0,max=2) )
             ,"fCol_Reb"     = list( rng=c(min=0,max=2) )
             ,"phReb_Reb"    = list( rng=c(min=0,max=2) )
             ,"xyCnt.fCol_Reb"   = list( rng=c(min=0,max=2) )
             ,"xyCnt.phase_Reb"  = list( rng=c(min=0,max=2) )
             ,"ph_sz"        = list( rng=c(min=0,max=4) )
             ,"fCol_sz"      = list( rng=c(min=0,max=3) )
             ,"dblHpnFlg_sz" = list( rng=c(min=0,max=3) )
             ,"ph_szDup"     = list( rng=c(min=0,max=1) )
             ,"fCol_szDup"   = list( rng=c(min=0,max=1) )
             ,"dblHpnFlg_szDup"  = list( rng=c(min=0,max=1) )
        )
    )
}

crMName <- "crScrN02PhEvt"
if( TRUE ){
    bCMtxCfg[[crMName]] <- list(  mName = crMName
        ,fCol = list(
              "e3Max"   = list( rng=c(min=0,max=1) )
             ,"e3MCnt"  = list( rng=c(min=0,max=1) )    # 2 for 2/20
             ,"e2Max"   = list( rng=c(min=0,max=2) )
             ,"e2MCnt"  = list( rng=c(min=0,max=2) )
             ,"e1Max"   = list( rng=c(min=0,max=2) )
             ,"e1MCnt"  = list( rng=c(min=0,max=3) )
             ,"rebRawMax"   = list( rng=c(min=0,max=1) )
             ,"rebRawMCnt"  = list( rng=c(min=0,max=1) )
             ,"rebEvtMax"   = list( rng=c(min=0,max=1) )
             ,"rebEvtMCnt"  = list( rng=c(min=0,max=1) )
        )
    )
}

crMName <- "crScrN02Sum"
if( TRUE ){
    bCMtxCfg[[crMName]] <- list(  mName = crMName
        ,fCol = list(
              "summSumRaw"      = list( rng=c(min=0,max= 5) )
             ,"summSumEvt"      = list( rng=c(min=0,max= 5) )
             ,"summSumOthRaw"   = list( rng=c(min=0,max= 3) )
             ,"summSumOthEvt"   = list( rng=c(min=0,max= 3) )
             ,"summSumRebRaw"   = list( rng=c(min=0,max= 1) )
             ,"summSumRebEvt"   = list( rng=c(min=0,max= 1) )
             ,"summSumRebOthRaw"= list( rng=c(min=0,max= 1) )
             ,"summSumRebOthEvt"= list( rng=c(min=0,max= 1) )
             ,"szSumRebCnt"     = list( rng=c(min=2,max=12) )   # min=1 796, 825, 826(0), 830(0), 854, 869, 898
             ,"szSumRebDup"     = list( rng=c(min=0,max= 2) )
        )
    )
}


for( idx in seq_len(length(bCMtxCfg)) ){

}
