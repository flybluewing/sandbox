if( FALSE ){    # document
    # 참고 코드 : bCMtxCfg.R
}

bSMtxMCfg <- list()

crMName <- "bSMScr01R"
if( FALSE ){
    bSMtxMCfg[[crMName]] <- list(  mName = crMName
        ,fCol = list(
              "hpn0"    =  list( rng=c(min=10,max=10) )
             ,"ph"      =  list( rng=c(min=10,max=10) )
             ,"fCol"    =  list( rng=c(min=10,max=10) )
             ,"phReb"   =  list( rng=c(min=10,max=10) )
             ,"xyCnt.fCol"  =  list( rng=c(min=10,max=10) )
             ,"xyCnt.phase" =  list( rng=c(min=10,max=10) )
             ,"ph_Reb"      =  list( rng=c(min=10,max=10) )
             ,"fCol_Reb"    =  list( rng=c(min=10,max=10) )
             ,"phReb_Reb"   =  list( rng=c(min=10,max=10) )
             ,"xyCnt.fCol_Reb"  =  list( rng=c(min=10,max=10) )
             ,"xyCnt.phase_Reb" =  list( rng=c(min=10,max=10) )
             ,"ph_sz"   =  list( rng=c(min=10,max=10) )
             ,"fCol_sz" =  list( rng=c(min=10,max=10) )
             ,"dblHpnFlg_sz"=  list( rng=c(min=10,max=10) )
             ,"ph_szDup"    =  list( rng=c(min=10,max=10) )
             ,"fCol_szDup"  =  list( rng=c(min=10,max=10) )
             ,"dblHpnFlg_szDup" = list( rng=c(min=10,max=10) )
        )
    )
}

crMName <- "bSMScr01E"
if( FALSE ){
    bSMtxMCfg[[crMName]] <- list(  mName = crMName
        ,fCol = list(
              "hpn0"    = list( rng=c(min=0,max=0) )
             ,"ph"      =  list( rng=c(min=0,max=0) )
             ,"fCol"    =  list( rng=c(min=0,max=0) )
             ,"phReb"   =  list( rng=c(min=0,max=0) )
             ,"xyCnt.fCol"  =  list( rng=c(min=0,max=0) )
             ,"xyCnt.phase" =  list( rng=c(min=0,max=0) )
             ,"ph_Reb"      =  list( rng=c(min=0,max=0) )
             ,"fCol_Reb"    =  list( rng=c(min=0,max=0) )
             ,"phReb_Reb"   =  list( rng=c(min=0,max=0) )
             ,"xyCnt.fCol_Reb"  =  list( rng=c(min=0,max=0) )
             ,"xyCnt.phase_Reb" =  list( rng=c(min=0,max=0) )
             ,"ph_sz"   =  list( rng=c(min=0,max=0) )
             ,"fCol_sz" =  list( rng=c(min=0,max=0) )
             ,"dblHpnFlg_sz"=  list( rng=c(min=0,max=0) )
             ,"ph_szDup"    =  list( rng=c(min=0,max=0) )
             ,"fCol_szDup"  =  list( rng=c(min=0,max=0) )
             ,"dblHpnFlg_szDup" = list( rng=c(min=0,max=0) )
        )
    )
}

crMName <- "bSMScr01PhEvt"
if( FALSE ){
    bSMtxMCfg[[crMName]] <- list(  mName = crMName
        ,fCol = list(
              "e3Max"   = list( rng=c(min=0,max=0) )
             ,"e3MCnt"  = list( rng=c(min=0,max=0) )
             ,"e2Max"   = list( rng=c(min=0,max=0) )
             ,"e2MCnt"  = list( rng=c(min=0,max=0) )
             ,"e1Max"   = list( rng=c(min=0,max=0) )
             ,"e1MCnt"  = list( rng=c(min=0,max=0) )
             ,"rebRawMax"   = list( rng=c(min=0,max=0) )
             ,"rebRawMCnt"  = list( rng=c(min=0,max=0) )
             ,"rebEvtMax"   = list( rng=c(min=0,max=0) )
             ,"rebEvtMCnt"  = list( rng=c(min=0,max=0) )
        )
    )
}

crMName <- "bSMScr01Sum"
if( FALSE ){
    bSMtxMCfg[[crMName]] <- list(  mName = crMName
        ,fCol = list(
              "summSumRaw"  = list( rng=c(min=0,max=1) )
             ,"summSumEvt"  = list( rng=c(min=0,max=1) )
             ,"summSumOthRaw"   = list( rng=c(min=0,max=1) )
             ,"summSumOthEvt"   = list( rng=c(min=0,max=1) )
             ,"summSumRebRaw"   = list( rng=c(min=0,max=1) )
             ,"summSumRebEvt"   = list( rng=c(min=0,max=1) )
             ,"summSumRebOthRaw"= list( rng=c(min=0,max=1) )
             ,"summSumRebOthEvt"= list( rng=c(min=0,max=1) )
             ,"szSumRebCnt" = list( rng=c(min=0,max=1) )
             ,"szSumRebDup" = list( rng=c(min=0,max=1) )
        )
    )
}

crMName <- "sample"
if( FALSE ){
    bSMtxMCfg[[crMName]] <- list(  mName = crMName
        ,fCol = list(
              "xxx"  = list( rng=c(min=0,max=1) )
             ,"xxx"   = list( rng=c(min=0,max=1) )
        )
    )
}

for( idx in seq_len(length(bSMtxMCfg)) ){

}




