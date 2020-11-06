
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



crMName <- "crScrN03R"
if( TRUE ){
    bCMtxCfg[[crMName]] <- list(  mName = crMName
        ,fCol = list(
              "hpn"  = list( rng=c(min=1,max=5) )
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

crMName <- "crScrN03E"
if( TRUE ){
    bCMtxCfg[[crMName]] <- list(  mName = crMName
        ,fCol = list(
              "hpn"  = list( rng=c(min=3,max=6) )
             ,"ph"   = list( rng=c(min=0,max=1) )       # 가능할까.. sfcLate 전용이긴 하지만. ㄷㄷ
             ,"fCol" = list( rng=c(min=0,max=1) )       #     상동
             ,"phReb"    = list( rng=c(min=0,max=1) )   #     상동
             ,"xyCnt.fCol"   = list( rng=c(min=0,max=1) )   # 상동 (789,799)
             ,"xyCnt.phase"  = list( rng=c(min=0,max=1) )
             ,"ph_Reb"       = list( rng=c(min=0,max=1) )
             ,"fCol_Reb"     = list( rng=c(min=0,max=1) )
             ,"phReb_Reb"    = list( rng=c(min=0,max=1) )
             ,"xyCnt.fCol_Reb"   = list( rng=c(min=0,max=1) )
             ,"xyCnt.phase_Reb"  = list( rng=c(min=0,max=1) )
             ,"ph_sz"        = list( rng=c(min=0,max=1) )
             ,"fCol_sz"      = list( rng=c(min=0,max=1) )   # 794
             ,"dblHpnFlg_sz" = list( rng=c(min=0,max=1) )
             ,"ph_szDup"     = list( rng=c(min=0,max=1) )
             ,"fCol_szDup"   = list( rng=c(min=0,max=1) )
             ,"dblHpnFlg_szDup"  = list( rng=c(min=0,max=1) )
        )
    )
}

crMName <- "crScrN03PhEvt"
if( TRUE ){
    bCMtxCfg[[crMName]] <- list(  mName = crMName
        ,fCol = list(
              "e3Max"   = list( rng=c(min=0,max=3) )
             ,"e3MCnt"  = list( rng=c(min=0,max=2) )
             ,"e2Max"   = list( rng=c(min=0,max=1) )
             ,"e2MCnt"  = list( rng=c(min=0,max=1) )
             ,"e1Max"   = list( rng=c(min=0,max=1) )
             ,"e1MCnt"  = list( rng=c(min=0,max=1) )
             ,"rebRawMax"   = list( rng=c(min=0,max=1) )
             ,"rebRawMCnt"  = list( rng=c(min=0,max=1) )
             ,"rebEvtMax"   = list( rng=c(min=0,max=1) )
             ,"rebEvtMCnt"  = list( rng=c(min=0,max=1) )
        )
    )
}

crMName <- "crScrN03Sum"
if( TRUE ){
    bCMtxCfg[[crMName]] <- list(  mName = crMName
        ,fCol = list(
              "summSumRaw"      = list( rng=c(min=0,max= 2) )
             ,"summSumEvt"      = list( rng=c(min=0,max= 1) )
             ,"summSumOthRaw"   = list( rng=c(min=0,max= 1) )
             ,"summSumOthEvt"   = list( rng=c(min=0,max= 1) )
             ,"summSumRebRaw"   = list( rng=c(min=0,max= 1) )
             ,"summSumRebEvt"   = list( rng=c(min=0,max= 1) )
             ,"summSumRebOthRaw"= list( rng=c(min=0,max= 1) )
             ,"summSumRebOthEvt"= list( rng=c(min=0,max= 1) )
             ,"szSumRebCnt"     = list( rng=c(min=0,max= 3) )
             ,"szSumRebDup"     = list( rng=c(min=0,max= 1) )
        )
    )
}



crMName <- "crScrN04R"
if( FALSE ){    # LA,LV에서 1 이하값은 모두 제거하고 있기 때문에 Raw 체크는 의미 없다.
    bCMtxCfg[[crMName]] <- list(  mName = crMName
        ,fCol = list(
              "hpn"  = list( rng=c(min=10,max=16) ) # c(min=10,max=16)
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

crMName <- "crScrN04E"
if( TRUE ){
    bCMtxCfg[[crMName]] <- list(  mName = crMName
        ,fCol = list(
              "hpn"  = list( rng=c(min=10,max=16) ) # c(min=10,max=16)
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

crMName <- "crScrN04PhEvt"
if( TRUE ){
    bCMtxCfg[[crMName]] <- list(  mName = crMName
        ,fCol = list(
              "e3Max"   = list( rng=c(min=0,max=6) )
             ,"e3MCnt"  = list( rng=c(min=0,max=2) )
             ,"e2Max"   = list( rng=c(min=0,max=1) )
             ,"e2MCnt"  = list( rng=c(min=0,max=1) )
             ,"e1Max"   = list( rng=c(min=0,max=1) )
             ,"e1MCnt"  = list( rng=c(min=0,max=1) )
             ,"rebRawMax"   = list( rng=c(min=0,max=1) )
             ,"rebRawMCnt"  = list( rng=c(min=0,max=1) )
             ,"rebEvtMax"   = list( rng=c(min=0,max=1) )
             ,"rebEvtMCnt"  = list( rng=c(min=0,max=1) )
        )
    )
}

crMName <- "crScrN04Sum"
if( TRUE ){
    bCMtxCfg[[crMName]] <- list(  mName = crMName
        ,fCol = list(
              "summSumRaw"      = list( rng=c(min=0,max= 1) )
             ,"summSumEvt"      = list( rng=c(min=0,max= 1) )
             ,"summSumOthRaw"   = list( rng=c(min=0,max= 1) )
             ,"summSumOthEvt"   = list( rng=c(min=0,max= 1) )
             ,"summSumRebRaw"   = list( rng=c(min=0,max= 1) )
             ,"summSumRebEvt"   = list( rng=c(min=0,max= 1) )
             ,"summSumRebOthRaw"= list( rng=c(min=0,max= 1) )
             ,"summSumRebOthEvt"= list( rng=c(min=0,max= 1) )
             ,"szSumRebCnt"     = list( rng=c(min=0,max= 1) )
             ,"szSumRebDup"     = list( rng=c(min=0,max= 1) )
        )
    )
}






crMName <- "crScrN02SumClM"
if( TRUE ){
    bCMtxCfg[[crMName]] <- list(  mName = crMName
        ,fCol = list(
              "sumTotHpn" = list( rng=c(min=3,max= 6) ) # 3~
             ,"sumTot1"   = list( rng=c(min=1,max= 6) ) # 1~
             ,"sumTot2"   = list( rng=c(min=0,max= 6) ) # 0~
             ,"sumTot3"   = list( rng=c(min=0,max= 1) ) # 0~0
        )
    )
}

crMName <- "crScrN03SumClM"
if( TRUE ){
    bCMtxCfg[[crMName]] <- list(  mName = crMName
        #   .reportworkRpt/CutRstCLM_sumTotA.txt 참고
        ,fCol = list(
              "sumTotHpn" = list( rng=c(min=0,max= 4) ) #   ~4  예외 : 826
             ,"sumTot1"   = list( rng=c(min=0,max= 2) ) # 
             ,"sumTot2"   = list( rng=c(min=0,max= 2) ) # 
             ,"sumTot3"   = list( rng=c(min=0,max= 1) ) #   ~0  예외 : 884 
        )
    )
}

crMName <- "crScrN04SumClM"
if( TRUE ){
    bCMtxCfg[[crMName]] <- list(  mName = crMName
        #   .reportworkRpt/CutRstCLM_sumTotL.txt 참고
        ,fCol = list(
              "sumTotHpn" = list( rng=c(min=0,max= 1) ) #
             ,"sumTot1"   = list( rng=c(min=0,max= 1) ) # 
             ,"sumTot2"   = list( rng=c(min=0,max= 1) ) # 
             ,"sumTot3"   = list( rng=c(min=0,max= 1) ) #
        )
    )
}


for( idx in seq_len(length(bCMtxCfg)) ){

}
