if( FALSE ){    # document
    # 참고 코드 : bCMtxCfg.R
}

bSMtxMCfg <- list()

crMName <- "bSMScr02R"
if( TRUE ){
    bSMtxMCfg[[crMName]] <- list(  mName = crMName ,ver=2.0
        ,fCol = list(
              "hpn0"    =  list( rng=c(min=2,max=6) 
                            ,evt=matrix( c(c(1,1,2),c(2,5,6)) ,ncol=2)
              )
             ,"ph"      =  list( rng=c(min=0,max=2) 
                            ,evt=matrix( c(c(1,3,3),c(1,2,3)) ,ncol=2)
             )
             ,"fCol"    =  list( rng=c(min=0,max=2) 
                            ,evt=matrix( c(c(1,3,3),c(1,2,3)) ,ncol=2)
             )
             ,"phReb"   =  list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(2,3,3),c(1,2,3)) ,ncol=2)
             )
             ,"xyCnt.fCol"  =  list( rng=c(min=0,max=2) 
                            ,evt=matrix( c(c(2,3,3),c(1,2,3)) ,ncol=2)
             )
             ,"xyCnt.phase" =  list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(3,3,3),c(1,2,3)) ,ncol=2)
             )
             ,"ph_Reb"      =  list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(2,3,3),c(1,2,3)) ,ncol=2)
             )
             ,"fCol_Reb"    =  list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(2,3,3),c(1,2,3)) ,ncol=2)
             )
             ,"phReb_Reb"   =  list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(3,3,3),c(1,2,3)) ,ncol=2)
             )
             ,"xyCnt.fCol_Reb"  =  list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(2,3,3),c(1,2,3)) ,ncol=2)
             )
             ,"xyCnt.phase_Reb" =  list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(2,3,3),c(1,2,3)) ,ncol=2)
             )
             ,"ph_sz"   =  list( rng=c(min=0,max=3) 
                            ,evt=matrix( c(c(1,3),c(2,3)) ,ncol=2)
             )
             ,"fCol_sz" =  list( rng=c(min=0,max=2) 
                            ,evt=matrix( c(c(1,3),c(2,3)) ,ncol=2)
             )
             ,"dblHpnFlg_sz"=  list( rng=c(min=0,max=1)
                            ,evt=matrix( c(c(3,3),c(1,2)) ,ncol=2)
             )
             ,"ph_szDup"    =  list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(3,3),c(1,2)) ,ncol=2)
             )
             ,"fCol_szDup"  =  list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(3,3),c(1,2)) ,ncol=2)
             )
             ,"dblHpnFlg_szDup" = list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(3,3),c(1,2)) ,ncol=2)
             )
        )
        ,evtMax = c("lev1"=1 ,"lev2"=1 ,"lev3"=1)
    )
}

crMName <- "bSMScr02E"
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

crMName <- "bSMScr02PhEvt"
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

crMName <- "bSMScr02Sum"
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

crMName <- "crScrN02SumClM"
if( FALSE ){
    bSMtxMCfg[[crMName]] <- list(  mName = crMName
        ,fCol = list(
              "sumTotHpn" = list( rng=c(min=0,max= 1) )
             ,"sumTot1"   = list( rng=c(min=0,max= 1) )
             ,"sumTot2"   = list( rng=c(min=0,max= 1) )
             ,"sumTot3"   = list( rng=c(min=0,max= 1) )
        )
    )
}



crMName <- "bSMScr04R"
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

crMName <- "bSMScr04E"
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

crMName <- "bSMScr04PhEvt"
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

crMName <- "bSMScr04Sum"
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

crMName <- "crScrN04SumClM"
if( FALSE ){
    bSMtxMCfg[[crMName]] <- list(  mName = crMName
        ,fCol = list(
              "sumTotHpn" = list( rng=c(min=0,max= 1) )
             ,"sumTot1"   = list( rng=c(min=0,max= 1) )
             ,"sumTot2"   = list( rng=c(min=0,max= 1) )
             ,"sumTot3"   = list( rng=c(min=0,max= 1) )
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


for( crMName in names( bSMtxMCfg ) ){ # naming 추가.

    for( fcName in names(bSMtxMCfg[[crMName]]$fCol) ){
        colnames(bSMtxMCfg[[crMName]]$fCol[[fcName]]$evt) <- c("lev","val")
    }

    if( is.null(bSMtxMCfg[[crMName]]$evtMax) ){
        #   한 개 phase 내에서의 이벤트 발생 제한.
        #   "levN"=M    N등급 이상 이벤트 갯수는 M개 이내.
        #   주의 : 이름이 붙었을 뿐, 사실상 evtMax 순서가 evt 레벨로서 사용된다.
        bSMtxMCfg[[crMName]]$evtMax     <- c("lev1"=3 ,"lev2"=2 ,"lev3"=1)
    }

}

