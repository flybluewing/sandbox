if( FALSE ){    # document
    # 참고 코드 : bCMtxCfg.R
}

bSMtxMCfg <- list()

crMName <- "bSMScr02R"
if( TRUE ){
    bSMtxMCfg[[crMName]] <- list(  mName = crMName ,ver=2.0
        ,fCol = list(
              "hpn0"    =  list( rng=c(min=2,max=6)         # H797 hpn0(1)
                            ,evt=matrix( c(c(1,2),c(2,6)) ,ncol=2)
              )
             ,"ph"      =  list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(1,3),c(2,3)) ,ncol=2)
             )
             ,"fCol"    =  list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(1,2),c(1,3)) ,ncol=2)
             )
             ,"phReb"   =  list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(3,3,3),c(1,2,3)) ,ncol=2)
             )
             ,"xyCnt.fCol"  =  list( rng=c(min=0,max=1)     # H834 xyCnt.fCol(2)
                            ,evt=matrix( c(c(1,2,3),c(1,2,3)) ,ncol=2)
             )
             ,"xyCnt.phase" =  list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(3,3,3),c(1,2,3)) ,ncol=2)
             )
             ,"ph_Reb"      =  list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(3,3,3),c(1,2,3)) ,ncol=2)
             )
             ,"fCol_Reb"    =  list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(1,3),c(1,3)) ,ncol=2)
             )
             ,"phReb_Reb"   =  list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(3,3,3),c(1,2,3)) ,ncol=2)
             )
             ,"xyCnt.fCol_Reb"  =  list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(1,2,3),c(1,2,3)) ,ncol=2)
             )
             ,"xyCnt.phase_Reb" =  list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(3,3,3),c(1,2,3)) ,ncol=2)
             )
             ,"ph_sz"   =  list( rng=c(min=0,max=3) 
                            ,evt=matrix( c(c(1,2),c(2,3)) ,ncol=2)
             )
             ,"fCol_sz" =  list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(1,3),c(2,3)) ,ncol=2)
             )
             ,"dblHpnFlg_sz"=  list( rng=c(min=0,max=1)
                            ,evt=matrix( c(c(3,3),c(1,2)) ,ncol=2)
             )
             ,"ph_szDup"    =  list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(3,3),c(1,2)) ,ncol=2)
             )
             ,"fCol_szDup"  =  list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(1,3),c(1,2)) ,ncol=2)
             )
             ,"dblHpnFlg_szDup" = list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(3,3),c(1,2)) ,ncol=2)
             )
        )
        ,evtMax = c("lev1"=3 ,"lev2"=2 ,"lev3"=1)
    )
}

crMName <- "bSMScr02E"
if( TRUE ){
    bSMtxMCfg[[crMName]] <- list(  mName=crMName ,ver=2.0
        ,fCol = list(
              "hpn0"    = list( rng=c(min=4,max=7)  # H896(8)  ,H797/H812(2,3)
                            ,evt=matrix( c(c(1),c(7)) ,ncol=2)
              )
             ,"ph"      =  list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(2,3),c(1,2)) ,ncol=2)
              )
             ,"fCol"    =  list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(3),c(2)) ,ncol=2)
              )
             ,"phReb"   =  list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(3),c(1)) ,ncol=2)
              )
             ,"xyCnt.fCol"  =  list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(1),c(2)) ,ncol=2)
              )
             ,"xyCnt.phase" =  list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(2),c(1)) ,ncol=2)
              )
             ,"ph_Reb"      =  list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(3),c(1)) ,ncol=2)
              )
             ,"fCol_Reb"    =  list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(3),c(1)) ,ncol=2)
              )
             ,"phReb_Reb"   =  list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(3),c(1)) ,ncol=2)
              )
             ,"xyCnt.fCol_Reb"  =  list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(1),c(1)) ,ncol=2)
              )
             ,"xyCnt.phase_Reb" =  list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(3),c(1)) ,ncol=2)
              )
             ,"ph_sz"   =  list( rng=c(min=0,max=2) 
                            ,evt=matrix( c(c(1),c(2)) ,ncol=2)
              )
             ,"fCol_sz" =  list( rng=c(min=0,max=2) 
                            ,evt=matrix( c(c(1),c(3)) ,ncol=2)
              )
             ,"dblHpnFlg_sz"=  list( rng=c(min=0,max=0) 
                            ,evt=matrix( c(c(1),c(1)) ,ncol=2)
              )
             ,"ph_szDup"    =  list( rng=c(min=0,max=0) 
                            ,evt=matrix( c(c(3),c(1)) ,ncol=2)
              )
             ,"fCol_szDup"  =  list( rng=c(min=0,max=0) 
                            ,evt=matrix( c(c(3),c(1)) ,ncol=2)
              )
             ,"dblHpnFlg_szDup" = list( rng=c(min=0,max=0) 
                            ,evt=matrix( c(c(3),c(1)) ,ncol=2)
              )
        )
        ,evtMax = c("lev1"=3 ,"lev2"=2 ,"lev3"=1)
    )
}

crMName <- "bSMScr02PhEvt"
if( TRUE ){
    bSMtxMCfg[[crMName]] <- list(  mName=crMName ,ver=2.0
        ,fCol = list(
              "e3Max"   = list( rng=c(min=0,max=2)      # H795(3)
                            ,evt=matrix( c(c(3),c(4)) ,ncol=2)
              )
             ,"e3MCnt"  = list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(2),c(1)) ,ncol=2)
              )
             ,"e2Max"   = list( rng=c(min=0,max=2)      # H880(3)
                            ,evt=matrix( c(c(2),c(2)) ,ncol=2)
              )
             ,"e2MCnt"  = list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(2),c(1)) ,ncol=2)
              )
             ,"e1Max"   = list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(2),c(1)) ,ncol=2)
              )
             ,"e1MCnt"  = list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(2),c(1)) ,ncol=2)
              )
             ,"rebRawMax"   = list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(2),c(2)) ,ncol=2)
              )
             ,"rebRawMCnt"  = list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(1),c(2)) ,ncol=2)
              )
             ,"rebEvtMax"   = list( rng=c(min=0,max=1)      # H832(2)
                            ,evt=matrix( c(c(1),c(2)) ,ncol=2)
              )
             ,"rebEvtMCnt"  = list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(1),c(2)) ,ncol=2)
              )
        )
        ,evtMax = c("lev1"=3 ,"lev2"=2 ,"lev3"=1)
    )
}

crMName <- "bSMScr02Sum"
if( TRUE ){
    bSMtxMCfg[[crMName]] <- list(  mName = crMName ,ver=2.0
        ,fCol = list(
              "summSumRaw"  = list( rng=c(min=0,max=2) 
                            ,evt=matrix( c(c(2,3),c(2,3)) ,ncol=2)
              )
             ,"summSumEvt"  = list( rng=c(min=0,max=2)
                            ,evt=matrix( c(c(1,1),c(1,2)) ,ncol=2)
              )
             ,"summSumOthRaw"   = list( rng=c(min=0,max=1)      # H834(2)
                            ,evt=matrix( c(c(1),c(2)) ,ncol=2)
              )
             ,"summSumOthEvt"   = list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(1),c(2)) ,ncol=2)
              )
             ,"summSumRebRaw"   = list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(1),c(2)) ,ncol=2)
              )
             ,"summSumRebEvt"   = list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(2),c(1)) ,ncol=2)
              )
             ,"summSumRebOthRaw"= list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(2),c(1)) ,ncol=2)
              )
             ,"summSumRebOthEvt"= list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(2),c(1)) ,ncol=2)
              )
             ,"szSumRebCnt" = list( rng=c(min=0,max=6) 
                            ,evt=matrix( c(c(1),c(6)) ,ncol=2)
              )
             ,"szSumRebDup" = list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(1),c(1)) ,ncol=2)
              )
        )
        ,evtMax = c("lev1"=3,"lev2"=2,"lev3"=1)
    )
}

crMName <- "bSMScr02SumClM"
if( TRUE ){
    bSMtxMCfg[[crMName]] <- list(  mName=crMName ,ver=2.0
        ,fCol = list(
              "sumTotHpn" = list( rng=c(min=0,max= 5)   # H864
                            ,evt=matrix( c(c(1),c(6)) ,ncol=2)
              )
             ,"sumTot1"   = list( rng=c(min=0,max= 4) 
                            ,evt=matrix( c(c(1),c(6)) ,ncol=2)
              )
             ,"sumTot2"   = list( rng=c(min=0,max= 4)
                            ,evt=matrix( c(c(1),c(6)) ,ncol=2)
              )
             ,"sumTot3"   = list( rng=c(min=0,max= 1) 
                            ,evt=matrix( c(c(1),c(6)) ,ncol=2)
              )
        )
        ,evtMax = c("lev1"=3,"lev2"=2,"lev3"=1)
    )
}



crMName <- "bSMScr04R"
if( TRUE ){
    bSMtxMCfg[[crMName]] <- list(  mName=crMName ,ver=2.0
        ,fCol = list(
              "hpn0"    =  list( rng=c(min=4,max=15)        # H789(16)
                            ,evt=matrix( c(c(1,1,1),c( 4,15,16)) ,ncol=2)
              )
             ,"ph"      =  list( rng=c(min=0,max=2) 
                            ,evt=matrix( c(c(3),c(3)) ,ncol=2)
              )
             ,"fCol"    =  list( rng=c(min=0,max=2) 
                            ,evt=matrix( c(c(3),c(3)) ,ncol=2)
              )
             ,"phReb"   = list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(3),c(1)) ,ncol=2)
              )
             ,"xyCnt.fCol"  =  list( rng=c(min=0,max=2) 
                            ,evt=matrix( c(c(1),c(2)) ,ncol=2)
              )
             ,"xyCnt.phase" =  list( rng=c(min=0,max=2) 
                            ,evt=matrix( c(c(1),c(2)) ,ncol=2)
              )
             ,"ph_Reb"      =  list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(3),c(1)) ,ncol=2)
              )
             ,"fCol_Reb"    =  list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(3),c(1)) ,ncol=2)
              )
             ,"phReb_Reb"   =  list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(3),c(1)) ,ncol=2)
              )
             ,"xyCnt.fCol_Reb"  =  list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(3),c(1)) ,ncol=2)
              )
             ,"xyCnt.phase_Reb" =  list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(3),c(1)) ,ncol=2)
              )
             ,"ph_sz"   =  list( rng=c(min=0,max=5) 
                            ,evt=matrix( c(c(1),c(5)) ,ncol=2)
              )
             ,"fCol_sz" =  list( rng=c(min=0,max=4) 
                            ,evt=matrix( c(c(1),c(4)) ,ncol=2)
              )
             ,"dblHpnFlg_sz"=  list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(3),c(1)) ,ncol=2)
              )
             ,"ph_szDup"    =  list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(1),c(1)) ,ncol=2)
              )
             ,"fCol_szDup"  =  list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(1),c(1)) ,ncol=2)
              )
             ,"dblHpnFlg_szDup" = list( rng=c(min=0,max=1) 
                            ,evt=matrix( c(c(1),c(1)) ,ncol=2)
              )
        )
        ,evtMax = c("lev1"=3,"lev2"=2,"lev3"=1)
    )
}

crMName <- "bSMScr04E"
if( TRUE ){
    bSMtxMCfg[[crMName]] <- list(  mName = crMName
        ,fCol = list(
              "hpn0"    = list( rng=c(min=12,max=16)        ,evt=matrix(0,ncol=2,nrow=0) )
             ,"ph"      = list( rng=c(min= 0,max= 1)        ,evt=matrix(c(2,1),ncol=2) )
             ,"fCol"    = list( rng=c(min= 0,max= 1)        ,evt=matrix(c(2,1),ncol=2) )
             ,"phReb"   = list( rng=c(min= 0,max= 1)        ,evt=matrix(c(2,1),ncol=2) )
             ,"xyCnt.fCol"  =  list( rng=c(min=0,max=1)     ,evt=matrix(c(2,1),ncol=2) )
             ,"xyCnt.phase" =  list( rng=c(min=0,max=1)     ,evt=matrix(c(2,1),ncol=2) )
             ,"ph_Reb"      =  list( rng=c(min=0,max=1)     ,evt=matrix(c(2,1),ncol=2) )
             ,"fCol_Reb"    =  list( rng=c(min=0,max=1)     ,evt=matrix(c(2,1),ncol=2) )
             ,"phReb_Reb"   =  list( rng=c(min=0,max=1)     ,evt=matrix(c(2,1),ncol=2) )
             ,"xyCnt.fCol_Reb"  =  list( rng=c(min=0,max=1) ,evt=matrix(c(2,1),ncol=2) )
             ,"xyCnt.phase_Reb" =  list( rng=c(min=0,max=1) ,evt=matrix(c(2,1),ncol=2) )
             ,"ph_sz"   =  list( rng=c(min=0,max=1)         ,evt=matrix(c(2,1),ncol=2) )
             ,"fCol_sz" =  list( rng=c(min=0,max=1)         ,evt=matrix(c(2,1),ncol=2) )
             ,"dblHpnFlg_sz"=  list( rng=c(min=0,max=1)     ,evt=matrix(c(2,1),ncol=2) )
             ,"ph_szDup"    =  list( rng=c(min=0,max=1)     ,evt=matrix(c(2,1),ncol=2) )
             ,"fCol_szDup"  =  list( rng=c(min=0,max=1)     ,evt=matrix(c(2,1),ncol=2) )
             ,"dblHpnFlg_szDup" = list( rng=c(min=0,max=1)  ,evt=matrix(c(2,1),ncol=2) )
        )
        ,evtMax = c("lev1"=3,"lev2"=2,"lev3"=1)
    )
}

crMName <- "bSMScr04PhEvt"
if( TRUE ){
    bSMtxMCfg[[crMName]] <- list(  mName=crMName ,ver=2.0
        ,fCol = list(
              "e3Max"   = list( rng=c(min=0,max=3)      # H873(4),H878(4),H889(4)
                            ,evt=matrix( c(c(3,4),c(3,4)) ,ncol=2)
              )
             ,"e3MCnt"  = list( rng=c(min=0,max=1)
                            ,evt=matrix( c(c(3),c(13)) ,ncol=2)
              )
             ,"e2Max"   = list( rng=c(min=0,max=1)  
                            ,evt=matrix( c(c(3),c(13)) ,ncol=2)
              )
             ,"e2MCnt"  = list( rng=c(min=0,max=1)  
                            ,evt=matrix( c(c(3),c(13)) ,ncol=2)
              )
             ,"e1Max"   = list( rng=c(min=0,max=1)  
                            ,evt=matrix( c(c(3),c(13)) ,ncol=2)
              )
             ,"e1MCnt"  = list( rng=c(min=0,max=1)  
                            ,evt=matrix( c(c(3),c(13)) ,ncol=2)
              )
             ,"rebRawMax"   = list( rng=c(min=0,max=2)  
                            ,evt=matrix( c(c(1),c(2)) ,ncol=2)
              )
             ,"rebRawMCnt"  = list( rng=c(min=0,max=1)  
                            ,evt=matrix( c(c(3),c(13)) ,ncol=2)
              )
             ,"rebEvtMax"   = list( rng=c(min=0,max=1)  
                            ,evt=matrix( c(c(3),c(13)) ,ncol=2)
              )
             ,"rebEvtMCnt"  = list( rng=c(min=0,max=1)  
                            ,evt=matrix( c(c(3),c(13)) ,ncol=2)
              )
        )
        ,evtMax = c("lev1"=3,"lev2"=2,"lev3"=1)
    )
}

crMName <- "bSMScr04Sum"
if( TRUE ){    # need assess
    bSMtxMCfg[[crMName]] <- list(  mName=crMName ,ver=2.0
        ,fCol = list(
              "summSumRaw"  = list( rng=c(min=0,max=4)  
                            ,evt=matrix( c(c(1),c(4)) ,ncol=2)
              )
             ,"summSumEvt"  = list( rng=c(min=0,max=1)  
                            ,evt=matrix( c(c(3,4),c(3,4)) ,ncol=2)
              )
             ,"summSumOthRaw"   = list( rng=c(min=0,max=2)  
                            ,evt=matrix( c(c(3,4),c(3,4)) ,ncol=2)
              )
             ,"summSumOthEvt"   = list( rng=c(min=0,max=1)  
                            ,evt=matrix( c(c(3,4),c(3,4)) ,ncol=2)
              )
             ,"summSumRebRaw"   = list( rng=c(min=0,max=1)  
                            ,evt=matrix( c(c(3,4),c(3,4)) ,ncol=2)
              )
             ,"summSumRebEvt"   = list( rng=c(min=0,max=1)  
                            ,evt=matrix( c(c(3,4),c(3,4)) ,ncol=2)
              )
             ,"summSumRebOthRaw"= list( rng=c(min=0,max=1)
                            ,evt=matrix( c(c(3,4),c(3,4)) ,ncol=2)
              )
             ,"summSumRebOthEvt"= list( rng=c(min=0,max=1)  
                            ,evt=matrix( c(c(3,4),c(3,4)) ,ncol=2)
              )
             ,"szSumRebCnt" = list( rng=c(min=0,max=7)      # H787(8),H806(8),H844(8)
                            ,evt=matrix( c(c(1,1),c(7,8)) ,ncol=2)
              )
             ,"szSumRebDup" = list( rng=c(min=0,max=1)  
                            ,evt=matrix( c(c(3,4),c(3,4)) ,ncol=2)
              )
        )
        ,evtMax = c("lev1"=3,"lev2"=2,"lev3"=1)
    )
}

crMName <- "bSMScr04SumClM"
if( TRUE ){    # need assess
    bSMtxMCfg[[crMName]] <- list(  mName=crMName ,ver=2.0
        ,fCol = list(
              "sumTotHpn" = list( rng=c(min=0,max= 3)   # H873(4)
                            ,evt=matrix( c(c(1),c(4)) ,ncol=2)
              )
             ,"sumTot1"   = list( rng=c(min=0,max= 2)   # H794(3)
                            ,evt=matrix( c(c(1),c(3)) ,ncol=2)
              )
             ,"sumTot2"   = list( rng=c(min=0,max= 2)   # H873(3) 
                            ,evt=matrix( c(c(3,4),c(3,4)) ,ncol=2)
              )
             ,"sumTot3"   = list( rng=c(min=0,max= 1)  
                            ,evt=matrix( c(c(1),c(3)) ,ncol=2)
              )
        )
        ,evtMax = c("lev1"=3,"lev2"=2,"lev3"=1)
    )
}


crMName <- "bSMScr05R"
if( TRUE ){
    bSMtxMCfg[[crMName]] <- list(  mName=crMName ,ver=2.0
        ,fCol = list(
              "hpn0"        = list( rng=c(min=0,max=4) ,evt=matrix( c(c(5),c(-1)) ,ncol=2)   )
             ,"ph"          = list( rng=c(min=0,max=3) ,evt=matrix( c(c(1),c(-1)) ,ncol=2)  )
             ,"fCol"        = list( rng=c(min=0,max=2) ,evt=matrix( c(c(1),c(-1)) ,ncol=2)  )
             ,"phReb"       = list( rng=c(min=0,max=1) ,evt=matrix( c(c(1),c(-1)) ,ncol=2)  )
             ,"xyCnt.fCol"  = list( rng=c(min=0,max=2) ,evt=matrix( c(c(1),c(-1)) ,ncol=2)  )
             ,"xyCnt.phase" = list( rng=c(min=0,max=1) ,evt=matrix( c(c(1),c(-1)) ,ncol=2)  )
             ,"ph_Reb"      = list( rng=c(min=0,max=1) ,evt=matrix( c(c(1),c(-1)) ,ncol=2)  )
             ,"fCol_Reb"    = list( rng=c(min=0,max=1) ,evt=matrix( c(c(1),c(-1)) ,ncol=2)  )
             ,"phReb_Reb"   = list( rng=c(min=0,max=1) ,evt=matrix( c(c(1),c(-1)) ,ncol=2)  )
             ,"xyCnt.fCol_Reb"  = list( rng=c(min=0,max=1) ,evt=matrix( c(c(1),c(-1)) ,ncol=2)  )
             ,"xyCnt.phase_Reb" = list( rng=c(min=0,max=1) ,evt=matrix( c(c(1),c(-1)) ,ncol=2)  )
             ,"ph_sz"       = list( rng=c(min=0,max=4) ,evt=matrix( c(c(1),c(-1)) ,ncol=2)  )
             ,"fCol_sz"     = list( rng=c(min=0,max=4) ,evt=matrix( c(c(1),c(-1)) ,ncol=2)  )
             ,"dblHpnFlg_sz"= list( rng=c(min=0,max=1) ,evt=matrix( c(c(1),c(-1)) ,ncol=2)  )
             ,"ph_szDup"    = list( rng=c(min=0,max=2) ,evt=matrix( c(c(1),c(-1)) ,ncol=2)  )
             ,"fCol_szDup"  = list( rng=c(min=0,max=1) ,evt=matrix( c(c(1),c(-1)) ,ncol=2)  )
             ,"dblHpnFlg_szDup" = list( rng=c(min=0,max=1) ,evt=matrix( c(c(1),c(-1)) ,ncol=2)   )
        )
        ,evtMax = c("lev1"=3,"lev2"=2,"lev3"=1)
    )
}

crMName <- "bSMScr05E"
if( TRUE ){
    bSMtxMCfg[[crMName]] <- list(  mName = crMName
        ,fCol = list(
              "hpn0"        = list( rng=c(min= 2,max= 6)   ,evt=matrix(0,ncol=2,nrow=0) )
             ,"ph"          = list( rng=c(min= 0,max= 1)   ,evt=matrix(c(2,-1),ncol=2) )
             ,"fCol"        = list( rng=c(min= 0,max= 1)   ,evt=matrix(c(2,-1),ncol=2) )
             ,"phReb"       = list( rng=c(min= 0,max= 1)   ,evt=matrix(c(2,-1),ncol=2) )
             ,"xyCnt.fCol"  = list( rng=c(min=0,max=1)     ,evt=matrix(c(2,-1),ncol=2) )
             ,"xyCnt.phase" = list( rng=c(min=0,max=1)     ,evt=matrix(c(2,-1),ncol=2) )
             ,"ph_Reb"      = list( rng=c(min=0,max=1)     ,evt=matrix(c(2,-1),ncol=2) )
             ,"fCol_Reb"    = list( rng=c(min=0,max=1)     ,evt=matrix(c(2,-1),ncol=2) )
             ,"phReb_Reb"   = list( rng=c(min=0,max=1)     ,evt=matrix(c(2,-1),ncol=2) )
             ,"xyCnt.fCol_Reb"  = list( rng=c(min=0,max=1) ,evt=matrix(c(2,-1),ncol=2) )
             ,"xyCnt.phase_Reb" = list( rng=c(min=0,max=1) ,evt=matrix(c(2,-1),ncol=2) )
             ,"ph_sz"       = list( rng=c(min=0,max=1)     ,evt=matrix(c(2,-1),ncol=2) )
             ,"fCol_sz"     = list( rng=c(min=0,max=1)     ,evt=matrix(c(2,-1),ncol=2) )
             ,"dblHpnFlg_sz"= list( rng=c(min=0,max=1)     ,evt=matrix(c(2,-1),ncol=2) )
             ,"ph_szDup"    = list( rng=c(min=0,max=1)     ,evt=matrix(c(2,-1),ncol=2) )
             ,"fCol_szDup"  = list( rng=c(min=0,max=1)     ,evt=matrix(c(2,-1),ncol=2) )
             ,"dblHpnFlg_szDup" = list( rng=c(min=0,max=1) ,evt=matrix(c(2,-1),ncol=2) )
        )
        ,evtMax = c("lev1"=3,"lev2"=2,"lev3"=1)
    )
}

crMName <- "bSMScr05PhEvt"  # QQE:Todo
if( TRUE ){
    bSMtxMCfg[[crMName]] <- list(  mName=crMName ,ver=2.0
        ,fCol = list(
              "e3Max"   = list( rng=c(min=0,max=2)      ,evt=matrix( c(c(3),c(-1)) ,ncol=2) )
             ,"e3MCnt"  = list( rng=c(min=0,max=1)      ,evt=matrix( c(c(3),c(-1)) ,ncol=2) )
             ,"e2Max"   = list( rng=c(min=0,max=1)      ,evt=matrix( c(c(3),c(-1)) ,ncol=2) )
             ,"e2MCnt"  = list( rng=c(min=0,max=1)      ,evt=matrix( c(c(3),c(-1)) ,ncol=2) )
             ,"e1Max"   = list( rng=c(min=0,max=1)      ,evt=matrix( c(c(3),c(-1)) ,ncol=2) )
             ,"e1MCnt"  = list( rng=c(min=0,max=1)      ,evt=matrix( c(c(3),c(-1)) ,ncol=2) )
             ,"rebRawMax"   = list( rng=c(min=0,max=2)  ,evt=matrix( c(c(1),c(-1)) ,ncol=2) )
             ,"rebRawMCnt"  = list( rng=c(min=0,max=1)  ,evt=matrix( c(c(3),c(-1)) ,ncol=2) )
             ,"rebEvtMax"   = list( rng=c(min=0,max=1)  ,evt=matrix( c(c(3),c(-1)) ,ncol=2) )
             ,"rebEvtMCnt"  = list( rng=c(min=0,max=1)  ,evt=matrix( c(c(3),c(-1)) ,ncol=2) )
        )
        ,evtMax = c("lev1"=3,"lev2"=2,"lev3"=1)
    )
}

crMName <- "bSMScr05Sum"  # QQE:Todo
if( TRUE ){
    bSMtxMCfg[[crMName]] <- list(  mName=crMName ,ver=2.0
        ,fCol = list(
              "summSumRaw"  = list( rng=c(min=0,max=7)      ,evt=matrix( c(c(1),c(-1)) ,ncol=2)  )
             ,"summSumEvt"  = list( rng=c(min=0,max=1)      ,evt=matrix( c(c(1),c(-1)) ,ncol=2)  )
             ,"summSumOthRaw"   = list( rng=c(min=0,max=2)  ,evt=matrix( c(c(1),c(-1)) ,ncol=2)  )
             ,"summSumOthEvt"   = list( rng=c(min=0,max=1)  ,evt=matrix( c(c(1),c(-1)) ,ncol=2)  )
             ,"summSumRebRaw"   = list( rng=c(min=0,max=1)  ,evt=matrix( c(c(1),c(-1)) ,ncol=2)  )
             ,"summSumRebEvt"   = list( rng=c(min=0,max=1)  ,evt=matrix( c(c(1),c(-1)) ,ncol=2)  )
             ,"summSumRebOthRaw"= list( rng=c(min=0,max=1)  ,evt=matrix( c(c(1),c(-1)) ,ncol=2)  )
             ,"summSumRebOthEvt"= list( rng=c(min=0,max=1)  ,evt=matrix( c(c(1),c(-1)) ,ncol=2)  )
             ,"szSumRebCnt" = list( rng=c(min=0,max=9)      ,evt=matrix( c(c(1),c(-1)) ,ncol=2)  )
             ,"szSumRebDup" = list( rng=c(min=0,max=2)      ,evt=matrix( c(c(1),c(-1)) ,ncol=2)  )
        )
        ,evtMax = c("lev1"=3,"lev2"=2,"lev3"=1)
    )
}

crMName <- "bSMScr05SumClM"  # QQE:Todo
if( TRUE ){
    bSMtxMCfg[[crMName]] <- list(  mName=crMName ,ver=2.0
        ,fCol = list(
              "sumTotHpn" = list( rng=c(min=0,max= 4)   ,evt=matrix( c(c(1),c(-1)) ,ncol=2)  )
             ,"sumTot1"   = list( rng=c(min=0,max= 4)   ,evt=matrix( c(c(1),c(-1)) ,ncol=2)  )
             ,"sumTot2"   = list( rng=c(min=0,max= 4)   ,evt=matrix( c(c(4),c(-1)) ,ncol=2)  )
             ,"sumTot3"   = list( rng=c(min=0,max= 2)   ,evt=matrix( c(c(1),c(-1)) ,ncol=2)  )
        )
        ,evtMax = c("lev1"=3,"lev2"=2,"lev3"=1)
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

