bsScoreMtxCfg <- list()
if( FALSE ){    # Document
    # bsScoreMtxCfg$fCol
    #   - 일단 Lev1, Lev2를 남겨놓고 Lev2만 사용한다. 나중에 필요해질 수도 있을 듯.

    # mName <- "template"
    # bsScoreMtxCfg[[mName]] <- list(
    #     mName = mName   ,style=c( freqZero=TRUE )
    #     ,fCol = list(
    #         "xxx"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
    #                         ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
    #                         ,evtMax.fCol=NULL
    #                     ) 
    #         ,"xxx"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
    #                         ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
    #                         ,evtMax.fCol=NULL
    #                     )
    #     )
    #     ,evtMax = NULL  ,evtMaxFColTot = NULL
    #     ,rowReb = NULL  ,rowRebDup=NULL
    #     ,summMtx = NULL ,summMtx.reb = NULL ,summMtx.sum = NULL
    #     ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
    #     ,isHard=NULL  # use default
    # )

}

summMtxName <- list( cName=c( "all" ,"ph" ,"fCol" ,"phReb" ,"xyCnt.fCol" ,"xyCnt.phase" ) 
                    ,rName=c( "raw" ,"evt" )
)
summMtx.rebName <- list( cName=c( "all" ,"ph" ,"fCol" ,"phReb" ,"xyCnt.fCol" ,"xyCnt.phase" )
                    ,rName=c( "raw" ,"evt" )
)
scMtx.szName <- list(   cName=c( "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg" )
                        ,rName=c( "rebCnt" ,"rebDup" )
)

mName <- "sScore01"
if( TRUE ){
    bsScoreMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "rem0.num"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol <- c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
                        )
            ,"rem0.len.tot"=list( rng=matrix( c(0,1 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol = c( lev1Max=3 ,lev2Max=3 ,lev3Max=2 )
                            ,freqVal=c(1,2)
                        )
            ,"rem0.len.val"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL
                        )
            ,"rem1.num"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL
                        )
            ,"rem1.len.tot"=list( rng=matrix( c(0,1 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol = c( lev1Max=3 ,lev2Max=3 ,lev3Max=2 )
                            ,freqVal=c(1,2)
                        )
            ,"rem1.len.val"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL
                        )
            ,"c0.num"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL
                        )
            ,"c0.len.tot"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol = c( lev1Max=3 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"c0.len.val"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL
                        )
            ,"c1.num"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL
                        )
            ,"c1.len.tot"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol = c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                            ,forbidEvtReb = c(3,4)
                        )
            ,"c1.len.val"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(2,3,4,5)) ,ncol=2)  # H848
                            ,evtMax.fCol=NULL
                            ,forbidEvtReb = c(2,3,4)
                        )
            ,"f0.num"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL
                        )
            ,"f0.len.tot"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL
                        )
            ,"f0.len.val"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL
                        )
            ,"f1.num"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL
                        )
            ,"f1.len.tot"=list( rng=matrix( c(0,1 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL
                        )
            ,"f1.len.val"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL
                        )
        )
        ,evtMax = NULL  ,evtMaxFColTot = NULL
        ,rowReb = c( rawMin=3 ,lowE=3 ,rareE=1 ,dupESum=1 )    ,rowRebDup=NULL
        ,summMtx = matrix(  # all ph fCol phReb xyCnt.fCol xyCnt.phase      / raw ,evt
                            c(  1 ,3 ,2 ,2 ,2 ,2    ,1 ,2 ,2 ,2 ,2 ,2  ) ,byrow=T
                        ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                        ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
        ,summMtx.reb = NULL
        ,summMtx.sum = c(raw=3 ,evt=3)
        ,scMtx.sz = matrix( # r.ph r.fCol r.dblHpnFlg e.ph e.fCol e.dblHpnFlg  /  rebCnt ,rebDup
                            c(   4 ,2 ,1 ,2 ,2 ,1   ,3 ,1 ,1 ,1 ,1 ,1  )
                        ,byrow=T
                        ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                        ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
        )
        ,scMtx.sz.sum = c(rebCnt.r=4 ,rebCnt.e=3)
        ,isHard=NULL  # use default
    )
}

mName <- "sScore02"
bsScoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "rebC.r"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)  
                        ,freqVal=1      ,forbidEvtReb=c(3)
                        ,evtMax.fCol=NULL       # H881 fCol EvtCnt4AllPh(lev2ClM) rebC.r:3
                    ) 
        ,"rebC.c"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3),c(3,4,5)) ,ncol=2)  ,evtMax.fCol=NULL
                        ,freqVal=1
                    )
        ,"rebC.f"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)  ,evtMax.fCol=NULL
                        ,freqVal=1
                    )
        ,"rebC2.r"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c(lev1Max=5,lev2Max=3,lev3Max=2)   # H795 lev1ClM(4)
                        ,freqVal=1
                    )
        ,"rebC2.c"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3),c(3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c(lev1Max=5,lev2Max=3,lev3Max=2)
                        ,freqVal=1
                    )
        ,"rebC2.f"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)  ,evtMax.fCol=NULL
                        ,freqVal=1
                    )
        ,"inc.r"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)  ,evtMax.fCol=NULL
                        ,freqVal=1
                    )
        ,"inc.c"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)  ,evtMax.fCol=NULL
                        ,freqVal=1
                    )
        ,"inc.f"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)  ,evtMax.fCol=NULL
                        ,freqVal=1
                    )
        ,"inc.r2"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c(lev1Max=5,lev2Max=3,lev3Max=2)
                        ,freqVal=1
                    )
        ,"inc.c2"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c(lev1Max=5,lev2Max=3,lev3Max=2)
                        ,freqVal=1
                    )
        ,"inc.f2"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)  ,evtMax.fCol=NULL
                        ,freqVal=1
                    )
    )
    ,evtMax = matrix( c(2,2,3,1 ,2,3,3,2)   ,byrow=T ,ncol=4
                ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
    )
    ,evtMaxFColTot = NULL
    ,rowReb = c( rawMin=1 ,lowE=2 ,rareE=2 ,dupESum=1 )    ,rowRebDup=NULL
    ,summMtx = matrix(  # all ph fCol phReb xyCnt.fCol xyCnt.phase      / raw ,evt
                        c(  1 ,3 ,4 ,1 ,1 ,1    ,1 ,2 ,2 ,1 ,2 ,2  ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,summMtx.reb = matrix(  # all ph fCol phReb xyCnt.fCol xyCnt.phase      / raw ,evt
                        c(  1 ,1 ,2 ,1 ,1 ,1    ,1 ,1 ,1 ,1 ,1 ,1  ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,summMtx.sum = c(raw=4 ,evt=3)
    ,scMtx.sz = matrix( # r.ph r.fCol r.dblHpnFlg e.ph e.fCol e.dblHpnFlg  /  rebCnt ,rebDup
                        c(  3 ,4 ,1 ,3 ,3 ,1   ,2 ,2 ,1 ,1 ,1 ,1  )
                    ,byrow=T
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
    )
    ,scMtx.sz.sum = c(rebCnt.r=4 ,rebCnt.e=4)
    ,isHard=NULL  # use default
)

mName <- "sScore03"
bsScoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "rebPtn.1"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    ) 
        ,"rebPtn.n"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"snR3"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                        ,evt=matrix( c(c(3),c(1)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"snMax.r"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"snFCnt.r"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"snMax.c"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"snFCnt.c"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"snMax.f"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"snFCnt.f"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
    )
    ,evtMax = NULL    ,evtMaxFColTot = NULL
    ,rowReb = c( rawMin=1 ,lowE=2 ,rareE=1 ,dupESum=1 )    ,rowRebDup=NULL
    ,summMtx = matrix(  # all ph fCol phReb xyCnt.fCol xyCnt.phase      / raw ,evt
                        c(  1 ,2 ,2 ,1 ,2 ,2    ,1 ,2 ,2 ,1 ,2 ,2  ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,summMtx.sum = c(raw=2 ,evt=2)
    ,summMtx.reb = matrix(  # all ph fCol phReb xyCnt.fCol xyCnt.phase      / raw ,evt
                        c(  1 ,1 ,1 ,1 ,2 ,2    ,1 ,1 ,1 ,1 ,1 ,1  ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,scMtx.sz = matrix( c( 3 ,3 ,1 ,2 ,2 ,1     ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                        #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                        #   "rebCnt" ,"rebDup"
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
    ,scMtx.sz.sum = c(rebCnt.r=3 ,rebCnt.e=3)
    ,isHard=NULL  # use default
)

mName <- "sScore04"
bsScoreMtxCfg[[mName]] <- list(
    # sScore04는 evt는 커녕, hpn자체도 드물다. 
    #   bS_stdCut.hIdx() 에서 hpn 수를 가지고 필터링 조건을 추가해 줄 필요가 있음.
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "pBanN.r"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"pBanN.n"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pLCol"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pE3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pE4"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pMH"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pfNum"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"iBanN"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"iLCol"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )   # H864 ,H880 검토 요.
                    )
        ,"iE3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"iE4"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"iMH"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"ifNum"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"FVa.m"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"FVa.c"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"m4"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
                    )
    )
    ,evtMax = NULL    ,evtMaxFColTot = NULL
    ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 )    
    ,rowRebDup=NULL
    ,summMtx = NULL   ,summMtx.sum = NULL    ,summMtx.reb = NULL
    ,scMtx.sz = matrix( c( 2 ,2 ,1 ,2 ,2 ,1     ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                        #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                        #   "rebCnt" ,"rebDup"
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
    ,scMtx.sz.sum = c(rebCnt.r=2 ,rebCnt.e=2)
    ,isHard=NULL  # use default
)

mName <- "sScore05"
bsScoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "pBanN.r"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                        ,freqVal=c(1)
                    ) 
        ,"pBanN.n"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"pLCol"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                        ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    )
        ,"pE3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"pE4"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"pMH"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"pfNum"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"iBanN"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        # ,freqVal=c(1)     # H867
                        ,evtMax.fCol=NULL
                    )
        ,"iLCol"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"iE3"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"iE4"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"iMH"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"ifNum"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"FVa.m"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(1,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,freqVal=c(1,2)
                        ,evtMax.fCol=NULL
                    )
        ,"FVa.c"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"m4"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
    )
    ,evtMax = NULL    ,evtMaxFColTot = NULL
    # ,rowReb =c( rawMin=3,lowE=2,rareE=1 ,dupESum=1 )    ,rowRebDup=NULL
    ,rowReb =c( rawMin=1,lowE=2,rareE=1 ,dupESum=1 )    ,rowRebDup=NULL
    ,summMtx = matrix(  # all ph fCol phReb xyCnt.fCol xyCnt.phase      / raw ,evt
                        c(  1 ,3 ,3 ,1 ,2 ,2    ,1 ,2 ,2 ,1 ,2 ,2  ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,summMtx.sum = c(raw=4 ,evt=2)
    ,summMtx.reb = matrix(  # all ph fCol phReb xyCnt.fCol xyCnt.phase      / raw ,evt
                        c(  1 ,1 ,1 ,1 ,1 ,1    ,1 ,1 ,1 ,1 ,2 ,2  ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,scMtx.sz = matrix( c( 3 ,3 ,1 ,3 ,3 ,1     ,2 ,2 ,1 ,2 ,2 ,1 ) ,byrow=T
                        #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                        #   "rebCnt" ,"rebDup"
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
    ,scMtx.sz.sum = c(rebCnt.r=5 ,rebCnt.e=3)
    ,isHard=NULL  # use default
)

mName <- "sScore06"
bsScoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "pBanN.r"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    ) 
        ,"pBanN.n"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"pLCol"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"pE3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"pE4"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"pMH"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"pfNum"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"iBanN"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"iLCol"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"iE3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"iE4"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"iMH"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"ifNum"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"FVa.m"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"FVa.c"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"m4"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
    )
    ,evtMax = NULL    ,evtMaxFColTot = NULL
    # ,rowReb = c( rawMin=1,lowE=2,rareE=1 ,dupESum=1 )
    ,rowReb = c( rawMin=1,lowE=1,rareE=1 ,dupESum=1 )
    ,rowRebDup=NULL
    ,summMtx = NULL   ,summMtx.sum = NULL    ,summMtx.reb = NULL
    ,scMtx.sz = matrix( c( 3 ,3 ,1 ,2 ,2 ,1     ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                        #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                        #   "rebCnt" ,"rebDup"
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
    ,scMtx.sz.sum = c(rebCnt.r=3 ,rebCnt.e=2)
    ,isHard=NULL  # use default
)

mName <- "sScore07"
bsScoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "pBanN.r"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    ) 
        ,"pBanN.n"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"pLCol"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"pE3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"pE4"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"pMH"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"pfNum"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"iBanN"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"iLCol"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"iE3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"iE4"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"iMH"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"ifNum"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"FVa.m"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"FVa.c"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"m4"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
    )
    ,evtMax = NULL    ,evtMaxFColTot = NULL
    ,rowReb = c( rawMin=1,lowE=1,rareE=1 ,dupESum=1 )
    ,rowRebDup=NULL
    ,summMtx = NULL   ,summMtx.sum = NULL    ,summMtx.reb = NULL
    ,scMtx.sz = matrix( c( 2 ,2 ,1 ,2 ,2 ,1     ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                        #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                        #   "rebCnt" ,"rebDup"
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
    ,scMtx.sz.sum = c(rebCnt.r=2 ,rebCnt.e=2)
    ,isHard=NULL  # use default
)

mName <- "sScore08"
bsScoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "max3"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    ) 
        ,"min3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"max2MatCnt"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"min2MatCnt"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"minMax2MatCnt"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                        ,evt=matrix( c(c(3),c(5)) ,ncol=2)  # extention으로 뺄까?
                        ,evtMax.fCol=NULL
                    )
        ,"cTbl"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"fTbl"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
    )
    ,evtMax = NULL    ,evtMaxFColTot = NULL
    ,rowReb = c( rawMin=1,lowE=1,rareE=1 ,dupESum=1 )
    ,rowRebDup=NULL
    ,summMtx = matrix(  # all ph fCol phReb xyCnt.fCol xyCnt.phase      / raw ,evt
                        c(  1 ,2 ,2 ,1 ,2 ,2    ,1 ,2 ,2 ,1 ,1 ,1  ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,summMtx.sum = NULL    ,summMtx.reb = NULL
    ,scMtx.sz = matrix( c( 3 ,2 ,1 ,1 ,1 ,1     ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                        #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                        #   "rebCnt" ,"rebDup"
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
    ,scMtx.sz.sum = c(rebCnt.r=4 ,rebCnt.e=2)
    ,isHard=NULL  # use default
)

mName <- "sScore09"
bsScoreMtxCfg[[mName]] <- list(
    #   rCnt, eCnt, cCnt, fCnt 에서 1 값은 생략되어 있다. 
    #   1 생략 철회 및 관련 cut cfg 업데이트 적용 필요.
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "rCnt"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                        ,freqVal=1
                    ) 
        ,"rD2"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"rDn"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"rLr"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"rRl"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"eCnt"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(4,5,6)) ,ncol=2)
                        ,evtMax.fCol=NULL
                        ,freqVal=c(1,2)
                    ) 
        ,"eD2"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(1,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                        ,freqVal=1
                    )
        ,"eDn"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                        ,freqVal=c(1)
                    )
        ,"eLr"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                        ,freqVal=c(1)
                    )
        ,"eRl"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                        ,freqVal=c(1)
                    )
        ,"cCnt"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(4,5,6)) ,ncol=2)
                        ,evtMax.fCol=NULL
                        ,freqVal=c(1,2)
                    ) 
        ,"cD2"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                        ,freqVal=c(1)
                    )
        ,"cDn"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"cLr"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                        ,freqVal=c(1)
                    )
        ,"cRl"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                        ,freqVal=c(1)
                    )
        ,"fCnt"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                        ,freqVal=1
                    ) 
        ,"fD2"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"fDn"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"fLr"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"fRl"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
    )
    ,evtMax = NULL    ,evtMaxFColTot = NULL
    # ,rowReb =c( rawMin=3,lowE=2,rareE=1,dupESum=1 )
    ,rowReb =c( rawMin=1,lowE=1,rareE=1,dupESum=1 ) 
    ,rowRebDup=NULL
    ,summMtx = matrix(  # all ph fCol phReb xyCnt.fCol xyCnt.phase      / raw ,evt
                        c(  1 ,3 ,2 ,1 ,1 ,1    ,1 ,1 ,1 ,1 ,2 ,1  ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,summMtx.sum = c(raw=4 ,evt=2)
    ,summMtx.reb =  matrix(  # all ph fCol phReb xyCnt.fCol xyCnt.phase      / raw ,evt
                        c(  1 ,2 ,2 ,1 ,1 ,1    ,1 ,1 ,1 ,1 ,1 ,1  ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,scMtx.sz = matrix( c( 3 ,2 ,1 ,2 ,2 ,1     ,2 ,2 ,1 ,1 ,1 ,1 ) ,byrow=T
                        #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                        #   "rebCnt" ,"rebDup"
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
    ,scMtx.sz.sum = c(rebCnt.r=4 ,rebCnt.e=2)
    ,isHard=NULL  # use default
)

mName <- "sScore0LAr13"
bsScoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "colA1"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    ) 
        ,"colA2"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA3"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA4"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA5"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA6"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB1"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    ) 
        ,"colB2"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB3"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB4"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB5"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB6"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
    )
    ,evtMax = NULL    ,evtMaxFColTot = NULL
    ,rowReb =c( rawMin=1,lowE=1,rareE=1,dupESum=1 )   ,rowRebDup=NULL
    ,summMtx = matrix(  # all ph fCol phReb xyCnt.fCol xyCnt.phase      / raw ,evt
                        c(  1 ,2 ,2 ,1 ,2 ,2    ,1 ,1 ,1 ,1 ,1 ,1  ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,summMtx.sum = c(raw=2 ,evt=2)
    ,summMtx.reb = matrix( c( 1 ,1 ,1 ,1 ,2 ,2   ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName) 
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
                )
    ,scMtx.sz = matrix( c( 2 ,2 ,1 ,1 ,1 ,1     ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                        #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                        #   "rebCnt" ,"rebDup"
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
    ,scMtx.sz.sum = c(rebCnt.r=3 ,rebCnt.e=2)
    ,isHard=NULL  # use default
)

mName <- "sScore0LAr24"
bsScoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "colA1"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    ) 
        ,"colA2"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA3"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA4"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA5"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA6"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB1"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    ) 
        ,"colB2"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB3"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB4"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB5"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB6"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
    )
    ,evtMax = NULL    ,evtMaxFColTot = NULL
    ,rowReb =c( rawMin=1,lowE=1,rareE=1,dupESum=1 )   ,rowRebDup=NULL
    ,summMtx = matrix(  # all ph fCol phReb xyCnt.fCol xyCnt.phase      / raw ,evt
                        c(  1 ,2 ,2 ,1 ,2 ,2    ,1 ,1 ,1 ,1 ,1 ,1  ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,summMtx.sum = c(raw=2 ,evt=2)
    ,summMtx.reb = matrix( c( 1 ,1 ,1 ,1 ,2 ,2   ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName) 
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
                )
    ,scMtx.sz = matrix( c( 2 ,2 ,1 ,1 ,1 ,1     ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                        #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                        #   "rebCnt" ,"rebDup"
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
    ,scMtx.sz.sum = c(rebCnt.r=3 ,rebCnt.e=2)
    ,isHard=NULL  # use default
)

mName <- "sScore0LVr13"
bsScoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "colA1"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    ) 
        ,"colA2"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA3"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA4"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA5"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA6"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB1"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    ) 
        ,"colB2"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB3"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB4"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB5"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB6"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
    )
    ,evtMax = NULL    ,evtMaxFColTot = NULL
    ,rowReb =c( rawMin=1,lowE=1,rareE=1,dupESum=1 )   ,rowRebDup=NULL
    ,summMtx = matrix(  # all ph fCol phReb xyCnt.fCol xyCnt.phase      / raw ,evt
                        c(  1 ,2 ,2 ,1 ,2 ,2    ,1 ,1 ,1 ,1 ,1 ,1  ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,summMtx.sum = c(raw=2 ,evt=2)
    ,summMtx.reb = matrix( c( 1 ,1 ,1 ,1 ,2 ,2   ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName) 
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
                )
    ,scMtx.sz = matrix( c( 2 ,2 ,1 ,1 ,1 ,1     ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                        #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                        #   "rebCnt" ,"rebDup"
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
    ,scMtx.sz.sum = c(rebCnt.r=3 ,rebCnt.e=2)
    ,isHard=NULL  # use default
)

mName <- "sScore0LVr24"
bsScoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "colA1"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    ) 
        ,"colA2"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA3"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA4"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA5"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA6"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB1"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    ) 
        ,"colB2"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB3"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB4"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB5"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB6"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
    )
    ,evtMax = NULL    ,evtMaxFColTot = NULL
    ,rowReb =c( rawMin=1,lowE=1,rareE=1,dupESum=1 )   ,rowRebDup=NULL
    ,summMtx = matrix(  # all ph fCol phReb xyCnt.fCol xyCnt.phase      / raw ,evt
                        c(  1 ,2 ,2 ,1 ,2 ,2    ,1 ,1 ,1 ,1 ,1 ,1  ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,summMtx.sum = c(raw=2 ,evt=2)
    ,summMtx.reb = matrix( c( 1 ,1 ,1 ,1 ,2 ,2   ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName) 
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
                )
    ,scMtx.sz = matrix( c( 2 ,2 ,1 ,1 ,1 ,1     ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                        #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                        #   "rebCnt" ,"rebDup"
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
    ,scMtx.sz.sum = c(rebCnt.r=3 ,rebCnt.e=2)
    ,isHard=NULL  # use default
)

#   873, 880, 889에서 colA13,colB13  에서 3이 발생된 적이 있긴 하다..
mName <- "sScore0LAe13"
bsScoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "colA1"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    ) 
        ,"colA2"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA3"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA4"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA5"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA6"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB1"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    ) 
        ,"colB2"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB3"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB4"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB5"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB6"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
    )
    ,evtMax = NULL    ,evtMaxFColTot = NULL
    ,rowReb =c( rawMin=2,lowE=1,rareE=1,dupESum=1 )   ,rowRebDup=NULL
    ,summMtx = matrix(  # all ph fCol phReb xyCnt.fCol xyCnt.phase      / raw ,evt
                        c(  1 ,2 ,2 ,1 ,2 ,2    ,1 ,1 ,1 ,1 ,2 ,2  ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,summMtx.sum = c(raw=3 ,evt=2)
    ,summMtx.reb = NULL
    ,scMtx.sz = matrix( c( 3 ,3 ,1 ,2 ,2 ,1     ,2 ,2 ,1 ,1 ,1 ,1 ) ,byrow=T
                        #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                        #   "rebCnt" ,"rebDup"
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
    ,scMtx.sz.sum = c(rebCnt.r=4 ,rebCnt.e=3)
    ,isHard=NULL  # use default
)

mName <- "sScore0LAe24"
bsScoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "colA1"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    ) 
        ,"colA2"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA3"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA4"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA5"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA6"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB1"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    ) 
        ,"colB2"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB3"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB4"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB5"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB6"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
    )
    ,evtMax = NULL    ,evtMaxFColTot = NULL
    ,rowReb =c( rawMin=2,lowE=1,rareE=1,dupESum=1 )   ,rowRebDup=NULL
    ,summMtx = matrix(  # all ph fCol phReb xyCnt.fCol xyCnt.phase      / raw ,evt
                        c(  1 ,2 ,2 ,1 ,2 ,2    ,2 ,2 ,1 ,1 ,2 ,2  ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,summMtx.sum = c(raw=3 ,evt=2)
    ,summMtx.reb = NULL
    ,scMtx.sz = matrix( c( 3 ,3 ,1 ,2 ,2 ,1     ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                        #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                        #   "rebCnt" ,"rebDup"
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
    ,scMtx.sz.sum = c(rebCnt.r=4 ,rebCnt.e=3)
    ,isHard=NULL  # use default
)

mName <- "sScore0LVe13"
bsScoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "colA1"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    ) 
        ,"colA2"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA3"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA4"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA5"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA6"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB1"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    ) 
        ,"colB2"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB3"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB4"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB5"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB6"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
    )
    ,evtMax = NULL    ,evtMaxFColTot = NULL
    ,rowReb =c( rawMin=2,lowE=1,rareE=1,dupESum=1 )   ,rowRebDup=NULL
    ,summMtx = matrix(  # all ph fCol phReb xyCnt.fCol xyCnt.phase      / raw ,evt
                        c(  1 ,2 ,2 ,1 ,2 ,2    ,2 ,2 ,1 ,1 ,2 ,2  ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,summMtx.sum = c(raw=3 ,evt=2)
    ,summMtx.reb = NULL
    ,scMtx.sz = matrix( c( 3 ,3 ,1 ,2 ,2 ,1     ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                        #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                        #   "rebCnt" ,"rebDup"
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
    ,scMtx.sz.sum = c(rebCnt.r=4 ,rebCnt.e=3)
    ,isHard=NULL  # use default
)

mName <- "sScore0LVe24"
bsScoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "colA1"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    ) 
        ,"colA2"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA3"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA4"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA5"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA6"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB1"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    ) 
        ,"colB2"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB3"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB4"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB5"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB6"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
    )
    ,evtMax = NULL    ,evtMaxFColTot = NULL
    ,rowReb =c( rawMin=2,lowE=2,rareE=1,dupESum=1 )   ,rowRebDup=NULL
    ,summMtx = matrix(  # all ph fCol phReb xyCnt.fCol xyCnt.phase      / raw ,evt
                        c(  1 ,2 ,2 ,1 ,2 ,2    ,2 ,2 ,1 ,1 ,2 ,2  ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,summMtx.sum = c(raw=3 ,evt=2)
    ,summMtx.reb = NULL
    ,scMtx.sz = matrix( c( 3 ,3 ,1 ,2 ,2 ,1     ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                        #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                        #   "rebCnt" ,"rebDup"
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
    ,scMtx.sz.sum = c(rebCnt.r=4 ,rebCnt.e=3)
    ,isHard=NULL  # use default
)

mName <- "sScore0LAc13"
bsScoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "colA1"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    ) 
        ,"colA2"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    )
        ,"colA3"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    )
        ,"colA4"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    )
        ,"colA5"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    )
        ,"colA6"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    )
        ,"colB1"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    ) 
        ,"colB2"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    )
        ,"colB3"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    )
        ,"colB4"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    )
        ,"colB5"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    )
        ,"colB6"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    )
    )
    ,evtMax = NULL    ,evtMaxFColTot = NULL
    ,rowReb =c( rawMin=1,lowE=1,rareE=1,dupESum=1 )   ,rowRebDup=NULL
    ,summMtx = matrix(  # all ph fCol phReb xyCnt.fCol xyCnt.phase      / raw ,evt
                        c(  1 ,2 ,2 ,1 ,2 ,2    ,1 ,1 ,1 ,1 ,1 ,1  ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,summMtx.sum = c(raw=3 ,evt=2)
    ,summMtx.reb = NULL
    ,scMtx.sz = matrix( c( 2 ,2 ,1 ,1 ,1 ,1     ,2 ,2 ,1 ,1 ,1 ,1 ) ,byrow=T
                        #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                        #   "rebCnt" ,"rebDup"
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
    ,scMtx.sz.sum = c(rebCnt.r=3 ,rebCnt.e=2)
    ,isHard=NULL  # use default
)

mName <- "sScore0LAc24"
bsScoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "colA1"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    ) 
        ,"colA2"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    )
        ,"colA3"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    )
        ,"colA4"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    )
        ,"colA5"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    )
        ,"colA6"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    )
        ,"colB1"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    ) 
        ,"colB2"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    )
        ,"colB3"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    )
        ,"colB4"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    )
        ,"colB5"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    )
        ,"colB6"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    )
    )
    ,evtMax = NULL    ,evtMaxFColTot = NULL
    ,rowReb =c( rawMin=1,lowE=1,rareE=1,dupESum=1 )   ,rowRebDup=NULL
    ,summMtx = matrix(  # all ph fCol phReb xyCnt.fCol xyCnt.phase      / raw ,evt
                        c(  1 ,2 ,2 ,1 ,2 ,2    ,1 ,1 ,1 ,1 ,1 ,1  ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,summMtx.sum = c(raw=3 ,evt=2)
    ,summMtx.reb = NULL
    ,scMtx.sz = matrix( c( 2 ,2 ,1 ,1 ,1 ,1     ,2 ,2 ,1 ,1 ,1 ,1 ) ,byrow=T
                        #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                        #   "rebCnt" ,"rebDup"
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
    ,scMtx.sz.sum = c(rebCnt.r=3 ,rebCnt.e=2)
    ,isHard=NULL  # use default
)

mName <- "sScore0LVc13"
bsScoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "colA1"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    ) 
        ,"colA2"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    )
        ,"colA3"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    )
        ,"colA4"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    )
        ,"colA5"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    )
        ,"colA6"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    )
        ,"colB1"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    ) 
        ,"colB2"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    )
        ,"colB3"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    )
        ,"colB4"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    )
        ,"colB5"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    )
        ,"colB6"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    )
    )
    ,evtMax = NULL    ,evtMaxFColTot = NULL
    ,rowReb =c( rawMin=1,lowE=1,rareE=1,dupESum=1 )   ,rowRebDup=NULL
    ,summMtx = matrix(  # all ph fCol phReb xyCnt.fCol xyCnt.phase      / raw ,evt
                        c(  1 ,2 ,2 ,1 ,2 ,2    ,1 ,1 ,1 ,1 ,1 ,1  ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,summMtx.sum = c(raw=3 ,evt=2)
    ,summMtx.reb = NULL
    ,scMtx.sz = matrix( c( 2 ,2 ,1 ,1 ,1 ,1     ,2 ,2 ,1 ,1 ,1 ,1 ) ,byrow=T
                        #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                        #   "rebCnt" ,"rebDup"
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
    ,scMtx.sz.sum = c(rebCnt.r=3 ,rebCnt.e=2)
    ,isHard=NULL  # use default
)

mName <- "sScore0LVc24"
bsScoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "colA1"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    ) 
        ,"colA2"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    )
        ,"colA3"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    )
        ,"colA4"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    )
        ,"colA5"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    )
        ,"colA6"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    )
        ,"colB1"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    ) 
        ,"colB2"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    )
        ,"colB3"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    )
        ,"colB4"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    )
        ,"colB5"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    )
        ,"colB6"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2) ,freqVal=c(1)
                        ,evtMax.fCol=NULL
                    )
    )
    ,evtMax = NULL    ,evtMaxFColTot = NULL
    ,rowReb =c( rawMin=1,lowE=1,rareE=1,dupESum=1 )   ,rowRebDup=NULL
    ,summMtx = matrix(  # all ph fCol phReb xyCnt.fCol xyCnt.phase      / raw ,evt
                        c(  1 ,2 ,2 ,1 ,2 ,2    ,1 ,1 ,1 ,1 ,1 ,1  ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,summMtx.sum = c(raw=3 ,evt=2)
    ,summMtx.reb = NULL
    ,scMtx.sz = matrix( c( 2 ,2 ,1 ,1 ,1 ,1     ,2 ,2 ,1 ,1 ,1 ,1 ) ,byrow=T
                        #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                        #   "rebCnt" ,"rebDup"
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
    ,scMtx.sz.sum = c(rebCnt.r=3 ,rebCnt.e=2)
    ,isHard=NULL  # use default
)

mName <- "sScore0LAf13"
bsScoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "colA1"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    ) 
        ,"colA2"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA3"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA4"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA5"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA6"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB1"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    ) 
        ,"colB2"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB3"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB4"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB5"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB6"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
    )
    ,evtMax = NULL    ,evtMaxFColTot = NULL
    ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 )             ,rowRebDup=NULL
    ,summMtx = matrix( c(   1 ,1 ,1 ,1 ,2 ,2  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
            ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
            ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
    ,summMtx.reb = NULL     ,summMtx.sum = NULL
    ,scMtx.sz = matrix( c( 2 ,2 ,1 ,1 ,1 ,1  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                # c( "rebCnt" ,"rebDup" ) / c( "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg" )
                ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
            )
    ,scMtx.sz.sum = c(rebCnt.r=3 ,rebCnt.e=2)
    ,isHard=NULL  # use default
)

mName <- "sScore0LAf24"
bsScoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "colA1"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    ) 
        ,"colA2"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA3"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA4"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA5"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA6"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB1"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    ) 
        ,"colB2"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB3"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB4"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB5"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB6"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
    )
    ,evtMax = NULL    ,evtMaxFColTot = NULL
    ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 )             ,rowRebDup=NULL
    ,summMtx = matrix( c(   1 ,1 ,1 ,1 ,2 ,2  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
            ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
            ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
    ,summMtx.reb = NULL     ,summMtx.sum = NULL
    ,scMtx.sz = matrix( c( 2 ,2 ,1 ,1 ,1 ,1  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                # c( "rebCnt" ,"rebDup" ) / c( "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg" )
                ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
            )
    ,scMtx.sz.sum = c(rebCnt.r=3 ,rebCnt.e=2)
    ,isHard=NULL  # use default
)

mName <- "sScore0LVf13"
bsScoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "colA1"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    ) 
        ,"colA2"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA3"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA4"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA5"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA6"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB1"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    ) 
        ,"colB2"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB3"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB4"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB5"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB6"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
    )
    ,evtMax = NULL    ,evtMaxFColTot = NULL
    ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 )             ,rowRebDup=NULL
    ,summMtx = matrix( c(   1 ,1 ,1 ,1 ,2 ,2  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
            ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
            ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
    ,summMtx.reb = NULL     ,summMtx.sum = NULL
    ,scMtx.sz = matrix( c( 2 ,2 ,1 ,1 ,1 ,1  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                # c( "rebCnt" ,"rebDup" ) / c( "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg" )
                ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
            )
    ,scMtx.sz.sum = c(rebCnt.r=3 ,rebCnt.e=2)
    ,isHard=NULL  # use default
)

mName <- "sScore0LVf24"
bsScoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "colA1"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    ) 
        ,"colA2"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA3"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA4"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA5"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colA6"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB1"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    ) 
        ,"colB2"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB3"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB4"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB5"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"colB6"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
    )
    ,evtMax = NULL    ,evtMaxFColTot = NULL
    ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 )             ,rowRebDup=NULL
    ,summMtx = matrix( c(   1 ,1 ,1 ,1 ,2 ,2  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
            ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
            ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
    ,summMtx.reb = NULL     ,summMtx.sum = NULL
    ,scMtx.sz = matrix( c( 2 ,2 ,1 ,1 ,1 ,1  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                # c( "rebCnt" ,"rebDup" ) / c( "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg" )
                ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
            )
    ,scMtx.sz.sum = c(rebCnt.r=3 ,rebCnt.e=2)
    ,isHard=NULL  # use default
)



mName <- "sScore0GS"
bsScoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
            "rMatCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"rExtMax"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"rSumCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"rValCnt"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            # rem은 rng,evt 적용의미 없음. 오로지 reb 활용목적으로만 쓸 것.
            ,"eMatCnt"=list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3),c(6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"eExtMax"=list( rng=matrix( c(0,4 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(3),c(5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"eSumCnt"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"eValCnt"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"cMatCnt"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"cExtMax"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"cSumCnt"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"cValCnt"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"fMatCnt"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"fExtMax"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"fSumCnt"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"fValCnt"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
    )
    ,evtMax = NULL    ,evtMaxFColTot = NULL
    ,rowReb = c( rawMin=3 ,lowE=1 ,rareE=1 ,dupESum=1 )      ,rowRebDup=NULL
    ,summMtx = matrix( c( 1 ,2 ,3 ,1 ,2 ,2  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
            ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
            ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,summMtx.sum = c(raw=4 ,evt=1)
    ,summMtx.reb = matrix( c( 1 ,1 ,1 ,1 ,2 ,1  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
            ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
            ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,scMtx.sz = matrix( c( 3 ,3 ,1 ,1 ,1 ,1  ,2 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                # c( "rebCnt" ,"rebDup" ) / c( "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg" )
                ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
            )
    ,scMtx.sz.sum = c(rebCnt.r=4 ,rebCnt.e=1)
    ,isHard=NULL  # use default
)
mName <- "sScore0GSh2"
bsScoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "rMatCnt"=list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(2,2,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        ,freqVal=c(1)
                    )
        ,"rExtMax"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        ,freqVal=c(1)
                    )
        ,"rSumCnt"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"rValCnt"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                    )
        # rem은 rng,evt 적용의미 없음. 오로지 reb 활용목적으로만 쓸 것.
        ,"eMatCnt"=list( rng=matrix( c(0,4 ,0,4) ,ncol=2 )
                        ,evt=matrix( c(c(3),c(6)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        ,freqVal=c(1)
                    )
        ,"eExtMax"=list( rng=matrix( c(0,4 ,0,4) ,ncol=2 )
                        ,evt=matrix( c(c(3),c(5)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        ,freqVal=c(1,2)
                    )
        ,"eSumCnt"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3),c(5)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        ,freqVal=c(1)   # scoreGSh2에서는 나름 빈도있게 발생함.
                    )
        ,"eValCnt"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol= c( lev1Max=4 ,lev2Max=3 ,lev3Max=3 )
                    )
        ,"cMatCnt"=list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        ,freqVal=c(1)
                    )
        ,"cExtMax"=list( rng=matrix( c(0,4 ,0,4) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,2,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        ,freqVal=c(1)
                    )
        ,"cSumCnt"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"cValCnt"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"fMatCnt"=list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        ,freqVal=c(1)
                    )
        ,"fExtMax"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(1,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        ,freqVal=c(1)
                    )
        ,"fSumCnt"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"fValCnt"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                    )
    )
    ,evtMax = matrix( c(2,4,3,2 ,2,4,3,2)   ,byrow=T ,ncol=4
                ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
    )
    ,evtMaxFColTot = NULL
    ,rowReb = c( rawMin=3 ,lowE=1 ,rareE=1 ,dupESum=1 )      ,rowRebDup=NULL
    ,summMtx = matrix( c( 2 ,3 ,3 ,2 ,2 ,2  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
            ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
            ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,summMtx.sum = c(raw=7 ,evt=1)  # *H801 초토화..
    ,summMtx.reb = matrix( c( 2 ,2 ,2 ,1 ,2 ,1  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
            ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
            ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,scMtx.sz = matrix( c( 6 ,4 ,1 ,1 ,1 ,1  ,2 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                # c( "rebCnt" ,"rebDup" ) / c( "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg" )
                ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
            )
    ,scMtx.sz.sum = c(rebCnt.r=4 ,rebCnt.e=1)
    ,isHard=NULL  # use default
)

mName <- "sScore0GS3"
bsScoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "rMatCntH1"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"rMatCntH2"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"rRebMaxH1"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"rRebMaxH2"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"eMatCntH1"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        ,freqVal=1
                    )
        ,"eMatCntH2"=list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                        ,evtMax.fCol= c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )  # H799 (3,2,3)
                        ,freqVal=1
                    )
        ,"eRebMaxH1"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        ,freqVal=1
                    )
        ,"eRebMaxH2"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(1,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        ,freqVal=1
                    )
        ,"cMatCntH1"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"cMatCntH2"=list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(1,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"cRebMaxH1"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"cRebMaxH2"=list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"fMatCntH1"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(1,3,3,3,3,3,3),c(1,2,3,4,5,6,7)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"fMatCntH2"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(1,3,3,3,3,3,3),c(1,2,3,4,5,6,7)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"fRebMaxH1"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(1,3,3,3,3,3,3),c(1,2,3,4,5,6,7)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"fRebMaxH2"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(1,3,3,3,3,3,3),c(1,2,3,4,5,6,7)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                    )
    )
    ,evtMax = NULL    ,evtMaxFColTot = NULL
    ,rowReb = NULL    ,rowRebDup=NULL
    ,summMtx = matrix( c( 1 ,3 ,3 ,1 ,2 ,2  ,1 ,1 ,1 ,1 ,2 ,2 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
            ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
            ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,summMtx.sum = c(raw=4 ,evt=2)
    ,summMtx.reb = NULL
    ,scMtx.sz = matrix( c( 4 ,4 ,1 ,2 ,2 ,1  ,2 ,2 ,1 ,1 ,1 ,1 ) ,byrow=T
                # c( "rebCnt" ,"rebDup" ) / c( "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg" )
                ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
    )
    ,scMtx.sz.sum = c(rebCnt.r=4 ,rebCnt.e=3)
    ,isHard=NULL  # use default
)


mName <- "sScore0PSh"
if( TRUE ){
    bsScoreMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "rSeq0"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3,3),c(1,2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"rSeq1"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"rSeqN"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"rNSeq"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"rSyc0"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"rSyc1"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"rColCnt"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"eSeq0"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"eSeq1"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"eSeqN"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"eNSeq"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"eSyc0"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"eSyc1"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"eColCnt"=list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )      # H873(4)
                            ,evt=matrix( c(c(2,3,3,3,3),c(3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"cSeq0"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"cSeq1"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"cSeqN"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"cNSeq"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"cSyc0"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"cSyc1"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"cColCnt"=list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"fSeq0"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(1,2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"fSeq1"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"fSeqN"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"fNSeq"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"fSyc0"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"fSyc1"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"fColCnt"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
        )
        ,evtMax = NULL      ,evtMaxFColTot = NULL
        ,rowReb = c( rawMin=3 ,lowE=1 ,rareE=1 ,dupESum=1 )      ,rowRebDup=NULL    # ColCnt 컬럼으로 인해 hpn이 2개씩 생성된다.
        ,summMtx = matrix( c( 1 ,3 ,3 ,1 ,2 ,2  ,1 ,1 ,1 ,1 ,2 ,2 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
        ,summMtx.sum = c(raw=4 ,evt=2)
        ,summMtx.reb = matrix( c( 1 ,2 ,2 ,1 ,1 ,1  ,1 ,1 ,1 ,1 ,2 ,1 ) ,byrow=T
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
        ,scMtx.sz = matrix( c( 3 ,4 ,1 ,2 ,3 ,1  ,2 ,2 ,1 ,1 ,1 ,1 ) ,byrow=T
                    # c( "rebCnt" ,"rebDup" ) / c( "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg" )
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
        )
        ,scMtx.sz.sum = c(rebCnt.r=6 ,rebCnt.e=3)
        ,isHard=NULL  # use default
    )
}

mName <- "sScore0PSrp"
if( TRUE ){
    bsScoreMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "r1TotSize"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"r1ValSize"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"r2TotSize"=list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"r2ValSize"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"e1TotSize"=list( rng=matrix( c(0,4 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"e1ValSize"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"e2TotSize"=list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"e2ValSize"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"c1TotSize"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"c1ValSize"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"c2TotSize"=list( rng=matrix( c(0,5 ,0,5) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"c2ValSize"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"f1TotSize"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"f1ValSize"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"f2TotSize"=list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"f2ValSize"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
        )
        ,evtMax = NULL      ,evtMaxFColTot = NULL
        ,rowReb = c( rawMin=2 ,lowE=1 ,rareE=1 ,dupESum=1 )      ,rowRebDup=NULL    # ColCnt 컬럼으로 인해 hpn이 2개씩 생성된다.
        ,summMtx = matrix( c( 1 ,3 ,3 ,1 ,2 ,2  ,1 ,1 ,1 ,1 ,2 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
        ,summMtx.sum = c(raw=4 ,evt=2)
        ,summMtx.reb = matrix( c( 1 ,2 ,2 ,1 ,1 ,1  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
        ,scMtx.sz = matrix( c( 4 ,4 ,1 ,2 ,2 ,1  ,2 ,2 ,1 ,1 ,1 ,1 ) ,byrow=T
                    # c( "rebCnt" ,"rebDup" ) / c( "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg" )
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
        )
        ,scMtx.sz.sum = c(rebCnt.r=5 ,rebCnt.e=3)
        ,isHard=NULL  # use default
    )
}


mName <- "sScore0PSrpRaw"
if( TRUE ){
    bsScoreMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "r1TotSize"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"r1ValSize"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"r2TotSize"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                            ,freqVal=1
                        )
            ,"r2ValSize"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                            ,freqVal=1
                        )
            ,"e1TotSize"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"e1ValSize"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"e2TotSize"=list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                            ,freqVal=1
                        )
            ,"e2ValSize"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                            ,freqVal=1
                        )
            ,"c1TotSize"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"c1ValSize"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"c2TotSize"=list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                            ,freqVal=1
                        )
            ,"c2ValSize"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                            ,freqVal=1
                        )
            ,"f1TotSize"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"f1ValSize"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"f2TotSize"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                            ,freqVal=1
                        )
            ,"f2ValSize"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                            ,freqVal=1
                        )
        )
        ,evtMax = NULL      ,evtMaxFColTot = NULL
        ,rowReb = c( rawMin=2 ,lowE=1 ,rareE=1 ,dupESum=1 )      ,rowRebDup=NULL    # ColCnt 컬럼으로 인해 hpn이 2개씩 생성된다.
        ,summMtx = matrix( c( 1 ,2 ,2 ,2 ,2 ,2  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                            # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
        ,summMtx.sum = c(raw=3 ,evt=2)
        ,summMtx.reb = matrix( c( 1 ,2 ,2 ,1 ,1 ,1  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
        ,scMtx.sz = matrix( c( 4 ,4 ,1 ,1 ,1 ,1  ,2 ,2 ,1 ,1 ,1 ,1 ) ,byrow=T
                    # c( "rebCnt" ,"rebDup" ) / c( "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg" )
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
        )
        ,scMtx.sz.sum = c(rebCnt.r=5 ,rebCnt.e=1)
        ,isHard=NULL  # use default
    )
}





# Multi_R ------------------------------------------------
#   추가 후보
#       - bsMR2569
#       - bsMR29Lecf
mName <- "bsMR4567"     # freqVal 재활용 필요.
if( TRUE ){
    bsScoreMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "pBanN.r"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                    ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                    ,evtMax.fCol=NULL
                    ,freqVal=1
            ) 
            ,"pBanN.n"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                    ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                    ,evtMax.fCol=NULL
            )
            ,"pLCol"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                    ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                    ,evtMax.fCol=NULL
                    ,freqVal=1
            )
            ,"pE3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                    ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                    ,evtMax.fCol=NULL
            )
            ,"pE4"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                    ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                    ,evtMax.fCol=NULL
            )
            ,"pMH"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                    ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                    ,evtMax.fCol=NULL
            )
            ,"pfNum"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                    ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                    ,evtMax.fCol=NULL
            )
            ,"iBanN"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                    ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                    ,evtMax.fCol=NULL
                    ,freqVal=1
            )
            ,"iLCol"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                    ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                    ,evtMax.fCol=NULL
            )
            ,"iE3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                    ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                    ,evtMax.fCol=NULL
            )
            ,"iE4"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                    ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                    ,evtMax.fCol=NULL
            )
            ,"iMH"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                    ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                    ,evtMax.fCol=NULL
            )
            ,"ifNum"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                    ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                    ,evtMax.fCol=NULL
            )
            ,"FVa.m"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                    ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                    ,evtMax.fCol=NULL
                    ,freqVal=1
            )
            ,"FVa.c"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                    ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                    ,evtMax.fCol=NULL
            )
            ,"m4"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                    ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                    ,evtMax.fCol=NULL
            )
            ,"xBan.x"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                    ,evt=matrix( c(c(3,3),c(3,4)) ,ncol=2)
                    ,evtMax.fCol=NULL
            )
            ,"xLCol"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                    ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                    ,evtMax.fCol=NULL
            )
            ,"xEn"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                    ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                    ,evtMax.fCol=NULL
            )
            ,"xfNum"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                    ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                    ,evtMax.fCol=NULL
            )
            ,"xMH"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                    ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                    ,evtMax.fCol=NULL
            )
            ,"eSum_FVaM4"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                    ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                    ,evtMax.fCol=NULL
            )
        )
        ,evtMax = NULL  ,evtMaxFColTot = NULL
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 )     ,rowRebDup=NULL
        ,summMtx = matrix( c(   1 ,2 ,3 ,1 ,2 ,2  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
            ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
            ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
        ,summMtx.sum = c(raw=4 ,evt=2)
        ,summMtx.reb = matrix( c(   1 ,2 ,2 ,1 ,1 ,1  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
            ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
            ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
        ,scMtx.sz = matrix( c( 3 ,3 ,1 ,2 ,2 ,1  ,2 ,2 ,1 ,1 ,1 ,1 ) ,byrow=T
                # c( "rebCnt" ,"rebDup" ) / c( "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg" )
                ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
        )
        ,scMtx.sz.sum = c(rebCnt.r=5 ,rebCnt.e=2)
        ,isHard=NULL  # use default
    )
}

mName <- "bsMR1234"     # freqVal 재활용 필요.
if( TRUE ){
    bsScoreMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "s1rem0.len.tot"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            ) 
            ,"s1rem1.len.tot"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"s1c0.len.tot"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"s1c1.len.tot"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"s1f0.len.tot"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"s1f1.len.tot"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"s2rebC.r"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
                    ,freqVal=1
            )
            ,"s2rebC.c"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
                    ,freqVal=1
            )
            ,"s2rebC.f"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
                    # ,freqVal=1        # H868 rReb(hpn:3)
            )
            ,"s2rebC2.r"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
                    ,freqVal=1
            )
            ,"s2rebC2.c"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
                    ,freqVal=1
            )
            ,"s2rebC2.f"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"s2inc.r"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
                    ,freqVal=1
            )
            ,"s2inc.c"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
                    ,freqVal=1
            )
            ,"s2inc.f"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"s2inc.r2"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
                    ,freqVal=1
            )
            ,"s2inc.c2"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
                    ,freqVal=1
            )
            ,"s2inc.f2"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"s3rebPtn.1"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"s3snMax.r"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"s3snMax.c"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"s3snMax.f"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"s4pBanN.r"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"s4pBanN.n"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"s4pLCol"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"s4pE3"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"s4pfNum"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"s4iBanN"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"s4iLCol"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"s4ifNum"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"s4FVa.m"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
        )
        ,evtMax = NULL  ,evtMaxFColTot = NULL
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 )     ,rowRebDup=NULL
        ,summMtx = matrix( c(   1 ,2 ,4 ,2 ,1 ,1  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
            ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
            ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
        ,summMtx.sum = c(raw=5 ,evt=2)
        ,summMtx.reb = matrix( c(   1 ,1 ,2 ,1 ,1 ,1  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
            ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
            ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
        ,scMtx.sz = matrix( c( 2 ,5 ,1 ,1 ,1 ,1  ,2 ,2 ,1 ,1 ,1 ,1 ) ,byrow=T
                # c( "rebCnt" ,"rebDup" ) / c( "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg" )
                ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
        )
        ,scMtx.sz.sum = c(rebCnt.r=5 ,rebCnt.e=2)
        ,isHard=NULL  # use default
    )
}

mName <- "bsMR569Lecf"
if( TRUE ){
    bsScoreMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "pBanN.r"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"pBanN.n"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"pLCol"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"pE3"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"pfNum"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"iBanN"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"iLCol"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"ifNum"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"FVa.m"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"eCnt"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
                    ,freqVal=1:3
            )
            ,"cCnt"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
                    ,freqVal=1:2
            )
            ,"fCnt"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
                    ,freqVal=1
            )
            ,"Ae13Hpn1A"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
                    ,freqVal=1
            )
            ,"Ae13Hpn1B"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
                    ,freqVal=1
            )
            ,"Ae24Hpn1A"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
                    ,freqVal=1
            )
            ,"Ae24Hpn1B"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
                    ,freqVal=1
            )
            ,"Ve13Hpn1A"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
                    ,freqVal=1
            )
            ,"Ve13Hpn1B"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
                    ,freqVal=1
            )
            ,"Ve24Hpn1A"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
                    ,freqVal=1
            )
            ,"Ve24Hpn1B"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
                    ,freqVal=1
            )

            ,"Acf13Hpn1A"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"Acf13Hpn1B"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"Acf24Hpn1A"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"Acf24Hpn1B"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"Vcf13Hpn1A"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"Vcf13Hpn1B"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"Vcf24Hpn1A"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"Vcf24Hpn1B"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(3),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
        )
        ,evtMax = NULL  ,evtMaxFColTot = NULL
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 )     ,rowRebDup=NULL
        ,summMtx = matrix( c(   1 ,3 ,4 ,2 ,1 ,1  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
            ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
            ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
        ,summMtx.sum = c(raw=4 ,evt=2)
        ,summMtx.reb = matrix( c(   1 ,2 ,2 ,1 ,1 ,1  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
            ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
            ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
        ,scMtx.sz = matrix( c( 3 ,6 ,1 ,1 ,1 ,1  ,2 ,2 ,1 ,1 ,1 ,1 ) ,byrow=T
                # c( "rebCnt" ,"rebDup" ) / c( "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg" )
                ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
        )
        ,scMtx.sz.sum = c(rebCnt.r=6 ,rebCnt.e=2)
        ,isHard=NULL  # use default
    )
}


mName <- "bsMREreb1"
if( TRUE ){
    bsScoreMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "sGSeExtMax"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(1),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"sGSh2eExtMax"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(1),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"sGS3eRebMaxHn"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(1),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"sPSheColCnt"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(1),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"sPSrpeNTotSize"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(1),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"sPSrpRaweNTotSize"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(1),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
        )
        ,evtMax = NULL  ,evtMaxFColTot = NULL
        ,rowReb = c( rawMin=2 ,lowE=2 ,rareE=1 ,dupESum=1 )     ,rowRebDup=NULL     # H823
        ,summMtx = matrix( c(  1 ,3 ,2 ,2 ,2 ,2  ,1 ,2 ,2 ,1 ,2 ,2 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
            ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
            ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
        ,summMtx.sum = c(raw=4 ,evt=5)
        ,summMtx.reb = matrix( c(  1 ,2 ,1 ,1 ,1 ,1  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
            ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
            ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
        ,scMtx.sz = matrix( c( 3 ,2 ,1 ,3 ,2 ,1  ,3 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
        )
        ,scMtx.sz.sum = c(rebCnt.r=4 ,rebCnt.e=4)
        ,isHard=NULL  # use default
    )
}

mName <- "bsMREreb0"
if( TRUE ){
    bsScoreMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
             "sGSeExtMax"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(1),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"sGSh2eExtMax"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(1),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"sGS3eRebMaxHn"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(1),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"sPSheColCnt"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(1),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"sPSrpeNTotSize"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(1),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"sPSrpRaweNTotSize"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(1),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"s5HpnCnt"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(1),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
                    ,freqVal=1:2
            )
            ,"s6HpnCnt"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(1),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
                    ,freqVal=1:2
            )
            # ,"s7HpnCnt"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
            #         ,evt=matrix( c(c(1),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            #         ,freqVal=1:2
            # )
            ,"s9eCnt"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(1),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
                    ,freqVal=1:2
            )
            ,"AVec13Hpn1A"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(1),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
                    ,freqVal=1:2
            )
            ,"AVec13Hpn1B"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(1),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
                    ,freqVal=1:2
            )
            ,"AVec24Hpn1A"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(1),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
                    ,freqVal=1:2
            )
            ,"AVec24Hpn1B"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(1),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
                    ,freqVal=1:2
            )
        )
        ,evtMax = NULL  ,evtMaxFColTot = NULL
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 )     ,rowRebDup=NULL
        ,summMtx = matrix( c(  1 ,3 ,3 ,2 ,1 ,1  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
            ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
            ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
        ,summMtx.sum = c(raw=3 ,evt=1)
        ,summMtx.reb = matrix( c(  1 ,1 ,1 ,1 ,1 ,1  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
            ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
            ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
        ,scMtx.sz = matrix( c( 3 ,3 ,1 ,1 ,1 ,1  ,2 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
        )
        ,scMtx.sz.sum = c(rebCnt.r=4 ,rebCnt.e=1)
        ,isHard=NULL  # use default
    )
}

mName <- "bsMRRCF"
if( TRUE ){
    bsScoreMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
             "sGSxExtMax"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(1),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"sGS3xRebMaxHn"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(1),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"sPShxColCnt"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(1),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"sPSrpxNTotSize"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(1),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"sPSrpRawxNTotSize"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(1),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            )
            ,"s467HpnCnt"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(1),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
                    ,freqVal=1:2
            )
            # ,"s9xCnt"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
            #         ,evt=matrix( c(c(1),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
            #         ,freqVal=1:3
            # )
            ,"s9eCnt"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(1),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
                    ,freqVal=1:3
            )
            ,"s9cCnt"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(1),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
                    ,freqVal=1:3
            )
            ,"s9fCnt"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(1),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
                    ,freqVal=1:3
            )
            ,"AVx13Hpn1A"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(1),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
                    ,freqVal=1:3
            )
            ,"AVx13Hpn1B"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(1),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
                    ,freqVal=1:3
            )
            ,"AVx24Hpn1A"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(1),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
                    ,freqVal=1:3
            )
            ,"AVx24Hpn1B"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                    ,evt=matrix( c(c(1),c(9)) ,ncol=2)  ,evtMax.fCol=NULL
                    ,freqVal=1:3
            )
        )
        ,evtMax = NULL  ,evtMaxFColTot = NULL
        ,rowReb = c( rawMin=2 ,lowE=1 ,rareE=1 ,dupESum=1 )     ,rowRebDup=NULL
        ,summMtx = matrix( c(  1 ,3 ,3 ,2 ,1 ,1  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
            ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
            ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
        ,summMtx.sum = c(raw=3 ,evt=1)
        ,summMtx.reb = matrix( c(  1 ,2 ,1 ,1 ,1 ,1  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
            ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
            ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
        ,scMtx.sz = matrix( c( 4 ,4 ,1 ,1 ,1 ,1  ,2 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
        )
        ,scMtx.sz.sum = c(rebCnt.r=4 ,rebCnt.e=1)
        ,isHard=NULL  # use default
    )
}




for( mName in names( bsScoreMtxCfg ) ){ # naming 추가.

    for( fcName in names(bsScoreMtxCfg[[mName]]$fCol) ){
        colnames(bsScoreMtxCfg[[mName]]$fCol[[fcName]]$rng) <- c("lev1","lev2")
        rownames(bsScoreMtxCfg[[mName]]$fCol[[fcName]]$rng) <- c("min","max")

        colnames(bsScoreMtxCfg[[mName]]$fCol[[fcName]]$evt) <- c("lev","val")

        if( is.null(bsScoreMtxCfg[[mName]]$fCol[[fcName]]$evtMax.fCol ) ){
            # fCol 별 전체 phase 대상으로 evt 발생 제한.( >= 기준 cut )
            # bsScoreMtxCfg[[mName]]$fCol[[fcName]]$evtMax.fCol <- c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
            bsScoreMtxCfg[[mName]]$fCol[[fcName]]$evtMax.fCol <- c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
        }
        if( is.null(bsScoreMtxCfg[[mName]]$fCol[[fcName]]$forbidEvtReb) ){
            bsScoreMtxCfg[[mName]]$fCol[[fcName]]$forbidEvtReb <- c(2,3,4)
        }

    }

    if( is.null(bsScoreMtxCfg[[mName]]$evtMaxFColTot) ){
        #   fCol 4 all Ph에서 CloseMax 가 나타난 fCol 수.
        bsScoreMtxCfg[[mName]]$evtMaxFColTot  <- c( lev1Max=3 ,lev2Max=3 ,lev3Max=3 )
    }
    if( is.null(bsScoreMtxCfg[[mName]]$evtMax) ){
        #   한 개 phase 내에서의 이벤트 발생 제한.
        #   "minLev"이상 이벤트가 maxHpn 보다 초과할 때 Cut
        evtMax <- matrix( c(2,2,3,1 ,2,3,3,2)
                            ,byrow=T ,ncol=4
                            ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
                        )
        bsScoreMtxCfg[[mName]]$evtMax     <- evtMax
    }
    if( is.null(bsScoreMtxCfg[[mName]]$rowReb) ){
        bsScoreMtxCfg[[mName]]$rowReb <- c( rawMin=1 ,lowE=2 ,rareE=1 ,dupESum=1 )
    }
    if( is.null(bsScoreMtxCfg[[mName]]$rowRebDup) ){  # 조건 : >=
        bsScoreMtxCfg[[mName]]$rowRebDup <- c( lowE=1 ,rareE=1 )
    }
    if( is.null(bsScoreMtxCfg[[mName]]$summMtx) ){
        #     $summMtx    all ph fCol phReb xyCnt.fCol xyCnt.phase
        #             raw   0  0    0     0          0           0
        #             evt   0  0    0     0          0           0
        # cName <- c( "all" ,"ph" ,"fCol" ,"phReb" ,"xyCnt.fCol" ,"xyCnt.phase" )
        # rName <- c( "raw" ,"evt" )
        thldVal <- c(   1 ,2 ,2 ,2 ,2 ,1    # xyCnt.fCol, xyCnt.fCol은 
                        ,1 ,2 ,2 ,2 ,2 ,1 
                    )
        bsScoreMtxCfg[[mName]]$summMtx <- matrix( thldVal ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
                )
    }
    if( is.null(bsScoreMtxCfg[[mName]]$summMtx.reb ) ){
        #     $summMtx.reb    all ph fCol phReb xyCnt.fCol xyCnt.phase
        #                 raw   0  0    0     0          0           0
        #                 evt   0  0    0     0          0           0
        cName <- c( "all" ,"ph" ,"fCol" ,"phReb" ,"xyCnt.fCol" ,"xyCnt.phase" )
        rName <- c( "raw" ,"evt" )
        thldVal <- c(   1 ,1 ,1 ,1 ,1 ,1    # xyCnt.fCol, xyCnt.fCol은 봐가며 조절해야 할 듯.
                        ,1 ,1 ,1 ,1 ,1 ,1 
                    )
        bsScoreMtxCfg[[mName]]$summMtx.reb <- matrix( thldVal ,byrow=T
                    ,ncol=length(cName) ,nrow=length(rName) ,dimnames=list(rName,cName)
                )
    }
    if( is.null(bsScoreMtxCfg[[mName]]$scMtx.sz ) ){
        #     $scMtx.sz      r.ph r.fCol r.dblHpnFlg e.ph e.fCol e.dblHpnFlg
        #             rebCnt    0      0           0    0      0           0
        #             rebDup    0      0           0    0      0           0
        # cName <- c( "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg" )
        # rName <- c( "rebCnt" ,"rebDup" )
        # thldVal <- c(   2 ,2 ,1 ,2 ,2 ,1
        #                 ,1 ,1 ,1 ,1 ,1 ,1 
        #             )
        thldVal <- c(    1 ,1 ,1 ,1 ,1 ,1
                        ,1 ,1 ,1 ,1 ,1 ,1 
                    )
        bsScoreMtxCfg[[mName]]$scMtx.sz <- matrix( thldVal ,byrow=T
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
    }
    if( is.null(bsScoreMtxCfg[[mName]]$summMtx.sum) ){
        # 개개 cut은 피했지만 전체 발생 총합은 한계를 넘는 경우.
        bsScoreMtxCfg[[mName]]$summMtx.sum <- c(raw=2 ,evt=2)
    }
    if( is.null(bsScoreMtxCfg[[mName]]$scMtx.sz.sum ) ){
        #   rebCnt.r = r.ph+r.fCol    rebCnt.e = e.ph+e.fCol
        bsScoreMtxCfg[[mName]]$scMtx.sz.sum <- c(rebCnt.r=2 ,rebCnt.e=2)
    }

}




# =============================================================================================================
#   bsSfcMtxCfg
# =============================================================================================================

bsSfcMtxCfg <- list()   # B.makeHMtxLst() 의 stdFilter참고
if( 0==length(bsSfcMtxCfg) ){
    sfcName <- "sfcLate"
    bsSfcMtxCfg[[sfcName]] <- list(
        basic=list( prime=NULL
        )
        ,bScr=list(  )
    )

    sfcName <- "sfc0"
    bsSfcMtxCfg[[sfcName]] <- list(
        basic=list( prime=NULL
        )
        ,bScr=list(  )
    )

    sfcName <- "sfc1"
    bsSfcMtxCfg[[sfcName]] <- list(
        basic=list( prime=NULL
        )
        ,bScr=list(  )
    )

    sfcName <- "sfc2"
    bsSfcMtxCfg[[sfcName]] <- list(
        basic=list( prime=NULL
        )
        ,bScr=list(  )
    )

    sfcName <- "NGD0000.A"
    bsSfcMtxCfg[[sfcName]] <- list(
        basic=list( prime=NULL
        )
        ,bScr=list(  )
    )

    sfcName <- "NGA0100.A"
    bsSfcMtxCfg[[sfcName]] <- list(
        basic=list( prime=NULL
        )
        ,bScr=list(  )
    )

    sfcName <- "NGAP000.E"
    bsSfcMtxCfg[[sfcName]] <- list(
        basic=list( prime=NULL
        )
        ,bScr=list(  )
    )
}

for( hName in names(bsSfcMtxCfg) ){

    if( is.null(bsSfcMtxCfg[[hName]]$basic$prime) ){
        bsSfcMtxCfg[[hName]]$basic$prime <- matrix( c(1,8,0,1 ,0,1,0,1 ,0,9,0,4 ,0,0,0,1 ) 
                            ,ncol=2 ,byrow=T
                            ,dimnames=list(c(   "zeroCntM_raw" ,"zeroCntPh_raw" 
                                                ,"rebMtxM_raw" ,"rebMtxPh_raw" 
                                                ,"zeroCntM_evt","zeroCntPh_evt"
                                                ,"rebMtxM_evt" ,"rebMtxPh_evt"  # 전제조건 : hpn > 1
                                            )
                                            ,c("min","max")
                            ) 
                        )
    }

    # if( is.null(bsSfcMtxCfg[[hName]]$basic$zeroCnt) ){
    #     bsSfcMtxCfg[[hName]]$basic$zeroCntM <- matrix( c(0,0 ,0,1) 
    #                         ,nrow=2 ,byrow=T 
    #                         ,dimnames=list(c("zeroCntM","zeroCntPh"),c("min","max")) 
    #                     )
    # }

    # if( is.null(bsSfcMtxCfg[[hName]]$evtMax) ){   }

}




# =============================================================================================================
#   참고용 백업 코드
# =============================================================================================================
if( FALSE ){

    mName <- "bsMRLArn"
    if( TRUE ){
        bsScoreMtxCfg[[mName]] <- list(
            mName = mName   ,style=c( freqZero=TRUE )
            ,fCol = list(
                # "xxx"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                #         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                #         ,evtMax.fCol=NULL
                # ) 
                # ,"xxx"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                #         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                #         ,evtMax.fCol=NULL
                # )
            )
            ,evtMax = NULL  ,evtMaxFColTot = NULL
            ,rowReb =c( rawMin=1,lowE=1,rareE=1,dupESum=1 )   ,rowRebDup=NULL
            ,summMtx = NULL ,summMtx.sum = NULL ,summMtx.reb = NULL
            ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
            ,isHard=NULL  # use default
        )
    }

    mName <- "bsMRLVrn"
    if( TRUE ){
        bsScoreMtxCfg[[mName]] <- list(
            mName = mName   ,style=c( freqZero=TRUE )
            ,fCol = list(
                # "xxx"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                #         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                #         ,evtMax.fCol=NULL
                # ) 
                # ,"xxx"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                #         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                #         ,evtMax.fCol=NULL
                # )
            )
            ,evtMax = NULL  ,evtMaxFColTot = NULL
            ,rowReb =c( rawMin=1,lowE=1,rareE=1,dupESum=1 )   ,rowRebDup=NULL
            ,summMtx = NULL ,summMtx.sum = NULL ,summMtx.reb = NULL
            ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
            ,isHard=NULL  # use default
        )
    }

    mName <- "bsMRLAVrn"
    if( TRUE ){
        bsScoreMtxCfg[[mName]] <- list(
            mName = mName   ,style=c( freqZero=TRUE )
            ,fCol = list(
                # "xxx"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                #         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                #         ,evtMax.fCol=NULL
                # ) 
                # ,"xxx"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                #         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                #         ,evtMax.fCol=NULL
                # )
            )
            ,evtMax = NULL  ,evtMaxFColTot = NULL
            ,rowReb =c( rawMin=1,lowE=1,rareE=1,dupESum=1 )   ,rowRebDup=NULL
            ,summMtx = NULL ,summMtx.sum = NULL ,summMtx.reb = NULL
            ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
            ,isHard=NULL  # use default
        )
    }

    mName <- "bsMRLAen"
    if( TRUE ){
        bsScoreMtxCfg[[mName]] <- list(
            mName = mName   ,style=c( freqZero=TRUE )
            ,fCol = list(
                # "xxx"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                #         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                #         ,evtMax.fCol=NULL
                # ) 
                # ,"xxx"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                #         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                #         ,evtMax.fCol=NULL
                # )
            )
            ,evtMax = NULL  ,evtMaxFColTot = NULL
            ,rowReb =c( rawMin=1,lowE=1,rareE=1,dupESum=1 )   ,rowRebDup=NULL
            ,summMtx = NULL ,summMtx.sum = NULL ,summMtx.reb = NULL
            ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
            ,isHard=NULL  # use default
        )
    }

    mName <- "bsMRLVen"
    if( TRUE ){
        bsScoreMtxCfg[[mName]] <- list(
            mName = mName   ,style=c( freqZero=TRUE )
            ,fCol = list(
                # "xxx"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                #         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                #         ,evtMax.fCol=NULL
                # ) 
                # ,"xxx"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                #         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                #         ,evtMax.fCol=NULL
                # )
            )
            ,evtMax = NULL  ,evtMaxFColTot = NULL
            ,rowReb =c( rawMin=1,lowE=1,rareE=1,dupESum=1 )   ,rowRebDup=NULL
            ,summMtx = NULL ,summMtx.sum = NULL ,summMtx.reb = NULL
            ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
            ,isHard=NULL  # use default
        )
    }

    mName <- "bsMRLAcn"
    if( TRUE ){
        bsScoreMtxCfg[[mName]] <- list(
            mName = mName   ,style=c( freqZero=TRUE )
            ,fCol = list(
                # "xxx"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                #         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                #         ,evtMax.fCol=NULL
                # ) 
                # ,"xxx"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                #         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                #         ,evtMax.fCol=NULL
                # )
            )
            ,evtMax = NULL  ,evtMaxFColTot = NULL
            ,rowReb =c( rawMin=1,lowE=1,rareE=1,dupESum=1 )   ,rowRebDup=NULL
            ,summMtx = NULL ,summMtx.sum = NULL ,summMtx.reb = NULL
            ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
            ,isHard=NULL  # use default
        )
    }

    mName <- "bsMRLVcn"
    if( TRUE ){
        bsScoreMtxCfg[[mName]] <- list(
            mName = mName   ,style=c( freqZero=TRUE )
            ,fCol = list(
                # "xxx"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                #         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                #         ,evtMax.fCol=NULL
                # ) 
                # ,"xxx"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                #         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                #         ,evtMax.fCol=NULL
                # )
            )
            ,evtMax = NULL  ,evtMaxFColTot = NULL
            ,rowReb =c( rawMin=1,lowE=1,rareE=1,dupESum=1 )   ,rowRebDup=NULL
            ,summMtx = NULL ,summMtx.sum = NULL ,summMtx.reb = NULL
            ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
            ,isHard=NULL  # use default
        )
    }

    mName <- "bsMRLAfn"
    if( TRUE ){
        bsScoreMtxCfg[[mName]] <- list(
            mName = mName   ,style=c( freqZero=TRUE )
            ,fCol = list(
                # "xxx"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                #         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                #         ,evtMax.fCol=NULL
                # ) 
                # ,"xxx"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                #         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                #         ,evtMax.fCol=NULL
                # )
            )
            ,evtMax = NULL  ,evtMaxFColTot = NULL
            ,rowReb =c( rawMin=1,lowE=1,rareE=1,dupESum=1 )   ,rowRebDup=NULL
            ,summMtx = NULL ,summMtx.sum = NULL ,summMtx.reb = NULL
            ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
            ,isHard=NULL  # use default
        )
    }

    mName <- "bsMRLVfn"
    if( TRUE ){
        bsScoreMtxCfg[[mName]] <- list(
            mName = mName   ,style=c( freqZero=TRUE )
            ,fCol = list(
                # "xxx"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                #         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                #         ,evtMax.fCol=NULL
                # ) 
                # ,"xxx"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                #         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                #         ,evtMax.fCol=NULL
                # )
            )
            ,evtMax = NULL  ,evtMaxFColTot = NULL
            ,rowReb =c( rawMin=1,lowE=1,rareE=1,dupESum=1 )   ,rowRebDup=NULL
            ,summMtx = NULL ,summMtx.sum = NULL ,summMtx.reb = NULL
            ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
            ,isHard=NULL  # use default
        )
    }

    mName <- "bsMRLAVfn"
    if( TRUE ){
        bsScoreMtxCfg[[mName]] <- list(
            mName = mName   ,style=c( freqZero=TRUE )
            ,fCol = list(
                # "xxx"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                #         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                #         ,evtMax.fCol=NULL
                # ) 
                # ,"xxx"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                #         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                #         ,evtMax.fCol=NULL
                # )
            )
            ,evtMax = NULL  ,evtMaxFColTot = NULL
            ,rowReb =c( rawMin=1,lowE=1,rareE=1,dupESum=1 )   ,rowRebDup=NULL
            ,summMtx = NULL ,summMtx.sum = NULL ,summMtx.reb = NULL
            ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
            ,isHard=NULL  # use default
        )
    }

    mName <- "bsMRLAecf13"
    if( TRUE ){
        bsScoreMtxCfg[[mName]] <- list(
            mName = mName   ,style=c( freqZero=TRUE )
            ,fCol = list(
                # "xxx"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                #         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                #         ,evtMax.fCol=NULL
                # ) 
                # ,"xxx"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                #         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                #         ,evtMax.fCol=NULL
                # )
            )
            ,evtMax = NULL  ,evtMaxFColTot = NULL
            ,rowReb =c( rawMin=1,lowE=1,rareE=1,dupESum=1 )   ,rowRebDup=NULL
            ,summMtx = NULL ,summMtx.sum = NULL ,summMtx.reb = NULL
            ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
            ,isHard=NULL  # use default
        )
    }

    mName <- "bsMRLAecf24"
    if( TRUE ){
        bsScoreMtxCfg[[mName]] <- list(
            mName = mName   ,style=c( freqZero=TRUE )
            ,fCol = list(
                # "xxx"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                #         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                #         ,evtMax.fCol=NULL
                # ) 
                # ,"xxx"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                #         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                #         ,evtMax.fCol=NULL
                # )
            )
            ,evtMax = NULL  ,evtMaxFColTot = NULL
            ,rowReb =c( rawMin=1,lowE=1,rareE=1,dupESum=1 )   ,rowRebDup=NULL
            ,summMtx = NULL ,summMtx.sum = NULL ,summMtx.reb = NULL
            ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
            ,isHard=NULL  # use default
        )
    }

    mName <- "bsMRLVecf13"
    if( TRUE ){
        bsScoreMtxCfg[[mName]] <- list(
            mName = mName   ,style=c( freqZero=TRUE )
            ,fCol = list(
                # "xxx"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                #         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                #         ,evtMax.fCol=NULL
                # ) 
                # ,"xxx"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                #         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                #         ,evtMax.fCol=NULL
                # )
            )
            ,evtMax = NULL  ,evtMaxFColTot = NULL
            ,rowReb =c( rawMin=1,lowE=1,rareE=1,dupESum=1 )   ,rowRebDup=NULL
            ,summMtx = NULL ,summMtx.sum = NULL ,summMtx.reb = NULL
            ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
            ,isHard=NULL  # use default
        )
    }

    mName <- "bsMRLVecf24"
    if( TRUE ){
        bsScoreMtxCfg[[mName]] <- list(
            mName = mName   ,style=c( freqZero=TRUE )
            ,fCol = list(
                # "xxx"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                #         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                #         ,evtMax.fCol=NULL
                # ) 
                # ,"xxx"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                #         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                #         ,evtMax.fCol=NULL
                # )
            )
            ,evtMax = NULL  ,evtMaxFColTot = NULL
            ,rowReb =c( rawMin=1,lowE=1,rareE=1,dupESum=1 )   ,rowRebDup=NULL
            ,summMtx = NULL ,summMtx.sum = NULL ,summMtx.reb = NULL
            ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
            ,isHard=NULL  # use default
        )
    }

    mName <- "bsMR_FV1234"
    if( TRUE ){
        bsScoreMtxCfg[[mName]] <- list(
            mName = mName   ,style=c( freqZero=TRUE )
            ,fCol = list(
                # "xxx"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                #         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                #         ,evtMax.fCol=NULL
                # ) 
                # ,"xxx"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                #         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                #         ,evtMax.fCol=NULL
                # )
            )
            ,evtMax = NULL  ,evtMaxFColTot = NULL
            ,rowReb =c( rawMin=1,lowE=1,rareE=1,dupESum=1 )   ,rowRebDup=NULL
            ,summMtx = NULL ,summMtx.sum = NULL ,summMtx.reb = NULL
            ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
            ,isHard=NULL  # use default
        )
    }

    mName <- "bsMR_FV567"
    if( TRUE ){
        bsScoreMtxCfg[[mName]] <- list(
            mName = mName   ,style=c( freqZero=TRUE )
            ,fCol = list(
                # "xxx"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                #         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                #         ,evtMax.fCol=NULL
                # ) 
                # ,"xxx"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                #         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                #         ,evtMax.fCol=NULL
                # )
            )
            ,evtMax = NULL  ,evtMaxFColTot = NULL
            ,rowReb =c( rawMin=1,lowE=1,rareE=1,dupESum=1 )   ,rowRebDup=NULL
            ,summMtx = NULL ,summMtx.sum = NULL ,summMtx.reb = NULL
            ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
            ,isHard=NULL  # use default
        )
    }

    mName <- "bsMR_FV89"
    if( TRUE ){
        bsScoreMtxCfg[[mName]] <- list(
            mName = mName   ,style=c( freqZero=TRUE )
            ,fCol = list(
                # "xxx"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                #         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                #         ,evtMax.fCol=NULL
                # ) 
                # ,"xxx"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                #         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                #         ,evtMax.fCol=NULL
                # )
            )
            ,evtMax = NULL  ,evtMaxFColTot = NULL
            ,rowReb =c( rawMin=1,lowE=1,rareE=1,dupESum=1 )   ,rowRebDup=NULL
            ,summMtx = NULL ,summMtx.sum = NULL ,summMtx.reb = NULL
            ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
            ,isHard=NULL  # use default
        )
    }

    mName <- "bsMRAVr_hpn1AB"
    if( TRUE ){
        bsScoreMtxCfg[[mName]] <- list(
            mName = mName   ,style=c( freqZero=TRUE )
            ,fCol = list(
                # "xxx"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                #         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                #         ,evtMax.fCol=NULL
                # ) 
                # ,"xxx"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                #         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                #         ,evtMax.fCol=NULL
                # )
            )
            ,evtMax = NULL  ,evtMaxFColTot = NULL
            ,rowReb =c( rawMin=1,lowE=1,rareE=1,dupESum=1 )   ,rowRebDup=NULL
            ,summMtx = NULL ,summMtx.sum = NULL ,summMtx.reb = NULL
            ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
            ,isHard=NULL  # use default
        )
    }

    mName <- "bsMRAVe_hpn1AB"
    if( TRUE ){
        bsScoreMtxCfg[[mName]] <- list(
            mName = mName   ,style=c( freqZero=TRUE )
            ,fCol = list(
                # "xxx"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                #         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                #         ,evtMax.fCol=NULL
                # ) 
                # ,"xxx"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                #         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                #         ,evtMax.fCol=NULL
                # )
            )
            ,evtMax = NULL  ,evtMaxFColTot = NULL
            ,rowReb =c( rawMin=1,lowE=1,rareE=1,dupESum=1 )   ,rowRebDup=NULL
            ,summMtx = NULL ,summMtx.sum = NULL ,summMtx.reb = NULL
            ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
            ,isHard=NULL  # use default
        )
    }

    mName <- "bsMRAVc_hpn1AB"
    if( TRUE ){
        bsScoreMtxCfg[[mName]] <- list(
            mName = mName   ,style=c( freqZero=TRUE )
            ,fCol = list(
                # "xxx"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                #         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                #         ,evtMax.fCol=NULL
                # ) 
                # ,"xxx"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                #         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                #         ,evtMax.fCol=NULL
                # )
            )
            ,evtMax = NULL  ,evtMaxFColTot = NULL
            ,rowReb =c( rawMin=1,lowE=1,rareE=1,dupESum=1 )   ,rowRebDup=NULL
            ,summMtx = NULL ,summMtx.sum = NULL ,summMtx.reb = NULL
            ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
            ,isHard=NULL  # use default
        )
    }

    mName <- "bsMRAVf_hpn1AB"
    if( TRUE ){
        bsScoreMtxCfg[[mName]] <- list(
            mName = mName   ,style=c( freqZero=TRUE )
            ,fCol = list(
                # "xxx"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                #         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                #         ,evtMax.fCol=NULL
                # ) 
                # ,"xxx"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                #         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                #         ,evtMax.fCol=NULL
                # )
            )
            ,evtMax = NULL  ,evtMaxFColTot = NULL
            ,rowReb =c( rawMin=1,lowE=1,rareE=1,dupESum=1 )   ,rowRebDup=NULL
            ,summMtx = NULL ,summMtx.sum = NULL ,summMtx.reb = NULL
            ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
            ,isHard=NULL  # use default
        )
    }

}