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
bsScoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "rem0.num"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"rem0.len.tot"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"rem0.len.val"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"rem1.num"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"rem1.len.tot"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
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
                        ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
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
                        ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"c1.len.val"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"f0.num"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"f0.len.tot"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
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
                        ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"f1.len.val"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
    )
    ,evtMax = NULL  ,evtMaxFColTot = NULL
    ,rowReb = c( rawMin=3 ,lowE=2 ,rareE=1 )    ,rowRebDup=NULL
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

mName <- "sScore02"
bsScoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "rebC.r"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    ) 
        ,"rebC.c"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"rebC.f"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"rebC2.r"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"rebC2.c"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"rebC2.f"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"inc.r"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"inc.c"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"inc.f"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"inc.r2"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"inc.c2"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"inc.f2"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
    )
    ,evtMax = matrix( c(2,2,3,1 ,2,3,3,4)   ,byrow=T ,ncol=4
                ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
    )
    ,evtMaxFColTot = NULL
    ,rowReb = c( rawMin=2 ,lowE=2 ,rareE=1 )    ,rowRebDup=NULL
    ,summMtx = matrix(  # all ph fCol phReb xyCnt.fCol xyCnt.phase      / raw ,evt
                        c(  1 ,3 ,2 ,2 ,2 ,1    ,1 ,1 ,1 ,1 ,1 ,1  ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,summMtx.reb = matrix(  # all ph fCol phReb xyCnt.fCol xyCnt.phase      / raw ,evt
                        c(  1 ,2 ,2 ,1 ,1 ,1    ,1 ,1 ,1 ,1 ,1 ,1  ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,summMtx.sum = c(raw=4 ,evt=2)
    ,scMtx.sz = matrix( # r.ph r.fCol r.dblHpnFlg e.ph e.fCol e.dblHpnFlg  /  rebCnt ,rebDup
                        c(  3 ,3 ,1 ,1 ,2 ,1   ,2 ,2 ,1 ,1 ,1 ,1  )
                    ,byrow=T
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
    )
    ,scMtx.sz.sum = c(rebCnt.r=5 ,rebCnt.e=2)
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
    ,rowReb = c( rawMin=1 ,lowE=2 ,rareE=1 )    ,rowRebDup=NULL
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
        ,"FVa.m"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"FVa.c"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"m4"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
    )
    ,evtMax = NULL    ,evtMaxFColTot = NULL
    ,rowReb = NULL    ,rowRebDup=NULL
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
    ,rowReb =c( rawMin=3,lowE=2,rareE=1 )    ,rowRebDup=NULL
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
    ,rowReb = NULL    ,rowRebDup=NULL
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
    ,rowReb = NULL    ,rowRebDup=NULL
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
        "max3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
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
        ,"cTbl"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"fTbl"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
    )
    ,evtMax = NULL    ,evtMaxFColTot = NULL
    ,rowReb = NULL    ,rowRebDup=NULL
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
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "rCnt"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
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
                    ) 
        ,"eD2"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(1,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"eDn"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"eLr"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"eRl"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"cCnt"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(4,5,6)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    ) 
        ,"cD2"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"cDn"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"cLr"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"cRl"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"fCnt"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
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
    ,rowReb =c( rawMin=3,lowE=2,rareE=1 )   ,rowRebDup=NULL
    ,summMtx = matrix(  # all ph fCol phReb xyCnt.fCol xyCnt.phase      / raw ,evt
                        c(  1 ,2 ,3 ,1 ,1 ,1    ,1 ,1 ,1 ,1 ,2 ,2  ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,summMtx.sum = c(raw=3 ,evt=2)
    ,summMtx.reb =  matrix(  # all ph fCol phReb xyCnt.fCol xyCnt.phase      / raw ,evt
                        c(  1 ,2 ,2 ,1 ,1 ,1    ,1 ,1 ,1 ,1 ,1 ,1  ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,scMtx.sz = matrix( c( 2 ,3 ,1 ,2 ,2 ,1     ,2 ,2 ,1 ,1 ,1 ,1 ) ,byrow=T
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
    ,rowReb =c( rawMin=2,lowE=2,rareE=1 )   ,rowRebDup=NULL
    ,summMtx = matrix(  # all ph fCol phReb xyCnt.fCol xyCnt.phase      / raw ,evt
                        c(  1 ,2 ,2 ,1 ,1 ,1    ,1 ,1 ,1 ,1 ,1 ,1  ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,summMtx.sum = c(raw=2 ,evt=2)
    ,summMtx.reb = NULL
    ,scMtx.sz = matrix( c( 2 ,2 ,1 ,1 ,1 ,1     ,2 ,2 ,1 ,1 ,1 ,1 ) ,byrow=T
                        #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                        #   "rebCnt" ,"rebDup"
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
    ,scMtx.sz.sum = c(rebCnt.r=2 ,rebCnt.e=2)
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
    ,rowReb =c( rawMin=2,lowE=2,rareE=1 )   ,rowRebDup=NULL
    ,summMtx = matrix(  # all ph fCol phReb xyCnt.fCol xyCnt.phase      / raw ,evt
                        c(  1 ,2 ,2 ,1 ,1 ,1    ,1 ,1 ,1 ,1 ,1 ,1  ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,summMtx.sum = c(raw=2 ,evt=2)
    ,summMtx.reb = NULL
    ,scMtx.sz = matrix( c( 2 ,2 ,1 ,1 ,1 ,1     ,2 ,2 ,1 ,1 ,1 ,1 ) ,byrow=T
                        #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                        #   "rebCnt" ,"rebDup"
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
    ,scMtx.sz.sum = c(rebCnt.r=2 ,rebCnt.e=2)
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
    ,rowReb =c( rawMin=2,lowE=2,rareE=1 )   ,rowRebDup=NULL
    ,summMtx = matrix(  # all ph fCol phReb xyCnt.fCol xyCnt.phase      / raw ,evt
                        c(  1 ,2 ,2 ,1 ,1 ,1    ,1 ,1 ,1 ,1 ,1 ,1  ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,summMtx.sum = c(raw=2 ,evt=2)
    ,summMtx.reb = NULL
    ,scMtx.sz = matrix( c( 2 ,2 ,1 ,1 ,1 ,1     ,2 ,2 ,1 ,1 ,1 ,1 ) ,byrow=T
                        #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                        #   "rebCnt" ,"rebDup"
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
    ,scMtx.sz.sum = c(rebCnt.r=2 ,rebCnt.e=2)
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
    ,rowReb =c( rawMin=2,lowE=2,rareE=1 )   ,rowRebDup=NULL
    ,summMtx = matrix(  # all ph fCol phReb xyCnt.fCol xyCnt.phase      / raw ,evt
                        c(  1 ,2 ,2 ,1 ,1 ,1    ,1 ,1 ,1 ,1 ,1 ,1  ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,summMtx.sum = c(raw=2 ,evt=2)
    ,summMtx.reb = NULL
    ,scMtx.sz = matrix( c( 2 ,2 ,1 ,1 ,1 ,1     ,2 ,2 ,1 ,1 ,1 ,1 ) ,byrow=T
                        #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                        #   "rebCnt" ,"rebDup"
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
    ,scMtx.sz.sum = c(rebCnt.r=2 ,rebCnt.e=2)
    ,isHard=NULL  # use default
)

#   873, 880, 889에서 colA13,colB13  에서 3이 발생된 적이 있긴 하다..
mName <- "sScore0LAe13"
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
    ,rowReb =c( rawMin=2,lowE=2,rareE=1 )   ,rowRebDup=NULL
    ,summMtx = matrix(  # all ph fCol phReb xyCnt.fCol xyCnt.phase      / raw ,evt
                        c(  1 ,2 ,2 ,1 ,2 ,2    ,1 ,1 ,1 ,1 ,2 ,2  ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,summMtx.sum = c(raw=2 ,evt=2)
    ,summMtx.reb = NULL
    ,scMtx.sz = matrix( c( 2 ,2 ,1 ,1 ,1 ,1     ,2 ,2 ,1 ,1 ,1 ,1 ) ,byrow=T
                        #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                        #   "rebCnt" ,"rebDup"
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
    ,scMtx.sz.sum = c(rebCnt.r=2 ,rebCnt.e=2)
    ,isHard=NULL  # use default
)

mName <- "sScore0LAe24"
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
    ,rowReb =c( rawMin=2,lowE=2,rareE=1 )   ,rowRebDup=NULL
    ,summMtx = matrix(  # all ph fCol phReb xyCnt.fCol xyCnt.phase      / raw ,evt
                        c(  1 ,2 ,2 ,1 ,2 ,2    ,1 ,1 ,1 ,1 ,2 ,2  ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,summMtx.sum = c(raw=2 ,evt=2)
    ,summMtx.reb = NULL
    ,scMtx.sz = matrix( c( 2 ,2 ,1 ,1 ,1 ,1     ,2 ,2 ,1 ,1 ,1 ,1 ) ,byrow=T
                        #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                        #   "rebCnt" ,"rebDup"
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
    ,scMtx.sz.sum = c(rebCnt.r=2 ,rebCnt.e=2)
    ,isHard=NULL  # use default
)

mName <- "sScore0LVe13"
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
    ,rowReb =c( rawMin=2,lowE=2,rareE=1 )   ,rowRebDup=NULL
    ,summMtx = matrix(  # all ph fCol phReb xyCnt.fCol xyCnt.phase      / raw ,evt
                        c(  1 ,2 ,2 ,1 ,2 ,2    ,1 ,1 ,1 ,1 ,2 ,2  ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,summMtx.sum = c(raw=2 ,evt=2)
    ,summMtx.reb = NULL
    ,scMtx.sz = matrix( c( 2 ,2 ,1 ,1 ,1 ,1     ,2 ,2 ,1 ,1 ,1 ,1 ) ,byrow=T
                        #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                        #   "rebCnt" ,"rebDup"
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
    ,scMtx.sz.sum = c(rebCnt.r=2 ,rebCnt.e=2)
    ,isHard=NULL  # use default
)

mName <- "sScore0LVe24"
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
    ,rowReb =c( rawMin=2,lowE=2,rareE=1 )   ,rowRebDup=NULL
    ,summMtx = matrix(  # all ph fCol phReb xyCnt.fCol xyCnt.phase      / raw ,evt
                        c(  1 ,2 ,2 ,1 ,2 ,2    ,1 ,1 ,1 ,1 ,2 ,2  ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,summMtx.sum = c(raw=2 ,evt=2)
    ,summMtx.reb = NULL
    ,scMtx.sz = matrix( c( 2 ,2 ,1 ,1 ,1 ,1     ,2 ,2 ,1 ,1 ,1 ,1 ) ,byrow=T
                        #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                        #   "rebCnt" ,"rebDup"
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
    ,scMtx.sz.sum = c(rebCnt.r=2 ,rebCnt.e=2)
    ,isHard=NULL  # use default
)

mName <- "sScore0LAc13"
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
    ,rowReb =c( rawMin=2,lowE=2,rareE=1 )   ,rowRebDup=NULL
    ,summMtx = matrix(  # all ph fCol phReb xyCnt.fCol xyCnt.phase      / raw ,evt
                        c(  1 ,2 ,2 ,1 ,1 ,1    ,1 ,1 ,1 ,1 ,1 ,1  ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,summMtx.sum = c(raw=2 ,evt=2)
    ,summMtx.reb = NULL
    ,scMtx.sz = matrix( c( 2 ,2 ,1 ,1 ,1 ,1     ,2 ,2 ,1 ,1 ,1 ,1 ) ,byrow=T
                        #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                        #   "rebCnt" ,"rebDup"
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
    ,scMtx.sz.sum = c(rebCnt.r=2 ,rebCnt.e=2)
    ,isHard=NULL  # use default
)

mName <- "sScore0LAc24"
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
    ,rowReb =c( rawMin=2,lowE=2,rareE=1 )   ,rowRebDup=NULL
    ,summMtx = matrix(  # all ph fCol phReb xyCnt.fCol xyCnt.phase      / raw ,evt
                        c(  1 ,2 ,2 ,1 ,1 ,1    ,1 ,1 ,1 ,1 ,1 ,1  ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,summMtx.sum = c(raw=2 ,evt=2)
    ,summMtx.reb = NULL
    ,scMtx.sz = matrix( c( 2 ,2 ,1 ,1 ,1 ,1     ,2 ,2 ,1 ,1 ,1 ,1 ) ,byrow=T
                        #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                        #   "rebCnt" ,"rebDup"
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
    ,scMtx.sz.sum = c(rebCnt.r=2 ,rebCnt.e=2)
    ,isHard=NULL  # use default
)

mName <- "sScore0LVc13"
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
    ,rowReb =c( rawMin=2,lowE=2,rareE=1 )   ,rowRebDup=NULL
    ,summMtx = matrix(  # all ph fCol phReb xyCnt.fCol xyCnt.phase      / raw ,evt
                        c(  1 ,2 ,2 ,1 ,1 ,1    ,1 ,1 ,1 ,1 ,1 ,1  ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,summMtx.sum = c(raw=2 ,evt=2)
    ,summMtx.reb = NULL
    ,scMtx.sz = matrix( c( 2 ,2 ,1 ,1 ,1 ,1     ,2 ,2 ,1 ,1 ,1 ,1 ) ,byrow=T
                        #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                        #   "rebCnt" ,"rebDup"
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
    ,scMtx.sz.sum = c(rebCnt.r=2 ,rebCnt.e=2)
    ,isHard=NULL  # use default
)

mName <- "sScore0LVc24"
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
    ,rowReb =c( rawMin=2,lowE=2,rareE=1 )   ,rowRebDup=NULL
    ,summMtx = matrix(  # all ph fCol phReb xyCnt.fCol xyCnt.phase      / raw ,evt
                        c(  1 ,2 ,2 ,1 ,1 ,1    ,1 ,1 ,1 ,1 ,1 ,1  ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,summMtx.sum = c(raw=2 ,evt=2)
    ,summMtx.reb = NULL
    ,scMtx.sz = matrix( c( 2 ,2 ,1 ,1 ,1 ,1     ,2 ,2 ,1 ,1 ,1 ,1 ) ,byrow=T
                        #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                        #   "rebCnt" ,"rebDup"
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
    ,scMtx.sz.sum = c(rebCnt.r=2 ,rebCnt.e=2)
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
    ,rowReb =c( rawMin=2,lowE=2,rareE=1 )   ,rowRebDup=NULL
    ,summMtx = matrix(  # all ph fCol phReb xyCnt.fCol xyCnt.phase      / raw ,evt
                        c(  1 ,2 ,2 ,1 ,1 ,1    ,1 ,1 ,1 ,1 ,1 ,1  ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,summMtx.sum = c(raw=2 ,evt=2)
    ,summMtx.reb = NULL
    ,scMtx.sz = matrix( c( 2 ,2 ,1 ,1 ,1 ,1     ,2 ,2 ,1 ,1 ,1 ,1 ) ,byrow=T
                        #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                        #   "rebCnt" ,"rebDup"
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
    ,scMtx.sz.sum = c(rebCnt.r=2 ,rebCnt.e=2)
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
    ,rowReb =c( rawMin=2,lowE=2,rareE=1 )   ,rowRebDup=NULL
    ,summMtx = matrix(  # all ph fCol phReb xyCnt.fCol xyCnt.phase      / raw ,evt
                        c(  1 ,2 ,2 ,1 ,1 ,1    ,1 ,1 ,1 ,1 ,1 ,1  ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,summMtx.sum = c(raw=2 ,evt=2)
    ,summMtx.reb = NULL
    ,scMtx.sz = matrix( c( 2 ,2 ,1 ,1 ,1 ,1     ,2 ,2 ,1 ,1 ,1 ,1 ) ,byrow=T
                        #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                        #   "rebCnt" ,"rebDup"
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
    ,scMtx.sz.sum = c(rebCnt.r=2 ,rebCnt.e=2)
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
    ,rowReb =c( rawMin=2,lowE=2,rareE=1 )   ,rowRebDup=NULL
    ,summMtx = matrix(  # all ph fCol phReb xyCnt.fCol xyCnt.phase      / raw ,evt
                        c(  1 ,2 ,2 ,1 ,1 ,1    ,1 ,1 ,1 ,1 ,1 ,1  ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,summMtx.sum = c(raw=2 ,evt=2)
    ,summMtx.reb = NULL
    ,scMtx.sz = matrix( c( 2 ,2 ,1 ,1 ,1 ,1     ,2 ,2 ,1 ,1 ,1 ,1 ) ,byrow=T
                        #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                        #   "rebCnt" ,"rebDup"
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
    ,scMtx.sz.sum = c(rebCnt.r=2 ,rebCnt.e=2)
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
    ,rowReb =c( rawMin=2,lowE=2,rareE=1 )   ,rowRebDup=NULL
    ,summMtx = matrix(  # all ph fCol phReb xyCnt.fCol xyCnt.phase      / raw ,evt
                        c(  1 ,2 ,2 ,1 ,1 ,1    ,1 ,1 ,1 ,1 ,1 ,1  ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,summMtx.sum = c(raw=2 ,evt=2)
    ,summMtx.reb = NULL
    ,scMtx.sz = matrix( c( 2 ,2 ,1 ,1 ,1 ,1     ,2 ,2 ,1 ,1 ,1 ,1 ) ,byrow=T
                        #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                        #   "rebCnt" ,"rebDup"
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
    ,scMtx.sz.sum = c(rebCnt.r=2 ,rebCnt.e=2)
    ,isHard=NULL  # use default
)



# Multi_R ------------------------------------------------
mName <- "bsMR4567"
if( FALSE ){
    bsScoreMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "pBanN.r"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                    ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                    ,evtMax.fCol=NULL
            ) 
            ,"pBanN.n"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                    ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                    ,evtMax.fCol=NULL
            )
            ,"pLCol"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                    ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                    ,evtMax.fCol=NULL
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
            ,"iBanN"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                    ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                    ,evtMax.fCol=NULL
            )
            ,"iLCol"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                    ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
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
            )
            ,"FVa.c"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                    ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                    ,evtMax.fCol=NULL
            )
            ,"m4"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                    ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                    ,evtMax.fCol=NULL
            )
            ,"xBan.x"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                    ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                    ,evtMax.fCol=NULL
            )
            ,"xLCol"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                    ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                    ,evtMax.fCol=NULL
            )
            ,"xEn"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                    ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                    ,evtMax.fCol=NULL
            )
            ,"xfNum"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                    ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                    ,evtMax.fCol=NULL
            )
            ,"xMH"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                    ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                    ,evtMax.fCol=NULL
            )
            ,"eSum_FVaM4"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                    ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                    ,evtMax.fCol=NULL
            )
        )
        ,evtMax = NULL  ,evtMaxFColTot = NULL
        ,rowReb = NULL  ,rowRebDup=NULL
        ,summMtx = NULL ,summMtx.sum = NULL ,summMtx.reb = NULL
        ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
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
        bsScoreMtxCfg[[mName]]$rowReb <- c( rawMin=1 ,lowE=2 ,rareE=1 )
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



