scrExtMtxCfg <- list()

mName <- "score1"
scrExtMtxCfg[[mName]] <- list()
if( TRUE ){
    scrExtMtxCfg[[mName]]$filter01 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "remN.num"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"remN.len.tot"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"remN.len.val"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"cN.num"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"cN.len.tot"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"cN.len.val"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"fN.num"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"fN.len.tot"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"fN.len.val"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = NULL        ,rowReb = NULL        ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )
    scrExtMtxCfg[[mName]]$filter02 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "evt0.num"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"evt0.len.tot"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"evt0.len.val"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"evt1.num"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"evt1.len.tot"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"evt1.len.val"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = NULL        ,rowReb = NULL        ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )
}

mName <- "score2"
scrExtMtxCfg[[mName]] <- list()
if( TRUE ){
    scrExtMtxCfg[[mName]]$filter01 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "rebCN.r"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"rebCN.c"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"rebCN.f"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"incN.r"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"incN.c"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"incN.f"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = NULL        ,rowReb = NULL        ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )
    scrExtMtxCfg[[mName]]$filter02 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "evtRebLR"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"evtRebC"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"evtRebC2"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"evtInc"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"evtInc2"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"evtInc3"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = NULL        ,rowReb = NULL        ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )
}


mName <- "codeSample"
if( FALSE ){    # sample
    scrExtMtxCfg[[mName]] <- list()

    scrExtMtxCfg[[mName]]$filter01 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "col02"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"col02"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = NULL        ,rowReb = NULL        ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )
    scrExtMtxCfg[[mName]]$filter02 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "col02"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"col02"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = NULL        ,rowReb = NULL        ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )
}


for( mName in names( scrExtMtxCfg ) ){              # mName <- names( scrExtMtxCfg )[1]
    for( fName in names(scrExtMtxCfg[[mName]]) ){   # fName <- names(scrExtMtxCfg[[mName]])[1]
        # scrExtMtxCfg[[mName]][[fName]]
        for( fcName in names(scrExtMtxCfg[[mName]][[fName]]$fCol) ){    # fcName <- names(scrExtMtxCfg[[mName]][[fName]]$fCol)[1]
            colnames(scrExtMtxCfg[[mName]][[fName]]$fCol[[fcName]]$rng) <- c("lev1","lev2")
            rownames(scrExtMtxCfg[[mName]][[fName]]$fCol[[fcName]]$rng) <- c("min","max")

            colnames(scrExtMtxCfg[[mName]][[fName]]$fCol[[fcName]]$evt) <- c("lev","val")

            if( is.null(scrExtMtxCfg[[mName]][[fName]]$fCol[[fcName]]$evtMax.fCol ) ){
                # fCol 별 전체 phase 대상으로 evt 발생 제한.
                scrExtMtxCfg[[mName]][[fName]]$fCol[[fcName]]$evtMax.fCol <- c( minLev=2 ,maxHpn=2 )
            }
        }

        if( is.null(scrExtMtxCfg[[mName]][[fName]]$evtMax) ){
            #   한 개 phase 내에서의 이벤트 발생 제한.
            #   "minLev"이상 이벤트가 maxHpn 보다 초과할 때 Cut
            evtMax <- matrix( c(2,2,3,1 ,2,3,3,2)
                                ,byrow=T ,ncol=4
                                ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
                            )
            scrExtMtxCfg[[mName]][[fName]]$evtMax     <- evtMax
        }
        if( is.null(scrExtMtxCfg[[mName]][[fName]]$rowReb) ){
            scrExtMtxCfg[[mName]][[fName]]$rowReb <- c( rawMin=1 ,lowE=2 ,rareE=1 )
        }
        if( is.null(scrExtMtxCfg[[mName]][[fName]]$rowRebDup) ){  # 조건 : >=
            scrExtMtxCfg[[mName]][[fName]]$rowRebDup <- c( lowE=1 ,rareE=1 )
        }
        if( is.null(scrExtMtxCfg[[mName]][[fName]]$summMtx) ){
            #     $summMtx    all ph fCol phReb xyCnt.fCol xyCnt.phase
            #             raw   0  0    0     0          0           0
            #             evt   0  0    0     0          0           0
            # cName <- c( "all" ,"ph" ,"fCol" ,"phReb" ,"xyCnt.fCol" ,"xyCnt.phase" )
            # rName <- c( "raw" ,"evt" )
            thldVal <- c(   1 ,2 ,2 ,2 ,2 ,1    # xyCnt.fCol, xyCnt.fCol은 
                            ,1 ,2 ,2 ,2 ,2 ,1 
                        )
            scrExtMtxCfg[[mName]][[fName]]$summMtx <- matrix( thldVal ,byrow=T
                        ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                        ,dimnames=list(summMtxName$rName,summMtxName$cName)
                    )
        }
        if( is.null(scrExtMtxCfg[[mName]][[fName]]$summMtx.reb ) ){
            #     $summMtx.reb    all ph fCol phReb xyCnt.fCol xyCnt.phase
            #                 raw   0  0    0     0          0           0
            #                 evt   0  0    0     0          0           0
            cName <- c( "all" ,"ph" ,"fCol" ,"phReb" ,"xyCnt.fCol" ,"xyCnt.phase" )
            rName <- c( "raw" ,"evt" )
            thldVal <- c(   1 ,1 ,1 ,1 ,1 ,1    # xyCnt.fCol, xyCnt.fCol은 봐가며 조절해야 할 듯.
                            ,1 ,1 ,1 ,1 ,1 ,1 
                        )
            scrExtMtxCfg[[mName]][[fName]]$summMtx.reb <- matrix( thldVal ,byrow=T
                        ,ncol=length(cName) ,nrow=length(rName) ,dimnames=list(rName,cName)
                    )
        }
        if( is.null(scrExtMtxCfg[[mName]][[fName]]$scMtx.sz ) ){
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
            scrExtMtxCfg[[mName]][[fName]]$scMtx.sz <- matrix( thldVal ,byrow=T
                        ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                        ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                    )
        }
        if( is.null(scrExtMtxCfg[[mName]][[fName]]$summMtx.sum) ){
            # 개개 cut은 피했지만 전체 발생 총합은 한계를 넘는 경우.
            scrExtMtxCfg[[mName]][[fName]]$summMtx.sum <- c(raw=2 ,evt=2)
        }
        if( is.null(scrExtMtxCfg[[mName]][[fName]]$scMtx.sz.sum ) ){
            #   rebCnt.r = r.ph+r.fCol    rebCnt.e = e.ph+e.fCol
            scrExtMtxCfg[[mName]][[fName]]$scMtx.sz.sum <- c(rebCnt.r=2 ,rebCnt.e=2)
        }

    }   # fName
}   # mName

