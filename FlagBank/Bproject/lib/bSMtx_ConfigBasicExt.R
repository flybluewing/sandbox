bsScrExtMtxCfg <- list()


mName <- "sScore01"
if( FALSE ){

    bsScrExtMtxCfg[[mName]] <- list()

    bsScrExtMtxCfg[[mName]]$filter01 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "remN.num"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ) 
            ,"remN.len.tot"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"remN.len.val"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"cN.num"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"cN.len.tot"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"cN.len.val"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"fN.num"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"fN.len.tot"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"fN.len.val"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = NULL        ,rowReb = NULL                        ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL                   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )

    bsScrExtMtxCfg[[mName]]$filter02 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "evt0.num"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ) 
            ,"evt0.len.tot"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"evt0.len.val"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"evt1.num"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"evt1.len.tot"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"evt1.len.val"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = NULL        ,rowReb = NULL                        ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL                   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )

}


mName <- "sScore02"
if( FALSE ){

    bsScrExtMtxCfg[[mName]] <- list()

    bsScrExtMtxCfg[[mName]]$filter01 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "xxx"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ) 
            ,"xxx"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = NULL        ,rowReb = NULL                        ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL                   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )

    bsScrExtMtxCfg[[mName]]$filter02 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "xxx"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ) 
            ,"xxx"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = NULL        ,rowReb = NULL                        ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL                   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )

}


mName <- "sScore09"
if( FALSE ){

    bsScrExtMtxCfg[[mName]] <- list()

    bsScrExtMtxCfg[[mName]]$filter01 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "xCntECnt"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ) 
            ,"xD2ECnt"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"xDnECnt"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"xLrECnt"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"xRlECnt"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"r2EvtCnt"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"e2EvtCnt"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"c2EvtCnt"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"f2EvtCnt"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = NULL        ,rowReb = c(rawMin=2,lowE=2,rareE=1)  ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL                   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )

}



