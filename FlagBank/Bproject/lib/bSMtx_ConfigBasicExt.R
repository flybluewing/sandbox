bsScrExtMtxCfg <- list()

mName <- "score9"
if( TRUE ){

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



