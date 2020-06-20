
mName <- "score1"
scrExtMtxCfg[[mName]] <- list()
if( TRUE ){
    scrExtMtxCfg[[mName]]$filter01 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "remN.num"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(1,2,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                        ) 
            ,"remN.len.tot"=list( rng=matrix( c(0,4 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3),c(3,4,5,6)) ,ncol=2)
                        )
            ,"remN.len.val"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                        )
            ,"cN.num"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                        )
            ,"cN.len.tot"=list( rng=matrix( c(0,4 ,0,5) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(3,4,5,6)) ,ncol=2)
                        )
            ,"cN.len.val"=list( rng=matrix( c(0,4 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)
                        )
            ,"fN.num"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                        )
            ,"fN.len.tot"=list( rng=matrix( c(0,4 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3),c(3,4,5,6)) ,ncol=2)
                        )
            ,"fN.len.val"=list( rng=matrix( c(0,4 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3),c(3,4,5,6)) ,ncol=2)
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
            "evt0.num"=list( rng=matrix( c(0,4 ,0,6) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(1,2,3,3,3,3,3),c(2,3,4,5,6,7,8)) ,ncol=2)
                        ) 
            ,"evt0.len.tot"=list( rng=matrix( c(0,4 ,0,6) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3,3,3),c(2,3,4,5,6,7,8)) ,ncol=2)
                        )
            ,"evt0.len.val"=list( rng=matrix( c(0,4 ,0,6) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3,3,3),c(2,3,4,5,6,7,8)) ,ncol=2)
                        )
            ,"evt1.num"=list( rng=matrix( c(0,4 ,0,6) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3,3,3),c(2,3,4,5,6,7,8)) ,ncol=2)
                        )
            ,"evt1.len.tot"=list( rng=matrix( c(0,4 ,0,6) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3,3,3),c(2,3,4,5,6,7,8)) ,ncol=2)
                        )
            ,"evt1.len.val"=list( rng=matrix( c(0,4 ,0,6) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3,3,3),c(2,3,4,5,6,7,8)) ,ncol=2)
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
            "rebCN.r"=list( rng=matrix( c(0,3 ,0,5) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5,6)) ,ncol=2)
                        ) 
            ,"rebCN.c"=list( rng=matrix( c(0,5 ,0,5) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3),c(3,4,5,6,7)) ,ncol=2)
                        )
            ,"rebCN.f"=list( rng=matrix( c(0,4 ,0,5) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3),c(3,4,5,6,7)) ,ncol=2)
                        )
            ,"incN.r"=list( rng=matrix( c(0,3 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3),c(3,4,5,6)) ,ncol=2)
                        )
            ,"incN.c"=list( rng=matrix( c(0,3 ,0,5) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3),c(3,4,5,6,7)) ,ncol=2)
                        )
            ,"incN.f"=list( rng=matrix( c(0,4 ,0,5) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3),c(3,4,5,6,7)) ,ncol=2)
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
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                        ) 
            ,"evtRebC"=list( rng=matrix( c(0,3 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                        )
            ,"evtRebC2"=list( rng=matrix( c(0,3 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                        )
            ,"evtInc"=list( rng=matrix( c(0,3 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                        )
            ,"evtInc2"=list( rng=matrix( c(0,3 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                        )
            ,"evtInc3"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                        )
        )
        ,evtMax = NULL        ,rowReb = NULL        ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )
}










#-[Parallel init work]-------------------------------------------------------------
source("header.r")
source("B_H.R")

prllNum <- 5     # 실수가 잦아서 그냥 오류 코드로 놔둔다.
prllLog <- k.getFlogObj( "./log/parallel_log_Hist.txt" )
prll.initHeader <- function( ){
    k <- sfLapply(1:prllNum,function(prllId){
        curWd <- getwd()
        setwd("..")
        source("hCommon.R")

        setwd( curWd )
        source("header.r")
        source("B_H.R")
    })
}

sfInit( parallel=T, cpus=prllNum )


for( lastH in c( 820,840,860,880,900) ){
    load(sprintf("../Aproject/Obj_allIdxLstZ%d.save",lastH) )
    load(sprintf("../Aproject/save/Obj_fRstLstZ%d.save",lastH) )
    names(fRstLst) <- names(allIdxLst$stdFiltedCnt)
    load(sprintf("../Aproject/save/Obj_gEnvZ%d.save",lastH))

    sfExport("prllLog") ;sfExport("lastH")
    sfExport("gEnv")    ;sfExport("fRstLst")    ;sfExport("allIdxLst")
    prll.initHeader( )
    prllLog$fLogStr("parallel init", pTime=T ,pAppend=F )
    cat(sprintf("* Parallel ready... see log : %s \n",prllLog$fileName))

    tgt.scMtx <- NULL       # default : NULL   하도 실수가 잦아서 일부러 문법 오류로 놔둔다.. -_-;
        #   "bScr01"

    configH <- lastH-20    # configH는 기본 cutting값을 얻기 위하는 시점에 따라 조절.
    testSpan <- (lastH - 19:0)   # configH 보다는 큰 시점에서 시작해야 함을 유의.
    if( TRUE ){ # stdFiltedCnt 0~2내에서만 테스트
        sfc.InTest <- allIdxLst$stdFiltedCnt[as.character(testSpan)]
        testSpan <- testSpan[sfc.InTest %in% 0:2]
    }

    # hMtxLst <- B.makeHMtxLst( gEnv, allIdxLst, fRstLst, lastH=configH, tgt.scMtx )
    testData.grp <- B.get_testData.grp( testSpan ,gEnv ,allIdxLst ,fRstLst ,tgt.scMtx=tgt.scMtx)
    save( testData.grp ,file=sprintf("Obj_testData.grp.%d.%s.save",lastH,ifelse(is.null(tgt.scMtx),"all",tgt.scMtx) ) )
}


