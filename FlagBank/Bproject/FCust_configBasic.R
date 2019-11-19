

scoreMtxCfg <- list()

mName <- "score1"
scoreMtxCfg[[mName]] <- list(
    mtxName = mName
    ,fCol = list(
        "rem0.num"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,1,3),c(10,20,30)) ,ncol=2)
                    ) 
        ,"rem0.len.tot"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,1,3),c(10,20,30)) ,ncol=2)
                    )
        ,"rem0.len.val"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,1,3),c(10,20,30)) ,ncol=2)
                    )
        ,"rem1.num"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,1,3),c(10,20,30)) ,ncol=2)
                    )
        ,"rem1.len.tot"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,1,3),c(10,20,30)) ,ncol=2)
                    )
        ,"rem1.len.val"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,1,3),c(10,20,30)) ,ncol=2)
                    )
        ,"c0.num"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,1,3),c(10,20,30)) ,ncol=2)
                    )
        ,"c0.len.tot"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,1,3),c(10,20,30)) ,ncol=2)
                    )
        ,"c0.len.val"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,1,3),c(10,20,30)) ,ncol=2)
                    )
        ,"c1.num"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,1,3),c(10,20,30)) ,ncol=2)
                    )
        ,"c1.len.tot"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,1,3),c(10,20,30)) ,ncol=2)
                    )
        ,"c1.len.val"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,1,3),c(10,20,30)) ,ncol=2)
                    )
        ,"f0.num"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,1,3),c(10,20,30)) ,ncol=2)
                    )
        ,"f0.len.tot"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,1,3),c(10,20,30)) ,ncol=2)
                    )
        ,"f0.len.val"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,1,3),c(10,20,30)) ,ncol=2)
                    )
        ,"f1.num"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,1,3),c(10,20,30)) ,ncol=2)
                    )
        ,"f1.len.tot"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,1,3),c(10,20,30)) ,ncol=2)
                    )
        ,"f1.len.val"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,1,3),c(10,20,30)) ,ncol=2)
                    )
        ,"zwNum"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,1,3),c(10,20,30)) ,ncol=2)
                    )
        ,"zwC1Num"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,1,3),c(10,20,30)) ,ncol=2)
                    )
    )
    ,rowReb = NULL  # 이건 만들면서 고려해보자.
    ,isHard=NULL  # use default
)

mName <- "score2"
scoreMtxCfg[[mName]] <- list(
    mtxName = mName
    ,fCol = list(
        "rebV.r"=list( rng=matrix( c(0,1 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(1,1,3),c(10,20,30)) ,ncol=2)
                    )
        ,"rebL"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,1,3),c(10,20,30)) ,ncol=2)
                    ) 
        ,"rebR"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,1,3),c(10,20,30)) ,ncol=2)
                    ) 
        ,"rebC.r"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(1,1,3),c(10,20,30)) ,ncol=2)
                    ) 
        ,"rebC.c"=list( rng=matrix( c(0,1 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(1,1,3),c(10,20,30)) ,ncol=2)
                    ) 
        ,"rebC.f"=list( rng=matrix( c(0,1 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(1,1,3),c(10,20,30)) ,ncol=2)
                    ) 
        ,"rebC2.r"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(1,1,3),c(10,20,30)) ,ncol=2)
                    ) 
        ,"rebC2.c"=list( rng=matrix( c(0,1 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(1,1,3),c(10,20,30)) ,ncol=2)
                    ) 
        ,"rebC2.f"=list( rng=matrix( c(0,1 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(1,1,3),c(10,20,30)) ,ncol=2)
                    ) 
        ,"inc.r"=list( rng=matrix( c(0,1 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(1,1,3),c(10,20,30)) ,ncol=2)
                    ) 
        ,"inc.c"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(1,1,3),c(10,20,30)) ,ncol=2)
                    ) 
        ,"inc.f"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(1,1,3),c(10,20,30)) ,ncol=2)
                    ) 
        ,"inc.r2"=list( rng=matrix( c(0,1 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(1,1,3),c(10,20,30)) ,ncol=2)
                    ) 
        ,"inc.c2"=list( rng=matrix( c(0,1 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(1,1,3),c(10,20,30)) ,ncol=2)
                    ) 
        ,"inc.f2"=list( rng=matrix( c(0,1 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(1,1,3),c(10,20,30)) ,ncol=2)
                    ) 
        ,"inc.r3"=list( rng=matrix( c(0,1 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(1,1,3),c(10,20,30)) ,ncol=2)
                    ) 
        ,"inc.c3"=list( rng=matrix( c(0,1 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(1,1,3),c(10,20,30)) ,ncol=2)
                    ) 
    ) 
    ,rowReb = NULL  # 이건 만들면서 고려해보자.
    ,isHard=NULL  # use default
)




for( mName in names(scoreMtxCfg) ){ # naming 추가.
    for( fcName in names(scoreMtxCfg[[mName]]$fCol) ){
        colnames(scoreMtxCfg[[mName]]$fCol[[fcName]]$rng) <- c("lev1","lev2")
        rownames(scoreMtxCfg[[mName]]$fCol[[fcName]]$rng) <- c("min","max")

        colnames(scoreMtxCfg[[mName]]$fCol[[fcName]]$evt) <- c("lev","val")
    }
}






