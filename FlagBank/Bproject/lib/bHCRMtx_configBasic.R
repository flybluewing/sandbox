HCRMtxCfg <- list()


mName <- "HCRsz_bf01Ph"
if( FALSE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "score1"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"score2"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"score3"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,freqVal=c(1)
                        )
            ,"score5"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"score8"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"score9"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = NULL
        ,rowReb = c( rawMin=1 ,lowE=3 ,rareE=1 ,dupESum=2 )
        ,isHard=NULL  # use default
    )

}

mName <- "HCRsz_bf01fCol"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "score1"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,1,3,3),c(1,1,3,4)) ,ncol=2)
                        ) 
            ,"score2"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"score3"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"score5"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"score8"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"score9"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = NULL
        ,rowReb = NULL
        ,isHard=NULL  # use default
    )

}

mName <- "HCRreb_szC01R"
if( FALSE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "score1"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"score2"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore01"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore02"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = NULL
        ,rowReb = NULL
        ,isHard=NULL  # use default
    )

}

for( mName in names( HCRMtxCfg ) ){

    for( fcName in names(HCRMtxCfg[[mName]]$fCol) ){
        colnames(HCRMtxCfg[[mName]]$fCol[[fcName]]$rng) <- c("lev1","lev2")
        rownames(HCRMtxCfg[[mName]]$fCol[[fcName]]$rng) <- c("min","max")

        colnames(HCRMtxCfg[[mName]]$fCol[[fcName]]$evt) <- c("lev","val")

        # ,forbidEvtReb=c(2,3) ,freqVal=c(1,2)
        if( is.null(HCRMtxCfg[[mName]]$fCol[[fcName]]$forbidEvtReb) ){
            HCRMtxCfg[[mName]]$fCol[[fcName]]$forbidEvtReb <- c(2,3,4)
        }
    }

    if( is.null(HCRMtxCfg[[mName]]$evtMax) ){
        #   이벤트 발생 제한.
        #   "minLev"이상 이벤트가 maxHpn 보다 초과할 때 Cut
        HCRMtxCfg[[mName]]$evtMax = matrix( c(2,2,3,1 ,2,3,3,2)     ,byrow=T ,ncol=4
                            ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
    }
    if( is.null(HCRMtxCfg[[mName]]$rowReb) ){
        HCRMtxCfg[[mName]]$rowReb = c( rawMin=1 ,lowE=2 ,rareE=1 ,dupESum=2 )
    }

}
