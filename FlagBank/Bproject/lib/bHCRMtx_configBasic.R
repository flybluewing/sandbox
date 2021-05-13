HCRMtxCfg <- list()

# ------------------------------------------------------------------------------------------
#   szMtx for bFMtx (Ph,fCol)
# ------------------------------------------------------------------------------------------
mName <- "HCRsz_bf01Ph"
if( TRUE ){ # done. OK
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
mName <- "HCRsz_bf2APh"
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
             "score4"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"score6"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"score7"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreA"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"scoreB"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreC"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreD"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreE"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,freqVal=c(1)
                        )
            ,"scoreF"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,freqVal=c(1)
                        )
            ,"scoreLAe13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,freqVal=c(1)
                        )
            ,"scoreLAe24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,freqVal=c(1)
                        )
            ,"scoreLVe13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,freqVal=c(1)
                        )
            ,"scoreLVe24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,freqVal=c(1)
                        )
        )
        ,evtMax = NULL
        ,rowReb = NULL  # c( rawMin=1 ,lowE=2 ,rareE=1 ,dupESum=2 )
        ,isHard=NULL  # use default
    )
}
mName <- "HCRsz_bfavPh"
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
             "scoreLAr13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"scoreLAr24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLVr13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLVr24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLAc13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLAc24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLVc13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLVc24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLAf13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLAf24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLVf13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLVf24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = NULL
        ,rowReb = NULL  # c( rawMin=1 ,lowE=2 ,rareE=1 ,dupESum=2 )
        ,isHard=NULL  # use default
    )

}
mName <- "HCRsz_bfAZPh"
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
             "scoreA"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"scoreB"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreC"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreD"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreE"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreF"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreFV"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreGS"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"scoreGSh2"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,1,3,3,3,3,3),c(2,3,4,5,6,7,8)) ,ncol=2)
                        )
            ,"scoreGS3"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scorePSh"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,1,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scorePSrp"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scorePSrpRaw"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3),c(2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = NULL
        ,rowReb = c( rawMin=2 ,lowE=3 ,rareE=1 ,dupESum=2 )
        ,isHard=NULL  # use default
    )
}



mName <- "HCRsz_bf01fCol"
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "score1"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,1,3,3),c(1,2,3,4)) ,ncol=2)
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
mName <- "HCRsz_bf2AfCol"
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
             "score4"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"score6"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"score7"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreA"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"scoreB"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreC"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreD"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreE"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreF"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLAe13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLAe24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLVe13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLVe24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = NULL
        ,rowReb = NULL  # c( rawMin=1 ,lowE=2 ,rareE=1 ,dupESum=2 )
        ,isHard=NULL  # use default
    )
}
mName <- "HCRsz_bfavfCol"
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
             "scoreLAr13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"scoreLAr24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLVr13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLVr24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLAc13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLAc24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLVc13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLVc24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLAf13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLAf24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLVf13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLVf24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = NULL
        ,rowReb = NULL  # c( rawMin=1 ,lowE=2 ,rareE=1 ,dupESum=2 )
        ,isHard=NULL  # use default
    )
}
mName <- "HCRsz_bfAZfCol"
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
             "scoreA"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"scoreB"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreC"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreD"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreE"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreF"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreFV"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreGS"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreGSh2"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreGS3"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scorePSh"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scorePSrp"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scorePSrpRaw"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = NULL
        ,rowReb = c( rawMin=2 ,lowE=2 ,rareE=1 ,dupESum=2 )
        ,isHard=NULL  # use default
    )
}


# ------------------------------------------------------------------------------------------
#   szMtx for bSMtx (Ph,fCol)
# ------------------------------------------------------------------------------------------
mName <- "HCRsz_bS01Ph"
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "sScore01"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"sScore02"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore03"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore05"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore08"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore09"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = NULL
        ,rowReb = c( rawMin=2 ,lowE=2,rareE=1 ,dupESum=2 )
        ,isHard=NULL  # use default
    )
}
mName <- "HCRsz_bS2APh"
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "sScore04"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"sScore06"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore07"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LAe13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LAe24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LVe13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LVe24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = NULL
        ,rowReb = NULL  # c( rawMin=1 ,lowE=3 ,rareE=1 ,dupESum=2 )
        ,isHard=NULL  # use default
    )
}
mName <- "HCRsz_bSavPh"
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "sScore0LAr13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"sScore0LAr24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LVr13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LVr24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LAc13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LAc24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LVc13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LVc24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LAf13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LAf24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LVf13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LVf24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = NULL
        ,rowReb = c( rawMin=2 ,lowE=3 ,rareE=1 ,dupESum=2 )
        ,isHard=NULL  # use default
    )
}
mName <- "HCRsz_bSAZPh"
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "sScore0GS"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"sScore0GSh2"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0GS3"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0PSh"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0PSrp"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0PSrpRaw"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)  # H876(2)
                        )
        )
        ,evtMax = NULL
        ,rowReb = c( rawMin=2 ,lowE=3 ,rareE=1 ,dupESum=2 )     # H802(2)
        ,isHard=NULL  # use default
    )
}


mName <- "HCRsz_bS1avPh"    # bS01Ph + bSavPh
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "sScore01"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"sScore02"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore03"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore05"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,freqVal=c(1)   #
                        )
            ,"sScore08"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore09"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,freqVal=c(1)   #
                        )
            ,"sScore0LAr13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"sScore0LAr24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LVr13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LVr24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LAc13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LAc24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LVc13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LVc24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LAf13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LAf24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LVf13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LVf24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = NULL
        ,rowReb = c( rawMin=2 ,lowE=2 ,rareE=1 ,dupESum=2 )
                # 812 825 838 853 reb(1)
        ,isHard=NULL  # use default
    )
}
mName <- "HCRsz_bS01AVefCol"
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "sScore01"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,freqVal=c(1)
                        ) 
            ,"sScore02"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore03"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore05"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,freqVal=c(1)
                        )
            ,"sScore08"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore09"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,freqVal=c(1)
                        )
            ,"sScore0LAe13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LAe24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LVe13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LVe24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = NULL
        ,rowReb = c( rawMin=1 ,lowE=2,rareE=1 ,dupESum=2 )  # freqVal로 처리하긴 했는데... rawWin=2이 나으려나?
        ,isHard=NULL  # use default
    )
}
mName <- "HCRsz_bS2AfCol"
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "sScore01"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,freqVal=c(1)
                        ) 
            ,"sScore02"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,freqVal=c(1)
                        )
            ,"sScore03"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore04"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"sScore06"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore07"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LAe13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LAe24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LVe13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LVe24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = NULL
        ,rowReb = c( rawMin=1 ,lowE=2,rareE=1 ,dupESum=2 )
        ,isHard=NULL  # use default
    )
}
mName <- "HCRsz_bSavfCol"
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "sScore0LAr13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"sScore0LAr24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LVr13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LVr24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LAc13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LAc24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LVc13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LVc24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LAf13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LAf24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LVf13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LVf24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = NULL
        ,rowReb = c( rawMin=1 ,lowE=2,rareE=1 ,dupESum=2 )
        ,isHard=NULL  # use default
    )
}
mName <- "HCRsz_bSAZfCol"
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "sScore0GS"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"sScore0GSh2"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0GS3"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0PSh"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,1,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0PSrp"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0PSrpRaw"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,1,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = NULL
        ,rowReb = c( rawMin=2 ,lowE=2,rareE=1 ,dupESum=2 )
        ,isHard=NULL  # use default
    )
}



# ------------------------------------------------------------------------------------------
#   szMtx for bfMtx + bSMtx (Ph,fCol)
# ------------------------------------------------------------------------------------------
mName <- "HCRsz_bfS01Ph"
if( TRUE ){ # done. OK
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
            ,"sScore01"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"sScore02"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore03"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore05"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore08"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore09"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = NULL
        ,rowReb = c( rawMin=1 ,lowE=3,rareE=1 ,dupESum=2 )
        ,isHard=NULL  # use default
    )
}
mName <- "HCRsz_bfS2APh"
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "score4"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"score6"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"score7"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreA"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreB"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreC"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreD"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreE"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,freqVal=c(1)
                        )
            ,"scoreF"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore04"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore06"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore07"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            # ,"scoreLAe13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
            #                 ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
            #             )
            # ,"scoreLAe24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
            #                 ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
            #             )
            # ,"scoreLVe13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
            #                 ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
            #             )
            # ,"scoreLVe24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
            #                 ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
            #             )
            # ,"sScore0LAe13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
            #                 ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
            #             )
            # ,"sScore0LAe24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
            #                 ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
            #             )
            # ,"sScore0LVe13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
            #                 ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
            #             )
            # ,"sScore0LVe24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
            #                 ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
            #             )
        )
        ,evtMax = NULL
        ,rowReb = c( rawMin=1 ,lowE=3,rareE=1 ,dupESum=2 )
        ,isHard=NULL  # use default
    )
}
mName <- "HCRsz_bfSAVePh"
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "scoreLAe13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLAe24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLVe13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLVe24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LAe13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LAe24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LVe13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LVe24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = NULL
        ,rowReb = c( rawMin=1 ,lowE=3,rareE=1 ,dupESum=2 )  # H853 H884
        ,isHard=NULL  # use default
    )
}
mName <- "HCRsz_bfSavPh"
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "scoreLAr13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLAr24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLVr13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLVr24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLAc13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLAc24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLVc13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLVc24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLAf13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLAf24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLVf13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLVf24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )

            ,"sScore0LAr13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LAr24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LVr13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LVr24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LAc13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LAc24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LVc13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LVc24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LAf13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LAf24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LVf13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LVf24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = NULL
        ,rowReb = c( rawMin=1 ,lowE=3,rareE=1 ,dupESum=2 )
        ,isHard=NULL  # use default
    )
}
mName <- "HCRsz_bfSAZPh"
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "scoreA"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreB"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreC"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreD"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreE"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreF"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreFV"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreGS"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"scoreGSh2"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,1,3,3,3,3,3),c(2,3,4,5,6,7,8)) ,ncol=2)
                        )
            ,"scoreGS3"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scorePSh"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,1,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scorePSrp"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scorePSrpRaw"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"sScore0GS"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0GSh2"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0GS3"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0PSh"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0PSrp"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0PSrpRaw"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )   # H876
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = NULL
        ,rowReb = c( rawMin=1 ,lowE=4,rareE=1 ,dupESum=3 )
        ,isHard=NULL  # use default
    )
}




mName <- "HCRsz_bfS01fCol"
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "score1"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,1,3,3),c(1,2,3,4)) ,ncol=2)  # 
                        ) 
            ,"score2"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)  # 
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
            ,"sScore01"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"sScore02"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore03"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore05"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore08"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore09"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = NULL
        ,rowReb = c( rawMin=1 ,lowE=3,rareE=1 ,dupESum=2 )
        ,isHard=NULL  # use default
    )
}
mName <- "HCRsz_bfS2AfCol"
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "score4"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"score6"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"score7"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreA"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreB"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreC"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreD"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreE"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,freqVal=c(1)
                        )
            ,"scoreF"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore04"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore06"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore07"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            # ,"scoreLAe13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
            #                 ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
            #             )
            # ,"scoreLAe24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
            #                 ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
            #             )
            # ,"scoreLVe13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
            #                 ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
            #             )
            # ,"scoreLVe24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
            #                 ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
            #             )
            # ,"sScore0LAe13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
            #                 ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
            #             )
            # ,"sScore0LAe24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
            #                 ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
            #             )
            # ,"sScore0LVe13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
            #                 ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
            #             )
            # ,"sScore0LVe24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
            #                 ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
            #             )
        )
        ,evtMax = NULL
        ,rowReb = c( rawMin=1 ,lowE=3,rareE=1 ,dupESum=2 )
        ,isHard=NULL  # use default
    )
}
mName <- "HCRsz_bfSAVefCol"
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "scoreLAe13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLAe24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLVe13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLVe24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LAe13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LAe24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LVe13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LVe24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = NULL
        ,rowReb = c( rawMin=1 ,lowE=3,rareE=1 ,dupESum=2 )
        ,isHard=NULL  # use default
    )
}
mName <- "HCRsz_bfSavfCol"
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "scoreLAr13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLAr24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLVr13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLVr24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLAc13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLAc24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLVc13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLVc24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLAf13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLAf24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLVf13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreLVf24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )

            ,"sScore0LAr13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LAr24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LVr13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LVr24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LAc13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LAc24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LVc13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LVc24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LAf13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LAf24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LVf13"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0LVf24"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = NULL
        ,rowReb = c( rawMin=1 ,lowE=3,rareE=1 ,dupESum=2 )
        ,isHard=NULL  # use default
    )
}
mName <- "HCRsz_bfSAZfCol"
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "scoreA"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreB"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreC"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreD"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreE"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreF"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreFV"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreGS"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreGSh2"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreGS3"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scorePSh"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scorePSrp"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scorePSrpRaw"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0GS"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0GSh2"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0GS3"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0PSh"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,1,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0PSrp"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0PSrpRaw"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,1,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = NULL
        ,rowReb = c( rawMin=1 ,lowE=3,rareE=1 ,dupESum=2 )
        ,isHard=NULL  # use default
    )
}




# ------------------------------------------------------------------------------------------
#   szMtx for bFMtx/bSMtx reb (r.ph,r.fCol,r.dblHpnFlg)       HCR.MtxTmpl_rebSz()
# ------------------------------------------------------------------------------------------
#   발생 자체가 희귀해서, 1 발생을 하나만 허용하고, 1의 반복도 한 번만 허용하도록 세팅함.
#       - HCRreb_rawX01R_x, HCRreb_rawX02R_x 는 발생자체가 너무 희귀해서, bf와 bS를 합쳐도 될 듯.
mName <- "HCRreb_szC01R"
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "score1"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        ) 
            ,"score2"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"score3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"score5"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            # HCRreb_szCAavR
            ,"scoreA"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        ) 
            ,"scoreB"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreC"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreD"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreE"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreF"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreLAe13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreLAe24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreLVe13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreLVe24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
        )
        ,evtMax = matrix( c(2,2,3,1 ,2,2,3,2)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,isHard=NULL  # use default
    )
}
mName <- "HCRreb_szC02R"
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "score4"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        ) 
            ,"score6"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"score7"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"score8"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"score9"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            # HCRreb_szCavR
            ,"scoreLAr13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreLAr24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreLVr13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreLVr24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreLAc13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreLAc24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreLVc13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreLVc24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreLAf13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreLAf24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreLVf13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreLVf24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
        )
        ,evtMax = matrix( c(2,2,3,1 ,2,2,3,2)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,isHard=NULL  # use default
    )
}
mName <- "HCRreb_szC03R"
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "scoreA"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"scoreB"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreC"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreD"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreE"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreF"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreFV"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreGS"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,1,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreGSh2"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )            # H888(3)
                            ,evt=matrix( c(c(1,1,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreGS3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scorePSh"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scorePSrp"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scorePSrpRaw"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=2 ,lowE=2 ,rareE=1 ,dupESum=2 )
        ,isHard=NULL  # use default
    )
}
mName <- "HCRreb_szS01R"
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
             "sScore01"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        ) 
            ,"sScore02"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"sScore03"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"sScore05"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"sScore08"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"sScore09"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )

            ,"sScore0LAe13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"sScore0LAe24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"sScore0LVe13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"sScore0LVe24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
        )
        ,evtMax = matrix( c(2,2,3,1 ,2,2,3,2)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,isHard=NULL  # use default
    )
}
mName <- "HCRreb_szS02R"
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
             "sScore04"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        ) 
            ,"sScore06"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"sScore07"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )

            ,"sScore0LAr13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"sScore0LAr24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"sScore0LVr13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"sScore0LVr24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"sScore0LAc13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"sScore0LAc24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"sScore0LVc13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"sScore0LVc24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"sScore0LAf13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"sScore0LAf24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"sScore0LVf13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"sScore0LVf24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
        )
        ,evtMax = matrix( c(2,2,3,1 ,2,2,3,2)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,isHard=NULL  # use default
    )
}
mName <- "HCRreb_szS03R"    # Hpn이 너무 적은데 Reb는 발생... 되게 애매하다.
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "sScore0GS"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"sScore0GSh2"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"sScore0GS3"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"sScore0PSh"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"sScore0PSrp"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"sScore0PSrpRaw"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,2,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,isHard=NULL  # use default
    )
}

mName <- "HCRreb_rawC01R_a"     # c( "ph","fCol","phReb" )
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "score1"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        ) 
            ,"score2"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"score3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"score5"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            # --
            ,"scoreA"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        ) 
            ,"scoreB"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreC"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreD"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreE"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreF"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreLAe13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreLAe24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreLVe13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreLVe24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
        )
        ,evtMax = matrix( c(2,2,3,1 ,2,2,3,2)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 ) # c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,isHard=NULL  # use default
    )
}
mName <- "HCRreb_rawC01R_b"     # c( "all","phReb","xyCnt.fCol","xyCnt.phase" )
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "score1"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        ) 
            ,"score2"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"score3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"score5"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            # --
            ,"scoreA"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        ) 
            ,"scoreB"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreC"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreD"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreE"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreF"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreLAe13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreLAe24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreLVe13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreLVe24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
        )
        ,evtMax = matrix( c(2,2,3,1 ,2,2,3,2)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 ) # c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,isHard=NULL  # use default
    )
}
mName <- "HCRreb_rawC02R_a"
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "score4"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        ) 
            ,"score6"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"score7"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"score8"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"score9"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )

            ,"scoreLAr13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"scoreLAr24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"scoreLVr13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"scoreLVr24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"scoreLAc13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"scoreLAc24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"scoreLVc13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"scoreLVc24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"scoreLAf13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"scoreLAf24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"scoreLVf13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"scoreLVf24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
        )
        ,evtMax = matrix( c(2,2,3,1 ,2,2,3,2)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 ) # c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,isHard=NULL  # use default
    )
}
mName <- "HCRreb_rawC02R_b"
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "score4"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        ) 
            ,"score6"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"score7"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"score8"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"score9"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )

            ,"scoreLAr13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"scoreLAr24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"scoreLVr13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"scoreLVr24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"scoreLAc13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"scoreLAc24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"scoreLVc13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"scoreLVc24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"scoreLAf13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"scoreLAf24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"scoreLVf13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"scoreLVf24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
        )
        ,evtMax = matrix( c(2,2,3,1 ,2,2,3,2)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 ) # c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,isHard=NULL  # use default
    )
}
mName <- "HCRreb_rawC03R_a"
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "scoreA"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        ) 
            ,"scoreB"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"scoreC"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"scoreD"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"scoreE"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"scoreF"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"scoreFV"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"scoreGS"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"scoreGSh2"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"scoreGS3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"scorePSh"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"scorePSrp"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"scorePSrpRaw"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
        )
        # ,evtMax = NULL
        # ,rowReb = NULL
        ,evtMax = matrix( c(2,2,3,1 ,2,2,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=1 )
        ,isHard=NULL  # use default
    )
}
mName <- "HCRreb_rawC03R_b"
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "scoreA"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"scoreB"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreC"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreD"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreE"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreF"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreFV"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreGS"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreGSh2"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scoreGS3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scorePSh"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scorePSrp"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"scorePSrpRaw"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 )
        ,isHard=NULL  # use default
    )
}


mName <- "HCRreb_rawS01R_a"
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "sScore01"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        ) 
            ,"sScore02"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"sScore03"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"sScore05"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"sScore08"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"sScore09"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)    # H816
                        )
            ,"sScore0LAe13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"sScore0LAe24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"sScore0LVe13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"sScore0LVe24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
        )
        ,evtMax = matrix( c(2,2,3,1 ,2,2,3,2)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 ) # c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,isHard=NULL  # use default
    )
}
mName <- "HCRreb_rawS01R_b"
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "sScore01"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        ) 
            ,"sScore02"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"sScore03"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"sScore05"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"sScore08"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"sScore09"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"sScore0LAe13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"sScore0LAe24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"sScore0LVe13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"sScore0LVe24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
        )
        ,evtMax = matrix( c(2,2,3,1 ,2,2,3,2)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 ) # c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,isHard=NULL  # use default
    )
}
mName <- "HCRreb_rawS02R_a"
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "sScore04"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        ) 
            ,"sScore06"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"sScore07"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"sScore0LAr13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"sScore0LAr24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"sScore0LVr13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"sScore0LVr24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"sScore0LAc13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"sScore0LAc24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"sScore0LVc13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"sScore0LVc24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"sScore0LAf13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"sScore0LAf24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"sScore0LVf13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"sScore0LVf24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
        )
        ,evtMax = matrix( c(2,2,3,1 ,2,2,3,2)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 ) # c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,isHard=NULL  # use default
    )
}
mName <- "HCRreb_rawS02R_b"
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "sScore04"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        ) 
            ,"sScore06"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"sScore07"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"sScore0LAr13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"sScore0LAr24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"sScore0LVr13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"sScore0LVr24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"sScore0LAc13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"sScore0LAc24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"sScore0LVc13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"sScore0LVc24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"sScore0LAf13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"sScore0LAf24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"sScore0LVf13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
            ,"sScore0LVf24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,forbidEvtReb=c(3)
                        )
        )
        ,evtMax = matrix( c(2,2,3,1 ,2,2,3,2)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 ) # c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,isHard=NULL  # use default
    )
}
mName <- "HCRreb_rawS03R_a" # (너무 적은 mName)
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "sScore0GS"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"sScore0GSh2"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0GS3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0PSh"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0PSrp"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0PSrpRaw"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 )
        ,isHard=NULL  # use default
    )
}
mName <- "HCRreb_rawS03R_b" # (너무 적은 mName)
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "sScore0GS"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"sScore0GSh2"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0GS3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0PSh"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0PSrp"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"sScore0PSrpRaw"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 )
        ,isHard=NULL  # use default
    )
}



# ------------------------------------------------------------------------------------------
#   ph reb by mNames for bfMtx/bSMtx sz     HCR.MtxTmpl_phRebCnt_sz()
# ------------------------------------------------------------------------------------------
# HCRraw_bf01XXX 부터 작업한 후 진행.
mName <- "HCRreb_phSzF01"
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "basic"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"nextZW"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"nextQuo10"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"nextBin"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"nextRebNum"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"nextCStepBin"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"nextFStepBin"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"nextColVal_1"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"nextColVal_2"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"nextColVal_3"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"nextColVal_4"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 ) # H844
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"nextColVal_5"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 ) # H873
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"nextColVal_6"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,2,3,1 ,2,2,3,2)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 ) # c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,isHard=NULL  # use default
    )
}
mName <- "HCRreb_phSzS01"
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "colVal1"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"colVal3"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"colVal6"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )  # H844
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"remPair"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"zw"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"cSCVal1"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )      # H814
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"cSCVal2"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"cSCVal3"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"cSCVal4"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"cSCVal5"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,2,3,1 ,2,2,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 ) # c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,isHard=NULL  # use default
    )
}
mName <- "HCRreb_phSzF02"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "basic"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"nextZW"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"nextQuo10"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"nextBin"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"nextRebNum"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"nextCStepBin"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"nextFStepBin"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"nextColVal_1"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"nextColVal_2"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"nextColVal_3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"nextColVal_4"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"nextColVal_5"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"nextColVal_6"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,2,3,1 ,2,2,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 ) # c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,isHard=NULL  # use default
    )
}
mName <- "HCRreb_phSzS02" 
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "colVal1"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"colVal3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"colVal6"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"remPair"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"zw"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"cSCVal1"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"cSCVal2"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"cSCVal3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"cSCVal4"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"cSCVal5"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,2,3,1 ,2,2,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 ) # c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,isHard=NULL  # use default
    )
}


# ------------------------------------------------------------------------------------------
#   summMtx for bFMtx (all ph fCol phReb xyCnt.fCol xyCnt.phase)    HCR.MtxTmpl_rawReb()
# ------------------------------------------------------------------------------------------
#   HCR.MtxTmpl_szReb() 의 raw 버전. 
#   phReb xyCnt.fCol xyCnt.phase 에 대한 reb 체크를 위해 사용.
mName <- "HCRraw_bf01Sum01"
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "score1"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        ) 
            ,"score2"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"score3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"score5"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreA"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        ) 
            ,"scoreB"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreC"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreD"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreE"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreF"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreFV"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreLAe13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreLAe24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreLVe13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreLVe24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
        )
        ,evtMax = matrix( c(2,2,3,1 ,2,2,3,2)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,isHard=NULL  # use default
    )
}
mName <- "HCRraw_bf02Sum01"
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "score4"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        ) 
            ,"score6"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"score7"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"score8"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"score9"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreLAr13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreLAr24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreLVr13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreLVr24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreLAc13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreLAc24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreLVc13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreLVc24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreLAf13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreLAf24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreLVf13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"scoreLVf24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
        )
        ,evtMax = matrix( c(2,2,3,1 ,2,2,3,2)     ,byrow=T ,ncol=4      # H827
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,isHard=NULL  # use default
    )
}
mName <- "HCRraw_bf03Sum01"
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "scoreA"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        ) 
            ,"scoreB"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"scoreC"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"scoreD"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"scoreE"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"scoreF"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"scoreFV"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"scoreGS"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"scoreGSh2"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"scoreGS3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"scorePSh"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"scorePSrp"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"scorePSrpRaw"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
        )
        ,evtMax = matrix( c(2,2,3,1 ,2,2,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 ) # c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,isHard=NULL  # use default
    )
}
mName <- "HCRraw_bS01Sum01"
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "sScore01"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        ) 
            ,"sScore02"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"sScore03"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"sScore05"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"sScore0LAe13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"sScore0LAe24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"sScore0LVe13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"sScore0LVe24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
        )
        ,evtMax = matrix( c(2,2,3,1 ,2,2,3,2)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,isHard=NULL  # use default
    )
}
mName <- "HCRraw_bS02Sum01"
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "sScore04"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        ) 
            ,"sScore06"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"sScore07"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"sScore08"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"sScore09"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"sScore0LAr13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"sScore0LAr24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"sScore0LVr13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"sScore0LVr24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"sScore0LAc13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"sScore0LAc24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"sScore0LVc13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"sScore0LVc24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"sScore0LAf13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"sScore0LAf24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"sScore0LVf13"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
            ,"sScore0LVf24"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=c(3)
                        )
        )
        ,evtMax = matrix( c(2,2,3,1 ,2,2,3,2)     ,byrow=T ,ncol=4      # H827
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,isHard=NULL  # use default
    )
}
mName <- "HCRraw_bS03Sum01"
if( TRUE ){ # done. OK
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "sScore0GS"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        ) 
            ,"sScore0GSh2"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )  # H801
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"sScore0GS3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"sScore0PSh"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"sScore0PSrp"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"sScore0PSrpRaw"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4      # H853
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 ) # c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,isHard=NULL  # use default
    )
}


# ------------------------------------------------------------------------------------------
#   Reb for bCMtx.R ,bSMtx_Multi_C.R
# ------------------------------------------------------------------------------------------

#- HCR.MtxTmpl_crScrNnx -----------------------------------------------------------------------------------------------
mName <- "HCR_crScrN01_RszPhFCol"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "Fhpn0"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,freqVal=1
                        ) 
            ,"Fph_sz"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3),c(2,3,4)) ,ncol=2)
                            ,freqVal=1
                        )
            ,"FfCol_sz"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,freqVal=1
                        )
            ,"Shpn0"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,freqVal=1
                        )
            ,"Sph_sz"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,freqVal=1
                        )
            ,"SfCol_sz"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,freqVal=1
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=2 ,rareE=1 ,dupESum=2 ) # H842(3)   
        ,hIMtxHpnCnt = matrix( c(0,1,3,4,5,6  ,0,1,3,1,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(2,5  ,2,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN01_EszPhFCol"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "Fhpn0"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3),c(2,3,4)) ,ncol=2)
                        ) 
            ,"Fph_sz"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3),c(2,3,4)) ,ncol=2)
                            ,freqVal=1:2
                        )
            ,"FfCol_sz"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                            ,freqVal=1
                        )
            ,"Shpn0"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                            ,freqVal=1:2
                        )
            ,"Sph_sz"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3),c(2,3,4)) ,ncol=2)  # H896
                            ,freqVal=1
                        )
            ,"SfCol_sz"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=3 ,lowE=2 ,rareE=1 ,dupESum=2 )     # H838(3)
        ,hIMtxHpnCnt = matrix( c(0,1,4,5,6  ,0,1,1,1,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(2,5  ,2,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN01_RcommPhFCol"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "Fhpn0"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                            ,freqVal=1
                        ) 
            ,"Fph"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                            ,freqVal=1
                        )
            ,"FfCol"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"Shpn0"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3),c(2,3,4)) ,ncol=2)
                            ,freqVal=1:2
                        )
            ,"Sph"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"SfCol"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=2 ,rareE=1 ,dupESum=2 ) # c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(3,4,5,6  ,1,0,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(2,5  ,2,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,2    ,TRUE ,F ,2 )  # H854(2)  H879(2)  freqVal이 3개라서 적용가치 있다.
                ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) 
        )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN01_EcommPhFCol"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "Fhpn0"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                        ) 
            ,"Fph"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"FfCol"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"Shpn0"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3),c(2,3,4)) ,ncol=2)
                            ,freqVal=1:2
                        )
            ,"Sph"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"SfCol"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=3 ,lowE=3 ,rareE=1 ,dupESum=2 ) # c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(3,4,5,6  ,1,0,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(2,5  ,2,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, TRUE ,1    ,TRUE ,TRUE ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN01_Rrares"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "_phReb"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        ) 
            ,"_xyCnt.fCol"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"_xyCnt.phase"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"_ph_Reb"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"_fCol_Reb"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"_phReb_Reb"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"_xyCnt.fCol_Reb"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"_xyCnt.phase_Reb"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"_dblHpnFlg_sz"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"_ph_szDup"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"_fCol_szDup"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"_dblHpnFlg_szDup"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
        )
        ,evtMax = matrix( c(2,2,3,1 ,2,2,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 ) # c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(1,2,3,4,5,6  ,0,0,0,0,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(2,5  ,2,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}


mName <- "HCR_crScrN02_RszPhFCol"
if( TRUE ){     # lower Evt
    # Lower Evt
    #   Fhpn0:0     Shpn0:1
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "Fhpn0"=list( rng=matrix( c(0,0 ,0,6) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3),c(4,5,6)) ,ncol=2)
                        ) 
            ,"Fph_sz"=list( rng=matrix( c(0,0 ,0,5) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3),c(3,4,5,6)) ,ncol=2)
                        )
            ,"FfCol_sz"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3),c(3,4,5)) ,ncol=2)
                        )
            ,"Shpn0"=list( rng=matrix( c(0,0 ,1,6) ,ncol=2 )
                            ,evt=matrix( c(c(1,1,3,3),c(5,6,7,8)) ,ncol=2)
                        )
            ,"Sph_sz"=list( rng=matrix( c(0,0 ,0,5) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                        )
            ,"SfCol_sz"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(3,4,5,6)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,2,3,1 ,2,2,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=3 ,rareE=1 ,dupESum=2 ) # c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(0,1,2,3,6  ,0,0,0,2,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )  # H807
        # ,hIMtxValSum = matrix( c(2,5  ,2,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN02_EszPhFCol"
if( TRUE ){     # Lower Evt
    # Lower Evt
    #   Fhpn0:1     Shpn0:~3
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "Fhpn0"=list( rng=matrix( c(0,0 ,1,6) ,ncol=2 )
                            ,evt=matrix( c(c(1,1),c(4,5)) ,ncol=2)
                        ) 
            ,"Fph_sz"=list( rng=matrix( c(0,0 ,0,5) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(4)) ,ncol=2)
                        )
            ,"FfCol_sz"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(3)) ,ncol=2)
                        )
            ,"Shpn0"=list( rng=matrix( c(0,0 ,2,8) ,ncol=2 )
                            ,evt=matrix( c(c(1,1,3,3),c(6,7,8,9)) ,ncol=2)
                        )
            ,"Sph_sz"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"SfCol_sz"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(3,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,2,3,1 ,2,2,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=3 ,rareE=1 ,dupESum=2 ) # H838(4)
        ,hIMtxHpnCnt = matrix( c(0,1,2,3,6  ,0,0,0,2,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(2,5  ,2,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN02_RcommPhFCol"
if( TRUE ){     # Lower Evt
    # Lower Evt
    #   Fhpn0:~0     Shpn0:~1   totHpn:~1
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "Fhpn0"=list( rng=matrix( c(0,0 ,0,5) ,ncol=2 )
                            ,evt=matrix( c(c(1,3),c(4,5)) ,ncol=2)
                            ,freqVal=2:4
                        ) 
            ,"Fph"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3),c(2,3,4)) ,ncol=2)
                            ,freqVal=1:2
                        )
            ,"FfCol"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3),c(2,3,4)) ,ncol=2)
                            ,freqVal=1:2
                        )
            ,"Shpn0"=list( rng=matrix( c(0,0 ,1,6) ,ncol=2 )
                            ,evt=matrix( c(c(1,1),c(5,6)) ,ncol=2)
                            ,freqVal=3:6
                            ,forbidEvtReb=3
                        )
            ,"Sph"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"SfCol"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=4 ,rareE=2 ,dupESum=2 )     # H891(4) H898(3)
        ,hIMtxHpnCnt = matrix( c(0,1,5,6  ,0,0,1,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(2,5  ,2,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN02_EcommPhFCol"
if( TRUE ){     # Lower Evt
    # Lower Evt
    #   Fhpn0:~1     Shpn0:~2   totHpn:~1
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "Fhpn0"=list( rng=matrix( c(0,0 ,1,6) ,ncol=2 )
                            ,evt=matrix( c(c(1,2),c(5,6)) ,ncol=2)
                            ,freqVal=2:4
                        ) 
            ,"Fph"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(2,3),c(3,4)) ,ncol=2)
                        )
            ,"FfCol"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(2,3),c(2,3)) ,ncol=2)
                        )
            ,"Shpn0"=list( rng=matrix( c(0,0 ,2,8) ,ncol=2 )
                            ,evt=matrix( c(c(2,3),c(7,8)) ,ncol=2)
                            ,freqVal=4:6
                            ,forbidEvtReb=3
                        )
            ,"Sph"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3),c(1,2)) ,ncol=2)
                        )
            ,"SfCol"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2),c(1)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
        )
        ,evtMax = matrix( c(2,2,3,1 ,2,2,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=3 ,rareE=2 ,dupESum=2 ) # c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(0,1,5,6  ,0,0,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(2,5  ,2,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN02_Rrares"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "_phReb"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        ) 
            ,"_xyCnt.fCol"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )      # H834(2)
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"_xyCnt.phase"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"_ph_Reb"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"_fCol_Reb"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"_phReb_Reb"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"_xyCnt.fCol_Reb"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"_xyCnt.phase_Reb"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"_dblHpnFlg_sz"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"_ph_szDup"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"_fCol_szDup"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                            ,freqVal=1
                        )
            ,"_dblHpnFlg_szDup"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
        )
        ,evtMax = matrix( c(2,2,3,1 ,2,2,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=3 ) # 
        ,hIMtxHpnCnt = matrix( c(1,2  ,2,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(2,5  ,2,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}


mName <- "HCR_crScrN03_RszPhFCol"
if( TRUE ){     # Lower Evt
    # Lower Evt
    #   Fhpn0:~0     Shpn0:NA   totHpn:~1
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "Fhpn0"=list( rng=matrix( c(0,0 ,1,7) ,ncol=2 )
                            ,evt=matrix( c(c(3,3),c(6,7)) ,ncol=2)
                        ) 
            ,"Fph_sz"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3),c(3,4)) ,ncol=2)
                        )
            ,"FfCol_sz"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(3)) ,ncol=2)
                        )
            ,"Shpn0"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(4,5,6,7)) ,ncol=2)
                        )
            ,"Sph_sz"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"SfCol_sz"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3),c(3,4,5)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=2 ,rareE=1 ,dupESum=2 ) # c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(1,5,6  ,1,1,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(2,5  ,2,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN03_EszPhFCol"
if( TRUE ){     # Lower Evt
    # Lower Evt
    #   Fhpn0:~2     Shpn0:NA   totHpn:~1 (2 이상만 보임.)
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "Fhpn0"=list( rng=matrix( c(0,0 ,2,10) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,2,1),c(1,2,3,4)) ,ncol=2)  # 6~10이 대부분이라서.
                            ,freqVal=5:10
                        ) 
            ,"Fph_sz"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"FfCol_sz"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"Shpn0"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(2),c(1)) ,ncol=2)  # 0은 많은데 1이 안보인다?
                            ,freqVal=2:4
                        )
            ,"Sph_sz"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2),c(1)) ,ncol=2)
                        )
            ,"SfCol_sz"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2),c(1)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=2 ,rareE=1 ,dupESum=2 ) # c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(0,1,3,4,5,6  ,0,0,1,0,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(2,5  ,2,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, T ,1    ,TRUE ,T ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN03_RcommPhFCol"
if( TRUE ){     # Lower Evt
    # Lower Evt
    #   Fhpn0:~1     Shpn0:NA   totHpn:~0
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "Fhpn0"=list( rng=matrix( c(0,0 ,0,7) ,ncol=2 )
                            ,evt=matrix( c(c(3,2,3),c(0,6,7)) ,ncol=2)
                            ,freqVal=2:5
                        ) 
            ,"Fph"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3),c(3,4)) ,ncol=2)
                        )
            ,"FfCol"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,3),c(3,4)) ,ncol=2)
                        )
            ,"Shpn0"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,2),c(3,4)) ,ncol=2)      # H842(reb 4)
                            ,freqVal=1:3
                        )
            ,"Sph"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"SfCol"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )            # H853(2)
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=2 ,rareE=1 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(0,3,4,5,6  ,0,1,0,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(2,5  ,2,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, T ,1    ,TRUE ,T ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN03_EcommPhFCol"
if( TRUE ){     # Lower Evt
    # Lower Evt
    #   Fhpn0:~1     Shpn0:NA   totHpn:~ (Fhpn0,Shpn0 에서만 값이 발생하는 상태..)   
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "Fhpn0"=list( rng=matrix( c(0,0 ,2,10) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,1),c(0,1,2)) ,ncol=2)
                            ,freqVal=4:9
                        ) 
            ,"Fph"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"FfCol"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"Shpn0"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3),c(0,1)) ,ncol=2)  # 1이 없다..
                            ,freqVal=2:4
                        )
            ,"Sph"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SfCol"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 ) # c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(0,1,3,4,5,6  ,0,0,0,0,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(2,5  ,2,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, T ,1    ,TRUE ,T ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN03_Rrares"
if( FALSE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "_phReb"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"_xyCnt.fCol"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"_xyCnt.phase"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"_ph_Reb"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"_fCol_Reb"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"_phReb_Reb"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"_xyCnt.fCol_Reb"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"_xyCnt.phase_Reb"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"_dblHpnFlg_sz"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"_ph_szDup"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"_fCol_szDup"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"_dblHpnFlg_szDup"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
        )
        ,evtMax = matrix( c(2,2,3,1 ,2,2,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(0,1,2,3,4,5  ,0,1,0,0,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        ,hIMtxValSum = matrix( c(2  ,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}


mName <- "HCR_crScrN04_RszPhFCol"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "Fhpn0"=list( rng=matrix( c(0,0 ,2,12) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3),c(10,11,12,13)) ,ncol=2)
                            ,freqVal=5:8
                        ) 
            ,"Fph_sz"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(3,4,5,6)) ,ncol=2)
                        )
            ,"FfCol_sz"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                        )
            ,"Shpn0"=list( rng=matrix( c(0,0 ,2,12) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3),c(11,12,13)) ,ncol=2)
                            ,freqVal=5:10   ,forbidEvtReb=3
                        )
            ,"Sph_sz"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(2,3),c(4,5)) ,ncol=2)
                        )
            ,"SfCol_sz"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
        )
        ,evtMax = matrix( c(2,2,3,1 ,2,2,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=2 ,rareE=2 ,dupESum=2 ) # c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(2  ,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN04_EszPhFCol"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "Fhpn0"=list( rng=matrix( c(0,0 ,8,12) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,1),c(6,7,8,9,10)) ,ncol=2)
                            ,freqVal=12
                        ) 
            ,"Fph_sz"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"FfCol_sz"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"Shpn0"=list( rng=matrix( c(0,0 ,9,12) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,1),c(7,8,9,10)) ,ncol=2)
                            ,freqVal=12
                        )
            ,"Sph_sz"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SfCol_sz"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 )
        # ,hIMtxHpnCnt = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(2  ,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, T ,1    ,TRUE ,T ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN04_RcommPhFCol"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "Fhpn0"=list( rng=matrix( c(0,0 ,2,11) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(1,2,11)) ,ncol=2)
                        ) 
            ,"Fph"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"FfCol"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"Shpn0"=list( rng=matrix( c(0,0 ,2,12) ,ncol=2 )
                            ,evt=matrix( c(c(1,1,1,1),c(2,3,4,5)) ,ncol=2)
                            ,freqVal=10:11
                        )
            ,"Sph"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,1,3),c(1,2,3)) ,ncol=2)
                        )
            ,"SfCol"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,1,3),c(1,2,3)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=2 ,rareE=1 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(3,4,5,6  ,0,0,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )      # H865
        # ,hIMtxValSum = matrix( c(2  ,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE,T ,1    ,TRUE ,T ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN04_EcommPhFCol"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "Fhpn0"=list( rng=matrix( c(0,0 ,8,12) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,2,1),c(7,8,9,10)) ,ncol=2)
                            ,freqVal=12
                        ) 
            ,"Fph"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"FfCol"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"Shpn0"=list( rng=matrix( c(0,0 ,10,12) ,ncol=2 )
                            ,evt=matrix( c(c(3,2,1),c(9,10,11)) ,ncol=2)
                            ,freqVal=12
                        )
            ,"Sph"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SfCol"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=2 ,rareE=1 ,dupESum=2 ) # c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 )
        # ,hIMtxHpnCnt = matrix( c(0,1,2,3  ,0,0,2,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(2,5  ,2,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, T ,1    ,TRUE ,T ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN04_Rrares"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "_phReb"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"_xyCnt.fCol"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                            ,freqVal=1
                        )
            ,"_xyCnt.phase"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                            ,freqVal=1
                        )
            ,"_ph_Reb"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"_fCol_Reb"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"_phReb_Reb"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"_xyCnt.fCol_Reb"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"_xyCnt.phase_Reb"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"_dblHpnFlg_sz"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"_ph_szDup"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"_fCol_szDup"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"_dblHpnFlg_szDup"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 )
        # ,hIMtxHpnCnt = matrix( c(0,1,2,3  ,0,0,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        ,hIMtxValSum = matrix( c(1,2,3,4  ,1,1,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}


mName <- "HCR_crScrN05_RszPhFCol"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "Fhpn0"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                            ,freqVal=1
                        ) 
            ,"Fph_sz"=list( rng=matrix( c(0,0 ,0,5) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(0,5,6,7)) ,ncol=2)
                            ,freqVal=2:4
                        )
            ,"FfCol_sz"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,freqVal=1
                        )
            ,"Shpn0"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3),c(3,4,5)) ,ncol=2)
                            ,freqVal=1:2
                        )
            ,"Sph_sz"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,1,3,3),c(2,3,4,5)) ,ncol=2)
                            ,freqVal=1:2
                        )
            ,"SfCol_sz"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,1,3,3),c(2,3,4,5)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,2,3,1 ,2,2,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=3 ,rareE=1 ,dupESum=2 ) # c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(6  ,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN05_EszPhFCol"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "Fhpn0"=list( rng=matrix( c(0,0 ,0,6) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,1),c(0,1,6)) ,ncol=2)
                            ,forbidEvtReb=3     ,freqVal=5:6
                        ) 
            ,"Fph_sz"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                            ,freqVal=1
                        )
            ,"FfCol_sz"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"Shpn0"=list( rng=matrix( c(0,0 ,2,6) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,2),c(1,2,3)) ,ncol=2)
                            ,freqVal=4:6
                        )
            ,"Sph_sz"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SfCol_sz"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,2,3,1 ,2,2,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(4,5,6  ,1,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN05_RcommPhFCol"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "Fhpn0"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                            ,freqVal=1
                        ) 
            ,"Fph"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3),c(3,4,5)) ,ncol=2)
                            ,freqVal=1:2
                        )
            ,"FfCol"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"Shpn0"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3),c(3,4,5)) ,ncol=2)
                            ,freqVal=1:2
                            ,forbidEvtReb=3
                        )
            ,"Sph"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"SfCol"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,2,3,1 ,2,2,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=2 ,rareE=2 ,dupESum=2 ) # c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(0,1,4,5,6  ,0,1,1,1,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN05_EcommPhFCol"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "Fhpn0"=list( rng=matrix( c(0,0 ,0,6) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,2),c(0,1,2)) ,ncol=2)
                            ,freqVal=4:6
                        ) 
            ,"Fph"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"FfCol"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"Shpn0"=list( rng=matrix( c(0,0 ,2,6) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,1),c(1,2,3)) ,ncol=2)
                            ,freqVal=4:6
                        )
            ,"Sph"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SfCol"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=2 ,rareE=1 ,dupESum=2 ) # c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(0,1,3,4,5,6  ,0,0,0,0,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, T ,1    ,TRUE ,T ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN05_Rrares"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "_phReb"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"_xyCnt.fCol"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,freqVal=1
                        )
            ,"_xyCnt.phase"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"_ph_Reb"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"_fCol_Reb"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"_phReb_Reb"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"_xyCnt.fCol_Reb"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"_xyCnt.phase_Reb"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"_dblHpnFlg_sz"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"_ph_szDup"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,freqVal=1
                            ,forbidEvtReb=3
                        )
            ,"_fCol_szDup"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"_dblHpnFlg_szDup"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,3,3,1 ,2,3,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(2,3,4,5,6  ,2,0,0,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, T ,1    ,TRUE ,T ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}

mName <- "HCR_crScrN2345_Rrares"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "_phReb"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        ) 
            ,"_xyCnt.fCol"=list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"_xyCnt.phase"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"_ph_Reb"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"_fCol_Reb"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )    # H844(2)
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"_phReb_Reb"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"_xyCnt.fCol_Reb"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"_xyCnt.phase_Reb"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"_dblHpnFlg_sz"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"_ph_szDup"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"_fCol_szDup"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"_dblHpnFlg_szDup"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,3,3,1 ,2,3,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(3,4,5,6  ,1,0,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(2,5  ,2,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}


#- HCR.MtxTmpl_crScrNnPhEvt -----------------------------------------------------------------------------------------------
mName <- "HCR_crScrN01_PhEvtmax"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "Fe3Max"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3),c(2,3,4)) ,ncol=2)
                            ,freqVal=1
                        ) 
            ,"Fe2Max"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3),c(2,3,4)) ,ncol=2)
                            ,freqVal=1
                        )
            ,"Fe1Max"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)
                            ,freqVal=1:2
                        )
            ,"FrebRawMax"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"Se3Max"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        )
            ,"Se2Max"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        )
            ,"Se1Max"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SrebRawMax"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,2,3,1 ,2,2,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=3 ,rareE=1 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(0,3,4,5,6,7  ,1,2,1,1,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN01_PhEvtcnt"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "Fe3MCnt"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"Fe2MCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"Fe1MCnt"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"FrebRawMCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"Se3MCnt"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"Se2MCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"Se1MCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SrebRawMCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(1,3,4,5,6  ,0,0,0,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN01_PhEvtrebEvtMax"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "FrebEvtMax"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3 ,freqVal=1
                        ) 
            ,"SrebEvtMax"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,2,3,1 ,2,2,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=3 )
        ,hIMtxHpnCnt = matrix( c(1,3,4,5,6  ,2,0,0,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, T ,1    ,TRUE ,T ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN01_PhEvtrebEvtMCnt"   # 발생 자체가 없는 거 같다.
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "FrebEvtMCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"SrebEvtMCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(1,3,4,5,6  ,0,0,0,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}

mName <- "HCR_crScrN02_PhEvtmax"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "Fe3Max"=list( rng=matrix( c(0,0 ,0,7) ,ncol=2 )
                            ,evt=matrix( c(c(1,1,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,freqVal=1:2
                        ) 
            ,"Fe2Max"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3),c(2,3,4)) ,ncol=2)
                            ,freqVal=1
                        )
            ,"Fe1Max"=list( rng=matrix( c(0,0 ,0,5) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3),c(3,4,5)) ,ncol=2)
                            ,freqVal=2:3
                        )
            ,"FrebRawMax"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                            ,freqVal=1
                        )
            ,"Se3Max"=list( rng=matrix( c(0,0 ,0,5) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"Se2Max"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"Se1Max"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SrebRawMax"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3),c(1,2,3)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,2,3,2 ,2,2,3,2)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=3 ,rareE=2 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(1,2,6  ,0,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN02_PhEvtcnt"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "Fe3MCnt"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        ) 
            ,"Fe2MCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"Fe1MCnt"=list( rng=matrix( c(0,0 ,0,5) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,2,3,3,3),c(1,2,3,4,5,6)) ,ncol=2)
                            ,freqVal=2:3    ,forbidEvtReb=3
                        )
            ,"FrebRawMCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"Se3MCnt"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"Se2MCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"Se1MCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SrebRawMCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,2,3,1 ,2,2,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=2 ,rareE=2 ,dupESum=3 )
        ,hIMtxHpnCnt = matrix( c(2,3,4,5,6  ,1,0,0,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, T ,1    ,TRUE ,T ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN02_PhEvtrebEvtMax"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "FrebEvtMax"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                            ,freqVal=1
                        ) 
            ,"SrebEvtMax"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=3 ,lowE=2 ,rareE=2 ,dupESum=2 ) # c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(2  ,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = NULL
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN02_PhEvtrebEvtMCnt"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "FrebEvtMCnt"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"SrebEvtMCnt"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(1,3,4,5,6  ,0,0,0,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}

mName <- "HCR_crScrN03_PhEvtmax"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "Fe3Max"=list( rng=matrix( c(0,0 ,0,7) ,ncol=2 )
                            ,evt=matrix( c(c(2,2,2),c(5,6,7)) ,ncol=2)
                            ,freqVal=1:2
                        ) 
            ,"Fe2Max"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                            ,freqVal=1
                        )
            ,"Fe1Max"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"FrebRawMax"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"Se3Max"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(3,4,5,6)) ,ncol=2)
                            ,freqVal=1:2
                        )
            ,"Se2Max"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"Se1Max"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SrebRawMax"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,2,3,1 ,2,2,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=2 ,rareE=1 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(4,5,6  ,0,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,2    ,TRUE ,F ,2 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN03_PhEvtcnt"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "Fe3MCnt"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(2,2,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,forbidEvtReb=3
                        ) 
            ,"Fe2MCnt"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,2,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"Fe1MCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"FrebRawMCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"Se3MCnt"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"Se2MCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"Se1MCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SrebRawMCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,2,3,1 ,2,2,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(2,3,4  ,0,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN03_PhEvtrebEvtMax"   # 데이터 자체가 없다.
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "FrebEvtMax"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"SrebEvtMax"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 ) # c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(1,2  ,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN03_PhEvtrebEvtMCnt"   # 데이터 자체가 없다.
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "FrebEvtMCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"SrebEvtMCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(1,2  ,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}

mName <- "HCR_crScrN04_PhEvtmax"   # 데이터 자체가 없다.
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "Fe3Max"=list( rng=matrix( c(0,0 ,0,6) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                        ) 
            ,"Fe2Max"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"Fe1Max"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"FrebRawMax"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"Se3Max"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3),c(2,3,4)) ,ncol=2)
                        )
            ,"Se2Max"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"Se1Max"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SrebRawMax"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=2 ,lowE=2 ,rareE=1 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(2,3,4,5,6  ,0,0,0,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN04_PhEvtcnt"   # 데이터 자체가 거의 없다.
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "Fe3MCnt"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"Fe2MCnt"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"Fe1MCnt"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"FrebRawMCnt"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"Se3MCnt"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"Se2MCnt"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"Se1MCnt"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SrebRawMCnt"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(1  ,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN04_PhEvtrebEvtMax"   # 데이터 자체가 거의 없다.
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "FrebEvtMax"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"SrebEvtMax"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 ) # c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(1,2  ,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN04_PhEvtrebEvtMCnt"   # 데이터 자체가 거의 없다.
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "FrebEvtMCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"SrebEvtMCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(1,2  ,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}

mName <- "HCR_crScrN05_PhEvtmax"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "Fe3Max"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(2,3),c(3,4)) ,ncol=2)
                            ,freqVal=1:2
                        ) 
            ,"Fe2Max"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"Fe1Max"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"FrebRawMax"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )           # H826(evtReb)
                            ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                            ,freqVal=1
                        )
            ,"Se3Max"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                            ,freqVal=1
                        )
            ,"Se2Max"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"Se1Max"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SrebRawMax"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                            ,freqVal=1
                        )
        )
        ,evtMax = matrix( c(2,2,3,1 ,2,2,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=2 ,rareE=1 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(0,3,4,5,6  ,1,3,1,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,2    ,TRUE ,F ,2 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN05_PhEvtcnt"   # 데이터 자체가 거의 없다.
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "Fe3MCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"Fe2MCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"Fe1MCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"FrebRawMCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"Se3MCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"Se2MCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"Se1MCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SrebRawMCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 )
        # ,hIMtxHpnCnt = matrix( c(0,1,3,4,5,6  ,0,0,0,0,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        # ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN05_PhEvtrebEvtMax"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "FrebEvtMax"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,freqVal=1  ,forbidEvtReb=3
                        ) 
            ,"SrebEvtMax"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=4 ) # c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 )
        # ,hIMtxHpnCnt = matrix( c(0,1,3,4,5,6  ,0,0,0,0,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        # ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN05_PhEvtrebEvtMCnt"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "FrebEvtMCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"SrebEvtMCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 )
        # ,hIMtxHpnCnt = matrix( c(0,1,3,4,5,6  ,0,0,0,0,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        # ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}

mName <- "HCR_crScrN2345_PhEvtrebEvtMax"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "F2rebEvtMax"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                            ,freqVal=1
                        ) 
            ,"S2rebEvtMax"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                            ,freqVal=1
                        )
            ,"F3rebEvtMax"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"S3rebEvtMax"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"F4rebEvtMax"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"S4rebEvtMax"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"F5rebEvtMax"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"S5rebEvtMax"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=2 ,rareE=1 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(2,3,4  ,2,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, T ,1    ,TRUE ,T ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}

mName <- "HCR_crScrN2345_PhEvtrebEvtMCnt"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "F2rebEvtMCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"S2rebEvtMCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"F3rebEvtMCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"S3rebEvtMCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"F4rebEvtMCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"S4rebEvtMCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"F5rebEvtMCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"S5rebEvtMCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(1,2  ,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}


#- HCR.MtxTmpl_crScrNnSum -----------------------------------------------------------------------------------------------
mName <- "HCR_crScrN01_sumsumRaw"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "FsummSumRaw"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,2,3),c(1,2,3,4)) ,ncol=2)
                            ,freqVal=1:3
                            ,forbidEvtReb=3
                        ) 
            ,"FsummSumOthRaw"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"FsummSumRebRaw"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"FsummSumRebOthRaw"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"SsummSumRaw"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3 ,freqVal=1
                        )
            ,"SsummSumOthRaw"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"SsummSumRebRaw"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"SsummSumRebOthRaw"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
        )
        ,evtMax = matrix( c(2,2,3,1 ,2,2,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=3 ,rareE=3 ,dupESum=3 )
        ,hIMtxHpnCnt = matrix( c(2,3,4,5,6  ,1,0,0,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN01_sumsumEvt"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "FsummSumEvt"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,1,1,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"FsummSumOthEvt"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"FsummSumRebEvt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"FsummSumRebOthEvt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SsummSumEvt"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SsummSumOthEvt"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SsummSumRebEvt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SsummSumRebOthEvt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,2,3,1 ,2,2,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=2 ,lowE=2 ,rareE=1 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(2,3,4,5,6  ,1,0,0,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,2    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN01_sumszSumRebCnt"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "FszSumRebCnt"=list( rng=matrix( c(0,0 ,0,12) ,ncol=2 )
                            ,evt=matrix( c(c(2),c(0)) ,ncol=2)
                            ,freqVal=2:8    ,forbidEvtReb=3
                        ) 
            ,"SszSumRebCnt"=list( rng=matrix( c(0,0 ,0,6) ,ncol=2 )
                            ,evt=matrix( c(c(2,3),c(5,6)) ,ncol=2)
                            ,freqVal=1:2
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=2 ,rareE=2 ,dupESum=3 )
        ,hIMtxHpnCnt = matrix( c(0  ,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN01_sumszSumRebDup"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "FszSumRebDup"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3     ,freqVal=2
                        ) 
            ,"SszSumRebDup"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(1,2  ,1,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}

mName <- "HCR_crScrN02_sumsumRaw"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "FsummSumRaw"=list( rng=matrix( c(0,0 ,0,7) ,ncol=2 )
                            ,evt=matrix( c(c(1,1,1,1,3),c(3,4,5,6,7)) ,ncol=2)
                            ,freqVal=1
                        ) 
            ,"FsummSumOthRaw"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,freqVal=1
                        )
            ,"FsummSumRebRaw"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"FsummSumRebOthRaw"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SsummSumRaw"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(3,4,5,6)) ,ncol=2)
                            # ,freqVal=1
                        )
            ,"SsummSumOthRaw"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SsummSumRebRaw"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SsummSumRebOthRaw"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,2,3,1 ,2,2,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=2 ,lowE=2 ,rareE=1 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(3,4,5,6  ,0,0,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, T ,1    ,TRUE ,T ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN02_sumsumEvt"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "FsummSumEvt"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3),c(3,4,5)) ,ncol=2)
                            ,freqVal=1:3
                        ) 
            ,"FsummSumOthEvt"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"FsummSumRebEvt"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"FsummSumRebOthEvt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SsummSumEvt"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,1,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SsummSumOthEvt"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SsummSumRebEvt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SsummSumRebOthEvt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=2 ,rareE=1 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(3,4,5,6  ,1,1,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,2    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN02_sumszSumRebCnt"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "FszSumRebCnt"=list( rng=matrix( c(0,0 ,0,22) ,ncol=2 )
                            ,evt=matrix( c(c(3,1,1),c(0,1,16)) ,ncol=2)
                            ,freqVal=2:6
                        ) 
            ,"SszSumRebCnt"=list( rng=matrix( c(0,0 ,0,8) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(8)) ,ncol=2)
                            ,freqVal=1:4
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(0,1  ,0,2)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN02_sumszSumRebDup"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "FszSumRebDup"=list( rng=matrix( c(0,0 ,0,6) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)
                            ,freqVal=1:2  ,forbidEvtReb=3
                        ) 
            ,"SszSumRebDup"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,2,3,1 ,2,2,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=2 ,rareE=2 ,dupESum=2 )
        # ,hIMtxHpnCnt = matrix( c(0,1,3,4,5,6  ,0,0,0,0,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}

mName <- "HCR_crScrN03_sumsumRaw"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "FsummSumRaw"=list( rng=matrix( c(0,0 ,0,5) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(3,4,5,6)) ,ncol=2)
                            ,freqVal=1
                        ) 
            ,"FsummSumOthRaw"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"FsummSumRebRaw"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"FsummSumRebOthRaw"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SsummSumRaw"=list( rng=matrix( c(0,0 ,0,5) ,ncol=2 )
                            ,evt=matrix( c(c(2,2,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"SsummSumOthRaw"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,2,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SsummSumRebRaw"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SsummSumRebOthRaw"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(2,3,4  ,0,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN03_sumsumEvt"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "FsummSumEvt"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        ) 
            ,"FsummSumOthEvt"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"FsummSumRebEvt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"FsummSumRebOthEvt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"SsummSumEvt"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"SsummSumOthEvt"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"SsummSumRebEvt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
            ,"SsummSumRebOthEvt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,forbidEvtReb=3
                        )
        )
        ,evtMax = matrix( c(2,2,3,1 ,2,2,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(1,2,3  ,0,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN03_sumszSumRebCnt"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "FszSumRebCnt"=list( rng=matrix( c(0,0 ,0,8) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(4,5,6,7,8)) ,ncol=2)
                            ,freqVal=1:4    ,forbidEvtReb=3
                        ) 
            ,"SszSumRebCnt"=list( rng=matrix( c(0,0 ,0,8) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(4,5,6,7,8)) ,ncol=2)
                            ,freqVal=1:2    ,forbidEvtReb=3
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(0,2  ,1,2)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, T ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN03_sumszSumRebDup"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "FszSumRebDup"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"SszSumRebDup"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(1  ,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}

mName <- "HCR_crScrN04_sumsumRaw"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "FsummSumRaw"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(3,4,5,6)) ,ncol=2)
                        ) 
            ,"FsummSumOthRaw"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3),c(2,3,4)) ,ncol=2)
                            ,freqVal=1
                        )
            ,"FsummSumRebRaw"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"FsummSumRebOthRaw"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SsummSumRaw"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(2,2,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SsummSumOthRaw"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"SsummSumRebRaw"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SsummSumRebOthRaw"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=2 ,rareE=1 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(2,3,4  ,0,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )  # H865(2)
        # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN04_sumsumEvt"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "FsummSumEvt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"FsummSumOthEvt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"FsummSumRebEvt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"FsummSumRebOthEvt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SsummSumEvt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SsummSumOthEvt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SsummSumRebEvt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SsummSumRebOthEvt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 )
        # ,hIMtxHpnCnt = matrix( c(0,1,3,4,5,6  ,0,0,0,0,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        # ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN04_sumszSumRebCnt"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "FszSumRebCnt"=list( rng=matrix( c(0,0 ,0,6) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3),c(4,5,6)) ,ncol=2)
                            ,freqVal=1:4    ,forbidEvtReb=3
                        ) 
            ,"SszSumRebCnt"=list( rng=matrix( c(0,0 ,0,8) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(4,5,6,7,8)) ,ncol=2)
                            ,freqVal=1:4    ,forbidEvtReb=3
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(2  ,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )  # H898(2)
        # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN04_sumszSumRebDup"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "FszSumRebDup"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"SszSumRebDup"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 )
        # ,hIMtxHpnCnt = matrix( c(0,1,3,4,5,6  ,0,0,0,0,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        # ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}

mName <- "HCR_crScrN05_sumsumRaw"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "FsummSumRaw"=list( rng=matrix( c(0,0 ,0,8) ,ncol=2 )
                            ,evt=matrix( c(c(2,3),c(7,8)) ,ncol=2)
                            ,freqVal=1:2
                        ) 
            ,"FsummSumOthRaw"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"FsummSumRebRaw"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"FsummSumRebOthRaw"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SsummSumRaw"=list( rng=matrix( c(0,0 ,0,8) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(4,5,6,7)) ,ncol=2)
                            ,freqVal=1
                        )
            ,"SsummSumOthRaw"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SsummSumRebRaw"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SsummSumRebOthRaw"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,2,3,1 ,2,2,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=2 ,rareE=1 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(3,4,5,6  ,0,0,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN05_sumsumEvt"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "FsummSumEvt"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                            ,freqVal=1
                        ) 
            ,"FsummSumOthEvt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"FsummSumRebEvt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"FsummSumRebOthEvt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SsummSumEvt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SsummSumOthEvt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SsummSumRebEvt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SsummSumRebOthEvt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 )
        ,hIMtxHpnCnt = matrix( c(1,2,3  ,1,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN05_sumszSumRebCnt"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "FszSumRebCnt"=list( rng=matrix( c(0,0 ,0,13) ,ncol=2 )
                            ,evt=matrix( c(c(1,1,1,3,3,3),c(8,9,10,11,12,13)) ,ncol=2)
                            ,freqVal=2:6
                        ) 
            ,"SszSumRebCnt"=list( rng=matrix( c(0,0 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(3,3),c(8,9)) ,ncol=2)
                            ,freqVal=1:3    
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=2 ,rareE=2 ,dupESum=2 )
        # ,hIMtxHpnCnt = matrix( c(1  ,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN05_sumszSumRebDup"
if( TRUE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "FszSumRebDup"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                            ,freqVal=1
                        ) 
            ,"SszSumRebDup"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)  # H884(1)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=2 ,rareE=1 ,dupESum=2 )
        # ,hIMtxHpnCnt = matrix( c(1,2  ,2,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )  # H827(2)
        # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        ,rowRebDupBan = matrix( c( TRUE, T ,1    ,TRUE ,T ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}

mName <- "HCR_crScrN2345_sumszSumRebCnt"   # QQE:Todo --------------------------------------------------------
if( FALSE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "F2szSumRebCnt"=list( rng=matrix( c(0,0 ,10,11) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"S2szSumRebCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"F3szSumRebCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"S3szSumRebCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"F4szSumRebCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"S4szSumRebCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"F5szSumRebCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"S5szSumRebCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 )
        # ,hIMtxHpnCnt = matrix( c(0,1,3,4,5,6  ,0,0,0,0,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        # ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}

mName <- "HCR_crScrN2345_sumszSumRebDup"   # QQE:Todo --------------------------------------------------------
if( FALSE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "F2szSumRebDup"=list( rng=matrix( c(0,0 ,10,11) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"S2szSumRebDup"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"F3szSumRebDup"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"S3szSumRebDup"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"F4szSumRebDup"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"S4szSumRebDup"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"F5szSumRebDup"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"S5szSumRebDup"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 )
        # ,hIMtxHpnCnt = matrix( c(0,1,3,4,5,6  ,0,0,0,0,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        # ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}

#- HCR.MtxTmpl_crScrNnSumClM -----------------------------------------------------------------------------------------------
mName <- "HCR_crScrN01_sumClMsumTotX"   # QQE:Todo --------------------------------------------------------
if( FALSE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "FsumTot1"=list( rng=matrix( c(0,0 ,10,11) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"FsumTot2"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"FsumTot3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SsumTot1"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SsumTot2"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SsumTot3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 )
        # ,hIMtxHpnCnt = matrix( c(0,1,3,4,5,6  ,0,0,0,0,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        # ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN02_sumClMsumTotX"   # QQE:Todo --------------------------------------------------------
if( FALSE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "FsumTot1"=list( rng=matrix( c(0,0 ,10,11) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"FsumTot2"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"FsumTot3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SsumTot1"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SsumTot2"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SsumTot3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 )
        # ,hIMtxHpnCnt = matrix( c(0,1,3,4,5,6  ,0,0,0,0,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        # ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN03_sumClMsumTotX"   # QQE:Todo --------------------------------------------------------
if( FALSE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "FsumTot1"=list( rng=matrix( c(0,0 ,10,11) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"FsumTot2"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"FsumTot3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SsumTot1"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SsumTot2"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SsumTot3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 )
        # ,hIMtxHpnCnt = matrix( c(0,1,3,4,5,6  ,0,0,0,0,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        # ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN04_sumClMsumTotX"   # QQE:Todo --------------------------------------------------------
if( FALSE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "FsumTot1"=list( rng=matrix( c(0,0 ,10,11) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"FsumTot2"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"FsumTot3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SsumTot1"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SsumTot2"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SsumTot3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 )
        # ,hIMtxHpnCnt = matrix( c(0,1,3,4,5,6  ,0,0,0,0,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        # ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}
mName <- "HCR_crScrN05_sumClMsumTotX"   # QQE:Todo --------------------------------------------------------
if( FALSE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "FsumTot1"=list( rng=matrix( c(0,0 ,10,11) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"FsumTot2"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"FsumTot3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SsumTot1"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SsumTot2"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"SsumTot3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 )
        # ,hIMtxHpnCnt = matrix( c(0,1,3,4,5,6  ,0,0,0,0,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        # ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}

mName <- "HCR_crScrN2345_sumClMsumTotHpn"   # QQE:Todo --------------------------------------------------------
if( FALSE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "F2sumTotHpn"=list( rng=matrix( c(0,0 ,10,11) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"S2sumTotHpn"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"F3sumTotHpn"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"S3sumTotHpn"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"F4sumTotHpn"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"S4sumTotHpn"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"F5sumTotHpn"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"S5sumTotHpn"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 )
        # ,hIMtxHpnCnt = matrix( c(0,1,3,4,5,6  ,0,0,0,0,0,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
        # # ,hIMtxValSum = matrix( c(5,6  ,1,1)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
        # ,rowRebDupBan = matrix( c( TRUE, F ,1    ,TRUE ,F ,1 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
        ,isHard=NULL  # use default
    )
}



# ........

mName <- "xxxx"   # QQE:Todo --------------------------------------------------------
if( FALSE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "xxx"=list( rng=matrix( c(0,0 ,10,11) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"xxx"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 ) # c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 )
        ,isHard=NULL  # use default
    )
}



mName <- "sssss" # template --------------------------------------------------------
if( FALSE ){
    HCRMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "xxx"=list( rng=matrix( c(0,0 ,10,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"xxx"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )  # H801
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)     ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 ) # c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 )
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

    # 추가 옵션 코드 예시 (디폴트값은 NULL)
    # ,hIMtxHpnCnt = matrix( c(0,1,2,3  ,0,0,2,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("hpnCnt","thld"),NULL) )
    # ,hIMtxValSum = matrix( c(2,5  ,2,0)   ,byrow=T  ,nrow=2  ,dimnames=list(c("valSum","thld"),NULL) )
    # ,rowRebDupBan = matrix( c( TRUE, F ,2    ,TRUE ,F ,2 )   ,byrow=T ,nrow=2 ,dimnames=list(c("AA_A","AAB_B"),c("OnOff","freqVal","rawMin")) )
    #               BanAA_Afv,BanAAB_Bfv 는 freqVal 옵션을 적용할 것인지의 여부이다.
    #               BanAA_A,BanAAB_B 상태에서 중복이 발생했어도 fv가 TRUE이면 freqVal 옵션 체크가 적용된다.

}
