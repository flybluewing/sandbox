
# ===========================================================================================
# -- bFMtx ----------------------------------------------------------------------------------
if( FALSE ){    # debug Code

    allIdxFBak <- allIdxF
    allIdxF <- allIdxF[ sort(sample(length(allIdxF),1000)) ]

    stdMI.grp <- bUtil.getStdMILst( gEnv ,fRstLst )     ;stdMI.grp$anyWarn( )
    aZoidMtx <- gEnv$allZoidMtx[allIdxF ,,drop=F]
    cut.grp <- bFCust.getFCustGrp( hMtxLst ,tgt.scMtx )
    filter.grp <- getFilter.grp( stdMI.grp ,tgt.scMtx=tgt.scMtx )

    scoreMtx.grp <- getScoreMtx.grp( aZoidMtx ,filter.grp ,tgt.scMtx=tgt.scMtx )


}

cutIH.bFMtx <- function( mName ){
    # cutIH.bFMtx_MC()  검토/적용부터 끝내고 나서 하자.
}

cutIH.bFMtx_Ext <- function( mName ,hName="sfcLate" ){
    # cutIH.bFMtx_MC()  검토/적용부터 끝내고 나서 하자.
}

cutIH.bFMtx_MR <- function( mName ,hName="sfcLate" ){
    # cutIH.bFMtx_MC()  검토/적용부터 끝내고 나서 하자.
}

cutIH.bFMtx_MC <- function( crMName ,aZoidMtx ,phVP.grp ,cut.grp ,scoreMtx.grp ,fHName ){
    #   crMName.grp <- names(bCMtxLst)

    ispMtx <- NULL

    mtxMaker <- bCMtxLst[[crMName]]()
    crScrMtx <- mtxMaker$fMtxObj( scoreMtx.grp ,cut.grp ,fHName )

    ispMtx <- scrScrMtx
}



# ===========================================================================================
# -- bSMtx ----------------------------------------------------------------------------------
if( FALSE ){    # debug Code

    #   cutRst.bS <- cutH.bS.Cut( gEnv ,allIdxF ,hMtxLst_bS ,fHName ,tgt.scMtx=tgt.scMtx )
    allIdxFBak <- allIdxF
    allIdxF <- allIdxF[ sort(sample(length(allIdxF),1000)) ]

    aZoidMtx <- gEnv$allZoidMtx[allIdxF ,,drop=F]
    phVP.grp <- bS.getPhVPGrp( gEnv ,aZoidMtx )
    cut.grp <- bS.getCutGrp( hMtxLst_bS ,tgt.scMtx=tgt.scMtx )  # curHMtxLst 적용 추가 필요.
    scoreMtx.grp <- bS.getScoreMtx.grp( phVP.grp ,aZoidMtx ,tgt.scMtx=tgt.scMtx )
}

cutIH.bSMtx <- function( mName ){
    # cutIH.bFMtx_MC()  검토/적용부터 끝내고 나서 하자.
}

cutIH.bSMtx_MC <- function( crMName ,aZoidMtx ,phVP.grp ,cut.grp ,scoreMtx.grp ,fHName ){
    #   crMName.grp <- names(bSMtxCMLst)

    ispMtx <- NULL

    mtxMaker <- bSMtxCMLst[[crMName]]()
    crScrMtx <- mtxMaker$fMtxObj( scoreMtx.grp ,cut.grp ,fHName )

    ispMtx <- scrScrMtx
}

