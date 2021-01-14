#
HCR.getMName <- function( workMName ,crScrH ){
    # bFMtx에 해당하는 mName과 bSMtx에 해당하는 mName 분류

}



HCR.MtxTmpl_szReb <- function( mName ,wMLst ,szColName ){
    #   HCR.getMName(tgt.scMtx ,crScrH)     # bFMtx, bSMtx에 따라 분리추출된 mName

	rObj <- list( 	idStr=idStr ,wMLst=wMLst
                    ,mInfo=c("mName"=mName ,"szColName"=szColName )
				)

    rObj$fMtxObj <- function( crLst ){
        # crLst : cutRst List. bFMtx/bSMtx 모두 포함된 리스트

    }

    return( rObj )
}



