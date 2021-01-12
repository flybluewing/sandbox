#
HCR.getMName <- function( workMName ,crObjH ){
    # bFMtx에 해당하는 mName과 bSMtx에 해당하는 mName 분류

}



HCR.MtxTmpl_szPhReb <- function( idStr ,hName ,mName ,workMName ,crObjH ){

	rObj <- list( 	idStr=idStr ,mNameLst=HCR.getMName(workMName ,crObjH)
                    ,mInfo=c("hName"=hName,"mName"=mName)
				)

    # mNameLst에 따라 crObjH 에서 필요한 부분만 추출, HMtx 형태를 만듬.


    rObj$fMtxObj <- function( crMtx ){

    }

    return( rObj )
}



