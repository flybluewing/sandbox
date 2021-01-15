
HCR.getMName <- function( workMName ,crScrH ,warn=T ){
    # bFMtx에 해당하는 mName과 bSMtx에 해당하는 mName 분류

    bfMNames <- intersect( workMName ,names(crScrH$std.grp[[1]]$sfcLate$basic) )
    bSMNames <- intersect( workMName ,names(crScrH$bS.grp[[1]]$sfcLate$basic) )

    if( warn ){
        haveFlag <- (workMName %in% bfMNames) | (workMName %in% bSMNames)
        if( any(!haveFlag) ){
            missingM <- workMName[!haveFlag]
            cat(sprintf("    Warning!! missing in crScrH : %s \n",paste(missingM,collapse=",")))
        }
    }

    return( list(bf=bfMNames ,bS=bSMNames) )
}


HCR.MtxTmpl_szReb <- function( mName ,wMLst ,szColName ,szRowName ){
    #   HCR.getMName(tgt.scMtx ,crScrH)     # bFMtx, bSMtx에 따라 분리추출된 mName
    #   swColName : r.ph r.fCol r.dblHpnFlg e.ph e.fCol e.dblHpnFlg
    #   swRowName : rebCnt / rebDup

	rObj <- list( 	mInfo=c("mName"=mName ,"szColName"=szColName ) ,wMLst=wMLst
				)

    rObj$cName <- c( wMLst$bf ,wMLst$bS )

    rObj$fMtxObj <- function( crLst ){
        # crLst : cutRst List. bFMtx/bSMtx 모두 포함된 리스트



    }

    return( rObj )
}



