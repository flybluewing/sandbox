
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

	rObj <- list( 	mInfo=c("mName"=mName ,"szRowName"=szRowName ,"szColName"=szColName  ) ,wMLst=wMLst
				)

    rObj$cName <- c( wMLst$bf ,wMLst$bS )

    rObj$fMtxObj <- function( crScr ){
        # crScr : cutRst List. bFMtx/bSMtx 모두 포함된 리스트
        #   std.grp ,bS.grp

        datLen <- length(crScr$std.grp) ;datNam <- names(crScr$std.grp) # datNam은 NULL일 수도 있다.

        scrMtx <- matrix( 0 ,nrow=datLen ,ncol=length(rObj$cName) ,dimnames=list(datNam,rObj$cName) )
        for( rIdx in seq_len(datLen) ){
            std.grp <- crScr$std.grp[[rIdx]]$sfcLate$basic
            bS.grp <- crScr$bS.grp[[rIdx]]$sfcLate$basic

            for( wmName in rObj$wMLst$bf ){
                scMtx.sz <- std.grp[[wmName]]$summ$scMtx.sz
                scrMtx[rIdx,wmName] <- scMtx.sz[ rObj$mInfo["szRowName"] ,rObj$mInfo["szColName"] ]
            }
            for( wmName in rObj$wMLst$bS ){
                scMtx.sz <- bS.grp[[wmName]]$summ$scMtx.sz
                scrMtx[rIdx,wmName] <- scMtx.sz[ rObj$mInfo["szRowName"] ,rObj$mInfo["szColName"] ]
            }
        }

        return( scrMtx )
    }

    return( rObj )
}



