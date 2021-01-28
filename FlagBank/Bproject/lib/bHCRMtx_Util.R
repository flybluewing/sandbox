
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

HCR.getHCRNames <- function( tgt.scMtx=NULL ){

    tgt.HCRMtx <- character(0)
    if( !is.null(tgt.scMtx) ){
        for( mName in names(bHCRMtxLst) ){
            flag_bf <- all( bHCRMtxLst[[mName]]$wMLst$bf %in% tgt.scMtx )
            flag_bS <- all( bHCRMtxLst[[mName]]$wMLst$bS %in% tgt.scMtx )

            if( flag_bf && flag_bS ){
                tgt.HCRMtx <- c( tgt.HCRMtx ,mName )
            }
        }
    } else {
        tgt.HCRMtx <- names(bHCRMtxLst)
    }

    return( tgt.HCRMtx )
}

HCR.getFilter.grp <- function( tgt.scMtx=NULL ){   # tgt.scMtx
    #   bFMtx.R getFilter.grp() 참고.

    tgt.HCRMtx <- HCR.getHCRNames( tgt.scMtx )

    return( bHCRMtxLst[tgt.HCRMtx] )

}

HCR.makeHCRMtxLst <- function( crScrH ,allIdxLst ,fRstLst ,lastH=NULL ,tgt.scMtx=NULL ){
    # 파라미터
    #   crScrH <- crScrHTool$getData()
    # 참고
    #   B.makeHMtxLst( gEnv, allIdxLst, fRstLst, lastH=lastH, tgt.scMtx ) 참고

    baseSpan <- as.integer(names(crScrH$stdIdx))
    baseSpan.len <- length(baseSpan)
    if( any(baseSpan[1:(baseSpan.len-1)]!=(baseSpan[2:baseSpan.len]-1)) ){
        return( "Error. check crScrH. missing step found." )
    }

    if( is.null(lastH) ){   lastH <- baseSpan[baseSpan.len]
    } else {
        fndIdx <- which(baseSpan==lastH)
        if( 0<length(fndIdx) ){
            baseSpan <- baseSpan[1:fndIdx]
        } else {    return( "Error. check crScrH range for lastH" ) }
    }

    fRstLst.hSpan <- as.integer(names(fRstLst)[1]):lastH
    fRstLst <- fRstLst[as.character(fRstLst.hSpan)]
    

    sfcHLst <- bUtil.getSfcHLst( stdFiltedCnt=allIdxLst$stdFiltedCnt[as.character(baseSpan)] ,baseSpan ,fRstLst )
    # filterLst <- HCR.getFilter.grp()  # HCR.getScoreMtx.grp() 내부에서 호출된다.

    scoreMtxLst <- list()
    for( sfcIdx in names(sfcHLst) ){

        hIdxStr <- as.character( sfcHLst[[sfcIdx]] )
        scoreMtx.grp <- HCR.getScoreMtx.grp( crScrH ,hIdxStr ,tgt.scMtx=tgt.scMtx )
        scoreMtxLst[[sfcIdx]] <- scoreMtx.grp
    }

    rObj <- list( scoreMtxLst=scoreMtxLst ,mInfo=list(hName=names(scoreMtxLst)) )
    rObj$mInfo$mName <- names(scoreMtxLst[[1]]$basic)

    return( rObj )
}

HCR.getScoreMtx.grp <- function( crScrH ,hIdxStr=NULL ,tgt.scMtx=NULL ){
    #   ph 단계가 없음을 유의하자.

    rObj <- list()

    crScrW <- crScrH
    if( !is.null(hIdxStr) ){
        crScrW$stdIdx   <- crScrW$stdIdx[hIdxStr]
        crScrW$std.grp  <- crScrW$std.grp[hIdxStr]
        crScrW$bS.grp   <- crScrW$bS.grp[hIdxStr]
    }

    filterLst <- HCR.getFilter.grp( tgt.scMtx )

    scoreMtxLst <- list()
    for( mName in names(filterLst) ){
        scoreMtxLst[[mName]] <- filterLst[[mName]]$fMtxObj( crScrW )
    }

    rObj$basic <- scoreMtxLst

    return( rObj )
}

HCR.getCutterGrp <- function( hMtxLst_HCR ,tgt.scMtx ){
    # bFCust.getFCustGrp( hMtxLst ,tgt.scMtx ) 참고.

    #   crScrHTool$addData
    # BUtil.makeCrScrHTool()
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



