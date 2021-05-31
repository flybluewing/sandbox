may.searchCutRst <- function( hIdx ,cutRstGrp ){

    for( nIdx in names(cutRstGrp) ){
        cutRst <- cutRstGrp[[nIdx]]
        fndIdx <- which( hIdx==names(cutRst$cutRstLst) )

        if( 0<length(fndIdx) ){
            cutRstLst <- cutRst$cutRstLst[[fndIdx]]
            cutRstLstHCR <- cutRst$cutRstLstHCR[[fndIdx]]
            return( list(cutRstLst=cutRstLst ,cutRstLstHCR=cutRstLstHCR) )
        }
    }

    return( NULL )
}

may.getTyp_mName <- function( cutInfoLst ){
    # cName <- c( "sN","sNx","sA","sAx","sMlt","sCrScr","sSN","sSNx","sSA","sSAx","sSMlt","sSCrScr","HCR","other" )
    # scrTyp <- rep( 0 ,length(cName) )   ;names(scrTyp)<-cName

    if( 0==length(cutInfoLst) ){
        return( character(0) )
    }

    typ <- rep("other",length(cutInfoLst))
    for( idx in 1:length(cutInfoLst) ){
        ci <- cutInfoLst[[idx]]

        if( grepl("^score\\d",ci["mName"]) ){
            typ[idx] <- ifelse( is.na(ci["fltName"]) ,"sN" ,"sNx" )     ;next
        }
        if( grepl("^score[A-Z]",ci["mName"]) ){
            typ[idx] <- ifelse( is.na(ci["fltName"]) ,"sA" ,"sAx" )     ;next
        }

        if( grepl("^mf",ci["mName"]) ){             typ[idx] <-"sMlt"   ;next   }

        if( grepl("^crScrN",ci["mName"]) ){         typ[idx] <-"sCrScr" ;next   }


        if( grepl("^sScore0\\d",ci["mName"]) ){
            typ[idx] <- ifelse( is.na(ci["fltName"]) ,"sSN" ,"sSNx" )   ;next
        }
        if( grepl("^sScore0[A-Z]",ci["mName"]) ){
            typ[idx] <- ifelse( is.na(ci["fltName"]) ,"sSA" ,"sSAx" )   ;next
        }

        if( grepl("^bsMR",ci["mName"]) ){             typ[idx] <-"sSMlt"   ;next  }

        if( grepl("^bSMScr",ci["mName"]) ){           typ[idx] <-"sSCrScr" ;next  }

        if( grepl("^HCR",ci["mName"]) ){              typ[idx] <-"HCR" ;next      }

        if( grepl("^bScr",ci["mName"]) ){             typ[idx] <-"bScr";next      }

    }

    return(typ)
}
may.getTyp_info <- function( cutInfoLst ){

    if( 0==length(cutInfoLst) ){
        return( character(0) )
    }

    typ <- rep("other",length(cutInfoLst))
    for( idx in 1:length(cutInfoLst) ){
        ci <- cutInfoLst[[idx]]     ;infoStr <- ci["info"]

        if( grepl("^[A-Za-z0-9_\\.]+\\([0-9]+\\)",infoStr) ){  typ[idx]<-"colRng(n)"   ;next   }

        if( grepl("^rReb\\(hpn:",infoStr) ){        typ[idx]<-"rReb"                ;next   }

        if( grepl("^evtCnt:",infoStr) ){            typ[idx]<-"evtCnt"              ;next   }

        if( grepl("^Evt Reb",infoStr) ){            typ[idx]<-"evt Reb"              ;next   }


        if( grepl("^forbidden Evt Reb",infoStr) ){  typ[idx]<-"forbidden Evt Reb"   ;next   }

        if( grepl("^fCol evtMaxFColTot",infoStr) ){ typ[idx]<-"fCol evtMaxFColTot"  ;next   }

        if( grepl("^fCol EvtCnt4AllPh",infoStr) ){  typ[idx]<-"fCol EvtCnt4AllPh"   ;next   }



        if( grepl("^summMtx\\.cut",infoStr) ){      typ[idx]<-"summMtx"             ;next   }

        if( grepl("^summMtx\\.reb\\.cut",infoStr) ){typ[idx]<-"summMtx.reb"         ;next   }

        if( grepl("^scMtx\\.sz\\.cut",infoStr) ){   typ[idx]<-"scMtx.sz"            ;next   }


        if( grepl("^rowRebDupBan\\(",infoStr) ){    typ[idx]<-"rowRebDupBan"        ;next   }

        if( grepl("^hIMtxHpnCnt\\(",infoStr) ){     typ[idx]<-"hIMtxHpnCnt"         ;next   }


        if( grepl("^l[1-9]c[1-9]:",infoStr) ){  typ[idx]<-"bUtil.chkStdMIPair()"     ;next   }

    }

    # k <- sapply(cutInfoLst,function(p){p["info"]})  ;names(k)<-NULL
    # k[ typ=="other" ]

    # infoStr <- k[ typ=="other" ]
    # kFlag <- grepl("^l1c1:",infoStr)
    # infoStr[kFlag]


    return( typ )
}

may.getTypLst <- function( cutRstGrp ){

    typLst <- list()
    tName <- c("sN","sNx","sA","sAx" ,"sMlt","sCrScr" ,"sSN" ,"sSNx","sSA","sSAx" ,"sSMlt","sSCrScr" ,"HCR" ,"bScr" ,"other")

    # cName <- c( "scoreN","scoreA","scoreMlt","sScoreN","sScoreA","sScoreMlt","HCR","other" )
    # scrTypMtx <- matrix( 0 ,nrow=0,ncol=length(cName),dimnames=list(NULL,cName) )

    #   grpId<-names(cutRstGrp)[1]  ;cutRstLst <- cutRstGrp[[grpId]]$cutRstLst  ;cutRstLstHCR <- cutRstGrp[[grpId]]$cutRstLstHCR
    #   hIdx <- names(cutRstLst)[5] ;cutInfoLst<-cutRstLst[[hIdx]]$cutInfoLst
    for( grpId in names(cutRstGrp) ){
        cutRstLst <- cutRstGrp[[grpId]]$cutRstLst
        cutRstLstHCR <- cutRstGrp[[grpId]]$cutRstLstHCR

        scrTyp <- rep( 0 ,length(cName) )   ;names(scrTyp)<- cName
        for( hIdx in names(cutRstLst) ){
            typ <- may.getTyp_mName( cutRstLst[[hIdx]]$cutInfoLst )
            typ <- c( typ ,may.getTyp_mName(cutRstLstHCR[[hIdx]]$cutInfoLst) )
            typLst[[hIdx]] <- typ
        }
    }

    return( list(typLst=typLst,tName=tName) )
}

may.loadSaves <- function( lastHSpan=NULL ){

    if( is.null(lastHSpan) ){
        lastHSpan <- c(800,820,840,860,880,900,920) # c(800,820,840,860,880,900,920,940,960)
    }

    cutRstGrp <- list()
    for( lastH in lastHSpan ){
        fileName <- sprintf("./report/workRpt/Obj_cutRstObj_%d.save",lastH)
        kName <- load( fileName )
        cutRstObj$lastH <- lastH
        cutRstGrp[[sprintf("H%d",lastH)]] <- cutRstObj
    }
    return( cutRstGrp )
}




may.plot <- function( datMtx ,ylim=NULL ,col=NULL ){
    #   datMtx <- cutCntMtx

    if( is.null(col) ){
        col <- colors()
        col <- col[!grepl("[[:digit:]]",col)]
        col <- col[!(col %in% c("white"))]

        col <- sample( col ,nrow(datMtx) )
    }
    if( is.null(ylim) ){
        ylim <- c(min(datMtx),max(datMtx))
    }
    xlim <- c(1 ,ncol(datMtx) )

    plot( NULL ,xlim=xlim, ylim=ylim )
    for( idx in 1:nrow(datMtx) ){
        lines( xlim[1]:xlim[2] ,datMtx[idx ,] ,col=col[idx] )
    }

    yPos <- ylim[2]-4*(1:nrow(datMtx))
    text( ncol(datMtx)*0.9 ,yPos ,rownames(datMtx) ,col=col )

}

