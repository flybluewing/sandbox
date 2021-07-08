if( FALSE ){
    install.packages( "gridExtra" )
}
library(gridExtra)      # ggplot2의 화면분할 용.

may.getLowH <- function( cntDf ,thld=10 ){
    # cntDf <- cntDfA 
    lDf <- ddply(cntDf,.(hIdx),function(pDf){
        data.frame(lowCut=nrow(pDf)<thld)
    })
    return( lDf$hIdx[lDf$lowCut] )
}


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

may.searchTLst <- function( tLst ,pGrpId=NULL ,pHIdx=NULL ,pMName=NULL ,pM=NULL ,pI=NULL ){

    typLst <- tLst$typLst
    flagLst <- lapply( typLst ,function( pMtx ){ rep(T,ncol(pMtx)) } )
    
    rDf <- as.data.frame( t(typLst[[1]][,0,drop=F]) )
    for( tIdx in names(typLst) ){
        mtx <- typLst[[tIdx]]
        if( 0==ncol(mtx) ) next

        if( !is.null(pGrpId) ){     # pGrpId=c("H800","H820")
            if( !(mtx["grpId",1]%in%pGrpId) ){
                flagLst[[tIdx]][] <- F
            }
        }

        if( !is.null(pHIdx) ){      # pHIdx=c("H786_1","H790_2")
            if( tIdx %in% pHIdx ){
                flagLst[[tIdx]][] <- F
            }
        }

        if( !is.null(pMName) ){     # pMName=c("score3","score9")
            flagLst[[tIdx]] <- flagLst[[tIdx]] & mtx["mName",]%in%pMName
        }

        if( !is.null(pM) ){         # pM = c("other")
            flagLst[[tIdx]] <- flagLst[[tIdx]] & mtx["M",]%in%pM
        }

        if( !is.null(pI) ){         # pI = c("rReb")
            flagLst[[tIdx]] <- flagLst[[tIdx]] & mtx["I",]%in%pI
        }

        selIdx <- which(flagLst[[tIdx]])        # selected(survived) column index
        if( 0<length(selIdx) ){
            sDf <- as.data.frame( t(typLst[[tIdx]][ ,selIdx]) )
            sDf$hIdx <- tIdx
            sDf$cIdx <- selIdx

            rDf <- rbind( rDf ,sDf )
        }
        
    }

    return( rDf )
}

may.searchTLst_df <- function( tLst ,pSrchDf ){
    #   pSrchDf : mName M I     <--- Unique 상태로 정리해놓을 것.
    mCol <- c("mName","M","I")
    mCol <- intersect( colnames(pSrchDf) ,mCol )

    pSrchDf <- ddply( pSrchDf ,mCol ,nrow )

    typLst <- tLst$typLst
    flagLst <- lapply( typLst ,function( pMtx ){ rep(T,ncol(pMtx)) } )

    rDf <- as.data.frame( t(typLst[[1]][,0,drop=F]) )
    for( tIdx in names(typLst) ){
        mtx <- typLst[[tIdx]]
        if( 0==ncol(mtx) ) next

        mFlag <- apply( mtx ,2 ,function( cDat ){
            for( sIdx in 1:nrow(pSrchDf) ){
                if( all(cDat[mCol]==pSrchDf[sIdx,mCol]) ){
                    return( TRUE )
                }
            }
            return( FALSE )
        })

        if( any(mFlag) ){
            sDf <- as.data.frame( t(mtx[,mFlag]) )
            sDf$hIdx <- tIdx    ;sDf$cIdx <- which(mFlag)
            rDf <- rbind( rDf ,sDf )
        }

    }

    rDf <- rDf[order(rDf$hIdx,rDf$cIdx) ,]

    return( rDf )
}

may.cutInfoLst_rmDup <- function( cutRstGrp ){

    getUniqueFlag <- function( cutInfoLst ){
        #   cutInfoLst <- cutRstGrp[["H880"]]$cutRstLst[["H880_2"]]$cutInfoLst

        cLen <- length(cutInfoLst)
        if( 2> cLen ){
            return( cutInfoLst )
        }

        rLst <- list()
        surFlag <- rep( T ,cLen <- length(cutInfoLst) )
        for( idx1 in 1:(cLen-1) ){
            if( !surFlag[idx1] )    next

            cName1 <- names(cutInfoLst[[idx1]])
            for( idx2 in (idx1+1):cLen ){
                cName2 <- names(cutInfoLst[[idx2]])

                fltNameFlag <- c( "fltName"%in%cName1 ,"fltName"%in%cName2 )
                if( 1==sum(fltNameFlag) )   next            # fltName이 양쪽 모두 있거나 모두 없거나..

                chkName <- c("typ","mName","pName","info")
                if( all(fltNameFlag) ){
                    chkName <- c("typ","mName","pName","fltName","info")
                }

                matFlag <- cutInfoLst[[idx1]][chkName] == cutInfoLst[[idx2]][chkName]
                if( all(matFlag) ){
                    surFlag[idx2] <- FALSE
                }
            }

        }
        # dbgMtx <- sapply( cutInfoLst ,function(p){ p[c("typ","mName","pName","info")] })
        # dbgMtx <- cbind( surFlag ,t(dbgMtx) )   ;dbgMtx

        rLst <- cutInfoLst[surFlag]
        return( rLst )
    }

    for( gIdx in names(cutRstGrp) ){
        for( hIdx in names(cutRstGrp[[gIdx]]$cutRstLst) ){
            cutRstGrp[[gIdx]]$cutRstLst[[hIdx]]$cutInfoLst <- getUniqueFlag( cutRstGrp[[gIdx]]$cutRstLst[[hIdx]]$cutInfoLst )
            cutRstGrp[[gIdx]]$cutRstLstHCR[[hIdx]]$cutInfoLst <- getUniqueFlag( cutRstGrp[[gIdx]]$cutRstLstHCR[[hIdx]]$cutInfoLst )
        }
    }

    return(cutRstGrp)
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
    #   grpId<-names(cutRstGrp)[1]  ;cutRstLst <- cutRstGrp[[grpId]]$cutRstLst  ;cutRstLstHCR <- cutRstGrp[[grpId]]$cutRstLstHCR
    #   hIdx <- names(cutRstLst)[5] ;cutInfoLst<-cutRstLst[[hIdx]]$cutInfoLst
    for( grpId in names(cutRstGrp) ){
        cutRstLst <- cutRstGrp[[grpId]]$cutRstLst
        cutRstLstHCR <- cutRstGrp[[grpId]]$cutRstLstHCR

        for( hIdx in names(cutRstLst) ){
            mName <- sapply( cutRstLst[[hIdx]]$cutInfoLst ,function(ci){ci["mName"]})
            typM <- may.getTyp_mName( cutRstLst[[hIdx]]$cutInfoLst )
            typI <- may.getTyp_info( cutRstLst[[hIdx]]$cutInfoLst )
            infoStr <- sapply( cutRstLst[[hIdx]]$cutInfoLst ,function(ci){ci["info"]})

            if( 0<length(cutRstLstHCR[[hIdx]]$cutInfoLst) ){
                mName <- c( mName ,sapply( cutRstLstHCR[[hIdx]]$cutInfoLst ,function(ci){ci["mName"]}) )
                typM <- c( typM ,may.getTyp_mName(cutRstLstHCR[[hIdx]]$cutInfoLst) )
                typI <- c( typI ,may.getTyp_info(cutRstLstHCR[[hIdx]]$cutInfoLst) )
                infoStr <- c( infoStr ,sapply( cutRstLstHCR[[hIdx]]$cutInfoLst ,function(ci){ci["info"]}) )
            }

            typMtx <- matrix( "" ,nrow=5 ,ncol=0 )   ;rownames(typMtx) <- c("grpId","mName","M","I","info")
            if( 0<length(typM)){
                typMtx <- matrix( "" ,nrow=5 ,ncol=length(typM) ,dimnames=list(c("grpId","mName","M","I","info"),NULL) )
                typMtx["grpId",] <- grpId
                typMtx["mName",]<- mName    ;typMtx["M",] <- typM   ;typMtx["I",] <- typI
                typMtx["info",] <- infoStr
            }

            typLst[[hIdx]] <- typMtx
        }
    }

    nameLst <- list(M=NULL ,I=NULL)
    for( nIdx in names(typLst) ){
        mtx <- typLst[[nIdx]]
        nameLst[["M"]]  <- c( nameLst[["M"]] ,mtx["M",] )
        nameLst[["I"]]  <- c( nameLst[["I"]] ,mtx["I",] )
    }

    return( list(typLst=typLst ,typName=lapply( nameLst ,function(p){ unique(p) }) ) )
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



may.anaReb_inB <- function( cntDfA ,cntDfB ,freqThld=1 ){

    rtLen <- nrow(cntDfB)

    mCol <- c("mName","M","I")
    cntDfA_HCnt <- ddply(cntDfA ,.(mName,M,I) ,function(pDf){ data.frame(cntH=nrow(pDf)) })
    cntDfB_HCnt <- ddply(cntDfB ,.(mName,M,I) ,function(pDf){ data.frame(cntH=nrow(pDf)) })
    hCntDf <- merge(cntDfA_HCnt ,cntDfB_HCnt ,by=mCol  ,suffixes = c(".A",".B"),)
    #               mName       M                    I cntH.A cntH.B
    #         1    score2      sN    forbidden Evt Reb      1      1
    #         2    score2      sN             scMtx.sz      1      1
    #         3    score3      sN             scMtx.sz      1      1

    hpnDf <- data.frame( hpnYN=rep(F,rtLen) )
    matDf <- hCntDf[hCntDf$cntH.A>=freqThld ,]  ;ALen <- nrow(matDf)
    for( rIdx in seq_len(rtLen) ){
        for( aIdx in seq_len(ALen) ){
            if( all(cntDfB[rIdx,mCol]==matDf[aIdx,mCol]) ){
                hpnDf$hpnYN[rIdx] <- TRUE
                next
            }
        }
    }

    return( list(hpnDf=hpnDf ,hCntDf=hCntDf) )
}




may.plot <- function( datMtx ,ylim=NULL ,col=NULL ){
    #   datMtx <- cutCntMtx
    #   col : darksalmon navy darkgoldenrod darkseagreen mediumaquamarine firebrick salmon
    #           purple navy mediumvioletred indianred   peru mediumblue lightslateblue
    #           darkred paleturquoise rosybrown slategray

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

    yPos <- ylim[2] - (1:nrow(datMtx)-1)
    text( ncol(datMtx)*0.9 ,yPos ,sprintf("%s(%s)",rownames(datMtx),col) ,col=col )

    return( col )
}

may.histPlot <- function( datMtx ,ylim=NULL ,col=NULL ){

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

    # WORKING    

}



may.cutRstGrp_2Df <- function( cutRstGrp ){

    rDf <- data.frame( gIdx=character(0) ,hIdx=character(0) ,hName=character(0) ,pName=character(0) ,mName=character(0) ,fltName=character(0) ,info=character(0)  )
    codeDf <- data.frame( M=character(0) ,I=character(0) )

    cName <- c( "hName" ,"mName" ,"pName" ,"fltName" ,"info" )
    cNameDf <- c( "gIdx" ,"hIdx" ,"hName" ,"pName" ,"mName" ,"fltName" ,"info" )
    for( gIdx in names(cutRstGrp) ){
        cutRstLst   <- cutRstGrp[[gIdx]]$cutRstLst
        cutRstLstHCR<- cutRstGrp[[gIdx]]$cutRstLstHCR

        for( hIdx in names(cutRstLst) ){            
            # cutRstLst
            cutInfoLst <- cutRstLst[[hIdx]]$cutInfoLst
            if( 0<length(cutInfoLst) ){
                mtx <- sapply( cutInfoLst ,function(p){ p[cName] })
                rownames( mtx ) <- cName        # row 값이 모두 NA인 경우, 해당 row 이름도 NA가 되어버리는 거 방지
                df <- as.data.frame( t(mtx) )
                df$fltName[is.na(df$fltName)] <- "N/A"
                df <- ddply( df ,.(mName,pName,fltName,info) ,function(pDf){ 
                    data.frame(gIdx=gIdx ,hIdx=hIdx ,hName=pDf$hName[1]) 
                })

                rDf <- rbind( rDf ,df[,cNameDf] )
            }
            
            # cutRstLstHCR
            cutInfoLst <- cutRstLstHCR[[hIdx]]$cutInfoLst
            if( 0<length(cutInfoLst) ){
                mtx <- sapply( cutInfoLst ,function(p){ p[cName] })
                rownames( mtx ) <- cName    # row 값이 모두 NA인 경우, 해당 row 이름도 NA가 되어버리는 거 방지
                df <- as.data.frame( t(mtx) )
                df$fltName[is.na(df$fltName)] <- "N/A"
                df <- ddply( df ,.(mName,pName,fltName,info) ,function(pDf){ 
                    data.frame(gIdx=gIdx ,hIdx=hIdx ,hName=pDf$hName[1]) 
                })

                rDf <- rbind( rDf ,df[,cNameDf] )
            }
        }
    }

    return( rDf )

}

