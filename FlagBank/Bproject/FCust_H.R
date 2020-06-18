# Utils =================================================================================
bFCust.defaultHardFlag <- function( pName ){
    hardPh	<- c("basic")
    pFlag <- pName %in% hardPh

	flag <- c( p=pFlag )
	return( list( flag=flag ) )
}
bFCust.getEvt <- function( smRow ,fCol ){
    #   evt : evt define matrix. colname:("lev","val")

    evtMtx <- matrix( NA, nrow=2 ,ncol=length(smRow) )
    colnames( evtMtx ) <- names(smRow)
    rownames( evtMtx ) <- c( "val" ,"lev" )
    for( fcName in names(fCol) ){
        fndIdx <- which( smRow[fcName]==fCol[[fcName]]$evt[,"val"] )
        if( 0==length(fndIdx) ){
            evtMtx[,fcName] <- NA
        } else {
            evtMtx["val",fcName] <- smRow[fcName]
            evtMtx["lev",fcName] <- fCol[[fcName]]$evt[fndIdx,"lev"]
        }
    }

    return( evtMtx )
}
bFCust.evtComp <- function( evtLevH1 ,evtLevH2 ,levMin=1 ){
    # evtLevH1가 가장 최신, evtLevH2는 그 이전.

    matFlag <- evtLevH1 == evtLevH2
    matFlag[is.na(matFlag) | evtLevH1<levMin ] <- FALSE

    # evtLevDup
    evtLevDup <- evtLevH1
    evtLevDup[!matFlag] <- NA

    allMat <- FALSE
    evt1Cnt <- sum( (evtLevH1>=levMin) ,na.rm=T )
    evt2Cnt <- sum( (evtLevH2>=levMin) ,na.rm=T )
    if( evt1Cnt==evt2Cnt ){
        allMat <- evt1Cnt == sum( !is.na(evtLevDup) )
    }
    
    return( list( levDup=evtLevDup ,allMat=allMat ) )
}
bFCust.getEvtMtx <- function( scMtx ,evtCfg ){

    eValMtx <- scMtx       ;eLevMtx <- scMtx
    for( pName in colnames(scMtx) ){
        evtMtx <- bFCust.getEvt( scMtx[,pName] ,evtCfg$fCol )
        eValMtx[,pName] <- evtMtx["val",]
        eLevMtx[,pName] <- evtMtx["lev",]
    }

    return( list(eValMtx=eValMtx ,eLevMtx=eLevMtx) )
}
bFCust.evtCompMtx <- function( eLevMtxH1 ,eLevMtxH2 ,levMin=1 ){
    dupMtx <- eLevMtxH1
    allMat.ph <- rep( F ,ncol(dupMtx) )
    names(allMat.ph) <- colnames(dupMtx)

    for( pName in colnames(dupMtx) ){
        compRst <- bFCust.evtComp( eLevMtxH1[,pName] ,eLevMtxH2[,pName] ,levMin )
        dupMtx[,pName] <- compRst$levDup
        allMat.ph[pName] <- compRst$allMat
    }

    allMat.fCol <- rep( F ,nrow(dupMtx) )
    names(allMat.fCol) <- rownames(dupMtx)
    for( fcName in rownames(dupMtx) ){
        compRst <- bFCust.evtComp( eLevMtxH1[fcName ,] ,eLevMtxH2[fcName ,] ,levMin )
        allMat.ph[pName] <- compRst$allMat
    }

    return( list(dupMtx=dupMtx ,allMat.ph=allMat.ph ,allMat.fCol=allMat.fCol) )
}
bFCust.getEvt_byHIdx <- function( scMtx ,cfg ,lastEvt=NULL ,ignoreOpt=NULL ){
	#	ignoreOpt : 모든 확인에서 0을 무시하려면 "all"=0, 무시 없으려면 ignoreOpt=NULL
    #   cfg : scoreMtxCfg[[mName]]
	#	lastEvt
	#		hIdxObj <- B.getHMtxLst_byHIdx( curHMtxLst )
	#		mtxLst=hIdxObj[[hName]][[mName]]
	#		lastEvt <- bUtil.getEvt_byHIdx( mtxLst[[length(mtxLst)]] ,evtLst ,mtxLst[[length(mtxLst)-1]] )

    getEvtMatMtx <- function( eMtx1 ,eMtx2 ){
        mask <- eMtx1==eMtx2
        mask[is.na(mask)] <- F

        matMtx <- eMtx1
        matMtx[!mask] <- NA
        return( matMtx )
    }
	getPhaseRebMtx <- function( mtx ,ignoreThld=0 ){ # 바로 다음 phase와의 동일여부

		if( !is.null(ignoreThld) ){
			mtx[mtx<=ignoreThld] <- NA
		}
		mtxCSize <- ncol(mtx)

		rName <- c("reb","hpn","sum")
		rebMtx <- matrix( 0 ,nrow=length(rName) ,ncol=(mtxCSize-1) )
		rownames( rebMtx ) <- rName
		colnames( rebMtx ) <- colnames(mtx)[1:(mtxCSize-1)]

		vCnt <- apply( mtx ,2 ,function(byCV){ sum(!is.na(byCV)) })
		for( cIdx in 1:(mtxCSize-1) ){

			matCnt <- sum( mtx[,cIdx]==mtx[,cIdx+1] ,na.rm=T )
			if( vCnt[cIdx]>0 && vCnt[cIdx]==vCnt[cIdx+1] ){	# 전체 매치 여부만 T/F로 따지자.
				rebMtx["reb",cIdx] <- matCnt == vCnt[cIdx]
			}
			rebMtx["hpn",cIdx] <- sum( mtx[,cIdx]>0 ,na.rm=T )
			rebMtx["sum",cIdx] <- sum( mtx[,cIdx] ,na.rm=T )
		}

		return( rebMtx )
	}
	getHpnInfo <- function( rawMtx ,ignoreThld ){
		hpnInfo <- list( tot=	sum(rawMtx>0)
							,fCol=	apply( rawMtx ,1 ,function(byRV){ sum(byRV>0) })
							,phase= apply( rawMtx ,2 ,function(byCV){ sum(byCV>0) })
							,rawMtx=rawMtx
						)
		hpnInfo$phaseReb <- getPhaseRebMtx( rawMtx ,ignoreThld=ignoreThld )
		return( hpnInfo )
	}
	getEvtInfo <- function( rawMtx ,cfg ){
		evtInfo <- list()

        eObj <- bFCust.getEvtMtx( rawMtx ,cfg )
        eValMtx <- eObj$eValMtx

		evtInfo$eValMtx <- eValMtx
		evtInfo$evtMask <- !is.na(eValMtx)
		evtInfo$tot = sum(!evtInfo$evtMask)
		evtInfo$fCol = apply( evtInfo$evtMask ,1 ,function(byRV){ sum(byRV) })
		evtInfo$phase = apply( evtInfo$evtMask ,2 ,function(byCV){ sum(byCV) } )

        evtInfo$phaseReb <- getPhaseRebMtx( eValMtx ,ignoreThld=NULL )
		return( evtInfo )
	}

	rObj <- list( dim=dim(scMtx) )
	rObj$hpnInfo <- getHpnInfo( scMtx ,ignoreThld=ifelse( cfg$style["freqZero"] ,0 ,-1 ) )
	rObj$evtInfo <- getEvtInfo( scMtx ,cfg )


	rObj$rebInfo <- NULL
	rObj$rebSummReb <- NULL
	if( !is.null(lastEvt) ){
		rebInfo <- list()

		if( TRUE ){		# rebInfo$rebMtx.*
			# rebInfo$rebMtx.all ----------------------------------------------
			rName <- c("rebFlag","hpn")
			cName <- c("rebRaw","rebEvt")
			rebMtx.all <- matrix( 0 ,nrow=length(rName) ,ncol=length(cName) )
			rownames(rebMtx.all) <- rName	;colnames(rebMtx.all) <- cName

			matMask <- !( rObj$hpnInfo$rawMtx==0 & lastEvt$hpnInfo$rawMtx==0 )
			if( !cfg$style["freqZero"] ) matMask[,] <- TRUE
			rebMtx <- rObj$hpnInfo$rawMtx == lastEvt$hpnInfo$rawMtx
			rebMtx[!matMask] <- F
			rebMtx.all["hpn","rebRaw"] <- sum(rObj$hpnInfo$rawMtx >0)
			rebMtx.all["rebFlag","rebRaw"] <- (sum(matMask)>0)  &&  (sum(rebMtx)==sum(matMask))

			matMask <- !( is.na(rObj$evtInfo$eValMtx) & is.na(lastEvt$evtInfo$eValMtx) )
			rebMtx <- rObj$evtInfo$eValMtx==lastEvt$evtInfo$eValMtx
			rebMtx.all["hpn","rebEvt"] <- sum(rObj$evtInfo$eValMtx>0,na.rm=T)
			rebMtx.all["rebFlag","rebEvt"] <- (sum(matMask)>0) && (sum(rebMtx,na.rm=T)==sum(matMask))

			rebInfo$rebMtx.all <- rebMtx.all

			# rebInfo$rebMtx.ph (rawMtx/eValMtx)----------------------------------------------
			rName <- c("rebFlag.raw","hpn.raw","rebFlag.evt","hpn.evt")
			cName <- colnames(rObj$hpnInfo$rawMtx)
			rebMtx.ph <- matrix( 0 ,nrow=length(rName) ,ncol=length(cName) )
			rownames(rebMtx.ph) <- rName	;colnames(rebMtx.ph) <- cName

			rebMtx.ph["hpn.raw",] <- apply( rObj$hpnInfo$rawMtx>0 ,2 ,sum )
			rebMtx.ph["hpn.evt",] <- apply( !is.na(rObj$evtInfo$eValMtx) ,2 ,sum )
			for( pName in colnames(rObj$hpnInfo$rawMtx) ){
				matMask <- !( rObj$hpnInfo$rawMtx[,pName]==0 & lastEvt$hpnInfo$rawMtx[,pName]==0 )
				if( !cfg$style["freqZero"] ) matMask[] <- TRUE
				rebFlag <- rObj$hpnInfo$rawMtx[,pName] == lastEvt$hpnInfo$rawMtx[,pName]
				rebFlag[!matMask] <- F
				rebMtx.ph["rebFlag.raw",pName] <- (sum(matMask)>0) && (sum(rebFlag)==sum(matMask))

				matMask <- !( is.na(rObj$evtInfo$eValMtx[,pName]) & is.na(lastEvt$evtInfo$eValMtx[,pName]) )
				rebFlag <- rObj$evtInfo$eValMtx[,pName]==lastEvt$evtInfo$eValMtx[,pName]
				rebMtx.ph["rebFlag.evt",pName] <- (sum(matMask)>0) && (sum(rebFlag,na.rm=T)==sum(matMask))
			}

			rebInfo$rebMtx.ph <- rebMtx.ph

			# rebInfo$rebMtx.fCol (rawMtx/eValMtx)----------------------------------------------
			rName <- c("rebFlag.raw","hpn.raw","rebFlag.evt","hpn.evt")
			cName <- rownames(rObj$hpnInfo$rawMtx)
			rebMtx.fCol <- matrix( 0 ,nrow=length(rName) ,ncol=length(cName) )
			rownames(rebMtx.fCol) <- rName	;colnames(rebMtx.fCol) <- cName

			rebMtx.fCol["hpn.raw",] <- apply( rObj$hpnInfo$rawMtx>0 ,1 ,sum )
			rebMtx.fCol["hpn.evt",] <- apply( !is.na(rObj$evtInfo$eValMtx) ,1 ,sum )
			for( fcName in rownames(rObj$hpnInfo$rawMtx) ){
				matMask <- !( rObj$hpnInfo$rawMtx[fcName ,]==0 & lastEvt$hpnInfo$rawMtx[fcName ,]==0 )
				if( !cfg$style["freqZero"] ) matMask[] <- TRUE
				rebFlag <- rObj$hpnInfo$rawMtx[fcName ,] == lastEvt$hpnInfo$rawMtx[fcName ,]
				rebFlag[!matMask] <- F
				rebMtx.fCol["rebFlag.raw" ,fcName] <- (sum(matMask)>0)  &&  (sum(rebFlag)==sum(matMask))

				matMask <- !( is.na(rObj$evtInfo$eValMtx[fcName ,]) & is.na(lastEvt$evtInfo$eValMtx[fcName ,]) )
				rebFlag <- rObj$evtInfo$eValMtx[fcName ,]==lastEvt$evtInfo$eValMtx[fcName ,]
				rebMtx.fCol["rebFlag.evt" ,fcName] <- (sum(matMask)>0)  &&  (sum(rebFlag,na.rm=T)==sum(matMask))
			}

			rebInfo$rebMtx.fCol <- rebMtx.fCol


			# rebInfo$rebMtx.phReb (phaseReb)----------------------------------------------
			rName <- c("raw","evt")
			cName <- colnames(rObj$hpnInfo$phaseReb)
			rebMtx.phReb <- matrix( 0 ,nrow=length(rName) ,ncol=length(cName) )
			rownames(rebMtx.phReb) <- rName	;colnames(rebMtx.phReb) <- cName

			rebMask <- rObj$hpnInfo$phaseReb["hpn",]>0 & lastEvt$hpnInfo$phaseReb["hpn",]>0
            if( !cfg$style["freqZero"] ) matMask[] <- TRUE
			rebMtx.phReb["raw" ,] <- rObj$hpnInfo$phaseReb["reb",] & lastEvt$hpnInfo$phaseReb["reb",]
			rebMtx.phReb["raw",!rebMask] <- FALSE
			rebMask <- rObj$evtInfo$phaseReb["hpn",]>0 & lastEvt$evtInfo$phaseReb["hpn",]>0
			rebMtx.phReb["evt" ,] <- rObj$evtInfo$phaseReb["reb",] & lastEvt$evtInfo$phaseReb["reb",]
			rebMtx.phReb["evt",!rebMask] <- FALSE

			rebInfo$rebMtx.phReb <- rebMtx.phReb

			# rebInfo$rebMtx.xyCnt ----------------------------------------------
			rName <- c("raw","evt")
			cName <- c("fCol.allMat","fCol.cntHpn","phase.allMat","phase.cntHpn")
			rebMtx <- matrix( 0 ,nrow=length(rName) ,ncol=length(cName) )
			rownames(rebMtx) <- rName	;colnames(rebMtx) <- cName

			rebMtx["raw","fCol.allMat"] <- all( rObj$hpnInfo$fCol==lastEvt$hpnInfo$fCol )
			rebMtx["raw","fCol.cntHpn"] <- sum( rObj$hpnInfo$fCol > 0 )
			rebMtx["raw","phase.allMat"] <- all( rObj$hpnInfo$phase==lastEvt$hpnInfo$phase )
			rebMtx["raw","phase.cntHpn"] <- sum( rObj$hpnInfo$phase > 0 )
			rebMtx["evt","fCol.allMat"] <- all( rObj$evtInfo$fCol==lastEvt$evtInfo$fCol )
			rebMtx["evt","fCol.cntHpn"] <- sum( rObj$evtInfo$fCol > 0 )
			rebMtx["evt","phase.allMat"] <- all( rObj$evtInfo$phase==lastEvt$evtInfo$phase )
			rebMtx["evt","phase.cntHpn"] <- sum( rObj$evtInfo$phase > 0 )

            if( cfg$style["freqZero"] ){
                if( 0==rebMtx["raw","fCol.cntHpn"] )    rebMtx["raw","fCol.allMat"] <- FALSE
                if( 0==rebMtx["raw","phase.cntHpn"] )   rebMtx["raw","phase.allMat"] <- FALSE
                if( 0==rebMtx["evt","fCol.cntHpn"] )    rebMtx["evt","fCol.allMat"] <- FALSE
                if( 0==rebMtx["evt","phase.cntHpn"] )   rebMtx["evt","phase.allMat"] <- FALSE
            }

			rebInfo$rebMtx.xyCnt <- rebMtx

			# rebInfo$summary ----------------------------------------------
			rName <- c("raw","evt")
			cName <- c("all","ph","fCol","phReb","xyCnt.fCol","xyCnt.phase")
			summMtx <- matrix( 0 ,nrow=length(rName) ,ncol=length(cName) )
			rownames(summMtx) <- rName	;colnames(summMtx) <- cName

			summMtx[,"all"]			<- rebInfo$rebMtx.all["rebFlag" ,]

			rowMask <- (rebInfo$rebMtx.ph["hpn.raw",]>0)
			if( !cfg$style["freqZero"] ) rowMask[] <- TRUE
			summMtx["raw","ph"]		<- sum( rebInfo$rebMtx.ph["rebFlag.raw",] & rowMask )
			summMtx["evt","ph"]		<- sum( rebInfo$rebMtx.ph["rebFlag.evt",] & (rebInfo$rebMtx.ph["hpn.evt",]>0) )

			rowMask <- (rebInfo$rebMtx.fCol["hpn.raw",]>0)
			if( !cfg$style["freqZero"] ) rowMask[] <- TRUE
			summMtx["raw","fCol"]	<- sum( rebInfo$rebMtx.fCol["rebFlag.raw",] & rowMask )
			summMtx["evt","fCol"]	<- sum( rebInfo$rebMtx.fCol["rebFlag.evt",] & (rebInfo$rebMtx.fCol["hpn.evt",]>0) )

			summMtx[,"phReb"]		<- apply( rebInfo$rebMtx.phReb ,1 ,sum )

			summMtx[,"xyCnt.fCol"]	<- rebInfo$rebMtx.xyCnt[,"fCol.allMat"]
			summMtx[,"xyCnt.phase"]	<- rebInfo$rebMtx.xyCnt[,"phase.allMat"]

			rebInfo$summMtx <- summMtx



			# eValMtx,eLevMtx ----------------------------------------------
            #   H1,H2에서의 연속발생
            #   연속발생이 아예 없으면 NULL
            eObj.last <- bFCust.getEvtMtx( lastEvt$hpnInfo$rawMtx ,cfg )
            eObj.cur <- bFCust.getEvtMtx( scMtx ,cfg )
            rebInfo$eValMtx <- getEvtMatMtx( eObj.last$eValMtx ,eObj.cur$eValMtx )
            rebInfo$eLevMtx <- getEvtMatMtx( eObj.last$eLevMtx ,eObj.cur$eLevMtx )
            if( 0==sum(!is.na(rebInfo$eValMtx)) )   rebInfo$eValMtx <- NULL
            if( 0==sum(!is.na(rebInfo$eLevMtx)) )   rebInfo$eLevMtx <- NULL
        }
		rObj$rebInfo <- rebInfo


		if( !is.null(lastEvt$rebInfo) ){	# rObj$rebSummReb
			hpnRebMtx <- rebInfo$summMtx>0 & lastEvt$rebInfo$summMtx>0

			valMatMtx <- rebInfo$summMtx == lastEvt$rebInfo$summMtx
			valMatMtx[ !hpnRebMtx ] <- FALSE

			rObj$rebSummReb <- list( hpnRebMtx=hpnRebMtx ,valMatMtx=valMatMtx )
        }
    }

	return( rObj )
}
bFCust.getSkipZero_byHIdx <- function( mtxLst ,cfg ,lastSZ=NULL ){

    getSkipZero <- function( mtxLst ){
        hLen <- length(mtxLst)

        mtx <- mtxLst[[1]]
        mtx[,] <- 0

        mtx.ph <- mtx
        for( pName in colnames(mtx.ph) ){
            for( hIdx in hLen:1 ){
                if( 0 < sum(mtxLst[[hIdx]][,pName]>0,na.rm=T) ){
                    mtx.ph[,pName] <- mtxLst[[hIdx]][,pName]
                    break
                }
            }
        }

        mtx.fCol <- mtx
        for( fColName in rownames(mtx.fCol) ){
            for( hIdx in hLen:1 ){
                if( 0 < sum(mtxLst[[hIdx]][fColName,]>0,na.rm=T) ){
                    mtx.fCol[fColName,] <- mtxLst[[hIdx]][fColName,]
                    break
                }
            }
        }

        matFlag <- mtx.ph==mtx.fCol
        matFlag[is.na(matFlag) | mtx.ph==0] <- FALSE

        return( list(ph=mtx.ph ,fCol=mtx.fCol ,matFlag=matFlag) )
    }

    skipZero.raw <- getSkipZero(mtxLst)
    szRaw <- list( ph=skipZero.raw$ph   ,fCol=skipZero.raw$fCol )
    szRaw$dblHpn <- skipZero.raw$ph
    szRaw$dblHpn[!skipZero.raw$matFlag] <- NA

    mtxLst.eVal <- lapply( mtxLst ,function( mtx ){
        eObj <- bFCust.getEvtMtx( mtx ,cfg )
        return(eObj$eValMtx)
    })
    skipZero.eVal <- getSkipZero(mtxLst.eVal)
    szEVal <- list( ph=skipZero.eVal$ph   ,fCol=skipZero.eVal$fCol )
    szEVal$dblHpn <- skipZero.eVal$ph
    szEVal$dblHpn[!skipZero.eVal$matFlag] <- NA

    szObj <- list(raw=szRaw ,eVal=szEVal)

    rebInfo <- NULL
    if( !is.null(lastSZ) ){
        #   이전 대에서의 sz정보와 마지막 발생 값과의 비교.
        scMtx <- mtxLst[[length(mtxLst)]]
        scMtxEvt <- bFCust.getEvtMtx( scMtx ,cfg )$eValMtx
        rebInfo <- bFCust.getSkipZero_byHIdx.ass( lastSZ ,scMtx ,scMtxEvt )
    }
    szObj$rebInfo <- rebInfo

    return( szObj )
}
bFCust.getSkipZero_byHIdx.ass <- function( szObj ,scMtx ,scMtxEvt ){
    #   szObj <- bFCust.getSkipZero_byHIdx( mtxLst ,cfg )
    #   scMtxEvt <- bFCust.getEvtMtx(scMtx,cfg)$eValMtx

    chkMatch <- function( szInfo ,mtx ,hpnMin=1 ,hpnMin.dbl=5 ){
        # ignrVal : NULL 이면 mtx를 NA를 비교대상에서 제외.
        #           0 등의 값을 주면 mtx내 0을 비교대상에서 제외.

        rName <- c("mat","hpn") # matFlag, hpnCnt

        # mtxPh
        mtxPh <- matrix( 0 ,ncol=ncol(szInfo$ph) ,nrow=length(rName) )
        colnames(mtxPh) <- colnames(szInfo$ph)  ;rownames(mtxPh) <- rName
        mtxPh["hpn",] <- apply( szInfo$ph ,2 ,function(fCol){sum(fCol>0,na.rm=T)} )
        for( pName in colnames(mtxPh) ){
            if( mtxPh["hpn",pName]!=sum(mtx[,pName]>0,na.rm=T) ){
                mtxPh["mat",pName] <- 0
                next    
            }

            matCnt <- sum( szInfo$ph[,pName]==mtx[,pName] ,na.rm=T )
            availCnt <- sum( !is.na(szInfo$ph[,pName]) )
            mtxPh["mat",pName] <- availCnt>0 && (matCnt==availCnt)
        }
        mtxPh["mat" ,mtxPh["hpn",]<hpnMin] <- 0

        # mtxFCol
        mtxFCol <- matrix( 0 ,ncol=nrow(szInfo$fCol) ,nrow=length(rName) )
        colnames(mtxFCol) <- rownames(szInfo$fCol)  ;rownames(mtxFCol) <- rName
        mtxFCol["hpn",] <- apply( szInfo$fCol ,1 ,function(ph){sum(ph>0,na.rm=T)} )
        for( fcName in colnames(mtxFCol) ){
            if( mtxFCol["hpn",fcName] != sum(mtx[fcName,]>0,na.rm=T) ){
                mtxFCol["mat",fcName] <- 0
                next
            }

            matCnt <- sum( szInfo$fCol[fcName,]==mtx[fcName,] ,na.rm=T )
            availCnt <- sum( !is.na(szInfo$fCol[fcName,]) )
            mtxFCol["mat",fcName] <- availCnt>0 && (matCnt==availCnt)
        }
        mtxFCol["mat" ,mtxFCol["hpn",]<hpnMin] <- 0

        # dblHpn
        dblHpn <- c( mat=0 ,hpn=sum( !is.na(szInfo$dblHpn) ) )
        if( dblHpn["hpn"] >=hpnMin.dbl ){
            matCnt <- sum( szInfo$dblHpn==mtx  ,na.rm=T )
            dblHpn["mat"] <- matCnt==dblHpn["hpn"]
        }

        return( list(ph=mtxPh,fCol=mtxFCol,dblHpn=dblHpn) )
    }

    matRaw <- chkMatch( szObj$raw ,scMtx )
    matEvt <- chkMatch( szObj$eVal ,scMtxEvt )

    return( list(matRaw=matRaw,matEvt=matEvt) )
}


# Cutter =================================================================================

FCust_stdCut.rawRow <- function( hName ,mName ,pName ,scoreMtxH ){

    rObj <- list( defId=c(hName=hName,mName=mName,pName=pName) )

    hLen <- nrow(scoreMtxH)
    rObj$lastScore <- scoreMtxH[hLen,]
    rObj$available <- TRUE

    if( rObj$available ){
        cfg <- scoreMtxCfg[[mName]]
        if( !is.null(cfg) ){
            rObj$isHard <- if( is.null(cfg$isHard) ) bFCust.defaultHardFlag else cfg$isHard

            rObj$lastEvt <- bFCust.getEvt( rObj$lastScore ,cfg$fCol )
            if( hLen>1 ){
                evt2 <- bFCust.getEvt( scoreMtxH[hLen-1 ,] ,cfg$fCol )
                rObj$evtReb <- bFCust.evtComp( evt2["lev",] ,rObj$lastEvt["lev",] ) # ,levMin=2
                if( all(is.na(rObj$evtReb$levDup)) )    rObj$evtReb <- NULL
            }

        } else {
            rObj$available <- FALSE
        }   # cfg

    }


    rObj$cut <- function( scoreMtx ,alreadyDead=NULL ,anaMode=TRUE ){

        val.len <- nrow(scoreMtx)
        if( is.null(alreadyDead) )	alreadyDead <- rep( F, val.len )

        cutLst <- list()
        if( !rObj$available ) return( list(cutLst=cutLst,surFlag=!alreadyDead) )

        hardFlag <- rObj$isHard( rObj$defId["pName"] )$flag["p"]
        cfg <- scoreMtxCfg[[ rObj$defId["mName"] ]]

        # each fCol --------------------------------------------
        cutLst.fCol <- list()
        for( fcName in names(cfg$fCol) ){
            for( aIdx in seq_len(val.len) ){
                if( !anaMode && alreadyDead[aIdx] ) next

                surWin <- cfg$fCol[[fcName]]$rng[ ,ifelse(hardFlag,"lev1","lev2")]
                val <- scoreMtx[aIdx,fcName]
                if( !bUtil.in(val,surWin) ){
                    alreadyDead[aIdx] <- TRUE

                    infoStr <- sprintf("%s(%d)",fcName,val )
                    cObj <- cutLst.fCol[[as.character(aIdx)]]
                    if( is.null(cObj) ){
                        cutLst.fCol[[as.character(aIdx)]] <- list( idx=aIdx ,info=infoStr )
                    } else {
                        cutLst.fCol[[as.character(aIdx)]]$info <- paste( cObj$info, infoStr, collapse=", " )
                    }

                    # infoStr <- sprintf("val:%d",val )
                    # idObjDesc <- c( typ="rawFCol" ,rObj$defId ,fcName=fcName )
                    # cutLst[[1+length(cutLst)]] <- list( idx=aIdx ,idObjDesc=idObjDesc ,info=infoStr )
                }

            }

        }

        # sm row: evtCnt  --------------------------------------------
        cutLst.rowE <- list()
        for( aIdx in seq_len(val.len) ){
            if( !anaMode && alreadyDead[aIdx] ) next

            evt.sm <- bFCust.getEvt( scoreMtx[aIdx ,] ,cfg$fCol )
            evtMin <- cfg$evtMax[ifelse(hardFlag,"lev1","lev2"),]
            evtCnt <- sum( evt.sm["lev" ,]>=evtMin["minLev"] ,na.rm=T )
            evtCntH <- sum( evt.sm["lev" ,]>=evtMin["minLevH"] ,na.rm=T )
            if( evtCnt>evtMin["maxHpn"] || evtCntH>evtMin["maxHpnH"] ){
                alreadyDead[aIdx] <- TRUE

                infoStr=""
                if( anaMode ){
                    flag <- evt.sm["lev",]>=evtMin["minLev"]
                    flag[is.na(flag)] <- F

                    infoStr <- sprintf("evtCnt:%d(%s)",evtCnt,paste(evt.sm["lev" ,flag],collapse=","))
                }
                cutLst.rowE[[as.character(aIdx)]] <- list( idx=aIdx ,info=infoStr )
            }
        }

        # sm row: rebound --------------------------------------------
        cutLst.reb <- list()
        checkRawReb.cnt <- sum(rObj$lastScore>0)
        checkRawReb.flag <- checkRawReb.cnt >= cfg$rowReb["rawMin"]
        checkEvtReb.flag <- FALSE
        if( 0<sum(rObj$lastEvt["lev"],na.rm=T) ){
            checkEvtReb.cnt <- c( lowE=sum(rObj$lastEvt["lev",]>0,na.rm=T) ,rareE=sum(rObj$lastEvt["lev",]>1,na.rm=T) )
            checkEvtReb.flag <- any( checkEvtReb.cnt >= cfg$rowReb[c("lowE","rareE")] )
            checkEvtReb.str <- paste(names(checkEvtReb.cnt),checkEvtReb.cnt,sep=".")
            checkEvtReb.str <- paste( checkEvtReb.str ,collapse="," )
        }
        checkEvtReb.Dbl.flag <- !is.null(rObj$evtReb)
        if( checkEvtReb.Dbl.flag ){
            levDup <- rObj$evtReb$levDup[!is.na(rObj$evtReb$levDup)]
            # str <- paste( names(levDup) ,levDup ,sep=":")
            # checkEvtReb.Dbl.str <- paste( str ,collapse=", ")
            checkEvtReb.Dbl.levDup <- levDup
        }
        for( aIdx in seq_len(val.len) ){
            smRow <- scoreMtx[aIdx ,]
            evt.sm <- bFCust.getEvt(smRow,cfg$fCol)

            # raw Reb
            if( checkRawReb.flag ){
                if( !anaMode && alreadyDead[aIdx] ) next

                if( all(rObj$lastScore==smRow) ){
                    alreadyDead[aIdx] <- TRUE

                    infoStr <- sprintf("rReb(last:%d)",checkRawReb.cnt )
                    cutLst.reb[[as.character(aIdx)]] <- list( idx=aIdx ,info=infoStr )
                }
            }

            # evt Reb
            if( checkEvtReb.flag ){
                if( !anaMode && alreadyDead[aIdx] ) next

                evtComp <- bFCust.evtComp( evt.sm["lev",] ,rObj$lastEvt["lev",] )
                if( 1<sum(!is.na(evtComp$levDup)) ){    # evtComp$allMat으로 해야 하려나?
                    alreadyDead[aIdx] <- TRUE

                    infoStr <- sprintf("rebE(last:%s)",checkEvtReb.str)
                    cObj <- cutLst.reb[[as.character(aIdx)]]
                    if( is.null(cObj) ){
                        cutLst.reb[[as.character(aIdx)]] <- list( idx=aIdx ,info=infoStr )
                    } else {
                        cutLst.reb[[as.character(aIdx)]]$info <- paste( cObj$info, infoStr, collapse=", " )
                    }
                }
            }

            # evt Reb Dup
            if( checkEvtReb.Dbl.flag && (sum(evt.sm["lev",]>0,na.rm=T)>0) ){
                if( !anaMode && alreadyDead[aIdx] ) next

                levDupName <- names(checkEvtReb.Dbl.levDup)
                evtComp <- bFCust.evtComp( evt.sm["lev",levDupName] ,rObj$lastEvt["lev",levDupName] )
                names(evtComp$levDup) <- levDupName # 값이 1개일때는 컬럼명이 따라오지 않아서..

                # cfg$rowReb.dup <- c( lowE=1 ,rareE=1 )

                eRebDup.cnt <- c( lowE=sum(evtComp$levDup>0,na.rm=T) ,rareE=sum(evtComp$levDup>1,na.rm=T) )
                eRebDup.flag <- any( eRebDup.cnt >= cfg$rowRebDup[c("lowE","rareE")] )
                if( eRebDup.flag ){ 
                    alreadyDead[aIdx] <- TRUE

                    levDup <- evtComp$levDup[!is.na(evtComp$levDup)]
                    str <- paste( paste( levDupName[!is.na(evtComp$levDup)] ,levDup ,sep=":" ) ,collapse=", " )
                    infoStr <- sprintf("rebE.dup(last:%s)",str)
                    cObj <- cutLst.reb[[as.character(aIdx)]]
                    if( is.null(cObj) ){
                        cutLst.reb[[as.character(aIdx)]] <- list( idx=aIdx ,info=infoStr )
                    } else {
                        cutLst.reb[[as.character(aIdx)]]$info <- paste( cObj$info, infoStr, collapse=", " )
                    }

                }

                # dupFlag <- evt.sm["lev",names(checkEvtReb.Dbl.levDup)] == checkEvtReb.Dbl.levDup
                # if( 1<=sum(checkEvtReb.Dbl.levDup[dupFlag],na.rm=T) ){ 
                #     # 3연속 발생한 Evt의 총 합을... 2 이상으로 할까?
                #     alreadyDead[aIdx] <- TRUE

                #     infoStr <- sprintf("rebE.dup(last:%s)",checkEvtReb.Dbl.str)
                #     cObj <- cutLst.reb[[as.character(aIdx)]]
                #     if( is.null(cObj) ){
                #         cutLst.reb[[as.character(aIdx)]] <- list( idx=aIdx ,info=infoStr )
                #     } else {
                #         cutLst.reb[[as.character(aIdx)]]$info <- paste( cObj$info, infoStr, collapse=", " )
                #     }
                # }


            }

        }

        if( anaMode ){  # build cutLst 
            idxFCol <- if( length(cutLst.fCol)==0 ) integer(0) else sapply( cutLst.fCol  ,function(p){p$idx} )
            idxReb  <- if( length(cutLst.reb)==0 ) integer(0) else sapply( cutLst.reb   ,function(p){p$idx} )
            idxRowE <- if( length(cutLst.rowE)==0 ) integer(0) else sapply( cutLst.rowE  ,function(p){p$idx} )

            idxAll <- union(idxFCol,idxReb)
            idxAll <- sort(union(idxAll,idxRowE))

            cutLst <- list()
            names(cutLst.fCol)  <- idxFCol
            names(cutLst.reb)   <- idxReb
            names(idxRowE)      <- idxRowE

            for( aIdx in idxAll ){
                idStr <- as.character(aIdx)

                cLst <- list()
                if( !is.null(cutLst.fCol[[idStr]]) ){
                    cLst[["rawFCol"]] <- cutLst.fCol[[idStr]]
                    cLst[["rawFCol"]]$idObjDesc <- c( typ="rawFCol" ,rObj$defId )
                }
                if( !is.null(cutLst.reb[[idStr]]) ){
                    cLst[["rawReb"]] <- cutLst.reb[[idStr]]
                    cLst[["rawReb"]]$idObjDesc <- c( typ="rawReb" ,rObj$defId )
                }
                if( !is.null(cutLst.rowE[[idStr]]) ){
                    cLst[["rowE"]] <- cutLst.rowE[[idStr]]
                    cLst[["rowE"]]$idObjDesc <- c( typ="rowE" ,rObj$defId )
                }

                cutLst[[idStr]] <- list( idx=aIdx ,cLst=cLst )
            }
        }

        return( list(cutLst=cutLst,surFlag=!alreadyDead) )
    }

    return( rObj )

}


FCust_stdCutExt.rawRow <- function( hName ,mName ,pName ,scoreMtxH ,fltName ){
    #   scoreMtxH <- hMtxLst$scoreMtxLst[[hName]][[pName]][[mName]]$scoreMtx

    rObj <- list( defId=c(hName=hName,mName=mName,pName=pName,fltName=fltName) )

    extFilter <- bFMtxExtFltLst[[mName]][[fltName]]
    scrExtMtxH <- extFilter$getScoreMtx( scoreMtxH )

    hLen <- nrow(scrExtMtxH)
    rObj$lastScore <- scrExtMtxH[hLen,]
    rObj$available <- TRUE

    if( rObj$available ){

        cfg <- scrExtMtxCfg[[mName]][[fltName]]
        if( !is.null(cfg) ){
            rObj$isHard <- if( is.null(cfg$isHard) ) bFCust.defaultHardFlag else cfg$isHard

            rObj$lastEvt <- bFCust.getEvt( rObj$lastScore ,cfg$fCol )
            if( hLen>1 ){
                evt2 <- bFCust.getEvt( scrExtMtxH[hLen-1 ,] ,cfg$fCol )
                rObj$evtReb <- bFCust.evtComp( evt2["lev",] ,rObj$lastEvt["lev",] ) # ,levMin=2
                if( all(is.na(rObj$evtReb$levDup)) )    rObj$evtReb <- NULL
            }

        } else {
            rObj$available <- FALSE
        }   # cfg

    }


    rObj$cut <- function( scoreMtx ,alreadyDead=NULL ,anaMode=TRUE ){

        val.len <- nrow(scoreMtx)
        if( is.null(alreadyDead) )	alreadyDead <- rep( F, val.len )

        cutLst <- list()
        if( !rObj$available ) return( list(cutLst=cutLst,surFlag=!alreadyDead) )

        hardFlag <- rObj$isHard( rObj$defId["pName"] )$flag["p"]

        extFilter <- bFMtxExtFltLst[[ rObj$defId["mName"] ]][[ rObj$defId["fltName"] ]]
        scrExtMtx <- extFilter$getScoreMtx( scoreMtx )
        cfg <- scrExtMtxCfg[[ rObj$defId["mName"] ]][[ rObj$defId["fltName"] ]]

        # each fCol --------------------------------------------
        cutLst.fCol <- list()
        for( fcName in names(cfg$fCol) ){
            for( aIdx in seq_len(val.len) ){
                if( !anaMode && alreadyDead[aIdx] ) next

                surWin <- cfg$fCol[[fcName]]$rng[ ,ifelse(hardFlag,"lev1","lev2")]
                val <- scrExtMtx[aIdx,fcName]
                if( !bUtil.in(val,surWin) ){
                    alreadyDead[aIdx] <- TRUE

                    infoStr <- sprintf("%s(%d)",fcName,val )
                    cObj <- cutLst.fCol[[as.character(aIdx)]]
                    if( is.null(cObj) ){
                        cutLst.fCol[[as.character(aIdx)]] <- list( idx=aIdx ,info=infoStr )
                    } else {
                        cutLst.fCol[[as.character(aIdx)]]$info <- paste( cObj$info, infoStr, collapse=", " )
                    }

                    # infoStr <- sprintf("val:%d",val )
                    # idObjDesc <- c( typ="rawFCol" ,rObj$defId ,fcName=fcName )
                    # cutLst[[1+length(cutLst)]] <- list( idx=aIdx ,idObjDesc=idObjDesc ,info=infoStr )
                }

            }

        }

        # sm row: evtCnt  --------------------------------------------
        cutLst.rowE <- list()
        for( aIdx in seq_len(val.len) ){
            if( !anaMode && alreadyDead[aIdx] ) next

            evt.sm <- bFCust.getEvt( scrExtMtx[aIdx ,] ,cfg$fCol )
            evtMin <- cfg$evtMax[ifelse(hardFlag,"lev1","lev2"),]
            evtCnt <- sum( evt.sm["lev" ,]>=evtMin["minLev"] ,na.rm=T )
            evtCntH <- sum( evt.sm["lev" ,]>=evtMin["minLevH"] ,na.rm=T )
            if( evtCnt>evtMin["maxHpn"] || evtCntH>evtMin["maxHpnH"] ){
                alreadyDead[aIdx] <- TRUE

                infoStr=""
                if( anaMode ){
                    flag <- evt.sm["lev",]>=evtMin["minLev"]
                    flag[is.na(flag)] <- F

                    infoStr <- sprintf("evtCnt:%d(%s)",evtCnt,paste(evt.sm["lev" ,flag],collapse=","))
                }
                cutLst.rowE[[as.character(aIdx)]] <- list( idx=aIdx ,info=infoStr )
            }
        }

        # sm row: rebound --------------------------------------------
        cutLst.reb <- list()
        checkRawReb.cnt <- sum(rObj$lastScore>0)
        checkRawReb.flag <- checkRawReb.cnt >= cfg$rowReb["rawMin"]
        checkEvtReb.flag <- FALSE
        if( 0<sum(rObj$lastEvt["lev"],na.rm=T) ){
            checkEvtReb.cnt <- c( lowE=sum(rObj$lastEvt["lev",]>0,na.rm=T) ,rareE=sum(rObj$lastEvt["lev",]>1,na.rm=T) )
            checkEvtReb.flag <- any( checkEvtReb.cnt >= cfg$rowReb[c("lowE","rareE")] )
            checkEvtReb.str <- paste(names(checkEvtReb.cnt),checkEvtReb.cnt,sep=".")
            checkEvtReb.str <- paste( checkEvtReb.str ,collapse="," )
        }
        checkEvtReb.Dbl.flag <- !is.null(rObj$evtReb)
        if( checkEvtReb.Dbl.flag ){
            levDup <- rObj$evtReb$levDup[!is.na(rObj$evtReb$levDup)]
            # str <- paste( names(levDup) ,levDup ,sep=":")
            # checkEvtReb.Dbl.str <- paste( str ,collapse=", ")
            checkEvtReb.Dbl.levDup <- levDup
        }
        for( aIdx in seq_len(val.len) ){
            smRow <- scrExtMtx[aIdx ,]
            evt.sm <- bFCust.getEvt(smRow,cfg$fCol)

            # raw Reb
            if( checkRawReb.flag ){
                if( !anaMode && alreadyDead[aIdx] ) next

                if( all(rObj$lastScore==smRow) ){
                    alreadyDead[aIdx] <- TRUE

                    infoStr <- sprintf("rReb(last:%d)",checkRawReb.cnt )
                    cutLst.reb[[as.character(aIdx)]] <- list( idx=aIdx ,info=infoStr )
                }
            }

            # evt Reb
            if( checkEvtReb.flag ){
                if( !anaMode && alreadyDead[aIdx] ) next

                evtComp <- bFCust.evtComp( evt.sm["lev",] ,rObj$lastEvt["lev",] )
                if( 1<sum(!is.na(evtComp$levDup)) ){    # evtComp$allMat으로 해야 하려나?
                    alreadyDead[aIdx] <- TRUE

                    infoStr <- sprintf("rebE(last:%s)",checkEvtReb.str)
                    cObj <- cutLst.reb[[as.character(aIdx)]]
                    if( is.null(cObj) ){
                        cutLst.reb[[as.character(aIdx)]] <- list( idx=aIdx ,info=infoStr )
                    } else {
                        cutLst.reb[[as.character(aIdx)]]$info <- paste( cObj$info, infoStr, collapse=", " )
                    }
                }
            }

            # evt Reb Dup
            if( checkEvtReb.Dbl.flag && (sum(evt.sm["lev",]>0,na.rm=T)>0) ){
                if( !anaMode && alreadyDead[aIdx] ) next

                levDupName <- names(checkEvtReb.Dbl.levDup)
                evtComp <- bFCust.evtComp( evt.sm["lev",levDupName] ,rObj$lastEvt["lev",levDupName] )
                names(evtComp$levDup) <- levDupName # 값이 1개일때는 컬럼명이 따라오지 않아서..

                # cfg$rowReb.dup <- c( lowE=1 ,rareE=1 )

                eRebDup.cnt <- c( lowE=sum(evtComp$levDup>0,na.rm=T) ,rareE=sum(evtComp$levDup>1,na.rm=T) )
                eRebDup.flag <- any( eRebDup.cnt >= cfg$rowRebDup[c("lowE","rareE")] )
                if( eRebDup.flag ){ 
                    alreadyDead[aIdx] <- TRUE

                    levDup <- evtComp$levDup[!is.na(evtComp$levDup)]
                    str <- paste( paste( levDupName[!is.na(evtComp$levDup)] ,levDup ,sep=":" ) ,collapse=", " )
                    infoStr <- sprintf("rebE.dup(last:%s)",str)
                    cObj <- cutLst.reb[[as.character(aIdx)]]
                    if( is.null(cObj) ){
                        cutLst.reb[[as.character(aIdx)]] <- list( idx=aIdx ,info=infoStr )
                    } else {
                        cutLst.reb[[as.character(aIdx)]]$info <- paste( cObj$info, infoStr, collapse=", " )
                    }

                }

            }

        }

        if( anaMode ){  # build cutLst 
            idxFCol <- if( length(cutLst.fCol)==0 ) integer(0) else sapply( cutLst.fCol  ,function(p){p$idx} )
            idxReb  <- if( length(cutLst.reb)==0 ) integer(0) else sapply( cutLst.reb   ,function(p){p$idx} )
            idxRowE <- if( length(cutLst.rowE)==0 ) integer(0) else sapply( cutLst.rowE  ,function(p){p$idx} )

            idxAll <- union(idxFCol,idxReb)
            idxAll <- sort(union(idxAll,idxRowE))

            cutLst <- list()
            names(cutLst.fCol)  <- idxFCol
            names(cutLst.reb)   <- idxReb
            names(idxRowE)      <- idxRowE

            for( aIdx in idxAll ){
                idStr <- as.character(aIdx)

                cLst <- list()
                if( !is.null(cutLst.fCol[[idStr]]) ){
                    cLst[["rawFCol"]] <- cutLst.fCol[[idStr]]
                    cLst[["rawFCol"]]$idObjDesc <- c( typ="rawFCol" ,rObj$defId )
                }
                if( !is.null(cutLst.reb[[idStr]]) ){
                    cLst[["rawReb"]] <- cutLst.reb[[idStr]]
                    cLst[["rawReb"]]$idObjDesc <- c( typ="rawReb" ,rObj$defId )
                }
                if( !is.null(cutLst.rowE[[idStr]]) ){
                    cLst[["rowE"]] <- cutLst.rowE[[idStr]]
                    cLst[["rowE"]]$idObjDesc <- c( typ="rowE" ,rObj$defId )
                }

                cutLst[[idStr]] <- list( idx=aIdx ,cLst=cLst )
            }
        }

        return( list(cutLst=cutLst,surFlag=!alreadyDead) )
    }

    return( rObj )

}





FCust_stdCut.hIdx <- function( hName ,mName ,mtxLst ){

    rObj <- list( defId=c(hName=hName,mName=mName) )

    hLen <- length(mtxLst)
    rObj$lastMtx <- mtxLst[[hLen]]
    rObj$available <- TRUE

    if( rObj$available ){
        cfg <- scoreMtxCfg[[mName]]
        if( !is.null(cfg) ){

            # standard Event check
            stdEvt.H2 <- NULL
            if( 1<length(mtxLst) ){
            	stdEvt.H2 <- bFCust.getEvt_byHIdx( mtxLst[[length(mtxLst)-1]] ,cfg ,NULL )
            }
            rObj$stdEvt.H1 <- bFCust.getEvt_byHIdx( mtxLst[[length(mtxLst)]] ,cfg ,lastEvt=stdEvt.H2 )

            # rebound check(skip zero)
            # bFCust.getSkipZero_byHIdx.ass
            szObj.H2 <- NULL
            if( 1<length(mtxLst) ){
                mtxLst.H2 <- mtxLst[1:(length(mtxLst)-1)]
                szObj.H2 <- bFCust.getSkipZero_byHIdx( mtxLst.H2 ,cfg )
            }
            rObj$szObj <- bFCust.getSkipZero_byHIdx( mtxLst ,cfg ,lastSZ=szObj.H2 )

        } else {
            rObj$available <- FALSE
        }   # cfg

    }

    rObj$getRawScore <- function( rawMtx ){

        if( !rObj$available ) return( NULL )

        cfg <- scoreMtxCfg[[ rObj$defId["mName"] ]]
        evtObj <- bFCust.getEvtMtx( rawMtx ,cfg )

        curEvt <- bFCust.getEvt_byHIdx( rawMtx ,cfg ,lastEvt=rObj$stdEvt.H1 )
        rebInfo <- bFCust.getSkipZero_byHIdx.ass( rObj$szObj ,rawMtx ,evtObj$eValMtx )

        return( list( cfg=cfg ,evtObj=evtObj ,curEvt=curEvt ,rebInfo=rebInfo ) )
    }

    rObj$getRaw4Ass <- function( rawObj ){
        #   mName 단위가 아닌, 전체 mName 범위로 평가하기 위한 데이터 추출.
        #   bUtil.getCut1Score( ) 함수 참고.

        r4Ass <- list()

        phaseHpnCnt <- rbind( rObj$stdEvt.H1$hpnInfo$phase ,rObj$stdEvt.H1$evtInfo$phase )
        rownames( phaseHpnCnt ) <- c("raw","evt")

        phaseRebCnt <- rbind( rObj$stdEvt.H1$hpnInfo$phaseReb["reb",] ,rObj$stdEvt.H1$evtInfo$phaseReb["reb",] )
        rownames( phaseRebCnt ) <- c("raw","evt")

        r4Ass$phaseHpnCnt <- phaseHpnCnt
        r4Ass$phaseRebCnt <- phaseRebCnt

        r4Ass$rebMtx.ph <- rawObj$curEvt$rebInfo$rebMtx.ph

        return( r4Ass )

    }

    rObj$getSummScore <- function( rawObj ){
        scoreObj <- list( )

        # cfg <- scoreMtxCfg[[ rObj$defId["mName"] ]]
        # evtObj <- bFCust.getEvtMtx( rawMtx ,cfg )
        # rawObj <- rObj$getRawScore( rawMtx )
        if( is.null(rawObj) ){
            rName <- c("raw","evt")
			cName <- c("all","ph","fCol","phReb","xyCnt.fCol","xyCnt.phase")
			summMtx <- matrix( 0 ,nrow=length(rName) ,ncol=length(cName) )
			rownames(summMtx) <- rName	;colnames(summMtx) <- cName

            scoreObj$summMtx        <- summMtx
            scoreObj$summMtx.reb    <- summMtx # 내부 구조는 같다.

            cName <- c("r.ph","r.fCol","r.dblHpnFlg" ,"e.ph","e.fCol","e.dblHpnFlg")
            rName <- c("rebCnt","rebDup")   # 반복 수, H1에서의 재현이 반복되었는지? ,발생 수
            scMtx.sz <- matrix( 0 ,ncol=length(cName) ,nrow=length(rName) ,dimnames=list(rName,cName) )
            scoreObj$scMtx.sz
            return( scoreObj )
        }
        cfg     <- rawObj$cfg
        evtObj  <- rawObj$evtObj
        curEvt  <- rawObj$curEvt
        rebInfo <- rawObj$rebInfo

        # 일단 scoreMtx들부터 만들고, forCut은 나중에 적용하자.

        #   summMtx,summMtx.reb / stdEvt.H1 --------------------------------------------------------
        # curEvt <- bFCust.getEvt_byHIdx( rawMtx ,cfg ,lastEvt=rObj$stdEvt.H1 )
        scoreObj$summMtx <- curEvt$rebInfo$summMtx
        scoreObj$summMtx.reb <- NULL
        if( !is.null(rObj$stdEvt.H1$rebInfo) ){
            summMtx.reb <- scoreObj$summMtx
            summMtx.reb[,] <- 0     # 하나라도 0 이 아니면 Cut...

            rebInfo.H1 <- rObj$stdEvt.H1$rebInfo

            summMtx.reb["raw","all"] <- scoreObj$summMtx["raw","all"]>0 && curEvt$rebInfo$summMtx["raw","all"]>0
            summMtx.reb["evt","all"] <- scoreObj$summMtx["evt","all"]>0 && curEvt$rebInfo$summMtx["evt","all"]>0

            summMtx.reb["raw","ph"] <- sum( rebInfo.H1$rebMtx.ph["rebFlag.raw",]>0 & curEvt$rebInfo$rebMtx.ph["rebFlag.raw",]>0 )
            summMtx.reb["evt","ph"] <- sum( rebInfo.H1$rebMtx.ph["rebFlag.evt",]>0 & curEvt$rebInfo$rebMtx.ph["rebFlag.evt",]>0 )

            summMtx.reb["raw","fCol"] <- sum( rebInfo.H1$rebMtx.fCol["rebFlag.raw",]>0 & curEvt$rebInfo$rebMtx.fCol["rebFlag.raw",]>0 )
            summMtx.reb["evt","fCol"] <- sum( rebInfo.H1$rebMtx.fCol["rebFlag.evt",]>0 & curEvt$rebInfo$rebMtx.fCol["rebFlag.evt",]>0 )

            summMtx.reb["raw","phReb"] <- sum( rebInfo.H1$rebMtx.phReb["raw",]>0 & curEvt$rebInfo$rebMtx.phReb["raw",]>0 )
            summMtx.reb["evt","phReb"] <- sum( rebInfo.H1$rebMtx.phReb["evt",]>0 & curEvt$rebInfo$rebMtx.phReb["evt",]>0 )

            summMtx.reb["raw","xyCnt.fCol"] <- rebInfo.H1$rebMtx.xyCnt["raw","fCol.allMat"]>0 && curEvt$rebInfo$rebMtx.xyCnt["raw","fCol.allMat"]
            summMtx.reb["evt","xyCnt.fCol"] <- rebInfo.H1$rebMtx.xyCnt["evt","fCol.allMat"]>0 && curEvt$rebInfo$rebMtx.xyCnt["evt","fCol.allMat"]

            summMtx.reb["raw","xyCnt.phase"] <- rebInfo.H1$rebMtx.xyCnt["raw","phase.allMat"]>0 && curEvt$rebInfo$rebMtx.xyCnt["raw","phase.allMat"]
            summMtx.reb["evt","xyCnt.phase"] <- rebInfo.H1$rebMtx.xyCnt["evt","phase.allMat"]>0 && curEvt$rebInfo$rebMtx.xyCnt["evt","phase.allMat"]


            scoreObj$summMtx.reb <- summMtx.reb
        }

        #   scMtx.sz / szObj ------------------------------------------------------------
        # rebInfo <- bFCust.getSkipZero_byHIdx.ass( rObj$szObj ,rawMtx ,evtObj$eValMtx )
        cName <- c("r.ph","r.fCol","r.dblHpnFlg" ,"e.ph","e.fCol","e.dblHpnFlg")
        rName <- c("rebCnt","rebDup")   # 반복 수, H1에서의 재현이 반복되었는지? ,발생 수
        scMtx.sz <- matrix( 0 ,ncol=length(cName) ,nrow=length(rName) ,dimnames=list(rName,cName) )
        if( TRUE ){
            scMtx.sz["rebCnt","r.ph"] <- sum(rebInfo$matRaw$ph["mat",])
            scMtx.sz["rebCnt","r.fCol"] <- sum(rebInfo$matRaw$fCol["mat",])
            scMtx.sz["rebCnt","r.dblHpnFlg"] <- rebInfo$matRaw$dblHpn["mat"]
            scMtx.sz["rebCnt","e.ph"] <- sum(rebInfo$matEvt$ph["mat",])
            scMtx.sz["rebCnt","e.fCol"] <- sum(rebInfo$matEvt$fCol["mat",])
            scMtx.sz["rebCnt","e.dblHpnFlg"] <- rebInfo$matEvt$dblHpn["mat"]

            if( !is.null(rObj$szObj$rebInfo) ){
                matFlag <- (rObj$szObj$rebInfo$matRaw$ph["mat",]>0) & (rebInfo$matRaw$ph["mat",]>0)
                scMtx.sz["rebDup","r.ph"] <- sum( matFlag )
                matFlag <- (rObj$szObj$rebInfo$matRaw$fCol["mat",]>0) & (rebInfo$matRaw$fCol["mat",]>0)
                scMtx.sz["rebDup","r.fCol"] <- sum( matFlag )
                scMtx.sz["rebDup","r.dblHpnFlg"] <- (rObj$szObj$rebInfo$matRaw$dblHpn["mat"]>0) && (rebInfo$matRaw$dblHpn["mat"]>0)

                matFlag <- (rObj$szObj$rebInfo$matEvt$ph["mat",]>0) & (rebInfo$matEvt$ph["mat",]>0)
                scMtx.sz["rebDup","e.ph"] <- sum( matFlag )
                matFlag <- (rObj$szObj$rebInfo$matEvt$fCol["mat",]>0) & (rebInfo$matEvt$fCol["mat",]>0)
                scMtx.sz["rebDup","e.fCol"] <- sum( matFlag )
                scMtx.sz["rebDup","e.dblHpnFlg"] <- (rObj$szObj$rebInfo$matEvt$dblHpn["mat"]>0) && (rebInfo$matEvt$dblHpn["mat"]>0)
            }            
        }
        scoreObj$scMtx.sz <- scMtx.sz

        return( scoreObj )
    }

    rObj$cut <- function( scoreMtx ,anaMode=TRUE ){   # 하나씩 오므로, alreadyDead 처리.

        cLst <- list( )     #   cutLst[[1]]$cLst
                            #   만약 생존했으면 cLst의 길이는 0
        if( !rObj$available ) return( cLst )

        rawObj <- rObj$getRawScore( scoreMtx )
        scObj <- rObj$getSummScore( rawObj )
        # scObj <- rObj$getSummScore( scoreMtx )
        cfg <- scoreMtxCfg[[ rObj$defId["mName"] ]]

        survive <- TRUE

        if( survive ){ #   summMtx.cut
            infoStr <- ""
            #     $summMtx    all ph fCol phReb xyCnt.fCol xyCnt.phase
            #             raw   0  0    0     0          0           0
            #             evt   0  0    0     0          0           0
            summMtx.cut <- scObj$summMtx >= cfg$summMtx
            if( any(summMtx.cut["raw",]) ){
                survive <- F
                cLst[["summMtx.cut raw"]] <- "summMtx.cut raw"
                if( anaMode ){  # infoStr
                    flag <- summMtx.cut["raw",]
                    str <- paste( names(scObj$summMtx["raw",])[flag] ,scObj$summMtx["raw",flag] ,sep=":" )
                    infoStr <- sprintf("summMtx.cut - raw %s",paste(str,collapse=", ") )
                    cLst[["summMtx.cut raw"]] <- infoStr
                }
            }
            if( any(summMtx.cut["evt",]) ){
                survive <- F
                cLst[["summMtx.cut evt"]] <- "summMtx.cut evt"
                if( anaMode ){  # infoStr
                    flag <- summMtx.cut["evt",]
                    str <- paste( names(scObj$summMtx["evt",])[flag] ,scObj$summMtx["evt",flag] ,sep=":" )
                    infoStr <- sprintf("summMtx.cut - evt %s",paste(str,collapse=", ") )
                    cLst[["summMtx.cut evt"]] <- infoStr
                }
            }
            if( cfg$summMtx.sum["raw"]<=sum(scObj$summMtx["raw",]) ){
                survive <- F
                cLst[["summMtx.cut raw.sum"]] <- "summMtx.cut raw.sum"
                if( anaMode ){  # infoStr
                    flag <- scObj$summMtx["raw",]>0
                    str <- paste( names(scObj$summMtx["raw",])[flag] ,scObj$summMtx["raw",flag] ,sep=":" )
                    infoStr <- sprintf("summMtx.cut - raw.sum %s",paste(str,collapse=", ") )
                    cLst[["summMtx.cut raw.sum"]] <- infoStr
                }
            }
            if( cfg$summMtx.sum["evt"]<=sum(scObj$summMtx["evt",]) ){
                survive <- F
                cLst[["summMtx.cut evt.sum"]] <- "summMtx.cut evt.sum"
                if( anaMode ){  # infoStr
                    flag <- scObj$summMtx["evt",]>0
                    str <- paste( names(scObj$summMtx["evt",])[flag] ,scObj$summMtx["evt",flag] ,sep=":" )
                    infoStr <- sprintf("summMtx.cut - evt.sum %s",paste(str,collapse=", ") )
                    cLst[["summMtx.cut evt.sum"]] <- infoStr
                }
            }
        }

        if( survive ){ #   summMtx.reb.cut
            #     $summMtx.reb  all ph fCol phReb xyCnt.fCol xyCnt.phase
            #               raw   0  0    0     0          0           0
            #               evt   0  0    0     0          0           0
            summMtx.reb.cut <- scObj$summMtx.reb >= cfg$summMtx.reb
            if( any(summMtx.reb.cut["raw",]) ){
                survive <- F
                cLst[["summMtx.reb.cut raw"]] <- "summMtx.reb.cut raw"
                if( anaMode ){  # infoStr
                    flag <- summMtx.reb.cut["raw",]
                    str <- paste( names(scObj$summMtx.reb["raw",])[flag] ,scObj$summMtx.reb["raw",flag] ,sep=":" )
                    infoStr <- sprintf("summMtx.reb.cut - raw %s",paste(str,collapse=", ") )
                    cLst[["summMtx.reb.cut raw"]] <- infoStr
                }
            }
            if( any(summMtx.reb.cut["evt",]) ){
                survive <- F
                cLst[["summMtx.reb.cut evt"]] <- "summMtx.reb.cut evt"
                if( anaMode ){  # infoStr
                    flag <- summMtx.reb.cut["evt",]
                    str <- paste( names(scObj$summMtx.reb["evt",])[flag] ,scObj$summMtx.reb["evt",flag] ,sep=":" )
                    infoStr <- sprintf("summMtx.reb.cut - evt %s",paste(str,collapse=", ") )
                    cLst[["summMtx.reb.cut evt"]] <- infoStr
                }
            }
            # sum 체크를 해야 할 일은 없을 듯 하다.
        }

        if( survive ){ #   scMtx.sz.cut
            #     $scMtx.sz   r.ph r.fCol r.dblHpnFlg e.ph e.fCol e.dblHpnFlg
            #             rebCnt    0      0           0    0      0           0
            #             rebDup    0      0           0    0      0           0
            scMtx.sz.cut <- scObj$scMtx.sz >= cfg$scMtx.sz
            if( any(scMtx.sz.cut["rebCnt",]) ){ #   rebCnt
                survive <- F
                cLst[["scMtx.sz.cut rebCnt"]] <- "scMtx.sz.cut rebCnt"
                if( anaMode ){  # infoStr
                    flag <- scMtx.sz.cut["rebCnt",]
                    str <- paste( names(scObj$scMtx.sz["rebCnt",])[flag] ,scObj$scMtx.sz["rebCnt",flag] ,sep=":" )
                    infoStr <- sprintf("scMtx.sz.cut - rebCnt %s",paste(str,collapse=", ") )
                    cLst[["scMtx.sz.cut rebCnt"]] <- infoStr
                }
            }
            if( any(scMtx.sz.cut["rebDup",]) ){ #   rebDup
                survive <- F
                cLst[["scMtx.sz.cut rebDup"]] <- "scMtx.sz.cut rebDup"
                if( anaMode ){  # infoStr
                    flag <- scMtx.sz.cut["rebDup",]
                    str <- paste( names(scObj$scMtx.sz["rebDup",])[flag] ,scObj$scMtx.sz["rebDup",flag] ,sep=":" )
                    infoStr <- sprintf("scMtx.sz.cut - rebDup %s",paste(str,collapse=", ") )
                    cLst[["scMtx.sz.cut rebDup"]] <- infoStr
                }
            }

            sumCol <- c("r.ph","r.fCol","r.dblHpnFlg")
            if( cfg$scMtx.sz.sum["rebCnt.r"]<=sum(scObj$scMtx.sz["rebCnt",sumCol]) ){
                survive <- F
                cLst[["scMtx.sz.cut rebCnt.r.sum"]] <- "scMtx.sz.cut rebCnt.r.sum"
                if( anaMode ){  # infoStr
                    str <- paste( sumCol ,scObj$scMtx.sz["rebCnt",sumCol] ,sep=":" )
                    infoStr <- sprintf("scMtx.sz.cut - rebCnt.r.sum %s",paste(str,collapse=", ") )
                    cLst[["scMtx.sz.cut rebCnt.r.sum"]] <- infoStr
                }
            }
            sumCol <- c("e.ph","e.fCol","e.dblHpnFlg")
            if( cfg$scMtx.sz.sum["rebCnt.e"]<=sum(scObj$scMtx.sz["rebCnt",sumCol]) ){
                survive <- F
                cLst[["scMtx.sz.cut rebCnt.e.sum"]] <- "scMtx.sz.cut rebCnt.e.sum"
                if( anaMode ){  # infoStr
                    str <- paste( sumCol ,scObj$scMtx.sz["rebCnt",sumCol] ,sep=":" )
                    infoStr <- sprintf("scMtx.sz.cut - rebCnt.e.sum %s",paste(str,collapse=", ") )
                    cLst[["scMtx.sz.cut rebCnt.e.sum"]] <- infoStr
                }
            }

            # cfg$scMtx.sz.sum["rebDup",] 의 총합을 체크해야 할 일은 없을 듯.

        }

        if( TRUE ){ # bUtil.closeMax() QQE 차후 적용 필요.
            # bUtil.closeMax_Mtx <- function( scoreMtx ,windMtxMin=0 ,windMtxMax ,distVal=3 )
            windMtxMin <- cfg$scMtx.sz
            windMtxMin[,] <- 0
            cm_scMtx.sz <- bUtil.closeMax_Mtx( scObj$scMtx.sz ,windMtxMin=NULL ,windMtxMax=cfg$scMtx.sz )
                # close max for scObj$scMtx.sz by cfg$scMtx.sz
                #   예 시 (distVal=3 일때) : 
                #         scObj$scMtx.sz
                #                        r.ph r.fCol r.dblHpnFlg e.ph e.fCol e.dblHpnFlg
                #                 rebCnt    0      1           0    0      1           0
                #                 rebDup    0      1           0    0      1           0
                #         cfg$scMtx.sz
                #                        r.ph r.fCol r.dblHpnFlg e.ph e.fCol e.dblHpnFlg
                #                 rebCnt    2      2           1    2      2           1
                #                 rebDup    1      1           1    1      1           1
                #         cm_scMtx.sz
                #                        r.ph r.fCol r.dblHpnFlg e.ph e.fCol e.dblHpnFlg
                #                 rebCnt    0      2           0    0      2           0
                #                 rebDup    0      3           0    0      3           0
        }

        #   fCol 에서의 높은 등급 Evt 갯수 제한도 있어야 함.
        #    scoreMtxCfg[[mName]]$fCol[[fcName]]$evtMax.fCol <- c( minLev=2 ,maxHpn=2 )

        return( list( cLst=cLst ,scObj=scObj ) )
    }

    return( rObj )
}


bFCust.getFCustGrp <- function( hMtxLst ,tgt.scMtx=NULL ){

    rObj <- list(   sfcHLst = hMtxLst$sfcHLst
                    ,mtxInfoLst = hMtxLst$mtxInfoLst
					,mtxInfoLst.bScr = hMtxLst$mtxInfoLst.bScr
                    ,phaseName = hMtxLst$phaseName
    )

	# custObj <- bFCust.getCust()
    if( !is.null(tgt.scMtx) ){
        availMtx <- intersect( tgt.scMtx ,names(rObj$mtxInfoLst) )
        rObj$mtxInfoLst <- rObj$mtxInfoLst[availMtx]

        availMtx <- intersect( tgt.scMtx ,names(rObj$mtxInfoLst.bScr) )
        rObj$mtxInfoLst.bScr <- rObj$mtxInfoLst.bScr[availMtx]
    }

	cutterLst <- list()
	cutterLst.bScr <- list()
	for( hName in names(rObj$sfcHLst) ){	# hName <- names(rObj$sfcHLst)[1]

        mLst <- list()  #   cutterLst
        for( mName in names(rObj$mtxInfoLst) ){	# mName <- names(rObj$mtxInfoLst)[1]
            # <stdCut>
            stdCut <- list()
            for( pName in rObj$phaseName ){
                scoreMtxObj <- B.HMtxLst_getMtxLst( hMtxLst , hName ,mName ,pName )
                stdCut[[pName]] <- FCust_stdCut.rawRow( hName ,mName ,pName ,scoreMtxObj$scoreMtx )
            }

            # <fColCut>
            fColCut <- list()   # preserve

            # <hIdxCut>
			hIdxObj <- B.getHMtxLst_byHIdx( hMtxLst )
            hIdxCut <- FCust_stdCut.hIdx( hName ,mName ,mtxLst=hIdxObj[[hName]][[mName]] )

            mLst[[mName]] <- list( stdCut=stdCut ,fColCut=fColCut ,hIdxCut=hIdxCut )
        }
        cutterLst[[hName]] <- mLst

		mLst <- list()  #   cutterLst.bScr
		for( mName in names(rObj$mtxInfoLst.bScr) ){
            scoreMtxObj <- B.HMtxLst_getMtxLst( hMtxLst , hName ,mName ,pName=NULL ,tgt="mfMtxLst" )
            mLst[[mName]] <- FCust_stdCut.rawRow( hName ,mName ,pName="N/A" ,scoreMtxObj$scoreMtx )
        }
		cutterLst.bScr[[hName]] <- mLst

    }

    rObj$cutterLst <- cutterLst
    rObj$cutterLst.bScr <- cutterLst.bScr

    return( rObj )

}


bFCust.cut <- function( scoreMtx.grp ,cut.grp ,fHName ,tgt.scMtx=NULL ,anaOnly=F ,logger=NULL ){
    #   anaOnly=T : scoreMtx[1,] 만 분석하며, 그 대신 cutting 정보를 추가한다.
	#	logger <- k.getFlogObj( "./log/cutLog.txt" )

	reportStatus <- function( tStmp ,strWhere ,surFlag ,logger ){
		#	strWhere <- sprintf("[%s,%s] stdLst",hName,mName)
		if( is.null(logger) )	return( NULL )
		tDiff <- Sys.time() - tStmp
		rptStr <- sprintf("    %s  %d/%d  %.1f%s ",strWhere,sum(surFlag),length(surFlag),tDiff,units(tDiff) )
		logger$fLogStr( rptStr )
	}

    # scoreMtx.grp <- wScoreMtx.grp ;anaOnly=T
    scMtxName <- names(cut.grp$mtxInfoLst)
	if( !is.null(tgt.scMtx) )	scMtxName <- intersect( scMtxName ,tgt.scMtx )

	# bScrMtxName
	bScrMtxName <- names(cut.grp$mtxInfoLst.bScr)
	if( !is.null(tgt.scMtx) )	bScrMtxName <- intersect( bScrMtxName ,tgt.scMtx )

    datLen <- 1
	cutInfoLst <- list()
	if( !anaOnly ){
		cutInfoLst <- NULL

		if( 0<length(scMtxName) ){
			datLen <- nrow(scoreMtx.grp$basic[[1]][[ scMtxName[1] ]]$scoreMtx)
		} else if( 0<length(bScrMtxName) ){
			datLen <- nrow(scoreMtx.grp$mf[[ bScrMtxName[1] ]]$scoreMtx)
		}
	}

	tStmp <- Sys.time()
	if( !is.null(logger) ) logger$fLogStr("Start", pTime=T ,pAppend=F )

    surFlag <- rep( T ,datLen )
    for( hName in fHName ){ # hName <- fHName[1]
        for( mName in scMtxName ){ # mName <- scMtxName[1]

            #   "stdLst" ------------------------------------------
            for( pName in cut.grp$phaseName ){   # pName <- cut.grp$phaseName[1]
                cutterObj <- cut.grp$cutterLst[[hName]][[mName]]$stdLst[[pName]]
                scoreMtx <- scoreMtx.grp$basic[[pName]][[mName]]$scoreMtx

                rst <- cutterObj$cut( scoreMtx ,!surFlag )
                if( 0<length(rst$cutLst) ){
                    if( anaOnly ){	
                        # cutInfoLst[[1+length(cutInfoLst)]] <- c( cuttedLst[[1]]$idObjDesc ,info=cuttedLst[[1]]$info )
                        cutInfoLst[[1+length(cutInfoLst)]] <- lapply( rst$cutLst[[1]]$cLst ,function(p){ c(p$idObjDesc, info=p$info) })

                    } else {
                        cut_aIdx <- sapply( rst$cutLst ,function(p){p$idx} )
                        surFlag[cut_aIdx] <- FALSE
                    }
                }

				reportStatus( tStmp ,sprintf("     %s",pName) ,surFlag ,logger )
            }
			reportStatus( tStmp ,sprintf("[%s,%s] stdLst",hName,mName) ,surFlag ,logger )


            #   "hIdxLst" ------------------------------------------
            mtxGrp <- getScoreMtx.grp_byHIdx( scoreMtx.grp )
            # for( aIdx in seq_len(datLen) ){	# aIdx <- 1
            #     # 이전과 달리, 1개 aZoid에 대한 처리임을 주의.
            #     if( !surFlag[aIdx] && !anaOnly ) next

            #     cutLst <- cut.grp$cutterLst[[hName]][[mName]]$hIdxLst
            #     scoreMtx <- mtxGrp[[mName]][[aIdx]]
            #     for( cnIdx in names(cutLst) ){  # cnIdx <- names(cutLst)[1]
            #         cuttedLst <- cutLst[[cnIdx]]$cut( scoreMtx ,aIdx )
            #         if( 0<length(cuttedLst) ){
            #             if( anaOnly ){	cutInfoLst[[1+length(cutInfoLst)]] <- c( cuttedLst[[1]]$idObjDesc ,cuttedLst[[1]]$info )
            #             } else {
            #                 surFlag[aIdx] <- FALSE
            #             }
            #         }
            #     }
            # }

        }

		for( mName in bScrMtxName ){
			reportStatus( tStmp ,sprintf("[%s,%s] bScrMtx",hName,mName) ,surFlag ,logger )
        }
    }

    return( list( surFlag=surFlag ,cutInfoLst=cutInfoLst ) )
}

FCust_stdCut_AllM <- function(){

    rObj <- list( )

    rObj$rawMtxSum <- function( mtx ,hpn=F ){
        #   mtx : mName * phName
        sumRst <- list()
        if( hpn ){
            sumRst$hpnCntM <- apply( mtx ,1 ,function(val){sum(0<val)})
            sumRst$hpnCntPh <- apply( mtx ,2 ,function(val){sum(0<val)})
        } else {
            sumRst$zeroCntM <- apply( mtx ,1 ,function(val){sum(0==val)})
            sumRst$zeroCntPh <- apply( mtx ,2 ,function(val){sum(0==val)})
        }

        # rebind next
        colName <- rownames(mtx)[1:(nrow(mtx)-1)]
        rebMtxM <- matrix( 0 ,ncol=length(colName) ,nrow=2 ,dimnames=list(c("flag","cnt"),colName) )
        rebMtxM["cnt",] <- apply(mtx,1,function(val){sum(val>0)})[1:ncol(rebMtxM)]
        for( idx in 1:ncol(rebMtxM) ){
            if( 0<rebMtxM["cnt",idx] ){
                rebMtxM["flag",idx] <- all(mtx[idx,]==mtx[idx+1,])
            }
        }
        sumRst$rebMtxM <- rebMtxM

        colName <- colnames(mtx)[1:(ncol(mtx)-1)]
        rebMtxPh <- matrix( 0 ,ncol=length(colName) ,nrow=2 ,dimnames=list(c("flag","cnt"),colName) )
        rebMtxPh["cnt",] <- apply( mtx, 2, function(val){sum(val>0)})[1:ncol(rebMtxPh)]
        for( idx in 1:ncol(rebMtxPh) ){
            if( 0<rebMtxPh["cnt",idx] ){
                rebMtxPh["flag",idx] <- all(mtx[,idx]==mtx[,idx+1])
            }
        }
        sumRst$rebMtxPh <- rebMtxPh

        return( sumRst )
    }

    rObj$getRawScore <- function( cutRstScr ){
        # basic -----------------------------------------------------------
        basicLst <- list()
        basicLst$hpnMtxRaw      <- rObj$rawMtxSum( cutRstScr$basic$hpnMtxRaw ,hpn=T )
        basicLst$hpnMtxEvt      <- rObj$rawMtxSum( cutRstScr$basic$hpnMtxEvt ,hpn=T )
        basicLst$phRebMtxRaw    <- rObj$rawMtxSum( cutRstScr$basic$phRebMtxRaw ,hpn=T )
        basicLst$phRebMtxEvt    <- rObj$rawMtxSum( cutRstScr$basic$phRebMtxEvt ,hpn=T )

        basicLst$summMtxRaw     <- rObj$rawMtxSum( cutRstScr$basic$summMtxRaw ,hpn=T )
        basicLst$summMtx.RebRaw  <- rObj$rawMtxSum( cutRstScr$basic$summMtx.RebRaw ,hpn=T )
        basicLst$sumMtx         <- cutRstScr$basic$sumMtx
        #   summMtxRaw ,summMtxEvt      summMtx.RebRaw ,summMtx.RebEvt
        #   szMtxCnt ,szMtxDup
        #   sumMtx
        # -------------> bUtil.cutRst1_assScore( )

        # bScr -----------------------------------------------------------
        bScrLst <- list()

        return( list(basic=basicLst ,bScr=bScrLst) )
    }

    rObj$getSummScore <- function( cutRstScr ){ 
        # cutRst1Score <- bUtil.getCut1Score( scoreMtx.grp ,cut.grp ,fHName ,tgt.scMtx=tgt.scMtx )
        # cutRstScrSet <- bUtil.cutRst1_scoreMtx(cutRst1Score$aLst[[1]])
        # cutRstScr <- cutRstScrSet[[hName]]

        rawScore <- rObj$getRawScore( cutRstScr )

        # basic --------------------------------------------------------------------------
        hpnMtxRaw <- rawScore$basic$hpnMtxRaw
        hpnMtxEvt <- rawScore$basic$hpnMtxEvt
        phRebMtxRaw <- rawScore$basic$phRebMtxRaw
        phRebMtxEvt <- rawScore$basic$phRebMtxEvt
        summMtxRaw  <- rawScore$basic$summMtxRaw

        #   summMtxRaw ,summMtxEvt      summMtx.RebRaw ,summMtx.RebEvt
        #   szMtxCnt ,szMtxDup
        #   sumMtx

        rebMtxM_evt =sum(hpnMtxEvt$rebMtxM["flag",] & hpnMtxEvt$rebMtxM["flag",]>1)
        rebMtxPh_evt=sum(hpnMtxEvt$rebMtxPh["flag",] & hpnMtxEvt$rebMtxPh["cnt",]>1)
        primeSumm <- c( zeroCntM_raw=sum(0==hpnMtxRaw$hpnCntM)          ,zeroCntPh_raw=sum(0==hpnMtxRaw$hpnCntPh)
                        ,rebMtxM_raw=sum(hpnMtxRaw$rebMtxM["flag",])    ,rebMtxPh_raw=sum(hpnMtxRaw$rebMtxPh["flag",])
                        ,zeroCntM_evt=sum(0==hpnMtxEvt$hpnCntM)         ,zeroCntPh_evt=sum(0==hpnMtxEvt$hpnCntPh)
                        ,rebMtxM_evt=rebMtxM_evt                        ,rebMtxPh_evt=rebMtxPh_evt
                    )

        basicLst <- list( prime = primeSumm
                    ,summMtxRaw=rawScore$basic$summMtxRaw
                )

        # bScr --------------------------------------------------------------------------
        bScrLst <- list()

        return( list(basic=basicLst,bScr=bScrLst) )
    }

    rObj$cut <- function( summScoreGrp ,hName ,anaOnly=F ){
        #   summScoreGrp <- rObj$getSummScore( cutRstScr )

        cLst <- list( )     #   cutLst[[1]]$cLst
                            #   만약 생존했으면 cLst의 길이는 0

        cfg <- sfcMtxCfg[[hName]]

        if( !is.null(summScoreGrp$basic$prime) ){

            for( nIdx in names(summScoreGrp$basic$prime) ){ # nIdx <- names(summScoreGrp$basic$prime)[1]
                inFlag <- bUtil.in(summScoreGrp$basic$prime[nIdx],cfg$basic$prime[nIdx,])
                if( !inFlag ){
                    if( anaOnly ){
                        keyId <- sprintf("basic_prime_%s",nIdx)
                        cLst[[keyId]] <- sprintf("basic_prime_%s:%d",nIdx,summScoreGrp$basic$prime[nIdx])
                    } else {
                        cLst[["basic_prime"]] <- "basic$prime"
                        return( cLst )
                    }
                }
            }

            # keep working
        }

        if( !is.null(summScoreGrp$bScr) ){
            # todo
        }

        return( cLst )
    }

    return( rObj )
}

