

bUtil.cut1 <- function( scoreMtx.grp ,cut.grp ,fHName ,tgt.scMtx=NULL ,anaOnly=F ,logger=NULL ){
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
	auxInfoLst <- list( basic=list() ,mf=list() )
	mtxGrp <- NULL
	if( 0<length(scMtxName) ){
		mtxGrp <- getScoreMtx.grp_byHIdx( scoreMtx.grp )
	}
    for( hName in fHName ){ # hName <- fHName[1]
        for( mName in scMtxName ){ # mName <- scMtxName[1]
            #   "stdCut" -------------------------------------------
            for( pName in cut.grp$phaseName ){   # pName <- cut.grp$phaseName[1]
				scoreMtx <- scoreMtx.grp$basic[[pName]][[mName]]$scoreMtx

                cutObj <- cut.grp$cutterLst[[hName]][[mName]]$stdCut[[pName]]
                cRst <- cutObj$cut( scoreMtx ,alreadyDead=!surFlag ,anaMode=anaOnly )
				if( !anaOnly ){	surFlag <- surFlag & cRst$surFlag
				} else {
					if( 0<length(cRst$cutLst) ){
						cutInfoLst <- append( cutInfoLst 
											,lapply( cRst$cutLst[[1]]$cLst ,function(p){ c(p$idObjDesc ,info=p$info) } ) 
										)
					}
				}

				for( extFltName in names(cut.grp$cutterExtLst[[hName]][[mName]]$stdCut[[pName]]) ){
					cutExtObj <- cut.grp$cutterExtLst[[hName]][[mName]]$stdCut[[pName]][[extFltName]]
					cRst <- cutExtObj$cut( scoreMtx ,alreadyDead=!surFlag ,anaMode=anaOnly )
					if( !anaOnly ){	surFlag <- surFlag & cRst$surFlag
					} else {
						if( 0<length(cRst$cutLst) ){
							cutInfoLst <- append( cutInfoLst 
												,lapply( cRst$cutLst[[1]]$cLst ,function(p){ c(p$idObjDesc ,info=p$info) } ) 
											)
						}
					}
				}

				reportStatus( tStmp ,sprintf("     %s",pName) ,surFlag ,logger )
            }
			reportStatus( tStmp ,sprintf("[%s,%s] stdLst",hName,mName) ,surFlag ,logger )

			#   "hIdxLst" ------------------------------------------
			hIdxCut <- cut.grp$cutterLst[[hName]][[mName]]$hIdxCut
			for( aIdx in seq_len(datLen) ){
				if( !anaOnly && !surFlag[aIdx] )	next

				cutInfo <- hIdxCut$cut( mtxGrp[[mName]][[aIdx]] ,anaMode=anaOnly )
				if( 0<length(cutInfo$cLst) ){
					if( !anaOnly ){	surFlag[aIdx] <- FALSE
					} else {
						for( idx in seq_len(length(cutInfo$cLst)) ){
							idxName <- sprintf("hIdxCut_%dth",1+length(cutInfoLst))
							cutInfoLst[[idxName]] <- c( typ=names(cutInfo$cLst)[idx] ,hIdxCut$defId ,pName="ALL" ,info=cutInfo$cLst[[idx]] )
						}
					}
				}

				if( anaOnly && ("sfcLate"==hName) ){	# anaOnly상태이면 aIdx는 항상 1이라는 가정.
					auxInfoLst$basic[[mName]] <- cutInfo$scObj
				}
				# if( 1<length(cRst$cutLst) ){
				# 	cutInfoLst[[1+length(cutInfoLst)]] <- cRst
				# }
			}

			reportStatus( tStmp ,sprintf("[%s,%s] hIdxLst",hName,mName) ,surFlag ,logger )

        }

		# bFMtxMulti ----------------------------------------------------------------
		availMFName <- names(cut.grp$cutterExtMLst[[hName]]$stdCut[[1]])
		for( mfName in availMFName ){
			mtxMaker <- bFMtxMFltLst[[mfName]]( tgt.scMtx )
			if( !mtxMaker$available )   next

			mtxLst <- list()
			for( pName in cut.grp$phaseName ){
				scoreMtxLst <- scoreMtx.grp$basic[[pName]]
				mtxLst[[pName]] <- mtxMaker$getScoreMtx( scoreMtxLst )

                cutObj <- cut.grp$cutterExtMLst[[hName]]$stdCut[[pName]][[mfName]]
                cRst <- cutObj$cut( mtxLst[[pName]] ,alreadyDead=!surFlag ,anaMode=anaOnly )
				if( !anaOnly ){	surFlag <- surFlag & cRst$surFlag
				} else {
					if( 0<length(cRst$cutLst) ){
						cutInfoLst <- append( cutInfoLst 
											,lapply( cRst$cutLst[[1]]$cLst ,function(p){ c(p$idObjDesc ,info=p$info) } ) 
										)
					}
				}
			}

			#   "hIdxLst" ------------------------------------------
			# mtxGrp$score1[[1]]
			mtx <- matrix( 0 ,nrow=length(mtxMaker$mInfo$cName) ,ncol=length(cut.grp$phaseName) 
						,dimnames=list( mtxMaker$mInfo$cName ,cut.grp$phaseName )
			)
			for( aIdx in seq_len(datLen) ){
				if( !anaOnly && !surFlag[aIdx] )	next

				for( pName in cut.grp$phaseName ){
					mtx[,pName] <- mtxLst[[pName]][aIdx,]
				}

				hIdxCut <- cut.grp$cutterExtMLst[[hName]]$hIdxCut[[mfName]]
				cutInfo <- hIdxCut$cut( mtx ,anaMode=anaOnly )
				if( 0<length(cutInfo$cLst) ){
					if( !anaOnly ){	surFlag[aIdx] <- FALSE
					} else {
						for( idx in seq_len(length(cutInfo$cLst)) ){
							idxName <- sprintf("hIdxCut_%dth",1+length(cutInfoLst))
							cutInfoLst[[idxName]] <- c( typ=names(cutInfo$cLst)[idx] ,hIdxCut$defId ,pName="ALL" ,info=cutInfo$cLst[[idx]] )
						}
					}
				}

			}

		}


		# QQE : auxInfoLst$mf 추가( auxInfoLst: cut2()의 실행시간이 너무 오래걸려서 cut1()에서 처리시도하기 위한 정보.)
		for( mName in bScrMtxName ){
			scoreMtx <- scoreMtx.grp$mf[[mName]]$scoreMtx

			cutObj <- cut.grp$cutterLst.bScr[[hName]][[mName]]
			cRst <- cutObj$cut( scoreMtx ,alreadyDead=!surFlag ,anaMode=anaOnly )
			if( !anaOnly ){	surFlag <- surFlag & cRst$surFlag
			} else {
				if( 0<length(cRst$cutLst) ){
					cutInfoLst <- append( cutInfoLst 
										,lapply( cRst$cutLst[[1]]$cLst ,function(p){ c(p$idObjDesc ,info=p$info) } ) 
									)
				}
			}

			for( extFltName in names(cut.grp$cutterExtLst.bScr[[hName]][[mName]]) ){
				cutExtObj <- cut.grp$cutterExtLst.bScr[[hName]][[mName]][[extFltName]]
				cRst <- cutExtObj$cut( scoreMtx ,alreadyDead=!surFlag ,anaMode=anaOnly )
				if( !anaOnly ){	surFlag <- surFlag & cRst$surFlag
				} else {
					if( 0<length(cRst$cutLst) ){
						cutInfoLst <- append( cutInfoLst 
											,lapply( cRst$cutLst[[1]]$cLst ,function(p){ c(p$idObjDesc ,info=p$info) } ) 
										)
					}
				}
			}

			reportStatus( tStmp ,sprintf("[%s,%s] bScrMtx",hName,mName) ,surFlag ,logger )

			if( anaOnly && ("sfcLate"==hName) ){	# anaOnly상태이면 aIdx는 항상 1이라는 가정.
				auxInfoLst$mf[[mName]] <- cutInfo$scObj
			}

		}

    }

    return( list( surFlag=surFlag ,cutInfoLst=cutInfoLst ,auxInfoLst=auxInfoLst ) )

} # bUtil.cut1()

bUtil.getCut1Score <- function(  scoreMtx.grp ,cut.grp ,fHName ,tgt.scMtx=NULL ,logger=NULL ,deepInfo=F ){

	reportStatus <- function( tStmp ,strWhere ,surFlag ,logger ){
		#	strWhere <- sprintf("[%s,%s] stdLst",hName,mName)
		if( is.null(logger) )	return( NULL )
		tDiff <- Sys.time() - tStmp
		rptStr <- sprintf("    %s  %d/%d  %.1f%s ",strWhere,sum(surFlag),length(surFlag),tDiff,units(tDiff) )
		logger$fLogStr( rptStr )
	}

    scMtxName <- names(cut.grp$mtxInfoLst)
	if( !is.null(tgt.scMtx) )	scMtxName <- intersect( scMtxName ,tgt.scMtx )
	bScrMtxName <- names(cut.grp$mtxInfoLst.bScr)
	if( !is.null(tgt.scMtx) )	bScrMtxName <- intersect( bScrMtxName ,tgt.scMtx )

	datLen <- nrow(scoreMtx.grp$basic[[1]][[ scMtxName[1] ]]$scoreMtx)
	if( is.null(datLen) ){
		datLen <- nrow(scoreMtx.grp$mf[[ bScrMtxName[1] ]]$scoreMtx)
	}

	tStmp <- Sys.time()
	if( !is.null(logger) ) logger$fLogStr("Start", pTime=T ,pAppend=F )

	mtxGrp <- getScoreMtx.grp_byHIdx( scoreMtx.grp ,tgt.scMtx=scMtxName )	# QQE : tgt.scMtx 적용필요.
	aLst <- list()
	for( aIdx in seq_len(datLen) ){
		hLst <- list()
		for( hName in fHName ){ # hName <- fHName[1]
			basicLst <- list()
			for( mName in scMtxName ){ # mName <- scMtxName[1]
				#   "hIdxLst" ------------------------------------------
				hIdxCut <- cut.grp$cutterLst[[hName]][[mName]]$hIdxCut
				rawObj <- hIdxCut$getRawScore( mtxGrp[[mName]][[aIdx]] )
				raw4Ass <- hIdxCut$getRaw4Ass( rawObj )
				summObj <- hIdxCut$getSummScore( rawObj )

				basicLst[[mName]] <- list(raw=raw4Ass ,summ=summObj)
				if( deepInfo){
					# rawObj$rebInfo 는 bFCust.getSkipZero_byHIdx.ass() 로부터 나왔다.
					basicLst[[mName]]$rawSz <- list( ph=rawObj$rebInfo$matRaw$ph ,fCol=rawObj$rebInfo$matRaw$fCol )
				}

				# reportStatus( tStmp ,sprintf("[%s,%s] hIdxLst",hName,mName) ,surFlag ,logger )
			}

			bScrLst <- list()	# QQE:hold
			bScrMtxName <- character(0)

			hLst[[hName]] <- list( basic=basicLst ,bScr=bScrLst )
		}

		aLst[[as.character(aIdx)]] <- hLst
	}	# for(aIdx)

	cut1ScoreObj <- list( aLst=aLst )
	cut1ScoreObj$metaInfo <- list( datLen=datLen ,scMtxName=scMtxName ,bScrMtxName=bScrMtxName )

	return( cut1ScoreObj )
} # bUtil.cut1Score()

# bUtil.cutAZoidMtx <- function( gEnv ,allIdxF ,cutGrp ){	# 용도 까먹음.
# 	#	cutGrp <- bFCust.getFCustGrp( stdCtrlCfgGrp )
# } # bUtil.cutAZoidMtx( )

bUtil.cutRst1_scoreMtx <- function( cutRst1 ){
	# cutRst1 <- cutRst1Lst.grp[[1]]$aLst[[1]]

	mtxObj <- list()
	for( hName in names(cutRst1) ){	# hName <- names(cutRst1)[1]
		basicLst <- list()
		basicMName <- names(cutRst1[[hName]]$basic)
		if( 0<length(basicMName) ){

			# raw value ------------------------------------------------------------------------
			rawGrp <- cutRst1[[hName]]$basic[[1]]$raw	# 메타정보 가져오기.

			# hpnMtxRaw <- matrix( 0 ,nrow=length(basicMName) ,ncol=ncol(rawGrp$phaseHpnCnt) 
			# 					,dimnames=list( basicMName ,colnames(rawGrp$phaseHpnCnt) )
			# 				)
			# phRebMtxRaw <- matrix( 0 ,nrow=length(basicMName) ,ncol=ncol(rawGrp$phaseRebCnt) 
			# 					,dimnames=list( basicMName ,colnames(rawGrp$phaseRebCnt) )
			# 				)
			hpnMtxRaw <- matrix( 0 ,nrow=length(basicMName) ,ncol=ncol(rawGrp$rebMtx.ph)
								,dimnames=list( basicMName ,colnames(rawGrp$rebMtx.ph) )
							)
			phRebMtxRaw <- matrix( 0 ,nrow=length(basicMName) ,ncol=ncol(rawGrp$phaseReb) 
								,dimnames=list( basicMName ,colnames(rawGrp$phaseReb) )
							)
			rebMtxRaw <- matrix( 0 ,nrow=length(basicMName) ,ncol=ncol(rawGrp$rebMtx.ph)
								,dimnames=list( basicMName ,colnames(rawGrp$rebMtx.ph) )
							)
			hpnMtxEvt <- hpnMtxRaw
			phRebMtxEvt <- phRebMtxRaw
			rebMtxEvt <- rebMtxRaw

			for( mName in basicMName ){
				phRebMtxRaw[mName,]	<- cutRst1[[hName]]$basic[[mName]]$raw$phaseReb["rebFlag.raw",]
				phRebMtxEvt[mName,]	<- cutRst1[[hName]]$basic[[mName]]$raw$phaseReb["rebFlag.evt",]
				hpnMtxRaw[mName,]	<- cutRst1[[hName]]$basic[[mName]]$raw$rebMtx.ph["hpn.raw",]
				hpnMtxEvt[mName,]	<- cutRst1[[hName]]$basic[[mName]]$raw$rebMtx.ph["hpn.evt",]				
				rebMtxRaw[mName,]	<- cutRst1[[hName]]$basic[[mName]]$raw$rebMtx.ph["rebFlag.raw",]
				rebMtxEvt[mName,]	<- cutRst1[[hName]]$basic[[mName]]$raw$rebMtx.ph["rebFlag.evt",]
			}
			basicLst$hpnMtxRaw		<- hpnMtxRaw			;basicLst$hpnMtxEvt		<- hpnMtxEvt
			basicLst$phRebMtxRaw	<- phRebMtxRaw			;basicLst$phRebMtxEvt	<- phRebMtxEvt
			basicLst$rebMtxRaw		<- rebMtxRaw			;basicLst$rebMtxEvt		<- rebMtxEvt

			# summary --------------------------------------------------------------------------
			summ <- cutRst1[[hName]]$basic[[1]]$summ	# 이건 단순히 메타정보 파악 용.

			summColName <- colnames(summ$summMtx)
			summMtxRaw <- matrix( 0 ,nrow=length(basicMName) ,ncol=length(summColName)
								,dimnames=list(basicMName,summColName)
							)
			summMtxEvt		<- summMtxRaw
			summMtx.RebRaw	<- summMtxRaw
			summMtx.RebEvt	<- summMtxRaw

			szColName <- colnames(summ$scMtx.sz)
			szMtxCnt <- matrix( 0 ,nrow=length(basicMName) ,ncol=length(szColName)
								,dimnames=list(basicMName,szColName)
							)
			szMtxDup <- szMtxCnt
			for( mName in basicMName ){
				summ <- cutRst1[[hName]]$basic[[mName]]$summ
				summMtxRaw[mName,]		<- summ$summMtx["raw",]
				summMtxEvt[mName,]		<- summ$summMtx["evt",]
				summMtx.RebRaw[mName,]	<- summ$summMtx.reb["raw",]
				summMtx.RebEvt[mName,]	<- summ$summMtx.reb["evt",]
				szMtxCnt[mName,]		<- summ$scMtx.sz["rebCnt",]
				szMtxDup[mName,]		<- summ$scMtx.sz["rebDup",]
			}

			#	info summ * mName
			basicLst$summMtxRaw		<- summMtxRaw
			basicLst$summMtxEvt		<- summMtxEvt
			basicLst$summMtx.RebRaw	<- summMtx.RebRaw
			basicLst$summMtx.RebEvt	<- summMtx.RebEvt
			basicLst$szMtxCnt		<- szMtxCnt
			basicLst$szMtxDup		<- szMtxDup

			#	sumMtx : sume of column value
			cName <- c("summRawSum" ,"summEvtSum" ,"szRawSum" ,"szEvtSum")
				#	szRawSum : c("r.ph","r.fCol","r.dblHpnFlg")
				#	szEvtSum : c("e.ph","e.fCol","e.dblHpnFlg")
			sumMtx <- matrix( 0 ,nrow=length(basicMName) ,ncol=length(cName) 
							,dimname=list( basicMName ,cName )
						)
			for( mName in basicMName ){
				sumMtx[mName,"summRawSum"]	<- sum( basicLst$summMtxRaw[mName,] )
				sumMtx[mName,"summEvtSum"]	<- sum( basicLst$summMtxEvt[mName,] )
				sumMtx[mName,"szRawSum"]	<- sum( basicLst$szMtxCnt[mName,c("r.ph","r.fCol","r.dblHpnFlg")] )
				sumMtx[mName,"szEvtSum"]	<- sum( basicLst$szMtxCnt[mName,c("e.ph","e.fCol","e.dblHpnFlg")] )
			}
			basicLst$sumMtx	<- sumMtx

		}

		bScrLst <- list()
		bScrMName <- names(cutRst1[[hName]]$bScr)
		# for( mName in names(cutRst1[[hName]]$bScr) ){		}

		mtxObj[[hName]] <- list( basic=basicLst ,bScr=bScrLst )
	}

	return( mtxObj )
}

bUtil.cutRst1_assScore <- function( cr1ScrGrp ){
	#	cr1ScrGrp : cutRst1 ScoreMtx Grp (for 1 hName)
	#		basic ,bScr

	getLev <- function( val ,eadgeMax ){
		
		maxScore <- 4
		valLen <- length(val)
		lev <- rep( 0 ,valLen )

		for( idx in seq_len(valLen) ){

			lev <- 4 - (eadgeMax-val)

			lev[lev<0	] <- 0
			lev[val<=0	] <- 0
			lev[val>=eadgeMax] <- maxScore
			
		}

		return(lev)
	}

	assScore <- cr1ScrGrp	# 리턴되는 구조는 같으므로.

	# basic
	if( 0<length(cr1ScrGrp$basic) ){
		for( mName in rownames(assScore$basic$summMtxRaw) ){
			cfg <- scoreMtxCfg[[mName]]

			val <- cr1ScrGrp$basic$summMtxRaw[mName,]
			eadgeMax <- cfg$summMtx["raw",]
			assScore$basic$summMtxRaw[mName,] <- getLev( val, eadgeMax )

			val <- cr1ScrGrp$basic$summMtxEvt[mName,]
			eadgeMax <- cfg$summMtx["evt",]
			assScore$basic$summMtxEvt[mName,] <- getLev( val, eadgeMax )

			val <- cr1ScrGrp$basic$summMtx.RebRaw[mName,]
			eadgeMax <- cfg$summMtx.reb["raw",]
			assScore$basic$summMtx.RebRaw[mName,] <- getLev( val, eadgeMax )

			val <- cr1ScrGrp$basic$summMtx.RebRaw[mName,]
			eadgeMax <- cfg$summMtx.reb["evt",]
			assScore$basic$summMtx.RebEvt[mName,] <- getLev( val, eadgeMax )

			val <- cr1ScrGrp$basic$szMtxCnt[mName,]
			eadgeMax <- cfg$scMtx.sz["rebCnt",]
			assScore$basic$szMtxCnt[mName,] <- getLev( val, eadgeMax )

			val <- cr1ScrGrp$basic$szMtxDup[mName,]
			eadgeMax <- cfg$scMtx.sz["rebDup",]
			assScore$basic$szMtxDup[mName,] <- getLev( val, eadgeMax )

			val <- cr1ScrGrp$basic$sumMtx[mName,]
			eadgeMax <- c( cfg$summMtx.sum ,cfg$scMtx.sz.sum )
			assScore$basic$sumMtx[mName,] <- getLev( val, eadgeMax )
		}
	}

	# assScore$bScr	# todo
	assScore$bScr <- list()	# dummy

	return( assScore )
}

bUtil.cut2 <- function( cutRst1Score ,fHName ,tgt.scMtx=NULL ,anaOnly=F ,logger=NULL ){
	#	cutRst1Score <- bUtil.getCut1Score( scoreMtx.grp ,cut.grp ,fHName ,tgt.scMtx=tgt.scMtx )

	reportStatus <- function( tStmp ,strWhere ,surFlag ,logger ){
		#	strWhere <- sprintf("[%s,%s] stdLst",hName,mName)
		if( is.null(logger) )	return( NULL )
		tDiff <- Sys.time() - tStmp
		rptStr <- sprintf("    %s  %d/%d  %.1f%s ",strWhere,sum(surFlag),length(surFlag),tDiff,units(tDiff) )
		logger$fLogStr( rptStr )
	}

    # scoreMtx.grp <- wScoreMtx.grp ;anaOnly=T
    scMtxName <- cutRst1Score$metaInfo$scMtxName
	if( !is.null(tgt.scMtx) )	scMtxName <- intersect( scMtxName ,tgt.scMtx )

	# bScrMtxName
	bScrMtxName <- cutRst1Score$metaInfo$bScrMtxName
	if( !is.null(tgt.scMtx) )	bScrMtxName <- intersect( bScrMtxName ,tgt.scMtx )

    datLen <- 1
	cutInfoLst <- list()
	if( !anaOnly ){
		cutInfoLst <- NULL
		datLen <- cutRst1Score$metaInfo$datLen
	}

	tStmp <- Sys.time()
	if( !is.null(logger) ) logger$fLogStr("Start", pTime=T ,pAppend=F )

	stdCut <- FCust_stdCut_AllM( )
    surFlag <- rep( T ,datLen )
	for( aIdx in seq_len(datLen) ){	# aIdx <- 1
		cutRstScrSet <- bUtil.cutRst1_scoreMtx(cutRst1Score$aLst[[aIdx]])

		for( hName in names(cutRstScrSet) ){	# hName <- names(cutRstScrSet)[1]
			summScoreGrp <- stdCut$getSummScore( cutRstScrSet[[hName]] )
			cRst <- stdCut$cut( summScoreGrp ,hName ,anaOnly )
			if( 0 < length(cRst) ){
				if( !anaOnly ){	surFlag[aIdx] <- FALSE	;break
				} else {
					for( idx in seq_len(length(cRst)) ){
						idxName <- sprintf("cut2_%dth",1+length(cutInfoLst))
						cutInfoLst[[idxName]] <- c( typ="cut2_byHName" ,hName=hName ,mName="m*" ,pName="p*" ,info=cRst[[idx]] )
					}
				}
			}
			# c( typ=names(cRst)[idx] ,hIdxCut$defId ,pName="ALL" ,info=cRst[[idx]] )
		}

		# todo
		# if( surFlag[aIdx] ){	}	# hName 전체에 대한 cutting
	}

    return( list( surFlag=surFlag ,cutInfoLst=cutInfoLst ) )

}


bUtil.getSfcHLst <- function( stdFiltedCnt ,baseSpan ,fRstLst ){
	lastH <- baseSpan[length(baseSpan)]

    sfcHLst <- list(    sfcLate= baseSpan[length(baseSpan)] - 20:0
                        ,sfc0=as.integer(names(stdFiltedCnt)[stdFiltedCnt==0])
                        ,sfc1=as.integer(names(stdFiltedCnt)[stdFiltedCnt==1])
                        ,sfc2=as.integer(names(stdFiltedCnt)[stdFiltedCnt==2])
                    )

    stdFilter <- c("D0000.A","A0100.A","AP000.E")   # "AR000.B","AL000A","C1000.A"
    for( sfnIdx in stdFilter ){
        #   sfnIdx <- "D0000.A"
        hSpan <- baseSpan[sapply( fRstLst[as.character(baseSpan)] ,function(p){ sfnIdx %in% p } )]
        hSpan.NG <- hSpan+1
        hSpan.NG <- hSpan.NG[hSpan.NG<=lastH]
        sfcHLst[[sprintf("NG%s",sfnIdx)]] <- hSpan.NG
    }
    lenMax <- 20
    for( hName in names(sfcHLst) ){ # hLst 범위는 20 이내로 하자.
        hLen <- length(sfcHLst[[hName]])
        if( lenMax < hLen ){
            sfcHLst[[hName]] <- sfcHLst[[hName]][ (hLen-lenMax+1):hLen ]
        }
    }

	return( sfcHLst )
}

bUtil.makeStdCtrlCfgGrp <- function( hMtxLst ){

	rObj <- list( createInfo=sprintf("lastH:%d when %s",hMtxLst$lastH,Sys.time()) 
                    ,sfcHLst = hMtxLst$sfcHLst
                    ,mtxInfoLst = hMtxLst$mtxInfoLst
                    ,phaseName = hMtxLst$phaseName
                )

	byFCol <- B.getHMtxLst_byFCol( hMtxLst )
    byHIdx <- B.getHMtxLst_byHIdx( hMtxLst )

	#	names(hMtxLst$sfcHLst)	# "sfcLate"   "NGD0000.A"
	ctrlCfgLst <- list()
	for( hName in names(hMtxLst$sfcHLst) ){	# hName <- names(hMtxLst$sfcHLst)[1]

		mLst <- list()
		for( mName in names(hMtxLst$mtxInfoLst) ){	# mName <- names(hMtxLst$mtxInfoLst)[1]

			stdLst <- list()
			for( pName in hMtxLst$phaseName ){	# pName <- hMtxLst$phaseName[1]
				scoreMtx <- hMtxLst$scoreMtxLst[[hName]][[pName]][[mName]]$scoreMtx
				stdLst[[pName]] <- bUtil.stdCtrlCfg.scoreMtx( scoreMtx )
			}

			fColLst <- list()
			# for( fcName in hMtxLst$mtxInfoLst[[mName]] ){	# fcName <- hMtxLst$mtxInfoLst[[mName]][1]
			# 	mtx <- byFCol[[hName]][[mName]][[fcName]]	# h * phase
			# 	fColLst[[fcName]] <- bUtil.stdCtrlCfg.h_ph4FCol( mtx )
			# }

			hIdxLst <- list()
			# for( hIdxName in as.character(hMtxLst$sfcHLst[[hName]]) ){	# hIdxName <- as.character(hMtxLst$sfcHLst[[hName]])[1]
			# 	mtx <- byHIdx[[hName]][[mName]][[hIdxName]]	# fCol * phase
			# 	hIdxLst[[hIdxName]] <- bUtil.stdCtrlCfg.h_ph4FCol( mtx )
			# }

            mLst[[mName]] <- list( stdLst=stdLst ,fColLst=fColLst ,hIdxLst=hIdxLst )
		} # for(mName)

		ctrlCfgLst[[hName]] <- mLst
	}

	rObj$ctrlCfgLst <- ctrlCfgLst

	return( rObj )
}


bUtil.stdCtrlCfg.scoreMtx <- function( scoreMtx ){
	rObj <- list()

	# [,n] - column direction
	colDirLst <- list()
	for( cnIdx in colnames(scoreMtx) ){
		colDirLst[[cnIdx]] <- bUtil.getCtrlCfg( scoreMtx[,cnIdx] )
	}
	rObj$colDirLst <- colDirLst

	# [,n-1],[,n] - rebind pattern from pre column
	rpCLst <- list()	# rebind pattern, column direction
	rObj$rpCLst <- rpCLst

	# [m-1,],[m,] - rebind pattern from pre row 
	rpRLst <- list()	# rebind pattern, row direction
	rObj$rpRLst <- rpRLst

	return( rObj )

} # bUtil.stdCtrlCfg.scoreMtx()

bUtil.getCtrlCfg <- function( hVal ){

    toString <- function(){
        rptStr <- sprintf("maxMin:%d~%d  evtVal:%s  extVal:%s  hVal.len:%d",maxMin[1],maxMin[2]
                        ,paste( evtVal,collapse=",")    ,paste( extVal,collapse=",")
                        ,hVal.len
                    )
        return( rptStr )
    }

    hVal.len <- length(hVal)
    vUnq <- sort(unique(hVal),decreasing=T)
    vTbl <- table(hVal)[as.character(vUnq)]
    vTbl.len <- length(vTbl)

    maxMin <- vUnq[c(1,vTbl.len)] # valRange 범위 내에서만 허용.(2개 모두 같은 값일수도 있다.)
    evtVal <- integer(0)    # event로서 다룰 값.(주로 maxMin값이지만 발생빈도가 낮은 값.)
    extVal <- integer(0)    # min,max값이었으나, 발생 빈도가 1개라 maxMin에서 제외된 값.

    if( (10<=hVal.len) && (2<=vTbl.len) ){
        extVal.size <- hVal.len %/% 10
        if( extVal.size >= vTbl[as.character(maxMin[1])] ){
            extVal <- c( extVal ,maxMin[1] )
            maxMin[1] <- vUnq[1+1]
        }
        if( extVal.size >= vTbl[as.character(maxMin[2])] ){
            extVal <- c( extVal ,maxMin[2] )
            maxMin[2] <- vUnq[vTbl.len-1]
        }

        evtVal.size <- hVal.len %/% 5
        if( evtVal.size >= vTbl[as.character(maxMin[1])] ){
            evtVal <- c( evtVal ,maxMin[1] )
        }
        if( evtVal.size >= vTbl[as.character(maxMin[2])] ){
            evtVal <- c( evtVal ,maxMin[2] )
        }
    }

    rObj <- list( maxMin=maxMin ,evtVal=evtVal ,extVal=extVal ,hVal.len=hVal.len ,description=toString() )

    return( rObj )

} # bUtil.getCtrlCfg( )

bUtil.filtByCtrlCfg <- function( val ,ctrlCfg ){

	val.len <- length(val)

	cName <- c("survive","evt","ext")
	flagMtx <- matrix( F, nrow=val.len, ncol=length(cName) )
	colnames(flagMtx) <- cName

	for( idx in seq_len(val.len) ){
		flagMtx[idx,"survive"] <- (ctrlCfg$maxMin[1]>=val[idx]) && (val[idx]>=ctrlCfg$maxMin[2])
		flagMtx[idx,"evt"] <- val[idx] %in% ctrlCfg$evt
		flagMtx[idx,"ext"] <- val[idx] %in% ctrlCfg$ext		
	}

	return( flagMtx )
} # bUtil.filtByCtrlCfg()

bUtil.getStdMILst <- function( gEnv ,fRstLst=NULL ){
    # stdMI.basic <- fCutU.getStdMI( gEnv )
	if( FALSE ){	# comment
		# fRstLst : stdMI.bDup, stdMI.mf 생성을 위해 추가해놓긴 했는데...
	}

	stdMILst.basic <- list()
	if( TRUE ){
		zMtx <- gEnv$zhF
		stdMILst.basic[["basic"]] <- list( stdMI=fCutU.getMtxInfo(zMtx) ,zMtx=zMtx )

		zMtx <- fCutU.getNextZW( gEnv )$zMtx
		stdMILst.basic[["nextZW"]] <- list( stdMI=fCutU.getMtxInfo(zMtx) ,zMtx=zMtx )

		zMtx <- fCutU.getNextQuo10( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
		stdMILst.basic[["nextQuo10"]] <- list( stdMI=fCutU.getMtxInfo(zMtx) ,zMtx=zMtx )

		zMtx <- fCutU.getNextBin( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
		stdMILst.basic[["nextBin"]] <- list( stdMI=fCutU.getMtxInfo(zMtx) ,zMtx=zMtx )

		rebObj <- fCutU.getNextRebNumPtn( gEnv ,numPtn=NULL )
		zMtx <- rebObj$zMtx	# rptObj<-anaQuoTbl( zMtx )
		stdMILst.basic[["nextRebNum"]] <- list( stdMI=fCutU.getMtxInfo(zMtx) ,zMtx=zMtx )

		zMtx <- fCutU.getNextCStepBin( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
		stdMILst.basic[["nextCStepBin"]] <- list( stdMI=fCutU.getMtxInfo(zMtx) ,zMtx=zMtx )

		zMtx <- fCutU.getNextFStepBin( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
		stdMILst.basic[["nextFStepBin"]] <- list( stdMI=fCutU.getMtxInfo(zMtx) ,zMtx=zMtx )

		zMtx <- fCutU.getNextColVal( gEnv ,1 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
		stdMILst.basic[["nextColVal_1"]] <- list( stdMI=fCutU.getMtxInfo(zMtx) ,zMtx=zMtx )

		zMtx <- fCutU.getNextColVal( gEnv ,2 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
		stdMILst.basic[["nextColVal_2"]] <- list( stdMI=fCutU.getMtxInfo(zMtx) ,zMtx=zMtx )

		zMtx <- fCutU.getNextColVal( gEnv ,3 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
		stdMILst.basic[["nextColVal_3"]] <- list( stdMI=fCutU.getMtxInfo(zMtx) ,zMtx=zMtx )

		zMtx <- fCutU.getNextColVal( gEnv ,4 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
		stdMILst.basic[["nextColVal_4"]] <- list( stdMI=fCutU.getMtxInfo(zMtx) ,zMtx=zMtx )

		zMtx <- fCutU.getNextColVal( gEnv ,5 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
		stdMILst.basic[["nextColVal_5"]] <- list( stdMI=fCutU.getMtxInfo(zMtx) ,zMtx=zMtx )

		zMtx <- fCutU.getNextColVal( gEnv ,6 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
		stdMILst.basic[["nextColVal_6"]] <- list( stdMI=fCutU.getMtxInfo(zMtx) ,zMtx=zMtx )
	}

    # todo stdFiltedCnt
	#   stdMILst.basic[["stdFCnt"]]

    # todo
    stdMI.bDup <- list()        # basic에서 동일 발생한 것들 끼리의 stdMI (예:colval_1값과 ZW값이 동일했던 적)

    # todo
    stdMI.mf <- list()        # lastZoid가 해당되던 main filter(D0000.A, A0100.A 등)

	rObj <- list( basic=stdMILst.basic ,bDup=stdMI.bDup ,mf=stdMI.mf )
	rObj$anyWarn <- function(){
		rptStr <- character(0)

		chkStr <- character(0)
		for( nIdx in names(rObj$basic) ){
			rCnt <- nrow(rObj$basic[[nIdx]]$stdMI$rawTail)
			if( rCnt<6 ) chkStr <- c( chkStr ,sprintf("%s:%d",nIdx,rCnt) )
		}
		if( 0<length(chkStr) ){
			rptStr <- c( rptStr ,"stdMI.basic")
			rptStr <- c( rptStr ,sprintf("    warn!! %s",paste(chkStr,collapse=" ")) )
		}

		chkStr <- character(0)
		for( nIdx in names(rObj$bDup) ){
			rCnt <- nrow(rObj$bDup[[nIdx]]$stdMI$rawTail)
			if( rCnt<6 ) chkStr <- c( chkStr ,sprintf("%s:%d",nIdx,rCnt) )
		}
		if( 0<length(chkStr) ){
			rptStr <- c( rptStr ,"stdMI.bDup")
			rptStr <- c( rptStr ,sprintf("    warn!! %s",paste(chkStr,collapse=" ")) )
		}

		chkStr <- character(0)
		for( nIdx in names(rObj$mf) ){
			rCnt <- nrow(rObj$mf[[nIdx]]$stdMI$rawTail)
			if( rCnt<6 ) chkStr <- c( chkStr ,sprintf("%s:%d",nIdx,rCnt) )
		}
		if( 0<length(chkStr) ){
			rptStr <- c( rptStr ,"stdMI.mf")
			rptStr <- c( rptStr ,sprintf("    warn!! %s",paste(chkStr,collapse=" ")) )
		}

		rptStr <- paste( rptStr ,collapse="\n" )
		return( sprintf("%s\n",rptStr) )
	} # rObj$rpt( )

    return( rObj )

}	# bUtil.getStdMILst()

bUtil.getSfcLstName <- function( lastStdFilted ,curStdFiltedCnt ,cut.grp ){
	#	lastStdFilted <- fRstLst.w[[length(fRstLst.w)]]
	#	cut.grp <- bFCust.getFCustGrp( stdCtrlCfgGrp ,curHMtxLst ) 

	#	B.makeHMtxLst() 의 sfcHLst 생성 코드 참고할 것.
	fHName <- c( "sfcLate" )
	fHName <- c( fHName  ,sprintf("sfc%d",curStdFiltedCnt) )

	if( 0 < length(lastStdFilted) ) fHName <- c( fHName  ,paste("NG",lastStdFilted,sep="") )

	fHName <- intersect( fHName ,names(cut.grp$sfcHLst) )

	return( fHName )
} # bUtil.getSfcLstName()


bUtil.in <- function( val ,eadge=c(min=0,max=0) ){
	# minEadge, maxEadge 범위 내 포함되면 true
	rst <- val >= eadge["min"]
	rst <- rst & ( val <= eadge["max"] )
	names(rst) <- val
	return( rst )
} # bUtil.in()

bUtil.allSame <- function( val ){
	val.len <- length(val)
	if( 1>=val.len )	return( TRUE )

	return( all(val[1]==val[2:val.len]) )
} # bUtil.allSame()

bUtil.closeMax <- function( score ,wind=c(min=0,max=1) ,distVal=3 ){	# max 값과 얼마나 가까운가.
	distScore <- distVal - ( wind["max"]-score )
	distScore[ distScore>distVal ] <- distVal
	distScore[ distScore<0 ] <- 0
	distScore[ score<=wind["min"] ] <- 0

	return( distScore )
}

bUtil.closeMax_Mtx <- function( scoreMtx ,windMtxMin=NULL ,windMtxMax ,distVal=3 ){

	distMtx <- scoreMtx
	distMtx[,] <- 0

	if( is.null(windMtxMin) ){
		windMtxMin <- windMtxMax
		windMtxMin[,] <- 0
	}

	for( rIdx in seq_len(nrow(scoreMtx)) ){
		for( cIdx in seq_len(ncol(scoreMtx)) ){
			distMtx[rIdx,cIdx] <- bUtil.closeMax( scoreMtx[rIdx,cIdx] 
										,wind=c(min=windMtxMin[rIdx,cIdx],max=windMtxMax[rIdx,cIdx]) 
										,distVal=distVal 
									)
		}
	}

	return(distMtx)
}

bUtil.mtxPtn <- function( mtx ){

	rObj <- list( hpnCode=rep(0,nrow(mtx)) )
	rObj$mHpnLst <- list()	# multi happen
	rObj$nHpnLst <- list()	# next happen
	rObj$symmHpn <- c( "abbA"=0 ,"abxbA"=0 )	# Symmetry happen

	rNum <- nrow(mtx)
	if( 1>= rNum ){
		return( rObj )
	}

	mHpnLst <- list()	# multi happen
	checkedFlag <- rep( F ,rNum )
	fndIdx <- rep( NA ,rNum )
	for( aIdx in 1:(rNum-1) ){
		fndIdx[] <- NA
		for( bIdx in (aIdx+1):rNum ){
			if(checkedFlag[bIdx]) next 

			if( all(mtx[aIdx,]==mtx[bIdx,]) ){
				fndIdx[ which(is.na(fndIdx))[1] ] <- bIdx
				checkedFlag[bIdx] <- TRUE
			}
		}
		if( 0<any(!is.na(fndIdx)) ){
			mHpnLst[[1+length(mHpnLst)]] <- c( aIdx ,fndIdx[!is.na(fndIdx)] )
		}
	}

	if( 0<length(mHpnLst) ){
		rObj$mHpnLst <- mHpnLst
		for( mhIdx in 1:length(mHpnLst) ){
			rObj$hpnCode[ mHpnLst[[mhIdx]] ] <- mhIdx
		}
	}


	if( 0<length(mHpnLst) ){	# next happen
		nHpnLst <- list()
		for( mhIdx in 1:length(mHpnLst) ){
			hpnPos <- mHpnLst[[mhIdx]]
			nHpn <- rep( 0, length(hpnPos)-1 )
			for( idx in 1:(length(hpnPos)-1) ){
				stepSize <- rNum - hpnPos[idx+1]
				nHpn[idx] <- hpnPos[idx]+(stepSize+1)
			}
			nHpnLst[[mhIdx]] <- nHpn
		}
		rObj$nHpnLst <- nHpnLst	# next happen
	}


	if( 0<length(mHpnLst) ){	# Symmetry happen
		#	"babA", "baaA", "bbaA", "a,x,x,a,x,x" 등의 패턴은 nHpnLst에 이미 포함되어 있다.

		if( rNum>=3 && rObj$hpnCode[rNum]>0 ){	# symmCode["abbA"]
			if( rObj$hpnCode[rNum]==rObj$hpnCode[rNum-1] ){
				rObj$symmHpn["abbA"] <- which( rObj$hpnCode[rNum]==rObj$hpnCode )[1] - 1
					# -1 이므로, mtx의 모든 row가 동일하다면 자연스레 0 이 됨.
			}
		}

		if( rNum>=4 && rObj$hpnCode[rNum]>0){	# symmCode["abxbA"]
			if( rObj$hpnCode[rNum]==rObj$hpnCode[rNum-2] ){
				if( rObj$hpnCode[rNum]!=rObj$hpnCode[rNum-1] ){
					rObj$symmHpn["abxbA"] <- rNum-3
				}
			}
		}

	}

	return( rObj )
}

bUtil.mtxColPtn <- function( mtx ,rmAllMat=T ){
	# rmAllMat : 모든 컬럼이 해당되는 경우를 제외.
	#			bUtil.mtxPtn() 결과와의 중복 회피용.

	rObj <- list( aaALst=list() ,bbaA=list() ,babA=list() ,abxbA=list() )
			# "baaB"는 "aaA" 패턴에서 자동적으로 유추된다.

	rNum <- nrow(mtx)	;cNum <- ncol(mtx)
	if( 1>=rNum ){
		return( rObj )
	}

	aaALst <- list()	# erIdx(eadge row idx) ,mFlag(match flag) ,dbgStr
	cmFlag <- NULL
	for( rIdx in (rNum-1):1 ){
		matFlag <- mtx[rIdx,]==mtx[rIdx+1,]
		if( is.null(cmFlag) )	cmFlag <- matFlag

		matFlag[ !cmFlag ] <- F	# 이전까지의 연속적으로 일치해 온 것만 남김.

		if( !all(matFlag==cmFlag) ){
			idStr <- paste(ifelse(cmFlag,"T","."),collapse="")
			dbgStr <- sprintf("%d %s",(rIdx+1),idStr)

			aaALst[[1+length(aaALst)]] <- list( erIdx=(rIdx+1) ,mFlag=cmFlag ,dbgStr=dbgStr )

			cmFlag <- matFlag
		}

		if( !any(cmFlag) )
			break
	}

	bbaA <- list()
	if( 3<=rNum ){
		matFlag <- mtx[rNum-1,]==mtx[rNum-2,]
		if( any(matFlag) ){

			exmFlag <- mtx[rNum-1,]==mtx[rNum  ,]	# a와 b는 달라야 한다.
			if( !all(exmFlag[matFlag]) ){
				bbaA$mFlag <- matFlag
				bbaA$erIdx <- rNum
			}

		}
	}

	babA <- list()
	if( 3<=rNum ){
		matFlag <- mtx[rNum,]==mtx[rNum-2,]
		if( any(matFlag) ){

			exmFlag <- mtx[rNum-1,]==mtx[rNum  ,]	# a와 b는 달라야 한다.
			if( !all(exmFlag[matFlag]) ){
				babA$mFlag <- matFlag
				babA$erIdx <- rNum-1
			}
		}
	}

	abxbA <- list()
	if( 4<=rNum ){
		matFlag <- mtx[rNum-2,] == mtx[rNum,]
		if( any(matFlag) ){

			exmFlagAB <- mtx[rNum-3,]==mtx[rNum-2,]	# a와 b는 달라야 한다.
			exmFlagAX <- mtx[rNum-3,]==mtx[rNum-1,]	# a와 x는 달라야 한다.
			exmFlagBX <- mtx[rNum-2,]==mtx[rNum-1,]	# b와 x는 달라야 한다.

			if( !all(exmFlagAB[matFlag]) && !all(exmFlagAX[matFlag]) && !all(exmFlagBX[matFlag]) ){
				abxbA$mFlag <- matFlag
				abxbA$erIdx <- rNum-3
			}
		}
	}


	if( rmAllMat ){

		# aaALst
		mCnt <- sapply( aaALst ,function(p){sum(p$mFlag)})
		aaALst <- aaALst[mCnt<cNum]

		if( cNum<=sum(bbaA$mFlag) ){
			bbaA <- list()
		}
		if( cNum<=sum(babA$mFlag) ){
			babA <- list()
		}
		if( cNum<=sum(abxbA$mFlag) ){
			abxbA <- list()
		}

	}

	rObj$aaALst <- aaALst
	rObj$bbaA	<- bbaA
	rObj$babA	<- babA
	rObj$abxbA	<- abxbA


	# rObj$cutMinThld <- function( mcPtnObj ,thldMtx=NULL ){
	# 	#	mcPtnObj <- bUtil.mtxColPtn( mtx )
	# 	#		최소 매치 기준을 한정지으려는 건데, 작업보류... 필요 없을 거 같기도 하다.
	# 	if( is.null(thldMtx) ){
	# 		thldMtx <- matrix( 0, nrow=0 ,ncol=2 )
	# 		colnames(thldMtx) <- c("colThld","rowThld")
	# 		for( idx in 1:3 ){
	# 			if( idx >= rNum ) break
	# 			thldMtx <- rbind( thldMtx ,c(rNum-idx ,cNum-idx) )
	# 		}
	# 	}

	# 	return( mcPtnObj )
	# }

	return( rObj )
}

#	src컬럼을 지정할 필요 없음. evtLst 의 이름을 이용함.
bUtil.getEvtVal <- function( src ,evtLst ){
	evtVal <- src[names(evtLst)]
	for( nIdx in names(evtLst) ){
		if( !(evtVal[nIdx] %in% evtLst[[nIdx]]) ) evtVal[nIdx] <- NA
	}
	return( evtVal )
} # bUtil.getEvtVal( )

bUtil.getMtxEvt_byRow <- function( srcMtxLst ,evtLst ){
	# [fCol,phase]
	#	hIdxObj <- B.getHMtxLst_byHIdx( hMtxLst )
	#	srcMtxLst <- hIdxObj[[hName]][[mName]]

	mtxLen <- length(srcMtxLst)

	eMtxLst <- lapply( srcMtxLst ,function( srcMtx ){
						rMtx <- srcMtx
						for( rnIdx in rownames(srcMtx) ){ # rnIdx <- rownames(srcMtx)[1]
							for( cIdx in 1:ncol(srcMtx) ){
								if( !(rMtx[rnIdx,cIdx] %in% evtLst[[rnIdx]]) ){
									rMtx[rnIdx,cIdx] <- NA
								}
							}
						}
						return( rMtx )
	})
	eMtxLst <- eMtxLst[mtxLen:1]	# 작업 편의를 위해 순서를 바꾸자.

	lastMtx <- eMtxLst[[1]]
	rebCntMtx <- lastMtx
	rebCntMtx[!is.na(rebCntMtx)] <- 0
	if( 1<mtxLen ){
		maskMtx <- !is.na(rebCntMtx)
		for( idx in 2:mtxLen){
			matMtx <- lastMtx==eMtxLst[[idx]]
			incFlag <- matMtx
			incFlag [is.na(incFlag)] <- F
			incFlag[ !maskMtx ] <- FALSE
			if( all(!incFlag) ) break

			rebCntMtx[ incFlag ] <- 1 + rebCntMtx[ incFlag ]

			maskMtx <- incFlag
		}
	}

	return( list(lastMtx=lastMtx ,rebCntMtx=rebCntMtx ,maskMtx=!is.na(lastMtx) ,lastMtxRaw=srcMtxLst[[mtxLen]]) )

} # bUtil.getEvtMtx()


bUtil.getMtxRebPtn.skipZero <- function( mtxLst.h ,hpnThld.fCol=NA ,hpnThld.ph=NA ){
	# - 패턴을 찾지 못한 경우, rep( NA ,n ) 데이터가 주어진다.

	# * mtxLst.h : fCol*phase 형태의 scoreMtx가 history 별로 저장된 상태.
	#		hIdxObj <- B.getHMtxLst_byHIdx(hMtxLst)
	#		mtxLst.h <- hIdxObj[["sfcLate"]][["score3"]]
	# * hpnThld.fCol / hpnThld.ph : 0 이 많지않은 scoreMtx의 경우, 일정 hpn수 이상만 찾도록 제약
	#		(NA 이면 무시.)

	#	Debugging Code -------------------------------------------------------------------
	# 	mtxLst.h=hIdxObj[["sfcLate"]][["score3"]]
	# 	rObj <- bUtil.getMtxRebPtn.skipZero( mtxLst.h ,hpnThld.fCol=2 ,hpnThld.ph=3 )
	# 		# sapply( rObj$fColLst ,function(obj){ ifelse(is.null(obj$fndInfo),"",obj$fndInfo) })
	# 		# sapply( rObj$phLst ,function(obj){ ifelse(is.null(obj$fndInfo),"",obj$fndInfo) })
	#
	# 	scoreMtx <- mtxLst[["839"]]
	# 	rObj$diffCnt.fCol( scoreMtx )
	# 	rObj$diffCnt.ph( scoreMtx )

	mtxLst.len <- length(mtxLst.h)
	mtxLst <- mtxLst.h[mtxLst.len:1]	# 처리 편하게..
	mtxNames <- names(mtxLst)

	fColLst <- list()
	for( rIdx in 1:nrow(mtxLst[[1]]) ){
		hpnVal <- NULL
		fndInfo <- NULL
		for( bHIdx in 1:mtxLst.len ){
			cnt <- sum( mtxLst[[bHIdx]][rIdx,]>0 ,na.rm=T )
			
			if( 0==cnt ) next

			if( !is.na(hpnThld.fCol) && cnt<=hpnThld.fCol ) next

			hpnVal <- mtxLst[[bHIdx]][rIdx,]
			fndInfo <- mtxNames[bHIdx]
			break
		}

		fColLst[[sprintf("R%d",rIdx)]] <- list(hpnVal=hpnVal ,fndInfo=fndInfo)
	}
	names(fColLst) <- rownames(mtxLst[[1]])

	phLst <- list()
	for( cIdx in 1:ncol(mtxLst[[1]]) ){
		hpnVal <- NULL
		fndInfo <- NULL
		for( bHIdx in 1:mtxLst.len ){
			cnt <- sum( mtxLst[[bHIdx]][,cIdx]>0 ,na.rm=T )
			
			if( 0==cnt ) next

			if( !is.na(hpnThld.ph) && cnt<=hpnThld.ph ) next

			hpnVal <- mtxLst[[bHIdx]][,cIdx]
			fndInfo <- mtxNames[bHIdx]
			break
		}

		hpnVal <- mtxLst[[bHIdx]][,cIdx]
		fndInfo <- mtxNames[bHIdx]

		phLst[[sprintf("C%d",cIdx)]] <- list(hpnVal=hpnVal ,fndInfo=fndInfo)
	}
	names(phLst) <- colnames(mtxLst[[1]])

	rObj <- list(fColLst=fColLst ,phLst=phLst)

	rObj$diffCnt.fCol <- function( scoreMtx ){
		cnt <- rep( ncol(scoreMtx) ,nrow(scoreMtx) )
		names(cnt) <- rownames(scoreMtx)
		for( rIdx in 1:nrow(scoreMtx) ){
			hpnVal <-rObj$fColLst[[rIdx]]$hpnVal
			if( is.null(hpnVal) ) next

			cnt[rIdx] <- sum( scoreMtx[rIdx,]!=hpnVal ,na.rm=T )
		}

		return( cnt )
	}

	rObj$diffCnt.ph <- function( scoreMtx ){
		cnt <- rep( nrow(scoreMtx) ,ncol(scoreMtx) )
		names(cnt) <- colnames(scoreMtx)
		for( cIdx in 1:ncol(scoreMtx) ){
			hpnVal <-rObj$phLst[[cIdx]]$hpnVal
			if( is.null(hpnVal) ) next

			cnt[cIdx] <- sum( scoreMtx[,cIdx]!=hpnVal ,na.rm=T )
		}

		return( cnt )
	}

	return( rObj )

} # bUtil.getMtxRebPtn.skipZero( )


bUtil.getEvt_byHIdx <- function( scoreMtx ,evtLst ,lastEvt=NULL ,ignoreOpt=NULL ){
	#	ignoreOpt : 모든 확인에서 0을 무시하려면 "all"=0, 무시 없으려면 ignoreOpt=NULL
	#				일부분만 무시하려면... 흠 나중에 추가하자.
	#	lastEvt
	#		hIdxObj <- B.getHMtxLst_byHIdx( curHMtxLst )
	#		mtxLst=hIdxObj[[hName]][[mName]]
	#		lastEvt <- bUtil.getEvt_byHIdx( mtxLst[[length(mtxLst)]] ,evtLst ,mtxLst[[length(mtxLst)-1]] )

	ignLst <- list( hpnInfo_phaseReb=NULL 
						,rebMtx.all.rebRaw=0	,rebMtx.ph.raw=0	,rebMtx.fCol.raw=0	,rebMtx.phReb.raw=0
						,summary.raw.ph=0		,summary.raw.fCol=0
					)	# NULL값이면 ignore가 없다는 의미
	if( !is.null(ignLst) )	{
		for( nIdx in names(ignoreOpt) ){
			ignLst[[nIdx]] <- if( is.na(ignoreOpt[nIdx]) ) NULL else ignoreOpt[nIdx]
		}
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
			if( vCnt[cIdx]==vCnt[cIdx+1] ){	# 전체 매치 여부만 T/F로 따지자.
				rebMtx["reb",cIdx] <- matCnt == vCnt[cIdx]
			}
			rebMtx["hpn",cIdx] <- sum( mtx[,cIdx]>0 ,na.rm=T )
			rebMtx["sum",cIdx] <- sum( mtx[,cIdx] ,na.rm=T )
		}

		return( rebMtx )
	}
	getEvtInfo <- function( rawMtx ,evtLst ){
		evtInfo <- list()

		evtMtx <- rawMtx
		for( cIdx in seq_len(ncol(evtMtx)) ){
			evtMtx[,cIdx] <- mapply( function(val,evt){ return( if(val%in%evt) val else NA )	}
									,rawMtx[,cIdx],evtLst
								)
		}

		evtInfo$evtMtx <- evtMtx
		evtInfo$evtMask <- !is.na(evtMtx)
		evtInfo$tot = sum(!evtInfo$evtMask)
		evtInfo$fCol = apply( evtInfo$evtMask ,1 ,function(byRV){ sum(byRV) })
		evtInfo$phase = apply( evtInfo$evtMask ,2 ,function(byCV){ sum(byCV) } )
		evtInfo$phaseReb <- getPhaseRebMtx( evtMtx ,ignoreThld=NULL )
		return( evtInfo )
	}
	getHpnInfo <- function( rawMtx ){
		hpnInfo <- list( tot=	sum(rawMtx>0)
							,fCol=	apply( rawMtx ,1 ,function(byRV){ sum(byRV>0) })
							,phase= apply( rawMtx ,2 ,function(byCV){ sum(byCV>0) })
							,rawMtx=rawMtx
						)
		hpnInfo$phaseReb <- getPhaseRebMtx( rawMtx ,ignoreThld=ignLst[["hpnInfo_phaseReb"]] )
		return( hpnInfo )
	}

	rObj <- list( dim=dim(scoreMtx) )
	rObj$hpnInfo <- getHpnInfo( scoreMtx )
	rObj$evtInfo <- getEvtInfo( scoreMtx ,evtLst )

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
			if( is.null(ignLst[["rebMtx.all.rebRaw"]]) ) matMask[,] <- TRUE
			rebMtx <- rObj$hpnInfo$rawMtx == lastEvt$hpnInfo$rawMtx
			rebMtx[!matMask] <- F
			rebMtx.all["hpn","rebRaw"] <- sum(rObj$hpnInfo$rawMtx >0)
			rebMtx.all["rebFlag","rebRaw"] <- sum(rebMtx)==sum(matMask)

			matMask <- !( is.na(rObj$evtInfo$evtMtx) & is.na(lastEvt$evtInfo$evtMtx) )
			rebMtx <- rObj$evtInfo$evtMtx==lastEvt$evtInfo$evtMtx
			rebMtx.all["hpn","rebEvt"] <- sum(rObj$evtInfo$evtMtx>0,na.rm=T)
			rebMtx.all["rebFlag","rebEvt"] <- sum(rebMtx,na.rm=T)==sum(matMask)

			rebInfo$rebMtx.all <- rebMtx.all

			# rebInfo$rebMtx.ph ----------------------------------------------
			rName <- c("rebFlag.raw","hpn.raw","rebFlag.evt","hpn.evt")
			cName <- colnames(rObj$hpnInfo$rawMtx)
			rebMtx.ph <- matrix( 0 ,nrow=length(rName) ,ncol=length(cName) )
			rownames(rebMtx.ph) <- rName	;colnames(rebMtx.ph) <- cName

			rebMtx.ph["hpn.raw",] <- apply( rObj$hpnInfo$rawMtx>0 ,2 ,sum )
			rebMtx.ph["hpn.evt",] <- apply( !is.na(rObj$evtInfo$evtMtx) ,2 ,sum )
			for( pName in colnames(rObj$hpnInfo$rawMtx) ){
				matMask <- !( rObj$hpnInfo$rawMtx[,pName]==0 & lastEvt$hpnInfo$rawMtx[,pName]==0 )
				if( is.null(ignLst[["rebMtx.ph.raw"]]) ) matMask[] <- TRUE
				rebFlag <- rObj$hpnInfo$rawMtx[,pName] == lastEvt$hpnInfo$rawMtx[,pName]
				rebFlag[!matMask] <- F
				rebMtx.ph["rebFlag.raw",pName] <- sum(rebFlag) == sum(matMask)

				matMask <- !( is.na(rObj$evtInfo$evtMtx[,pName]) & is.na(lastEvt$evtInfo$evtMtx[,pName]) )
				rebFlag <- rObj$evtInfo$evtMtx[,pName]==lastEvt$evtInfo$evtMtx[,pName]
				rebMtx.ph["rebFlag.evt",pName] <- sum(rebFlag,na.rm=T)==sum(matMask)
			}

			rebInfo$rebMtx.ph <- rebMtx.ph

			# rebInfo$rebMtx.fCol ----------------------------------------------
			rName <- c("rebFlag.raw","hpn.raw","rebFlag.evt","hpn.evt")
			cName <- rownames(rObj$hpnInfo$rawMtx)
			rebMtx.fCol <- matrix( 0 ,nrow=length(rName) ,ncol=length(cName) )
			rownames(rebMtx.fCol) <- rName	;colnames(rebMtx.fCol) <- cName

			rebMtx.fCol["hpn.raw",] <- apply( rObj$hpnInfo$rawMtx>0 ,1 ,sum )
			rebMtx.fCol["hpn.evt",] <- apply( !is.na(rObj$evtInfo$evtMtx) ,1 ,sum )
			for( fcName in rownames(rObj$hpnInfo$rawMtx) ){
				matMask <- !( rObj$hpnInfo$rawMtx[fcName ,]==0 & lastEvt$hpnInfo$rawMtx[fcName ,]==0 )
				if( is.null(ignLst[["rebMtx.fCol.raw"]]) ) matMask[] <- TRUE
				rebFlag <- rObj$hpnInfo$rawMtx[fcName ,] == lastEvt$hpnInfo$rawMtx[fcName ,]
				rebFlag[!matMask] <- F
				rebMtx.fCol["rebFlag.raw" ,fcName] <- sum(rebFlag) == sum(matMask)

				matMask <- !( is.na(rObj$evtInfo$evtMtx[fcName ,]) & is.na(lastEvt$evtInfo$evtMtx[fcName ,]) )
				rebFlag <- rObj$evtInfo$evtMtx[fcName ,]==lastEvt$evtInfo$evtMtx[fcName ,]
				rebMtx.fCol["rebFlag.evt" ,fcName] <- sum(rebFlag,na.rm=T)==sum(matMask)
			}

			rebInfo$rebMtx.fCol <- rebMtx.fCol

			# rebInfo$rebMtx.phReb ----------------------------------------------
			#	hpn이 없는 것은 rebound를 인정치 않는다.
			rName <- c("raw","evt")
			cName <- colnames(rObj$hpnInfo$phaseReb)
			rebMtx.phReb <- matrix( 0 ,nrow=length(rName) ,ncol=length(cName) )
			rownames(rebMtx.phReb) <- rName	;colnames(rebMtx.phReb) <- cName

			rebMask <- rObj$hpnInfo$phaseReb["hpn",]>0 & lastEvt$hpnInfo$phaseReb["hpn",]>0
			if( is.null(ignLst[["rebMtx.phReb.raw"]]) ) rebMask[] <- TRUE
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

			rebInfo$rebMtx.xyCnt <- rebMtx

			# rebInfo$summary ----------------------------------------------
			rName <- c("raw","evt")
			cName <- c("all","ph","fCol","phReb","xyCnt.fCol","xyCnt.phase")
			summMtx <- matrix( 0 ,nrow=length(rName) ,ncol=length(cName) )
			rownames(summMtx) <- rName	;colnames(summMtx) <- cName

			summMtx[,"all"]			<- rebInfo$rebMtx.all["rebFlag" ,]

			rowMask <- (rebInfo$rebMtx.ph["hpn.raw",]>0)
			if( is.null(ignLst[["summary.raw.ph"]]) ) rowMask[] <- TRUE
			summMtx["raw","ph"]		<- sum( rebInfo$rebMtx.ph["rebFlag.raw",] & rowMask )
			summMtx["evt","ph"]		<- sum( rebInfo$rebMtx.ph["rebFlag.evt",] & (rebInfo$rebMtx.ph["hpn.evt",]>0) )

			rowMask <- (rebInfo$rebMtx.fCol["hpn.raw",]>0)
			if( is.null(ignLst[["summary.raw.fCol"]]) ) rowMask[] <- TRUE
			summMtx["raw","fCol"]	<- sum( rebInfo$rebMtx.fCol["rebFlag.raw",] & rowMask )
			summMtx["evt","fCol"]	<- sum( rebInfo$rebMtx.fCol["rebFlag.evt",] & (rebInfo$rebMtx.fCol["hpn.evt",]>0) )

			summMtx[,"phReb"]		<- apply( rebInfo$rebMtx.phReb ,1 ,sum )

			summMtx[,"xyCnt.fCol"]	<- rebInfo$rebMtx.xyCnt[,"fCol.allMat"]
			summMtx[,"xyCnt.phase"]	<- rebInfo$rebMtx.xyCnt[,"phase.allMat"]

			rebInfo$summMtx <- summMtx

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

} # bUtil.getEvt_byHIdx()

bUtil.getShortPhaseName <- function( phaseName ){
    phaseName <- gsub("^next","",phaseName)
    phaseName <- gsub("^ColVal_","cv",phaseName)
    phaseName <- gsub("StepBin","Bin",phaseName)
    return( phaseName )
} # bUtil.getShortPhaseName()


bUtil.chkStdMIPair <- function( gEnv ,aZoidMtx ){

	stdMI.grp <- bUtil.getStdMILst( gEnv )

	fndPairCol <- function( mtx ){	# mtx내에는 NA 값 존재.
		# mtx <- lastZoidMtx

		fndLst <- list()
		fndFlag <- rep( F ,ncol(mtx) )
		gFndFlag <- fndFlag
		for( oIdx in 1:(ncol(mtx)-1) ){
			if( gFndFlag[oIdx] ) next

			fndFlag[] <- F	# 아직은 oIdx가 포함되지 않은 상태.
			for( iIdx in (oIdx+1):ncol(mtx) ){
				matFlag <- mtx[,oIdx]==mtx[,iIdx]
				if( any(is.na(matFlag)) )	next

				if( all(matFlag) ){
					fndFlag[iIdx] <- T
				}
			}

			if( any(fndFlag) ){
				fndFlag[oIdx] <- T
				gFndFlag <- gFndFlag | fndFlag
				fndLst[[sprintf("col%d",oIdx)]] <- which(fndFlag)
			}
		}

		return( fndLst )
	}

	lastZoidMtx <- sapply( stdMI.grp$basic ,function(stdMIObj){ 
		zLen <- nrow(stdMIObj$zMtx)
		if( 0==zLen ){  return( rep(NA,ncol(stdMIObj$zMtx)) )
		} else {    return(stdMIObj$zMtx[zLen,]) }
	})

	lastPair <- fndPairCol( lastZoidMtx )
	if( 0==length(lastPair) ){
		rebLst <- lapply( seq_len(nrow(aZoidMtx)) ,function(p){ list() })
		return( rebLst )
	}

	rebLst <- list()
	for( aIdx in seq_len(nrow(aZoidMtx)) ){
		gEnv.n <- gEnv
		gEnv.n$zhF <- rbind( gEnv.n$zhF,aZoidMtx[aIdx,] )
		zhName <- rownames(gEnv.n$zhF)
		zhName[nrow(gEnv.n$zhF)] <- "aZd"
		rownames(gEnv.n$zhF) <- zhName

		stdMI.grpN <- bUtil.getStdMILst( gEnv.n )
		mtx <- sapply( stdMI.grpN$basic ,function(stdMIObj){ 
			zLen <- nrow(stdMIObj$zMtx)
			if( 0==zLen ){  return( rep(NA,ncol(stdMIObj$zMtx)) )
			} else {    return(stdMIObj$zMtx[zLen,]) }
		})
		
		curPair <- fndPairCol( mtx )
		if( 0==length(curPair) ){
			rebLst[[sprintf("a%d",aIdx)]] <- list()
			next
		}

		fndPairLst <- list()
		for( lIdx in 1:length(lastPair) ){
			for( curIdx in 1:length(curPair) ){
				fndPair <- intersect(lastPair[[lIdx]],curPair[[curIdx]])
				if( 1<length(fndPair) ){
					dbgStr <- sprintf("lastPair:%s curPair:%s"
								,paste(lastPair[[lIdx]],collapse=",")
								,paste(curPair[[curIdx]],collapse=",")
					)
					fndPairLst[[sprintf("l%dc%d",lIdx,curIdx)]] <- list( fndPair=fndPair ,dbgStr=dbgStr )
				}
			}
		}

		rebLst[[sprintf("a%d",aIdx)]] <- fndPairLst
	}

	return( rebLst )
}


bUtil.findSeg <- function( aCode ,pairMaxLen=NULL ,useValName=TRUE ){

	segLst <- list()

	tbl <- table(aCode)
	if( all(tbl<2) ){
		return( segLst )
	} else {
		tbl <- tbl[tbl>=2]
		if( !is.null(pairMaxLen) ){
			tbl <- tbl[ tbl<=pairMaxLen ]
		}
	}

	for( val in as.integer(names(tbl)) ){
		segObj <- list( val=val ,idx=which(aCode==val) )
		segLst[[1+length(segLst)]] <- segObj
	}

	if( useValName ){
		names(segLst) <- paste("val",names(tbl),sep="")
	} else {	
		names(segLst) <- sapply(segLst,function(p){ paste(p$idx,collapse="_") })
	}
	
	return( segLst )
}

bUtil.findLinearPtn <- function( codeMtx ,yIdx ,typ="V" ){
	#	typ : V A W M		W,M 타입은 일단 보류.

	if( FALSE ){	# working code
		typ <- "V"	;yIdx<-NULL
		codeMtx <- matrix( sample(1:100,24) ,nrow=4 ,ncol=6 )
	}

	getPtnIdx_V <- function( dimVal ,yIdx ){
		idxLst <- list()
		for( cIdx in seq_len(dimVal[2]) ){
			baseIdx <- c(yIdx,cIdx)
			leftMtx <- matrix(0,nrow=0,ncol=2)	;rightMtx <- matrix(0,nrow=0,ncol=2)
			
			# left side
			minDistance <- min(yIdx-1,cIdx-1)
			for( idx in seq_len(minDistance) ){
				leftMtx <- rbind( leftMtx ,c(yIdx-idx,cIdx-idx) )
			}

			# right side
			minDistance <- min(yIdx-1,dimVal[2]-cIdx)
			for( idx in seq_len(minDistance) ){
				rightMtx <- rbind( rightMtx ,c(yIdx-idx,cIdx+idx) )
			}

			idxLst[[sprintf("col%d",cIdx)]] <- list( baseIdx=baseIdx ,leftMtx=leftMtx ,rightMtx=rightMtx )
		}

		return( idxLst )
	}
	getPtnIdx_A <- function( dimVal ,yIdx ){
		idxLst <- list()
		for( cIdx in seq_len(dimVal[2]) ){
			baseIdx <- c(yIdx,cIdx)
			leftMtx <- matrix(0,nrow=0,ncol=2)	;rightMtx <- matrix(0,nrow=0,ncol=2)

			# left side
			minDistance <- min(dimVal[1]-yIdx,cIdx-1)
			for( idx in seq_len(minDistance) ){
				leftMtx <- rbind( leftMtx ,c(yIdx+idx,cIdx-idx) )
			}

			# right side
			minDistance <- min(dimVal[1]-yIdx,dimVal[2]-cIdx)
			for( idx in seq_len(minDistance) ){
				rightMtx <- rbind( rightMtx ,c(yIdx+idx,cIdx+idx) )
			}

			idxLst[[sprintf("col%d",cIdx)]] <- list( baseIdx=baseIdx ,leftMtx=leftMtx ,rightMtx=rightMtx )
		}

		return( idxLst )
	}

	idxLst <- NULL
	if( "V"==typ ){
		idxLst <- getPtnIdx_V( dimVal=dim(codeMtx) ,yIdx )
	} else if( "A"==typ ){
		idxLst <- getPtnIdx_A( dimVal=dim(codeMtx) ,yIdx )
	}

	for( idx in seq_len(length(idxLst)) ){
		idxLst[[idx]]$baseIdx <- c( idxLst[[idx]]$baseIdx ,codeMtx[idxLst[[idx]]$baseIdx[1],idxLst[[idx]]$baseIdx[2]] )
		names(idxLst[[idx]]$baseIdx) <- c("row","col","val")

		if( 0<nrow(idxLst[[idx]]$leftMtx) ){
			leftVal <- apply(idxLst[[idx]]$leftMtx,1,function(idxRow){ codeMtx[idxRow[1],idxRow[2]] })
			idxLst[[idx]]$leftMtx <- cbind( idxLst[[idx]]$leftMtx ,leftVal )
		} else {
			idxLst[[idx]]$leftMtx <- matrix( 0 ,nrow=0 ,ncol=3 )
		}
		colnames( idxLst[[idx]]$leftMtx ) <- c("row","col","val")

		if( 0<nrow(idxLst[[idx]]$rightMtx) ){
			rightVal <- apply(idxLst[[idx]]$rightMtx,1,function(idxRow){ codeMtx[idxRow[1],idxRow[2]] })
			idxLst[[idx]]$rightMtx <- cbind( idxLst[[idx]]$rightMtx ,rightVal )
		} else {
			idxLst[[idx]]$rightMtx <- matrix( 0 ,nrow=0 ,ncol=3 )
		}
		colnames( idxLst[[idx]]$rightMtx ) <- c("row","col","val")
	}

	return( idxLst )
}

bUtil.checkMatch_LinearPtn <- function( lPtn ,aCode ){
	#	lPtn <- bUtil.findLinearPtn()
	pLen <- length(lPtn)
	matCnt <- rep( 0 ,pLen )

	for( cIdx in seq_len(pLen) ){
		ptn <- lPtn[[cIdx]]
		if( aCode[ptn$baseIdx["col"]] != ptn$baseIdx["val"] )	next

		for( rIdx in seq_len(nrow(ptn$leftMtx)) ){
			flag <- aCode[ptn$leftMtx[rIdx,"col"]] == ptn$leftMtx[rIdx,"val"]
			
			if( flag ){	matCnt[cIdx] <- 1 + matCnt[cIdx]
			} else break
		}
		for( rIdx in seq_len(nrow(ptn$rightMtx)) ){
			flag <- aCode[ptn$rightMtx[rIdx,"col"]] == ptn$rightMtx[rIdx,"val"]

			if( flag ){	matCnt[cIdx] <- 1 + matCnt[cIdx]
			} else break
		}
	}

	return( matCnt )
}


bUtil.getClM_cutRst1Score <- function( cutRst1 ,cfgLst ,mNameGrp ,fHName=NULL ){
	#	cutRst1 <- cutRst1Score$aLst[[1]]
	#	cfgLst <- scoreMtxCfg	# scoreMtxCfg 호환 객체 리스트
	#	mNameGrp <- tgt.scMtx
	summMtxMin <- NULL	;summMtx.rebMin <- NULL	;scMtx.szMin <- NULL
	wind <- c("min"=0 ,"max"=0)

	summMtxLst <- NULL		;summMtx.rebLst <- NULL		;scMtx.szLst <- NULL
	sumTotLst <- NULL

	if( is.null(fHName) ){
		fHName <- names(cutRst1)
	}
	for( hName in fHName ){
		for( mName in mNameGrp ){
			summ <- cutRst1[[hName]]$basic[[mName]]$summ
			cfg <- cfgLst[[mName]]
			
			# summMtx
			if( is.null(summMtxMin) ){
				summMtxMin <- cfg$summMtx
				summMtxMin[,] <- 0
			}
			clMtx <- bUtil.closeMax_Mtx( scoreMtx=summ$summMtx ,windMtxMin=summMtxMin ,windMtxMax=cfg$summMtx )
			summMtxLst[[sprintf("%s_%s",hName,mName)]] <- clMtx
			
			# summMtx.reb
			if( is.null(summMtx.rebMin) ){
				summMtx.rebMin <- cfg$summMtx
				summMtx.rebMin[,] <- 0
			}
			clMtx <- bUtil.closeMax_Mtx( scoreMtx=summ$summMtx.reb ,windMtxMin=summMtx.rebMin ,windMtxMax=cfg$summMtx.reb )
			summMtx.rebLst[[sprintf("%s_%s",hName,mName)]] <- clMtx

			# scMtx.sz
			if( is.null(scMtx.szMin) ){
				scMtx.szMin <- cfg$summMtx
				scMtx.szMin[,] <- 0
			}
			clMtx <- bUtil.closeMax_Mtx( scoreMtx=summ$scMtx.sz ,windMtxMin=scMtx.szMin ,windMtxMax=cfg$scMtx.sz )
			scMtx.szLst[[sprintf("%s_%s",hName,mName)]] <- clMtx

			# sumTot ----------------------------------------------------------------------
			#	FCust_H.R의 FCust_stdCut.hIdx() 함수 참고.
			sumTot <- c("lev2ClM"=0 ,"lev3ClM"=0 ,"summMtxRaw"=0 ,"summMtxEvt"=0 ,"scMtxSzRaw"=0 ,"scMtxSzEvt"=0 )
			if( TRUE ){
				botThld		<- summ$fColEvt$closeMaxDistVal - 2
				eSumLev2	<- sum(summ$fColEvt$fClMMtx[,"lev2ClM"] > botThld )
				wind["max"] <- cfg$evtMaxFColTot["lev2Max"]
				sumTot["lev2ClM"] <- bUtil.closeMax( eSumLev2 ,wind=wind )
				botThld <- summ$fColEvt$closeMaxDistVal - 2
				eSumLev3 <- sum(summ$fColEvt$fClMMtx[,"lev3ClM"] > botThld )
				wind["max"] <- cfg$evtMaxFColTot["lev3Max"]
				sumTot["lev3ClM"] <- bUtil.closeMax( eSumLev3 ,wind=wind )

				wind["max"] <- cfg$summMtx.sum["raw"]
				sumTot["summMtxRaw"] <- bUtil.closeMax( sum(summ$summMtx["raw",]) ,wind=wind )
				wind["max"] <- cfg$summMtx.sum["evt"]
				sumTot["summMtxEvt"] <- bUtil.closeMax( sum(summ$summMtx["evt",]) ,wind=wind )

				wind["max"] <- cfg$scMtx.sz.sum["rebCnt.r"]
				sumCol <- c("r.ph","r.fCol","r.dblHpnFlg")
				sumTot["scMtxSzRaw"] <- bUtil.closeMax( sum(summ$scMtx.sz["rebCnt",sumCol]) ,wind=wind )

				wind["max"] <- cfg$scMtx.sz.sum["rebCnt.e"]
				sumCol <- c("e.ph","e.fCol","e.dblHpnFlg")
				sumTot["scMtxSzEvt"] <- bUtil.closeMax( sum(summ$scMtx.sz["rebCnt",sumCol]) ,wind=wind )
			}
			sumTotLst[[sprintf("%s_%s",hName,mName)]] <- sumTot

		}
	}

	summMtxClM		<- bUtil.getMtxMaxVal_fromLst( summMtxLst )
	summMtx.rebClM	<- bUtil.getMtxMaxVal_fromLst( summMtx.rebLst )
	scMtx.szClM		<- bUtil.getMtxMaxVal_fromLst( scMtx.szLst )
	
	sumTotClM		<- NULL
	for( idx in seq_len(length(sumTotLst)) ){
		if( idx==1 ){
			sumTotClM <- sumTotLst[[idx]]
			next
		}

		updateFlag <- sumTotClM < sumTotLst[[idx]]
		sumTotClM[updateFlag] <- sumTotLst[[idx]][updateFlag]
	}

	return( list(summMtx=summMtxClM,summMtx.reb=summMtx.rebClM,scMtx.sz=scMtx.szClM,sumTot=sumTotClM) )
}

bUtil.getClMCnt_cutRst1Score <- function( cutRst1 ,cfgLst ,mNameGrp ,fHName=NULL ){
	# bUtil.getClM_cutRst1Score() 와 유사하나 Max가 아닌 발생 횟수를 조사함.(hName이 아닌 mName 단위)

	summMtxMin <- NULL	;summMtx.rebMin <- NULL	;scMtx.szMin <- NULL
	wind <- c("min"=0 ,"max"=0)

	summMtxLst <- list()		;summMtx.rebLst <- list()		;scMtx.szLst <- list()
	sumTotLst <- list()

	if( is.null(fHName) ){
		fHName <- names(cutRst1)
	}
	for( mName in mNameGrp ){
		for( hName in fHName ){

			summ <- cutRst1[[hName]]$basic[[mName]]$summ
			cfg <- cfgLst[[mName]]
			
			# summMtx
			if( is.null(summMtxMin) ){
				summMtxMin <- cfg$summMtx
				summMtxMin[,] <- 0
			}
			clMtx <- bUtil.closeMax_Mtx( scoreMtx=summ$summMtx ,windMtxMin=summMtxMin ,windMtxMax=cfg$summMtx )
			if( is.null(summMtxLst[[mName]]) ){
				summMtxLst[[mName]] <- clMtx>0
			} else {
				summMtxLst[[mName]] <- summMtxLst[[mName]]>0 | clMtx>0
			}

			# summMtx.reb
			if( is.null(summMtx.rebMin) ){
				summMtx.rebMin <- cfg$summMtx
				summMtx.rebMin[,] <- 0
			}
			clMtx <- bUtil.closeMax_Mtx( scoreMtx=summ$summMtx.reb ,windMtxMin=summMtx.rebMin ,windMtxMax=cfg$summMtx.reb )
			if( is.null(summMtx.rebLst[[mName]]) ){
				summMtx.rebLst[[mName]] <- clMtx>0
			} else {
				summMtx.rebLst[[mName]] <- summMtx.rebLst[[mName]]>0 | clMtx>0
			}

			# scMtx.sz
			if( is.null(scMtx.szMin) ){
				scMtx.szMin <- cfg$summMtx
				scMtx.szMin[,] <- 0
			}
			clMtx <- bUtil.closeMax_Mtx( scoreMtx=summ$scMtx.sz ,windMtxMin=scMtx.szMin ,windMtxMax=cfg$scMtx.sz )
			if( is.null(scMtx.szLst[[mName]]) ){
				scMtx.szLst[[mName]] <- clMtx>0
			} else {
				scMtx.szLst[[mName]] <- scMtx.szLst[[mName]]>0 | clMtx>0
			}



			# sumTot ----------------------------------------------------------------------
			#	FCust_H.R의 FCust_stdCut.hIdx() 함수 참고.
			sumTot <- c("lev2ClM"=0 ,"lev3ClM"=0 ,"summMtxRaw"=0 ,"summMtxEvt"=0 ,"scMtxSzRaw"=0 ,"scMtxSzEvt"=0 )
			if( TRUE ){
				botThld		<- summ$fColEvt$closeMaxDistVal - 2
				eSumLev2	<- sum(summ$fColEvt$fClMMtx[,"lev2ClM"] > botThld )
				wind["max"] <- cfg$evtMaxFColTot["lev2Max"]
				sumTot["lev2ClM"] <- bUtil.closeMax( eSumLev2 ,wind=wind )
				botThld <- summ$fColEvt$closeMaxDistVal - 2
				eSumLev3 <- sum(summ$fColEvt$fClMMtx[,"lev3ClM"] > botThld )
				wind["max"] <- cfg$evtMaxFColTot["lev3Max"]
				sumTot["lev3ClM"] <- bUtil.closeMax( eSumLev3 ,wind=wind )

				wind["max"] <- cfg$summMtx.sum["raw"]
				sumTot["summMtxRaw"] <- bUtil.closeMax( sum(summ$summMtx["raw",]) ,wind=wind )
				wind["max"] <- cfg$summMtx.sum["evt"]
				sumTot["summMtxEvt"] <- bUtil.closeMax( sum(summ$summMtx["evt",]) ,wind=wind )

				wind["max"] <- cfg$scMtx.sz.sum["rebCnt.r"]
				sumCol <- c("r.ph","r.fCol","r.dblHpnFlg")
				sumTot["scMtxSzRaw"] <- bUtil.closeMax( sum(summ$scMtx.sz["rebCnt",sumCol]) ,wind=wind )

				wind["max"] <- cfg$scMtx.sz.sum["rebCnt.e"]
				sumCol <- c("e.ph","e.fCol","e.dblHpnFlg")
				sumTot["scMtxSzEvt"] <- bUtil.closeMax( sum(summ$scMtx.sz["rebCnt",sumCol]) ,wind=wind )
			}
			if( is.null(sumTotLst[[mName]]) ){
				sumTotLst[[mName]] <- sumTot>0
			} else {
				sumTotLst[[mName]] <- sumTotLst[[mName]]>0 | sumTot>0
			}

		}
	}

	summMtxClM		<- bUtil.getMtxSumVal_fromLst( summMtxLst )
	summMtx.rebClM	<- bUtil.getMtxSumVal_fromLst( summMtx.rebLst )
	scMtx.szClM		<- bUtil.getMtxSumVal_fromLst( scMtx.szLst )
	sumTotClM		<- bUtil.getMtxSumVal_fromLst( sumTotLst )

	return( list(summMtx=summMtxClM,summMtx.reb=summMtx.rebClM,scMtx.sz=scMtx.szClM,sumTot=sumTotClM) )

}

bUtil.getMtxMaxVal_fromLst <- function( mtxLst ){
	# mtxLst <- summMtxLst
	rMtx <- NULL
	for( idx in seq_len(length(mtxLst)) ){
		if( idx==1 ){
			rMtx <- mtxLst[[1]]
			next
		}

		mtxFlag <- rMtx<mtxLst[[idx]]
		rMtx[mtxFlag] <- mtxLst[[idx]][mtxFlag]
	}
	return( rMtx )
}

bUtil.getMtxMinVal_fromLst <- function( mtxLst ){
	rMtx <- NULL
	for( idx in seq_len(length(mtxLst)) ){
		if( idx==1 ){
			rMtx <- mtxLst[[1]]
			next
		}

		mtxFlag <- rMtx>mtxLst[[idx]]
		rMtx[mtxFlag] <- mtxLst[[idx]][mtxFlag]
	}
	return( rMtx )
}

bUtil.getMtxSumVal_fromLst <- function( mtxLst ){
	mtxSum <- NULL
	for( idx in seq_len(length(mtxLst)) ){
		if( idx==1 ){
			mtxSum <- mtxLst[[idx]]
			next
		}

		mtxSum <- mtxSum + mtxLst[[idx]]
	}
	return( mtxSum )
}


bUtil.zoidMtx_ana <- function( pMtx ,banCfg=NULL ){
	# Aproject/lib/u0_H.R u0.zoidMtx_ana() ,u0.zoidCMtx_ana() ,u0.zoidFMtx_ana() 기능통합.
	#	banCfg : {NULL,"raw","rem","cStep","fStep"}
	#		banLst에 대한 후처리 작업.
	#			NULL : 후처리 안함.
	#			raw  : 1 ~ zoid gen max
	#			rem  : 0 ~ 9, 10 이상에 대한 재처리
	#			cStep : 1 이상
	#			fStep : abs() 범위.

	getSpanLeft <- function( rNum ,cNum ){	# "/"
		rLst <- list()
		if( 2>=cNum || 2>=rNum )	return( rLst )

		for( cIdx in 1:(cNum-2) ){
			spanLen <- min( rNum ,(cNum-cIdx) )
			spanMtx <- matrix( c( rNum-0:(spanLen-1) ,cIdx+1:(spanLen) )
				,byrow=T ,nrow=2
			)
			rownames(spanMtx) <- c("rSpan","cSpan")

			rLst[[as.character(cIdx)]] <- list( cIdx=cIdx ,spanMtx=spanMtx )
		}

		return( rLst )
	}
	getSpanRight <- function( rNum ,cNum ){	# "\"
		rLst <- list()
		if( 2>=cNum || 2>=rNum )	return( rLst )

		for( cIdx in 3:cNum ){
			spanLen <- min( rNum ,(cIdx-1) )
			spanMtx <- matrix( c( rNum-0:(spanLen-1) ,cIdx-1:(spanLen) )
				,byrow=T ,nrow=2
			)
			rownames(spanMtx) <- c("rSpan","cSpan")

			rLst[[as.character(cIdx)]] <- list( cIdx=cIdx ,spanMtx=spanMtx)
		}
		return( rLst )
	}
	getVal <- function( sInfo ,pMtx ){
		# sInfo : span info.
		rVal <- rep(NA,ncol(sInfo$spanMtx))
		for( idx in 1:length(rVal) ){
			rVal[idx] <- pMtx[ sInfo$spanMtx["rSpan",idx] ,sInfo$spanMtx["cSpan",idx] ]
		}
		return( rVal )
	}

	banLst <- list()
	pMtxLen	<- nrow( pMtx )
	cWidth	<- ncol( pMtx )

	for( cIdx in 1:cWidth ){
		if( 1>=pMtxLen ) break

		lst <- u0.srchStep_std( pMtx[pMtxLen:1,cIdx] )
		for( lIdx in seq_len(length(lst)) ){
			lst[[lIdx]]$tgt.col <- cIdx
			lst[[lIdx]]$tgt.dir <- "col"
		}
		banLst <- c( banLst ,lst )

		lst <- u0.srchStep_seqReb( pMtx[pMtxLen:1,cIdx] )
		for( lIdx in seq_len(length(lst)) ){
			lst[[lIdx]]$tgt.col <- cIdx
			lst[[lIdx]]$tgt.dir <- "col"
		}
		banLst <- c( banLst ,lst )

		lst <- u0.srchStep_symm( pMtx[pMtxLen:1,cIdx] )
		for( lIdx in seq_len(length(lst)) ){
			lst[[lIdx]]$tgt.col <- cIdx
			lst[[lIdx]]$tgt.dir <- "col"
		}
		banLst <- c( banLst ,lst )

		lst <- u0.srchStep_ptnReb( pMtx[pMtxLen:1,cIdx] )
		for( lIdx in seq_len(length(lst)) ){
			lst[[lIdx]]$tgt.col <- cIdx
			lst[[lIdx]]$tgt.dir <- "col"
		}
		banLst <- c( banLst ,lst )
	}

	# ana left slide /
	spanLst <- getSpanLeft( rNum=pMtxLen ,cNum=cWidth )
	for( idx in seq_len(length(spanLst)) ){
		val <- getVal( spanLst[[idx]] ,pMtx )

		lst <- u0.srchStep_std( val )
		for( lIdx in seq_len(length(lst)) ){
			lst[[lIdx]]$tgt.col <- spanLst[[idx]]$cIdx
			lst[[lIdx]]$tgt.dir <- "Slide/"
		}
		banLst <- c( banLst ,lst )

		lst <- u0.srchStep_seqReb( val )
		for( lIdx in seq_len(length(lst)) ){
			lst[[lIdx]]$tgt.col <- spanLst[[idx]]$cIdx
			lst[[lIdx]]$tgt.dir <- "Slide/"
		}
		banLst <- c( banLst ,lst )

		lst <- u0.srchStep_symm( val )
		for( lIdx in seq_len(length(lst)) ){
			lst[[lIdx]]$tgt.col <- spanLst[[idx]]$cIdx
			lst[[lIdx]]$tgt.dir <- "Slide/"
		}
		banLst <- c( banLst ,lst )

		lst <- u0.srchStep_ptnReb( val )
		for( lIdx in seq_len(length(lst)) ){
			lst[[lIdx]]$tgt.col <- spanLst[[idx]]$cIdx
			lst[[lIdx]]$tgt.dir <- "Slide/"
		}
		banLst <- c( banLst ,lst )
	}

	# ana right slide \
	spanLst <- getSpanRight( rNum=pMtxLen ,cNum=cWidth )
	for( idx in seq_len(length(spanLst)) ){
		val <- getVal( spanLst[[idx]] ,pMtx )

		lst <- u0.srchStep_std( val )
		for( lIdx in seq_len(length(lst)) ){
			lst[[lIdx]]$tgt.col <- spanLst[[idx]]$cIdx
			lst[[lIdx]]$tgt.dir <- "Slide\\"
		}
		banLst <- c( banLst ,lst )

		lst <- u0.srchStep_seqReb( val )
		for( lIdx in seq_len(length(lst)) ){
			lst[[lIdx]]$tgt.col <- spanLst[[idx]]$cIdx
			lst[[lIdx]]$tgt.dir <- "Slide\\"
		}
		banLst <- c( banLst ,lst )

		lst <- u0.srchStep_symm( val )
		for( lIdx in seq_len(length(lst)) ){
			lst[[lIdx]]$tgt.col <- spanLst[[idx]]$cIdx
			lst[[lIdx]]$tgt.dir <- "Slide\\"
		}
		banLst <- c( banLst ,lst )

		lst <- u0.srchStep_ptnReb( val )
		for( lIdx in seq_len(length(lst)) ){
			lst[[lIdx]]$tgt.col <- spanLst[[idx]]$cIdx
			lst[[lIdx]]$tgt.dir <- "Slide\\"
		}
		banLst <- c( banLst ,lst )
	}

	# for debug -------------------------------------------------------------------------
	# sapply( banLst ,function(bInfo){ sprintf("%d %s(%s)",bInfo$tgt.col,bInfo$descript,bInfo$tgt.dir) })

	banDF <- data.frame( tgt.col=integer(0) ,banVal=integer(0)
					,descript=character(0)	,tgt.dir=character(0)
	)
	if( 0 < length(banLst) ){

		banDF <- do.call( rbind ,lapply(banLst,function(ban){
			data.frame( tgt.col=ban$tgt.col ,banVal=ban$banVal 
					,descript=ban$descript	,tgt.dir=ban$tgt.dir
			)
		}))

		# 종류별 데이터 후처리
		if( is.null(banCfg) ){	# 전체 banDF 보존
		} else if( "raw"==banCfg ){
			surFlag <- (banDF$banVal>0) & (banDF$banVal<46)
			banDF <- banDF[ surFlag , ,drop=F]
		} else if( "rem"==banCfg ){
			surFlag <- (banDF$banVal>= 0) & (banDF$banVal<=10)
			# 구 버전 R에서는 data.frame의 row가 0일 때 조건식을 시도하면 에러 났었다.
			#	따라서 필터링 조건들은 미리 flag 값으로 설정하고서 data.frame 을 잘라내도록 한다. 구버전 호환을 위해..
			banDF <- banDF[ surFlag , ,drop=F]
			if( 0<nrow(banDF) ){
				banDF[ banDF$banVal==10 ,"banVal"] <- 0
			}
		} else if( "cStep"==banCfg ){
			surFlag <- (banDF$banVal>0)
			banDF <- banDF[ surFlag , ,drop=F]
		} else if( "fStep"==banCfg ){
			# abs() 범위를 설정할 필요가 있으려나..
		}
	}

	return( banDF )
}

bUtil.getMatCnt <- function( smRow ,cfgFColFreq ){

	matCnt <- c("tot"=0,"freq"=0)
	matCnt["tot"] <- sum(smRow>0,na.rm=T)
	for( nIdx in names(smRow)[smRow>0] ){
		if( !is.null(cfgFColFreq[[nIdx]]) ){
			if( smRow[nIdx] %in% cfgFColFreq[[nIdx]] ){
				matCnt["freq"] <- matCnt["freq"] + 1
			}
		}		
	}

	return( matCnt )
}

bUtil.getCfgOptions <- function( cfg ){
	# cfg : scoreMtxCfg$fCol 호환 객체.

	optLst <- list( freq=list() ,evtRebFbd=list() )	# Option List

	freqFlag <- sapply( cfg$fCol ,function(fColObj){ !is.null(fColObj$freqVal) })
	optLst$freq <- lapply( cfg$fCol[freqFlag] ,function(fColObj){fColObj$freqVal})

	forbidFlag <- sapply( cfg$fCol ,function(fColObj){ !is.null(fColObj$forbidEvtReb) } )
	optLst$evtRebFbd <- lapply( cfg$fCol[forbidFlag] ,function(fColObj){ fColObj$forbidEvtReb } )

	return( optLst )
}

#	row cutter 객체에서 사용되는 rebind check cutter.
#		lastScore 자체가 없는 경우, rowReb/evtReb 가 NULL이 된다.
bUtil.getRowRebCutter <- function( rCObj ,cfg ){
	# rCObj : row cutter obj.
	#	FCust_stdCut.rawRow() 의 rObj.

	#	ctrObj : cutter Obj
	ctrObj <- list( cfg=cfg ,rebInfo=NULL ,rawReb=NULL ,evtReb=NULL )
	ctrObj$getEvtHpnCnt <- function( evtLev ){
		#	evtLev <- ctrObj$rebInfo["evt",]
		evtHpnCnt <- c( lowE=sum(evtLev>0,na.rm=T) ,rareE=sum(evtLev>1,na.rm=T) )
		return( evtHpnCnt )
	}

	lastScore <- rCObj$lastScore
	lastEvt <- rCObj$lastEvt
	evtReb <- rCObj$evtReb	# h1,h2에서의 이벤트 중복발생 상태. (없으면 NULL)

	# ctrObj$rebInfo, flagFreqVal/flagForbidEvtReb
	flag <- sapply( cfg$fCol ,function(cObj){ !is.null(cObj$freqVal) })
	freqVal <- lapply( cfg$fCol[flag] ,function(cObj){ cObj$freqVal} )
	flag <- sapply( cfg$fCol ,function(cObj){ !is.null(cObj$forbidEvtReb) })
	forbidEvtReb <- lapply( cfg$fCol[flag] ,function(cObj){ cObj$forbidEvtReb })

	flagFreqVal <- rep( FALSE , length(lastScore) )
	names(flagFreqVal) <- names(lastScore)
	flagForbidEvtReb <- rep( FALSE , length(lastScore) )
	names(flagForbidEvtReb) <- names(lastScore)
	for( nIdx in names(lastScore) ){
		flagFreqVal[nIdx] <- lastScore[nIdx] %in% freqVal[[nIdx]]
		flagForbidEvtReb[nIdx] <- lastEvt["lev",nIdx] %in% forbidEvtReb[[nIdx]]
	}

	evtDup <- if( !is.null(evtReb) ) !is.na(evtReb$levDup) else rep(FALSE,length(lastScore))
	rebInfo <- rbind( lastScore ,lastEvt["lev",] ,evtDup ,flagFreqVal ,flagForbidEvtReb )
	rownames(rebInfo) <- c("val","evt","evtDup","freqVal","fbdEvt")
	ctrObj$rebInfo <- rebInfo

	# ctrObj$rawReb	: NULL 아니면 reb 체크 활성화.
	hpnCol <- colnames(ctrObj$rebInfo)[ctrObj$rebInfo["val",]>0]
	if( length(hpnCol) >= cfg$rowReb["rawMin"] ){
		# rawReb 체크가 필요 없는지 여부만 확인하면 된다.
		rawReb <- list( infoStr=sprintf("rReb(hpn:%d)",sum(rCObj$lastScore>0)) )
		ctrObj$rawReb <- rawReb

		if( any(ctrObj$rebInfo["fbdEvt",hpnCol]>0) ) {
			# 재발생이 금지된 Evt 가 lastScore에 포함되어 있는 상태이므로
			# 	--> rawReb 검사자체를 할 필요가 없는 상태.
			ctrObj$rawReb <- NULL
		} else if( all( ctrObj$rebInfo["freqVal",hpnCol]>0 ) ){
			# 발생이 일어난 컬럼들이 모두 빈번발생 값들인지? (freqVal)
			# 	--> rawReb 검사자체를 할 필요가 없는 상태.
			ctrObj$rawReb <- NULL
		} else if( any(ctrObj$rebInfo["evtDup",hpnCol]>0) ){
			# 2번 연속발생 Evt 존재하고
			# 때문에 rawReb를 체크할 필요자체가 없는 조건이라면..

			rebDupFlag <- !is.na(ctrObj$rebInfo["evt",]) & (ctrObj$rebInfo["evtDup",]>0)
			dupESum <- sum(ctrObj$rebInfo["evt",rebDupFlag],na.rm=T)
			if( cfg$rowReb["dupESum"] <= dupESum ){
				ctrObj$rawReb <- NULL
			}
		}

	}

	# ctrObj$evtReb	: NULL 아니면 reb 체크 활성화.
	#		evt 중복발생(evtHpnDup) 정보도 포함한다.
	if( all(is.na(ctrObj$rebInfo["evt",])) ){
		ctrObj$evtReb <- NULL
	} else {
		# forbiddenEvt 나 EvtRebDup는 aZoid/aCode 체크 시 확인해야 만 한다.
		#	lastCode와 aCode에서 매치되는 evt가 얼마나 있을 지 알 수 없으므로.
		evtReb <- list()
		ctrObj$evtReb <- evtReb
	}

	ctrObj$cut <- function( aIdx ,smRow ,evt.sm ){
		# smRow <- scoreMtx[aIdx ,]
		# evt.sm <- bFCust.getEvt(smRow,cfg$fCol)
		rebCutLst <- list( idx=aIdx ,infoStr=NULL )
		surFlag <- TRUE

		if( !is.null(ctrObj$rawReb) ){
			matFlag <- ctrObj$rebInfo["val",]==smRow
			if( all(matFlag) ){
				rebCutLst$infoStr <- c( rebCutLst$infoStr ,ctrObj$rawReb$infoStr )
				surFlag <- FALSE
			}
		}

		if( surFlag && !is.null(ctrObj$evtReb) ){
			if( any(!is.na(evt.sm["lev",])) ){

				# evtHpnCol <- ctrObj$evtReb$evtHpnCol
				levDup <- bFCust.evtComp( evt.sm["lev",] ,ctrObj$rebInfo["evt",] )$levDup

				forbidEvtCheck <- !is.na(levDup) & ctrObj$rebInfo["fbdEvt",]>0
				if( any(forbidEvtCheck) ){	# 금지된 Evt 반복은 없다.
					infoStr <- names(levDup)[forbidEvtCheck]
					infoStr <- sprintf("forbidden Evt Reb -- %s", paste(infoStr,collapse=",") )
					rebCutLst$infoStr <- c( rebCutLst$infoStr ,infoStr )
					surFlag <- FALSE
				}

				if( surFlag ){
					# 3번째 중복발생 이벤트 존재여부
					evtRebDupCheck <- !is.na(levDup) & ctrObj$rebInfo["evtDup",]>0
					if( cfg$rowReb["dupESum"]<=sum(levDup[evtRebDupCheck]) ){
						infoStr <- paste( names(levDup[evtRebDupCheck]),levDup[evtRebDupCheck],sep=":")
						infoStr <- sprintf("Evt Reb Dup -- %s", paste(infoStr,collapse=",") )
						rebCutLst$infoStr <- c( rebCutLst$infoStr ,infoStr )
						surFlag <- FALSE
					}
				}

				if( surFlag ){
					evtCnt <- ctrObj$getEvtHpnCnt(levDup)
					if( any( evtCnt>=cfg$rowReb[c("lowE","rareE")] ) ){
						evtHpn <- !is.na(levDup)
						infoStr <- paste( names(levDup[evtHpn]),levDup[evtHpn],sep=":")
						infoStr <- sprintf("Evt Reb Cnt -- %s", paste(infoStr,collapse=",") )
						rebCutLst$infoStr <- c( rebCutLst$infoStr ,infoStr )
						surFlag <- FALSE
					}
				}

			}	# if( any(!is.na(evt.sm["lev",])) )
		}

		if( 0==length(rebCutLst$infoStr) )	rebCutLst<-NULL

		return( rebCutLst )
	}

	return( ctrObj )

}



bUtil.getFarValDist <- function( hCodeMtx ,aCode ,naVal=0 ){
	rVal <- rep( naVal ,length(aCode) )

	rLen <- nrow(hCodeMtx)
	for( cIdx in 1:length(aCode) ){
		fndIdx <- which(hCodeMtx[,cIdx]==aCode[cIdx])
		fLen <- length(fndIdx)
		if( 0==fLen )	next

		rVal[cIdx] <- rLen - fndIdx[fLen] +1
	}

	return( rVal )
}
bUtil.getFarValRebMtx <- function( codeMtx ,nearDist=6 ,naVal=0 ){
	# stdMIObj <- stdMI.grp$basic[["nextZW"]]
	# codeMtx <- stdMIObj$zMtx[,2:6] - stdMIObj$zMtx[,1:5]

	dLen <- nrow( codeMtx )
	rMtx <- matrix( naVal ,ncol=ncol(codeMtx) ,nrow=nearDist )
		# stdMI$rawTail 값이 6개가 안돼더라도, rMtx는 6개로 통일하는 게 다루기 좋을 듯.

	if( nearDist >= dLen ){
		return( rMtx )
	}

	for( rIdx in 1:nearDist ){
		curRIdx <- dLen-rIdx + 1
		# aCode <- codeMtx[dLen-1+1,]		;hCodeMtx <- codeMtx[1:(dLen-1), ,drop=F]
		rMtx[nearDist-rIdx+1 ,] <- bUtil.getFarValDist( hCodeMtx=codeMtx[1:(curRIdx-1), ,drop=F]  ,aCode=codeMtx[curRIdx,] ,naVal=naVal )
	}

	return( rMtx )
}
bUtil.engineScoreFDC <- function( ){

}


bUtil.getLastHpnDist <- function( hCodeMtx ,aCode ,srchIdx=NULL ,naVal=0 ,depth=6 ){
	# aCode <- c( 8,10,13,36,37,40 )
	#	aCode의 길이는 hCodeMtx 폭과 상관이 없다.(h에서의 hpn만 체크하므로)

	rObj <- list( fndLst=list() )

	if( is.null(srchIdx) ){
		srchIdx <- nrow(hCodeMtx)
	} else if( srchIdx > nrow(hCodeMtx) ){
		return( rObj )
	}

	for( cIdx in 1:length(aCode) ){
		for( rIdx in srchIdx:1 ){
			fndIdx <- which(hCodeMtx[rIdx,]==aCode[cIdx])
			if( 0<length(fndIdx) ){
				fndInfo <- c( cIdx ,(srchIdx-rIdx+1) ,fndIdx[1] )	;names(fndInfo)<-c("originCol","dist","firstCol")
				rObj$fndLst[[as.character(cIdx)]] <- list( info=fndInfo ,fndColIdx=fndIdx )
				break
			}
		}
	}

	return( rObj )

}

bUtil.mtxEngine_LastHpnDist <- function( hCodeMtx ){

	rEngObj <- list( available=F )

	optVal <- c( "naVal"=0 ,"depth"=6 )
	rEngObj$distMtx <- matrix( optVal["naVal"] ,nrow=optVal["depth"] ,ncol=ncol(hCodeMtx) )
	rEngObj$fColMtx <- rEngObj$distMtx		# first fnd Col

	hLen <- nrow(hCodeMtx)
	if( optVal["depth"] < hLen ){
		for( rIdx in 1:optVal["depth"] ){
			fndRst <- bUtil.getLastHpnDist( hCodeMtx ,aCode=hCodeMtx[hLen-rIdx+1,] ,srchIdx=(hLen-rIdx) ,depth=optVal["depth"] )
			for( idx in seq_len(length(fndRst$fndLst)) ){
				fndInfo <- fndRst$fndLst[[idx]]$info
				rEngObj$distMtx[ optVal["depth"]-rIdx+1 ,fndInfo["originCol"] ] <- fndInfo["dist"]
				rEngObj$fColMtx[ optVal["depth"]-rIdx+1 ,fndInfo["originCol"] ] <- fndInfo["firstCol"]
			}
		}

		# rEngObj$available <- sum( rEngObj$distMtx==optVal["naVal"] )
		rEngObj$available <- any( rEngObj$distMtx==optVal["naVal"] )
	}

	return( rEngObj )

}

bUtil.getLastHpnPtn <- function( hCodeMtx ,aCode ,srchIdx=NULL ,naVal=0 ,depth=6 ,thld=c("L"=2,"C"=3) ){
	# 
	rObj <- list( fndLst=list() )

	if( is.null(srchIdx) ){
		srchIdx <- nrow(hCodeMtx)
	} else if( srchIdx > nrow(hCodeMtx) ){
		return( rObj )
	}

	for( cIdx in 1:length(aCode) ){
		fndIdx <- which(hCodeMtx[srchIdx:1,cIdx]==aCode[cIdx])
		if( 0==length(fndIdx) || 1==fndIdx[1] ) next

		rIdx <- srchIdx - fndIdx[1] + 1
		matCntL <- sum(hCodeMtx[rIdx-1,]==hCodeMtx[srchIdx,])	# before
		matCntC <- sum(hCodeMtx[rIdx,]==aCode)		# current
		if( thld["L"]<=matCntL && thld["C"]<=matCntC ){
			matInfo <- c( fndIdx[1] ,cIdx ,matCntL ,matCntC ,aCode[cIdx] )
			names(matInfo) <- c("dist","cIdx","cntL","cntC","val")
			rObj$fndLst[[as.character(cIdx)]] <- list( info=matInfo ,matFlag=(hCodeMtx[rIdx,]==aCode) )
		}
	}

	return( rObj )
}
bUtil.mtxEngine_LastHpnPtn <- function( hCodeMtx ){
	rEngObj <- list( available=F )
	return( rEngObj )
}


bUtil.uniqueCombi <- function( popLen ){

	uniqueCombi <- function( leftPosFlag ,pairLst=list() ){
		leftIdx <- which(leftPosFlag)
		leftIdx.len <- length(leftIdx)
		if( 2 > leftIdx.len ){
			#	2개 pair만 다루기로 하자. 1개, 3개 등등 다루면 너무 복잡해짐.
			return( list(pairLst) )
		}

		rLst <- list()
		for( sIdx in 2:leftIdx.len ){
			pPair <- c(leftIdx[1],leftIdx[sIdx])

			curFlag <- leftPosFlag
			curFlag[pPair] <- F
			newLst <- pairLst
			newLst[[1+length(newLst)]] <- pPair

			child <- uniqueCombi( pairLst=newLst ,leftPosFlag=curFlag )

			rLst <- append( rLst ,child )
		}

		return( rLst )
	}

	rLst <- NULL
	if( 0==(popLen%%2) ){
		combLst <- uniqueCombi( leftPosFlag=rep(T,popLen) )
		rLst <- combLst
	} else {
		# 홀수인 경우, 짝이 없이 남겨지는 1개의 값에 따른 pair를 맞춰줘야 한다.
		combLst <- uniqueCombi( leftPosFlag=rep(T,popLen-1) )

		rLst <- list()
		for( idx in 1:popLen ){
			plt <- (1:popLen)[-idx]	# pallet
			# rLst[[1+length(rLst)]] <- lapply( combLst[[idx]] ,function(comb){ plt[comb] } )
			for( cIdx in 1:length(combLst) ){
				pairLst <- lapply( combLst[[cIdx]] ,function(comb){ plt[comb] } )
				# pStr <- sapply( pairLst ,function(pVal){ paste(pVal,collapse=",") })
				rLst[[1+length(rLst)]] <- pairLst
			}
		}
	}


	for( idx in seq_len(length(rLst)) ){
		names(rLst[[idx]]) <- sapply( rLst[[idx]] ,function(pair){sprintf("%d%d",pair[1],pair[2])})
	}

	ucNames <- sapply( rLst ,function( uc ){
		pairStr <- sapply( uc ,function(pair){sprintf("%d%d",pair[1],pair[2])})
		return( paste( pairStr ,collapse="" ) )
	})
	names(rLst) <- sapply( rLst ,function(uc){ paste(names(uc),collapse="") })

	return( rLst )

}

bUtil.scoreGS <- function( val ){
	# val <- c( 8, 1, 6, 7, 2 )

	ucLst <- bUtil.uniqueCombi( length(val) )	# unique combination List
	cMtx <- combinations( length(ucLst[[1]]) ,2 )

	fndLst <- list()
	for( idx in 1:length(ucLst) ){
		uc <- ucLst[[idx]]
		for( rIdx in 1:nrow(cMtx) ){
			v1 <- sum(val[ uc[[cMtx[rIdx,1]]] ])
			v2 <- sum(val[ uc[[cMtx[rIdx,2]]] ])

			if( v1==v2 ){
				mtx <- rbind( c( uc[[cMtx[rIdx,1]]] ,val[ uc[[cMtx[rIdx,1]]] ] ,sum(val[ uc[[cMtx[rIdx,1]]] ]) )
							 ,c( uc[[cMtx[rIdx,2]]] ,val[ uc[[cMtx[rIdx,2]]] ] ,sum(val[ uc[[cMtx[rIdx,2]]] ]) )
				)
				colnames(mtx) <- c("idx1","idx2","v1","v2","sum")
				fndLst[[1+length(fndLst)]] <- mtx
			}
		}
	}

	gsObj <- list( fndLst=fndLst )
	gsObj$check <- function( code ,dbg=F ){
		# matLst[[1]]	$matInfo		sumMatFlg     valMatCnt  valMatCntIdx fndLst_RowIdx 
		# 										1             2             2             1 
		# 				$dbgStr		"fndIdx 1 sum(1,7)==sum(6,2)"
		# matLst[[2]]
		# 				$matInfo		sumMatFlg     valMatCnt  valMatCntIdx fndLst_RowIdx 
		# 										1             2             2             2 
		# 				$dbgStr		"fndIdx 2 sum(1,7)==sum(6,2)"

		matLst <- list()
		for( fIdx in seq_len(length(gsObj$fndLst)) ){
			pairMtx <- gsObj$fndLst[[fIdx]]
			v1 <- sum(code[ pairMtx[1,c("idx1","idx2")] ])
			v2 <- sum(code[ pairMtx[2,c("idx1","idx2")] ])
			if( v1==v2 ){
				matInfo <- c( sumMatFlg=0 ,valMatCnt=0 ,valMatCntIdx=0 ,fndLst_RowIdx=fIdx )

				# 합까지 일치하는 경우.
				matInfo["sumMatFlg"] <- pairMtx[1,"sum"]==v1

				cIdx1 <- pairMtx[1,c("idx1","idx2")]
				cIdx2 <- pairMtx[2,c("idx1","idx2")]

				# 동일한 pair val이 존재하는 경우.
				matInfo["valMatCnt"] <- matInfo["valMatCnt"] + all(code[ cIdx1 ]==pairMtx[1,c("v1","v2")])
				matInfo["valMatCnt"] <- matInfo["valMatCnt"] + all(code[ cIdx1 ]==pairMtx[2,c("v1","v2")])
				matInfo["valMatCnt"] <- matInfo["valMatCnt"] + all(code[ cIdx2 ]==pairMtx[1,c("v1","v2")])
				matInfo["valMatCnt"] <- matInfo["valMatCnt"] + all(code[ cIdx2 ]==pairMtx[2,c("v1","v2")])

				# 동일한 pair val에다가 idx까지 일치.
				matInfo["valMatCntIdx"] <- matInfo["valMatCntIdx"] + all(code[ cIdx1 ]==pairMtx[1,c("v1","v2")])
				matInfo["valMatCntIdx"] <- matInfo["valMatCntIdx"] + all(code[ cIdx2 ]==pairMtx[2,c("v1","v2")])

				matRst <- list( matInfo=matInfo )
				if( dbg ){
					dbgStr <- sprintf("fndLst %d sum(%d,%d)==sum(%d,%d)",fIdx,code[cIdx1["idx1"]],code[cIdx1["idx2"]],code[cIdx2["idx1"]],code[cIdx2["idx2"]])
					matRst$dbgStr <- dbgStr
				}
				matLst[[1+length(matLst)]] <- matRst
			}
		}
		return( matLst )
	}

	return( gsObj )
}

bUtil.scoreGS3 <- function( codeMtx ){
	# codeMtx <- stdMI$rawTail

	codeLen <- nrow(codeMtx)
	ucLst <- bUtil.uniqueCombi( ncol(codeMtx) )	# unique combination List

	sumHLst <- list()
	for( nIdx in names(ucLst) ){
		uc <- ucLst[[nIdx]]
		cName <- names(uc)
		sumMtx <- matrix( 0 ,nrow=2, ncol=length(uc) ,dimnames=list(c("h1","h2"),cName))

		if( 1<codeLen ){
			sumMtx["h1",cName] <- sapply(uc,function(pCol){ sum(codeMtx[codeLen,pCol]) })[cName]
		}
		if( 2<codeLen ){
			sumMtx["h2",cName] <- sapply(uc,function(pCol){ sum(codeMtx[codeLen-1,pCol]) })[cName]
		}

		sumHLst[[nIdx]] <- sumMtx
	}

	gsObj <- list( sumHLst=sumHLst ,ucLst=ucLst )
	if( 1<codeLen ){
		gsObj$h1 <- codeMtx[codeLen,]
	}
	if( 2<codeLen ){
		gsObj$h2 <- codeMtx[codeLen-1,]
	}

	gsObj$check <- function( code ,dbg=F ){
		matHLst <- list()
		for( hnIdx in c("h1","h2") ){
			valRebCntLst <- list()	# val reb 까지 발생한 pair의 수.
			for( unIdx in names(gsObj$ucLst) ){
				uc <- gsObj$ucLst[[unIdx]]
				sumVal <- sapply(uc,function(pCol){sum(code[pCol])})

				# 3개 pair sum이 모두 같다면..!!
				if( all(sumVal==gsObj$sumHLst[[unIdx]][hnIdx,]) ){
					#	sum 뿐만 아니라 val까지 reb인지 체크(순서는 상관없음.)
					#		- 순서는 상관없다.
					#		- 하나만 체크해도 됨.(나머지는 값은 뻔하니.)
					valRebFlag <- sapply(uc,function(pCol){ code[pCol][1] %in% gsObj[[hnIdx]][pCol]  })
					
					valRebCntLst[[unIdx]] <- sum( valRebFlag )
				}
			}

			matInfo <- c( matCnt=length(valRebCntLst) ,valRebMax=0 )
			if( 0<matInfo["matCnt"] ){
				matInfo["valRebMax"] <- max( sapply(valRebCntLst,function(val){val}) )

				# code 내 val이 모두 reb상태인 경우, matCnt가 너무 커질 수 있다. 관리 편의를 위해 최대값 제한.
				matInfo["matCnt"] <- ifelse( 4<length(matInfo["matCnt"]) ,4 ,matInfo["matCnt"] )
			}
			
			matHLst[[hnIdx]] <- matInfo
		}
		return( matHLst )
	}

	return( gsObj )
}

bUtil.scorePSh <- function( codeMtx ){
	# codeMtx <- stdMI$rawTail%%10	

	psObj <- list()

	cbMtx <- combinations( ncol(codeMtx) ,2 )
	rownames(cbMtx) <- apply( cbMtx ,1 ,function(rVal){ sprintf("%d%d",rVal[1],rVal[2]) })
	psObj$cbMtx <- cbMtx[sort(rownames(cbMtx)),]	# 혹시나 하는 마음에... -_-;

	psObj$getSumMtx <- function( codeMtx ){
		sumMtx <- apply( codeMtx ,1 ,function( crVal ){
			apply( psObj$cbMtx ,1 ,function(rVal){ sum(crVal[rVal]) })
		})
		return( t(sumMtx) )
	}

	psObj$chkLst <- NULL
	if( TRUE ){	# analyzer
		codeLen <- nrow(codeMtx)
		sumMtx <- psObj$getSumMtx( codeMtx )	#	sumMtx 디버그 용도로 필요할지도..
		
		chkLst <- list()
		if( 1<codeLen ){	# seq0 seq1
			# seq0	x,a,a..a
			matIdx <- which(sumMtx[codeLen,]==sumMtx[codeLen-1,])
			if( 0<length(matIdx) ){
				chkDf <- data.frame( idx=matIdx ,val=sumMtx[codeLen,matIdx] )
				descStr <- apply( sumMtx[codeLen-1:0 ,chkDf$idx,drop=F] ,2 ,function(cVal){paste(cVal,collapse=",")})
				chkDf$desc <- sprintf("[\"%s\"]~%s..%d",colnames(sumMtx)[chkDf$idx],descStr,chkDf$val)
				chkLst[["seq0"]] <- chkDf
			}

			# seq1	a1,a2..a3
			diff1 <- sumMtx[codeLen-0,]-sumMtx[codeLen-1,]
			matIdx <- which( 1==abs(diff1) )
			if( 1<length(matIdx) ){
				incVal <- diff1[matIdx]
				chkDf <- data.frame( idx=matIdx ,val=(sumMtx[codeLen,matIdx]+incVal) )
				descStr <- apply( sumMtx[codeLen-1:0 ,chkDf$idx,drop=F] ,2 ,function(cVal){paste(cVal,collapse=",")})
				chkDf$desc <- sprintf("[\"%s\"]~%s..%d",colnames(sumMtx)[chkDf$idx],descStr,chkDf$val)
				chkLst[["seq1"]] <- chkDf
			}

		}

		if( 2<codeLen ){	# seqN nSeq syc0

			# seqN	a1,a2,a3..a4
			diff1 <- sumMtx[codeLen-0,]-sumMtx[codeLen-1,]
			diff2 <- sumMtx[codeLen-1,]-sumMtx[codeLen-2,]
			matIdx <- which(diff1==diff2)
			matIdx <- matIdx[ 1<abs(diff1[matIdx]) ]	# 증감이 abs(1)인 것만
			if( 1<length(matIdx) ){
				incVal <- diff1[matIdx]
				chkDf <- data.frame( idx=matIdx ,val=(sumMtx[codeLen,matIdx]+incVal) )
				descStr <- apply( sumMtx[codeLen-2:0 ,chkDf$idx,drop=F] ,2 ,function(cVal){paste(cVal,collapse=",")})
				chkDf$desc <- sprintf("[\"%s\"]~%s..%d",colnames(sumMtx)[chkDf$idx],descStr,chkDf$val)
				chkLst[["seqN"]] <- chkDf
			}


			# nSeq	a,a,b..b
			matFlg <- sumMtx[codeLen-1,]==sumMtx[codeLen-2,]
			difFlg <- sumMtx[codeLen-0,]!=sumMtx[codeLen-1,]
			matIdx <- which( matFlg & difFlg )
			if( 1<length(matIdx) ){
				chkDf <- data.frame( idx=matIdx ,val=sumMtx[codeLen,matIdx] )
				descStr <- apply( sumMtx[codeLen-2:0 ,chkDf$idx,drop=F] ,2 ,function(cVal){paste(cVal,collapse=",")})
				chkDf$desc <- sprintf("[\"%s\"]~%s..%d",colnames(sumMtx)[chkDf$idx],descStr,chkDf$val)
				chkLst[["nSeq"]] <- chkDf
			}


			# syc0	b,a,a..b (a,a,a..a 도 포함시키자.)
			matIdx <- which( sumMtx[codeLen,]==sumMtx[codeLen-1,] )
			if( 1<length(matIdx) ){		
				chkDf <- data.frame( idx=matIdx ,val=sumMtx[codeLen-2,matIdx] )
				descStr <- apply( sumMtx[codeLen-2:0 ,chkDf$idx,drop=F] ,2 ,function(cVal){paste(cVal,collapse=",")})
				chkDf$desc <- sprintf("[\"%s\"]~%s..%d",colnames(sumMtx)[chkDf$idx],descStr,chkDf$val)
				chkLst[["syc0"]] <- chkDf
			}

		}

		if( 3<codeLen ){	# syc1
			# syc1	c,a,b,a..c
			matFlag <- sumMtx[codeLen,]==sumMtx[codeLen-2,]
			difFlag <- sumMtx[codeLen,]!=sumMtx[codeLen-1,]
			matIdx <- which( matFlag & difFlag )
			if( 1<length(matIdx) ){
				chkDf <- data.frame( idx=matIdx ,val=sumMtx[codeLen-3,matIdx] )
				descStr <- apply( sumMtx[codeLen-3:0 ,chkDf$idx,drop=F] ,2 ,function(cVal){paste(cVal,collapse=",")})
				chkDf$desc <- sprintf("[\"%s\"]~%s..%d",colnames(sumMtx)[chkDf$idx],descStr,chkDf$val)
				chkLst[["syc1"]] <- chkDf
			}
		}

		psObj$chkLst <- chkLst
	}

	psObj$check <- function( sumCode ,dbg=F ){
		#	sumCode : psObj$getSumMtx(aCodeMtx)[1,]

		# sumCode
		# 	12 13 14 15 16 23 24 25 26 34 35 36 45 46 56 
		# 	4  6  9  7  8 10 13 11 12 15 13 14 16 17 15 
		matLst <- list()
		dfCol <- if( dbg ) c("idx","val","desc") else c("idx","val")
		for( nIdx in names(psObj$chkLst) ){
			chkDf <- psObj$chkLst[[nIdx]]
				# idx val          desc
				#   7   5   ~5,14,14..5
				#   8   4   ~4,15,15..4

			chkFlg <- sumCode[chkDf$idx]==chkDf$val
			if( any(chkFlg) ){
				matLst[[nIdx]] <- chkDf[chkFlg,dfCol]
			}
		}

		return( matLst )
	}

	return( psObj )
}

bUtil.scorePSrp <- function( codeMtx ){

	psObj <- list()

	cbMtx <- combinations( ncol(codeMtx) ,2 )
	rownames(cbMtx) <- apply( cbMtx ,1 ,function(rVal){ sprintf("%d%d",rVal[1],rVal[2]) })
	psObj$cbMtx <- cbMtx[sort(rownames(cbMtx)),]	# 혹시나 하는 마음에... -_-;

	psObj$getSumMtx <- function( codeMtx ){
		sumMtx <- apply( codeMtx ,1 ,function( crVal ){
			apply( psObj$cbMtx ,1 ,function(rVal){ sum(crVal[rVal]) })
		})
		return( t(sumMtx) )
	}
	psObj$getRebPtn <- function( valOld ,valNew ){
		# valOld<-codeMtx[codeLen-1,]	;valNew<-codeMtx[codeLen,]
		val <- unique(valOld)

		rpLst <- list()		#rebPtn
		for( vIdx in val ){
			rebIdx <- which(valNew==vIdx)
			if( 0==length(rebIdx) ) next

			oldIdx <- which(valOld==vIdx)
			if( 1<length(oldIdx) ){	
				# 다수 발생이라면, valNew에서도 해당 위치의 값들이 모두 동일한지 체크.
				flag <- valNew[ oldIdx[1] ] == valNew[ oldIdx[2:length(oldIdx)] ]
				if( !all(flag) )	next		# valNew 의 oldIdx에 놓인 값들이 동일하지 않다!!
			}

			size <- max(length(rebIdx),length(oldIdx))
			rpLst[[as.character(vIdx)]] <- list( size=size ,rebIdx=rebIdx ,oldIdx=oldIdx )
			rpLst[[as.character(vIdx)]]$lastVal <- valNew
			rpLst[[as.character(vIdx)]]$valRebF <- valNew[ oldIdx[1] ] == valOld[ oldIdx[1] ]

		}

		return( rpLst )
	}

	psObj$chkLst <- NULL
	if( TRUE ){		# analyzer
		codeLen <- nrow(codeMtx)
		sumMtx <- psObj$getSumMtx( codeMtx )	#	sumMtx 디버그 용도로 필요할지도..

		chkLst <- list()
		if( 2<codeLen ){	# H2->H1   H1->h0
			chkLst[["h1"]] <- psObj$getRebPtn( valOld=sumMtx[codeLen-1,] ,valNew=sumMtx[codeLen,] )
		}
		if( 4<codeLen ){	# H4->H2   H2->h0
			chkLst[["h2"]] <- psObj$getRebPtn( valOld=sumMtx[codeLen-3,] ,valNew=sumMtx[codeLen-1,] )
		}

		psObj$chkLst <- chkLst
	}

	psObj$check <- function( sumCode ,dbg=F ){
		# sumCode <- c( 3, 3, 3, 8, 4, 7 )

		cName <- c("totCnt","totSize","valCnt","valSize")
		chkMtx <- matrix( 0 ,nrow=2,ncol=length(cName),dimnames=list(c("h1","h2"),cName))

		for( cnIdx in names(psObj$chkLst) ){
			for( nIdx in names(psObj$chkLst[[cnIdx]]) ){
				ptnObj <- psObj$chkLst[[cnIdx]][[nIdx]]

				matFlag <- sumCode[ptnObj$rebIdx] == ptnObj$lastVal[ ptnObj$oldIdx[1] ]
				if( all(matFlag) ){
					chkMtx[cnIdx,"totCnt"] <- chkMtx[cnIdx,"totCnt"] + 1
					chkMtx[cnIdx,"totSize"] <- chkMtx[cnIdx,"totSize"] + ptnObj$size
					if( ptnObj$valRebF ){	# 위치 패턴뿐만 아니라 val까지 재발된 상태
						chkMtx[cnIdx,"valCnt"] <- chkMtx[cnIdx,"valCnt"] + 1
						chkMtx[cnIdx,"valSize"] <- chkMtx[cnIdx,"valSize"] + ptnObj$size
					}
				}
			}
		}


		return( chkMtx )
	}

	return( psObj )
}

bUtil.scorePSrpRaw <- function( codeMtx ){

	psObj <- list()

	cbMtx <- combinations( ncol(codeMtx) ,2 )
	rownames(cbMtx) <- apply( cbMtx ,1 ,function(rVal){ sprintf("%d%d",rVal[1],rVal[2]) })
	psObj$cbMtx <- cbMtx[sort(rownames(cbMtx)),]	# 혹시나 하는 마음에... -_-;

	psObj$getRebPtn <- function( valOld ,valNew ){
		# valOld<-codeMtx[codeLen-1,]	;valNew<-codeMtx[codeLen,]
		val <- unique(valOld)

		rpLst <- list()		#rebPtn
		for( vIdx in val ){
			rebIdx <- which(valNew==vIdx)
			if( 0==length(rebIdx) ) next

			oldIdx <- which(valOld==vIdx)
			if( 1<length(oldIdx) ){	
				# 다수 발생이라면, valNew에서도 해당 위치의 값들이 모두 동일한지 체크.
				flag <- valNew[ oldIdx[1] ] == valNew[ oldIdx[2:length(oldIdx)] ]
				if( !all(flag) )	next		# valNew 의 oldIdx에 놓인 값들이 동일하지 않다!!
			}

			size <- max(length(rebIdx),length(oldIdx))
			rpLst[[as.character(vIdx)]] <- list( size=size ,rebIdx=rebIdx ,oldIdx=oldIdx )
			rpLst[[as.character(vIdx)]]$lastVal <- valNew
			rpLst[[as.character(vIdx)]]$valRebF <- valNew[ oldIdx[1] ] == valOld[ oldIdx[1] ]

		}

		return( rpLst )
	}

	psObj$chkLst <- NULL
	if( TRUE ){		# analyzer
		codeLen <- nrow(codeMtx)

		chkLst <- list()
		if( 2<codeLen ){	# H2->H1   H1->h0
			chkLst[["h1"]] <- psObj$getRebPtn( valOld=codeMtx[codeLen-1,] ,valNew=codeMtx[codeLen,] )
		}
		if( 4<codeLen ){	# H4->H2   H2->h0
			chkLst[["h2"]] <- psObj$getRebPtn( valOld=codeMtx[codeLen-3,] ,valNew=codeMtx[codeLen-1,] )
		}

		psObj$chkLst <- chkLst
	}

	psObj$check <- function( code ,dbg=F ){
		# code <- c( 3, 3, 3, 8, 4, 7 )

		cName <- c("totCnt","totSize","valCnt","valSize")
		chkMtx <- matrix( 0 ,nrow=2,ncol=length(cName),dimnames=list(c("h1","h2"),cName))

		for( cnIdx in names(psObj$chkLst) ){
			for( nIdx in names(psObj$chkLst[[cnIdx]]) ){
				ptnObj <- psObj$chkLst[[cnIdx]][[nIdx]]

				matFlag <- code[ptnObj$rebIdx] == ptnObj$lastVal[ ptnObj$oldIdx[1] ]
				if( all(matFlag) ){
					chkMtx[cnIdx,"totCnt"] <- chkMtx[cnIdx,"totCnt"] + 1
					chkMtx[cnIdx,"totSize"] <- chkMtx[cnIdx,"totSize"] + ptnObj$size
					if( ptnObj$valRebF ){	# 위치 패턴뿐만 아니라 val까지 재발된 상태
						chkMtx[cnIdx,"valCnt"] <- chkMtx[cnIdx,"valCnt"] + 1
						chkMtx[cnIdx,"valSize"] <- chkMtx[cnIdx,"valSize"] + ptnObj$size
					}
				}
			}
		}


		return( chkMtx )
	}

	return( psObj )
}


#-----------------------------------------------------------------------------------
#	crScrHTool	: ./worklet/HBuild_cr.R
#-----------------------------------------------------------------------------------
crScrHTool <- NULL
	# 	crScrH	: stdIdx	,std.grp	,bS.grp
	# 		std.grp/bS.grp	: raw ,summ
	# 			raw : rebMtx.ph ,evtHpnLevMtx ,phaseReb
	# 			summ : fColEvt ,summMtx ,summMtx.reb ,scMtx.sz


BUtil.makeCrScrHTool <- function(){

	sMtxHObj <- list( scrFile="./save/System/Obj_crScrH.save" )

	sMtxHObj$initData <- function( ){
		crScrH <- list( std.grp=list() ,bS.grp=list()  ,bN.grp=list() ,stdIdx=integer(0) )
		crScrH$mInfo <- list( std.grp=list() ,bS.grp=list() ,bN.grp=list() )
		save( crScrH ,file=sMtxHObj$scrFile )

		cat(sprintf("    crScrH is initiated(%s) \n",sMtxHObj$scrFile))
	}

	sMtxHObj$addData <- function( hSpan ,tgt.scMtx=NULL ){

		objName <- load(sMtxHObj$scrFile)	# crScrH
		cat(sprintf("    %s is loaded from \"%s\" \n",objName,sMtxHObj$scrFile))

		lastH <- hSpan[length(hSpan)]
		fileName <- sprintf("../Aproject/Obj_allIdxLstZ%d.save",lastH)
		cat( sprintf("    Loading %s \n",fileName) )	;load(fileName)
		fileName <- sprintf("../Aproject/save/Obj_fRstLstZ%d.save",lastH)
		cat( sprintf("    Loading %s \n",fileName) )	;load(fileName)		;names(fRstLst) <- names(allIdxLst$stdFiltedCnt)
		fileName <- sprintf("../Aproject/save/Obj_gEnvZ%d.save",lastH)
		cat( sprintf("    Loading %s \n",fileName) )	;load(fileName)

		tStmp <- Sys.time()
		sfExport("gEnv")	;sfExport("fRstLst")	;sfExport("allIdxLst")	;sfExport("tgt.scMtx")
		resultLst <- sfLapply( hSpan ,function( curHIdx ){

			idStr <- as.character(curHIdx)
			stdZoid <- gEnv$zhF[curHIdx,]
			stdIdx <- k.getIdx_AllZoidMtx( gEnv, stdZoid )


			wLastH <- curHIdx-1
			wLastSpan <- 1:which(names(fRstLst)==wLastH)
			gEnv.w <- gEnv              ;gEnv.w$zhF <- gEnv$zhF[1:wLastH,]
			allIdxLst.w <- allIdxLst    ;allIdxLst.w$stdFiltedCnt <- allIdxLst$stdFiltedCnt[wLastSpan]
										allIdxLst.w$infoMtx <- allIdxLst$infoMtx[wLastSpan,]
			fRstLst.w <- fRstLst[wLastSpan]

			curHMtxLst <- B.makeHMtxLst( gEnv.w, allIdxLst.w, fRstLst.w, tgt.scMtx )
			cut.grp <- bFCust.getFCustGrp( curHMtxLst ,tgt.scMtx )
			curStdFilted <- fRstLst[[as.character(curHIdx)]]    #   평가가 아닌 실제에선, remLst 으로부터 가져올 것.
			fHName <- bUtil.getSfcLstName( fRstLst.w[[length(fRstLst.w)]] ,curStdFiltedCnt=length(curStdFilted) ,cut.grp )

			stdMI.grp <- bUtil.getStdMILst( gEnv.w ,fRstLst.w )
			filter.grp <- getFilter.grp( stdMI.grp ,tgt.scMtx=tgt.scMtx )
			scoreMtx.grp <- getScoreMtx.grp( matrix(stdZoid,nrow=1) ,filter.grp ,makeInfoStr=T )
			std.grp <- bUtil.getCut1Score( scoreMtx.grp ,cut.grp ,fHName ,tgt.scMtx=tgt.scMtx ,deepInfo=T )	# deepInfo : sz 정보 추가

			hMtxLst_bS <- bS.makeHMtxLst( gEnv.w, allIdxLst.w, fRstLst.w ,tgt.scMtx )
			aZoidMtx <- matrix(stdZoid ,nrow=1) # Bprll.bSCut() 참고
			phVP.grp <- bS.getPhVPGrp( gEnv.w ,aZoidMtx )
			scoreMtx.grp <- bS.getScoreMtx.grp( phVP.grp ,aZoidMtx ,tgt.scMtx=tgt.scMtx )
			cut.grp <- bS.getCutGrp( hMtxLst_bS ,tgt.scMtx )
			bS.grp <- bS.getCut1Score( scoreMtx.grp ,cut.grp ,fHName ,tgt.scMtx=tgt.scMtx ,deepInfo=T )	# deepInfo : sz 정보 추가


			stdMI.grp   <- bN.getStdMILst( gEnv.w ,fRstLst.w )          ;stdMI.grp$anyWarn( )
			hMtxLst_bN	<- bN.makeHMtxLst( gEnv.w, allIdxLst.w, fRstLst.w ,tgt.scMtx )
			cut.grp     <- bN.getFCustGrp( hMtxLst_bN ,tgt.scMtx )
			filter.grp  <- bN.getFilter.grp( stdMI.grp ,tgt.scMtx=tgt.scMtx )
			scoreMtx.grp<- bN.getScoreMtx.grp( matrix(stdZoid,nrow=1) ,filter.grp ,tgt.scMtx=tgt.scMtx )

			cutRst1 <- bN.cut1( scoreMtx.grp ,cut.grp ,fHName ,tgt.scMtx=tgt.scMtx ,anaOnly=F ) 
			bN.grp <- bN.getCut1Score( scoreMtx.grp ,cut.grp ,fHName ,tgt.scMtx=tgt.scMtx ,deepInfo=T )

			return( list(idStr=idStr ,stdIdx=stdIdx,std.grp=std.grp,bS.grp=bS.grp,bN.grp=bN.grp) )
		})

		# Merge and sort
		for( idx in 1:length(resultLst) ){
			idStr <- resultLst[[idx]]$idStr
			crScrH$stdIdx[idStr]	<- resultLst[[idx]]$stdIdx
			crScrH$std.grp[[idStr]]	<- resultLst[[idx]]$std.grp$aLst[[1]]
			crScrH$bS.grp[[idStr]]	<- resultLst[[idx]]$bS.grp$aLst[[1]]
			crScrH$bN.grp[[idStr]]	<- resultLst[[idx]]$bN.grp$aLst[[1]]
		}

		idx <- as.integer(names(crScrH$std.grp))
		crScrH$stdIdx	<- crScrH$stdIdx[ order(idx) ]
		crScrH$std.grp	<- crScrH$std.grp[ order(idx) ]
		crScrH$bS.grp	<- crScrH$bS.grp[ order(idx) ]
		crScrH$bN.grp	<- crScrH$bN.grp[ order(idx) ]

		if( 0<length(crScrH$std.grp) ){	# update meta info

			mName <- names(crScrH$std.grp[[1]]$sfcLate$basic)
			pName <- colnames(crScrH$std.grp[[1]]$sfcLate$basic[[1]]$raw$rebMtx.ph)
			pName.phReb <- colnames(crScrH$std.grp[[1]]$sfcLate$basic[[1]]$raw$phaseReb)
			crScrH$mInfo$std.grp <- list( mName=mName ,pName=pName ,pName.phReb=pName.phReb )

			mName <- names(crScrH$bS.grp[[1]]$sfcLate$basic)
			pName <- colnames(crScrH$bS.grp[[1]]$sfcLate$basic[[1]]$raw$rebMtx.ph)
			pName.phReb <- colnames(crScrH$bS.grp[[1]]$sfcLate$basic[[1]]$raw$phaseReb)
			crScrH$mInfo$bS.grp <- list( mName=mName ,pName=pName ,pName.phReb=pName.phReb )

			mName <- names(crScrH$bN.grp[[1]]$sfcLate$basic)
			pName <- colnames(crScrH$bN.grp[[1]]$sfcLate$basic[[1]]$raw$rebMtx.ph)
			pName.phReb <- colnames(crScrH$bN.grp[[1]]$sfcLate$basic[[1]]$raw$phaseReb)
			crScrH$mInfo$bN.grp <- list( mName=mName ,pName=pName ,pName.phReb=pName.phReb )

		}

		save( crScrH ,file=sMtxHObj$scrFile )
		
		tDiff <- Sys.time() - tStmp
		rptStr <- sprintf("    time cost :%.1f%s \n",tDiff,units(tDiff))
		cat( rptStr )
	}

	sMtxHObj$getData <- function( ){
		load(sMtxHObj$scrFile)	# crScrH

		# missingH <- hSpan
		# cat( sprintf("    Warn! missing History : %s \n" ,paste(missingH,collapse=",")) )

		return( crScrH )
	}

	sMtxHObj$bySpan <- function( crScrH ,lastH ){
		crScrW <- crScrH

		crSpan <- as.integer(names(crScrW$stdIdx))
		if( lastH > crSpan[length(crSpan)] ){
			cat(sprintf("  Error! no data for lastH(%d) in crSpan \n",lastH ))
			return( NULL )
		}

		spanFlag <- crSpan<=lastH
		
		crScrW$stdIdx <- crScrW$stdIdx[spanFlag]
		crScrW$std.grp <- crScrW$std.grp[spanFlag]
		crScrW$bS.grp <- crScrW$bS.grp[spanFlag]

		return( crScrW )
	}

	return( sMtxHObj )

}
crScrHTool <- BUtil.makeCrScrHTool()



BUtill.buildAllScoreMtx <- function( hSpan ,gEnv ,tgt.scMtx=NULL ){

    stdScoreMtx.grp <- list()
    bSScoreMtx.grp <- list()
    stdIdxSet <- rep(0,length(hSpan))   ;names(stdIdxSet) <- hSpan
    for( curHIdx in hSpan ){    # curHIdx <- hSpan[1]
        idStr <- as.character(curHIdx)

        gEnv.w <- gEnv              ;gEnv.w$zhF <- gEnv$zhF[1:(curHIdx-1),]

        stdZoid <- gEnv$zhF[curHIdx,]
        stdIdx <- k.getIdx_AllZoidMtx( gEnv, stdZoid )
        stdIdxSet[[idStr]] <- stdIdx

        stdMI.grp <- bUtil.getStdMILst( gEnv.w ,fRstLst=NULL )
        filter.grp <- getFilter.grp( stdMI.grp ,tgt.scMtx=tgt.scMtx )
        scoreMtx.grp <- getScoreMtx.grp( matrix(stdZoid,nrow=1) ,filter.grp ,makeInfoStr=F )
        stdScoreMtx.grp[[idStr]] <- scoreMtx.grp

        aZoidMtx <- matrix(stdZoid ,nrow=1)
        phVP.grp <- bS.getPhVPGrp( gEnv.w ,aZoidMtx )
        scoreMtx.grp <- bS.getScoreMtx.grp( phVP.grp ,aZoidMtx ,tgt.scMtx=tgt.scMtx )
        bSScoreMtx.grp[[idStr]] <- scoreMtx.grp
    }

}


