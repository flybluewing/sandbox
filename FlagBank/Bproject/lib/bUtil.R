

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
			mtxGrp <- getScoreMtx.grp_byHIdx( scoreMtx.grp )
			hIdxCut <- cut.grp$cutterLst[[hName]][[mName]]$hIdxCut
			for( aIdx in seq_len(datLen) ){
				if( !anaOnly && !surFlag[aIdx] )	next

				cutInfo <- hIdxCut$cut( mtxGrp[[mName]][[aIdx]] ,anaMode=anaOnly )
				if( 0<length(cutInfo$cRst) ){
					if( !anaOnly ){	surFlag[aIdx] <- FALSE
					} else {
						for( idx in seq_len(length(cutInfo$cRst)) ){
							idxName <- sprintf("hIdxCut_%dth",1+length(cutInfoLst))
							cutInfoLst[[idxName]] <- c( typ=names(cutInfo$cRst)[idx] ,hIdxCut$defId ,pName="ALL" ,info=cutInfo$cRst[[idx]] )
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

bUtil.getCut1Score <- function(  scoreMtx.grp ,cut.grp ,fHName ,tgt.scMtx=NULL ,logger=NULL ){

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

	aLst <- list()
	for( aIdx in seq_len(datLen) ){
		hLst <- list()
		for( hName in fHName ){ # hName <- fHName[1]
			basicLst <- list()
			for( mName in scMtxName ){ # mName <- scMtxName[1]
				#   "hIdxLst" ------------------------------------------
				mtxGrp <- getScoreMtx.grp_byHIdx( scoreMtx.grp )
				hIdxCut <- cut.grp$cutterLst[[hName]][[mName]]$hIdxCut
				rawObj <- hIdxCut$getRawScore( mtxGrp[[mName]][[aIdx]] )
				raw4Ass <- hIdxCut$getRaw4Ass( rawObj )
				summObj <- hIdxCut$getSummScore( rawObj )

				basicLst[[mName]] <- list(raw=raw4Ass ,summ=summObj)
				reportStatus( tStmp ,sprintf("[%s,%s] hIdxLst",hName,mName) ,surFlag ,logger )
			}

			bScrLst <- list()	# QQE:hold
			bScrMtxName <- character(0)
			# for( mName in bScrMtxName ){
			# 	cutLst <- cut.grp$cutterLst.bScr[[hName]][[mName]]$stdLst
			# 	scoreMtx <- scoreMtx.grp$mf[[mName]]$scoreMtx
			# 	for( cnIdx in names(cutLst) ){  # cnIdx <- names(cutLst)[1]
			# 		cuttedLst <- cutLst[[cnIdx]]$cut( scoreMtx ,!surFlag )
			# 		if( 0<length(cuttedLst) ){
			# 			if( anaOnly ){	cutInfoLst[[1+length(cutInfoLst)]] <- c( cuttedLst[[1]]$idObjDesc ,cuttedLst[[1]]$info )
			# 			} else {
			# 				cut_aIdx <- sapply( cuttedLst ,function(p){p$idx} )
			# 				surFlag[cut_aIdx] <- FALSE
			# 			}
			# 		}
			# 	}

			# 	reportStatus( tStmp ,sprintf("[%s,%s] bScrMtx",hName,mName) ,surFlag ,logger )
			# }

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

			hpnMtxRaw <- matrix( 0 ,nrow=length(basicMName) ,ncol=ncol(rawGrp$phaseHpnCnt) 
								,dimnames=list( basicMName ,colnames(rawGrp$phaseHpnCnt) )
							)
			phRebMtxRaw <- matrix( 0 ,nrow=length(basicMName) ,ncol=ncol(rawGrp$phaseRebCnt) 
								,dimnames=list( basicMName ,colnames(rawGrp$phaseRebCnt) )
							)
			rebMtxRaw <- matrix( 0 ,nrow=length(basicMName) ,ncol=ncol(rawGrp$rebMtx.ph)
								,dimnames=list( basicMName ,colnames(rawGrp$rebMtx.ph) )
							)
			hpnMtxEvt <- hpnMtxRaw
			phRebMtxEvt <- phRebMtxRaw
			rebMtxEvt <- rebMtxRaw

			for( mName in basicMName ){
				hpnMtxRaw[mName,]	<- cutRst1[[hName]]$basic[[mName]]$raw$phaseHpnCnt["raw",]
				hpnMtxEvt[mName,]	<- cutRst1[[hName]]$basic[[mName]]$raw$phaseHpnCnt["evt",]
				phRebMtxRaw[mName,]	<- cutRst1[[hName]]$basic[[mName]]$raw$phaseRebCnt["raw",]
				phRebMtxEvt[mName,]	<- cutRst1[[hName]]$basic[[mName]]$raw$phaseRebCnt["evt",]
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

bUtil.getStdMILst <- function( gEnv ,fRstLst ){

    # stdMI.basic <- fCutU.getStdMI( gEnv )

	stdMILst.basic <- list()

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

		if( rNum>=3 ){	# symmCode["abbA"]
			if( rObj$hpnCode[rNum]==rObj$hpnCode[rNum-1] ){
				rObj$symmHpn["abbA"] <- rNum-2
			}
		}

		if( rNum>=4 ){	# symmCode["abxbA"]
			if( rObj$hpnCode[rNum]==rObj$hpnCode[rNum-2] ){
				rObj$symmHpn["abxbA"] <- rNum-3
			}
		}

	}

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
