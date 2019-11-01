

bUtil.cut <- function( scoreMtx.grp ,cut.grp ,fHName ,tgt.scMtx=NULL ,anaOnly=F ,logger=NULL ){
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
            #   "stdLst" -------------------------------------------
            for( pName in cut.grp$phaseName ){   # pName <- cut.grp$phaseName[1]
                cutLst <- cut.grp$cutterLst[[hName]][[mName]]$stdLst[[pName]]
                scoreMtx <- scoreMtx.grp$basic[[pName]][[mName]]$scoreMtx
                for( cnIdx in names(cutLst) ){  # cnIdx <- names(cutLst)[1]
                    cuttedLst <- cutLst[[cnIdx]]$cut( scoreMtx ,!surFlag )
					if( 0<length(cuttedLst) ){
						if( anaOnly ){	cutInfoLst[[1+length(cutInfoLst)]] <- c( cuttedLst[[1]]$idObjDesc ,cuttedLst[[1]]$info )
						} else {
							cut_aIdx <- sapply( cuttedLst ,function(p){p$idx} )
	                        surFlag[cut_aIdx] <- FALSE
						}
					}
                }
				reportStatus( tStmp ,sprintf("     %s",pName) ,surFlag ,logger )
            }
			reportStatus( tStmp ,sprintf("[%s,%s] stdLst",hName,mName) ,surFlag ,logger )

			#   "fColLst" ------------------------------------------
			mtxGrp <- getScoreMtx.grp_byFCol( scoreMtx.grp )
            for( fcName in cut.grp$mtxInfoLst[[mName]] ){	# fcName <- cut.grp$mtxInfoLst[[mName]][1]
				cutLst <- cut.grp$cutterLst[[hName]][[mName]]$fColLst[[fcName]]
				scoreMtx <- mtxGrp[[mName]][[fcName]]
                for( cnIdx in names(cutLst) ){  # cnIdx <- names(cutLst)[1]
                    cuttedLst <- cutLst[[cnIdx]]$cut( scoreMtx ,!surFlag )
					if( 0<length(cuttedLst) ){
						if( anaOnly ){	cutInfoLst[[1+length(cutInfoLst)]] <- c( cuttedLst[[1]]$idObjDesc ,cuttedLst[[1]]$info )
						} else {
							cut_aIdx <- sapply( cuttedLst ,function(p){p$idx} )
	                        surFlag[cut_aIdx] <- FALSE
						}
					}
                }
			}
			reportStatus( tStmp ,sprintf("[%s,%s] fColLst",hName,mName) ,surFlag ,logger )

			#   "hIdxLst" ------------------------------------------
			mtxGrp <- getScoreMtx.grp_byHIdx( scoreMtx.grp )
			for( aIdx in seq_len(datLen) ){	# aIdx <- 1
				# 이전과 달리, 1개 aZoid에 대한 처리임을 주의.
				if( !surFlag[aIdx] && !anaOnly ) next

				cutLst <- cut.grp$cutterLst[[hName]][[mName]]$hIdxLst
				scoreMtx <- mtxGrp[[mName]][[aIdx]]
                for( cnIdx in names(cutLst) ){  # cnIdx <- names(cutLst)[1]
                    cuttedLst <- cutLst[[cnIdx]]$cut( scoreMtx ,aIdx )
					if( 0<length(cuttedLst) ){
						if( anaOnly ){	cutInfoLst[[1+length(cutInfoLst)]] <- c( cuttedLst[[1]]$idObjDesc ,cuttedLst[[1]]$info )
						} else {
	                        surFlag[aIdx] <- FALSE
						}
					}
                }
			}
			reportStatus( tStmp ,sprintf("[%s,%s] hIdxLst",hName,mName) ,surFlag ,logger )

        }

		for( mName in bScrMtxName ){
			cutLst <- cut.grp$cutterLst.bScr[[hName]][[mName]]$stdLst
			scoreMtx <- scoreMtx.grp$mf[[mName]]$scoreMtx
			for( cnIdx in names(cutLst) ){  # cnIdx <- names(cutLst)[1]
				cuttedLst <- cutLst[[cnIdx]]$cut( scoreMtx ,!surFlag )
				if( 0<length(cuttedLst) ){
					if( anaOnly ){	cutInfoLst[[1+length(cutInfoLst)]] <- c( cuttedLst[[1]]$idObjDesc ,cuttedLst[[1]]$info )
					} else {
						cut_aIdx <- sapply( cuttedLst ,function(p){p$idx} )
						surFlag[cut_aIdx] <- FALSE
					}
				}
			}

			reportStatus( tStmp ,sprintf("[%s,%s] bScrMtx",hName,mName) ,surFlag ,logger )			
		}

    }

    return( list( surFlag=surFlag ,cutInfoLst=cutInfoLst ) )

} # bUtil.cut()



bUtil.cutAZoidMtx <- function( gEnv ,allIdxF ,cutGrp ){
	#	cutGrp <- bFCust.getFCustGrp( stdCtrlCfgGrp )
} # bUtil.cutAZoidMtx( )


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


