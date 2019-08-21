

bUtil.cut <- function( scoreMtx.grp ,cut.grp ,fHName ,anaOnly=F ){
    #   anaOnly=T : scoreMtx[1,] 만 분석하며, 그 대신 cutting 정보를 추가한다.

    # scoreMtx.grp <- wScoreMtx.grp ;anaOnly=T
    scMtxName <- names(cut.grp$mtxInfoLst)
    datLen <- nrow(scoreMtx.grp$basic[[1]][[1]]$scoreMtx)
    cutInfoLst <- NULL
    if( anaOnly ){
        datLen <- 1
        cutInfoLst <- list()
    }

    surFlag <- rep( T ,datLen )
    for( hName in fHName ){ # hName <- fHName[1]
        for( mName in scMtxName ){ # mName <- scMtxName[1]
            #   "stdLst" -------------------------------------------
            for( pName in cut.grp$phaseName ){   # pName <- cut.grp$phaseName[1]
                cutLst <- cut.grp$cutterLst[[hName]][[mName]]$stdLst[[pName]]
                scoreMtx <- scoreMtx.grp$basic[[pName]][[mName]]$scoreMtx
                for( cnIdx in names(cutLst) ){  # cnIdx <- names(cutLst)[1]
                    cutRstObj <- cutLst[[cnIdx]]$cut( scoreMtx ,!surFlag )
                    if( anaOnly && !cutRstObj$surDf[1,"surv"] ){
                        cutInfoLst[[1+length(cutInfoLst)]] <- cutRstObj$cutLst[[1]]
                    }
                    if( !anaOnly ){
                        surFlag <- surFlag & cutRstObj$surDf[,"surv"]
                    }
                }
            }

			#   "fColLst" ------------------------------------------
			mtxGrp <- getScoreMtx.grp_byFCol( scoreMtx.grp )
            for( fcName in cut.grp$mtxInfoLst[[mName]] ){	# fcName <- cut.grp$mtxInfoLst[[mName]][1]
				cutLst <- cut.grp$cutterLst[[hName]][[mName]]$fColLst[[fcName]]
				scoreMtx <- mtxGrp[[mName]][[fcName]]
                for( cnIdx in names(cutLst) ){  # cnIdx <- names(cutLst)[1]
                    cutRstObj <- cutLst[[cnIdx]]$cut( scoreMtx ,!surFlag )
                    if( anaOnly && !cutRstObj$surDf[1,"surv"] ){
                        cutInfoLst[[1+length(cutInfoLst)]] <- cutRstObj$cutLst[[1]]
                    }
                    if( !anaOnly ){
                        surFlag <- surFlag & cutRstObj$surDf[,"surv"]
                    }
                }
			}

			#   "hIdxLst" ------------------------------------------
			mtxGrp <- getScoreMtx.grp_byHIdx( scoreMtx.grp )
			for( aIdx in seq_len(datLen) ){	# aIdx <- 1
				# cutLst <- cut.grp$cutterLst[[hName]][[mName]]$hIdxLst[[aIdx]]
				# scoreMtx <- mtxGrp[["score2"]][[aIdx]]
                # for( cnIdx in names(cutLst) ){  # cnIdx <- names(cutLst)[1]
                #     cutRstObj <- cutLst[[cnIdx]]$cut( scoreMtx ,!surFlag )
                #     if( anaOnly && !cutRstObj$surDf[1,"surv"] ){
                #         cutInfoLst[[1+length(cutInfoLst)]] <- cutRstObj$cutLst[[1]]
                #     }
                #     if( !anaOnly ){
                #         surFlag <- surFlag & cutRstObj$surDf[,"surv"]
                #     }
                # }
			}

        }
    }

    return( list( surFlag=surFlag ,cutInfoLst=cutInfoLst ) )

} # bUtil.cut()



bUtil.cutAZoidMtx <- function( gEnv ,allIdxF ,cutGrp ){
	#	cutGrp <- bFCust.getFCustGrp( stdCtrlCfgGrp )
} # bUtil.cutAZoidMtx( )


bUtil.makeStdCtrlCfgGrp <- function( hMtxLst ){

	rObj <- list( createInfo=sprintf("lastH:%d when %s",hMtxLst$lastH,Sys.time()) )

	byFCol <- B.getHMtxLst_byFCol( hMtxLst )
    byHIdx <- B.getHMtxLst_byHIdx( hMtxLst )

	#	names(hMtxLst$sfcHLst)	# "sfcLate"   "NGD0000.A"
	ctrlCfgLst <- list()
	for( hName in names(hMtxLst$sfcHLst) ){	# hName <- names(hMtxLst$sfcHLst)[1]

		mLst <- list()
		for( mName in names(hMtxLst$mtxInfoLst) ){	# mName <- names(hMtxLst$mtxInfoLst)[1]
			ctrlCfg <- list()

			pLst <- list()
			for( pName in hMtxLst$phaseName ){	# pName <- hMtxLst$phaseName[1]
				scoreMtx <- hMtxLst$scoreMtxLst[[hName]][[pName]][[mName]]$scoreMtx
				pLst[[pName]] <- bUtil.stdCtrlCfg.scoreMtx( scoreMtx )
			}
			ctrlCfg$std <- pLst

			fColLst <- list()
			for( fcName in hMtxLst$mtxInfoLst[[mName]] ){	# fcName <- hMtxLst$mtxInfoLst[[mName]][1]
				mtx <- byFCol[[hName]][[mName]][[fcName]]	# h * phase
				fColLst[[fcName]] <- bUtil.stdCtrlCfg.h_ph4FCol( mtx )
			}
			ctrlCfg$byFCol <- fColLst

			hIdxLst <- list()
			for( hIdxName in as.character(hMtxLst$sfcHLst[[hName]]) ){	# hIdxName <- as.character(hMtxLst$sfcHLst[[hName]])[1]
				mtx <- byHIdx[[hName]][[mName]][[hIdxName]]	# fCol * phase
				hIdxLst[[hIdxName]] <- bUtil.stdCtrlCfg.h_ph4FCol( mtx )
			}

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

bUtil.stdCtrlCfg.h_ph4FCol <- function( scoreMtx ){	
	# qqe work
	rObj <- list()
	return( rObj )
} # bUtil.stdCtrlCfg.h_ph4FCol()

bUtil.stdCtrlCfg.h_ph4FCol <- function( scoreMtx ){
	# qqe work
	rObj <- list()
	return( rObj )
} # bUtil.stdCtrlCfg.h_ph4FCol()

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

    return( rObj )

}	# bUtil.getStdMILst()

bUtil.getSfcLstName <- function( lastStdFilted ,curStdFilted ,cut.grp ){
	#	lastStdFilted <- fRstLst.w[[length(fRstLst.w)]]
	#	cut.grp <- bFCust.getFCustGrp( stdCtrlCfgGrp ,curHMtxLst ) 

	#	B.makeHMtxLst() 의 sfcHLst 생성 코드 참고할 것.
	fHName <- c( "sfcLate" )

	if( 0 < length(curStdFilted) ) fHName <- c( fHName  ,sprintf("sfc%d",length(curStdFilted)) )

	if( 0 < length(lastStdFilted) ) fHName <- c( fHName  ,paste("NG",lastStdFilted,sep="") )

	fHName <- intersect( fHName ,names(cut.grp$sfcHLst) )

	return( fHName )
} # bUtil.getSfcLstName()


