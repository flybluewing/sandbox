source("FCust_Score01_H.R")	;source("FCust_Score02_H.R")	;source("FCust_Score03_H.R")	;source("FCust_Score04_H.R")
source("FCust_Score05_H.R")	;source("FCust_Score06_H.R")	;source("FCust_Score07_H.R")	;source("FCust_Score08_H.R")
source("FCust_Score09_H.R")

source("FCust_bScr01_H.R")	;source("FCust_bScr02_H.R")


scoreEvtLst <- list()
scoreEvtLst[["score1"]] <- FCust_score1EvtLst	;scoreEvtLst[["score2"]] <- FCust_score2EvtLst
scoreEvtLst[["score3"]] <- FCust_score3EvtLst	;scoreEvtLst[["score4"]] <- FCust_score4EvtLst
scoreEvtLst[["score5"]] <- FCust_score5EvtLst	;scoreEvtLst[["score6"]] <- FCust_score6EvtLst
scoreEvtLst[["score7"]] <- FCust_score7EvtLst	;scoreEvtLst[["score8"]] <- FCust_score8EvtLst
scoreEvtLst[["score9"]] <- FCust_score9EvtLst

bScrEvtLst <- list()
bScrEvtLst[["bScr01"]] <- FCust_bScr01EvtLst	;bScrEvtLst[["bScr01"]] <- FCust_bScr01EvtLst
bScrEvtLst[["bScr02"]] <- FCust_bScr02EvtLst	;bScrEvtLst[["bScr02"]] <- FCust_bScr02EvtLst


# -----------------------------------------------------
# 추가 scoreMtx
#	- Quo 에 따른 판단.
#	- Binary ( stdZoid %% 2 )

#   stdCtrlCfgGrp <- bUtil.makeStdCtrlCfgGrp(hMtxLst)
bFCust.getFCustGrp <- function( stdCtrlCfgGrp ,hMtxLst ){

    rObj <- list(   sfcHLst = hMtxLst$sfcHLst
                    ,mtxInfoLst = hMtxLst$mtxInfoLst
					,mtxInfoLst.bScr = hMtxLst$mtxInfoLst.bScr
                    ,phaseName = hMtxLst$phaseName
    )

    ctrlCfgLst <- stdCtrlCfgGrp$ctrlCfgLst
	custObj <- bFCust.getCust()

	cutterLst <- list()
	cutterLst.bScr <- list()
	for( hName in names(rObj$sfcHLst) ){	# hName <- names(rObj$sfcHLst)[1]
		# cutterLst ------------------------------------------------------------
        mLst <- list()
        for( mName in names(rObj$mtxInfoLst) ){	# mName <- names(rObj$mtxInfoLst)[1]
            #    stdLst ,fColLst ,hIdxLst

			stdLst <- list()
			for( pName in rObj$phaseName ){	# pName <- rObj$phaseName[1]
				fcLst <- list()
                for( fcName in rObj$mtxInfoLst[[mName]] ){ # fcName <- rObj$mtxInfoLst[[mName]][1]
					ctrlCfg <- ctrlCfgLst[[hName]][[mName]][["stdLst"]][[pName]][["colDirLst"]][[fcName]]
					tgtId <- c(hName=hName, mName=mName, pName=pName, fcName=fcName)

					fLst <- custObj$getCustF_1Col( ctrlCfg ,tgtId ,auxInfo=c(fCol=fcName) )
					if( 0<length(fLst) ){
						# 일단 컬럼 하나 당 cutting 함수 하나씩이라고 가정.
						fcLst[[fcName]] <- fLst[[1]]	# fcLst[[fcName]]$idObjDesc
					} else {
						fcLst[[fcName]] <- bFCust.defaultStdColCutter( ctrlCfg ,tgtId ,auxInfo=c(fCol=fcName) )
					}

                }
				# for each row
				tgtId <- c(hName=hName, mName=mName, pName=pName)
				fcLst <- append(fcLst ,custObj$getCustF_NCol(tgtId) ) 
				fcLst <- append(fcLst ,custObj$getCustF_RReb(hMtxLst,tgtId) ) 

				stdLst[[pName]] <- fcLst
			}

			fColLst <- list()
			fColObj <- B.getHMtxLst_byFCol( hMtxLst )
			for( fcName in rObj$mtxInfoLst[[mName]] ){	# fcName <- rObj$mtxInfoLst[[mName]][1]
				lastMtx <- fColObj[[hName]][[mName]][[fcName]]	# h * phase
				tgtId <- c(hName=hName, mName=mName, fcName=fcName)
				fcLst <- list()
				fcLst <- append(fcLst ,custObj$getCustF_byFCol(lastMtx,tgtId) ) 
				fColLst[[fcName]] <- fcLst
			}

			hIdxLst <- list()
			hIdxObj <- B.getHMtxLst_byHIdx( hMtxLst )
			tgtId <- c(hName=hName, mName=mName)
			hIdxLst <- append(hIdxLst ,custObj$getCustF_byHIdx( mtxLst=hIdxObj[[hName]][[mName]], tgtId) ) 

            mLst[[mName]] <- list( stdLst=stdLst ,fColLst=fColLst ,hIdxLst=hIdxLst)
		} # for(mName)
		cutterLst[[hName]] <- mLst

		# cutterLst.bScr -------------------------------------------------------
		mLst <- list()
		for( mName in names(rObj$mtxInfoLst.bScr) ){
			stdLst <- list()
			for( fcName in rObj$mtxInfoLst.bScr[[mName]] ){ # fcName <- rObj$mtxInfoLst.bScr[[mName]][1]
				tgtId <- c(hName=hName, mName=mName, fcName=fcName)

				fLst <- custObj$getCustF_1Col.bScr( tgtId ,auxInfo=c(fCol=fcName) )

				# 일단 컬럼 하나 당 cutting 함수 하나씩이라고 가정.
				stdLst[[fcName]] <- fLst[[1]]	# stdLst[[fcName]]$idObjDesc
			}

			# for each row
			tgtId <- c(hName=hName, mName=mName )
			stdLst <- append(stdLst ,custObj$getCustF_NCol.bScr(tgtId) ) 
			stdLst <- append(stdLst ,custObj$getCustF_RReb.bScr(hMtxLst,tgtId) ) 

			mLst[[mName]] <- list( stdLst=stdLst )
		}
		cutterLst.bScr[[hName]] <- mLst
    }

    rObj$cutterLst <- cutterLst
    rObj$cutterLst.bScr <- cutterLst.bScr	# mf 와 동일
    return( rObj )

} # bFCust.getFCustGrp( )


bFCust.defaultStdColCutter <- function( ctrlCfg ,tgtId=c(hName="", mName="", pName="", fcName=""), auxInfo=c(auxInfo="") ){

	rObj <- list(	description=ctrlCfg$description
					,maxMin=ctrlCfg$maxMin ,evtVal=ctrlCfg$evtVal ,extVal=ctrlCfg$extVal 
	)
	rObj$idObj <- c( typ="stdColCut"	,tgtId	,auxInfo )

	rObj$cut <- function( scoreMtx ,alreadyDead=NULL ){

		val <- scoreMtx[,rObj$idObj["fcName"]]
		val.len <- length( val )
		if( is.null(alreadyDead) ){
			alreadyDead <- rep( F, val.len )
		}

		cutLst <- list()
		for( idx in seq_len(val.len) ){
			if( alreadyDead[idx] )	next

			if( !bUtil.in(val[idx],c(min=rObj$maxMin[2],max=rObj$maxMin[1])) ){
				infoStr <- c(info=sprintf("val:%d",val[idx]))
				cutLst[[1+length(cutLst)]] <- list( idx=idx ,idObjDesc=rObj$idObjDesc ,info=infoStr )
			}
		}

		return( cutLst )
	}

	return( rObj )

} # bFCust.defaultStdColCutter()


bFCust.getCust <- function(){

	rObj <- list()

	rObj$fLst_1Col <- list()	# check each col value in one row
	if( TRUE ){
		rObj$fLst_1Col[[1+length(rObj$fLst_1Col)]] <- bFCust.A_score1_A_A( )

		rObj$fLst_1Col[[1+length(rObj$fLst_1Col)]] <- bFCust.A_score2_A_A( )
		# rObj$fLst_1Col[[1+length(rObj$fLst_1Col)]] <- bFCust.A_score2_A_rebVR( )
		# rObj$fLst_1Col[[1+length(rObj$fLst_1Col)]] <- bFCust.A_score2_A_rebL( )
		# rObj$fLst_1Col[[1+length(rObj$fLst_1Col)]] <- bFCust.A_score2_A_rebR( )

		rObj$fLst_1Col[[1+length(rObj$fLst_1Col)]] <- bFCust.A_score3_A_A( )
		rObj$fLst_1Col[[1+length(rObj$fLst_1Col)]] <- bFCust.A_score4_A_A( )
		rObj$fLst_1Col[[1+length(rObj$fLst_1Col)]] <- bFCust.A_score5_A_A( )
		rObj$fLst_1Col[[1+length(rObj$fLst_1Col)]] <- bFCust.A_score6_A_A( )
		rObj$fLst_1Col[[1+length(rObj$fLst_1Col)]] <- bFCust.A_score7_A_A( )
		rObj$fLst_1Col[[1+length(rObj$fLst_1Col)]] <- bFCust.A_score8_A_A( )
		rObj$fLst_1Col[[1+length(rObj$fLst_1Col)]] <- bFCust.A_score9_A_A( )

	}
	rObj$getCustF_1Col <- function( ctrlCfg ,tgtId=c(hName="", mName="", pName="", fcName="") ,auxInfo=c(auxInfo="") ){
		# tgtId=c(hName="sfcHLst", mName="score2", pName="basic", fcName="inc.f")
		fFLst <- list()	# found fLst
		for( idx in seq_len(length(rObj$fLst_1Col)) ){
			fF <- rObj$fLst_1Col[[idx]]$createCutter( ctrlCfg ,tgtId )
			if( !(fF$defId["hName"]=="*" || fF$defId["hName"]==tgtId["hName"]) ) next
			if( !(fF$defId["mName"]=="*" || fF$defId["mName"]==tgtId["mName"]) ) next
			if( !(fF$defId["pName"]=="*" || fF$defId["pName"]==tgtId["pName"]) ) next
			if( !(fF$defId["fcName"]=="*" || fF$defId["fcName"]==tgtId["fcName"]) ) next

			fFLst[[1+length(fFLst)]] <- fF
		}

		#	fFLst에서 우선순위 밀리는 "*" 설정을 제외한다.
		#		- 값이 정확히 지정된 FLst가 있다면, "*" 설정들은 배제.
		#		- 우선순위는 hName, mName, pName, fcName

		selVal <- sapply( fFLst ,function(fLst){ fLst$defId["hName"] })
		if( any(selVal==tgtId["hName"]) )	fFLst <- fFLst[ selVal==tgtId["hName"] ]

		selVal <- sapply( fFLst ,function(fLst){ fLst$defId["mName"] })
		if( any(selVal==tgtId["mName"]) )	fFLst <- fFLst[ selVal==tgtId["mName"] ]

		selVal <- sapply( fFLst ,function(fLst){ fLst$defId["pName"] })
		if( any(selVal==tgtId["pName"]) )	fFLst <- fFLst[ selVal==tgtId["pName"] ]

		selVal <- sapply( fFLst ,function(fLst){ fLst$defId["fcName"] })
		if( any(selVal==tgtId["fcName"]) )	fFLst <- fFLst[ selVal==tgtId["fcName"] ]

		return( fFLst )
	} # rObj$getCustF()

	rObj$fLst_NCol <- list()	# check by row
	if( TRUE ){
		rObj$fLst_NCol[[1+length(rObj$fLst_NCol)]] <- bFCust.A_score1_A_Row01()
		rObj$fLst_NCol[[1+length(rObj$fLst_NCol)]] <- bFCust.A_score2_A_Row01()
		rObj$fLst_NCol[[1+length(rObj$fLst_NCol)]] <- bFCust.A_score3_A_Row01()
		rObj$fLst_NCol[[1+length(rObj$fLst_NCol)]] <- bFCust.A_score4_A_Row01()
		rObj$fLst_NCol[[1+length(rObj$fLst_NCol)]] <- bFCust.A_score5_A_Row01()
		rObj$fLst_NCol[[1+length(rObj$fLst_NCol)]] <- bFCust.A_score6_A_Row01()
		rObj$fLst_NCol[[1+length(rObj$fLst_NCol)]] <- bFCust.A_score7_A_Row01()

		# rObj$fLst_NCol[[1+length(rObj$fLst_NCol)]] <- bFCust.A_score8_A_Row01()
		# rObj$fLst_NCol[[1+length(rObj$fLst_NCol)]] <- bFCust.A_score9_A_Row01()

	}
	rObj$getCustF_NCol <- function( tgtId=c(hName="", mName="", pName="") ,auxInfo=c(auxInfo="") ){
		# tgtId=c(hName="sfcHLst", mName="score2", pName="basic")
		fFLst <- list()	# found fLst
		for( idx in seq_len(length(rObj$fLst_NCol)) ){
			fF <- rObj$fLst_NCol[[idx]]$createCutter( tgtId ,auxInfo )
			if( !(fF$defId["hName"]=="*" || fF$defId["hName"]==tgtId["hName"]) ) next
			if( !(fF$defId["mName"]=="*" || fF$defId["mName"]==tgtId["mName"]) ) next
			if( !(fF$defId["pName"]=="*" || fF$defId["pName"]==tgtId["pName"]) ) next

			fFLst[[1+length(fFLst)]] <- fF
		}
		if( 0==length(fFLst) ) return( fFLst )

		selVal <- sapply( fFLst ,function(fLst){ fLst$defId["hName"] })
		if( any(selVal==tgtId["hName"]) )	fFLst <- fFLst[ selVal==tgtId["hName"] ]

		selVal <- sapply( fFLst ,function(fLst){ fLst$defId["mName"] })
		if( any(selVal==tgtId["mName"]) )	fFLst <- fFLst[ selVal==tgtId["mName"] ]

		selVal <- sapply( fFLst ,function(fLst){ fLst$defId["pName"] })
		if( any(selVal==tgtId["pName"]) )	fFLst <- fFLst[ selVal==tgtId["pName"] ]

		names( fFLst ) <- paste("NCol",1:length(fFLst),sep="")
		return( fFLst )
	} # rObj$getCustF_NCol()

	rObj$fLst_rReb <- list()
	if( TRUE ){

		rObj$fLst_rReb[[1+length(rObj$fLst_rReb)]] <- bFCust.A_score1_A_rReb01()
		rObj$fLst_rReb[[1+length(rObj$fLst_rReb)]] <- bFCust.A_score1_A_rRebAA()

		rObj$fLst_rReb[[1+length(rObj$fLst_rReb)]] <- bFCust.A_score2_A_rReb01()
		rObj$fLst_rReb[[1+length(rObj$fLst_rReb)]] <- bFCust.A_score2_A_rRebAA()
		rObj$fLst_rReb[[1+length(rObj$fLst_rReb)]] <- bFCust.A_score3_A_rReb01()
		rObj$fLst_rReb[[1+length(rObj$fLst_rReb)]] <- bFCust.A_score3_A_rRebAA()

		rObj$fLst_rReb[[1+length(rObj$fLst_rReb)]] <- bFCust.A_score4_A_rReb01()
		rObj$fLst_rReb[[1+length(rObj$fLst_rReb)]] <- bFCust.A_score4_A_rRebAA()
		rObj$fLst_rReb[[1+length(rObj$fLst_rReb)]] <- bFCust.A_score5_A_rReb01()
		rObj$fLst_rReb[[1+length(rObj$fLst_rReb)]] <- bFCust.A_score5_A_rRebAA()
		rObj$fLst_rReb[[1+length(rObj$fLst_rReb)]] <- bFCust.A_score6_A_rReb01()
		rObj$fLst_rReb[[1+length(rObj$fLst_rReb)]] <- bFCust.A_score6_A_rRebAA()
		rObj$fLst_rReb[[1+length(rObj$fLst_rReb)]] <- bFCust.A_score7_A_rReb01()
		rObj$fLst_rReb[[1+length(rObj$fLst_rReb)]] <- bFCust.A_score7_A_rRebAA()

		# rObj$fLst_rReb[[1+length(rObj$fLst_rReb)]] <- bFCust.A_score8_A_rReb01()
		# rObj$fLst_rReb[[1+length(rObj$fLst_rReb)]] <- bFCust.A_score8_A_rRebAA()

		# rObj$fLst_rReb[[1+length(rObj$fLst_rReb)]] <- bFCust.A_score9_A_rReb01()
		# rObj$fLst_rReb[[1+length(rObj$fLst_rReb)]] <- bFCust.A_score9_A_rRebAA()

	}
	rObj$getCustF_RReb <- function( lastMtx ,tgtId=c(hName="", mName="", pName="") ,auxInfo=c(auxInfo="") ){
		# tgtId=c(hName="sfcHLst", mName="score2", pName="basic")
		fFLst <- list()	# found fLst
		for( idx in seq_len(length(rObj$fLst_rReb)) ){
			fF <- rObj$fLst_rReb[[idx]]$createCutter( lastMtx ,tgtId ,auxInfo )
			if( !(fF$defId["hName"]=="*" || fF$defId["hName"]==tgtId["hName"]) ) next
			if( !(fF$defId["mName"]=="*" || fF$defId["mName"]==tgtId["mName"]) ) next
			if( !(fF$defId["pName"]=="*" || fF$defId["pName"]==tgtId["pName"]) ) next

			fFLst[[1+length(fFLst)]] <- fF
		}
		if( 0==length(fFLst) ) return( fFLst )

		selVal <- sapply( fFLst ,function(fLst){ fLst$defId["hName"] })
		if( any(selVal==tgtId["hName"]) )	fFLst <- fFLst[ selVal==tgtId["hName"] ]

		selVal <- sapply( fFLst ,function(fLst){ fLst$defId["mName"] })
		if( any(selVal==tgtId["mName"]) )	fFLst <- fFLst[ selVal==tgtId["mName"] ]

		selVal <- sapply( fFLst ,function(fLst){ fLst$defId["pName"] })
		if( any(selVal==tgtId["pName"]) )	fFLst <- fFLst[ selVal==tgtId["pName"] ]

		names( fFLst ) <- paste("RReb",1:length(fFLst),sep="")
		return( fFLst )
	} # rObj$getCustF_RReb()

	rObj$fLst_byFCol <- list()
	if( TRUE ){

		rObj$fLst_byFCol[[1+length(rObj$fLst_byFCol)]] <- bFCust.byFCol_A_score1_A()
		rObj$fLst_byFCol[[1+length(rObj$fLst_byFCol)]] <- bFCust.byFCol_A_score1_A_rReb01()
		rObj$fLst_byFCol[[1+length(rObj$fLst_byFCol)]] <- bFCust.byFCol_A_score1_A_rRebAA()

		rObj$fLst_byFCol[[1+length(rObj$fLst_byFCol)]] <- bFCust.byFCol_A_score2_A()
		rObj$fLst_byFCol[[1+length(rObj$fLst_byFCol)]] <- bFCust.byFCol_A_score2_A_rReb01()
		rObj$fLst_byFCol[[1+length(rObj$fLst_byFCol)]] <- bFCust.byFCol_A_score2_A_rRebAA()

		rObj$fLst_byFCol[[1+length(rObj$fLst_byFCol)]] <- bFCust.byFCol_A_score3_A()
		rObj$fLst_byFCol[[1+length(rObj$fLst_byFCol)]] <- bFCust.byFCol_A_score3_A_rReb01()
		rObj$fLst_byFCol[[1+length(rObj$fLst_byFCol)]] <- bFCust.byFCol_A_score3_A_rRebAA()

		rObj$fLst_byFCol[[1+length(rObj$fLst_byFCol)]] <- bFCust.byFCol_A_score4_A()
		rObj$fLst_byFCol[[1+length(rObj$fLst_byFCol)]] <- bFCust.byFCol_A_score4_A_rReb01()
		rObj$fLst_byFCol[[1+length(rObj$fLst_byFCol)]] <- bFCust.byFCol_A_score4_A_rRebAA()

		rObj$fLst_byFCol[[1+length(rObj$fLst_byFCol)]] <- bFCust.byFCol_A_score5_A()
		rObj$fLst_byFCol[[1+length(rObj$fLst_byFCol)]] <- bFCust.byFCol_A_score5_A_rReb01()
		rObj$fLst_byFCol[[1+length(rObj$fLst_byFCol)]] <- bFCust.byFCol_A_score5_A_rRebAA()

		rObj$fLst_byFCol[[1+length(rObj$fLst_byFCol)]] <- bFCust.byFCol_A_score6_A()
		rObj$fLst_byFCol[[1+length(rObj$fLst_byFCol)]] <- bFCust.byFCol_A_score6_A_rReb01()
		rObj$fLst_byFCol[[1+length(rObj$fLst_byFCol)]] <- bFCust.byFCol_A_score6_A_rRebAA()

		rObj$fLst_byFCol[[1+length(rObj$fLst_byFCol)]] <- bFCust.byFCol_A_score7_A()
		rObj$fLst_byFCol[[1+length(rObj$fLst_byFCol)]] <- bFCust.byFCol_A_score7_A_rReb01()
		rObj$fLst_byFCol[[1+length(rObj$fLst_byFCol)]] <- bFCust.byFCol_A_score7_A_rRebAA()

		# rObj$fLst_byFCol[[1+length(rObj$fLst_byFCol)]] <- bFCust.byFCol_A_score8_A()
		# rObj$fLst_byFCol[[1+length(rObj$fLst_byFCol)]] <- bFCust.byFCol_A_score8_A_rReb01()
		# rObj$fLst_byFCol[[1+length(rObj$fLst_byFCol)]] <- bFCust.byFCol_A_score8_A_rRebAA()

		# rObj$fLst_byFCol[[1+length(rObj$fLst_byFCol)]] <- bFCust.byFCol_A_score9_A()
		# rObj$fLst_byFCol[[1+length(rObj$fLst_byFCol)]] <- bFCust.byFCol_A_score9_A_rReb01()
		# rObj$fLst_byFCol[[1+length(rObj$fLst_byFCol)]] <- bFCust.byFCol_A_score9_A_rRebAA()

	}
	rObj$getCustF_byFCol <- function( lastMtx ,tgtId=c(hName="", mName="", fcName="") ,auxInfo=c(auxInfo="") ){
		# tgtId=c(hName="sfcHLst", mName="score2", fcName="rebV.r")
		fFLst <- list()	# found fLst
		for( idx in seq_len(length(rObj$fLst_byFCol)) ){
			fF <- rObj$fLst_byFCol[[idx]]$createCutter( lastMtx ,tgtId ,auxInfo )
			if( !(fF$defId["hName"]=="*" || fF$defId["hName"]==tgtId["hName"]) ) next
			if( !(fF$defId["mName"]=="*" || fF$defId["mName"]==tgtId["mName"]) ) next
			if( !(fF$defId["fcName"]=="*" || fF$defId["fcName"]==tgtId["fcName"]) ) next

			fFLst[[1+length(fFLst)]] <- fF
		}
		if( 0==length(fFLst) ) return( fFLst )

		selVal <- sapply( fFLst ,function(fLst){ fLst$defId["hName"] })
		if( any(selVal==tgtId["hName"]) )	fFLst <- fFLst[ selVal==tgtId["hName"] ]

		selVal <- sapply( fFLst ,function(fLst){ fLst$defId["mName"] })
		if( any(selVal==tgtId["mName"]) )	fFLst <- fFLst[ selVal==tgtId["mName"] ]

		selVal <- sapply( fFLst ,function(fLst){ fLst$defId["fcName"] })
		if( any(selVal==tgtId["fcName"]) )	fFLst <- fFLst[ selVal==tgtId["fcName"] ]

		names( fFLst ) <- paste("byFCol",1:length(fFLst),sep="")
		return( fFLst )
	} # rObj$getCustF_byFCol()

	rObj$fLst_byHIdx <- list()
	if( TRUE ){
		rObj$fLst_byHIdx[[1+length(rObj$fLst_byHIdx)]] <- bFCust.byHIdx_A_score1()
		rObj$fLst_byHIdx[[1+length(rObj$fLst_byHIdx)]] <- bFCust.byHIdx_A_score2()
		rObj$fLst_byHIdx[[1+length(rObj$fLst_byHIdx)]] <- bFCust.byHIdx_A_score3()
		rObj$fLst_byHIdx[[1+length(rObj$fLst_byHIdx)]] <- bFCust.byHIdx_A_score4()
		rObj$fLst_byHIdx[[1+length(rObj$fLst_byHIdx)]] <- bFCust.byHIdx_A_score5()
		rObj$fLst_byHIdx[[1+length(rObj$fLst_byHIdx)]] <- bFCust.byHIdx_A_score6()
		rObj$fLst_byHIdx[[1+length(rObj$fLst_byHIdx)]] <- bFCust.byHIdx_A_score7()
		# rObj$fLst_byHIdx[[1+length(rObj$fLst_byHIdx)]] <- bFCust.byHIdx_A_score8()
		# rObj$fLst_byHIdx[[1+length(rObj$fLst_byHIdx)]] <- bFCust.byHIdx_A_score9()
	}
	rObj$getCustF_byHIdx <- function( mtxLst ,tgtId=c(hName="", mName="") ,auxInfo=c(auxInfo="") ){
		fFLst <- list()	# found fLst
		for( idx in seq_len(length(rObj$fLst_byHIdx)) ){
			fF <- rObj$fLst_byHIdx[[idx]]$createCutter( mtxLst ,tgtId ,auxInfo )
			if( !(fF$defId["hName"]=="*" || fF$defId["hName"]==tgtId["hName"]) ) next
			if( !(fF$defId["mName"]=="*" || fF$defId["mName"]==tgtId["mName"]) ) next
			fFLst[[1+length(fFLst)]] <- fF
		}
		if( 0==length(fFLst) ) return( fFLst )

		selVal <- sapply( fFLst ,function(fLst){ fLst$defId["hName"] })
		if( any(selVal==tgtId["hName"]) )	fFLst <- fFLst[ selVal==tgtId["hName"] ]

		selVal <- sapply( fFLst ,function(fLst){ fLst$defId["mName"] })
		if( any(selVal==tgtId["mName"]) )	fFLst <- fFLst[ selVal==tgtId["mName"] ]

		names( fFLst ) <- paste("byHIdx",1:length(fFLst),sep="")
		return( fFLst )
	} # rObj$getCustF_byHIdx()


	# ---------------------------------------------------------------------------------------------
	#	bScr
	rObj$fLst_1Col.bScr <- list()	# check each col value in one row
	if( TRUE ){
		rObj$fLst_1Col.bScr[[1+length(rObj$fLst_1Col.bScr)]] <- bFCust.A_bScr01_A_A( )
		rObj$fLst_1Col.bScr[[1+length(rObj$fLst_1Col.bScr)]] <- bFCust.A_bScr02_A_A( )
	}
	rObj$getCustF_1Col.bScr <- function( tgtId=c(hName="", mName="", fcName="") ,auxInfo=c(auxInfo="") ){
		# tgtId=c(hName="sfcHLst", mName="score2", fcName="inc.f")
		fFLst <- list()	# found fLst
		for( idx in seq_len(length(rObj$fLst_1Col.bScr)) ){
			fF <- rObj$fLst_1Col.bScr[[idx]]$createCutter( tgtId )
			if( !(fF$defId["hName"]=="*" || fF$defId["hName"]==tgtId["hName"]) ) next
			if( !(fF$defId["mName"]=="*" || fF$defId["mName"]==tgtId["mName"]) ) next
			if( !(fF$defId["fcName"]=="*" || fF$defId["fcName"]==tgtId["fcName"]) ) next

			fFLst[[1+length(fFLst)]] <- fF
		}

		#	fFLst에서 우선순위 밀리는 "*" 설정을 제외한다.
		#		- 값이 정확히 지정된 FLst가 있다면, "*" 설정들은 배제.
		#		- 우선순위는 hName, mName, pName, fcName

		selVal <- sapply( fFLst ,function(fLst){ fLst$defId["hName"] })
		if( any(selVal==tgtId["hName"]) )	fFLst <- fFLst[ selVal==tgtId["hName"] ]

		selVal <- sapply( fFLst ,function(fLst){ fLst$defId["mName"] })
		if( any(selVal==tgtId["mName"]) )	fFLst <- fFLst[ selVal==tgtId["mName"] ]

		selVal <- sapply( fFLst ,function(fLst){ fLst$defId["fcName"] })
		if( any(selVal==tgtId["fcName"]) )	fFLst <- fFLst[ selVal==tgtId["fcName"] ]
		
		return( fFLst )
	}

	rObj$fLst_NCol.bScr <- list()	# check by row
	if( TRUE ){
		rObj$fLst_NCol.bScr[[1+length(rObj$fLst_NCol.bScr)]] <- bFCust.A_bScr01_A_Row01()
		rObj$fLst_NCol.bScr[[1+length(rObj$fLst_NCol.bScr)]] <- bFCust.A_bScr02_A_Row01()
    }
	rObj$getCustF_NCol.bScr <- function( tgtId=c(hName="", mName="" ) ,auxInfo=c(auxInfo="") ){
		# tgtId=c(hName="sfcHLst", mName="score2")
		fFLst <- list()	# found fLst
		for( idx in seq_len(length(rObj$fLst_NCol.bScr)) ){
			fF <- rObj$fLst_NCol.bScr[[idx]]$createCutter( tgtId ,auxInfo )
			if( !(fF$defId["hName"]=="*" || fF$defId["hName"]==tgtId["hName"]) ) next
			if( !(fF$defId["mName"]=="*" || fF$defId["mName"]==tgtId["mName"]) ) next
			fFLst[[1+length(fFLst)]] <- fF
		}
		if( 0==length(fFLst) ) return( fFLst )

		selVal <- sapply( fFLst ,function(fLst){ fLst$defId["hName"] })
		if( any(selVal==tgtId["hName"]) )	fFLst <- fFLst[ selVal==tgtId["hName"] ]

		selVal <- sapply( fFLst ,function(fLst){ fLst$defId["mName"] })
		if( any(selVal==tgtId["mName"]) )	fFLst <- fFLst[ selVal==tgtId["mName"] ]

		names( fFLst ) <- paste("NCol",1:length(fFLst),sep="")
		return( fFLst )
	}

	rObj$fLst_rReb.bScr <- list()
	if( TRUE ){
		rObj$fLst_rReb.bScr[[1+length(rObj$fLst_rReb.bScr)]] <- bFCust.A_bScr01_A_rReb01()
		rObj$fLst_rReb.bScr[[1+length(rObj$fLst_rReb.bScr)]] <- bFCust.A_bScr01_A_rRebAA()

		rObj$fLst_rReb.bScr[[1+length(rObj$fLst_rReb.bScr)]] <- bFCust.A_bScr02_A_rReb01()
		rObj$fLst_rReb.bScr[[1+length(rObj$fLst_rReb.bScr)]] <- bFCust.A_bScr02_A_rRebAA()
    }
	rObj$getCustF_RReb.bScr <- function( lastMtx ,tgtId=c(hName="", mName="" ) ,auxInfo=c(auxInfo="") ){
		# tgtId=c(hName="sfcHLst", mName="score2", pName="basic")
		fFLst <- list()	# found fLst
		for( idx in seq_len(length(rObj$fLst_rReb.bScr)) ){
			fF <- rObj$fLst_rReb.bScr[[idx]]$createCutter( lastMtx ,tgtId ,auxInfo )
			if( !(fF$defId["hName"]=="*" || fF$defId["hName"]==tgtId["hName"]) ) next
			if( !(fF$defId["mName"]=="*" || fF$defId["mName"]==tgtId["mName"]) ) next

			fFLst[[1+length(fFLst)]] <- fF
		}
		if( 0==length(fFLst) ) return( fFLst )

		selVal <- sapply( fFLst ,function(fLst){ fLst$defId["hName"] })
		if( any(selVal==tgtId["hName"]) )	fFLst <- fFLst[ selVal==tgtId["hName"] ]

		selVal <- sapply( fFLst ,function(fLst){ fLst$defId["mName"] })
		if( any(selVal==tgtId["mName"]) )	fFLst <- fFLst[ selVal==tgtId["mName"] ]

		names( fFLst ) <- paste("RReb",1:length(fFLst),sep="")
		return( fFLst )

	}

	return( rObj )

} # bFCust.getCust()
