
#   stdCtrlCfgGrp <- bUtil.makeStdCtrlCfgGrp(hMtxLst)
bFCust.getFCustGrp <- function( stdCtrlCfgGrp ,hMtxLst ){

    rObj <- list(   sfcHLst = stdCtrlCfgGrp$sfcHLst
                    ,mtxInfoLst = stdCtrlCfgGrp$mtxInfoLst
                    ,phaseName = stdCtrlCfgGrp$phaseName        
    )

    ctrlCfgLst <- stdCtrlCfgGrp$ctrlCfgLst
	custObj <- bFCust.getCust()

	cutterLst <- list()
	for( hName in names(rObj$sfcHLst) ){	# hName <- names(rObj$sfcHLst)[1]
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
	            #   work : n개 이상 컬럼에 대한 통제가 정의되어 있으면 pLst에 추가.

				stdLst[[pName]] <- fcLst
			}

			fColLst <- list()
			# for( fcName in rObj$mtxInfoLst[[mName]] ){	# fcName <- rObj$mtxInfoLst[[mName]][1]
			# 	mtx <- byFCol[[hName]][[mName]][[fcName]]	# h * phase
			# 	fColLst[[fcName]] <- bUtil.stdCtrlCfg.h_ph4FCol( mtx )
			# }

			hIdxLst <- list()
			# for( hIdxName in as.character(rObj$sfcHLst[[hName]]) ){	# hIdxName <- as.character(rObj$sfcHLst[[hName]])[1]
			# 	mtx <- byHIdx[[hName]][[mName]][[hIdxName]]	# fCol * phase
			# 	hIdxLst[[hIdxName]] <- bUtil.stdCtrlCfg.h_ph4FCol( mtx )
			# }

            mLst[[mName]] <- list( stdLst=stdLst ,fCol=fColLst ,hIdxLst=hIdxLst)
		} # for(mName)

		cutterLst[[hName]] <- mLst
    }

    rObj$cutterLst <- cutterLst
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

		extMaxMin <- range( c(rObj$maxMin,rObj$extVal) )[2:1]

		surDf <- data.frame( surv=rep(F,val.len) ,evt=rep(NA,val.len) ,info=rep(NA,val.len) )
		cutLst <- vector("list",val.len)
		for( idx in seq_len(val.len) ){
			if( alreadyDead[idx] ){
				surDf[idx,"surv"] <- F
				surDf[idx,"info"] <- sprintf("%d, already dead",val[idx])
				next
			}

			if( val[idx] %in% rObj$evtVal )	surDf[idx,"evt"] <- val[idx]

			surDf[idx,"info"] <- sprintf("%d",val[idx])

			if( (rObj$maxMin[1]>=val[idx]) && (val[idx]>=rObj$maxMin[2]) ){ 
				surDf[idx,"surv"] <- T
			} else {
				if( (extMaxMin[1]>=val[idx]) && (val[idx]>=extMaxMin[2]) ){ 
					surDf[idx,"info"] <- sprintf("%d in ext(%d~%d)",val[idx],extMaxMin[1],extMaxMin[2]) 
				}
				cutLst[[idx]] <- rObj$idObj
			}

		}

		rstObj <- list( surDf=surDf ,cutLst=cutLst )
		return( rstObj )
	}
	return( rObj )

} # bFCust.defaultStdColCutter()


bFCust.getCust <- function(){

	fLst_1Col <- list()
	fLst_1Col[[1+length(fLst_1Col)]] <- bFCust.A_score2_A_A( )
	fLst_1Col[[1+length(fLst_1Col)]] <- bFCust.A_score2_A_rebL( )
	fLst_1Col[[1+length(fLst_1Col)]] <- bFCust.A_score2_A_rebR( )
	fLst_1Col[[1+length(fLst_1Col)]] <- bFCust.A_score3_A_A( )
	fLst_1Col[[1+length(fLst_1Col)]] <- bFCust.A_score3_A_rebPtnN( )
	fLst_1Col[[1+length(fLst_1Col)]] <- bFCust.A_score3_A_snFCntR( )

	rObj <- list( fLst_1Col=fLst_1Col )
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
		if( any(selVal==tgtId["hName"]) ){
			fFLst <- fFLst[ selVal==tgtId["hName"] ]
		}

		selVal <- sapply( fFLst ,function(fLst){ fLst$defId["mName"] })
		if( any(selVal==tgtId["mName"]) ){
			fFLst <- fFLst[ selVal==tgtId["mName"] ]
		}

		selVal <- sapply( fFLst ,function(fLst){ fLst$defId["pName"] })
		if( any(selVal==tgtId["pName"]) ){
			fFLst <- fFLst[ selVal==tgtId["pName"] ]
		}

		selVal <- sapply( fFLst ,function(fLst){ fLst$defId["fcName"] })
		if( any(selVal==tgtId["fcName"]) ){
			fFLst <- fFLst[ selVal==tgtId["fcName"] ]
		}

		#	하나만 있는 경우
		#	둘 있는 경우
		#	아무것도 없는 경우.
		return( fFLst )
	} # rObj$getCustF()

	return( rObj )

} # bFCust.getCust()

#	[Col Cutter]

#	c( typ="cust"	,hName="*"	,mName="score2"	,pName="*"	,fcName="*" )
bFCust.A_score2_A_A <- function(  ){

	rObj <- list( maxMin=c(max=2,min=0) ,evtVal=c(2) ,extVal=integer(0) )
	rObj$defId <- c( typ="cust"	,hName="*"	,mName="score2"	,pName="*"	,fcName="*" )
	rObj$description <- sprintf("(cust)maxMin:%d~%d  evtVal:%s  extVal:%s  "
								,rObj$maxMin["max"]	,rObj$maxMin["min"] 
								,paste(rObj$evtVal,collapse=",") ,paste(rObj$extVal,collapse=",")
							)
	rObj$createCutter <- function( ctrlCfg ,tgtId=c(hName="", mName="", pName="", fcName="") ,auxInfo=c(auxInfo="") ){

		cutterObj <- rObj	;cutterObj$createCutter <- NULL

		#	hName="testNA"; mName="testNA"; pName="testNA"; fcName="testNA"; auxInfo=c(auxInfo="")
		idObjDesc <- rObj$defId
		if( idObjDesc["hName"]!=tgtId["hName"] ) idObjDesc["hName"] <- sprintf("(%s)%s",idObjDesc["hName"],tgtId["hName"])
		if( idObjDesc["mName"]!=tgtId["mName"] ) idObjDesc["mName"] <- sprintf("(%s)%s",idObjDesc["mName"],tgtId["mName"])
		if( idObjDesc["pName"]!=tgtId["pName"] ) idObjDesc["pName"] <- sprintf("(%s)%s",idObjDesc["pName"],tgtId["pName"])
		if( idObjDesc["fcName"]!=tgtId["fcName"] ) idObjDesc["fcName"] <- sprintf("(%s)%s",idObjDesc["fcName"],tgtId["fcName"])
		idObjDesc <- c( idObjDesc ,auxInfo )
		cutterObj$idObjDesc <- idObjDesc

		cutterObj$idObj <- rObj$defId
		cutterObj$idObj[names(tgtId)] <- tgtId

		cutterObj$cut <- function( scoreMtx ,alreadyDead=NULL ){

			val <- scoreMtx[,cutterObj$idObj["fcName"]]
			val.len <- length( val )
			if( is.null(alreadyDead) ){
				alreadyDead <- rep( F, val.len )
			}

			extMaxMin <- range( c(cutterObj$maxMin,cutterObj$extVal) )[2:1]

			surDf <- data.frame( surv=rep(F,val.len) ,evt=rep(NA,val.len) ,info=rep(NA,val.len) )
			cutLst <- vector("list",val.len)
			for( idx in seq_len(val.len) ){
				if( alreadyDead[idx] ){
					surDf[idx,"surv"] <- F
					surDf[idx,"info"] <- sprintf("%d, already dead",val[idx])
					next
				}

				if( val[idx] %in% cutterObj$evtVal )	surDf[idx,"evt"] <- val[idx]

				surDf[idx,"info"] <- sprintf("%d",val[idx])

				if( (cutterObj$maxMin[1]>=val[idx]) && (val[idx]>=cutterObj$maxMin[2]) ){ 
					surDf[idx,"surv"] <- T
				} else {
					if( (extMaxMin[1]>=val[idx]) && (val[idx]>=extMaxMin[2]) ){ 
						surDf[idx,"info"] <- sprintf("%d in ext(%d~%d)",val[idx],extMaxMin[1],extMaxMin[2]) 
					}
					cutLst[[idx]] <- cutterObj$idObjDesc
				}

			}

			rstObj <- list( surDf=surDf ,cutLst=cutLst )
			return( rstObj )

		} # cutterObj$cut()

		return(cutterObj)
	}

	return(rObj)
} # bFCust.A_score2_A_A( )


#	c( typ="cust"	,hName="*"	,mName="score2"	,pName="*"	,fcName="rebL" )
bFCust.A_score2_A_rebL <- function(  ){

	rObj <- list( maxMin=c(max=1,min=0) ,evtVal=c(1) ,extVal=integer(0) )
	rObj$defId <- c( typ="cust"	,hName="*"	,mName="score2"	,pName="*"	,fcName="rebL" )
	rObj$description <- sprintf("(cust)maxMin:%d~%d  evtVal:%s  extVal:%s  "
								,rObj$maxMin["max"]	,rObj$maxMin["min"] 
								,paste(rObj$evtVal,collapse=",") ,paste(rObj$extVal,collapse=",")
							)
	rObj$createCutter <- function( ctrlCfg ,tgtId=c(hName="", mName="", pName="", fcName="") ,auxInfo=c(auxInfo="") ){

		cutterObj <- rObj	;cutterObj$createCutter <- NULL

		#	hName="testNA"; mName="testNA"; pName="testNA"; fcName="testNA"; auxInfo=c(auxInfo="")
		idObjDesc <- rObj$defId
		if( idObjDesc["hName"]!=tgtId["hName"] ) idObjDesc["hName"] <- sprintf("(%s)%s",idObjDesc["hName"],tgtId["hName"])
		if( idObjDesc["mName"]!=tgtId["mName"] ) idObjDesc["mName"] <- sprintf("(%s)%s",idObjDesc["mName"],tgtId["mName"])
		if( idObjDesc["pName"]!=tgtId["pName"] ) idObjDesc["pName"] <- sprintf("(%s)%s",idObjDesc["pName"],tgtId["pName"])
		if( idObjDesc["fcName"]!=tgtId["fcName"] ) idObjDesc["fcName"] <- sprintf("(%s)%s",idObjDesc["fcName"],tgtId["fcName"])
		idObjDesc <- c( idObjDesc ,auxInfo )
		cutterObj$idObjDesc <- idObjDesc

		cutterObj$idObj <- rObj$defId
		cutterObj$idObj[names(tgtId)] <- tgtId

		cutterObj$cut <- function( scoreMtx ,alreadyDead=NULL ){

			val <- scoreMtx[,cutterObj$idObj["fcName"]]
			val.len <- length( val )
			if( is.null(alreadyDead) ){
				alreadyDead <- rep( F, val.len )
			}

			extMaxMin <- range( c(cutterObj$maxMin,cutterObj$extVal) )[2:1]

			surDf <- data.frame( surv=rep(F,val.len) ,evt=rep(NA,val.len) ,info=rep(NA,val.len) )
			cutLst <- vector("list",val.len)
			for( idx in seq_len(val.len) ){
				if( alreadyDead[idx] ){
					surDf[idx,"surv"] <- F
					surDf[idx,"info"] <- sprintf("%d, already dead",val[idx])
					next
				}

				if( val[idx] %in% cutterObj$evtVal )	surDf[idx,"evt"] <- val[idx]

				surDf[idx,"info"] <- sprintf("%d",val[idx])

				if( (cutterObj$maxMin[1]>=val[idx]) && (val[idx]>=cutterObj$maxMin[2]) ){ 
					surDf[idx,"surv"] <- T
				} else {
					if( (extMaxMin[1]>=val[idx]) && (val[idx]>=extMaxMin[2]) ){ 
						surDf[idx,"info"] <- sprintf("%d in ext(%d~%d)",val[idx],extMaxMin[1],extMaxMin[2]) 
					}
					cutLst[[idx]] <- cutterObj$idObjDesc
				}

			}

			rstObj <- list( surDf=surDf ,cutLst=cutLst )
			return( rstObj )

		} # cutterObj$cut()

		return(cutterObj)
	}

	return(rObj)
} # bFCust.A_score2_A_rebL( )

#	c( typ="cust"	,hName="*"	,mName="score2"	,pName="*"	,fcName="rebR" )
bFCust.A_score2_A_rebR <- function(  ){

	rObj <- list( maxMin=c(max=1,min=0) ,evtVal=c(1) ,extVal=integer(0) )
	rObj$defId <- c( typ="cust"	,hName="*"	,mName="score2"	,pName="*"	,fcName="rebR" )
	rObj$description <- sprintf("(cust)maxMin:%d~%d  evtVal:%s  extVal:%s  "
								,rObj$maxMin["max"]	,rObj$maxMin["min"] 
								,paste(rObj$evtVal,collapse=",") ,paste(rObj$extVal,collapse=",")
							)
	rObj$createCutter <- function( ctrlCfg ,tgtId=c(hName="", mName="", pName="", fcName="") ,auxInfo=c(auxInfo="") ){

		cutterObj <- rObj	;cutterObj$createCutter <- NULL

		#	hName="testNA"; mName="testNA"; pName="testNA"; fcName="testNA"; auxInfo=c(auxInfo="")
		idObjDesc <- rObj$defId
		if( idObjDesc["hName"]!=tgtId["hName"] ) idObjDesc["hName"] <- sprintf("(%s)%s",idObjDesc["hName"],tgtId["hName"])
		if( idObjDesc["mName"]!=tgtId["mName"] ) idObjDesc["mName"] <- sprintf("(%s)%s",idObjDesc["mName"],tgtId["mName"])
		if( idObjDesc["pName"]!=tgtId["pName"] ) idObjDesc["pName"] <- sprintf("(%s)%s",idObjDesc["pName"],tgtId["pName"])
		if( idObjDesc["fcName"]!=tgtId["fcName"] ) idObjDesc["fcName"] <- sprintf("(%s)%s",idObjDesc["fcName"],tgtId["fcName"])
		idObjDesc <- c( idObjDesc ,auxInfo )
		cutterObj$idObjDesc <- idObjDesc

		cutterObj$idObj <- rObj$defId
		cutterObj$idObj[names(tgtId)] <- tgtId

		cutterObj$cut <- function( scoreMtx ,alreadyDead=NULL ){

			val <- scoreMtx[,cutterObj$idObj["fcName"]]
			val.len <- length( val )
			if( is.null(alreadyDead) ){
				alreadyDead <- rep( F, val.len )
			}

			extMaxMin <- range( c(cutterObj$maxMin,cutterObj$extVal) )[2:1]

			surDf <- data.frame( surv=rep(F,val.len) ,evt=rep(NA,val.len) ,info=rep(NA,val.len) )
			cutLst <- vector("list",val.len)
			for( idx in seq_len(val.len) ){
				if( alreadyDead[idx] ){
					surDf[idx,"surv"] <- F
					surDf[idx,"info"] <- sprintf("%d, already dead",val[idx])
					next
				}

				if( val[idx] %in% cutterObj$evtVal )	surDf[idx,"evt"] <- val[idx]

				surDf[idx,"info"] <- sprintf("%d",val[idx])

				if( (cutterObj$maxMin[1]>=val[idx]) && (val[idx]>=cutterObj$maxMin[2]) ){ 
					surDf[idx,"surv"] <- T
				} else {
					if( (extMaxMin[1]>=val[idx]) && (val[idx]>=extMaxMin[2]) ){ 
						surDf[idx,"info"] <- sprintf("%d in ext(%d~%d)",val[idx],extMaxMin[1],extMaxMin[2]) 
					}
					cutLst[[idx]] <- cutterObj$idObjDesc
				}

			}

			rstObj <- list( surDf=surDf ,cutLst=cutLst )
			return( rstObj )

		} # cutterObj$cut()

		return(cutterObj)
	}

	return(rObj)
} # bFCust.A_score2_A_rebR( )



#	c( typ="cust"	,hName="*"	,mName="score3"	,pName="*"	,fcName="*" )
bFCust.A_score3_A_A <- function(  ){

	rObj <- list( maxMin=c(max=2,min=0) ,evtVal=c(2) ,extVal=integer(0) )
	rObj$defId <- c( typ="cust"	,hName="*"	,mName="score3"	,pName="*"	,fcName="*" )
	rObj$description <- sprintf("(cust)maxMin:%d~%d  evtVal:%s  extVal:%s  "
								,rObj$maxMin["max"]	,rObj$maxMin["min"] 
								,paste(rObj$evtVal,collapse=",") ,paste(rObj$extVal,collapse=",")
							)
	rObj$createCutter <- function( ctrlCfg ,tgtId=c(hName="", mName="", pName="", fcName="") ,auxInfo=c(auxInfo="") ){

		cutterObj <- rObj	;cutterObj$createCutter <- NULL

		#	hName="testNA"; mName="testNA"; pName="testNA"; fcName="testNA"; auxInfo=c(auxInfo="")
		idObjDesc <- rObj$defId
		if( idObjDesc["hName"]!=tgtId["hName"] ) idObjDesc["hName"] <- sprintf("(%s)%s",idObjDesc["hName"],tgtId["hName"])
		if( idObjDesc["mName"]!=tgtId["mName"] ) idObjDesc["mName"] <- sprintf("(%s)%s",idObjDesc["mName"],tgtId["mName"])
		if( idObjDesc["pName"]!=tgtId["pName"] ) idObjDesc["pName"] <- sprintf("(%s)%s",idObjDesc["pName"],tgtId["pName"])
		if( idObjDesc["fcName"]!=tgtId["fcName"] ) idObjDesc["fcName"] <- sprintf("(%s)%s",idObjDesc["fcName"],tgtId["fcName"])
		idObjDesc <- c( idObjDesc ,auxInfo )
		cutterObj$idObjDesc <- idObjDesc

		cutterObj$idObj <- rObj$defId
		cutterObj$idObj[names(tgtId)] <- tgtId

		cutterObj$cut <- function( scoreMtx ,alreadyDead=NULL ){

			val <- scoreMtx[,cutterObj$idObj["fcName"]]
			val.len <- length( val )
			if( is.null(alreadyDead) ){
				alreadyDead <- rep( F, val.len )
			}

			extMaxMin <- range( c(cutterObj$maxMin,cutterObj$extVal) )[2:1]

			surDf <- data.frame( surv=rep(F,val.len) ,evt=rep(NA,val.len) ,info=rep(NA,val.len) )
			cutLst <- vector("list",val.len)
			for( idx in seq_len(val.len) ){
				if( alreadyDead[idx] ){
					surDf[idx,"surv"] <- F
					surDf[idx,"info"] <- sprintf("%d, already dead",val[idx])
					next
				}

				if( val[idx] %in% cutterObj$evtVal )	surDf[idx,"evt"] <- val[idx]

				surDf[idx,"info"] <- sprintf("%d",val[idx])

				if( (cutterObj$maxMin[1]>=val[idx]) && (val[idx]>=cutterObj$maxMin[2]) ){ 
					surDf[idx,"surv"] <- T
				} else {
					if( (extMaxMin[1]>=val[idx]) && (val[idx]>=extMaxMin[2]) ){ 
						surDf[idx,"info"] <- sprintf("%d in ext(%d~%d)",val[idx],extMaxMin[1],extMaxMin[2]) 
					}
					cutLst[[idx]] <- cutterObj$idObjDesc
				}

			}

			rstObj <- list( surDf=surDf ,cutLst=cutLst )
			return( rstObj )

		} # cutterObj$cut()

		return(cutterObj)
	}

	return(rObj)
} # bFCust.A_score3_A_A( )

#	c( typ="cust"	,hName="*"	,mName="score3"	,pName="*"	,fcName="rebPtn.n" )
bFCust.A_score3_A_rebPtnN <- function(  ){

	rObj <- list( maxMin=c(max=0,min=0) ,evtVal=integer(0) ,extVal=integer(0) )
	rObj$defId <- c( typ="cust"	,hName="*"	,mName="score3"	,pName="*"	,fcName="rebPtn.n" )
	rObj$description <- sprintf("(cust)maxMin:%d~%d  evtVal:%s  extVal:%s  "
								,rObj$maxMin["max"]	,rObj$maxMin["min"] 
								,paste(rObj$evtVal,collapse=",") ,paste(rObj$extVal,collapse=",")
							)
	rObj$createCutter <- function( ctrlCfg ,tgtId=c(hName="", mName="", pName="", fcName="") ,auxInfo=c(auxInfo="") ){

		cutterObj <- rObj	;cutterObj$createCutter <- NULL

		#	hName="testNA"; mName="testNA"; pName="testNA"; fcName="testNA"; auxInfo=c(auxInfo="")
		idObjDesc <- rObj$defId
		if( idObjDesc["hName"]!=tgtId["hName"] ) idObjDesc["hName"] <- sprintf("(%s)%s",idObjDesc["hName"],tgtId["hName"])
		if( idObjDesc["mName"]!=tgtId["mName"] ) idObjDesc["mName"] <- sprintf("(%s)%s",idObjDesc["mName"],tgtId["mName"])
		if( idObjDesc["pName"]!=tgtId["pName"] ) idObjDesc["pName"] <- sprintf("(%s)%s",idObjDesc["pName"],tgtId["pName"])
		if( idObjDesc["fcName"]!=tgtId["fcName"] ) idObjDesc["fcName"] <- sprintf("(%s)%s",idObjDesc["fcName"],tgtId["fcName"])
		idObjDesc <- c( idObjDesc ,auxInfo )
		cutterObj$idObjDesc <- idObjDesc

		cutterObj$idObj <- rObj$defId
		cutterObj$idObj[names(tgtId)] <- tgtId

		cutterObj$cut <- function( scoreMtx ,alreadyDead=NULL ){

			val <- scoreMtx[,cutterObj$idObj["fcName"]]
			val.len <- length( val )
			if( is.null(alreadyDead) ){
				alreadyDead <- rep( F, val.len )
			}

			extMaxMin <- range( c(cutterObj$maxMin,cutterObj$extVal) )[2:1]

			surDf <- data.frame( surv=rep(F,val.len) ,evt=rep(NA,val.len) ,info=rep(NA,val.len) )
			cutLst <- vector("list",val.len)
			for( idx in seq_len(val.len) ){
				if( alreadyDead[idx] ){
					surDf[idx,"surv"] <- F
					surDf[idx,"info"] <- sprintf("%d, already dead",val[idx])
					next
				}

				if( val[idx] %in% cutterObj$evtVal )	surDf[idx,"evt"] <- val[idx]

				surDf[idx,"info"] <- sprintf("%d",val[idx])

				if( (cutterObj$maxMin[1]>=val[idx]) && (val[idx]>=cutterObj$maxMin[2]) ){ 
					surDf[idx,"surv"] <- T
				} else {
					if( (extMaxMin[1]>=val[idx]) && (val[idx]>=extMaxMin[2]) ){ 
						surDf[idx,"info"] <- sprintf("%d in ext(%d~%d)",val[idx],extMaxMin[1],extMaxMin[2]) 
					}
					cutLst[[idx]] <- cutterObj$idObjDesc
				}

			}

			rstObj <- list( surDf=surDf ,cutLst=cutLst )
			return( rstObj )

		} # cutterObj$cut()

		return(cutterObj)
	}

	return(rObj)
} # bFCust.A_score3_A_rebPtnN( )

#	c( typ="cust"	,hName="*"	,mName="score3"	,pName="*"	,fcName="snFCnt.r" )
bFCust.A_score3_A_snFCntR <- function(  ){

	rObj <- list( maxMin=c(max=1,min=0) ,evtVal=integer(1) ,extVal=integer(0) )
	rObj$defId <- c( typ="cust"	,hName="*"	,mName="score3"	,pName="*"	,fcName="snFCnt.r" )
	rObj$description <- sprintf("(cust)maxMin:%d~%d  evtVal:%s  extVal:%s  "
								,rObj$maxMin["max"]	,rObj$maxMin["min"] 
								,paste(rObj$evtVal,collapse=",") ,paste(rObj$extVal,collapse=",")
							)
	rObj$createCutter <- function( ctrlCfg ,tgtId=c(hName="", mName="", pName="", fcName="") ,auxInfo=c(auxInfo="") ){

		cutterObj <- rObj	;cutterObj$createCutter <- NULL

		#	hName="testNA"; mName="testNA"; pName="testNA"; fcName="testNA"; auxInfo=c(auxInfo="")
		idObjDesc <- rObj$defId
		if( idObjDesc["hName"]!=tgtId["hName"] ) idObjDesc["hName"] <- sprintf("(%s)%s",idObjDesc["hName"],tgtId["hName"])
		if( idObjDesc["mName"]!=tgtId["mName"] ) idObjDesc["mName"] <- sprintf("(%s)%s",idObjDesc["mName"],tgtId["mName"])
		if( idObjDesc["pName"]!=tgtId["pName"] ) idObjDesc["pName"] <- sprintf("(%s)%s",idObjDesc["pName"],tgtId["pName"])
		if( idObjDesc["fcName"]!=tgtId["fcName"] ) idObjDesc["fcName"] <- sprintf("(%s)%s",idObjDesc["fcName"],tgtId["fcName"])
		idObjDesc <- c( idObjDesc ,auxInfo )
		cutterObj$idObjDesc <- idObjDesc

		cutterObj$idObj <- rObj$defId
		cutterObj$idObj[names(tgtId)] <- tgtId

		cutterObj$cut <- function( scoreMtx ,alreadyDead=NULL ){

			val <- scoreMtx[,cutterObj$idObj["fcName"]]
			val.len <- length( val )
			if( is.null(alreadyDead) ){
				alreadyDead <- rep( F, val.len )
			}

			extMaxMin <- range( c(cutterObj$maxMin,cutterObj$extVal) )[2:1]

			surDf <- data.frame( surv=rep(F,val.len) ,evt=rep(NA,val.len) ,info=rep(NA,val.len) )
			cutLst <- vector("list",val.len)
			for( idx in seq_len(val.len) ){
				if( alreadyDead[idx] ){
					surDf[idx,"surv"] <- F
					surDf[idx,"info"] <- sprintf("%d, already dead",val[idx])
					next
				}

				if( val[idx] %in% cutterObj$evtVal )	surDf[idx,"evt"] <- val[idx]

				surDf[idx,"info"] <- sprintf("%d",val[idx])

				if( (cutterObj$maxMin[1]>=val[idx]) && (val[idx]>=cutterObj$maxMin[2]) ){ 
					surDf[idx,"surv"] <- T
				} else {
					if( (extMaxMin[1]>=val[idx]) && (val[idx]>=extMaxMin[2]) ){ 
						surDf[idx,"info"] <- sprintf("%d in ext(%d~%d)",val[idx],extMaxMin[1],extMaxMin[2]) 
					}
					cutLst[[idx]] <- cutterObj$idObjDesc
				}

			}

			rstObj <- list( surDf=surDf ,cutLst=cutLst )
			return( rstObj )

		} # cutterObj$cut()

		return(cutterObj)
	}

	return(rObj)
} # bFCust.A_score3_A_snFCntR( )

