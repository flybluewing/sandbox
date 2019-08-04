
#   stdCtrlCfgGrp <- bUtil.makeStdCtrlCfgGrp(hMtxLst)
bFCust.getFCustGrp <- function( stdCtrlCfgGrp ){

    rObj <- list(   sfcHLst = stdCtrlCfgGrp$sfcHLst
                    ,mtxInfoLst = stdCtrlCfgGrp$mtxInfoLst
                    ,phaseName = stdCtrlCfgGrp$phaseName        
    )

    ctrlCfgLst <- stdCtrlCfgGrp$ctrlCfgLst

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
					# 정의 내역이 있으면 정의내역을 stdLst에 붙이고
					# 없으면 bFCust.defaultStdColCutter( ... )
					fcLst[[fcName]] <- bFCust.defaultStdColCutter( ctrlCfg ,hName, mName, pName, fcName, auxInfo=c(fCol=fcName) )
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


bFCust.defaultStdColCutter <- function( ctrlCfg ,hName, mName, pName, fcName, auxInfo=c(auxInfo="") ){

	rObj <- list(	description=ctrlCfg$description
					,maxMin=ctrlCfg$maxMin ,evtVal=ctrlCfg$evtVal ,extVal=ctrlCfg$extVal 
	)
	rObj$idObj <- c( typ="stdColCut"	,hName=hName	,mName=mName	,pName=pName	,fcName=fcName	,auxInfo )

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

	fLst <- list()
	fLst[[1+length(fLst)]] <- bFCust.A_score2_A_inc.f( )
	fLst[[1+length(fLst)]] <- test_bFCust.sfcHLst_score2_A_inc.f( )

	rObj <- list( fLst=fLst )
	rObj$getCustF <- function( hName, mName, pName, fcName ){
		#	hName=""; mName=""; pName=""; fcName=""
		# fFLst <- list()	# found fLst
		# for( idx in seq_len(length(rObj$fLst)) ){

		# }

	} # rObj$getCustF()

	return( rObj )

} # bFCust.getCust()

#	[Col Cutter]
bFCust.A_score2_A_inc.f <- function(  ){

	rObj <- list( maxMin=c(max=3,min=0) ,evtVal=c(2,3) ,extVal=integer(0) )
	rObj$defId <- c( typ="cust"	,hName="*"	,mName="score2"	,pName="*"	,fcName="inc.f" )
	rObj$description <- sprintf("(cust)maxMin:%d~%d  evtVal:%s  extVal:%s  "
								,rObj$maxMin["max"]	,rObj$maxMin["min"] 
								,paste(rObj$evtVal,collapse=",") ,paste(rObj$extVal,collapse=",")
							)
	rObj$createCutter <- function( ctrlCfg ,hName, mName, pName, fcName ,auxInfo=c(auxInfo="") ){
		#	hName="testNA"; mName="testNA"; pName="testNA"; fcName="testNA"; auxInfo=c(auxInfo="")
		idObj <- rObj$defId
		if( idObj["hName"]!=hName ) idObj["hName"] <- sprintf("(%s)%s",idObj["hName"],hName)
		if( idObj["mName"]!=mName ) idObj["mName"] <- sprintf("(%s)%s",idObj["mName"],mName)
		if( idObj["pName"]!=pName ) idObj["pName"] <- sprintf("(%s)%s",idObj["pName"],pName)
		if( idObj["fcName"]!=fcName ) idObj["fcName"] <- sprintf("(%s)%s",idObj["fcName"],fcName)
		idObj <- c( idObj ,auxInfo )

		cutterObj <- rObj
		cutterObj$idObj <- idObj
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
					cutLst[[idx]] <- cutterObj$idObj
				}

			}

			rstObj <- list( surDf=surDf ,cutLst=cutLst )
			return( rstObj )

		} # cutterObj$cut()

	}

} # bFCust.A_score2_A_inc.f( )


test_bFCust.sfcHLst_score2_A_inc.f <- function(  ){

	rObj <- list( maxMin=c(max=3,min=0) ,evtVal=c(2,3) ,extVal=integer(0) )
	rObj$defId <- c( typ="cust"	,hName="sfcHLst"	,mName="score2"	,pName="*"	,fcName="inc.f" )
	rObj$description <- sprintf("(cust)maxMin:%d~%d  evtVal:%s  extVal:%s  "
								,rObj$maxMin["max"]	,rObj$maxMin["min"] 
								,paste(rObj$evtVal,collapse=",") ,paste(rObj$extVal,collapse=",")
							)
	rObj$createCutter <- function( ctrlCfg ,hName, mName, pName, fcName ,auxInfo=c(auxInfo="") ){
		#	hName="testNA"; mName="testNA"; pName="testNA"; fcName="testNA"; auxInfo=c(auxInfo="")
		idObj <- rObj$defId
		if( idObj["hName"]!=hName ) idObj["hName"] <- sprintf("(%s)%s",idObj["hName"],hName)
		if( idObj["mName"]!=mName ) idObj["mName"] <- sprintf("(%s)%s",idObj["mName"],mName)
		if( idObj["pName"]!=pName ) idObj["pName"] <- sprintf("(%s)%s",idObj["pName"],pName)
		if( idObj["fcName"]!=fcName ) idObj["fcName"] <- sprintf("(%s)%s",idObj["fcName"],fcName)
		idObj <- c( idObj ,auxInfo )

		cutterObj <- rObj
		cutterObj$idObj <- idObj
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
					cutLst[[idx]] <- cutterObj$idObj
				}

			}

			rstObj <- list( surDf=surDf ,cutLst=cutLst )
			return( rstObj )

		} # cutterObj$cut()

	}

} # bFCust.A_score2_A_inc.f( )




Sample_bFCust.A_score2_A_inc.f <- function( ctrlCfg ,hName, mName, pName, fcName ,auxInfo=c(auxInfo="") ){
	#	hName="testNA"; mName="testNA"; pName="testNA"; fcName="testNA"; auxInfo=c(auxInfo="")

	toString <- function( rObj ){
		rptStr <- sprintf("(cust)maxMin:%d~%d  evtVal:%s  extVal:%s  "
				,rObj$maxMin["max"]	,rObj$maxMin["min"] ,paste(rObj$evtVal,collapse=",") ,paste(rObj$extVal,collapse=",")
			)
		return( rptStr )
	}
	getIdObj <- function( rObj ,hName	,mName	,pName	,fcName	,auxInfo ){
		idObj <- rObj$defId
		if( idObj["hName"]!=hName ) idObj["hName"] <- sprintf("(%s)%s",idObj["hName"],hName)
		if( idObj["mName"]!=mName ) idObj["mName"] <- sprintf("(%s)%s",idObj["mName"],mName)
		if( idObj["pName"]!=pName ) idObj["pName"] <- sprintf("(%s)%s",idObj["pName"],pName)
		if( idObj["fcName"]!=fcName ) idObj["fcName"] <- sprintf("(%s)%s",idObj["fcName"],fcName)

		idObj <- c( idObj ,auxInfo )
		return( idObj )
	}

	rObj <- list( maxMin=c(max=3,min=0) ,evtVal=c(2,3) ,extVal=integer(0) )
	rObj$defId <- c( typ="cust"	,hName="sfcLst"	,mName="score2"	,pName="*"	,fcName="inc.f" )
	rObj$description <- toString(rObj)
	rObj$idObj <- getIdObj( rObj ,hName=hName	,mName=mName	,pName=pName	,fcName=fcName	,auxInfo )

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

}

