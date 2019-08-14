

#	[score3:Col Cutter] ------------------------------------------------------------------
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

#	[score3:Row Cutter] ------------------------------------------------------------------









