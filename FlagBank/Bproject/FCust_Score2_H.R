
#	[score2:Col Cutter] ------------------------------------------------------------------
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

#	c( typ="cust"	,hName="*"	,mName="score2"	,pName="*"	,fcName="rebV.r" )
bFCust.A_score2_A_rebVR <- function(  ){

	rObj <- list( maxMin=c(max=3,min=0) ,evtVal=c(2,3) ,extVal=integer(0) )
	rObj$defId <- c( typ="cust"	,hName="*"	,mName="score2"	,pName="*"	,fcName="rebV.r" )
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
} # bFCust.A_score2_A_rebVR( )

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

#	[score2:Col Cutter] ------------------------------------------------------------------
bFCust.A_score2_A_Row01 <- function(  ){
	rObj <- list( )
	rObj$defId <- c( typ="cust"	,hName="*"	,mName="score2"	,pName="*"	,rFId="Row01" )	# row filt ID
	rObj$description <- sprintf("(cust)  ")

	rObj$cutFLst <- list()
	rObj$cutFLst[[1+length(rObj$cutFLst)]] <- function( smRow ){	# smRow:scoreMtx row
		crObj <- list( cutFlag=F ,cId="01" ) # cut result object, cut Id
		return( crObj )
	} # rObj$cutFLst[1]( )
	rObj$cutFLst[[1+length(rObj$cutFLst)]] <- function( smRow ){	# for testing

		crObj <- list( cutFlag=F ,cId="Test.rebC" ) # cut result object, cut Id
		evtThld <- c("rebC.r"=2,"rebC.c"=2)

		evtFlag <- smRow[names(evtThld)] == evtThld
		if( any(evtFlag) ) crObj$cutFlag <- TRUE

		return( crObj )
	} # rObj$cutFLst[1]( )
	rObj$cutFLst[[1+length(rObj$cutFLst)]] <- function( smRow ){	# for testing

		crObj <- list( cutFlag=F ,cId="Test.rebLR" ) # cut result object, cut Id
		evtThld <- c("rebL"=1,"rebR"=1)

		evtFlag <- smRow[names(evtThld)] == evtThld
		if( any(evtFlag) ) crObj$cutFlag <- TRUE

		return( crObj )
	} # rObj$cutFLst[1]( )

	# QQE working

	rObj$createCutter <- function( tgtId=c(hName="", mName="", pName="") ,auxInfo=c(auxInfo="") ){

		cutterObj <- rObj
		cutterObj$createCutter <- NULL	;cutterObj$cutFLst <- NULL

		#	hName="testNA"; mName="testNA"; pName="testNA"; fcName="testNA"; auxInfo=c(auxInfo="")
		idObjDesc <- rObj$defId
		if( idObjDesc["hName"]!=tgtId["hName"] ) idObjDesc["hName"] <- sprintf("(%s)%s",idObjDesc["hName"],tgtId["hName"])
		if( idObjDesc["mName"]!=tgtId["mName"] ) idObjDesc["mName"] <- sprintf("(%s)%s",idObjDesc["mName"],tgtId["mName"])
		if( idObjDesc["pName"]!=tgtId["pName"] ) idObjDesc["pName"] <- sprintf("(%s)%s",idObjDesc["pName"],tgtId["pName"])
		idObjDesc <- c( idObjDesc ,auxInfo )
		cutterObj$idObjDesc <- idObjDesc

		cutterObj$idObj <- rObj$defId
		cutterObj$idObj[names(tgtId)] <- tgtId

		cutterObj$cut <- function( scoreMtx ,alreadyDead=NULL ){
			val.len <- nrow( scoreMtx )
			if( is.null(alreadyDead) ){
				alreadyDead <- rep( F, val.len )
			}

			surDf <- data.frame( surv=rep(F,val.len) ,info=rep(NA,val.len) )
			cutLst <- vector("list",val.len)
			for( idx in seq_len(val.len) ){
				if( alreadyDead[idx] ){
					surDf[idx,"surv"] <- F
					surDf[idx,"info"] <- sprintf("%d, already dead",val[idx])
					next
				}

				lst <- lapply( rObj$cutFLst ,function( pFunc ){ pFunc( scoreMtx[idx,] ) } )
				cutFlag <- sapply( lst ,function(p){ p$cutFlag })
				if( any(cutFlag) ){
					fId <- sapply( lst[cutFlag] ,function(p){p$cId})
					surDf[idx,"info"] <- sprintf("cut Id : ",paste(cId,collapse=",") )
					cutLst[[idx]] <- c( cutterObj$idObjDesc ,cutId=surDf[idx,"info"] )
				} else {
					surDf[idx,"surv"] <- T
				}

			}

			rstObj <- list( surDf=surDf ,cutLst=cutLst )
			return( rstObj )

		} # cutterObj$cut()

		return(cutterObj)

	}

	return( rObj )
} # bFCust.A_score2_A_Row01()








