FCust_score2EvtLst <- list("rebV.r"=c(2,3) ,"rebL"=1 ,"rebR"=1
								,"rebC.r"=2 ,"rebC.c"=2 ,"rebC.f"=2 ,"rebC2.r"=2 ,"rebC2.c"=2 ,"rebC2.f"=2 
								,"inc.r"=2 ,"inc.c"=2 ,"inc.f"=2 ,"inc.r2"=2 ,"inc.c2"=2 ,"inc.f2"=2 
								,"inc.r3"=2 ,"inc.c3"=2
							)

#	[score2:Col Cutter(1 col)] ------------------------------------------------------------------
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
			cutLst <- list()
			for( idx in seq_len(val.len) ){
				if( alreadyDead[idx] )	next

				if( !bUtil.in(val[idx],cutterObj$maxMin) ){
					infoStr <- c(info=sprintf("val:%d",val[idx]))
					cutLst[[1+length(cutLst)]] <- list( idx=idx ,idObjDesc=cutterObj$idObjDesc ,info=infoStr )
				}
			}

			return( cutLst )
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

			cutLst <- list()
			for( idx in seq_len(val.len) ){
				if( alreadyDead[idx] )	next

				if( !bUtil.in(val[idx],cutterObj$maxMin) ){
					infoStr <- c(info=sprintf("val:%d",val[idx]))
					cutLst[[1+length(cutLst)]] <- list( idx=idx ,idObjDesc=cutterObj$idObjDesc ,info=infoStr )
				}
			}

			return( cutLst )
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

			cutLst <- list()
			for( idx in seq_len(val.len) ){
				if( alreadyDead[idx] )	next

				if( !bUtil.in(val[idx],cutterObj$maxMin) ){
					infoStr <- c(info=sprintf("val:%d",val[idx]))
					cutLst[[1+length(cutLst)]] <- list( idx=idx ,idObjDesc=cutterObj$idObjDesc ,info=infoStr )
				}
			}

			return( cutLst )
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
			if( is.null(alreadyDead) )	alreadyDead <- rep( F, val.len )

			cutLst <- list()
			for( idx in seq_len(val.len) ){
				if( alreadyDead[idx] )	next

				if( !bUtil.in(val[idx],cutterObj$maxMin) ){
					infoStr <- c(info=sprintf("val:%d",val[idx]))
					cutLst[[1+length(cutLst)]] <- list( idx=idx ,idObjDesc=cutterObj$idObjDesc ,info=infoStr )
				}
			}

			return( cutLst )
		} # cutterObj$cut()

		return(cutterObj)
	}

	return(rObj)
} # bFCust.A_score2_A_rebR( )

#	[score2:Col Cutter(N col)] ------------------------------------------------------------------
bFCust.A_score2_A_Row01 <- function(  ){
	rObj <- list( )
	rObj$defId <- c( typ="cust_NCol"	,hName="*"	,mName="score2"	,pName="*"	,rFId="Row01" )	# row filt ID
	rObj$description <- sprintf("(cust)  ")

	rObj$evtLst <- FCust_score2EvtLst

	rObj$cutFLst <- list()
	# Sample code ================================================================
	rObj$cutFLst[[1+length(rObj$cutFLst)]] <- function( smRow ,evt ){
		crObj <- list( cutFlag=F ,cId="evtNum" ) # cut result object, cut Id
		if( 4<=sum(!is.na(evt)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- sprintf( "%s  %d" ,crObj$cId ,sum(!is.na(evt)) )
		}
		return( crObj )
	} # rObj$cutFLst[1]( )
	rObj$cutFLst[[1+length(rObj$cutFLst)]] <- function( smRow ,evt ){
		crObj <- list( cutFlag=F ,cId="hpnOne" ) # cut result object, cut Id
		if( 0<sum(smRow==1) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- sprintf( "%s  %d" ,crObj$cId ,sum(smRow==1) )
		}
		return( crObj )
	} # rObj$cutFLst[1]( )

	rObj$createCutter <- function( tgtId=c(hName="", mName="", pName="") ,auxInfo=c(auxInfo="") ){

		cutterObj <- rObj
		cutterObj$createCutter <- NULL

		#	hName="testNA"; mName="testNA"; pName="testNA"; fcName="testNA"; auxInfo=c(auxInfo="")
		idObjDesc <- rObj$defId
		if( idObjDesc["hName"]!=tgtId["hName"] ) idObjDesc["hName"] <- sprintf("(%s)%s",idObjDesc["hName"],tgtId["hName"])
		if( idObjDesc["mName"]!=tgtId["mName"] ) idObjDesc["mName"] <- sprintf("(%s)%s",idObjDesc["mName"],tgtId["mName"])
		if( idObjDesc["pName"]!=tgtId["pName"] ) idObjDesc["pName"] <- sprintf("(%s)%s",idObjDesc["pName"],tgtId["pName"])
		idObjDesc <- c( idObjDesc ,auxInfo )
		cutterObj$idObjDesc <- idObjDesc

		cutterObj$idObj <- rObj$defId
		cutterObj$idObj[names(tgtId)] <- tgtId

		cutterObj$getEvtVal <- function( src ,evtLst ){
			evtVal <- src[names(evtLst)]
			for( nIdx in names(evtLst) ){
				if( !(evtVal[nIdx] %in% evtLst[[nIdx]]) ) evtVal[nIdx] <- NA
			}
			return( evtVal )
		} # cutterObj$getLastVal()


		cutterObj$cut <- function( scoreMtx ,alreadyDead=NULL ){
			val.len <- nrow( scoreMtx )
			if( is.null(alreadyDead) )	alreadyDead <- rep( F, val.len )

			cutLst <- list()
			for( idx in seq_len(val.len) ){
				if( alreadyDead[idx] ) next

				smRow <- scoreMtx[idx,]
				evt <- cutterObj$getEvtVal( smRow ,rObj$evtLst )

				lst <- lapply( rObj$cutFLst ,function( pFunc ){ pFunc( smRow, evt ) } )
				cutFlag <- sapply( lst ,function(p){ p$cutFlag })
				if( any(cutFlag) ){
					firedCId <- sapply( lst[cutFlag] ,function(p){p$cId})
					infoStr <- sprintf("cut Id : %s",paste(firedCId,collapse=",") )
					cutLst[[1+length(cutLst)]] <- list( idx=idx ,idObjDesc=cutterObj$idObjDesc ,info=infoStr )
				}
			}

			return( cutLst )
		} # cutterObj$cut()

		return(cutterObj)

	}

	return( rObj )
} # bFCust.A_score2_A_Row01()


#	[score2:Col Cutter(Rebound/Sequencial)] -----------------------------------------------------
bFCust.A_score2_A_rReb01 <- function(  ){
	#	row rebind라고는 했지만 사실 seq 이다. 
	#	즉 이전 evt 컬럼이 다음에도 동일 evt가 일어나는지 체크(다음에서 추가적으로 발생하는 evt는 상관없음.)
	rObj <- list( )
	rObj$defId <- c( typ="cust_RReb"	,hName="*"	,mName="score2"	,pName="*"	,rFId="rReb01" )	# row filt ID
	rObj$description <- sprintf("(cust)  ")

	rObj$evtLst <- FCust_score2EvtLst

	rObj$createCutter <- function( hMtxLst ,tgtId=c(hName="", mName="", pName="") ,auxInfo=c(auxInfo="") ){

		cutterObj <- rObj
		cutterObj$createCutter <- NULL
		cutterObj$getEvtVal <- function( src ,evtLst ){
			evtVal <- src[names(evtLst)]
			for( nIdx in names(evtLst) ){
				if( !(evtVal[nIdx] %in% evtLst[[nIdx]]) ) evtVal[nIdx] <- NA
			}
			return( evtVal )
		} # cutterObj$getLastVal()

		#	hName="testNA"; mName="testNA"; pName="testNA"; fcName="testNA"; auxInfo=c(auxInfo="")
		idObjDesc <- rObj$defId
		if( idObjDesc["hName"]!=tgtId["hName"] ) idObjDesc["hName"] <- sprintf("(%s)%s",idObjDesc["hName"],tgtId["hName"])
		if( idObjDesc["mName"]!=tgtId["mName"] ) idObjDesc["mName"] <- sprintf("(%s)%s",idObjDesc["mName"],tgtId["mName"])
		if( idObjDesc["pName"]!=tgtId["pName"] ) idObjDesc["pName"] <- sprintf("(%s)%s",idObjDesc["pName"],tgtId["pName"])
		idObjDesc <- c( idObjDesc ,auxInfo )
		cutterObj$idObjDesc <- idObjDesc

		cutterObj$idObj <- rObj$defId
		cutterObj$idObj[names(tgtId)] <- tgtId

		scoreMtxObj <- hMtxLst$getScoreMtxObj( tgtId["hName"] ,tgtId["mName"] ,tgtId["pName"] )
		scoreMtx <- if( is.null(scoreMtxObj) ) NULL else scoreMtxObj$scoreMtx
		cutterObj$scoreMtx <- scoreMtx	# just for debug later..

		cutterObj$evtChkLst <- list()
		if( !is.null(scoreMtx) ){	# build checkLst
			#	fireThld 는 fire가 일어날 동일 패턴 수. NA이면 전부 매치.
			scoreMtx.last <- scoreMtx[nrow(scoreMtx),]

			evtChkInfo <- list( cutId="01" ,fireThld=2 ,evtLst=rObj$evtLst[c("rebV.r","rebL","rebR")] )
			evtChkInfo$evtLast <- cutterObj$getEvtVal( scoreMtx.last ,evtChkInfo$evtLst )
			cutterObj$evtChkLst[[1+length(cutterObj$evtChkLst)]] <- evtChkInfo

			evtChkInfo <- list( cutId="02" ,fireThld=2 ,evtLst=rObj$evtLst[c("rebC.r","rebC.c","rebC.f")] )
			evtChkInfo$evtLast <- cutterObj$getEvtVal( scoreMtx.last ,evtChkInfo$evtLst )
			cutterObj$evtChkLst[[1+length(cutterObj$evtChkLst)]] <- evtChkInfo

			evtChkInfo <- list( cutId="03" ,fireThld=2 ,evtLst=rObj$evtLst[c("rebC2.r","rebC2.c","rebC2.f")] )
			evtChkInfo$evtLast <- cutterObj$getEvtVal( scoreMtx.last ,evtChkInfo$evtLst )
			cutterObj$evtChkLst[[1+length(cutterObj$evtChkLst)]] <- evtChkInfo

			evtChkInfo <- list( cutId="04" ,fireThld=2 ,evtLst=rObj$evtLst[c("inc.r","inc.c","inc.f")] )
			evtChkInfo$evtLast <- cutterObj$getEvtVal( scoreMtx.last ,evtChkInfo$evtLst )
			cutterObj$evtChkLst[[1+length(cutterObj$evtChkLst)]] <- evtChkInfo

			evtChkInfo <- list( cutId="05" ,fireThld=2 ,evtLst=rObj$evtLst[c("inc.r2","inc.c2","inc.f2")] )
			evtChkInfo$evtLast <- cutterObj$getEvtVal( scoreMtx.last ,evtChkInfo$evtLst )
			cutterObj$evtChkLst[[1+length(cutterObj$evtChkLst)]] <- evtChkInfo

			evtChkInfo <- list( cutId="06" ,fireThld=2 ,evtLst=rObj$evtLst[c("rebL","rebR","inc.r3","inc.c3")] )
			evtChkInfo$evtLast <- cutterObj$getEvtVal( scoreMtx.last ,evtChkInfo$evtLst )
			cutterObj$evtChkLst[[1+length(cutterObj$evtChkLst)]] <- evtChkInfo

			evtChkInfo <- list( cutId="evtAll" ,fireThld=2 ,evtLst=rObj$evtLst )
			evtChkInfo$evtLast <- cutterObj$getEvtVal( scoreMtx.last ,evtChkInfo$evtLst )
			cutterObj$evtChkLst[[1+length(cutterObj$evtChkLst)]] <- evtChkInfo

			# 기타등등..

			names(cutterObj$evtChkLst) <- sapply( cutterObj$evtChkLst ,function(p){p$cutId})
			for( nIdx in names(cutterObj$evtChkLst) ){
				cutterObj$evtChkLst[[nIdx]]$evtNaMask <- !is.na(cutterObj$evtChkLst[[nIdx]]$evtLast)
			}
		}

		cutterObj$checkRow <- function( smRow ){	# scoreMtx row
			# 속도 최적화 면에서는 아주 안 좋지만, 일단 기능과 성능 확인이 급하니..
			# cutterObj$checkLst 설정내역을 흝어가며 체크.

			firedCutId <- character(0)
			for( idx in seq_len(length(cutterObj$evtChkLst)) ){
				evtChkInfo <- cutterObj$evtChkLst[[idx]]
				if( 0==sum(evtChkInfo$evtNaMask) ) next

				fireThld <- evtChkInfo$fireThld
				if( is.na(fireThld) ) fireThld <- sum(evtChkInfo$evtNaMask)

				src <- cutterObj$getEvtVal( smRow ,evtChkInfo$evtLst )
				chk <- (src==evtChkInfo$evtLast)[evtChkInfo$evtNaMask]

				if( fireThld <= sum(chk,na.rm=T) ){
					firedCutId <- c( firedCutId ,evtChkInfo$cutId )
				}

			}

			chkRstObj <- list( firedCutId=firedCutId ,cutFlag=(length(firedCutId)>0) )

			return( chkRstObj )
		}

		cutterObj$cut <- function( scoreMtx ,alreadyDead=NULL ){
			val.len <- nrow( scoreMtx )
			if( is.null(alreadyDead) )	alreadyDead <- rep( F, val.len )

			cutLst <- list()
			for( idx in seq_len(val.len) ){
				if( alreadyDead[idx] ) next

				chkRst <- cutterObj$checkRow( scoreMtx[idx,] )
				if( chkRst$cutFlag ){
					infoStr <- sprintf("cut Id : %s",paste(chkRst$firedCutId,collapse=",") )
					cutLst[[1+length(cutLst)]] <- list( idx=idx ,idObjDesc=cutterObj$idObjDesc ,info=infoStr )
				}
			}

			return( cutLst )
		} # cutterObj$cut()

		return(cutterObj)

	}

	return( rObj )
} # bFCust.A_score2_A_rReb01()

bFCust.A_score2_A_rRebAA <- function(  ){
	#	이전 마지막 score(cutterObj$lastRow) 값과의 일치여부(raw Value) 
	rObj <- list( )
	rObj$defId <- c( typ="cust_RReb"	,hName="*"	,mName="score2"	,pName="*"	,rFId="rRebAA" )	# row filt ID
	rObj$description <- sprintf("(cust)  ")

	rObj$createCutter <- function( hMtxLst ,tgtId=c(hName="", mName="", pName="") ,auxInfo=c(auxInfo="") ){

		cutterObj <- rObj
		cutterObj$createCutter <- NULL
		cutterObj$getEvtVal <- function( src ,evtLst ){
			evtVal <- src[names(evtLst)]
			for( nIdx in names(evtLst) ){
				if( !(evtVal[nIdx] %in% evtLst[[nIdx]]) ) evtVal[nIdx] <- NA
			}
			return( evtVal )
		} # cutterObj$getLastVal()

		#	hName="testNA"; mName="testNA"; pName="testNA"; fcName="testNA"; auxInfo=c(auxInfo="")
		idObjDesc <- rObj$defId
		if( idObjDesc["hName"]!=tgtId["hName"] ) idObjDesc["hName"] <- sprintf("(%s)%s",idObjDesc["hName"],tgtId["hName"])
		if( idObjDesc["mName"]!=tgtId["mName"] ) idObjDesc["mName"] <- sprintf("(%s)%s",idObjDesc["mName"],tgtId["mName"])
		if( idObjDesc["pName"]!=tgtId["pName"] ) idObjDesc["pName"] <- sprintf("(%s)%s",idObjDesc["pName"],tgtId["pName"])
		idObjDesc <- c( idObjDesc ,auxInfo )
		cutterObj$idObjDesc <- idObjDesc

		cutterObj$idObj <- rObj$defId
		cutterObj$idObj[names(tgtId)] <- tgtId

		scoreMtxObj <- hMtxLst$getScoreMtxObj( tgtId["hName"] ,tgtId["mName"] ,tgtId["pName"] )
		if( is.null(scoreMtxObj) ){	cutterObj$lastRow <- NULL
		} else {
			cutterObj$lastRow <- scoreMtxObj$scoreMtx[nrow(scoreMtxObj$scoreMtx),]
		}

		cutterObj$cut <- function( scoreMtx ,alreadyDead=NULL ){
			val.len <- nrow( scoreMtx )
			if( is.null(alreadyDead) )	alreadyDead <- rep( F, val.len )

			cutLst <- list()
			for( idx in seq_len(val.len) ){
				if( alreadyDead[idx] ) next

				if( is.null(cutterObj$lastRow) )	next

				matFlag <- all( cutterObj$lastRow==scoreMtx[idx,] )
				if( all(matFlag) ){
					infoStr <- sprintf("cut Id : %s",cutterObj$idObjDesc["rFId"] )
					cutLst[[1+length(cutLst)]] <- list( idx=idx ,idObjDesc=cutterObj$idObjDesc ,info=infoStr )
				}
			}

			return( cutLst )
		} # cutterObj$cut()

		return(cutterObj)
	}

	return( rObj )
} # bFCust.A_score2_A_rRebAA()

#	[score2:byFCol(Rebound/Sequencial)] ---------------------------------------------------------
#		- nRow 대상이긴 하지만, mtx는 각 scoreMtx 의 fCol 별로 생성된다는 점을 주의
#		- column이 phase이므로 pName 구분이 없고, tgtId에서도 pName이 빠진다. 대신 fcName 필요.
bFCust.byFCol_A_score2_rebVR <- function( ){

	rObj <- list( )
	rObj$defId <- c( typ="cust_byFCol"	,hName="*"	,mName="score2"	,fcName="rebV.r"  )
	rObj$description <- sprintf("(cust)  ")

	rObj$cutFLst <- list()
	# Sample code ================================================================
	# rObj$cutFLst[[1+length(rObj$cutFLst)]] <- function( smRow ){	# for testing

	# 	crObj <- list( cutFlag=F ,cId="Test.phase" ) # cut result object, cut Id
	# 	evtThld <- c("basic"=2,"nextZW"=1)

	# 	evtFlag <- smRow[names(evtThld)] == evtThld
	# 	if( all(evtFlag) ) crObj$cutFlag <- TRUE

	# 	return( crObj )
	# } # rObj$cutFLst[1]( )

	rObj$createCutter <- function( lastMtx=NULL ,tgtId=c(hName="", mName="", fcName="") ,auxInfo=c(auxInfo="") ){
		#	사실 lastMtx는 여기에서 필요 없는데, 다른 함수들과 파라미터 맞추느라..

		cutterObj <- rObj
		cutterObj$createCutter <- NULL

		#	hName="testNA"; mName="testNA"; pName="testNA"; fcName="testNA"; auxInfo=c(auxInfo="")
		idObjDesc <- rObj$defId
		if( idObjDesc["hName"]!=tgtId["hName"] ) idObjDesc["hName"] <- sprintf("(%s)%s",idObjDesc["hName"],tgtId["hName"])
		if( idObjDesc["mName"]!=tgtId["mName"] ) idObjDesc["mName"] <- sprintf("(%s)%s",idObjDesc["mName"],tgtId["mName"])
		if( idObjDesc["fcName"]!=tgtId["fcName"] ) idObjDesc["fcName"] <- sprintf("(%s)%s",idObjDesc["fcName"],tgtId["fcName"])
		idObjDesc <- c( idObjDesc ,auxInfo )
		cutterObj$idObjDesc <- idObjDesc

		cutterObj$idObj <- rObj$defId
		cutterObj$idObj[names(tgtId)] <- tgtId

		cutterObj$cut <- function( scoreMtx ,alreadyDead=NULL ){
			val.len <- nrow( scoreMtx )
			if( is.null(alreadyDead) )	alreadyDead <- rep( F, val.len )

			cutLst <- list()
			for( idx in seq_len(val.len) ){
				if( alreadyDead[idx] ) next

				lst <- lapply( rObj$cutFLst ,function( pFunc ){ pFunc( scoreMtx[idx,] ) } )
				cutFlag <- sapply( lst ,function(p){ p$cutFlag })
				if( any(cutFlag) ){
					firedCId <- sapply( lst[cutFlag] ,function(p){p$cId})
					infoStr <- sprintf("cut Id : %s",paste(firedCId,collapse=",") )
					cutLst[[1+length(cutLst)]] <- list( idx=idx ,idObjDesc=cutterObj$idObjDesc ,info=infoStr )
				}
			}

			return( cutLst )
		} # cutterObj$cut()

		return(cutterObj)
	}

	return( rObj )
} # bFCust.byFCol_A_score2_rebVR( )

bFCust.byFCol_A_score2_A_rReb01 <- function( ){
	#	row rebind라고는 했지만 사실 seq 이다. 
	#	즉 이전 evt 발생 phase에서 다음에도 동일 evt가 일어나는지 체크(다음에서 추가적으로 발생하는 evt는 상관없음.)
	rObj <- list( )
	rObj$defId <- c( typ="cust_byFCol"	,hName="*"	,mName="score2"	,pName="*"	,fcName="rebV.r" )	# row filt ID
	rObj$description <- sprintf("(cust)  ")

	rObj$evtLst <- FCust_score2EvtLst
	rObj$fireThld.min <- c("rebV.r"=2 ,"rebL"=2 ,"rebR"=2
						,"rebC.r"=2 ,"rebC.c"=2 ,"rebC.f"=2 ,"rebC2.r"=2 ,"rebC2.c"=2 ,"rebC2.f"=2 
						,"inc.r"=2 ,"inc.c"=2 ,"inc.f"=2 ,"inc.r2"=2 ,"inc.c2"=2 ,"inc.f2"=2 
						,"inc.r3"=2 ,"inc.c3"=2
					)

	rObj$createCutter <- function( lastMtx ,tgtId=c(hName="", mName="", fcName="") ,auxInfo=c(auxInfo="") ){

		cutterObj <- rObj
		cutterObj$createCutter <- NULL	;cutterObj$evtLst <- NULL

		#	hName="testNA"; mName="testNA"; pName="testNA"; fcName="testNA"; auxInfo=c(auxInfo="")
		idObjDesc <- rObj$defId
		if( idObjDesc["hName"]!=tgtId["hName"] ) idObjDesc["hName"] <- sprintf("(%s)%s",idObjDesc["hName"],tgtId["hName"])
		if( idObjDesc["mName"]!=tgtId["mName"] ) idObjDesc["mName"] <- sprintf("(%s)%s",idObjDesc["mName"],tgtId["mName"])
		if( idObjDesc["fcName"]!=tgtId["fcName"] ) idObjDesc["fcName"] <- sprintf("(%s)%s",idObjDesc["fcName"],tgtId["fcName"])
		idObjDesc <- c( idObjDesc ,auxInfo )
		cutterObj$idObjDesc <- idObjDesc

		cutterObj$idObj <- rObj$defId
		cutterObj$idObj[names(tgtId)] <- tgtId

		cutterObj$lastMtx <- lastMtx	# just for debug later..
		cutterObj$evtVal <- rObj$evtLst[[ tgtId["fcName"] ]]
		cutterObj$lastEvt <- lastMtx[nrow(lastMtx),]
		cutterObj$lastEvt[ !(cutterObj$lastEvt %in% cutterObj$evtVal) ] <- NA
		cutterObj$evtNaMask <- !is.na(cutterObj$lastEvt)
		cutterObj$fireThld.min <- rObj$fireThld.min[ tgtId["fcName"] ]

		cutterObj$cut <- function( scoreMtx ,alreadyDead=NULL ){
			val.len <- nrow( scoreMtx )
			if( is.null(alreadyDead) )	alreadyDead <- rep( F, val.len )

			cutLst <- list()
			for( idx in seq_len(val.len) ){
				if( alreadyDead[idx] ) next

				chkRst <- cutterObj$checkRow( scoreMtx[idx,] )
				if( chkRst$cutFlag ){
					infoStr <- sprintf("cut Id : rReb01(thld min:%d)",cutterObj$fireThld.min )
					cutLst[[1+length(cutLst)]] <- list( idx=idx ,idObjDesc=cutterObj$idObjDesc ,info=infoStr )
				}
			}

			return( cutLst )
		} # cutterObj$cut()

		cutterObj$checkRow <- function( smRow ){

			chkRstObj <- list( cutFlag=FALSE ,fireCutId=character(0) )

			# event Check -------------------------------------------------------------
			fireThld.min <- cutterObj$fireThld.min
			if( is.na(fireThld.min) ) fireThld.min <- sum(cutterObj$evtNaMask)

			if( 0<sum(cutterObj$evtNaMask) && fireThld.min<=sum(cutterObj$evtNaMask) ){
				matFlag <- (smRow==cutterObj$lastEvt)[cutterObj$evtNaMask]
				if( fireThld.min <= sum(matFlag) ){
					chkRstObj$cutFlag = TRUE
					chkRstObj$fireCutId = c( chkRstObj$fireCutId ,"evtReb" )
				}
			}

			# 기타 추가 reb Check -------------------------------------------------------------

			return( chkRstObj )
		}

		return(cutterObj)
	}

	return( rObj )
} # bFCust.byFCol_A_score2_A_rReb01( )

bFCust.byFCol_A_score2_A_rRebAA <- function( ){
	rObj <- list( )
	rObj$defId <- c( typ="cust_byFCol"	,hName="*"	,mName="score2"	,fcName="*"  )
	rObj$description <- sprintf("(cust)  ")

	# last score에서, happen 수가 fireThld.min이상 존재할 때에만 완전 매치여부 확인.
	rObj$fireThld.min <- c("rebV.r"=2 ,"rebL"=2 ,"rebR"=2
						,"rebC.r"=2 ,"rebC.c"=2 ,"rebC.f"=2 ,"rebC2.r"=2 ,"rebC2.c"=2 ,"rebC2.f"=2 
						,"inc.r"=2 ,"inc.c"=2 ,"inc.f"=2 ,"inc.r2"=2 ,"inc.c2"=2 ,"inc.f2"=2 
						,"inc.r3"=2 ,"inc.c3"=2
					)

	rObj$createCutter <- function( lastMtx ,tgtId=c(hName="", mName="", pName="") ,auxInfo=c(auxInfo="") ){

		cutterObj <- rObj
		cutterObj$createCutter <- NULL	;cutterObj$evtLst <- NULL

		#	hName="testNA"; mName="testNA"; pName="testNA"; fcName="testNA"; auxInfo=c(auxInfo="")
		idObjDesc <- rObj$defId
		if( idObjDesc["hName"]!=tgtId["hName"] ) idObjDesc["hName"] <- sprintf("(%s)%s",idObjDesc["hName"],tgtId["hName"])
		if( idObjDesc["mName"]!=tgtId["mName"] ) idObjDesc["mName"] <- sprintf("(%s)%s",idObjDesc["mName"],tgtId["mName"])
		if( idObjDesc["fcName"]!=tgtId["fcName"] ) idObjDesc["fcName"] <- sprintf("(%s)%s",idObjDesc["fcName"],tgtId["fcName"])
		idObjDesc <- c( idObjDesc ,auxInfo )
		cutterObj$idObjDesc <- idObjDesc

		cutterObj$idObj <- rObj$defId
		cutterObj$idObj[names(tgtId)] <- tgtId

		cutterObj$defId["fcName"] <- tgtId["fcName"]
			# 주의 : defId에서의 "*" 값은 타 정의된 cutter 함수가 있으면 제거된다.
			#		때문에 무조건 살리기 위해서는 fcName값을 강제 설정해야만 한다.
			#		이런 경우, fcName 별 별도 로직을 추가하려면 createCutter() 함수 내에서 구현해야 한다.

		cutterObj$lastRow <- lastMtx[nrow(lastMtx),]
		cutterObj$fireThld.min <- rObj$fireThld.min[tgtId["fcName"]]
		cutterObj$activated <- sum(cutterObj$lastRow>0) >= cutterObj$fireThld.min

		cutterObj$cut <- function( scoreMtx ,alreadyDead=NULL ){

			val.len <- nrow( scoreMtx )
			if( is.null(alreadyDead) ){
				alreadyDead <- rep( F, val.len )
			}

			cutLst <- list()
			for( idx in seq_len(val.len) ){
				if( alreadyDead[idx] ) next

				if( !cutterObj$activated || is.null(cutterObj$lastRow) ) next

				matFlag <- all( cutterObj$lastRow==scoreMtx[idx,] )
				if( all(matFlag) ){
					infoStr <- sprintf("cut Id : rRebAA" )
					cutLst[[1+length(cutLst)]] <- list( idx=idx ,idObjDesc=cutterObj$idObjDesc ,info=infoStr )
				}
			}

			return( cutLst )
		} # cutterObj$cut()

		return(cutterObj)
	}

	return( rObj )
} # bFCust.byFCol_A_score2_A_rRebAA( )



#	[score2:byHIdx(...)] ---------------------------------------------------------
#		- [fCol,phName] 
#       - 개개 score의 최대 최소값이나, evt 발생등은 이미 앞에서 모두 확인되었다.)
#		  따라서 col 방향, row 방향, mtx전체에 대한 rebPtn 체크.
#			(fCol 별 evt 기준 때문에 scoreMtx 개개별로 작성 필요.)

#		c( typ="cust_byHIdx"	,hName="*"	,mName="score2" )
bFCust.byHIdx_A_score2 <- function( ){

	rObj <- list( )
	rObj$defId <- c( typ="cust_byHIdx"	,hName="*"	,mName="score2" )
	rObj$description <- sprintf("(cust)  ")

	rObj$evtLst <- FCust_score2EvtLst
	rObj$getMtxEvt_byRow <- function( srcMtxLst ){	# [fCol,phase]
		
		mtxLen <- length(srcMtxLst)

		eMtxLst <- lapply( srcMtxLst ,function( srcMtx ){
							rMtx <- srcMtx
							for( rnIdx in rownames(srcMtx) ){ # rnIdx <- rownames(srcMtx)[1]
								for( cIdx in 1:ncol(srcMtx) ){
									if( !(rMtx[rnIdx,cIdx] %in% rObj$evtLst[[rnIdx]]) ){
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
	} # rObj$getMtxEvt_byRow()

	rObj$createCutter <- function( mtxLst=NULL ,tgtId=c(hName="", mName="") ,auxInfo=c(auxInfo="") ){
		#	mtxLst : 사실상 맨 마지막 mtx만 필요하긴 한데, 차후 h간 연속발생 갯수도 체크할 기능을 만들 수 있게 하기 위해 전체 list를 받음.

		cutterObj <- rObj
		cutterObj$createCutter <- NULL	;cutterObj$evtLst <- NULL	;cutterObj$getMtxEvt_byRow <- NULL

		#	hName="testNA"; mName="testNA"; pName="testNA"; fcName="testNA"; auxInfo=c(auxInfo="")
		idObjDesc <- rObj$defId
		if( idObjDesc["hName"]!=tgtId["hName"] ) idObjDesc["hName"] <- sprintf("(%s)%s",idObjDesc["hName"],tgtId["hName"])
		if( idObjDesc["mName"]!=tgtId["mName"] ) idObjDesc["mName"] <- sprintf("(%s)%s",idObjDesc["mName"],tgtId["mName"])
		idObjDesc <- c( idObjDesc ,auxInfo )
		cutterObj$idObjDesc <- idObjDesc

		cutterObj$idObj <- rObj$defId
		cutterObj$idObj[names(tgtId)] <- tgtId

		cutterObj$evt <- rObj$getMtxEvt_byRow( mtxLst ) 

		cutterObj$cut <- function( scoreMtx ,aIdx ){
			# scoreMtx 는 1개 aZoid에 관한 [fCol,phase] mtx임을 유의.
			# 	(즉, 이 함수는 한 개 aZoid에 대한 처리로직이다.)
			#	단 surDf와 cutLst는 다른 cut함수들 결과와의 호환성 유지를 위해 구조유지.
			cutId <- character(0)

			for( cutIdx in names(cutterObj$cutLst) ){ # cutIdx <- names(cutterObj$cutLst)[1]
				cutId <- c( cutId ,cutterObj$cutLst[[cutIdx]]( scoreMtx ) )
			}

			cutLst <- list()
			if( 0<length(cutId) ){
				infoStr <- sprintf("cut Id : %s",paste(cutId,collapse=",") )
				cutLst[[1+length(cutLst)]] <- list( idx=aIdx ,idObjDesc=cutterObj$idObjDesc ,info=infoStr )
			}

			return( cutLst )
		} # cutterObj$cut()

		cutterObj$cutLst <- list()
		cutterObj$cutLst[["F_matEvt"]] <- function( scoreMtx ){
			rCutId <- character(0)

			srcEvt <- scoreMtx
			for( rnIdx in rownames(scoreMtx) ){ # rnIdx <- rownames(scoreMtx)[1]
				for( cIdx in 1:ncol(scoreMtx) ){
					if( !(srcEvt[rnIdx,cIdx] %in% rObj$evtLst[[rnIdx]]) ){
						srcEvt[rnIdx,cIdx] <- NA
					}
				}
			}

			matMtx <- srcEvt == cutterObj$evt$lastMtx
			matMtx[is.na(matMtx)] <- FALSE

			# match happen count ----------------------------------
			if( 3<=sum( matMtx ) ) rCutId <- c( rCutId, sprintf("matHpnCnt.%d",sum( matMtx )) )

			# match happen count(sequencial rebind) ----------------------------------
			rebCnt <- cutterObj$evt$rebCntMtx[matMtx]
			if( any(rebCnt>2) ) rCutId <- c( rCutId, sprintf("matRebCnt.%d/%d",sum(rebCnt>1),max(rebCnt)) )

			# raw idff -------------------------------------------
			nMatchCnt <- sum(scoreMtx!=cutterObj$evt$lastMtxRaw)
			surWindow <- c(min=50,max=70)
			if( !bUtil.in(nMatchCnt,surWindow) ) rCutId <- c( rCutId, sprintf("rawDiffCnt.%d(%d~%d)",nMatchCnt,surWindow["min"],surWindow["max"]) )


			return( rCutId )
		}
		cutterObj$cutLst[["F_colMat"]] <- function( scoreMtx ){
			rCutId <- character(0)

			# match happen count ----------------------------------
			surWindow <- c(min=0,max=1)	# survive window
			srcEvt <- scoreMtx
			for( rnIdx in rownames(scoreMtx) ){ # rnIdx <- rownames(scoreMtx)[1]
				for( cIdx in 1:ncol(scoreMtx) ){
					if( !(srcEvt[rnIdx,cIdx] %in% rObj$evtLst[[rnIdx]]) ){
						srcEvt[rnIdx,cIdx] <- NA
					}
				}
			}
			for( cIdx in 2:ncol(scoreMtx) ){
				matCnt <- sum( srcEvt[,cIdx-1]==srcEvt[,cIdx] ,na.rm=T )
				if( !bUtil.in(matCnt,surWindow) ){
					rCutId <- c( rCutId, sprintf("colEvtMat.%d(%d~%d)",matCnt,surWindow["min"],surWindow["max"]) )
					break
				}
			}

			# raw match -------------------------------------------
			surWindow <- c(min=1,max=10)	# survive window
			for( cIdx in 2:ncol(scoreMtx) ){
				matCnt <- sum( scoreMtx[,cIdx-1]!=scoreMtx[,cIdx] )
				if( !bUtil.in(matCnt,surWindow) ){
					rCutId <- c( rCutId, sprintf("colRawDiff.%d(%d~%d)",matCnt,surWindow["min"],surWindow["max"]) )
					break
				}
			}

			return( rCutId )
		}
		# - custom --------------------------------------------------
		cutterObj$cutLst[["F_colPairMat"]] <- function( scoreMtx ){
			rCutId <- character(0)

			# rebC r/c/f -------------------------------------------
			matCnt <- apply( scoreMtx ,2 ,function(cVal){
				pair1 <- cVal[c("rebC.r","rebC.c","rebC.f")]
				pair2 <- cVal[c("rebC2.r","rebC2.c","rebC2.f")]

				if( 0==(sum(pair1)+sum(pair2)) )	return( 0 )

				return( sum(pair1==pair2) )
			})
			if(TRUE){	# check
				matCntSum <- sum(matCnt==3)
				if( 0<matCntSum )	rCutId <- c( rCutId, sprintf("rebC.m3 %d",matCntSum) )

				matCntSum <- sum(matCnt==2)
				if( !bUtil.in(matCntSum,eadge=c(min=1,max=1)) ){
					rCutId <- c( rCutId, sprintf("rebC.m2 %d",sum(matCnt==2)) )
				}
			}

			# inc r/c/f -------------------------------------------
			matCnt <- apply( scoreMtx ,2 ,function(cVal){
				pair1 <- cVal[c("inc.r","inc.c","inc.f")]
				pair2 <- cVal[c("inc.r2","inc.c2","inc.f2")]

				if( 0==(sum(pair1)+sum(pair2)) )	return( 0 )

				return( sum(pair1==pair2) )
			})
			if(TRUE){	# check
				matCntSum <- sum(matCnt==3)
				if( 0<matCntSum )	rCutId <- c( rCutId, sprintf("inc.m3 %d",matCntSum) )

				matCntSum <- sum(matCnt==2)
				if( !bUtil.in(matCntSum,eadge=c(min=1,max=1)) ){
					rCutId <- c( rCutId, sprintf("inc.m2 %d",sum(matCnt==2)) )
				}
			}

			# inc r/c (1,2,3) -------------------------------------------
			matFlag <- apply( scoreMtx ,2 ,function(cVal){
				pair1 <- cVal[c("inc.r","inc.c")]
				pair2 <- cVal[c("inc.r2","inc.c2")]
				pair3 <- cVal[c("inc.r3","inc.c3")]

				if( 0==sum(pair1) )	return( FALSE )

				return( (pair1==pair2)&&(pair1==pair3) )
			})
			if(TRUE){	# check
				matCntSum <- sum(matFlag)
				if( 0<matCntSum )	rCutId <- c( rCutId, sprintf("inc.rc %d",matCntSum) )
			}

			return( rCutId )
		}
		cutterObj$cutLst[["F_rebLR"]] <- function( scoreMtx ){
			rCutId <- character(0)

			sum.rebLR <- scoreMtx["rebL",]+scoreMtx["rebR",]

			chkCol <- setdiff( colnames(scoreMtx),"basic" )
			if( any(sum.rebLR[chkCol]>1) ){
				rCutId <- c( rCutId, sprintf("rebLR %d",sum(chkCol[chkCol])) )
			}

			return( rCutId )
		}
		cutterObj$cutLst[["F_rowPairMat"]] <- function( scoreMtx ){
			rCutId <- character(0)

			rowPair <- c("rebC.r","rebC2.r")	# scoreMtx[rowPair,]
			matFlag <- scoreMtx[rowPair[1],]==scoreMtx[rowPair[2],]
			validFlag <- 0<(scoreMtx[rowPair[1],]+scoreMtx[rowPair[2],])
			matCnt <- sum( matFlag[validFlag] )
			if( !bUtil.in(matCnt,eadge=c(min=1,max=1)) ){
				rCutId <- c( rCutId, sprintf("rr %d",matCnt) )
			}

			rowPair <- c("rebC.c","rebC2.c")	# scoreMtx[rowPair,]
			matFlag <- scoreMtx[rowPair[1],]==scoreMtx[rowPair[2],]
			validFlag <- 0<(scoreMtx[rowPair[1],]+scoreMtx[rowPair[2],])
			matCnt <- sum( matFlag[validFlag] )
			if( !bUtil.in(matCnt,eadge=c(min=1,max=1)) ){
				rCutId <- c( rCutId, sprintf("rc %d",matCnt) )
			}

			rowPair <- c("rebC.f","rebC2.f")	# scoreMtx[rowPair,]
			matFlag <- scoreMtx[rowPair[1],]==scoreMtx[rowPair[2],]
			validFlag <- 0<(scoreMtx[rowPair[1],]+scoreMtx[rowPair[2],])
			matCnt <- sum( matFlag[validFlag] )
			if( !bUtil.in(matCnt,eadge=c(min=1,max=1)) ){
				rCutId <- c( rCutId, sprintf("rf %d",matCnt) )
			}

			rowPair <- c("inc.r","inc.r2")	# scoreMtx[rowPair,]
			matFlag <- scoreMtx[rowPair[1],]==scoreMtx[rowPair[2],]
			validFlag <- 0<(scoreMtx[rowPair[1],]+scoreMtx[rowPair[2],])
			matCnt <- sum( matFlag[validFlag] )
			if( !bUtil.in(matCnt,eadge=c(min=1,max=1)) ){
				rCutId <- c( rCutId, sprintf("ir12 %d",matCnt) )
			}

			rowPair <- c("inc.r","inc.r3")	# scoreMtx[rowPair,]
			matFlag <- scoreMtx[rowPair[1],]==scoreMtx[rowPair[2],]
			validFlag <- 0<(scoreMtx[rowPair[1],]+scoreMtx[rowPair[2],])
			matCnt <- sum( matFlag[validFlag] )
			if( !bUtil.in(matCnt,eadge=c(min=1,max=1)) ){
				rCutId <- c( rCutId, sprintf("ir13 %d",matCnt) )
			}

			rowPair <- c("inc.r2","inc.r3")	# scoreMtx[rowPair,]
			matFlag <- scoreMtx[rowPair[1],]==scoreMtx[rowPair[2],]
			validFlag <- 0<(scoreMtx[rowPair[1],]+scoreMtx[rowPair[2],])
			matCnt <- sum( matFlag[validFlag] )
			if( !bUtil.in(matCnt,eadge=c(min=1,max=1)) ){
				rCutId <- c( rCutId, sprintf("ir23 %d",matCnt) )
			}

			rowPair <- c("inc.c","inc.c2")	# scoreMtx[rowPair,]
			matFlag <- scoreMtx[rowPair[1],]==scoreMtx[rowPair[2],]
			validFlag <- 0<(scoreMtx[rowPair[1],]+scoreMtx[rowPair[2],])
			matCnt <- sum( matFlag[validFlag] )
			if( !bUtil.in(matCnt,eadge=c(min=1,max=1)) ){
				rCutId <- c( rCutId, sprintf("ic12 %d",matCnt) )
			}

			rowPair <- c("inc.c","inc.c3")	# scoreMtx[rowPair,]
			matFlag <- scoreMtx[rowPair[1],]==scoreMtx[rowPair[2],]
			validFlag <- 0<(scoreMtx[rowPair[1],]+scoreMtx[rowPair[2],])
			matCnt <- sum( matFlag[validFlag] )
			if( !bUtil.in(matCnt,eadge=c(min=1,max=1)) ){
				rCutId <- c( rCutId, sprintf("ic13 %d",matCnt) )
			}

			rowPair <- c("inc.c2","inc.c3")	# scoreMtx[rowPair,]
			matFlag <- scoreMtx[rowPair[1],]==scoreMtx[rowPair[2],]
			validFlag <- 0<(scoreMtx[rowPair[1],]+scoreMtx[rowPair[2],])
			matCnt <- sum( matFlag[validFlag] )
			if( !bUtil.in(matCnt,eadge=c(min=1,max=1)) ){
				rCutId <- c( rCutId, sprintf("ic23 %d",matCnt) )
			}

			rowPair <- c("inc.f","inc.f2")	# scoreMtx[rowPair,]
			matFlag <- scoreMtx[rowPair[1],]==scoreMtx[rowPair[2],]
			validFlag <- 0<(scoreMtx[rowPair[1],]+scoreMtx[rowPair[2],])
			matCnt <- sum( matFlag[validFlag] )
			if( !bUtil.in(matCnt,eadge=c(min=1,max=1)) ){
				rCutId <- c( rCutId, sprintf("if12 %d",matCnt) )
			}

			return( rCutId )
		}

		return(cutterObj)
	}

	return( rObj )
} # bFCust.byHIdx_A_score2



getEvtMtx_byRow <- function( evtLst ,srcMtx ){
	# evtLst <- FCust_score2EvtLst	;srcMtx <- scoreMtx
	rMtx <- srcMtx
	for( rnIdx in rownames(srcMtx) ){ # rnIdx <- rownames(srcMtx)[1]
		for( cIdx in 1:ncol(srcMtx) ){
			if( !(rMtx[rnIdx,cIdx] %in% evtLst[[rnIdx]]) ){
				rMtx[rnIdx,cIdx] <- NA
			}
		}
	}
	return( rMtx )
} # getEvtMtx



