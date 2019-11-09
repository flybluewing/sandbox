FCust_score2EvtLst <- list("rebV.r"=3:6 ,"rebL"=1:3 ,"rebR"=1:3
								,"rebC.r"=2:6 ,"rebC.c"=2:6 ,"rebC.f"=2:6 ,"rebC2.r"=2:6 ,"rebC2.c"=2:6 ,"rebC2.f"=2:6 
								,"inc.r"=2:6 ,"inc.c"=2:6 ,"inc.f"=2:6 ,"inc.r2"=2:6 ,"inc.c2"=2:6 ,"inc.f2"=2:6 
								,"inc.r3"=2:6 ,"inc.c3"=2:6
							)

FCust_score2minMaxLst <- list( "rebV.r"=c(min=0,max=3) ,"rebL"=c(min=0,max=1) ,"rebR"=c(min=0,max=1)
								,"rebC.r"=c(min=0,max=2) ,"rebC.c"=c(min=0,max=3) ,"rebC.f"=c(min=0,max=3)
								,"rebC2.r"=c(min=0,max=2) ,"rebC2.c"=c(min=0,max=3) ,"rebC2.f"=c(min=0,max=3) 
								,"inc.r"=c(min=0,max=3) ,"inc.c"=c(min=0,max=2) ,"inc.f"=c(min=0,max=2) 
								,"inc.r2"=c(min=0,max=3) ,"inc.c2"=c(min=0,max=3) ,"inc.f2"=c(min=0,max=3) 
								,"inc.r3"=c(min=0,max=3) ,"inc.c3"=c(min=0,max=3)
							)

#	[score2:Col Cutter(1 col)] ------------------------------------------------------------------
#	c( typ="cust"	,hName="*"	,mName="score2"	,pName="*"	,fcName="*" )
bFCust.A_score2_A_A <- function(  ){

	rObj <- list( defId=c( typ="cust"	,hName="*"	,mName="score2"	,pName="*"	,fcName="*" ) )

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

        cutterObj$minMax <- FCust_score2minMaxLst[[tgtId["fcName"]]]

        cutterObj$description <- sprintf("(%s.%s %d~%d)",cutterObj$idObj["mName"],tgtId["fcName"]
                                    ,cutterObj$minMax["min"],cutterObj$minMax["max"] 
        )

		cutterObj$idObj <- rObj$defId
		cutterObj$idObj[names(tgtId)] <- tgtId

		cutterObj$cut <- function( scoreMtx ,alreadyDead=NULL ){

			val <- scoreMtx[,cutterObj$idObj["fcName"]]
			val.len <- length( val )
			if( is.null(alreadyDead) )	alreadyDead <- rep( F, val.len )

			cutLst <- list()
			for( idx in seq_len(val.len) ){
				if( alreadyDead[idx] )	next

				if( !bUtil.in(val[idx],cutterObj$minMax) ){
					infoStr <- c(info=sprintf("%s val:%d",cutterObj$description,val[idx]))
					cutLst[[1+length(cutLst)]] <- list( idx=idx ,idObjDesc=cutterObj$idObjDesc ,info=infoStr )
				}
			}

			return( cutLst )
		} # cutterObj$cut()

		return(cutterObj)
	}

	return(rObj)
} # bFCust.A_score2_A_A( )

bFCust.A_score2_A_A.old <- function(  ){	# 폐지예정

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
} # bFCust.A_score2_A_A.old( )

#	c( typ="cust"	,hName="*"	,mName="score2"	,pName="*"	,fcName="rebV.r" )
bFCust.A_score2_A_rebVR <- function(  ){	# 폐지예정

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
bFCust.A_score2_A_rebL <- function(  ){		# 폐지예정

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
bFCust.A_score2_A_rebR <- function(  ){		# 폐지예정

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
	rObj$defId <- c( typ="c_NCol.2"	,hName="*"	,mName="score2"	,pName="*"	,rFId="Row01" )	# row filt ID
	rObj$description <- sprintf("(cust)  ")

	rObj$evtLst <- FCust_score2EvtLst

	rObj$cutFLst <- list()
	# Sample code ================================================================
	rObj$cutFLst[[1+length(rObj$cutFLst)]] <- function( smRow ,evt ){
		crObj <- list( cutFlag=F ,cId="evtNum" ) # cut result object, cut Id
		if( 6<=sum(!is.na(evt)) ){	# 4~>6
			crObj$cutFlag <- TRUE
			crObj$cId <- sprintf( "%s  %d" ,crObj$cId ,sum(!is.na(evt)) )
		}
		return( crObj )
	} # rObj$cutFLst[1]( )
	rObj$cutFLst[[1+length(rObj$cutFLst)]] <- function( smRow ,evt ){
		crObj <- list( cutFlag=F ,cId="hpnOne" ) # cut result object, cut Id
		cnt <- sum(smRow==1)
		if( !bUtil.in(cnt,c(min=0,max=8)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- sprintf( "%s  %d" ,crObj$cId ,cnt )
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
bFCust.A_score2_A_rReb01 <- function(  ){	# evt rebind
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

		scoreMtxObj <- B.HMtxLst_getMtxLst( hMtxLst , tgtId["hName"] ,tgtId["mName"] ,tgtId["pName"] )
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
			# 속도 최적화 면에서는 아주 안 좋지만, 일단 기능의 유효성 확인이 급하니..
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

bFCust.A_score2_A_rRebAA <- function(  ){	#	이전 마지막 score(cutterObj$lastRow) 값과의 일치여부(raw Value) 

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

		scoreMtxObj <- B.HMtxLst_getMtxLst( hMtxLst , tgtId["hName"] ,tgtId["mName"] ,tgtId["pName"] )
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

				diffCnt <- sum( cutterObj$lastRow!=scoreMtx[idx,] )
				hpnCnt <- sum(cutterObj$lastRow>0)
				if( hpnCnt>3 && diffCnt==0 ){
					infoStr <- sprintf("cut Id : %s diffCnt.%d",cutterObj$idObjDesc["rFId"],hpnCnt )
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
#		- mtxGrp <- getScoreMtx.grp_byFCol( scoreMtx.grp )
#		- nRow 대상이긴 하지만, mtx는 각 scoreMtx 의 fCol 별로 생성된다는 점을 주의
#		- column이 phase이므로 pName 구분이 없고, tgtId에서도 pName이 빠진다. 대신 fcName 필요.

#	c( typ="c_byFCol"	,hName="*"	,mName="score2"	,fcName="*"  )
bFCust.byFCol_A_score2_A <- function( ){

	rObj <- list( )
	rObj$defId <- c( typ="c_byFCol"	,hName="*"	,mName="score2"	,fcName="*"  )
	rObj$description <- sprintf("(cust)  ")

	rObj$cutFLst <- list()
	rObj$cutFLst[["rebV.r"]] <- function( smRow ,fcName ){

		crObj <- list( cutFlag=F ,cId="_A rebV.r" ) # cut result object, cut Id

		cnt <- sum(smRow==0)
		if( !bUtil.in(cnt,c(min=0,max=11)) ){
			crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 00.%d",crObj$cId,cnt)
		}

		cnt <- sum(smRow==2)
		if( !bUtil.in(cnt,c(min=0,max=5)) ){
			crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 02.%d",crObj$cId,cnt)
		}

		cnt <- sum(smRow==3)
		if( !bUtil.in(cnt,c(min=0,max=3)) ){
			crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 03.%d",crObj$cId,cnt)
		}

		return( crObj )
	} # rObj$cutFLst[1]( )
	rObj$cutFLst[["rebLR"]] <- function( smRow ,fcName ){
		crObj <- list( cutFlag=F ,cId="_A rebLR" ) # cut result object, cut Id
		cnt <- sum(smRow>0)
		if( !bUtil.in(cnt,c(min=0,max=2)) ){
			crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 00.%d",crObj$cId,cnt)
		}
		return( crObj )
	} # rObj$cutFLst[1]( )
	rObj$cutFLst[["rebCn.r"]] <- function( smRow ,fcName ){
		crObj <- list( cutFlag=F ,cId="_A rebCn.r" ) # cut result object, cut Id
		cnt <- sum(smRow==0)
		if( !bUtil.in(cnt,c(min=2,max=13)) ){
			crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 00.%d",crObj$cId,cnt)
		}
		cnt <- sum(smRow==2)
		if( !bUtil.in(cnt,c(min=0,max=2)) ){
			crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 02.%d",crObj$cId,cnt)
		}
		cnt <- sum(smRow>=3)
		if( !bUtil.in(cnt,c(min=0,max=1)) ){
			crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
		}
		return( crObj )
	} # rObj$cutFLst[1]( )
	rObj$cutFLst[["rebCn.c"]] <- function( smRow ,fcName ){
		crObj <- list( cutFlag=F ,cId="_A rebCn.c" ) # cut result object, cut Id
		cnt <- sum(smRow==0)
		if( !bUtil.in(cnt,c(min=5,max=13)) ){
			crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 00.%d",crObj$cId,cnt)
		}
		cnt <- sum(smRow==2)
		if( !bUtil.in(cnt,c(min=0,max=4)) ){
			crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 02.%d",crObj$cId,cnt)
		}
		cnt <- sum(smRow>=3)
		if( !bUtil.in(cnt,c(min=0,max=3)) ){
			crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
		}
		return( crObj )
	} # rObj$cutFLst[1]( )
	rObj$cutFLst[["rebCn.f"]] <- function( smRow ,fcName ){
		crObj <- list( cutFlag=F ,cId="_A rebCn.f" ) # cut result object, cut Id
		cnt <- sum(smRow==1)
		if( !bUtil.in(cnt,c(min=0,max=8)) ){
			crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
		}
		cnt <- sum(smRow==2)
		if( !bUtil.in(cnt,c(min=0,max=3)) ){
			crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 02.%d",crObj$cId,cnt)
		}
		cnt <- sum(smRow>=3)
		if( !bUtil.in(cnt,c(min=0,max=1)) ){
			crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
		}
		return( crObj )
	} # rObj$cutFLst[1]( )
	rObj$cutFLst[["inc.rn"]] <- function( smRow ,fcName ){
		crObj <- list( cutFlag=F ,cId="_A inc.rn" ) # cut result object, cut Id
		cnt <- sum(smRow==1)
		if( !bUtil.in(cnt,c(min=0,max=6)) ){
			crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
		}
		cnt <- sum(smRow==2)
		if( !bUtil.in(cnt,c(min=0,max=1)) ){
			crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 02.%d",crObj$cId,cnt)
		}
		cnt <- sum(smRow>=3)
		if( !bUtil.in(cnt,c(min=0,max=1)) ){
			crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
		}
		return( crObj )
	} # rObj$cutFLst[1]( )
	rObj$cutFLst[["inc.cn"]] <- function( smRow ,fcName ){
		crObj <- list( cutFlag=F ,cId="_A inc.cn" ) # cut result object, cut Id
		cnt <- sum(smRow==1)
		if( !bUtil.in(cnt,c(min=0,max=6)) ){
			crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
		}
		cnt <- sum(smRow==2)
		if( !bUtil.in(cnt,c(min=0,max=3)) ){
			crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 02.%d",crObj$cId,cnt)
		}
		cnt <- sum(smRow>=3)
		if( !bUtil.in(cnt,c(min=0,max=1)) ){
			crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
		}
		return( crObj )
	} # rObj$cutFLst[1]( )
	rObj$cutFLst[["inc.fn"]] <- function( smRow ,fcName ){	# for testing
		crObj <- list( cutFlag=F ,cId="_A inc.fn" ) # cut result object, cut Id
		cnt <- sum(smRow==1)
		if( !bUtil.in(cnt,c(min=0,max=5)) ){
			crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
		}
		cnt <- sum(smRow==4)
		if( !bUtil.in(cnt,c(min=0,max=1)) ){
			crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 02.%d",crObj$cId,cnt)
		}
		cnt <- sum(smRow>=3)
		if( !bUtil.in(cnt,c(min=0,max=1)) ){
			crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
		}
		return( crObj )
	} # rObj$cutFLst[1]( )

	rObj$createCutter <- function( lastMtx ,tgtId=c(hName="", mName="", fcName="") ,auxInfo=c(auxInfo="") ){

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

				if( cutterObj$idObj[["fcName"]] %in% c("rebV.r") ) 					cutF <- rObj$cutFLst[["rebV.r"	]]
				if( cutterObj$idObj[["fcName"]] %in% c("rebL","rebR") ) 			cutF <- rObj$cutFLst[["rebLR"	]]
				if( cutterObj$idObj[["fcName"]] %in% c("rebC.r","rebC2.r") ) 		cutF <- rObj$cutFLst[["rebCn.r"	]]
				if( cutterObj$idObj[["fcName"]] %in% c("rebC.c","rebC2.c") ) 		cutF <- rObj$cutFLst[["rebCn.c"	]]
				if( cutterObj$idObj[["fcName"]] %in% c("rebC.f","rebC2.f") ) 		cutF <- rObj$cutFLst[["rebCn.f"	]]
				if( cutterObj$idObj[["fcName"]] %in% c("inc.r","inc.r2","inc.r3") ) cutF <- rObj$cutFLst[["inc.rn"	]]
				if( cutterObj$idObj[["fcName"]] %in% c("inc.c","inc.c2","inc.c3") ) cutF <- rObj$cutFLst[["inc.cn"	]]
				if( cutterObj$idObj[["fcName"]] %in% c("inc.f","inc.f2") ) 			cutF <- rObj$cutFLst[["inc.fn"	]]

				cObj <- cutF( scoreMtx[idx,] ,cutterObj$idObj[["fcName"]] )
				if( cObj$cutFlag ){
					infoStr <- sprintf("cut Id : %s",paste(cObj$cId,collapse=",") )
					cutLst[[1+length(cutLst)]] <- list( idx=idx ,idObjDesc=cutterObj$idObjDesc ,info=infoStr )
				}
			}

			return( cutLst )
		} # cutterObj$cut()

		return(cutterObj)
	}

	return( rObj )
} # bFCust.byFCol_A_score2_A( )


bFCust.byFCol_A_score2_rebVR <- function( ){	# 폐지 예정.

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

#	typ="c_byFCol"	,hName="*"	,mName="score2"	,pName="*"	,fcName="*" )
bFCust.byFCol_A_score2_A_rReb01 <- function( ){
	#	row rebind라고는 했지만 사실 seq 이다. 
	#	즉 이전 evt 발생 phase에서 다음에도 동일 evt가 일어나는지 체크(다음에서 추가적으로 발생하는 evt는 상관없음.)
	rObj <- list( )
	rObj$defId <- c( typ="c_byFCol"	,hName="*"	,mName="score2"	,pName="*"	,fcName="*" )	# row filt ID
	rObj$description <- sprintf("(cust)  ")

	rObj$evtLst <- FCust_score2EvtLst
	rObj$fireThld.min <- c("rebV.r"=3 ,"rebL"=2 ,"rebR"=2
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
					infoStr <- sprintf("cut Id : rReb01(%s)",chkRst$fireCutId )
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
					chkRstObj$fireCutId = c( chkRstObj$fireCutId ,sprintf("evtReb %d/%d",sum(matFlag),fireThld.min ) 
					)
				}
			}

			# 기타 추가 reb Check -------------------------------------------------------------

			return( chkRstObj )
		}

		return(cutterObj)
	}

	return( rObj )
} # bFCust.byFCol_A_score2_A_rReb01( )

#	c( typ="c_byFCol"	,hName="*"	,mName="score2"	,fcName="*"  )
bFCust.byFCol_A_score2_A_rRebAA <- function( ){
	rObj <- list( )
	rObj$defId <- c( typ="c_byFCol"	,hName="*"	,mName="score2"	,fcName="*"  )
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

		# cutterObj$defId["fcName"] <- tgtId["fcName"]
		# 	# 주의 : defId에서의 "*" 값은 타 정의된 cutter 함수가 있으면 제거된다.
		# 	#		때문에 무조건 살리기 위해서는 fcName값을 강제 설정해야만 한다.
		# 	#		이런 경우, fcName 별 별도 로직을 추가하려면 createCutter() 함수 내에서 구현해야 한다.
		#	bFCust.byFCol_A_score2_rebVR() 이 폐지됨에 따라 필요 없어졌다.

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

				diffCnt <- sum( cutterObj$lastRow!=scoreMtx[idx,] )
				if( !bUtil.in(diffCnt,c(min=1,max=11)) ){
					infoStr <- sprintf("cut Id : rRebAA (raw all diffCnt:%d hpnCnt:%d)",diffCnt,sum(cutterObj$lastRow>0) )
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
#		- mtxGrp <- getScoreMtx.grp_byHIdx( scoreMtx.grp )
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
		if( rObj$defId["mName"] != tgtId["mName"] )	return( cutterObj )

		cutterObj$evt <- rObj$getMtxEvt_byRow( mtxLst )
		if( tgtId["mName"]==cutterObj$defId["mName"] ){
			cutterObj$chkEvt.last <- bFCust.get_byHIdx_score2ChkEvt( mtxLst[[length(mtxLst)]] )
		}

		cutterObj$mtxLst <- mtxLst

		stdEvt.l2 <- NULL
		if( 1<length(mtxLst) ){
			stdEvt.l2 <- bUtil.getEvt_byHIdx( mtxLst[[length(mtxLst)-1]] ,rObj$evtLst ,NULL )
		}
		cutterObj$stdEvt.l <- bUtil.getEvt_byHIdx( mtxLst[[length(mtxLst)]] ,rObj$evtLst ,lastEvt=stdEvt.l2 )

		cutterObj$cut <- function( scoreMtx ,aIdx ){
			# scoreMtx 는 1개 aZoid에 관한 [fCol,phase] mtx임을 유의.
			# 	(즉, 이 함수는 한 개 aZoid에 대한 처리로직이다.)
			#	단 surDf와 cutLst는 다른 cut함수들 결과와의 호환성 유지를 위해 구조유지.
			cutId <- character(0)

			chkEvt <- bFCust.get_byHIdx_score2ChkEvt( scoreMtx )
			for( cutIdx in names(cutterObj$cutLst) ){ # cutIdx <- names(cutterObj$cutLst)[1]
				cutId <- c( cutId ,cutterObj$cutLst[[cutIdx]]( scoreMtx ,chkEvt ) )
			}

			cutLst <- list()
			if( 0<length(cutId) ){
				infoStr <- sprintf("cut Id : %s",paste(cutId,collapse=",") )
				cutLst[[1+length(cutLst)]] <- list( idx=aIdx ,idObjDesc=cutterObj$idObjDesc ,info=infoStr )
			}

			return( cutLst )
		} # cutterObj$cut()

		cutterObj$cutLst <- list()
		cutterObj$cutLst[["F_stdEvtChk"]] <- function( scoreMtx ,chkEvt=NULL ){
			rCutId <- character(0)

			ignoreOpt <- c( hpnInfo_phaseReb=NA
								,rebMtx.all.rebRaw=NA	,rebMtx.ph.raw=NA	,rebMtx.fCol.raw=NA	,rebMtx.phReb.raw=NA
								,summary.raw.ph=NA		,summary.raw.fCol=NA
							)	# ignoreOpt 자체가 NULL값이면 기본 세팅으로 ignore 동작. NA이면 그 부분에 대해서 ignore

			stdEvt <- bUtil.getEvt_byHIdx( scoreMtx ,evtLst=rObj$evtLst ,lastEvt=cutterObj$stdEvt.l ,ignoreOpt=ignoreOpt )
			evtCnt.tot <- 0

			# <Standard> ----------------------------------------------------------------------

			rebCnt <- sum( stdEvt$hpnInfo$phaseReb["reb",] & (stdEvt$hpnInfo$phaseReb["hpn",]>0) )
			maxThld <- 2		;evtCnt.tot <- evtCnt.tot + rebCnt
			if( rebCnt>=maxThld ) rCutId <- c( rCutId, sprintf("stdEvt.hpn.phaseReb %d",rebCnt) )

			evtReb <- stdEvt$evtInfo$phaseReb["reb",]
			evtCnt <- sum( evtReb[ stdEvt$evtInfo$phaseReb["hpn",]>0 ] )
			maxThld <- 2		;evtCnt.tot <- evtCnt.tot + evtCnt
			if( evtCnt>=maxThld ) rCutId <- c( rCutId, sprintf("stdEvt.evt.phaseReb %d",evtCnt) )

			if( !is.null(stdEvt$rebInfo) ){
				summCnt <- apply( stdEvt$rebInfo$summMtx ,1 ,sum )
				evtCnt.tot <- evtCnt.tot + sum(summCnt)
				maxThld <- 6
				if( summCnt["raw"] >=maxThld ){
					summ <- stdEvt$rebInfo$summMtx["raw",]
					summ <- summ[ summ>0 ]
					rptStr <- paste( names(summ) ,summ ,sep=":")
					rCutId <- c( rCutId, sprintf("stdEvt.rebInfo.summ raw - %s",paste(rptStr,collapse="/") ) )
				}

				maxThld <- 4
				if( summCnt["evt"] >=maxThld ){
					summ <- stdEvt$rebInfo$summMtx["evt",]
					summ <- summ[ summ>0 ]
					rptStr <- paste( names(summ) ,summ ,sep=":")
					rCutId <- c( rCutId, sprintf("stdEvt.rebInfo.summ evt - %s",paste(rptStr,collapse="/") ) )
				}
			}

			if( !is.null(stdEvt$rebSummReb) ){
				rsrSum <- sum( stdEvt$rebSummReb$hpnRebMtx )	# tot sum of rebSummReb 
				maxThld <- 4		;evtCnt.tot <- evtCnt.tot + rsrSum
				if( rsrSum>=maxThld )	rCutId <- c( rCutId, sprintf("stdEvt.rebSummReb - %d",rsrSum) )
			}

			evtMask <- stdEvt$evtInfo$evtMask
			colSum <- apply( evtMask ,2 ,sum )
			evtCnt <- sum(colSum >= 4)
			maxThld <- 2		;evtCnt.tot <- evtCnt.tot + evtCnt
			if( evtCnt >= maxThld )	rCutId <- c( rCutId, sprintf("evtRowSum %d",evtCnt) )

			maxThld <- 9
			if( evtCnt.tot >= maxThld )	rCutId <- c( rCutId, sprintf("evtCnt.tot %d",evtCnt.tot) )

			return( rCutId )
		}
		cutterObj$cutLst[["F_matEvt"]] <- function( scoreMtx ,chkEvt=NULL ){
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
			if( any(rebCnt>3) ) rCutId <- c( rCutId, sprintf("matRebCnt.%d/%d",sum(rebCnt>1),max(rebCnt)) )

			# raw idff -------------------------------------------
			nMatchCnt <- sum(scoreMtx!=cutterObj$evt$lastMtxRaw)
			surWindow <- c(min=50,max=80)
			if( !bUtil.in(nMatchCnt,surWindow) ) rCutId <- c( rCutId, sprintf("rawDiffCnt.%d(%d~%d)",nMatchCnt,surWindow["min"],surWindow["max"]) )

			# phRawMat -------------------------------------------
			fireThld <- 2
			cnt.allMat <- 0
			for( pName in colnames(scoreMtx) ){
				# 0 이 많으면 적용.
				# if( 0==sum(scoreMtx[,pName]) )	next

				if( any(scoreMtx[,pName]!=cutterObj$evt$lastMtxRaw[,pName]) ) next

				cnt.allMat <- cnt.allMat + 1
			}
			if( fireThld<=cnt.allMat )	rCutId <- c( rCutId, sprintf("phRawMatCnt.%d",cnt.allMat) )

			# fColRawMat -------------------------------------------
			fireThld <- 4
			cnt.allMat <- 0
			for( fColName in rownames(scoreMtx) ){
				# 0 이 많으면 적용.
				if( 0==sum(scoreMtx[fColName,]) )	next

				if( any(scoreMtx[fColName,]!=cutterObj$evt$lastMtxRaw[fColName,]) ) next

				cnt.allMat <- cnt.allMat + 1
			}
			if( fireThld<=cnt.allMat )	rCutId <- c( rCutId, sprintf("fColRawMatCnt.%d",cnt.allMat) )

			# banPastH -------------------------------------------
			banPastH <- 19	# 바로 이전 H를 제외한 크기(rawDiffCnt에서 체크되므로)
			surWindow <- c(min=30,max=89)
			mtxLst.len <- length(cutterObj$mtxLst)
			endPoint <- mtxLst.len - banPastH -1
			checkSpan <- (mtxLst.len-1):ifelse(1>endPoint,1,endPoint)
			if( length(mtxLst) < banPastH )	banPastH <- length(mtxLst)
			for( idx in seq_len(length(checkSpan)) ){
				chkIdx <- checkSpan[idx]
				nMatchCnt <- sum(scoreMtx!=cutterObj$mtxLst[[chkIdx]])
				if( !bUtil.in(nMatchCnt,surWindow) ){
					rCutId <- c( rCutId, sprintf("banPastH.%d in past %d(%d~%d)",nMatchCnt,idx,surWindow["min"],surWindow["max"]) )
					break
				}
			}

			return( rCutId )
		}
		cutterObj$cutLst[["F_colMat"]] <- function( scoreMtx ,chkEvt=NULL ){
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
			surWindow <- c(min=0,max=10)	# survive window
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
		cutterObj$cutLst[["F_hpnInfo"]] <- function( scoreMtx ,chkEvt ){
			rCutId <- character(0)

			# Hpn(17*13) -------------------------------------------
			tot <- chkEvt$hpnInfo$tot
			surWindow <- c(min=25,max=65)
			if( !bUtil.in(tot,surWindow) ) rCutId <- c( rCutId, sprintf("HpnTot.%d(%d~%d)",tot,surWindow["min"],surWindow["max"]) )

			tot <- sum(chkEvt$hpnInfo$fCol==0)
			surWindow <- c(min=1,max=6)
			if( !bUtil.in(tot,surWindow) ) rCutId <- c( rCutId, sprintf("HpnFCol.%d(%d~%d)",tot,surWindow["min"],surWindow["max"]) )

			tot <- sum(chkEvt$hpnInfo$phase==0)
			surWindow <- c(min=0,max=3)
			if( !bUtil.in(tot,surWindow) ) rCutId <- c( rCutId, sprintf("HpnPh.%d(%d~%d)",tot,surWindow["min"],surWindow["max"]) )

			# Hpn Last-----------------------------------------
			hpnInfo.c <- chkEvt$hpnInfo
			hpnInfo.l <- cutterObj$chkEvt.last$hpnInfo

			surWindow <- c(min=34,max=55)	# chkEvt$hpnInfo$tot 범위 참조
			tot.surLC <- c( lEvt=bUtil.in(hpnInfo.l$tot,surWindow) ,cEvt=bUtil.in(hpnInfo.c$tot,surWindow) )
			if( all(!tot.surLC) ){
				rCutId <- c( rCutId, sprintf("Hpn.tot Evt dup(%d~%d) %d->%d"
							,surWindow["min"],surWindow["max"],hpnInfo.l$tot,hpnInfo.c$tot) )	
			}

			surWindow <- c(min=1,max=6)	# chkEvt$hpnInfo$fCol 범위 참조
			tot.l <- sum(hpnInfo.l$fCol==0)
			tot.c <- sum(hpnInfo.c$fCol==0)
			fCol.surLC <- c( lEvt=bUtil.in(tot.l,surWindow) ,cEvt=bUtil.in(tot.c,surWindow) )
			if( all(!fCol.surLC) ){
				rCutId <- c( rCutId, sprintf("Hpn.fCol Evt dup(%d~%d) %d->%d"
							,surWindow["min"],surWindow["max"],tot.l,tot.c) )	
			}

			surWindow <- c(min=0,max=1)	# chkEvt$hpnInfo$phase 범위 참조
			tot.l <- sum(hpnInfo.l$phase==0)
			tot.c <- sum(hpnInfo.c$phase==0)
			phase.surLC <- c( lEvt=bUtil.in(tot.l,surWindow) ,cEvt=bUtil.in(tot.c,surWindow) )
			if( all(!phase.surLC) ){
				rCutId <- c( rCutId, sprintf("Hpn.phase Evt dup(%d~%d) %d->%d"
							,surWindow["min"],surWindow["max"],tot.l,tot.c) )	
			}

			cnt.fCol <- sum(hpnInfo.c$fCol!=hpnInfo.l$fCol)
			cnt.phase <- sum(hpnInfo.c$phase!=hpnInfo.l$phase)
			firThld <- 17
			if( firThld >= (cnt.fCol+cnt.phase) ){
				rCutId <- c( rCutId, sprintf("HpnXY.(thld:%d) fCol:%d phase:%d",firThld,cnt.fCol,cnt.phase) 
				)
			}

			return( rCutId )
		}
		cutterObj$cutLst[["F_colPairMat"]] <- function( scoreMtx ,chkEvt ){

			rCutId <- character(0)
			hpnSum <- 0

			# rebC r/c/f -------------------------------------------
			matCnt <- chkEvt$colPairMat[["rebC.m"]]
			matCntSum <- sum(matCnt==3)
			if( !bUtil.in(matCntSum,eadge=c(min=0,max=2)) ){
				rCutId <- c( rCutId, sprintf("rebC.m3 %d",matCntSum) )
			}

			matCntSum <- sum(matCnt==2)
			if( !bUtil.in(matCntSum,eadge=c(min=0,max=4)) ){
				rCutId <- c( rCutId, sprintf("rebC.m2 %d",matCntSum) )
			}

			# inc r/c/f -------------------------------------------
			matCnt <- chkEvt$colPairMat[["inc.m"]]
			matCntSum <- sum(matCnt==3)
			if( !bUtil.in(matCntSum,eadge=c(min=0,max=2)) ){
				rCutId <- c( rCutId, sprintf("inc.m3 %d",matCntSum) )
			}
			hpnSum <- hpnSum + matCntSum

			matCntSum <- sum(matCnt==2)
			if( !bUtil.in(matCntSum,eadge=c(min=0,max=2)) ){
				rCutId <- c( rCutId, sprintf("inc.m2 %d",matCntSum) )
			}
			hpnSum <- hpnSum + matCntSum

			# inc r/c (1,2,3) -------------------------------------------
			matFlag <- chkEvt$colPairMat[["incRC123.m"]]
			if( !bUtil.in(matCntSum,eadge=c(min=0,max=1)) ){
				rCutId <- c( rCutId, sprintf("inc.rc %d",matCntSum) )
			}
			hpnSum <- hpnSum + matCntSum

			if( !bUtil.in(hpnSum,eadge=c(min=0,max=4)) ){
				rCutId <- c( rCutId, sprintf("hpnSum.cpm %d",hpnSum) )
			}

			return( rCutId )
		}
		cutterObj$cutLst[["F_colPairMat.L"]] <- function( scoreMtx ,chkEvt ){	# last happen

			rCutId <- character(0)
			hpnSum <- 0

			chkEvt.last <- cutterObj$chkEvt.last
			# rebC r/c/f -------------------------------------------
			matCnt <- chkEvt$colPairMat[["rebC.m"]]
			flag <- chkEvt$colPairMat[["rebC.m"]] == chkEvt.last$colPairMat[["rebC.m"]]
			flag[ chkEvt$colPairMat[["rebC.m"]]==0 ] <- FALSE

			matCntSum <- sum(flag)
			if( !bUtil.in(matCntSum,eadge=c(min=0,max=1)) ){
				rCutId <- c( rCutId, sprintf("LrebC.m %d",matCntSum) )
			}
			hpnSum <- hpnSum + matCntSum
			matCntSum <- sum(matCnt[flag]==3)
			if( !bUtil.in(matCntSum,eadge=c(min=0,max=1)) ){
				rCutId <- c( rCutId, sprintf("LrebC.m3 %d",matCntSum) )
			}

			# matCnt <- chkEvt$colPairMat[["inc.m"]]	# inc.m
			matCnt <- chkEvt$colPairMat[["inc.m"]]
			flag <- chkEvt$colPairMat[["inc.m"]] == chkEvt.last$colPairMat[["inc.m"]]
			flag[ chkEvt$colPairMat[["inc.m"]]==0 ] <- FALSE

			matCntSum <- sum(flag)
			if( !bUtil.in(matCntSum,eadge=c(min=0,max=2)) ){
				rCutId <- c( rCutId, sprintf("Linc.m %d",matCntSum) )
			}
			hpnSum <- hpnSum + matCntSum
			matCntSum <- sum(matCnt[flag]==3)
			if( !bUtil.in(matCntSum,eadge=c(min=0,max=2)) ){
				rCutId <- c( rCutId, sprintf("Linc.m3 %d",matCntSum) )
			}

			# inc r/c (1,2,3) -------------------------------------------
			matCnt <- chkEvt$colPairMat[["incRC123.m"]]
			flag <- chkEvt$colPairMat[["incRC123.m"]] & chkEvt.last$colPairMat[["incRC123.m"]]

			matCntSum <- sum(flag)
			if( !bUtil.in(matCntSum,eadge=c(min=0,max=2)) ){
				rCutId <- c( rCutId, sprintf("Linc123.m %d",matCntSum) )
			}
			hpnSum <- hpnSum + matCntSum

			if( !bUtil.in(hpnSum,eadge=c(min=0,max=2)) ){
				rCutId <- c( rCutId, sprintf("L.hpnSum.cpm %d",hpnSum) )
			}

			return( rCutId )
		}
		cutterObj$cutLst[["F_rebLR"]] <- function( scoreMtx ,chkEvt=NULL ){
			rCutId <- character(0)

			sum.rebLR <- scoreMtx["rebL",]+scoreMtx["rebR",]

			chkCol <- setdiff( colnames(scoreMtx),"basic" )
			if( any(sum.rebLR[chkCol]>1) ){
				hpnPh <- names(sum.rebLR)[ sum.rebLR[chkCol]>1 ]
				rCutId <- c( rCutId, sprintf("rebLR %s",paste(hpnPh,collapse="/") ) )
			}

			return( rCutId )
		}
		cutterObj$cutLst[["F_rowPairMat"]] <- function( scoreMtx ,chkEvt ){

			rCutId <- character(0)
			hpnSum <- 0

			idStr <- "rr"
			matCnt <- sum( chkEvt$rowPairMat[[idStr]] )
			if( !bUtil.in(matCnt,eadge=c(min=0,max=6)) ){
				rCutId <- c( rCutId, sprintf("%s %d",idStr,matCnt) )
			}
			idStr <- "rc"
			matCnt <- sum( chkEvt$rowPairMat[[idStr]] )
			if( !bUtil.in(matCnt,eadge=c(min=0,max=4)) ){
				rCutId <- c( rCutId, sprintf("%s %d",idStr,matCnt) )
			}
			idStr <- "rf"
			matCnt <- sum( chkEvt$rowPairMat[[idStr]] )
			if( !bUtil.in(matCnt,eadge=c(min=0,max=2)) ){
				rCutId <- c( rCutId, sprintf("%s %d",idStr,matCnt) )
			}
            hpnSum <- hpnSum + matCnt


			idStr <- "ir12"
			matCnt <- sum( chkEvt$rowPairMat[[idStr]] )
			if( !bUtil.in(matCnt,eadge=c(min=0,max=2)) ){
				rCutId <- c( rCutId, sprintf("%s %d",idStr,matCnt) )
			}
            hpnSum <- hpnSum + matCnt
			idStr <- "ir13"
			matCnt <- sum( chkEvt$rowPairMat[[idStr]] )
			if( !bUtil.in(matCnt,eadge=c(min=0,max=2)) ){
				rCutId <- c( rCutId, sprintf("%s %d",idStr,matCnt) )
			}
            hpnSum <- hpnSum + matCnt			
			idStr <- "ir23"
			matCnt <- sum( chkEvt$rowPairMat[[idStr]] )
			if( !bUtil.in(matCnt,eadge=c(min=0,max=2)) ){
				rCutId <- c( rCutId, sprintf("%s %d",idStr,matCnt) )
			}
            hpnSum <- hpnSum + matCnt


			idStr <- "ic12"
			matCnt <- sum( chkEvt$rowPairMat[[idStr]] )
			if( !bUtil.in(matCnt,eadge=c(min=0,max=2)) ){
				rCutId <- c( rCutId, sprintf("%s %d",idStr,matCnt) )
			}
            hpnSum <- hpnSum + matCnt
			idStr <- "ic12"
			matCnt <- sum( chkEvt$rowPairMat[[idStr]] )
			if( !bUtil.in(matCnt,eadge=c(min=0,max=2)) ){
				rCutId <- c( rCutId, sprintf("%s %d",idStr,matCnt) )
			}
            hpnSum <- hpnSum + matCnt
			idStr <- "ic23"
			matCnt <- sum( chkEvt$rowPairMat[[idStr]] )
			if( !bUtil.in(matCnt,eadge=c(min=0,max=2)) ){
				rCutId <- c( rCutId, sprintf("%s %d",idStr,matCnt) )
			}
            hpnSum <- hpnSum + matCnt


			idStr <- "if12"
			matCnt <- sum( chkEvt$rowPairMat[[idStr]] )
			if( !bUtil.in(matCnt,eadge=c(min=0,max=2)) ){
				rCutId <- c( rCutId, sprintf("%s %d",idStr,matCnt) )
			}
            hpnSum <- hpnSum + matCnt

			if( !bUtil.in(hpnSum,eadge=c(min=0,max=8)) ){
				rCutId <- c( rCutId, sprintf("hpnSum.rpm %d",hpnSum) )
			}

			return( rCutId )
		}
		cutterObj$cutLst[["F_rowPairMat.L"]] <- function( scoreMtx ,chkEvt ){

			rCutId <- character(0)
			chkEvt.last <- cutterObj$chkEvt.last
            hpnSum <- 0

			idStr <- "rr"
			matCnt <- sum( chkEvt$rowPairMat[[idStr]] & chkEvt.last$rowPairMat[[idStr]] )
			if( !bUtil.in(matCnt,eadge=c(min=0,max=1)) ){
				rCutId <- c( rCutId, sprintf("L%s %d",idStr,matCnt) )
			}
            hpnSum <- hpnSum + matCnt
			idStr <- "rc"
			matCnt <- sum( chkEvt$rowPairMat[[idStr]] & chkEvt.last$rowPairMat[[idStr]] )
			if( !bUtil.in(matCnt,eadge=c(min=0,max=1)) ){
				rCutId <- c( rCutId, sprintf("L%s %d",idStr,matCnt) )
			}
            hpnSum <- hpnSum + matCnt
			idStr <- "rf"
			matCnt <- sum( chkEvt$rowPairMat[[idStr]] & chkEvt.last$rowPairMat[[idStr]] )
			if( !bUtil.in(matCnt,eadge=c(min=0,max=1)) ){
				rCutId <- c( rCutId, sprintf("L%s %d",idStr,matCnt) )
			}
            hpnSum <- hpnSum + matCnt


			idStr <- "ir12"
			matCnt <- sum( chkEvt$rowPairMat[[idStr]] & chkEvt.last$rowPairMat[[idStr]] )
			if( !bUtil.in(matCnt,eadge=c(min=0,max=1)) ){
				rCutId <- c( rCutId, sprintf("L%s %d",idStr,matCnt) )
			}
            hpnSum <- hpnSum + matCnt
			idStr <- "ir13"
			matCnt <- sum( chkEvt$rowPairMat[[idStr]] & chkEvt.last$rowPairMat[[idStr]] )
			if( !bUtil.in(matCnt,eadge=c(min=0,max=1)) ){
				rCutId <- c( rCutId, sprintf("L%s %d",idStr,matCnt) )
			}
            hpnSum <- hpnSum + matCnt
			idStr <- "ir23"
			matCnt <- sum( chkEvt$rowPairMat[[idStr]] & chkEvt.last$rowPairMat[[idStr]] )
			if( !bUtil.in(matCnt,eadge=c(min=0,max=1)) ){
				rCutId <- c( rCutId, sprintf("L%s %d",idStr,matCnt) )
			}
            hpnSum <- hpnSum + matCnt


			idStr <- "ic12"
			matCnt <- sum( chkEvt$rowPairMat[[idStr]] & chkEvt.last$rowPairMat[[idStr]] )
			if( !bUtil.in(matCnt,eadge=c(min=0,max=1)) ){
				rCutId <- c( rCutId, sprintf("L%s %d",idStr,matCnt) )
			}
            hpnSum <- hpnSum + matCnt
			idStr <- "ic12"
			matCnt <- sum( chkEvt$rowPairMat[[idStr]] & chkEvt.last$rowPairMat[[idStr]] )
			if( !bUtil.in(matCnt,eadge=c(min=0,max=1)) ){
				rCutId <- c( rCutId, sprintf("L%s %d",idStr,matCnt) )
			}
            hpnSum <- hpnSum + matCnt
			idStr <- "ic23"
			matCnt <- sum( chkEvt$rowPairMat[[idStr]] & chkEvt.last$rowPairMat[[idStr]] )
			if( !bUtil.in(matCnt,eadge=c(min=0,max=1)) ){
				rCutId <- c( rCutId, sprintf("L%s %d",idStr,matCnt) )
			}
            hpnSum <- hpnSum + matCnt


			idStr <- "if12"
			matCnt <- sum( chkEvt$rowPairMat[[idStr]] & chkEvt.last$rowPairMat[[idStr]] )
			if( !bUtil.in(matCnt,eadge=c(min=0,max=1)) ){
				rCutId <- c( rCutId, sprintf("L%s %d",idStr,matCnt) )
			}
            hpnSum <- hpnSum + matCnt


			if( !bUtil.in(hpnSum,eadge=c(min=0,max=2)) ){
				rCutId <- c( rCutId, sprintf("L.hpnSum.rpm %d",hpnSum) )
			}

			return( rCutId )
		}

		return(cutterObj)
	} # rObj$createCutter( )

	return( rObj )
} # bFCust.byHIdx_A_score2


bFCust.get_byHIdx_score2ChkEvt <- function( scoreMtx ){

	# colPairMat --------------------------------------------------
	colPairMat <- list()
	if( TRUE ){
		# rebC r/c/f -------------------------------------------
		matCnt <- apply( scoreMtx ,2 ,function(cVal){
			pair1 <- cVal[c("rebC.r","rebC.c","rebC.f")]
			pair2 <- cVal[c("rebC2.r","rebC2.c","rebC2.f")]

			flag <- pair1==pair2
			if( 0==sum(pair1[flag]) )	return( 0 )

			return( sum(pair1==pair2) )
		})
		colPairMat[["rebC.m"]] <- matCnt

		# inc r/c/f -------------------------------------------
		matCnt <- apply( scoreMtx ,2 ,function(cVal){
			pair1 <- cVal[c("inc.r","inc.c","inc.f")]
			pair2 <- cVal[c("inc.r2","inc.c2","inc.f2")]

			flag <- pair1==pair2
			if( 0==sum(pair1[flag]) )	return( 0 )

			return( sum(pair1==pair2) )
		})
		colPairMat[["inc.m"]] <- matCnt

		# inc r/c (1,2,3) -------------------------------------------
		matFlag <- apply( scoreMtx ,2 ,function(cVal){
			pair1 <- cVal[c("inc.r","inc.c")]
			pair2 <- cVal[c("inc.r2","inc.c2")]
			pair3 <- cVal[c("inc.r3","inc.c3")]

			if( 0==sum(pair1) )	return( FALSE )

			return( (pair1==pair2)&&(pair1==pair3) )
		})
		colPairMat[["incRC123.m"]] <- matFlag

	}

	# rowPairMat
	rowPairMat <- list()
	if( TRUE ){

		F_rowPairMat <- function( rowPair ){
			matFlag <- scoreMtx[rowPair[1],]==scoreMtx[rowPair[2],]
			validFlag <- 0<(scoreMtx[rowPair[1],]+scoreMtx[rowPair[2],])
			matFlag <- matFlag & validFlag
			return( matFlag )
		} # F_rowPairMat( )

		rowPairMat[["rr"]] <- F_rowPairMat( c("rebC.r","rebC2.r") )
		rowPairMat[["rc"]] <- F_rowPairMat( c("rebC.c","rebC2.c") )
		rowPairMat[["rf"]] <- F_rowPairMat( c("rebC.f","rebC2.f") )
		rowPairMat[["ir12"]] <- F_rowPairMat( c("inc.r","inc.r2") )
		rowPairMat[["ir13"]] <- F_rowPairMat( c("inc.r","inc.r3") )
		rowPairMat[["ir23"]] <- F_rowPairMat( c("inc.r2","inc.r3") )
		rowPairMat[["ic12"]] <- F_rowPairMat( c("inc.c","inc.c2") )
		rowPairMat[["ic13"]] <- F_rowPairMat( c("inc.c","inc.c3") )
		rowPairMat[["ic23"]] <- F_rowPairMat( c("inc.c2","inc.c3") )
		rowPairMat[["if12"]] <- F_rowPairMat( c("inc.f","inc.f2") )
	}

	# hpnInfo
	hpnInfo <- list( tot=	sum(scoreMtx>0)
					,fCol=	apply( scoreMtx ,1 ,function(byRV){ length(byRV) - sum(byRV==0) })
					,phase= apply( scoreMtx ,2 ,function(byCV){ length(byCV) - sum(byCV==0)})
	)

	rObj <- list( colPairMat=colPairMat ,rowPairMat=rowPairMat ,hpnInfo=hpnInfo )

	return( rObj )
} # bFCust.get_byHIdx_score2ChkEvt( )


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


