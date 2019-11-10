FCust_score3EvtLst <- list( "rebPtn.1"=2:4 ,"rebPtn.n"=1
                                ,"snMax.r"=2:4  ,"snFCnt.r"=2:4
                                ,"snMax.c"=1  ,"snFCnt.c"=1
							)

FCust_score3minMaxLst <- list( "rebPtn.1"=c(min=0,max=2) ,"rebPtn.n"=c(min=0,max=0) 
                                ,"snMax.r"=c(min=0,max=3)  ,"snFCnt.r"=c(min=0,max=2)
                                ,"snMax.c"=c(min=0,max=1)  ,"snFCnt.c"=c(min=0,max=1)
                            )

bFCust.score3HardFlag <- function( hName ,pName ){
	hardPh	<- c("sfcLate")
	hardH	<- c("basic")
	
	hFlag <- hName %in% hardPh
	pFlag <- pName %in% hardH
	
	flag <- c( all=(hFlag&&pFlag) ,any=(hFlag||pFlag) ,h=hFlag ,p=pFlag )
	return( list( flag=flag ,hardPh=hardPh ,hardH=hardH ) )
}

#	[score3:Col Cutter(1 col)] ------------------------------------------------------------------
#	c( typ="cust"	,hName="*"	,mName="score3"	,pName="*"	,fcName="*" )
bFCust.A_score3_A_A <- function(  ){

	rObj <- list( defId=c( typ="cust"	,hName="*"	,mName="score3"	,pName="*"	,fcName="*" ) )

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

        cutterObj$minMax <- FCust_score3minMaxLst[[tgtId["fcName"]]]

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
} # bFCust.A_score3_A_A( )

#	[score3:Col Cutter(N col)] ------------------------------------------------------------------
bFCust.A_score3_A_Row01 <- function(  ){
	rObj <- list( )
	rObj$defId <- c( typ="c_NCol.5"	,hName="*"	,mName="score3"	,pName="*"	,rFId="Row01" )	# row filt ID
	rObj$description <- sprintf("(cust)  ")

	rObj$evtLst <- FCust_score3EvtLst

	rObj$cutFLst <- list()
	rObj$cutFLst[[1+length(rObj$cutFLst)]] <- function( smRow ,evt ){
		crObj <- list( cutFlag=F ,cId="hpnOne" ) # cut result object, cut Id

		rCol	<- c("snMax.r","snFCnt.r")
		cCol	<- c("snMax.c","snFCnt.c")

		# matFlag <- smRow[rCol]==smRow[cCol]
		# if( all(matFlag) && (sum(smRow[rCol])>0) ){
		# 	#	적용 대기 
		# 	crObj$cutFlag <- TRUE
		# 	crObj$cId <- c( crObj$cId ,sprintf( "<rcValMat %d>",sum(matFlag) ) )
		# }

		evtCnt <- sum( !is.na(bUtil.getEvtVal(smRow,rObj$evtLst)) )
		if( !bUtil.in(evtCnt,c(min=0,max=1)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<EvtCnt %d>",evtCnt) )
		}

		cnt <- sum( smRow==0 )
		if( !bUtil.in(cnt,c(min=3,max=6)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<ZeroCnt %d>",cnt) )
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
} # bFCust.A_score3_A_Row01( )

#	[score3:Col Cutter(Rebound/Sequencial)] -----------------------------------------------------
bFCust.A_score3_A_rReb01 <- function(  ){	# evt rebind
	#	row rebind라고는 했지만 사실 seq 이다. 
	#	즉 이전 evt 컬럼이 다음에도 동일 evt가 일어나는지 체크 (다음에서 추가적으로 발생하는 evt는 상관없음)
	#	하드 기준 : hName:sfcLate, pName:basic
	rObj <- list( )
	rObj$defId <- c( typ="cust_RReb"	,hName="*"	,mName="score3"	,pName="*"	,rFId="rReb01" )	# row filt ID
	rObj$description <- sprintf("(cust)  ")

	rObj$evtLst <- FCust_score3EvtLst

	rObj$createCutter <- function( hMtxLst ,tgtId=c(hName="", mName="", pName="") ,auxInfo=c(auxInfo="") ){

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
		hardObj <- bFCust.score3HardFlag(cutterObj$idObj["hName"],cutterObj$idObj["pName"])
		cutterObj$hardFlag <- hardObj$flag	;cutterObj$hardPh <- hardObj$hardPh	;cutterObj$hardH <- hardObj$hardH


		scoreMtxObj <- B.HMtxLst_getMtxLst( hMtxLst , tgtId["hName"] ,tgtId["mName"] ,tgtId["pName"] )
		scoreMtx <- if( is.null(scoreMtxObj) ) NULL else scoreMtxObj$scoreMtx
		cutterObj$scoreMtx <- scoreMtx	# just for debug later..

		cutterObj$evtChkLst <- list()
		if( !is.null(scoreMtx) ){	# build checkLst
			#	fireThld 는 fire가 일어날 동일 패턴 수. NA이면 전부 매치.
			scoreMtx.last <- scoreMtx[nrow(scoreMtx),]

			# cName <- c("pBanN.r","pBanN.n","pLCol","pE3","pE4","pMH","pfNum")
			evtChkInfo <- list( cutId="evtAllReb" ,fireThld=ifelse(cutterObj$hardFlag["p"],1,2) ,evtLst=rObj$evtLst )
			evtChkInfo$evtLast <- bUtil.getEvtVal( scoreMtx.last ,evtChkInfo$evtLst )
			cutterObj$evtChkLst[[1+length(cutterObj$evtChkLst)]] <- evtChkInfo

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

				src <- bUtil.getEvtVal( smRow ,evtChkInfo$evtLst )
				chk <- (src==evtChkInfo$evtLast)[evtChkInfo$evtNaMask]

				if( fireThld <= sum(chk,na.rm=T) ){
					firedCutId <- c( firedCutId ,sprintf("%s.%d",evtChkInfo$cutId,sum(chk,na.rm=T)) )
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
} # bFCust.A_score3_A_rReb01()

bFCust.A_score3_A_rRebAA <- function(  ){	#	이전 마지막 score(cutterObj$lastRow) 값과의 일치여부(raw Value) 

	rObj <- list( )
	rObj$defId <- c( typ="cust_RReb"	,hName="*"	,mName="score3"	,pName="*"	,rFId="rRebAA" )	# row filt ID
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
		
		hardObj <- bFCust.score3HardFlag(cutterObj$idObj["hName"],cutterObj$idObj["pName"])
		cutterObj$hardFlag <- hardObj$flag	;cutterObj$hardPh <- hardObj$hardPh	;cutterObj$hardH <- hardObj$hardH

		scoreMtxObj <- B.HMtxLst_getMtxLst( hMtxLst , tgtId["hName"] ,tgtId["mName"] ,tgtId["pName"] )
		if( is.null(scoreMtxObj) ){	cutterObj$lastRow <- NULL
		} else {
			cutterObj$lastRow <- scoreMtxObj$scoreMtx[nrow(scoreMtxObj$scoreMtx),]

			#	byHIdx 적용
			# if( all(cutterObj$lastRow==0) ){	# 0 이 빈번하다면..
			# 	cutterObj$lastRow <- NULL
			# }
			if( 1 >= sum(cutterObj$lastRow) ){
				cutterObj$lastRow <- NULL
			}

		}

		cutterObj$cut <- function( scoreMtx ,alreadyDead=NULL ){
			val.len <- nrow( scoreMtx )
			if( is.null(alreadyDead) )	alreadyDead <- rep( F, val.len )

			cutLst <- list()
			for( idx in seq_len(val.len) ){
				if( alreadyDead[idx] ) next

				if( is.null(cutterObj$lastRow) )	next

				diffCnt <- sum( cutterObj$lastRow!=scoreMtx[idx,] )
				if( 0==diffCnt ){
					infoStr <- sprintf("cut Id : %s valSum:%d",cutterObj$idObjDesc["rFId"],sum(cutterObj$lastRow) )
					cutLst[[1+length(cutLst)]] <- list( idx=idx ,idObjDesc=cutterObj$idObjDesc ,info=infoStr )
				}
			}

			return( cutLst )
		} # cutterObj$cut()

		return(cutterObj)
	}

	return( rObj )
} # bFCust.A_score3_A_rRebAA()



#	[score3:byFCol(Rebound/Sequencial)] ---------------------------------------------------------
#		- mtxGrp <- getScoreMtx.grp_byFCol( scoreMtx.grp )
#		- nRow 대상이긴 하지만, mtx는 각 scoreMtx 의 fCol 별로 생성된다는 점을 주의
#		- column이 phase이므로 pName 구분이 없고, tgtId에서도 pName이 빠진다. 대신 fcName 필요.
#	c( typ="c_byFCol"	,hName="*"	,mName="score4"	,fcName="*"  )
bFCust.byFCol_A_score3_A <- function( ){

	rObj <- list( )
	rObj$defId <- c( typ="c_byFCol"	,hName="*"	,mName="score3"	,fcName="*"  )
	rObj$description <- sprintf("(cust)  ")

	rObj$cutFLst <- list()
	if( TRUE ){	# build cutFLst
		rObj$cutFLst[["rebPtn.1"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A rebPtn.1" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=5)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow==2)
			if( !bUtil.in(cnt,c(min=0,max=2)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 02.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow>=3)
			if( !bUtil.in(cnt,c(min=0,max=0)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["rebPtn.n"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A rebPtn.n" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow>=2)
			if( !bUtil.in(cnt,c(min=0,max=0)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["snMax.r"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A snMax.r" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=4)) ){
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
		rObj$cutFLst[["snFCnt.r"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A snFCnt.r" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=3)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow==2)
			if( !bUtil.in(cnt,c(min=0,max=2)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 02.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow>=3)
			if( !bUtil.in(cnt,c(min=0,max=0)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["snMax.c"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A snMax.c" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=7)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow==2)
			if( !bUtil.in(cnt,c(min=0,max=2)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 02.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow>=3)
			if( !bUtil.in(cnt,c(min=0,max=0)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["snFCnt.c"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A snFCnt.c" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow==2)
			if( !bUtil.in(cnt,c(min=0,max=0)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 02.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow>=3)
			if( !bUtil.in(cnt,c(min=0,max=0)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )

	}


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

		cutterObj$cutF <- rObj$cutFLst[[ cutterObj$idObj[["fcName"]] ]]

		cutterObj$cut <- function( scoreMtx ,alreadyDead=NULL ){
			val.len <- nrow( scoreMtx )
			if( is.null(alreadyDead) )	alreadyDead <- rep( F, val.len )

			cutLst <- list()
			for( idx in seq_len(val.len) ){
				if( alreadyDead[idx] ) next

				cObj <- cutterObj$cutF( scoreMtx[idx,] ,cutterObj$idObj[["fcName"]] )
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
} # bFCust.byFCol_A_score3_A( )

#	typ="c_byFCol"	,hName="*"	,mName="score3"	,pName="*"	,fcName="*" )
bFCust.byFCol_A_score3_A_rReb01 <- function( ){
	#	row rebind라고는 했지만 사실 seq 이다. 
	#	즉 이전 evt 발생 phase에서 다음에도 동일 evt가 일어나는지 체크(다음에서 추가적으로 발생하는 evt는 상관없음.)
	rObj <- list( )
	rObj$defId <- c( typ="c_byFCol"	,hName="*"	,mName="score3"	,pName="*"	,fcName="*" )	# row filt ID
	rObj$description <- sprintf("(cust)  ")

	rObj$evtLst <- FCust_score3EvtLst
	# rObj$fireThld.min <- c( "rebPtn.1"=1 ,"rebPtn.n"=1 ,"snMax.r"=1 ,"snFCnt.r"=1 ,"snMax.c"=1 ,"snFCnt.c"=1 )
	rObj$fireThld.min <- c( "rebPtn.1"=1 ,"rebPtn.n"=1 ,"snMax.r"=2 ,"snFCnt.r"=1 ,"snMax.c"=2 ,"snFCnt.c"=1 )

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
} # bFCust.byFCol_A_score3_A_rReb01( )

#	c( typ="c_byFCol"	,hName="*"	,mName="score3"	,fcName="*"  )
bFCust.byFCol_A_score3_A_rRebAA <- function( ){
	rObj <- list( )
	rObj$defId <- c( typ="c_byFCol"	,hName="*"	,mName="score3"	,fcName="*"  )
	rObj$description <- sprintf("(cust)  ")

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

		cutterObj$lastRow <- lastMtx[nrow(lastMtx),]
		cutterObj$activated <- sum(cutterObj$lastRow>0) > 0

		cutterObj$cut <- function( scoreMtx ,alreadyDead=NULL ){

			val.len <- nrow( scoreMtx )
			if( is.null(alreadyDead) ){
				alreadyDead <- rep( F, val.len )
			}

			cutLst <- list()
			for( idx in seq_len(val.len) ){
				if( alreadyDead[idx] ) next

				if( !cutterObj$activated || is.null(cutterObj$lastRow) ) next

				if( 1>=sum(scoreMtx[idx,]) ) next

				diffCnt <- sum( cutterObj$lastRow!=scoreMtx[idx,] )
				if( 0==diffCnt ){
					infoStr <- sprintf("cut Id : rRebAA (raw all Mat len:%d)",sum(cutterObj$lastRow>0) )
					cutLst[[1+length(cutLst)]] <- list( idx=idx ,idObjDesc=cutterObj$idObjDesc ,info=infoStr )
				}
			}

			return( cutLst )
		} # cutterObj$cut()

		return(cutterObj)
	}

	return( rObj )
} # bFCust.byFCol_A_score3_A_rRebAA( )








bFCust.byHIdx_A_score3 <- function( ){

	rObj <- list( )
	rObj$defId <- c( typ="cust_byHIdx"	,hName="*"	,mName="score3" )
	rObj$description <- sprintf("(cust)  ")

	rObj$evtLst <- FCust_score2EvtLst

	rObj$createCutter <- function( mtxLst ,tgtId=c(hName="", mName="") ,auxInfo=c(auxInfo="") ){

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
		if( cutterObj$idObj["mName"]!=rObj$defId["mName"] )	return( cutterObj )

		cutterObj$evt <- bUtil.getMtxEvt_byRow( mtxLst ,rObj$evtLst )
		if( tgtId["mName"]==cutterObj$defId["mName"] ){
			cutterObj$chkEvt.last <- bFCust.get_byHIdx_score3ChkEvt( mtxLst[[length(mtxLst)]] )
		}

		cutterObj$mtxLst <- mtxLst
		cutterObj$rebPtn.skipZero <- bUtil.getMtxRebPtn.skipZero( mtxLst ,hpnThld.fCol=NA ,hpnThld.ph=NA )

		cutterObj$cut <- function( scoreMtx ,aIdx ){
			# scoreMtx 는 1개 aZoid에 관한 [fCol,phase] mtx임을 유의.
			# 	(즉, 이 함수는 한 개 aZoid에 대한 처리로직이다.)
			#	단 surDf와 cutLst는 다른 cut함수들 결과와의 호환성 유지를 위해 구조유지.
			cutId <- character(0)

			chkEvt <- bFCust.get_byHIdx_score3ChkEvt( scoreMtx )
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
		cutterObj$cutLst[["F_rebPtn.skipZero"]] <- function( scoreMtx ,chkEvt=NULL ){
			rCutId <- character(0)

			surWin <- c(min=0,max=1)
			cnt.fCol <- sum( 0==cutterObj$rebPtn.skipZero$diffCnt.fCol(scoreMtx) )
			if( !bUtil.in(cnt.fCol,surWin) ){
				rCutId <- c( rCutId, sprintf("skipZero.fCol.%d (%d~%d)",cnt.fCol,surWin["min"],surWin["max"]) )
			}

			surWin <- c(min=0,max=3)
			cnt.ph <- sum( 0==cutterObj$rebPtn.skipZero$diffCnt.ph(scoreMtx) )
			if( !bUtil.in(cnt.ph,surWin) ){
				rCutId <- c( rCutId, sprintf("skipZero.ph.%d (%d~%d)",cnt.ph,surWin["min"],surWin["max"]) )
			}

			surWin <- c(min=0,max=3)
			cntTot <- cnt.fCol + cnt.ph
			if( !bUtil.in(cntTot,surWin) ){
				rCutId <- c( rCutId, sprintf("skipZero.sum.%d (%d~%d)",cntTot,surWin["min"],surWin["max"]) )
			}

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
			if( 1<=sum( matMtx ) ) rCutId <- c( rCutId, sprintf("matHpnCnt.%d",sum( matMtx )) )

			# match happen count(sequencial rebind) ----------------------------------
			rebCnt <- cutterObj$evt$rebCntMtx[matMtx]
			if( any(rebCnt>1) ) rCutId <- c( rCutId, sprintf("matRebCnt.%d/%d",sum(rebCnt>1),max(rebCnt)) )

			# raw idff -------------------------------------------
			nMatchCnt <- sum(scoreMtx!=cutterObj$evt$lastMtxRaw)
			surWindow <- c(min=1,max=17)
			if( !bUtil.in(nMatchCnt,surWindow) ) rCutId <- c( rCutId, sprintf("rawDiffCnt.%d(%d~%d)",nMatchCnt,surWindow["min"],surWindow["max"]) )

			# # phRawMat -------------------------------------------
			fireThld <- 4
			cnt.allMat <- 0
			for( pName in colnames(scoreMtx) ){
				# 0 이 많으면 적용.
				if( 0==sum(scoreMtx[,pName]) )	next

				if( any(scoreMtx[,pName]!=cutterObj$evt$lastMtxRaw[,pName]) ) next

				cnt.allMat <- cnt.allMat + 1
			}
			if( fireThld<=cnt.allMat )	rCutId <- c( rCutId, sprintf("phRawMatCnt.%d",cnt.allMat) )

			# # fColRawMat -------------------------------------------
			fireThld <- 2
			cnt.allMat <- 0
			for( fColName in rownames(scoreMtx) ){
				# 0 이 많으면 적용.
				if( 0==sum(scoreMtx[fColName,]) )	next

				if( any(scoreMtx[fColName,]!=cutterObj$evt$lastMtxRaw[fColName,]) ) next

				cnt.allMat <- cnt.allMat + 1
			}
			if( fireThld<=cnt.allMat )	rCutId <- c( rCutId, sprintf("fColRawMatCnt.%d",cnt.allMat) )

			# # banPastH -------------------------------------------
            if( 1 < sum(scoreMtx>0) ){  #  0 이 너무 많다면 적용.
                banPastH <- 10	# 바로 이전 H를 제외한 크기(rawDiffCnt에서 체크되므로)
                surWindow <- c(min=1,max=22)
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
			surWindow <- c(min=0,max=2)	# survive window
			for( cIdx in 2:ncol(scoreMtx) ){
				if( 2<sum(scoreMtx[,cIdx-0:1]) ) next	# 0이 빈번한 경우.

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

			# Hpn(18*13) -------------------------------------------
			tot <- chkEvt$hpnInfo$tot
			surWindow <- c(min=0,max=11)
			if( !bUtil.in(tot,surWindow) ) rCutId <- c( rCutId, sprintf("HpnTot.%d(%d~%d)",tot,surWindow["min"],surWindow["max"]) )

			tot <- sum(chkEvt$hpnInfo$fCol==0)
			surWindow <- c(min=2,max=6)
			if( !bUtil.in(tot,surWindow) ) rCutId <- c( rCutId, sprintf("HpnFCol.%d(%d~%d)",tot,surWindow["min"],surWindow["max"]) )

			tot <- sum(chkEvt$hpnInfo$phase==0)
			surWindow <- c(min=4,max=13)
			if( !bUtil.in(tot,surWindow) ) rCutId <- c( rCutId, sprintf("HpnPh.%d(%d~%d)",tot,surWindow["min"],surWindow["max"]) )

			# Hpn Last-----------------------------------------
			#	hpn 값이 이전에도 특이했는데 이번에도 특이한 경우를 제거
			#	즉 위에서의 surWindow에 살짝 걸친 경우가 연속발생하는 것을 부정.
			hpnInfo.c <- chkEvt$hpnInfo
			hpnInfo.l <- cutterObj$chkEvt.last$hpnInfo

			cnt.fCol <- sum(hpnInfo.c$fCol!=hpnInfo.l$fCol)
			cnt.phase <- sum(hpnInfo.c$phase!=hpnInfo.l$phase)

			surWindow <- c(min=0,max=4)
			if( !bUtil.in(cnt.fCol,surWindow) ) rCutId <- c( rCutId, sprintf("cnt.fCol.%d(%d~%d)",cnt.fCol,surWindow["min"],surWindow["max"]) )
			surWindow <- c(min=1,max=10)
			if( !bUtil.in(cnt.phase,surWindow) ) rCutId <- c( rCutId, sprintf("cnt.phase.%d(%d~%d)",cnt.phase,surWindow["min"],surWindow["max"]) )

			surWindow <- c(min=2,max=13)
			tot <- sum(cnt.fCol+cnt.phase)
			if( !bUtil.in(tot,surWindow) ) rCutId <- c( rCutId, sprintf("HpnXY.%d(%d~%d)",tot,surWindow["min"],surWindow["max"]) )

			return( rCutId )
		}

		return(cutterObj)
	} # rObj$createCutter( )

	return( rObj )
} # bFCust.byHIdx_A_score2

bFCust.get_byHIdx_score3ChkEvt <- function( scoreMtx ){

	hpnInfo <- list( tot=	sum(scoreMtx>0)
					,fCol=	apply( scoreMtx ,1 ,function(byRV){ length(byRV) - sum(byRV==0) })
					,phase= apply( scoreMtx ,2 ,function(byCV){ length(byCV) - sum(byCV==0)})
	)

	return( list(hpnInfo=hpnInfo) )

} # bFCust.get_byHIdx_score3ChkEvt( )

