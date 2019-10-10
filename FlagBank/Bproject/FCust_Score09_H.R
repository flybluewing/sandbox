FCust_score9EvtLst <- list(		"rCnt"=2:4 ,"rD2"=2:4 ,"rDn"=1:4 ,"rLr"=2:4 ,"rRl"=2:4
								,"eCnt"=2:5 ,"eD2"=2:4 ,"eDn"=1:4 ,"eLr"=2:4 ,"eRl"=2:4
								,"cCnt"=2:4 ,"cD2"=2:4 ,"cDn"=1:4 ,"cLr"=2:4 ,"cRl"=2:4
								,"fCnt"=2:4 ,"fD2"=2:4 ,"fDn"=1:4 ,"fLr"=2:4 ,"fRl"=2:4
							)

FCust_score9minMaxLst <- list( "rCnt"=c(min=0,max=2) ,"rD2"=c(min=0,max=1) ,"rDn"=c(min=0,max=0) ,"rLr"=c(min=0,max=1) ,"rRl"=c(min=0,max=1)
								,"eCnt"=c(min=0,max=2) ,"eD2"=c(min=0,max=1) ,"eDn"=c(min=0,max=1) ,"eLr"=c(min=0,max=3) ,"eRl"=c(min=0,max=3)
								,"cCnt"=c(min=0,max=2) ,"cD2"=c(min=0,max=1) ,"cDn"=c(min=0,max=0) ,"cLr"=c(min=0,max=1) ,"cRl"=c(min=0,max=1)
								,"fCnt"=c(min=0,max=1) ,"fD2"=c(min=0,max=1) ,"fDn"=c(min=0,max=0) ,"fLr"=c(min=0,max=1) ,"fRl"=c(min=0,max=1)
							)


#	[score9:Col Cutter(1 col)] ------------------------------------------------------------------
#	c( typ="cust"	,hName="*"	,mName="score9"	,pName="*"	,fcName="*" )
bFCust.A_score9_A_A <- function(  ){

	rObj <- list( defId=c( typ="cust"	,hName="*"	,mName="score9"	,pName="*"	,fcName="*" ) )

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

        cutterObj$minMax <- FCust_score9minMaxLst[[tgtId["fcName"]]]

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
} # bFCust.A_score9_A_A( )

#	[score9:Col Cutter(N col)] ------------------------------------------------------------------
bFCust.A_score9_A_Row01 <- function(  ){
	rObj <- list( )
	rObj$defId <- c( typ="c_NCol.5"	,hName="*"	,mName="score9"	,pName="*"	,rFId="Row01" )	# row filt ID
	rObj$description <- sprintf("(cust)  ")

	rObj$evtLst <- FCust_score9EvtLst

	rObj$cutFLst <- list()
	rObj$cutFLst[[1+length(rObj$cutFLst)]] <- function( smRow ,evt ){
		crObj <- list( cutFlag=F ,cId="hpnOne" ) # cut result object, cut Id

		evt <- bUtil.getEvtVal( smRow ,FCust_score9EvtLst )

		cntEvt <- sum( evt ,na.rm=T )
		if( !bUtil.in(cntEvt,c(min=0,max=5)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<cntEvt %d>",cntEvt) )
		}

		banCnt <- sum( smRow[c("rCnt","eCnt","cCnt","fCnt")] )
		if( !bUtil.in(banCnt,c(min=0,max=5)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<banCnt %d>",banCnt) )
		}


		d2Cnt <- sum( smRow[c("rD2","eD2","cD2","fD2")] )
		if( !bUtil.in(d2Cnt,c(min=0,max=2)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<d2Cnt %d>",d2Cnt) )
		}

		dnCnt <- sum( smRow[c("rDn","eDn","cDn","fDn")] )
		if( !bUtil.in(dnCnt,c(min=0,max=1)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<dnCnt %d>",dnCnt) )
		}

		dxCnt <- d2Cnt + dnCnt
		if( !bUtil.in(dxCnt,c(min=0,max=2)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<dxCnt %d>",dxCnt) )
		}

		lrEvtCnt <- sum( evt[c("rLr","rRl","eLr","eRl","cLr","cRl","fLr","fRl")] ,na.rm=T )
		if( !bUtil.in(lrEvtCnt,c(min=0,max=3)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<lrEvtCnt %d>",lrEvtCnt) )
		}

		# cnt <- sum( smRow==0 )	#	banCnt 에서 0이 존재하는 이상, 의미없음.
		# if( !bUtil.in(cnt,c(min=16,max=20)) ){
		# 	crObj$cutFlag <- TRUE
		# 	crObj$cId <- c( crObj$cId ,sprintf( "<ZeroCnt %d>",cnt) )
		# }

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
} # bFCust.A_score9_A_Row01( )

#	[score9:Col Cutter(Rebound/Sequencial)] -----------------------------------------------------
bFCust.A_score9_A_rReb01 <- function(  ){	# evt rebind
	#	row rebind라고는 했지만 사실 seq 이다. 
	#	즉 이전 evt 컬럼이 다음에도 동일 evt가 일어나는지 체크 (다음에서 추가적으로 발생하는 evt는 상관없음)
	rObj <- list( )
	rObj$defId <- c( typ="cust_RReb"	,hName="*"	,mName="score9"	,pName="*"	,rFId="rReb01" )	# row filt ID
	rObj$description <- sprintf("(cust)  ")

	rObj$evtLst <- FCust_score9EvtLst

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

		scoreMtxObj <- hMtxLst$getScoreMtxObj( tgtId["hName"] ,tgtId["mName"] ,tgtId["pName"] )
		scoreMtx <- if( is.null(scoreMtxObj) ) NULL else scoreMtxObj$scoreMtx
		cutterObj$scoreMtx <- scoreMtx	# just for debug later..

		cutterObj$evtChkLst <- list()
		if( !is.null(scoreMtx) ){	# build checkLst
			#	fireThld 는 fire가 일어날 동일 패턴 수. NA이면 전부 매치.
			scoreMtx.last <- scoreMtx[nrow(scoreMtx),]

			cName <- c("rCnt","eCnt","cCnt","fCnt")
			evtChkInfo <- list( cutId="banCnt" ,fireThld=1 ,evtLst=rObj$evtLst[cName] )
			evtChkInfo$evtLast <- bUtil.getEvtVal( scoreMtx.last ,evtChkInfo$evtLst )
			cutterObj$evtChkLst[[1+length(cutterObj$evtChkLst)]] <- evtChkInfo

			cName <- c("rD2","eD2","cD2","fD2")
			evtChkInfo <- list( cutId="d2Cnt" ,fireThld=1 ,evtLst=rObj$evtLst[cName] )
			evtChkInfo$evtLast <- bUtil.getEvtVal( scoreMtx.last ,evtChkInfo$evtLst )
			cutterObj$evtChkLst[[1+length(cutterObj$evtChkLst)]] <- evtChkInfo

			cName <- c("rDn","eDn","cDn","fDn")
			evtChkInfo <- list( cutId="dnCnt" ,fireThld=1 ,evtLst=rObj$evtLst[cName] )
			evtChkInfo$evtLast <- bUtil.getEvtVal( scoreMtx.last ,evtChkInfo$evtLst )
			cutterObj$evtChkLst[[1+length(cutterObj$evtChkLst)]] <- evtChkInfo

			cName <- c("rD2","eD2","cD2","fD2"	,"rDn","eDn","cDn","fDn")
			evtChkInfo <- list( cutId="dxCnt" ,fireThld=1 ,evtLst=rObj$evtLst[cName] )
			evtChkInfo$evtLast <- bUtil.getEvtVal( scoreMtx.last ,evtChkInfo$evtLst )
			cutterObj$evtChkLst[[1+length(cutterObj$evtChkLst)]] <- evtChkInfo

			cName <- c("rLr","rRl","eLr","eRl","cLr","cRl","fLr","fRl")
			evtChkInfo <- list( cutId="lrEvtCnt" ,fireThld=1 ,evtLst=rObj$evtLst[cName] )
			evtChkInfo$evtLast <- bUtil.getEvtVal( scoreMtx.last ,evtChkInfo$evtLst )
			cutterObj$evtChkLst[[1+length(cutterObj$evtChkLst)]] <- evtChkInfo

			evtChkInfo <- list( cutId="evtAll" ,fireThld=1 ,evtLst=rObj$evtLst )
			evtChkInfo$evtLast <- bUtil.getEvtVal( scoreMtx.last ,evtChkInfo$evtLst )
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

				src <- bUtil.getEvtVal( smRow ,evtChkInfo$evtLst )
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
} # bFCust.A_score9_A_rReb01()

bFCust.A_score9_A_rRebAA <- function(  ){	#	이전 마지막 score(cutterObj$lastRow) 값과의 일치여부(raw Value) 

	rObj <- list( )
	rObj$defId <- c( typ="cust_RReb"	,hName="*"	,mName="score9"	,pName="*"	,rFId="rRebAA" )	# row filt ID
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

			if( 2>=sum(cutterObj$lastRow) ){
				cutterObj$lastRow <- NULL
			}

			#	빈번하게 발생하는 컬럼은 확인대상 제외...
			# cName <- setdiff( names(cutterObj$lastRow) ,c("min2") )
			# if( all(cutterObj$lastRow[cName]==0) ){
			# 	cutterObj$lastRow <- NULL
			# }
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
} # bFCust.A_score9_A_rRebAA()



#	[score9:byFCol(Rebound/Sequencial)] ---------------------------------------------------------
#		- mtxGrp <- getScoreMtx.grp_byFCol( scoreMtx.grp )
#		- nRow 대상이긴 하지만, mtx는 각 scoreMtx 의 fCol 별로 생성된다는 점을 주의
#		- column이 phase이므로 pName 구분이 없고, tgtId에서도 pName이 빠진다. 대신 fcName 필요.
#	c( typ="c_byFCol"	,hName="*"	,mName="score4"	,fcName="*"  )
bFCust.byFCol_A_score9_A <- function( ){

	rObj <- list( )
	rObj$defId <- c( typ="c_byFCol"	,hName="*"	,mName="score9"	,fcName="*"  )
	rObj$description <- sprintf("(cust)  ")

	rObj$cutFLst <- list()
	if( TRUE ){	# build cutFLst
		#	rCnt rD2 rDn rLr rRl eCnt eD2 eDn eLr eRl cCnt cD2 cDn cLr cRl fCnt fD2 fDn fLr fRl
		rObj$cutFLst[["rCnt"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A rCnt" ) # cut result object, cut Id
			cnt <- sum(smRow)
			if( !bUtil.in(cnt,c(min=1,max=7)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s tot.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=1,max=7)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow==2)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 02.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow >2)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["rD2"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A rD2" ) # cut result object, cut Id
			cnt <- sum(smRow)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s tot.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow==2)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 02.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow >2)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["rDn"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A rDn" ) # cut result object, cut Id
			cnt <- sum(smRow)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["rLr"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A rLr" ) # cut result object, cut Id
			cnt <- sum(smRow)
			if( !bUtil.in(cnt,c(min=0,max=2)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s tot.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow>1)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["rRl"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A rRl" ) # cut result object, cut Id
			cnt <- sum(smRow)
			if( !bUtil.in(cnt,c(min=0,max=2)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s tot.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow>1)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["eCnt"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A eCnt" ) # cut result object, cut Id

			cnt <- sum(smRow)
			if( !bUtil.in(cnt,c(min=0,max=10)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s tot.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=10)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow==2)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 02.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow >2)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}

			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["eD2"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A eD2" ) # cut result object, cut Id
			cnt <- sum(smRow)
			if( !bUtil.in(cnt,c(min=0,max=2)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s tot.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=2)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow==2)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 02.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow >2)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["eDn"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A eDn" ) # cut result object, cut Id
			cnt <- sum(smRow)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["eLr"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A eLr" ) # cut result object, cut Id
			cnt <- sum(smRow)
			if( !bUtil.in(cnt,c(min=0,max=4)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s tot.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow>1)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["eRl"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A eRl" ) # cut result object, cut Id
			cnt <- sum(smRow)
			if( !bUtil.in(cnt,c(min=0,max=4)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s tot.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow>1)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["cCnt"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A cCnt" ) # cut result object, cut Id

			cnt <- sum(smRow)
			if( !bUtil.in(cnt,c(min=0,max=7)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s tot.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=7)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow==2)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 02.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow >2)
			if( !bUtil.in(cnt,c(min=0,max=0)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}

			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["cD2"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A cD2" ) # cut result object, cut Id
			cnt <- sum(smRow)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s tot.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow==2)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 02.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow >2)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["cDn"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A cDn" ) # cut result object, cut Id
			cnt <- sum(smRow)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["cLr"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A cLr" ) # cut result object, cut Id
			cnt <- sum(smRow)
			if( !bUtil.in(cnt,c(min=0,max=2)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s tot.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow>1)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["cRl"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A cRl" ) # cut result object, cut Id
			cnt <- sum(smRow)
			if( !bUtil.in(cnt,c(min=0,max=2)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s tot.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow>1)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["fCnt"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A fCnt" ) # cut result object, cut Id

			cnt <- sum(smRow)
			if( !bUtil.in(cnt,c(min=0,max=2)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s tot.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=2)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow==2)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 02.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow >2)
			if( !bUtil.in(cnt,c(min=0,max=0)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}

			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["fD2"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A fD2" ) # cut result object, cut Id
			cnt <- sum(smRow)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s tot.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow==2)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 02.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow >2)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["fDn"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A fDn" ) # cut result object, cut Id
			cnt <- sum(smRow)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["fLr"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A fLr" ) # cut result object, cut Id
			cnt <- sum(smRow)
			if( !bUtil.in(cnt,c(min=0,max=2)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s tot.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow>1)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["fRl"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A fRl" ) # cut result object, cut Id
			cnt <- sum(smRow)
			if( !bUtil.in(cnt,c(min=0,max=2)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s tot.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow>1)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
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
} # bFCust.byFCol_A_score9_A( )

#	typ="c_byFCol"	,hName="*"	,mName="score9"	,pName="*"	,fcName="*" )
bFCust.byFCol_A_score9_A_rReb01 <- function( ){
	#	row rebind라고는 했지만 사실 seq 이다. 
	#	즉 이전 evt 발생 phase에서 다음에도 동일 evt가 일어나는지 체크(다음에서 추가적으로 발생하는 evt는 상관없음.)
	rObj <- list( )
	rObj$defId <- c( typ="c_byFCol"	,hName="*"	,mName="score9"	,pName="*"	,fcName="*" )	# row filt ID
	rObj$description <- sprintf("(cust)  ")

	rObj$evtLst <- FCust_score9EvtLst
	rObj$fireThld.min <- c( "rCnt"=1 ,"rD2"=1 ,"rDn"=1 ,"rLr"=1 ,"rRl"=1 ,"eCnt"=1 ,"eD2"=1 ,"eDn"=1 ,"eLr"=1 ,"eRl"=1 
							,"cCnt"=1 ,"cD2"=1 ,"cDn"=1 ,"cLr"=1 ,"cRl"=1 ,"fCnt"=1 ,"fD2"=1 ,"fDn"=1 ,"fLr"=1 ,"fRl"=1
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
} # bFCust.byFCol_A_score9_A_rReb01( )

#	c( typ="c_byFCol"	,hName="*"	,mName="score9"	,fcName="*"  )
bFCust.byFCol_A_score9_A_rRebAA <- function( ){
	rObj <- list( )
	rObj$defId <- c( typ="c_byFCol"	,hName="*"	,mName="score9"	,fcName="*"  )
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

		rObj$sum.min <- c( "rCnt"=0 ,"rD2"=1 ,"rDn"=1 ,"rLr"=1 ,"rRl"=1 ,"eCnt"=1 ,"eD2"=1 ,"eDn"=1 ,"eLr"=1 ,"eRl"=1 
								,"cCnt"=1 ,"cD2"=1 ,"cDn"=1 ,"cLr"=1 ,"cRl"=1 ,"fCnt"=1 ,"fD2"=1 ,"fDn"=1 ,"fLr"=1 ,"fRl"=1
						)

		cutterObj$cut <- function( scoreMtx ,alreadyDead=NULL ){

			val.len <- nrow( scoreMtx )
			if( is.null(alreadyDead) ){
				alreadyDead <- rep( F, val.len )
			}

			cutLst <- list()

			for( idx in seq_len(val.len) ){
				if( alreadyDead[idx] ) next

				sumFlag <- sum(scoreMtx[idx,]) >= rObj$sum.min[ cutterObj$idObj[["fcName"]] ]

				if( !sumFlag || is.null(cutterObj$lastRow) ) next

				matFlag <- cutterObj$lastRow==scoreMtx[idx,]
				if( all(matFlag) ){

					infoStr <- sprintf("cut Id : rRebAA (raw all Mat. cnt:%d)",sum(scoreMtx[idx,]) )
					infoStr <- c( infoStr ,sprintf("- %s ",cutterObj$idObj[["fcName"]]) )
					cutLst[[1+length(cutLst)]] <- list( idx=idx ,idObjDesc=cutterObj$idObjDesc ,info=infoStr )

				}
			}

			return( cutLst )
		} # cutterObj$cut()

		return(cutterObj)
	}

	return( rObj )
} # bFCust.byFCol_A_score9_A_rRebAA( )



#	[score9:byHIdx(...)] ---------------------------------------------------------
#		- [fCol,phName] 
#		- mtxGrp <- getScoreMtx.grp_byHIdx( scoreMtx.grp )
#		- hIdxObj <- B.getHMtxLst_byHIdx( hMtxLst )
#       - 개개 score의 최대 최소값이나, evt 발생등은 이미 앞에서 모두 확인되었다.)
#		  따라서 col 방향, row 방향, mtx전체에 대한 rebPtn 체크.
#			(fCol 별 evt 기준 때문에 scoreMtx 개개별로 작성 필요.)

#		c( typ="cust_byHIdx"	,hName="*"	,mName="score9" )
bFCust.byHIdx_A_score9 <- function( ){

	rObj <- list( )
	rObj$defId <- c( typ="cust_byHIdx"	,hName="*"	,mName="score9" )
	rObj$description <- sprintf("(cust)  ")

	rObj$evtLst <- FCust_score2EvtLst

	rObj$createCutter <- function( mtxLst=NULL ,tgtId=c(hName="", mName="") ,auxInfo=c(auxInfo="") ){

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

		cutterObj$evt <- bUtil.getMtxEvt_byRow( mtxLst ,rObj$evtLst )
		if( tgtId["mName"]==cutterObj$defId["mName"] ){
			cutterObj$chkEvt.last <- bFCust.get_byHIdx_score9ChkEvt( mtxLst[[length(mtxLst)]] )
		}

		cutterObj$mtxLst <- mtxLst
		cutterObj$rebPtn.skipZero <- bUtil.getMtxRebPtn.skipZero( mtxLst ,hpnThld.fCol=NA ,hpnThld.ph=NA )

		cutterObj$cut <- function( scoreMtx ,aIdx ){
			# scoreMtx 는 1개 aZoid에 관한 [fCol,phase] mtx임을 유의.
			# 	(즉, 이 함수는 한 개 aZoid에 대한 처리로직이다.)
			#	단 surDf와 cutLst는 다른 cut함수들 결과와의 호환성 유지를 위해 구조유지.
			cutId <- character(0)

			chkEvt <- bFCust.get_byHIdx_score9ChkEvt( scoreMtx )
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
			surWindow <- c(min=2,max=30)
			if( !bUtil.in(nMatchCnt,surWindow) ) rCutId <- c( rCutId, sprintf("rawDiffCnt.%d(%d~%d)",nMatchCnt,surWindow["min"],surWindow["max"]) )

			
			# phRawMat -------------------------------------------
			fireThld <- 3
			cnt.allMat <- 0
			for( pName in colnames(scoreMtx) ){
				# 0 이 많으면 적용.
				if( 0==sum(scoreMtx[,pName]) )	next

				if( any(scoreMtx[,pName]!=cutterObj$evt$lastMtxRaw[,pName]) ) next

				cnt.allMat <- cnt.allMat + 1
			}
			if( fireThld<=cnt.allMat )	rCutId <- c( rCutId, sprintf("phRawMatCnt.%d",cnt.allMat) )

			# fColRawMat -------------------------------------------
			fireThld <- 1
			cnt.allMat <- 0
			for( fColName in rownames(scoreMtx) ){
				# 0 이 많으면 적용.
				if( 0==sum(scoreMtx[fColName,]) )	next

				if( any(scoreMtx[fColName,]!=cutterObj$evt$lastMtxRaw[fColName,]) ) next

				cnt.allMat <- cnt.allMat + 1
			}
			if( fireThld<=cnt.allMat )	rCutId <- c( rCutId, sprintf("fColRawMatCnt.%d",cnt.allMat) )

			# banPastH -------------------------------------------
            if( 1 < sum(scoreMtx>0) ){  #  0 이 너무 많다면 적용.
                banPastH <- 20	# 바로 이전 H를 제외한 크기(rawDiffCnt에서 체크되므로)
                surWindow <- c(min=4,max=46)
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
			surWindow <- c(min=0,max=5)	# survive window
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
		cutterObj$cutLst[["F_hpnInfo"]] <- function( scoreMtx ,chkEvt ){	# working
			rCutId <- character(0)

			# Hpn(18*13) -------------------------------------------
			tot <- chkEvt$hpnInfo$tot
			surWindow <- c(min=1,max=24)
			if( !bUtil.in(tot,surWindow) ) rCutId <- c( rCutId, sprintf("HpnTot.%d(%d~%d)",tot,surWindow["min"],surWindow["max"]) )

			tot <- sum(chkEvt$hpnInfo$fCol==0)
			surWindow <- c(min=10,max=19)
			if( !bUtil.in(tot,surWindow) ) rCutId <- c( rCutId, sprintf("HpnFCol.%d(%d~%d)",tot,surWindow["min"],surWindow["max"]) )

			tot <- sum(chkEvt$hpnInfo$phase==0)
			surWindow <- c(min=2,max=12)
			if( !bUtil.in(tot,surWindow) ) rCutId <- c( rCutId, sprintf("HpnPh.%d(%d~%d)",tot,surWindow["min"],surWindow["max"]) )

			# Hpn Last-----------------------------------------
			#	hpn 값이 이전에도 특이했는데 이번에도 특이한 경우를 제거
			#	즉 위에서의 surWindow에 살짝 걸친 경우가 연속발생하는 것을 부정.
			hpnInfo.c <- chkEvt$hpnInfo
			hpnInfo.l <- cutterObj$chkEvt.last$hpnInfo

			# surWindow <- c(min=0,max=10)	# chkEvt$hpnInfo$tot 범위 참조
			# tot.surLC <- c( lEvt=bUtil.in(hpnInfo.l$tot,surWindow) ,cEvt=bUtil.in(hpnInfo.c$tot,surWindow) )
			# if( all(!tot.surLC) ){
			# 	rCutId <- c( rCutId, sprintf("Hpn.tot Evt dup(%d~%d) %d->%d"
			# 				,surWindow["min"],surWindow["max"],hpnInfo.l$tot,hpnInfo.c$tot) )	
			# }

			# surWindow <- c(min=12,max=18)	# chkEvt$hpnInfo$fCol 범위 참조
			# tot.l <- sum(hpnInfo.l$fCol==0)
			# tot.c <- sum(hpnInfo.c$fCol==0)
			# fCol.surLC <- c( lEvt=bUtil.in(tot.l,surWindow) ,cEvt=bUtil.in(tot.c,surWindow) )
			# if( all(!fCol.surLC) ){
			# 	rCutId <- c( rCutId, sprintf("Hpn.fCol Evt dup(%d~%d) %d->%d"
			# 				,surWindow["min"],surWindow["max"],tot.l,tot.c) )	
			# }

			# surWindow <- c(min=9,max=13)	# chkEvt$hpnInfo$phase 범위 참조
			# tot.l <- sum(hpnInfo.l$phase==0)
			# tot.c <- sum(hpnInfo.c$phase==0)
			# phase.surLC <- c( lEvt=bUtil.in(tot.l,surWindow) ,cEvt=bUtil.in(tot.c,surWindow) )
			# if( all(!phase.surLC) ){
			# 	rCutId <- c( rCutId, sprintf("Hpn.phase Evt dup(%d~%d) %d->%d"
			# 				,surWindow["min"],surWindow["max"],tot.l,tot.c) )	
			# }


			# --------------------------------------------------------------------------------------------------------------
			cnt.fCol <- sum(hpnInfo.c$fCol!=hpnInfo.l$fCol)
			cnt.phase <- sum(hpnInfo.c$phase!=hpnInfo.l$phase)

			surWindow <- c(min=0,max=11)
			if( !bUtil.in(cnt.fCol,surWindow) ) rCutId <- c( rCutId, sprintf("cnt.fCol.%d(%d~%d)",cnt.fCol,surWindow["min"],surWindow["max"]) )
			surWindow <- c(min=2,max=11)
			if( !bUtil.in(cnt.phase,surWindow) ) rCutId <- c( rCutId, sprintf("cnt.phase.%d(%d~%d)",cnt.phase,surWindow["min"],surWindow["max"]) )

			surWindow <- c(min=2,max=21)
			tot <- sum(cnt.fCol+cnt.phase)
			if( !bUtil.in(tot,surWindow) ) rCutId <- c( rCutId, sprintf("HpnXY.%d(%d~%d)",tot,surWindow["min"],surWindow["max"]) )

			return( rCutId )
		}
		# cutterObj$cutLst[["F_RareCol"]] <- function( scoreMtx ,chkEvt ){	# working
		F_RareCol <- function( scoreMtx ,chkEvt ){
			rCutId <- character(0)

			pRareCol <- c("pLCol","pE3","pE4","pMH","pfNum")
			iRareCol <- c("iLCol","iE3","iE4","iMH","ifNum")

			pRareHpn <- apply( scoreMtx[pRareCol,] ,2 ,function(cVal){ sum(cVal>0) })
			iRareHpn <- apply( scoreMtx[iRareCol,] ,2 ,function(cVal){ sum(cVal>0) })
			matFlag <- pRareHpn == iRareHpn
			matFlag[pRareHpn==0] <- FALSE
			tot <- sum(matFlag)
			surWin <- c(min=0,max=0)
			if( !bUtil.in(tot,surWin) ) rCutId <- c( rCutId, sprintf("RareHpnCntMat.%d(%d~%d)",tot,surWin["min"],surWin["max"]) )


			FVCol <- c("FVa.m","FVa.c","aFV.m","aFV.c")
			FV0Flg <- apply( scoreMtx[FVCol,] ,2 ,function(cVal){ all(cVal==0) })
			tot <- sum(FV0Flg)
			surWin <- c(min=9,max=13)
			if( !bUtil.in(tot,surWin) ) rCutId <- c( rCutId, sprintf("FV0Hpn.%d(%d~%d)",tot,surWin["min"],surWin["max"]) )

			return( rCutId )
		}

		return(cutterObj)
	} # rObj$createCutter( )

	return( rObj )
} # bFCust.byHIdx_A_score2



bFCust.get_byHIdx_score9ChkEvt <- function( scoreMtx ){

	hpnInfo <- list( tot=	sum(scoreMtx>0)
					,fCol=	apply( scoreMtx ,1 ,function(byRV){ length(byRV) - sum(byRV==0) })
					,phase= apply( scoreMtx ,2 ,function(byCV){ length(byCV) - sum(byCV==0)})
	)

	return( list(hpnInfo=hpnInfo) )

} # bFCust.get_byHIdx_score9ChkEvt( )

