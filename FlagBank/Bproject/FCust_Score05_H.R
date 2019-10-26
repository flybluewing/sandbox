FCust_score5EvtLst <- list( "pBanN.r"=2 ,"pBanN.n"=2 ,"pLCol"=1 ,"pE3"=2 ,"pE4"=1 ,"pMH"=1 ,"pfNum"=2
                            ,"iBanN"=2:4 ,"iLCol"=1 ,"iE3"=2 ,"iE4"=1 ,"iMH"=1 ,"ifNum"=2
                            ,"FVa.m"=2:3 ,"FVa.c"=3:4 ,"aFV.m"=2:3 ,"aFV.c"=3:4
                            ,"m4"=1:4
							)

FCust_score5minMaxLst <- list( "pBanN.r"=c(min=0,max=2) ,"pBanN.n"=c(min=0,max=2) ,"pLCol"=c(min=0,max=1) 
                                    ,"pE3"=c(min=0,max=2) ,"pE4"=c(min=0,max=1) ,"pMH"=c(min=0,max=1) ,"pfNum"=c(min=0,max=2) 
                            ,"iBanN"=c(min=0,max=2) ,"iLCol"=c(min=0,max=1) 
                                    ,"iE3"=c(min=0,max=2) ,"iE4"=c(min=0,max=1) ,"iMH"=c(min=0,max=1) ,"ifNum"=c(min=0,max=2) 
                            ,"FVa.m"=c(min=0,max=3) ,"FVa.c"=c(min=0,max=4) ,"aFV.m"=c(min=0,max=3) ,"aFV.c"=c(min=0,max=4) 
                            ,"m4"=c(min=0,max=0)
							)


#	[score5:Col Cutter(1 col)] ------------------------------------------------------------------
#	c( typ="cust"	,hName="*"	,mName="score5"	,pName="*"	,fcName="*" )
bFCust.A_score5_A_A <- function(  ){

	rObj <- list( defId=c( typ="cust"	,hName="*"	,mName="score5"	,pName="*"	,fcName="*" ) )

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

        cutterObj$minMax <- FCust_score5minMaxLst[[tgtId["fcName"]]]

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
} # bFCust.A_score5_A_A( )


#	[score5:Col Cutter(N col)] ------------------------------------------------------------------
bFCust.A_score5_A_Row01 <- function(  ){
	rObj <- list( )
	rObj$defId <- c( typ="c_NCol.5"	,hName="*"	,mName="score5"	,pName="*"	,rFId="Row01" )	# row filt ID
	rObj$description <- sprintf("(cust)  ")

	rObj$evtLst <- FCust_score5EvtLst

	rObj$cutFLst <- list()
	rObj$cutFLst[[1+length(rObj$cutFLst)]] <- function( smRow ,evt ){
		crObj <- list( cutFlag=F ,cId="hpnOne" ) # cut result object, cut Id

		cnt <- sum(smRow[c("pBanN.r","pBanN.n","iBanN")])
		if( !bUtil.in(cnt,c(min=0,max=3)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<BanCnt %d>",cnt) )
		}


		pRareCol <- c("pLCol","pE3","pE4","pMH","pfNum")
		iRareCol <- c("iLCol","iE3","iE4","iMH","ifNum")

		pCnt <- sum(smRow[pRareCol])
		if( !bUtil.in(pCnt,c(min=0,max=3)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<pEvtCnt %d>",pCnt) )
		}
		iCnt <- sum(smRow[iRareCol])
		if( !bUtil.in(iCnt,c(min=0,max=3)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<iEvtCnt %d>",iCnt) )
		}
		if( !bUtil.in((pCnt+iCnt),c(min=0,max=3)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<piEvtCnt %d>",(pCnt+iCnt)) )
		}

		matFlag <- smRow[pRareCol]==smRow[iRareCol]
		if( all(matFlag) && (sum(pCnt)>0) ){ 
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<piEvtSyn %d>",pCnt ) )
		}

		FVCol <- c("FVa.m","FVa.c","aFV.m","aFV.c")
		cnt <- sum(smRow[FVCol]==4)
		if( !bUtil.in(cnt,c(min=0,max=1)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<FVCol4 %d>",cnt) )
		}


		cnt <- sum( smRow==0 )
		if( !bUtil.in(cnt,c(min=10,max=18)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<ZeroCnt %d>",cnt) )
		}

		evt <- bUtil.getEvtVal( smRow ,FCust_score5EvtLst )
		cnt <- sum( !is.na(evt) )
		if( !bUtil.in(cnt,c(min=0,max=5)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<EvtCnt %d>",cnt) )
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
} # bFCust.A_score5_A_Row01( )

#	[score5:Col Cutter(Rebound/Sequencial)] -----------------------------------------------------
bFCust.A_score5_A_rReb01 <- function(  ){	# evt rebind
	#	row rebind라고는 했지만 사실 seq 이다. 
	#	즉 이전 evt 컬럼이 다음에도 동일 evt가 일어나는지 체크 (다음에서 추가적으로 발생하는 evt는 상관없음)
	rObj <- list( )
	rObj$defId <- c( typ="cust_RReb"	,hName="*"	,mName="score5"	,pName="*"	,rFId="rReb01" )	# row filt ID
	rObj$description <- sprintf("(cust)  ")

	rObj$evtLst <- FCust_score5EvtLst

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

		scoreMtxObj <- B.HMtxLst_getMtxLst( hMtxLst , tgtId["hName"] ,tgtId["mName"] ,tgtId["pName"] )
		scoreMtx <- if( is.null(scoreMtxObj) ) NULL else scoreMtxObj$scoreMtx
		cutterObj$scoreMtx <- scoreMtx	# just for debug later..

		cutterObj$evtChkLst <- list()
		if( !is.null(scoreMtx) ){	# build checkLst
			#	fireThld 는 fire가 일어날 동일 패턴 수. NA이면 전부 매치.
			scoreMtx.last <- scoreMtx[nrow(scoreMtx),]

			cName <- c("pBanN.r","pBanN.n","pLCol","pE3","pE4","pMH","pfNum")
			evtChkInfo <- list( cutId="grp.p" ,fireThld=2 ,evtLst=rObj$evtLst[cName] )
			evtChkInfo$evtLast <- bUtil.getEvtVal( scoreMtx.last ,evtChkInfo$evtLst )
			cutterObj$evtChkLst[[1+length(cutterObj$evtChkLst)]] <- evtChkInfo

			cName <- c("iBanN","iLCol","iE3","iE4","iMH","ifNum")
			evtChkInfo <- list( cutId="grp.i" ,fireThld=2 ,evtLst=rObj$evtLst[cName] )
			evtChkInfo$evtLast <- bUtil.getEvtVal( scoreMtx.last ,evtChkInfo$evtLst )
			cutterObj$evtChkLst[[1+length(cutterObj$evtChkLst)]] <- evtChkInfo

			#	byHIdx에서 다수 발생 체크
			# cName <- c("FVa.m","FVa.c","aFV.m","aFV.c")
			# evtChkInfo <- list( cutId="grp.fv" ,fireThld=2 ,evtLst=rObj$evtLst[cName] )
			# evtChkInfo$evtLast <- bUtil.getEvtVal( scoreMtx.last ,evtChkInfo$evtLst )
			# cutterObj$evtChkLst[[1+length(cutterObj$evtChkLst)]] <- evtChkInfo

			#	byHIdx에서 다수 발생 체크
			# evtChkInfo <- list( cutId="evtAll" ,fireThld=2 ,evtLst=rObj$evtLst )
			# evtChkInfo$evtLast <- bUtil.getEvtVal( scoreMtx.last ,evtChkInfo$evtLst )
			# cutterObj$evtChkLst[[1+length(cutterObj$evtChkLst)]] <- evtChkInfo

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
} # bFCust.A_score5_A_rReb01()

bFCust.A_score5_A_rRebAA <- function(  ){	#	이전 마지막 score(cutterObj$lastRow) 값과의 일치여부(raw Value) 

	rObj <- list( )
	rObj$defId <- c( typ="cust_RReb"	,hName="*"	,mName="score5"	,pName="*"	,rFId="rRebAA" )	# row filt ID
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

			#	허나, FV 그룹은 유사한 경우가 자주 있어서..
			cName <- setdiff( names(cutterObj$lastRow) ,c("FVa.m","FVa.c","aFV.m","aFV.c") )
			if( all(cutterObj$lastRow[cName]==0) ){
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
				if( 1 > diffCnt ){
					infoStr <- sprintf("cut Id : %s diffCnt:%d",cutterObj$idObjDesc["rFId"],diffCnt )
					cutLst[[1+length(cutLst)]] <- list( idx=idx ,idObjDesc=cutterObj$idObjDesc ,info=infoStr )
				}
			}

			return( cutLst )
		} # cutterObj$cut()

		return(cutterObj)
	}

	return( rObj )
} # bFCust.A_score5_A_rRebAA()


#	[score4:byFCol(Rebound/Sequencial)] ---------------------------------------------------------
#		- mtxGrp <- getScoreMtx.grp_byFCol( scoreMtx.grp )
#		- nRow 대상이긴 하지만, mtx는 각 scoreMtx 의 fCol 별로 생성된다는 점을 주의
#		- column이 phase이므로 pName 구분이 없고, tgtId에서도 pName이 빠진다. 대신 fcName 필요.
#	c( typ="c_byFCol"	,hName="*"	,mName="score4"	,fcName="*"  )
bFCust.byFCol_A_score5_A <- function( ){

	rObj <- list( )
	rObj$defId <- c( typ="c_byFCol"	,hName="*"	,mName="score5"	,fcName="*"  )
	rObj$description <- sprintf("(cust)  ")

	rObj$cutFLst <- list()
	if( TRUE ){	# build cutFLst
		rObj$cutFLst[["pBanN.r"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A pBanN.r" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=5)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow>=2)
			if( !bUtil.in(cnt,c(min=0,max=2)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["pBanN.n"]] <- function( smRow ,fcName ){	# for testing
			crObj <- list( cutFlag=F ,cId="_A pBanN.n" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=2)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow>=2)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["pLCol"]] <- function( smRow ,fcName ){	# for testing
			crObj <- list( cutFlag=F ,cId="_A pLCol" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=2)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["pE3"]] <- function( smRow ,fcName ){	# for testing
			crObj <- list( cutFlag=F ,cId="_A pE3" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=2)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["pE4"]] <- function( smRow ,fcName ){	# for testing
			crObj <- list( cutFlag=F ,cId="_A pE4" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["pMH"]] <- function( smRow ,fcName ){	# for testing
			crObj <- list( cutFlag=F ,cId="_A pMH" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=2)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["pfNum"]] <- function( smRow ,fcName ){	# for testing
			crObj <- list( cutFlag=F ,cId="_A pfNum" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["iBanN"]] <- function( smRow ,fcName ){	# for testing
			crObj <- list( cutFlag=F ,cId="_A iBanN" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=5)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow>=2)
			if( !bUtil.in(cnt,c(min=0,max=2)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["iLCol"]] <- function( smRow ,fcName ){	# for testing
			crObj <- list( cutFlag=F ,cId="_A iLCol" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=2)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow>=2)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["iE3"]] <- function( smRow ,fcName ){	# for testing
			crObj <- list( cutFlag=F ,cId="_A iE3" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=2)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow>=2)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["iE4"]] <- function( smRow ,fcName ){	# for testing
			crObj <- list( cutFlag=F ,cId="_A iE4" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow>=2)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["iMH"]] <- function( smRow ,fcName ){	# for testing
			crObj <- list( cutFlag=F ,cId="_A iMH" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow>=2)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["ifNum"]] <- function( smRow ,fcName ){	# for testing
			crObj <- list( cutFlag=F ,cId="_A ifNum" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow>=2)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["FVa.m"]] <- function( smRow ,fcName ){	# for testing
			crObj <- list( cutFlag=F ,cId="_A FVa.m" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=1,max=12)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow==2)
			if( !bUtil.in(cnt,c(min=0,max=4)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 02.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow>=3)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["FVa.c"]] <- function( smRow ,fcName ){	# for testing
			crObj <- list( cutFlag=F ,cId="_A FVa.c" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=1,max=8)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow==2)
			if( !bUtil.in(cnt,c(min=0,max=6)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 02.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow>=3)
			if( !bUtil.in(cnt,c(min=0,max=4)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["aFV.m"]] <- function( smRow ,fcName ){	# for testing
			crObj <- list( cutFlag=F ,cId="_A aFV.m" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=1,max=10)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow==2)
			if( !bUtil.in(cnt,c(min=0,max=3)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 02.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow>=3)
			if( !bUtil.in(cnt,c(min=0,max=2)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["aFV.c"]] <- function( smRow ,fcName ){	# for testing
			crObj <- list( cutFlag=F ,cId="_A aFV.c" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=1,max=7)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow==2)
			if( !bUtil.in(cnt,c(min=1,max=7)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 02.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow>=3)
			if( !bUtil.in(cnt,c(min=0,max=4)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["m4"]] <- function( smRow ,fcName ){	# for testing
			crObj <- list( cutFlag=F ,cId="_A m4" ) # cut result object, cut Id
			cnt <- sum(smRow>=1)
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
} # bFCust.byFCol_A_score5_A( )

#	typ="c_byFCol"	,hName="*"	,mName="score5"	,pName="*"	,fcName="*" )
bFCust.byFCol_A_score5_A_rReb01 <- function( ){
	#	row rebind라고는 했지만 사실 seq 이다. 
	#	즉 이전 evt 발생 phase에서 다음에도 동일 evt가 일어나는지 체크(다음에서 추가적으로 발생하는 evt는 상관없음.)
	rObj <- list( )
	rObj$defId <- c( typ="c_byFCol"	,hName="*"	,mName="score5"	,pName="*"	,fcName="*" )	# row filt ID
	rObj$description <- sprintf("(cust)  ")

	rObj$evtLst <- FCust_score5EvtLst
	rObj$fireThld.min <- c( "pBanN.r"=1 ,"pBanN.n"=1 ,"pLCol"=1 ,"pE3"=1 ,"pE4"=1 ,"pMH"=1 ,"pfNum"=1
							,"iBanN"=1 ,"iLCol"=2 ,"iE3"=1 ,"iE4"=1 ,"iMH"=1 ,"ifNum"=1
							,"FVa.m"=2 ,"FVa.c"=2 ,"aFV.m"=2 ,"aFV.c"=2 ,"m4"=1
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
} # bFCust.byFCol_A_score5_A_rReb01( )

#	c( typ="c_byFCol"	,hName="*"	,mName="score5"	,fcName="*"  )
bFCust.byFCol_A_score5_A_rRebAA <- function( ){
	rObj <- list( )
	rObj$defId <- c( typ="c_byFCol"	,hName="*"	,mName="score5"	,fcName="*"  )
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

				diffCnt <- sum( cutterObj$lastRow!=scoreMtx[idx,] )
				if( 1>diffCnt ){
					infoStr <- sprintf("cut Id : rRebAA (raw diffCnt:%d)",diffCnt )
					cutLst[[1+length(cutLst)]] <- list( idx=idx ,idObjDesc=cutterObj$idObjDesc ,info=infoStr )
				}
			}

			return( cutLst )
		} # cutterObj$cut()

		return(cutterObj)
	}

	return( rObj )
} # bFCust.byFCol_A_score5_A_rRebAA( )





#	[score5:byHIdx(...)] ---------------------------------------------------------
#		- [fCol,phName] 
#		- mtxGrp <- getScoreMtx.grp_byHIdx( scoreMtx.grp )
#		- hIdxObj <- B.getHMtxLst_byHIdx( hMtxLst )
#       - 개개 score의 최대 최소값이나, evt 발생등은 이미 앞에서 모두 확인되었다.)
#		  따라서 col 방향, row 방향, mtx전체에 대한 rebPtn 체크.
#			(fCol 별 evt 기준 때문에 scoreMtx 개개별로 작성 필요.)

#		c( typ="cust_byHIdx"	,hName="*"	,mName="score5" )
bFCust.byHIdx_A_score5 <- function( ){

	rObj <- list( )
	rObj$defId <- c( typ="cust_byHIdx"	,hName="*"	,mName="score5" )
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
			cutterObj$chkEvt.last <- bFCust.get_byHIdx_score5ChkEvt( mtxLst[[length(mtxLst)]] )
		}

		cutterObj$mtxLst <- mtxLst
		cutterObj$rebPtn.skipZero <- bUtil.getMtxRebPtn.skipZero( mtxLst ,hpnThld.fCol=NA ,hpnThld.ph=NA )

		cutterObj$cut <- function( scoreMtx ,aIdx ){
			# scoreMtx 는 1개 aZoid에 관한 [fCol,phase] mtx임을 유의.
			# 	(즉, 이 함수는 한 개 aZoid에 대한 처리로직이다.)
			#	단 surDf와 cutLst는 다른 cut함수들 결과와의 호환성 유지를 위해 구조유지.
			cutId <- character(0)

			chkEvt <- bFCust.get_byHIdx_score5ChkEvt( scoreMtx )
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

			surWin <- c(min=0,max=2)
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
			if( any(rebCnt>2) ) rCutId <- c( rCutId, sprintf("matRebCnt.%d/%d",sum(rebCnt>1),max(rebCnt)) )

			# raw idff -------------------------------------------
			nMatchCnt <- sum(scoreMtx!=cutterObj$evt$lastMtxRaw)
			surWindow <- c(min=28,max=64)
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
			fireThld <- 2
			cnt.allMat <- 0
			for( fColName in rownames(scoreMtx) ){
				# 0 이 많으면 적용.
				if( 0==sum(scoreMtx[fColName,]) )	next

				if( any(scoreMtx[fColName,]!=cutterObj$evt$lastMtxRaw[fColName,]) ) next

				cnt.allMat <- cnt.allMat + 1
			}
			if( fireThld<=cnt.allMat )	rCutId <- c( rCutId, sprintf("fColRawMatCnt.%d",cnt.allMat) )

			# banPastH -------------------------------------------
			banPastH <- 40	# 바로 이전 H를 제외한 크기(rawDiffCnt에서 체크되므로)
			surWindow <- c(min=27,max=67)
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
			surWindow <- c(min=0,max=8)	# survive window
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
			surWindow <- c(min=31,max=56)
			if( !bUtil.in(tot,surWindow) ) rCutId <- c( rCutId, sprintf("HpnTot.%d(%d~%d)",tot,surWindow["min"],surWindow["max"]) )

			tot <- sum(chkEvt$hpnInfo$fCol==0)
			surWindow <- c(min=5,max=12)
			if( !bUtil.in(tot,surWindow) ) rCutId <- c( rCutId, sprintf("HpnFCol.%d(%d~%d)",tot,surWindow["min"],surWindow["max"]) )
			
			tot <- sum(chkEvt$hpnInfo$phase==0)
			surWindow <- c(min=1,max=5)
			if( !bUtil.in(tot,surWindow) ) rCutId <- c( rCutId, sprintf("HpnPh.%d(%d~%d)",tot,surWindow["min"],surWindow["max"]) )

			# Hpn Last-----------------------------------------
			#	hpn 값이 이전에도 특이했는데 이번에도 특이한 경우를 제거
			#	즉 위에서의 surWindow에 살짝 걸친 경우가 연속발생하는 것을 부정.
			hpnInfo.c <- chkEvt$hpnInfo
			hpnInfo.l <- cutterObj$chkEvt.last$hpnInfo

			surWindow <- c(min=35,max=56)	# chkEvt$hpnInfo$tot 범위 참조
			tot.surLC <- c( lEvt=bUtil.in(hpnInfo.l$tot,surWindow) ,cEvt=bUtil.in(hpnInfo.c$tot,surWindow) )
			if( all(!tot.surLC) ){
				rCutId <- c( rCutId, sprintf("Hpn.tot Evt dup(%d~%d) %d->%d"
							,surWindow["min"],surWindow["max"],hpnInfo.l$tot,hpnInfo.c$tot) )	
			}

			surWindow <- c(min=6,max=11)	# chkEvt$hpnInfo$fCol 범위 참조
			tot.l <- sum(hpnInfo.l$fCol==0)
			tot.c <- sum(hpnInfo.c$fCol==0)
			fCol.surLC <- c( lEvt=bUtil.in(tot.l,surWindow) ,cEvt=bUtil.in(tot.c,surWindow) )
			if( all(!fCol.surLC) ){
				rCutId <- c( rCutId, sprintf("Hpn.fCol Evt dup(%d~%d) %d->%d"
							,surWindow["min"],surWindow["max"],tot.l,tot.c) )	
			}

			surWindow <- c(min=1,max=4)	# chkEvt$hpnInfo$phase 범위 참조
			tot.l <- sum(hpnInfo.l$phase==0)
			tot.c <- sum(hpnInfo.c$phase==0)
			phase.surLC <- c( lEvt=bUtil.in(tot.l,surWindow) ,cEvt=bUtil.in(tot.c,surWindow) )
			if( all(!phase.surLC) ){
				rCutId <- c( rCutId, sprintf("Hpn.phase Evt dup(%d~%d) %d->%d"
							,surWindow["min"],surWindow["max"],tot.l,tot.c) )	
			}


			# --------------------------------------------------------------------------------------------------------------
			cnt.fCol <- sum(hpnInfo.c$fCol!=hpnInfo.l$fCol)
			cnt.phase <- sum(hpnInfo.c$phase!=hpnInfo.l$phase)

			surWindow <- c(min=4,max=13)
			if( !bUtil.in(cnt.fCol,surWindow) ) rCutId <- c( rCutId, sprintf("cnt.fCol.%d(%d~%d)",cnt.fCol,surWindow["min"],surWindow["max"]) )
			surWindow <- c(min=8,max=13)
			if( !bUtil.in(cnt.phase,surWindow) ) rCutId <- c( rCutId, sprintf("cnt.phase.%d(%d~%d)",cnt.phase,surWindow["min"],surWindow["max"]) )

			surWindow <- c(min=13,max=26)
			tot <- sum(cnt.fCol+cnt.phase)
			if( !bUtil.in(tot,surWindow) ) rCutId <- c( rCutId, sprintf("HpnXY.%d(%d~%d)",tot,surWindow["min"],surWindow["max"]) )

			return( rCutId )
		}
		cutterObj$cutLst[["F_RareCol"]] <- function( scoreMtx ,chkEvt ){	# working
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
			surWin <- c(min=1,max=7)
			if( !bUtil.in(tot,surWin) ) rCutId <- c( rCutId, sprintf("FV0Hpn.%d(%d~%d)",tot,surWin["min"],surWin["max"]) )
			FV1Flg <- apply( scoreMtx[FVCol,] ,2 ,function(cVal){ all(cVal==1) })
			tot <- sum(FV1Flg)
			surWin <- c(min=1,max=5)
			if( !bUtil.in(tot,surWin) ) rCutId <- c( rCutId, sprintf("FV1Hpn.%d(%d~%d)",tot,surWin["min"],surWin["max"]) )
			FV2Cnt <- apply( scoreMtx[FVCol,] ,2 ,function(cVal){ sum(cVal==2) })
			tot <- sum(FV2Cnt>0)
			surWin <- c(min=2,max=8)
			if( !bUtil.in(tot,surWin) ) rCutId <- c( rCutId, sprintf("FV2Hpn.%d(%d~%d)",tot,surWin["min"],surWin["max"]) )
			FV3Cnt <- apply( scoreMtx[FVCol,] ,2 ,function(cVal){ sum(cVal==3) })
			tot <- sum(FV3Cnt>0)
			surWin <- c(min=0,max=4)
			if( !bUtil.in(tot,surWin) ) rCutId <- c( rCutId, sprintf("FV3Hpn.%d(%d~%d)",tot,surWin["min"],surWin["max"]) )


			return( rCutId )
		}

		return(cutterObj)
	} # rObj$createCutter( )

	return( rObj )
} # bFCust.byHIdx_A_score2



bFCust.get_byHIdx_score5ChkEvt <- function( scoreMtx ){

	hpnInfo <- list( tot=	sum(scoreMtx>0)
					,fCol=	apply( scoreMtx ,1 ,function(byRV){ length(byRV) - sum(byRV==0) })
					,phase= apply( scoreMtx ,2 ,function(byCV){ length(byCV) - sum(byCV==0)})
	)

	return( list(hpnInfo=hpnInfo) )

} # bFCust.get_byHIdx_score5ChkEvt( )

