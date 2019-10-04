# 한글
FCust_score8EvtLst <- list( 	"c31"=1,"c32"=1,"c33"=1,"c34"=1
								,"c21"=1,"c22"=1,"c23"=1,"c24"=1,"c25"=1
								,"max3"=1,"min3"=1,"max2"=1,"min2"=1
								,"cTbl"=1
							)

FCust_score8minMaxLst <- list( "c31"=c(min=0,max=1),"c32"=c(min=0,max=1),"c33"=c(min=0,max=1),"c34"=c(min=0,max=1)
								,"c21"=c(min=0,max=1),"c22"=c(min=0,max=1),"c23"=c(min=0,max=1),"c24"=c(min=0,max=1),"c25"=c(min=0,max=1)
								,"max3"=c(min=0,max=0),"min3"=c(min=0,max=1),"max2"=c(min=0,max=1),"min2"=c(min=0,max=1)
								,"cTbl"=c(min=0,max=0)
							)

#	[score8:Col Cutter(1 col)] ------------------------------------------------------------------
#	c( typ="cust"	,hName="*"	,mName="score8"	,pName="*"	,fcName="*" )
bFCust.A_score8_A_A <- function(  ){

	rObj <- list( defId=c( typ="cust"	,hName="*"	,mName="score8"	,pName="*"	,fcName="*" ) )

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

        cutterObj$minMax <- FCust_score8minMaxLst[[tgtId["fcName"]]]

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
} # bFCust.A_score8_A_A( )

#	[score8:Col Cutter(N col)] ------------------------------------------------------------------
bFCust.A_score8_A_Row01 <- function(  ){
	rObj <- list( )
	rObj$defId <- c( typ="c_NCol.5"	,hName="*"	,mName="score8"	,pName="*"	,rFId="Row01" )	# row filt ID
	rObj$description <- sprintf("(cust)  ")

	rObj$evtLst <- FCust_score8EvtLst

	rObj$cutFLst <- list()
	rObj$cutFLst[[1+length(rObj$cutFLst)]] <- function( smRow ,evt ){
		crObj <- list( cutFlag=F ,cId="hpnOne" ) # cut result object, cut Id

		cntM3 <- sum(smRow[c("max3","min3")])
		if( !bUtil.in(cntM3,c(min=0,max=1)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<minMax3 %d>",cntM3) )
		}

		cntM2 <- sum(smRow[c("max2","min2")])
		if( !bUtil.in(cntM2,c(min=0,max=1)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<minMax2 %d>",cntM2) )
		}

		cntC3 <- sum(smRow[c("c31","c32","c33","c34")])
		if( !bUtil.in(cntC3,c(min=0,max=2)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<c3 %d>",cntC3) )
		}

		cntC2 <- sum(smRow[c("c21","c22","c23","c24","c25")])
		if( !bUtil.in(cntC2,c(min=0,max=2)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<c2 %d>",cntC2) )
		}

		cnt <- cntM3+cntC3
		if( !bUtil.in(cnt,c(min=0,max=2)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<M3C3 %d>",cnt) )
		}

		cnt <- cntM2+cntC2
		if( !bUtil.in(cnt,c(min=0,max=2)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<M2C2 %d>",cnt) )
		}

		cnt <- sum( smRow==0 )
		if( !bUtil.in(cnt,c(min=11,max=14)) ){
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
} # bFCust.A_score8_A_Row01( )

#	[score8:Col Cutter(Rebound/Sequencial)] -----------------------------------------------------
bFCust.A_score8_A_rReb01 <- function(  ){	# evt rebind
	#	row rebind라고는 했지만 사실 seq 이다. 
	#	즉 이전 evt 컬럼이 다음에도 동일 evt가 일어나는지 체크 (다음에서 추가적으로 발생하는 evt는 상관없음)
	rObj <- list( )
	rObj$defId <- c( typ="cust_RReb"	,hName="*"	,mName="score8"	,pName="*"	,rFId="rReb01" )	# row filt ID
	rObj$description <- sprintf("(cust)  ")

	rObj$evtLst <- FCust_score8EvtLst

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

			cName <- c("max3","min3")
			evtChkInfo <- list( cutId="minMax3" ,fireThld=1 ,evtLst=rObj$evtLst[cName] )
			evtChkInfo$evtLast <- bUtil.getEvtVal( scoreMtx.last ,evtChkInfo$evtLst )
			cutterObj$evtChkLst[[1+length(cutterObj$evtChkLst)]] <- evtChkInfo

			cName <- c("max2","min2")	# byHIdx 에서 체크
			evtChkInfo <- list( cutId="minMax2" ,fireThld=2 ,evtLst=rObj$evtLst[cName] )
			evtChkInfo$evtLast <- bUtil.getEvtVal( scoreMtx.last ,evtChkInfo$evtLst )
			cutterObj$evtChkLst[[1+length(cutterObj$evtChkLst)]] <- evtChkInfo

			cName <- c("c31","c32","c33","c34")
			evtChkInfo <- list( cutId="c3" ,fireThld=1 ,evtLst=rObj$evtLst[cName] )
			evtChkInfo$evtLast <- bUtil.getEvtVal( scoreMtx.last ,evtChkInfo$evtLst )
			cutterObj$evtChkLst[[1+length(cutterObj$evtChkLst)]] <- evtChkInfo

			cName <- c("c21","c22","c23","c24","c25")	# byHIdx에서 체크
			evtChkInfo <- list( cutId="c2" ,fireThld=2 ,evtLst=rObj$evtLst[cName] )
			evtChkInfo$evtLast <- bUtil.getEvtVal( scoreMtx.last ,evtChkInfo$evtLst )
			cutterObj$evtChkLst[[1+length(cutterObj$evtChkLst)]] <- evtChkInfo

			#	byHIdx에서 다수 발생 체크
			evtChkInfo <- list( cutId="evtAll" ,fireThld=2 ,evtLst=rObj$evtLst )
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
} # bFCust.A_score8_A_rReb01()

bFCust.A_score8_A_rRebAA <- function(  ){	#	이전 마지막 score(cutterObj$lastRow) 값과의 일치여부(raw Value) 

	rObj <- list( )
	rObj$defId <- c( typ="cust_RReb"	,hName="*"	,mName="score8"	,pName="*"	,rFId="rRebAA" )	# row filt ID
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

			#	빈번하게 발생하는 컬럼은 확인대상 제외...
			cName <- setdiff( names(cutterObj$lastRow) ,c("min2") )
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
} # bFCust.A_score8_A_rRebAA()



#	[score8:byFCol(Rebound/Sequencial)] ---------------------------------------------------------
#		- mtxGrp <- getScoreMtx.grp_byFCol( scoreMtx.grp )
#		- nRow 대상이긴 하지만, mtx는 각 scoreMtx 의 fCol 별로 생성된다는 점을 주의
#		- column이 phase이므로 pName 구분이 없고, tgtId에서도 pName이 빠진다. 대신 fcName 필요.
#	c( typ="c_byFCol"	,hName="*"	,mName="score4"	,fcName="*"  )
bFCust.byFCol_A_score8_A <- function( ){

	rObj <- list( )
	rObj$defId <- c( typ="c_byFCol"	,hName="*"	,mName="score8"	,fcName="*"  )
	rObj$description <- sprintf("(cust)  ")

	rObj$cutFLst <- list()
	if( TRUE ){	# build cutFLst
		#	c31 c32 c33 c34 c21 c22 c23 c24 c25 max3 min3 max2 min2 cTbl
		rObj$cutFLst[["c31"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A c31" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=2)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["c32"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A c32" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=3)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["c33"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A c33" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=3)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["c34"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A c34" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=3)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["c21"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A c21" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=5)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["c22"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A c22" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=5)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["c23"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A c23" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=5)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["c24"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A c24" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=5)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["c25"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A c25" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=5)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["max3"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A max3" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["min3"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A min3" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["max2"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A max2" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["min2"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A min2" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=6)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["cTbl"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A cTbl" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
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
} # bFCust.byFCol_A_score8_A( )

#	typ="c_byFCol"	,hName="*"	,mName="score8"	,pName="*"	,fcName="*" )
bFCust.byFCol_A_score8_A_rReb01 <- function( ){
	#	row rebind라고는 했지만 사실 seq 이다. 
	#	즉 이전 evt 발생 phase에서 다음에도 동일 evt가 일어나는지 체크(다음에서 추가적으로 발생하는 evt는 상관없음.)
	rObj <- list( )
	rObj$defId <- c( typ="c_byFCol"	,hName="*"	,mName="score8"	,pName="*"	,fcName="*" )	# row filt ID
	rObj$description <- sprintf("(cust)  ")

	rObj$evtLst <- FCust_score8EvtLst
	rObj$fireThld.min <- c( "c31"=1 ,"c32"=1 ,"c33"=1 ,"c34"=1 
						,"c21"=2 ,"c22"=2 ,"c23"=2 ,"c24"=2 ,"c25"=2 
						,"max3"=1 ,"min3"=1 ,"max2"=1 ,"min2"=3 ,"cTbl"=1 
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
} # bFCust.byFCol_A_score8_A_rReb01( )

#	c( typ="c_byFCol"	,hName="*"	,mName="score8"	,fcName="*"  )
bFCust.byFCol_A_score8_A_rRebAA <- function( ){
	rObj <- list( )
	rObj$defId <- c( typ="c_byFCol"	,hName="*"	,mName="score8"	,fcName="*"  )
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
			fqCol <- c( "c21" ,"c22" ,"c23" ,"c24" ,"c25" ,"min2" )	# 발생이 빈번한 컬럼.

			for( idx in seq_len(val.len) ){
				if( alreadyDead[idx] ) next

				if( !cutterObj$activated || is.null(cutterObj$lastRow) ) next

				matFlag <- cutterObj$lastRow==scoreMtx[idx,]
				if( all(matFlag) ){
					if( cutterObj$idObj[["fcName"]] %in% c(fqCol) ){
						if( 1>=sum(cutterObj$lastRow) )	next
					}

					infoStr <- sprintf("cut Id : rRebAA (raw all Mat. cnt:%d)",sum(cutterObj$lastRow) )
					infoStr <- c( infoStr ,sprintf("- %s ",cutterObj$idObj[["fcName"]]) )
					cutLst[[1+length(cutLst)]] <- list( idx=idx ,idObjDesc=cutterObj$idObjDesc ,info=infoStr )

				}
			}

			return( cutLst )
		} # cutterObj$cut()

		return(cutterObj)
	}

	return( rObj )
} # bFCust.byFCol_A_score8_A_rRebAA( )





