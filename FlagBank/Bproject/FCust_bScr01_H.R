FCust_bScr01EvtLst <- list(	"raw.1"=3:7 ,"raw.3"=3:7 ,"raw.4"=3:7 ,"raw.6"=3:7 
							,"rem.1"=3:7 ,"rem.2"=3:7 ,"rem.3"=3:7 ,"rem.4"=3:7 ,"rem.5"=3:7 ,"rem.6"=3:7 
							,"c.1"=3:7 ,"c.2"=3:7 ,"c.3"=3:7 ,"c.4"=3:7 ,"c.5"=3:7 ,"c.6"=3:7 
							,"raw.ZW"=3:7 ,"rem.ZW"=3:7 ,"c.ZW"=3:7
						)

FCust_bScr01minMaxLst <- list( "raw.1"=c(min=0,max=3) ,"raw.3"=c(min=0,max=3) ,"raw.4"=c(min=0,max=3) ,"raw.6"=c(min=0,max=3) 
								,"rem.1"=c(min=0,max=4) ,"rem.2"=c(min=0,max=4) ,"rem.3"=c(min=0,max=4) 
								,"rem.4"=c(min=0,max=4) ,"rem.5"=c(min=0,max=4) ,"rem.6"=c(min=0,max=4) 
								,"c.1"=c(min=0,max=2) ,"c.2"=c(min=0,max=2) ,"c.3"=c(min=0,max=2) 
								,"c.4"=c(min=0,max=2) ,"c.5"=c(min=0,max=2) ,"c.6"=c(min=0,max=2) 
								,"raw.ZW"=c(min=0,max=2) ,"rem.ZW"=c(min=0,max=3) ,"c.ZW"=c(min=0,max=2)
							)
 

#	[bScr01:Col Cutter(1 col)] ------------------------------------------------------------------
#	c( typ="cust"	,hName="*"	,mName="bScr01"	,fcName="*" )
bFCust.A_bScr01_A_A <- function(  ){

	rObj <- list( defId=c( typ="cust"	,hName="*"	,mName="bScr01"	,fcName="*" ) )

	rObj$createCutter <- function( tgtId=c(hName="", mName="", fcName="") ,auxInfo=c(auxInfo="") ){

		cutterObj <- rObj	;cutterObj$createCutter <- NULL

		#	hName="testNA"; mName="testNA"; fcName="testNA"; auxInfo=c(auxInfo="")
		idObjDesc <- rObj$defId
		if( idObjDesc["hName"]!=tgtId["hName"] ) idObjDesc["hName"] <- sprintf("(%s)%s",idObjDesc["hName"],tgtId["hName"])
		if( idObjDesc["mName"]!=tgtId["mName"] ) idObjDesc["mName"] <- sprintf("(%s)%s",idObjDesc["mName"],tgtId["mName"])
		if( idObjDesc["fcName"]!=tgtId["fcName"] ) idObjDesc["fcName"] <- sprintf("(%s)%s",idObjDesc["fcName"],tgtId["fcName"])
		idObjDesc <- c( idObjDesc ,auxInfo )
		cutterObj$idObjDesc <- idObjDesc

        cutterObj$minMax <- FCust_bScr01minMaxLst[[tgtId["fcName"]]]

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
} # bFCust.A_bScr01_A_A( )

#	[bScr01:Col Cutter(N col)] ------------------------------------------------------------------
bFCust.A_bScr01_A_Row01 <- function(  ){
	rObj <- list( )
	rObj$defId <- c( typ="c_NCol.5"	,hName="*"	,mName="bScr01"	,rFId="Row01" )	# row filt ID
	rObj$description <- sprintf("(cust)  ")

	rObj$evtLst <- FCust_bScr01EvtLst

	rObj$cutFLst <- list()
	rObj$cutFLst[[1+length(rObj$cutFLst)]] <- function( smRow ,evt ){
		crObj <- list( cutFlag=F ,cId="hpnOne" ) # cut result object, cut Id

		evt <- bUtil.getEvtVal( smRow ,FCust_bScr01EvtLst )

		grpRawN <-	c("raw.1" ,"raw.3" ,"raw.4" ,"raw.6")
		grpRemN <-	c("rem.1" ,"rem.2" ,"rem.3" ,"rem.4" ,"rem.5" ,"rem.6")
		grpCN <- 	c("c.1" ,"c.2" ,"c.3" ,"c.4" ,"c.5" ,"c.6")
		grpZW <-	c("raw.ZW" ,"rem.ZW" ,"c.ZW")

		cntEvt <- sum( !is.na(evt) )
		if( !bUtil.in(cntEvt,c(min=0,max=6)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<cntEvt %d>",cntEvt) )
		}

		grpRawN.Cnt2 <- sum( smRow[grpRawN]==2 )
		if( !bUtil.in(grpRawN.Cnt2,c(min=0,max=3)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<grpRawN.Cnt2 %d>",grpRawN.Cnt2) )
		}
		grpRawN.CntEvt <- sum( !is.na(evt[grpRawN]) )
		if( !bUtil.in(grpRawN.CntEvt,c(min=0,max=3)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<grpRawN.CntEvt %d>",grpRawN.CntEvt) )
		}

		grpRemN.Cnt2 <- sum( smRow[grpRemN]==2 )
		if( !bUtil.in(grpRemN.Cnt2,c(min=0,max=6)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<grpRemN.Cnt2 %d>",grpRemN.Cnt2) )
		}
		grpRemN.CntEvt <- sum( !is.na(evt[grpRemN]) )
		if( !bUtil.in(grpRemN.CntEvt,c(min=0,max=4)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<grpRemN.CntEvt %d>",grpRemN.CntEvt) )
		}

		grpCN.Cnt0 <- sum( smRow[grpCN]==0 )
		if( !bUtil.in(grpCN.Cnt0,c(min=0,max=6)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<grpCN.Cnt0 %d>",grpCN.Cnt0) )
		}
		grpCN.Cnt1 <- sum( smRow[grpCN]==1 )
		if( !bUtil.in(grpCN.Cnt1,c(min=0,max=4)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<grpCN.Cnt1 %d>",grpCN.Cnt1) )
		}
		grpCN.Cnt2 <- sum( smRow[grpCN]==2 )
		if( !bUtil.in(grpCN.Cnt2,c(min=0,max=4)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<grpCN.Cnt2 %d>",grpCN.Cnt2) )
		}
		grpCN.CntEvt <- sum( !is.na(evt[grpCN]) )
		if( !bUtil.in(grpCN.CntEvt,c(min=0,max=1)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<grpCN.CntEvt %d>",grpCN.CntEvt) )
		}

		grpZW.Cnt2 <- sum( smRow[grpZW]==2 )
		if( !bUtil.in(grpZW.Cnt2,c(min=0,max=3)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<grpZW.Cnt2 %d>",grpZW.Cnt2) )
		}
		grpZW.CntEvt <- sum( !is.na(evt[grpZW]) )
		if( !bUtil.in(grpZW.CntEvt,c(min=0,max=1)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<grpZW.CntEvt %d>",grpZW.CntEvt) )
		}

		cnt <- sum( smRow==0 )	#	banCnt 에서 0이 존재하는 이상, 의미없음.
		if( !bUtil.in(cnt,c(min=1,max=10)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<ZeroCnt %d>",cnt) )
		}

		return( crObj )
	} # rObj$cutFLst[1]( )

	rObj$createCutter <- function( tgtId=c(hName="", mName="") ,auxInfo=c(auxInfo="") ){

		cutterObj <- rObj
		cutterObj$createCutter <- NULL

		#	hName="testNA"; mName="testNA"; pName="testNA"; fcName="testNA"; auxInfo=c(auxInfo="")
		idObjDesc <- rObj$defId
		if( idObjDesc["hName"]!=tgtId["hName"] ) idObjDesc["hName"] <- sprintf("(%s)%s",idObjDesc["hName"],tgtId["hName"])
		if( idObjDesc["mName"]!=tgtId["mName"] ) idObjDesc["mName"] <- sprintf("(%s)%s",idObjDesc["mName"],tgtId["mName"])
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
} # bFCust.A_bScr01_A_Row01( )

#	[bScr01:Col Cutter(Rebound/Sequencial)] -----------------------------------------------------
bFCust.A_bScr01_A_rReb01 <- function(  ){	# evt rebind
	#	row rebind라고는 했지만 사실 seq 이다. 
	#	즉 이전 evt 컬럼이 다음에도 동일 evt가 일어나는지 체크 (다음에서 추가적으로 발생하는 evt는 상관없음)
	rObj <- list( )
	rObj$defId <- c( typ="cust_RReb"	,hName="*"	,mName="bScr01"	,pName="*"	,rFId="rReb01" )	# row filt ID
	rObj$description <- sprintf("(cust)  ")

	rObj$evtLst <- FCust_bScr01EvtLst

	rObj$createCutter <- function( hMtxLst ,tgtId=c(hName="", mName="", pName="") ,auxInfo=c(auxInfo="") ){

		cutterObj <- rObj
		cutterObj$createCutter <- NULL

		#	hName="testNA"; mName="testNA"; pName="testNA"; fcName="testNA"; auxInfo=c(auxInfo="")
		idObjDesc <- rObj$defId
		if( idObjDesc["hName"]!=tgtId["hName"] ) idObjDesc["hName"] <- sprintf("(%s)%s",idObjDesc["hName"],tgtId["hName"])
		if( idObjDesc["mName"]!=tgtId["mName"] ) idObjDesc["mName"] <- sprintf("(%s)%s",idObjDesc["mName"],tgtId["mName"])
		idObjDesc <- c( idObjDesc ,auxInfo )
		cutterObj$idObjDesc <- idObjDesc

		cutterObj$idObj <- rObj$defId
		cutterObj$idObj[names(tgtId)] <- tgtId

		scoreMtxObj <- B.HMtxLst_getMtxLst( hMtxLst , tgtId["hName"] ,tgtId["mName"] ,tgt="mfMtxLst" )
		scoreMtx <- if( is.null(scoreMtxObj) ) NULL else scoreMtxObj$scoreMtx
		cutterObj$scoreMtx <- scoreMtx	# just for debug later..

		cutterObj$evtChkLst <- list()
		if( !is.null(scoreMtx) ){	# build checkLst
			#	fireThld 는 fire가 일어날 동일 패턴 수. NA이면 전부 매치.
			scoreMtx.last <- scoreMtx[nrow(scoreMtx),]

			evtChkInfo <- list( cutId="evtAll" ,fireThld=4 ,evtLst=rObj$evtLst )
			evtChkInfo$evtLast <- bUtil.getEvtVal( scoreMtx.last ,evtChkInfo$evtLst )
			cutterObj$evtChkLst[[1+length(cutterObj$evtChkLst)]] <- evtChkInfo

			grpRawN <-	c("raw.1" ,"raw.3" ,"raw.4" ,"raw.6")
			grpRemN <-	c("rem.1" ,"rem.2" ,"rem.3" ,"rem.4" ,"rem.5" ,"rem.6")
			grpCN <- 	c("c.1" ,"c.2" ,"c.3" ,"c.4" ,"c.5" ,"c.6")
			grpZW <-	c("raw.ZW" ,"rem.ZW" ,"c.ZW")

			evtChkInfo <- list( cutId="grpRawN" ,fireThld=2 ,evtLst=rObj$evtLst[grpRawN] )
			evtChkInfo$evtLast <- bUtil.getEvtVal( scoreMtx.last ,evtChkInfo$evtLst )
			cutterObj$evtChkLst[[1+length(cutterObj$evtChkLst)]] <- evtChkInfo

			evtChkInfo <- list( cutId="grpRemN" ,fireThld=3 ,evtLst=rObj$evtLst[grpRemN] )
			evtChkInfo$evtLast <- bUtil.getEvtVal( scoreMtx.last ,evtChkInfo$evtLst )
			cutterObj$evtChkLst[[1+length(cutterObj$evtChkLst)]] <- evtChkInfo

			evtChkInfo <- list( cutId="grpCN" ,fireThld=1 ,evtLst=rObj$evtLst[grpCN] )
			evtChkInfo$evtLast <- bUtil.getEvtVal( scoreMtx.last ,evtChkInfo$evtLst )
			cutterObj$evtChkLst[[1+length(cutterObj$evtChkLst)]] <- evtChkInfo

			evtChkInfo <- list( cutId="grpZW" ,fireThld=1 ,evtLst=rObj$evtLst[grpZW] )
			evtChkInfo$evtLast <- bUtil.getEvtVal( scoreMtx.last ,evtChkInfo$evtLst )
			cutterObj$evtChkLst[[1+length(cutterObj$evtChkLst)]] <- evtChkInfo

			# cName <- c("rCnt","eCnt","cCnt","fCnt")
			# evtChkInfo <- list( cutId="banCnt" ,fireThld=1 ,evtLst=rObj$evtLst[cName] )
			# evtChkInfo$evtLast <- bUtil.getEvtVal( scoreMtx.last ,evtChkInfo$evtLst )
			# cutterObj$evtChkLst[[1+length(cutterObj$evtChkLst)]] <- evtChkInfo

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
} # bFCust.A_bScr01_A_rReb01()


bFCust.A_bScr01_A_rRebAA <- function(  ){	#	이전 마지막 score(cutterObj$lastRow) 값과의 일치여부(raw Value) 

	rObj <- list( )
	rObj$defId <- c( typ="cust_RReb"	,hName="*"	,mName="bScr01"	,rFId="rRebAA" )	# row filt ID
	rObj$description <- sprintf("(cust)  ")

	rObj$createCutter <- function( hMtxLst ,tgtId=c(hName="", mName="") ,auxInfo=c(auxInfo="") ){

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
		idObjDesc <- c( idObjDesc ,auxInfo )
		cutterObj$idObjDesc <- idObjDesc

		cutterObj$idObj <- rObj$defId
		cutterObj$idObj[names(tgtId)] <- tgtId

		scoreMtxObj <- B.HMtxLst_getMtxLst( hMtxLst , tgtId["hName"] ,tgtId["mName"] ,tgt="mfMtxLst" )
		if( is.null(scoreMtxObj) ){	cutterObj$lastRow <- NULL
		} else {
			cutterObj$lastRow <- scoreMtxObj$scoreMtx[nrow(scoreMtxObj$scoreMtx),]

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

				diffCnt <- sum( cutterObj$lastRow!=scoreMtx[idx,] )
				# if( 14 < diffCnt ){
				if( !bUtil.in(diffCnt,c(min=1,max=15)) ){
					infoStr <- sprintf("cut Id : %s (diff %d)",cutterObj$idObjDesc["rFId"],diffCnt )
					cutLst[[1+length(cutLst)]] <- list( idx=idx ,idObjDesc=cutterObj$idObjDesc ,info=infoStr )
				}
			}

			return( cutLst )
		} # cutterObj$cut()

		return(cutterObj)
	}

	return( rObj )
} # bFCust.A_bScr01_A_rRebAA()




