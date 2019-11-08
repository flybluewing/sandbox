FCust_score1EvtLst <- list(	"rem0.num"=1:3 ,"rem0.len.tot"=3:8 ,"rem0.len.val"=2:6
							,"rem1.num"=1:3 ,"rem1.len.tot"=3:8 ,"rem1.len.val"=2:6
							,"c0.num"=1:3 ,"c0.len.tot"=2:8 ,"c0.len.val"=2:6
							,"c1.num"=1:3 ,"c1.len.tot"=3:8 ,"c1.len.val"=2:6
							,"f0.num"=1:3 ,"f0.len.tot"=2:8 ,"f0.len.val"=2:6
							,"f1.num"=1:3 ,"f1.len.tot"=2:8 ,"f1.len.val"=2:6
							,"zwNum"=1:4	,"zwC1Num"=1:2
						)

FCust_score1minMaxLst <- list( "rem0.num"=c(min=0,max=1) ,"rem0.len.tot"=c(min=0,max=2) ,"rem0.len.val"=c(min=0,max=2)
								,"rem1.num"=c(min=0,max=2) ,"rem1.len.tot"=c(min=0,max=4) ,"rem1.len.val"=c(min=0,max=2)
								,"c0.num"=c(min=0,max=1) ,"c0.len.tot"=c(min=0,max=2) ,"c0.len.val"=c(min=0,max=2)
								,"c1.num"=c(min=0,max=2) ,"c1.len.tot"=c(min=0,max=4) ,"c1.len.val"=c(min=0,max=2)
								,"f0.num"=c(min=0,max=1) ,"f0.len.tot"=c(min=0,max=3) ,"f0.len.val"=c(min=0,max=2)
								,"f1.num"=c(min=0,max=1) ,"f1.len.tot"=c(min=0,max=3) ,"f1.len.val"=c(min=0,max=2)
								,"zwNum"=c(min=0,max=2)	,"zwC1Num"=c(min=0,max=1)
							)

#	[score1:Col Cutter(1 col)] ------------------------------------------------------------------
#	c( typ="cust"	,hName="*"	,mName="score1"	,pName="*"	,fcName="*" )
bFCust.A_score1_A_A <- function(  ){

	rObj <- list( defId=c( typ="cust"	,hName="*"	,mName="score1"	,pName="*"	,fcName="*" ) )

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

        cutterObj$minMax <- FCust_score1minMaxLst[[tgtId["fcName"]]]

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
} # bFCust.A_score1_A_A( )

#	[score1:Col Cutter(N col)] ------------------------------------------------------------------
bFCust.A_score1_A_Row01 <- function(  ){
	rObj <- list( )
	rObj$defId <- c( typ="c_NCol.5"	,hName="*"	,mName="score1"	,pName="*"	,rFId="Row01" )	# row filt ID
	rObj$description <- sprintf("(cust)  ")

	rObj$evtLst <- FCust_score1EvtLst

	rObj$cutFLst <- list()
	rObj$cutFLst[[1+length(rObj$cutFLst)]] <- function( smRow ,evt ){
		crObj <- list( cutFlag=F ,cId="hpnOne" ) # cut result object, cut Id

		remN.len.tot <- sum(smRow[c("rem0.len.tot","rem1.len.tot")])
		if( !bUtil.in(remN.len.tot,c(min=0,max=4)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<remN.len.tot %d>",remN.len.tot) )
		}
		remN.len.val <- sum(smRow[c("rem0.len.val","rem1.len.val")])
		if( !bUtil.in(remN.len.val,c(min=0,max=2)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<remN.len.val %d>",remN.len.val) )
		}


		cN.len.tot <- sum(smRow[c("c0.len.tot","c1.len.tot")])
		if( !bUtil.in(cN.len.tot,c(min=0,max=4)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<cN.len.tot %d>",cN.len.tot) )
		}
		cN.len.val <- sum(smRow[c("c0.len.val","c1.len.val")])
		if( !bUtil.in(cN.len.val,c(min=0,max=2)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<cN.len.val %d>",cN.len.val) )
		}


		fN.len.tot <- sum(smRow[c("f0.len.tot","f1.len.tot")])
		if( !bUtil.in(fN.len.tot,c(min=0,max=4)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<fN.len.tot %d>",fN.len.tot) )
		}
		fN.len.val <- sum(smRow[c("f0.len.val","f1.len.val")])
		if( !bUtil.in(fN.len.val,c(min=0,max=2)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<fN.len.val %d>",fN.len.val) )
		}

		lenTotCnt <- remN.len.tot + cN.len.tot + fN.len.tot
		if( !bUtil.in(lenTotCnt,c(min=0,max=6)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<lenTotCnt %d>",lenTotCnt) )
		}

		valTotCnt <- remN.len.val + cN.len.val + fN.len.val
		if( !bUtil.in(valTotCnt,c(min=0,max=4)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<valTotCnt %d>",valTotCnt) )
		}

		grp0.num <- c( "rem0.num" ,"c0.num" ,"f0.num" )
		grp1.num <- c( "rem1.num" ,"c1.num" ,"f1.num" )
		grp0.len <- c( "rem0.len.tot" ,"c0.len.tot" ,"f0.len.tot" )
		grp1.len <- c( "rem1.len.tot" ,"c1.len.tot" ,"f1.len.tot" )
		grp0.val <- c( "rem0.len.val" ,"c0.len.val" ,"f0.len.val" )
		grp1.val <- c( "rem1.len.val" ,"c1.len.val" ,"f1.len.val" )

		flag <- smRow[grp0.num]==smRow[grp1.num]
		flag[smRow[grp0.num]==0] <- FALSE
		grp.num.cnt <- sum(flag)
		if( !bUtil.in(grp.num.cnt,c(min=0,max=1)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<grp.num.cnt %d>",grp.num.cnt) )
		}

		flag <- smRow[grp0.len]==smRow[grp1.len]
		flag[smRow[grp0.len]==0] <- FALSE
		grp.len.cnt <- sum(flag)
		if( !bUtil.in(grp.len.cnt,c(min=0,max=1)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<grp.len.cnt %d>",grp.len.cnt) )
		}

		flag <- smRow[grp0.val]==smRow[grp1.val]
		flag[smRow[grp0.val]==0] <- FALSE
		grp.val.cnt <- sum(flag)
		if( !bUtil.in(grp.val.cnt,c(min=0,max=1)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<grp.val.cnt %d>",grp.val.cnt) )
		}

		grp.cnt <- grp.num.cnt + grp.len.cnt + grp.val.cnt
		if( !bUtil.in(grp.cnt,c(min=0,max=2)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<grp.cnt %d>",grp.cnt) )
		}

		zwCnt <- sum(smRow[c("zwNum","zwC1Num")])
		if( !bUtil.in(zwCnt,c(min=0,max=2)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<zwCnt %d>",zwCnt) )
		}

		gCnt <- grp.cnt + zwCnt
		if( !bUtil.in(gCnt,c(min=0,max=3)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<gCnt %d>",gCnt) )
		}

		cnt <- sum( smRow==0 )
		if( !bUtil.in(cnt,c(min=12,max=20)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<ZeroCnt %d>",cnt) )
		}

		evt <- bUtil.getEvtVal( smRow ,FCust_score1EvtLst )
		cnt <- sum( !is.na(evt) )
		if( !bUtil.in(cnt,c(min=0,max=7)) ){
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
} # bFCust.A_score1_A_Row01( )

#	[score1:Col Cutter(Rebound/Sequencial)] -----------------------------------------------------
bFCust.A_score1_A_rReb01 <- function(  ){	# evt rebind
	#	row rebind라고는 했지만 사실 seq 이다. 
	#	즉 이전 evt 컬럼이 다음에도 동일 evt가 일어나는지 체크 (다음에서 추가적으로 발생하는 evt는 상관없음)
	rObj <- list( )
	rObj$defId <- c( typ="cust_RReb"	,hName="*"	,mName="score1"	,pName="*"	,rFId="rReb01" )	# row filt ID
	rObj$description <- sprintf("(cust)  ")

	rObj$evtLst <- FCust_score1EvtLst

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

			grp0.num <- c( "rem0.num" ,"c0.num" ,"f0.num" )
			grp1.num <- c( "rem1.num" ,"c1.num" ,"f1.num" )
			grp0.len <- c( "rem0.len.tot" ,"c0.len.tot" ,"f0.len.tot" )
			grp1.len <- c( "rem1.len.tot" ,"c1.len.tot" ,"f1.len.tot" )
			grp0.val <- c( "rem0.len.val" ,"c0.len.val" ,"f0.len.val" )
			grp1.val <- c( "rem1.len.val" ,"c1.len.val" ,"f1.len.val" )

			#	byHIdx에서 다수 발생 체크
			evtChkInfo <- list( cutId="grp0.num" ,fireThld=2 ,evtLst=rObj$evtLst[grp0.num] )
			evtChkInfo$evtLast <- bUtil.getEvtVal( scoreMtx.last ,evtChkInfo$evtLst )
			cutterObj$evtChkLst[[1+length(cutterObj$evtChkLst)]] <- evtChkInfo
			evtChkInfo <- list( cutId="grp1.num" ,fireThld=2 ,evtLst=rObj$evtLst[grp1.num] )
			evtChkInfo$evtLast <- bUtil.getEvtVal( scoreMtx.last ,evtChkInfo$evtLst )
			cutterObj$evtChkLst[[1+length(cutterObj$evtChkLst)]] <- evtChkInfo

			evtChkInfo <- list( cutId="grp0.len" ,fireThld=1 ,evtLst=rObj$evtLst[grp0.len] )
			evtChkInfo$evtLast <- bUtil.getEvtVal( scoreMtx.last ,evtChkInfo$evtLst )
			cutterObj$evtChkLst[[1+length(cutterObj$evtChkLst)]] <- evtChkInfo

			evtChkInfo <- list( cutId="grp1.len" ,fireThld=2 ,evtLst=rObj$evtLst[grp1.len] )
			evtChkInfo$evtLast <- bUtil.getEvtVal( scoreMtx.last ,evtChkInfo$evtLst )
			cutterObj$evtChkLst[[1+length(cutterObj$evtChkLst)]] <- evtChkInfo

			evtChkInfo <- list( cutId="grp0.val" ,fireThld=1 ,evtLst=rObj$evtLst[grp0.val] )
			evtChkInfo$evtLast <- bUtil.getEvtVal( scoreMtx.last ,evtChkInfo$evtLst )
			cutterObj$evtChkLst[[1+length(cutterObj$evtChkLst)]] <- evtChkInfo

			evtChkInfo <- list( cutId="grp1.val" ,fireThld=1 ,evtLst=rObj$evtLst[grp1.val] )
			evtChkInfo$evtLast <- bUtil.getEvtVal( scoreMtx.last ,evtChkInfo$evtLst )
			cutterObj$evtChkLst[[1+length(cutterObj$evtChkLst)]] <- evtChkInfo


			cName <- c("rem0.num","rem1.num","c0.num","c1.num")
			evtChkInfo <- list( cutId="rcN.num" ,fireThld=2 ,evtLst=rObj$evtLst[cName] )
			evtChkInfo$evtLast <- bUtil.getEvtVal( scoreMtx.last ,evtChkInfo$evtLst )
			cutterObj$evtChkLst[[1+length(cutterObj$evtChkLst)]] <- evtChkInfo

			cName <- c("rem0.num","rem1.num","f0.num","f1.num")
			evtChkInfo <- list( cutId="rfN.num" ,fireThld=2 ,evtLst=rObj$evtLst[cName] )
			evtChkInfo$evtLast <- bUtil.getEvtVal( scoreMtx.last ,evtChkInfo$evtLst )
			cutterObj$evtChkLst[[1+length(cutterObj$evtChkLst)]] <- evtChkInfo

			cName <- c("c0.num","c1.num","f0.num","f1.num")
			evtChkInfo <- list( cutId="cfN.num" ,fireThld=2 ,evtLst=rObj$evtLst[cName] )
			evtChkInfo$evtLast <- bUtil.getEvtVal( scoreMtx.last ,evtChkInfo$evtLst )
			cutterObj$evtChkLst[[1+length(cutterObj$evtChkLst)]] <- evtChkInfo


			#	byHIdx에서 다수 발생 체크
			# cName <- c("zwNum","zwC1Num")
			# evtChkInfo <- list( cutId="zwNum" ,fireThld=1 ,evtLst=rObj$evtLst[cName] )
			# evtChkInfo$evtLast <- bUtil.getEvtVal( scoreMtx.last ,evtChkInfo$evtLst )
			# cutterObj$evtChkLst[[1+length(cutterObj$evtChkLst)]] <- evtChkInfo

			evtChkInfo <- list( cutId="evtAll" ,fireThld=3 ,evtLst=rObj$evtLst )
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
					firedCutId <- c( firedCutId ,sprintf("%s/%d",evtChkInfo$cutId,sum(chk,na.rm=T)) )
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
} # bFCust.A_score1_A_rReb01()

bFCust.A_score1_A_rRebAA <- function(  ){	#	이전 마지막 score(cutterObj$lastRow) 값과의 일치여부(raw Value) 

	rObj <- list( )
	rObj$defId <- c( typ="cust_RReb"	,hName="*"	,mName="score1"	,pName="*"	,rFId="rRebAA" )	# row filt ID
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

			#	빈번하게 발생하는 컬럼은 확인대상 제외...
			cName <- setdiff( names(cutterObj$lastRow) ,c() )
			if( 3>sum(cutterObj$lastRow[cName] >0) ){
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

				diffCnt <- sum(cutterObj$lastRow!=scoreMtx[idx,])
				if( !bUtil.in(diffCnt,c(min=1,max=12)) ){
					infoStr <- sprintf("cut Id : %s mat:%d",cutterObj$idObjDesc["rFId"],diffCnt )
					cutLst[[1+length(cutLst)]] <- list( idx=idx ,idObjDesc=cutterObj$idObjDesc ,info=infoStr )
				}
			}

			return( cutLst )
		} # cutterObj$cut()

		return(cutterObj)
	}

	return( rObj )
} # bFCust.A_score1_A_rRebAA()


#	[score1:byFCol(Rebound/Sequencial)] ---------------------------------------------------------
#		- mtxGrp <- getScoreMtx.grp_byFCol( scoreMtx.grp )
#		- nRow 대상이긴 하지만, mtx는 각 scoreMtx 의 fCol 별로 생성된다는 점을 주의
#		- column이 phase이므로 pName 구분이 없고, tgtId에서도 pName이 빠진다. 대신 fcName 필요.
#	c( typ="c_byFCol"	,hName="*"	,mName="score4"	,fcName="*"  )
bFCust.byFCol_A_score1_A <- function( ){

	rObj <- list( )
	rObj$defId <- c( typ="c_byFCol"	,hName="*"	,mName="score1"	,fcName="*"  )
	rObj$description <- sprintf("(cust)  ")

	rObj$cutFLst <- list()
	if( TRUE ){	# build cutFLst
		#	"rem0.num" ,"rem0.len.tot" ,"rem0.len.val"	,"rem1.num" ,"rem1.len.tot" ,"rem1.len.val"
		rObj$cutFLst[["rem0.num"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A rem0.num" ) # cut result object, cut Id
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
		rObj$cutFLst[["rem0.len.tot"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A rem0.len.tot" ) # cut result object, cut Id
			cnt <- sum(smRow==2)
			if( !bUtil.in(cnt,c(min=0,max=7)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 02.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow==3)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 03.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow >3)
			if( !bUtil.in(cnt,c(min=0,max=0)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["rem0.len.val"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A rem0.len.val" ) # cut result object, cut Id
			cnt <- sum(smRow==2)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 02.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow==3)
			if( !bUtil.in(cnt,c(min=0,max=0)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 03.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow >3)
			if( !bUtil.in(cnt,c(min=0,max=0)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["rem1.num"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A rem1.num" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=4)) ){
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
		rObj$cutFLst[["rem1.len.tot"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A rem1.len.tot" ) # cut result object, cut Id
			cnt <- sum(smRow==2)
			if( !bUtil.in(cnt,c(min=0,max=4)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 02.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow==3)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 03.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow >3)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["rem1.len.val"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A rem1.len.val" ) # cut result object, cut Id
			cnt <- sum(smRow==2)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 02.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow==3)
			if( !bUtil.in(cnt,c(min=0,max=0)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 03.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow >3)
			if( !bUtil.in(cnt,c(min=0,max=0)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )

		#	,"c0.num" ,"c0.len.tot" ,"c0.len.val"	,"c1.num" ,"c1.len.tot" ,"c1.len.val"
		#	,"f0.num" ,"f0.len.tot" ,"f0.len.val"	,"f1.num" ,"f1.len.tot" ,"f1.len.val"
		rObj$cutFLst[["c0.num"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A c0.num" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=3)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow==2)
			if( !bUtil.in(cnt,c(min=0,max=0)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 02.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow >2)
			if( !bUtil.in(cnt,c(min=0,max=0)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["c0.len.tot"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A c0.len.tot" ) # cut result object, cut Id
			cnt <- sum(smRow==2)
			if( !bUtil.in(cnt,c(min=0,max=3)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 02.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow==3)
			if( !bUtil.in(cnt,c(min=0,max=0)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 03.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow >3)
			if( !bUtil.in(cnt,c(min=0,max=0)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["c0.len.val"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A c0.len.val" ) # cut result object, cut Id
			cnt <- sum(smRow==2)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow==3)
			if( !bUtil.in(cnt,c(min=0,max=0)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 02.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow >3)
			if( !bUtil.in(cnt,c(min=0,max=0)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["c1.num"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A c1.num" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=3)) ){
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
		rObj$cutFLst[["c1.len.tot"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A c1.len.tot" ) # cut result object, cut Id
			cnt <- sum(smRow==2)
			if( !bUtil.in(cnt,c(min=0,max=3)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 02.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow==3)
			if( !bUtil.in(cnt,c(min=0,max=0)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 03.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow >3)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["c1.len.val"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A c1.len.val" ) # cut result object, cut Id
			cnt <- sum(smRow==2)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow==3)
			if( !bUtil.in(cnt,c(min=0,max=0)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 02.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow >3)
			if( !bUtil.in(cnt,c(min=0,max=0)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )

		rObj$cutFLst[["f0.num"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A f0.num" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=3)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow==2)
			if( !bUtil.in(cnt,c(min=0,max=0)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 02.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow >2)
			if( !bUtil.in(cnt,c(min=0,max=0)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["f0.len.tot"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A f0.len.tot" ) # cut result object, cut Id
			cnt <- sum(smRow==2)
			if( !bUtil.in(cnt,c(min=0,max=2)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 02.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow==3)
			if( !bUtil.in(cnt,c(min=0,max=0)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 03.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow >3)
			if( !bUtil.in(cnt,c(min=0,max=0)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["f0.len.val"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A f0.len.val" ) # cut result object, cut Id
			cnt <- sum(smRow==2)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow==3)
			if( !bUtil.in(cnt,c(min=0,max=0)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 02.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow >3)
			if( !bUtil.in(cnt,c(min=0,max=0)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["f1.num"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A f1.num" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=4)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow==2)
			if( !bUtil.in(cnt,c(min=0,max=0)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 02.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow >2)
			if( !bUtil.in(cnt,c(min=0,max=0)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["f1.len.tot"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A f1.len.tot" ) # cut result object, cut Id
			cnt <- sum(smRow==2)
			if( !bUtil.in(cnt,c(min=0,max=3)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow==3)
			if( !bUtil.in(cnt,c(min=0,max=0)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 02.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow >3)
			if( !bUtil.in(cnt,c(min=0,max=0)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["f1.len.val"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A f1.len.val" ) # cut result object, cut Id
			cnt <- sum(smRow==2)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow==3)
			if( !bUtil.in(cnt,c(min=0,max=0)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 02.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow >3)
			if( !bUtil.in(cnt,c(min=0,max=0)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )

		#	,"zwNum"	,"zwC1Num"
		rObj$cutFLst[["zwNum"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A zwNum" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=3)) ){
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
		rObj$cutFLst[["zwC1Num"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A zwC1Num" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow==2)
			if( !bUtil.in(cnt,c(min=0,max=0)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 02.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow >2)
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
} # bFCust.byFCol_A_score1_A( )

#	typ="c_byFCol"	,hName="*"	,mName="score1"	,pName="*"	,fcName="*" )
bFCust.byFCol_A_score1_A_rReb01 <- function( ){
	#	row rebind라고는 했지만 사실 seq 이다. 
	#	즉 이전 evt 발생 phase에서 다음에도 동일 evt가 일어나는지 체크(다음에서 추가적으로 발생하는 evt는 상관없음.)
	rObj <- list( )
	rObj$defId <- c( typ="c_byFCol"	,hName="*"	,mName="score1"	,pName="*"	,fcName="*" )	# row filt ID
	rObj$description <- sprintf("(cust)  ")

	rObj$evtLst <- FCust_score1EvtLst
	rObj$fireThld.min <- c(  "rem0.num"=2 ,"rem0.len.tot"=1 ,"rem0.len.val"=1
							,"rem1.num"=3 ,"rem1.len.tot"=1 ,"rem1.len.val"=1
							,"c0.num"=2 ,"c0.len.tot"=2 ,"c0.len.val"=1
							,"c1.num"=2 ,"c1.len.tot"=2 ,"c1.len.val"=1
							,"f0.num"=1 ,"f0.len.tot"=1 ,"f0.len.val"=1
							,"f1.num"=2 ,"f1.len.tot"=1 ,"f1.len.val"=1
							,"zwNum"=3	,"zwC1Num"=1 
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
} # bFCust.byFCol_A_score1_A_rReb01( )

#	c( typ="c_byFCol"	,hName="*"	,mName="score1"	,fcName="*"  )
bFCust.byFCol_A_score1_A_rRebAA <- function( ){
	rObj <- list( )
	rObj$defId <- c( typ="c_byFCol"	,hName="*"	,mName="score1"	,fcName="*"  )
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
			fqCol <- c( "rem1.num" ,"rem1.len.tot" ,"c1.num" ,"c1.len.tot" ,"f1.num" ,"f1.len.tot" )	# 발생이 빈번한 컬럼.

			for( idx in seq_len(val.len) ){
				if( alreadyDead[idx] ) next

				if( !cutterObj$activated || is.null(cutterObj$lastRow) ) next

				diffCnt <- sum( cutterObj$lastRow!=scoreMtx[idx,] )
				if( !bUtil.in(diffCnt,c(min=1,max=8)) ){
					if( cutterObj$idObj[["fcName"]] %in% c(fqCol) ){
						if( 1>=sum(cutterObj$lastRow>0) )	next
					}

					infoStr <- sprintf("cut Id : rRebAA (raw all Mat. cnt:%d diffCnt:%d)",sum(cutterObj$lastRow),diffCnt )
					infoStr <- c( infoStr ,sprintf("- %s ",cutterObj$idObj[["fcName"]]) )
					cutLst[[1+length(cutLst)]] <- list( idx=idx ,idObjDesc=cutterObj$idObjDesc ,info=infoStr )

				}
			}

			return( cutLst )
		} # cutterObj$cut()

		return(cutterObj)
	}

	return( rObj )
} # bFCust.byFCol_A_score1_A_rRebAA( )



#	[score1:byHIdx(...)] ---------------------------------------------------------
#		- [fCol,phName] 
#		- mtxGrp <- getScoreMtx.grp_byHIdx( scoreMtx.grp )
#		- hIdxObj <- B.getHMtxLst_byHIdx( hMtxLst )
#       - 개개 score의 최대 최소값이나, evt 발생등은 이미 앞에서 모두 확인되었다.)
#		  따라서 col 방향, row 방향, mtx전체에 대한 rebPtn 체크.
#			(fCol 별 evt 기준 때문에 scoreMtx 개개별로 작성 필요.)

#		c( typ="cust_byHIdx"	,hName="*"	,mName="score1" )
bFCust.byHIdx_A_score1 <- function( ){

	rObj <- list( )
	rObj$defId <- c( typ="cust_byHIdx"	,hName="*"	,mName="score1" )
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
			cutterObj$chkEvt.last <- bFCust.get_byHIdx_score1ChkEvt( mtxLst[[length(mtxLst)]] )
		}

		cutterObj$mtxLst <- mtxLst
		cutterObj$rebPtn.skipZero <- bUtil.getMtxRebPtn.skipZero( mtxLst ,hpnThld.fCol=NA ,hpnThld.ph=NA )

		if( tgtId["mName"]==cutterObj$defId["mName"] ){
			cutterObj$chkEvt.last <- bFCust.get_byHIdx_score1ChkEvt( mtxLst[[length(mtxLst)]] )
		}


		cutterObj$cut <- function( scoreMtx ,aIdx ){
			# scoreMtx 는 1개 aZoid에 관한 [fCol,phase] mtx임을 유의.
			# 	(즉, 이 함수는 한 개 aZoid에 대한 처리로직이다.)
			#	단 surDf와 cutLst는 다른 cut함수들 결과와의 호환성 유지를 위해 구조유지.
			cutId <- character(0)

			chkEvt <- bFCust.get_byHIdx_score1ChkEvt( scoreMtx )
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

			surWin <- c(min=0,max=2)
			cnt.fCol <- sum( 0==cutterObj$rebPtn.skipZero$diffCnt.fCol(scoreMtx) )
			if( !bUtil.in(cnt.fCol,surWin) ){
				rCutId <- c( rCutId, sprintf("skipZero.fCol.%d (%d~%d)",cnt.fCol,surWin["min"],surWin["max"]) )
			}

			surWin <- c(min=0,max=4)
			cnt.ph <- sum( 0==cutterObj$rebPtn.skipZero$diffCnt.ph(scoreMtx) )
			if( !bUtil.in(cnt.ph,surWin) ){
				rCutId <- c( rCutId, sprintf("skipZero.ph.%d (%d~%d)",cnt.ph,surWin["min"],surWin["max"]) )
			}

			surWin <- c(min=0,max=4)
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
			surWindow <- c(min=2,max=45)
			if( !bUtil.in(nMatchCnt,surWindow) ) rCutId <- c( rCutId, sprintf("rawDiffCnt.%d(%d~%d)",nMatchCnt,surWindow["min"],surWindow["max"]) )

			
			# phRawMat -------------------------------------------
			fireThld <- 2
			cnt.allMat <- 0
			for( pName in colnames(scoreMtx) ){
				# 0 이 많으면 적용.
				if( 0==sum(scoreMtx[,pName]) )	next

				if( any(scoreMtx[,pName]!=cutterObj$evt$lastMtxRaw[,pName]) ) next

				cnt.allMat <- cnt.allMat + 1
			}
			if( fireThld<=cnt.allMat )	rCutId <- c( rCutId, sprintf("phRawMatCnt.%d",cnt.allMat) )

			# fColRawMat -------------------------------------------
			fireThld <- 3
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
                banPastH <- 30	# 바로 이전 H를 제외한 크기(rawDiffCnt에서 체크되므로)
                surWindow <- c(min=2,max=55)
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
			surWindow <- c(min=0,max=32)
			if( !bUtil.in(tot,surWindow) ) rCutId <- c( rCutId, sprintf("HpnTot.%d(%d~%d)",tot,surWindow["min"],surWindow["max"]) )

			tot <- sum(chkEvt$hpnInfo$fCol==0)
			surWindow <- c(min=8,max=20)
			if( !bUtil.in(tot,surWindow) ) rCutId <- c( rCutId, sprintf("HpnFCol.%d(%d~%d)",tot,surWindow["min"],surWindow["max"]) )

			tot <- sum(chkEvt$hpnInfo$phase==0)
			surWindow <- c(min=3,max=13)
			if( !bUtil.in(tot,surWindow) ) rCutId <- c( rCutId, sprintf("HpnPh.%d(%d~%d)",tot,surWindow["min"],surWindow["max"]) )

			# Hpn Last-----------------------------------------
			#	hpn 값이 이전에도 특이했는데 이번에도 특이한 경우를 제거
			#	즉 위에서의 surWindow에 살짝 걸친 경우가 연속발생하는 것을 부정.
			hpnInfo.c <- chkEvt$hpnInfo
			hpnInfo.l <- cutterObj$chkEvt.last$hpnInfo

			surWindow <- c(min=0,max=31)	# chkEvt$hpnInfo$tot 범위 참조
			tot.surLC <- c( lEvt=bUtil.in(hpnInfo.l$tot,surWindow) ,cEvt=bUtil.in(hpnInfo.c$tot,surWindow) )
			if( all(!tot.surLC) ){
				rCutId <- c( rCutId, sprintf("Hpn.tot Evt dup(%d~%d) %d->%d"
							,surWindow["min"],surWindow["max"],hpnInfo.l$tot,hpnInfo.c$tot) )	
			}

			surWindow <- c(min=7,max=19)	# chkEvt$hpnInfo$fCol 범위 참조
			tot.l <- sum(hpnInfo.l$fCol==0)
			tot.c <- sum(hpnInfo.c$fCol==0)
			fCol.surLC <- c( lEvt=bUtil.in(tot.l,surWindow) ,cEvt=bUtil.in(tot.c,surWindow) )
			if( all(!fCol.surLC) ){
				rCutId <- c( rCutId, sprintf("Hpn.fCol Evt dup(%d~%d) %d->%d"
							,surWindow["min"],surWindow["max"],tot.l,tot.c) )	
			}

			surWindow <- c(min=4,max=12)	# chkEvt$hpnInfo$phase 범위 참조
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

			surWindow <- c(min=2,max=15)
			if( !bUtil.in(cnt.fCol,surWindow) ) rCutId <- c( rCutId, sprintf("cnt.fCol.%d(%d~%d)",cnt.fCol,surWindow["min"],surWindow["max"]) )
			surWindow <- c(min=1,max=11)
			if( !bUtil.in(cnt.phase,surWindow) ) rCutId <- c( rCutId, sprintf("cnt.phase.%d(%d~%d)",cnt.phase,surWindow["min"],surWindow["max"]) )

			surWindow <- c(min=3,max=24)
			tot <- sum(cnt.fCol+cnt.phase)
			if( !bUtil.in(tot,surWindow) ) rCutId <- c( rCutId, sprintf("HpnXY.%d(%d~%d)",tot,surWindow["min"],surWindow["max"]) )

			return( rCutId )
		}
		cutterObj$cutLst[["F_RareCol"]] <- function( scoreMtx ,chkEvt ){	# working

			rCutId <- character(0)

			stdEvt <- bUtil.getEvt_byHIdx( scoreMtx ,evtLst=rObj$evtLst ,lastEvt=cutterObj$stdEvt.l )

			# <Standard> ----------------------------------------------------------------------

			rebCnt <- sum( stdEvt$hpnInfo$phaseReb["reb",] & (stdEvt$hpnInfo$phaseReb["hpn",]>0) )
			maxThld <- 2
			if( rebCnt>=maxThld ) rCutId <- c( rCutId, sprintf("stdEvt.hpn.phaseReb %d",rebCnt) )

			evtReb <- stdEvt$evtInfo$phaseReb["reb",]
			evtCnt <- sum( evtReb[ stdEvt$evtInfo$phaseReb["hpn",]>0 ] )
			maxThld <- 2
			if( evtCnt>=maxThld ) rCutId <- c( rCutId, sprintf("stdEvt.evt.phaseReb %d",evtCnt) )

			if( !is.null(stdEvt$rebInfo) ){
				summCnt <- apply( stdEvt$rebInfo$summMtx ,1 ,sum )
				maxThld <- 1
				if( summCnt["raw"] >=maxThld ){
					summ <- stdEvt$rebInfo$summMtx["raw",]
					summ <- summ[ summ>0 ]
					rptStr <- paste( names(summ) ,summ ,sep=":")
					rCutId <- c( rCutId, sprintf("stdEvt.rebInfo.summ raw - %s",paste(rptStr,collapse="/") ) )
				}

				maxThld <- 1
				if( summCnt["evt"] >=maxThld ){
					summ <- stdEvt$rebInfo$summMtx["evt",]
					summ <- summ[ summ>0 ]
					rptStr <- paste( names(summ) ,summ ,sep=":")
					rCutId <- c( rCutId, sprintf("stdEvt.rebInfo.summ evt - %s",paste(rptStr,collapse="/") ) )
				}
			}

			if( !is.null(stdEvt$rebSummReb) ){
				rsrSum <- sum( stdEvt$rebSummReb$hpnRebMtx )	# tot sum of rebSummReb 
				maxThld <- 1
				if( rsrSum>=maxThld )	rCutId <- c( rCutId, sprintf("stdEvt.rebSummReb - %d",rsrSum) )
			}

			evtMask <- stdEvt$evtInfo$evtMask
			colSum <- apply( evtMask ,2 ,sum )
			evtCnt <- sum(colSum >= 4)
			maxThld <- 2
			if( evtCnt >= maxThld )	rCutId <- c( rCutId, sprintf("evtRowSum %d",evtCnt) )

			# <Custom> ----------------------------------------------------------------------

			chkLst <- list()
			chkLst[["remN.len.tot"]] <- list( max=3 ,evt=4 ,cName=c("rem0.len.tot","rem1.len.tot") )
			chkLst[["remN.len.val"]] <- list( max=2 ,evt=2 ,cName=c("rem0.len.val","rem1.len.val") )
			chkLst[["cN.len.tot"]] <- list( max=2 ,evt=4 ,cName=c("c0.len.tot","c1.len.tot") )
			chkLst[["cN.len.val"]] <- list( max=2 ,evt=2 ,cName=c("c0.len.val","c1.len.val") )
			chkLst[["fN.len.tot"]] <- list( max=2 ,evt=4 ,cName=c("f0.len.tot","f1.len.tot") )
			chkLst[["fN.len.val"]] <- list( max=2 ,evt=4 ,cName=c("f0.len.val","f1.len.val") )

			chkLst.over <- list( )
			chkLst.over[["lenTotCnt"]] <- list( max=3 ,evt=6 ,cName=c("remN.len.tot","cN.len.tot","fN.len.tot") )
			chkLst.over[["valTotCnt"]] <- list( max=2 ,evt=4 ,cName=c("remN.len.val","cN.len.val","fN.len.val") )

			sumMtx <- sapply( chkLst ,function(chk){ apply(scoreMtx[chk$cName,],2,sum) })

			for( cnIdx in names(chkLst) ){	# chkLst
				chk <- chkLst[[cnIdx]]
				evtCnt <- sum( sumMtx[,cnIdx] >= chk$evt )
				if( evtCnt >= chk$max )	rCutId <- c( rCutId, sprintf("chkLst[[\"%s\"]] %d",cnIdx,evtCnt) )
			}
			for( lnIdx in names(chkLst.over) ){
				co <- chkLst.over[[lnIdx]]
				sumCnt <- apply( sumMtx[,co$cName] ,1 ,sum )
				evtCnt <- sum( sumCnt >= co$evt )
				if( evtCnt >= co$max )	rCutId <- c( rCutId, sprintf("chkLst.over[[\"%s\"]] %d",lnIdx,evtCnt) )
			}


			grpLst <- list()
			grpLst[["grp.num"]] <- list( max=2 ,evt=1 ,cName1=c("rem0.num","c0.num","f0.num") ,cName2=c("rem1.num","c1.num","f1.num") )
			grpLst[["grp.len"]] <- list( max=2 ,evt=1 ,cName1=c("rem0.len.tot","c0.len.tot","f0.len.tot") ,cName2=c("rem1.len.tot","c1.len.tot","f1.len.tot") )
			grpLst[["grp.val"]] <- list( max=2 ,evt=1 ,cName1=c("rem0.len.val","c0.len.val","f0.len.val") ,cName2=c("rem1.len.val","c1.len.val","f1.len.val") )
			sumMtx <- sapply( grpLst ,function(grp){
							matMtx <- scoreMtx[grp$cName1,] == scoreMtx[grp$cName2,]
							matMtx[ scoreMtx[grp$cName1,]==0 ] <- FALSE
							return( apply(matMtx,2,sum) )
			})
			for( gIdx in names(grpLst) ){
				grp <- grpLst[[gIdx]]
				evtCnt <- sum(sumMtx[,gIdx]>=grp$evt)
				if( evtCnt >= grp$max )	rCutId <- c( rCutId, sprintf("grpLst[[\"%s\"]] %d",gIdx,evtCnt) )
			}



			gCnt <- apply( sumMtx ,1 ,sum )
			evtCnt <- sum(gCnt>=2)
			if( evtCnt >= 2 )	rCutId <- c( rCutId, sprintf("gCnt %d",evtCnt) )


			zwCnt <- apply( scoreMtx[c("zwNum","zwC1Num") ,] ,2 ,sum )
			evtCnt <- sum( zwCnt>=2 )
			if( evtCnt >= 2 )	rCutId <- c( rCutId, sprintf("zwCnt %d",evtCnt) )

			evtCnt <- sum( (gCnt+evtCnt)>= 3)
			if( evtCnt >= 1 )	rCutId <- c( rCutId, sprintf("g+zw Cnt %d",evtCnt) )

			return( rCutId )
		}

		return(cutterObj)
	} # rObj$createCutter( )

	return( rObj )
} # bFCust.byHIdx_A_score2



bFCust.get_byHIdx_score1ChkEvt <- function( scoreMtx ){

	hpnInfo <- list( tot=	sum(scoreMtx>0)
					,fCol=	apply( scoreMtx ,1 ,function(byRV){ length(byRV) - sum(byRV==0) })
					,phase= apply( scoreMtx ,2 ,function(byCV){ length(byCV) - sum(byCV==0)})
	)

	return( list(hpnInfo=hpnInfo) )

} # bFCust.get_byHIdx_score1ChkEvt( )



