FCust_score4EvtLst <- list( "pBanN.r"=1:4 ,"pBanN.n"=1:4 ,"pLCol"=1 ,"pE3"=1 ,"pE4"=1 ,"pMH"=1 ,"pfNum"=1 
                            ,"iBanN"=1:4 ,"iLCol"=1 ,"iE3"=1 ,"iE4"=1 ,"iMH"=1 ,"ifNum"=1 
                            ,"FVa.m"=2:3 ,"FVa.c"=2:3 ,"aFV.m"=2:3 ,"aFV.c"=2:3
                            ,"m4"=1:4
							)

FCust_score4minMaxLst <- list( "pBanN.r"=c(min=0,max=1) ,"pBanN.n"=c(min=0,max=0) ,"pLCol"=c(min=0,max=1) 
                                    ,"pE3"=c(min=0,max=0) ,"pE4"=c(min=0,max=0) ,"pMH"=c(min=0,max=0) ,"pfNum"=c(min=0,max=0) 
                            ,"iBanN"=c(min=0,max=1) ,"iLCol"=c(min=0,max=1) 
                                    ,"iE3"=c(min=0,max=0) ,"iE4"=c(min=0,max=0) ,"iMH"=c(min=0,max=0) ,"ifNum"=c(min=0,max=0) 
                            ,"FVa.m"=c(min=0,max=1) ,"FVa.c"=c(min=0,max=3) ,"aFV.m"=c(min=0,max=1) ,"aFV.c"=c(min=0,max=3) 
                            ,"m4"=c(min=0,max=0)
							)

#	[score4:Col Cutter(1 col)] ------------------------------------------------------------------
#	c( typ="cust"	,hName="*"	,mName="score4"	,pName="*"	,fcName="*" )
bFCust.A_score4_A_A <- function(  ){

	rObj <- list( defId=c( typ="cust"	,hName="*"	,mName="score4"	,pName="*"	,fcName="*" ) )

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

        cutterObj$minMax <- FCust_score4minMaxLst[[tgtId["fcName"]]]

        cutterObj$description <- sprintf("(score4.%s %d~%d)",tgtId["fcName"]
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
} # bFCust.A_score4_A_A( )


#	[score4:Col Cutter(N col)] ------------------------------------------------------------------
bFCust.A_score4_A_Row01 <- function(  ){
	rObj <- list( )
	rObj$defId <- c( typ="c_NCol.4"	,hName="*"	,mName="score4"	,pName="*"	,rFId="Row01" )	# row filt ID
	rObj$description <- sprintf("(cust)  ")

	rObj$evtLst <- FCust_score4EvtLst

	rObj$cutFLst <- list()
	rObj$cutFLst[[1+length(rObj$cutFLst)]] <- function( smRow ,evt ){
		crObj <- list( cutFlag=F ,cId="hpnOne" ) # cut result object, cut Id
        
        cId <- ""

		cnt <- sum(smRow[c("pBanN.r","pBanN.n","iBanN")])
		if( !bUtil.in(cnt,c(min=0,max=2)) ){
			crObj$cutFlag <- TRUE
			cId <- c( cId ,sprintf( "<BanCnt %d>",cnt) )
		}

		cnt <- sum(smRow[c("pLCol","iLCol")])
		if( !bUtil.in(cnt,c(min=0,max=1)) ){
			crObj$cutFlag <- TRUE
			cId <- c( cId ,sprintf( "<LCol %d>",cnt) )
		}

		cnt <- sum(smRow[c("FVa.m","aFV.m")])
		if( !bUtil.in(cnt,c(min=0,max=2)) ){
			crObj$cutFlag <- TRUE
			cId <- c( cId ,sprintf( "<FV.m %d>",cnt) )
		}

		cnt <- sum(smRow[c("FVa.c","aFV.c")])
		if( !bUtil.in(cnt,c(min=0,max=5)) ){
			crObj$cutFlag <- TRUE
			cId <- c( cId ,sprintf( "<FV.c %d>",cnt) )
		}

        crObj$cId <- sprintf( "%s %s" ,crObj$cId ,cId )
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
} # bFCust.A_score4_A_Row01( )


#	[score4:byFCol(Rebound/Sequencial)] ---------------------------------------------------------
#		- mtxGrp <- getScoreMtx.grp_byFCol( scoreMtx.grp )
#		- nRow 대상이긴 하지만, mtx는 각 scoreMtx 의 fCol 별로 생성된다는 점을 주의
#		- column이 phase이므로 pName 구분이 없고, tgtId에서도 pName이 빠진다. 대신 fcName 필요.
#	c( typ="c_byFCol"	,hName="*"	,mName="score4"	,fcName="*"  )
bFCust.byFCol_A_score4_A <- function( ){

	rObj <- list( )
	rObj$defId <- c( typ="c_byFCol"	,hName="*"	,mName="score4"	,fcName="*"  )
	rObj$description <- sprintf("(cust)  ")

	rObj$cutFLst <- list()
	if( TRUE ){	# build cutFLst
		rObj$cutFLst[["pBanN.r"]] <- function( smRow ,fcName ){
			crObj <- list( cutFlag=F ,cId="_A pBanN.r" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow==2)
			if( !bUtil.in(cnt,c(min=0,max=0)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["pBanN.n"]] <- function( smRow ,fcName ){	# for testing
			crObj <- list( cutFlag=F ,cId="_A pBanN.n" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			cnt <- sum(smRow==2)
			if( !bUtil.in(cnt,c(min=0,max=0)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 0n.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["pLCol"]] <- function( smRow ,fcName ){	# for testing
			crObj <- list( cutFlag=F ,cId="_A pLCol" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
				crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 01.%d",crObj$cId,cnt)
			}
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["pE3"]] <- function( smRow ,fcName ){	# for testing
			crObj <- list( cutFlag=F ,cId="_A pE3" ) # cut result object, cut Id
			cnt <- sum(smRow==1)
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
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
			if( !bUtil.in(cnt,c(min=0,max=1)) ){
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
			# cnt <- sum(smRow==0)
			# if( !bUtil.in(cnt,c(min=0,max=0)) ){
			# 	crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 00.%d",crObj$cId,cnt)
			# }
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["iLCol"]] <- function( smRow ,fcName ){	# for testing
			crObj <- list( cutFlag=F ,cId="_A iLCol" ) # cut result object, cut Id
			# cnt <- sum(smRow==0)
			# if( !bUtil.in(cnt,c(min=0,max=0)) ){
			# 	crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 00.%d",crObj$cId,cnt)
			# }
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["iE3"]] <- function( smRow ,fcName ){	# for testing
			crObj <- list( cutFlag=F ,cId="_A iE3" ) # cut result object, cut Id
			# cnt <- sum(smRow==0)
			# if( !bUtil.in(cnt,c(min=0,max=0)) ){
			# 	crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 00.%d",crObj$cId,cnt)
			# }
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["iE4"]] <- function( smRow ,fcName ){	# for testing
			crObj <- list( cutFlag=F ,cId="_A iE4" ) # cut result object, cut Id
			# cnt <- sum(smRow==0)
			# if( !bUtil.in(cnt,c(min=0,max=0)) ){
			# 	crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 00.%d",crObj$cId,cnt)
			# }
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["iMH"]] <- function( smRow ,fcName ){	# for testing
			crObj <- list( cutFlag=F ,cId="_A iMH" ) # cut result object, cut Id
			# cnt <- sum(smRow==0)
			# if( !bUtil.in(cnt,c(min=0,max=0)) ){
			# 	crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 00.%d",crObj$cId,cnt)
			# }
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["ifNum"]] <- function( smRow ,fcName ){	# for testing
			crObj <- list( cutFlag=F ,cId="_A ifNum" ) # cut result object, cut Id
			# cnt <- sum(smRow==0)
			# if( !bUtil.in(cnt,c(min=0,max=0)) ){
			# 	crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 00.%d",crObj$cId,cnt)
			# }
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["FVa.m"]] <- function( smRow ,fcName ){	# for testing
			crObj <- list( cutFlag=F ,cId="_A FVa.m" ) # cut result object, cut Id
			# cnt <- sum(smRow==0)
			# if( !bUtil.in(cnt,c(min=0,max=0)) ){
			# 	crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 00.%d",crObj$cId,cnt)
			# }
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["FVa.c"]] <- function( smRow ,fcName ){	# for testing
			crObj <- list( cutFlag=F ,cId="_A FVa.c" ) # cut result object, cut Id
			# cnt <- sum(smRow==0)
			# if( !bUtil.in(cnt,c(min=0,max=0)) ){
			# 	crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 00.%d",crObj$cId,cnt)
			# }
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["aFV.m"]] <- function( smRow ,fcName ){	# for testing
			crObj <- list( cutFlag=F ,cId="_A aFV.m" ) # cut result object, cut Id
			# cnt <- sum(smRow==0)
			# if( !bUtil.in(cnt,c(min=0,max=0)) ){
			# 	crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 00.%d",crObj$cId,cnt)
			# }
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["aFV.c"]] <- function( smRow ,fcName ){	# for testing
			crObj <- list( cutFlag=F ,cId="_A aFV.c" ) # cut result object, cut Id
			# cnt <- sum(smRow==0)
			# if( !bUtil.in(cnt,c(min=0,max=0)) ){
			# 	crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 00.%d",crObj$cId,cnt)
			# }
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["m4"]] <- function( smRow ,fcName ){	# for testing
			crObj <- list( cutFlag=F ,cId="_A m4" ) # cut result object, cut Id
			# cnt <- sum(smRow==0)
			# if( !bUtil.in(cnt,c(min=0,max=0)) ){
			# 	crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 00.%d",crObj$cId,cnt)
			# }
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
} # bFCust.byFCol_A_score2_A( )

#	typ="c_byFCol"	,hName="*"	,mName="score4"	,pName="*"	,fcName="*" )
bFCust.byFCol_A_score4_A_rReb01 <- function( ){
	#	row rebind라고는 했지만 사실 seq 이다. 
	#	즉 이전 evt 발생 phase에서 다음에도 동일 evt가 일어나는지 체크(다음에서 추가적으로 발생하는 evt는 상관없음.)
	rObj <- list( )
	rObj$defId <- c( typ="c_byFCol"	,hName="*"	,mName="score4"	,pName="*"	,fcName="*" )	# row filt ID
	rObj$description <- sprintf("(cust)  ")

	rObj$evtLst <- FCust_score4EvtLst
	rObj$fireThld.min <- c( "pBanN.r"=2 ,"pBanN.n"=2 ,"pLCol"=2 ,"pE3"=2 ,"pE4"=2 ,"pMH"=2 ,"pfNum"=2
							,"iBanN"=2 ,"iLCol"=2 ,"iE3"=2 ,"iE4"=2 ,"iMH"=2 ,"ifNum"=2
							,"FVa.m"=2 ,"FVa.c"=2 ,"aFV.m"=2 ,"aFV.c"=2 ,"m4"=2
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
} # bFCust.byFCol_A_score4_A_rReb01( )

#	c( typ="c_byFCol"	,hName="*"	,mName="score4"	,fcName="*"  )
bFCust.byFCol_A_score4_A_rRebAA <- function( ){
	rObj <- list( )
	rObj$defId <- c( typ="c_byFCol"	,hName="*"	,mName="score4"	,fcName="*"  )
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

				matFlag <- all( cutterObj$lastRow==scoreMtx[idx,] )
				if( all(matFlag) ){
					infoStr <- sprintf("cut Id : rRebAA (raw all Mat %d)",length(matFlag) )
					cutLst[[1+length(cutLst)]] <- list( idx=idx ,idObjDesc=cutterObj$idObjDesc ,info=infoStr )
				}
			}

			return( cutLst )
		} # cutterObj$cut()

		return(cutterObj)
	}

	return( rObj )
} # bFCust.byFCol_A_score4_A_rRebAA( )




#	[score4:byHIdx(...)] ---------------------------------------------------------
#		- [fCol,phName] 
#		- mtxGrp <- getScoreMtx.grp_byHIdx( scoreMtx.grp )
#		- hIdxObj <- B.getHMtxLst_byHIdx( hMtxLst )
#       - 개개 score의 최대 최소값이나, evt 발생등은 이미 앞에서 모두 확인되었다.)
#		  따라서 col 방향, row 방향, mtx전체에 대한 rebPtn 체크.
#			(fCol 별 evt 기준 때문에 scoreMtx 개개별로 작성 필요.)

#		c( typ="cust_byHIdx"	,hName="*"	,mName="score4" )
bFCust.byHIdx_A_score4 <- function( ){

	rObj <- list( )
	rObj$defId <- c( typ="cust_byHIdx"	,hName="*"	,mName="score4" )
	rObj$description <- sprintf("(cust)  ")

	rObj$evtLst <- FCust_score2EvtLst

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

		cutterObj$evt <- bUtil.getMtxEvt_byRow( mtxLst ,rObj$evtLst )
		if( tgtId["mName"]==cutterObj$defId["mName"] ){
			cutterObj$chkEvt.last <- bFCust.get_byHIdx_score4ChkEvt( mtxLst[[length(mtxLst)]] )
		}

		cutterObj$cut <- function( scoreMtx ,aIdx ){
			# scoreMtx 는 1개 aZoid에 관한 [fCol,phase] mtx임을 유의.
			# 	(즉, 이 함수는 한 개 aZoid에 대한 처리로직이다.)
			#	단 surDf와 cutLst는 다른 cut함수들 결과와의 호환성 유지를 위해 구조유지.
			cutId <- character(0)

			chkEvt <- bFCust.get_byHIdx_score4ChkEvt( scoreMtx )
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
			surWindow <- c(min=4,max=25)
			if( !bUtil.in(nMatchCnt,surWindow) ) rCutId <- c( rCutId, sprintf("rawDiffCnt.%d(%d~%d)",nMatchCnt,surWindow["min"],surWindow["max"]) )


			return( rCutId )
		}
		cutterObj$cutLst[["F_colMat"]] <- function( scoreMtx ,chkEvt=NULL ){
			rCutId <- character(0)

			# match happen count ----------------------------------
			surWindow <- c(min=0,max=0)	# survive window
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
		cutterObj$cutLst[["F_hpnInfo"]] <- function( scoreMtx ,chkEvt ){
			rCutId <- character(0)

			# Hpn(18*13) -------------------------------------------
			#	tot 에서의 0 발생가능성 인정. 이에 따라 HpnFCol, HpnPh 최대값 확인은 의미 없어진다.
			#		그런데 tot 0은 1 건 밖에 없었다. 적용 가능성도 있을 듯.
			# tot <- chkEvt$hpnInfo$tot
			# surWindow <- c(min=0,max=16)
			# if( !bUtil.in(tot,surWindow) ) rCutId <- c( rCutId, sprintf("HpnTot.%d(%d~%d)",tot,surWindow["min"],surWindow["max"]) )
			#
			# tot <- sum(chkEvt$hpnInfo$fCol==0)
			# surWindow <- c(min=12,max=16)
			# if( !bUtil.in(tot,surWindow) ) rCutId <- c( rCutId, sprintf("HpnFCol.%d(%d~%d)",tot,surWindow["min"],surWindow["max"]) )
			#
			# tot <- sum(chkEvt$hpnInfo$phase==0)
			# surWindow <- c(min=7,max=12)
			# if( !bUtil.in(tot,surWindow) ) rCutId <- c( rCutId, sprintf("HpnPh.%d(%d~%d)",tot,surWindow["min"],surWindow["max"]) )

			# Hpn Last-----------------------------------------
			hpnInfo.c <- chkEvt$hpnInfo
			hpnInfo.l <- cutterObj$chkEvt.last$hpnInfo

			surWindow <- c(min=1,max=16)	# chkEvt$hpnInfo$tot 범위 참조
			tot.surLC <- c( lEvt=bUtil.in(hpnInfo.l$tot,surWindow) ,cEvt=bUtil.in(hpnInfo.c$tot,surWindow) )
			if( all(!tot.surLC) ){
				rCutId <- c( rCutId, sprintf("Hpn.tot Evt dup(%d~%d) %d->%d"
							,surWindow["min"],surWindow["max"],hpnInfo.l$tot,hpnInfo.c$tot) )	
			}

			surWindow <- c(min=12,max=16)	# chkEvt$hpnInfo$fCol 범위 참조
			tot.l <- sum(hpnInfo.l$fCol==0)
			tot.c <- sum(hpnInfo.c$fCol==0)
			fCol.surLC <- c( lEvt=bUtil.in(tot.l,surWindow) ,cEvt=bUtil.in(tot.c,surWindow) )
			if( all(!fCol.surLC) ){
				rCutId <- c( rCutId, sprintf("Hpn.fCol Evt dup(%d~%d) %d->%d"
							,surWindow["min"],surWindow["max"],tot.l,tot.c) )	
			}

			surWindow <- c(min=7,max=12)	# chkEvt$hpnInfo$phase 범위 참조
			tot.l <- sum(hpnInfo.l$phase==0)
			tot.c <- sum(hpnInfo.c$phase==0)
			phase.surLC <- c( lEvt=bUtil.in(tot.l,surWindow) ,cEvt=bUtil.in(tot.c,surWindow) )
			if( all(!phase.surLC) ){
				rCutId <- c( rCutId, sprintf("Hpn.phase Evt dup(%d~%d) %d->%d"
							,surWindow["min"],surWindow["max"],tot.l,tot.c) )	
			}


			cnt.fCol <- sum(hpnInfo.c$fCol!=hpnInfo.l$fCol)
			cnt.phase <- sum(hpnInfo.c$phase!=hpnInfo.l$phase)
			firThld <- 2
			if( firThld >= (cnt.fCol+cnt.phase) ){
				rCutId <- c( rCutId, sprintf("HpnXY.(thld:%d) fCol:%d phase:%d",firThld,cnt.fCol,cnt.phase) 
				)
			}

			return( rCutId )
		}


		return(cutterObj)
	} # rObj$createCutter( )

	return( rObj )
} # bFCust.byHIdx_A_score2


bFCust.get_byHIdx_score4ChkEvt <- function( scoreMtx ){

	hpnInfo <- list( tot=	sum(scoreMtx>0)
					,fCol=	apply( scoreMtx ,1 ,function(byRV){ length(byRV) - sum(byRV==0) })
					,phase= apply( scoreMtx ,2 ,function(byCV){ length(byCV) - sum(byCV==0)})
	)

	return( list(hpnInfo=hpnInfo) )

} # bFCust.get_byHIdx_score4ChkEvt( )






