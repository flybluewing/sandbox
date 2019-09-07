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


#	[score4:Col Cutter] ------------------------------------------------------------------
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
			# cnt <- sum(smRow==0)
			# if( !bUtil.in(cnt,c(min=0,max=0)) ){
			# 	crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 00.%d",crObj$cId,cnt)
			# }
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["pLCol"]] <- function( smRow ,fcName ){	# for testing
			crObj <- list( cutFlag=F ,cId="_A pLCol" ) # cut result object, cut Id
			# cnt <- sum(smRow==0)
			# if( !bUtil.in(cnt,c(min=0,max=0)) ){
			# 	crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 00.%d",crObj$cId,cnt)
			# }
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["pE3"]] <- function( smRow ,fcName ){	# for testing
			crObj <- list( cutFlag=F ,cId="_A pE3" ) # cut result object, cut Id
			# cnt <- sum(smRow==0)
			# if( !bUtil.in(cnt,c(min=0,max=0)) ){
			# 	crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 00.%d",crObj$cId,cnt)
			# }
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["pE4"]] <- function( smRow ,fcName ){	# for testing
			crObj <- list( cutFlag=F ,cId="_A pE4" ) # cut result object, cut Id
			# cnt <- sum(smRow==0)
			# if( !bUtil.in(cnt,c(min=0,max=0)) ){
			# 	crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 00.%d",crObj$cId,cnt)
			# }
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["pMH"]] <- function( smRow ,fcName ){	# for testing
			crObj <- list( cutFlag=F ,cId="_A pMH" ) # cut result object, cut Id
			# cnt <- sum(smRow==0)
			# if( !bUtil.in(cnt,c(min=0,max=0)) ){
			# 	crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 00.%d",crObj$cId,cnt)
			# }
			return( crObj )
		} # rObj$cutFLst[1]( )
		rObj$cutFLst[["pfNum"]] <- function( smRow ,fcName ){	# for testing
			crObj <- list( cutFlag=F ,cId="_A pfNum" ) # cut result object, cut Id
			# cnt <- sum(smRow==0)
			# if( !bUtil.in(cnt,c(min=0,max=0)) ){
			# 	crObj$cutFlag <- TRUE	;crObj$cId <- sprintf("%s 00.%d",crObj$cId,cnt)
			# }
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




#	[score4:Row Cutter] ------------------------------------------------------------------









