FCust_score6EvtLst <- list( "pBanN.r"=1 ,"pBanN.n"=1 ,"pLCol"=1 ,"pE3"=1 ,"pE4"=1 ,"pMH"=1 ,"pfNum"=1
                            ,"iBanN"=1 ,"iLCol"=1 ,"iE3"=1 ,"iE4"=1 ,"iMH"=1 ,"ifNum"=1
                            ,"FVa.m"=2 ,"FVa.c"=2 ,"aFV.m"=2 ,"aFV.c"=2
                            ,"m4"=1
							)

FCust_score6minMaxLst <- list( "pBanN.r"=c(min=0,max=1) ,"pBanN.n"=c(min=0,max=1) ,"pLCol"=c(min=0,max=1) 
                                    ,"pE3"=c(min=0,max=0) ,"pE4"=c(min=0,max=0) ,"pMH"=c(min=0,max=0) ,"pfNum"=c(min=0,max=0) 
                            ,"iBanN"=c(min=0,max=1) ,"iLCol"=c(min=0,max=1) 
                                    ,"iE3"=c(min=0,max=0) ,"iE4"=c(min=0,max=0) ,"iMH"=c(min=0,max=0) ,"ifNum"=c(min=0,max=0) 
                            ,"FVa.m"=c(min=0,max=2) ,"FVa.c"=c(min=0,max=2) ,"aFV.m"=c(min=0,max=2) ,"aFV.c"=c(min=0,max=2) 
                            ,"m4"=c(min=0,max=0)
							)


#	[score6:Col Cutter(1 col)] ------------------------------------------------------------------
#	c( typ="cust"	,hName="*"	,mName="score6"	,pName="*"	,fcName="*" )
bFCust.A_score6_A_A <- function(  ){

	rObj <- list( defId=c( typ="cust"	,hName="*"	,mName="score6"	,pName="*"	,fcName="*" ) )

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

        cutterObj$minMax <- FCust_score6minMaxLst[[tgtId["fcName"]]]

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
} # bFCust.A_score6_A_A( )


#	[score6:Col Cutter(N col)] ------------------------------------------------------------------
bFCust.A_score6_A_Row01 <- function(  ){
	rObj <- list( )
	rObj$defId <- c( typ="c_NCol.5"	,hName="*"	,mName="score6"	,pName="*"	,rFId="Row01" )	# row filt ID
	rObj$description <- sprintf("(cust)  ")

	rObj$evtLst <- FCust_score6EvtLst

	rObj$cutFLst <- list()
	rObj$cutFLst[[1+length(rObj$cutFLst)]] <- function( smRow ,evt ){
		crObj <- list( cutFlag=F ,cId="hpnOne" ) # cut result object, cut Id

		cnt <- sum(smRow[c("pBanN.r","pBanN.n","iBanN")])
		if( !bUtil.in(cnt,c(min=0,max=1)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<BanCnt %d>",cnt) )
		}


		pRareCol <- c("pLCol","pE3","pE4","pMH","pfNum")
		iRareCol <- c("iLCol","iE3","iE4","iMH","ifNum")

		pCnt <- sum(smRow[pRareCol])
		if( !bUtil.in(pCnt,c(min=0,max=1)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<pEvtCnt %d>",pCnt) )
		}
		iCnt <- sum(smRow[iRareCol])
		if( !bUtil.in(iCnt,c(min=0,max=1)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<iEvtCnt %d>",iCnt) )
		}
		if( !bUtil.in((pCnt+iCnt),c(min=0,max=1)) ){
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
		if( !bUtil.in(cnt,c(min=13,max=18)) ){
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
} # bFCust.A_score6_A_Row01( )




#	[score6:Col Cutter] ------------------------------------------------------------------

#	[score6:Row Cutter] ------------------------------------------------------------------









