FCust_score9EvtLst <- list(		"rCnt"=1 ,"rD2"=1 ,"rDn"=1 ,"rLr"=1 ,"rRl"=1
								,"eCnt"=1 ,"eD2"=1 ,"eDn"=1 ,"eLr"=1 ,"eRl"=1
								,"cCnt"=1 ,"cD2"=1 ,"cDn"=1 ,"cLr"=1 ,"cRl"=1
								,"fCnt"=1 ,"fD2"=1 ,"fDn"=1 ,"fLr"=1 ,"fRl"=1
							)

FCust_score9minMaxLst <- list( "rCnt"=c(min=0,max=1) ,"rD2"=c(min=0,max=1) ,"rDn"=c(min=0,max=1) ,"rLr"=c(min=0,max=1) ,"rRl"=c(min=0,max=1)
								,"eCnt"=c(min=0,max=1) ,"eD2"=c(min=0,max=1) ,"eDn"=c(min=0,max=1) ,"eLr"=c(min=0,max=1) ,"eRl"=c(min=0,max=1)
								,"cCnt"=c(min=0,max=1) ,"cD2"=c(min=0,max=1) ,"cDn"=c(min=0,max=1) ,"cLr"=c(min=0,max=1) ,"cRl"=c(min=0,max=1)
								,"fCnt"=c(min=0,max=1) ,"fD2"=c(min=0,max=1) ,"fDn"=c(min=0,max=1) ,"fLr"=c(min=0,max=1) ,"fRl"=c(min=0,max=1)
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






