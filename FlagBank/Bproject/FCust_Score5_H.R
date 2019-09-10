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


#	[score5:Row Cutter] ------------------------------------------------------------------









