# ÇÑ±Û
FCust_score8EvtLst <- list( 	"c41"=1,"c42"=1,"c43"=1
								,"c31"=1,"c32"=1,"c33"=1,"c34"=1
								,"c21"=1,"c22"=1,"c23"=1,"c24"=1,"c25"=1
								,"max3"=1,"min3"=1,"max2"=1,"min2"=1
								,"cTbl"=1
							)

FCust_score8minMaxLst <- list( "c41"=c(min=0,max=0),"c42"=c(min=0,max=0),"c43"=c(min=0,max=0)
								,"c31"=c(min=0,max=0),"c32"=c(min=0,max=0),"c33"=c(min=0,max=0),"c34"=c(min=0,max=0)
								,"c21"=c(min=0,max=0),"c22"=c(min=0,max=0),"c23"=c(min=0,max=0),"c24"=c(min=0,max=0),"c25"=c(min=0,max=0)
								,"max3"=c(min=0,max=0),"min3"=c(min=0,max=0),"max2"=c(min=0,max=0),"min2"=c(min=0,max=0)
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

