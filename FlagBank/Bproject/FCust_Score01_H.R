FCust_score1EvtLst <- list(	"rem0.num"=1 ,"rem0.len.tot"=1 ,"rem0.len.val"=1
							,"rem1.num"=1 ,"rem1.len.tot"=1 ,"rem1.len.val"=1
							,"c0.num"=1 ,"c0.len.tot"=1 ,"c0.len.val"=1
							,"c1.num"=1 ,"c1.len.tot"=1 ,"c1.len.val"=1
							,"f0.num"=1 ,"f0.len.tot"=1 ,"f0.len.val"=1
							,"f1.num"=1 ,"f1.len.tot"=1 ,"f1.len.val"=1
							,"zwNum"=1	,"zwC1Num"=1
						)

FCust_score9minMaxLst <- list( "rem0.num"=c(min=0,max=0) ,"rem0.len.tot"=c(min=0,max=0) ,"rem0.len.val"=c(min=0,max=0)
								,"rem1.num"=c(min=0,max=0) ,"rem1.len.tot"=c(min=0,max=0) ,"rem1.len.val"=c(min=0,max=0)
								,"c0.num"=c(min=0,max=0) ,"c0.len.tot"=c(min=0,max=0) ,"c0.len.val"=c(min=0,max=0)
								,"c1.num"=c(min=0,max=0) ,"c1.len.tot"=c(min=0,max=0) ,"c1.len.val"=c(min=0,max=0)
								,"f0.num"=c(min=0,max=0) ,"f0.len.tot"=c(min=0,max=0) ,"f0.len.val"=c(min=0,max=0)
								,"f1.num"=c(min=0,max=0) ,"f1.len.tot"=c(min=0,max=0) ,"f1.len.val"=c(min=0,max=0)
								,"zwNum"=c(min=0,max=0)	,"zwC1Num"=c(min=0,max=0)
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







