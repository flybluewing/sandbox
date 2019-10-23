FCust_bScr02EvtLst <- list(	"r.lm"=2:5 ,"r.m2"=2:5 ,"r.mN"=1:5
							,"sq.lma"=1:5 ,"sq.lmt"=2:5 
							,"sq.lmaRem"=1:5 ,"sq.lmtRem"=3:5 ,"sq.ma"=1:5 ,"sq.mt"=3:5
							,"c2.lm"=2:5 ,"c2.m2"=2:5 ,"c2.mN"=1:5
							,"sq3.lma"=1:5 ,"sq3.lmt"=2:5 ,"sq3.lmaRem"=1:5 ,"sq3.lmtRem"=2:5 
							,"sq3.ma"=1:5 ,"sq3.m2"=1:5 ,"sq3.mt"=2:5
							,"c3.lma"=1:5 ,"c3.lmt"=3:5 ,"c3.ma"=1:5 ,"c3.mt"=3:5
						)

FCust_bScr02minMaxLst <- list( "r.lm"=c(min=0,max=2) ,"r.m2"=c(min=0,max=2) ,"r.mN"=c(min=0,max=0)
								,"sq.lma"=c(min=0,max=1) ,"sq.lmt"=c(min=0,max=2) 
								,"sq.lmaRem"=c(min=0,max=1) ,"sq.lmtRem"=c(min=0,max=3) ,"sq.ma"=c(min=0,max=1) ,"sq.mt"=c(min=0,max=2)
								,"c2.lm"=c(min=0,max=2) ,"c2.m2"=c(min=0,max=2) ,"c2.mN"=c(min=0,max=0)
								,"sq3.lma"=c(min=0,max=0) ,"sq3.lmt"=c(min=0,max=2) ,"sq3.lmaRem"=c(min=0,max=0) ,"sq3.lmtRem"=c(min=0,max=2) 
								,"sq3.ma"=c(min=0,max=0) ,"sq3.m2"=c(min=0,max=1) ,"sq3.mt"=c(min=0,max=2)
								,"c3.lma"=c(min=0,max=1) ,"c3.lmt"=c(min=0,max=2) ,"c3.ma"=c(min=0,max=1) ,"c3.mt"=c(min=0,max=2)	
							)


#	[bScr02:Col Cutter(1 col)] ------------------------------------------------------------------
#	c( typ="cust"	,hName="*"	,mName="bScr02"	,fcName="*" )
bFCust.A_bScr02_A_A <- function(  ){

	rObj <- list( defId=c( typ="cust"	,hName="*"	,mName="bScr02"	,fcName="*" ) )

	rObj$createCutter <- function( tgtId=c(hName="", mName="", fcName="") ,auxInfo=c(auxInfo="") ){

		cutterObj <- rObj	;cutterObj$createCutter <- NULL

		#	hName="testNA"; mName="testNA"; fcName="testNA"; auxInfo=c(auxInfo="")
		idObjDesc <- rObj$defId
		if( idObjDesc["hName"]!=tgtId["hName"] ) idObjDesc["hName"] <- sprintf("(%s)%s",idObjDesc["hName"],tgtId["hName"])
		if( idObjDesc["mName"]!=tgtId["mName"] ) idObjDesc["mName"] <- sprintf("(%s)%s",idObjDesc["mName"],tgtId["mName"])
		if( idObjDesc["fcName"]!=tgtId["fcName"] ) idObjDesc["fcName"] <- sprintf("(%s)%s",idObjDesc["fcName"],tgtId["fcName"])
		idObjDesc <- c( idObjDesc ,auxInfo )
		cutterObj$idObjDesc <- idObjDesc

        cutterObj$minMax <- FCust_bScr02minMaxLst[[tgtId["fcName"]]]

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
} # bFCust.A_bScr02_A_A( )

#	[bScr02:Col Cutter(N col)] ------------------------------------------------------------------
bFCust.A_bScr02_A_Row01 <- function(  ){
	rObj <- list( )
	rObj$defId <- c( typ="c_NCol.5"	,hName="*"	,mName="bScr02"	,rFId="Row01" )	# row filt ID
	rObj$description <- sprintf("(cust)  ")

	rObj$evtLst <- FCust_bScr02EvtLst

	rObj$cutFLst <- list()
	rObj$cutFLst[[1+length(rObj$cutFLst)]] <- function( smRow ,evt ){
		crObj <- list( cutFlag=F ,cId="hpnOne" ) # cut result object, cut Id

		evt <- bUtil.getEvtVal( smRow ,FCust_bScr02EvtLst )

		cntEvt <- sum( !is.na(evt) )
		if( !bUtil.in(cntEvt,c(min=0,max=2)) ){
			crObj$cutFlag <- TRUE
			crObj$cId <- c( crObj$cId ,sprintf( "<cntEvt %d>",cntEvt) )
		}

		cnt <- sum( smRow==0 )	#	banCnt 에서 0이 존재하는 이상, 의미없음.
		if( !bUtil.in(cnt,c(min=15,max=22)) ){
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
} # bFCust.A_bScr02_A_Row01( )





