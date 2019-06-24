
#	fMtx 박스 생성


bFMtx.score2 <- function( stdMIObj ,zMtx ){
	# stdMIObj	: stdMI ,zMtx

	#	...ab. <-- left slide ( k, a, b 패턴에 대한 V 값)
	#   ..k...
	#   .V....

	getSlideReb <- function( pZMtx ){

		hSize <- nrow(pZMtx)
		if( 3>hSize ){ return( NULL ) }

		hSpan <- (hSize-1):2		;hWidth <- ncol(pZMtx)

		rObj <- list()	;dbgObj <- list()

		if( TRUE ){	# left slide
			colSpan <- 1:(hWidth-2)
			rName <- c("col","val","ref1","ref2")
			lMtx <- matrix( NA, nrow=length(rName), ncol=length(colSpan) )
			rownames( lMtx ) <- rName	;colnames( lMtx ) <- paste( "c" ,colSpan,sep="")
			lMtx["col",] <- colSpan

			for( idx in 1:ncol(lMtx) ){
				colIdx <- lMtx["col",idx]
				lMtx[c("ref1","ref2"),idx] <- c( pZMtx[hSize,colIdx+1] ,pZMtx[hSize-1,colIdx+2] )
			}

			dbgInfo <- list()
			for( hIdx in hSize:3 ){
				sObj <- getSlideReb.ptnLst( pZMtx ,hIdx ,"left" )
				for( idx in 1:ncol(lMtx) ){
					if( !is.na(lMtx["val",idx]) ){
						next
					}
					for( lIdx in 1:length(sObj) ){
						matFlag <- lMtx[c("ref1","ref2"),idx]==sObj[[lIdx]]$val[c("ref-1","ref-2")]
						if( all(matFlag) ){
							lMtx["val",idx] <- sObj[[lIdx]]$val["tgtV"]
							dbgInfo[[1+length(dbgInfo)]] <- c( hIdx ,idx ,lIdx ,sObj[[lIdx]]$val[c("ref-1","ref-2")] )
							names(dbgInfo[[length(dbgInfo)]]) <- c("hIdx","idx","lIdx",c("ref-1","ref-2"))
							break
						}
					}
				}

				if( all(!is.na(lMtx["val",])) ) {
					break
				}
			}
			#	hIdx<-719    ;pZMtx[(hIdx-4):hIdx,]
			rObj$lMtx <- lMtx
		}

		if( TRUE ){ # right slide
			colSpan <- 3:hWidth
			rName <- c("col","val","ref1","ref2")
			rMtx <- matrix( NA, nrow=length(rName), ncol=length(colSpan) )
			rownames( rMtx ) <- rName	;colnames( rMtx ) <- paste( "c" ,colSpan,sep="")
			rMtx["col",] <- colSpan

			for( idx in 1:ncol(rMtx) ){
				colIdx <- rMtx["col",idx]
				rMtx[c("ref1","ref2"),idx] <- c( pZMtx[hSize,colIdx-1] ,pZMtx[hSize-1,colIdx-2] )
			}

			dbgInfo <- list()
			for( hIdx in hSize:3 ){
				sObj <- getSlideReb.ptnLst( pZMtx ,hIdx ,"right" )
				for( idx in 1:ncol(rMtx) ){
					if( !is.na(rMtx["val",idx]) ){
						next
					}
					for( lIdx in 1:length(sObj) ){
						matFlag <- rMtx[c("ref1","ref2"),idx]==sObj[[lIdx]]$val[c("ref-1","ref-2")]
						if( all(matFlag) ){
							rMtx["val",idx] <- sObj[[lIdx]]$val["tgtV"]
							dbgInfo[[1+length(dbgInfo)]] <- c( hIdx ,idx ,lIdx ,sObj[[lIdx]]$val[c("ref-1","ref-2")] )
							names(dbgInfo[[length(dbgInfo)]]) <- c("hIdx","idx","lIdx",c("ref-1","ref-2"))
							break
						}
					}
				}

				if( all(!is.na(rMtx["val",])) ) {
					break
				}
			}
			#	hIdx<-333    ;pZMtx[(hIdx-4):hIdx,]

			rObj$rMtx <- rMtx
		}

		return( rObj )
	}	# getSlideReb()
	getSlideReb.ptnLst <- function( pZMtx ,curHIdx ,direc="left" ){
		rObj <- list()

		hWidth <- ncol(pZMtx)
		if( "left"==direc ){
			cSpan <- 1:(hWidth-2)
			for( cIdx in cSpan ){
				uObj <- list( col=cIdx )
				uObj$val <- c( pZMtx[curHIdx,cIdx] ,pZMtx[curHIdx-1,cIdx+1] ,pZMtx[curHIdx-2,cIdx+2] )
				names( uObj$val ) <- c("tgtV","ref-1","ref-2")
				rObj[[1+length(rObj)]] <- uObj
			}
		} else {
			cSpan <- 3:hWidth
			for( cIdx in cSpan ){
				uObj <- list( col=cIdx )
				uObj$val <- c( pZMtx[curHIdx,cIdx] ,pZMtx[curHIdx-1,cIdx-1] ,pZMtx[curHIdx-2,cIdx-2] )
				names( uObj$val ) <- c("tgtV","ref-1","ref-2")
				rObj[[1+length(rObj)]] <- uObj
			}
		}
		return( rObj )
	}	# getSlideReb.ptnLst()

	stdMI <- stdMIObj$stdMI
	zMtx <- stdMIObj$zMtx
	rObj <- list( lastZoid=stdMI$lastZoid ,lastCStep=stdMI$cStep ,lastFStep=stdMI$fStep )

	if( TRUE ){
		inc.stdRaw		<- if( 2>stdMI$mtxLen ){ NULL 
							} else {
								vDiff <- stdMI$lastZoid - zMtx[stdMI$mtxLen-1,]
								stdMI$lastZoid+vDiff
							}
		inc.stdRaw2		<- if( 4>stdMI$mtxLen ){ NULL 
							} else {
								vDiff <- zMtx[stdMI$mtxLen-1,] - zMtx[stdMI$mtxLen-3,]
								zMtx[stdMI$mtxLen-1,]+vDiff
							}
		inc.stdRaw3		<- if( 6>stdMI$mtxLen ){ NULL 
							} else {
								vDiff <- zMtx[stdMI$mtxLen-2,] - zMtx[stdMI$mtxLen-5,]
								zMtx[stdMI$mtxLen-2,]+vDiff
							}
		inc.stdCStep	<- if( 2>stdMI$mtxLen ){ NULL 
							} else {
								h2Zoid <- zMtx[stdMI$mtxLen-1,]
								vDiff <- stdMI$cStep - (h2Zoid[2:6]-h2Zoid[1:5])
								stdMI$cStep+vDiff
							}
		inc.stdCStep2	<- if( 4>stdMI$mtxLen ){ NULL 
							} else {
								cStep <- zMtx[,2:6] - zMtx[,1:5]
								vDiff <- cStep[stdMI$mtxLen-1,] - cStep[stdMI$mtxLen-3,]
								cStep[stdMI$mtxLen-1,]+vDiff
							}
		inc.stdCStep3	<- if( 6>stdMI$mtxLen ){ NULL 
							} else {
								cStep <- zMtx[,2:6] - zMtx[,1:5]
								vDiff <- cStep[stdMI$mtxLen-2,] - cStep[stdMI$mtxLen-5,]
								cStep[stdMI$mtxLen-2,]+vDiff
							}
		fStepLen <- nrow(stdMI$fStepTail)
		inc.stdFStep	<- if( 2>fStepLen ){ NULL 
							} else {
								vDiff <- stdMI$fStep - stdMI$fStepTail[fStepLen-1,]
								stdMI$fStep+vDiff
							}
		inc.stdFStep2	<- if( 4>fStepLen ){ NULL 
							} else {
								vDiff <- stdMI$fStepTail[fStepLen-1,] - stdMI$fStepTail[fStepLen-3,]
								stdMI$fStepTail[fStepLen-1,]+vDiff
							}
		inc.stdFStep3	<- if( 6>fStepLen ){ NULL 
							} else {
								vDiff <- stdMI$fStepTail[fStepLen-2,] - stdMI$fStepTail[fStepLen-5,]
								stdMI$fStepTail[fStepLen-2,]+vDiff
							}

		rObj$inc.stdRaw <- inc.stdRaw	;rObj$inc.stdRaw2 <- inc.stdRaw2	;rObj$inc.stdRaw3 <- inc.stdRaw3
		rObj$inc.stdCStep <- inc.stdCStep	;rObj$inc.stdCStep2 <- inc.stdCStep2	;rObj$inc.stdCStep3 <- inc.stdCStep3
		rObj$inc.stdFStep <- inc.stdFStep	;rObj$inc.stdFStep2 <- inc.stdFStep2	;rObj$inc.stdFStep3 <- inc.stdFStep3

		rObj$slideObj <- getSlideReb( zMtx )
		
	}

	#	colNames :	"rebV.r","rebL","rebR"											--> 동일 값 발생, 사선 방향 동일 패턴 발생.
	#				,"rebC.r","rebC.c","rebC.f","rebC2,r","rebC2,c","rebC2,f"		--> 동일컬럼 값 재발생.
	#				,"inc.r","inc.c","inc.f","inc.r2","inc.c2","inc.f2","inc.r3","inc.c3","inc.f3"	--> 증감 패턴이 다음에도 유지.
	#				,"rebL.info","rebR.info"
	rObj$fMtxObj <- function( aZoidMtx ,makeInfoStr=F ){
		#	fMtxObj <- list( scoreMtx ,infoMtx )
		aLen <- nrow(aZoidMtx)
		cName <- c("rebV.r","rebL","rebR","rebC.r","rebC.c","rebC.f","rebC2,r","rebC2,c","rebC2,f")
		cName <- c( cName, c("inc.r","inc.c","inc.f","inc.r2","inc.c2","inc.f2","inc.r3","inc.c3","inc.f3") )
		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(cName) )	;colnames(scoreMtx) <- cName

		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c("rebL.info","rebR.info")
			infoMtx <- matrix( "" ,nrow=aLen ,ncol=length(cName) )	;colnames(infoMtx) <- cName
		}

		for( aIdx in 1:aLen ){
			#	working

			if( makeInfoStr ){

			}
		}

		return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
	}
	return( rObj )
}
