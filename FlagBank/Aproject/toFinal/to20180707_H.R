# to20180707_H.R 최종접근

	# 	799 12 17 23 34 42 45
	# 	800  1  4 10 12 28 45
	# 	801 17 25 28 37 43 44
	# 	802 10 11 12 18 24 42
	# 	803  5  9 14 26 30 43
	# 	804  1 10 13 26 32 36
	#   805  3 12 13 18 31 32
	#   806 14,20,23,31,37,38
	#   807  6,10,18,25,34,35
	#   808 15 21 31 32 41 43
	#	809  6 11 15 17 23 40
	#   810  5 10 13 21 39 43
	#	811  8 11 19 21 36 45
    #   812  1  3 12 14 16 43
	#	813 11 30 34 35 42 44

	# colValSeqNext( ,pColSize=2 )
	# 765  9 30   14 20    1  2    7 11
	# 762  3  8    8 16   13 22   16 24
	# 568  3  6    7  9   20 30   19 39
	# 484 17 22    6  9    7  8   13 15
	# 430 18 22    8 13   23 24   37 42
	# 410 11 14    1  3   17 27   25 26
	# 392  9 16    6 14    7 20
	# 299  7  9   17 18    8 18
	# 281  2  5    6 15   13 15
	# 223  4 19    6 30    8  9
	# 205  1  2            5  7
	# 203  3 12            3 21
	# 101 17 22            1  3
	# 99   1  7
	# 96   6  7
	# 76   2 18

fCut.colValSeqNext <- function( gEnv ,allIdxF ){

	stdIdx <- 6796699	;stdZoid <- c( 11, 30,34,35,42,44)
	allIdxF <- stdIdx

	lastZoid <- gEnv$zhF[nrow(gEnv$zhF),]

	# =========================================================
	# anaColEndPtn()
	colPtnLst <- anaColEndPtn( gEnv$zhF ,pDebug=T )
	# 15 10  1  6
	#  3  5  2 23 16  5 15
	# 19 27 13 16
	# 13 25 22 10
	# NA
	# 29 43 41 44 45 43 26 31 41

	# <recycle>
	banVal <- sapply( colPtnLst ,function( p ){
					return( if(length(p$val)>0) p$val[1] else NA )
				})
	banVal.idx <- which( !is.na(banVal) )
	banVal <- banVal[banVal.idx]

	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( !any(aZoid[banVal.idx]==banVal) )
				})	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# <recycle>
	banVal.rem <- banVal%%10
	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid[banVal.idx] %% 10
					cnt <- sum( aCode==banVal.rem )
					return( cnt<2 )
				})	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	#
	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					cnt <- ifelse( aZoid[1]%in%c(15,10, 1, 6) 
									,cnt+1 ,cnt )
					cnt <- ifelse( aZoid[2]%in%c( 3, 5, 2,23,16, 5,15) 
									,cnt+1 ,cnt )
					cnt <- ifelse( aZoid[3]%in%c(19,27,13,16) 
									,cnt+1 ,cnt )
					cnt <- ifelse( aZoid[4]%in%c(13,25,22,10) 
									,cnt+1 ,cnt )
					cnt <- ifelse( aZoid[6]%in%c(29,26,31,42,43) 
									,cnt+1 ,cnt )
					return( cnt<2 )
				})	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# =========================================================
	# colValSeqNext( ,pColSize=2 )
	cvSeqNextLst <- colValSeqNext( gEnv$zhF ,pColSize=2 )

	banCodeLst <- lapply( cvSeqNextLst, function(p){ 
						if( 0<nrow(p$fndMtx) ) p$fndMtx[1,]%%10 else integer(0)
					})
	banCode.span <- which( sapply( banCodeLst ,function(p){length(p)>0}) )

	# <recycle>
	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F]%%10 ,1 ,function( aCode ){
					cnt <- 0
					for( cIdx in banCode.span ){
						cnt <- ifelse( all(aCode[cIdx:(cIdx+1)]==banCodeLst[[cIdx]]) 
										,cnt+1 ,cnt )
					}
					return( cnt==0 )
				})	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# <recycle>
	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F]%%10 ,1 ,function( aCode ){
					cnt <- 0
					for( cIdx in banCode.span ){
						cnt <- ifelse( all(aCode[cIdx:(cIdx+1)]==banCodeLst[[cIdx]]) 
										,cnt+1 ,cnt )
					}
					return( cnt==0 )
				})	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# <recycle>
	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F]%%10 ,1 ,function( aCode ){
					cnt <- 0
					for( cIdx in banCode.span ){
						cnt <- cnt + sum(aCode[cIdx:(cIdx+1)]==banCodeLst[[cIdx]])
					}
					return( cnt<2 )
				})	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# <recycle> 과거 패턴은 재발되지 않는 듯 하다..
	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aCode ){
					for( cIdx in banCode.span ){
						for( rIdx in 1:nrow(cvSeqNextLst[[cIdx]]$fndMtx) ){
							if( all(aCode[cIdx:(cIdx+1)]==cvSeqNextLst[[cIdx]]$fndMtx[rIdx,]) ){
								return( FALSE )
							}
						}
					}
					return( TRUE )
				})	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	#
	flag <- !(gEnv$allZoidMtx[allIdxF,1]%in%c(9))	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# 한 개 정도는 허용하는 게 recycle을 위해서는 적절할 듯.
    #  21   5   3   5   4   3   7   2   3  15   1   9   5   6   1  16 
    #   6   8   2   3   5   2   8   1   9  24 
    #   1   9  10   1   1  10  13  10   2   1   2  18   2 
    #   4   8  20   2   5   1 
	banCode <- lapply( cvSeqNextLst, function(p){ 
						if( 0<nrow(p$fndMtx) ) (p$fndMtx[1,2]-p$fndMtx[1,1]) else integer(0)
					})
	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aCode ){
					cnt <- 0
					for( cIdx in banCode.span ){
						if( banCode[cIdx]==(aCode[cIdx+1]-aCode[cIdx]) ){
							cnt <- cnt+1
						}
					}
					return( cnt<2 )
				})	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid[2:6]-aZoid[1:5]
					cnt <- 0
					cnt <- ifelse( aCode[1]==5 ,cnt+1 ,cnt )	# 5(?) ,21 ,5 ,x ,5
					cnt <- ifelse( aCode[3]==8 ,cnt+1 ,cnt )	# 1+9=10 , 8(?)+1=9
					return( cnt<2 )
				})	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid[2:6]-aZoid[1:5]
					if( all(aCode[1:4]==c( 5, 8, 9, 8)) ) return( FALSE )
					if( all(aCode[1:4]==c( 3, 2,10,20)) ) return( FALSE )
					if( all(aCode[1:4]==c( 5, 3, 1, 2)) ) return( FALSE )
					if( all(aCode[1:4]==c( 4, 5, 1, 5)) ) return( FALSE )
					if( all(aCode[1:4]==c( 3, 2,10, 1)) ) return( FALSE )
					return( TRUE )
				})	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# =========================================================
	# colValSeqNext( ,pColSize=3 )
	cvSeqNextLst <- colValSeqNext( gEnv$zhF ,pColSize=3 )
	#	[1]  3  8 16
	#	[2]  1  3 21	
	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid%%10
					if( all(aCode[1:3]==c( 3, 8, 6)) ) return( FALSE )
					if( all(aCode[2:4]==c( 1, 3, 1)) ) return( FALSE )
					return( TRUE )
				})	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	return( allIdxF )

} # fCut.colValSeqNext()

fCut.colValSeqNext.fStep <- function( gEnv ,allIdxF ){

	lastZoid <- gEnv$zhF[nrow(gEnv$zhF),]
	getAcodeMtx <- function( pAllIdxF ){
		aCodeMtx <- t(apply(gEnv$allZoidMtx[pAllIdxF,] ,1 ,function(aZoid){aZoid-lastZoid}))
		return( aCodeMtx )
	} # getAcodeMtx()

	stdCodeMtx <- gEnv$zhF[2:nrow(gEnv$zhF),] - gEnv$zhF[1:(nrow(gEnv$zhF)-1),]
	

	# =========================================================
	# anaColEndPtn()
	colPtnLst <- anaColEndPtn( stdCodeMtx ,pDebug=T )
	# 15   6 -11   6  12   4  -9   5 -12   1
	#  8   3   5  -5  24  -3   7   5
	#  7   4   5
	# -5   2  -5  14   3   9   1 -14   7 -17
	# 15
	# -4 -13   8   0  10   1   3  11   0   0 -12


	# =========================================================
	# colValSeqNext( ,pColSize=2 )
	cvSeqNextLst <- colValSeqNext( stdCodeMtx ,pColSize=2 )

	# =========================================================
	# colValSeqNext( ,pColSize=3 )
	cvSeqNextLst <- colValSeqNext( stdCodeMtx ,pColSize=3 )
	#	found NONE. -_-;

	return( allIdxF )

} # fCut.colValSeqNext.fStep( )

finalFlt.cust <- function( gEnv ,allIdxF ){
	
	lastZoid <- gEnv$zhF[nrow(gEnv$zhF),]

	fltCnt <- rep( 0 ,length(allIdxF) )

	# Quo
	zQuo <- lastZoid %/% 10
	flag <- apply( gEnv$allZoidMtx[allIdxF,]%/%10 ,1 ,function(aCode){ any(aCode!=zQuo) } )
	kIdx <- head(which(flag))
	fltCnt[!flag] <- 1+fltCnt[!flag]

	# zWidth : 38  37  36(?)
	flag <- !( (gEnv$allZoidMtx[allIdxF,6]-gEnv$allZoidMtx[allIdxF,1]) %in% c(36,37) )
	kIdx <- head(which(!flag))
	fltCnt[!flag] <- 1+fltCnt[!flag]

	# --------------------------------------------
	# anaColEndPtn()
	colPtnLst <- anaColEndPtn( gEnv$zhF ,pDebug=F )
	# colPtnLst[[1]]$val  12 15 15  7  4 19  1
	# colPtnLst[[2]]$val  15  7  7 17
	# colPtnLst[[3]]$val  20  4
	# colPtnLst[[4]]$val  36 31 18
	# colPtnLst[[5]]$val  37 23 24 34 42 30 28
	# colPtnLst[[6]]$val  41 40 42 35 43 45 42 42 44 42 42

    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- ifelse( aZoid[1]==12 ,1 ,0 )
					cnt <- cnt + ifelse( aZoid[2]==15 ,1 ,0 )
					cnt <- cnt + ifelse( aZoid[6]==42 ,1 ,0 )
					return( cnt>0 )
				})	;kIdx<-head(which(!flag))
	fltCnt[!flag] <- 1+fltCnt[!flag]

	# --------------------------------------------
	# colValSeqNext()
	cvSeqNextLst <- colValSeqNext( gEnv$zhF ,pColSize=2 )
	#	hold point
	#		cvSeqNextLst[[1]] zoid[1:2] - 13 21 ( 8,17  21,25 )
	#		cvSeqNextLst[[2]] zoid[2:3] -  7 12 (11,20   9,27 )
	#		cvSeqNextLst[[3]] zoid[3:4] - 18 19 (18,28  15,18)
	#		cvSeqNextLst[[4]] zoid[4:5] - 18 24
	#		cvSeqNextLst[[5]] zoid[5:6] - 18 22 (33,44)
	step <- sapply( cvSeqNextLst ,function(p){ ifelse(0<nrow(p$fndMtx),p$fndMtx[,2]-p$fndMtx[,1],-1) })
    cnt <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					for( idx in 1:5 ){
						if( step[idx] == (aZoid[idx+1]-aZoid[idx]) ){
							cnt <- cnt+1
						}
					}
					return( cnt )
				})	
	flag <- cnt<1 ;kIdx<-head(which(!flag))
	fltCnt[!flag] <- 1+fltCnt[!flag]

	banValLst <- lapply( cvSeqNextLst ,function(p){ if(0<nrow(p$fndMtx)) p$fndMtx[1,]%%10 else c(-1,-1)  })
    cnt <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					for( idx in 1:5 ){
						if( all(banValLst[[idx]]==(aZoid[idx:(idx+1)]%%10)) ){
							cnt <- cnt+1
						}
					}
					return( cnt )
				})	
	flag <- cnt<1 ;kIdx<-head(which(!flag))
	fltCnt[!flag] <- 1+fltCnt[!flag]

	# cvSeqNextLst[[1]]$fndMtx
	flag <- gEnv$allZoidMtx[allIdxF,2]!=8	;kIdx<-head(which(!flag))
	fltCnt[!flag] <- 1+fltCnt[!flag]

	# cvSeqNextLst[[2]]$fndMtx
	flag <- gEnv$allZoidMtx[allIdxF,2]!=20	;kIdx<-head(which(!flag))
	fltCnt[!flag] <- 1+fltCnt[!flag]
	flag <- gEnv$allZoidMtx[allIdxF,3]!=22	;kIdx<-head(which(!flag))
	fltCnt[!flag] <- 1+fltCnt[!flag]

	# cvSeqNextLst[[3]]$fndMtx
	flag <- (gEnv$allZoidMtx[allIdxF,3]%%10)!=8	;kIdx<-head(which(!flag))
	fltCnt[!flag] <- 1+fltCnt[!flag]
	flag <- (gEnv$allZoidMtx[allIdxF,3]%%10)!=9	;kIdx<-head(which(!flag))
	fltCnt[!flag] <- 1+fltCnt[!flag]
	flag <- (gEnv$allZoidMtx[allIdxF,3])!=19	;kIdx<-head(which(!flag))
	fltCnt[!flag] <- 1+fltCnt[!flag]
	flag <- apply( gEnv$allZoidMtx[allIdxF,3:4]%%10 ,1 ,function(aCode){ 
					return( !all(aCode==c(8,8)) )
				})	;kIdx<-head(which(!flag))
	fltCnt[!flag] <- 1+fltCnt[!flag]

	# cvSeqNextLst[[4]]$fndMtx
	# cvSeqNextLst[[5]]$fndMtx
	flag <- (gEnv$allZoidMtx[allIdxF,6])!=44	;kIdx<-head(which(!flag))
	fltCnt[!flag] <- 1+fltCnt[!flag]

	# cvSeqNextLst -> fStep
	banValLst <- lapply( cvSeqNextLst ,function( p ){
					if( 2 > nrow(p$fndMtx) ) {
						return( c(-1,-1) )
					} else {
						return( p$fndMtx[1,] + (p$fndMtx[1,]-p$fndMtx[2,]) )
					}
				})
	#	by Pattern
	cnt <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					cnt <- 0
					for( idx in 1:5 ){
						if( all(aZoid[idx:(idx+1)] == banValLst[[idx]]) ){
							cnt <- cnt + 1
						}
					}
					return( cnt )
				})
	flag <- cnt<1 ;kIdx<-head(which(!flag))
	fltCnt[!flag] <- 1+fltCnt[!flag]

	#	by Value
	cnt <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					cnt <- 0
					for( idx in 1:5 ){
						cnt <- cnt + sum(aZoid[idx:(idx+1)] == banValLst[[idx]])
					}
					return( cnt )
				})
	flag <- cnt<2 ;kIdx<-head(which(!flag))
	fltCnt[!flag] <- 1+fltCnt[!flag]

	rObj <- list( fltCnt=fltCnt )
	return( rObj )
} # finalFlt.cust()


testFunc <- function(){

	testSpan <- 400:nrow(gEnv$zhF)

	tEnv <- gEnv
	rstLst <- list()
	for( tIdx in testSpan ){
		tEnv$zhF <- gEnv$zhF[1:(tIdx-1),]
		tEnv$allZoidMtx <- rbind( gEnv$zhF[tIdx,,drop=F] ,c(1,11,21,31,41,42) )
		tAllIdxF <- 1:2
		uAnaLstGrp <- getUAnaLstGrp( tEnv ,tAllIdxF ,pDefaultCut=FALSE ,pReport=F )

		uAnaCutDataLst.c <- list()	# uAnaCutDataLst custom
		for( nIdx in attributes(uAnaLstGrp)$names ){	# nIdx <- "uAnaLst.rebCnt"
			uAnaLst <- uAnaLstGrp[[nIdx]]
			cutDataLst <- list()
			for( uIdx in seq_len(length(uAnaLst)) ){
				cutData <- list( )
				cutData$colVal		<- uAnaLst[[uIdx]]$uAnaCutData$colVal
				cutData$colVal.f	<- uAnaLst[[uIdx]]$uAnaCutData$colVal.f
				cutData$colVal.c	<- uAnaLst[[uIdx]]$uAnaCutData$colVal.c
				cutDataLst[[uAnaLst[[uIdx]]$idStr]] <- cutData
			} # uIdx
			uAnaCutDataLst.c[[nIdx]] <- cutDataLst
		} # nIdx
		# uAnaCutDataLst.c <- customizeCutData( uAnaCutDataLst.c )

		k.FLogStr(sprintf("=[tIdx:%d  %s]=====================================" 
							,tIdx ,paste(tEnv$allZoidMtx[1,],collapse=",") 
				))

		rObj <- banValScan.grp( pAllIdxF=tAllIdxF ,pBanLst=NULL ,grpIdx=1 ,pPhase="colVal" ,pLog=T ,gEnv=tEnv )
		rstLst[[1+length(rstLst)]] <- rObj
	}

	fltPos <- sapply( rstLst ,function(p){p$fltPos[1]})

} # testFunc()

