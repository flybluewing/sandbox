# to20180707_H.R 최종접근

fCut.hasPtn <- function( src ,tgt ){

	src.len <- length(src)	;tgt.len <- length(tgt)
	colSpan <- 1:src.len - 1
	for( cIdx in 1:(tgt.len-src.len+1) ){
		if( all(tgt[cIdx+colSpan]==src) ){
			return( TRUE )
		}
	}
	return( FALSE )

} # fCut.hasPtn()

fCut.customStatic <- function( gEnv ,allIdxF ){

	lastZoid <- gEnv$zhF[nrow(gEnv$zhF),]
	cStep <- lastZoid[2:6] - lastZoid[1:5]
	fStep <- lastZoid - gEnv$zhF[nrow(gEnv$zhF)-1,]

	# <recycle> 2개 이상 재현은 제외시키자.
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( 2>sum(aZoid %in% lastZoid) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # rem 동일 2 이상
    zRem <- lastZoid %% 10
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
                    cnt <- sum( zRem==(aZoid%%10) )
					return( cnt<2 )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# cStep 반복. 동일 재현이 2개 붙어서 발생.
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid[2:6] - aZoid[1:5]
					for( cIdx in 1:4 ){
						fnd <- fCut.hasPtn( cStep[cIdx+0:1] ,aCode )
						if( fnd ) return( FALSE )
					}
					return( TRUE )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# cStep 반복 전체갯수는 3개 이하.(2가 너무 많다보니 변경.)
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- sum( cStep==(aZoid[2:6]-aZoid[1:5]) )
					return( 3 > cnt )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	#
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					rem1 <- sum(aZoid%/%10 == 1)
					rem3 <- sum(aZoid%/%10 == 3)
					rem4 <- sum(aZoid%/%10 == 4)
					return( !(rem3==3&&rem1==1) && !(rem3==3&&rem4==2) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	#
	flag <- !(gEnv$allZoidMtx[allIdxF,6]%in%c(43,45))	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	#
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( 2 > sum(aZoid%%11==0) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	#
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					for( cIdx in 2:6 ){
						fVal <- aZoid[cIdx]-lastZoid[cIdx]
						cVal <- aZoid[cIdx]-aZoid[cIdx-1]
						if( fVal==1 && cVal==2 ){
							return( FALSE )
						}
					}
					return( TRUE )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	#
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- sum(aZoid%%11==0)+sum(aZoid%%10==0)
					return( cnt<2 )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	return( allIdxF )

} # fCut.customStatic()

fCutCnt.customCnt <- function( gEnv ,allIdxF ){

	fStepCstep.01 <- function( aZoid ,lastZoid ){
		fStep <- aZoid - lastZoid
		cStep <- aZoid[2:6] - aZoid[1:5]
		for( cIdx in 2:6 ){
			if( (fStep[cIdx]*2)==cStep[cIdx-1] ){
				return( FALSE )
			}
		}
		return( TRUE )
	} # fStepCstep.01()
	quoTbl.01 <- function( tbl ){
		# 1 3 2 # table( lastZoid%/%10 )
		if( 3==length(tbl) && all(tbl==c(1,3,2)) ){
			return( FALSE )
		}
		return( TRUE )
	}

	lastZoid <- gEnv$zhF[nrow(gEnv$zhF),]
	cStep <- lastZoid[2:6] - lastZoid[1:5]
	fStep <- lastZoid - gEnv$zhF[nrow(gEnv$zhF)-1,]

	flgCnt <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function(aZoid){
		cnt <- 0
		cnt <- ifelse( aZoid[1]%%10==1 ,cnt+1 ,cnt )	#  1,11,?
		cnt <- ifelse( aZoid[1]==3     ,cnt+1 ,cnt )	# 21,12,30,?
		cnt <- ifelse( !(fStepCstep.01(aZoid,lastZoid)),cnt+1 ,cnt )
		cnt <- ifelse(  1==(aZoid[4]-aZoid[3]) ,cnt+1 ,cnt )	# 2,2,1 ?
		cnt <- ifelse( 33==(aZoid[6]-aZoid[1]) ,cnt+1 ,cnt )	#
		cnt <- ifelse( 33==(aZoid[6]-aZoid[1]) ,cnt+1 ,cnt )	#
		cnt <- ifelse( 41==(aZoid[4]) ,cnt+1 ,cnt )	# 43,42,..
		cnt <- ifelse(  4==(aZoid[2]%%10) ,cnt+1 ,cnt )	# 14,34,...
		cnt <- ifelse( !quoTbl.01(table(aZoid%/%10)) ,cnt+1 ,cnt )
		cnt <- ifelse( any(aZoid==lastZoid) ,cnt+1 ,cnt )
	})

	return( flgCnt )
}

fCut.colValSeqNext <- function( gEnv ,allIdxF ){

	hasRow <- function( val ,mtx ){
		for( rIdx in 1:nrow(mtx) ){
			if( all(val==mtx[rIdx,]) ){
				return( TRUE )
			}
		}
		return( FALSE )
	}

	lastZoid <- gEnv$zhF[nrow(gEnv$zhF),]

	# =========================================================
	# anaColEndPtn()
	colPtnLst <- anaColEndPtn( gEnv$zhF ,pDebug=T )

	# <recycle>
	banVal <- sapply( colPtnLst ,function( p ){
					return( if(length(p$val)>0) p$val[1] else 0 )
				})
	banVal.idx <- which( banVal!=0 )
	banVal <- banVal[banVal.idx]

	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( !any(aZoid[banVal.idx]==banVal) )
				})	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# <recycle> conditional cnt < 1
	banVal.rem <- banVal%%10
	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid[banVal.idx] %% 10
					cnt <- sum( aCode==banVal.rem )
					return( cnt<2 )
				})	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	#
	flag <- !(gEnv$allZoidMtx[allIdxF,6]%in%c(41))	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	#
	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					cnt <- ifelse( aZoid[1]%in%c(2) ,cnt+1 ,cnt )	# (2) ,3 ,4
					cnt <- ifelse( aZoid[6]%in%c(39) ,cnt+1 ,cnt )
					cnt <- ifelse( any(aZoid[banVal.idx]%%10 == banVal.rem) ,cnt+1 ,cnt )
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

	# <recycle> colditional:cnt==0
	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F]%%10 ,1 ,function( aCode ){
					cnt <- 0
					for( cIdx in banCode.span ){
						fndCodeMtx <- cvSeqNextLst[[cIdx]]$fndMtx%%10
						fnd <- hasRow( aCode[cIdx:(cIdx+1)] ,fndCodeMtx )
						cnt <- ifelse( fnd ,cnt+1 ,cnt )
					}
					return( cnt<2 )
				})	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# <recycle> conditional:cnt<2
	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F]%%10 ,1 ,function( aCode ){
					cnt <- 0
					for( cIdx in banCode.span ){
						cnt <- cnt + sum(aCode[cIdx:(cIdx+1)]==banCodeLst[[cIdx]])
					}
					return( cnt<3 )
				})	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))


	#
	flag <- !(gEnv$allZoidMtx[allIdxF,2]%in%c(15))	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# 한 개 정도는 허용하는 게 recycle을 위해서는 적절할 듯.
    #	2 :  5  10  17   2   7   2  14   2   1 
    #	3 :  2  11   2   2   8   7   6   6   7   2  14  15  15   7 
    #	4 : 14   6   1  13 
    #	5 :  1  16  23   1   2  14   4 
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
					if( 2<sum(aCode[2:5]==c( 5, 2,14, 1)) ) return( FALSE )
					if( 2<sum(aCode[2:5]==c(10,11, 6,16)) ) return( FALSE )
					if( 2<sum(aCode[2:5]==c(17, 2, 1,23)) ) return( FALSE )
					if( 2<sum(aCode[2:5]==c( 2, 2,13, 1)) ) return( FALSE )
					return( TRUE )
				})	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# =========================================================
	# colValSeqNext( ,pColSize=3 )
	cvSeqNextLst <- colValSeqNext( gEnv$zhF ,pColSize=3 )
	#	[2]		
	#	15 20 31
	#	36 38 40
	#	conditional : 하나도 일치하는 거 없도록.
	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid%%10
					if( 1<sum(aCode[2:4]==c( 5, 0, 1)) ) return( FALSE )
					if( 1<sum(aCode[2:4]==c( 6, 8, 0)) ) return( FALSE )
					return( TRUE )
				})	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	return( allIdxF )

} # fCut.colValSeqNext()

fCutCnt.colValSeqNext <- function( gEnv ,allIdxF ){

	hasRow <- function( val ,mtx ){
		for( rIdx in 1:nrow(mtx) ){
			if( all(val==mtx[rIdx,]) ){
				return( TRUE )
			}
		}
		return( FALSE )
	}

	lastZoid <- gEnv$zhF[nrow(gEnv$zhF),]
	flgCnt <- rep( 0 ,length(allIdxF) )

	# =========================================================
	# anaColEndPtn()
	colPtnLst <- anaColEndPtn( gEnv$zhF ,pDebug=T )
	banVal <- sapply( colPtnLst ,function( p ){
					return( if(length(p$val)>0) p$val[1] else 0 )
				})
	banVal.idx <- which( banVal!=0 )
	banVal <- banVal[banVal.idx]

	#
	banVal.rem <- banVal%%10
	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					cnt <- ifelse( aZoid[1]%in%c(2) ,cnt+1 ,cnt )	# (2) ,3 ,4
					cnt <- ifelse( aZoid[6]%in%c(39) ,cnt+1 ,cnt )
					cnt <- ifelse( any(aZoid[banVal.idx]%%10 == banVal.rem) ,cnt+1 ,cnt )
					return( cnt<1 )
				})	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
	flgCnt[!flag] <- flgCnt[!flag] + 1
	cat(sprintf("flaged %d(%.1f%%)\n",sum(!flag),100*sum(!flag)/length(flag)))


	# =========================================================
	# colValSeqNext( ,pColSize=2 )
	cvSeqNextLst <- colValSeqNext( gEnv$zhF ,pColSize=2 )

	banCodeLst <- lapply( cvSeqNextLst, function(p){ 
						if( 0<nrow(p$fndMtx) ) p$fndMtx[1,]%%10 else integer(0)
					})
	banCode.span <- which( sapply( banCodeLst ,function(p){length(p)>0}) )

	# <recycle> colditional:cnt==0
	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F]%%10 ,1 ,function( aCode ){
					cnt <- 0
					for( cIdx in banCode.span ){
						fndCodeMtx <- cvSeqNextLst[[cIdx]]$fndMtx%%10
						fnd <- hasRow( aCode[cIdx:(cIdx+1)] ,fndCodeMtx )
						cnt <- ifelse( fnd ,cnt+1 ,cnt )
					}
					return( cnt==0 )
				})	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
	flgCnt[!flag] <- flgCnt[!flag] + 1
	cat(sprintf("flaged %d(%.1f%%)\n",sum(!flag),100*sum(!flag)/length(flag)))

	# <recycle> conditional:cnt<2
	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F]%%10 ,1 ,function( aCode ){
					cnt <- 0
					for( cIdx in banCode.span ){
						cnt <- cnt + sum(aCode[cIdx:(cIdx+1)]==banCodeLst[[cIdx]])
					}
					return( cnt<2 )
				})	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
	flgCnt[!flag] <- flgCnt[!flag] + 1
	cat(sprintf("flaged %d(%.1f%%)\n",sum(!flag),100*sum(!flag)/length(flag)))

	#
    #	2 :  5  10  17   2   7   2  14   2   1 
    #	3 :  2  11   2   2   8   7   6   6   7   2  14  15  15   7 
    #	4 : 14   6   1  13 
    #	5 :  1  16  23   1   2  14   4 
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
					return( cnt==0 )
				})	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
	flgCnt[!flag] <- flgCnt[!flag] + 1
	cat(sprintf("flaged %d(%.1f%%)\n",sum(!flag),100*sum(!flag)/length(flag)))

	#
	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid[2:6]-aZoid[1:5]
					cnt <- 0
					cnt <- ifelse( aCode[2]==11 ,cnt+1 ,cnt )	# 
					return( cnt<2 )
				})	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
	flgCnt[!flag] <- flgCnt[!flag] + 1
	cat(sprintf("flaged %d(%.1f%%)\n",sum(!flag),100*sum(!flag)/length(flag)))

	# =========================================================
	# colValSeqNext( ,pColSize=3 )
	cvSeqNextLst <- colValSeqNext( gEnv$zhF ,pColSize=3 )
	#	[2]		
	#	15 20 31
	#	36 38 40
	#	conditional : 하나도 일치하는 거 없도록.
	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid%%10
					return( !any(aCode[2:4]==c( 5, 0, 1)) )
				})	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
	flgCnt[!flag] <- flgCnt[!flag] + 1
	cat(sprintf("flaged %d(%.1f%%)\n",sum(!flag),100*sum(!flag)/length(flag)))

	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( !(aZoid[1]%%10==0) )
				})	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
	flgCnt[!flag] <- flgCnt[!flag] + 1
	cat(sprintf("flaged %d(%.1f%%)\n",sum(!flag),100*sum(!flag)/length(flag)))

	return( flgCnt )

} # fCutCnt.colValSeqNext()

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








# pastBanLst 에 해당하는 allIdxF 파트를 전달받고, 또 전달해주도록 해야 한다.
# global : uAnaCutDataLst.c ,uAnaLstGrp ,pPhase
pPhase <- "colVal"
grpIdx <- 2	;uIdx <- 1
pBanLst <- initValLst( uAnaCutDataLst.c[[grpIdx-1]][[uIdx]][[pPhase]] )
pAllIdxF <- allIdxF

# pAllIdxF=tAllIdxF;pBanLst=NULL;grpIdx=1;pPhase="colVal";pLog=F;gEnv=tEnv
# grpIdx.bak=grpIdx;uIdx.bak=uIdx;pBanLst.bak=pBanLst;pAllIdxF.bak=pAllIdxF;fltPos.bak=fltPos;banMtxLst.bak=banMtxLst
bak.1.1 <- list(grpIdx=grpIdx,uIdx=uIdx,pBanLst=pBanLst,pAllIdxF=pAllIdxF,fltPos=fltPos,banMtxLst=banMtxLst)
bak.1.2 <- list(grpIdx=grpIdx,uIdx=uIdx,pBanLst=pBanLst,pAllIdxF=pAllIdxF,fltPos=fltPos,banMtxLst=banMtxLst)

Obj <- bak.1.1
grpIdx=Obj$grpIdx;uIdx=Obj$uIdx;pBanLst=Obj$pBanLst;pAllIdxF=Obj$pAllIdxF;fltPos=Obj$fltPos;banMtxLst=Obj$banMtxLst

banValScan.grp <- function( pAllIdxF ,pBanLst=NULL ,grpIdx ,pPhase="colVal" ,pLog=F ,gEnv ) {

	initValLst <- function( banLst ){
		valLst <- lapply( banLst ,function(banMtx){
							val <- sort(unique(banMtx[,"banVal"]))
							tbl <- table(banMtx[,"banVal"])[as.character(val)]
							return( cbind(val,tbl) )
						})
		return( valLst )
	} # initValLst()
	mergeValLst <- function( valLst ,banLst ){
		valLst.work <- initValLst( banLst )
		valLst.f <- lapply( 1:length(valLst) ,function(vIdx){
						vMtx <- valLst[[vIdx]]
						vMtx.work <- valLst.work[[vIdx]]
						dupVal <- intersect( vMtx.work[,"val"] ,vMtx[,"val"] )
						if( 0<length(dupVal) ){
							vMtx[vMtx[,"val"]%in%dupVal ,"tbl"] <- 
								vMtx[vMtx[,"val"]%in%dupVal ,"tbl"] + vMtx.work[vMtx.work[,"val"]%in%dupVal ,"tbl"]
						}
						vMtx.f <- rbind( vMtx ,vMtx.work[!vMtx.work[,"val"]%in%vMtx[,"val"] ,] )
						return(vMtx.f)
					})
		return( valLst.f )
	} # mergeValLst()

	#==========================================================================
	logIndent <- paste(rep(" ",grpIdx) ,collapse="" )

	fltPos <- rep( 0 ,length(pAllIdxF) )	;names(fltPos) <- pAllIdxF
	if( grpIdx > length(uAnaCutDataLst.c) ){
		return( list(fltPos=fltPos) )
	}

	# sapply( uAnaLstGrp[[grpIdx]] ,function(p){p$idStr} )
	for( uIdx in seq_len( length(uAnaCutDataLst.c[[grpIdx]]) ) ){

		banMtxLst <- NULL
		if( is.null(pBanLst) ){
			banMtxLst <- initValLst( uAnaCutDataLst.c[[grpIdx]][[uIdx]][[pPhase]] )
		} else {
			banMtxLst <- mergeValLst( pBanLst ,uAnaCutDataLst.c[[grpIdx]][[uIdx]][[pPhase]] )
		}

		banValLst <- lapply( banMtxLst ,function(banMtx){
							if( 0==nrow(banMtx) ){ return( integer(0) )
							} else {
								return(banMtx[banMtx[,"tbl"]>1 ,"val"])
							}
						})

		# uIdx에 해당하는 pAllIdxF 부분 추출
		flag.u <- apply( gEnv$allZoidMtx[pAllIdxF,,drop=F] ,1 ,function(aZoid){
						uAnaLstGrp[[grpIdx]][[uIdx]]$isTarget( aZoid )
					})

		flag <- apply( gEnv$allZoidMtx[pAllIdxF[flag.u],,drop=F] ,1 ,function(aZoid){
						for( idx in 1:6 ){
							if( aZoid[idx]%in%banValLst[[idx]] ){
								return( TRUE )
							}
						}
						return( FALSE )
					})

		idx.target <- which(flag.u)
		if( length(idx.target[flag])>0 ){
			fltPos[idx.target[flag]] <- grpIdx
			#	cat(sprintf("uIdx:%d %d\n",uIdx,sum(fltPos!=0)))
		}

		if( pLog ){
			k.FLogStr(sprintf("%s grpIdx:%d uIdx:%d flag.u[1]:%d fltPos[1]:%d"
						,logIndent ,grpIdx ,uIdx ,flag.u[1] ,fltPos[1])
					)
		}


		# banValScan.unit 투입 및 적용.
		#	아직 필터링 안된 대상 추출.
		idx.undone <- idx.target[!flag]
		if( 0<length(idx.undone) ){
			fltPos.next <- banValScan.grp( pAllIdxF=pAllIdxF[idx.undone] ,pBanLst=banMtxLst ,grpIdx=grpIdx+1 ,pLog=pLog ,gEnv=gEnv )$fltPos
				# pAllIdxF=pAllIdxF[idx.undone] ;pBanLst=banMtxLst ;grpIdx=grpIdx+1
			fltPos[idx.undone] <- ifelse( fltPos[idx.undone]==0 ,fltPos.next ,fltPos[idx.undone] )
		}

	}

	rObj <- list( fltPos=fltPos )
	return( rObj )

} # banValScan()




#====================================================================================
#====================================================================================
#====================================================================================
uAnaLstGrp <- getUAnaLstGrp( gEnv ,allIdxF ,pDefaultCut=FALSE ,pReport=TRUE )

# uAnaLstGrp에서 uAnaCutData 수집.
uAnaCutDataLst.c <- list()	# uAnaCutDataLst custom
for( nIdx in attributes(uAnaLstGrp)$names ){	# nIdx <- "uAnaLst.rebCnt"
	uAnaLst <- uAnaLstGrp[[nIdx]]
	cutDataLst <- list()
	for( uIdx in 1:length(uAnaLst) ){
		cutData <- list( )
		cutData$colVal		<- uAnaLst[[uIdx]]$uAnaCutData$colVal
		cutData$colVal.f	<- uAnaLst[[uIdx]]$uAnaCutData$colVal.f
		cutData$colVal.c	<- uAnaLst[[uIdx]]$uAnaCutData$colVal.c
		cutDataLst[[uAnaLst[[uIdx]]$idStr]] <- cutData
	} # uIdx
	uAnaCutDataLst.c[[nIdx]] <- cutDataLst
} # nIdx

# 상황에 맞게 제외조건 추가. to2018nnnn_H.R에서 정의
customizeCutData <- function( uAnaCutDataLst.c ){ 
	return(uAnaCutDataLst.c) 
} # customizeCutData()
uAnaCutDataLst.c <- customizeCutData( uAnaCutDataLst.c )

	# "uAnaLst.rawData" "uAnaLst.rebCnt"  
	# "uAnaLst.colVal1" "uAnaLst.colVal3" "uAnaLst.colVal4" "uAnaLst.colVal6" 
	# "uAnaLst.nextZW"  "uAnaLst.zw" 
