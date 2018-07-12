# to20180707_H.R 최종접근

fCutU.hasPtn <- function( src ,tgt ){ # < official >

	src.len <- length(src)	;tgt.len <- length(tgt)
	colSpan <- 1:src.len - 1
	for( cIdx in 1:(tgt.len-src.len+1) ){
		if( all(tgt[cIdx+colSpan]==src) ){
			return( TRUE )
		}
	}
	return( FALSE )

} # fCut.hasPtn()

fCutU.hasRow <- function( val ,mtx ){ # < official >
	for( rIdx in 1:nrow(mtx) ){
		if( all(val==mtx[rIdx,]) ){
			return( TRUE )
		}
	}
	return( FALSE )
} # fCut.hasRow()

fCutU.seqRebCnt <- function( pZh ,pZoid ,pRowLen=10 ,pLen=2 ){ # < official >

	colLen <- ncol(pZh)
	zhRowLen <- nrow(pZh)

	rObj <- list( cnt=0 ,dbgLst=list() )
	for( cIdx in 1:(colLen-pLen+1) ){
		for( hIdx in (zhRowLen-0:(pRowLen-1)) ){
			fnd <- fCutU.hasPtn( pZoid[cIdx+0:(pLen-1)] ,pZh[hIdx,] )
			if( fnd ){
				rObj$dbgLst[[ 1+length(rObj$dbgLst) ]] <- c( cIdx ,hIdx )
				rObj$cnt <- 1 + rObj$cnt
			}
		}
	}
	return( rObj )

} # fCut.seqRebCnt()

fCutU.getNextZW <- function( gEnv ){ # < official >

	hLen <- nrow( gEnv$zhF )

	hZW <- gEnv$zhF[,6] - gEnv$zhF[,1]
	nextZW.idx <- which( hZW[1:(hLen-1)]==hZW[hLen] )+1
	
	rObj <- list( zMtx=gEnv$zhF[nextZW.idx,] ,hZW=hZW )
	return( rObj )

} # fCutU.getNextZW()

fCutU.getNextQuo10 <- function( gEnv ){ # < official >

	hLen <- nrow( gEnv$zhF )
	lastQuoPtn <- table( gEnv$zhF[hLen,]%/%10 )
	lastQuoPtn.len <- length(lastQuoPtn)
	flag <- apply( gEnv$zhF[1:(hLen-1),]%/%10 ,1 ,function( aCode ){
					tbl <- table(aCode)
					if( length(tbl)!=lastQuoPtn.len ) return( FALSE )

					return( all(tbl==lastQuoPtn) )
				})
	indices <- which( flag )+1

	rObj <- list( zMtx=gEnv$zhF[indices,] ,indices=indices )
	return( rObj )

} # fCutU.getNextQuo10()


fCutU.getRebNum <- function( gEnv ,rebNum=0 ){ # < official >

	hLen <- nrow( gEnv$zhF )

	rebCnt <- sapply( 2:hLen ,function( hIdx ){ sum(gEnv$zhF[(hIdx-1),] %in% gEnv$zhF[hIdx,]) })
	rebCnt <- c( 0 ,rebCnt )
	flag.idx <- which( rebCnt==rebNum )

	rObj <- list( zMtx=gEnv$zhF[flag.idx,] ,rebCnt=rebCnt )
	return( rObj )

} # fCutU.getNextZW()

fCutU.hist.banValScan.grp <- function( gEnv ){ # < official >

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

	return( list(fltPos=fltPos ,rstLst=rstLst) )
} # hist.banValScan.grp()



# done
fCut.customStatic <- function( gEnv ,allIdxF ){ # < official >

	#	809  6 11 15 17 23 40
	#   810  5 10 13 21 39 43
	#	811  8 11 19 21 36 45
    #   812  1  3 12 14 16 43
	#	813 11 30 34 35 42 44
	#	814  2 21 28 38 42 45

	lastZoid <- gEnv$zhF[nrow(gEnv$zhF),]
	cStep <- lastZoid[2:6] - lastZoid[1:5]
	fStep <- lastZoid - gEnv$zhF[nrow(gEnv$zhF)-1,]

	#
	flag <- gEnv$allZoidMtx[allIdxF,1]<10	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# <recycle> 2개 이상 재현은 제외시키자.
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( 2>sum(aZoid %in% lastZoid) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# 동일 컬럼에 같은 값 재현은 제외.
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( !any(aZoid==lastZoid) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# zoid[2] 에서 quo3가 나온적은 15/814
	flag <- gEnv$allZoidMtx[allIdxF,2]<30	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
	cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # rem 동일 3이상
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
                    cnt <- max( table(aZoid%%10) )
					return( cnt<3 )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # rem 동일값 연속
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
                    aCode <- aZoid%%10
					return( !any(aCode[2:6]==aCode[1:5]) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # rem 재현 2 이상
    zRem <- lastZoid %% 10
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
                    cnt <- sum( zRem==(aZoid%%10) )
					return( cnt<2 )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # rem ptn 재현 3이상
	zRem <- lastZoid %% 10
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid%%10
					for( cIdx in 1:4 ){
						fnd <- fCutU.hasPtn( zRem[cIdx+0:2] ,aCode )
						if( fnd ) return( FALSE )
					}
					return( TRUE )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # rem ptn 재현 4이상 - hIdx-1 에서.
	zRem <- gEnv$zhF[nrow(gEnv$zhF)-1,] %% 10
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid%%10
					for( cIdx in 1:3 ){
						fnd <- fCutU.hasPtn( zRem[cIdx+0:3] ,aCode )
						if( fnd ) return( FALSE )
					}
					return( TRUE )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # rem ptn 2,4 발생 부정.( h812 ,h813 )
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid%%10
					for( cIdx in 1:5 ){
						fnd <- fCutU.hasPtn( c(2,4) ,aCode )
						if( fnd ) return( FALSE )
					}
					return( TRUE )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# cStep 반복. 동일 재현이 2개 붙어서 발생.
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid[2:6] - aZoid[1:5]
					for( cIdx in 1:4 ){
						fnd <- fCutU.hasPtn( cStep[cIdx+0:1] ,aCode )
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

	# cStep 2,4,6,8 같은 식의 증가 감소(11% 정도라 불안하긴 한데..)
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid[2:6]-aZoid[1:5]
					for( cIdx in 2:4 ){
						if( (aCode[cIdx]-aCode[cIdx-1])==(aCode[cIdx+1]-aCode[cIdx]) ){
							return( FALSE )
						}
					}
					return( TRUE )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# 2, 4, 8, 16, 32 처럼 배수가 나열되는 거 피하자.
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					for( cIdx in 2:5 ){
						fstF <- aZoid[cIdx]==(aZoid[cIdx-1]*2)
						sndF <- aZoid[cIdx+1]==(aZoid[cIdx]*2)
						if( fstF&&sndF ) return( FALSE )

						fstF <- aZoid[cIdx]==(aZoid[cIdx-1]*3)
						sndF <- aZoid[cIdx+1]==(aZoid[cIdx]*3)
						if( fstF&&sndF ) return( FALSE )
					}
					return( TRUE )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))


	# 동일한 값이 연속나열되는 건 빼자.
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid[2:6] - aZoid[1:5]
					return( !any(aCode[1:4]==aCode[2:5]) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# rem 2개 짜리가 2번 존재는..  2 21 28 38 42 45 (2*2, 8*2)
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					tbl <- table(aZoid%%10)
					return( 2>sum(tbl==2) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# cStep 19 연속발생 피하자.( 11,30 2,21 )
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid[2:6]-aZoid[1:5]
					return( aCode[1]!=19 )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# h813(11 30 34 35 42 44) 에서의 Quo3 값 재현가능성은..
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quo3 <- aZoid[ (aZoid%/%10)==3 ]
					if( 3!=length(quo3) ) return( TRUE )
					return( !all(quo3==c(30,34,35)) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# 45, x, 44, x, 43(?)
	flag <- !(gEnv$allZoidMtx[allIdxF,6]%in%c(43))	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))


	return( allIdxF )

} # fCut.customStatic()

# done
fCutCnt.customCnt <- function( gEnv ,allIdxF ){ # < official >
	# 혼동을 피하기 위해, 필터 함수들은 살아남는 Zoid를 TRUE로 반환 유지
	quo.01 <- function( aZoid ){
		quo <- aZoid %/% 10
		idx <- which(quo==3)
		if( 1 != length(idx) ) return( TRUE )
		return( aZoid[idx]!=38 )
	}
	quo.02 <- function( aZoid ,stdQuo ){
		tbl <- table( aZoid%/% 10 )
		if( length(stdQuo)!=length(tbl) ) return(TRUE)
		return( !all(stdQuo==tbl) )
	}

	lastZoid <- gEnv$zhF[nrow(gEnv$zhF),]
	cStep <- lastZoid[2:6] - lastZoid[1:5]
	fStep <- lastZoid - gEnv$zhF[nrow(gEnv$zhF)-1,]
	stdQuo <- table(lastZoid %/% 10)
	stdZw <- lastZoid[6] - lastZoid[1]

	flgCnt <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function(aZoid){
		cnt <- 0
		cnt <- ifelse( aZoid[1]%%10==1 ,cnt+1 ,cnt )	#  14,34,21, 1?
		cnt <- ifelse( aZoid[1]%%10==2 ,cnt+1 ,cnt )	#   1,11, 2,x2?
		cnt <- ifelse( aZoid[3]%%10==1 ,cnt+1 ,cnt )	#  11,21,x1?

		cnt <- ifelse( aZoid[3]%%10==1 ,cnt+1 ,cnt )	#  11,21,x1?

		cnt <- ifelse( !quo.01(aZoid) ,cnt+1 ,cnt )	#  30 대역이 하나뿐이고 값도 38
		cnt <- ifelse( !quo.02(aZoid,stdQuo) ,cnt+1 ,cnt )	#  quo 영역 크기 동일.(영역대는 무시)

		cnt <- ifelse( stdZw==(aZoid[6]-aZoid[1]) ,cnt+1 ,cnt )	#  zoid width

		rstObj <- fCutU.seqRebCnt( gEnv$zhF ,aZoid ,pRowLen=50 ,pLen=3 )
		cnt <- ifelse( 0<rstObj$cnt ,cnt+1 ,cnt )

		rstObj <- fCutU.seqRebCnt( gEnv$zhF ,aZoid ,pRowLen=10 ,pLen=2 )
		cnt <- ifelse( 0<rstObj$cnt ,cnt+1 ,cnt )
	})

	return( flgCnt )
}

# done
fCut.colValSeqNext <- function( gEnv ,allIdxF ){ # < official >

	lastZoid <- gEnv$zhF[nrow(gEnv$zhF),]

	# =========================================================
	# anaColEndPtn()
	colPtnLst <- anaColEndPtn( gEnv$zhF ,pDebug=T )

	# [1]	 4 5 5
	# [2]	NA
	# [3]	12 13
	# [4]	39 43 34 39 39 41 45
	# [5]	40 26 42 35 36 40 42
	# [6]	43 44 45 43 37 43 38 37 33 43 42 35

	# <recycle>
	banVal <- sapply( colPtnLst ,function( p ){
					return( if(length(p$val)>0) p$val[1] else 0 )
				})
	banVal.idx <- which( banVal!=0 )
	banVal <- banVal[banVal.idx]

	# <recycle> conditional 0==sum()
	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( 2>sum(aZoid[banVal.idx]==banVal) )
				})	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# zoid[c(4,6)] : 과거 39,43 패턴발생 다음 패턴은 34,45 이었다.
	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( !all(aZoid[c(4,6)]==c(34,45)) )
				})	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# zoid[c(5,6)] : 과거 40,43 패턴발생 다음 패턴은 36,37 이었다.
	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( !all(aZoid[5:6]==c(36,37)) )
				})	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# conditional : aZoid[c(4,5)] 가 c(34,36) 인 거 제외.

	# conditional : (34,42,xx)
	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( !all(aZoid[4:6]==c(34,42,45)) )
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

	# <recycle> remPtn. colditional:cnt==0
	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F]%%10 ,1 ,function( aCode ){
					cnt <- 0
					for( cIdx in banCode.span ){
						fndCodeMtx <- cvSeqNextLst[[cIdx]]$fndMtx%%10
						fnd <- fCutU.hasRow( aCode[cIdx:(cIdx+1)] ,fndCodeMtx )
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


	# 한 개 정도는 허용하는 게 recycle을 위해서는 적절할 듯.
    #	2 : 13   7   2 
    #	3 :  4   1   3   6  15 
    #	4 : 12   1   3   9   7   6   3   7 
    #	5 : 17  15   5   5  11   6   7   4   3   4   3   4   6   5 
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
					if( 2<sum(aCode[2:5]==c( 7, 1, 1,15)) ) return( FALSE )
					if( 2<sum(aCode[2:5]==c( 2, 3, 3, 5)) ) return( FALSE )

					if( 2<sum(aCode[3:5]==c(    6, 9, 5)) ) return( FALSE )
					if( 2<sum(aCode[3:5]==c(   15, 7,11)) ) return( FALSE )
					return( TRUE )
				})	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# =========================================================
	# colValSeqNext( ,pColSize=3 )
	cvSeqNextLst <- colValSeqNext( gEnv$zhF ,pColSize=3 )
	#	[3]			[4]
	#	29 33 34	21 30 37
	#	27 42 45
	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid%%10
					if( 1<sum(aCode[3:5]==c( 9, 3, 4)) ) return( FALSE )
					if( 1<sum(aCode[3:5]==c( 7, 2, 5)) ) return( FALSE )
					return( TRUE )
				})	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	#	conditional : grp3 하나도 일치하는 거 없도록.
	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					if( 1<sum(aZoid[3:5]==c(29,33,34)) ) return( FALSE )
					return( TRUE )
				})	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	#	conditional : grp4 하나도 일치하는 거 없도록.
	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					if( 1<sum(aZoid[4:6]==c(21,30,37)) ) return( FALSE )
					return( TRUE )
				})	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	#	conditional : grp4 rem 두개 이상 일치

	return( allIdxF )

} # fCut.colValSeqNext()

# done
fCutCnt.colValSeqNext <- function( gEnv ,allIdxF ){ # < official >

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

	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( !(aZoid[1] %in% c(3,4)) )
				})	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
	flgCnt[!flag] <- flgCnt[!flag] + 1
	cat(sprintf("flaged %d(%.1f%%)\n",sum(!flag),100*sum(!flag)/length(flag)))

	# <recycle> conditional 0==sum()
	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( 0==sum(aZoid[c(3,5,6)]==c(12,40,43)) )
				})	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
	flgCnt[!flag] <- flgCnt[!flag] + 1
	cat(sprintf("flaged %d(%.1f%%)\n",sum(!flag),100*sum(!flag)/length(flag)))

	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( !all(aZoid[c(4,5)]==c(34,36)) )
				})	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
	flgCnt[!flag] <- flgCnt[!flag] + 1
	cat(sprintf("flaged %d(%.1f%%)\n",sum(!flag),100*sum(!flag)/length(flag)))

	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( !all(aZoid[4:5]==c(34,42)) )
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
						fnd <- fCutU.hasRow( aCode[cIdx:(cIdx+1)] ,fndCodeMtx )
						cnt <- ifelse( fnd ,cnt+1 ,cnt )
					}
					return( cnt==0 )
				})	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
	flgCnt[!flag] <- flgCnt[!flag] + 1
	cat(sprintf("flaged %d(%.1f%%)\n",sum(!flag),100*sum(!flag)/length(flag)))

	#
	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F]%%10 ,1 ,function( aCode ){
					cnt <- 0
					for( cIdx in banCode.span ){
						cnt <- cnt + sum(aCode[cIdx:(cIdx+1)]==banCodeLst[[cIdx]])
					}
					return( cnt<2 )
				})	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
	flgCnt[!flag] <- flgCnt[!flag] + 1
	cat(sprintf("flaged %d(%.1f%%)\n",sum(!flag),100*sum(!flag)/length(flag)))

    #	2 : 13   7   2 
    #	3 :  4   1   3   6  15 
    #	4 : 12   1   3   9   7   6   3   7 
    #	5 : 17  15   5   5  11   6   7   4   3   4   3   4   6   5 
	banCode <- lapply( cvSeqNextLst, function(p){ 
						if( 0<nrow(p$fndMtx) ) (p$fndMtx[1,2]-p$fndMtx[1,1]) else integer(0)
					})
	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid[2:6]-aZoid[1:5]
					if( all(aCode[4:5]==c( 6, 6)) ) return( FALSE )
					if( all(aCode[4:5]==c( 3, 7)) ) return( FALSE )
					if( all(aCode[4:5]==c( 7, 4)) ) return( FALSE )
					return( TRUE )
				})	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
	flgCnt[!flag] <- flgCnt[!flag] + 1
	cat(sprintf("flaged %d(%.1f%%)\n",sum(!flag),100*sum(!flag)/length(flag)))

	# =========================================================
	# colValSeqNext( ,pColSize=3 )
	cvSeqNextLst <- colValSeqNext( gEnv$zhF ,pColSize=3 )
	#	[3]			[4]
	#	29 33 34	21 30 37
	#	27 42 45

	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					if( any(aZoid[3:5]==c(29,33,34)) ) return( FALSE )
					return( TRUE )
				})	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
	flgCnt[!flag] <- flgCnt[!flag] + 1
	cat(sprintf("flaged %d(%.1f%%)\n",sum(!flag),100*sum(!flag)/length(flag)))

	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					if( any(aZoid[4:6]==c(21,30,37)) ) return( FALSE )
					return( TRUE )
				})	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
	flgCnt[!flag] <- flgCnt[!flag] + 1
	cat(sprintf("flaged %d(%.1f%%)\n",sum(!flag),100*sum(!flag)/length(flag)))

	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid %% 10
					if( 2<=sum(aCode[4:6]==c(1,0,7)) ) return( FALSE )
					return( TRUE )
				})	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
	flgCnt[!flag] <- flgCnt[!flag] + 1
	cat(sprintf("flaged %d(%.1f%%)\n",sum(!flag),100*sum(!flag)/length(flag)))

	return( flgCnt )

} # fCutCnt.colValSeqNext()

# done
fCut.colValSeqNext.cStep <- function( gEnv ,allIdxF ){ # < official >

	lastZoid <- gEnv$zhF[nrow(gEnv$zhF),]
	getAcodeMtx <- function( pAllIdxF ){
		aCodeMtx <- gEnv$allZoidMtx[pAllIdxF,2:6,drop=F]-gEnv$allZoidMtx[pAllIdxF,1:5,drop=F]
		return( aCodeMtx )
	} # getAcodeMtx()

	stdCodeMtx <- gEnv$zhF[,2:6]-gEnv$zhF[,1:5]

	# =========================================================
	# anaColEndPtn()
	colPtnLst <- anaColEndPtn( stdCodeMtx ,pDebug=T )
	banVal <- sapply( colPtnLst ,function( p ){
					return( if(length(p$val)>0) p$val[1] else 0 )
				})
	banVal.idx <- which( banVal!=0 )
	banVal <- banVal[banVal.idx]

	# <recycle> conditional 0==sum()
	banVal.rem <- banVal %% 10
	flag <- apply( getAcodeMtx(allIdxF) ,1 ,function( aCode ){
					aCode <- aCode %% 10
					return( 2>sum(aCode[banVal.idx]==banVal.rem) )
				})	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# conditional : 2<=sum()
	flag <- apply( getAcodeMtx(allIdxF) ,1 ,function( aCode ){
					if( 2<sum(aCode[2:5]==c(18, 6,11,30)) ) return( FALSE )
					if( 2<sum(aCode[2:5]==c( 7, 1, 5, 1)) ) return( FALSE )
					if( 2<sum(aCode[2:5]==c(23, 1, 3,10)) ) return( FALSE )
					if( 2<sum(aCode[2:5]==c( 4, 8,19, 3)) ) return( FALSE )
					if( 2<sum(aCode[2:5]==c( 3, 1, 9, 5)) ) return( FALSE )
					if( 2<sum(aCode[2:5]==c(14,15,12, 8)) ) return( FALSE )
					if( 2<sum(aCode[2:5]==c( 7,14,11, 5)) ) return( FALSE )
					if( 2<sum(aCode[2:5]==c( 7, 6, 1, 7)) ) return( FALSE )
					if( 2<sum(aCode[2:5]==c( 8,14,13, 3)) ) return( FALSE )
					if( 2<sum(aCode[2:5]==c( 1, 2, 3,11)) ) return( FALSE )
					if( 2<sum(aCode[2:5]==c(12, 7,12, 1)) ) return( FALSE )
					if( 2<sum(aCode[2:5]==c( 4, 4, 6,10)) ) return( FALSE )
					if( 2<sum(aCode[2:5]==c( 3, 7, 7, 1)) ) return( FALSE )
					if( 2<sum(aCode[2:5]==c(11,15, 4,15)) ) return( FALSE )
					if( 2<sum(aCode[2:5]==c( 8, 2, 8, 7)) ) return( FALSE )
					if( 2<sum(aCode[2:5]==c( 7, 2, 1, 3)) ) return( FALSE )
					return( TRUE )
				})	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# conditional : 2<=sum()
	flag <- apply( getAcodeMtx(allIdxF) ,1 ,function( aCode ){
					if( all(aCode[c(2,4,5)]==c( 6, 1, 8)) ) return( FALSE )
					if( all(aCode[c(2,4,5)]==c( 7, 9, 7)) ) return( FALSE )
					if( all(aCode[c(2,4,5)]==c( 2,14, 5)) ) return( FALSE )
					if( all(aCode[c(2,4,5)]==c( 3, 1, 3)) ) return( FALSE )
					if( all(aCode[c(2,4,5)]==c( 2, 4,16)) ) return( FALSE )
					return( TRUE )
				})	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# =========================================================
	# colValSeqNext( ,pColSize=2 )
	cvSeqNextLst <- colValSeqNext( stdCodeMtx ,pColSize=2 )
	#sameRow
	#	(X)		(X)		(O)		(O)		
	#	 2  3	23  1	 8  3	24  3
	banValLst <- lapply( cvSeqNextLst, function(p){ 
						if( 0<nrow(p$fndMtx) ) p$fndMtx[1,] else integer(0)
					})
	banCode.span <- which( sapply( banValLst ,function(p){length(p)>0}) )

	# conditional 1>cnt
	aCodeMtx <- getAcodeMtx( allIdxF )
    flag <- apply( aCodeMtx ,1 ,function( aCode ){
					cnt <- 0
					for( cIdx in banCode.span ){
						if( all(banValLst[[cIdx]]==aCode[cIdx+0:1]) ){
							cnt <- cnt + 1
						}
					}
					return( cnt<2 )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	aCodeMtx <- getAcodeMtx( allIdxF )
    flag <- apply( aCodeMtx ,1 ,function( aCode ){
					cnt <- 0
					for( cIdx in banCode.span ){
						mCnt <- sum(banValLst[[cIdx]]==aCode[cIdx+0:1])
						if( mCnt==1 ){
							cnt <- cnt+1
						}
					}
					return( cnt<3 )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# conditional : depth 3 까지는 매치.

	# =========================================================
	# colValSeqNext( ,pColSize=3 )
	cvSeqNextLst <- colValSeqNext( stdCodeMtx ,pColSize=3 )
	banValLst <- lapply( cvSeqNextLst, function(p){ 
						if( 0<nrow(p$fndMtx) ) p$fndMtx[1,] else integer(0)
					})
	banCode.span <- which( sapply( banValLst ,function(p){length(p)>0}) )

	aCodeMtx <- getAcodeMtx( allIdxF )
    flag <- apply( aCodeMtx ,1 ,function( aCode ){
					return( 2>sum(aCode[3:5]==c( 5,13, 1)) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	return( allIdxF )

} # fCut.colValSeqNext.cStep( )
# done
fCutCnt.colValSeqNext.cStep <- function( gEnv ,allIdxF ){ # < official >

	lastZoid <- gEnv$zhF[nrow(gEnv$zhF),]
	flgCnt <- rep( 0 ,length(allIdxF) )

	getAcodeMtx <- function( pAllIdxF ){
		aCodeMtx <- gEnv$allZoidMtx[pAllIdxF,2:6,drop=F]-gEnv$allZoidMtx[pAllIdxF,1:5,drop=F]
		return( aCodeMtx )
	} # getAcodeMtx()

	stdCodeMtx <- gEnv$zhF[,2:6]-gEnv$zhF[,1:5]
	aCodeMtx <- getAcodeMtx( allIdxF )

	# =========================================================
	# anaColEndPtn()
	colPtnLst <- anaColEndPtn( stdCodeMtx ,pDebug=T )
	banVal <- sapply( colPtnLst ,function( p ){
					return( if(length(p$val)>0) p$val[1] else 0 )
				})
	banVal.idx <- which( banVal!=0 )
	banVal <- banVal[banVal.idx]
	# [1] NA
	# [2] 10 18  7 23  4  3 14  7  7  8  1 12  4  3 11  8  7  6  7  2  3  2  6  6
	# [3]  4  6  1  1  8  1 15 14  6 14  2  7  4  7 15  2  2
	# [4]  1 11  5  3 19  9 12 11  1 13  3 12  6  7  4  8  1  1  9 14  1  4
	# [5]  1 30  1 10  3  5  8  5  7  3 11  1 10  1 15  7  3  8  7  5  3 16  6  3 

	flag <- apply( getAcodeMtx(allIdxF) ,1 ,function( aCode ){
					return( 0==sum(aCode[banVal.idx]==banVal) )
				})	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
	flgCnt[!flag] <- flgCnt[!flag] + 1
	cat(sprintf("flaged %d(%.1f%%)\n",sum(!flag),100*sum(!flag)/length(flag)))

	# conditional : 1<sum()
	flag <- apply( getAcodeMtx(allIdxF) ,1 ,function( aCode ){
					if( 1<sum(aCode[2:5]==c(18, 6,11,30)) ) return( FALSE )
					if( 1<sum(aCode[2:5]==c( 7, 1, 5, 1)) ) return( FALSE )
					if( 1<sum(aCode[2:5]==c(23, 1, 3,10)) ) return( FALSE )
					if( 1<sum(aCode[2:5]==c( 4, 8,19, 3)) ) return( FALSE )
					if( 1<sum(aCode[2:5]==c( 3, 1, 9, 5)) ) return( FALSE )
					if( 1<sum(aCode[2:5]==c(14,15,12, 8)) ) return( FALSE )
					if( 1<sum(aCode[2:5]==c( 7,14,11, 5)) ) return( FALSE )
					if( 1<sum(aCode[2:5]==c( 7, 6, 1, 7)) ) return( FALSE )
					if( 1<sum(aCode[2:5]==c( 8,14,13, 3)) ) return( FALSE )
					if( 1<sum(aCode[2:5]==c( 1, 2, 3,11)) ) return( FALSE )
					if( 1<sum(aCode[2:5]==c(12, 7,12, 1)) ) return( FALSE )
					if( 1<sum(aCode[2:5]==c( 4, 4, 6,10)) ) return( FALSE )
					if( 1<sum(aCode[2:5]==c( 3, 7, 7, 1)) ) return( FALSE )
					if( 1<sum(aCode[2:5]==c(11,15, 4,15)) ) return( FALSE )
					if( 1<sum(aCode[2:5]==c( 8, 2, 8, 7)) ) return( FALSE )
					if( 1<sum(aCode[2:5]==c( 7, 2, 1, 3)) ) return( FALSE )
					return( TRUE )
				})	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
	flgCnt[!flag] <- flgCnt[!flag] + 1
	cat(sprintf("flaged %d(%.1f%%)\n",sum(!flag),100*sum(!flag)/length(flag)))

	flag <- apply( getAcodeMtx(allIdxF) ,1 ,function( aCode ){
					if( 1<sum(aCode[c(2,4,5)]==c( 6, 1, 8)) ) return( FALSE )
					if( 1<sum(aCode[c(2,4,5)]==c( 7, 9, 7)) ) return( FALSE )
					if( 1<sum(aCode[c(2,4,5)]==c( 2,14, 5)) ) return( FALSE )
					if( 1<sum(aCode[c(2,4,5)]==c( 3, 1, 3)) ) return( FALSE )
					if( 1<sum(aCode[c(2,4,5)]==c( 2, 4,16)) ) return( FALSE )
					return( TRUE )
				})	;kIdx <- c(head(which(!flag)),tail(which(!flag)))
	flgCnt[!flag] <- flgCnt[!flag] + 1
	cat(sprintf("flaged %d(%.1f%%)\n",sum(!flag),100*sum(!flag)/length(flag)))

	# =========================================================
	# colValSeqNext( ,pColSize=2 )
	cvSeqNextLst <- colValSeqNext( stdCodeMtx ,pColSize=2 )
	#	sameRow
	#	(X)		(X)		(O)		(O)		
	#	 2  3	23  1	 8  3	24  3
	#	 6  7	14  1	 1  7	10  9
	#			 7 12	 6  8	 8  3
	#			 7  5	 4  1	17  5
	banValLst <- lapply( cvSeqNextLst, function(p){ 
						if( 0<nrow(p$fndMtx) ) p$fndMtx[1,] else integer(0)
					})
	banCode.span <- which( sapply( banValLst ,function(p){length(p)>0}) )
	# 
	# conditional 1>cnt
	aCodeMtx <- getAcodeMtx( allIdxF )
    flag <- apply( aCodeMtx ,1 ,function( aCode ){
					cnt <- 0
					for( cIdx in banCode.span ){
						if( all(banValLst[[cIdx]]==aCode[cIdx+0:1]) ){
							cnt <- cnt + 1
						}
					}
					return( cnt<1 )
				})	;kIdx<-head(which(!flag))
	flgCnt[!flag] <- flgCnt[!flag] + 1
	cat(sprintf("flaged %d(%.1f%%)\n",sum(!flag),100*sum(!flag)/length(flag)))

	aCodeMtx <- getAcodeMtx( allIdxF )
    flag <- apply( aCodeMtx ,1 ,function( aCode ){
					if( all(aCode[1:2]==c( 6, 7)) ) return( FALSE )
					if( all(aCode[2:3]==c(14, 1)) ) return( FALSE )
					if( all(aCode[2:3]==c( 7,12)) ) return( FALSE )
					if( all(aCode[2:3]==c( 7, 5)) ) return( FALSE )
					if( all(aCode[3:4]==c( 1, 7)) ) return( FALSE )
					if( all(aCode[3:4]==c( 6, 8)) ) return( FALSE )
					if( all(aCode[3:4]==c( 4, 1)) ) return( FALSE )
					if( all(aCode[4:5]==c(10, 9)) ) return( FALSE )
					if( all(aCode[4:5]==c( 8, 3)) ) return( FALSE )
					if( all(aCode[4:5]==c(17, 5)) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-head(which(!flag))
	flgCnt[!flag] <- flgCnt[!flag] + 1
	cat(sprintf("flaged %d(%.1f%%)\n",sum(!flag),100*sum(!flag)/length(flag)))

	# =========================================================
	# colValSeqNext( ,pColSize=3 )
	cvSeqNextLst <- colValSeqNext( stdCodeMtx ,pColSize=3 )
	#	[1]			[2]			[3]
	#	 NA NA NA	 NA NA NA	 5 13  1
	banValLst <- lapply( cvSeqNextLst, function(p){ 
						if( 0<nrow(p$fndMtx) ) p$fndMtx[1,] else integer(0)
					})
	banCode.span <- which( sapply( banValLst ,function(p){length(p)>0}) )
	# 

	return( flgCnt )

} # fCutCnt.colValSeqNext.cStep( )

fCut.colValSeqNext.fStep <- function( gEnv ,allIdxF ){ # < official >

	lastZoid <- gEnv$zhF[nrow(gEnv$zhF),]
	getAcodeMtx <- function( pAllIdxF ){
		aCodeMtx <- t(apply(gEnv$allZoidMtx[pAllIdxF,] ,1 ,function(aZoid){aZoid-lastZoid}))
		return( aCodeMtx )
	} # getAcodeMtx()

	stdCodeMtx <- gEnv$zhF[2:nrow(gEnv$zhF),] - gEnv$zhF[1:(nrow(gEnv$zhF)-1),]
	

	# =========================================================
	# anaColEndPtn()
	colPtnLst <- anaColEndPtn( stdCodeMtx ,pDebug=T )
	# [1]	-14  -5  -3
	# [2]	  5
	# [6]	  2  -7   2  -8  11   0 -13   0   4  -1   7  13   2
	# conditional

	# =========================================================
	# colValSeqNext( ,pColSize=2 )
	cvSeqNextLst <- colValSeqNext( stdCodeMtx ,pColSize=2 )
	# 걸린 게 아무것도 없음

	# =========================================================
	# colValSeqNext( ,pColSize=3 )
	cvSeqNextLst <- colValSeqNext( stdCodeMtx ,pColSize=3 )
	#	found NONE. -_-;

	return( allIdxF )

} # fCut.colValSeqNext.fStep( )

fCutCnt.colValSeqNext.fStep <- function( gEnv ,allIdxF ){ # < official >

	lastZoid <- gEnv$zhF[nrow(gEnv$zhF),]
	flgCnt <- rep( 0 ,length(allIdxF) )

	getAcodeMtx <- function( pAllIdxF ){
		aCodeMtx <- t(apply(gEnv$allZoidMtx[pAllIdxF,] ,1 ,function(aZoid){aZoid-lastZoid}))
		return( aCodeMtx )
	} # getAcodeMtx()

	stdCodeMtx <- gEnv$zhF[2:nrow(gEnv$zhF),] - gEnv$zhF[1:(nrow(gEnv$zhF)-1),]
	allCodeMtx <- getAcodeMtx( allIdxF )

	# =========================================================
	# anaColEndPtn()
	colPtnLst <- anaColEndPtn( stdCodeMtx ,pDebug=T )
	# [1]	-14  -5  -3
	# [2]	  5
	# [6]	  2  -7   2  -8  11   0 -13   0   4  -1   7  13   2
	# conditional
	#	11-14 ,30+5 ,44+2 라서.. 쓸모있는 게 하나밖에 없네.

	# =========================================================
	# colValSeqNext( ,pColSize=2 )
	cvSeqNextLst <- colValSeqNext( stdCodeMtx ,pColSize=2 )
	# 걸린 게 아무것도 없음

	# =========================================================
	# colValSeqNext( ,pColSize=3 )
	cvSeqNextLst <- colValSeqNext( stdCodeMtx ,pColSize=3 )
	#	found NONE. -_-;

	return( allIdxF )

} # fCut.colValSeqNext.fStep( )

#========================================================================
# nextZW
# done
fCut.cust.nextZW <- function( gEnv ,allIdxF ){ # < official >
	zMtx <- fCutU.getNextZW( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	zCodeMtx <- t(apply(zMtx,1,function(aZoid){aZoid[2:6]-aZoid[1:5]}))
	lastZoid <- zMtx[nrow(zMtx),]
	cStep <- lastZoid[2:6] - lastZoid[1:5]
	fStep <- lastZoid - zMtx[nrow(zMtx)-1,]

	# 628  1  7 12 15 23 42
	# 647  5 16 21 23 24 30
	# 709 10 18 30 36 39 44
	# 722 12 14 21 30 39 43
	# 727  7  8 10 19 21 31
	# 732  2  4  5 17 27 32
	# 790  3  8 19 27 30 41

	# conditional : 5, 17, 27, 32 에서의 앞뒤 rem 3개 패턴.
	# conditional : zoid[1]은  4 아님 (2,3,4)
	# conditional : zoid[1]은 10 아님 (10=7+3) ,(12=2+?)
	# conditional : zoid[2]은 r4 아님. 18,14, 8, 4, 8, ?
	# conditional : zoid[3]은 27 아님. 31,27,27,?
	# conditional : zoid[4]은 r7,r9 아님. 19,27,27,?

	# <recycle> 동일 컬럼 값 재발생이 2개 이상은 제외.
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( 2>sum(aZoid==lastZoid) )
				})	;kIdx<-anaFlagFnd(!flag)
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # val ptn 재현 3이상 - hIdx+0:1 에서
	hSpan <- nrow(zMtx) - 0:2
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					for( hIdx in hSpan ){
						for( cIdx in 1:4 ){
							fnd <- fCutU.hasPtn( zMtx[hIdx,][cIdx+0:2] ,aZoid )
							if( fnd ) return( FALSE )
						}
					}
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag)
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # rem 동일 3이상
	lastZoid.rem <- lastZoid%%10
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid %% 10
					return( 3>sum(lastZoid.rem==aCode) )
				})	;kIdx<-anaFlagFnd(!flag)
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # rem ptn 재현 3이상
	zRem <- lastZoid %% 10
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid%%10
					for( cIdx in 1:4 ){
						fnd <- fCutU.hasPtn( zRem[cIdx+0:2] ,aCode )
						if( fnd ) return( FALSE )
					}
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag)
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # rem ptn 재현 4이상 - hIdx-1 에서.
	zRem <- zMtx[nrow(zMtx)-1,] %% 10
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid%%10
					for( cIdx in 1:3 ){
						fnd <- fCutU.hasPtn( zRem[cIdx+0:3] ,aCode )
						if( fnd ) return( FALSE )
					}
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag)
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# cStep( 5 11  8  3 11 ) 
	# conditional : 1>sum()
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid[2:6] - aZoid[1:5]
					return( 2 > sum(0==(aCode%%11)) )
				})	;kIdx<-anaFlagFnd(!flag)
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	#
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid[2:6] - aZoid[1:5]
					return( !all(aCode[c(1,3,4)]==c(5,8,3)) )
				})	;kIdx<-anaFlagFnd(!flag)
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	#	conditional : span <- 0:1
	span <- 0:2	;spanLen <- length(span)
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid[2:6] - aZoid[1:5]
					for( cIdx in 1:(5-spanLen+1) ){
						fnd <- fCutU.hasPtn( cStep[cIdx+span] ,aCode )
						if( fnd ) return( FALSE )
					}
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag)
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	return( allIdxF )

} # fCut.cust.nextZW( )
# done
fCutCnt.cust.nextZW <- function( gEnv ,allIdxF ){ # < official >
	zMtx <- fCutU.getNextZW( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	flgCnt <- rep( 0 ,length(allIdxF) )

	lastZoid <- zMtx[nrow(zMtx),]
	cStep <- lastZoid[2:6] - lastZoid[1:5]
	fStep <- lastZoid - zMtx[nrow(zMtx)-1,]

	# 628  1  7 12 15 23 42
	# 647  5 16 21 23 24 30
	# 709 10 18 30 36 39 44
	# 722 12 14 21 30 39 43
	# 727  7  8 10 19 21 31
	# 732  2  4  5 17 27 32
	# 790  3  8 19 27 30 41

	# conditional : 5, 17, 27, 32 에서의 앞뒤 rem 3개 패턴.
	stdQuo <- table( lastZoid %/% 10 )
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aQuo <- table( aZoid%/%10 )
					if( length(aQuo)!=length(stdQuo) ) return( TRUE )

					return( !all(aQuo==stdQuo) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	cat(sprintf("flaged %d(%.1f%%)\n",sum(!flag),100*sum(!flag)/length(flag)))

	# conditional : 5, 17, 27, 32 에서의 앞뒤 rem 3개 패턴.
	remVal <- c( 5, 7, 7, 2 )
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid%%10
					for( cIdx in 1:2 ){
						fnd <- fCutU.hasPtn( remVal[cIdx+0:2] ,aCode )
						if( fnd ) return( FALSE )
					}
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	cat(sprintf("flaged %d(%.1f%%)\n",sum(!flag),100*sum(!flag)/length(flag)))

	# conditional : zoid[1]은  4 아님 (2,3,4)
	flag <- !(gEnv$allZoidMtx[allIdxF,1] %in% c( 4) )	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	cat(sprintf("flaged %d(%.1f%%)\n",sum(!flag),100*sum(!flag)/length(flag)))

	# conditional : zoid[2]는  4 아님 (16,xx,xx,8,xx,xx,?)
	flag <- !(gEnv$allZoidMtx[allIdxF,2] %in% c( 4) )	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	cat(sprintf("flaged %d(%.1f%%)\n",sum(!flag),100*sum(!flag)/length(flag)))

	# conditional : zoid[2]은 r4 아님. 18,14, 8, 4, 8, ?
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid %% 10
					return( aCode[2]!=4 )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	cat(sprintf("flaged %d(%.1f%%)\n",sum(!flag),100*sum(!flag)/length(flag)))

	# conditional : zoid[4]은  32 아님 (10+17=27)
	flag <- !(gEnv$allZoidMtx[allIdxF,4] %in% c(32) )	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	cat(sprintf("flaged %d(%.1f%%)\n",sum(!flag),100*sum(!flag)/length(flag)))

	# conditional : 10=7+3 관계가 또 재발? (10,xx,xx,7,xx,xx,3)
	banVal <- c( 10 ,10 ,16 ,13 ,12 ,11 )
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( !any(aZoid==banVal) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	cat(sprintf("flaged %d(%.1f%%)\n",sum(!flag),100*sum(!flag)/length(flag)))


	# conditional : zoid[3]은 27 아님. 31,27,27,?
	flag <- !(gEnv$allZoidMtx[allIdxF,3] %in% c(27) )	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	cat(sprintf("flaged %d(%.1f%%)\n",sum(!flag),100*sum(!flag)/length(flag)))

	# conditional : zoid[4]은 r7,r9 아님. 19,27,27,?
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid %% 10
					return( !(aCode[4]%in%c(7,9)) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	cat(sprintf("flaged %d(%.1f%%)\n",sum(!flag),100*sum(!flag)/length(flag)))

	# <recycle> 동일 컬럼 값 재발생
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( 1>sum(aZoid==lastZoid) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	cat(sprintf("flaged %d(%.1f%%)\n",sum(!flag),100*sum(!flag)/length(flag)))

    # val ptn(2) 재현 - hIdx+0:1 에서
	hSpan <- nrow(zMtx) - 0:1
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					for( hIdx in hSpan ){
						for( cIdx in 1:5 ){
							fnd <- fCutU.hasPtn( zMtx[hIdx,][cIdx+0:1] ,aZoid )
							if( fnd ) return( FALSE )
						}
					}
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	cat(sprintf("flaged %d(%.1f%%)\n",sum(!flag),100*sum(!flag)/length(flag)))

	# cStep( 5 11  8  3 11 ) 
	# conditional : 1>sum()
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid[2:6] - aZoid[1:5]
					return( 0 == sum(aCode==11) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	cat(sprintf("flaged %d(%.1f%%)\n",sum(!flag),100*sum(!flag)/length(flag)))
	#
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid[2:6] - aZoid[1:5]
					return( aCode[3] != (aCode[1]+aCode[4]) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	cat(sprintf("flaged %d(%.1f%%)\n",sum(!flag),100*sum(!flag)/length(flag)))


	#
	lastZoid.zw <- lastZoid[6] - lastZoid[1]
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( lastZoid.zw != (aZoid[6]-aZoid[1]) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	cat(sprintf("flaged %d(%.1f%%)\n",sum(!flag),100*sum(!flag)/length(flag)))

	return( flgCnt )

} # fCutCnt.cust.nextZW( )

#========================================================================
# fCutU.getNextQuo10
# done
fCut.cust.NextQuo10 <- function( gEnv ,allIdxF ){ # < official >
	zMtx <- fCutU.getNextQuo10( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	zCodeMtx <- t(apply(zMtx,1,function(aZoid){aZoid[2:6]-aZoid[1:5]}))
	# 796  1 21 26 36 40 41
	# 797  5 22 31 32 39 45
	# 799 12 17 23 34 42 45
	# 802 10 11 12 18 24 42
	# 805  3 12 13 18 31 32
	# 808 15 21 31 32 41 43

	lastZoid <- zMtx[nrow(zMtx),]
	cStep <- lastZoid[2:6] - lastZoid[1:5]
	fStep <- lastZoid - zMtx[nrow(zMtx)-1,]

	# <recycle> 동일 컬럼 값 재발생이 2개 이상은 제외.
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( 2>sum(aZoid==lastZoid) )
				})	;kIdx<-anaFlagFnd(!flag)
    allIdxF <- allIdxF[flag]

    # val ptn 재현 3이상 - hIdx+0:1 에서
	hSpan <- nrow(zMtx) - 0:2
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					for( hIdx in hSpan ){
						for( cIdx in 1:4 ){
							fnd <- fCutU.hasPtn( zMtx[hIdx,][cIdx+0:2] ,aZoid )
							if( fnd ) return( FALSE )
						}
					}
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag)
    allIdxF <- allIdxF[flag]

    # rem 동일 3이상
	lastZoid.rem <- lastZoid%%10
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid %% 10
					return( 3>sum(lastZoid.rem==aCode) )
				})	;kIdx<-anaFlagFnd(!flag)
    allIdxF <- allIdxF[flag]

	#
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid[2:6] - aZoid[1:5]
					return( 3 > sum(aCode==cStep) )
				})	;kIdx<-anaFlagFnd(!flag)
    allIdxF <- allIdxF[flag]

	# conditinoal zoid==(51,12,13,23,14,34)2개 이내 12-21, 13,31
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( 2>sum(aZoid==c(51,12,13,23,14,34)) )
				})	;kIdx<-anaFlagFnd(!flag)
    allIdxF <- allIdxF[flag]

	return( allIdxF )

} # fCut.cust.NextQuo10( )

# done
fCutCnt.cust.NextQuo10 <- function( gEnv ,allIdxF ){ # < official >
	zMtx <- fCutU.getNextQuo10( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	flgCnt <- rep( 0 ,length(allIdxF) )
	# 796  1 21 26 36 40 41
	# 797  5 22 31 32 39 45
	# 799 12 17 23 34 42 45
	# 802 10 11 12 18 24 42
	# 805  3 12 13 18 31 32
	# 808 15 21 31 32 41 43

	# <recycle> 동일 컬럼 값 재발생
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( 1>sum(aZoid==lastZoid) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1

	# conditinoal Quo10 tbl 동일
	stdQuoTbl <- table(lastZoid%/%10)
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aQuoTbl <- table(aZoid%/%10)
					if( length(stdQuoTbl)!=length(aQuoTbl) ) return(FALSE)

					return( !all(stdQuoTbl==aQuoTbl) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1

	# conditional zoid[3] !33	31,32,?
	flag <- !(gEnv$allZoidMtx[allIdxF,3] %in% c(33) )	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1

	# conditional zoid[4] !32	18,18,32,?
	flag <- !(gEnv$allZoidMtx[allIdxF,4] %in% c(32) )	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1

	# conditinoal zoid[2] !r7	17,11,12,11,?
	# conditinoal zoid[5] !(r1,r4)	31,41,? 게다가 rem1이 셋이나 있다.
	# conditinoal zoid[6] !(r3,r4)	42,32,43,? 	
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aRem <- aZoid%%10
					if( aRem[2]%in%c(7  ) ) cnt <- cnt+1
					if( aRem[3]%in%c(3  ) ) cnt <- cnt+1
					if( aRem[5]%in%c(1,4) ) cnt <- cnt+1
					if( aRem[6]%in%c(3,4) ) cnt <- cnt+1

					return( cnt<2 )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1

	# conditinoal cStep[4] !1	1,1,1,?
	# conditinoal cStep[5] !3	1,2,?
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCstep <- aZoid[2:6]-aZoid[1:5]
					if( aCstep[4]==1 ) cnt <- cnt+1
					if( aCstep[5]==3 ) cnt <- cnt+1
					return( cnt<1 )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1

	# conditinoal zw !(28,27) 35 40 40 33 32 29 28 ?
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aw <- aZoid[6]-aZoid[1]
					return( !(aw %in% c(27,28)) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1

	return( flgCnt )

} # fCutCnt.cust.NextQuo10( )

#========================================================================
# zoid[1] - 3 of (3,5,7,9)
fCutCnt.cust.colval1_03 <- function( gEnv ,allIdxF ){ # < official >
	applyFlag <- gEnv$allZoidMtx[allIdxF,1]==3
	allIdxT <- allIdxF[applyFlag]
	flgCnt <- rep( 0 ,length(allIdxT) )

	zMtx <- gEnv$zhF[gEnv$zhF[,1]==3,]	# rptObj<-anaQuoTbl( zMtx )
	lastZoid <- zMtx[nrow(zMtx),]
	cStep <- lastZoid[2:6] - lastZoid[1:5]
	fStep <- lastZoid - zMtx[nrow(zMtx)-1,]
	zCodeMtx <- t(apply(zMtx,1,function(aZoid){aZoid[2:6]-aZoid[1:5]}))
	# 751  3  4 16 20 28 44
	# 763  3  8 16 32 34 43
	# 784  3 10 23 24 31 39
	# 790  3  8 19 27 30 41
	# 795  3 10 13 26 34 38
	# 805  3 12 13 18 31 32

	#
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					return( 2>sum(aZoid[2:6]==lastZoid[2:6]) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 2	# 필수이다 싶으면 2 증가시키자.
	#
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					return( 1>sum(aZoid[2:6]==lastZoid[2:6]) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1

	#
	lastRem <- lastZoid %% 10
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					aRem <- aZoid %% 10
					return( 3>sum(aRem[2:6]==lastRem[2:6]) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1

	#
	lastQuo <- table(lastZoid%/%10)
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					aQuoTbl <- table(aZoid%/%10)
					if( length(lastQuo)!=length(aQuoTbl) ) return( TRUE )
					return( !all(lastQuo==aQuoTbl) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1

	# cStep 3 이상 일치.
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					aCStep <- aZoid[2:6] - aZoid[1:5]
					return( 3>sum(cStep==aCStep) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1

    # val ptn 재현 2이상 - hIdx-1:1 에서
	hSpan <- nrow(zMtx) - 1:1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					for( hIdx in hSpan ){
						for( cIdx in 2:5 ){
							fnd <- fCutU.hasPtn( zMtx[hIdx,][cIdx+0:1] ,aZoid )
							if( fnd ) return( FALSE )
						}
					}
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1

	#
	flag <- !(gEnv$allZoidMtx[allIdxT,2]%in% c(10	))	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	#
	flag <- !(gEnv$allZoidMtx[allIdxT,3]%in% c(13 ,19))	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	#
	flag <- !(gEnv$allZoidMtx[allIdxT,6]%in% c(37    ))	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1

	# conditional zoid[3,5] qr,xx,rq 패턴 발생  13,18,31
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					aQuo <- aZoid %/% 10
					aRem <- aZoid %% 10
					return( !( (aQuo[3]==aRem[5])&&(aRem[3]==aQuo[5]) ) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1

	#  1,11, 2,-> 3  fStep 1	;tail(gEnv$zhF[,1])
	#	fStep 1이 한번에 3개 나오지는 않겠지.
	banVal <- lastZoid[2:6]+1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					return( 2>sum(aZoid[2:6]==banVal) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1


	QQE
	# 

	# allIdxF <- setdiff( allIdxF ,allIdxT[flgCnt>0] )
	flgCnt.all <- rep( 0 ,length(allIdxF) )
	return( allIdxF )

} # fCutCnt.cust.colval1_03( )


QQE binary rem 으로 fCut 추가.
	lastZoid[c(2,5)] 일치값에 대한 fCut 추가.
	cStep 3개, fStep 3개 일치값에 대한 fCut 추가.(컬럼고정)

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

testFunc.fCut.seqRebCnt <- function( gEnv ,allIdxF ){

	testSpan <- 300:nrow(gEnv$zhF)

	rstLst <- list()
	for( hIdx in testSpan ){
		stdZoid <- gEnv$zhF[hIdx,]
		rstObj <- fCut.seqRebCnt( gEnv$zhF[1:(hIdx-1),] ,stdZoid ,pRowLen=100 ,pLen=3 )
		rstLst[[1+length(rstLst)]] <- rstObj
	}
	fnd <- sapply( rstLst ,function(p){ p$cnt })


} # testFunc.fCut.seqRebCnt









# pastBanLst 에 해당하는 allIdxF 파트를 전달받고, 또 전달해주도록 해야 한다.
# global : uAnaCutDataLst.c ,uAnaLstGrp ,pPhase
pPhase <- "colVal"	;pValCnt=FALSE
grpIdx <- 2	;uIdx <- 1
pBanLst <- initValLst( uAnaCutDataLst.c[[grpIdx-1]][[uIdx]][[pPhase]] )
pAllIdxF <- allIdxF

# pAllIdxF=tAllIdxF;pBanLst=NULL;grpIdx=1;pPhase="colVal";pLog=F;gEnv=tEnv
# grpIdx.bak=grpIdx;uIdx.bak=uIdx;pBanLst.bak=pBanLst;pAllIdxF.bak=pAllIdxF;fltPos.bak=fltPos;banMtxLst.bak=banMtxLst
bak.1.1 <- list(grpIdx=grpIdx,uIdx=uIdx,pBanLst=pBanLst,pAllIdxF=pAllIdxF,fltPos=fltPos,banMtxLst=banMtxLst)
bak.1.2 <- list(grpIdx=grpIdx,uIdx=uIdx,pBanLst=pBanLst,pAllIdxF=pAllIdxF,fltPos=fltPos,banMtxLst=banMtxLst)

Obj <- bak.1.1
grpIdx=Obj$grpIdx;uIdx=Obj$uIdx;pBanLst=Obj$pBanLst;pAllIdxF=Obj$pAllIdxF;fltPos=Obj$fltPos;banMtxLst=Obj$banMtxLst

banValScan.grp <- function( pAllIdxF ,pBanLst=NULL ,grpIdx ,pPhase="colVal" ,pLog=F ,gEnv ,pValCnt=FALSE ) {

	initValLst <- function( banLst ){
		valLst <- lapply( banLst ,function(banMtx){
							val <- sort(unique(banMtx[,"banVal"]))
							tbl <- table(banMtx[,"banVal"])[as.character(val)]
							if( !pValCnt ){
								tbl[] <- 1
							}
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
