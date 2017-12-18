# 20171116_C_init_val.R
#	초기화에서 걸러지는 실제 Zoid History들의 목록을 확인하기 위함.
source("20171116_A_H.R")
source("20171116_A_H_cliper.R")
source("20171116_B_H.R")
source("20171116_C_H.R")

curWd <- getwd()	;setwd("..")
FB <- getFlagBank()
FB.f <- getFlagBank("./zoidHistory/ZH_Final.csv")
setwd(curWd)
zh	<- as.matrix( FB$zh )
zhF	<- as.matrix( FB.f$zh )
# allZoidMtx <- getAllZoid() # 38sec

#=[missHLst]===============================================================================
#	이 파일에서의 핵심 데이터.
#	사전 필터로 인해 유실되는 실제 Zoid History Index.
missHLst <- list()

# zhF ,allZoidMtx
testSpan <- 300:nrow(zhF)

#-[A0010]------------------------------------------------------
codeMtx <- zhF[,2:6]  - zhF[,1:5]
sLst <- apply( codeMtx ,1 ,function(p){ return( max(table(p)) ) })
missHLst[["A0010"]] <- which(sLst>=3)

#-[A0020]------------------------------------------------------
codeMtx <- zhF %% 2
cnt <- apply( codeMtx ,1 ,function(p){sum(p==0)})
missHLst[["A0020"]] <- which( (cnt==0)|(cnt==6) )

#-[A0030]------------------------------------------------------
codeMtx <- zhF %% 3
cnt <- apply( codeMtx ,1 ,function(p){sum(p==0)})
missHLst[["A0030"]] <- which( (cnt==6) )

# ====================================================
# Base on last Zoid History

#-[B0010]----------------------------------------------------------------------------------------
# abs(zhF[aIdx,]-zhF[bIdx,]) 가 동일한 것은 겨우 3번 발생했다.
comLst <- list()
for( aIdx in 1:(nrow(zhF)-1) ){
	for( bIdx in (aIdx+1):nrow(zhF) ){
		d <- abs(zhF[aIdx,]-zhF[bIdx,])
		if( 1==length(unique(d)) ){
			comLst[[1+length(comLst)]] <- c( aIdx ,bIdx )
		}
	}
}
missHLst[["B0010"]] <- sort( sapply(comLst,function(p){p[2]}) )

#-[B0020]----------------------------------------------------------------------------------------
comLst <- list()
for( aIdx in 1:(nrow(zhF)-1) ){
	for( bIdx in (aIdx+1):nrow(zhF) ){
		d <- abs(zhF[aIdx,]-zhF[bIdx,])
		if( 5<=max(table(d)) ){
			comLst[[1+length(comLst)]] <- c( aIdx ,bIdx )
		}
	}
}
idxDist <- sapply(comLst,function(p){p[2]-p[1]})	# 30까지 10개 발생.
missHLst[["B0020"]] <- sort(sapply( comLst[idxDist<31] ,function(p){p[2]} ))

#-[B0030]----------------------------------------------------------------------------------------
# abs(zhF[tIdx,]-zhF[(tIdx-1),]) 가 동일한 적은 없었다.
#	- 5개 동일한 경우는 딱 2번
codeMtx <- abs(zhF[1:(nrow(zhF)-1),] - zhF[2:nrow(zhF),])
comLst <- list()
for( aIdx in 1:(nrow(codeMtx)-1) ){
	for( bIdx in (aIdx+1):nrow(codeMtx) ){
		cnt <- sum( codeMtx[aIdx,]==codeMtx[bIdx,])
		if( 5<=cnt ){
			comLst[[1+length(comLst)]] <- c( aIdx, bIdx )
		}
	}
}
missHLst[["B0030"]] <- sort(sapply( comLst ,function(p){p[2]} )) + 1 # 1 차이 이므로.

#-[B0030A]---------------------------------------------------------------------------------------
# abs(zhF[tIdx,]-zhF[(tIdx-1),]) 가 동일한 적은 없었다.
#	- 4개 동일한 경우는 70번...
codeMtx <- abs(zhF[1:(nrow(zhF)-1),] - zhF[2:nrow(zhF),])
comLst <- list()
for( aIdx in 1:(nrow(codeMtx)-1) ){
	for( bIdx in (aIdx+1):nrow(codeMtx) ){
		cnt <- sum( codeMtx[aIdx,]==codeMtx[bIdx,])
		if( 4==cnt ){
			comLst[[1+length(comLst)]] <- c( aIdx, bIdx )
		}
	}
}
comMtx <- do.call( rbind ,comLst )
missHLst[["B0030"]] <- sort(unique(comMtx[,2])) + 1

#-[B0030A2]---------------------------------------------------------------------------------------
# abs(zhF[tIdx,]-zhF[(tIdx-1),]) 에서 동일한 값이 n개 이상 나오는 경우.
#	- QQE:Todo
#	- 수행 시간이 오래 걸릴 수 있음.(table함수사용땜시)

#-[B0030D2]----------------------------------------------------------------------------------------
# abs(zhF[tIdx,]-zhF[(tIdx-2),]) 가 동일한 적은 없었다.
#	- 5개 동일한 경우는 딱 1번.
hd <- 2
codeMtx <- abs(zhF[1:(nrow(zhF)-hd),] - zhF[(hd+1):nrow(zhF),])
comLst <- list()
for( aIdx in 1:(nrow(codeMtx)-1) ){
	for( bIdx in (aIdx+1):nrow(codeMtx) ){
		cnt <- sum( codeMtx[aIdx,]==codeMtx[bIdx,])
		if( 5<=cnt ){
			comLst[[1+length(comLst)]] <- c( aIdx, bIdx )
		}
	}
}
missHLst[["B0030D2"]] <- ( sort(unique(do.call( c ,comLst ))) )+hd

#-[B0040]----------------------------------------------------------------------------------------
# rebLen
	rebSpan <- 28:nrow(zhF)
	rebMtx <- matrix( 0 ,nrow=length(rebSpan) ,ncol=ncol(zhF) )
	rownames(rebMtx) <- rebSpan
	for( idx in 1:length(rebSpan) ){
		hIdx <- rebSpan[idx]
		ml <- getReboundLst( zhF[hIdx,] ,zhF[1:(hIdx-1),,drop=F] ,pSearchFirst=T )
		rebMtx[idx,] <- 
			sapply( ml ,function(p){ifelse(0==length(p$fIdx),NA,hIdx-p$fIdx[1])} )
	}

#-[B0041]----------------------------------------------------------------------------------------
#	매치가 4개 이상인 것. - 103+@ 개
compLst <- list()
for( aIdx in 1:(nrow(rebMtx)-1) ){
	for( bIdx in (aIdx+1):nrow(rebMtx) ){
		matchIdx <- which(rebMtx[aIdx,]==rebMtx[bIdx,])
		if( 1<length(matchIdx) ){
			compLst[[1+length(compLst)]] <- 
				list( aIdx=aIdx ,bIdx=bIdx ,matchIdx=matchIdx ,rebLen=rebMtx[aIdx,matchIdx] )
		}
	}
}
matCnt <- sapply( compLst ,function(p){length(p$matchIdx)})
missHLst[["B0041"]] <- sort( sapply(compLst[matCnt>=4] ,function(p){p$bIdx}) )


#=[C0000]========================================================================================
#	대량 손해 감수 파트.
#		100개 정도는 손실을 감수한다.
#-[C0010]----------------------------------------------------------------------------------------
#	Quotient
quoMtx <- zhF %/% 10
flagInit <- rep( 0 ,max(quoMtx)+1 )
codeMtx <- apply( quoMtx ,1 ,function(p){ 
				flag <- flagInit
				for( idx in 1:length(p) )
					flag[ p[idx]+1 ] <- flag[ p[idx]+1 ] + 1
				return( flag )
			})
codeMtx <- t(codeMtx)

#QQE : 한 코너에 5개 이상 몰린 거 삭제. 4개 몰린 것도 지우자.
#   2   3   4   5 
# 452 283  47   2 

compLst <- list()
for( aIdx in 1:(nrow(rebMtx)-1) ){
	for( bIdx in (aIdx+1):nrow(rebMtx) ){
		cnt <- sum(codeMtx[aIdx,] == codeMtx[bIdx,])
		if( 5<=cnt ){
			compLst[[1+length(compLst)]] <- c( aIdx, bIdx )
			next
		}
	}
}
idxMtx <- do.call( rbind ,compLst )
idxDist <- idxMtx[,2] - idxMtx[,1]	# table(idxDist)
missHLst[["C0010"]] <- sort(idxMtx[idxDist<=10,2]) # 최근 10 개

#-[C0011]----------------------------------------------------------------------------------------
#	remainder
codeMtx <- zhF %% 7
compLst <- list()
for( aIdx in 1:(nrow(rebMtx)-1) ){
	for( bIdx in (aIdx+1):nrow(rebMtx) ){
		cnt <- sum(codeMtx[aIdx,] == codeMtx[bIdx,])
		if( 5<=cnt ){
			compLst[[1+length(compLst)]] <- c( aIdx, bIdx )
			next
		}
	}
}
idxMtx <- do.call( rbind ,compLst ) # 전부 합쳐서 118.. 그냥 쓰자.
missHLst[["C0011"]] <- sort( idxMtx[,2] )

#-[C0012]----------------------------------------------------------------------------------------
#	rebNum	- 최근 5개 zh에서 마지막 zoid와 동일 코드를 3개이상 가진 zoid.. 91개.
compLst <- list()
for( aIdx in nrow(zhF):2 ){
	for( bIdx in (aIdx-1):1 ){
		cmn <- intersect( zhF[aIdx,] ,zhF[bIdx,] )
		if( 3<=length(cmn) ){
			compLst[[1+length(compLst)]] <- c( aIdx ,bIdx ,length(cmn) )
			break
		}
	}
}
idxMtx <- do.call( rbind ,compLst )
zhDist <- idxMtx[,1] - idxMtx[,2]
missHLst[["C0012"]] <- sort( idxMtx[zhDist<=5,1] )


#-[C0013A]---------------------------------------------------------------------------------------
#	lastPtn	- 과거 패턴의 재발여부.
mtx <- zhF %% 10
scanSpan <- 300:nrow(mtx)
filtH <- rep( 0 ,ncol(mtx) )
compLst <- list()
for( idx in 1:length(scanSpan) ){
	hIdx <- scanSpan[idx]
	lastH <- mtx[hIdx,]
	for( cIdx in 1:6 ){
		ptn <- getPastPtn( mtx[1:(hIdx-1),cIdx] ,pDepth=3 ,pScanAll=F )
		if( is.null(ptn) ) {	filtH[cIdx] <- 0
		} else {	filtH[cIdx] <- (lastH[cIdx]==ptn$nextVal)
		}
	}
	if( 0<sum(filtH) ){
		compLst[[1+length(compLst)]] <- list( hIdx=hIdx ,filtNum=sum(filtH) )
	}	
}

idxMtx <- do.call( rbind ,lapply(compLst,function(p){c(p$hIdx,p$filtNum)}) )
	# table(idxMtx[,2])	  1   2 
	# 					108  14 
missHLst[["C0013A"]] <- sort( idxMtx[,1] )


#-[C0014A]---------------------------------------------------------------------------------------
#	lastHPtn	- H 단위로 과거패턴 재발여부 확인.
#		- 패턴이 3개 이상 매치하는 것을 제거. (총 81개)
			# table(k)	 2  3  4  5 
			# 			98 56 19  6 
mtx <- zhF %% 3
scanSpan <- 300:nrow(mtx)
filtH <- rep( 0 ,ncol(mtx) )
compLst <- list()
for( idx in 1:length(scanSpan) ){
	hIdx <- scanSpan[idx]
	lastH <- mtx[hIdx,]
	nextObj <- getPastPtn.mtx( mtx[1:(hIdx-1),] )
	if( !is.null(nextObj) ){
		cnt <- sum(lastH==nextObj$nextH)
		if( 3<=cnt ){
			compLst[[1+length(compLst)]] <- list( hIdx=hIdx ,fIdx=nextObj$fIdx ,cnt=cnt)
		}
	} # if( !is.null(nextH) )
} # for

idxMtx <- do.call( rbind ,lapply(compLst,function(p){c(p$hIdx,p$fIdx,p$cnt)}) )
missHLst[["C0014A"]] <- sort( idxMtx[,1] )
#-[C0014AJ1]--------------------------------------------------------------------------------------
#	lastHPtn	- H 단위로 과거패턴 재발여부 확인. (n단계 jump)
#		QQE:Todo 

#-[C0014A1]--------------------------------------------------------------------------------------
#	lastHPtn	- H 단위로 과거패턴 재발여부 확인. (6개 일치 11개 발생.)
mtx <- zhF %% 2
scanSpan <- 300:nrow(mtx)
filtH <- rep( 0 ,ncol(mtx) )
compLst <- list()
for( idx in 1:length(scanSpan) ){
	hIdx <- scanSpan[idx]
	lastH <- mtx[hIdx,]
	nextObj <- getPastPtn.mtx( mtx[1:(hIdx-1),] )
	if( !is.null(nextObj) ){
		cnt <- sum(lastH==nextObj$nextH)
		if( 6<=cnt ){
			compLst[[1+length(compLst)]] <- list( hIdx=hIdx ,fIdx=nextObj$fIdx ,cnt=cnt)
		}
	} # if( !is.null(nextH) )
} # for

idxMtx <- do.call( rbind ,lapply(compLst,function(p){c(p$hIdx,p$fIdx,p$cnt)}) )
missHLst[["C0014A1"]] <- sort( idxMtx[,1] )


#-[C0014B]---------------------------------------------------------------------------------------
#	lastHPtn	- H 단위로 과거패턴 재발여부 확인.(증가간격)
#		- 패턴이 3개 이상 매치하는 것을 제거. (총 90개)
		# table( idxMtx[,3] )    3  4  5 
		# 						74 14  2
mtx <- (zhF[,2:6]-zhF[,1:5]) %% 3
scanSpan <- 300:nrow(mtx)
filtH <- rep( 0 ,ncol(mtx) )
compLst <- list()
for( idx in 1:length(scanSpan) ){
	hIdx <- scanSpan[idx]
	lastH <- mtx[hIdx,]
	nextObj <- getPastPtn.mtx( mtx[1:(hIdx-1),] )
	if( !is.null(nextObj) ){
		cnt <- sum(lastH==nextObj$nextH)
		if( 3<=cnt ){
			compLst[[1+length(compLst)]] <- list( hIdx=hIdx ,fIdx=nextObj$fIdx ,cnt=cnt)
		}
	} # if( !is.null(nextH) )
}
idxMtx <- do.call( rbind ,lapply(compLst,function(p){c(p$hIdx,p$fIdx,p$cnt)}) )
missHLst[["C0014B"]] <- sort( idxMtx[,1] )
#-[C0014BJ1]---------------------------------------------------------------------------------------
#	lastHPtn	- H 단위로 과거패턴 재발여부 확인.(증가간격)
#					n번 과거
#				QQE:Todo


#-[C0014B2]---------------------------------------------------------------------------------------
#	lastHPtn	- H 단위로 과거패턴 재발여부 확인.(증가간격)
#		- 패턴이 모두 매치하는 것을 제거. (총 19개)
mtx <- (zhF[,2:6]-zhF[,1:5]) %% 2
scanSpan <- 300:nrow(mtx)
filtH <- rep( 0 ,ncol(mtx) )
compLst <- list()
for( idx in 1:length(scanSpan) ){
	hIdx <- scanSpan[idx]
	lastH <- mtx[hIdx,]
	nextObj <- getPastPtn.mtx( mtx[1:(hIdx-1),] )
	if( !is.null(nextObj) ){
		cnt <- sum(lastH==nextObj$nextH)
		if( 5<=cnt ){
			compLst[[1+length(compLst)]] <- list( hIdx=hIdx ,fIdx=nextObj$fIdx ,cnt=cnt)
		}
	} # if( !is.null(nextH) )
}
idxMtx <- do.call( rbind ,lapply(compLst,function(p){c(p$hIdx,p$fIdx,p$cnt)}) )
missHLst[["C0014B2"]] <- sort( idxMtx[,1] )


#-[C0014C]---------------------------------------------------------------------------------------
#	lastHPtn	- H 단위로 과거패턴 재발여부 확인.(다음 H와의 간격)
#		- 패턴이 3개 이상 매치하는 것을 제거. (총 73개)
		# table( idxMtx[,3] )    3  4  5 
		# 						74 14  2
mtx <- abs(zhF[1:(nrow(zhF)-1),]-zhF[2:nrow(zhF),]) %% 3
scanSpan <- 300:nrow(mtx)
filtH <- rep( 0 ,ncol(mtx) )
compLst <- list()
for( idx in 1:length(scanSpan) ){
	hIdx <- scanSpan[idx]
	lastH <- mtx[hIdx,]
	nextObj <- getPastPtn.mtx( mtx[1:(hIdx-1),] )
	if( !is.null(nextObj) ){
		cnt <- sum(lastH==nextObj$nextH)
		if( 3<=cnt ){
			compLst[[1+length(compLst)]] <- list( hIdx=hIdx ,fIdx=nextObj$fIdx ,cnt=cnt)
		}
	} # if( !is.null(nextH) )
}
idxMtx <- do.call( rbind ,lapply(compLst,function(p){c(p$hIdx,p$fIdx,p$cnt)}) )
missHLst[["C0014C"]] <- sort( idxMtx[,1] )

#-[C0014C1]--------------------------------------------------------------------------------------
#	lastHPtn	- H 단위로 과거패턴 재발여부 확인.(다음 H와의 간격)
#				- 모두 일치 8개.
mtx <- abs(zhF[1:(nrow(zhF)-1),]-zhF[2:nrow(zhF),]) %% 2
scanSpan <- 300:nrow(mtx)
filtH <- rep( 0 ,ncol(mtx) )
compLst <- list()
for( idx in 1:length(scanSpan) ){
	hIdx <- scanSpan[idx]
	lastH <- mtx[hIdx,]
	nextObj <- getPastPtn.mtx( mtx[1:(hIdx-1),] )
	if( !is.null(nextObj) ){
		cnt <- sum(lastH==nextObj$nextH)
		if( 6<=cnt ){
			compLst[[1+length(compLst)]] <- list( hIdx=hIdx ,fIdx=nextObj$fIdx ,cnt=cnt)
		}
	} # if( !is.null(nextH) )
}
idxMtx <- do.call( rbind ,lapply(compLst,function(p){c(p$hIdx,p$fIdx,p$cnt)}) )
missHLst[["C0014C1"]] <- sort( idxMtx[,1] )

#===============================================================================================
#-[D0010]---------------------------------------------------------------------------------------
#	group 크기가 3 이상인 경우의 다음 연속 가능성.
#		332개 중 21개 발생.
quoMtx <- zhF %/% 10
maxLst <- list()
for( rIdx in 1:nrow(quoMtx) ){
	tbl <- table(quoMtx[rIdx,])
	maxObj <- list( freq=max(tbl) )
	if( 3<=maxObj$freq ){
		grpVal <- as.integer(names(tbl))
		maxObj$grpVal <- grpVal[tbl==maxObj$freq]
	}
	maxLst[[1+length(maxLst)]] <- maxObj
}
compLst <- list()
for( hIdx in 2:length(maxLst) ){
	compObj <- list( hIdx=hIdx ,maxFreq=maxLst[[hIdx]]$freq )
	if( maxLst[[hIdx]]$freq==maxLst[[hIdx-1]]$freq ){
		rebFlag <- maxLst[[hIdx]]$grpVal %in% maxLst[[hIdx-1]]$grpVal
		if( any(rebFlag) ){
			compObj$rebGrp <- maxLst[[hIdx]]$grpVal[rebFlag]
			compLst[[1+length(compLst)]] <- compObj
		}
	}
}
hauntCnt <- sum( sapply(maxLst,function(p){p$freq>=3}) )
missHLst[["D0010"]] <- sort( sapply(compLst,function(p){p$hIdx}) )


#===============================================================================================
#-[E0010]---------------------------------------------------------------------------------------
#	QQE:TODO
#		1. zhF[6]-zhF[1]

#=[Summary]======================================================================================
surFlag <- rep( 0 ,nrow(zhF) )
for( idx in 1:length(missHLst) ){
	surFlag[ missHLst[[idx]] ] <- idx
}

seqCntMtx <- k.seq(1*(surFlag==0))$seqCntMtx
seqCntMtx <- seqCntMtx[seqCntMtx[,"hIdx"]>=testSpan[1],]
table(seqCntMtx[seqCntMtx[,"val"]==0,"cnt"])
tail(seqCntMtx)

valFlag <- 1 * (surFlag[testSpan]==0)
seqCntMtx <- k.seq(valFlag)$seqCntMtx
sprintf("surv %.1f%%"
		,100*sum(valFlag)/length(valFlag)
	)
