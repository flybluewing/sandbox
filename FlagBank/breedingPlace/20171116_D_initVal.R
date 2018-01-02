# 20171116_C_exp.R
source("20171116_A_H.R")
source("20171116_A_H_cliper.R")
source("20171116_B_H.R")
source("20171116_C_H.R")
source("20171116_D_H.R")

curWd <- getwd()	;setwd("..")
FB <- getFlagBank()
FB.f <- getFlagBank("./zoidHistory/ZH_Final.csv")
setwd(curWd)
zh	<- as.matrix( FB$zh )
zhF	<- as.matrix( FB.f$zh )	;rownames(zhF) <- 1:nrow(zhF)
testSpan <- (nrow(zh)+1):nrow(zhF)
filtLst <- list()
getFiltHist <- function( pFiltId ,pTStmp ,pFlag=NULL ){
		rObj <- list( filtId=pFiltId ,tCost=(Sys.time()-pTStmp) ,flag=pFlag )
		return( rObj )
	}
#=[missHLst]===============================================================================
#	이 파일에서의 핵심 데이터.
#	사전 필터로 인해 유실되는 실제 Zoid History Index.
missHLst <- list()

# zhF ,allZoidMtx
testSpan <- 300:nrow(zhF)


#-[C0020.A]------------------------------------------------------
#	zhF[,6]-zhF[,1] 이 20 이하인 경우는 전체 5.6% 정도.. 자르자!!
stdCodeMtx <- cbind( zhF[,4]-zhF[,1] , zhF[,6]-zhF[,3] ,zhF[,6]-zhF[,1] )
k <- table(stdCodeMtx[,3])
sum(k[1:8])/sum(k)	# 0.05597964



#-[C0020.B]------------------------------------------------------
#	동일한 쌍의 발생이 42개 밖에 안됨.
stdCodeMtx <- cbind( zhF[,4]-zhF[,1] , zhF[,6]-zhF[,3] ,zhF[,6]-zhF[,1] )
compLst <- list()
for( aIdx in 1:(nrow(zhF)-1)){
	for( bIdx in (aIdx+1):nrow(zhF) ){
		if( all(stdCodeMtx[aIdx,]==stdCodeMtx[bIdx,]) ){
			compLst[[1+length(compLst)]] <- list( aIdx=aIdx ,bIdx=bIdx )
		}
	}
}



#-[C0030.A]------------------------------------------------------
#	5개 이상 일치 82개. 약 10%... 자르자.
stdCodeMtx <- zhF %% 8
compLst <- list()
for( aIdx in 1:(nrow(zhF)-1) ){
	for( bIdx in (aIdx+1):nrow(zhF)){
		cnt <- sum( stdCodeMtx[aIdx,]==stdCodeMtx[bIdx,] )
		if( 4<cnt ){
			compLst[[1+length(compLst)]] <- c( aIdx ,bIdx ,cnt )
		}
	}
}
compMtx <- do.call( rbind ,compLst )
tail( sort(unique( as.vector(compMtx[,1:2]) )) )

#-[C0040.A]------------------------------------------------------
#	5개 이상 일치하는 게 2개 밖에 없음..
stdCodeMtx <- abs(zhF[1:(nrow(zhF)-1),] - zhF[2:nrow(zhF),])
compLst <- list()
for( aIdx in 1:(nrow(stdCodeMtx)-1) ){
	for( bIdx in (aIdx+1):nrow(stdCodeMtx)){
		cnt <- sum( stdCodeMtx[aIdx,]==stdCodeMtx[bIdx,] )
		if( 4<cnt ){
			compLst[[1+length(compLst)]] <- c( aIdx ,bIdx ,cnt )
		}
	}
}
compMtx <- do.call( rbind ,compLst )

#-[C0040.B]------------------------------------------------------
#	n개 Jump.
#		stdCodeMtx에서 780번째 row이면 zhF에서의 780,783 차이값임.
jumpVal <- 2:10	# 29개 발생.
compMtxLst <- list()
for( jIdx in jumpVal ){
	stdCodeMtx <- abs(zhF[1:(nrow(zhF)-jIdx),] - zhF[(jIdx+1):nrow(zhF),])
	nrow(stdCodeMtx) <- jIdx + 1:nrow(stdCodeMtx)
	compLst <- list()
	for( aIdx in 1:(nrow(stdCodeMtx)-1) ){
		for( bIdx in (aIdx+1):nrow(stdCodeMtx)){
			cnt <- sum( stdCodeMtx[aIdx,]==stdCodeMtx[bIdx,] )
			if( 4<cnt ){
				compLst[[1+length(compLst)]] <- c( aIdx ,bIdx ,cnt )
			}
		}
	}
	compMtxLst[[1+length(compMtxLst)]] <- do.call( rbind ,compLst )
}

sapply(compMtxLst,nrow)

#-[D0010.A]------------------------------------------------------
#	최근 2개 History에서 발생한 요소가 재발생...
#		4개 이상은 26개 밖에 없었다.
bNum <- 2
flag <- rep( 0 ,nrow(zhF) )
for( hIdx in (bNum+1):nrow(zhF) ){
	val <- unique(as.vector(zhF[(hIdx-bNum):(hIdx-1),]))
	cnt <- sum( zhF[hIdx,] %in% val )
	flag[hIdx] <- cnt
}

#-[D0010.B]------------------------------------------------------
#	최근 10개 History에서 발생한 요소가 재발생...
#		1개 이하는 11개 밖에 없었다.
bNum <- 10
flag <- rep( 0 ,nrow(zhF) )
for( hIdx in (bNum+1):nrow(zhF) ){
	val <- unique(as.vector(zhF[(hIdx-bNum):(hIdx-1),]))
	cnt <- sum( zhF[hIdx,] %in% val )
	flag[hIdx] <- cnt
}

#=[패턴재현 제거]==============================================================================
# ptn <- getPtnReb( chkCodeMtx ,pDepth=3 )
testSpan <- 500:nrow(zhF)

#-[E0010.A5]------------------------------------------------------
#	6개 패턴은 아예 존재하질 않겠지..
#	따라서 5개 패턴부터 시작.
tStmp <- Sys.time()
filtLst <- list()
for( remCol in 1:ncol(zhF) ){
	stdCodeMtx <- zhF[,-remCol]
	flagLst <- list()
	for( testIdx in testSpan ){
		flagObj <- list( testIdx=testIdx )
		ptn <- NULL
		for( dIdx in 3:1 ){
			ptn <- getPtnReb( stdCodeMtx[1:(testIdx-1),] ,pDepth=dIdx )
			if( !is.null(ptn) ){
				flagObj$ptn <- ptn
				flagObj$depth <- dIdx
				flagObj$matchCnt <- sum(ptn$nextRow==stdCodeMtx[testIdx,])
				break
			}
		}
		flagLst[[1+length(flagLst)]] <- flagObj
	}

	filtLst[[1+length(filtLst)]] <- flagLst
}

# -- report --
rstLst <- list()
for( filtIdx in 1:length(filtLst) ){
	flagLst <- filtLst[[filtIdx]]
	fndIdx <- which(sapply(flagLst,function(p){ !is.null(p$depth) }))
	rstObj <- list( hIdx=testSpan[fndIdx] )
	rstObj$matchCnt <- sapply(flagLst[fndIdx],function(p){p$matchCnt})
	rstLst[[1+length(rstLst)]] <- rstObj
}

failH <- do.call( c ,lapply(rstLst ,function(p){ p$hIdx[p$matchCnt>0] }) )
tDiff <- Sys.time() - tStmp
k.FLogStr(sprintf("E0010.A5 : %.1f%s",tDiff,units(tDiff)))
missHLst[["E0010.A5"]] <- sort(unique(failH))


#-[E0010.A4]------------------------------------------------------
tStmp <- Sys.time()
chkColMtx <- combinations(ncol(zhF),4)
filtLst <- list()
for( chkIdx in 1:ncol(chkColMtx) ){
	stdCodeMtx <- zhF[,chkColMtx[chkIdx,]]
	flagLst <- list()
	for( testIdx in testSpan ){
		flagObj <- list( testIdx=testIdx )
		ptn <- NULL
		for( dIdx in 3:1 ){
			ptn <- getPtnReb( stdCodeMtx[1:(testIdx-1),] ,pDepth=dIdx )
			if( !is.null(ptn) ){
				flagObj$ptn <- ptn
				flagObj$depth <- dIdx
				flagObj$matchCnt <- sum(ptn$nextRow==stdCodeMtx[testIdx,])
				break
			}
		}
		flagLst[[1+length(flagLst)]] <- flagObj
	}

	filtLst[[1+length(filtLst)]] <- flagLst
}

# -- report --
rstLst <- list()
for( filtIdx in 1:length(filtLst) ){
	flagLst <- filtLst[[filtIdx]]
	fndIdx <- which(sapply(flagLst,function(p){ !is.null(p$depth) }))
	rstObj <- list( hIdx=testSpan[fndIdx] )
	rstObj$matchCnt <- sapply(flagLst[fndIdx],function(p){p$matchCnt})
	rstLst[[1+length(rstLst)]] <- rstObj
}

failH <- do.call( c ,lapply(rstLst ,function(p){ p$hIdx[p$matchCnt>0] }) )
tDiff <- Sys.time() - tStmp
k.FLogStr(sprintf("E0010.A4 : %.1f%s",tDiff,units(tDiff)))
missHLst[["E0010.A4"]] <- sort(unique(failH))


#-[E0010.A3]------------------------------------------------------
tStmp <- Sys.time()
chkColMtx <- combinations(ncol(zhF),3)
filtLst <- list()
for( chkIdx in 1:ncol(chkColMtx) ){
	stdCodeMtx <- zhF[,chkColMtx[chkIdx,]]
	flagLst <- list()
	for( testIdx in testSpan ){
		flagObj <- list( testIdx=testIdx )
		ptn <- NULL
		for( dIdx in 3:1 ){
			ptn <- getPtnReb( stdCodeMtx[1:(testIdx-1),] ,pDepth=dIdx )
			if( !is.null(ptn) ){
				flagObj$ptn <- ptn
				flagObj$depth <- dIdx
				flagObj$matchCnt <- sum(ptn$nextRow==stdCodeMtx[testIdx,])
				break
			}
		}
		flagLst[[1+length(flagLst)]] <- flagObj
	}

	filtLst[[1+length(filtLst)]] <- flagLst
}

# -- report --
rstLst <- list()
for( filtIdx in 1:length(filtLst) ){
	flagLst <- filtLst[[filtIdx]]
	fndIdx <- which(sapply(flagLst,function(p){ !is.null(p$depth) }))
	rstObj <- list( hIdx=testSpan[fndIdx] )
	rstObj$matchCnt <- sapply(flagLst[fndIdx],function(p){p$matchCnt})
	rstLst[[1+length(rstLst)]] <- rstObj
}

failH <- do.call( c ,lapply(rstLst ,function(p){ p$hIdx[p$matchCnt>0] }) )
tDiff <- Sys.time() - tStmp
k.FLogStr(sprintf("E0010.A3 : %.1f%s",tDiff,units(tDiff)))
missHLst[["E0010.A3"]] <- sort(unique(failH))


#-[E0010.A2]------------------------------------------------------
tStmp <- Sys.time()
chkColMtx <- combinations(ncol(zhF),2)
filtLst <- list()
for( chkIdx in 1:ncol(chkColMtx) ){
	stdCodeMtx <- zhF[,chkColMtx[chkIdx,]]
	flagLst <- list()
	for( testIdx in testSpan ){
		flagObj <- list( testIdx=testIdx )
		ptn <- NULL
		for( dIdx in 3:1 ){
			ptn <- getPtnReb( stdCodeMtx[1:(testIdx-1),] ,pDepth=dIdx )
			if( !is.null(ptn) ){
				flagObj$ptn <- ptn
				flagObj$depth <- dIdx
				flagObj$matchCnt <- sum(ptn$nextRow==stdCodeMtx[testIdx,])
				break
			}
		}
		flagLst[[1+length(flagLst)]] <- flagObj
	}

	filtLst[[1+length(filtLst)]] <- flagLst
}

# -- report --
rstLst <- list()
for( filtIdx in 1:length(filtLst) ){
	flagLst <- filtLst[[filtIdx]]
	fndIdx <- which(sapply(flagLst,function(p){ !is.null(p$depth) }))
	rstObj <- list( hIdx=testSpan[fndIdx] )
	rstObj$matchCnt <- sapply(flagLst[fndIdx],function(p){p$matchCnt})
	rstLst[[1+length(rstLst)]] <- rstObj
}

failH <- do.call( c ,lapply(rstLst ,function(p){ p$hIdx[p$matchCnt>0] }) )
tDiff <- Sys.time() - tStmp
k.FLogStr(sprintf("E0010.A2 : %.1f%s",tDiff,units(tDiff)))
missHLst[["E0010.A2"]] <- sort(unique(failH))


#-[E0010.AX]------------------------------------------------------
flagLst <- list()
for( testIdx in testSpan ){
	lastZoidMtx <- zhF[testIdx,,drop=F]
	filtGrp <- getPtnRebGrp( zhF[1:(testIdx-1),] ,pNextJump=1 )
	filtRst <- filtGrp$filt( lastZoidMtx )
	flagObj <- list( hIdx=testIdx ,filtRst=filtRst )
	flagObj$filtNum <- sapply(filtGrp$filtGrpLst ,function(p){length(p$filtLst)})
	flagLst[[1+length(flagLst)]] <- flagObj
}

# k <- sapply( flagLst ,function(p){ p$filtRst[[1]]$survive } )
#	겨우 3개 나옴. jumpNum 2,3,4도 시도해볼 수 있을 듯.



#=[SAVE]========================================================================================
deskObj <- list( allZoidMtx=allZoidMtx )
deskObj$memo <- sprintf("zhF size : %d.",nrow(zhF))
deskObj$filtLst <- filtLst
save( deskObj ,file="Obj_deskObj.ReadyGo.save" )
