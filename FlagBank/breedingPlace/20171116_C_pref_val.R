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
zhF	<- as.matrix( FB.f$zh )	;rownames(zhF) <- 1:nrow(zhF)
# allZoidMtx <- getAllZoid() # 38sec

#=[missHLst]===============================================================================
#	이 파일에서의 핵심 데이터.
#	사전 필터로 인해 유실되는 실제 Zoid History Index.
missHLst <- list()


#-[C0014AJ]--------------------------------------------------------------------------------------
#	lastHPtn	- H 단위로 과거패턴 재발여부 확인. (n단계 jump.)
backJump <- 2:20
filtH <- rep( 0 ,ncol(mtx) )
mtx <- zhF %% 3
scanSpan <- 300:nrow(mtx)
idxLst <- list()
for( bjIdx in backJump ){
	compLst <- list()
	for( idx in 1:length(scanSpan) ){
		hIdx <- scanSpan[idx]
		lastH <- mtx[hIdx,]
		nextObj <- getPastPtn.mtx( mtx[1:(hIdx-1),] ,pJump=bjIdx )
		if( !is.null(nextObj) ){
			cnt <- sum(lastH==nextObj$nextH)
			if( 5<=cnt ){
				compLst[[1+length(compLst)]] <- list( hIdx=hIdx ,fIdx=nextObj$fIdx ,cnt=cnt)
			}
		} # if( !is.null(nextH) )
	}
	idxMtx <- do.call( rbind ,lapply(compLst,function(p){c(p$hIdx,p$fIdx,p$cnt)}) )
	idxLst[[1+length(idxLst)]] <- if( 0<length(compLst) ) sort( idxMtx[,1] ) else integer(0)
} # for(bjIdx)
# length(do.call(c,idxLst))	# 101
missHLst[["C0014AJ"]] <- sort( do.call(c,idxLst) )


#-[C0014BJ]---------------------------------------------------------------------------------------
#	lastHPtn	- H 단위로 과거패턴 재발여부 확인.(증가간격)
backJump <- 2:30
filtH <- rep( 0 ,ncol(mtx) )
mtx <- (zhF[,2:6]-zhF[,1:5]) %% 4
scanSpan <- 300:nrow(mtx)
idxLst <- list()
for( bjIdx in backJump ){
	compLst <- list()
	for( idx in 1:length(scanSpan) ){
		hIdx <- scanSpan[idx]
		lastH <- mtx[hIdx,]
		nextObj <- getPastPtn.mtx( mtx[1:(hIdx-1),] ,pJump=bjIdx )
		if( !is.null(nextObj) ){
			cnt <- sum(lastH==nextObj$nextH)
			if( 4<=cnt ){
				compLst[[1+length(compLst)]] <- list( hIdx=hIdx ,fIdx=nextObj$fIdx ,cnt=cnt)
			}
		} # if( !is.null(nextH) )
	}
	idxMtx <- do.call( rbind ,lapply(compLst,function(p){c(p$hIdx,p$fIdx,p$cnt)}) )
	idxLst[[1+length(idxLst)]] <- if( 0<length(compLst) ) sort( idxMtx[,1] ) else integer(0)
} # for(bjIdx)
# length(do.call(c,idxLst))	# 104
missHLst[["C0014BJ"]] <- sort( do.call(c,idxLst) )


#-[C0014CJ]--------------------------------------------------------------------------------------
backJump <- 2:20	# 20까지인 경우 3개 제거됨.
filtH <- rep( 0 ,ncol(mtx) )
mtx <- abs(zhF[1:(nrow(zhF)-1),]-zhF[2:nrow(zhF),]) %% 3
scanSpan <- 300:nrow(mtx)
idxLst <- list()
for( bjIdx in backJump ){
	compLst <- list()
	for( idx in 1:length(scanSpan) ){
		hIdx <- scanSpan[idx]
		lastH <- mtx[hIdx,]
		nextObj <- getPastPtn.mtx( mtx[1:(hIdx-1),] ,pJump=bjIdx )
		if( !is.null(nextObj) ){
			cnt <- sum(lastH==nextObj$nextH)
			if( 5<=cnt ){
				compLst[[1+length(compLst)]] <- list( hIdx=hIdx ,fIdx=nextObj$fIdx ,cnt=cnt)
			}
		} # if( !is.null(nextH) )
	}
	idxMtx <- do.call( rbind ,lapply(compLst,function(p){c(p$hIdx,p$fIdx,p$cnt)}) )
	idxLst[[1+length(idxLst)]] <- if( 0<length(compLst) ) sort( idxMtx[,1] ) else integer(0)
} # for(bjIdx)
# length(do.call(c,idxLst))	# 94
missHLst[["C0014CJ"]] <- sort( do.call(c,idxLst) )



#-[D006XX]---------------------------------------------------------------------------------------
#	col 별 패턴 재발생 가능성.
scanSpan <- 500:nrow(zhF)
jumpStepSpan <- 1:100
compLst <- list()
flag <- rep( 0 ,nrow(zhF) )
for( sIdx in scanSpan ){
	surFlag <- rep( T ,6 )
	tgtZoid <- zhF[sIdx,]
	srcHist <- zhF[1:(sIdx-1),]
	surMtx <- matrix( 0 ,nrow=length(jumpStepSpan) ,ncol=6 )
	bjMtx  <- matrix( 0 ,nrow=length(jumpStepSpan) ,ncol=6 )
	for( jumpStep in jumpStepSpan ){
		ptnLst <- list()
		remVal <- rep( 0 ,6 )
		for( cIdx in 1:6 ){
			ptnLst[[cIdx]] <- NULL
			for( bjIdx in 5:2 ){
				ptn <- getPastPtn( srcHist[1:(nrow(srcHist)-jumpStep+1),cIdx] ,pDepth=bjIdx ,pScanAll=F )
				if( !is.null(ptn) ){
					ptn$bjIdx <- bjIdx
					bjMtx[ jumpStep,cIdx] <- bjIdx
					ptnLst[[cIdx]] <- ptn
					remVal[cIdx] <- srcHist[ptn$fIdx+jumpStep,cIdx]
					break
				}
			}
		} # for(cIdx)
		surFlag <- surFlag & !(tgtZoid==remVal)
		surMtx[jumpStep,] <- !(tgtZoid==remVal)
	}
	flag[sIdx] <- sum(surFlag)
	surMtx[bjMtx==0] <- NA
	missCnt <- apply(surMtx,1,function(p){ sum(p==0,na.rm=T) })
	ptnCnt <- apply(surMtx,1,function(p){ sum(!is.na(p)) })
	missReb <- NULL
	missReb.m <- NULL
	sameColMiss <- rep( FALSE ,nrow(surMtx) )
	if( 0<length(compLst) ){
		lastComp <- compLst[[length(compLst)]]
		missReb <- (missCnt>0) & (lastComp$missCnt>0)
		missReb.m <- (missCnt>1) & (lastComp$missCnt>1)
		for( rIdx in 1:nrow(surMtx) ){
			curF <- ( surMtx[rIdx,]==0 & !is.na(surMtx[rIdx,]) )
			lastF <- ( lastComp$surMtx[rIdx,]==0 & !is.na(lastComp$surMtx[rIdx,]) )
			sameColMiss[rIdx] <- any(curF & lastF)
		}
	}
	compLst[[ 1+length(compLst) ]] <- 
				list(	surMtx=surMtx		,bjMtx=bjMtx
						,missCnt=missCnt	,ptnCnt=ptnCnt
						,missReb=missReb	,missReb.m=missReb.m
						,sameColMiss=sameColMiss
					)
} # for(sIdx)


# missRebNum : 이전에 미스 나온 bj에서 요번에도 미스 나온 경우.
# missRebNum.m : 이전에 2개 이상의 미스가 나온 곳에서 요번에서도 2개 미스가 나온 경우.
# sameColMiss : 이전에 미스 나온 컬럼이 요번에도 미스 나온 경우.
# firstMiss : 처음으로 하나 이상의 미스 나온 곳
# firstMiss.multi : 처음으로 두개 이상의 미스 나온 곳.
cName <- c("compIdx","missRebNum","missRebNum.m"
			,"sameColMiss","missRebNum.1st","missRebNum.m.1st","sameColMiss.1st")
matMtx <- matrix(0,ncol=length(cName),nrow=length(compLst) )
colnames(matMtx) <- cName
for( chkIdx in 2:length(compLst) ){
	matMtx[chkIdx,"compIdx"] <- chkIdx
	compObj <- compLst[[chkIdx]]
	matMtx[chkIdx,"missRebNum"] <- sum(compObj$missReb)
	if( 0<sum(compObj$missReb) ){
		matMtx[chkIdx,"missRebNum.1st"] <- which(compObj$missReb)[1]
	}
	if( 0<sum(compObj$missReb.m) ){
		matMtx[chkIdx,"missRebNum.m.1st"] <- which(compObj$missReb.m)[1]
	}
	matMtx[chkIdx,"sameColMiss"] <- sum(compObj$sameColMiss)
	if( 0<sum(compObj$sameColMiss) ){
		matMtx[chkIdx,"sameColMiss.1st"] <- which(compObj$sameColMiss)[1]
	}
}

missRebNum.1st	<- matMtx[,"missRebNum.1st"]
missRebNum.m.1st	<- matMtx[,"missRebNum.m.1st"]
sameColMiss.1st	<- matMtx[,"sameColMiss.1st"]
# 결과가 상당히 비관적인데... 혹시 버그 있는지 결과 점검하자.
sum(missRebNum.1st>10) 
sum(missRebNum.m.1st>10) 
sum(sameColMiss.1st>10)

k <- (missRebNum.1st>15) & (sameColMiss.1st>15)

seqCntMtx[seqCntMtx[,"val"]==0,]

tail(sameColMiss.1st<15)

#-[D007XX]---------------------------------------------------------------------------------------
#	대각선 연속발생 불가제약 적용.



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

flagMtx <- matrix( 0 ,ncol=length(missHLst) ,nrow=nrow(zhF) )
colnames(flagMtx) <- attributes(missHLst)$names

for( cIdx in seq_len(length(missHLst)) ){
	flagMtx[ missHLst[[cIdx]] ,cIdx ] <- 1
}
flagMtx <- flagMtx[scanSpan,]

k <- apply( flagMtx ,1 ,sum )

