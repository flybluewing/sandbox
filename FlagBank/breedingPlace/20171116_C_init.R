# 20171116_C_exp.R
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
testSpan <- (nrow(zh)+1):nrow(zhF)
filtLst <- list()
getFiltHist <- function( pFiltId ,pTStmp ){
		rObj <- list( filtId=pFiltId ,tCost=(Sys.time()-pTStmp) )
		return( rObj )
	}

# zhF ,allZoidMtx
testSpan <- 300:nrow(zhF)
allZoidMtx <- getAllZoid() # 38sec

#-[A0010]------------------------------------------------------
codeMtx <- allZoidMtx[,2:6]-allZoidMtx[,1:5]
tStmp <- Sys.time()
stepM <- apply( codeMtx ,1 ,function(p){max(table(p))})
filtLst[[1+length(filtLst)]] <- getFiltHist( "A0010" ,tStmp )
allZoidMtx <- allZoidMtx[stepM<=2,]		# 7740330 

#-[A0020]------------------------------------------------------
codeMtx <- allZoidMtx %% 2
tStmp <- Sys.time()
cnt <- apply( codeMtx ,1 ,function(p){sum(p==0)})
filtLst[[1+length(filtLst)]] <- getFiltHist( "A0020" ,tStmp )
allZoidMtx <- allZoidMtx[(cnt>0)&(cnt<6),]	# 7599240

#-[A0030]------------------------------------------------------
codeMtx <- allZoidMtx %% 3
tStmp <- Sys.time()
cnt <- apply( codeMtx ,1 ,function(p){sum(p==0)})
filtLst[[1+length(filtLst)]] <- getFiltHist( "A0030" ,tStmp )
allZoidMtx <- allZoidMtx[(cnt>0),]	# 7041489


# ====================================================
# Base on last Zoid History

#-[B0010]----------------------------------------------------------------------------------------
# abs(zhF[aIdx,]-zhF[bIdx,]) 가 동일한 것은 겨우 3번 발생했다. 13hr
flag <- rep(0,nrow(allZoidMtx))
tStmp <- Sys.time()
for( hIdx in 1:nrow(zhF) ){
	for( zIdx in 1:nrow(allZoidMtx) ){
		d <- abs(zhF[hIdx,]-allZoidMtx[zIdx,])
		if( 1==length(unique(d)) ){
			flag[zIdx] <- hIdx
		}
	}
}
filtLst[[1+length(filtLst)]] <- getFiltHist( "B0010" ,tStmp )
allZoidMtx <- allZoidMtx[flag==0,] # 6975973

#-[B0020]----------------------------------------------------------------------------------------
# abs(zhF[aIdx,]-zhF[bIdx,]) 가 5개 동일한 것
flag <- rep( 0 ,nrow(allZoidMtx) )
tStmp <- Sys.time()
for( aIdx in 1:nrow(allZoidMtx) ){
	for( zIdx in (nrow(zhF)-29):nrow(zhF) ){
		tbl <- table(abs(allZoidMtx[aIdx,]-zhF[zIdx,]))
		if( 5<=max(tbl) ){
			flag[aIdx] <- zIdx
			break
		}
	}
	if( 0==(aIdx%%100000) ){
		k.FLogStr(sprintf("aIdx:%d",aIdx))
	}
}
filtLst[[1+length(filtLst)]] <- getFiltHist( "B0020" ,tStmp )
allZoidMtx <- allZoidMtx[flag==0,]

#-[B0030A]---------------------------------------------------------------------------------------
# abs(zhF[tIdx,]-zhF[(tIdx-1),]) 가 동일한 적은 없었다.(4개 동일한 경우는 70번)
hd <- 1
stdCodeMtx <- abs(zhF[1:(nrow(zhF)-hd),] - zhF[(hd+1):nrow(zhF),])
lastZoid <- zhF[(nrow(zhF)-hd+1),] # 마지막 history로부터 hd만큼 이전이어야 하므로..
allCodeMtx <- apply( allZoidMtx ,1 ,function(p){abs(p-lastZoid)} )
allCodeMtx <- t(allCodeMtx)
flag <- rep( 0 ,nrow(allCodeMtx) )
tStmp <- Sys.time()
for( stdIdx in 1:nrow(stdCodeMtx) ){
	for( aIdx in 1:nrow(allCodeMtx) ){
		cnt <- sum( allCodeMtx[aIdx,]==stdCodeMtx[stdIdx,] )
		if( 4<=cnt )
			flag[aIdx] <- stdIdx
	}
}
filtLst[[1+length(filtLst)]] <- getFiltHist( "B0030A" ,tStmp )
allZoidMtx <- allZoidMtx[flag==0,]

#-[B0030D2]----------------------------------------------------------------------------------------
# abs(zhF[tIdx,]-zhF[(tIdx-2),]) 가 동일한 적은 없었다.
#	- 5개 동일한 경우는 딱 1번.
hd <- 2
stdCodeMtx <- abs(zhF[1:(nrow(zhF)-hd),] - zhF[(hd+1):nrow(zhF),])
lastZoid <- zhF[(nrow(zhF)-hd+1),] # 마지막 history로부터 hd만큼 이전이어야 하므로..
allCodeMtx <- apply( allZoidMtx ,1 ,function(p){abs(p-lastZoid)} )
allCodeMtx <- t(allCodeMtx)
flag <- rep( 0 ,nrow(allCodeMtx) )
tStmp <- Sys.time()
for( stdIdx in 1:nrow(stdCodeMtx) ){
	for( aIdx in 1:nrow(allCodeMtx) ){
		cnt <- sum( allCodeMtx[aIdx,]==stdCodeMtx[stdIdx,] )
		if( 5<=cnt )
			flag[aIdx] <- stdIdx
	}
}
filtLst[[1+length(filtLst)]] <- getFiltHist( "B0030D2" ,tStmp )
allZoidMtx <- allZoidMtx[flag==0,]


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
	rebLen <- getRebLen( 1:45 ,zhF )
#-[B0041]----------------------------------------------------------------------------------------
#	매치가 5개 이상인 것.
flag <- rep(0,nrow(allZoidMtx))
tStmp <- Sys.time()
for( zIdx in 1:nrow(allZoidMtx) ){
	rebZ <- rebLen[allZoidMtx[zIdx,]]
	for( idx in 1:length(rebSpan) ){
		if( 5<=sum(rebZ==rebMtx[idx,]) ){
			flag[zIdx] <- rebSpan[idx]	# hIdx
			break
		}
	}
}
filtLst[[1+length(filtLst)]] <- getFiltHist( "B0041" ,tStmp )
allZoidMtx <- allZoidMtx[flag==0,]

#=[C0000]========================================================================================
#-[C0010]----------------------------------------------------------------------------------------
#	Quotient
lastDeep <- 10
flagInit <- rep( 0 ,(45 %/% lastDeep)+1 )
aQuoMtx <- allZoidMtx %/% lastDeep
aCodeMtx <- apply( aQuoMtx ,1 ,function(p){ 
				flag <- flagInit
				for( idx in 1:length(p) )
					flag[ p[idx]+1 ] <- flag[ p[idx]+1 ] + 1
				return( flag )
			})
aCodeMtx <- t(aCodeMtx)
aCode.max <- apply( aCodeMtx ,1 ,max )
allZoidMtx <- allZoidMtx[ (aCode.max<4) ,]
aCodeMtx <- aCodeMtx[ (aCode.max<4) ,]

stdQuoMtx <- zhF[(nrow(zhF)-lastDeep+1):nrow(zhF),] %/% 10
flagInit <- rep( 0 ,max(stdQuoMtx)+1 )
stdCodeMtx <- apply( stdQuoMtx ,1 ,function(p){ 
				flag <- flagInit
				for( idx in 1:length(p) )
					flag[ p[idx]+1 ] <- flag[ p[idx]+1 ] + 1
				return( flag )
			})
stdCodeMtx <- t(stdCodeMtx)
#	최근 10 개에 대해서만 동일반복 검토.
stdCodeMtx <- stdCodeMtx[(nrow(stdCodeMtx)-9):nrow(stdCodeMtx),]

flag <- rep( 0 ,nrow(aCodeMtx) )
tStmp <- Sys.time()
for( stdIdx in 1:nrow(stdCodeMtx) ){
	for( aIdx in 1:nrow(aCodeMtx) ){
		cnt <- sum( stdCodeMtx[stdIdx,]==aCodeMtx[aIdx,])
		if( 5<=cnt ){
			flag[aIdx] <- stdIdx
		}
	}
}
filtLst[[1+length(filtLst)]] <- getFiltHist( "C0010" ,tStmp )
allZoidMtx <- allZoidMtx[flag==0,]

#-[C0011]----------------------------------------------------------------------------------------
#	remainder
stdCodeMtx <- zhF %% 7
allCodeMtx <- allZoidMtx %% 7
flag <- rep( 0 ,nrow(allCodeMtx) )
tStmp <- Sys.time()
for( stdIdx in 1:nrow(stdCodeMtx) ){
	for( aIdx in 1:nrow(allCodeMtx) ){
		cnt <- sum( stdCodeMtx[stdIdx,]==allCodeMtx[aIdx,])
		if( 5<=cnt ){
			flag[aIdx] <- stdIdx
		}
	}
}
filtLst[[1+length(filtLst)]] <- getFiltHist( "C0011" ,tStmp )
allZoidMtx <- allZoidMtx[flag==0,]

#-[C0012]----------------------------------------------------------------------------------------
#	rebNum	- 최근 5개 zh에서 마지막 zoid와 동일 코드를 3개이상 가진 zoid.. 91개.
stdCodeMtx <- zhF[(nrow(zhF)-4):nrow(zhF),]
allCodeMtx <- allZoidMtx
flag <- rep( 0 ,nrow(allCodeMtx) )
tStmp <- Sys.time()
for( stdIdx in 1:nrow(stdCodeMtx) ){
	for( aIdx in 1:nrow(allCodeMtx) ){
		cmn <- intersect( stdCodeMtx[stdIdx,] ,allCodeMtx[aIdx,] )
		if( 3<=length(cmn) ){
			flag[aIdx] <- stdIdx
		}
	}
}
filtLst[[1+length(filtLst)]] <- getFiltHist( "C0012" ,tStmp )
allZoidMtx <- allZoidMtx[flag==0,]


#-[C0013A]---------------------------------------------------------------------------------------
#	lastPtn	- 과거 패턴의 재발여부
stdCodeMtx <- zhF %% 10
allCodeMtx <- allZoidMtx %% 10
filtH <- rep( NA ,ncol(stdCodeMtx) )
tStmp <- Sys.time()
for( cIdx in 1:ncol(stdCodeMtx) ){
	ptn <- getPastPtn( stdCodeMtx[,cIdx] ,pDepth=3 ,pScanAll=F )
	if( is.null(ptn) ) {	filtH[cIdx] <- NA
	} else {	filtH[cIdx] <- ptn$nextVal	
	}
}
flag <- apply( allCodeMtx ,1 ,function(p){
				return( sum(p==filtH,na.rm=T) )
			})
filtLst[[1+length(filtLst)]] <- getFiltHist( "C0013A" ,tStmp )
allZoidMtx <- allZoidMtx[flag==0,]


#-[C0014A]--------------------------------------------------------------------------------------
#	lastHPtn	- H 단위로 과거패턴 재발여부 확인.
stdCodeMtx <- zhF %% 3
allCodeMtx <- allZoidMtx %% 3
nextObj <- getPastPtn.mtx( stdCodeMtx )
tStmp <- Sys.time()
if( !is.null(nextObj) ){
	flag <- apply(allCodeMtx ,1 ,function(p){
					cnt <- sum(p==nextObj$nextH)
					return( ifelse( 3<=cnt ,cnt ,0 ) )
				})
	allZoidMtx <- allZoidMtx[flag==0,]
} # if( !is.null(nextObj) )
filtLst[[1+length(filtLst)]] <- getFiltHist( "C0014A" ,tStmp )

#-[C0014A1]--------------------------------------------------------------------------------------
#	lastHPtn	- H 단위로 과거패턴 재발여부 확인.
stdCodeMtx <- zhF %% 2
allCodeMtx <- allZoidMtx %% 2
nextObj <- getPastPtn.mtx( stdCodeMtx )
tStmp <- Sys.time()
if( !is.null(nextObj) ){
	flag <- apply(allCodeMtx ,1 ,function(p){
					cnt <- sum(p==nextObj$nextH)
					return( ifelse( 3<=cnt ,cnt ,0 ) )
				})
	allZoidMtx <- allZoidMtx[flag==0,]
} # if( !is.null(nextObj) )
filtLst[[1+length(filtLst)]] <- getFiltHist( "C0014A1" ,tStmp )

#-[C0014AJ]--------------------------------------------------------------------------------------
stdCodeMtx <- zhF %% 3
allCodeMtx <- allZoidMtx %% 3
backJump <- 2:20
filtPtnLst <- list()
for( bjIdx in backJump ){
	nextObj <- getPastPtn.mtx( stdCodeMtx ,pJump=bjIdx )
	if( !is.null(nextObj) ){
		filtPtnLst[[1+length(filtPtnLst)]] <- nextObj$nextH
	}
}

tStmp <- Sys.time()
flag <- rep( 0 ,nrow(allCodeMtx) )
for( ptnIdx in seq_len(length(filtPtnLst)) ){
	curFlag <- apply(allCodeMtx ,1 ,function(p){	all(p==filtPtnLst[[ptnIdx]])	})
	flag[which(curFlag)] <- ptnIdx
}
allZoidMtx <- allZoidMtx[flag==0,]
filtLst[[1+length(filtLst)]] <- getFiltHist( "C0014AJ" ,tStmp )

#-[C0014B]---------------------------------------------------------------------------------------
#	lastHPtn	- H 단위로 과거패턴 재발여부 확인.(증가간격)
stdCodeMtx <- (zhF[,2:6]-zhF[,1:5]) %% 3
allCodeMtx <- (allZoidMtx[,2:6]-allZoidMtx[,1:5]) %% 3
nextObj <- getPastPtn.mtx( stdCodeMtx )
tStmp <- Sys.time()
if( !is.null(nextObj) ){
	flag <- apply(allCodeMtx ,1 ,function(p){
					cnt <- sum(p==nextObj$nextH)
					return( ifelse( 3<=cnt ,cnt ,0 ) )
				})
	allZoidMtx <- allZoidMtx[flag==0,]
} # if( !is.null(nextObj) )
filtLst[[1+length(filtLst)]] <- getFiltHist( "C0014B" ,tStmp )

#-[C0014B2]---------------------------------------------------------------------------------------
#	lastHPtn	- H 단위로 과거패턴 재발여부 확인.(증가간격)
stdCodeMtx <- (zhF[,2:6]-zhF[,1:5]) %% 2
allCodeMtx <- (allZoidMtx[,2:6]-allZoidMtx[,1:5]) %% 2
nextObj <- getPastPtn.mtx( stdCodeMtx )
tStmp <- Sys.time()
if( !is.null(nextObj) ){
	flag <- apply(allCodeMtx ,1 ,function(p){
					cnt <- sum(p==nextObj$nextH)
					return( ifelse( 3<=cnt ,cnt ,0 ) )
				})
	allZoidMtx <- allZoidMtx[flag==0,]
} # if( !is.null(nextObj) )
filtLst[[1+length(filtLst)]] <- getFiltHist( "C0014B2" ,tStmp )

#-[C0014BJ]--------------------------------------------------------------------------------------
stdCodeMtx <- (zhF[,2:6]-zhF[,1:5]) %% 4
allCodeMtx <- (allZoidMtx[,2:6]-allZoidMtx[,1:5]) %% 4
backJump <- 2:20
filtPtnLst <- list()
for( bjIdx in backJump ){
	nextObj <- getPastPtn.mtx( stdCodeMtx ,pJump=bjIdx )
	if( !is.null(nextObj) ){
		filtPtnLst[[1+length(filtPtnLst)]] <- nextObj$nextH
	}
}

tStmp <- Sys.time()
flag <- rep( 0 ,nrow(allCodeMtx) )
for( ptnIdx in seq_len(length(filtPtnLst)) ){
	curFlag <- apply(allCodeMtx ,1 ,function(p){	all(p==filtPtnLst[[ptnIdx]])	})
	flag[which(curFlag)] <- ptnIdx
}
allZoidMtx <- allZoidMtx[flag==0,]
filtLst[[1+length(filtLst)]] <- getFiltHist( "C0014BJ" ,tStmp )



#-[C0014C]---------------------------------------------------------------------------------------
#	lastHPtn	- H 단위로 과거패턴 재발여부 확인.(다음 H와의 간격)
stdCodeMtx <- abs(zhF[1:(nrow(zhF)-1),]-zhF[2:nrow(zhF),]) %% 3
lastH <- zhF[nrow(zhF),]
allCodeMtx <- t( apply( allZoidMtx ,1 ,function(p){abs(p-lastH)}) )
allCodeMtx <- allCodeMtx %% 3
nextObj <- getPastPtn.mtx( stdCodeMtx )
tStmp <- Sys.time()
if( !is.null(nextObj) ){
	flag <- apply(allCodeMtx ,1 ,function(p){
					cnt <- sum(p==nextObj$nextH)
					return( ifelse( 3<=cnt ,cnt ,0 ) )
				})
	allZoidMtx <- allZoidMtx[flag==0,]
} # if( !is.null(nextObj) )
filtLst[[1+length(filtLst)]] <- getFiltHist( "C0014C" ,tStmp )

#-[C0014C1]--------------------------------------------------------------------------------------
stdCodeMtx <- abs(zhF[1:(nrow(zhF)-1),]-zhF[2:nrow(zhF),]) %% 2
lastH <- zhF[nrow(zhF),]
allCodeMtx <- t( apply( allZoidMtx ,1 ,function(p){abs(p-lastH)}) )
allCodeMtx <- allCodeMtx %% 2
nextObj <- getPastPtn.mtx( stdCodeMtx )
tStmp <- Sys.time()
if( !is.null(nextObj) ){
	flag <- apply(allCodeMtx ,1 ,function(p){
					cnt <- sum(p==nextObj$nextH)
					return( ifelse( 6<=cnt ,cnt ,0 ) )
				})
	allZoidMtx <- allZoidMtx[flag==0,]
} # if( !is.null(nextObj) )
filtLst[[1+length(filtLst)]] <- getFiltHist( "C0014C1" ,tStmp )

#-[C0014CJ]--------------------------------------------------------------------------------------
stdCodeMtx <- abs(zhF[1:(nrow(zhF)-1),]-zhF[2:nrow(zhF),]) %% 3
lastH <- zhF[nrow(zhF),]
allCodeMtx <- t( apply( allZoidMtx ,1 ,function(p){abs(p-lastH)}) )
allCodeMtx <- allCodeMtx %% 3
backJump <- 2:20
filtPtnLst <- list()
for( bjIdx in backJump ){
	nextObj <- getPastPtn.mtx( stdCodeMtx ,pJump=bjIdx )
	if( !is.null(nextObj) ){
		filtPtnLst[[1+length(filtPtnLst)]] <- nextObj$nextH
	}
}

tStmp <- Sys.time()
flag <- rep( 0 ,nrow(allCodeMtx) )
for( ptnIdx in seq_len(length(filtPtnLst)) ){
	curFlag <- apply(allCodeMtx ,1 ,function(p){	all(p==filtPtnLst[[ptnIdx]])	})
	flag[which(curFlag)] <- ptnIdx
}
allZoidMtx <- allZoidMtx[flag==0,]
filtLst[[1+length(filtLst)]] <- getFiltHist( "C0014CJ" ,tStmp )

#-[C0015]--------------------------------------------------------------------------------------
rebSpan <- 28:nrow(zhF)
rebMtx <- matrix( 0 ,nrow=length(rebSpan) ,ncol=ncol(zhF) )
rownames(rebMtx) <- rebSpan
for( idx in 1:length(rebSpan) ){
	hIdx <- rebSpan[idx]
	ml <- getReboundLst( zhF[hIdx,] ,zhF[1:(hIdx-1),,drop=F] ,pSearchFirst=T )
	rebMtx[idx,] <- 
		sapply( ml ,function(p){ifelse(0==length(p$fIdx),NA,hIdx-p$fIdx[1])} )
}

rebLen <- getRebLen( 1:45 ,zhF )

#-[C0015C0]--------------------------------------------------------------------------------------
stdCodeMtx <- rebMtx %% 2

allRebMtx <- allZoidMtx
for( rIdx in 1:nrow(allZoidMtx) ){
	allRebMtx[rIdx,] <- rebLen[ allZoidMtx[rIdx,] ]
}
allCodeMtx <- allRebMtx %% 2

flag <- rep(0,nrow(allCodeMtx))
tStmp <- Sys.time()
nextObj <- getPastPtn.mtx( stdCodeMtx )
if( !is.null(nextObj) ){
	flag <- apply(allCodeMtx ,1 ,function(p){
					cnt <- sum(p==nextObj$nextH)
					return( ifelse( 6<=cnt ,cnt ,0 ) )
				})
	allZoidMtx <- allZoidMtx[flag==0,]
} # if( !is.null(nextObj) )
filtLst[[1+length(filtLst)]] <- getFiltHist( "C0015C0" ,tStmp )

#-[C0015CJ]--------------------------------------------------------------------------------------
stdCodeMtx <- rebMtx %% 3

allRebMtx <- allZoidMtx
for( rIdx in 1:nrow(allZoidMtx) ){
	allRebMtx[rIdx,] <- rebLen[ allZoidMtx[rIdx,] ]
}
allCodeMtx <- allRebMtx %% 3

backJump <- 2:20
filtPtnLst <- list()
for( bjIdx in backJump ){
	nextObj <- getPastPtn.mtx( stdCodeMtx ,pJump=bjIdx )
	if( !is.null(nextObj) ){
		filtPtnLst[[1+length(filtPtnLst)]] <- nextObj$nextH
	}
}

tStmp <- Sys.time()
flag <- rep( 0 ,nrow(allCodeMtx) )
for( ptnIdx in seq_len(length(filtPtnLst)) ){
	curFlag <- apply(allCodeMtx ,1 ,function(p){	all(p==filtPtnLst[[ptnIdx]])	})
	flag[which(curFlag)] <- ptnIdx
}
allZoidMtx <- allZoidMtx[flag==0,]
filtLst[[1+length(filtLst)]] <- getFiltHist( "C0015CJ" ,tStmp )



#===============================================================================================
#-[D0010]---------------------------------------------------------------------------------------
#	group( %%10 ) 크기가 3 이상인 경우의 다음 연속 가능성.
#		QQE:TODO -> 마지막 Zoid에서 group 크기가 3 이상짜리 나오면 추가하기로 하자.

# filtLst[[1+length(filtLst)]] <- getFiltHist( "D0010" ,tStmp )


#===============================================================================================
#-[E0010]---------------------------------------------------------------------------------------
stdCode <- apply(zhF ,1 ,function(p){p[6]-p[1]} )
allCode <- apply(allZoidMtx ,1 ,function(p){p[6]-p[1]} )
ptn <- NULL
for( depIdx in 7:2 ){
	ptn <- getPastPtn( stdCode ,pDepth=depIdx ,pScanAll=F )
	if( !is.null(ptn) ){
		ptn$depIdx <- depIdx
		break
	}
}

tStmp <- Sys.time()
lastCode <- stdCode[length(stdCode)]
flag <- sapply( allCode ,function(p){  
					rst <- ifelse( p!=lastCode ,0 ,lastCode )
					if( rst==0 && !is.null(ptn) )
						rst <- ifelse( p!=ptn$nextVal ,0 ,ptn$nextVal )
					return( rst )
				})
allZoidMtx <- allZoidMtx[flag==0,]
filtLst[[1+length(filtLst)]] <- getFiltHist( "E0010" ,tStmp )

#-[E0020]---------------------------------------------------------------------------------------
stdCode <- apply( zhF %% 10 ,1 ,sum )
allCode <- apply( allZoidMtx %% 10 ,1 ,sum )
ptn <- NULL
for( depIdx in 7:2 ){
	ptn <- getPastPtn( stdCode ,pDepth=depIdx ,pScanAll=F )
	if( !is.null(ptn) ){
		ptn$depIdx <- depIdx
		break
	}
}

tStmp <- Sys.time()
lastCode <- stdCode[length(stdCode)]
flag <- sapply( allCode ,function(p){  
					rst <- ifelse( p!=lastCode ,0 ,lastCode )
					if( rst==0 && !is.null(ptn) )
						rst <- ifelse( p!=ptn$nextVal ,0 ,ptn$nextVal )
					return( rst )
				})
allZoidMtx <- allZoidMtx[flag==0,]
filtLst[[1+length(filtLst)]] <- getFiltHist( "E0020" ,tStmp )





#=[SAVE]========================================================================================
deskObj <- list( allZoidMtx=allZoidMtx )
deskObj$memo <- sprintf("zhF size : %d.",nrow(zhF))
deskObj$filtLst <- filtLst
save( deskObj ,file="Obj_deskObj03.save" )
