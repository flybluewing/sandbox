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
allZoidMtx <- getAllZoid() # 38sec

# zhF ,allZoidMtx
testSpan <- 300:nrow(zhF)

#-[A0010]------------------------------------------------------
codeMtx <- allZoidMtx[,2:6]-allZoidMtx[,1:5]
stepM <- apply( codeMtx ,1 ,function(p){max(table(p))})
allZoidMtx <- allZoidMtx[stepM<=2,]		# 7740330 

#-[A0020]------------------------------------------------------
codeMtx <- allZoidMtx %% 2
cnt <- apply( codeMtx ,1 ,function(p){sum(p==0)})
allZoidMtx <- allZoidMtx[(cnt>0)&(cnt<6),]	# 7599240

#-[A0030]------------------------------------------------------
codeMtx <- allZoidMtx %% 3
cnt <- apply( codeMtx ,1 ,function(p){sum(p==0)})
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
tDiff <- Sys.time()-tStmp
k.FLogStr(sprintf("abs(zhF[hIdx,]-allZoidMtx[zIdx,]) : %.1f%s ",tDiff,units(tDiff)))
allZoidMtx <- allZoidMtx[flag==0,] # 6975973

#-[B0020]----------------------------------------------------------------------------------------
# abs(zhF[aIdx,]-zhF[bIdx,]) 가 5개 동일한 것
#	최근 30개 내에서 5개 동일은 10개 발생.
# 시간이 너무 오래걸려서 일단 보류...


# QQE 


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
tDiff <- tStmp - Sys.time()
k.FLogStr(sprintf("[B0030A] cost %.f%s",tDiff,units(tDiff)))
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
tDiff <- tStmp - Sys.time()
k.FLogStr(sprintf("[B0030D2] cost %.f%s",tDiff,units(tDiff)))
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
for( zIdx in 1:nrow(allZoidMtx) ){
	rebZ <- rebLen[allZoidMtx[zIdx,]]
	for( idx in 1:length(rebSpan) ){
		if( 5<=sum(rebZ==rebMtx[idx,]) ){
			flag[zIdx] <- rebSpan[idx]	# hIdx
			break
		}
	}
}
allZoidMtx <- allZoidMtx[flag==0,]
# QQE 동작검사.

deskObj <- list( allZoidMtx=allZoidMtx )
deskObj$memo <- sprintf("zhF size : %d. finish C0011 and ready for B0030A",nrow(zhF))
save( deskObj ,file="Obj_deskObj_C01.save" )


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
for( stdIdx in 1:nrow(stdCodeMtx) ){
	for( aIdx in 1:nrow(aCodeMtx) ){
		cnt <- sum( stdCodeMtx[stdIdx,]==aCodeMtx[aIdx,])
		if( 5<=cnt ){
			flag[aIdx] <- stdIdx
		}
	}
}
allZoidMtx <- allZoidMtx[flag==0,]

#-[C0011]----------------------------------------------------------------------------------------
#	remainder
stdCodeMtx <- zhF %% 7
allCodeMtx <- allZoidMtx %% 7
flag <- rep( 0 ,nrow(allCodeMtx) )
tStmt <- Sys.time()
for( stdIdx in 1:nrow(stdCodeMtx) ){
	for( aIdx in 1:nrow(allCodeMtx) ){
		cnt <- sum( stdCodeMtx[stdIdx,]==allCodeMtx[aIdx,])
		if( 5<=cnt ){
			flag[aIdx] <- stdIdx
		}
	}
}
tDiff <- Sys.time() - tStmt
allZoidMtx <- allZoidMtx[flag==0,]

# QQE goto B0030A

