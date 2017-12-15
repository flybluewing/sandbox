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
# codeMtx <- zhF[,2:6]  - zhF[,1:5]
# sLst <- apply( codeMtx ,1 ,function(p){ return( max(table(p)) ) })
#	3개 이상은 삭제하자.
	# table(sLst)
	#	  1   2   3   4 
	#	341 401  39   3 

codeMtx <- allZoidMtx[,2:6]-allZoidMtx[,1:5]
stepM <- apply( codeMtx ,1 ,function(p){max(table(p))})
allZoidMtx <- allZoidMtx[stepM<=2,]		# 7740330 

#-------------------------------------------------------
# codeMtx <- zhF %% 2
# cnt <- apply( codeMtx ,1 ,function(p){sum(p==0)})
# 	table(cnt)	 0   1   2   3   4   5   6 
# 				15  56 205 275 175  52   6 
#	모두 짝수이거나 홀수인 경우는 21개 뿐. 
codeMtx <- allZoidMtx %% 2
cnt <- apply( codeMtx ,1 ,function(p){sum(p==0)})
allZoidMtx <- allZoidMtx[(cnt>0)&(cnt<6),]	# 7599240

#-------------------------------------------------------
# codeMtx <- zhF %% 3
# cnt <- apply( codeMtx ,1 ,function(p){sum(p==0)})
# 	table(cnt)	 0   1   2   3   4   5   6 
# 				59 216 264 188  49   6   2 
# 6 개 모두 3의 배수인 경우는 2개 뿐.
codeMtx <- allZoidMtx %% 3
cnt <- apply( codeMtx ,1 ,function(p){sum(p==0)})
allZoidMtx <- allZoidMtx[(cnt>0),]	# 7041489


# ====================================================
# Base on last Zoid History

#-----------------------------------------------------------------------------------------
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

#	- abs() 적용치 않을 시, 5개가 동일한 것은 25번 발생.
flag <- rep(0,nrow(allZoidMtx))
tStmp <- Sys.time()
for( hIdx in 1:nrow(zhF) ){
	for( zIdx in 1:nrow(allZoidMtx) ){
		d <- zhF[hIdx,]-allZoidMtx[zIdx,]
		if( 5<=max(table(d)) ){
			flag[zIdx] <- hIdx
		}
	}
	if( 0==(hIdx%%50) ){
		tDiff <- Sys.time() - tStmp
		k.FLogStr(sprintf("hIdx:%d %.1f%s",hIdx,tDiff,units(tDiff)))
	}
}
tDiff <- Sys.time()-tStmp
k.FLogStr(sprintf("abs(zhF[hIdx,]-allZoidMtx[zIdx,]) : %.1f%s ",tDiff,units(tDiff)))
allZoidMtx <- allZoidMtx[flag==0,]


#-----------------------------------------------------------------------------------------
# abs(zhF[tIdx,]-zhF[(tIdx-1),]) 가 동일한 적은 없었다.
#	- 5개 동일한 경우는 딱 2번
#	- 4개 동일한 경우는 63번...
codeMtx <- abs(zhF[1:(nrow(zhF)-1),]-zhF[2:nrow(zhF),])
lastH <- zhF[nrow(zhF),]
flag <- rep(0,nrow(allZoidMtx))
tStmp <- Sys.time()
for( zIdx in 1:nrow(allZoidMtx) ){
	d <- abs(allZoidMtx[zIdx,]-lastH)
	for( hIdx in 1:nrow(codeMtx) ){
		if( 4<=sum(d==codeMtx[hIdx,]) ){
			flag[zIdx] <- hIdx
		}
	} # for(hIdx)
}
tDiff <- Sys.time()-tStmp
k.FLogStr(sprintf("abs(allZoidMtx[zIdx,]-lastH) : %.1f%s ",tDiff,units(tDiff)))
allZoidMtx <- allZoidMtx[flag==0,]
# 무슨 필요지?
# allZCode <- apply( allZoidMtx ,1 ,function(p){abs(p-lastH)})

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
deskObj$memo <- sprintf("zhF size : %d",nrow(zhF))
save( deskObj ,file="Obj_deskObj_C.save" )


