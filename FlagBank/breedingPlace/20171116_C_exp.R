# 20171116_C_exp.R
source("20171116_A_H.R")
source("20171116_A_H_cliper.R")
source("20171116_B_H.R")

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
surMtx <- matrix( F ,ncol=0 ,nrow=length(testSpan) )

# =================================================================================
codeF <- zhF %% 5
allF <- allZoidMtx %% 5
rName <- c("last","nPred","hit")
rmMtx <- matrix(0,ncol=ncol(codeF),nrow=length(rName))	;rownames(rmMtx)<-rName
resLst <- list()
score <- rep(0,nrow(allF))
tStmp <- Sys.time()
for( tIdx in testSpan ){
    # 7.5min for 10
    rmMtx["last",] <- codeF[(tIdx-1),]
	for( cIdx in 1:6 ){
		pPtn <- getPastPtn( codeF[1:(tIdx-1),cIdx] )
		if( is.null(pPtn) ){
			rmMtx["nPred",cIdx] <- NA
		} else {
			rmMtx["nPred",cIdx] <- pPtn$nextVal
		}
		rmMtx["hit",cIdx] <- 
            rmMtx["last",cIdx]!=codeF[tIdx,cIdx] && rmMtx["nPred",cIdx]!=codeF[tIdx,cIdx]
	}
	# azFlag <- apply( allF ,1 ,function(p){sum(rmMtx[1,]!=p & rmMtx[2,]!=p)})

    rmObj <- list( rmMtx=rmMtx )
    # rmObj$azFlag=azFlag
	resLst[[1+length(resLst)]] <- rmObj
}
tDiff <- Sys.time() - tStmp
k.miss <- sapply( resLst ,function(p){sum(p$rmMtx["hit",]==0)} )
pos <- sapply( resLst ,function(p){ pos = which(p$rmMtx["hit",]==0)
				return( ifelse(0==length(pos),0,pos[1]) )
			} ) 

cbName <- colnames(surMtx)
surMtx <- cbind( surMtx ,k.miss<=1 )
colnames(surMtx) <- c( cbName ,"%%5" )

# =================================================================================

survLst <- list()
for( idx in 1:length(testSpan) ){
    hIdx <- testSpan[idx]
    fltObj <- flt.seqReb( zhF[1:(hIdx-1),] )    # 7 min, 32개 생존.
    sObj <- list( flag=fltObj$byLate(zhF[hIdx,,drop=F]) )
    survLst[[1+length(survLst)]] <- sObj
}

pos <- sapply( survLst ,function(p){p$flag})

cbName <- colnames(surMtx)
surMtx <- cbind(surMtx,pos)
colnames(surMtx) <- c( cbName ,"seqReb" )

# =================================================================================

codeF <- abs(zhF[1:(nrow(zhF)-1),] - zhF[2:nrow(zhF),])
codeF <- rbind( rep(-1,ncol(zhF)) ,codeF) # hIdx를 맞춰주기 위해..

rName <- c("last","nPred","hit")
rmMtx <- matrix(0,ncol=ncol(codeF),nrow=length(rName))	;rownames(rmMtx)<-rName
resLst <- list()
score <- rep(0,nrow(allF))
tStmp <- Sys.time()
for( tIdx in testSpan ){
    rmMtx["last",] <- codeF[(tIdx-1),]
	for( cIdx in 1:6 ){
		pPtn <- getPastPtn( codeF[1:(tIdx-1),cIdx] )
		if( is.null(pPtn) ){
			rmMtx["nPred",cIdx] <- NA
		} else {
			rmMtx["nPred",cIdx] <- pPtn$nextVal
		}
		rmMtx["hit",cIdx] <- rmMtx["nPred",cIdx]!=codeF[tIdx,cIdx]
	}

    rmObj <- list( rmMtx=rmMtx )
	resLst[[1+length(resLst)]] <- rmObj
}
tDiff <- Sys.time() - tStmp
k <- sapply( resLst ,function(p){sum(p$rmMtx["hit",]==0)} )
pos <- sapply( resLst ,function(p){ pos = which(p$rmMtx["hit",]==0)
				return( ifelse(0==length(pos),0,pos[1]) )
			} ) 



