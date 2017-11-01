# 
library(plyr)
source("20170911_A_H.R")
source("20170911_A_auxH.R")

source("20170917_A_H.R")

# 한글 깨지지는 않겠지?

curWd <- getwd()
setwd("..")
FB <- getFlagBank()
setwd(curWd)

# k1 <- getSeqProbMapObj( pFlag=sample(1:3 ,500 ,replace=T ,prob=c(2,1,1) ) )
# stdSeqObj <- getStdSeqProbMapObj( pTestNum=100 ,pSeqLogMax=300 )
# kObj <- k.seqNum( pFlag=pFlag )

tFlag <- FB$zh[,1] %% 4

seqNumObj <- k.seqNum( tFlag )

mtx <- FB$zh %% 4
naMtxRow <- NULL

diffSize <- 2
naMtxRow <- diffSize
mtxFinal <- mtx
mtxFinal[,] <- NA
mtxFinal[(diffSize+1):nrow(mtx),] <- 
	mtx[1:(nrow(mtx)-diffSize),] - mtx[(diffSize+1):nrow(mtx),] 
mtxFinal <- abs(mtxFinal)

#stdSeqObj 
myObj <- load("Obj_stdSeqObj.save")
seqProbPredictor <- getSeqProbPredictor(stdSeqObj)

pFlag <- mtxFinal[,1]

hSpan <- 100:(length(pFlag)-1)
codeVal <- sort(unique(pFlag),na.last=T)
if( !is.null(naMtxRow) ){
	codeVal <- sort(unique(pFlag[(1+naMtxRow):h]),na.last=T)
}
codeVal.idx <- which(is.na(codeVal)) # NA가 존재하는 경우.

validLst <- list()
for( hIdx in 1:length(hSpan) ){
	h <- hSpan[hIdx]
	flag <- pFlag[1:h]
	if( !is.null(naMtxRow) ){
		flag <- pFlag[(1+naMtxRow):h]
	}
	seqNumObj <- k.seqNum( flag	,pCodeVal=codeVal )
	probObj <- seqProbPredictor$predict( seqNumObj )
	
	vObj <- list( probMtx=probObj$probMtx )
	vObj$prob <- ifelse( probObj$probMtx["seqNum",]>0 
					,probObj$probMtx["prob",]
					,(1-probObj$probMtx["prob",])
				)
	vObj$pred.val <- seqNumObj$codeVal[which.max(vObj$prob)]
	vObj$isChanging <- ifelse(vObj$probMtx["seqNum",]>0
							,-vObj$probMtx["isChanging",]
							, vObj$probMtx["isChanging",]
						)

	validLst[[(1+length(validLst))]] <- vObj

	# k.FLogStr(sprintf("hIdx:%3d",hIdx))
} # for( hIdx )

# --[work:validation]-------------------------

p.hSpan <- hSpan[2:length(hSpan)]
r.Flag <- pFlag[p.hSpan]
lstSpan <- 1:(length(validLst)-1)

p.val <- sapply(validLst[lstSpan],function(p){p$pred.val})

p.mFlag <- p.val==r.Flag

df <- data.frame( hIdx=p.hSpan
					,hStd=pFlag[p.hSpan]
					,p.val=p.val
					,p.mFlag=p.mFlag
				)
isChMtx <- do.call( rbind ,lapply(validLst[lstSpan],function(p){p$isChanging}) )
	# NA가 있는 경우 isChMtx 의 컬럼명 등 상태 확인 필요.

th.pos <- rep(0,nrow(isChMtx))		;th.neg <- th.pos
for( rIdx in 1:nrow(isChMtx) ){ # rIdx <- 12
	curVal <- df$hStd[rIdx]
	curVal.idx <- which(sprintf("%s",curVal)==colnames(isChMtx))[1]

	cntPos=0	;cntNeg=0
	thIndices <- which( 0<isChMtx[rIdx,] )
	cntPos <- cntPos + sum(curVal.idx==thIndices)
	cntNeg <- cntNeg + sum(curVal.idx!=thIndices)

	thIndices <- which( 0>isChMtx[rIdx,] )
	cntPos <- cntPos + sum(curVal.idx!=thIndices)
	cntNeg <- cntNeg + sum(curVal.idx==thIndices)

	th.pos[rIdx] <- cntPos
	th.neg[rIdx] <- cntNeg
}

thNum <- th.pos + th.neg
df.all <- cbind( df ,thNum ,th.pos ,th.neg )

# --<Reporting>---------------------
k.FLogStr(sprintf("match   all:%.3f  guided:%.3f  false guided:%.3f"
				,sum(df.all[,"p.mFlag"])/nrow(df.all)
				,sum(df.all[df.all[,"thNum"]>0,"p.mFlag"])/nrow(df.all)
				,sum(df.all[df.all[,"th.neg"]>0,"p.mFlag"])/nrow(df.all)
		),pConsole=T)


# dfLst <- list()
# dfLst[["FB$zh%%4diff1"]] <- df.all

cName <- attributes(dfLst)$names
hitMtx.flag <- matrix( 0 ,ncol=length(cName) ,nrow=nrow(dfLst[[1]]) )
colnames(hitMtx.flag) <- cName

hitMtx.thNum <- hitMtx.flag
hitMtx.thPos <- hitMtx.flag
hitMtx.thNeg <- hitMtx.flag

for( nIdx in colnames(hitMtx.flag) ){ # nIdx <- colnames(hitMtx.flag)[1]
	hitMtx.flag[,nIdx]	<- dfLst[[nIdx]]$p.mFlag
	hitMtx.thNum[,nIdx]	<- dfLst[[nIdx]]$thNum
	hitMtx.thPos[,nIdx]	<- dfLst[[nIdx]]$th.pos
	hitMtx.thNeg[,nIdx]	<- dfLst[[nIdx]]$th.neg
}

nFlag	<- apply(hitMtx.flag,1,sum)
nGuide	<- apply(hitMtx.thNum,1,sum)
nPos	<- apply(hitMtx.thPos,1,sum)
nNeg	<- apply(hitMtx.thNeg,1,sum)


hitMtx <- matrix( 0 ,nrow=(1+length()) ,ncol=(1+length()) )
