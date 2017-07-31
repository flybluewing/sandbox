require( RSNNS )
source("03_H.R")
source("03_NN_test_H.R")

vals <- c(   1 ,1 ,1
			,0 ,0 ,1
			,1 ,0 ,0
			,0 ,1 ,0
			)
			
testMtx <- matrix( vals ,ncol=3 ,byrow=T )
colnames( testMtx ) <- c("x1","x2","y1")

fitMLP <- mlp( x=testMtx[,1:2] ,y=testMtx[,3] ,size=c(12,3) ,maxit=10000 ,initFunc="Ramdomize_Weights" ,initFuncParams=c(-0.3,0.3)
				,learnFunc="Std_Backpropagation" ,learnFuncParams=c(0.2,0)
				,updateFunc="Topological_Order" ,updateFuncParams=c(0)
				,hiddenActFunc="Act_Logistic" ,shufflePatterns=T
				,linOut=T
			)

predict( fitMLP ,testMtx[,1:2] )

fitMLP <- mlp( x=matrix(0:1,ncol=1) ,y=matrix(1:0,ncol=1) ,size=c(3,3) ,maxit=1000 ,initFunc="Ramdomize_Weights" ,initFuncParams=c(-0.3,0.3)
				,learnFunc="Std_Backpropagation" ,learnFuncParams=c(0.2,0)
				,updateFunc="Topological_Order" ,updateFuncParams=c(0)
				,hiddenActFunc="Act_Logistic" ,shufflePatterns=T
				,linOut=T
			)
predict( fitMLP ,matrix(0:1,ncol=1) )

# --------------------------------------
# 수렴 여부를 확인해야 할 듯 하다.
# --------------------------------------


# --------------------------------------
# 1차 실험 : 규칙적 패턴
# --------------------------------------

vals <- c(0,1,1,0,1,1,0,1,1,0,1,1,0,1,1)
vals <- c(vals,vals,vals,vals,vals)

pFlag <- vals
# seqMtx <- t03.seq(pFlag)

startIdx <- round(length(pFlag)/3)
lSpan <- startIdx:length(pFlag)

colName <- c("hIdx" ,"outVal" ,"seqNum1" ,"seqNum2" ,"seqNum3" ,"seqNum4" ,"seqNum5" ,"seqNum6")
rMtx <- matrix( 0 ,nrow=length(lSpan) ,ncol=length(colName) )
colnames(rMtx) <- colName

for( lIdx in lSpan ){
    # lIdx <- 25
    sInfo <- t03.seq( pFlag[1:(lIdx-1)] )
    nR <- nrow(sInfo$seqCntMtx)
    mtx <- sInfo$seqCntMtx[(nR-(6-1)):nR,]	# row가 충분치 않은 경우에 대한 검토필요
    seqNum <- mtx[,"cnt"]*ifelse(mtx[,"val"]==1,1,-1)
    rMtx[(lIdx-lSpan[1])+1,] <- c( lIdx, pFlag[lIdx], seqNum )
}

nR <- nrow(rMtx)
testIdx <- sort(sample( 1:nR ,5 ,F ))
inputCol <- c("seqNum1" ,"seqNum2" ,"seqNum3" ,"seqNum4" ,"seqNum5" ,"seqNum6")
outputCol <- c("outVal")

fitMLP <- mlp( x=rMtx[-testIdx,inputCol] ,y=rMtx[-testIdx,outputCol] ,size=c(12,3) ,maxit=10000 
				,initFunc="Ramdomize_Weights" ,initFuncParams=c(-0.3,0.3)
				,learnFunc="Std_Backpropagation" ,learnFuncParams=c(0.2,0)
				,updateFunc="Topological_Order" ,updateFuncParams=c(0)
				,hiddenActFunc="Act_Logistic" ,shufflePatterns=T
				,linOut=T
			)

output <- predict( fitMLP ,rMtx[testIdx,inputCol] )
round(output)

# --------------------------------------
# 2차 실험 : 1의 규칙적 연속, 0은 불규칙(0~n)
# --------------------------------------
pFlag <- t03.test.sample002()
t03.test.NN( pFlag ,pNSize=c(5,8) ,pMaxIt=50 ,pTestNum=5 )

pFlag <- t03.test.sample003()
t03.test.NN( pFlag ,pNSize=c(5,8) ,pMaxIt=50 ,pTestNum=5 )

# --------------------------------------
# 3차 실험 : 불규칙 연속
# --------------------------------------

pFlag <- t03.test.sample004()
t03.test.NN( pFlag ,pNSize=c(5,8) ,pMaxIt=50 ,pTestNum=5 )

pFlag <- tGlobal$rawZH[,1] %% 2
nOptMtx <- matrix( nrow=0 ,ncol=3 )
colnames(nOptMtx) <- c("pMaxIt" ,"nCol1" ,"nCol2")
nOptMtx <- rbind( nOptMtx ,c(    50, 5, 8) )
nOptMtx <- rbind( nOptMtx ,c(   500, 5, 8) )
nOptMtx <- rbind( nOptMtx ,c(  1000, 5, 8) )
nOptMtx <- rbind( nOptMtx ,c(  5000,10,16) )
nOptMtx <- rbind( nOptMtx ,c( 50000,20,32) )
nOptMtx <- rbind( nOptMtx ,c(100000,20,32) )

rstLst <- list()
myFLog(sprintf("start exprement %d times",nrow(nOptMtx)))
for( optIdx in 1:nrow(nOptMtx) ){
	myRst <- t03.NN( pFlag ,pNSize=nOptMtx[optIdx,2:3] ,pMaxIt=nOptMtx[optIdx,1] ,pTestNum=100 ,pLogId=optIdx )
	rstLst[[as.character(optIdx)]] <- myRst
}

for( nIdx in attributes(rstLst)$name ){
	# nIdx <- "1"
	testHitMtx <- rstLst[[nIdx]]$testHitMtx
	testIdxMtx <- rstLst[[nIdx]]$testIdxMtx
	nR <- nrow(rstLst[[nIdx]]$rMtx)

	# ------------------------------------------------------
	# 인덱스틀 별 명중률.
	hitIdc <- NULL;				missIdc <- NULL
	for( rIdx in 1:nrow(testHitMtx) ){
		hitIdc	<- c( hitIdc	,testIdxMtx[rIdx,testHitMtx[rIdx,]==1] )
		missIdc	<- c( missIdc	,testIdxMtx[rIdx,testHitMtx[rIdx,]==0] )
	}
	
	hitTbl <- table(hitIdc);	missTbl <- table(missIdc)
	
	allIdc <- rep(0,nR); names(allIdc) <- as.character(1:nR)
	allIdc[names(hitTbl)] <- hitTbl
	hitTbl <- allIdc
	
	allIdc <- rep(0,nR); names(allIdc) <- as.character(1:nR)
	allIdc[names(missTbl)] <- missTbl
	missTbl <- allIdc

	totTbl <- hitTbl+missTbl
	totTbl[totTbl==0] <- NA		# 테스트 자체가 없었던 인덱스	
	rstLst[[nIdx]]$hitRate <- hitTbl/totTbl
	
	
} # for(nIdx)

save( rstLst ,file="Obj_rstLst.save" ,compress="bzip2")
# load("Obj_rstLst.save")



nIdx <- "1"
hitRate <- rstLst[[nIdx]]$hitRate
hit.order <- order(hitRate)
cNames <- c("preVal" ,"seqNum1" ,"seqNum2" ,"seqNum3" ,"seqNum4" ,"seqNum5" ,"seqNum6")
trMtx <- rstLst[[nIdx]]$rMtx[,cNames]

rst.km <- kmeans( trMtx ,10 ,nstart=20 )
# plot( hitRate[hit.order] ,col=c(rst.km$cluster[hit.order]+1) ,pch="+" )

rst.hclust <- hclust( dist(trMtx) ,method="complete" )
cluster <- cutree( rst.hclust ,10 )
# plot( hitRate[hit.order] ,col=c(cluster[hit.order]+1) )
rst.hclust <- hclust( dist(trMtx), method="average" )
cluster <- cutree( rst.hclust , 8 )
# plot( hitRate[hit.order] ,col=c(cluster[hit.order]+1))


# ----------------------------------------------------------------------
#	Unsupervised classfy try.
# ----------------------------------------------------------------------



# ----------------------------------------------------------------------
#	Supervised classfy try.
# ----------------------------------------------------------------------

