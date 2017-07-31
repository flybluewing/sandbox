
# pFlag <- c( pFlag ,t03.test.sample004() )

rstLst <- list()
rstLst[["1"]] <- t03.NN( pFlag ,pNSize=c(5,8) ,pMaxIt=500 ,pTestNum=1000 )
myRst <- rstLst[["1"]]

# 동일 Row 확인
cNames <- c("preVal" ,"seqNum1" ,"seqNum2" ,"seqNum3" ,"seqNum4" ,"seqNum5" ,"seqNum6")
srLst <- sameRow( myRst$rMtx ,cNames )

cNames <- c("idx","totNum","hitNum")
mtx <- matrix(0 ,ncol=length(cNames) ,nrow=length(srLst) )
colnames( mtx ) <- cNames
rownames( mtx ) <- attributes(srLst)$names
for( aIdx in attributes(srLst)$names ){
	# aIdx <- "11"
	outVal <- myRst$rMtx[srLst[[aIdx]],"outVal"]
	# priNum <- max(table(outVal))	#많은쪽의 수.
	mtx[aIdx,] <- c( as.integer(aIdx) ,length(outVal) ,max(table(outVal)) )
}

missIdx <- mtx[ mtx[,"totNum"]!=mtx[,"hitNum"], ]


testHit <- as.vector(myRst$testHitMtx)


testIdx <- as.vector(myRst$testIdxMtx)


m1 <- matrix(1:6, ncol=2)
m1==c(2,4)
