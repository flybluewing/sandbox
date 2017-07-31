# 결과는 파일에 로깅됨.
t03.test.NN <- function( pFlag ,pNSize=c(12,30) ,pMaxIt=50000 ,pTestNum=2 ){

    startIdx <- round(length(pFlag)/3)
    lSpan <- startIdx:length(pFlag)

    for( bsIdx in 1:pTestNum ){

        myFLog(sprintf("[%d th]",bsIdx))
        colName <- c("hIdx" ,"outVal" ,"preVal" ,"seqNum1" ,"seqNum2" ,"seqNum3" ,"seqNum4" ,"seqNum5" ,"seqNum6")
        rMtx <- matrix( 0 ,nrow=length(lSpan) ,ncol=length(colName) )
        colnames(rMtx) <- colName

        for( lIdx in lSpan ){
            # lIdx <- 25
            sInfo <- t03.seq( pFlag[1:(lIdx-1)] )
            nR <- nrow(sInfo$seqCntMtx)
            mtx <- sInfo$seqCntMtx[(nR-(6-1)):nR,]	# row가 충분치 않은 경우에 대한 검토필요
            seqNum <- mtx[,"cnt"]*ifelse(mtx[,"val"]==1,1,-1)
            rMtx[(lIdx-lSpan[1])+1,] <- c( lIdx ,pFlag[lIdx] ,pFlag[lIdx-1] ,seqNum )
        }

        nR <- nrow(rMtx)
        testIdx <- sort(sample( 1:nR ,10 ,F ))
        inputCol <- c("preVal" ,"seqNum1" ,"seqNum2" ,"seqNum3" ,"seqNum4" ,"seqNum5" ,"seqNum6")
        outputCol <- c("outVal")
        myFLog(testIdx)
        myFLog(rMtx[testIdx,"outVal"])

        fitMLP <- mlp( x=rMtx[-testIdx,inputCol] ,y=rMtx[-testIdx,outputCol] 
					,size=pNSize ,maxit=pMaxIt
                        ,initFunc="Ramdomize_Weights" ,initFuncParams=c(-0.3,0.3)
                        ,learnFunc="Std_Backpropagation" ,learnFuncParams=c(0.2,0)
                        ,updateFunc="Topological_Order" ,updateFuncParams=c(0)
                        ,hiddenActFunc="Act_Logistic" ,shufflePatterns=T
                        ,linOut=T
                    )

        output <- predict( fitMLP ,rMtx[-testIdx,inputCol] )
        sprintf( "Result: %d of %d succeed." ,sum(round(output)==rMtx[-testIdx,outputCol]) ,length(output) )
        misHit <- which(round(output) != rMtx[-testIdx,"outVal"])
        #	for( idx in misHit ){	print( rMtx[(idx-3):(idx+3),"outVal"] )	}

        output <- predict( fitMLP ,rMtx[testIdx,inputCol] )
        perfTbl <- table(round(output),rMtx[testIdx,outputCol],dnn=c("predict","real"))
        myFLog(perfTbl)

    }

} # t03.test.NN

# --------------------------------------
# 테스트용 함수들.
# --------------------------------------

t03.test.sample002 <- function( pSize=40 ){
	# 불규칙한 0, 규칙적인 연속의 1
	rV <- c(1,1)
	zeroSeq <- sample( 1:7 ,pSize ,replace=T )
	for( idx in 1:length(zeroSeq) ){
		rV <- c( rV, rep(0,zeroSeq[idx]) ,c(1,1) )
	}
	return(rV)
}

t03.test.sample003 <- function( pSize=40 ){
	# 불규칙한 0, 규칙적 연속의 1 (2연속, 4연속, 3연속이 규칙적으로 반복.)
	zeroMtx <- matrix( sample(1:7,pSize*3,replace=T),nrow=pSize ,ncol=3 )
	
	rV <- 0
	for( idx in 1:pSize ){
		rV <- c( rV
					,c(1,1)		,rep(0,zeroMtx[idx,1])
					,c(1,1,1,1)	,rep(0,zeroMtx[idx,2])
					,c(1,1,1)	,rep(0,zeroMtx[idx,3])
				)
	}
	return(rV)
}

t03.test.sample004 <- function( pSize=5 ,pProb=0.5 ){
	# 불규칙한 0과 1의 혼재.
	# - 이전값이 1인 경우에 대해서만 train & test
	# - 불규칙/규칙이 반복되는 1에 대해서도 테스트 필요.
	rV <- sample( 0:1 ,10 ,replace=T ,prob=c((1-pProb),pProb))
	for( idx in 1:pSize ){
		rV <- c( rV ,sample( 0:1 ,10 ,replace=T ,prob=c((1-pProb),pProb)) )
	}
	return(rV)
}