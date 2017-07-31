
t03.seqTrainSet <- function( pFlag ){
		seqMtx <- t03.seq(pFlag)
	}

t03.seq <- function( pFlag ){
    
    flagL <- length(pFlag)

    # -[hSeqCntMtx]---------------------------------------------
    # hSeqCntMtx[1,2]는 항상 0임. 제일 첫번째 원소는 연속이 없으므로
    hSeqCntMtx <- matrix( 0 ,ncol=2 ,nrow=flagL )
    rownames(hSeqCntMtx) <- as.character(1:flagL)
    colnames(hSeqCntMtx) <- c("val","seqCnt")
    hSeqCntMtx[,1] <- pFlag
    
    seqIdx <- 2:flagL
    for( dIdx in 1:(flagL-1) ){
        seqIdx <- seqIdx[0<(seqIdx-dIdx)]
        seqIdx <- seqIdx[ pFlag[seqIdx] == pFlag[seqIdx-dIdx] ]
        if( 0==length(seqIdx) ){
            break
        } else {
            hSeqCntMtx[seqIdx,2] <- hSeqCntMtx[seqIdx,2]+1
        }
    }

    # -[seqCntMtx]---------------------------------------------
    seqEndIdx <- which(hSeqCntMtx[,"seqCnt"]==0)   # 연속이 "끝"난 지점들
    
    seqCntMtx <- matrix( 1 ,ncol=3 ,nrow=length(seqEndIdx) )
    colnames(seqCntMtx) <- c("hIdx","val","cnt")

    seqCntMtx[,"hIdx"] <- seqEndIdx
    seqCntMtx[,"val"] <- hSeqCntMtx[seqEndIdx,"val"]
    for( dIdx in 1:(flagL-1) ){
        seqEndIdx <- seqEndIdx[flagL>=(seqEndIdx+dIdx)]
        seqEndIdx <- seqEndIdx[ pFlag[seqEndIdx]==pFlag[seqEndIdx+dIdx] ]
        if( 0==length(seqEndIdx) ){
            break
        } else {
            mtxIdx <- match( seqEndIdx, seqCntMtx[,"hIdx"] )
            seqCntMtx[mtxIdx,"cnt"] <- seqCntMtx[mtxIdx,"cnt"]+1
        }
    }

    rLst <- list( hSeqCntMtx=hSeqCntMtx, seqCntMtx=seqCntMtx )
    return( rLst )
}


# 결과는 파일에 로깅됨.
t03.NN <- function( pFlag ,pTestIdx=NULL ,pNSize=c(12,30) ,pMaxIt=50000 ,pTestNum=2 ,pLogId=NULL ){

    startIdx <- round(length(pFlag)/3)
    lSpan <- startIdx:length(pFlag)
    testIdxNum <- ifelse( is.null(pTestIdx) ,10 ,length(pTestIdx) )

    colName <- c("hIdx" ,"outVal" ,"preVal" ,"seqNum1" ,"seqNum2" ,"seqNum3" ,"seqNum4" ,"seqNum5" ,"seqNum6")
    rMtx <- matrix( 0 ,nrow=length(lSpan) ,ncol=length(colName) )
    colnames(rMtx) <- colName

    for( lIdx in lSpan ){
        # lIdx <- 25
        sInfo <- t03.seq( pFlag[1:(lIdx-1)] )
        nR <- nrow(sInfo$seqCntMtx)
		if( nR < 6 ){
			printf( "row number of sInfo$seqCntMtx is less than 6.(not available data)")
		}
        mtx <- sInfo$seqCntMtx[(nR-(6-1)):nR,]	# row가 충분치 않은 경우에 대한 검토필요
        seqNum <- mtx[,"cnt"]*ifelse(mtx[,"val"]==1,1,-1)
        rMtx[(lIdx-lSpan[1])+1,] <- c( lIdx ,pFlag[lIdx] ,pFlag[lIdx-1] ,seqNum )
    }

	rLst <- list( nSize=pNSize ,maxIt=pMaxIt )
    rLst$testIdxMtx <- matrix( 0, nrow=pTestNum, ncol=testIdxNum )
    rLst$testHitMtx <- matrix( 0, nrow=pTestNum, ncol=testIdxNum )
    rLst$testRstMtx <- matrix( 0, nrow=pTestNum, ncol=4 )
    colnames( rLst$testRstMtx ) <- c( "testNum" ,"testMissNum" ,"trainNum" ,"trainMissNum" )
        # 각각 테스트 명중률, 훈련 샘플 수, 훈련에 실패한 샘플 수.

    for( bsIdx in 1:pTestNum ){

        myFLog(sprintf("[(ID:%s)%d th]",ifelse(is.null(pLogId),"",pLogId),bsIdx))

        nR <- nrow(rMtx)
		if( is.null(pTestIdx) ){
			testIdx <- sort(sample( 1:nR ,testIdxNum ,F ))
		} else {
			testIdx <- pTestIdx
		}
        rLst$testIdxMtx[bsIdx,] <- testIdx
		
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
        myFLog(sprintf( "Result: %d of %d succeed." ,sum(round(output)==rMtx[-testIdx,outputCol]) ,length(output) ))
        #   misHit <- which(round(output) != rMtx[-testIdx,"outVal"])
        #	for( idx in misHit ){	print( rMtx[(idx-3):(idx+3),"outVal"] )	}
        rLst$testRstMtx[bsIdx,"trainNum"]       <- length(output)
        rLst$testRstMtx[bsIdx,"trainMissNum"]   <- sum(round(output)!=rMtx[-testIdx,outputCol])

        output <- predict( fitMLP ,rMtx[testIdx,inputCol] )
        perfTbl <- table(round(output),rMtx[testIdx,outputCol],dnn=c("predict","real"))
        rLst$testHitMtx[bsIdx,] <- ifelse( round(output)==rMtx[testIdx,outputCol] ,1 ,0 )
        rLst$testRstMtx[bsIdx,"testNum"]        <- length(testIdx) 
        rLst$testRstMtx[bsIdx,"testMissNum"]    <- sum(round(output)!=rMtx[testIdx,outputCol])

        myFLog(perfTbl)
        myFLog(sprintf( "Assess : %.2f" ,sum(round(output)==rMtx[testIdx,outputCol])/length(output) ))

    }

    rLst$rMtx       <- rMtx
    return( rLst )

} # t03.NN


