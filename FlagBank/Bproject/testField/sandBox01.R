lastH <- 880
testSpan <- 200:lastH

dMatMtx <- matrix( 0 ,nrow=length(testSpan) ,ncol=6 )

for( idx in 1:length(testSpan) ){
    hIdx <- testSpan[idx]
    stdZoid <- gEnv$zhF[hIdx,]
    for( cIdx in 1:6 ){
        dMatMtx[idx,cIdx] <- stdZoid[cIdx] %in% gEnv$zhF[hIdx-1:2,cIdx]
    }
}

rownames(dMatMtx) <- testSpan



matCnt <- apply( dMatMtx ,1 ,sum)
rebCnt <- sapply( 2:nrow(dMatMtx) ,function(rIdx){ sum(dMatMtx[rIdx,]&dMatMtx[rIdx-1,]) })
rebCnt <- c( 0 ,rebCnt )

anaMtx <- t(rbind( matCnt ,rebCnt ))



#   실험 결론 : matCnt는 3이내로. 존재하는 예외 하나는 sfc 9 레벨.
#               rebCnt는 1이내료. 존재하는 예외 2개는 sfc 8 레벨, 4레벨
