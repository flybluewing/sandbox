curWd <- getwd()	;setwd("..")
FB <- getFlagBank()
FB.f <- getFlagBank("./zoidHistory/ZH_Final.csv")
setwd(curWd)

source("20171116_A_H.R")
source("20171116_A_H_cliper.R")

zh	<- as.matrix( FB$zh )
zhF	<- as.matrix( FB.f$zh )
testSpan <- (nrow(zh)+1):nrow(zhF)

clpObj <- cliper.quotient( zh ,pSanc=10 )
baseRst <- rep( 0 ,length(testSpan) )
for( idx in seq_len(length(testSpan)) ){
	tIdx <- testSpan[idx]
	baseRst[idx] <- clpObj$byBase( zhF[tIdx,,drop=F] ,zhF[1:(tIdx-1),] ,pDebugInfo=T )
}
    # table(baseRst,useNA="ifany")

lateRst <- rep( 0 ,length(testSpan))
for( idx in seq_len(length(testSpan)) ){
    tIdx <- testSpan[idx]
    lateRst[idx] <- clpObj$byLate( zhF[tIdx,,drop=F]
                                ,zhF[1:(tIdx-1),] ,pDebugInfo=T 
                            )
}



zh	<- as.matrix( FB$zh )
zhF	<- as.matrix( FB.f$zh )
testSpan <- (nrow(zh)+1):nrow(zhF)

clpObj <- cliper.dupNum( zh ,pSanc=3 )

baseH <- clpObj$getBaseH( as.matrix(FB.f$zh) )
testSpan <- (1+nrow(FB$zh)):nrow(baseH)
clipFlag <- clpObj$byBase( baseH[testSpan,] )
    # table(clipFlag)
clipIdx <- rep(0,length(testSpan))
for( idx in seq_len(length(testSpan)) ){
    tIdx <- testSpan[idx]
    curCodeMtx <- baseH[tIdx,,drop=F]
    clipIdx[idx] <- 
        clpObj$byLate( curCodeMtx ,baseH[1:(tIdx-1),] ,pDebugInfo=T )
}

pZoidMtx <- curCodeMtx
pBaseH <- baseH[1:(tIdx-1),]


