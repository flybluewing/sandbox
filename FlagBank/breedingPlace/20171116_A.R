curWd <- getwd()	;setwd("..")
FB <- getFlagBank()
FB.f <- getFlagBank("./zoidHistory/ZH_Final.csv")
setwd(curWd)

source("20171116_A_H.R")
source("20171116_A_H_cliper.R")

zh <- as.matrix(FB$zh)

clpObj <- cliper.dupNum( zh )

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


