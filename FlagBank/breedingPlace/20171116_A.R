curWd <- getwd()	;setwd("..")
FB <- getFlagBank()
FB.f <- getFlagBank("./zoidHistory/ZH_Final.csv")
setwd(curWd)

source("20171116_A_H.R")
source("20171116_A_H_cliper.R")

zh	<- as.matrix( FB$zh )
zhF	<- as.matrix( FB.f$zh )
testSpan <- (nrow(zh)+1):nrow(zhF)
allZoidMtx <- getAllZoid() # 38sec

masterObj <- list()
masterObj$clipLst <- list()

cName <- c("hIdx","rIdx")
testMtx <- matrix( 0 ,ncol=length(cName) ,nrow=length(testSpan) )
colnames(testMtx) <- cName
for( idx in seq_len(length(testSpan)) ){
    testMtx[idx,"hIdx"] <- testSpan[idx]
    fIndices <- apply( allZoidMtx ,1 ,function(p){all(p==zhF[testSpan[idx],])} )
    testMtx[idx,"rIdx"] <- which(fIndices)
}
masterObj$testHMtx <- testMtx
save( masterObj ,file="Obj_masterObj.save" )

myObj <- load("Obj_surviveObj.byBaseStep2.save")
idx1.8m <- which(surviveObj$surviveFlag)

allZoidMtx <- allZoidMtx[idx1.8m,]

zDist <- allZoidMtx[,2:6] - allZoidMtx[,1:5]
maxDist <- apply( zDist ,1 ,function(p){
    return( max(table(p)) )
})

sDist <- zhF[,2:6]-zhF[,1:5]
k <- apply( sDist ,1 ,function(p){
    tbl <- table(p)
    return( max(tbl) )
})


