# 20171116_B.R
myObj <- load("Obj_masterObj.save") # masterObj$testHMtx[,"rIdx"]
myObj <- load("Obj_clipedLst005.0.save")
finalFlag <- clipedLst[[5]]$finalFlag
initIndices <- clipedLst[[5]]$indices

indices.pool <- initIndices
indices.h <- masterObj$testHMtx[,"rIdx"]
indices.all <- sort(union( indices.pool ,indices.h ))
indices.flag <- finalFlag[indices.all]
allZoidMtx <- getAllZoid() # 38sec
allZoidMtx <- allZoidMtx[indices.all,]


curWd <- getwd()	;setwd("..")
FB <- getFlagBank()
FB.f <- getFlagBank("./zoidHistory/ZH_Final.csv")
setwd(curWd)
zh	<- as.matrix( FB$zh )
zhF	<- as.matrix( FB.f$zh )
testSpan <- (nrow(zh)+1):nrow(zhF)

deskObj <- list( zhF=zhF ,testSpan=testSpan ,allZoidMtx=allZoidMtx )
deskObj$indices.pool<- indices.pool
deskObj$indices.h   <- indices.h  
deskObj$indices.all <- indices.all
deskObj$indices.flag<- indices.flag

deskObj$indices.zoidMtx <- # testSpan에 대응되는 allZoidMtx 내에서의 좌표.
    sapply( deskObj$indices.h ,function(p){which(deskObj$indices.all==p)})

save( deskObj ,file="Obj_deskObj.save")

