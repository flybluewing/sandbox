# 20171116_B.R
# source("20171116_B_init.R")

# ===========================================================================
source("20171116_A_H.R")
source("20171116_A_H_cliper.R")
source("20171116_B_H.R")
myObj <- load( "Obj_deskObj.save" )
#   deskObj
#    - zhF testSpan allZoidMtx 
#    - indices.pool indices.h indices.all indices.flag indices.zoidMtx


zhF <- deskObj$zhF
allZoidMtx <- deskObj$allZoidMtx

tStmp <- Sys.time()
survLst <- list()
for( idx in 1:length(deskObj$testSpan) ){
    hIdx <- deskObj$testSpan[idx]
    fltObj <- flt.sStepMax( zhF[1:(hIdx-1),] )    # 10min, 85개 생존.

    sObj <- list( stdIdx= deskObj$indices.zoidMtx[idx] )
    sObj$flag <- fltObj$byLate( allZoidMtx )
    sObj$stdSurvive <- sObj$flag[ sObj$stdIdx ]
    sObj$rmCnt <- sum(!sObj$flag)
    survLst[[1+length(survLst)]] <- sObj
}
tDiff <- Sys.time() - tStmp

k <- sapply( survLst ,function(p){p$stdSurvive})


for( idx in 1:length(deskObj$testSpan) ){
    hIdx <- deskObj$testSpan[idx]
    fltObj <- flt.rebLen( zhF[1:(hIdx-1),] )    # 21min, 83개 생존.

    sObj <- list( stdIdx= deskObj$indices.zoidMtx[idx] )
    sObj$flag <- fltObj$byLate( allZoidMtx )
    sObj$stdSurvive <- sObj$flag[ sObj$stdIdx ]
    sObj$rmCnt <- sum(!sObj$flag)
    survLst[[1+length(survLst)]] <- sObj
}

survLst <- list()
for( idx in 1:length(deskObj$testSpan) ){
    hIdx <- deskObj$testSpan[idx]
    fltObj <- flt.rebLen2( zhF[1:(hIdx-1),] )    # 14min, 77개 생존.

    sObj <- list( stdIdx= deskObj$indices.zoidMtx[idx] )
    sObj$flag <- fltObj$byLate( allZoidMtx )
    sObj$stdSurvive <- sObj$flag[ sObj$stdIdx ]
    sObj$rmCnt <- sum(!sObj$flag)
    survLst[[1+length(survLst)]] <- sObj
}

survLst <- list()
for( idx in 1:length(deskObj$testSpan) ){
    hIdx <- deskObj$testSpan[idx]
    fltObj <- flt.width( zhF[1:(hIdx-1),] ,pBuf=3 )
    
    sObj <- list( stdIdx= deskObj$indices.zoidMtx[idx] )
    sObj$flag <- fltObj$byLate( allZoidMtx )
    sObj$stdSurvive <- sObj$flag[ sObj$stdIdx ]
    sObj$rmCnt <- sum(!sObj$flag)
    survLst[[1+length(survLst)]] <- sObj
}


survLst <- list()
for( idx in 1:length(deskObj$testSpan) ){
    hIdx <- deskObj$testSpan[idx]
    fltObj <- flt.reb( zhF[1:(hIdx-1),]  )
    
    sObj <- list( stdIdx= deskObj$indices.zoidMtx[idx] )
    sObj$flag <- fltObj$byLate( allZoidMtx )
    sObj$stdSurvive <- sObj$flag[ sObj$stdIdx ]
    sObj$rmCnt <- sum(!sObj$flag)
    survLst[[1+length(survLst)]] <- sObj
}

# 평가 : 정답들보다 상위에, 정답들 중간에, 정답들 하위에 속한 후보 수 들의 변화를 봐야 할 듯.



pZh <- zhF

