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


flt.jump <- function( pZh ){

    rObj <- list( idStr="jump" )

    getJumpMtx <- function( pZh ){

        codeMtx <- pZh[2:nrow(pZh),] - pZh[1:(nrow(pZh)-1),]
        # tLen <- apply( codeMtx ,1 ,function(p){length(unique(p))} )
        # table(tLen)      3   4   5   6 # 3이하는 제거할까?
        #                 10  98 332 341 

        sJumpLst <- list() # Same Jump 
        for( rIdx in 1:nrow(codeMtx) ){

        }

        maxIdx <- apply( codeMtx ,1 ,which.max )

        allStepMtx <- matrix( 0 ,nrow=nrow(codeMtx) ,ncol=3 )
        for( rIdx in 1:nrow(codeMtx) ){
            allStepMtx[rIdx,1] <- codeMtx[rIdx ,maxIdx[rIdx] ]
            allStepMtx[rIdx,2] <- pZh[ rIdx ,maxIdx[rIdx] ]
            allStepMtx[rIdx,3] <- maxIdx[rIdx]
        }
        colnames(allStepMtx) <- c("step","val","col")

        step <- sort(unique(allStepMtx[,"step"]))
        cName <- c("maxStep","val")
        stepMtx <- matrix(0,nrow=length(step),ncol=length(cName))
        colnames( stepMtx ) <- cName
        stepMtx[,"maxStep"] <- step
        for( rIdx in 1:nrow(allStepMtx) ){
            idx <- which( allStepMtx[rIdx,1]==stepMtx[,"maxStep"] )
            stepMtx[idx,"val"] <- allStepMtx[rIdx,"val"]
        }

        return( stepMtx )
    }

    rObj$stepMtx <- getStepMtx( pZh )

    rObj$byLate <- function( pZoidMtx ,pDebugInfo=F ){

        stepMtx <- pZoidMtx[,2:6] - pZoidMtx[,1:5]

        filted <- rep( 0 ,nrow(stepMtx) )
        for( rIdx in 1:nrow(stepMtx) ){
            maxCol <- which.max( stepMtx[rIdx,] )

            hrIdx <- which( rObj$stepMtx[,1] == stepMtx[rIdx,maxCol] )            
            if( 0==length(hrIdx) ){
                filted[rIdx] <- NA
                next
            }

            if( rObj$stepMtx[hrIdx,"val"]==pZoidMtx[rIdx,maxCol] ){
                filted[rIdx] <- stepMtx[rIdx,maxCol]
            } else {
                filted[rIdx] <- NA
            }
        }

        if( pDebugInfo ){   return( filted)
        } else {
            return( is.na(filted) )
        }
    }

    rObj$report <- function( pZh ){
        codeMtx <- pZh[2:nrow(pZh),] - pZh[1:(nrow(pZh)-1),]
        # 최대값에 대한 반복여부로 테스트 하는 것도 검토 요.
        # tLen <- apply( codeMtx ,1 ,function(p){length(unique(p))} )
        # table(tLen)      3   4   5   6 # 3이하는 제거할까?
        #                 10  98 332 341 

        sJumpLst <- list() # Same Jump 
        for( rIdx in 1:nrow(codeMtx) ){
            
        }


    }

    return( rObj )
} # flt.jump()



