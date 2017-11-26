curWd <- getwd()	;setwd("..")
FB <- getFlagBank()
FB.f <- getFlagBank("./zoidHistory/ZH_Final.csv")
setwd(curWd)

source("20171116_A_H.R")

zh	<- as.matrix( FB$zh )
zhF	<- as.matrix( FB.f$zh )
testSpan <- (nrow(zh)+1):nrow(zhF)

allZoidMtx <- getAllZoid()
searchArea <- seq_len( nrow(allZoidMtx) )
batchId <- 0
searchArea <- which(batchId==searchArea%%3)

#-----------------------------------------------------
# 기존에 발생한 Zoid와 아예 동일하거나, 
#   차이의 절대값이 동일한 Zoid는 제외하자.
#   (781회 동안 단지 3쌍 발생. 그것도 abs 1 차이로.. )
#   13hr 정도 소요 예상.
surviveFlag <- rep( T ,nrow(allZoidMtx) )
tStmp <- Sys.time()
for( hIdx in searchArea ){
    for( iIdx in seq_len(nrow(allZoidMtx)) ){
        dLen <- unique(abs(allZoidMtx[iIdx,]-zhF[hIdx,]))
        if( length(dLen)==1)
            surviveFlag[iIdx] <- FALSE
    }
    tDiff <- Sys.time() - tStmp
    k.FLogStr(sprintf("hIdx:%d batchId:%d (%.1f %s)",hIdx,batchId,tDiff,units(tDiff)),pTime=F)
}

surviveObj <- list(surviveFlag=surviveFlag ,batchId=batchId)
save( surviveObj ,file=sprintf("Obj_surviveObj%d.save",batchId) )





surviveFlag.G <- rep(T,nrow(allZoidMtx))
    #-----------------------------------------------------
    # Code 간의 거리.
    # stepWidthMtx <- zhF[,2:6] - zhF[,1:5]
    # stepNum <- apply( stepWidthMtx ,1 ,function(p){length(unique(p))} )
    # table(stepNum)
    #   2   3   4   5 # 2까지는 잘라도 될 지도..
    #   6  75 360 340 
    stepWidthMtx <- allZoidMtx[,2:6] - allZoidMtx[,1:5]
    stepNum <- apply( stepWidthMtx ,1 ,function(p){length(unique(p))} )
    # sum(stepNum<=1) 은 180개 밖에 안돼는...
    surviveFlag.G[stepNum<=2] <- FALSE  # 34710


    #-----------------------------------------------------
    # remainder
    # remMtx <- zhF %% 5
    # uNum <- apply( remMtx , 1 ,function(p){length(unique(p))})
    # table(uNum)
    #    2   3   4   5 
    #    21 224 421 115 
    remMtx <- allZoidMtx %% 5
    uNum <- apply( remMtx , 1 ,function(p){length(unique(p))})
    surviveFlag.G[uNum<=1] <- FALSE # 420개... 

    #-----------------------------------------------------
    # Quotient
    # valMtx <- zhF %/% 5
    # fVal <- apply( valMtx , 1 ,function(p){length(unique(p))})
    # table(fVal)   # 2 이하를 자르자.
    #     2   3   4   5   6 
    #     1  31 239 368 142 
    valMtx <- allZoidMtx %/% 5
    fVal <- apply( valMtx , 1 ,function(p){length(unique(p))})
    surviveFlag.G[fVal<3] <- FALSE # 6560

    #-----------------------------------------------------
    # 최대-최소 폭.
    # fVal <- zhF[,6] - zhF[,1]
    # table(fVal) # 15 이하를 자르기로 하자.
    #    10 13 15 16 17 18 19 20
    #     1  3  9  2  5  6  7 10
    fVal <- allZoidMtx[,6] - allZoidMtx[,1]
    surviveFlag.G[fVal<15] <- FALSE # 65065개

# ========================================================
#   cliper 적용.
#       위에서의 모든 필터링은 surviveObj$surviveFlag에 적용된 상태.
#           (Obj_surviveObj.F2.save)
#       맨 마지막 zh가 필요한 cliper$byBase()들은 신중을 기해야..
source("20171116_A_H_cliper.R")
#   myObj <- load("Obj_surviveObj.F2.save") # surviveObj
allZoidMtx <- allZoidMtx[ surviveObj$surviveFlag ,]
surviveFlag.G <- rep(T,nrow(allZoidMtx))


    flagLst <- list()
    clpObj <- cliper.quotient( zh )
    tStmp <- Sys.time()
    flag <- clpObj$byBase( allZoidMtx ,zh ,pDebugInfo=T )
    tDiff <- Sys.time() - tStmp # 2min for 100000  전체 2.6 hr 정도 예상.
    k.FLogStr(sprintf("cliper.quotient() %.f%s",tDiff,units(tDiff)))
    flagLst[[1+length(flagLst)]] <- flag


    clpObj <- cliper.remainder( zh )
    tStmp <- Sys.time()
    flag <- clpObj$byBase( allZoidMtx ,zh ,pDebugInfo=T )
    tDiff <- Sys.time() - tStmp # 2min for 100000
    k.FLogStr(sprintf("cliper.remainder() %.f%s",tDiff,units(tDiff)))
    flagLst[[1+length(flagLst)]] <- flag


    clpObj <- cliper.stepWidth( zh )
    tStmp <- Sys.time()
    flag <- clpObj$byBase( allZoidMtx ,zh ,pDebugInfo=T )
    tDiff <- Sys.time() - tStmp # 2min for 100000
    k.FLogStr(sprintf("cliper.stepWidth() %.f%s",tDiff,units(tDiff)))
    flagLst[[1+length(flagLst)]] <- flag

    for( idx in seq_len(length(flagLst)) ){
        surviveFlag.G <- surviveFlag.G & is.na(flagLst[[idx]])
    }
    
    indices.F2 <- which(surviveObj$surviveFlag)
    surviveObj$surviveFlag[indices.F2] <- surviveFlag.G
    surviveObj$time <- Sys.time()
    surviveObj$filter[[3]] <- "cliper$byBase() quotient, remainder, stepWidth"
    surviveObj$indices.F2 <- indices.F2
    save( surviveObj ,file="Obj_surviveObj.byBaseStep1.save" )

    #-----------------------------------------------------
    # cliper.dupNum()
    #       이 녀석은 너무 시간이 오래 걸려서 분리.    
    clpObj <- cliper.dupNum( zh )
    tStmp <- Sys.time()
    flag <- clpObj$byBase( allZoidMtx ,zh ,pDebugInfo=T )
    tDiff <- Sys.time() - tStmp # 
    k.FLogStr(sprintf("cliper.dumNum() %.f%s",tDiff,units(tDiff)))
    myObj <- load("Obj_surviveObj.byBaseStep1.save")
    indices.F2 <- surviveObj$indices.F2
    surviveObj$surviveFlag.dupNum <- flag
    surviveObj$surviveFlag[indices.F2] <- is.na(flag) & surviveObj$surviveFlag[indices.F2]
    surviveObj$time <- Sys.time()
    surviveObj$filter[[4]] <- "cliper$byBase() dupNum"

    #-----------------------------------------------------
    # cliper.backStep()

    #-----------------------------------------------------
    # cliper.rebLen()



# clp.dumNum <- cliper.dupNum( zh )
# clp.dumNum$report()
# baseH <- clp.dumNum$getBaseH( zh[5:15,] )   # 임의생성.
# zoidMtx <- zh[1:4,]                         # 임의생성.
# clp.dumNum$byBase( zoidMtx )
# clp.dumNum$byLate( zoidMtx ,baseH )






diffH <- zhF[,6] - zhF[,1]
pairLst <- list()
for( aIdx in 1:(length(diffH)-1)){
    for( bIdx in (aIdx+1):length(diffH) ){
        if( diffH[aIdx]==diffH[bIdx] )
            pairLst[[1+length(pairLst)]] <- c( aIdx ,bIdx ,diffH[aIdx] )
    }
}
mtx <- do.call( rbind ,pairLst )
tbl <- table(abs(mtx[,2]-mtx[,1]))
seqObj <- k.seq(diffH)










hDepth <- 2
spoon <- rep(0,hDepth)
hSpan <- (hDepth+1):nrow(zhF)
valLst <- list()
for( hIdx in hSpan ){
    for( idx in 1:hDepth ){
        spoon[idx] <- sum( zhF[(hIdx-idx),] == zhF[hIdx,] )
    }
    valLst[[1+length(valLst)]] <- spoon
}
spoonMtx <- do.call( rbind ,valLst )

mLst <- list()
for( rIdx in 1:(nrow(spoonMtx)-1) ){
    for( rrIdx in (rIdx+1):nrow(spoonMtx) ){
        flag <- all( spoonMtx[rIdx,]==spoonMtx[rrIdx,] )
        if( flag )
            mLst[[1+length(mLst)]] <- c( rIdx, rrIdx )
    }
}
mMtx <- do.call( rbind ,mLst )
flag <- abs(mMtx[,1]-mMtx[,2]) 
table(  )

