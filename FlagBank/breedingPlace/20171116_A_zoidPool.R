curWd <- getwd()	;setwd("..")
FB <- getFlagBank()
FB.f <- getFlagBank("./zoidHistory/ZH_Final.csv")
setwd(curWd)

source("20171116_A_H.R")
source("20171116_A_H_cliper.R")

zh	<- as.matrix( FB$zh )
zhF	<- as.matrix( FB.f$zh )
allZoidMtx <- getAllZoid() # 38sec

masterObj <- list()
masterObj$clipLst <- list()

testSpan <- (nrow(zh)+1):nrow(zhF)
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



clipedLst <- list()
# clipedObj <- list( flag=logical(0) ,idStr="" ,srchInx=integer(0) )

#==================================================================================
# Remove creepy zoid
#----------------------------------------------------------------------------------
filtLst <- list()

    tStmp <- Sys.time()
    #-----------------------------------------------------
    # Code 간의 거리가 몇 가지나 되는지.
    # stepWidthMtx <- zhF[,2:6] - zhF[,1:5]
    # stepNum <- apply( stepWidthMtx ,1 ,function(p){length(unique(p))} )
    # table(stepNum)
    #   2   3   4   5 # 2까지는 잘라도 될 지도..
    #   6  75 360 340 
    stepWidthMtx <- allZoidMtx[,2:6] - allZoidMtx[,1:5]
    flagVal <- apply( stepWidthMtx ,1 ,function(p){length(unique(p))} )
    # sum(flagVal<=1) 은 180개 밖에 안돼는...
    filtObj <- list( idStr="stepWidthNum more than 2" )
    filtObj$flag <- flagVal>2   # 34710
    filtLst[[1+length(filtLst)]] <- filtObj
    tDiff <- Sys.time() - tStmp
    k.FLogStr(sprintf("%sth filtObj %.1f %s",length(filtLst)
            ,tDiff ,units(tDiff)
        ))

    #-----------------------------------------------------
    # remainder
    # remMtx <- zhF %% 5
    # uNum <- apply( remMtx , 1 ,function(p){length(unique(p))})
    # table(uNum)
    #    2   3   4   5 
    #    21 224 421 115 
    codeMtx <- allZoidMtx %% 5
    flagVal <- apply( codeMtx , 1 ,function(p){length(unique(p))})
    filtObj <- list( idStr="remainder num more than 1" )
    filtObj$flag <- flagVal>1   # 420...
    filtLst[[1+length(filtLst)]] <- filtObj
    tDiff <- Sys.time() - tStmp
    k.FLogStr(sprintf("%sth filtObj %.1f %s",length(filtLst)
            ,tDiff ,units(tDiff)
        ))
    
    #-----------------------------------------------------
    # Quotient
    # valMtx <- zhF %/% 5
    # fVal <- apply( valMtx , 1 ,function(p){length(unique(p))})
    # table(fVal)   # 2 이하를 자르자.
    #     2   3   4   5   6 
    #     1  31 239 368 142 
    codeMtx <- allZoidMtx %/% 5
    flagVal <- apply( codeMtx , 1 ,function(p){length(unique(p))})
    filtObj <- list( idStr="Quotient num more than 2" )
    filtObj$flag <- flagVal > 2   # 6560
    filtLst[[1+length(filtLst)]] <- filtObj
    tDiff <- Sys.time() - tStmp
    k.FLogStr(sprintf("%sth filtObj %.1f %s",length(filtLst)
            ,tDiff ,units(tDiff)
        ))

    #-----------------------------------------------------
    # 최대-최소 폭.
    # fVal <- zhF[,6] - zhF[,1]
    # table(fVal) # 15 이하를 자르기로 하자.
    #    10 13 15 16 17 18 19 20
    #     1  3  9  2  5  6  7 10
    flagVal <- allZoidMtx[,6] - allZoidMtx[,1]
    filtObj <- list( idStr="width less then 15" )
    filtObj$flag <- flagVal > 14   # 65065개
    filtLst[[1+length(filtLst)]] <- filtObj
    tDiff <- Sys.time() - tStmp
    k.FLogStr(sprintf("%sth filtObj %.1f %s",length(filtLst)
            ,tDiff ,units(tDiff)
        ))
    
    #-----------------------------------------------------
    # 코드 간 거리가 동일반복
    # distMtx <- zhF[,2:6] - zhF[,1:5]
    # codeMtx <- distMtx[,1:4]==distMtx[,2:5]
    # flagVal <- apply( codeMtx ,1 ,function(p){sum(p)})
    # table(flagVal)
    #      0   1   2   3
    #    596 164  21   1
    distMtx <- allZoidMtx[,2:6] - allZoidMtx[,1:5]
    # codeMtx <- distMtx[,1:3]==distMtx[,2:4] & distMtx[,1:3]==distMtx[,3:5]
    codeMtx <- distMtx[,1:4]==distMtx[,2:5]
        # True이면 2번 이상 동일구간 증가
    flagVal <- apply( codeMtx ,1 ,function(p){sum(p)})
    filtObj <- list( idStr="width sequence no more than 2" )
    filtObj$flag <- flagVal == 0   # 129070
    filtLst[[1+length(filtLst)]] <- filtObj
    tDiff <- Sys.time() - tStmp
    k.FLogStr(sprintf("%sth filtObj %.1f %s",length(filtLst)
            ,tDiff ,units(tDiff)
        ))

    #-----------------------------------------------------
    # Quotient 하나에 너무 많이 몰린 것.
    # codeMtx <- zhF %/% 10
    # flagVal <- apply( codeMtx ,1 ,function(p){ return( max(table(p)) ) })
    # table(flagVal)
    #     2   3   4   5 
    #   451 283  46   2 
    codeMtx <- allZoidMtx %/% 10
    flagVal <- apply( codeMtx , 1 ,function(p){ return( max(table(p)) ) })
    filtObj <- list( idStr="a quotient has dna more than 4" )
    filtObj$flag <- flagVal < 4   # 497290
    filtLst[[1+length(filtLst)]] <- filtObj
    tDiff <- Sys.time() - tStmp
    k.FLogStr(sprintf("%sth filtObj %.1f %s",length(filtLst)
            ,tDiff ,units(tDiff)
        ))

    #-----------------------------------------------------
    # clipedObj
    finalFlag <- rep(T,nrow(allZoidMtx))
    for( idx in seq_len(length(filtLst)) ){
        finalFlag <- finalFlag & filtLst[[idx]]$flag
    }

    clipedObj <- list( idStr="Base clipping" 
                        ,filtLst=filtLst
                        ,finalFlag=finalFlag 
                        ,birth=Sys.time()
                        ,indices=which(finalFlag)
                    )
    clipedObj$cost <- "25min"
    clipedLst[[1]] <- clipedObj
save( clipedLst ,file="Obj_clipedLst001.0.save" )

#==================================================================================
# 기존 Zoid와 똑같은 거리로 떨어져 있는 RZoid제외.
#   unique(abs(allZoidMtx[iIdx,]-zhF[hIdx,]))
#----------------------------------------------------------------------------------
myObj <- load("Obj_clipedLst001.0.save")
allZoidMtx <- getAllZoid() # 38sec
initIndices <- clipedLst[[1]]$indices
finalFlag <- rep( T ,nrow(allZoidMtx) )
finalFlag[-initIndices] <- FALSE  # 001 단계에서 제외된 것들을 적용하자
allZoidMtx <- allZoidMtx[initIndices,]

k.FLogStr("start")
tStmp <- Sys.time()
surFlag <- rep( T ,nrow(allZoidMtx) )
for( hIdx in 1:nrow(zhF) ){
    for( iIdx in seq_len(nrow(allZoidMtx)) ){
        dLen <- unique(abs(allZoidMtx[iIdx,]-zhF[hIdx,]))
        if( length(dLen)==1)
            surFlag[iIdx] <- FALSE
    }
    tDiff <- Sys.time() - tStmp
    k.FLogStr(sprintf("hIdx:%d (%.1f %s)",hIdx,tDiff,units(tDiff)),pTime=F)
}
k.FLogStr(sprintf("finish (%.1f %s)",tDiff,units(tDiff)),pTime=F)

clipedObj <- list( idStr="unique(abs(allZoidMtx[iIdx,]-zhF[hIdx,]))" ,parent="Base clipping" )
clipedObj$surFlag <- surFlag
clipedObj$finalFlag <- finalFlag
clipedObj$finalFlag[initIndices] <- surFlag

# 테스트 영역은 다시 TRUE로..
myObj <- load("Obj_masterObj.save")
clipedObj$finalFlag[ masterObj$testHMtx[,"rIdx"] ] <- TRUE
clipedObj$indices <- which(clipedObj$finalFlag)
clipedObj$cost <- "12hr"

clipedLst[[2]] <- clipedObj
save( clipedLst ,file="Obj_clipedLst002.U.save" ) # 완성되지 못한 버전. 

#==================================================================================
# clip.dumNum$byBase()
#   시간이 엄청 오래걸려 분리함.
#----------------------------------------------------------------------------------

# 002가 실행된 이후의.. 순차적 진행 시.
# myObj <- load("Obj_masterObj.save")
# myObj <- load("Obj_clipedLst002.U.save")
# parentName <- "Lst002"
# saveFile <- "Obj_clipedLst003.0.save"

myObj <- load("Obj_clipedLst001.0.save")
parentName <- "Base clipping"
saveFile <- "Obj_clipedLst003.U.save"
finalFlag <- rep( T ,nrow(allZoidMtx) )
initIndices <- clipedLst[[1]]$indices
allZoidMtx <- getAllZoid() # 38sec
allZoidMtx <- allZoidMtx[initIndices,]

clpObj <- cliper.dupNum( zh )
k.FLogStr("start clip.dumNum$byBase()")
tStmp <- Sys.time()
surFlag <- clpObj$byBase( allZoidMtx ,zh ,pDebugInfo=T )
tDiff <- Sys.time() - tStmp # 1.4 day
k.FLogStr(sprintf("finish clip.dumNum$byBase() - %.1f%s",tDiff,units(tDiff)),pConsole=T )

clipedObj <- list( idStr="clip.dumNum$byBase()" ,parent=parentName )
clipedObj$surFlag <- surFlag
clipedObj$finalFlag <- finalFlag 
clipedObj$finalFlag[-initIndices] <- FALSE
clipedObj$finalFlag[initIndices] <- is.na(surFlag)
clipedObj$cost <- "1.4 day"
# 테스트 영역은 다시 TRUE로..
# 일단 저장 후 복구하자.
clipedLst[[3]] <- clipedObj
save( clipedLst ,file=saveFile ) # 완성되지 못한 버전. 
    # 003.0 버전에서 clipedLst[[2]]와 clipedLst[[3]]이 병합되어야 한다.

#==================================================================================
# Merge : "Obj_clipedLst002.U.save" ,"Obj_clipedLst003.U.save"
#   QQE 동작 테스트 필요.
#----------------------------------------------------------------------------------
myObj <- load("Obj_clipedLst002.U.save")
clipedObj <- clipedLst[[2]]

myObj <- load("Obj_clipedLst003.U.save")
clipedLst[[2]] <- clipedObj

clipedObj <- list( idStr="merge 2,3" ,parent="002.U 003.U" )
clipedObj$finalFlag <- clipedLst[[2]]$finalFlag & clipedLst[[3]]$finalFlag
clipedObj$indices <- which( clipedObj$finalFlag )

clipedLst[[4]] <- clipedObj
save( clipedLst ,file="Obj_clipedLst004.0.save" )

# clipedObj <- list( idStr="full suceed" ,parent="003.U" )
# clipedObj$finalFlag <- clipedLst[[3]]$finalFlag
# clipedObj$indices <- which( clipedObj$finalFlag )

# clipedLst[[4]] <- clipedObj
# save( clipedLst ,file="Obj_clipedLst004.F.save" )


#==================================================================================
# Cliper$byBase()
#----------------------------------------------------------------------------------
allZoidMtx <- getAllZoid() # 38sec
myObj <- load("Obj_clipedLst004.0.save")
finalFlag <- clipedLst[[4]]$finalFlag
initIndices <- clipedLst[[4]]$indices
allZoidMtx <- allZoidMtx[initIndices,]

    flagLst <- list()

    clpObj <- cliper.quotient( zh )
    tStmp <- Sys.time()
    flag <- clpObj$byBase( allZoidMtx ,zh ,pDebugInfo=T )
    tDiff <- Sys.time() - tStmp # 50 min
    k.FLogStr(sprintf("cliper.quotient() %.f%s",tDiff,units(tDiff)))
    flagLst[[1+length(flagLst)]] <- flag


    clpObj <- cliper.remainder( zh )
    tStmp <- Sys.time()
    flag <- clpObj$byBase( allZoidMtx ,zh ,pDebugInfo=T )
    tDiff <- Sys.time() - tStmp # 52 min
    k.FLogStr(sprintf("cliper.remainder() %.f%s",tDiff,units(tDiff)))
    flagLst[[1+length(flagLst)]] <- flag


    clpObj <- cliper.stepWidth( zh )
    tStmp <- Sys.time()
    flag <- clpObj$byBase( allZoidMtx ,zh ,pDebugInfo=T )
    tDiff <- Sys.time() - tStmp # 47 min
    k.FLogStr(sprintf("cliper.stepWidth() %.f%s",tDiff,units(tDiff)))
    flagLst[[1+length(flagLst)]] <- flag

lastFlag <- NULL
for( idx in seq_len(length(flagLst)) ){
    if( idx==1 ){
        lastFlag <- is.na(flagLst[[1]])
        next
    }
    lastFlag <- lastFlag & is.na(flagLst[[idx]])
}


clipedObj <- list( idStr="Cliper$byBase() " ,parent="004.0" ,cost="3 hr")
clipedObj$flagLst <- flagLst
clipedObj$finalFlag <- finalFlag
clipedObj$finalFlag[initIndices] <- lastFlag
clipedObj$indices <- which( clipedObj$finalFlag )
clipedLst[[5]] <- clipedObj
save( clipedLst ,file="Obj_clipedLst005.0.save" ) 


#==================================================================================
# Cliper last stand - byBase() ,byLate()
#----------------------------------------------------------------------------------
myObj <- load("Obj_masterObj.save") # masterObj$testHMtx[,"rIdx"]
myObj <- load("Obj_clipedLst005.0.save")
finalFlag <- clipedLst[[5]]$finalFlag
initIndices <- clipedLst[[5]]$indices
allZoidMtx <- getAllZoid() # 38sec
allZoidMtx <- allZoidMtx[initIndices,]

flagObjLst <- list()
for( idx in 1:nrow(masterObj$testHMtx) ){
    hIdx <- masterObj$testHMtx[idx,"hIdx"]
    zoidZh <- zhF[1:(hIdx-1),]
    
    flagLst <- list()
    tStmp <- Sys.time()
    #-----------------------------------------------------
    # cliper.backStep()
    clpObj <- cliper.backStep( zh )
    flag <- clpObj$byBase( allZoidMtx ,zoidZh ,pDebugInfo=T )
    tDiff <- Sys.time() - tStmp # 2 min for 100000
    flagLst[[1+length(flagLst)]] <- flag
    k.FLogStr(sprintf("cliper.backStep() %.f%s",tDiff,units(tDiff)),pConsole=T)
    #-----------------------------------------------------
    # cliper.rebLen()
    clpObj <- cliper.rebLen( zh )
    flag <- clpObj$byBase( allZoidMtx ,zoidZh ,pDebugInfo=T )
    tDiff <- Sys.time() - tStmp # 1 min for 100000
    flagLst[[1+length(flagLst)]] <- flag
    k.FLogStr(sprintf("cliper.rebLen() %.f%s",tDiff,units(tDiff)),pConsole=T)

    #=====================================================
    # cliper.byLate()
    #-----------------------------------------------------
    clpObj <- cliper.quotient( zh )
    flag <- clpObj$byLate( allZoidMtx ,zoidZh ,pDebugInfo=T )
    tDiff <- Sys.time() - tStmp
    k.FLogStr(sprintf("cliper.quotient() %.f%s",tDiff,units(tDiff)))
    flagLst[[1+length(flagLst)]] <- flag

    clpObj <- cliper.remainder( zh )
    flag <- clpObj$byLate( allZoidMtx ,zoidZh ,pDebugInfo=T )
    tDiff <- Sys.time() - tStmp
    k.FLogStr(sprintf("cliper.remainder() %.f%s",tDiff,units(tDiff)))
    flagLst[[1+length(flagLst)]] <- flag
	
    clpObj <- cliper.stepWidth( zh )
    flag <- clpObj$byLate( allZoidMtx ,zoidZh ,pDebugInfo=T )
    tDiff <- Sys.time() - tStmp
    k.FLogStr(sprintf("cliper.stepWidth() %.f%s",tDiff,units(tDiff)))
    flagLst[[1+length(flagLst)]] <- flag

    clpObj <- cliper.dupNum( zh )
    flag <- clpObj$byLate( allZoidMtx ,zoidZh ,pDebugInfo=T )
    tDiff <- Sys.time() - tStmp
    k.FLogStr(sprintf("cliper.dumNum() %.f%s",tDiff,units(tDiff)))
    flagLst[[1+length(flagLst)]] <- flag

    flag.f <- is.na(flagLst[[1]])
    for( fIdx in 2:length(flagLst) ){
        flag.f <- flag.f & is.na(flagLst[[fIdx]])
    }

    flagObj <- list( hIdx=hIdx ,flag.f=flag.f ,flagLst=flagLst ,cost=tDiff )
    flagObjLst[[1+length(flagObjLst)]] <- flagObj

} # for(hIdx)

