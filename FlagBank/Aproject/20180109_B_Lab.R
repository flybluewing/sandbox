# 20180109_B_Lab.R 실험실.
source("../breedingPlace/20171116_A_H.R")
source("../breedingPlace/20171116_A_H_cliper.R")
source("../breedingPlace/20171116_B_H.R")
source("../breedingPlace/20171116_C_H.R")
source("../breedingPlace/20171116_D_H.R")
source("20180109_A_H.R")
source("20180109_B_H.R")

# Basic Data load
saveId <- "0123_14"
load(sprintf("./save/Obj_gEnv%s.save",saveId))
load(sprintf("./save/Obj_fRstLst%s.save",saveId))
load(sprintf("./save/Obj_remLst%s.save",saveId))

allZoidMtx <- gEnv$allZoidMtx
zhF <- gEnv$zhF


stdMtx <- zhF %% 4
flagThread <- stdMtx[,1]
measureSpan <- 1:200
# flagThread <- c(1:4,1:4,1:4)

eleSet  <- sort(unique(flagThread))  # element set 발생가능 경우들.

rstLogLst <- list() ;logStart <- 300
for( hIdx in 100:length(flagThread) ){

    curThread <- flagThread[1:hIdx]
    eleMean <- table(curThread)/length(curThread)
    eleStatLst <- createEleStatLst( eleSet ,eleMean )

    for( chIdx in 1:hIdx ){
        for( idx in 1:length(eleSet) ){
            if( hauntVal==eleStatLst[[idx]]$val ){
                eleStatLst[[idx]] <- bank.haunt3( eleStatLst[[idx]] )
            } else{
                eleStatLst[[idx]] <- bank.quiet3( eleStatLst[[idx]] )
            }
        }
    }

    eleObj <- eleStatLst[[1]]
    logStr <- sprintf("[%s] E:%.3f B:%.3f (min/max %.3f/%.3f) - lastH:%s" ,ifelse(hauntVal==eleObj$val,"T"," ")
                    ,eleObj$energy  ,eleObj$bank    ,eleObj$eMin    ,eleObj$eMax
                    ,paste(ifelse(eleObj$lastH,"T","."),collapse=" ")
                )

    k.FLogStr(sprintf("%3d %s",hIdx,logStr))

}



eleMean <- table(flagThread[measureSpan])/length(measureSpan)   # 발생 평균. 1/n에 해당하는 값이기도 하다.


    # 테스트용 데이터. (25%용)
    #     flagThread <- c( 1 ,1 ,1 ,4 ,1    ,1 ,4 ,1 ,1 ,1
    #                     ,1 ,4 ,4 ,1 ,1    ,4 ,4 ,1 ,1 ,1
    #                     )


# eleStatLst <- createEleStatLst( eleSet ,eleMean ,pSalScale=rep(0.1,length(eleSet)) )
# eleStatLst <- createEleStatLst( eleSet ,rep(0.25,4) ,pSalScale=rep(0.1,length(eleSet)) )

# eleMean <- c( 0.195 ,0.280 ,0.305 ,0.220 )
# eleMean <- c( 0.21 ,0.33 ,0.275 ,0.220 )
eleStatLst <- createEleStatLst( eleSet ,eleMean ,pSalScale=rep(0.1,length(eleSet)) )

rstLogLst <- list() ;logStart <- 300
for( hIdx in 1:length(flagThread) ){
# for( hIdx in 1:12 ){

    logStr <- ""
    hauntVal <- flagThread[hIdx]
    
    if( hIdx >= logStart ){
        obj <- list( hIdx=hIdx ,hauntVal=hauntVal ,eleStatLst=eleStatLst )
        rstLogLst[[1+length(rstLogLst)]] <- obj
    }

    log.idx <- 0
    for( idx in 1:length(eleStatLst) ){
        logStr <- paste( logStr ,sprintf("[%s] %.3f"
                        ,ifelse( eleStatLst[[idx]]$val==hauntVal ,"T" ," " )
                        ,eleStatLst[[idx]]$energy
                    ))
        log.idx <- idx

        # 발생/비발생에 따른 다음 발생 에너지 계산.
        #   다음 H에서의(hIdx+1) 발생 에너지를 가늠한다.
        if( hauntVal==eleStatLst[[idx]]$val ){
            eleStatLst[[idx]] <- bank.haunt3( eleStatLst[[idx]] )
        } else{
            eleStatLst[[idx]] <- bank.quiet3( eleStatLst[[idx]] )
        }
    }

    eleObj <- eleStatLst[[1]]
    logStr <- sprintf("[%s] E:%.3f B:%.3f (min/max %.3f/%.3f) - lastH:%s" ,ifelse(hauntVal==eleObj$val,"T"," ")
                    ,eleObj$energy  ,eleObj$bank    ,eleObj$eMin    ,eleObj$eMax
                    ,paste(ifelse(eleObj$lastH,"T","."),collapse=" ")
                )

    k.FLogStr(sprintf("%3d %s",hIdx,logStr))

}


hauntVal <- sapply( rstLogLst ,function(p){p$hauntVal} )

par( mfrow=c(1,1) )
for( idx in 1:length(eleSet) ){
    # idx <- 2
    energy <- sapply( rstLogLst ,function(p){p$eleStatLst[[idx]]$energy} )
    energyTot <- sapply( rstLogLst ,function(p){ p$eleStatLst[[idx]]$energy+p$eleStatLst[[idx]]$bank } )
    hitFlag <- sapply( rstLogLst ,function(p){ p$hauntVal==p$eleStatLst[[idx]]$val } )
    mMeanSpan <- 10:(length(hitFlag)-10)
    mMean <- rep( 0 ,length(mMeanSpan) )
    for( mIdx in 1:length(mMeanSpan) ){
        cPos <- mMeanSpan[mIdx]
        mMean[mIdx] <- mean(hitFlag[(cPos-5):(cPos+5)])
    }

    plot( 1:length(rstLogLst) ,energyTot ,type="l" ,ylim=c(-2,2) )
    lines( 1:length(rstLogLst) ,energy ,col="blue")
    points( (1:length(rstLogLst))[hitFlag] ,rep(0.5,sum(hitFlag)) ,pch="." ,col="red" )
    lines( mMeanSpan ,mMean ,col="green" )

}




