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

rstLogLst <- list()
for( hIdx in 700:length(flagThread) ){

    curThread <- flagThread[1:hIdx]
    eleMean <- table(curThread)/length(curThread)
    eleStatLst <- createEleStatLst( eleSet ,eleMean )

    logStr <- ""
    for( chIdx in 1:hIdx ){

		hauntVal <- curThread[chIdx]
		# if( T ){
		if( chIdx==hIdx ){
			obj <- list( hIdx=hIdx ,hauntVal=hauntVal ,eleStatLst=eleStatLst )
			rstLogLst[[1+length(rstLogLst)]] <- obj
		}

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


hauntVal <- sapply( rstLogLst ,function(p){p$hauntVal} )

par( mfrow=c(4,1) )
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

    plot( 1:length(rstLogLst) ,energyTot*1.5 ,type="l" ,ylim=c(-2,2) )
	lines( c(1,length(rstLogLst)) ,c(0,0) ,col="yellow" )
	lines( c(1,length(rstLogLst)) ,c(1,1) ,col="yellow" )
	
    lines( 1:length(rstLogLst) ,energy ,col="blue")
    points( (1:length(rstLogLst))[hitFlag] ,rep(0.5,sum(hitFlag)) ,pch="." ,col="red" )
	magVal <- 2.0	# mMean 수치가 너무 낮아 보기 어려워 확대..
    lines( mMeanSpan ,mMean*magVal ,col="green" )

}



