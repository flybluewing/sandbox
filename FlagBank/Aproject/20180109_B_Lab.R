# 20180109_B_Lab.R 실험실.
source("../breedingPlace/20171116_A_H.R")
source("../breedingPlace/20171116_A_H_cliper.R")
source("../breedingPlace/20171116_B_H.R")
source("../breedingPlace/20171116_C_H.R")
source("../breedingPlace/20171116_D_H.R")
source("20180109_A_H.R")
source("20180109_B_H.R")

# Basic Data load
saveId <- "0127_23"
load(sprintf("./save/Obj_gEnv%s.save",saveId))
load(sprintf("./save/Obj_fRstLst%s.save",saveId))
load(sprintf("./save/Obj_remLst%s.save",saveId))

allZoidMtx <- gEnv$allZoidMtx
zhF <- gEnv$zhF


biObj <- getBiCoder( zhF )
allCodeMtx <- biObj$getAllCode()

cFlag.ele <- 1:4
eleMean <- rep( 0.25 ,length(cFlag.ele) )

testSpan <- 300:nrow(zhF)
hScoreBuf <- rep( 0 ,ncol(biObj$stdCodeMtx) )
aScoreBuf <- allCodeMtx    ;aScoreBuf[,] <- 0

hScore <- rep( 0 ,nrow(zhF)  )
aScore <- matrix( 0 ,ncol=nrow(allCodeMtx) ,nrow=nrow(zhF) )
aGrade <- rep( 0 ,nrow(zhF)  )

traceBackH <- 5
for( hIdx in testSpan ){

    for( cIdx in 1:5 ){
        eleStatLst <- createEleStatLst( cFlag.ele ,eleMean ,pSalScale=rep(0.2,length(cFlag.ele)) )
        # traceBackH부터 예측치 까지 계산
        for( tIdx in (hIdx-traceBackH):(hIdx-1) ){
            hauntVal <- biObj$stdCodeMtx[tIdx,cIdx]
            for( eIdx in 1:length(eleStatLst) ){
                if( hauntVal==eleStatLst[[eIdx]]$val ){
                    eleStatLst[[eIdx]] <- bank.haunt( eleStatLst[[eIdx]] )
                } else {
                    eleStatLst[[eIdx]] <- bank.quiet( eleStatLst[[eIdx]] )
                }
            } # eIdx
        } # tIdx

        # 이제 각 ele에 대한 기대치 계산.
		eleEnergy <- sapply( eleStatLst ,function(p){p$energy} )
		hScoreBuf[cIdx] <- eleEnergy[biObj$stdCodeMtx[hIdx,cIdx]==cFlag.ele]
        aScoreBuf[,cIdx] <- eleEnergy[ sapply( allCodeMtx[,cIdx] ,function(p){which(p==cFlag.ele)} ) ]
    } # cIdx

	hScore[hIdx] <- sum(hScoreBuf)
	aScore[hIdx,] <- apply( aScoreBuf ,1 ,sum )
	
	scoreVal <- c( hScore[hIdx], aScore[hIdx,] )
	aGrade[hIdx] <- 1 - which(order(scoreVal)==1)/length(scoreVal)

}

hist( aGrade[testSpan] )





# -------------------------------------------------------------------------------------------------------

# tFlag <- (zhF%%4)[,1]
tFlag <- biObj$stdCodeMtx[,1]
tEleSet <- sort(unique(tFlag))

# eleStatLst의 운영 성능 테스트
traceBackH <- 2	# 과거 발생상태를 반영키 위한 범위
rstLst <- list()
for( tIdx in 550:(length(tFlag)-20) ){
	cFlag <- tFlag[1:(tIdx-1)]
	cFlag.ele <- sort(unique(cFlag))

	# 최근 평균이동 추세를 eleMean에 반영 시키는 것도 좋을 듯.
	eleMean <- sapply( cFlag.ele ,function(p){ sum(cFlag==p)/length(cFlag) } )
	eleStatLst <- createEleStatLst( cFlag.ele ,eleMean ,pSalScale=rep(0.2,length(cFlag.ele)) )

	guessSpan <- (tIdx-traceBackH):(tIdx+15)
	guessLst <- list()
	for( gIdx in guessSpan ){
		hauntVal <- tFlag[gIdx]
		for( idx in 1:length(cFlag.ele) ){
			if( hauntVal==eleStatLst[[idx]]$val ){
				eleStatLst[[idx]] <- bank.haunt( eleStatLst[[idx]] )
			} else {
				eleStatLst[[idx]] <- bank.quiet( eleStatLst[[idx]] )
			}
		}
		guessLst[[1+length(guessLst)]] <- list( hIdx=gIdx ,eleStatLst=eleStatLst ,hauntVal=hauntVal)
	}

	rstLst[[1+length(rstLst)]] <- guessLst
}


par( mfrow=c(1,2) )

# plot(NULL, xlim=c(0,20), ylim=c(0,4.5 ), ylab="y label", xlab="x lablel")	
plot(NULL, xlim=c(0,20), ylim=c(0,1.1 ), ylab="y label", xlab="x lablel")	# Q
plotCol <- c("blue","red")

for( rstIdx in seq_len(length(rstLst)) ){
    guessLst <- rstLst[[rstIdx]]

    hitAss <- c( 0 ,0 ,0 )  ;names(hitAss) <- c("gIdx" ,"hit" ,"energy" )
    hitRank <- rep( 0 ,(length(guessLst)-1) )

    hitAssLst <- list()
    for( gIdx in 2:length(guessLst) ){
        hauntVal <- guessLst[[gIdx]]$hauntVal
        eleStatLst <- guessLst[[gIdx-1]]$eleStatLst

        hitAss[] <- 0
        hitIdx <- 0
        for( idx in 1:length(eleStatLst) ){
            hitAss["gIdx"]  <- gIdx
            hitAss["hit"]   <- ifelse( hauntVal==eleStatLst[[idx]]$val ,2 ,1 )
            hitAss["energy"]<- eleStatLst[[idx]]$energy
            hitAssLst[[1+length(hitAssLst)]] <- hitAss	# Q
            if( hauntVal==eleStatLst[[idx]]$val ){
                hitIdx <- idx
            }
        }

        energy <- sapply( eleStatLst ,function(p){p$energy} )
        hitRank[gIdx-1] <- which(order(energy,decreasing=T) == hitIdx )
    } # for(gIdx)

    # hitAssLst[[1+length(hitAssLst)]] <- list( hitAss=hitAss ,hitRank=hitRank )
    # points( jitter(1:length(hitRank)) ,jitter(hitRank) ,pch="." )

    hitMtx <- do.call( rbind ,hitAssLst )	# Q
	hitMtx <- hitMtx[hitMtx[,"hit"]==1 ,]	# Q
    points( jitter(hitMtx[,"gIdx"]) ,jitter(hitMtx[,"energy"]) ,col=plotCol[hitMtx[,"hit"]],pch="." )	# Q

}


plot(NULL, xlim=c(0,20), ylim=c(0,1.1 ), ylab="y label", xlab="x lablel")	# Q
plotCol <- c("blue","red")

for( rstIdx in seq_len(length(rstLst)) ){
    guessLst <- rstLst[[rstIdx]]

    hitAss <- c( 0 ,0 ,0 )  ;names(hitAss) <- c("gIdx" ,"hit" ,"energy" )
    hitRank <- rep( 0 ,(length(guessLst)-1) )

    hitAssLst <- list()
    for( gIdx in 2:length(guessLst) ){
        hauntVal <- guessLst[[gIdx]]$hauntVal
        eleStatLst <- guessLst[[gIdx-1]]$eleStatLst

        hitAss[] <- 0
        hitIdx <- 0
        for( idx in 1:length(eleStatLst) ){
            hitAss["gIdx"]  <- gIdx
            hitAss["hit"]   <- ifelse( hauntVal==eleStatLst[[idx]]$val ,2 ,1 )
            hitAss["energy"]<- eleStatLst[[idx]]$energy
            hitAssLst[[1+length(hitAssLst)]] <- hitAss	# Q
            if( hauntVal==eleStatLst[[idx]]$val ){
                hitIdx <- idx
            }
        }

        energy <- sapply( eleStatLst ,function(p){p$energy} )
        hitRank[gIdx-1] <- which(order(energy,decreasing=T) == hitIdx )
    } # for(gIdx)

    # hitAssLst[[1+length(hitAssLst)]] <- list( hitAss=hitAss ,hitRank=hitRank )
    # points( jitter(1:length(hitRank)) ,jitter(hitRank) ,pch="." )

    hitMtx <- do.call( rbind ,hitAssLst )	# Q
	hitMtx <- hitMtx[hitMtx[,"hit"]==2 ,]	# Q
    points( jitter(hitMtx[,"gIdx"]) ,jitter(hitMtx[,"energy"]) ,col=plotCol[hitMtx[,"hit"]],pch="." )	# Q

}



# -------------------------------------------------------------------------------------------------------
flagThread <- biObj$stdCodeMtx[,1]
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
	#for(chIdx in 1:700 ) {
		hauntVal <- curThread[chIdx]
		# if( T ){
		if( chIdx==hIdx ){
			obj <- list( hIdx=hIdx ,hauntVal=hauntVal ,eleStatLst=eleStatLst )
			rstLogLst[[1+length(rstLogLst)]] <- obj
		}

		for( idx in 1:length(eleSet) ){
            if( hauntVal==eleStatLst[[idx]]$val ){
                eleStatLst[[idx]] <- bank.haunt( eleStatLst[[idx]] )
            } else{
                eleStatLst[[idx]] <- bank.quiet( eleStatLst[[idx]] )
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
    points( (1:length(rstLogLst))[hitFlag] ,rep(0.5,sum(hitFlag)) ,pch="*" ,col="red" )
	magVal <- 2.0	# mMean 수치가 너무 낮아 보기 어려워 확대..
    lines( mMeanSpan ,mMean*magVal ,col="green" )

}





