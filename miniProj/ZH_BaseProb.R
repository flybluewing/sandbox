
source("ZH_BaseProb_H.R")

# ---------------------------------------------------------------------
# p rawZh : zoid history
# return : []
ZH_BaseProb <- function( rawZH ){
	nHis <- nrow(rawZH)
	statHisIdx <- nHis / 5	# 통계 대상이 되는 과거역사의 가장 마지막 지점.
	dnaTypeSize <- length(tGlobal$dnaTypes)

	dnaProbAll <- rep(0,dnaTypeSize)
	names(dnaProbAll) <- as.character( tGlobal$dnaTypes )
	probDistrib <- matrix( nrow=0, ncol=dnaTypeSize )
		# probDistrib : 과거 발생빈도 가중치를 낮은 순으로 정렬한 값
		#				최종 row 수는 length(statHisIdx:(nHis-1))가 된다.		
	hitDistrib <- rep(0,length(statHisIdx:(nHis-1)))
		# hitDistrib : 해당 시점의 probDistrib로 Random Zoid 추출 시 명중률
	hitPos <- matrix( ncol=2, nrow=0 )
		# hitPos : 정답 Zoid의 dna들은 가중치 분포에서 얼마나 상위권에 위치하는 지..
	hitTable <- matrix( ncol=(tGlobal$dnaLength+1), nrow=0 )
	
	tdpa <- matrix( ncol=length(tGlobal$dnaTypes), nrow=0 )
	for(idx in statHisIdx:(nHis-1)) {
		#	idx+1은 정답 zoid의 위치를 가리킨다.
		
		# 확률 분포 계산
		dnaProbAll[1:length(dnaProbAll)] <- 0
		dnaProb <- table( as.vector(as.matrix( rawZH[1:idx,] )) )
		dnaProbAll[names(dnaProb)] <- dnaProb
		# 정답 Zoid()의 dna는 몇 번째 순위를 차지하는지..
		tHitPos <- matrix( ncol=2, nrow=tGlobal$dnaLength )
		tHitPos[,1] <- idx
		tHitPos[,2] <- which(!is.na(  match(order(dnaProbAll),rawZH[idx+1,])  ))
		hitPos <- rbind( hitPos, tHitPos )
		
		# 확률 분포대로 시도하는 경우의 명중률
		#	dnaProbAll+1 : 아예 발생확률이 없는 것은 아니도록 보정
		# simResult <- ZH_BaseProb_Sim( dnaProbAll+1, rawZH[idx+1,], nZoid=1000 )
		myFLog( sprintf("%sth sim try",idx) )
		simResult <- ZH_BaseProb_Sim( rep(1,length(tGlobal$dnaTypes)), rawZH[idx+1,], nZoid=100000 )
		hitDistrib[(idx+1)-statHisIdx] <- simResult$hitMean
		hitTable <- rbind( hitTable, simResult$hitTable )
				
		# 확률 분포의 기울기.		
		ditSort <- sort(dnaProbAll)
		tdpa <- rbind(tdpa, dnaProbAll)
		ditInfo <- summary(ditSort)
		if( ditInfo["Mean"]!=0 ){	# 사실, 0이면 history자체가 없는 경우밖엔..
			ditSort <- ditSort/ditInfo["Mean"]
		}
		probDistrib <- rbind(probDistrib, ditSort )
		
	} # for

	colnames(hitPos) <- c("history","order")
	colnames(hitTable) <- as.character(0:tGlobal$dnaLength)
	
	myFLog("--------------------------------------------")
	myFLog( dim(hitTable) )
	myFLog( mean(hitDistrib) )
	myFLog( summary(hitTable) )
	
	#-[결과리뷰]----------------------------------------------------------------
	if( FALSE ){
		plot( hitPos[,"history"], hitPos[,"order"], pch="+" )
		plot( 1:length(hitDistrib), hitDistrib, xlab="history", ylab="mean(hit)" )
		persp( 1:nrow(probDistrib), 1:ncol(probDistrib), probDistrib
				, theta=30, phi=20, xlab="history", ylab="확률순서", zlab="확률" )
		image( 1:nrow(probDistrib), 1:ncol(probDistrib), probDistrib
				, xlab="history", ylab="확률분포" )

		probDistribSlop <- rep(0,length(statHisIdx:(nHis-1)))
			# probDistribSlop : 과거 발생빈도 가중치 정렬결과에 대한 기울기
			#					기울기가 클수록 변별력이 좋다.
		xd <- (1:dnaTypeSize)
		for( idx in 1:nrow(probDistrib) ) {
			probD.lm <- lm( probDistrib[idx,]~xd )
			probDistribSlop[idx] <- coef(probD.lm)["xd"]
		}
		plot( 1:nrow(probDistrib), probDistribSlop, xlab="history", ylab="slop" )
	}
	
	return (1)
} # ZH_BaseProb


# =[실험결과]=========================================================================
#	ZH_BaseProb_Log.TXT 파일 참고.
# =====================================================================================





