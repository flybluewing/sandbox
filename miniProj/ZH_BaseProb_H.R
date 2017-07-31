# ---------------------------------------------------------------------
# p dnaProbAll 	: 발생빈도 분포(모두 0이면 안됨.)
# p stdZoid		: 정답 Zoid
# p nZoid 		: Zoid 생성시도 수.
# return 		: 평균 명중 갯수
ZH_BaseProb_Sim <- function( dnaProbAll, stdZoid, nZoid=5 ){
	
	scores <- rep(0,nZoid)
	for( idx in 1:nZoid ){
		newZoid <- sort(sample( tGlobal$dnaTypes, tGlobal$dnaLength, prob=dnaProbAll ))
		scores[idx] <- sum(!is.na(match(newZoid, stdZoid)))
		# print(sprintf("score:%d",scores[idx]))
	}
	tHitTable <- table(scores)
	# print( tHitTable )
	tHitTableAll <- rep( 0, tGlobal$dnaLength+1 )	# 하나도 발견되지 않았을 때 역시 포함
	names(tHitTableAll) <- as.character(0:tGlobal$dnaLength)
	tHitTableAll[names(tHitTable)] <- tHitTable;
	return(list(
			 hitMean = mean(scores)
			,hitTable = tHitTableAll
		))
}	# end of ZH_BaseProb_Sim
