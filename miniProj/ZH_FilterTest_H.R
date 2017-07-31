# =====================================================================
#	Function Code 
#       N00 : Null 필터.(다른 필터의 성능평가 비교를 위한 최소기준 필터)
#		100 : DNA동일 위치.
# =====================================================================
#		평가특성 코드
#			v : dna 코드에 대한 확률 부여(주로 샘플링 당시부터 필터링)
#			d : dna 구조 특성에 대한 확률 부여.(주로 차후필터링)
#               filt( ) 함수를 갖는다.
# =====================================================================


# =====================================================================
# ZH_Filter.N01.v : Null 필터(아무 필터링 없음.)
# ---------------------------------------------------------------------
# ZH_Filter.N01.v.Obj : 평가를 위한 테이블 생성.
# p rawZh : zoid history (tGlobal$sampleStart 보다 커야 한다.)
# M	anaZoid : rawZH의 가장 마지막 History기준으로, 주어진 Zoid에 대한 특성분석
# return : []
#			recurMtx : History별 반복발견 숫자 표시.(3연속이면 2번 표기되는 셈)
ZH_Filter.N01.v.Obj <- function( ){
}

# =====================================================================
# ZH_Filter.100.d : 바로 이전과 같은 위치에 같은 염기가 반복되는 것.
#		이전 StdZoid가 1,10,20,30,40,41이고 다음 StdZoid가 1,20,21,30,35,41이면
#		동일위치 반복수는 3.(1,30,41)
# ---------------------------------------------------------------------
# ZH_Filter.100.d.Obj : 평가를 위한 테이블 생성.
# p recurNum : 각 History별 동일위치연속발생 수.
# return : []
#			recurMtx : History별 반복발견 숫자 표시.(3연속이면 2번 표기되는 셈)
#					NA	NA	NA	NA	NA	NA
#					1	NA	NA	30	NA	41
ZH_Filter.100.d.Obj <- function( ){
	filtName <- "ZH_Filter.100.d.Obj"
	rowCnt <- nrow(tGlobal$rawZH)
	recurMtx <- matrix( nrow=nrow(tGlobal$rawZH), ncol=ncol(tGlobal$rawZH) )
	for( idx in 1:tGlobal$dnaLength ){
		k <- tGlobal$rawZH[2:rowCnt,idx]!=tGlobal$rawZH[1:(rowCnt-1),idx]
		k <- c( T, k )  # 비교불가영역은 T로(연속발생 없음.)
		recurMtx[,idx] <- tGlobal$rawZH[,idx]
		recurMtx[k,idx] <- 0
	}
	
	recurNum <- apply( recurMtx, 1, function(z){sum(z>0)} )
	
	# rawZH의 마지막 stdZoid와 pZoid의 (위치별)DNA코드비교
	#	동일위치 동일염기이면 염기값을, 아니라면 NA반환.
	analyze <- function( pZoid, lastIdx ){
			# lastIdx는 stdZoid의 바로 이전 History.
			rZoid <- pZoid
			mch <- pZoid==tGlobal$rawZH[lastIdx,]
			rZoid[!mch] <- 0
			return(list(
					dnaMatch = rZoid
					,lastIdx = lastIdx
				))
		}
	
	# pZoid가 Filtering 대상인지 아닌지 파악.
	#	anaObj은 anaZoid() 함수의 결과물
	#	bias : filt 내용이 적중할 확률.
	#			특이사항의 연속발생 누적에 따라 가중됨.
	filt <- function( anaObj ){
			bias <- getFiltBias( anaObj$lastIdx )
			return(list(
					filtName = filtName
					,lastIdx = anaObj$lastIdx
					,bias = bias
					,result = sum(anaObj$dnaMatch)>0 
				))
		}
		
	# 해당 History 시점(pLastIdx)에서의 filter 적중 확률.
	#	특이사항 연속발생 통계와, pLastIdx 시점의 특이사항 누적에 따라
	#	가중치 증가(누적통계 없으면 가중치는 1.0)
	priv.filtBias <- list( hIdx=0 )
	priv.filtBias$bias <- 1.0
	getFiltBias <- function( pLastIdx ){
			
		}

	return(list(
			filtName = filtName
			,recurMtx = recurMtx
			,recurNum = recurNum
			,analyze = analyze
			,filt = filt
		))
}
