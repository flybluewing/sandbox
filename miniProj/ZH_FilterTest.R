source("ZH_FilterTest_H.R")


ZH_FilterTest <- function( pTestIdx, pZoidPoolSize=100000 ){
	logFileName <- "ZH_FilterTest.LOG"
	dlf <- DataLogFile( sprintf("ZH_FilterTest%d.Dat",pTestIdx) )
	
	myFLog( "ZH_FilterTest start.", fName=logFileName )
	myFLog( sprintf("ZH_FilterTest tIdx:%d  PoolSize:%d",pTestIdx,pZoidPoolSize), fName=logFileName)
	testHistory <- tGlobal$sampleStart:nrow(tGlobal$rawZH)	
	
	# Filter 초기화
	f100 <- ZH_Filter.100.d.Obj()
	filtNames <- c(f100$filtName)
	myFLog( "ZH_FilterTest init filters", fName=logFileName )

	# Data Log 메타정보 저장.
	metaInfo <- list( type="metaInfo" )
	metaInfo$filtMatrixNames <- filtNames
	metaInfo$zoidPoolSize <- pZoidPoolSize
	metaInfo$testHistory <- testHistory
	dlf$jsonLog( metaInfo )

	
	# test zoid pool
	zPool <- matrix( ncol=tGlobal$dnaLength, nrow=metaInfo$zoidPoolSize )
	for( idx in 1:nrow(zPool) ){ 
		zPool[idx,]<-sort( sample(tGlobal$dnaTypes,tGlobal$dnaLength) )
	}
	
	# test for each Zoid History
	zScores <- rep(0,nrow(zPool))		# 특정 History에서의 new zoid별 적합률
	zFiltMtx <- matrix( ncol=length(filtNames), nrow=nrow(zPool) )	# 특정 History에서의 new zoid가 제거대상이었는지 여부.
	colnames(zFiltMtx) <- filtNames
	
	myFLog( sprintf("ZH_FilterTest testing by history.(size:%d)",length(testHistory)), fName=logFileName )
	for( hIdx in testHistory ){
		stdZoid <- tGlobal$rawZH[hIdx,]
		for( zIdx in 1:nrow(zPool) ){
			# pool내 zoid들의 명중률 확인
			zScores[zIdx] <- sum(!is.na(match(zPool[zIdx,],stdZoid)))
			# 필터링 확인
			zFiltered <- f100$filt(f100$analyze( zPool[zIdx,],(hIdx-1) ))
			zFiltMtx[zIdx,zFiltered$filtName] = zFiltered$result
		}
		
		# 결과 평가 및 저장.
		if( (hIdx%%25)==0 ){
			myFLog( sprintf("ZH_FilterTest current hIdx:%d",hIdx), fName=logFileName )	
		}
		dLst <- list(  type="data" )
		dLst$historyIndex=hIdx		
		dLst$zoidScores <- zScores
		dLst$filtMatrix <- zFiltMtx
		dlf$jsonLog( dLst )
		
	} # for( hIdx )
	
	
	myFLog( "ZH_FilterTest finished.", fName=logFileName )
	
}

ZH_FilterTest.simul <- function(){

    f100 <- ZH_Filter.100.d.Obj()
    f100$recurFlag <- ifelse( f100$recurNum>0, 1, 0 )
    vals <- sort(unique(f100$recurFlag))
    testHistory <- 600:694
    probMtxLst.f100 <- matrix( ncol=length(vals), nrow=length(testHistory) )
    colnames(probMtxLst.f100) <- vals
    rownames(probMtxLst.f100) <- testHistory
    for( hIdx in testHistory ) {
		seqObj <- seqDistribute(f100$recurFlag[1:(hIdx-1)])

		pVal <- f100$recurFlag[hIdx-1]
		pValSeq <- 0    # 이전 값이 몇 번 연속발생상태인지?(횟수가 아닌 갯수)
		for( bhIdx in (hIdx-1):1 ){
			if( pVal==f100$recurFlag[bhIdx] ){
				pValSeq = pValSeq+1
			}else{ break }
		}

		seqDistObj <- seqDistClass( seqObj$vals, seqObj$recurMtx )
			# 현재 시점까지 발생한 값들만으로 평가하므로.
			#   vals가 아닌 seqObj$vals가 쓰인다.

		for( val in seqObj$vals ){ # 가용한 값 별 발생확률
			if( val == pVal ){
				seqProb <- seqDistObj$prob(val, pValSeq ) 
					# 이전값과 동일값으로 연속발생 확률
					#   연속발생 횟수이므로, 연속발생 갯수-1과 동일
				probMtxLst.f100[
						as.character(hIdx),as.character(val)
					]   <- seqProb
			} else {
				nSeqProb <- 1-seqDistObj$prob(pVal, pValSeq )
					# 이전값이 지금에도 연속발생했을 확률
					#   연속발생 횟수이므로, 연속발생 갯수-1과 동일
				prob <- with( seqObj, {
								# vals에서 pVal을 제외한 총계를 구하고
								otherSum = sum( valCnt[vals!=pVal] )
								# restProb 에 대한 분율을 계산
								ifelse( otherSum==0, 0, valCnt[vals==val]/otherSum )
							} )
				probMtxLst.f100[
						as.character(hIdx),as.character(val)
					]   <- prob*nSeqProb
			}
		}   # for( val )
	
    }   # for( hIdx )


    probMtxLst <- list()
    probMtxLst[[f100$filtName]] <- probMtxLst.f100

    return( probMtxLst )
}



ZH_FilterTest.LoadData <- function( pFileName, pMakeLog=F ){

	logFileName <- "ZH_FilterTest.LOG"
	rObject <- list( fileName=pFileName )

	if( pMakeLog )
		myFLog( sprintf("{ZH_FilterTest.Data}Loading Data File : %s",rObject$fileName), fName=logFileName )
	dataStr <- readLines( rObject$fileName )
	if( pMakeLog )
		myFLog( sprintf("{ZH_FilterTest.Data}dataSize : %d",length(dataStr)), fName=logFileName )
		
	jsonObj <- lapply( dataStr, fromJSON )
	if( pMakeLog )
		myFLog( "{ZH_FilterTest.Data} finish decoding.", fName=logFileName )
	dataStr <- NULL	# 디버깅 편의를 위해 사용된 변수의 메모리 삭제.
	
	objType <- sapply( jsonObj, function(x) return(x$type) )
	rObject$metaInfo <- jsonObj[objType=="metaInfo"][[1]]	# metaInfo는 하나만 존재하는 것으로 가정.
	rObject$dataLst <- jsonObj[objType!="metaInfo"]
	for( lIdx in 1:length(rObject$dataLst) ){
		colnames(rObject$dataLst[[lIdx]]$filtMatrix) <- rObject$metaInfo$filtMatrixNames
	}
	return( rObject )
}


