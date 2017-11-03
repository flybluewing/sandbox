getPredictor <- function( pSampleDf ,pIdStr="glm" ,pPoly.mean=3 ,pPoly.seq=3 ){
					rObj <- list( idStr=pIdStr )
					rObj$poly.mean <- pPoly.mean
					rObj$poly.seq  <- pPoly.seq
					rObj$lm.out <- lm( prob~poly(mean,pPoly.mean)+poly(seq,pPoly.seq)
											,data=data.frame(pSampleDf)
										)
					rObj$predict <- function( pDataDf ,pType="response" ){
											#probOut <- predict( rObj$lm.out ,pDataDf ,type=pType )
											probOut <- predict( rObj$lm.out ,pDataDf )
											return( probOut )
										}
					return( rObj )
} # getPredicter()

#	pStdSeqObj <- stdSeqObj	;pIdStr="testPredictor"
getSeqProbPredictor <- function( pStdSeqObj ,pIdStr="testPredictor" ){
	rObj <- list( idStr=pIdStr )
	rObj$probPMap <- pStdSeqObj$probPMap[1:(pStdSeqObj$seqLogMax-1),]
	rObj$probNMap <- pStdSeqObj$probNMap[1:(pStdSeqObj$seqLogMax-1),]
	rObj$mapMax <- nrow(rObj$probPMap)
	rObj$prob <- sapply(pStdSeqObj$probPLst ,function(p){p$mean})

	rObj$predict <- function( pSeqNumObj ){ # k.seqNum()
			# rObj$probMtx에 각 codeVal 별 확률 계산결과를 넣어 반환해준다.

			rPObj <- list( lastVal=pSeqNumObj$lastVal ,lastVal.idx=pSeqNumObj$lastVal.idx )

			# --[probMtx]-----------------------------------------------
			cName <- sprintf("%s",pSeqNumObj$codeVal)
			rName <- c("mean","mean.last","seqNum","prob","isChanging")
			probMtx <- matrix( 0 ,nrow=length(rName) ,ncol=length(cName) )
			colnames(probMtx) <- cName	;rownames(probMtx) <- rName
			for( cIdx in 1:length(pSeqNumObj$codeVal) ){ # code index
				mean.all <- pSeqNumObj$meanMtx[1,cIdx]
				mean.last<- pSeqNumObj$meanMtx[2,cIdx]
				# mean.last <- mean.all + (mean.all-mean.last) # 최근 변동을 반영하는 예.
				mean.last <- mean.all
				mean.idx <- which.min(abs(rObj$prob - mean.last))
				seqNum <- NULL
				prob <- NULL
				isChanging <- NULL
				if( pSeqNumObj$lastVal.idx == cIdx ){
					seqNum	<- pSeqNumObj$lastSeqMtx["pos",cIdx]
					seqNum	<- ifelse( rObj$mapMax<seqNum ,rObj$mapMax ,seqNum )
					prob	<- rObj$probPMap[seqNum,mean.idx]
					isChanging <- ifelse( prob <= rObj$prob[mean.idx]*0.9 ,-1 ,0 ) # 감소한다는 의미에서 -1
				} else {
					seqNum	<- pSeqNumObj$lastSeqMtx["neg",cIdx]
					seqNum	<- ifelse( rObj$mapMax<seqNum ,rObj$mapMax ,seqNum )
					prob	<- rObj$probNMap[seqNum,mean.idx] # 계속 안나올 확률.
					isChanging <- ifelse( prob <= (1-rObj$prob[mean.idx])*0.9 ,1 ,0 ) # 증가했다는 의미에서 1
					prob	<- 1 - prob # 계속 안나올 확률로 표현하면 해석하기 불편하므로 나올 확률
				}
				probMtx["mean"		,cIdx]	<- mean.all
				probMtx["mean.last" ,cIdx]	<- mean.last
				probMtx["seqNum"	,cIdx]	<- ifelse( pSeqNumObj$lastVal.idx == cIdx ,seqNum ,-seqNum )
				probMtx["prob"		,cIdx]	<- prob
				probMtx["isChanging",cIdx]	<- isChanging
			} # for(cIdx)

			rPObj$probMtx <- probMtx
			return( rPObj )
		} # rObj$predict()

	return( rObj )
} # getSeqProbPredictor( )



# pFlag=sample( c(1:3,NA) ,20 ,replace=T ,prob=c(2,1,1,1) )
#	pLMeanArea : 최근 평균치를 계산할 최근 영역 범위
k.seqNum <- function( pFlag ,pCodeVal=NULL ,pLMeanArea=10 ){

	codeVal <- pCodeVal
	if( is.null(codeVal) )
		codeVal <- sort(unique(pFlag),na.last=T)
	codeVal.naIdx <- which( is.na(codeVal) )
	flagLen <- length(pFlag)

	seqP.cntMtx <- matrix( 0 ,ncol=length(codeVal) ,nrow=flagLen )
	colnames( seqP.cntMtx ) <- sprintf("%s",codeVal)
	seqP.hIdxMtx <- seqP.cntMtx
	seqP.rowIdx	<- rep(0,length(codeVal)) # current row index for seqP.*Mtx
	names(seqP.rowIdx) <- colnames( seqP.cntMtx )
	
	seqN.cntMtx <- seqP.cntMtx
	seqN.hIdxMtx <- seqP.hIdxMtx
	seqN.rowIdx <- seqP.rowIdx

	valLast <- NULL	;valLastIdx <- NULL
	for( hIdx in 1:flagLen ){

		if( 1== hIdx ){
			valLast <- pFlag[hIdx]
			valLastIdx <- ifelse( is.na(valLast) ,codeVal.naIdx ,which(codeVal==valLast) )
			seqP.rowIdx[ valLastIdx] <- 1
			seqP.cntMtx[ seqP.rowIdx[valLastIdx] ,valLastIdx ] <- 1
			seqP.hIdxMtx[ seqP.rowIdx[valLastIdx] ,valLastIdx ] <- hIdx

			seqN.rowIdx[-valLastIdx] <- 1
			uColIdx	<- (1:ncol(seqN.cntMtx))[-valLastIdx]	# update column index
			for( cIdx in uColIdx ){
				seqN.cntMtx[  seqN.rowIdx[cIdx],cIdx] <- 1
				seqN.hIdxMtx[ seqN.rowIdx[cIdx],cIdx] <- hIdx
			}
			next
		}

		valCur <- pFlag[hIdx]	# current value
		valCurIdx <- ifelse( is.na(valCur) ,codeVal.naIdx ,which(codeVal==valCur) )
		if( valLastIdx != valCurIdx ){ # pFlag 내 NA 존재 가능성 때문에 valCur 대신 valCurIdx로 비교
			seqP.rowIdx[valCurIdx] <- 1 + seqP.rowIdx[valCurIdx]
			seqP.hIdxMtx[ seqP.rowIdx[valCurIdx] ,valCurIdx ] <- hIdx
			
			seqN.rowIdx[valLastIdx] <- 1 + seqN.rowIdx[valLastIdx]
			uColIdx <- (1:ncol(seqN.hIdxMtx))[-valLastIdx] # update column index
			for( cIdx in uColIdx ){
				seqN.hIdxMtx[ seqN.rowIdx[cIdx],cIdx ] <- hIdx
			}
		} # if(valLastIdx==valCurIdx)
		seqP.cntMtx[seqP.rowIdx[valCurIdx],valCurIdx] <-
				1 + seqP.cntMtx[seqP.rowIdx[valCurIdx],valCurIdx]
		uColIdx <- (1:ncol(seqN.cntMtx))[-valCurIdx]	# update column index
		for( cIdx in uColIdx ){
			seqN.cntMtx[ seqN.rowIdx[cIdx] ,cIdx ] <- 
					1 + seqN.cntMtx[ seqN.rowIdx[cIdx], cIdx ]
			seqN.hIdxMtx[seqN.rowIdx[cIdx] ,cIdx ] <- hIdx
		}

		valLast <- valCur	;valLastIdx <- valCurIdx

		if( F ){
			k.FLogStr(sprintf("hIdx:%d",hIdx))
			k.FLog( seqP.cntMtx[1:10,] )
		}
	} # for( hIdx )

	rObj <- list( codeVal=codeVal )

	# --[seqP.cntMtx ,seqN.cntMtx]---------------------------------------------------
	azIndex <- which(apply(seqP.cntMtx,1,function(p){all(p==0)}))	# all zero index
	if( 0==length(azIndex) ){
		rObj$seqP.cntMtx <- seqP.cntMtx
		rObj$seqP.hIdxMtx <- seqP.hIdxMtx
	} else {
		rObj$seqP.cntMtx <- seqP.cntMtx[ 1:azIndex[1] , ]
		rObj$seqP.hIdxMtx <- seqP.hIdxMtx[ 1:azIndex[1] , ]
		# 일부러 azIndex[1] 사용.
		# 모든 컬럼을 대상으로 연속발생 수의 맨 마지막은 0이 되는 게 일관성 있으니까.
	}

	azIndex <- which(apply(seqN.cntMtx,1,function(p){all(p==0)}))	# all zero index
	if( 0==length(azIndex) ){
		rObj$seqN.cntMtx <- seqN.cntMtx
		rObj$seqN.hIdxMtx <- seqN.hIdxMtx
	} else {
		rObj$seqN.cntMtx <- seqN.cntMtx[ 1:azIndex[1] , ]
		rObj$seqN.hIdxMtx <- seqN.hIdxMtx[ 1:azIndex[1] , ]
		# 일부러 azIndex[1] 사용.
		# 모든 컬럼을 대상으로 연속발생 수의 맨 마지막은 0이 되는 게 일관성 있으니까.
	}

	# --[lastSeqMtx]-----------------------------------------------------
	cName <- sprintf("%s",codeVal)	;rName <- c("pos","neg")
	lastSeqMtx <- matrix( 0 ,ncol=length(cName) ,nrow=length(rName) )
	colnames(lastSeqMtx) <- cName	;rownames(lastSeqMtx) <- rName
	lastSeqMtx[1,] <- apply( rObj$seqP.cntMtx ,2 ,function(p){
								endPoint <- which(p==0)[1]-1
								return( ifelse( endPoint<1 ,NA ,p[endPoint] ) )
							})
	lastSeqMtx[2,] <- apply( rObj$seqN.cntMtx ,2 ,function(p){
								endPoint <- which(p==0)[1]-1
								return( ifelse( endPoint<1 ,NA ,p[endPoint] ) )
							})
	rObj$lastSeqMtx <- lastSeqMtx

	# --[meanMtx]-----------------------------------------------------
	cName <- sprintf("%s",codeVal)
	rName <- c("all",sprintf("last%d",pLMeanArea))
	meanMtx <- matrix(0,nrow=length(rName),ncol=length(cName))
	colnames(meanMtx) <- cName	;rownames(meanMtx) <- rName

	naCount <- sum(is.na(pFlag)) # NA is not counted by table()
	if(0<naCount)
		meanMtx["all","NA"] <- naCount
	tbl <- table( pFlag )
	meanMtx["all",names(tbl)] <- tbl
	meanMtx["all",] <- meanMtx["all",] / flagLen

	latestFlag <- pFlag[(flagLen-pLMeanArea+1):flagLen]
	naCount <- sum(is.na(latestFlag)) # NA is not counted by table()
	if(0<naCount)
		meanMtx[2,"NA"] <- naCount
	tbl <- table( latestFlag )
	meanMtx[2,names(tbl)] <- tbl
	meanMtx[2,] <- meanMtx[2,] / pLMeanArea

	rObj$meanMtx <- meanMtx

	rObj$lastVal <- pFlag[flagLen]
	rObj$lastVal.idx <- which( colnames(rObj$meanMtx) == sprintf("%s",rObj$lastVal) )

	return( rObj )

} # k.seqNum( )



#	pTestNum=1000 ;pSeqLogMax <- 300
getStdSeqProbMapObj <- function( pTestNum=1000 ,pSampleNum=1000 ,pSeqLogMax=300 ){

	tStmp <- Sys.time()
	fProb <- c(1,5,10,15,20,25,30,35,40,45,50)
	fPLst <- list()	;fNLst <- list()
	for( sIdx in 1:pTestNum ){ # sample index
		for( idx in 1:length(fProb) ){
			flag <- sample( 1:2 ,pSampleNum ,replace=T
							,prob=c(fProb[idx],100-fProb[idx])
						)
			k <- k.seq2( pH=flag ,pSeqMax=pSeqLogMax 
							,pCode=1:2 ,pCodeProb=c(fProb[idx],100-fProb[idx])
						)
			fPLst <- append( fPLst ,k$hpnPLst )
			fNLst <- append( fNLst ,k$hpnNLst )
		} # idx

		tCost <- Sys.time() - tStmp
		k.FLogStr(sprintf("%3dth sample (%4.1f %s)"
						,sIdx ,tCost ,units(tCost)
					))
	} # sIdx

	probPLst <- list()	;probNLst <- list()
	for( idx in seq_len(length(fPLst)) ){
		meanStr <- sprintf("%5.3f",fPLst[[idx]]$mean)
		if( is.null(probPLst[[meanStr]]) ){
			mtx <- fPLst[[idx]]$hpnMtx
			zFlag = 0==fPLst[[idx]]$hpnMtx[,"cnt"]	# zero flag
			mtx[zFlag,"cnt"] <- 1
			probPLst[[meanStr]] <- list( mean=fPLst[[idx]]$mean ,mtx=mtx)

			mtx <- fNLst[[idx]]$hpnMtx
			zFlag = 0==fNLst[[idx]]$hpnMtx[,"cnt"]
			mtx[zFlag,"cnt"] <- 1		
			probNLst[[meanStr]] <- list( mean=fNLst[[idx]]$mean ,mtx=mtx)
		} else {
			probPLst[[meanStr]]$mtx[,"cnt"] <- 
					probPLst[[meanStr]]$mtx[,"cnt"] + fPLst[[idx]]$hpnMtx[,"cnt"]
			zFlag = 0==fPLst[[idx]]$hpnMtx[,"cnt"]
			probPLst[[meanStr]]$mtx[zFlag,"cnt"] <-
					1 + probPLst[[meanStr]]$mtx[zFlag,"cnt"]
			probPLst[[meanStr]]$mtx[,"happen"] <- 
				probPLst[[meanStr]]$mtx[,"happen"] + fPLst[[idx]]$hpnMtx[,"happen"]
			
			probNLst[[meanStr]]$mtx[,"cnt"] <-
					probNLst[[meanStr]]$mtx[,"cnt"] + fNLst[[idx]]$hpnMtx[,"cnt"]
			zFlag = 0==fNLst[[idx]]$hpnMtx[,"cnt"]
			probNLst[[meanStr]]$mtx[zFlag,"cnt"] <-
					1 + probNLst[[meanStr]]$mtx[zFlag,"cnt"]
			probNLst[[meanStr]]$mtx[,"happen"] <-
				probNLst[[meanStr]]$mtx[,"happen"] + fNLst[[idx]]$hpnMtx[,"happen"]
		}

		if( F ){
			dbgPLst = F	;dbgMeanStr="0.010"
			k.FLogStr(sprintf("# %3d %s %s------------",idx,ifelse(dbgPLst,"P","N"),dbgMeanStr ))
			if( dbgPLst ){
				k.FLog( head( probPLst[[dbgMeanStr]]$mtx ) )
			} else {
				k.FLog( head( probNLst[[dbgMeanStr]]$mtx ) )
			}
		}
	} # idx

	mNames <- attributes(probPLst)$names # mean names
	mNames.ord <- order(mNames)

	probPLst <- probPLst[mNames.ord]
	probNLst <- probNLst[mNames.ord]

	cName <- mNames[mNames.ord]
	probPMap <- matrix( 0 ,ncol=length(mNames) ,nrow=nrow(probPLst[[1]]$mtx) )
	probNMap <- matrix( 0 ,ncol=length(mNames) ,nrow=nrow(probNLst[[1]]$mtx) )
	colnames( probPMap ) <- cName
	colnames( probNMap ) <- cName

	for( cIdx in 1:length(probPLst) ){
		probPMap[,cIdx] <- probPLst[[cIdx]]$mtx[,"happen"] / probPLst[[cIdx]]$mtx[,"cnt"]
		probNMap[,cIdx] <- probNLst[[cIdx]]$mtx[,"happen"] / probNLst[[cIdx]]$mtx[,"cnt"]
	} # cIdx
	
	fPLst <- NULL	;fNLst <- NULL

	rObj <- list( probPMap=probPMap ,probNMap=probNMap ,probPLst=probPLst ,probNLst=probNLst )
	rObj$mNames <- mNames[mNames.ord]
	rObj$seqLogMax <- pSeqLogMax
	rObj$showProbMap <- function( pProbMap=NULL ,pXLim=c(1,20) ,pYLim=c(0,0.5) ){
							if( is.null(pProbMap) )
								pProbMap <- rObj$probPMap
								
							prob.col <- terrain.colors( ncol(pProbMap) )
							plot( NULL ,xlim=pXLim ,ylim=pYLim ,xlab="seq" ,ylab="prob" )
							for( cIdx in 1:ncol(pProbMap) ){
								lines( 1:nrow(pProbMap) ,pProbMap[,cIdx] ,col=prob.col[cIdx] )
							}
						}

	if( F ){
		# 차후 디버깅용.
		plot( NULL ,xlim=c(1,10) ,ylim=c(0,0.5) ,xlab="seq" ,ylab="prob" )
		prob.col <- terrain.colors( ncol(probPMap) )
		for( cIdx in 1:ncol(probPMap) ){
			lines( 1:nrow(probPMap) ,probPMap[,cIdx] ,col=prob.col[cIdx] )
		}
	}
	
	return( rObj )
	
} # getStdSeqProbMapObj( )


#	pFlag=sample(1:3 ,100 ,replace=T ,prob=c(2,1,1) )
#	pSeqLogMax=300
getSeqProbMapObj <- function( pFlag ,pSeqLogMax=300 ){

	fPLst <- list()	;fNLst <- list()
	k <- k.seq2( pH=pFlag ,pSeqMax=pSeqLogMax )
	codeVal		<- sapply( k$hpnPLst ,function(p){p$val} )
	codeProb	<- sapply( k$hpnPLst ,function(p){p$mean} )

	fPLst <- append( fPLst ,k$hpnPLst )
	fNLst <- append( fNLst ,k$hpnNLst )

	probPLst <- list()	;probNLst <- list()
	for( idx in seq_len(length(fPLst)) ){
		meanStr <- sprintf("%5.3f",fPLst[[idx]]$mean)
		if( is.null(probPLst[[meanStr]]) ){
			mtx <- fPLst[[idx]]$hpnMtx
			zFlag = 0==fPLst[[idx]]$hpnMtx[,"cnt"]	# zero flag
			mtx[zFlag,"cnt"] <- 1
			probPLst[[meanStr]] <- list( mean=fPLst[[idx]]$mean ,mtx=mtx)

			mtx <- fNLst[[idx]]$hpnMtx
			zFlag = 0==fNLst[[idx]]$hpnMtx[,"cnt"]
			mtx[zFlag,"cnt"] <- 1		
			probNLst[[meanStr]] <- list( mean=fNLst[[idx]]$mean ,mtx=mtx)
		} else {
			probPLst[[meanStr]]$mtx[,"cnt"] <- 
					probPLst[[meanStr]]$mtx[,"cnt"] + fPLst[[idx]]$hpnMtx[,"cnt"]
			zFlag = 0==fPLst[[idx]]$hpnMtx[,"cnt"]
			probPLst[[meanStr]]$mtx[zFlag,"cnt"] <-
					1 + probPLst[[meanStr]]$mtx[zFlag,"cnt"]
			probPLst[[meanStr]]$mtx[,"happen"] <- 
				probPLst[[meanStr]]$mtx[,"happen"] + fPLst[[idx]]$hpnMtx[,"happen"]
			
			probNLst[[meanStr]]$mtx[,"cnt"] <-
					probNLst[[meanStr]]$mtx[,"cnt"] + fNLst[[idx]]$hpnMtx[,"cnt"]
			zFlag = 0==fNLst[[idx]]$hpnMtx[,"cnt"]
			probNLst[[meanStr]]$mtx[zFlag,"cnt"] <-
					1 + probNLst[[meanStr]]$mtx[zFlag,"cnt"]
			probNLst[[meanStr]]$mtx[,"happen"] <-
				probNLst[[meanStr]]$mtx[,"happen"] + fNLst[[idx]]$hpnMtx[,"happen"]
		}

		if( F ){
			dbgPLst = F	;dbgMeanStr="0.010"
			k.FLogStr(sprintf("# %3d %s %s------------",idx,ifelse(dbgPLst,"P","N"),dbgMeanStr ))
			if( dbgPLst ){
				k.FLog( head( probPLst[[dbgMeanStr]]$mtx ) )
			} else {
				k.FLog( head( probNLst[[dbgMeanStr]]$mtx ) )
			}
		}
	} # idx

	mNames <- attributes(probPLst)$names # mean names
	mNames.ord <- order(mNames)

	probPLst <- probPLst[mNames.ord]
	probNLst <- probNLst[mNames.ord]

	cName <- mNames[mNames.ord]
	probPMap <- matrix( 0 ,ncol=length(mNames) ,nrow=nrow(probPLst[[1]]$mtx) )
	probNMap <- matrix( 0 ,ncol=length(mNames) ,nrow=nrow(probNLst[[1]]$mtx) )
	colnames( probPMap ) <- cName
	colnames( probNMap ) <- cName

	for( cIdx in 1:length(probPLst) ){
		probPMap[,cIdx] <- probPLst[[cIdx]]$mtx[,"happen"] / probPLst[[cIdx]]$mtx[,"cnt"]
		probNMap[,cIdx] <- probNLst[[cIdx]]$mtx[,"happen"] / probNLst[[cIdx]]$mtx[,"cnt"]
	} # cIdx

	rObj <- list( probPMap=probPMap ,probNMap=probNMap ,probPLst=probPLst ,probNLst=probNLst )
	rObj$mNames <- mNames[mNames.ord]
	rObj$seqLogMax	<- pSeqLogMax
	rObj$codeVal	<- codeVal
	rObj$codeProb	<- codeProb

	return(rObj)

} # getSeqProbMapObj( )


k.seq2 <- function( pH ,pCode=NULL ,pCodeProb=NULL ,pSeqMax=10 ,pDebOpt=NULL ){
	#  연속의 성공여부를 기록한다.
	#	즉 hMtx[k,] 가 5, 2, 7 이라면 
	#	8번째 연속 발생에 2번 성공했고 5번 실패라는 얘기.
	#	이전에 7개 연속발생 상태라면 8 연속발생에 대한 확률산정용.
	# - pSeqMax : seq가 이 값보다 크면 무조건 max값에 대하여 연산(사실 상 버리는 값.)
	# - pDebOpt : NULL, p, n
	
	# QQE:Todo
	#	- pSeqMtx 적용 테스트
	#	- 3가지 요소에 대한 기능 테스트

	cName <- c("seq","happen","cnt")

	val <- pCode
	if( is.null(pCode) )
		val <- sort(unique(pH))
	
	hpnPLst <- list()	;hpnNLst <- list()
	for( idx in 1:length(val) ){
		hpnMtx <- matrix( 0 ,ncol=length(cName) ,nrow=pSeqMax )
		colnames(hpnMtx) <- cName
		hpnMtx[,"seq"] <- 1:nrow(hpnMtx)
		hpnObj <- list( val=val[idx] ,hpnMtx=hpnMtx )
		hpnObj$mean <- ifelse( is.null(pCode) ,sum(pH==val[idx])/length(pH) ,pCodeProb[idx]/sum(pCodeProb) )
		hpnPLst[[(length(hpnPLst)+1)]] <- hpnObj
		hpnNLst[[(length(hpnNLst)+1)]] <- hpnObj
	}

	seqHpnP <- rep(0,length(val))	;names(seqHpnP)<-val
	seqHpnN <- rep(0,length(val))	;names(seqHpnN)<-val
	valCur.last<-NULL	;valIdx.last<-NULL
	for( hIdx in 1:length(pH) ){ # hIdx <- 2

		if( 1== hIdx ){
			valCur.last <- pH[hIdx]
			valIdx.last <- which(val==pH[hIdx])
			seqHpnP[ valIdx.last] <- 1
			seqHpnN[-valIdx.last] <- 1
			rptStr <- sprintf("hIdx:%3d seqHpnP:%s   seqHpnN:%s \n"
								,hIdx
								,paste(sprintf("%3d",seqHpnP),collapse=" ")
								,paste(sprintf("%3d",seqHpnN),collapse=" ")
							)
			if( !is.null(pDebOpt) )
				cat(rptStr)

			next
		}

		valCur <- pH[hIdx]
		valIdx <- which(val==valCur)

		if( valCur.last == valCur ){
			seqNum <- ifelse( pSeqMax<seqHpnP[valIdx] ,pSeqMax ,seqHpnP[valIdx] )
			hpnPLst[[valIdx]]$hpnMtx[seqNum,"cnt"] <- 
						1 + hpnPLst[[valIdx]]$hpnMtx[seqNum,"cnt"]
			hpnPLst[[valIdx]]$hpnMtx[seqNum,"happen"] <- 
						1 + hpnPLst[[valIdx]]$hpnMtx[seqNum,"happen"]
			for( vIdx in (1:length(val))[-valIdx] ){
				seqNum <- ifelse( pSeqMax<seqHpnN[vIdx] ,pSeqMax ,seqHpnN[vIdx] )
				hpnNLst[[vIdx]]$hpnMtx[seqNum,"cnt"] <- 
						1 + hpnNLst[[vIdx]]$hpnMtx[seqNum,"cnt"]
				hpnNLst[[vIdx]]$hpnMtx[seqNum,"happen"] <- 
						1 + hpnNLst[[vIdx]]$hpnMtx[seqNum,"happen"]
			}
		} else {
			seqNum <- ifelse( pSeqMax<seqHpnP[valIdx.last] ,pSeqMax ,seqHpnP[valIdx.last] )
			hpnPLst[[valIdx.last]]$hpnMtx[seqNum,"cnt"] <- 
						1 + hpnPLst[[valIdx.last]]$hpnMtx[seqNum,"cnt"]
			for( vIdx in (1:length(val))[-valIdx.last] ){
				seqNum <- ifelse( pSeqMax<seqHpnN[vIdx] ,pSeqMax ,seqHpnN[vIdx] )
				hpnNLst[[vIdx]]$hpnMtx[seqNum,"cnt"] <- 
						1 + hpnNLst[[vIdx]]$hpnMtx[seqNum,"cnt"]
				if( valIdx!=vIdx ){
					hpnNLst[[vIdx]]$hpnMtx[seqNum,"happen"] <- 
							1 + hpnNLst[[vIdx]]$hpnMtx[seqNum,"happen"]
				}
			}
		}

		if( valCur.last == valCur ){
			seqHpnP[ valIdx] <- seqHpnP[ valIdx] + 1
			seqHpnN[-valIdx] <- seqHpnN[-valIdx] + 1
		} else {
			seqHpnP[ valIdx.last] <- 0
			seqHpnP[ valIdx     ] <- 1
			seqHpnN[-valIdx     ] <- 1 + seqHpnN[-valIdx     ]
			seqHpnN[ valIdx     ] <- 0
		}

		valCur.last <- valCur	;valIdx.last <- valIdx
		if( is.null(pDebOpt) )
			next
		# ---[LOG]----------------------------------------------------
		rptStr <- sprintf("hIdx:%3d seqHpnP:%s   seqHpnN:%s \n"
							,hIdx
							,paste(sprintf("%3d",seqHpnP),collapse=" ")
							,paste(sprintf("%3d",seqHpnN),collapse=" ")
						)
		cat(rptStr)

		k.FLogStr(sprintf("%dth(val:%d->%d) %s [%s]---------------------"
					,hIdx,pH[(hIdx-1)],valCur,pDebOpt,paste(val,collapse=" ") 
				))
		if( "p"==pDebOpt ){
			k <- lapply( hpnPLst ,function(p){k.FLog(p$hpnMtx)} )
		} else {
			k <- lapply( hpnNLst ,function(p){k.FLog(p$hpnMtx)} )
		}

	} # hIdx

	rObj <- list( hpnPLst=hpnPLst ,hpnNLst=hpnNLst )
	return( rObj )

} # k.seq2( )
