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

#	pTestNum=1000 ;pSeqLogMax <- 300
getStdSeqProbMapObj <- function( pTestNum=1000 ,pSampleNum=1000 ,pSeqLogMax=300 ,pShowGraph=T ){

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

	if( pShowGraph ){
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
