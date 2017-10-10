# 한글한글

#	pFlag <- bFlag	;pHSpan <- 100:length(pFlag)	;pDiff=10
getFlagStatSample <- function( pFlag ,pHSpan ,pDiff=10){

					cName <- c("output","mean","mean.diff","seqHaunt")
					mtx <- matrix( 0 ,nrow=length(pHSpan) ,ncol=length(cName) )
					colnames(mtx) <- cName

					for( idx in 1:length(pHSpan) ){ # idx <- 1
						cfIdx <- pHSpan[idx]
						mtx[idx,"output"]		<- pFlag[cfIdx]
						curFlag <- pFlag[(cfIdx-1):1] # reversed.

						mtx[idx,"mean"]			<- mean(curFlag)
						mtx[idx,"mean.diff"]	<- mtx[idx,"mean"] - mean(curFlag[1:pDiff])

						cnt <- 0
						for( cntIdx in 2:length(curFlag) ){
							if( curFlag[cntIdx]==curFlag[1] ){
								cnt <- cnt+1
							} else {
								break
							}
						}
						mtx[idx,"seqHaunt"]	<- ifelse( curFlag[1] , cnt+1 , -cnt )
					} # for

					return( list( sampleMtx=mtx ) )
}	# getFlagStatSample


getStatCharact <- function( pFlag ,pNextVal ,pDiff=10 ){

						revFlag <- pFlag[length(pFlag):1]
						meanVal <- mean(pFlag)

						cnt <- 0
						for( cntIdx in 2:length(revFlag) ){
							if( revFlag[cntIdx]==revFlag[1] ){
								cnt <- cnt+1
							} else {
								break
							}
						}
						seqHaunt <- ifelse( revFlag[1] , cnt+1 , -cnt )

						rObj <- data.frame(	output = pNextVal
								,mean		= meanVal
								,mean.diff	= meanVal - mean(revFlag[1:pDiff])
								,seqHaunt	= seqHaunt
							)
						return(rObj)
}	# getStatCharact

#		- pProbSoften : 예상 확률이 발생빈도를 너무 무시하지 않게...
#						(발생빈도 낮은 코드들의 장기 미발생 깽판 방지)
#		- pSeqHauntMax : seqHaunt
nominateCode <- function( pCode ,pH ,pCharactList ,pProbFit ,pProbSoften=F ,pSeqHauntMax=20 ,pDiff=10){

					probAdjust <- T
					probMtx <- matrix(1,nrow=length(pCharactList),ncol=length(pCode))
					probMtx.mean <- probMtx	;probMtx.meanDiff <- probMtx	;probMtx.seqHaunt <- probMtx
					colnames(probMtx) <- pCode
					for( chIdx in seq_len(length(pCharactList)) ){ # chIdx <- 4

						statCharact <- NULL
						histCh <- pCharactList[[chIdx]]$getCharact( pH )
						for( cdIdx in pCharactList[[chIdx]]$codeRange ){	# cdIdx <- pCharactList[[chIdx]]$codeRange[2]
							tFlag <- histCh == cdIdx
							if( is.null(statCharact) ){
								statCharact <- getStatCharact( tFlag[1:length(tFlag)] ,TRUE ,pDiff=pDiff )
							} else {
								statCharact <- rbind( statCharact 
														,getStatCharact( tFlag[1:length(tFlag)] ,TRUE ,pDiff=pDiff )
													)
							}
						}
						if( !is.null(pSeqHauntMax) ){
							statCharact$seqHaunt <- ifelse( statCharact$seqHaunt > pSeqHauntMax 
																,pSeqHauntMax ,statCharact$seqHaunt )
							statCharact$seqHaunt <- ifelse( statCharact$seqHaunt < -pSeqHauntMax 
																,-pSeqHauntMax ,statCharact$seqHaunt )
						}

						prob <- predict( pProbFit ,statCharact[,c("mean","mean.diff","seqHaunt")] ,type="response" )
						if( pProbSoften )
							prob <- prob * statCharact$mean

						if( probAdjust ){
							prob <- 2*k.standardize( prob ,pPer=F )
													# 네거티브는 1이하, 포지티브는 1이상으로 만들기 위해 0~2 사이값으로 조절.
						}

						codeCh <- pCharactList[[chIdx]]$getCharact( pCode )
						indices <- mapply(function(p){which(p==pCharactList[[chIdx]]$codeRange)} ,codeCh)
						probMtx[chIdx,]			<- prob[indices]
						probMtx.mean[chIdx,]	<- statCharact$mean[indices]
						probMtx.meanDiff[chIdx,]<- statCharact$mean.diff[indices]
						probMtx.seqHaunt[chIdx,]<- statCharact$seqHaunt[indices]
					}

					finalProb <- apply( probMtx ,2 ,prod)

					rObj <- list( nominee=pCode[which(finalProb==max(finalProb))] )
					rObj$finalProb <- finalProb
					rObj$probMtx <- probMtx
					rObj$probMtx.mean		<- probMtx.mean
					rObj$probMtx.meanDiff	<- probMtx.meanDiff
					rObj$probMtx.seqHaunt	<- probMtx.seqHaunt
					return( rObj )

}	# nominateCode


#	Inspect Charact Performance.
# pNominee<-nominee		;pStd<-curDnaH[hIdx]  ;pCode<-curDna.code	;pCharactList<-charactList 
inspCharPerf <- function( pNominee ,pStd ,pCode ,pCharactList ){
		stdIndex <- which(pCode==pStd)
		threshold <- length(pCode)/2
		
		cName <- c("seqHaunt","success","fail")
		mtx <- matrix( 0 ,nrow=length(pCharactList) ,ncol=length(cName) )
		colnames( mtx ) <- cName
		rownames( mtx ) <- sapply(pCharactList,function(p){p$idStr})
		for( chIdx in 1:nrow(pNominee$probMtx) ){	# chIdx <- 3 # charactList index & row index
			mtx[chIdx,"seqHaunt"] <- pNominee$probMtx.seqHaunt[chIdx,stdIndex]	# seqHaunt Value
			if( rank(pNominee$probMtx[chIdx,])[stdIndex]>threshold ){
				mtx[chIdx,"success"]<- 1
			} else {
				mtx[chIdx,"fail"]	<- 1
			}
		} # chIdx

		return( mtx )
}

charactBasic <- function( pCodeRange ){
					rObj <- list(idStr="baseStart")
					rObj$codeRange <- pCodeRange
					rObj$getCharact <- function( pCode ){
								return( pCode )
							}
					return( rObj )
}

charactModu <- function( pBase=8 ){	# modulo
					rObj <- list(idStr=sprintf("charactModu.%02d",pBase))
					rObj$base <- pBase
					rObj$codeRange <- 0:(pBase-1)
					rObj$getCharact <- function( pCode ){
								return( pCode %% pBase )
							}
					return( rObj )
}	# charactModu

charactIntDiv <- function( pBase=5 ,pMaxCode=10 ){	# Integer Division
					rObj <- list(idStr=sprintf("charactIntDiv.%02d.%02d",pBase,pMaxCode))
					rObj$base <- pBase
					rObj$codeRange <- 0:pMaxCode
					rObj$getCharact <- function( pCode ){
								intDiv <- pCode %/% pBase
								return( ifelse(intDiv>pMaxCode,pMaxCode,intDiv) )
							}
					return( rObj )
}


createProbReg <- function( pFB ,pDiff=10 ){
						fLine <- pFB$zh[,1]

						hSpan <- 100:nrow(pFB$zh)
						divBase <- 8
						flag <- fLine %% divBase



						log.txt <- "./log/sampleMtx.txt"
						k.FLogStr("",pFile=log.txt)

						cName <- c("output","mean","mean.diff","seqHaunt")
						statDf <- data.frame( output=integer(0) ,mean=numeric(0) ,mean.diff=numeric(0) ,seqHaunt=integer(0) )

						for( cSize in 1:(divBase-1) ){
							combMtx <- combinations( divBase ,cSize )
							combMtx <- combMtx-1	# flag는 나머지들의 값이므로.

							for( combIdx in seq_len(nrow(combMtx)) ){
								bFlag <- flag %in% combMtx[combIdx,]
								rObj <- getFlagStatSample( bFlag ,hSpan ,pDiff=10 )
								k.FLogStr(sprintf("\\n cSize:%d combIdx:%d (mean:%03.f%%)----------------"
											,cSize ,combIdx ,100*mean(rObj$sampleMtx[,"mean"])
										)
										,pConsole=F ,pFile=log.txt
									)
								statDf <- rbind( statDf ,rObj$sampleMtx )
							}	# for(combIdx)
						} # for(cSize)


						glm.out <- glm( output~poly(mean,4)+poly(mean.diff,4)+poly(seqHaunt,4) 
											,data=statDf ,family=binomial
										)
						glm.out$diffSize <- pDiff

						if( T ) # 일반적으론 여기서 끝.
							return( glm.out )

						# 이하는 데이터 검토용 코드
						seqHaunt <- sort(unique(statDf$seqHaunt))
						seqHaunt.col <- terrain.colors(length(seqHaunt))

						xRange <- range( statDf$mean )
						for( shIdx in seq_len(length(seqHaunt)) ){

							rDataFlag <- statDf$seqHaunt==seqHaunt[shIdx]

							predDf <- data.frame( mean=seq( xRange[1] ,xRange[2] ,0.01 ) )
							predDf$mean.diff	<- rep( mean(statDf$mean.diff[rDataFlag]) ,nrow(predDf) )
							predDf$seqHaunt		<- rep( seqHaunt[shIdx] ,nrow(predDf) )
							glm.pred <- predict( glm.out ,predDf ,type="response" )

							if( shIdx == 1 ){
								plot( predDf$mean ,glm.pred
										,xlim=xRange ,ylim=c(-0.1,1.1)
										,pch="+" ,col=seqHaunt.col[shIdx] ,type="l" )
							} else {
								lines( predDf$mean ,glm.pred ,col=seqHaunt.col[shIdx] )
							}
						}

						return( glm.out )

}	# createProbReg


