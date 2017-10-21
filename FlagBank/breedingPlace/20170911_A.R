# 
library(plyr)
source("20170911_A_H.R")
source("20170911_A_auxH.R")

# 한글 깨지지는 않겠지?

curWd <- getwd()
setwd("..")
FB <- getFlagBank()
setwd(curWd)

glm.out <- createProbReg(FB ,pDiff=10)


# 발생가능 범위를 생각하고
tStmp <- Sys.time()
perfLst <- list()
for( colIdx in c(1,3,4,6) ){	# colIdx<-3

	curDnaH <- FB$zh[,colIdx]
	curDnaH.range <- range(curDnaH)
	curDna.code <- curDnaH.range[1]:curDnaH.range[2]

	freq.tbl <- table(curDnaH)
	freq.NomiNum <- 7
	freq.NomiVal <- as.integer( names(freq.tbl)[ order( freq.tbl ,decreasing=T )[1:freq.NomiNum] ] )
	freq.NomiPos <- mapply(function(p){ which(curDna.code==p) } ,freq.NomiVal)

	charactList <- list()
	charactList[[1]] <- charactBasic( curDna.code )	# 시작점 charact. idStr baseStart
	# charactList[[(1+length(charactList))]]	<- charactRebound( curDna.code )
	charactList[[(1+length(charactList))]]	<- charactModu( 2)
	charactList[[(1+length(charactList))]]	<- charactModu( 5)
	charactList[[(1+length(charactList))]]	<- charactModu( 7)
	charactList[[(1+length(charactList))]]	<- charactModu(11)
	charactList[[(1+length(charactList))]]	<- charactIntDiv(pBase= 3)
	charactList[[(1+length(charactList))]]	<- charactIntDiv(pBase= 6)
	charactList[[(1+length(charactList))]]	<- charactIntDiv(pBase= 9)
	charactList[[(1+length(charactList))]]	<- charactIntDiv(pBase=12)
	charactList[[(1+length(charactList))]]	<- charactPreDiff(pDiffSize= 3)
	charactList[[(1+length(charactList))]]	<- charactPreDiff(pDiffSize= 5)
	charactList[[(1+length(charactList))]]	<- charactPreDiff(pDiffSize= 7)
	charactList[[(1+length(charactList))]]	<- charactPreDiff(pDiffSize=11)

	hSpan <- 100:length(curDnaH)
	nomineeList <- list()
	hitOrd	<- matrix( 0 ,nrow=length(hSpan) ,ncol=2)
	for( idx in seq_len(length(hSpan)) ) { # idx <- 1
		hIdx <- hSpan[idx]
		nominee <- nominateCode( pCode=curDna.code ,pH=curDnaH[1:(hIdx-1)] 
							,pCharactList=charactList
							,pProbFit=glm.out
							,pSeqHauntMax=200
							,pDiff=glm.out$diffSize	# for self descriptive code.
						)

		nominee$perfMtx <- inspCharPerf( pNominee=nominee ,pStd=curDnaH[hIdx] ,pCode<-curDna.code ,pCharactList<-charactList )
		nominee$stdVal <- curDnaH[hIdx]
		nominee$hisIdx <- hIdx

		hitPos <- which(curDnaH[hIdx]==curDna.code[order(nominee$finalProb,decreasing=T)])
		hitOrd[idx,1] <- hitPos
		hitOrd[idx,2] <- hitOrd[idx,1]*100 %/% length(curDna.code)

		nomineeList[[(1+length(nomineeList))]] <- nominee

	} # for(idx)

	sh.eadge <- c(-20,20)
	sh.range <- sh.eadge[1]:sh.eadge[2]
	shMtx.success	<- matrix( 0 ,nrow=length(charactList) ,ncol=length(sh.range) )
	shMtx.fail		<- matrix( 0 ,nrow=length(charactList) ,ncol=length(sh.range) )
	shMtx.tot		<- matrix( 0 ,nrow=length(charactList) ,ncol=length(sh.range) )
	shMtx			<- matrix( 0 ,nrow=length(charactList) ,ncol=length(sh.range) )
	colnames(shMtx.success) <- sh.range	;colnames(shMtx.fail) <- colnames(shMtx.success)
	colnames(shMtx) <- sh.range
	rownames(shMtx) <- sapply( charactList ,function(p){p$idStr} )

	for( chIdx in 1:length(charactList) ){ # chIdx <- 3

		mtx <- do.call( rbind ,lapply(nomineeList,function(p){p$perfMtx[chIdx,]}) )
		mtx[,"seqHaunt"] <- ifelse( mtx[,"seqHaunt"]<sh.eadge[1] ,sh.eadge[1] ,mtx[,"seqHaunt"] )
		mtx[,"seqHaunt"] <- ifelse( mtx[,"seqHaunt"]>sh.eadge[2] ,sh.eadge[2] ,mtx[,"seqHaunt"] )

		sh.sum <- tapply( mtx[,"success"] ,mtx[,"seqHaunt"] ,sum )
		shMtx.success[chIdx,names(sh.sum)] <- sh.sum
		sh.sum <- tapply( mtx[,"fail"] ,mtx[,"seqHaunt"] ,sum )
		shMtx.fail[chIdx,names(sh.sum)] <- sh.sum
		shMtx.tot <- shMtx.success + shMtx.fail
		shMtx <- shMtx.success / shMtx.tot

	} # chIdx
	
	perfObj <- list( nomineeList=nomineeList ,hitOrd=hitOrd )
	perfObj$dna.Code<- curDna.code
	perfObj$shMtx	<- shMtx
	perfObj$shMtx.range <- sh.range
	perfObj$colIdx	<- colIdx
	perfObj$charactList <- charactList
	perfLst[[colIdx]] <- perfObj

	k.FLogStr(sprintf("colIdx:%d ",colIdx))
	k.FLogStr(sprintf("    lvl3:%4.2f   lvl4:%4.2f   lvl5:%4.2f   lvl6:%4.2f   lvl7:%4.2f   lvl8:%4.2f"
					,mean(hitOrd[,1]<4)	,mean(hitOrd[,1]<5)	,mean(hitOrd[,1]<6)
					,mean(hitOrd[,1]<7)	,mean(hitOrd[,1]<8)	,mean(hitOrd[,1]<9)
				),pTime=F)

}	# for(colIdx)

tCost <- Sys.time()-tStmp
cat(sprintf("Time Cost : %.1f %s\n",tCost,units(tCost)))
save( perfLst ,file="Obj_perfLst1016.save" )


insIdx <- 6	# inspect index




# ------------------------------------------------------------------------------------------------------

		log.txt <- sprintf("%s.txt",pRptFile)
		log.pdf <- sprintf("%s.pdf",pRptFile)
		FLog <- function( pObj ,pTime=F ,pAppend=T ,pConsole=F ){
				if( !is.null(pRptFile) )
					k.FLog( pObj ,pFile=log.txt ,pTime=pTime ,pAppend=pAppend ,pConsole=pConsole )
			}
		FLogStr <- function( pMsg ,pTime=F ,pAppend=T ,pConsole=F ){
				if( !is.null(pRptFile) )
					k.FLogStr( pMsg ,pFile=log.txt ,pTime=pTime ,pAppend=pAppend ,pConsole=pConsole )
			}

		FLogStr(sprintf("Start report for colIdx:%2d",pPerfObj$colIdx),pAppend=F,pTime=T)
		dna.codeCol <- terrain.colors( length(pPerfObj$dna.Code) )
		charactList <- pPerfObj$charactList

		useCharList <- list()
		for( idx in seq_len(length(charactList)) ){	# idx <- 1
			charact <- charactList[[idx]]
			if( "baseStart"==charact$idStr ){
				useCharList[[(1+length(useCharList))]] <- pPerfObj$shMtx.range
			} else {
				sh <- pPerfObj$shMtx.range[ pPerfObj$shMtx[idx,]>=pCharUseThre ]
				useCharList[[(1+length(useCharList))]] <- sh[!is.na(sh)]
			}
		}

		stdRankPer <- rep( 0 ,length(pPerfObj$nomineeList) )
		stdEarnMtx <- matrix( 0 ,nrow=0 ,ncol=length(charactList) )
		cName <- c( "val" ,"ord.first" ,"ord.last" ,"ch.pos" ,"ch.neg" )
		stdMoveMtx <- matrix( 0 ,nrow=length(pPerfObj$nomineeList) ,ncol=length(cName) )
		colnames( stdMoveMtx ) <- cName
		
		range.eadge <- range(pPerfObj$shMtx.range)
		for( repIdx in seq_len(length(pPerfObj$nomineeList)) ){	# repIdx <- 3	# Report Index
			nomiObj <- pPerfObj$nomineeList[[repIdx]]

			ordHisMtx <- nomiObj$probMtx
			
			if( T ){	# useCharMtx 적용
				for( rIdx in 1:nrow(ordHisMtx) ){	# rIdx <- 1
					# 현 History에서, 현 charact의 seqHaunt는 ?
					# ordHisMtx
					seqHaunt <- nomiObj$probMtx.seqHaunt[rIdx,]
					seqHaunt <- ifelse(seqHaunt<range.eadge[1],range.eadge[1],seqHaunt)
					seqHaunt <- ifelse(seqHaunt>range.eadge[2],range.eadge[2],seqHaunt)

					useFlag <- seqHaunt %in% useCharList[[rIdx]]
					ordHisMtx[rIdx,!useFlag] <- 1
				}
			}

			for( rIdx in 1:nrow(ordHisMtx) ){
				probLine <- nomiObj$probMtx[rIdx,]
				if( rIdx == 1){
					ordHisMtx[rIdx,] <- probLine
				} else {
					ordHisMtx[rIdx,] <- probLine * ordHisMtx[(rIdx-1),]
				}
			}

			ordHisMtx.rnk <- ordHisMtx
			ordHisMtx.one <- ordHisMtx
			for( rIdx in 1:nrow(ordHisMtx) ){
				ordHisMtx.one[rIdx,] <- ordHisMtx[rIdx,] / max(ordHisMtx[rIdx,])
				ordHisMtx.rnk[rIdx,] <- rank( ordHisMtx[rIdx,] )
			}

			winnerOrd <- order(ordHisMtx.rnk[5,],decreasing=T)
			FLogStr( sprintf("%d th Report",repIdx ) )
			FLogStr( sprintf("  stdCode:%d  winner :%s"
							,pDnaH[repIdx] ,paste(pPerfObj$dna.Code[winnerOrd[1:5]],collapse=" ") 
						) )

			stdIdx <- which(pPerfObj$dna.Code == pDnaH[repIdx])
			std.rank <- ordHisMtx.rnk[nrow(ordHisMtx.rnk),stdIdx]
			stdRankPer[repIdx] <- 100-(std.rank*100/length(pPerfObj$dna.Code)) 
			FLogStr(sprintf("    Rank of Std value :%5.1f of %d (%4.1f%%)"
							,std.rank
							,length(pPerfObj$dna.Code)
							,stdRankPer[repIdx]
						)
				)

			stdEarnMtx <- rbind(stdEarnMtx,ordHisMtx.rnk[,stdIdx])
			stdMoveMtx[repIdx,"val"]		<- pDnaH[repIdx]
			stdMoveMtx[repIdx,"ord.first"]	<- ordHisMtx.rnk[1,stdIdx]
			stdMoveMtx[repIdx,"ord.last"]	<- ordHisMtx.rnk[nrow(ordHisMtx.rnk),stdIdx]
			stdMoveMtx[repIdx,"ch.pos"]	<- sum( ordHisMtx[2:nrow(ordHisMtx),stdIdx] > 1.001 )
			stdMoveMtx[repIdx,"ch.neg"]	<- sum( ordHisMtx[2:nrow(ordHisMtx),stdIdx] < 0.999 )

			if( T ) next

			png( sprintf("%s%02d_%04d.png",pRptFile,pPerfObj$colIdx,repIdx) ,height=300 ,width=300 )
			for( idx in length(winnerOrd):1 ){ # length(winnerOrd):1
				wIdx <- winnerOrd[idx]
				if( idx==length(winnerOrd) ){
					plot(ordHisMtx.rnk[,wIdx] 
							,ylim=c(0,length(pPerfObj$dna.Code)+1) ,type="l" ,col=dna.codeCol[idx] 
							,main=sprintf("Report Index %d, STD",repIdx)
						)
				} else {
					lines( 1:nrow(ordHisMtx.rnk) ,ordHisMtx.rnk[,wIdx] ,col=dna.codeCol[idx] )
				}
			}

			# winner
			points( 1:nrow(ordHisMtx.rnk) ,ordHisMtx.rnk[,winnerOrd[1]] ,col="blue" )
			# stdCode
			lines( 1:nrow(ordHisMtx.rnk) ,ordHisMtx.rnk[,stdIdx] ,col="red" )
			dev.off()

		}	# for(repIdx)

		FLogStr(sprintf("    Mean of std rank(as %%) : %4.1f%%",mean(stdRankPer)))

		png( sprintf("%s%02d.png",pRptFile,pPerfObj$colIdx) ,height=500 ,width=500 )
		par( mfrow=c(2,2) )
		hist( stdRankPer )
		
		for( rIdx in 1:nrow(stdEarnMtx) ){
			if( rIdx == 1 ){
				plot( 1:ncol(stdEarnMtx) ,stdEarnMtx[rIdx,] 
					,ylim=c(0,length(pPerfObj$dna.Code)+1) ,type="l" )
			} else {
				lines( 1:ncol(stdEarnMtx) ,jitter(stdEarnMtx[rIdx,])  )
			}
		}

		graphLim <- length(pPerfObj$dna.Code)
		plot( stdMoveMtx[,"ord.first"],stdMoveMtx[,"ord.last"] ,xlim=c(0,graphLim) ,ylim=c(0,graphLim) 
				,main=sprintf("개선율 : %0.1f%%",100*mean(stdMoveMtx[,"ord.first"] < stdMoveMtx[,"ord.last"])) 
			)
		lines( c(0,graphLim) ,c(0,graphLim) ,col="red")

		par( mfrow=c(1,1) )

		dev.off()







