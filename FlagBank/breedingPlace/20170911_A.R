# 
library(plyr)
source("20170911_A_H.R")

# 한글 깨지지는 않겠지?

curWd <- getwd()
setwd("..")
FB <- getFlagBank()
setwd(curWd)

glm.out <- createProbReg(FB ,pDiff=10)


# 발생가능 범위를 생각하고
tStmp <- Sys.time()
perfLst <- list()
for( colIdx in 1:6 ){	# colIdx<-3

	curDnaH <- FB$zh[,colIdx]
	curDnaH.range <- range(curDnaH)
	curDna.code <- curDnaH.range[1]:curDnaH.range[2]

	freq.tbl <- table(curDnaH)
	freq.NomiNum <- 7
	freq.NomiVal <- as.integer( names(freq.tbl)[ order( freq.tbl ,decreasing=T )[1:freq.NomiNum] ] )
	freq.NomiPos <- mapply(function(p){ which(curDna.code==p) } ,freq.NomiVal)

	charactList <- list()
	charactList[[1]] <- charactBasic( curDna.code )	# 시작점 charact. 자기 자신의 mean, meanDiff, seqHaunt를 물고 시작. idStr baseStart
	charactList[[(1+length(charactList))]]	<- charactModu(2)
	charactList[[(1+length(charactList))]]	<- charactModu(5)
	charactList[[(1+length(charactList))]]	<- charactModu(7)
	charactList[[(1+length(charactList))]]	<- charactIntDiv(pBase=3)
	charactList[[(1+length(charactList))]]	<- charactIntDiv(pBase=6)
	
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





pPerfObj=perfLst[[1]]	;pDnaH=FB$zh[,1]	
pRptFile="./report/perfObj"	;pCharUseThre=0.7	;pCharactList=charactList


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

		FLogStr(sprintf("Start report"),pAppend=F,pTime=T)
		dna.codeCol <- terrain.colors( length(pPerfObj$dna.Code) )
		
		useCharList <- list()
		for( idx in seq_len(length(pCharactList)) ){	# idx <- 1
			charact <- pCharactList[[idx]]
			if( "baseStart"==charact$idStr ){
				useCharList[[(1+length(useCharList))]] <- perfObj$shMtx.range
			} else {
				sh <- perfObj$shMtx.range[ pPerfObj$shMtx[idx,]>=pCharUseThre ]
				useCharList[[(1+length(useCharList))]] <- sh[!is.na(sh)]
			}
		}

		stdRankPer <- rep( 0 ,length(pPerfObj$nomineeList) )
		for( repIdx in seq_len(length(pPerfObj$nomineeList)) ){	# repIdx <- 3	# Report Index
			nomiObj <- pPerfObj$nomineeList[[repIdx]]

			#	probMtx 로깅
			# str <- apply( nomiObj$probMtx ,1 ,function(p){ paste(sprintf("%4.1f",p),collapse=" ") } )
			# str <- paste( "  " ,str ,collapse="\n")
			# FLogStr( str ,pTime=F )

			ordHisMtx <- nomiObj$probMtx
			for( rIdx in 1:nrow(ordHisMtx) ){
				probLine <- nomiObj$probMtx[rIdx,]
				if( rIdx == 1){
					ordHisMtx[rIdx,] <- probLine
				} else {
					ordHisMtx[rIdx,] <- probLine * ordHisMtx[(rIdx-1),]
				}
			}

			# useCharMtx 적용
			for( rIdx in 1:nrow(ordHisMtx) ){	# rIdx <- 1
				# 현 History에서, 현 charact의 seqHaunt는 ?
				# ordHisMtx
				useFlag <- nomiObj$probMtx.seqHaunt[rIdx,] %in% useCharList[[rIdx]]
				ordHisMtx[rIdx,!useFlag] <- 1
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


			if( T ) next
			
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

		}	# for(repIdx)

		FLogStr(sprintf("    Mean of std rank(as %%) : %4.1f%%",mean(stdRankPer)))
		hist( stdRankPer )



# --[ Report ]----------------------------------
curDna.codeCol <- terrain.colors( length(curDna.code) )
stdCode <- curDnaH[hSpan]

repIdx <- 3
nomiObj <- nomineeList[[repIdx]]
winnerOrd <- order(nomiObj$finalProb,decreasing=T)

k.FLogStr( sprintf("%d th Report",repIdx ) )
k.FLogStr( sprintf("  stdCode:%d  winner :%s"
				,stdCode[repIdx] ,paste(curDna.code[winnerOrd[1:5]],collapse=" ") 
			) )

str <- apply( nomiObj$probMtx ,1 ,function(p){ paste(sprintf("%4.1f",p),collapse=" ") } )
str <- paste( "  " ,str ,collapse="\n")
k.FLogStr( str ,pTime=F )

ordHisMtx <- nomiObj$probMtx
ordHisMtx.rnk <- ordHisMtx
ordHisMtx.one <- ordHisMtx
for( rIdx in 1:nrow(ordHisMtx) ){

	probLine <- nomiObj$probMtx[rIdx,]
	if( rIdx == 1){
		ordHisMtx[rIdx,] <- probLine
	} else {
		ordHisMtx[rIdx,] <- probLine * ordHisMtx[(rIdx-1),]
	}

	ordHisMtx.one[rIdx,] <- ordHisMtx[rIdx,] / max(ordHisMtx[rIdx,])
	ordHisMtx.rnk[rIdx,] <- rank( ordHisMtx[rIdx,] )
}

for( idx in length(winnerOrd):1 ){ # length(winnerOrd):1
	wIdx <- winnerOrd[idx]
	if( idx==length(winnerOrd) ){
		plot(ordHisMtx.rnk[,wIdx] 
			,ylim=c(0,length(curDna.code)+1) ,type="l" ,col=curDna.codeCol[idx] )
	} else {
		lines( 1:nrow(ordHisMtx.rnk) ,ordHisMtx.rnk[,wIdx] ,col=curDna.codeCol[idx] )
	}
}

# winner
points( 1:nrow(ordHisMtx.rnk) ,ordHisMtx.rnk[,winnerOrd[1]] ,col="blue" )
# stdCode
stdIdx <- which(curDna.code == stdCode[repIdx])
lines( 1:nrow(ordHisMtx.rnk) ,ordHisMtx.rnk[,stdIdx] ,col="red" )




