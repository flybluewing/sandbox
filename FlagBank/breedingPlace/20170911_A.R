# 
library(plyr)
source("20170911_A_H.R")

# 한글 깨지지는 않겠지?

curWd <- getwd()
setwd("..")
FB <- getFlagBank()
setwd(curWd)

glm.out <- createProbReg(FB)

# 발생가능 범위를 생각하고

for( colIdx in 1:6 ){
	for( sMaxIdx in seq(3,50,3) ){
		curDnaH <- FB$zh[,colIdx]
		curDnaH.range <- range(curDnaH)
		curDna.code <- curDnaH.range[1]:curDnaH.range[2]

		charactList <- list()
		charactList[[(1+length(charactList))]]	<- charactModu(2)
		charactList[[(1+length(charactList))]]	<- charactModu(5)
		charactList[[(1+length(charactList))]]	<- charactModu(7)
		charactList[[(1+length(charactList))]]	<- charactIntDiv(pBase=3)
		charactList[[(1+length(charactList))]]	<- charactIntDiv(pBase=6)

		freq.tbl <- table(curDnaH)
		freq.NomiNum <- 7
		freq.NomiVal <- as.integer( names(freq.tbl)[ order( freq.tbl ,decreasing=T )[1:freq.NomiNum] ] )
		freq.NomiPos <- mapply(function(p){ which(curDna.code==p) } ,freq.NomiVal)

		hSpan <- 100:length(curDnaH)
		nomineeList <- list()
		hitOrd	<- matrix( 0 ,nrow=length(hSpan) ,ncol=2)
		for( idx in seq_len(length(hSpan)) ) { # idx <- 1
			hIdx <- hSpan[idx]
			nominee <- nominateCode( pCode=curDna.code ,pH=curDnaH[1:(hIdx-1)] 
								,pCharactList=charactList
								,pProbFit=glm.out
								,pSeqHauntMax=sMaxIdx
							)
			
			hitPos <- which(curDnaH[hIdx]==curDna.code[order(nominee$finalProb,decreasing=T)])
			hitOrd[idx,1] <- hitPos
			hitOrd[idx,2] <- hitOrd[idx,1]*100 %/% length(curDna.code)

			nomineeList[[(1+length(nomineeList))]] <- nominee

		}

		k.FLogStr(sprintf("colIdx:%d  sMaxIdx:%d",colIdx,sMaxIdx))
		k.FLogStr(sprintf("    lvl3:%4.2f   lvl4:%4.2f   lvl5:%4.2f   lvl6:%4.2f   lvl7:%4.2f   lvl8:%4.2f"
						,mean(hitOrd[,1]<4)	,mean(hitOrd[,1]<5)	,mean(hitOrd[,1]<6)
						,mean(hitOrd[,1]<7)	,mean(hitOrd[,1]<8)	,mean(hitOrd[,1]<9)
					),pTime=F)
	
	}	# for(sMaxIdx)
}	# for(colIdx)


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

for( wIdx in length(winnerOrd):1 ){
	if( wIdx==length(winnerOrd) ){
		plot(ordHisMtx.rnk[,winnerOrd[wIdx]] 
			,ylim=c(0,length(winnerOrd)+1) ,type="l" ,col=curDna.codeCol[wIdx] )
	} else {
		lines( 1:nrow(ordHisMtx.rnk) ,ordHisMtx.rnk[,wIdx] ,col=curDna.codeCol[wIdx] )
	}
}

# winner
points( 1:nrow(ordHisMtx.rnk) ,ordHisMtx.rnk[,winnerOrd[1]] ,col="blue" )
# stdCode
stdIdx <- which(curDna.code == stdCode[repIdx])
lines( 1:nrow(ordHisMtx.rnk) ,ordHisMtx.rnk[,stdIdx] ,col="red" )




