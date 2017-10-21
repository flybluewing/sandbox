
#	pPerfObj=perfLst[[insIdx]]	;pDnaH=FB$zh[,insIdx]
#	pRptFile="./report/perfObj"	;pCharUseThre=0.7
rptPerfObj <- function( pPerfObj ,pDnaH ,pRptFile="./report/perfObj" ,pCharUseThre=0.7 ){

	for( nIdx in seq_len(length(pPerfObj$nomineeList)) ){	# nIdx <- 1

		nomiObj <- pPerfObj$nomineeList[[nIdx]]
		pdf( sprintf("%s_c%03d_n%03d.pdf",pRptFile,pPerfObj$colIdx,nIdx) )
		par( mfrow=c(3,2))

		stdIdx <- which(pPerfObj$dna.Code==nomiObj$stdVal)
		sh.eadge <- c(-20,20)
		for( idx in 1:nrow(nomiObj$probMtx) ){
			seqHaunt <- nomiObj$probMtx.seqHaunt[idx,]
			seqHaunt <- ifelse( seqHaunt<sh.eadge[1] ,sh.eadge[1] ,seqHaunt )
			seqHaunt <- ifelse( seqHaunt>sh.eadge[2] ,sh.eadge[2] ,seqHaunt )
			seqHaunt <- 0.5 + seqHaunt / (sh.eadge[2]*2)
			plot( rep(0.5,ncol(nomiObj$probMtx)) ,ylim=c(0,1) ,type="l" 
				,main=sprintf("%d th %s",idx,pPerfObj$charactList[[idx]]$idStr) )
			lines( 1:ncol(nomiObj$probMtx) ,nomiObj$probMtx[idx,] ,col="red" )
			lines( 1:ncol(nomiObj$probMtx) ,seqHaunt ,col="blue" )
			lines( c(stdIdx,stdIdx) ,c(0,1) ,col="green")
		}

		dev.off()

		probAccMtx <- matrix(0,nrow=nrow(nomiObj$probMtx),ncol=ncol(nomiObj$probMtx))
		for( rIdx in 1:nrow(nomiObj$probMtx) ){
			if( 1==rIdx ){
				probAccMtx[rIdx,] <- nomiObj$probMtx[rIdx,]
			} else {
				probAccMtx[rIdx,] <- probAccMtx[(rIdx-1),] * nomiObj$probMtx[rIdx,] * 10
			}
		}

		probAccMtx.ord <- t( apply( probAccMtx, 1, rank ) )

		ord.col <- terrain.colors( length(pPerfObj$dna.Code) )

		png( sprintf("%s_c%03d_n%03d.png",pRptFile,pPerfObj$colIdx,nIdx) )
		plot( NULL ,xlab="charact" ,ylab="ord" 
				,xlim=c(1,nrow(probAccMtx.ord)) ,ylim=c(1,length(pPerfObj$dna.Code)) 
				,main=sprintf("Col:%3d StdVal:%3d",pPerfObj$colIdx,pPerfObj$dna.Code[stdIdx] )
			)

		drawOrd <- order(probAccMtx[nrow(probAccMtx),],decreasing=T)
		for( idx in 1:length(drawOrd) ){
			cIdx <- drawOrd[idx]
			lines( 1:nrow(probAccMtx.ord) ,probAccMtx.ord[,cIdx] ,col=ord.col[idx])
		}
		lines( 1:nrow(probAccMtx.ord) ,probAccMtx.ord[,stdIdx] ,col="red" )

		winnerIdx <- which.max( probAccMtx.ord[nrow(probAccMtx.ord),] )
		points( 1:nrow(probAccMtx.ord) ,probAccMtx.ord[,winnerIdx] ,col="blue" )
		dev.off()

	} # for(nIdx)

} # rptPerfObj( )
