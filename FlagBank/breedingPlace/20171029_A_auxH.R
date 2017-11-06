# 20171029_A_auxH.R

# analyzeSeq() 함수 결과물의 동일여부 확인
# 	pSet1 <- hAnaSet	;pSet2 <- hAnaSet
checkSame.hAnaSet <- function( pSet1 ,pSet2 ){

	hLst.len <- length(pSet1$hLst)
	if( hLst.len != length(pSet2$hLst) ){
		return(sprintf("$hLst length"))
	}

	for( hIdx in seq_len(hLst.len) ){
		if( pSet1$hLst[[hIdx]]$stdH != pSet2$hLst[[hIdx]]$stdH ){
			return(sprintf("hIdx:%3d stdH",hIdx))
		}
		
		ele.len <- length(pSet1$hLst[[hIdx]]$anaLst)
		if( ele.len != length(pSet2$hLst[[hIdx]]$anaLst) ){
			return(sprintf("hIdx:%3d ele.len",hIdx))
		}

		for( eIdx in seq_len(ele.len) ){
			col.len <- length(pSet1$hLst[[hIdx]]$anaLst[[eIdx]])
			if( col.len != length(pSet2$hLst[[hIdx]]$anaLst[[eIdx]]) ){
				return(sprintf("hIdx:%3d eIdx:%3d col.len",hIdx,eIdx))
			}
			anaObjLst1 <- pSet1$hLst[[hIdx]]$anaLst[[eIdx]]
			anaObjLst2 <- pSet2$hLst[[hIdx]]$anaLst[[eIdx]]
			for( cIdx in seq_len(col.len) ){
				cRst <- all.equal( anaObjLst1[[cIdx]] ,anaObjLst2[[cIdx]] )
				if( "logical"!=class(cRst) || !cRst ){
					return(sprintf("hIdx:%3d eIdx:%3d cIdx:%3d anaObj",hIdx,eIdx,cIdx))
				}
			}
		} # for(eIdx)

	} # for(hIdx)

	return("all same.")
}

