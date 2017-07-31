source("BuildTable_H.R")

# ---------------------------------------------------------------------
# DnaHisMtx : DNA발생 Matrix
# 	column : tGlobal$dnaTypes

DnaHisMtx <- matrix( ncol=length(tGlobal$dnaTypes), nrow=nrow(RawZH) )
for( idx in 1:nrow(RawZH) ){
	DnaHisMtx[idx,] <- match( tGlobal$dnaTypes, RawZH[idx,] )
}
DnaHisMtx[!is.na(DnaHisMtx)] <- 1
DnaHisMtx[ is.na(DnaHisMtx)] <- 0

DnaFreqLst <- list()
for( idx in 1:length(tGlobal$dnaTypes) ){
	DnaFreqLst[[idx]] <- seqCount(DnaHisMtx[,idx])
}

# ---------------------------------------------------------------------
# DnaPosHisMtx : 위치에 따른 DNA발생 Matrix
# 	column : tGlobal$dnaTypes
for( idx in 1:tGlobal$dnaLength ) {
	
	# 각 위치별로 발생한 DNA염기와, 발생(연속)빈도 파악.

}	# for(idx)

