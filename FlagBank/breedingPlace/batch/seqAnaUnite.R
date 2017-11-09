# 파일명 : seqAnaUnite
#	분할된 hAnaSet 데이터 병합 예시 코드
curWd <- getwd()
setwd("./../..")
source("hCommon.R")
FB <- getFlagBank() # 사실 FB는 필요 없는데.. H 파일 땜시.
setwd(curWd)

setwd("./..")
source("20170917_A_H.R")
source("20171029_A_H.R")
source("20171029_A_auxH.R")
setwd(curWd)

saveFiles <- dir(pattern="Obj1107_hAnaSet.+\\.save")

hAnaSetF <- NULL
for( idx in seq_len(length(saveFiles)) ){
	myObj <- load( saveFiles[idx] )	# hAnaSet
	k.FLogStr(sprintf("Merging %s",saveFiles[idx]) ,pConsole=T )
	
	if( 1==idx ){
		hAnaSetF <- hAnaSet
		next
	}
	for( hIdx in seq_len(length(hAnaSet$hLst)) ){
		for( eIdx in seq_len(length(hAnaSet$hLst[[hIdx]]$anaLst)) ){
			# hIdx<-1	;eIdx<-1
			colFlag <- sapply( hAnaSet$hLst[[hIdx]]$anaLst[[eIdx]] ,function(p){"seqAnaObj"==class(p)} )
			if( 0==length(colFlag) )
				next
			hAnaSetF$hLst[[hIdx]]$anaLst[[eIdx]][which(colFlag)] <- 
							hAnaSet$hLst[[hIdx]]$anaLst[[eIdx]][which(colFlag)]
		}
	}

} # for(idx)

save( hAnaSetF ,file="Obj1107_hAnaSet_Full.save")