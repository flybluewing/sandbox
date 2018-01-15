# 20180109_A_ref.R refinary
#	20180109_A.R 로부터 얻은 결과에 대한 정제.
source("../breedingPlace/20171116_A_H.R")
source("../breedingPlace/20171116_A_H_cliper.R")
source("../breedingPlace/20171116_B_H.R")
source("../breedingPlace/20171116_C_H.R")
source("../breedingPlace/20171116_D_H.R")
source("20180109_A_H.R")

# save( gEnv ,file="./save/Obj_gEnv.save" )
load("./save/Obj_gEnv.save")
load("./save/Obj_fRstLst.save")
load("./save/Obj_remLst.save")

allZoidMtx <- gEnv$allZoidMtx
zhF <- gEnv$zhF
testSpan <- 400:nrow(zhF)

# save( fRstLst ,file="Obj_fRstLst.save" )
# load("Obj_fRstLst.save")

# =====================================================================================
#	5개 필터 기준으로...
#
#
#
filtedCnt <- sapply( fRstLst ,length )
fRstMtx <- do.call( rbind ,fRstLst[filtedCnt==5] )
rownames(fRstMtx) <- testSpan[which( filtedCnt==5 )]
# -------------------------------------------------------------------
# fRstMtx 에 대해.. 최대 가뭄은 15번. 
#	- 2번 까지는 동일반복 없음 taskId.001
	matMtx <- scanSameRow( fRstMtx )
# 	- ptn$nextVal 일치검사. 19개 기존 패턴 중 5개 일치 없음.(4개는 2번) taskId.002
	inspSpan <- 10:nrow(fRstMtx)
	flag <- rep( -1 ,nrow(fRstMtx) )
	for( iIdx in inspSpan ){
		ptn <- getPtnReb( fRstMtx[1:(iIdx-1),] )
		if( !is.null(ptn) ){
			flag[iIdx] <- sum(fRstMtx[iIdx,]==ptn$nextRow)
		}
	}

# 

#  - 아예 한번도 안 나온 필터가 있긴 하다... taskId.003
stdFiltCnt <- table(as.vector(fRstMtx))
	# A0010 A0020 A0030 A0100.A A0110.A AK000.A AK000.C AK000.D AL000.A 
	#     4     2     6      42      40       5       4       5      14 
	# AP000.A AP000.B AP000.C AP000.D AP000.E AQ000.A AR000.A AR000.B AS000.A C0000.A C1000.A 
	#      4       4       3       2      29       4       9      18      62      69      19 


# save( remLst ,file="Obj_remLst.save" )
# load("Obj_remLst.save")

filtCnt <- rep( 0 ,nrow(gEnv$allZoidMtx) )
for( rIdx in 1:length(remLst) ){
	filtCnt[remLst[[rIdx]]] <- filtCnt[remLst[[rIdx]]] + 1
}
cnt5Idx <- which(filtCnt==5)	# filtCnt==5는 4만개 정도.

#========================================================================================
# candidate 들에 대한 정리. (cand.*)
filtName <- attributes(remLst)$name

candObj <- list( idx=cnt5Idx )
candObj$filtLst <- lapply(candObj$idx,function( p ){
							fndFlag <- sapply( remLst ,function(p2){ p %in% p2} )
							return( which(fndFlag) )
						}) # 7.5 min for 40k
candObj$filtIdMtx <- do.call( rbind ,candObj$filtLst )
candObj$filtNmMtx <- matrix( "" ,nrow=nrow(candObj$filtIdMtx) ,ncol=ncol(candObj$filtIdMtx) )
for( rIdx in 1:nrow(candObj$filtNmMtx) ){
	candObj$filtNmMtx[rIdx, ] <- filtName[candObj$filtIdMtx[rIdx,]]
}

cutCand <- function( rObj ,pCutIdx ){
	if( 0==length(pCutIdx) ){
		return( rObj )
	}
	newRObj <- list( idx=rObj$idx[-pCutIdx] )
	newRObj$filtLst <- rObj$filtLst[-pCutIdx]
	newRObj$filtIdMtx <- rObj$filtIdMtx[-pCutIdx,]
	newRObj$filtNmMtx <- rObj$filtNmMtx[-pCutIdx,]
	return( newRObj )
} # cutCand()

#========================================================================================
#	이제 4만개에서 하나씩 좁혀나가자.
#------------------------------------------
#	taskId.003 적용.
unUsedIdx <- which( !( filtName %in% names(stdFiltCnt) ) )	# filtName[unUsedIdx]
flagCnt <- apply( candObj$filtIdMtx ,1 ,function(p){sum(p%in%unUsedIdx)} )
candObj <- cutCand( candObj ,which(flagCnt>0) )

# taskId.001. 모두 동일한 패턴이 2H 연속으로 나온 적은 없다.
#				별반 효과 없는 거 같다....
flagCnt <- rep( 0 ,length(candObj$idx) )
for( dIdx in 1:2 ){
	extRow <- fRstMtx[nrow(fRstMtx)-dIdx+1 ,]
	flag <- apply( candObj$filtNmMtx ,1 ,function(p){all(p==extRow)})
	flagCnt[flag] <- dIdx
}
candObj <- cutCand( candObj ,which(flagCnt>0) )

# taskId.002. 모두 동일한 패턴이 2H 연속으로 나온 적은 없다.
#				이것도 별반 효과 없다.
flag <- rep( TRUE ,length(candObj$idx) )
ptn <- getPtnReb( fRstMtx )
if( !is.null(ptn) ){
	cnt <- apply( candObj$filtNmMtx ,1 ,function(p){sum(p==ptn$nextRow)} )
	flag <- cnt < ncol(candObj$filtNmMtx)
}
candObj <- cutCand( candObj ,which(!flag) )




