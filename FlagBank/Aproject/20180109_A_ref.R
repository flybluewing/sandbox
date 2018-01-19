# 20180109_A_ref.R refinary
#	20180109_A.R 로부터 얻은 결과에 대한 정제.
source("../breedingPlace/20171116_A_H.R")
source("../breedingPlace/20171116_A_H_cliper.R")
source("../breedingPlace/20171116_B_H.R")
source("../breedingPlace/20171116_C_H.R")
source("../breedingPlace/20171116_D_H.R")
source("20180109_A_H.R")

saveId <- "0117_17" # 대상에 따라 바꿔사용.
load(sprintf("./save/Obj_gEnv%s.save",saveId))
load(sprintf("./save/Obj_fRstLst%s.save",saveId))
load(sprintf("./save/Obj_remLst%s.save",saveId))

allZoidMtx <- gEnv$allZoidMtx
zhF <- gEnv$zhF
testSpan <- 400:nrow(zhF)

curLogStr <- function( pMsg ,pConsole=F ,pTime=T ){
	k.FLogStr( pMsg ,pConsole=pConsole ,pTime=pTime 
				,pFile=sprintf("./log/A_ref%s.log",saveId)
			)
}

# =====================================================================================
#	Zoid History 분석 (fRstLst)
# =====================================================================================
stdFiltedCnt <- sapply( fRstLst ,length )
		# 		1   2   3   4   5   6   7   8   9  12 
		# 		8  63  99 101  69  31  10   3   3   1 
stdFiltCnt.all <- table(do.call(c,fRstLst))
#	barplot( stdFiltCnt.all )

opt.groupSize <- 5
saveId <- sprintf("%sG%d",saveId,opt.groupSize)
# -------------------------------------------------------------------------------------
#	Zoid History 분석 (fRstLst)
stdRstMtx <- do.call( rbind ,fRstLst[stdFiltedCnt==opt.groupSize] )
rownames(stdRstMtx) <- testSpan[which( stdFiltedCnt==opt.groupSize )]

# stdFiltCnt <- table(as.vector(stdRstMtx)) # 의미 없는 듯.

# =====================================================================================
#	allZoidMtx 분석 (remLst)
# =====================================================================================
allFiltCnt <- rep( 0 ,nrow(gEnv$allZoidMtx) )
for( rIdx in 1:length(remLst) ){
	allFiltCnt[remLst[[rIdx]]] <- allFiltCnt[remLst[[rIdx]]] + 1
}
allChosenIdx <- which(allFiltCnt==opt.groupSize)	# allFiltCnt==5는 4만개 정도.
allFiltName <- attributes(remLst)$name

# =====================================================================================
#	임의 제거... 주사위를 굴려야 하는 부분.
# =====================================================================================

source("20180109_A_HDice.R")
surviveIdx <- dice789( allZoidMtx ,zhF ,allChosenIdx )

# =====================================================================================
#	candObj
# =====================================================================================
curLogStr("startAnalysis")
candObj <- list( idx=allChosenIdx )
candObj$filtLst <- lapply(candObj$idx,function( p ){
							fndFlag <- sapply( remLst ,function(p2){ p %in% p2} )
							return( which(fndFlag) )
						}) # 7.5 min for 40k
candObj$filtIdMtx <- do.call( rbind ,candObj$filtLst )
candObj$filtNmMtx <- matrix( "" ,nrow=nrow(candObj$filtIdMtx) ,ncol=ncol(candObj$filtIdMtx) )
for( rIdx in 1:nrow(candObj$filtNmMtx) ){
	candObj$filtNmMtx[rIdx, ] <- allFiltName[candObj$filtIdMtx[rIdx,]]
}
candObj$filtCnt <- table(as.vector( candObj$filtNmMtx ))

cutCand <- function( rObj ,pCutIdx ){
	if( 0==length(pCutIdx) ){
		return( rObj )
	}
	newRObj <- list( idx=rObj$idx[-pCutIdx] )
	newRObj$filtLst <- rObj$filtLst[-pCutIdx]
	newRObj$filtIdMtx <- rObj$filtIdMtx[-pCutIdx,]
	newRObj$filtNmMtx <- rObj$filtNmMtx[-pCutIdx,]
	newRObj$filtCnt <- table(as.vector(newRObj$filtNmMtx))
	return( newRObj )
} # cutCand()

curLogStr("start candObj : %d",length(candObj$idx))

# =====================================================================================
#	Zoid History 상에서 걸리지 않은 필터는 무조건 제외.
# =====================================================================================
unUsedIdx <- which( !( allFiltName %in% names(stdFiltCnt) ) )	# allFiltName[unUsedIdx]
flagCnt <- apply( candObj$filtIdMtx ,1 ,function(p){sum(p%in%unUsedIdx)} )
candObj <- cutCand( candObj ,which(flagCnt>0) )
curLogStr("remove unUsedIdx candObj : %d",length(candObj$idx))

# =====================================================================================
#	임의 절대 기준 탈락 확인.
# =====================================================================================
remIdx.flex <- integer(0)
lastZoid <- zhF[nrow(zhF),]

# lastZoid 이후로 1개 이상 값 발생.
allCodeMtx <- gEnv$allZoidMtx[candObj$idx,]
cnt <- apply( allCodeMtx ,1 ,function(p){sum(lastZoid%in%p)})
candObj <- cutCand( candObj ,which(cnt>1) )

curLogStr("remove flex candObj : %d",length(candObj$idx))

# =====================================================================================
#	표준 절대 기준 탈락 확인.
# =====================================================================================
tEnv <- gEnv	;tStmp <- Sys.time()
filtFuncLst <- getFiltLst.hard( )
for( fIdx in seq_len(length(filtFuncLst)) ){
	tEnv$allZoidMtx <- gEnv$allZoidMtx[candObj$idx,]
	rstObj <- filtFuncLst[[fIdx]]( tEnv )
	candObj <- cutCand( candObj ,which(!rstObj$flag) )
	tDiff <- Sys.time() - tStmp
	curLogStr(sprintf("current candidate : %d cost : %.1f%s (%s)"
					,length(candObj$idx),tDiff,units(tDiff),rstObj$filtId
			)
			,pConsole=T ,pTime=F
		)
}
curLogStr("remove std harden candObj : %d",length(candObj$idx))

# =====================================================================================
#	결과저장.
# =====================================================================================

candObj$groupSize <- opt.groupSize
save( candObj ,file=sprintf("Obj_candObj%s.save",saveId) )


		# > tail(zhF)
		# 	E1 E2 E3 E4 E5 E6
		# 784  3 10 23 24 31 39
		# 785  4  6 15 25 26 33
		# 786 12 15 16 20 24 30
		# 787  5  6 13 16 27 28
		# 788  2 10 11 19 35 39
		# 789  2  6  7 12 19 45

# 1st. 2,19 제외
# 2nd. 2,3컬럼에서의 1차이 제외
# 3rd. 5 컬럼에서의 13 제외
# 4rd. 2 컬럼에서의 6
# 5th. Qoutient 에서의 3,2,1 비율 제외
# 6th. 같은 컬럼에서의 반복 제외.
# 7th. h-1 에서 같은 컬럼에서의 반복 제외.

