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
#	A0. Zoid History 분석 (fRstLst)
# =====================================================================================
#		G0. 부분과 맞출 것.

stdFiltedCnt <- sapply( fRstLst ,length )
		# 		1   2   3   4   5   6   7   8   9  12 
		# 		8  63  99 101  69  31  10   3   3   1 
stdFiltCnt.all <- table(do.call(c,fRstLst))
#	barplot( stdFiltCnt.all )

stdRebCnt <- rep( 0 ,nrow(zhF) )
for( hIdx in 2:nrow(zhF) ){
	stdRebCnt[hIdx] <- sum(zhF[hIdx,]%in%zhF[(hIdx-1),])
}
stdRebCnt <- stdRebCnt[testSpan]
stdRebCnt.FiltedCnt <- sapply( fRstLst[stdRebCnt==0] ,length )
# table(stdRebCnt.FiltedCnt)
# 		 1  2  3  4  5  6  7 12 
# 		10 56 52 23  9  2  1  1 


opt.groupSize <- 3
saveId <- sprintf("%sG%d",saveId,opt.groupSize)

chosen.rebCnt <- which(stdRebCnt==0) # groupSize는 제각각.
chosen.rebCnt.hIdx <- testSpan[chosen.rebCnt]
chosen.group <- which(stdFiltedCnt==opt.groupSize)
chosen.group.hIdx <- testSpan[chosen.group]

# rebCnt==0 일때의 필터 분포. A0030 ,AK000.C 는 그냥 제외하자.
# table( do.call( c ,fRstLst[chosen.group] ) )
# 		A0010   A0020   A0030   A0040 A0100.A A0110.A 
# 			5       4       1       3      42      35 
# 		AJ000.B AJ000.C AK000.A AK000.B AK000.C AK000.D 
# 			5      11       3       7       2       3 
# 		AP000.A AP000.B AP000.C AP000.E AQ000.A AR000.A 
# 			8       8       7      25       3       8 
# 		AR000.B AS000.A C0000.A C1000.A 
# 			25       5     114      18 

# rebCnt==0 이고 opt.groupSize를 만족하는 상태에서의 필터분포
#  table( do.call( c ,fRstLst[intersect(chosen.rebCnt,chosen.group)] ) )
# 		A0010   A0020   A0030   A0040 A0100.A A0110.A AJ000.B 
# 			1       2       1       2      14      15       2
# 		AJ000.C AK000.A AK000.B AK000.C AK000.D AP000.A AP000.B 
# 			5       2       3       2       1       5       4 
# 		AP000.C AP000.E AQ000.A AR000.A AR000.B AS000.A C0000.A C1000.A 
# 			6       8       2       4      10       3      52      12 

k <- table( do.call( c ,fRstLst[intersect(chosen.rebCnt,chosen.group)] ) )

# -------------------------------------------------------------------------------------
#	Zoid History 분석 (fRstLst)

# opt.groupSize 기반.
stdRstMtx <- do.call( rbind ,fRstLst[chosen.group] )
rownames(stdRstMtx) <- testSpan[chosen.group]
stdFiltCnt <- table(as.vector(stdRstMtx)) # 선택된 opt.groupSize 내에서의 필터분포



# =====================================================================================
#	B0. allZoidMtx 분석 (remLst)
# =====================================================================================
allFiltCnt <- rep( 0 ,nrow(gEnv$allZoidMtx) )
for( rIdx in 1:length(remLst) ){
	allFiltCnt[remLst[[rIdx]]] <- allFiltCnt[remLst[[rIdx]]] + 1
}

allFiltName <- attributes(remLst)$name
allChosenIdx <- which(allFiltCnt==opt.groupSize)
curLogStr(sprintf("Initial allChosenIdx : %d",length(allChosenIdx)))

# =====================================================================================
#	C0. 임의 제거... 주사위를 굴려야 하는 부분.
# =====================================================================================
#	allChosenIdx 조정.

# rebCnt==0 일때의 필터 분포 (2번 이하로 나온 필터는 그냥 제거하자.)
# 		table( do.call( c ,fRstLst[chosen.group] ) )
rebFiltNm <- c( 	c( "A0030" ,"AK000.C" ) # 발생횟수 1회, 2회
					,setdiff( allFiltName ,unique(do.call(c,fRstLst[chosen.group])) )
				)
for( remNm in rebFiltNm ){
	allChosenIdx <- setdiff(allChosenIdx,remLst[[remNm]])
}

# rebCnt==0 이고 opt.groupSize 적용상태에서의 분포.
#	(1번 이하로 나온 필터는 그냥 제거하자.)
rebFiltNm <- c( 	c( "A0010" ,"A0030" ,"AK000.D") # 발생횟수 1회
					,setdiff( allFiltName 
							,unique(do.call(c,fRstLst[intersect(chosen.rebCnt,chosen.group)])) 
						)
				)
for( remNm in rebFiltNm ){
	allChosenIdx <- setdiff(allChosenIdx,remLst[[remNm]])
}

# rebCnt==0 이고 opt.groupSize 적용상태에서 자주 나오는 필터의 반복분포.
# QQE.todo

source("20180109_A_HDice.R")
allChosenIdx <- dice789( allZoidMtx ,zhF ,allChosenIdx )
curLogStr(sprintf("remove flex candObj : %d",length(allChosenIdx)))

# =====================================================================================
#	D0. candObj
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

curLogStr(sprintf("start candObj : %d",length(candObj$idx))

# =====================================================================================
#	E0. Zoid History 상에서 걸리지 않은 필터는 무조건 제외.
# =====================================================================================
unUsedIdx <- which( !( allFiltName %in% names(stdFiltCnt) ) )	# allFiltName[unUsedIdx]
flagCnt <- apply( candObj$filtIdMtx ,1 ,function(p){sum(p%in%unUsedIdx)} )
candObj <- cutCand( candObj ,which(flagCnt>0) )
curLogStr(sprintf("remove unUsedIdx candObj : %d",length(candObj$idx)))


# =====================================================================================
#	F0. 표준 절대 기준 탈락 확인.
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
curLogStr(sprintf("remove std harden candObj : %d",length(candObj$idx)))

# =====================================================================================
#	G0. 임의 기준 내(rebCnt==0) Zoid History 기반 절대 기준 탈락
# =====================================================================================
tEnv <- gEnv	;tStmp <- Sys.time()
stdRebCnt <- rep( 0 ,nrow(zhF) )	# A0 영역에서 testSpan 한정되었기 때문에 다시계산.
for( hIdx in 2:nrow(zhF) ){
	stdRebCnt[hIdx] <- sum(zhF[hIdx,]%in%zhF[(hIdx-1),])
}

tEnv$zhF <- gEnv$zhF[stdRebCnt==0,]

filtFuncLst <- getFiltLst.hard( ) # rebCnt==0 용도에 맞게 새로운 hard 함수 필요할 듯.
for( fIdx in seq_len(length(filtFuncLst)) ){
	tEnv$allZoidMtx <- gEnv$allZoidMtx[candObj$idx,]
	rstObj <- filtFuncLst[[fIdx]]( tEnv )
	candObj <- cutCand( candObj ,which(!rstObj$flag) )
	tDiff <- Sys.time() - tStmp
	curLogStr(sprintf("G0. current candidate : %d cost : %.1f%s (%s)"
					,length(candObj$idx),tDiff,units(tDiff),rstObj$filtId
			)
			,pConsole=T ,pTime=F
		)
}

curLogStr(sprintf("remove G0 harden candObj : %d",length(candObj$idx)))

# =====================================================================================
#	결과저장.
# =====================================================================================

candObj$groupSize <- opt.groupSize
save( candObj ,file=sprintf("./save/Obj_candObj%s.save",saveId) )


