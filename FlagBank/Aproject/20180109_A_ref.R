# 20180109_A_ref.R refinary
#	20180109_A.R 로부터 얻은 결과에 대한 정제.
source("../breedingPlace/20171116_A_H.R")
source("../breedingPlace/20171116_A_H_cliper.R")
source("../breedingPlace/20171116_B_H.R")
source("../breedingPlace/20171116_C_H.R")
source("../breedingPlace/20171116_D_H.R")
source("20180109_A_H.R")
source("20180109_B_H.R")

saveId <- "0124_12" # 대상에 따라 바꿔사용.
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
#	A0. Zoid History 분석
# =====================================================================================
#		G0. 부분과 맞출 것.

stdFiltedCnt <- sapply( fRstLst ,length )
	# 	   0    1    2    3    4    5    6    7    9 
	# 	14.6 29.2 30.0 13.0  8.4  3.3  0.8  0.5  0.3
stdFiltCnt.all <- table(do.call(c,fRstLst))
	# 필터별 zoid history 필터링 수.

allFiltName <- attributes(remLst)$names

allFiltCnt <- rep( 0 ,nrow(gEnv$allZoidMtx) )
for( rIdx in 1:length(remLst) ){
	allFiltCnt[remLst[[rIdx]]] <- allFiltCnt[remLst[[rIdx]]] + 1
}

# =====================================================================================
#	B0. 기초 필터링.
# =====================================================================================
#		gSel$surviveIdx 에서 단계적으로 제외시켜 나가자.
#			./save/Obj_remLstNNNN_NN_hard.save 데이터를 사용.
gSel <- list( surviveIdx = 1:nrow(gEnv$allZoidMtx) )
gSel$filtIdx0 <- which(allFiltCnt == 0 )
gSel$filtIdx3 <- which(allFiltCnt == 3 )

# -------------------------------------------------------------------------------------
#	B1.1 절대 제거 필터링 
#		특정 필터는 아예 발생불가로 치고, 절대기준으로서 잘라낸다.
forbiddenFilt <- c( 	"A0050"		# 2.5% Quo4 3개 이상.
						,"AK000.C"	# 4.0% width 과거패턴 재발.
						,"AP000.C"	# 2.5% 한가지 Quo가 4개 이상.
						,"AP000.D"	# 1.5% Quo의 nextRow
					)
for( filtName in forbiddenFilt ){
	gSel$surviveIdx <- setdiff( gSel$surviveIdx ,remLst[[filtName]] )
}
gSel$filtIdx0 <- gSel$filtIdx0 # 애시당초 아무런 필터링이 없으므로..
gSel$filtIdx3 <- intersect( gSel$surviveIdx ,gSel$filtIdx3 )


# =====================================================================================
#	C0. 기초 필터링.
# =====================================================================================
# gSel$filtIdx0 을 대상으로 함. 차후 함수로 바뀔 부분
#	정의될 파라미터
#		- surviveIdx ,gEnv

surviveIdx	<- gSel$filtIdx0	# surviveIdx는 allZoidMtx의 rIdx임을 명심!!!

# -------------------------------------------------------------------------------------
#	C1. .hard() 함수 적용.

filtFuncLst.hard <- getFiltLst.hard()	# filtIdx0에겐 c(5,12,18,19,21,22)만 유효..

tEnv <- gEnv
tEnv$allZoidMtx <- gEnv$allZoidMtx[surviveIdx,]

tStmp <- Sys.time()
for( filtIdx in seq_len(length(filtFuncLst.hard)) ){
	rstObj <- filtFuncLst.hard[[filtIdx]]( tEnv )
	surviveIdx <- surviveIdx[rstObj$flag]
	tEnv$allZoidMtx <- tEnv$allZoidMtx[rstObj$flag,]
	k.FLogStr(sprintf("[C1.]%s survive:%d",rstObj$filtId,sum(rstObj$flag)))
}
tDiff <- Sys.time() - tStmp		# 1.9hr for 55만
tEnv <- NULL	# 중요한 것은 surviveIdx임.

# -------------------------------------------------------------------------------------
#	C2.0 rebCnt 기준으로 나누어 처리.
#		역시 별도 함수 제작 대상.
#			pSurviveIdx ,pEnv ,pRebCnt
lastZoid <- gEnv$zhF[nrow(gEnv$zhF),]
allRebCnt <- apply( pEnv$allZoidMtx[surviveIdx,] ,1 ,function(p){sum(p%in%lastZoid)} )
workRebCnt <- 0

# 파라미터 준비구역.
pEnv <- gEnv
pSurviveIdx <- surviveIdx[allRebCnt==workRebCnt]
pRebCnt <- workRebCnt

# 함수시작.

stdRebCnt <- rep( 0 ,nrow(pEnv$zhF) )
for( hIdx in 2:nrow(pEnv$zhF) ){
	stdRebCnt[hIdx] <- sum( pEnv$zhF[hIdx,] %in% pEnv$zhF[(hIdx-1),] )
}

tStmp <- Sys.time()
tSurviveIdx <- pSurviveIdx
tEnv <- pEnv
tEnv$zhF <- pEnv$zhF[stdRebCnt==pRebCnt,]
filtLst.hard4RebCnt <- getFiltLst.hard4RebCnt()
for( filtIdx in seq_len(length(filtLst.hard4RebCnt)) ){
	tEnv$allZoidMtx <- pEnv$allZoidMtx[tSurviveIdx,]
	rstObj <- filtLst.hard4RebCnt[[filtIdx]](tEnv)
	tSurviveIdx <- tSurviveIdx[rstObj$flag]
}
tDiff <- Sys.time() - tStmp

#	return( tSurviveIdx )



# -[testing 영역]-------------------------------------------------------------
testSpan.reb <- 200:nrow(zhF.reb)	# AJ000.C 등에서 100이상 필요.

remHardLst <- list()
filtLst.hard4RebCnt <- getFiltLst.hard4RebCnt()
for( filtIdx in seq_len(length(filtLst.hard4RebCnt)) ){
	filtId <- NULL
	tFlag <- rep( TRUE ,nrow(zhF.reb) )
	tIdx.dbg <- 0
	for( tIdx in testSpan.reb ){
		tIdx.dbg <- tIdx
		tEnv <- pEnv
		tEnv$zhF <- zhF.reb[1:(tIdx-1),]
		tEnv$allZoidMtx <- zhF.reb[tIdx,,drop=F]

		rstObj <- filtLst.hard4RebCnt[[filtIdx]](tEnv)
		tFlag[tIdx] <- rstObj$flag[1]
		filtId <- rstObj$filtId
	}
	remHardLst[[filtId]] <- tFlag
	k.FLogStr(sprintf("  filtId:%s failed:%d/%d"
		,filtId,sum(!tFlag),length(testSpan.reb) ))
}









