#  중지!!! 결과를 합치는 과정에서 서로의 새로운 matchLst[[*]]을 확인하고 합치려다 rbind()가 과도하게 발생할 수 있다.
#	메모리 사용량도 걱정 됨...

# findOverlap.mc <- function( pMatchInfo ,pChunkSize=2000 ,pDebug=F )
# 	findOverlap.mc() 에서 사용될, export 시켜야 하는 함수들은 호출전에 미리 export 시킬 것.
#		(루프문이 돌 때마다 export 시키는 건 좋지 못하므로.)
#		sfExport("findOverlap.mc.sub");	sfExport("compareFilm.lf");	sfExport("k.FLogStr");	sfExport("k.FLogOpt");
library(snowfall)
sfInit( parallel=F ,cpus=2 )
sfExport("findOverlap.mc.sub");	sfExport("compareFilm.lf");	sfExport("k.FLogStr");	sfExport("k.FLogOpt");
pMatchInfo=stageLst[[1]]; pChunkSize=20; pDebug=T
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------

tStamp <- Sys.time()
matchLst.l <- length(pMatchInfo$matchLst)
chunkNum <- matchLst.l %/% pChunkSize
chunkBand <- 1:pChunkSize

mcLst <- list()
for( idx in 0:chunkNum ){
	band <- chunkBand+(idx*pChunkSize)
	band <- band[band<=matchLst.l]
	# print( band )
	mcLst[[(1+length(mcLst))]] <- pMatchInfo$matchLst[band]
}
if( pDebug ){
	k.FLogStr(sprintf(" debug:findOverlap.mc - timeCost(1):%.1f",Sys.time()-tStamp))
}

# 
stgLst <- sfLapply(mcLst,	function( p ){
							stg <- findOverlap.mc.sub( p ,pMCsub=T ,pDebug=pDebug )
							return( stg )
						} # function
				)
if( pDebug ){
	k.FLogStr(sprintf(" debug:findOverlap.mc - timeCost(2):%.1f",Sys.time()-tStamp))
}
				
rObj <- stgLst[[1]]
rObj$depth=(pMatchInfo$depth+1)
rObj$idStr.winDef <- pMatchInfo$idStr.winDef

# rObj에 stgLst[2:] 데이터 흡수시키기.
if( 1<length(stgLst) ){
	for( idx in 2:length(stgLst) ){
		# rObj$matchLst[[n]] 가 기존에 있는 것이면 흡수,
		# 없는 것이면 새로 추가...
		# 그런데 중단하자. 복잡하기도 하지만 대형 rbind() 작업이 여러번 돌면서 시간 까먹을 수도 있다.
	} # for
}

# QQE : compPairLst 으로부터 compPair 생성.


# ==========================================================================
# --------------------------------------------------------------------------
#	추가 함수
# --------------------------------------------------------------------------
