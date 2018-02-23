# 20180109_C.R 교차모델
#	Code : cf - cross filter
source("../breedingPlace/20171116_A_H.R")
source("../breedingPlace/20171116_A_H_cliper.R")
source("../breedingPlace/20171116_B_H.R")
source("../breedingPlace/20171116_C_H.R")
source("../breedingPlace/20171116_D_H.R")
source("20180109_A_H.R")
source("20180109_C_H.R")

saveId <- "0127_23"
load(sprintf("./save/Obj_gEnv%s.save",saveId))
load(sprintf("./save/Obj_fRstLst%s.save",saveId))
load(sprintf("./save/Obj_remLst%s.save",saveId))



cfObjLst <- NULL
encValLst <- list()
for( tIdx in 10:nrow(gEnv$zhF) ){	# lastZoid 기반 동작들 때문에 1부터 시작은 의미없다.
	tEnv <- gEnv
	tEnv$zhF <- gEnv$zhF[1:(tIdx-1),]
	tEnv$allZoidMtx <- gEnv$zhF[tIdx,,drop=F]
	
	cfObjLst <- getCFLst.base( tEnv )
	for( lIdx in seq_len(length(cfObjLst)) ){
		cfObj <- cfObjLst[[lIdx]]
		if( is.null(encValLst[[cfObj$idStr]]) ){
			encValLst[[cfObj$idStr]] <- list()
		}
		encValLst[[cfObj$idStr]][[1+length(encValLst[[cfObj$idStr]])]] <- cfObj$enc( tEnv$allZoidMtx )[[1]]
	}
}

cfNames <- sapply(cfObjLst,function(p){p$idStr})
pairNum <-	sapply( cfNames ,function(p){
						# 각 encVal에서 중복쌍이 얼마나 많이 나오는지 측정
						mtx <- evalScan.pair( encValLst ,p ,cfObjLst )
						return( nrow(mtx) )
					})
names(pairNum) <- cfNames
encVal.len <- length(encValLst[[1]])

cfNameIdxMtx <- permutations( length(cfNames) ,2 )
rstValLst <- list()
for( cfIdx in 1:nrow(cfNameIdxMtx) ){

	scanName <- cfNames[cfNameIdxMtx[cfIdx,]]
	sbc.name <- scanName[1]
	nzc.name <- scanName[2]
	
	testSpan <- 200:encVal.len
	rstVal <- rep( NA ,length(testSpan) )
	for( tIdx in 1:length(testSpan) ){
		tIdx.r <- testSpan[tIdx]
		valLst <- list()
		valLst[[sbc.name]] <- encValLst[[sbc.name]][1:(tIdx.r-1)]
		valLst[[nzc.name]] <- encValLst[[nzc.name]][1:(tIdx.r-1)]
		rstObj <- evlScan( valLst ,sbc.name ,nzc.name ,cfObjLst )
		if( !is.null(rstObj$lastZC) ){
			stdVal <- encValLst[[nzc.name]][[tIdx.r]]
			cfObj <- cfObjLst[[ cfNameIdxMtx[cfIdx,2] ]]	# nzc.name
			rstVal[tIdx] <- cfObj$diffCnt( stdVal ,rstObj$lastZC )
		}
	}

	rstValLst[[1+length(rstValLst)]] <- rstVal

} # cfIdx

lapply( rstValLst ,table )




