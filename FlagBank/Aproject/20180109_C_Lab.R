# 20180109_C.R ±³Â÷¸ðµ¨
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

getCFLst.base <- function( pEnv ){
	cfObjLst <- list()
	cfObjLst[[1+length(cfObjLst)]] <- cf_A0010( pEnv )
	cfObjLst[[1+length(cfObjLst)]] <- cf_A0020( pEnv )
	return( cfObjLst )
} # getCFLst.base()


testSpan <- 400:nrow(zhF)
cfObjLst <- NULL
encValLst <- list()
for( tIdx in testSpan ){
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


rObj <- evlScan( encValLst ,pSBC="A0010_o3" ,pNZC="A0020_o3" ,pCFLst=cfObjLst )
