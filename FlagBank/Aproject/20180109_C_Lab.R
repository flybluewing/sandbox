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

remLst

fltCnt <- rep( 0 ,nrow(gEnv$allZoidMtx) )
for( remIdx in 1:length(remLst) ){
	fltCnt[remLst[[remIdx]]] <- fltCnt[remLst[[remIdx]]] + 1
}


chosenZ.idx <- which( fltCnt==0 )
# chosenZ.idx <- sort(sample(chosenZ.idx,100))
chosenZMtx <- gEnv$allZoidMtx[chosenZ.idx,]

rstLst <- list()

tStmp <- Sys.time()
# ==================================================================
banObj <- getCFltObj( gEnv )
codeLst <- banObj$getCodeLst( chosenZMtx )
tDiff <- Sys.time() - tStmp
cat(sprintf("banObj %.1f%s\n",tDiff,units(tDiff)))

bRstObj <- ban.hntSameRow(banObj ,chosenZMtx ,pCodeLst=codeLst)
rstIdStr <- sprintf("%s_base",bRstObj$idStr)
rstLst[[rstIdStr]] <- bRstObj$filtedIdx
tDiff <- Sys.time() - tStmp
cat(sprintf("%s %.1f%s\n",rstIdStr,tDiff,units(tDiff)))

bRstObj <- ban.hntCrossDim(banObj ,chosenZMtx ,pCodeLst=codeLst)
rstIdStr <- sprintf("%s_base",bRstObj$idStr)
rstLst[[rstIdStr]] <- bRstObj$filtedIdx
tDiff <- Sys.time() - tStmp
cat(sprintf("%s %.1f%s\n",rstIdStr,tDiff,units(tDiff)))

bRstObj <- ban.multiDim(banObj ,chosenZMtx ,pCodeLst=codeLst)
rstIdStr <- sprintf("%s_base",bRstObj$idStr)
rstLst[[rstIdStr]] <- bRstObj$filtedIdx
tDiff <- Sys.time() - tStmp
cat(sprintf("%s %.1f%s\n",rstIdStr,tDiff,units(tDiff)))

bRstObj <- ban.throughH(banObj ,chosenZMtx ,pCodeLst=codeLst)
rstIdStr <- sprintf("%s_base",bRstObj$idStr)
rstLst[[rstIdStr]] <- bRstObj$filtedIdx
tDiff <- Sys.time() - tStmp
cat(sprintf("%s %.1f%s\n",rstIdStr,tDiff,units(tDiff)))

bRstObj <- ban.throughH2(banObj ,chosenZMtx ,pCodeLst=codeLst)
rstIdStr <- sprintf("%s_base",bRstObj$idStr)
rstLst[[rstIdStr]] <- bRstObj$filtedIdx
tDiff <- Sys.time() - tStmp
cat(sprintf("%s %.1f%s\n",rstIdStr,tDiff,units(tDiff)))

# ==================================================================
banCmbObj <- getCFltCmbObj( gEnv )
codeCmbLst <- banCmbObj$getCodeLst( chosenZMtx )
tDiff <- Sys.time() - tStmp
cat(sprintf("banCmbObj %.1f%s\n",tDiff,units(tDiff)))

bRstObj <- ban.hntSameRow(banCmbObj ,chosenZMtx ,pCodeLst=codeCmbLst)
rstIdStr <- sprintf("%s_comb",bRstObj$idStr)
rstLst[[rstIdStr]] <- bRstObj$filtedIdx
tDiff <- Sys.time() - tStmp
cat(sprintf("%s %.1f%s\n",rstIdStr,tDiff,units(tDiff)))

bRstObj <- ban.hntCrossDim(banCmbObj ,chosenZMtx ,pCodeLst=codeCmbLst)
rstIdStr <- sprintf("%s_comb",bRstObj$idStr)
rstLst[[rstIdStr]] <- bRstObj$filtedIdx
tDiff <- Sys.time() - tStmp
cat(sprintf("%s %.1f%s\n",rstIdStr,tDiff,units(tDiff)))

bRstObj <- ban.multiDim(banCmbObj ,chosenZMtx ,pCodeLst=codeCmbLst)
rstIdStr <- sprintf("%s_comb",bRstObj$idStr)
rstLst[[rstIdStr]] <- bRstObj$filtedIdx
tDiff <- Sys.time() - tStmp
cat(sprintf("%s %.1f%s\n",rstIdStr,tDiff,units(tDiff)))

bRstObj <- ban.throughH(banCmbObj ,chosenZMtx ,pCodeLst=codeCmbLst)
rstIdStr <- sprintf("%s_comb",bRstObj$idStr)
rstLst[[rstIdStr]] <- bRstObj$filtedIdx
tDiff <- Sys.time() - tStmp
cat(sprintf("%s %.1f%s\n",rstIdStr,tDiff,units(tDiff)))

bRstObj <- ban.throughH2(banCmbObj ,chosenZMtx ,pCodeLst=codeCmbLst)
rstIdStr <- sprintf("%s_comb",bRstObj$idStr)
rstLst[[rstIdStr]] <- bRstObj$filtedIdx
tDiff <- Sys.time() - tStmp
cat(sprintf("%s %.1f%s\n",rstIdStr,tDiff,units(tDiff)))


