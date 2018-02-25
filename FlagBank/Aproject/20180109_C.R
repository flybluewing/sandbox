# 20180109_C.R ±³Â÷¸ðµ¨
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

stdFiltedCnt <- sapply( fRstLst ,length )
stdFilted.tbl <- table(stdFiltedCnt)
stdFilted.per <- sprintf( "%.1f" ,100*stdFilted.tbl / length(fRstLst) )
names(stdFilted.per) <- names(stdFilted.tbl)


banObj <- getCFltObj( gEnv )

allZoid.fltCnt <- getAllZoidIdx.FltCnt( gEnv ,remLst )
allZoidMtx <- gEnv$allZoidMtx[(allZoid.fltCnt==0),]

tStmp <- Sys.time()
filtedIdxObj <- banObj$getFiltedIdx( allZoidMtx )
tDiff <- Sys.time() - tStmp
cat(sprintf("time cost %.1f%s \n",tDiff,units(tDiff)))

# ===============================================================================









