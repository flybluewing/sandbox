source("hCommon.R")
source("miniS_H.R")

ex.startP <- 128

fltLst <- list()

flagId <- "std"
fltLst[[length(fltLst)+1]] <- mS.getFltDef001( pFlagId=flagId ) # 없느니만 못한...
# ----
fltLst[[length(fltLst)+1]] <- mS.getFltDefS01( pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 2 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 3 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 4 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 5 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 6 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 7 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 8 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 9 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02(10 ,pFlagId=flagId )
# ----
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 2 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 3 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 4 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 5 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 6 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 7 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 8 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 9 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03(10 ,pFlagId=flagId )
# ----
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 2 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 3 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 4 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 5 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 6 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 7 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 8 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 9 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04(10 ,pFlagId=flagId )
# ---- pUseSeqPCum=T
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 2 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 3 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 4 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 5 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 6 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 7 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 8 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 9 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02(10 ,pFlagId=flagId ,pUseSeqPCum=T )
# ----
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 2 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 3 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 4 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 5 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 6 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 7 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 8 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 9 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03(10 ,pFlagId=flagId ,pUseSeqPCum=T )
# ----
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 2 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 3 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 4 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 5 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 6 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 7 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 8 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 9 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04(10 ,pFlagId=flagId ,pUseSeqPCum=T )

flagId <- "diff"
fltLst[[length(fltLst)+1]] <- mS.getFltDef001( pFlagId=flagId ) # 없느니만 못한...
# ----
fltLst[[length(fltLst)+1]] <- mS.getFltDefS01( pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 2 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 3 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 4 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 5 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 6 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 7 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 8 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 9 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02(10 ,pFlagId=flagId )
# ----
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 2 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 3 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 4 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 5 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 6 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 7 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 8 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 9 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03(10 ,pFlagId=flagId )
# ----
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 2 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 3 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 4 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 5 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 6 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 7 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 8 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 9 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04(10 ,pFlagId=flagId )
# ---- pUseSeqPCum=T
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 2 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 3 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 4 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 5 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 6 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 7 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 8 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 9 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02(10 ,pFlagId=flagId ,pUseSeqPCum=T )
# ----
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 2 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 3 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 4 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 5 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 6 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 7 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 8 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 9 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03(10 ,pFlagId=flagId ,pUseSeqPCum=T )
# ----
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 2 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 3 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 4 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 5 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 6 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 7 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 8 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 9 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04(10 ,pFlagId=flagId ,pUseSeqPCum=T )

flagId <- "div"
fltLst[[length(fltLst)+1]] <- mS.getFltDef001( pFlagId=flagId ) # 없느니만 못한...
# ----
fltLst[[length(fltLst)+1]] <- mS.getFltDefS01( pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 2 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 3 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 4 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 5 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 6 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 7 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 8 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 9 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02(10 ,pFlagId=flagId )
# ----
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 2 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 3 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 4 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 5 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 6 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 7 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 8 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 9 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03(10 ,pFlagId=flagId )
# ----
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 2 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 3 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 4 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 5 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 6 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 7 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 8 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 9 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04(10 ,pFlagId=flagId )
# ---- pUseSeqPCum=T
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 2 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 3 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 4 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 5 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 6 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 7 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 8 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 9 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02(10 ,pFlagId=flagId ,pUseSeqPCum=T )
# ----
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 2 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 3 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 4 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 5 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 6 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 7 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 8 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 9 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03(10 ,pFlagId=flagId ,pUseSeqPCum=T )
# ----
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 2 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 3 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 4 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 5 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 6 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 7 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 8 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 9 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04(10 ,pFlagId=flagId ,pUseSeqPCum=T )

flagId <- "left"
fltLst[[length(fltLst)+1]] <- mS.getFltDef001( pFlagId=flagId ) # 없느니만 못한...
# ----
fltLst[[length(fltLst)+1]] <- mS.getFltDefS01( pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 2 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 3 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 4 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 5 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 6 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 7 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 8 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 9 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02(10 ,pFlagId=flagId )
# ----
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 2 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 3 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 4 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 5 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 6 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 7 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 8 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 9 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03(10 ,pFlagId=flagId )
# ----
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 2 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 3 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 4 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 5 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 6 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 7 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 8 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 9 ,pFlagId=flagId )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04(10 ,pFlagId=flagId )
# ---- pUseSeqPCum=T
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 2 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 3 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 4 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 5 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 6 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 7 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 8 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02( 9 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS02(10 ,pFlagId=flagId ,pUseSeqPCum=T )
# ----
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 2 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 3 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 4 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 5 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 6 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 7 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 8 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03( 9 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS03(10 ,pFlagId=flagId ,pUseSeqPCum=T )
# ----
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 2 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 3 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 4 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 5 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 6 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 7 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 8 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04( 9 ,pFlagId=flagId ,pUseSeqPCum=T )
fltLst[[length(fltLst)+1]] <- mS.getFltDefS04(10 ,pFlagId=flagId ,pUseSeqPCum=T )


hRange <- ex.startP:nrow(FB$zh)

cName <- c( "hist" ,"val" ,"score" ,"order" ,"isStd" )
mtx <- matrix( 0 ,nrow=0 ,ncol=length(cName) )
colnames(mtx) <- cName

# 각 필터별 결과
fltRstLst <- lapply(fltLst,function(p){return(mtx)})

for( hIdx in 1:length(hRange) ){

	k.FLogStr( sprintf("   %d of %d",hIdx,length(hRange)) )
	wZh <- as.matrix(FB$zh[1:(hRange[hIdx]-1),])
	pCd.h	<- wZh[,1]
	pCd.std	<- FB$zh[hRange[hIdx],1]
	
	pPbg <- mS.getPredrBaseGrp( pCd.h )
	# pPb <- mS.getPredrBase( pCd.h )
	#pCd.x <- c( pPbg[["std"]]$cdRange ,pCd.std ,0 ) 
	pCd.x <- c( 1:15 ,pCd.std )
		# 0 : NA로직 체크를 위해 추가
		# pCd.std : 기존에 없던 원소가 발생한 경우를 위해 추가.
	std.idx <- which(pCd.x==pCd.std)[1]

	for( fIdx in 1:length(fltLst) ){
		flt <- fltLst[[fIdx]]
		flt$predr <- mS.predr( flt ,pPbg )
		ana <- mS.analyze( flt ,pCd.x )
		scr <- mS.score( flt ,ana )

		cName <- c( "hist" ,"val" ,"score" ,"order" ,"isStd" )
		rstMtx <- matrix( 0 ,nrow=length(pCd.x) ,ncol=length(cName) )
		colnames(rstMtx) <- cName
		rstMtx[,"val"] <- pCd.x
		rstMtx[,"hist"] <- hRange[hIdx]
		rstMtx[,"score"] <- scr$cd.s
		rstMtx[std.idx,"isStd"] <- 1
		
		score.order <- order( scr$cd.s ,decreasing=T ,na.last=T )
		score.order <- score.order*100/length(score.order)
		rstMtx[,"order"] <- score.order
		
		fltRstLst[[fIdx]] <- rbind( fltRstLst[[fIdx]] ,rstMtx )
	} # fIdx
	
} # hIdx

names(fltLst) <- sapply( fltLst ,function(p){p$idStr} )
miniS <- list( fltLst=fltLst ,fltRstLst=fltRstLst )

save( miniS ,file="Obj_miniS0125.save" )

# -----------------------------------------------------------------
# 수정 필요.
plot( rstMtx[,"order"] )

pdf("./report/rmLst.pdf")
par(mfrow=c(2,3))

for( idx in 1:length(fltLst)){
	mtx <- rmLst[[idx]]
	plot( mtx[,"order"] ,main=fltLst[[idx]]$idStr )
}

dev.off()

idx <- 4
rMtx <- rmLst[[idx]]

k <- sapply( rmLst ,function(p){summary(p[,"order"])} )
# ----------------------------------------------------

assRstMtx <- function( pMtx ){
                        ord <- pMtx[,"order"]
                        rObj <- c( mean(ord) ,var(ord) ,median(ord) )
                        names(rObj) <- c("mean","var","median")
                        return(rObj)
                    }

cName <- c("flt","mean","var","median")
mtx <- matrix( 0 ,nrow=0 ,ncol=length(cName) )
for( idx in 1:length(rmLst) ){
    mtx <- rbind( mtx , c(fltLst[[idx]]$idStr,assRstMtx(rmLst[[idx]])) )    
}

# ----------------------------------------------------
assHit <- function( pMtx ){
		hSize <- nrow(pMtx)
		q <- quantile( pMtx[,"order"] )
		p30 <- ifelse( pMtx[,"order"]<=30 ,1 ,0 )
		p50 <- ifelse( pMtx[,"order"]>=50 ,1 ,0 )		
	}

fltId <- c(   "mSflt001fstd"   ,"mSfltS01fstd"    
                ,"mSfltS02fstd_3" ,"mSfltS03fstd_4" ,"mSfltS04fstd_4"
                ,"mSflt001fdiff"  ,"mSfltS01fdiff"
                ,"mSfltS02fdiff_3","mSfltS03fdiff_4","mSfltS04fdiff_4"
                ,"mSflt001fdiv"   ,"mSfltS01fdiv"
                ,"mSfltS02fdiv_3" ,"mSfltS03fdiv_4","mSfltS04fdiv_4"
                ,"mSflt001fleft"  ,"mSfltS01fleft"
                ,"mSfltS02fleft_3","mSfltS03fleft_4","mSfltS04fleft_4"
            )
fltId.all <- sapply( fltLst ,function(p){p$idStr} )
# fltIdx <- which( fltId.all %in%  fltId )



