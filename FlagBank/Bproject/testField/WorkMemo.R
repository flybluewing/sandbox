# 작업 전달용 임시메모

curHIdx <- testSpan[1]

hMtxLst.bak <- hMtxLst

hMtxLst <- curHMtxLst
hName <- "sfcLate" ;mName <- "score2"    ;fcName <- "rebV.r" ;auxInfo=c(auxInfo="")
tgtId <- c(hName=hName,mName=mName,fcName=fcName)
fColObj <- B.getHMtxLst_byFCol( hMtxLst )
lastMtx <- fColObj[[hName]][[mName]][[fcName]]	# h * phase

mtxGrp <- getScoreMtx.grp_byFCol( scoreMtx.grp )

scoreMtx <- mtxGrp[[mName]][[fcName]]
smRow <- scoreMtx[1,]
alreadyDead=NULL
idx <- 1






