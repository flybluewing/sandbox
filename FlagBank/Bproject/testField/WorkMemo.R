
cnt <- apply( gEnv$zhF ,1 ,function(zoid){
	cStep <- zoid[2:6] - zoid[1:5]
	max(table(cStep))
})


lCStep <- c( 1, 2, 3, 1, 2 )

aZoidMtx <- rbind( aZoid ,aZoid-1 )
aZoidMtx <- rbind( aZoidMtx ,aZoid+1 )
aZoidMtx <- rbind( aZoidMtx ,aZoid+2 )


curHIdx <- 880	;hName <- "sfcLate"	;mName <- "score1"
tgtId <- c(hName=hName, mName=mName)

hIdxObj <- B.getHMtxLst_byHIdx( curHMtxLst )
hLen <- length(hIdxObj[[hName]][[mName]])
hIdxObj[[hName]][[mName]][[hLen]]

stdMI.grp <- bUtil.getStdMILst( gEnv.w ,fRstLst.w )
filter.grp <- getFilter.grp( stdMI.grp ,tgt.scMtx=tgt.scMtx )
scoreMtx.grp <- getScoreMtx.grp.4H( stdZoid ,filter.grp )
mtxGrp <- getScoreMtx.grp_byHIdx( scoreMtx.grp )
scoreMtx <- mtxGrp[[mName]][[1]]




