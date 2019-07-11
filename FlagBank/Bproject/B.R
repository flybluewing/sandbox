source("B_H.R")
lastH <- 860
#source(sprintf("./toFinal/toZ%d_H.R",workH))	# working

load(sprintf("./save/Obj_allIdxLstZ%d.save",lateH) )
load(sprintf("./save/Obj_fRstLstZ%d.save",lateH) )
load(sprintf("./save/Obj_gEnvZ%d.save",lateH))

names(fRstLst) <- names(allIdxLst$stdFiltedCnt)


baseSpan <- 800:lastH
stdFiltedCnt <- allIdxLst$stdFiltedCnt[as.character(baseSpan)]
fRstLst <- fRstLst[as.character(baseSpan)]

sfcHLst <- list(    sfcLate= baseSpan[length(baseSpan)] - 10:0
                    ,sfc0=as.integer(names(stdFiltedCnt)[stdFiltedCnt==0])
                    ,sfc1=as.integer(names(stdFiltedCnt)[stdFiltedCnt==1])
                    ,sfc2=as.integer(names(stdFiltedCnt)[stdFiltedCnt==2])
                )

for( sfnIdx in c("D0000.A","A0100.A","AP000.E") ){
    #   sfnIdx <- "D0000.A"
    hSpan <- baseSpan[sapply( fRstLst ,function(p){ sfnIdx %in% p } )]
    hSpan.NG <- hSpan+1
    hSpan.NG <- hSpan.NG[hSpan.NG<=lastH]
    sfcHLst[[sprintf("NG:%s",sfnIdx)]] <- hSpan.NG
}


for( sfcIdx in names(sfcHLst) ){
    hSpan <- sfcHLst[[sfcIdx]]
    #   qqe working
}

stdMI.grp <- bUtil.getStdMILst( gEnv ,fRstLst )
filter.grp <- getFilter.grp( stdMI.grp )

if(FALSE){

    stdZoid <- c( 8,22,35,38,39,41)
    scoreMtx.grp <- getScoreMtx.grp.4H( stdZoid ,filter.grp )
    
}
