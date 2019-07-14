source("B_H.R")
lastH <- 859
#source(sprintf("./toFinal/toZ%d_H.R",workH))	# working

load(sprintf("./save/Obj_allIdxLstZ%d.save",lastH) )
load(sprintf("./save/Obj_fRstLstZ%d.save",lastH) )
load(sprintf("./save/Obj_gEnvZ%d.save",lastH))

names(fRstLst) <- names(allIdxLst$stdFiltedCnt)
fRstLst.hSpan <- as.integer(names(fRstLst))


baseSpan <- 800:lastH
stdFiltedCnt <- allIdxLst$stdFiltedCnt[as.character(baseSpan)]

sfcHLst <- list(    sfcLate= baseSpan[length(baseSpan)] - 10:0
                    ,sfc0=as.integer(names(stdFiltedCnt)[stdFiltedCnt==0])
                    ,sfc1=as.integer(names(stdFiltedCnt)[stdFiltedCnt==1])
                    ,sfc2=as.integer(names(stdFiltedCnt)[stdFiltedCnt==2])
                )

for( sfnIdx in c("D0000.A","A0100.A","AP000.E") ){
    #   sfnIdx <- "D0000.A"
    hSpan <- baseSpan[sapply( fRstLst[as.character(baseSpan)] ,function(p){ sfnIdx %in% p } )]
    hSpan.NG <- hSpan+1
    hSpan.NG <- hSpan.NG[hSpan.NG<=lastH]
    sfcHLst[[sprintf("NG:%s",sfnIdx)]] <- hSpan.NG
}


scoreMtxLst <- list()
for( sfcIdx in names(sfcHLst) ){    # sfcIdx <- names(sfcHLst)[2]

    scoreMtx.grp.lst <- list()
    for( hIdx in sfcHLst[[sfcIdx]] ){   # hIdx <- hSpan[1]
        stdZoid <- gEnv$zhF[hIdx ,]
        wEnv <- gEnv
        wEnv$zhF <- gEnv$zhF[1:(hIdx-1),]

        fRstLst.w <- fRstLst[as.character(fRstLst.hSpan[fRstLst.hSpan<hIdx])]

        stdMI.grp <- bUtil.getStdMILst( wEnv ,fRstLst.w )
        filter.grp <- getFilter.grp( stdMI.grp )

        # QQE Todo
        # Error in if (2 > stdMI$mtxLen) NULL else stdMI$rawTail[nrow(stdMI$rawTail) -  : 
        #   인자의 길이가 0

        scoreMtx.grp <- getScoreMtx.grp.4H( stdZoid ,filter.grp )
        scoreMtx.grp.lst[[sprintf("hIdx:%d",hIdx)]] <- scoreMtx.grp
    }

    scoreMtxLst[[sfcIdx]] <- scoreMtx.grp.lst
}


if(FALSE){

    stdZoid <- c( 8,22,35,38,39,41)
    scoreMtx.grp <- getScoreMtx.grp.4H( stdZoid ,filter.grp )
    
}
