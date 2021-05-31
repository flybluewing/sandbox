setwd("c:/zproject/ISLRgit/FlagBank")   ;source("hCommon.R")    ;setwd("./Bproject")
source("header.r")  ;source("B_H.R") ;source("B_prll_H.R")
source("testField/may/projMay_Run_H.r")

lastHSpan <- c(800,820,840,860,880,900,920,940,960)
cutRstGrp <- may.loadSaves( lastHSpan )

tLst <- may.getTypLst( cutRstGrp )



cntLst <- list( )
if( TRUE ){
    cntLst[["sN"]] <- c("sN","sNx")
    cntLst[["sSN"]] <- c("sSN","sSNx")
}

cutCntMtx <- NULL
for( nIdx in names(cntLst) ){
    cutCnt <- sapply( tLst$typLst ,function(p){
        sum(p %in% cntLst[[nIdx]] )
    })

    cutCntMtx <- rbind( cutCntMtx ,cutCnt )
}

cutCntMtx <- rbind( cutCntMtx ,apply( cutCntMtx ,2 ,sum ) )
rownames(cutCntMtx) <- c( names(cntLst) ,"tot" )


if( FALSE ){    # function usage
    cr <- may.searchCutRst( "H844_2" ,cutRstGrp )

    may.plot( cutCntMtx )
}


