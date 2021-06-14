setwd("c:/zproject/ISLRgit/FlagBank")   ;source("hCommon.R")    ;setwd("./Bproject")
source("header.r")  ;source("B_H.R") ;source("B_prll_H.R")
source("testField/may/projMay_Run_H.r") ;source("testField/may/curWorking_H.r")

lastHSpan <- c(800,820,840,860)
cutRstGrp <- may.loadSaves( lastHSpan )
cutRstGrp <- may.cutInfoLst_rmDup(cutRstGrp)
tLstA <- may.getTypLst( cutRstGrp )

lastHSpan <- c(880,900,920,940)     # ,960
cutRstGrp <- may.loadSaves( lastHSpan )
cutRstGrp <- may.cutInfoLst_rmDup(cutRstGrp)
tLstB <- may.getTypLst( cutRstGrp )



if( FALSE ){     
    cntLst <- list( )
    cntLst[["sN"]] <- c("sN","sNx")
    cntLst[["sSN"]] <- c("sSN","sSNx")

    # cntLst[["rReb"]] <- c("rReb")
    # cntLst[["hIMtxHpnCnt"]] <- c("hIMtxHpnCnt")
    # cntLst[["rowRebDupBan"]] <- c("rowRebDupBan")

    # "sNx" "sN" "sA" "sAx" "sCrScr" "other"   "sMlt"    "sSCrScr" "HCR"     "sSN"     "sSNx"    "sSA"     "sSAx"    "bScr"    "sSMlt"  

    # "rReb" "fCol EvtCnt4AllPh" "colRng(n)" "evtCnt" "fCol evtMaxFColTot" "forbidden Evt Reb" "summMtx" "scMtx.sz" "bUtil.chkStdMIPair()" "evt Reb"             
    # "summMtx.reb" "hIMtxHpnCnt"  "other" "rowRebDupBan"

    cutCntMtx <- NULL
    for( nIdx in names(cntLst) ){
        cutCnt <- sapply( tLst$typLst ,function(p){  sum(p["M",] %in% cntLst[[nIdx]] )    })
        cutCntMtx <- rbind( cutCntMtx ,cutCnt )
    }
    rownames(cutCntMtx)<-names(cntLst)      ;may.plot( cutCntMtx )


    cutCntMtx <- NULL
    for( nIdx in names(cntLst) ){
        cutCnt <- sapply( tLst$typLst ,function(p){  sum(p["I",] %in% cntLst[[nIdx]] )    })
        cutCntMtx <- rbind( cutCntMtx ,cutCnt )
    }
    rownames(cutCntMtx)<-names(cntLst)      ;may.plot( cutCntMtx )

}

if( FALSE ){    # function usage
    cr <- may.searchCutRst( "H844_2" ,cutRstGrp )

    cutCntMtx <- rbind( cutCntMtx ,apply( cutCntMtx ,2 ,sum ) )
    rownames(cutCntMtx) <- c( names(cntLst) ,"tot" )
    may.plot( cutCntMtx )
}


