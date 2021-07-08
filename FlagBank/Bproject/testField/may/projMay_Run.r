setwd("c:/zproject/ISLRgit/FlagBank")   ;source("hCommon.R")    ;setwd("./Bproject")
source("header.r")  ;source("B_H.R") ;source("B_prll_H.R")  ;source("B_prll_Hrpt.R")
source("testField/may/projMay_Run_H.r") ;source("testField/may/curWorking_H.r")

lastHSpan <- c(800,820,840,860,880)
cutRstGrp <- may.loadSaves( lastHSpan )
cutRstGrp <- may.cutInfoLst_rmDup(cutRstGrp)
tLstA <- may.getTypLst( cutRstGrp )

lastHSpan <- c(900,920,940,960)     # 920,940,960
cutRstGrp <- may.loadSaves( lastHSpan )
cutRstGrp <- may.cutInfoLst_rmDup(cutRstGrp)
tLstB <- may.getTypLst( cutRstGrp )

cntDfA <- mayCw.getCntDf( tLstA )   ;cntDfA$ab <- "A"
lowHIdx <- may.getLowH( cntDfA ,10 )
cntDfA <- transform( cntDfA ,lowHpn= hIdx%in%lowHIdx )

cntDfB <- mayCw.getCntDf( tLstB )   ;cntDfB$ab <- "B"
lowHIdx <- may.getLowH( cntDfB ,15 )
cntDfB <- transform( cntDfB ,lowHpn= hIdx%in%lowHIdx )


# pastHpn
cntDfA$pastHpn <- 0 ;cntDfB$pastHpn <- 0
matCol <- c("mName","M","I")
pastHpnCnt <- apply( cntDfB[,matCol] ,1 ,function( pDfB ){
    mFlag <- apply( cntDfA[,matCol] ,1 ,function(pDfA){ all(pDfA==pDfB) } )
    return( sum(mFlag) )
})
cntDfB$pastHpn <- pastHpnCnt

cntDf <- rbind( cntDfA ,cntDfB )




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

    may.searchTLst( tLst=tLstA ,pGrpId=c("H900","H920") ,pI=c("other","rReb") )

    srchDf <- may.searchTLst_df(tLstB,cntDfB)
    cutRstGrp <- may.loadSaves( lastHSpan )
    cutRstGrp <- may.cutInfoLst_rmDup(cutRstGrp)

    crDf <- may.cutRstGrp_2Df( cutRstGrp )

    cr <- may.searchCutRst( "H844_2" ,cutRstGrp )

    cutCntMtx <- rbind( cutCntMtx ,apply( cutCntMtx ,2 ,sum ) )
    rownames(cutCntMtx) <- c( names(cntLst) ,"tot" )
    may.plot( cutCntMtx )
}


