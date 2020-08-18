#   cutRst1Score <- bUtil.getCut1Score( scoreMtx.grp ,cut.grp ,fHName ,tgt.scMtx=tgt.scMtx )

bCMtxLst <- list()


crMName <- "crScrN01R"  # Cut-Result, Score N, Raw val only
if( TRUE ){

    bCMtxLst[[crMName]] <- function( hCRScr=NULL ){
        # hCRScr : cutRst1Score 히스토리. Rebound 체크 기능은 나중에 구현한다.

        rObj <- list( 	idStr=crMName  ,mName=c("score1","score3","score8")
		)

        rObj$fMtxObj <- function( scoreMtx.grp ,cut.grp ,fHName="sfcLate" ){
            
            tgt.scMtx <- rObj$mName
            cutRst1Score <- bUtil.getCut1Score( scoreMtx.grp ,cut.grp ,fHName ,tgt.scMtx=tgt.scMtx )

            aLen <- length(cutRst1Score$aLst)
            cName <- c(	 "hpn"  
                        ,"ph"   ,"fCol" ,"phReb"    # hpn Cnt, ph/fCol reb from last H, ph reb from left ph
                        ,"xyCnt.fCol" ,"xyCnt.phase"
                        ,"ph_Reb"   ,"fCol_Reb" ,"phReb_Reb"    ,"xyCnt.fCol_Reb"   ,"xyCnt.phase_Reb"    # from summMtx.Reb
                        ,"ph_sz" ,"fCol_sz" ,"dblHpnFlg_sz" ,"ph_szDup" ,"fCol_szDup" ,"dblHpnFlg_szDup"    # from scMtx.sz
            )
            crScrMtx <- matrix( 0, nrow=aLen, ncol=length(cName) )	;colnames(crScrMtx) <- cName

            hName <- "sfcLate"  # 일단 rebound 없이 체크하는 부분.
            for( aIdx in 1:aLen ){
                curCR <- cutRst1Score$aLst[[aIdx]][[hName]]$basic   # "phaseHpnCnt" "phaseRebCnt" "rebMtx.ph"
                                                                    # 수정됨. --> H1.phHpnCnt, H1.phRebCnt, rebMtx.ph

                hpnCnt <- sapply( curCR ,function(crObj){ sum(crObj$raw$rebMtx.ph["hpn.raw",]>0) })
                crScrMtx[aIdx,"hpn"] <- sum(hpnCnt==0)

                rebCnt <- sapply( curCR ,function(crObj){ sum(crObj$summ$summMtx["raw","ph"]>0) })
                crScrMtx[aIdx,"ph"] <- sum(rebCnt)
                rebCnt <- sapply( curCR ,function(crObj){ sum(crObj$summ$summMtx["raw","fCol"]>0) })
                crScrMtx[aIdx,"fCol"] <- sum(rebCnt)
                rebCnt <- sapply( curCR ,function(crObj){ sum(crObj$summ$summMtx["raw","phReb"]>0) })
                crScrMtx[aIdx,"phReb"] <- sum(rebCnt)
                rebCnt <- sapply( curCR ,function(crObj){ sum(crObj$summ$summMtx["raw","xyCnt.fCol"]>0) })
                crScrMtx[aIdx,"xyCnt.fCol"] <- sum(rebCnt)
                rebCnt <- sapply( curCR ,function(crObj){ sum(crObj$summ$summMtx["raw","xyCnt.phase"]>0) })
                crScrMtx[aIdx,"xyCnt.phase"] <- sum(rebCnt)

                rebCnt <- sapply( curCR ,function(crObj){ sum(crObj$summ$summMtx.reb["raw","ph"]>0) })
                crScrMtx[aIdx,"ph_Reb"] <- sum(rebCnt)
                rebCnt <- sapply( curCR ,function(crObj){ sum(crObj$summ$summMtx.reb["raw","fCol"]>0) })
                crScrMtx[aIdx,"fCol_Reb"] <- sum(rebCnt)
                rebCnt <- sapply( curCR ,function(crObj){ sum(crObj$summ$summMtx.reb["raw","phReb"]>0) })
                crScrMtx[aIdx,"phReb_Reb"] <- sum(rebCnt)
                rebCnt <- sapply( curCR ,function(crObj){ sum(crObj$summ$summMtx.reb["raw","xyCnt.fCol"]>0) })
                crScrMtx[aIdx,"xyCnt.fCol_Reb"] <- sum(rebCnt)
                rebCnt <- sapply( curCR ,function(crObj){ sum(crObj$summ$summMtx.reb["raw","xyCnt.phase"]>0) })
                crScrMtx[aIdx,"xyCnt.phase_Reb"] <- sum(rebCnt)

                rebCnt <- sapply( curCR ,function(crObj){ sum(crObj$summ$scMtx.sz["rebCnt","r.ph"]>0) })
                crScrMtx[aIdx,"ph_sz"] <- sum(rebCnt)
                rebCnt <- sapply( curCR ,function(crObj){ sum(crObj$summ$scMtx.sz["rebCnt","r.fCol"]>0) })
                crScrMtx[aIdx,"fCol_sz"] <- sum(rebCnt)
                rebCnt <- sapply( curCR ,function(crObj){ sum(crObj$summ$scMtx.sz["rebCnt","r.dblHpnFlg"]>0) })
                crScrMtx[aIdx,"dblHpnFlg_sz"] <- sum(rebCnt)
                rebCnt <- sapply( curCR ,function(crObj){ sum(crObj$summ$scMtx.sz["rebDup","r.ph"]>0) })
                crScrMtx[aIdx,"ph_szDup"] <- sum(rebCnt)
                rebCnt <- sapply( curCR ,function(crObj){ sum(crObj$summ$scMtx.sz["rebDup","r.fCol"]>0) })
                crScrMtx[aIdx,"fCol_szDup"] <- sum(rebCnt)
                rebCnt <- sapply( curCR ,function(crObj){ sum(crObj$summ$scMtx.sz["rebDup","r.dblHpnFlg"]>0) })
                crScrMtx[aIdx,"dblHpnFlg_szDup"] <- sum(rebCnt)

                # hpnCnt <- sapply( curCR ,function(crObj){ sum(crObj$raw$rebMtx.ph["hpn.evt",]>0) })
                # rebCnt <- sapply( curCR ,function(crObj){ sum(crObj$summ$summMtx["evt","ph"]) })
                # rebCnt <- sapply( curCR ,function(crObj){ sum(crObj$summ$summMtx["evt","fCol"]) })
                # rebCnt <- sapply( curCR ,function(crObj){ sum(crObj$summ$summMtx["evt","phReb"]>0) })
                # rebCnt <- sapply( curCR ,function(crObj){ sum(crObj$summ$scMtx.sz["rebCnt","e.ph"]>0) })
                # rebCnt <- sapply( curCR ,function(crObj){ sum(crObj$summ$scMtx.sz["rebCnt","e.fCol"]>0) })
                # rebCnt <- sapply( curCR ,function(crObj){ sum(crObj$summ$scMtx.sz["rebCnt","e.dblHpnFlg"]>0) })

            }

            return( crScrMtx )
        }

        return( rObj )
    }

}

crMName <- "crScrN01E"  # Cut-Result, Score N, Raw val only
if( TRUE ){

    bCMtxLst[[crMName]] <- function( hCRScr=NULL ){
        # hCRScr : cutRst1Score 히스토리. Rebound 체크 기능은 나중에 구현한다.

        rObj <- list( 	idStr=crMName  ,mName=c("score1","score3","score8")
		)

        rObj$fMtxObj <- function( scoreMtx.grp ,cut.grp ,fHName="sfcLate" ){
            
            tgt.scMtx <- rObj$mName
            cutRst1Score <- bUtil.getCut1Score( scoreMtx.grp ,cut.grp ,fHName ,tgt.scMtx=tgt.scMtx )

            aLen <- length(cutRst1Score$aLst)
            cName <- c(	 "hpn"  
                        ,"ph"   ,"fCol" ,"phReb"    # hpn Cnt, ph/fCol reb from last H, ph reb from left ph
                        ,"xyCnt.fCol" ,"xyCnt.phase"
                        ,"ph_Reb"   ,"fCol_Reb" ,"phReb_Reb"    ,"xyCnt.fCol_Reb"   ,"xyCnt.phase_Reb"    # from summMtx.Reb
                        ,"ph_sz" ,"fCol_sz" ,"dblHpnFlg_sz" ,"ph_szDup" ,"fCol_szDup" ,"dblHpnFlg_szDup"    # from scMtx.sz
            )
            crScrMtx <- matrix( 0, nrow=aLen, ncol=length(cName) )	;colnames(crScrMtx) <- cName

            hName <- "sfcLate"  # 일단 rebound 없이 체크하는 부분.
            for( aIdx in 1:aLen ){
                curCR <- cutRst1Score$aLst[[aIdx]][[hName]]$basic   # "phaseHpnCnt" "phaseRebCnt" "rebMtx.ph"
                                                                    # 수정됨. --> H1.phHpnCnt, H1.phRebCnt, rebMtx.ph

                hpnCnt <- sapply( curCR ,function(crObj){ sum(crObj$raw$rebMtx.ph["hpn.evt",]>0) })
                crScrMtx[aIdx,"hpn"] <- sum(hpnCnt==0)

                rebCnt <- sapply( curCR ,function(crObj){ sum(crObj$summ$summMtx["evt","ph"]>0) })
                crScrMtx[aIdx,"ph"] <- sum(rebCnt)
                rebCnt <- sapply( curCR ,function(crObj){ sum(crObj$summ$summMtx["evt","fCol"]>0) })
                crScrMtx[aIdx,"fCol"] <- sum(rebCnt)
                rebCnt <- sapply( curCR ,function(crObj){ sum(crObj$summ$summMtx["evt","phReb"]>0) })
                crScrMtx[aIdx,"phReb"] <- sum(rebCnt)
                rebCnt <- sapply( curCR ,function(crObj){ sum(crObj$summ$summMtx["evt","xyCnt.fCol"]>0) })
                crScrMtx[aIdx,"xyCnt.fCol"] <- sum(rebCnt)
                rebCnt <- sapply( curCR ,function(crObj){ sum(crObj$summ$summMtx["evt","xyCnt.phase"]>0) })
                crScrMtx[aIdx,"xyCnt.phase"] <- sum(rebCnt)

                rebCnt <- sapply( curCR ,function(crObj){ sum(crObj$summ$summMtx.reb["evt","ph"]>0) })
                crScrMtx[aIdx,"ph_Reb"] <- sum(rebCnt)
                rebCnt <- sapply( curCR ,function(crObj){ sum(crObj$summ$summMtx.reb["evt","fCol"]>0) })
                crScrMtx[aIdx,"fCol_Reb"] <- sum(rebCnt)
                rebCnt <- sapply( curCR ,function(crObj){ sum(crObj$summ$summMtx.reb["evt","phReb"]>0) })
                crScrMtx[aIdx,"phReb_Reb"] <- sum(rebCnt)
                rebCnt <- sapply( curCR ,function(crObj){ sum(crObj$summ$summMtx.reb["evt","xyCnt.fCol"]>0) })
                crScrMtx[aIdx,"xyCnt.fCol_Reb"] <- sum(rebCnt)
                rebCnt <- sapply( curCR ,function(crObj){ sum(crObj$summ$summMtx.reb["evt","xyCnt.phase"]>0) })
                crScrMtx[aIdx,"xyCnt.phase_Reb"] <- sum(rebCnt)

                rebCnt <- sapply( curCR ,function(crObj){ sum(crObj$summ$scMtx.sz["rebCnt","e.ph"]>0) })
                crScrMtx[aIdx,"ph_sz"] <- sum(rebCnt)
                rebCnt <- sapply( curCR ,function(crObj){ sum(crObj$summ$scMtx.sz["rebCnt","e.fCol"]>0) })
                crScrMtx[aIdx,"fCol_sz"] <- sum(rebCnt)
                rebCnt <- sapply( curCR ,function(crObj){ sum(crObj$summ$scMtx.sz["rebCnt","e.dblHpnFlg"]>0) })
                crScrMtx[aIdx,"dblHpnFlg_sz"] <- sum(rebCnt)
                rebCnt <- sapply( curCR ,function(crObj){ sum(crObj$summ$scMtx.sz["rebDup","e.ph"]>0) })
                crScrMtx[aIdx,"ph_szDup"] <- sum(rebCnt)
                rebCnt <- sapply( curCR ,function(crObj){ sum(crObj$summ$scMtx.sz["rebDup","e.fCol"]>0) })
                crScrMtx[aIdx,"fCol_szDup"] <- sum(rebCnt)
                rebCnt <- sapply( curCR ,function(crObj){ sum(crObj$summ$scMtx.sz["rebDup","e.dblHpnFlg"]>0) })
                crScrMtx[aIdx,"dblHpnFlg_szDup"] <- sum(rebCnt)

                # hpnCnt <- sapply( curCR ,function(crObj){ sum(crObj$raw$rebMtx.ph["hpn.evt",]>0) })
                # rebCnt <- sapply( curCR ,function(crObj){ sum(crObj$summ$summMtx["evt","ph"]) })
                # rebCnt <- sapply( curCR ,function(crObj){ sum(crObj$summ$summMtx["evt","fCol"]) })
                # rebCnt <- sapply( curCR ,function(crObj){ sum(crObj$summ$summMtx["evt","phReb"]>0) })
                # rebCnt <- sapply( curCR ,function(crObj){ sum(crObj$summ$scMtx.sz["rebCnt","e.ph"]>0) })
                # rebCnt <- sapply( curCR ,function(crObj){ sum(crObj$summ$scMtx.sz["rebCnt","e.fCol"]>0) })
                # rebCnt <- sapply( curCR ,function(crObj){ sum(crObj$summ$scMtx.sz["rebCnt","e.dblHpnFlg"]>0) })

            }

            return( crScrMtx )
        }

        return( rObj )
    }

}

