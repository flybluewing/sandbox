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

crMName <- "crScrN01PhEvt"  # phase 별로 evt 갯수 제한.(phase에서 다수 mName을 대상으로..)
if( TRUE ){
    bCMtxLst[[crMName]] <- function( hCRScr=NULL ){
        # hCRScr : cutRst1Score 히스토리. Rebound 체크 기능은 나중에 구현한다.
        rObj <- list( 	idStr=crMName  ,mName=c("score1","score3","score8")
		)

        rObj$fMtxObj <- function( scoreMtx.grp ,cut.grp ,fHName="sfcLate" ){
            if( FALSE ){    # comment
                # sample code for debug
                #       mIdx <- "score1"
                #       rawMtx <- sapply( scoreMtx.grp$basic ,function(p){ p[[mIdx]]$scoreMtx[1,] })
                #       cfg <- scoreMtxCfg[[ mIdx ]]
                #       evtObj <- bFCust.getEvtMtx( rawMtx ,cfg )
                #       
            }

            tgt.scMtx <- rObj$mName
            cutRst1Score <- bUtil.getCut1Score( scoreMtx.grp ,cut.grp ,fHName ,tgt.scMtx=tgt.scMtx )

            aLen <- length(cutRst1Score$aLst)
            cName <- c(  "e3Max" ,"e3MCnt" ,"e2Max" ,"e2MCnt" ,"e1Max" ,"e1MCnt"   # Cnt는 evt가 2번 이상 발생한 pName 수
                        ,"rebRawMax" ,"rebRawMCnt" ,"rebEvtMax" ,"rebEvtMCnt"
            )
            crScrMtx <- matrix( 0, nrow=aLen, ncol=length(cName) )	;colnames(crScrMtx) <- cName

            phNameAll <- names(scoreMtx.grp$basic)
            #   중간 계산 및 디버깅용 데이터 매트릭스
            tempCName <- c("e1","e2","e3" ,"rebRCnt" ,"rebECnt" )
            tempMtx <- matrix( 0 ,nrow=length(tempCName) ,ncol=length(phNameAll) ,dimnames=list(tempCName,phNameAll) )

            hName <- "sfcLate"  # 일단 rebound 없이 체크하는 부분.
            for( aIdx in 1:aLen ){
                curCR <- cutRst1Score$aLst[[aIdx]][[hName]]$basic   # "rebMtx.ph"    "evtHpnLevMtx" "phaseReb"

                tempMtx[,] <- 0
                for( mIdx in rObj$mName ){
                    evtHpnLevMtx <- curCR[[mIdx]]$raw$evtHpnLevMtx
                    
                    # evt lev1은 너무 흔해서, 하나의 phase 내에서 lev1 2개 이하는 자르자.
                    lev1 <- evtHpnLevMtx["lev1",]
                    evtHpnLevMtx["lev1",] <- ifelse(lev1>2,lev1-2,0)

                    # evt lev2도 1개 이하는 자르자.                    
                    lev2 <- evtHpnLevMtx["lev2",]
                    evtHpnLevMtx["lev2",] <- ifelse(lev2>1,lev2-1,0)

                    for( pIdx in phNameAll ){  # nIdx == pName
                        tempMtx["e1",pIdx] <- tempMtx["e1",pIdx] + evtHpnLevMtx["lev1",pIdx]
                        tempMtx["e2",pIdx] <- tempMtx["e2",pIdx] + evtHpnLevMtx["lev2",pIdx]
                        tempMtx["e3",pIdx] <- tempMtx["e3",pIdx] + evtHpnLevMtx["lev3",pIdx]

                        rebMtx.ph <- curCR[[mIdx]]$raw$rebMtx.ph
                        tempMtx["rebRCnt",pIdx] <- tempMtx["rebRCnt",pIdx] + rebMtx.ph["rebFlag.raw",pIdx]
                        tempMtx["rebECnt",pIdx] <- tempMtx["rebECnt",pIdx] + rebMtx.ph["rebFlag.evt",pIdx]
                    }
                }

                crScrMtx[aIdx,"e1Max"]  <- max(tempMtx["e1",])
                crScrMtx[aIdx,"e1MCnt"] <- sum(tempMtx["e1",]>1)
                crScrMtx[aIdx,"e2Max"]  <- max(tempMtx["e2",])
                crScrMtx[aIdx,"e2MCnt"] <- sum(tempMtx["e2",]>1)
                crScrMtx[aIdx,"e3Max"]  <- max(tempMtx["e3",])
                crScrMtx[aIdx,"e3MCnt"] <- sum(tempMtx["e3",]>1)

                crScrMtx[aIdx,"rebRawMax"]  <- max(tempMtx["rebRCnt",])
                crScrMtx[aIdx,"rebRawMCnt"] <- sum(tempMtx["rebRCnt",]>1)
                crScrMtx[aIdx,"rebEvtMax"]  <- max(tempMtx["rebECnt",])
                crScrMtx[aIdx,"rebEvtMCnt"] <- sum(tempMtx["rebECnt",]>1)

                # MCnt가 1이면 Max 컬럼값과 중복의미가 되므로 0으로 없앤다.
                cName <- c("e1MCnt","e2MCnt","e3MCnt","rebRawMCnt","rebEvtMCnt")
                crScrMtx[aIdx,cName] <- ifelse( 2>crScrMtx[aIdx,cName] ,0 ,crScrMtx[aIdx,cName] )

            }

            return( crScrMtx )
        }
        return( rObj )
    }

}



crMName <- "crScrN02R"  # Cut-Result, Score N, Raw val only
if( TRUE ){

    bCMtxLst[[crMName]] <- function( hCRScr=NULL ){
        # hCRScr : cutRst1Score 히스토리. Rebound 체크 기능은 나중에 구현한다.

        rObj <- list( 	idStr=crMName  ,mName=c("score1","score2","score3","score4","score5","score6","score7","score8","score9")
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

            }

            return( crScrMtx )
        }

        return( rObj )
    }

}


crMName <- "crScrN02E"  # Cut-Result, Score N, Raw val only
if( TRUE ){

    bCMtxLst[[crMName]] <- function( hCRScr=NULL ){
        # hCRScr : cutRst1Score 히스토리. Rebound 체크 기능은 나중에 구현한다.

        rObj <- list( 	idStr=crMName  ,mName=c("score1","score2","score3","score4","score5","score6","score7","score8","score9")
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

            }

            return( crScrMtx )
        }

        return( rObj )
    }

}


crMName <- "crScrN02PhEvt"  # phase 별로 evt 갯수 제한.(phase에서 다수 mName을 대상으로..)
if( TRUE ){
    bCMtxLst[[crMName]] <- function( hCRScr=NULL ){
        # hCRScr : cutRst1Score 히스토리. Rebound 체크 기능은 나중에 구현한다.
        rObj <- list( 	idStr=crMName  ,mName=c("score1","score2","score3","score4","score5","score6","score7","score8","score9")
		)

        rObj$fMtxObj <- function( scoreMtx.grp ,cut.grp ,fHName="sfcLate" ){
            if( FALSE ){    # comment
                # sample code for debug
                #       mIdx <- "score1"
                #       rawMtx <- sapply( scoreMtx.grp$basic ,function(p){ p[[mIdx]]$scoreMtx[1,] })
                #       cfg <- scoreMtxCfg[[ mIdx ]]
                #       evtObj <- bFCust.getEvtMtx( rawMtx ,cfg )
                #       
            }

            tgt.scMtx <- rObj$mName
            cutRst1Score <- bUtil.getCut1Score( scoreMtx.grp ,cut.grp ,fHName ,tgt.scMtx=tgt.scMtx )

            aLen <- length(cutRst1Score$aLst)
            cName <- c(  "e3Max" ,"e3MCnt" ,"e2Max" ,"e2MCnt" ,"e1Max" ,"e1MCnt"   # Cnt는 evt가 2번 이상 발생한 pName 수
                        ,"rebRawMax" ,"rebRawMCnt" ,"rebEvtMax" ,"rebEvtMCnt"
            )
            crScrMtx <- matrix( 0, nrow=aLen, ncol=length(cName) )	;colnames(crScrMtx) <- cName

            phNameAll <- names(scoreMtx.grp$basic)
            #   중간 계산 및 디버깅용 데이터 매트릭스
            tempCName <- c("e1","e2","e3" ,"rebRCnt" ,"rebECnt" )
            tempMtx <- matrix( 0 ,nrow=length(tempCName) ,ncol=length(phNameAll) ,dimnames=list(tempCName,phNameAll) )

            hName <- "sfcLate"  # 일단 rebound 없이 체크하는 부분.
            for( aIdx in 1:aLen ){
                curCR <- cutRst1Score$aLst[[aIdx]][[hName]]$basic   # "rebMtx.ph"    "evtHpnLevMtx" "phaseReb"

                tempMtx[,] <- 0
                for( mIdx in rObj$mName ){
                    evtHpnLevMtx <- curCR[[mIdx]]$raw$evtHpnLevMtx
                    
                    # evt lev1은 너무 흔해서, 하나의 phase 내에서 lev1 2개 이하는 자르자.
                    lev1 <- evtHpnLevMtx["lev1",]
                    evtHpnLevMtx["lev1",] <- ifelse(lev1>2,lev1-2,0)

                    # evt lev2도 1개 이하는 자르자.                    
                    lev2 <- evtHpnLevMtx["lev2",]
                    evtHpnLevMtx["lev2",] <- ifelse(lev2>1,lev2-1,0)

                    for( pIdx in phNameAll ){  # nIdx == pName
                        tempMtx["e1",pIdx] <- tempMtx["e1",pIdx] + evtHpnLevMtx["lev1",pIdx]
                        tempMtx["e2",pIdx] <- tempMtx["e2",pIdx] + evtHpnLevMtx["lev2",pIdx]
                        tempMtx["e3",pIdx] <- tempMtx["e3",pIdx] + evtHpnLevMtx["lev3",pIdx]

                        rebMtx.ph <- curCR[[mIdx]]$raw$rebMtx.ph
                        tempMtx["rebRCnt",pIdx] <- tempMtx["rebRCnt",pIdx] + rebMtx.ph["rebFlag.raw",pIdx]
                        tempMtx["rebECnt",pIdx] <- tempMtx["rebECnt",pIdx] + rebMtx.ph["rebFlag.evt",pIdx]
                    }
                }

                crScrMtx[aIdx,"e1Max"]  <- max(tempMtx["e1",])
                crScrMtx[aIdx,"e1MCnt"] <- sum(tempMtx["e1",]>1)
                crScrMtx[aIdx,"e2Max"]  <- max(tempMtx["e2",])
                crScrMtx[aIdx,"e2MCnt"] <- sum(tempMtx["e2",]>1)
                crScrMtx[aIdx,"e3Max"]  <- max(tempMtx["e3",])
                crScrMtx[aIdx,"e3MCnt"] <- sum(tempMtx["e3",]>1)

                crScrMtx[aIdx,"rebRawMax"]  <- max(tempMtx["rebRCnt",])
                crScrMtx[aIdx,"rebRawMCnt"] <- sum(tempMtx["rebRCnt",]>1)
                crScrMtx[aIdx,"rebEvtMax"]  <- max(tempMtx["rebECnt",])
                crScrMtx[aIdx,"rebEvtMCnt"] <- sum(tempMtx["rebECnt",]>1)

                # MCnt가 1이면 Max 컬럼값과 중복의미가 되므로 0으로 없앤다.
                cName <- c("e1MCnt","e2MCnt","e3MCnt","rebRawMCnt","rebEvtMCnt")
                crScrMtx[aIdx,cName] <- ifelse( 2>crScrMtx[aIdx,cName] ,0 ,crScrMtx[aIdx,cName] )

            }

            return( crScrMtx )
        }
        return( rObj )
    }

}


crMName <- "crScrN02Sum"  # Cut-Result, Score N, Raw val only.   "sfcLate" only
if( TRUE ){

    bCMtxLst[[crMName]] <- function( hCRScr=NULL ){
        # hCRScr : cutRst1Score 히스토리. Rebound 체크 기능은 나중에 구현한다.

        rObj <- list( 	idStr=crMName  ,mName=c("score1","score2","score3","score4","score5","score6","score7","score8","score9")
		)

        rObj$fMtxObj <- function( scoreMtx.grp ,cut.grp ,fHName="sfcLate" ){
            tgt.scMtx <- rObj$mName
            cutRst1Score <- bUtil.getCut1Score( scoreMtx.grp ,cut.grp ,fHName ,tgt.scMtx=tgt.scMtx )

            aLen <- length(cutRst1Score$aLst)
            cName <- c(	 "summSumRaw"   ,"summSumEvt"       ,"summSumOthRaw"    ,"summSumOthEvt"
                        ,"summSumRebRaw","summSumRebEvt"    ,"summSumRebOthRaw" ,"summSumRebOthEvt"
                        ,"szSumRebCnt"  ,"szSumRebDup"
            )
            crScrMtx <- matrix( 0, nrow=aLen, ncol=length(cName) )	;colnames(crScrMtx) <- cName

            hName <- "sfcLate"  # 일단 rebound 없이 체크하는 부분.
            for( aIdx in 1:aLen ){

                summMtx <- NULL ;summMtx.reb <- NULL    ;scMtx.sz <- NULL
                for( mName in names(cutRst1Score$aLst[[aIdx]][[hName]]$basic) ){
                    rawObj <- cutRst1Score$aLst[[aIdx]][[hName]]$basic[[mName]]$raw
                    summObj <- cutRst1Score$aLst[[aIdx]][[hName]]$basic[[mName]]$summ

                    if( is.null(summMtx) ){    summMtx <- summObj$summMtx
                    } else {    summMtx <- summMtx + summObj$summMtx                }
                    if( is.null(summMtx.reb) ){    summMtx.reb <- summObj$summMtx.reb
                    } else {    summMtx.reb <- summMtx.reb + summObj$summMtx.reb    }

                    if( is.null(scMtx.sz) ){    scMtx.sz <- summObj$scMtx.sz
                    } else {    scMtx.sz <- scMtx.sz + summObj$scMtx.sz    }
                }

                crScrMtx[aIdx,"summSumRaw"]     <- sum( summMtx["raw",c("ph","fCol")] )
                crScrMtx[aIdx,"summSumEvt"]     <- sum( summMtx["evt",c("ph","fCol")] )
                crScrMtx[aIdx,"summSumOthRaw"]  <- sum( summMtx["raw",c("phReb","xyCnt.fCol","xyCnt.phase")] )
                crScrMtx[aIdx,"summSumOthEvt"]  <- sum( summMtx["evt",c("phReb","xyCnt.fCol","xyCnt.phase")] )

                crScrMtx[aIdx,"summSumRebRaw"]      <- sum( summMtx.reb["raw",c("ph","fCol")] )
                crScrMtx[aIdx,"summSumRebEvt"]      <- sum( summMtx.reb["evt",c("ph","fCol")] )
                crScrMtx[aIdx,"summSumRebOthRaw"]   <- sum( summMtx.reb["raw",c("phReb","xyCnt.fCol","xyCnt.phase")] )
                crScrMtx[aIdx,"summSumRebOthEvt"]   <- sum( summMtx.reb["evt",c("phReb","xyCnt.fCol","xyCnt.phase")] )

                crScrMtx[aIdx,"szSumRebCnt"]    <- sum( scMtx.sz["rebCnt",c("r.ph","r.fCol","e.ph","e.fCol")] )
                crScrMtx[aIdx,"szSumRebDup"]    <- sum( scMtx.sz["rebDup",c("r.ph","r.fCol","e.ph","e.fCol")] )
            }
            return( crScrMtx )
        }

        return( rObj )
    }

}

crMName <- "crScrN02SumClM"    # close max (for all hName)
if( TRUE ){

    bCMtxLst[[crMName]] <- function( hCRScr=NULL ){

        rObj <- list( 	idStr=crMName  ,mName=c("score1","score2","score3","score4","score5","score6","score7","score8","score9")
		)

        rObj$fMtxObj <- function( scoreMtx.grp ,cut.grp ,fHName ){
            # fHName은 cutRst에서 얻어낸다.
            if( FALSE ){    # comment
                # sample code for debug
                #       mIdx <- "score1"
                #       rawMtx <- sapply( scoreMtx.grp$basic ,function(p){ p[[mIdx]]$scoreMtx[1,] })
                #       cfg <- scoreMtxCfg[[ mIdx ]]
                #       evtObj <- bFCust.getEvtMtx( rawMtx ,cfg )
                #       
            }

            tgt.scMtx <- rObj$mName
            cutRst1Score <- bUtil.getCut1Score( scoreMtx.grp ,cut.grp ,fHName ,tgt.scMtx=tgt.scMtx )

            aLen <- length(cutRst1Score$aLst)
            cName <- c(  "sumTotHpn" ,"sumTot1" ,"sumTot2" ,"sumTot3"
            )
            crScrMtx <- matrix( 0, nrow=aLen, ncol=length(cName) )	;colnames(crScrMtx) <- cName

            phNameAll <- names(scoreMtx.grp$basic)

            for( aIdx in 1:aLen ){
                clM <- bUtil.getClM_cutRst1Score( cutRst1=cutRst1Score$aLst[[aIdx]] ,cfgLst=scoreMtxCfg ,mNameGrp=rObj$mName ,fHName )
                crScrMtx[aIdx,"sumTotHpn"]  <- sum(clM$sumTot>0)
                crScrMtx[aIdx,"sumTot1"]  <- sum(clM$sumTot==1)
                crScrMtx[aIdx,"sumTot2"]  <- sum(clM$sumTot==2)
                crScrMtx[aIdx,"sumTot3"]  <- sum(clM$sumTot==3)

                # clM$summMtx ,clM$summMtx.reb에 대해서는... 차후에 검토하자.
            }

            return( crScrMtx )
        }
        return( rObj )
    }

}



