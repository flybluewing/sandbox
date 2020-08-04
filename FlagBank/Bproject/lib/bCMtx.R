#   cutRst1Score <- bUtil.getCut1Score( scoreMtx.grp ,cut.grp ,fHName ,tgt.scMtx=tgt.scMtx )

bCMtxLst <- list()


crMName <- "crScrN01"  # Cut-Result, Score N
if( TRUE ){

    bCMtxLst[[crMName]] <- function( hCRScr=NULL ){
        # hCRScr : cutRst1Score 히스토리. Rebound 체크 기능은 나중에 구현한다.

        rObj <- list( 	idStr=crMName  ,mName=c("score1","score3","score8")
		)

        rObj$fMtxObj <- function( scoreMtx.grp ,cut.grp ,fHName="sfcLate" ){
            
            tgt.scMtx <- rObj$mName
            cutRst1Score <- bUtil.getCut1Score( scoreMtx.grp ,cut.grp ,fHName ,tgt.scMtx=tgt.scMtx )

            aLen <- length(cutRst1Score$aLst)
            cName <- c(	 "rHpnE0Cnt" ,"eHpnl1Cnt" # raw==0 cnt, evt<=1 cnt
                        ,"phRebCntR" ,"phRebCntE" 
                    )
            crScrMtx <- matrix( 0, nrow=aLen, ncol=length(cName) )	;colnames(crScrMtx) <- cName

            hName <- "sfcLate"  # 일단 rebound 없이 체크하는 부분.
            for( aIdx in 1:aLen ){
                curCR <- cutRst1Score$aLst[[aIdx]][[hName]]$basic   # "phaseHpnCnt" "phaseRebCnt" "rebMtx.ph"

                hpnCnt <- sapply( curCR ,function(crObj){ sum(crObj$raw$phaseHpnCnt["raw",]>0) })
                crScrMtx[aIdx,"rHpnE0Cnt"] <- sum(hpnCnt==0)

                hpnCnt <- sapply( curCR ,function(crObj){ sum(crObj$raw$phaseHpnCnt["evt",]>0) })
                crScrMtx[aIdx,"eHpnl1Cnt"] <- sum(hpnCnt<=1)

                rebCnt <- sapply( curCR ,function(crObj){ sum(crObj$raw$rebMtx.ph["rebFlag.raw",]>0) })
                crScrMtx[aIdx,"phRebCntR"] <- sum(rebCnt)

                rebCnt <- sapply( curCR ,function(crObj){ sum(crObj$raw$rebMtx.ph["rebFlag.evt",]>0) })
                crScrMtx[aIdx,"phRebCntE"] <- sum(rebCnt)
            }

            return( crScrMtx )
        }

        return( rObj )
    }

}


