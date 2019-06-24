

bUtil.getStdMILst <- function( gEnv ,fRstLst ){

    # stdMI.basic <- fCutU.getStdMI( gEnv )

	stdMILst.basic <- list()

	zMtx <- gEnv$zhF
	stdMILst.basic[["basic"]] <- list( stdMI=fCutU.getMtxInfo(zMtx) ,zMtx=zMtx )

	zMtx <- fCutU.getNextZW( gEnv )$zMtx
	stdMILst.basic[["nextZW"]] <- list( zMtx=zMtx )
	stdMILst.basic[["nextZW"]]$stdMI <- if( 0==nrow(zMtx) ) NULL else fCutU.getMtxInfo( zMtx )

	zMtx <- fCutU.getNextQuo10( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	stdMILst.basic[["nextQuo10"]] <- list( zMtx=zMtx )
	stdMILst.basic[["nextQuo10"]]$stdMI <- if( 0==nrow(zMtx) ) NULL else fCutU.getMtxInfo( zMtx )

	zMtx <- fCutU.getNextBin( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	stdMILst.basic[["nextBin"]] <- list( zMtx=zMtx )
	stdMILst.basic[["nextBin"]]$stdMI <- if( 0==nrow(zMtx) ) NULL else fCutU.getMtxInfo( zMtx )

	rebObj <- fCutU.getNextRebNumPtn( gEnv ,numPtn=NULL )
	zMtx <- rebObj$zMtx	# rptObj<-anaQuoTbl( zMtx )
	stdMILst.basic[["nextRebNum"]] <- list( zMtx=zMtx )
	stdMILst.basic[["nextRebNum"]]$stdMI <- if( 0==nrow(zMtx) ) NULL else fCutU.getMtxInfo( zMtx )

	zMtx <- fCutU.getNextCStepBin( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	stdMILst.basic[["nextCStepBin"]] <- list( zMtx=zMtx )
	stdMILst.basic[["nextCStepBin"]]$stdMI <- if( 0==nrow(zMtx) ) NULL else fCutU.getMtxInfo( zMtx )

	zMtx <- fCutU.getNextFStepBin( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	stdMILst.basic[["nextFStepBin"]] <- list( zMtx=zMtx )
	stdMILst.basic[["nextFStepBin"]]$stdMI <- if( 0==nrow(zMtx) ) NULL else fCutU.getMtxInfo( zMtx )

	zMtx <- fCutU.getNextColVal( gEnv ,1 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	stdMILst.basic[["nextColVal_1"]] <- list( zMtx=zMtx )
	stdMILst.basic[["nextColVal_1"]]$stdMI <- if( 0==nrow(zMtx) ) NULL else fCutU.getMtxInfo( zMtx )

	zMtx <- fCutU.getNextColVal( gEnv ,2 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	stdMILst.basic[["nextColVal_2"]] <- list( zMtx=zMtx )
	stdMILst.basic[["nextColVal_2"]]$stdMI <- if( 0==nrow(zMtx) ) NULL else fCutU.getMtxInfo( zMtx )

	zMtx <- fCutU.getNextColVal( gEnv ,3 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	stdMILst.basic[["nextColVal_3"]] <- list( zMtx=zMtx )
	stdMILst.basic[["nextColVal_3"]]$stdMI <- if( 0==nrow(zMtx) ) NULL else fCutU.getMtxInfo( zMtx )

	zMtx <- fCutU.getNextColVal( gEnv ,4 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	stdMILst.basic[["nextColVal_4"]] <- list( zMtx=zMtx )
	stdMILst.basic[["nextColVal_4"]]$stdMI <- if( 0==nrow(zMtx) ) NULL else fCutU.getMtxInfo( zMtx )

	zMtx <- fCutU.getNextColVal( gEnv ,5 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	stdMILst.basic[["nextColVal_5"]] <- list( zMtx=zMtx )
	stdMILst.basic[["nextColVal_5"]]$stdMI <- if( 0==nrow(zMtx) ) NULL else fCutU.getMtxInfo( zMtx )

	zMtx <- fCutU.getNextColVal( gEnv ,6 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	stdMILst.basic[["nextColVal_6"]] <- list( zMtx=zMtx )
	stdMILst.basic[["nextColVal_6"]]$stdMI <- if( 0==nrow(zMtx) ) NULL else fCutU.getMtxInfo( zMtx )

    # todo stdFiltedCnt
	#   stdMILst.basic[["stdFCnt"]]

    # todo
    stdMI.bDup <- list()        # basic에서 동일 발생한 것들 끼리의 stdMI (예:colval_1값과 ZW값이 동일했던 적)

    # todo
    stdMI.mf <- list()        # lastZoid가 해당되던 main filter(D0000.A, A0100.A 등)

	rObj <- list( basic=stdMILst.basic ,bDup=stdMI.bDup ,mf=stdMI.mf )

    return( rObj )

}	# bUtil.getStdMILst()
