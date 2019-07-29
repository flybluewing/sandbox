
#   stdCtrlCfgGrp <- bUtil.makeStdCtrlCfgGrp(hMtxLst)
bFCust.getFCustGrp <- function( stdCtrlCfgGrp ){

    rObj <- list(   sfcHLst = stdCtrlCfgGrp$sfcHLst
                    ,mtxInfoLst = stdCtrlCfgGrp$mtxInfoLst
                    ,phaseName = stdCtrlCfgGrp$phaseName        
    )

    ctrlCfgLst <- stdCtrlCfgGrp$ctrlCfgLst

	filterLst <- list()
	for( hName in names(rObj$sfcHLst) ){	# hName <- names(rObj$sfcHLst)[1]
        mLst <- list()
        for( mName in names(rObj$mtxInfoLst) ){	# mName <- names(rObj$mtxInfoLst)[1]

			stdLst <- list()
			for( pName in rObj$phaseName ){	# pName <- rObj$phaseName[1]
                #    stdLst ,fColLst ,hIdxLst
                colDirLst <- ctrlCfgLst[[hName]][[mName]][["stdLst"]][[pName]][["colDirLst"]]
                for( fcName in rObj$mtxInfoLst[[mName]] ){ # fcName <- rObj$mtxInfoLst[[mName]][1]
                    # working colDirLst[[fcName]]
                }

				# pLst[[pName]] <- bUtil.stdCtrlCfg.scoreMtx( scoreMtx )
			}
            #   work : n개 이상 컬럼에 대한 통제를 stdLst에 추가.

			fColLst <- list()
			# for( fcName in rObj$mtxInfoLst[[mName]] ){	# fcName <- rObj$mtxInfoLst[[mName]][1]
			# 	mtx <- byFCol[[hName]][[mName]][[fcName]]	# h * phase
			# 	fColLst[[fcName]] <- bUtil.stdCtrlCfg.h_ph4FCol( mtx )
			# }

			hIdxLst <- list()
			# for( hIdxName in as.character(rObj$sfcHLst[[hName]]) ){	# hIdxName <- as.character(rObj$sfcHLst[[hName]])[1]
			# 	mtx <- byHIdx[[hName]][[mName]][[hIdxName]]	# fCol * phase
			# 	hIdxLst[[hIdxName]] <- bUtil.stdCtrlCfg.h_ph4FCol( mtx )
			# }

            mLst[[mName]] <- list( std=stdLst ,fCol=fColLst ,hIdxLst)
		} # for(mName)

		filterLst[[hName]] <- mLst
    }

    rObj$filterLst <- filterLst
    return( rObj )

} # bFCust.getFCustGrp( )


bFCust.defaultStdColFilter <- function( hName, mName, pName, ){

} # bFCust.defaultStdColFilter()
