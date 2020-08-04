
    # cutRst1Score <- bUtil.getCut1Score( scoreMtx.grp ,cut.grp ,fHName ,tgt.scMtx=tgt.scMtx )
    # cut2Rst <- bUtil.cut2( cutRst1Score ,fHName ,tgt.scMtx=tgt.scMtx ,anaOnly=T )

    # cutRst$cutInfoLst <- append( cutRst$cutInfoLst ,cut2Rst$cutInfoLst )


bC.cut <- function( crMName ,scoreMtx.grp ,cut.grp ,anaOnly=F ,logger=NULL ){
    # 참고사항
    #   - 일단은 crMName 하나씩 처리하는 것으로 하자. 나중에 상태를 봐 가며 다수 cutter 추가 적용.
    #     bCMtxLst 내에서 bUtil.getCut1Score() 실행되는 횟수를 최소화 하기 위함.
    #   - scoreMtx.grp 을 외부에서 전체적으로(모든 mName) 만든 후에 파라미터로 넘기는 게 나은지,
    #     aZoidMtx를 넘겨받고서 crMName에 해당하는 것만 만드는 게 나은 지 측정 필요.
	reportStatus <- function( tStmp ,strWhere ,surFlag ,logger ){
		#	strWhere <- sprintf("[%s,%s] stdLst",hName,mName)
		if( is.null(logger) )	return( NULL )
		tDiff <- Sys.time() - tStmp
		rptStr <- sprintf("    %s  %d/%d  %.1f%s ",strWhere,sum(surFlag),length(surFlag),tDiff,units(tDiff) )
		logger$fLogStr( rptStr )
	}


    mtxMaker <- bCMtxLst[[crMName]]( )
    crScrMtx <- mtxMaker$fMtxObj( scoreMtx.grp ,cut.grp )

    datLen <- nrow(crScrMtx)
    surFlag <- rep( T ,datLen )
	cutInfoLst <- list()

    cutObj <- FCust_stdCut.CMtxRow( crMName )
    cRst <- cutObj$cut( crScrMtx )
    if( !anaOnly ){	surFlag <- surFlag & cRst$surFlag   #  차후 다수 필터 적용을 대비한 & 연산
    } else {
        if( 0<length(cRst$cutLst) ){
            cutInfoLst <- append( cutInfoLst 
                                ,lapply( cRst$cutLst[[1]]$cLst ,function(p){ c(p$idObjDesc ,info=p$info) } ) 
                            )
        }
    }

    return( list( surFlag=surFlag ,cutInfoLst=cutInfoLst ) )

}



