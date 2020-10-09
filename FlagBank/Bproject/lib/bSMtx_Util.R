
bS.getPhVPGrp <- function( gEnv ,aZoidMtx ){    # bUtil.getStdMILst( ) 와 비슷
    phVPLst <- list()

    phVPLst[[1+length(phVPLst)]] <- bS.vp_ColVal( gEnv, aZoidMtx, fixCol=1 )
    phVPLst[[1+length(phVPLst)]] <- bS.vp_ColVal( gEnv, aZoidMtx, fixCol=3 )
    phVPLst[[1+length(phVPLst)]] <- bS.vp_ColVal( gEnv, aZoidMtx, fixCol=6 )
    phVPLst[[1+length(phVPLst)]] <- bS.vp_remPair( gEnv, aZoidMtx )
    phVPLst[[1+length(phVPLst)]] <- bS.vp_zw( gEnv, aZoidMtx )
    #   차후 bS.vp_fStepBin() 도 추가.

    names(phVPLst) <- sapply( phVPLst ,function(p){p$idStr})

    phVPGrp <- list( phVPLst=phVPLst )
    phVPGrp$anyWarn <- function(){
        warnMsg <- character(0)
        for( pName in names(phVPGrp$phVPLst) ){
            phVP <- phVPGrp$phVPLst[[pName]]
            for( subPName in names(phVP$stdMILst) ){
                stdMI <- phVP$stdMILst[[subPName]]
                if( 6>nrow(stdMI$rawTail) ){
                    warnMsg <- c( warnMsg ,sprintf("    %s(%s) rawTail %d\n",pName,subPName,nrow(stdMI$rawTail)) )
                }
            }
        }
        return(warnMsg)
    }

    return( phVPGrp )
}

bS.makeHMtxLst <- function( gEnv, allIdxLst, fRstLst ,tgt.scMtx=NULL ,lastH=NULL ){

    hStr <- names(allIdxLst$stdFiltedCnt)
    names(fRstLst) <- hStr

    if( is.null(tgt.scMtx) ){
        tgt.scMtx <- names(bSMtxLst)
    } else {
        tgt.scMtx <- intersect( tgt.scMtx ,names(bSMtxLst) )
    }


    tStmp <- Sys.time()
    # ----------------------------------------------------
    firstH <- as.integer(hStr[1])
    if( is.null(lastH) ){
        lastH <-as.integer(hStr[length(hStr)])
    }
    fRstLst <- fRstLst[as.character(firstH:lastH)]

    fRstLst.hSpan <- as.integer(names(fRstLst))

    baseSpan <- 700:lastH
    stdFiltedCnt <- allIdxLst$stdFiltedCnt[as.character(baseSpan)]

    # ----------------------------------------------------
    sfcHLst <- list(    sfcLate= baseSpan[length(baseSpan)] - 20:0
                        ,sfc0=as.integer(names(stdFiltedCnt)[stdFiltedCnt==0])
                        ,sfc1=as.integer(names(stdFiltedCnt)[stdFiltedCnt==1])
                        ,sfc2=as.integer(names(stdFiltedCnt)[stdFiltedCnt==2])
                    )

    stdFilter <- c("D0000.A","A0100.A","AP000.E")   # "AR000.B","AL000A","C1000.A"
    for( sfnIdx in stdFilter ){
        #   sfnIdx <- "D0000.A"
        hSpan <- baseSpan[sapply( fRstLst[as.character(baseSpan)] ,function(p){ sfnIdx %in% p } )]
        hSpan.NG <- hSpan+1
        hSpan.NG <- hSpan.NG[hSpan.NG<=lastH]
        sfcHLst[[sprintf("NG%s",sfnIdx)]] <- hSpan.NG
    }
    lenMax <- 20
    for( hName in names(sfcHLst) ){ # hLst 범위는 20 이내로 하자.
        hLen <- length(sfcHLst[[hName]])
        if( lenMax < hLen ){
            sfcHLst[[hName]] <- sfcHLst[[hName]][ (hLen-lenMax+1):hLen ]
        }
    }

    scoreMtxLst <- list()
    for( sfcIdx in names(sfcHLst) ){    # sfcIdx <- names(sfcHLst)[2]

        scoreMtx.grp.lst <- list( )
        for( hIdx in sfcHLst[[sfcIdx]] ){   # hIdx <- sfcHLst[[sfcIdx]][1]
            stdZoid <- gEnv$zhF[hIdx ,]
            wEnv <- gEnv
            wEnv$zhF <- gEnv$zhF[1:(hIdx-1),]

            fRstLst.w <- fRstLst[as.character(fRstLst.hSpan[fRstLst.hSpan<hIdx])]

            aZoidMtx <- matrix(stdZoid ,nrow=1)
            phVP.grp <- bS.getPhVPGrp( wEnv ,aZoidMtx )
            warnMsg <- phVP.grp$anyWarn()
            # if( 0<length(warnMsg) ){      # bS에서의 phVP.grp 생성은 과거 데이터가 적은 경우가 너무 많아서...
            #     warnMsg <- paste("    ",warnMsg,sep="")
            #     warnMsg <- paste(warnMsg,collapse="")
            #     cat(sprintf("sfcIdx:%s  hIdx:%s \n%s",sfcIdx,hIdx,warnMsg))
            # }

            scoreMtx.grp <- bS.getScoreMtx.grp( phVP.grp ,aZoidMtx ,tgt.scMtx )
            scoreMtx.grp.lst[[sprintf("hIdx:%d",hIdx)]] <- scoreMtx.grp
        }

        basicHMtxLst <- list()
        # scoreMtxNames <- names(scoreMtx.grp.lst[[1]]$basic[[1]]) # 필터링이 아닌 H를 보려하는 것이므로 basic만 다룬다.
        # scoreMtxNames <- tgt.scMtx
        for( pName in names(scoreMtx.grp.lst[[1]]$basic) ){ # nIdx<-names(scoreMtx.grp.lst[[1]]$basic)[1]
           # for( nIdx in names(scoreMtx.grp.lst[[1]]$basic) ){ # nIdx<-names(scoreMtx.grp.lst[[1]]$basic)[1]
            mtxLst <- list()
            for( mName in tgt.scMtx ){ # smnIdx <-scoreMtxNames[1]
                # for( smnIdx in scoreMtxNames ){ # smnIdx <-scoreMtxNames[1]
                scoreMtx <- NULL    ;infoMtx<-NULL
                for( rIdx in seq_len(length(scoreMtx.grp.lst)) ){
                    scoreObj <- scoreMtx.grp.lst[[rIdx]]$basic[[pName]][[mName]]
                    scoreMtx <- rbind( scoreMtx ,scoreObj$scoreMtx[1,] )
                    if( any(is.na(scoreObj$scoreMtx[1,])) ){
                        hStr <- sfcHLst[[sfcIdx]][rIdx]
                        colStr <- paste( names(scoreObj$scoreMtx[1,])[which(is.na(scoreObj$scoreMtx[1,]))],collapse=",")
                        k.FLogStr(sprintf("WARN : NA - %s, %s, %s(%s), %s",sfcIdx,pName,mName,colStr,hStr)
                                    ,pConsole=T
                                )
                    }
                    if( !is.null(scoreObj$infoMtx) ){
                        infoMtx <- rbind( infoMtx ,scoreObj$infoMtx[1,] )
                    }
                }

                if( !is.null(scoreMtx) )    rownames(scoreMtx) <- sfcHLst[[sfcIdx]]

                if( !is.null(infoMtx) ) rownames(infoMtx) <- sfcHLst[[sfcIdx]]

                mtxLst[[mName]] <- list( scoreMtx=scoreMtx ,infoMtx=infoMtx )
            }

            basicHMtxLst[[pName]] <- mtxLst
        }

        scoreMtxLst[[sfcIdx]] <- basicHMtxLst
    }


    mtxInfoLst <- lapply( scoreMtxLst[[1]][[1]] ,function( pLst ){
                        colnames(pLst$scoreMtx)
                    })
    phaseName <- names(scoreMtxLst[[1]])

    rObj <- list( sfcHLst=sfcHLst ,lastH=lastH
                    ,mtxInfoLst=mtxInfoLst
                    ,phaseName=phaseName
                    ,scoreMtxLst=scoreMtxLst 
    )


    cnt <- sapply(sfcHLst,length)
    tDiff <- Sys.time() - tStmp
    cat(sprintf("       %d time %.1f%s(tgt.scMtx:%s)   %s\n", lastH
            ,tDiff  ,units(tDiff)   ,ifelse( is.null(tgt.scMtx),"*",paste(tgt.scMtx,collapse=",") )
            ,paste(paste(names(cnt),cnt,sep=":") ,collapse="   " ) 
    ))

    return( rObj )

}


bS.getScoreMtx.grp <- function( phVP.grp ,aZoidMtx ,tgt.scMtx=NULL ){

    rObj <- list( basic=list() ,bDup=list() ,mf=list() )    # 호환성을 위해, getScoreMtx.grp()와 유사한 구조로 맞춘다.

    if( is.null(tgt.scMtx) ){
        tgt.scMtx <- names(bSMtxLst)
    } else {
        tgt.scMtx <- intersect( tgt.scMtx ,names(bSMtxLst) )
    }

    # working
    for( pName in names(phVP.grp$phVPLst) ){
        phVP <- phVP.grp$phVPLst[[pName]]
        scoreMtxLst <- list()
        for( mName in tgt.scMtx ){
            scoreMtxLst[[mName]] <- bSMtxLst[[mName]](phVP ,aZoidMtx)
        }
        rObj$basic[[pName]] <- scoreMtxLst
    }

    return( rObj )
}


bS.getCutGrp <- function( hMtxLst_bS ,tgt.scMtx=NULL ){
    #   bFCust.getFCustGrp( hMtxLst_bS ,tgt.scMtx )  참고

    rObj <- list(   sfcHLst = hMtxLst_bS$sfcHLst
                    ,mtxInfoLst = hMtxLst_bS$mtxInfoLst
                    ,phaseName = hMtxLst_bS$phaseName
    )

    if( !is.null(tgt.scMtx) ){
        availMtx <- intersect( tgt.scMtx ,names(rObj$mtxInfoLst) )
        rObj$mtxInfoLst <- rObj$mtxInfoLst[availMtx]
    }

	cutterLst <- list()         ;cutterExtLst <- list()         ;cutterExtMLst <- list()
	for( hName in names(rObj$sfcHLst) ){	    # hName <- names(rObj$sfcHLst)[1]

        mLst <- list()  ;mExtLst <- list()  #   cutterLst
        for( mName in names(rObj$mtxInfoLst) ){	# mName <- names(rObj$mtxInfoLst)[1]

            # <stdCut>
            stdCut <- list()    ;stdCutExt <- list()
            for( pName in rObj$phaseName ){     # pName <- rObj$phaseName[1]
                scoreMtxObj <- bS.HMtxLst_getMtxLst( hMtxLst_bS , hName ,mName ,pName )
                stdCut[[pName]] <- bS_stdCut.rawRow( hName ,mName ,pName ,scoreMtxObj$scoreMtx )

                if( is.null(bSMtxExtFltLst[[mName]]) ){   stdCutExt[[mName]] <- list()
                } else {
                    fltLst <- list()
                    for( nIdx in names(bSMtxExtFltLst[[mName]]) ){
                        # bSMtxExtFltLst[[mName]][[nIdx]] 는 FCust_stdCutExt.rawRow() 내부에서 이용됨.
                        #     fltLst[[nIdx]] <- FCust_stdCutExt.rawRow( hName ,mName ,pName ,scoreMtxObj$scoreMtx ,fltName=nIdx )
                        fltLst[[nIdx]] <- bS_stdCutExt.rawRow( hName ,mName ,pName ,scoreMtxObj$scoreMtx ,fltName=nIdx )
                    }
                    stdCutExt[[pName]] <- fltLst
                }

            }

            # <fColCut>
            fColCut <- list()   # preserve

            # <hIdxCut>
			hIdxObj <- bS.getHMtxLst_byHIdx( hMtxLst_bS )
            hIdxCut <- bS_stdCut.hIdx( hName ,mName ,mtxLst=hIdxObj[[hName]][[mName]] )

            mLst[[mName]] <- list( stdCut=stdCut ,fColCut=fColCut ,hIdxCut=hIdxCut )
            mExtLst[[mName]] <- list( stdCut=stdCutExt )
        }
        cutterLst[[hName]] <- mLst
        cutterExtLst[[hName]] <- mExtLst

        # cutterExtMLst ---------------------------------------------------------------
        if( TRUE ){        }

    }

    rObj$cutterLst          <- cutterLst
    rObj$cutterExtLst       <- cutterExtLst
    rObj$cutterExtMLst      <- cutterExtMLst

    return( rObj )

}


bS.HMtxLst_getMtxLst <- function( hMtxLst_bS ,hName ,mName ,pName ,tgt="scoreMtxLst" ){

    mtxLst <- NULL
    if( tgt=="scoreMtxLst" ){
        mtxLst <- hMtxLst_bS[[tgt]][[hName]][[pName]][[mName]]
    } else if( tgt=="mfMtxLst" ){
        mtxLst <- hMtxLst_bS[[tgt]][[hName]][[mName]]
    }

    return( mtxLst )

}


bS.getHMtxLst_byHIdx <- function( hMtxLst_bS ){
    #   phase * FCol for each HIdx
    #       B.getHMtxLst_byHIdx() 참고
    rLst <- list()

    scoreMtxLst <- hMtxLst_bS$scoreMtxLst
    sfcHLst <- hMtxLst_bS$sfcHLst
    mtxInfoLst <- hMtxLst_bS$mtxInfoLst
    phaseName <- hMtxLst_bS$phaseName

    for( hName in names(scoreMtxLst) ){ # hName <- names(scoreMtxLst)[1]
        scmLst <- list()
        for( mName in names(mtxInfoLst) ){ # mName <- names(mtxInfoLst)[1]
            mtx <- matrix( 0, ncol=length(phaseName) ,nrow=length(mtxInfoLst[[mName]]) )
            rownames(mtx) <- mtxInfoLst[[mName]]
            colnames(mtx) <- phaseName
            byHLst <- list()
            for( hIdx in as.character(sfcHLst[[hName]]) ){ # hIdx <- as.character(sfcHLst[[hName]])[1]
                mtx[,] <- 0
                for( pnIdx in phaseName ){ # pnIdx <- phaseName[1]
                    scoreMtx <- scoreMtxLst[[hName]][[pnIdx]][[mName]]$scoreMtx
                    mtx[,pnIdx] <- scoreMtx[hIdx,]
                }
                byHLst[[hIdx]] <- mtx
            }
            scmLst[[mName]] <- byHLst
        }
        rLst[[hName]] <- scmLst
    }

    return( rLst )

}


#	byHIdx이긴 하지만, 사실은 각 scoreMtxN 에 대한 [col,phase] 테이블이다.
#	즉 allIdx 단위별로 List가 만들어짐.
bS.getScoreMtx.grp_byHIdx <- function( scoreMtx.grp ,tgt.scMtx=NULL ){
	#	scoreMtx.grp <- getScoreMtx.grp( gEnv$allZoidMtx[allIdxF,] ,filter.grp )
	#	aZoid, scoreMtx별로 [fCol,phName] 구조.
	#		(Column이 phase 이므로 기본 phase만 가능하다.)
	phaseName <- names(scoreMtx.grp$basic)
	mtxInfoLst <- list()
	if( is.null(tgt.scMtx) ){
		mtxInfoLst <- lapply(scoreMtx.grp$basic[[1]] ,function( scoreObj ){ colnames(scoreObj$scoreMtx) })
	} else {
		mtxInfoLst <- lapply(scoreMtx.grp$basic[[1]][tgt.scMtx] ,function( scoreObj ){ colnames(scoreObj$scoreMtx) })
	}

	rowSize <- nrow(scoreMtx.grp$basic[[1]][[1]]$scoreMtx)

	# hMtx_byHIdx[["sfcLate"]][["score2"]][["820"]]
	mLst <- list()
	for( mName in names(mtxInfoLst) ){	# mName <- names(mtxInfoLst)[1]
		aZoidLst <- list()
		for( aIdx in seq_len(rowSize) ){ # aIdx <- 1
			mtx <- matrix( 0, nrow=length(mtxInfoLst[[mName]]), ncol=length(phaseName) )
			colnames(mtx) <- phaseName	;rownames(mtx) <- mtxInfoLst[[mName]]
			for( pName in phaseName ){	# pName <- phaseName[1]
				mtx[,pName] <- scoreMtx.grp$basic[[pName]][[mName]]$scoreMtx[aIdx,]
			}
			aZoidLst[[aIdx]] <- mtx
		}
		mLst[[mName]] <- aZoidLst
	}

	return( mLst )

}


bS.cut <- function( scoreMtx.grp ,cut.grp ,fHName ,tgt.scMtx=NULL ,anaOnly=T ,logger=NULL ){
    # bUtil.R - bUtil.cut1() 참고

	reportStatus <- function( tStmp ,strWhere ,surFlag ,logger ){
		#	strWhere <- sprintf("[%s,%s] stdLst",hName,mName)
		if( is.null(logger) )	return( NULL )
		tDiff <- Sys.time() - tStmp
		rptStr <- sprintf("    %s  %d/%d  %.1f%s ",strWhere,sum(surFlag),length(surFlag),tDiff,units(tDiff) )
		logger$fLogStr( rptStr )
	}


    # scoreMtx.grp <- wScoreMtx.grp ;anaOnly=T
    scMtxName <- names(cut.grp$mtxInfoLst)
	if( !is.null(tgt.scMtx) )	scMtxName <- intersect( scMtxName ,tgt.scMtx )

	# bScrMtxName
	bScrMtxName <- names(cut.grp$mtxInfoLst.bScr)
	if( !is.null(tgt.scMtx) )	bScrMtxName <- intersect( bScrMtxName ,tgt.scMtx )

    datLen <- 1
	cutInfoLst <- list()
	if( !anaOnly ){
		cutInfoLst <- NULL

		if( 0<length(scMtxName) ){
			datLen <- nrow(scoreMtx.grp$basic[[1]][[ scMtxName[1] ]]$scoreMtx)
		} else if( 0<length(bScrMtxName) ){
			datLen <- nrow(scoreMtx.grp$mf[[ bScrMtxName[1] ]]$scoreMtx)
		}
	}

	tStmp <- Sys.time()
	if( !is.null(logger) ) logger$fLogStr("Start", pTime=T ,pAppend=F )

    surFlag <- rep( T ,datLen )
	auxInfoLst <- list( basic=list() ,mf=list() )
	mtxGrp <- NULL
	if( 0<length(scMtxName) ){
		mtxGrp <- bS.getScoreMtx.grp_byHIdx( scoreMtx.grp )    # getScoreMtx.grp_byHIdx() 대체
	}

    for( hName in fHName ){ # hName <- fHName[1]

        for( mName in scMtxName ){ # mName <- scMtxName[1]
            #   "stdCut" -------------------------------------------
            for( pName in cut.grp$phaseName ){   # pName <- cut.grp$phaseName[1]
				scoreMtx <- scoreMtx.grp$basic[[pName]][[mName]]$scoreMtx

                cutObj <- cut.grp$cutterLst[[hName]][[mName]]$stdCut[[pName]]
                cRst <- cutObj$cut( scoreMtx ,alreadyDead=!surFlag ,anaMode=anaOnly )
				if( !anaOnly ){	surFlag <- surFlag & cRst$surFlag
				} else {
					if( 0<length(cRst$cutLst) ){
						cutInfoLst <- append( cutInfoLst 
											,lapply( cRst$cutLst[[1]]$cLst ,function(p){ c(p$idObjDesc ,info=p$info) } ) 
										)
					}
				}

				for( extFltName in names(cut.grp$cutterExtLst[[hName]][[mName]]$stdCut[[pName]]) ){
					cutExtObj <- cut.grp$cutterExtLst[[hName]][[mName]]$stdCut[[pName]][[extFltName]]
					cRst <- cutExtObj$cut( scoreMtx ,alreadyDead=!surFlag ,anaMode=anaOnly )
					if( !anaOnly ){	surFlag <- surFlag & cRst$surFlag
					} else {
						if( 0<length(cRst$cutLst) ){
							cutInfoLst <- append( cutInfoLst 
												,lapply( cRst$cutLst[[1]]$cLst ,function(p){ c(p$idObjDesc ,info=p$info) } ) 
											)
						}
					}
				}

				reportStatus( tStmp ,sprintf("     %s",pName) ,surFlag ,logger )
            }
			reportStatus( tStmp ,sprintf("[%s,%s] stdLst",hName,mName) ,surFlag ,logger )

			#   "hIdxLst" ------------------------------------------
			hIdxCut <- cut.grp$cutterLst[[hName]][[mName]]$hIdxCut
			for( aIdx in seq_len(datLen) ){
				if( !anaOnly && !surFlag[aIdx] )	next

				cutInfo <- hIdxCut$cut( mtxGrp[[mName]][[aIdx]] ,anaMode=anaOnly )
				if( 0<length(cutInfo$cLst) ){
					if( !anaOnly ){	surFlag[aIdx] <- FALSE
					} else {
						for( idx in seq_len(length(cutInfo$cLst)) ){
							idxName <- sprintf("hIdxCut_%dth",1+length(cutInfoLst))
							cutInfoLst[[idxName]] <- c( typ=names(cutInfo$cLst)[idx] ,hIdxCut$defId ,pName="ALL" ,info=cutInfo$cLst[[idx]] )
						}
					}
				}

				if( anaOnly && ("sfcLate"==hName) ){	# anaOnly상태이면 aIdx는 항상 1이라는 가정.
					auxInfoLst$basic[[mName]] <- cutInfo$scObj
				}
				# if( 1<length(cRst$cutLst) ){
				# 	cutInfoLst[[1+length(cutInfoLst)]] <- cRst
				# }
			}

			reportStatus( tStmp ,sprintf("[%s,%s] hIdxLst",hName,mName) ,surFlag ,logger )

        }

		# bSMtxMulti ----------------------------------------------------------------
		availMFName <- names(cut.grp$cutterExtMLst[[hName]]$stdCut[[1]])
		for( mfName in availMFName ){
			mtxMaker <- bSMtxMFltLst[[mfName]]( tgt.scMtx )
			if( !mtxMaker$available )   next

			mtxLst <- list()
			for( pName in cut.grp$phaseName ){
				scoreMtxLst <- scoreMtx.grp$basic[[pName]]
				mtxLst[[pName]] <- mtxMaker$getScoreMtx( scoreMtxLst )

                cutObj <- cut.grp$cutterExtMLst[[hName]]$stdCut[[pName]][[mfName]]
                cRst <- cutObj$cut( mtxLst[[pName]] ,alreadyDead=!surFlag ,anaMode=anaOnly )
				if( !anaOnly ){	surFlag <- surFlag & cRst$surFlag
				} else {
					if( 0<length(cRst$cutLst) ){
						cutInfoLst <- append( cutInfoLst 
											,lapply( cRst$cutLst[[1]]$cLst ,function(p){ c(p$idObjDesc ,info=p$info) } ) 
										)
					}
				}
			}

			#   "hIdxLst" ------------------------------------------
			# mtxGrp$score1[[1]]
			mtx <- matrix( 0 ,nrow=length(mtxMaker$mInfo$cName) ,ncol=length(cut.grp$phaseName) 
						,dimnames=list( mtxMaker$mInfo$cName ,cut.grp$phaseName )
			)
			for( aIdx in seq_len(datLen) ){
				if( !anaOnly && !surFlag[aIdx] )	next

				for( pName in cut.grp$phaseName ){
					mtx[,pName] <- mtxLst[[pName]][aIdx,]
				}

				hIdxCut <- cut.grp$cutterExtMLst[[hName]]$hIdxCut[[mfName]]
				cutInfo <- hIdxCut$cut( mtx ,anaMode=anaOnly )
				if( 0<length(cutInfo$cLst) ){
					if( !anaOnly ){	surFlag[aIdx] <- FALSE
					} else {
						for( idx in seq_len(length(cutInfo$cLst)) ){
							idxName <- sprintf("hIdxCut_%dth",1+length(cutInfoLst))
							cutInfoLst[[idxName]] <- c( typ=names(cutInfo$cLst)[idx] ,hIdxCut$defId ,pName="ALL" ,info=cutInfo$cLst[[idx]] )
						}
					}
				}

			}

		}



    }

    return( list( surFlag=surFlag ,cutInfoLst=cutInfoLst ,auxInfoLst=auxInfoLst ) )

}
