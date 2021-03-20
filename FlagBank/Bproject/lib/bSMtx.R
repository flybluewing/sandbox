bSMtxLst <- list()


if( TRUE ){ # "sScore01"

	bS.sScore1.cName <- c(	"rem0.num" ,"rem0.len.tot" ,"rem0.len.val"	# 이전에 값이 동일했던 컬럼쌍이, 다음에도 동일 쌍으로 나타나는 경우.
                            ,"rem1.num" ,"rem1.len.tot" ,"rem1.len.val"
                            ,"c0.num" ,"c0.len.tot" ,"c0.len.val" ,"c1.num" ,"c1.len.tot" ,"c1.len.val"
                            ,"f0.num" ,"f0.len.tot" ,"f0.len.val" ,"f1.num" ,"f1.len.tot" ,"f1.len.val"
	)

    bSMtx.sScore1 <- function( stdMI ,workArea ,wMI ,aObj ,aZoidMtx ){
        # stdMI<-phVP$stdMILst[[pvName]]
        # wMI <- phVP$getCodeH( stdMI )

        findSeg <- function( aCode ){

            segLst <- list()

            tbl <- table(aCode)
            if( all(tbl<2) ){
                return( segLst )
            } else {
                tbl <- tbl[tbl>=2]
            }

            for( val in as.integer(names(tbl)) ){
                segObj <- list( val=val ,idx=which(aCode==val) )
                segLst[[1+length(segLst)]] <- segObj
            }
            names(segLst) <- paste("val",names(tbl),sep="")

            return( segLst )
        }
        checkSeg <- function( segLst ,aCode ){
            rVal <- c( num=0 ,lenTot=0 ,lenVal=0 )
            for( lIdx in seq_len(length(segLst)) ){
                idx <- segLst[[lIdx]]$idx
                if( !all(aCode[idx[1]]==aCode[idx]) ) next

                rVal["num"]     <- rVal["num"] + 1
                rVal["lenTot"]  <- rVal["lenTot"] + length(idx)
                rVal["lenVal"]  <- rVal["lenVal"] + ifelse( segLst[[lIdx]]$val==aCode[idx[1]] ,length(idx) ,0 )
            }
            return( rVal )
        }

		scrMtx <- matrix( 0, nrow=length(workArea), ncol=length(bS.sScore1.cName) )	;colnames(scrMtx) <- bS.sScore1.cName
        rownames(scrMtx) <- workArea
        stdMILen <- nrow(wMI$rawTail)
        if( 0==stdMILen ){
            return( scrMtx )
        }

        segGrp <- list()
        segGrp[["rem0"]]<- findSeg( wMI$lastRaw%%10 )
        segGrp[["c0"]]  <- findSeg( wMI$cStepTail[stdMILen,] )
        segGrp[["f0"]]  <- NULL
        segGrp[["rem1"]]<-NULL      ;segGrp[["c1"]]<-NULL   ;segGrp[["f1"]]<-NULL
        if( 0<stdMI$mtxLen ){
            segGrp[["rem1"]]<- findSeg( wMI$rawTail[stdMILen-1,]%%10 )
            segGrp[["c1"]]  <- findSeg( wMI$cStepTail[stdMILen-1,] )

            segGrp[["f0"]]  <- findSeg( wMI$fStepTail[stdMILen,] )
        }
        if( 1<stdMI$mtxLen ){
            segGrp[["f1"]]  <- findSeg( wMI$fStepTail[stdMILen-1,] )
        }

        for( wIdx in seq_len(length(workArea)) ){
            aIdx <- workArea[wIdx]
            aRem <- aObj$aZoidMtx[aIdx,]%%10
            aCStep <- aObj$cStepMtx[aIdx,]
            aFStep <- aObj$fStepMtx[aIdx,]

            fndInfo <- checkSeg( segGrp[["rem0"]] ,aRem )
            scrMtx[wIdx,"rem0.num"]     <- fndInfo["num"]
            scrMtx[wIdx,"rem0.len.tot"] <- fndInfo["lenTot"]
            scrMtx[wIdx,"rem0.len.val"] <- fndInfo["lenVal"]

            fndInfo <- checkSeg( segGrp[["c0"]] ,aCStep )
            scrMtx[wIdx,"c0.num"]     <- fndInfo["num"]
            scrMtx[wIdx,"c0.len.tot"] <- fndInfo["lenTot"]
            scrMtx[wIdx,"c0.len.val"] <- fndInfo["lenVal"]

            if( !is.null(segGrp[["f0"]]) ){
                fndInfo <- checkSeg( segGrp[["f0"]] ,aFStep )
                scrMtx[wIdx,"f0.num"]     <- fndInfo["num"]
                scrMtx[wIdx,"f0.len.tot"] <- fndInfo["lenTot"]
                scrMtx[wIdx,"f0.len.val"] <- fndInfo["lenVal"]
            }

            if( !is.null(segGrp[["rem1"]]) ){
                fndInfo <- checkSeg( segGrp[["rem1"]] ,aRem )
                scrMtx[wIdx,"rem1.num"]     <- fndInfo["num"]
                scrMtx[wIdx,"rem1.len.tot"] <- fndInfo["lenTot"]
                scrMtx[wIdx,"rem1.len.val"] <- fndInfo["lenVal"]
            }
            if( !is.null(segGrp[["c1"]]) ){
                fndInfo <- checkSeg( segGrp[["c1"]] ,aCStep )
                scrMtx[wIdx,"c1.num"]     <- fndInfo["num"]
                scrMtx[wIdx,"c1.len.tot"] <- fndInfo["lenTot"]
                scrMtx[wIdx,"c1.len.val"] <- fndInfo["lenVal"]
            }
            if( !is.null(segGrp[["f1"]]) ){
                fndInfo <- checkSeg( segGrp[["f1"]] ,aFStep )
                scrMtx[wIdx,"f1.num"]     <- fndInfo["num"]
                scrMtx[wIdx,"f1.len.tot"] <- fndInfo["lenTot"]
                scrMtx[wIdx,"f1.len.val"] <- fndInfo["lenVal"]
            }

            if( any(0<scrMtx[wIdx,]) ){
                aQuo <- fCutU.getQuoObj( aZoidMtx[aIdx,] ,valSet=T )
                if( all(aQuo$size==stdMI$quoTail[stdMILen,]) ){
                    if( 0<scrMtx[wIdx,"rem0.len.tot"] ){
                        cName <- c("rem0.num" ,"rem0.len.tot" ,"rem0.len.val")
                        scrMtx[wIdx,cName] <- 1 + scrMtx[wIdx,cName]
                    }
                    if( 0<scrMtx[wIdx,"c0.len.tot"] ){
                        cName <- c( "c0.num" ,"c0.len.tot" ,"c0.len.val" )
                        scrMtx[wIdx,cName] <- 1 + scrMtx[wIdx,cName]
                    }
                    if( 0<scrMtx[wIdx,"f0.len.tot"] ){
                        cName <- c( "f0.num" ,"f0.len.tot" ,"f0.len.val" )
                        scrMtx[wIdx,cName] <- 1 + scrMtx[wIdx,cName]
                    }
                }
                if( 1<stdMILen && all(aQuo$size==stdMI$quoTail[stdMILen-1,]) ){
                    if( 0<scrMtx[wIdx,"rem1.len.tot"] ){
                        cName <- c( "rem1.num" ,"rem1.len.tot" ,"rem1.len.val" )
                        scrMtx[wIdx,cName] <- 1 + scrMtx[wIdx,cName]
                    }
                    if( 0<scrMtx[wIdx,"c1.len.tot"] ){
                        cName <- c( "c1.num" ,"c1.len.tot" ,"c1.len.val" )
                        scrMtx[wIdx,cName] <- 1 + scrMtx[wIdx,cName]
                    }
                    if( 0<scrMtx[wIdx,"f1.len.tot"] ){
                        cName <- c( "f1.num" ,"f1.len.tot" ,"f1.len.val" )
                        scrMtx[wIdx,cName] <- 1 + scrMtx[wIdx,cName]
                    }
                }
            }

        }

        #  의미 없는 부분들에 대한 데이터 삭제.
        if( TRUE ){
            # 의미 중복이 발생한 컬럼 데이터 제거
            for( cIdx in c("rem0.num","rem1.num","c0.num","c1.num","f0.num","f1.num") ){
                scrMtx[(1>=scrMtx[,cIdx]) ,cIdx] <- 0
            }

            # f1.len.tot 에서 2는 너무 흔하게 나온다.
            for( cIdx in c("f0.len.tot","f1.len.tot") ){
                scrMtx[(2==scrMtx[,cIdx]) ,cIdx] <- 0
            }

        }

        return( scrMtx )
    }

    bSMtxLst[["sScore01"]] <- function( phVP ,aZoidMtx ,makeInfoStr=F ){
        # phVP <- phVP.grp$phVPLst[[1]]
		aLen <- nrow(aZoidMtx)
        aObj <- phVP$getCodeW( aZoidMtx )

		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(bS.sScore1.cName) )	;colnames(scoreMtx) <- bS.sScore1.cName
        rownames(scoreMtx) <- aObj$miIdStr
		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c( "pvSubHLen" ,"pvSubName" )
			infoMtx <- matrix( "" ,nrow=aLen ,ncol=length(cName) )	;colnames(infoMtx) <- cName
			infoMtx[,"pvSubName"] <- aObj$miIdStr
            for( pvName in names(phVP$stdMILst) ){
                infoMtx[infoMtx[,"pvSubName"]==pvName ,"pvSubHLen"] <- phVP$stdMILst[[pvName]]$mtxLen
            }
		}
		if( 0==length(aObj$miIdStr) ){
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

        for( pvName in aObj$miNames ){  # pvName <- aObj$miNames[2]
            workArea <- which(aObj$miIdStr==pvName)
            stdMI<-phVP$stdMILst[[pvName]]
            wMI <- phVP$getCodeH( stdMI )
            scrMtx <- bSMtx.sScore1( stdMI=stdMI ,workArea ,wMI=wMI ,aObj ,aZoidMtx )

            scoreMtx[workArea,] <- scrMtx
        }

        return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
    }

}

if( TRUE ){ # "sScore02"

	bS.sScore2.cName <- c(	 "rebC.r","rebC.c","rebC.f","rebC2.r","rebC2.c","rebC2.f"   # 컬럼 값 유지
                            ,"inc.r","inc.c","inc.f","inc.r2","inc.c2","inc.f2"         # 증가량 유지.
	)

    bSMtx.sScore2 <- function( stdMI ,workArea ,wMI ,aObj ,aZoidMtx ){

		scrMtx <- matrix( 0, nrow=length(workArea), ncol=length(bS.sScore2.cName) )	;colnames(scrMtx) <- bS.sScore2.cName
        rownames(scrMtx) <- workArea
        stdMILen <- nrow(wMI$rawTail)
        if( 0==stdMILen ){
            return( scrMtx )
        }

        inc.raw <- NULL     ;inc.cStep <-NULL   ;inc.fStep <-NULL
        inc.raw2<-NULL      ;inc.cStep2<-NULL   ;inc.fStep2<-NULL
        if( 2<stdMILen ){
            inc.raw     <- wMI$rawTail[stdMILen,] + (wMI$rawTail[stdMILen,]-wMI$rawTail[stdMILen-1,])
            inc.cStep   <- wMI$cStep[stdMILen,]   + (wMI$cStep[stdMILen,] - wMI$cStep[stdMILen-1,])
        }
        if( 3<stdMILen){
            inc.fStep   <- wMI$fStep[stdMILen,] + (wMI$fStep[stdMILen,] - wMI$fStep[stdMILen-1,])
        }

        if( 4<stdMILen){
            inc.raw2    <- wMI$rawTail[stdMILen-1,] + (wMI$rawTail[stdMILen-1,] - wMI$rawTail[stdMILen-3,])
            inc.cStep2  <- wMI$cStep[stdMILen-1,] + (wMI$cStep[stdMILen-1,] - wMI$cStep[stdMILen-3,])
        }
        if( 5<stdMILen){
            inc.fStep2  <- wMI$fStep[stdMILen-1,] + (wMI$fStep[stdMILen-1,] - wMI$fStep[stdMILen-3,])
        }


        for( wIdx in seq_len(length(workArea)) ){
            aIdx <- workArea[wIdx]
            aCode <- aObj$aZoidMtx[aIdx,]       ;aRem <- aCode%%10
            aCStep <- aObj$cStepMtx[aIdx,]      ;aFStep <- aObj$fStepMtx[aIdx,]

            scrMtx[wIdx,"rebC.r"] <- sum(aCode==wMI$rawTail[stdMILen,])
            scrMtx[wIdx,"rebC.c"] <- sum(aCStep==wMI$cStepTail[stdMILen,])
            if( 1<stdMILen ){
                scrMtx[wIdx,"rebC.f"] <- sum(aFStep==wMI$fStepTail[stdMILen,])

                scrMtx[wIdx,"rebC2.r"] <- sum(aCode==wMI$rawTail[stdMILen-1,])
                scrMtx[wIdx,"rebC2.c"] <- sum(aCStep==wMI$cStepTail[stdMILen-1,])
            }
            if( 2<stdMILen ){
                scrMtx[wIdx,"rebC2.f"] <- sum(aFStep==wMI$fStepTail[stdMILen-1,])

            }

            scrMtx[wIdx,"inc.r"] <- sum(inc.raw==aCode)
            scrMtx[wIdx,"inc.c"] <- sum(inc.cStep==aCStep)
            if( !is.null(inc.fStep) ){
                scrMtx[wIdx,"inc.f"] <- sum(inc.fStep==aFStep)
            }
            if( !is.null(inc.raw2) ){
                scrMtx[wIdx,"inc.r2"] <- sum(inc.raw2==aCode)
                scrMtx[wIdx,"inc.c2"] <- sum(inc.cStep2==aCStep)
            }
            if( !is.null(inc.fStep2) ){
                scrMtx[wIdx,"inc.f2"] <- sum(inc.fStep2==aFStep)
            }

            #   freqVal 옵션 적용에 따라 1무시 정책은 폐기.
            # remFlag <- scrMtx[wIdx,]==1
            # scrMtx[wIdx,remFlag] <- 0

            if( any(scrMtx[wIdx,]>=2) ){
                aQuo <- fCutU.getQuoObj( aZoidMtx[aIdx,] ,valSet=T )
                if( all(aQuo$size==stdMI$quoTail[stdMILen,]) ){
                    evtFlag <- scrMtx[wIdx,]>=2
                    scrMtx[wIdx,evtFlag] <- 1 + scrMtx[wIdx,evtFlag]
                }
            }

        }

        return( scrMtx )
    }

    bSMtxLst[["sScore02"]] <- function( phVP ,aZoidMtx ,makeInfoStr=F ){
        # phVP <- phVP.grp$phVPLst[[1]]
		aLen <- nrow(aZoidMtx)
        aObj <- phVP$getCodeW( aZoidMtx )

		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(bS.sScore2.cName) )	;colnames(scoreMtx) <- bS.sScore2.cName
        rownames(scoreMtx) <- aObj$miIdStr
		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c( "pvSubHLen" ,"pvSubName" )
			infoMtx <- matrix( "" ,nrow=aLen ,ncol=length(cName) )	;colnames(infoMtx) <- cName
			infoMtx[,"pvSubName"] <- aObj$miIdStr
            for( pvName in names(phVP$stdMILst) ){
                infoMtx[infoMtx[,"pvSubName"]==pvName ,"pvSubHLen"] <- phVP$stdMILst[[pvName]]$mtxLen
            }
		}
		if( 0==length(aObj$miIdStr) ){
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

        for( pvName in aObj$miNames ){  # pvName <- aObj$miNames[2]
            workArea <- which(aObj$miIdStr==pvName)
            stdMI<-phVP$stdMILst[[pvName]]
            wMI <- phVP$getCodeH( stdMI )
            scrMtx <- bSMtx.sScore2( stdMI=stdMI ,workArea ,wMI=wMI ,aObj ,aZoidMtx )
            scoreMtx[workArea,] <- scrMtx
        }

        return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
    }

}

if( TRUE ){ # "sScore03"

	bS.sScore3.cName <- c( "rebPtn.1","rebPtn.n","snR3" ,"snMax.r","snFCnt.r" ,"snMax.c","snFCnt.c" ,"snMax.f","snFCnt.f" )

    bSMtx.sScore3 <- function( stdMI ,workArea ,wMI ,aObj ,aZoidMtx ){
		scrMtx <- matrix( 0, nrow=length(workArea), ncol=length(bS.sScore3.cName) )	;colnames(scrMtx) <- bS.sScore3.cName
        rownames(scrMtx) <- workArea
        stdMILen <- nrow(wMI$rawTail)
        if( 0==stdMILen ){
            return( scrMtx )
        }

        # preWork -----------------------------------------------------------------
        getRebPtn.1 <- function( stdMI ){
            rObj <- list( matInfo=matrix(0,nrow=0,ncol=4) )
            rowLen <- nrow( stdMI$rawTail )
            if( 2>rowLen ) return( rObj )

            matLst <- list()
            for( rIdx in rowLen:2 ){
                cVal <- intersect(stdMI$rawTail[rIdx,] ,stdMI$rawTail[rIdx-1,])
                if( 1!=length(cVal) ) next

                matLst[[1+length(matLst)]] <- c( rIdx, cVal
                                                    , which(stdMI$rawTail[rIdx-1,]==cVal) 
                                                    , which(stdMI$rawTail[rIdx  ,]==cVal)
                                                )
            }

            if( 0<length(matLst) ){
                matInfo <- do.call( rbind ,matLst )
                colnames( matInfo ) <- c("row","val","fromC","toC")
                rObj$matInfo <- matInfo
            }
            return( rObj )
        } # getRebPtn.1()
        getRebPtn.n <- function( stdMI ){
            rObj <- list( )
            rowLen <- nrow( stdMI$rawTail )
            if( 2>rowLen ) return( rObj )

            matLst <- list()	;matInfo <- NULL
            for( rIdx in rowLen:2 ){
                cVal <- intersect(stdMI$rawTail[rIdx,] ,stdMI$rawTail[rIdx-1,])
                if( 2>length(cVal) ) next

                matMtx <- matrix( NA, nrow=2, ncol=length(cVal) )
                rownames(matMtx) <- c("from","to")	;colnames(matMtx) <- paste("val",cVal)
                for( idx in seq_len(length(cVal)) ){
                    val <- cVal[idx]
                    matMtx["from",idx] <- which(stdMI$rawTail[rIdx-1,]==val)
                    matMtx["to"  ,idx] <- which(stdMI$rawTail[rIdx  ,]==val)
                }
                matInfo <- c( matInfo ,sprintf("%d:%s",rIdx,paste(cVal,collapse=",")) )
                matLst[[1+length(matLst)]] <- matMtx
            }
            names(matLst) <- matInfo

            return( matLst )
        } # getRebPtn.n()
        getSeqPtn <- function( mtx ){
            rObj <- list( )
            rowLen <- ifelse( is.null(mtx) ,0 ,nrow( mtx ) )
            colLen <- ifelse( is.null(mtx) ,0 ,ncol( mtx ) )
            if( 2>rowLen ){
                rObj$filt <- function( aCode ){ return( list( matCnt=0 ) ) }
                return( rObj )
            }

            banLst <- list()
            for( cIdx in 1:colLen ){	# lastCode
                lc <- mtx[rowLen,cIdx]
                fColIdx <- integer(0)
                fRowIdx <- integer(0)
                dbgStr <- ""
                for( rIdx in (rowLen-1):1 ){
                    fColIdx <- which(mtx[rIdx,]==lc)
                    if( 0<length(fColIdx) ){
                        fRowIdx <- rIdx
                        # dbgStr <- sprintf("col:%d(val:%d)  found in row:%d col:%s",cIdx,lc,fRowIdx,paste(fColIdx,collapse=","))
                        break
                    }
                }

                dbgStr <- ""
                for( fcIdx in fColIdx ){
                    olSpan <- fCutU.overlapSpan( colLen ,colIdx.pre=fcIdx ,colIdx.post=cIdx )
                    if( 1>sum(olSpan$info[c("lMargin","rMargin")]) )	next

                    # valInc <- mtx[fRowIdx+1,olSpan$span.pre]-mtx[fRowIdx,olSpan$span.pre] <- bug?
                    valInc <- mtx[rowLen,olSpan$span.post]-mtx[fRowIdx,olSpan$span.pre]
                    banVal <- mtx[rowLen,olSpan$span.post]+valInc
                    fixPoint <- banVal	;fixPoint[-(olSpan$info["lMargin"]+1)] <- NA
                    dbgStr <- sprintf("colIdx:%d(val:%d) from (%d,%d)  %s/%s --> %s/%s..?",cIdx,lc,fRowIdx,fcIdx
                                        ,paste(mtx[fRowIdx  ,olSpan$span.pre],collapse=",")
                                        ,paste(mtx[rowLen	,olSpan$span.post],collapse=",")
                                        ,paste(mtx[rowLen	,olSpan$span.post],collapse=",")
                                        ,paste(banVal,collapse=",")
                                    )
                    banObj <- list( banVal=banVal ,banSpan=olSpan$span.post ,fixPoint=fixPoint ,dbgStr=dbgStr )

                    fixIdx <- which( !is.na(fixPoint) )
                    banObj$info <- c( length(banVal) ,fixIdx )
                    names(banObj$info) <- c("len","fixIdx")

                    banLst[[1+length(banLst)]] <- banObj
                }
                
            }

            rObj$banLst <- banLst

            rObj$filt <- function( aCode ){
                rstObj <- list( matCnt=0 )
                if( 0==length(rObj$banLst) ) return( rstObj )

                matCnt <- sapply( rObj$banLst ,function( banInfo ){
                    cnt <- sum( aCode[banInfo$banSpan] == banInfo$banVal )
                    flagFixPoint <- all(aCode[banInfo$banSpan]==banInfo$fixPoint,na.rm=T)
                    if( flagFixPoint ){
                        return( cnt )
                    } else {
                        return( 0 )
                    }
                })

                rstObj$matCnt = matCnt
                return( rstObj )
            }

            return( rObj )
        } # getSeqPtn()

        rObj <- list( 	 lastZoid=wMI$lastRaw    # bFMtx.score3() 과 최대한 유사 코드 생성하기 위함.
                        ,rebPtn.1=getRebPtn.1(wMI)	,rebPtn.n=getRebPtn.n( wMI )
                        ,seqNextPtn.raw=getSeqPtn( wMI$rawTail )
                        ,seqNextPtn.cStep=getSeqPtn( wMI$cStepTail )
                        ,seqNextPtn.fStep=getSeqPtn( wMI$fStepTail )
        )

        for( wIdx in seq_len(length(workArea)) ){
            aIdx <- workArea[wIdx]
            aCode <- aObj$aZoidMtx[aIdx,]       ;aRem <- aCode%%10
            aCStep <- aObj$cStepMtx[aIdx,]      ;aFStep <- aObj$fStepMtx[aIdx,]

			# rebPtn.1
			infoStr.rebPtn.1 <- ""
			if( 0<nrow(rObj$rebPtn.1$matInfo) ){
				reb.lastZoid <- rObj$lastZoid[rObj$rebPtn.1$matInfo[,"fromC"]]
				reb.aZoid <- aCode[rObj$rebPtn.1$matInfo[,"toC"]]
				scrMtx[wIdx,"rebPtn.1"] <- sum( reb.aZoid==reb.lastZoid )
                if( 1==scrMtx[wIdx,"rebPtn.1"] ) scrMtx[wIdx,"rebPtn.1"] <- 0

				if( 0<scrMtx[wIdx,"rebPtn.1"] ){
					fromCol <- rObj$rebPtn.1$matInfo[,"fromC"][reb.aZoid==reb.lastZoid]
					toCol <- rObj$rebPtn.1$matInfo[,"toC"][reb.aZoid==reb.lastZoid]
					infoStr.rebPtn.1 <- sprintf("reb col: (%s)->(%s)",paste(fromCol,collapse=","),paste(toCol,collapse=","))
				}
			}

			# rebPtn.n
			infoStr.rebPtn.n <- ""
			if( length(rObj$rebPtn.n)>0 ){
				flag <- sapply( rObj$rebPtn.n ,function( matMtx ){
								fromVal <- rObj$lastZoid[matMtx["from",]]
								toVal <- aCode[matMtx["to",]]
								return( all(fromVal==toVal) )
							})
				scrMtx[wIdx,"rebPtn.n"] <- sum( flag )
				if( 0<scrMtx[wIdx,"rebPtn.n"] ){
					infoStr.rebPtn.n <- sprintf("%s",paste(names(flag)[flag],collapse=" "))
				}
			}

			#	"snR3"	- 재발값을 기준으로 증감이 동일하게 반복되는 것이 3개 이상.
			banLst <- rObj$seqNextPtn.raw$banLst
			for( idx in seq_len(length(banLst)) ){
				if( 3>banLst[[idx]]$info["len"] ) next

				flag <- fCutU.hasPtn( banLst[[idx]]$banVal, aCode ,thld=3 ,fixIdx=banLst[[idx]]$info["fixIdx"] )
				if( flag ){
					scrMtx[wIdx,"snR3"] <- 1 + scrMtx[wIdx,"snR3"]
					# break             # bSMtx에서는 이미 갯수가 적으니까 속도 문제 걱정은 필요 없겠지.
				}
			}

			#	"sncMax.raw" ,"sncFCnt.raw" 
			snMatCnt.raw <- rObj$seqNextPtn.raw$filt( aCode )$matCnt
			scrMtx[wIdx,"snMax.r"] <- max( snMatCnt.raw )
			if( 1==scrMtx[wIdx,"snMax.r"] ) scrMtx[wIdx,"snMax.r"] <- 0
			scrMtx[wIdx,"snFCnt.r"] <- sum( snMatCnt.raw>=2 )
            if( 1==scrMtx[wIdx,"snFCnt.r"] ) scrMtx[wIdx,"snFCnt.r"] <- 0

			#	"sncMax.cStep" ,"sncFCnt.cStep"
			snMatCnt.cStep <- rObj$seqNextPtn.cStep$filt( aCStep )$matCnt
			scrMtx[wIdx,"snMax.c"] <- max( snMatCnt.cStep )
			if( 1==scrMtx[wIdx,"snMax.c"] ) scrMtx[wIdx,"snMax.c"] <- 0
			scrMtx[wIdx,"snFCnt.c"] <- sum( snMatCnt.cStep>=2 )
            if( 1==scrMtx[wIdx,"snFCnt.c"] ) scrMtx[wIdx,"snFCnt.c"] <- 0

			#	"sncMax.fStep" ,"sncFCnt.fStep"
			snMatCnt.fStep <- rObj$seqNextPtn.fStep$filt( aFStep )$matCnt
			scrMtx[wIdx,"snMax.f"] <- max( snMatCnt.fStep )
			if( 1==scrMtx[wIdx,"snMax.f"] ) scrMtx[wIdx,"snMax.f"] <- 0
			scrMtx[wIdx,"snFCnt.f"] <- sum( snMatCnt.fStep>=2 )
            if( 1==scrMtx[wIdx,"snFCnt.f"] ) scrMtx[wIdx,"snFCnt.f"] <- 0

            # scrMtx[wIdx,"rebC.r"] <- sum(aCode==wMI$rawTail[stdMILen,])
            # scrMtx[wIdx,"rebC.c"] <- sum(aCStep==wMI$cStepTail[stdMILen,])
        }

        return( scrMtx )
    }

    bSMtxLst[["sScore03"]] <- function( phVP ,aZoidMtx ,makeInfoStr=F ){
        # phVP <- phVP.grp$phVPLst[[1]]
		aLen <- nrow(aZoidMtx)
        aObj <- phVP$getCodeW( aZoidMtx )

		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(bS.sScore3.cName) )	;colnames(scoreMtx) <- bS.sScore3.cName
        rownames(scoreMtx) <- aObj$miIdStr
		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c( "pvSubHLen" ,"pvSubName" )
			infoMtx <- matrix( "" ,nrow=aLen ,ncol=length(cName) )	;colnames(infoMtx) <- cName
			infoMtx[,"pvSubName"] <- aObj$miIdStr
            for( pvName in names(phVP$stdMILst) ){
                infoMtx[infoMtx[,"pvSubName"]==pvName ,"pvSubHLen"] <- phVP$stdMILst[[pvName]]$mtxLen
            }
		}
		if( 0==length(aObj$miIdStr) ){
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

        for( pvName in aObj$miNames ){  # pvName <- aObj$miNames[2]
            workArea <- which(aObj$miIdStr==pvName)
            stdMI<-phVP$stdMILst[[pvName]]
            wMI <- phVP$getCodeH( stdMI )
            scrMtx <- bSMtx.sScore3( stdMI=stdMI ,workArea ,wMI=wMI ,aObj ,aZoidMtx )
            scoreMtx[workArea,] <- scrMtx
        }

        return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
    }

}

if( TRUE ){ # "sScore04"

	bS.sScore04.cName <- c( "pBanN.r","pBanN.n"	    # found num of rebound ptn ( ptn itself, next ptn in right column )
                            ,"pLCol" ,"pE3" ,"pE4"	,"pMH" ,"pfNum"
                            ,"iBanN"				# found num of inc.ptn
                            ,"iLCol" ,"iE3" ,"iE4"	,"iMH" ,"ifNum"	#  rebLastCol     extMat3     extMat4 multiHpn.TF    foundNum 
                            ,"FVa.m","FVa.c"	    # FVa.max FVa.hpnCnt
                            ,"m4"								# match4
    )

    bSMtx.sScore04 <- function( stdMI ,workArea ,wMI ,aObj ,aZoidMtx ){
		scrMtx <- matrix( 0, nrow=length(workArea), ncol=length(bS.sScore04.cName) )	;colnames(scrMtx) <- bS.sScore04.cName
        rownames(scrMtx) <- workArea
        stdMILen <- nrow(wMI$rawTail)
        if( 2>stdMILen ){
            return( scrMtx )
        }

        rObj <- list( fInfo=fCutU.getFiltObjPair(wMI$rawTail) )   # rObj$fInfo$explain( )


        for( wIdx in seq_len(length(workArea)) ){
            aIdx <- workArea[wIdx]
            aCode <- aObj$aZoidMtx[aIdx,]       ;aRem <- aCode%%10
            aCStep <- aObj$cStepMtx[aIdx,]      ;aFStep <- aObj$fStepMtx[aIdx,]

            rstObj <- bFMtx.util.fMtxObj.score4567( aCode ,rObj$fInfo ,makeInfoStr=F )

			# pairBanLst
			pBan.cutInfo <- rstObj$F_pBanLst( makeInfoStr=F )
			scrMtx[wIdx ,"pBanN.r"] <- pBan.cutInfo$cutHpn["rebPtn"]
			scrMtx[wIdx ,"pBanN.n"] <- pBan.cutInfo$cutHpn["nextPtn"]
			workCol <- c("pLCol" ,"pE3" ,"pE4"	,"pMH" ,"pfNum")
			scrMtx[wIdx ,workCol] <- pBan.cutInfo$cutHpn[c("rebLastCol","extMat3","extMat4","multiHpn","foundNum")]

			# iBanLst
			iBan.cutInfo <- rstObj$F_iBanLst( makeInfoStr=F )
			scrMtx[wIdx ,"iBanN"] <- length(rstObj$iBanLst)
			workCol <- c("iLCol" ,"iE3" ,"iE4"	,"iMH" ,"ifNum")
			scrMtx[wIdx ,workCol] <- iBan.cutInfo$cutHpn[c("rebLastCol","extMat3","extMat4","multiHpn","foundNum")]

			# pairHpn
			if( 0<length(rstObj$pairHpn) ){
				workCol <- c("FVa.m","FVa.c")
				scrMtx[wIdx ,workCol ] <- rstObj$pairHpn$foundInfo[c("FVa.max","FVa.hpnCnt")]

                if( 0<scrMtx[wIdx ,"FVa.m"] && 1==scrMtx[wIdx ,"FVa.c"] ){
                    scrMtx[wIdx ,"FVa.c"] <- 0
                }
				if( 1==scrMtx[wIdx ,"FVa.m" ] )	scrMtx[wIdx ,"FVa.m" ] <- 0   # "FVa.m","aFV.m" 에서 1은 너무 흔한 듯.
			}

			# match4
			if( 0<length(rstObj$match4) ){
				scrMtx[wIdx, "m4"] <- rstObj$match4$foundInfo["matCnt"]
			}

            # scrMtx[wIdx,"rebC.r"] <- sum(aCode==wMI$rawTail[stdMILen,])
            # scrMtx[wIdx,"rebC.c"] <- sum(aCStep==wMI$cStepTail[stdMILen,])
        }

        return( scrMtx )

    }

    bSMtxLst[["sScore04"]] <- function( phVP ,aZoidMtx ,makeInfoStr=F ){
        # phVP <- phVP.grp$phVPLst[[1]]
		aLen <- nrow(aZoidMtx)
        aObj <- phVP$getCodeW( aZoidMtx )

		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(bS.sScore04.cName) )	;colnames(scoreMtx) <- bS.sScore04.cName
        rownames(scoreMtx) <- aObj$miIdStr
		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c( "pvSubHLen" ,"pvSubName" )
			infoMtx <- matrix( "" ,nrow=aLen ,ncol=length(cName) )	;colnames(infoMtx) <- cName
			infoMtx[,"pvSubName"] <- aObj$miIdStr
            for( pvName in names(phVP$stdMILst) ){
                infoMtx[infoMtx[,"pvSubName"]==pvName ,"pvSubHLen"] <- phVP$stdMILst[[pvName]]$mtxLen
            }
		}
		if( 0==length(aObj$miIdStr) ){
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

        for( pvName in aObj$miNames ){  # pvName <- aObj$miNames[2]
            workArea <- which(aObj$miIdStr==pvName)
            stdMI<-phVP$stdMILst[[pvName]]
            wMI <- phVP$getCodeH( stdMI )
            scrMtx <- bSMtx.sScore04( stdMI=stdMI ,workArea ,wMI=wMI ,aObj ,aZoidMtx )
            scoreMtx[workArea,] <- scrMtx
        }

        return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
    }

}

if( TRUE ){ # "sScore05"

	bS.sScore05.cName <- c( "pBanN.r","pBanN.n"	    # found num of rebound ptn ( ptn itself, next ptn in right column )
                            ,"pLCol" ,"pE3" ,"pE4"	,"pMH" ,"pfNum"
                            ,"iBanN"				# found num of inc.ptn
                            ,"iLCol" ,"iE3" ,"iE4"	,"iMH" ,"ifNum"	#  rebLastCol     extMat3     extMat4 multiHpn.TF    foundNum 
                            ,"FVa.m","FVa.c"	    # FVa.max FVa.hpnCnt
                            ,"m4"								# match4
    )

    bSMtx.sScore05 <- function( stdMI ,workArea ,wMI ,aObj ,aZoidMtx ){
		scrMtx <- matrix( 0, nrow=length(workArea), ncol=length(bS.sScore05.cName) )	;colnames(scrMtx) <- bS.sScore05.cName
        rownames(scrMtx) <- workArea
        stdMILen <- nrow(wMI$rawTail)
        if( 2>stdMILen ){
            return( scrMtx )
        }

        # rObj$fInfo <- fCutU.getFiltObjPair( wMI$rawTail %% 10 )	# rObj$fInfo$explain( )
        rObj <- list( fInfo=fCutU.getFiltObjPair( wMI$rawTail%%10 ) )   # rObj$fInfo$explain( )

        for( wIdx in seq_len(length(workArea)) ){
            aIdx <- workArea[wIdx]
            aCode <- aObj$aZoidMtx[aIdx,]       ;aRem <- aCode%%10
            aCStep <- aObj$cStepMtx[aIdx,]      ;aFStep <- aObj$fStepMtx[aIdx,]

            rstObj <- bFMtx.util.fMtxObj.score4567( aRem ,rObj$fInfo ,makeInfoStr=F )

			# pairBanLst
			pBan.cutInfo <- rstObj$F_pBanLst( makeInfoStr=F )
			scrMtx[wIdx ,"pBanN.r"] <- pBan.cutInfo$cutHpn["rebPtn"]
			scrMtx[wIdx ,"pBanN.n"] <- pBan.cutInfo$cutHpn["nextPtn"]
			workCol <- c("pLCol" ,"pE3" ,"pE4"	,"pMH" ,"pfNum")
			scrMtx[wIdx ,workCol] <- pBan.cutInfo$cutHpn[c("rebLastCol","extMat3","extMat4","multiHpn","foundNum")]

			# iBanLst
			iBan.cutInfo <- rstObj$F_iBanLst( makeInfoStr=F )
			scrMtx[wIdx ,"iBanN"] <- length(rstObj$iBanLst)
			workCol <- c("iLCol" ,"iE3" ,"iE4"	,"iMH" ,"ifNum")
			scrMtx[wIdx ,workCol] <- iBan.cutInfo$cutHpn[c("rebLastCol","extMat3","extMat4","multiHpn","foundNum")]

			# pairHpn
			if( 0<length(rstObj$pairHpn) ){
				workCol <- c("FVa.m","FVa.c")
				scrMtx[wIdx ,workCol ] <- rstObj$pairHpn$foundInfo[c("FVa.max","FVa.hpnCnt")]

                if( 0<scrMtx[wIdx ,"FVa.m"] && 1==scrMtx[wIdx ,"FVa.c"] ){
                    scrMtx[wIdx ,"FVa.c"] <- 0
                }
				if( 1==scrMtx[wIdx ,"FVa.m" ] )	scrMtx[wIdx ,"FVa.m" ] <- 0   # "FVa.m","aFV.m" 에서 1은 너무 흔한 듯.
			}

			# match4
			if( 0<length(rstObj$match4) ){
				scrMtx[wIdx, "m4"] <- rstObj$match4$foundInfo["matCnt"]
			}

            # scrMtx[wIdx,"rebC.r"] <- sum(aCode==wMI$rawTail[stdMILen,])
            # scrMtx[wIdx,"rebC.c"] <- sum(aCStep==wMI$cStepTail[stdMILen,])
        }

        return( scrMtx )

    }

    bSMtxLst[["sScore05"]] <- function( phVP ,aZoidMtx ,makeInfoStr=F ){
        # phVP <- phVP.grp$phVPLst[[1]]
		aLen <- nrow(aZoidMtx)
        aObj <- phVP$getCodeW( aZoidMtx )

		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(bS.sScore05.cName) )	;colnames(scoreMtx) <- bS.sScore05.cName
        rownames(scoreMtx) <- aObj$miIdStr
		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c( "pvSubHLen" ,"pvSubName" )
			infoMtx <- matrix( "" ,nrow=aLen ,ncol=length(cName) )	;colnames(infoMtx) <- cName
			infoMtx[,"pvSubName"] <- aObj$miIdStr
            for( pvName in names(phVP$stdMILst) ){
                infoMtx[infoMtx[,"pvSubName"]==pvName ,"pvSubHLen"] <- phVP$stdMILst[[pvName]]$mtxLen
            }
		}
		if( 0==length(aObj$miIdStr) ){
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

        for( pvName in aObj$miNames ){  # pvName <- aObj$miNames[2]
            workArea <- which(aObj$miIdStr==pvName)
            stdMI<-phVP$stdMILst[[pvName]]
            wMI <- phVP$getCodeH( stdMI )
            scrMtx <- bSMtx.sScore05( stdMI=stdMI ,workArea ,wMI=wMI ,aObj ,aZoidMtx )
            scoreMtx[workArea,] <- scrMtx
        }

        return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
    }

}

if( TRUE ){ # "sScore06"

	bS.sScore06.cName <- c( "pBanN.r","pBanN.n"	    # found num of rebound ptn ( ptn itself, next ptn in right column )
                            ,"pLCol" ,"pE3" ,"pE4"	,"pMH" ,"pfNum"
                            ,"iBanN"				# found num of inc.ptn
                            ,"iLCol" ,"iE3" ,"iE4"	,"iMH" ,"ifNum"	#  rebLastCol     extMat3     extMat4 multiHpn.TF    foundNum 
                            ,"FVa.m","FVa.c"	    # FVa.max FVa.hpnCnt
                            ,"m4"								# match4
    )

    bSMtx.sScore06 <- function( stdMI ,workArea ,wMI ,aObj ,aZoidMtx ){
		scrMtx <- matrix( 0, nrow=length(workArea), ncol=length(bS.sScore06.cName) )	;colnames(scrMtx) <- bS.sScore06.cName
        rownames(scrMtx) <- workArea
        stdMILen <- nrow(wMI$rawTail)
        if( 2>stdMILen ){
            return( scrMtx )
        }

        # rObj$fInfo <- fCutU.getFiltObjPair( wMI$cStepTail )	# rObj$fInfo$explain( )
        rObj <- list( fInfo=fCutU.getFiltObjPair( wMI$cStepTail ) )   # rObj$fInfo$explain( )


        for( wIdx in seq_len(length(workArea)) ){
            aIdx <- workArea[wIdx]
            aCode <- aObj$aZoidMtx[aIdx,]       ;aRem <- aCode%%10
            aCStep <- aObj$cStepMtx[aIdx,]      ;aFStep <- aObj$fStepMtx[aIdx,]

            rstObj <- bFMtx.util.fMtxObj.score4567( aCStep ,rObj$fInfo ,makeInfoStr=F )

			# pairBanLst
			pBan.cutInfo <- rstObj$F_pBanLst( makeInfoStr=F )
			scrMtx[wIdx ,"pBanN.r"] <- pBan.cutInfo$cutHpn["rebPtn"]
			scrMtx[wIdx ,"pBanN.n"] <- pBan.cutInfo$cutHpn["nextPtn"]
			workCol <- c("pLCol" ,"pE3" ,"pE4"	,"pMH" ,"pfNum")
			scrMtx[wIdx ,workCol] <- pBan.cutInfo$cutHpn[c("rebLastCol","extMat3","extMat4","multiHpn","foundNum")]

			# iBanLst
			iBan.cutInfo <- rstObj$F_iBanLst( makeInfoStr=F )
			scrMtx[wIdx ,"iBanN"] <- length(rstObj$iBanLst)
			workCol <- c("iLCol" ,"iE3" ,"iE4"	,"iMH" ,"ifNum")
			scrMtx[wIdx ,workCol] <- iBan.cutInfo$cutHpn[c("rebLastCol","extMat3","extMat4","multiHpn","foundNum")]

			# pairHpn
			if( 0<length(rstObj$pairHpn) ){
				workCol <- c("FVa.m","FVa.c")
				scrMtx[wIdx ,workCol ] <- rstObj$pairHpn$foundInfo[c("FVa.max","FVa.hpnCnt")]

                if( 0<scrMtx[wIdx ,"FVa.m"] && 1==scrMtx[wIdx ,"FVa.c"] ){
                    scrMtx[wIdx ,"FVa.c"] <- 0
                }
				if( 1==scrMtx[wIdx ,"FVa.m" ] )	scrMtx[wIdx ,"FVa.m" ] <- 0   # "FVa.m","aFV.m" 에서 1은 너무 흔한 듯.
			}

			# match4
			if( 0<length(rstObj$match4) ){
				scrMtx[wIdx, "m4"] <- rstObj$match4$foundInfo["matCnt"]
			}

            # scrMtx[wIdx,"rebC.r"] <- sum(aCode==wMI$rawTail[stdMILen,])
            # scrMtx[wIdx,"rebC.c"] <- sum(aCStep==wMI$cStepTail[stdMILen,])
        }

        return( scrMtx )

    }

    bSMtxLst[["sScore06"]] <- function( phVP ,aZoidMtx ,makeInfoStr=F ){
        # phVP <- phVP.grp$phVPLst[[1]]
		aLen <- nrow(aZoidMtx)
        aObj <- phVP$getCodeW( aZoidMtx )

		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(bS.sScore06.cName) )	;colnames(scoreMtx) <- bS.sScore06.cName
        rownames(scoreMtx) <- aObj$miIdStr
		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c( "pvSubHLen" ,"pvSubName" )
			infoMtx <- matrix( "" ,nrow=aLen ,ncol=length(cName) )	;colnames(infoMtx) <- cName
			infoMtx[,"pvSubName"] <- aObj$miIdStr
            for( pvName in names(phVP$stdMILst) ){
                infoMtx[infoMtx[,"pvSubName"]==pvName ,"pvSubHLen"] <- phVP$stdMILst[[pvName]]$mtxLen
            }
		}
		if( 0==length(aObj$miIdStr) ){
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

        for( pvName in aObj$miNames ){  # pvName <- aObj$miNames[2]
            workArea <- which(aObj$miIdStr==pvName)
            stdMI<-phVP$stdMILst[[pvName]]
            wMI <- phVP$getCodeH( stdMI )
            scrMtx <- bSMtx.sScore06( stdMI=stdMI ,workArea ,wMI=wMI ,aObj ,aZoidMtx )
            scoreMtx[workArea,] <- scrMtx
        }

        return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
    }

}

if( TRUE ){ # "sScore07"

	bS.sScore07.cName <- c( "pBanN.r","pBanN.n"	    # found num of rebound ptn ( ptn itself, next ptn in right column )
                            ,"pLCol" ,"pE3" ,"pE4"	,"pMH" ,"pfNum"
                            ,"iBanN"				# found num of inc.ptn
                            ,"iLCol" ,"iE3" ,"iE4"	,"iMH" ,"ifNum"	#  rebLastCol     extMat3     extMat4 multiHpn.TF    foundNum 
                            ,"FVa.m","FVa.c"	    # FVa.max FVa.hpnCnt
                            ,"m4"								# match4
    )

    bSMtx.sScore07 <- function( stdMI ,workArea ,wMI ,aObj ,aZoidMtx ){
		scrMtx <- matrix( 0, nrow=length(workArea), ncol=length(bS.sScore07.cName) )	;colnames(scrMtx) <- bS.sScore07.cName
        rownames(scrMtx) <- workArea
        stdMILen <- nrow(wMI$rawTail)
        if( 3>stdMILen ){
            return( scrMtx )
        }

        # rObj$fInfo <- fCutU.getFiltObjPair( wMI$fStepTail )	# rObj$fInfo$explain( )
        rObj <- list( fInfo=fCutU.getFiltObjPair( wMI$fStepTail ) )   # rObj$fInfo$explain( )


        for( wIdx in seq_len(length(workArea)) ){
            aIdx <- workArea[wIdx]
            aCode <- aObj$aZoidMtx[aIdx,]       ;aRem <- aCode%%10
            aCStep <- aObj$cStepMtx[aIdx,]      ;aFStep <- aObj$fStepMtx[aIdx,]

            rstObj <- bFMtx.util.fMtxObj.score4567( aFStep ,rObj$fInfo ,makeInfoStr=F )

			# pairBanLst
			pBan.cutInfo <- rstObj$F_pBanLst( makeInfoStr=F )
			scrMtx[wIdx ,"pBanN.r"] <- pBan.cutInfo$cutHpn["rebPtn"]
			scrMtx[wIdx ,"pBanN.n"] <- pBan.cutInfo$cutHpn["nextPtn"]
			workCol <- c("pLCol" ,"pE3" ,"pE4"	,"pMH" ,"pfNum")
			scrMtx[wIdx ,workCol] <- pBan.cutInfo$cutHpn[c("rebLastCol","extMat3","extMat4","multiHpn","foundNum")]

			# iBanLst
			iBan.cutInfo <- rstObj$F_iBanLst( makeInfoStr=F )
			scrMtx[wIdx ,"iBanN"] <- length(rstObj$iBanLst)
			workCol <- c("iLCol" ,"iE3" ,"iE4"	,"iMH" ,"ifNum")
			scrMtx[wIdx ,workCol] <- iBan.cutInfo$cutHpn[c("rebLastCol","extMat3","extMat4","multiHpn","foundNum")]

			# pairHpn
			if( 0<length(rstObj$pairHpn) ){
				workCol <- c("FVa.m","FVa.c")
				scrMtx[wIdx ,workCol ] <- rstObj$pairHpn$foundInfo[c("FVa.max","FVa.hpnCnt")]

                if( 0<scrMtx[wIdx ,"FVa.m"] && 1==scrMtx[wIdx ,"FVa.c"] ){
                    scrMtx[wIdx ,"FVa.c"] <- 0
                }
				if( 1==scrMtx[wIdx ,"FVa.m" ] )	scrMtx[wIdx ,"FVa.m" ] <- 0   # "FVa.m","aFV.m" 에서 1은 너무 흔한 듯.
			}

			# match4
			if( 0<length(rstObj$match4) ){
				scrMtx[wIdx, "m4"] <- rstObj$match4$foundInfo["matCnt"]
			}

            # scrMtx[wIdx,"rebC.r"] <- sum(aCode==wMI$rawTail[stdMILen,])
            # scrMtx[wIdx,"rebC.c"] <- sum(aCStep==wMI$cStepTail[stdMILen,])
        }

        return( scrMtx )

    }

    bSMtxLst[["sScore07"]] <- function( phVP ,aZoidMtx ,makeInfoStr=F ){
        # phVP <- phVP.grp$phVPLst[[1]]
		aLen <- nrow(aZoidMtx)
        aObj <- phVP$getCodeW( aZoidMtx )

		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(bS.sScore07.cName) )	;colnames(scoreMtx) <- bS.sScore07.cName
        rownames(scoreMtx) <- aObj$miIdStr
		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c( "pvSubHLen" ,"pvSubName" )
			infoMtx <- matrix( "" ,nrow=aLen ,ncol=length(cName) )	;colnames(infoMtx) <- cName
			infoMtx[,"pvSubName"] <- aObj$miIdStr
            for( pvName in names(phVP$stdMILst) ){
                infoMtx[infoMtx[,"pvSubName"]==pvName ,"pvSubHLen"] <- phVP$stdMILst[[pvName]]$mtxLen
            }
		}
		if( 0==length(aObj$miIdStr) ){
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

        for( pvName in aObj$miNames ){  # pvName <- aObj$miNames[2]
            workArea <- which(aObj$miIdStr==pvName)
            stdMI<-phVP$stdMILst[[pvName]]
            wMI <- phVP$getCodeH( stdMI )
            scrMtx <- bSMtx.sScore07( stdMI=stdMI ,workArea ,wMI=wMI ,aObj ,aZoidMtx )
            scoreMtx[workArea,] <- scrMtx
        }

        return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
    }

}

if( TRUE ){ # "sScore08"

	bS.sScore08.cName <- c(
					 "max3","min3","max2MatCnt","min2MatCnt","minMax2MatCnt"
					,"cTbl","fTbl"
    )

    bSMtx.sScore08 <- function( stdMI ,workArea ,wMI ,aObj ,aZoidMtx ){
		scrMtx <- matrix( 0, nrow=length(workArea), ncol=length(bS.sScore08.cName) )	;colnames(scrMtx) <- bS.sScore08.cName
        rownames(scrMtx) <- workArea
        stdMILen <- nrow(wMI$rawTail)
        if( 0==stdMILen ){
            return( scrMtx )
        }

        rObj <- list( cInfo=NULL ,fTbl=NULL )
        if( TRUE ){
            cInfo <- list()
            wMI$cStep <- wMI$cStepTail[stdMILen,]
            cStep.srt <- sort(unique(wMI$cStep))
            cLen <- length(cStep.srt)
            if( 3<=cLen ){
                cInfo$max3 <- cStep.srt[cLen-0:2]	;cInfo$min3 <- cStep.srt[1:3]
            }
            if( 2<=cLen ){
                max2 <- cStep.srt[cLen-0:1]
                min2 <- cStep.srt[1:2]
                cInfo$max2MatFlag <- (wMI$cStep %in% max2)
                cInfo$min2MatFlag <- (wMI$cStep %in% min2)
                cInfo$cStep <- wMI$cStep
            }

            # 일단 보류. bSMtx는 phase에 따라 rawTail, cStepTail 컬럼 수가 다르다.
            # cInfo$mat3Lst <- list()
            # for( idx in 1:4 ){	# c3.x
            # 	logId <- sprintf("c3%d",idx)
            # 	cInfo$mat3Lst[[logId]] <- fCutU.getChkCStepValReb( zMtx[,0:2+idx,drop=F] )
            # }
            # cInfo$mat2Lst <- list()
            # for( idx in 1:5 ){	# c2.x
            # 	logId <- sprintf("c2%d",idx)
            # 	cInfo$mat2Lst[[logId]] <- fCutU.getChkCStepValReb( zMtx[,0:1+idx,drop=F] )
            # }

            cInfo$cTbl <- table(wMI$cStep)
            rObj$cInfo <- cInfo

            if( 1<nrow(wMI$rawTail) ){
                wMI$fStep <- wMI$fStepTail[nrow(wMI$fStepTail),]
                rObj$fTbl <- table(wMI$fStep)
            }
        }


        for( wIdx in seq_len(length(workArea)) ){
            aIdx <- workArea[wIdx]
            aCode <- aObj$aZoidMtx[aIdx,]       ;aRem <- aCode%%10
            aCStep <- aObj$cStepMtx[aIdx,]      ;aFStep <- aObj$fStepMtx[aIdx,]

			# minN, maxN
			aCStep.srt <- sort(unique(aCStep))
			cLen <- length(aCStep.srt)
			if( 3<=cLen ){
                if( !is.null(rObj$cInfo$max3) ){
                    scrMtx[wIdx ,"max3"] <- all( aCStep.srt[cLen-0:2]==rObj$cInfo$max3 )
                }
                if( !is.null(rObj$cInfo$min3) ){
                    scrMtx[wIdx ,"min3"] <- all( aCStep.srt[1:3]==rObj$cInfo$min3 )
                }
			}
			if( 2<=cLen ){
				matFlag <- ( aCStep==rObj$cInfo$cStep )
                if( !is.null(rObj$cInfo$max2MatFlag) ){
				    scrMtx[wIdx ,"max2MatCnt"] <- sum(matFlag[rObj$cInfo$max2MatFlag])
                }
                if( !is.null(rObj$cInfo$min2MatFlag) ){
				    scrMtx[wIdx ,"min2MatCnt"] <- sum(matFlag[rObj$cInfo$min2MatFlag])
                }
				scrMtx[wIdx ,"minMax2MatCnt"] <- scrMtx[wIdx ,"min2MatCnt"] + scrMtx[wIdx ,"max2MatCnt"]

				if( 2>scrMtx[wIdx ,"max2MatCnt"] ){	scrMtx[wIdx ,"max2MatCnt"] <- 0	}

				if( 2>scrMtx[wIdx ,"min2MatCnt"] ){	scrMtx[wIdx ,"min2MatCnt"] <- 0	}

				if( 3>scrMtx[wIdx ,"minMax2MatCnt"] ){	scrMtx[wIdx ,"minMax2MatCnt"] <- 0	}

			}

			cTbl <- table(aCStep)
			if( length(rObj$cInfo$cTbl)==length(cTbl) ){
				scrMtx[wIdx ,"cTbl"] <- all(names(cTbl)==names(rObj$cInfo$cTbl)) && all(cTbl==rObj$cInfo$cTbl)
			}

			if( !is.null(rObj$fTbl) ){
				fTbl <- table(aFStep)
				if( length(rObj$fTbl)==length(fTbl) ){
					scrMtx[wIdx ,"fTbl"] <- all(names(fTbl)==names(rObj$fTbl)) && all(fTbl==rObj$fTbl)
				}
			}

            # scrMtx[wIdx,"rebC.r"] <- sum(aCode==wMI$rawTail[stdMILen,])
            # scrMtx[wIdx,"rebC.c"] <- sum(aCStep==wMI$cStepTail[stdMILen,])
        }

        return( scrMtx )

    }

    bSMtxLst[["sScore08"]] <- function( phVP ,aZoidMtx ,makeInfoStr=F ){
        # phVP <- phVP.grp$phVPLst[[1]]
		aLen <- nrow(aZoidMtx)
        aObj <- phVP$getCodeW( aZoidMtx )

		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(bS.sScore08.cName) )	;colnames(scoreMtx) <- bS.sScore08.cName
        rownames(scoreMtx) <- aObj$miIdStr
		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c( "pvSubHLen" ,"pvSubName" )
			infoMtx <- matrix( "" ,nrow=aLen ,ncol=length(cName) )	;colnames(infoMtx) <- cName
			infoMtx[,"pvSubName"] <- aObj$miIdStr
            for( pvName in names(phVP$stdMILst) ){
                infoMtx[infoMtx[,"pvSubName"]==pvName ,"pvSubHLen"] <- phVP$stdMILst[[pvName]]$mtxLen
            }
		}
		if( 0==length(aObj$miIdStr) ){
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

        for( pvName in aObj$miNames ){  # pvName <- aObj$miNames[2]
            workArea <- which(aObj$miIdStr==pvName)
            stdMI<-phVP$stdMILst[[pvName]]
            wMI <- phVP$getCodeH( stdMI )
            scrMtx <- bSMtx.sScore08( stdMI=stdMI ,workArea ,wMI=wMI ,aObj ,aZoidMtx )
            scoreMtx[workArea,] <- scrMtx
        }

        return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
    }

}

if( TRUE ){ # "sScore09"

	bS.sScore09.cName <- c( "rCnt","rD2","rDn","rLr","rRl"	,"eCnt","eD2","eDn","eLr","eRl"
                            ,"cCnt","cD2","cDn","cLr","cRl"	,"fCnt","fD2","fDn","fLr","fRl"
    )

    bSMtx.sScore09 <- function( stdMI ,workArea ,wMI ,aObj ,aZoidMtx ){
		scrMtx <- matrix( 0, nrow=length(workArea), ncol=length(bS.sScore09.cName) )	;colnames(scrMtx) <- bS.sScore09.cName
        rownames(scrMtx) <- workArea
        stdMILen <- nrow(wMI$rawTail)
        if( 0==stdMILen ){
            return( scrMtx )
        }

        rObj <- list( rawBan=NULL ,remBan=NULL ,cBan=NULL ,fBan=NULL )
        if( 0<nrow(stdMI$rawTail) ){

            # rawBan ------------------------------------------
            df <- bUtil.zoidMtx_ana( wMI$rawTail ,banCfg="raw" )
            chkCol <- sort(unique(df[,"tgt.col"]))
            dfLst <- vector("list",6)
            for( cIdx in chkCol ){	dfLst[[cIdx]]<-df[df[,"tgt.col"]==cIdx ,] }
            rObj$rawBan <- list( chkCol=chkCol, dfLst=dfLst ,df=df )

            # remBan ------------------------------------------
            df <- bUtil.zoidMtx_ana( wMI$rawTail%%10 ,banCfg="rem" )
            chkCol <- sort(unique(df[,"tgt.col"]))
            dfLst <- vector("list",6)
            for( cIdx in chkCol ){	dfLst[[cIdx]]<-df[df[,"tgt.col"]==cIdx ,] }
            rObj$remBan <- list( chkCol=chkCol, dfLst=dfLst ,df=df )

            # cBan ------------------------------------------
            df <- bUtil.zoidMtx_ana( wMI$cStep ,banCfg="cStep" )
            chkCol <- sort(unique(df[,"tgt.col"]))
            dfLst <- vector("list",6)
            for( cIdx in chkCol ){	dfLst[[cIdx]]<-df[df[,"tgt.col"]==cIdx ,] }
            rObj$cBan <- list( chkCol=chkCol, dfLst=dfLst ,df=df )

            # fBan ------------------------------------------
            fStep <- wMI$fStep
            if( is.null(fStep) ){
                fStep <- matrix( 0 ,nrow=0 ,ncol=6 )
            } else if( is.na(fStep[1,1]) ){
                fStep <- fStep[-1, ,drop=F]
            }

            df <- bUtil.zoidMtx_ana( fStep ,banCfg="fStep" )
            chkCol <- sort(unique(df[,"tgt.col"]))
            dfLst <- vector("list",6)
            for( cIdx in chkCol ){	dfLst[[cIdx]]<-df[df[,"tgt.col"]==cIdx ,] }
            rObj$fBan <- list( chkCol=chkCol, dfLst=dfLst ,df=df )

        }
        rObj$checkBan <- function( srcVal ,banObj ){
            #	srcVal <- c( 8,23,32,33,34,40)	;banObj <- rObj$rawBan
            fLst <- list()
            for( cIdx in banObj$chkCol ){
                if( !(srcVal[cIdx] %in% banObj$dfLst[[cIdx]][,"banVal"]) ) next	# 처리 속도를 위해

                flag <- srcVal[cIdx] == banObj$dfLst[[cIdx]][,"banVal"]
                fObj <- list( fInfo=c(cIdx=cIdx ,val=srcVal[cIdx] ,dupLen=sum(flag))
                            ,typ=sort(as.character(banObj$dfLst[[cIdx]][flag,"tgt.dir"]))
                        )
                fLst[[sprintf("C%d",cIdx)]] <- fObj
            }

            bDupCnt <- sapply( fLst ,function(obj){obj$fInfo["dupLen"]})
            names(bDupCnt) <- names(fLst)

            typ <- do.call( c ,lapply(fLst,function(obj){obj$typ}))
            rFObj <- list( cnt=length(fLst) ,bDupCnt=bDupCnt ,typCnt=table(typ) )

            return( rFObj )
        } # rObj$checkBan( )

        for( wIdx in seq_len(length(workArea)) ){
            aIdx <- workArea[wIdx]
            aCode <- aObj$aZoidMtx[aIdx,]       ;aRem <- aCode%%10
            aCStep <- aObj$cStepMtx[aIdx,]      ;aFStep <- aObj$fStepMtx[aIdx,]

			banR <- rObj$checkBan( aCode ,rObj$rawBan )
			scrMtx[wIdx,"rCnt"] <- banR$cnt
			scrMtx[wIdx,"rD2"]	<- sum(banR$bDupCnt==2)
			scrMtx[wIdx,"rDn"]	<- sum(banR$bDupCnt >2)
			scrMtx[wIdx,"rLr"]	<- ifelse(is.na(banR$typCnt["Slide\\"]),0,banR$typCnt["Slide\\"])
			scrMtx[wIdx,"rRl"]	<- ifelse(is.na(banR$typCnt["Slide/"]),0,banR$typCnt["Slide/"])

			banE <- rObj$checkBan( aRem ,rObj$remBan )
			scrMtx[wIdx,"eCnt"] <- banE$cnt
			scrMtx[wIdx,"eD2"]	<- sum(banE$bDupCnt==2)
			scrMtx[wIdx,"eDn"]	<- sum(banE$bDupCnt >2)
			scrMtx[wIdx,"eLr"]	<- ifelse(is.na(banE$typCnt["Slide\\"]),0,banE$typCnt["Slide\\"])
			scrMtx[wIdx,"eRl"]	<- ifelse(is.na(banE$typCnt["Slide/"]),0,banE$typCnt["Slide/"])

			banC <- rObj$checkBan( aCStep ,rObj$cBan )
			scrMtx[wIdx,"cCnt"] <- banC$cnt
			scrMtx[wIdx,"cD2"]	<- sum(banC$bDupCnt==2)
			scrMtx[wIdx,"cDn"]	<- sum(banC$bDupCnt >2)
			scrMtx[wIdx,"cLr"]	<- ifelse(is.na(banC$typCnt["Slide\\"]),0,banC$typCnt["Slide\\"])
			scrMtx[wIdx,"cRl"]	<- ifelse(is.na(banC$typCnt["Slide/"]),0,banC$typCnt["Slide/"])

			banF <- rObj$checkBan( aFStep ,rObj$fBan )
			scrMtx[wIdx,"fCnt"] <- banF$cnt
			scrMtx[wIdx,"fD2"]	<- sum(banF$bDupCnt==2)
			scrMtx[wIdx,"fDn"]	<- sum(banF$bDupCnt >2)
			scrMtx[wIdx,"fLr"]	<- ifelse(is.na(banF$typCnt["Slide\\"]),0,banF$typCnt["Slide\\"])
			scrMtx[wIdx,"fRl"]	<- ifelse(is.na(banF$typCnt["Slide/"]),0,banF$typCnt["Slide/"])

            # scrMtx[wIdx,"rebC.r"] <- sum(aCode==wMI$rawTail[stdMILen,])
            # scrMtx[wIdx,"rebC.c"] <- sum(aCStep==wMI$cStepTail[stdMILen,])
        }

        return( scrMtx )

    }

    bSMtxLst[["sScore09"]] <- function( phVP ,aZoidMtx ,makeInfoStr=F ){
        # phVP <- phVP.grp$phVPLst[[1]]
		aLen <- nrow(aZoidMtx)
        aObj <- phVP$getCodeW( aZoidMtx )

		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(bS.sScore09.cName) )	;colnames(scoreMtx) <- bS.sScore09.cName
        rownames(scoreMtx) <- aObj$miIdStr
		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c( "pvSubHLen" ,"pvSubName" )
			infoMtx <- matrix( "" ,nrow=aLen ,ncol=length(cName) )	;colnames(infoMtx) <- cName
			infoMtx[,"pvSubName"] <- aObj$miIdStr
            for( pvName in names(phVP$stdMILst) ){
                infoMtx[infoMtx[,"pvSubName"]==pvName ,"pvSubHLen"] <- phVP$stdMILst[[pvName]]$mtxLen
            }
		}
		if( 0==length(aObj$miIdStr) ){
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

        for( pvName in aObj$miNames ){  # pvName <- aObj$miNames[1]
            workArea <- which(aObj$miIdStr==pvName)
            stdMI<-phVP$stdMILst[[pvName]]
            wMI <- phVP$getCodeH( stdMI )
            scrMtx <- bSMtx.sScore09( stdMI=stdMI ,workArea ,wMI=wMI ,aObj ,aZoidMtx )
            scoreMtx[workArea,] <- scrMtx
        }

        return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
    }

}





if( TRUE ){ # "sScore0LAr13"

    # 주의사항 : bSMtx는 tailMtx 폭이 가면적이므로 
    #           "colA5","colB5" ,"colA6","colB6" 값은 없을 수도 있다.
	bS.sScore0LAr13.cName <- c(  "colA1","colA2","colA3","colA4","colA5","colA6"
					            ,"colB1","colB2","colB3","colB4","colB5","colB6"
    )

    bSMtx.sScore0LAr13 <- function( stdMI ,workArea ,wMI ,aObj ,aZoidMtx ){
		scrMtx <- matrix( 0, nrow=length(workArea), ncol=length(bS.sScore0LAr13.cName) )	;colnames(scrMtx) <- bS.sScore0LAr13.cName
        rownames(scrMtx) <- workArea
        stdMILen <- nrow(wMI$rawTail)
        if( 0==stdMILen ){
            return( scrMtx )
        }

        rObj <- list( lPtn1=NULL ,lPtn2=NULL ,colLen=ncol(wMI$rawTail) )
        if( TRUE ){
            tailMtx <- wMI$rawTail
            yIdxA <- nrow(tailMtx) -6 +1	# 바닥으로부터 자신 포함한 높이 6
            yIdxB <- nrow(tailMtx) -6 +3	# 바닥으로부터 자신 포함한 높이 4
            if( 0<yIdxA ){
                rObj$lPtn1 <- bUtil.findLinearPtn( tailMtx ,yIdx=yIdxA ,typ="A" )
            }
            if( 0<yIdxB ){
                rObj$lPtn2 <- bUtil.findLinearPtn( tailMtx ,yIdx=yIdxB ,typ="A" )
            }
        }

        matCntA <- rep( 0 ,rObj$colLen )
        matCntB <- rep( 0 ,rObj$colLen )
        for( wIdx in seq_len(length(workArea)) ){
            aIdx <- workArea[wIdx]
            aCode <- aObj$aZoidMtx[aIdx,]       ;aRem <- aCode%%10
            aCStep <- aObj$cStepMtx[aIdx,]      ;aFStep <- aObj$fStepMtx[aIdx,]

            matCntA[] <- 0      ;matCntB[] <- 0
            if( !is.null(rObj$lPtn1) ){
                matCntA <- bUtil.checkMatch_LinearPtn( rObj$lPtn1 ,aCode )
            }
            if( !is.null(rObj$lPtn2) ){
                matCntB <- bUtil.checkMatch_LinearPtn( rObj$lPtn2 ,aCode )
            }

            scrMtx[wIdx,"colA1"] <- matCntA[1]
            scrMtx[wIdx,"colB1"] <- matCntB[1]
            scrMtx[wIdx,"colA2"] <- matCntA[2]
            scrMtx[wIdx,"colB2"] <- matCntB[2]
            if( 3<=rObj$colLen ){
                scrMtx[wIdx,"colA3"] <- matCntA[3]
                scrMtx[wIdx,"colB3"] <- matCntB[3]
            }
            if( 4<=rObj$colLen ){
                scrMtx[wIdx,"colA4"] <- matCntA[4]
                scrMtx[wIdx,"colB4"] <- matCntB[4]
            }
            if( 5<=rObj$colLen ){
                scrMtx[wIdx,"colA5"] <- matCntA[5]
                scrMtx[wIdx,"colB5"] <- matCntB[5]
            }
            if( 6<=rObj$colLen ){
                scrMtx[wIdx,"colA6"] <- matCntA[6]
                scrMtx[wIdx,"colB6"] <- matCntB[6]
            }

            # scrMtx[wIdx ,scrMtx[wIdx,]==1] <- 0
            # scrMtx[wIdx,"rebC.r"] <- sum(aCode==wMI$rawTail[stdMILen,])
            # scrMtx[wIdx,"rebC.c"] <- sum(aCStep==wMI$cStepTail[stdMILen,])
        }

        return( scrMtx )

    }

    bSMtxLst[["sScore0LAr13"]] <- function( phVP ,aZoidMtx ,makeInfoStr=F ){
        # phVP <- phVP.grp$phVPLst[[1]]
		aLen <- nrow(aZoidMtx)
        aObj <- phVP$getCodeW( aZoidMtx )

		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(bS.sScore0LAr13.cName) )	;colnames(scoreMtx) <- bS.sScore0LAr13.cName
        rownames(scoreMtx) <- aObj$miIdStr
		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c( "pvSubHLen" ,"pvSubName" )
			infoMtx <- matrix( "" ,nrow=aLen ,ncol=length(cName) )	;colnames(infoMtx) <- cName
			infoMtx[,"pvSubName"] <- aObj$miIdStr
            for( pvName in names(phVP$stdMILst) ){
                infoMtx[infoMtx[,"pvSubName"]==pvName ,"pvSubHLen"] <- phVP$stdMILst[[pvName]]$mtxLen
            }
		}
		if( 0==length(aObj$miIdStr) ){
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

        for( pvName in aObj$miNames ){  # pvName <- aObj$miNames[2]
            workArea <- which(aObj$miIdStr==pvName)
            stdMI<-phVP$stdMILst[[pvName]]
            wMI <- phVP$getCodeH( stdMI )
            scrMtx <- bSMtx.sScore0LAr13( stdMI=stdMI ,workArea ,wMI=wMI ,aObj ,aZoidMtx )
            scoreMtx[workArea,] <- scrMtx
        }

        return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
    }

}

if( TRUE ){ # "sScore0LAr24"

    # 주의사항 : bSMtx는 tailMtx 폭이 가면적이므로 
    #           "colA5","colB5" ,"colA6","colB6" 값은 없을 수도 있다.
	bS.sScore0LAr24.cName <- c(  "colA1","colA2","colA3","colA4","colA5","colA6"
					            ,"colB1","colB2","colB3","colB4","colB5","colB6"
    )

    bSMtx.sScore0LAr24 <- function( stdMI ,workArea ,wMI ,aObj ,aZoidMtx ){
		scrMtx <- matrix( 0, nrow=length(workArea), ncol=length(bS.sScore0LAr24.cName) )	;colnames(scrMtx) <- bS.sScore0LAr24.cName
        rownames(scrMtx) <- workArea
        stdMILen <- nrow(wMI$rawTail)
        if( 0==stdMILen ){
            return( scrMtx )
        }

        rObj <- list( lPtn1=NULL ,lPtn2=NULL ,colLen=ncol(wMI$rawTail) )
        if( TRUE ){
            tailMtx <- wMI$rawTail
            yIdxA <- nrow(tailMtx) -6 +2	# 바닥으로부터 자신 포함한 높이 6
            yIdxB <- nrow(tailMtx) -6 +4	# 바닥으로부터 자신 포함한 높이 4
            if( 0<yIdxA ){
                rObj$lPtn1 <- bUtil.findLinearPtn( tailMtx ,yIdx=yIdxA ,typ="A" )
            }
            if( 0<yIdxB ){
                rObj$lPtn2 <- bUtil.findLinearPtn( tailMtx ,yIdx=yIdxB ,typ="A" )
            }
        }

        matCntA <- rep( 0 ,rObj$colLen )
        matCntB <- rep( 0 ,rObj$colLen )
        for( wIdx in seq_len(length(workArea)) ){
            aIdx <- workArea[wIdx]
            aCode <- aObj$aZoidMtx[aIdx,]       ;aRem <- aCode%%10
            aCStep <- aObj$cStepMtx[aIdx,]      ;aFStep <- aObj$fStepMtx[aIdx,]

            matCntA[] <- 0      ;matCntB[] <- 0
            if( !is.null(rObj$lPtn1) ){
                matCntA <- bUtil.checkMatch_LinearPtn( rObj$lPtn1 ,aCode )
            }
            if( !is.null(rObj$lPtn2) ){
                matCntB <- bUtil.checkMatch_LinearPtn( rObj$lPtn2 ,aCode )
            }

            scrMtx[wIdx,"colA1"] <- matCntA[1]
            scrMtx[wIdx,"colB1"] <- matCntB[1]
            scrMtx[wIdx,"colA2"] <- matCntA[2]
            scrMtx[wIdx,"colB2"] <- matCntB[2]
            if( 3<=rObj$colLen ){
                scrMtx[wIdx,"colA3"] <- matCntA[3]
                scrMtx[wIdx,"colB3"] <- matCntB[3]
            }
            if( 4<=rObj$colLen ){
                scrMtx[wIdx,"colA4"] <- matCntA[4]
                scrMtx[wIdx,"colB4"] <- matCntB[4]
            }
            if( 5<=rObj$colLen ){
                scrMtx[wIdx,"colA5"] <- matCntA[5]
                scrMtx[wIdx,"colB5"] <- matCntB[5]
            }
            if( 6<=rObj$colLen ){
                scrMtx[wIdx,"colA6"] <- matCntA[6]
                scrMtx[wIdx,"colB6"] <- matCntB[6]
            }

            # scrMtx[wIdx ,scrMtx[wIdx,]==1] <- 0
            # scrMtx[wIdx,"rebC.r"] <- sum(aCode==wMI$rawTail[stdMILen,])
            # scrMtx[wIdx,"rebC.c"] <- sum(aCStep==wMI$cStepTail[stdMILen,])
        }

        return( scrMtx )

    }

    bSMtxLst[["sScore0LAr24"]] <- function( phVP ,aZoidMtx ,makeInfoStr=F ){
        # phVP <- phVP.grp$phVPLst[[1]]
		aLen <- nrow(aZoidMtx)
        aObj <- phVP$getCodeW( aZoidMtx )

		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(bS.sScore0LAr24.cName) )	;colnames(scoreMtx) <- bS.sScore0LAr24.cName
        rownames(scoreMtx) <- aObj$miIdStr
		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c( "pvSubHLen" ,"pvSubName" )
			infoMtx <- matrix( "" ,nrow=aLen ,ncol=length(cName) )	;colnames(infoMtx) <- cName
			infoMtx[,"pvSubName"] <- aObj$miIdStr
            for( pvName in names(phVP$stdMILst) ){
                infoMtx[infoMtx[,"pvSubName"]==pvName ,"pvSubHLen"] <- phVP$stdMILst[[pvName]]$mtxLen
            }
		}
		if( 0==length(aObj$miIdStr) ){
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

        for( pvName in aObj$miNames ){  # pvName <- aObj$miNames[2]
            workArea <- which(aObj$miIdStr==pvName)
            stdMI<-phVP$stdMILst[[pvName]]
            wMI <- phVP$getCodeH( stdMI )
            scrMtx <- bSMtx.sScore0LAr24( stdMI=stdMI ,workArea ,wMI=wMI ,aObj ,aZoidMtx )
            scoreMtx[workArea,] <- scrMtx
        }

        return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
    }

}

if( TRUE ){ # "sScore0LVr13"

    # 주의사항 : bSMtx는 tailMtx 폭이 가면적이므로 
    #           "colA5","colB5" ,"colA6","colB6" 값은 없을 수도 있다.
	bS.sScore0LVr13.cName <- c(  "colA1","colA2","colA3","colA4","colA5","colA6"
					            ,"colB1","colB2","colB3","colB4","colB5","colB6"
    )

    bSMtx.sScore0LVr13 <- function( stdMI ,workArea ,wMI ,aObj ,aZoidMtx ){
		scrMtx <- matrix( 0, nrow=length(workArea), ncol=length(bS.sScore0LVr13.cName) )	;colnames(scrMtx) <- bS.sScore0LVr13.cName
        rownames(scrMtx) <- workArea
        stdMILen <- nrow(wMI$rawTail)
        if( 0==stdMILen ){
            return( scrMtx )
        }

        rObj <- list( lPtn1=NULL ,lPtn2=NULL ,colLen=ncol(wMI$rawTail) )
        if( TRUE ){
            tailMtx <- wMI$rawTail
            if( 1<stdMILen ){
                rObj$lPtn1 <- bUtil.findLinearPtn( tailMtx ,yIdx=nrow(tailMtx) ,typ="V" )
            }
            if( 3<stdMILen ){
                rObj$lPtn2 <- bUtil.findLinearPtn( tailMtx ,yIdx=nrow(tailMtx)-2 ,typ="V" )
            }
        }

        matCntA <- rep( 0 ,rObj$colLen )
        matCntB <- rep( 0 ,rObj$colLen )
        for( wIdx in seq_len(length(workArea)) ){
            aIdx <- workArea[wIdx]
            aCode <- aObj$aZoidMtx[aIdx,]       ;aRem <- aCode%%10
            aCStep <- aObj$cStepMtx[aIdx,]      ;aFStep <- aObj$fStepMtx[aIdx,]

            matCntA[] <- 0      ;matCntB[] <- 0
            if( !is.null(rObj$lPtn1) ){
                matCntA <- bUtil.checkMatch_LinearPtn( rObj$lPtn1 ,aCode )
            }
            if( !is.null(rObj$lPtn2) ){
                matCntB <- bUtil.checkMatch_LinearPtn( rObj$lPtn2 ,aCode )
            }

            scrMtx[wIdx,"colA1"] <- matCntA[1]
            scrMtx[wIdx,"colB1"] <- matCntB[1]
            scrMtx[wIdx,"colA2"] <- matCntA[2]
            scrMtx[wIdx,"colB2"] <- matCntB[2]
            if( 3<=rObj$colLen ){
                scrMtx[wIdx,"colA3"] <- matCntA[3]
                scrMtx[wIdx,"colB3"] <- matCntB[3]
            }
            if( 4<=rObj$colLen ){
                scrMtx[wIdx,"colA4"] <- matCntA[4]
                scrMtx[wIdx,"colB4"] <- matCntB[4]
            }
            if( 5<=rObj$colLen ){
                scrMtx[wIdx,"colA5"] <- matCntA[5]
                scrMtx[wIdx,"colB5"] <- matCntB[5]
            }
            if( 6<=rObj$colLen ){
                scrMtx[wIdx,"colA6"] <- matCntA[6]
                scrMtx[wIdx,"colB6"] <- matCntB[6]
            }

            # scrMtx[wIdx ,scrMtx[wIdx,]==1] <- 0
            # scrMtx[wIdx,"rebC.r"] <- sum(aCode==wMI$rawTail[stdMILen,])
            # scrMtx[wIdx,"rebC.c"] <- sum(aCStep==wMI$cStepTail[stdMILen,])
        }

        return( scrMtx )

    }

    bSMtxLst[["sScore0LVr13"]] <- function( phVP ,aZoidMtx ,makeInfoStr=F ){
        # phVP <- phVP.grp$phVPLst[[1]]
		aLen <- nrow(aZoidMtx)
        aObj <- phVP$getCodeW( aZoidMtx )

		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(bS.sScore0LVr13.cName) )	;colnames(scoreMtx) <- bS.sScore0LVr13.cName
        rownames(scoreMtx) <- aObj$miIdStr
		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c( "pvSubHLen" ,"pvSubName" )
			infoMtx <- matrix( "" ,nrow=aLen ,ncol=length(cName) )	;colnames(infoMtx) <- cName
			infoMtx[,"pvSubName"] <- aObj$miIdStr
            for( pvName in names(phVP$stdMILst) ){
                infoMtx[infoMtx[,"pvSubName"]==pvName ,"pvSubHLen"] <- phVP$stdMILst[[pvName]]$mtxLen
            }
		}
		if( 0==length(aObj$miIdStr) ){
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

        for( pvName in aObj$miNames ){  # pvName <- aObj$miNames[2]
            workArea <- which(aObj$miIdStr==pvName)
            stdMI<-phVP$stdMILst[[pvName]]
            wMI <- phVP$getCodeH( stdMI )
            scrMtx <- bSMtx.sScore0LVr13( stdMI=stdMI ,workArea ,wMI=wMI ,aObj ,aZoidMtx )
            scoreMtx[workArea,] <- scrMtx
        }

        return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
    }

}

if( TRUE ){ # "sScore0LVr24"

    # 주의사항 : bSMtx는 tailMtx 폭이 가면적이므로 
    #           "colA5","colB5" ,"colA6","colB6" 값은 없을 수도 있다.
	bS.sScore0LVr24.cName <- c(  "colA1","colA2","colA3","colA4","colA5","colA6"
					            ,"colB1","colB2","colB3","colB4","colB5","colB6"
    )

    bSMtx.sScore0LVr24 <- function( stdMI ,workArea ,wMI ,aObj ,aZoidMtx ){
		scrMtx <- matrix( 0, nrow=length(workArea), ncol=length(bS.sScore0LVr24.cName) )	;colnames(scrMtx) <- bS.sScore0LVr24.cName
        rownames(scrMtx) <- workArea
        stdMILen <- nrow(wMI$rawTail)
        if( 0==stdMILen ){
            return( scrMtx )
        }

        rObj <- list( lPtn1=NULL ,lPtn2=NULL ,colLen=ncol(wMI$rawTail) )
        if( TRUE ){
            tailMtx <- wMI$rawTail
            if( 2<stdMILen ){
                rObj$lPtn1 <- bUtil.findLinearPtn( tailMtx ,yIdx=nrow(tailMtx)-1 ,typ="V" )
            }
            if( 4<stdMILen ){
                rObj$lPtn2 <- bUtil.findLinearPtn( tailMtx ,yIdx=nrow(tailMtx)-3 ,typ="V" )
            }
        }

        matCntA <- rep( 0 ,rObj$colLen )
        matCntB <- rep( 0 ,rObj$colLen )
        for( wIdx in seq_len(length(workArea)) ){
            aIdx <- workArea[wIdx]
            aCode <- aObj$aZoidMtx[aIdx,]       ;aRem <- aCode%%10
            aCStep <- aObj$cStepMtx[aIdx,]      ;aFStep <- aObj$fStepMtx[aIdx,]

            matCntA[] <- 0      ;matCntB[] <- 0
            if( !is.null(rObj$lPtn1) ){
                matCntA <- bUtil.checkMatch_LinearPtn( rObj$lPtn1 ,aCode )
            }
            if( !is.null(rObj$lPtn2) ){
                matCntB <- bUtil.checkMatch_LinearPtn( rObj$lPtn2 ,aCode )
            }

            scrMtx[wIdx,"colA1"] <- matCntA[1]
            scrMtx[wIdx,"colB1"] <- matCntB[1]
            scrMtx[wIdx,"colA2"] <- matCntA[2]
            scrMtx[wIdx,"colB2"] <- matCntB[2]
            if( 3<=rObj$colLen ){
                scrMtx[wIdx,"colA3"] <- matCntA[3]
                scrMtx[wIdx,"colB3"] <- matCntB[3]
            }
            if( 4<=rObj$colLen ){
                scrMtx[wIdx,"colA4"] <- matCntA[4]
                scrMtx[wIdx,"colB4"] <- matCntB[4]
            }
            if( 5<=rObj$colLen ){
                scrMtx[wIdx,"colA5"] <- matCntA[5]
                scrMtx[wIdx,"colB5"] <- matCntB[5]
            }
            if( 6<=rObj$colLen ){
                scrMtx[wIdx,"colA6"] <- matCntA[6]
                scrMtx[wIdx,"colB6"] <- matCntB[6]
            }

            # scrMtx[wIdx ,scrMtx[wIdx,]==1] <- 0
            # scrMtx[wIdx,"rebC.r"] <- sum(aCode==wMI$rawTail[stdMILen,])
            # scrMtx[wIdx,"rebC.c"] <- sum(aCStep==wMI$cStepTail[stdMILen,])
        }

        return( scrMtx )

    }

    bSMtxLst[["sScore0LVr24"]] <- function( phVP ,aZoidMtx ,makeInfoStr=F ){
        # phVP <- phVP.grp$phVPLst[[1]]
		aLen <- nrow(aZoidMtx)
        aObj <- phVP$getCodeW( aZoidMtx )

		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(bS.sScore0LVr24.cName) )	;colnames(scoreMtx) <- bS.sScore0LVr24.cName
        rownames(scoreMtx) <- aObj$miIdStr
		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c( "pvSubHLen" ,"pvSubName" )
			infoMtx <- matrix( "" ,nrow=aLen ,ncol=length(cName) )	;colnames(infoMtx) <- cName
			infoMtx[,"pvSubName"] <- aObj$miIdStr
            for( pvName in names(phVP$stdMILst) ){
                infoMtx[infoMtx[,"pvSubName"]==pvName ,"pvSubHLen"] <- phVP$stdMILst[[pvName]]$mtxLen
            }
		}
		if( 0==length(aObj$miIdStr) ){
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

        for( pvName in aObj$miNames ){  # pvName <- aObj$miNames[2]
            workArea <- which(aObj$miIdStr==pvName)
            stdMI<-phVP$stdMILst[[pvName]]
            wMI <- phVP$getCodeH( stdMI )
            scrMtx <- bSMtx.sScore0LVr24( stdMI=stdMI ,workArea ,wMI=wMI ,aObj ,aZoidMtx )
            scoreMtx[workArea,] <- scrMtx
        }

        return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
    }

}



if( TRUE ){ # "sScore0LAe13"

    # 주의사항 : bSMtx는 tailMtx 폭이 가면적이므로 
    #           "colA5","colB5" ,"colA6","colB6" 값은 없을 수도 있다.
	bS.sScore0LAe13.cName <- c(  "colA1","colA2","colA3","colA4","colA5","colA6"
					            ,"colB1","colB2","colB3","colB4","colB5","colB6"
    )

    bSMtx.sScore0LAe13 <- function( stdMI ,workArea ,wMI ,aObj ,aZoidMtx ){
		scrMtx <- matrix( 0, nrow=length(workArea), ncol=length(bS.sScore0LAe13.cName) )	;colnames(scrMtx) <- bS.sScore0LAe13.cName
        rownames(scrMtx) <- workArea
        stdMILen <- nrow(wMI$rawTail)
        if( 0==stdMILen ){
            return( scrMtx )
        }

        rObj <- list( lPtn1=NULL ,lPtn2=NULL ,colLen=ncol(wMI$rawTail) )
        if( TRUE ){
            tailMtx <- wMI$rawTail%%10
            yIdxA <- nrow(tailMtx) -6 +1	# 바닥으로부터 자신 포함한 높이 6
            yIdxB <- nrow(tailMtx) -6 +3	# 바닥으로부터 자신 포함한 높이 4
            if( 0<yIdxA ){
                rObj$lPtn1 <- bUtil.findLinearPtn( tailMtx ,yIdx=yIdxA ,typ="A" )
            }
            if( 0<yIdxB ){
                rObj$lPtn2 <- bUtil.findLinearPtn( tailMtx ,yIdx=yIdxB ,typ="A" )
            }
        }

        matCntA <- rep( 0 ,rObj$colLen )
        matCntB <- rep( 0 ,rObj$colLen )
        for( wIdx in seq_len(length(workArea)) ){
            aIdx <- workArea[wIdx]
            aCode <- aObj$aZoidMtx[aIdx,]       ;aRem <- aCode%%10
            aCStep <- aObj$cStepMtx[aIdx,]      ;aFStep <- aObj$fStepMtx[aIdx,]

            matCntA[] <- 0      ;matCntB[] <- 0
            if( !is.null(rObj$lPtn1) ){
                matCntA <- bUtil.checkMatch_LinearPtn( rObj$lPtn1 ,aRem )
            }
            if( !is.null(rObj$lPtn2) ){
                matCntB <- bUtil.checkMatch_LinearPtn( rObj$lPtn2 ,aRem )
            }

            scrMtx[wIdx,"colA1"] <- matCntA[1]
            scrMtx[wIdx,"colB1"] <- matCntB[1]
            scrMtx[wIdx,"colA2"] <- matCntA[2]
            scrMtx[wIdx,"colB2"] <- matCntB[2]
            if( 3<=rObj$colLen ){
                scrMtx[wIdx,"colA3"] <- matCntA[3]
                scrMtx[wIdx,"colB3"] <- matCntB[3]
            }
            if( 4<=rObj$colLen ){
                scrMtx[wIdx,"colA4"] <- matCntA[4]
                scrMtx[wIdx,"colB4"] <- matCntB[4]
            }
            if( 5<=rObj$colLen ){
                scrMtx[wIdx,"colA5"] <- matCntA[5]
                scrMtx[wIdx,"colB5"] <- matCntB[5]
            }
            if( 6<=rObj$colLen ){
                scrMtx[wIdx,"colA6"] <- matCntA[6]
                scrMtx[wIdx,"colB6"] <- matCntB[6]
            }

            # scrMtx[wIdx ,scrMtx[wIdx,]==1] <- 0
            # scrMtx[wIdx,"rebC.r"] <- sum(aCode==wMI$rawTail[stdMILen,])
            # scrMtx[wIdx,"rebC.c"] <- sum(aCStep==wMI$cStepTail[stdMILen,])
        }

        return( scrMtx )

    }

    bSMtxLst[["sScore0LAe13"]] <- function( phVP ,aZoidMtx ,makeInfoStr=F ){
        # phVP <- phVP.grp$phVPLst[[1]]
		aLen <- nrow(aZoidMtx)
        aObj <- phVP$getCodeW( aZoidMtx )

		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(bS.sScore0LAe13.cName) )	;colnames(scoreMtx) <- bS.sScore0LAe13.cName
        rownames(scoreMtx) <- aObj$miIdStr
		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c( "pvSubHLen" ,"pvSubName" )
			infoMtx <- matrix( "" ,nrow=aLen ,ncol=length(cName) )	;colnames(infoMtx) <- cName
			infoMtx[,"pvSubName"] <- aObj$miIdStr
            for( pvName in names(phVP$stdMILst) ){
                infoMtx[infoMtx[,"pvSubName"]==pvName ,"pvSubHLen"] <- phVP$stdMILst[[pvName]]$mtxLen
            }
		}
		if( 0==length(aObj$miIdStr) ){
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

        for( pvName in aObj$miNames ){  # pvName <- aObj$miNames[2]
            workArea <- which(aObj$miIdStr==pvName)
            stdMI<-phVP$stdMILst[[pvName]]
            wMI <- phVP$getCodeH( stdMI )
            scrMtx <- bSMtx.sScore0LAe13( stdMI=stdMI ,workArea ,wMI=wMI ,aObj ,aZoidMtx )
            scoreMtx[workArea,] <- scrMtx
        }

        return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
    }

}

if( TRUE ){ # "sScore0LAe24"

    # 주의사항 : bSMtx는 tailMtx 폭이 가면적이므로 
    #           "colA5","colB5" ,"colA6","colB6" 값은 없을 수도 있다.
	bS.sScore0LAe24.cName <- c(  "colA1","colA2","colA3","colA4","colA5","colA6"
					            ,"colB1","colB2","colB3","colB4","colB5","colB6"
    )

    bSMtx.sScore0LAe24 <- function( stdMI ,workArea ,wMI ,aObj ,aZoidMtx ){
		scrMtx <- matrix( 0, nrow=length(workArea), ncol=length(bS.sScore0LAe24.cName) )	;colnames(scrMtx) <- bS.sScore0LAe24.cName
        rownames(scrMtx) <- workArea
        stdMILen <- nrow(wMI$rawTail)
        if( 0==stdMILen ){
            return( scrMtx )
        }

        rObj <- list( lPtn1=NULL ,lPtn2=NULL ,colLen=ncol(wMI$rawTail) )
        if( TRUE ){
            tailMtx <- wMI$rawTail%%10
            yIdxA <- nrow(tailMtx) -6 +2	# 바닥으로부터 자신 포함한 높이 6
            yIdxB <- nrow(tailMtx) -6 +4	# 바닥으로부터 자신 포함한 높이 4
            if( 0<yIdxA ){
                rObj$lPtn1 <- bUtil.findLinearPtn( tailMtx ,yIdx=yIdxA ,typ="A" )
            }
            if( 0<yIdxB ){
                rObj$lPtn2 <- bUtil.findLinearPtn( tailMtx ,yIdx=yIdxB ,typ="A" )
            }
        }

        matCntA <- rep( 0 ,rObj$colLen )
        matCntB <- rep( 0 ,rObj$colLen )
        for( wIdx in seq_len(length(workArea)) ){
            aIdx <- workArea[wIdx]
            aCode <- aObj$aZoidMtx[aIdx,]       ;aRem <- aCode%%10
            aCStep <- aObj$cStepMtx[aIdx,]      ;aFStep <- aObj$fStepMtx[aIdx,]

            matCntA[] <- 0      ;matCntB[] <- 0
            if( !is.null(rObj$lPtn1) ){
                matCntA <- bUtil.checkMatch_LinearPtn( rObj$lPtn1 ,aRem )
            }
            if( !is.null(rObj$lPtn2) ){
                matCntB <- bUtil.checkMatch_LinearPtn( rObj$lPtn2 ,aRem )
            }

            scrMtx[wIdx,"colA1"] <- matCntA[1]
            scrMtx[wIdx,"colB1"] <- matCntB[1]
            scrMtx[wIdx,"colA2"] <- matCntA[2]
            scrMtx[wIdx,"colB2"] <- matCntB[2]
            if( 3<=rObj$colLen ){
                scrMtx[wIdx,"colA3"] <- matCntA[3]
                scrMtx[wIdx,"colB3"] <- matCntB[3]
            }
            if( 4<=rObj$colLen ){
                scrMtx[wIdx,"colA4"] <- matCntA[4]
                scrMtx[wIdx,"colB4"] <- matCntB[4]
            }
            if( 5<=rObj$colLen ){
                scrMtx[wIdx,"colA5"] <- matCntA[5]
                scrMtx[wIdx,"colB5"] <- matCntB[5]
            }
            if( 6<=rObj$colLen ){
                scrMtx[wIdx,"colA6"] <- matCntA[6]
                scrMtx[wIdx,"colB6"] <- matCntB[6]
            }

            # scrMtx[wIdx ,scrMtx[wIdx,]==1] <- 0
            # scrMtx[wIdx,"rebC.r"] <- sum(aCode==wMI$rawTail[stdMILen,])
            # scrMtx[wIdx,"rebC.c"] <- sum(aCStep==wMI$cStepTail[stdMILen,])
        }

        return( scrMtx )

    }

    bSMtxLst[["sScore0LAe24"]] <- function( phVP ,aZoidMtx ,makeInfoStr=F ){
        # phVP <- phVP.grp$phVPLst[[1]]
		aLen <- nrow(aZoidMtx)
        aObj <- phVP$getCodeW( aZoidMtx )

		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(bS.sScore0LAe24.cName) )	;colnames(scoreMtx) <- bS.sScore0LAe24.cName
        rownames(scoreMtx) <- aObj$miIdStr
		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c( "pvSubHLen" ,"pvSubName" )
			infoMtx <- matrix( "" ,nrow=aLen ,ncol=length(cName) )	;colnames(infoMtx) <- cName
			infoMtx[,"pvSubName"] <- aObj$miIdStr
            for( pvName in names(phVP$stdMILst) ){
                infoMtx[infoMtx[,"pvSubName"]==pvName ,"pvSubHLen"] <- phVP$stdMILst[[pvName]]$mtxLen
            }
		}
		if( 0==length(aObj$miIdStr) ){
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

        for( pvName in aObj$miNames ){  # pvName <- aObj$miNames[2]
            workArea <- which(aObj$miIdStr==pvName)
            stdMI<-phVP$stdMILst[[pvName]]
            wMI <- phVP$getCodeH( stdMI )
            scrMtx <- bSMtx.sScore0LAe24( stdMI=stdMI ,workArea ,wMI=wMI ,aObj ,aZoidMtx )
            scoreMtx[workArea,] <- scrMtx
        }

        return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
    }

}

if( TRUE ){ # "sScore0LVe13"

    # 주의사항 : bSMtx는 tailMtx 폭이 가변적이므로 
    #           "colA5","colB5" ,"colA6","colB6" 값은 없을 수도 있다.
	bS.sScore0LVe13.cName <- c(  "colA1","colA2","colA3","colA4","colA5","colA6"
					            ,"colB1","colB2","colB3","colB4","colB5","colB6"
    )

    bSMtx.sScore0LVe13 <- function( stdMI ,workArea ,wMI ,aObj ,aZoidMtx ){
		scrMtx <- matrix( 0, nrow=length(workArea), ncol=length(bS.sScore0LVe13.cName) )	;colnames(scrMtx) <- bS.sScore0LVe13.cName
        rownames(scrMtx) <- workArea
        stdMILen <- nrow(wMI$rawTail)
        if( 0==stdMILen ){
            return( scrMtx )
        }

        rObj <- list( lPtn1=NULL ,lPtn2=NULL ,colLen=ncol(wMI$rawTail) )
        if( TRUE ){
            tailMtx <- wMI$rawTail%%10
            if( 1<stdMILen ){
                rObj$lPtn1 <- bUtil.findLinearPtn( tailMtx ,yIdx=nrow(tailMtx) ,typ="V" )
            }
            if( 3<stdMILen ){
                rObj$lPtn2 <- bUtil.findLinearPtn( tailMtx ,yIdx=nrow(tailMtx)-2 ,typ="V" )
            }
        }

        matCntA <- rep( 0 ,rObj$colLen )
        matCntB <- rep( 0 ,rObj$colLen )
        for( wIdx in seq_len(length(workArea)) ){
            aIdx <- workArea[wIdx]
            aCode <- aObj$aZoidMtx[aIdx,]       ;aRem <- aCode%%10
            aCStep <- aObj$cStepMtx[aIdx,]      ;aFStep <- aObj$fStepMtx[aIdx,]

            matCntA[] <- 0      ;matCntB[] <- 0
            if( !is.null(rObj$lPtn1) ){
                matCntA <- bUtil.checkMatch_LinearPtn( rObj$lPtn1 ,aRem )
            }
            if( !is.null(rObj$lPtn2) ){
                matCntB <- bUtil.checkMatch_LinearPtn( rObj$lPtn2 ,aRem )
            }

            scrMtx[wIdx,"colA1"] <- matCntA[1]
            scrMtx[wIdx,"colB1"] <- matCntB[1]
            scrMtx[wIdx,"colA2"] <- matCntA[2]
            scrMtx[wIdx,"colB2"] <- matCntB[2]
            if( 3<=rObj$colLen ){
                scrMtx[wIdx,"colA3"] <- matCntA[3]
                scrMtx[wIdx,"colB3"] <- matCntB[3]
            }
            if( 4<=rObj$colLen ){
                scrMtx[wIdx,"colA4"] <- matCntA[4]
                scrMtx[wIdx,"colB4"] <- matCntB[4]
            }
            if( 5<=rObj$colLen ){
                scrMtx[wIdx,"colA5"] <- matCntA[5]
                scrMtx[wIdx,"colB5"] <- matCntB[5]
            }
            if( 6<=rObj$colLen ){
                scrMtx[wIdx,"colA6"] <- matCntA[6]
                scrMtx[wIdx,"colB6"] <- matCntB[6]
            }

            # scrMtx[wIdx ,scrMtx[wIdx,]==1] <- 0
            # scrMtx[wIdx,"rebC.r"] <- sum(aCode==wMI$rawTail[stdMILen,])
            # scrMtx[wIdx,"rebC.c"] <- sum(aCStep==wMI$cStepTail[stdMILen,])
        }

        return( scrMtx )

    }

    bSMtxLst[["sScore0LVe13"]] <- function( phVP ,aZoidMtx ,makeInfoStr=F ){
        # phVP <- phVP.grp$phVPLst[[1]]
		aLen <- nrow(aZoidMtx)
        aObj <- phVP$getCodeW( aZoidMtx )

		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(bS.sScore0LVe13.cName) )	;colnames(scoreMtx) <- bS.sScore0LVe13.cName
        rownames(scoreMtx) <- aObj$miIdStr
		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c( "pvSubHLen" ,"pvSubName" )
			infoMtx <- matrix( "" ,nrow=aLen ,ncol=length(cName) )	;colnames(infoMtx) <- cName
			infoMtx[,"pvSubName"] <- aObj$miIdStr
            for( pvName in names(phVP$stdMILst) ){
                infoMtx[infoMtx[,"pvSubName"]==pvName ,"pvSubHLen"] <- phVP$stdMILst[[pvName]]$mtxLen
            }
		}
		if( 0==length(aObj$miIdStr) ){
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

        for( pvName in aObj$miNames ){  # pvName <- aObj$miNames[2]
            workArea <- which(aObj$miIdStr==pvName)
            stdMI<-phVP$stdMILst[[pvName]]
            wMI <- phVP$getCodeH( stdMI )
            scrMtx <- bSMtx.sScore0LVe13( stdMI=stdMI ,workArea ,wMI=wMI ,aObj ,aZoidMtx )
            scoreMtx[workArea,] <- scrMtx
        }

        return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
    }

}

if( TRUE ){ # "sScore0LVe24"

    # 주의사항 : bSMtx는 tailMtx 폭이 가변적이므로 
    #           "colA5","colB5" ,"colA6","colB6" 값은 없을 수도 있다.
	bS.sScore0LVe24.cName <- c(  "colA1","colA2","colA3","colA4","colA5","colA6"
					            ,"colB1","colB2","colB3","colB4","colB5","colB6"
    )

    bSMtx.sScore0LVe24 <- function( stdMI ,workArea ,wMI ,aObj ,aZoidMtx ){
		scrMtx <- matrix( 0, nrow=length(workArea), ncol=length(bS.sScore0LVe24.cName) )	;colnames(scrMtx) <- bS.sScore0LVe24.cName
        rownames(scrMtx) <- workArea
        stdMILen <- nrow(wMI$rawTail)
        if( 0==stdMILen ){
            return( scrMtx )
        }

        rObj <- list( lPtn1=NULL ,lPtn2=NULL ,colLen=ncol(wMI$rawTail) )
        if( TRUE ){
            tailMtx <- wMI$rawTail%%10
            if( 2<stdMILen ){
                rObj$lPtn1 <- bUtil.findLinearPtn( tailMtx ,yIdx=nrow(tailMtx)-1 ,typ="V" )
            }
            if( 4<stdMILen ){
                rObj$lPtn2 <- bUtil.findLinearPtn( tailMtx ,yIdx=nrow(tailMtx)-3 ,typ="V" )
            }
        }

        matCntA <- rep( 0 ,rObj$colLen )
        matCntB <- rep( 0 ,rObj$colLen )
        for( wIdx in seq_len(length(workArea)) ){
            aIdx <- workArea[wIdx]
            aCode <- aObj$aZoidMtx[aIdx,]       ;aRem <- aCode%%10
            aCStep <- aObj$cStepMtx[aIdx,]      ;aFStep <- aObj$fStepMtx[aIdx,]

            matCntA[] <- 0      ;matCntB[] <- 0
            if( !is.null(rObj$lPtn1) ){
                matCntA <- bUtil.checkMatch_LinearPtn( rObj$lPtn1 ,aRem )
            }
            if( !is.null(rObj$lPtn2) ){
                matCntB <- bUtil.checkMatch_LinearPtn( rObj$lPtn2 ,aRem )
            }

            scrMtx[wIdx,"colA1"] <- matCntA[1]
            scrMtx[wIdx,"colB1"] <- matCntB[1]
            scrMtx[wIdx,"colA2"] <- matCntA[2]
            scrMtx[wIdx,"colB2"] <- matCntB[2]
            if( 3<=rObj$colLen ){
                scrMtx[wIdx,"colA3"] <- matCntA[3]
                scrMtx[wIdx,"colB3"] <- matCntB[3]
            }
            if( 4<=rObj$colLen ){
                scrMtx[wIdx,"colA4"] <- matCntA[4]
                scrMtx[wIdx,"colB4"] <- matCntB[4]
            }
            if( 5<=rObj$colLen ){
                scrMtx[wIdx,"colA5"] <- matCntA[5]
                scrMtx[wIdx,"colB5"] <- matCntB[5]
            }
            if( 6<=rObj$colLen ){
                scrMtx[wIdx,"colA6"] <- matCntA[6]
                scrMtx[wIdx,"colB6"] <- matCntB[6]
            }

            # scrMtx[wIdx ,scrMtx[wIdx,]==1] <- 0
            # scrMtx[wIdx,"rebC.r"] <- sum(aCode==wMI$rawTail[stdMILen,])
            # scrMtx[wIdx,"rebC.c"] <- sum(aCStep==wMI$cStepTail[stdMILen,])
        }

        return( scrMtx )

    }

    bSMtxLst[["sScore0LVe24"]] <- function( phVP ,aZoidMtx ,makeInfoStr=F ){
        # phVP <- phVP.grp$phVPLst[[1]]
		aLen <- nrow(aZoidMtx)
        aObj <- phVP$getCodeW( aZoidMtx )

		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(bS.sScore0LVe24.cName) )	;colnames(scoreMtx) <- bS.sScore0LVe24.cName
        rownames(scoreMtx) <- aObj$miIdStr
		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c( "pvSubHLen" ,"pvSubName" )
			infoMtx <- matrix( "" ,nrow=aLen ,ncol=length(cName) )	;colnames(infoMtx) <- cName
			infoMtx[,"pvSubName"] <- aObj$miIdStr
            for( pvName in names(phVP$stdMILst) ){
                infoMtx[infoMtx[,"pvSubName"]==pvName ,"pvSubHLen"] <- phVP$stdMILst[[pvName]]$mtxLen
            }
		}
		if( 0==length(aObj$miIdStr) ){
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

        for( pvName in aObj$miNames ){  # pvName <- aObj$miNames[2]
            workArea <- which(aObj$miIdStr==pvName)
            stdMI<-phVP$stdMILst[[pvName]]
            wMI <- phVP$getCodeH( stdMI )
            scrMtx <- bSMtx.sScore0LVe24( stdMI=stdMI ,workArea ,wMI=wMI ,aObj ,aZoidMtx )
            scoreMtx[workArea,] <- scrMtx
        }

        return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
    }

}



if( TRUE ){ # "sScore0LAc13"

    # 주의사항 : bSMtx는 tailMtx 폭이 가면적이므로 
    #           "colA5","colB5" ,"colA6","colB6" 값은 없을 수도 있다.
	bS.sScore0LAc13.cName <- c(  "colA1","colA2","colA3","colA4","colA5","colA6"
					            ,"colB1","colB2","colB3","colB4","colB5","colB6"
    )

    bSMtx.sScore0LAc13 <- function( stdMI ,workArea ,wMI ,aObj ,aZoidMtx ){
		scrMtx <- matrix( 0, nrow=length(workArea), ncol=length(bS.sScore0LAc13.cName) )	;colnames(scrMtx) <- bS.sScore0LAc13.cName
        rownames(scrMtx) <- workArea
        stdMILen <- nrow(wMI$rawTail)
        if( 0==stdMILen ){
            return( scrMtx )
        }

        rObj <- list( lPtn1=NULL ,lPtn2=NULL ,colLen=ncol(wMI$cStepTail) )
        if( TRUE ){
            tailMtx <- wMI$cStepTail
            yIdxA <- nrow(tailMtx) -6 +1	# 바닥으로부터 자신 포함한 높이 6
            yIdxB <- nrow(tailMtx) -6 +3	# 바닥으로부터 자신 포함한 높이 4
            if( 0<yIdxA ){
                rObj$lPtn1 <- bUtil.findLinearPtn( tailMtx ,yIdx=yIdxA ,typ="A" )
            }
            if( 0<yIdxB ){
                rObj$lPtn2 <- bUtil.findLinearPtn( tailMtx ,yIdx=yIdxB ,typ="A" )
            }
        }

        matCntA <- rep( 0 ,rObj$colLen )
        matCntB <- rep( 0 ,rObj$colLen )
        for( wIdx in seq_len(length(workArea)) ){
            aIdx <- workArea[wIdx]
            aCode <- aObj$aZoidMtx[aIdx,]       ;aRem <- aCode%%10
            aCStep <- aObj$cStepMtx[aIdx,]      ;aFStep <- aObj$fStepMtx[aIdx,]

            matCntA[] <- 0      ;matCntB[] <- 0
            if( !is.null(rObj$lPtn1) ){
                matCntA <- bUtil.checkMatch_LinearPtn( rObj$lPtn1 ,aCStep )
            }
            if( !is.null(rObj$lPtn2) ){
                matCntB <- bUtil.checkMatch_LinearPtn( rObj$lPtn2 ,aCStep )
            }

            scrMtx[wIdx,"colA1"] <- matCntA[1]
            scrMtx[wIdx,"colB1"] <- matCntB[1]
            scrMtx[wIdx,"colA2"] <- matCntA[2]
            scrMtx[wIdx,"colB2"] <- matCntB[2]
            if( 3<=rObj$colLen ){
                scrMtx[wIdx,"colA3"] <- matCntA[3]
                scrMtx[wIdx,"colB3"] <- matCntB[3]
            }
            if( 4<=rObj$colLen ){
                scrMtx[wIdx,"colA4"] <- matCntA[4]
                scrMtx[wIdx,"colB4"] <- matCntB[4]
            }
            if( 5<=rObj$colLen ){
                scrMtx[wIdx,"colA5"] <- matCntA[5]
                scrMtx[wIdx,"colB5"] <- matCntB[5]
            }
            if( 6<=rObj$colLen ){
                scrMtx[wIdx,"colA6"] <- matCntA[6]
                scrMtx[wIdx,"colB6"] <- matCntB[6]
            }

            # scrMtx[wIdx ,scrMtx[wIdx,]==1] <- 0
            # scrMtx[wIdx,"rebC.r"] <- sum(aCode==wMI$rawTail[stdMILen,])
            # scrMtx[wIdx,"rebC.c"] <- sum(aCStep==wMI$cStepTail[stdMILen,])
        }

        return( scrMtx )

    }

    bSMtxLst[["sScore0LAc13"]] <- function( phVP ,aZoidMtx ,makeInfoStr=F ){
        # phVP <- phVP.grp$phVPLst[[1]]
		aLen <- nrow(aZoidMtx)
        aObj <- phVP$getCodeW( aZoidMtx )

		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(bS.sScore0LAc13.cName) )	;colnames(scoreMtx) <- bS.sScore0LAc13.cName
        rownames(scoreMtx) <- aObj$miIdStr
		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c( "pvSubHLen" ,"pvSubName" )
			infoMtx <- matrix( "" ,nrow=aLen ,ncol=length(cName) )	;colnames(infoMtx) <- cName
			infoMtx[,"pvSubName"] <- aObj$miIdStr
            for( pvName in names(phVP$stdMILst) ){
                infoMtx[infoMtx[,"pvSubName"]==pvName ,"pvSubHLen"] <- phVP$stdMILst[[pvName]]$mtxLen
            }
		}
		if( 0==length(aObj$miIdStr) ){
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

        for( pvName in aObj$miNames ){  # pvName <- aObj$miNames[2]
            workArea <- which(aObj$miIdStr==pvName)
            stdMI<-phVP$stdMILst[[pvName]]
            wMI <- phVP$getCodeH( stdMI )
            scrMtx <- bSMtx.sScore0LAc13( stdMI=stdMI ,workArea ,wMI=wMI ,aObj ,aZoidMtx )
            scoreMtx[workArea,] <- scrMtx
        }

        return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
    }

}

if( TRUE ){ # "sScore0LAc24"

    # 주의사항 : bSMtx는 tailMtx 폭이 가면적이므로 
    #           "colA5","colB5" ,"colA6","colB6" 값은 없을 수도 있다.
	bS.sScore0LAc24.cName <- c(  "colA1","colA2","colA3","colA4","colA5","colA6"
					            ,"colB1","colB2","colB3","colB4","colB5","colB6"
    )

    bSMtx.sScore0LAc24 <- function( stdMI ,workArea ,wMI ,aObj ,aZoidMtx ){
		scrMtx <- matrix( 0, nrow=length(workArea), ncol=length(bS.sScore0LAc24.cName) )	;colnames(scrMtx) <- bS.sScore0LAc24.cName
        rownames(scrMtx) <- workArea
        stdMILen <- nrow(wMI$rawTail)
        if( 0==stdMILen ){
            return( scrMtx )
        }

        rObj <- list( lPtn1=NULL ,lPtn2=NULL ,colLen=ncol(wMI$cStepTail) )
        if( TRUE ){
            tailMtx <- wMI$cStepTail
            yIdxA <- nrow(tailMtx) -6 +2	# 바닥으로부터 자신 포함한 높이 6
            yIdxB <- nrow(tailMtx) -6 +4	# 바닥으로부터 자신 포함한 높이 4
            if( 0<yIdxA ){
                rObj$lPtn1 <- bUtil.findLinearPtn( tailMtx ,yIdx=yIdxA ,typ="A" )
            }
            if( 0<yIdxB ){
                rObj$lPtn2 <- bUtil.findLinearPtn( tailMtx ,yIdx=yIdxB ,typ="A" )
            }
        }

        matCntA <- rep( 0 ,rObj$colLen )
        matCntB <- rep( 0 ,rObj$colLen )
        for( wIdx in seq_len(length(workArea)) ){
            aIdx <- workArea[wIdx]
            aCode <- aObj$aZoidMtx[aIdx,]       ;aRem <- aCode%%10
            aCStep <- aObj$cStepMtx[aIdx,]      ;aFStep <- aObj$fStepMtx[aIdx,]

            matCntA[] <- 0      ;matCntB[] <- 0
            if( !is.null(rObj$lPtn1) ){
                matCntA <- bUtil.checkMatch_LinearPtn( rObj$lPtn1 ,aCStep )
            }
            if( !is.null(rObj$lPtn2) ){
                matCntB <- bUtil.checkMatch_LinearPtn( rObj$lPtn2 ,aCStep )
            }

            scrMtx[wIdx,"colA1"] <- matCntA[1]
            scrMtx[wIdx,"colB1"] <- matCntB[1]
            scrMtx[wIdx,"colA2"] <- matCntA[2]
            scrMtx[wIdx,"colB2"] <- matCntB[2]
            if( 3<=rObj$colLen ){
                scrMtx[wIdx,"colA3"] <- matCntA[3]
                scrMtx[wIdx,"colB3"] <- matCntB[3]
            }
            if( 4<=rObj$colLen ){
                scrMtx[wIdx,"colA4"] <- matCntA[4]
                scrMtx[wIdx,"colB4"] <- matCntB[4]
            }
            if( 5<=rObj$colLen ){
                scrMtx[wIdx,"colA5"] <- matCntA[5]
                scrMtx[wIdx,"colB5"] <- matCntB[5]
            }
            if( 6<=rObj$colLen ){
                scrMtx[wIdx,"colA6"] <- matCntA[6]
                scrMtx[wIdx,"colB6"] <- matCntB[6]
            }

            # scrMtx[wIdx ,scrMtx[wIdx,]==1] <- 0
            # scrMtx[wIdx,"rebC.r"] <- sum(aCode==wMI$rawTail[stdMILen,])
            # scrMtx[wIdx,"rebC.c"] <- sum(aCStep==wMI$cStepTail[stdMILen,])
        }

        return( scrMtx )

    }

    bSMtxLst[["sScore0LAc24"]] <- function( phVP ,aZoidMtx ,makeInfoStr=F ){
        # phVP <- phVP.grp$phVPLst[[1]]
		aLen <- nrow(aZoidMtx)
        aObj <- phVP$getCodeW( aZoidMtx )

		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(bS.sScore0LAc24.cName) )	;colnames(scoreMtx) <- bS.sScore0LAc24.cName
        rownames(scoreMtx) <- aObj$miIdStr
		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c( "pvSubHLen" ,"pvSubName" )
			infoMtx <- matrix( "" ,nrow=aLen ,ncol=length(cName) )	;colnames(infoMtx) <- cName
			infoMtx[,"pvSubName"] <- aObj$miIdStr
            for( pvName in names(phVP$stdMILst) ){
                infoMtx[infoMtx[,"pvSubName"]==pvName ,"pvSubHLen"] <- phVP$stdMILst[[pvName]]$mtxLen
            }
		}
		if( 0==length(aObj$miIdStr) ){
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

        for( pvName in aObj$miNames ){  # pvName <- aObj$miNames[2]
            workArea <- which(aObj$miIdStr==pvName)
            stdMI<-phVP$stdMILst[[pvName]]
            wMI <- phVP$getCodeH( stdMI )
            scrMtx <- bSMtx.sScore0LAc24( stdMI=stdMI ,workArea ,wMI=wMI ,aObj ,aZoidMtx )
            scoreMtx[workArea,] <- scrMtx
        }

        return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
    }

}

if( TRUE ){ # "sScore0LVc13"

    # 주의사항 : bSMtx는 tailMtx 폭이 가면적이므로 
    #           "colA5","colB5" ,"colA6","colB6" 값은 없을 수도 있다.
	bS.sScore0LVc13.cName <- c(  "colA1","colA2","colA3","colA4","colA5","colA6"
					            ,"colB1","colB2","colB3","colB4","colB5","colB6"
    )

    bSMtx.sScore0LVc13 <- function( stdMI ,workArea ,wMI ,aObj ,aZoidMtx ){
		scrMtx <- matrix( 0, nrow=length(workArea), ncol=length(bS.sScore0LVc13.cName) )	;colnames(scrMtx) <- bS.sScore0LVc13.cName
        rownames(scrMtx) <- workArea
        stdMILen <- nrow(wMI$rawTail)
        if( 0==stdMILen ){
            return( scrMtx )
        }

        rObj <- list( lPtn1=NULL ,lPtn2=NULL ,colLen=ncol(wMI$cStepTail) )
        if( TRUE ){
            tailMtx <- wMI$cStepTail
            if( 1<stdMILen ){
                rObj$lPtn1 <- bUtil.findLinearPtn( tailMtx ,yIdx=nrow(tailMtx) ,typ="V" )
            }
            if( 3<stdMILen ){
                rObj$lPtn2 <- bUtil.findLinearPtn( tailMtx ,yIdx=nrow(tailMtx)-2 ,typ="V" )
            }
        }

        matCntA <- rep( 0 ,rObj$colLen )
        matCntB <- rep( 0 ,rObj$colLen )
        for( wIdx in seq_len(length(workArea)) ){
            aIdx <- workArea[wIdx]
            aCode <- aObj$aZoidMtx[aIdx,]       ;aRem <- aCode%%10
            aCStep <- aObj$cStepMtx[aIdx,]      ;aFStep <- aObj$fStepMtx[aIdx,]

            matCntA[] <- 0      ;matCntB[] <- 0
            if( !is.null(rObj$lPtn1) ){
                matCntA <- bUtil.checkMatch_LinearPtn( rObj$lPtn1 ,aCStep )
            }
            if( !is.null(rObj$lPtn2) ){
                matCntB <- bUtil.checkMatch_LinearPtn( rObj$lPtn2 ,aCStep )
            }

            scrMtx[wIdx,"colA1"] <- matCntA[1]
            scrMtx[wIdx,"colB1"] <- matCntB[1]
            scrMtx[wIdx,"colA2"] <- matCntA[2]
            scrMtx[wIdx,"colB2"] <- matCntB[2]
            if( 3<=rObj$colLen ){
                scrMtx[wIdx,"colA3"] <- matCntA[3]
                scrMtx[wIdx,"colB3"] <- matCntB[3]
            }
            if( 4<=rObj$colLen ){
                scrMtx[wIdx,"colA4"] <- matCntA[4]
                scrMtx[wIdx,"colB4"] <- matCntB[4]
            }
            if( 5<=rObj$colLen ){
                scrMtx[wIdx,"colA5"] <- matCntA[5]
                scrMtx[wIdx,"colB5"] <- matCntB[5]
            }
            if( 6<=rObj$colLen ){
                scrMtx[wIdx,"colA6"] <- matCntA[6]
                scrMtx[wIdx,"colB6"] <- matCntB[6]
            }

            # scrMtx[wIdx ,scrMtx[wIdx,]==1] <- 0
            # scrMtx[wIdx,"rebC.r"] <- sum(aCode==wMI$rawTail[stdMILen,])
            # scrMtx[wIdx,"rebC.c"] <- sum(aCStep==wMI$cStepTail[stdMILen,])
        }

        return( scrMtx )

    }

    bSMtxLst[["sScore0LVc13"]] <- function( phVP ,aZoidMtx ,makeInfoStr=F ){
        # phVP <- phVP.grp$phVPLst[[1]]
		aLen <- nrow(aZoidMtx)
        aObj <- phVP$getCodeW( aZoidMtx )

		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(bS.sScore0LVc13.cName) )	;colnames(scoreMtx) <- bS.sScore0LVc13.cName
        rownames(scoreMtx) <- aObj$miIdStr
		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c( "pvSubHLen" ,"pvSubName" )
			infoMtx <- matrix( "" ,nrow=aLen ,ncol=length(cName) )	;colnames(infoMtx) <- cName
			infoMtx[,"pvSubName"] <- aObj$miIdStr
            for( pvName in names(phVP$stdMILst) ){
                infoMtx[infoMtx[,"pvSubName"]==pvName ,"pvSubHLen"] <- phVP$stdMILst[[pvName]]$mtxLen
            }
		}
		if( 0==length(aObj$miIdStr) ){
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

        for( pvName in aObj$miNames ){  # pvName <- aObj$miNames[2]
            workArea <- which(aObj$miIdStr==pvName)
            stdMI<-phVP$stdMILst[[pvName]]
            wMI <- phVP$getCodeH( stdMI )
            scrMtx <- bSMtx.sScore0LVc13( stdMI=stdMI ,workArea ,wMI=wMI ,aObj ,aZoidMtx )
            scoreMtx[workArea,] <- scrMtx
        }

        return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
    }

}

if( TRUE ){ # "sScore0LVc24"

    # 주의사항 : bSMtx는 tailMtx 폭이 가면적이므로 
    #           "colA5","colB5" ,"colA6","colB6" 값은 없을 수도 있다.
	bS.sScore0LVc24.cName <- c(  "colA1","colA2","colA3","colA4","colA5","colA6"
					            ,"colB1","colB2","colB3","colB4","colB5","colB6"
    )

    bSMtx.sScore0LVc24 <- function( stdMI ,workArea ,wMI ,aObj ,aZoidMtx ){
		scrMtx <- matrix( 0, nrow=length(workArea), ncol=length(bS.sScore0LVc24.cName) )	;colnames(scrMtx) <- bS.sScore0LVc24.cName
        rownames(scrMtx) <- workArea
        stdMILen <- nrow(wMI$rawTail)
        if( 0==stdMILen ){
            return( scrMtx )
        }

        rObj <- list( lPtn1=NULL ,lPtn2=NULL ,colLen=ncol(wMI$cStepTail) )
        if( TRUE ){
            tailMtx <- wMI$cStepTail
            if( 2<stdMILen ){
                rObj$lPtn1 <- bUtil.findLinearPtn( tailMtx ,yIdx=nrow(tailMtx)-1 ,typ="V" )
            }
            if( 4<stdMILen ){
                rObj$lPtn2 <- bUtil.findLinearPtn( tailMtx ,yIdx=nrow(tailMtx)-3 ,typ="V" )
            }
        }

        matCntA <- rep( 0 ,rObj$colLen )
        matCntB <- rep( 0 ,rObj$colLen )
        for( wIdx in seq_len(length(workArea)) ){
            aIdx <- workArea[wIdx]
            aCode <- aObj$aZoidMtx[aIdx,]       ;aRem <- aCode%%10
            aCStep <- aObj$cStepMtx[aIdx,]      ;aFStep <- aObj$fStepMtx[aIdx,]

            matCntA[] <- 0      ;matCntB[] <- 0
            if( !is.null(rObj$lPtn1) ){
                matCntA <- bUtil.checkMatch_LinearPtn( rObj$lPtn1 ,aCStep )
            }
            if( !is.null(rObj$lPtn2) ){
                matCntB <- bUtil.checkMatch_LinearPtn( rObj$lPtn2 ,aCStep )
            }

            scrMtx[wIdx,"colA1"] <- matCntA[1]
            scrMtx[wIdx,"colB1"] <- matCntB[1]
            scrMtx[wIdx,"colA2"] <- matCntA[2]
            scrMtx[wIdx,"colB2"] <- matCntB[2]
            if( 3<=rObj$colLen ){
                scrMtx[wIdx,"colA3"] <- matCntA[3]
                scrMtx[wIdx,"colB3"] <- matCntB[3]
            }
            if( 4<=rObj$colLen ){
                scrMtx[wIdx,"colA4"] <- matCntA[4]
                scrMtx[wIdx,"colB4"] <- matCntB[4]
            }
            if( 5<=rObj$colLen ){
                scrMtx[wIdx,"colA5"] <- matCntA[5]
                scrMtx[wIdx,"colB5"] <- matCntB[5]
            }
            if( 6<=rObj$colLen ){
                scrMtx[wIdx,"colA6"] <- matCntA[6]
                scrMtx[wIdx,"colB6"] <- matCntB[6]
            }

            # scrMtx[wIdx ,scrMtx[wIdx,]==1] <- 0
            # scrMtx[wIdx,"rebC.r"] <- sum(aCode==wMI$rawTail[stdMILen,])
            # scrMtx[wIdx,"rebC.c"] <- sum(aCStep==wMI$cStepTail[stdMILen,])
        }

        return( scrMtx )

    }

    bSMtxLst[["sScore0LVc24"]] <- function( phVP ,aZoidMtx ,makeInfoStr=F ){
        # phVP <- phVP.grp$phVPLst[[1]]
		aLen <- nrow(aZoidMtx)
        aObj <- phVP$getCodeW( aZoidMtx )

		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(bS.sScore0LVc24.cName) )	;colnames(scoreMtx) <- bS.sScore0LVc24.cName
        rownames(scoreMtx) <- aObj$miIdStr
		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c( "pvSubHLen" ,"pvSubName" )
			infoMtx <- matrix( "" ,nrow=aLen ,ncol=length(cName) )	;colnames(infoMtx) <- cName
			infoMtx[,"pvSubName"] <- aObj$miIdStr
            for( pvName in names(phVP$stdMILst) ){
                infoMtx[infoMtx[,"pvSubName"]==pvName ,"pvSubHLen"] <- phVP$stdMILst[[pvName]]$mtxLen
            }
		}
		if( 0==length(aObj$miIdStr) ){
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

        for( pvName in aObj$miNames ){  # pvName <- aObj$miNames[2]
            workArea <- which(aObj$miIdStr==pvName)
            stdMI<-phVP$stdMILst[[pvName]]
            wMI <- phVP$getCodeH( stdMI )
            scrMtx <- bSMtx.sScore0LVc24( stdMI=stdMI ,workArea ,wMI=wMI ,aObj ,aZoidMtx )
            scoreMtx[workArea,] <- scrMtx
        }

        return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
    }

}


if( TRUE ){ # "sScore0LAf13"

    # 주의사항 : bSMtx는 tailMtx 폭이 가면적이므로 
    #           "colA5","colB5" ,"colA6","colB6" 값은 없을 수도 있다.
	bS.sScore0LAf13.cName <- c(  "colA1","colA2","colA3","colA4","colA5","colA6"
					            ,"colB1","colB2","colB3","colB4","colB5","colB6"
    )

    bSMtx.sScore0LAf13 <- function( stdMI ,workArea ,wMI ,aObj ,aZoidMtx ){
		scrMtx <- matrix( 0, nrow=length(workArea), ncol=length(bS.sScore0LAf13.cName) )	;colnames(scrMtx) <- bS.sScore0LAf13.cName
        rownames(scrMtx) <- workArea
        stdMILen <- nrow(wMI$rawTail)
        if( 3>stdMILen ){
            return( scrMtx )
        }

        rObj <- list( lPtn1=NULL ,lPtn2=NULL ,colLen=ncol(wMI$fStepTail) )
        if( TRUE ){
            tailMtx <- wMI$fStepTail
            if( is.na(tailMtx[1,1]) ){
                tailMtx <- tailMtx[2:nrow(tailMtx),,drop=F]
            }
            yIdxA <- nrow(tailMtx) -6 +1	# 바닥으로부터 자신 포함한 높이 6
            yIdxB <- nrow(tailMtx) -6 +3	# 바닥으로부터 자신 포함한 높이 4
            if( 0<yIdxA ){
                rObj$lPtn1 <- bUtil.findLinearPtn( tailMtx ,yIdx=yIdxA ,typ="A" )
            }
            if( 0<yIdxB ){
                rObj$lPtn2 <- bUtil.findLinearPtn( tailMtx ,yIdx=yIdxB ,typ="A" )
            }
        }

        matCntA <- rep( 0 ,rObj$colLen )
        matCntB <- rep( 0 ,rObj$colLen )
        for( wIdx in seq_len(length(workArea)) ){
            aIdx <- workArea[wIdx]
            aCode <- aObj$aZoidMtx[aIdx,]       ;aRem <- aCode%%10
            aCStep <- aObj$cStepMtx[aIdx,]      ;aFStep <- aObj$fStepMtx[aIdx,]

            matCntA[] <- 0      ;matCntB[] <- 0
            if( !is.null(rObj$lPtn1) ){
                matCntA <- bUtil.checkMatch_LinearPtn( rObj$lPtn1 ,aFStep )
            }
            if( !is.null(rObj$lPtn2) ){
                matCntB <- bUtil.checkMatch_LinearPtn( rObj$lPtn2 ,aFStep )
            }

            scrMtx[wIdx,"colA1"] <- matCntA[1]
            scrMtx[wIdx,"colB1"] <- matCntB[1]
            scrMtx[wIdx,"colA2"] <- matCntA[2]
            scrMtx[wIdx,"colB2"] <- matCntB[2]
            if( 3<=rObj$colLen ){
                scrMtx[wIdx,"colA3"] <- matCntA[3]
                scrMtx[wIdx,"colB3"] <- matCntB[3]
            }
            if( 4<=rObj$colLen ){
                scrMtx[wIdx,"colA4"] <- matCntA[4]
                scrMtx[wIdx,"colB4"] <- matCntB[4]
            }
            if( 5<=rObj$colLen ){
                scrMtx[wIdx,"colA5"] <- matCntA[5]
                scrMtx[wIdx,"colB5"] <- matCntB[5]
            }
            if( 6<=rObj$colLen ){
                scrMtx[wIdx,"colA6"] <- matCntA[6]
                scrMtx[wIdx,"colB6"] <- matCntB[6]
            }

            # scrMtx[wIdx ,scrMtx[wIdx,]==1] <- 0
            # scrMtx[wIdx,"rebC.r"] <- sum(aCode==wMI$rawTail[stdMILen,])
            # scrMtx[wIdx,"rebC.c"] <- sum(aCStep==wMI$cStepTail[stdMILen,])
        }

        return( scrMtx )

    }

    bSMtxLst[["sScore0LAf13"]] <- function( phVP ,aZoidMtx ,makeInfoStr=F ){
        # phVP <- phVP.grp$phVPLst[[1]]
		aLen <- nrow(aZoidMtx)
        aObj <- phVP$getCodeW( aZoidMtx )

		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(bS.sScore0LAf13.cName) )	;colnames(scoreMtx) <- bS.sScore0LAf13.cName
        rownames(scoreMtx) <- aObj$miIdStr
		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c( "pvSubHLen" ,"pvSubName" )
			infoMtx <- matrix( "" ,nrow=aLen ,ncol=length(cName) )	;colnames(infoMtx) <- cName
			infoMtx[,"pvSubName"] <- aObj$miIdStr
            for( pvName in names(phVP$stdMILst) ){
                infoMtx[infoMtx[,"pvSubName"]==pvName ,"pvSubHLen"] <- phVP$stdMILst[[pvName]]$mtxLen
            }
		}
		if( 0==length(aObj$miIdStr) ){
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

        for( pvName in aObj$miNames ){  # pvName <- aObj$miNames[2]
            workArea <- which(aObj$miIdStr==pvName)
            stdMI<-phVP$stdMILst[[pvName]]
            wMI <- phVP$getCodeH( stdMI )
            scrMtx <- bSMtx.sScore0LAf13( stdMI=stdMI ,workArea ,wMI=wMI ,aObj ,aZoidMtx )
            scoreMtx[workArea,] <- scrMtx
        }

        return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
    }

}

if( TRUE ){ # "sScore0LAf24"

    # 주의사항 : bSMtx는 tailMtx 폭이 가면적이므로 
    #           "colA5","colB5" ,"colA6","colB6" 값은 없을 수도 있다.
	bS.sScore0LAf24.cName <- c(  "colA1","colA2","colA3","colA4","colA5","colA6"
					            ,"colB1","colB2","colB3","colB4","colB5","colB6"
    )

    bSMtx.sScore0LAf24 <- function( stdMI ,workArea ,wMI ,aObj ,aZoidMtx ){
		scrMtx <- matrix( 0, nrow=length(workArea), ncol=length(bS.sScore0LAf24.cName) )	;colnames(scrMtx) <- bS.sScore0LAf24.cName
        rownames(scrMtx) <- workArea
        stdMILen <- nrow(wMI$rawTail)
        if( 3>stdMILen ){
            return( scrMtx )
        }

        rObj <- list( lPtn1=NULL ,lPtn2=NULL ,colLen=ncol(wMI$fStepTail) )
        if( TRUE ){
            tailMtx <- wMI$fStepTail
            if( is.na(tailMtx[1,1]) ){
                tailMtx <- tailMtx[2:nrow(tailMtx),,drop=F]
            }
            yIdxA <- nrow(tailMtx) -6 +2	# 바닥으로부터 자신 포함한 높이 6
            yIdxB <- nrow(tailMtx) -6 +4	# 바닥으로부터 자신 포함한 높이 4
            if( 0<yIdxA ){
                rObj$lPtn1 <- bUtil.findLinearPtn( tailMtx ,yIdx=yIdxA ,typ="A" )
            }
            if( 0<yIdxB ){
                rObj$lPtn2 <- bUtil.findLinearPtn( tailMtx ,yIdx=yIdxB ,typ="A" )
            }
        }

        matCntA <- rep( 0 ,rObj$colLen )
        matCntB <- rep( 0 ,rObj$colLen )
        for( wIdx in seq_len(length(workArea)) ){
            aIdx <- workArea[wIdx]
            aCode <- aObj$aZoidMtx[aIdx,]       ;aRem <- aCode%%10
            aCStep <- aObj$cStepMtx[aIdx,]      ;aFStep <- aObj$fStepMtx[aIdx,]

            matCntA[] <- 0      ;matCntB[] <- 0
            if( !is.null(rObj$lPtn1) ){
                matCntA <- bUtil.checkMatch_LinearPtn( rObj$lPtn1 ,aFStep )
            }
            if( !is.null(rObj$lPtn2) ){
                matCntB <- bUtil.checkMatch_LinearPtn( rObj$lPtn2 ,aFStep )
            }

            scrMtx[wIdx,"colA1"] <- matCntA[1]
            scrMtx[wIdx,"colB1"] <- matCntB[1]
            scrMtx[wIdx,"colA2"] <- matCntA[2]
            scrMtx[wIdx,"colB2"] <- matCntB[2]
            if( 3<=rObj$colLen ){
                scrMtx[wIdx,"colA3"] <- matCntA[3]
                scrMtx[wIdx,"colB3"] <- matCntB[3]
            }
            if( 4<=rObj$colLen ){
                scrMtx[wIdx,"colA4"] <- matCntA[4]
                scrMtx[wIdx,"colB4"] <- matCntB[4]
            }
            if( 5<=rObj$colLen ){
                scrMtx[wIdx,"colA5"] <- matCntA[5]
                scrMtx[wIdx,"colB5"] <- matCntB[5]
            }
            if( 6<=rObj$colLen ){
                scrMtx[wIdx,"colA6"] <- matCntA[6]
                scrMtx[wIdx,"colB6"] <- matCntB[6]
            }

            # scrMtx[wIdx ,scrMtx[wIdx,]==1] <- 0
            # scrMtx[wIdx,"rebC.r"] <- sum(aCode==wMI$rawTail[stdMILen,])
            # scrMtx[wIdx,"rebC.c"] <- sum(aCStep==wMI$cStepTail[stdMILen,])
        }

        return( scrMtx )

    }

    bSMtxLst[["sScore0LAf24"]] <- function( phVP ,aZoidMtx ,makeInfoStr=F ){
        # phVP <- phVP.grp$phVPLst[[1]]
		aLen <- nrow(aZoidMtx)
        aObj <- phVP$getCodeW( aZoidMtx )

		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(bS.sScore0LAf24.cName) )	;colnames(scoreMtx) <- bS.sScore0LAf24.cName
        rownames(scoreMtx) <- aObj$miIdStr
		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c( "pvSubHLen" ,"pvSubName" )
			infoMtx <- matrix( "" ,nrow=aLen ,ncol=length(cName) )	;colnames(infoMtx) <- cName
			infoMtx[,"pvSubName"] <- aObj$miIdStr
            for( pvName in names(phVP$stdMILst) ){
                infoMtx[infoMtx[,"pvSubName"]==pvName ,"pvSubHLen"] <- phVP$stdMILst[[pvName]]$mtxLen
            }
		}
		if( 0==length(aObj$miIdStr) ){
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

        for( pvName in aObj$miNames ){  # pvName <- aObj$miNames[2]
            workArea <- which(aObj$miIdStr==pvName)
            stdMI<-phVP$stdMILst[[pvName]]
            wMI <- phVP$getCodeH( stdMI )
            scrMtx <- bSMtx.sScore0LAf24( stdMI=stdMI ,workArea ,wMI=wMI ,aObj ,aZoidMtx )
            scoreMtx[workArea,] <- scrMtx
        }

        return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
    }

}

if( TRUE ){ # "sScore0LVf13"

    # 주의사항 : bSMtx는 tailMtx 폭이 가면적이므로 
    #           "colA5","colB5" ,"colA6","colB6" 값은 없을 수도 있다.
	bS.sScore0LVf13.cName <- c(  "colA1","colA2","colA3","colA4","colA5","colA6"
					            ,"colB1","colB2","colB3","colB4","colB5","colB6"
    )

    bSMtx.sScore0LVf13 <- function( stdMI ,workArea ,wMI ,aObj ,aZoidMtx ){
		scrMtx <- matrix( 0, nrow=length(workArea), ncol=length(bS.sScore0LVf13.cName) )	;colnames(scrMtx) <- bS.sScore0LVf13.cName
        rownames(scrMtx) <- workArea
        stdMILen <- nrow(wMI$rawTail)
        if( 3>stdMILen ){
            return( scrMtx )
        }

        rObj <- list( lPtn1=NULL ,lPtn2=NULL ,colLen=ncol(wMI$fStepTail) )
        if( TRUE ){
            tailMtx <- wMI$fStepTail
            if( is.na(tailMtx[1,1]) ){
                tailMtx <- tailMtx[2:nrow(tailMtx),,drop=F]
            }

            if( 1<stdMILen ){
                rObj$lPtn1 <- bUtil.findLinearPtn( tailMtx ,yIdx=nrow(tailMtx) ,typ="V" )
            }
            if( 3<stdMILen ){
                rObj$lPtn2 <- bUtil.findLinearPtn( tailMtx ,yIdx=nrow(tailMtx)-2 ,typ="V" )
            }
        }

        matCntA <- rep( 0 ,rObj$colLen )
        matCntB <- rep( 0 ,rObj$colLen )
        for( wIdx in seq_len(length(workArea)) ){
            aIdx <- workArea[wIdx]
            aCode <- aObj$aZoidMtx[aIdx,]       ;aRem <- aCode%%10
            aCStep <- aObj$cStepMtx[aIdx,]      ;aFStep <- aObj$fStepMtx[aIdx,]

            matCntA[] <- 0      ;matCntB[] <- 0
            if( !is.null(rObj$lPtn1) ){
                matCntA <- bUtil.checkMatch_LinearPtn( rObj$lPtn1 ,aFStep )
            }
            if( !is.null(rObj$lPtn2) ){
                matCntB <- bUtil.checkMatch_LinearPtn( rObj$lPtn2 ,aFStep )
            }

            scrMtx[wIdx,"colA1"] <- matCntA[1]
            scrMtx[wIdx,"colB1"] <- matCntB[1]
            scrMtx[wIdx,"colA2"] <- matCntA[2]
            scrMtx[wIdx,"colB2"] <- matCntB[2]
            if( 3<=rObj$colLen ){
                scrMtx[wIdx,"colA3"] <- matCntA[3]
                scrMtx[wIdx,"colB3"] <- matCntB[3]
            }
            if( 4<=rObj$colLen ){
                scrMtx[wIdx,"colA4"] <- matCntA[4]
                scrMtx[wIdx,"colB4"] <- matCntB[4]
            }
            if( 5<=rObj$colLen ){
                scrMtx[wIdx,"colA5"] <- matCntA[5]
                scrMtx[wIdx,"colB5"] <- matCntB[5]
            }
            if( 6<=rObj$colLen ){
                scrMtx[wIdx,"colA6"] <- matCntA[6]
                scrMtx[wIdx,"colB6"] <- matCntB[6]
            }

            # scrMtx[wIdx ,scrMtx[wIdx,]==1] <- 0
            # scrMtx[wIdx,"rebC.r"] <- sum(aCode==wMI$rawTail[stdMILen,])
            # scrMtx[wIdx,"rebC.c"] <- sum(aCStep==wMI$cStepTail[stdMILen,])
        }

        return( scrMtx )

    }

    bSMtxLst[["sScore0LVf13"]] <- function( phVP ,aZoidMtx ,makeInfoStr=F ){
        # phVP <- phVP.grp$phVPLst[[1]]
		aLen <- nrow(aZoidMtx)
        aObj <- phVP$getCodeW( aZoidMtx )

		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(bS.sScore0LVf13.cName) )	;colnames(scoreMtx) <- bS.sScore0LVf13.cName
        rownames(scoreMtx) <- aObj$miIdStr
		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c( "pvSubHLen" ,"pvSubName" )
			infoMtx <- matrix( "" ,nrow=aLen ,ncol=length(cName) )	;colnames(infoMtx) <- cName
			infoMtx[,"pvSubName"] <- aObj$miIdStr
            for( pvName in names(phVP$stdMILst) ){
                infoMtx[infoMtx[,"pvSubName"]==pvName ,"pvSubHLen"] <- phVP$stdMILst[[pvName]]$mtxLen
            }
		}
		if( 0==length(aObj$miIdStr) ){
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

        for( pvName in aObj$miNames ){  # pvName <- aObj$miNames[2]
            workArea <- which(aObj$miIdStr==pvName)
            stdMI<-phVP$stdMILst[[pvName]]
            wMI <- phVP$getCodeH( stdMI )
            scrMtx <- bSMtx.sScore0LVf13( stdMI=stdMI ,workArea ,wMI=wMI ,aObj ,aZoidMtx )
            scoreMtx[workArea,] <- scrMtx
        }

        return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
    }

}

if( TRUE ){ # "sScore0LVf24"

    # 주의사항 : bSMtx는 tailMtx 폭이 가면적이므로 
    #           "colA5","colB5" ,"colA6","colB6" 값은 없을 수도 있다.
	bS.sScore0LVf24.cName <- c(  "colA1","colA2","colA3","colA4","colA5","colA6"
					            ,"colB1","colB2","colB3","colB4","colB5","colB6"
    )

    bSMtx.sScore0LVf24 <- function( stdMI ,workArea ,wMI ,aObj ,aZoidMtx ){
		scrMtx <- matrix( 0, nrow=length(workArea), ncol=length(bS.sScore0LVf24.cName) )	;colnames(scrMtx) <- bS.sScore0LVf24.cName
        rownames(scrMtx) <- workArea
        stdMILen <- nrow(wMI$rawTail)
        if( 3>stdMILen ){
            return( scrMtx )
        }

        rObj <- list( lPtn1=NULL ,lPtn2=NULL ,colLen=ncol(wMI$fStepTail) )
        if( TRUE ){
            tailMtx <- wMI$fStepTail
            if( is.na(tailMtx[1,1]) ){
                tailMtx <- tailMtx[2:nrow(tailMtx),,drop=F]
            }
            if( 2<stdMILen ){
                rObj$lPtn1 <- bUtil.findLinearPtn( tailMtx ,yIdx=nrow(tailMtx)-1 ,typ="V" )
            }
            if( 4<stdMILen ){
                rObj$lPtn2 <- bUtil.findLinearPtn( tailMtx ,yIdx=nrow(tailMtx)-3 ,typ="V" )
            }
        }

        matCntA <- rep( 0 ,rObj$colLen )
        matCntB <- rep( 0 ,rObj$colLen )
        for( wIdx in seq_len(length(workArea)) ){
            aIdx <- workArea[wIdx]
            aCode <- aObj$aZoidMtx[aIdx,]       ;aRem <- aCode%%10
            aCStep <- aObj$cStepMtx[aIdx,]      ;aFStep <- aObj$fStepMtx[aIdx,]

            matCntA[] <- 0      ;matCntB[] <- 0
            if( !is.null(rObj$lPtn1) ){
                matCntA <- bUtil.checkMatch_LinearPtn( rObj$lPtn1 ,aFStep )
            }
            if( !is.null(rObj$lPtn2) ){
                matCntB <- bUtil.checkMatch_LinearPtn( rObj$lPtn2 ,aFStep )
            }

            scrMtx[wIdx,"colA1"] <- matCntA[1]
            scrMtx[wIdx,"colB1"] <- matCntB[1]
            scrMtx[wIdx,"colA2"] <- matCntA[2]
            scrMtx[wIdx,"colB2"] <- matCntB[2]
            if( 3<=rObj$colLen ){
                scrMtx[wIdx,"colA3"] <- matCntA[3]
                scrMtx[wIdx,"colB3"] <- matCntB[3]
            }
            if( 4<=rObj$colLen ){
                scrMtx[wIdx,"colA4"] <- matCntA[4]
                scrMtx[wIdx,"colB4"] <- matCntB[4]
            }
            if( 5<=rObj$colLen ){
                scrMtx[wIdx,"colA5"] <- matCntA[5]
                scrMtx[wIdx,"colB5"] <- matCntB[5]
            }
            if( 6<=rObj$colLen ){
                scrMtx[wIdx,"colA6"] <- matCntA[6]
                scrMtx[wIdx,"colB6"] <- matCntB[6]
            }

            # scrMtx[wIdx ,scrMtx[wIdx,]==1] <- 0
            # scrMtx[wIdx,"rebC.r"] <- sum(aCode==wMI$rawTail[stdMILen,])
            # scrMtx[wIdx,"rebC.c"] <- sum(aCStep==wMI$cStepTail[stdMILen,])
        }

        return( scrMtx )

    }

    bSMtxLst[["sScore0LVf24"]] <- function( phVP ,aZoidMtx ,makeInfoStr=F ){
        # phVP <- phVP.grp$phVPLst[[1]]
		aLen <- nrow(aZoidMtx)
        aObj <- phVP$getCodeW( aZoidMtx )

		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(bS.sScore0LVf24.cName) )	;colnames(scoreMtx) <- bS.sScore0LVf24.cName
        rownames(scoreMtx) <- aObj$miIdStr
		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c( "pvSubHLen" ,"pvSubName" )
			infoMtx <- matrix( "" ,nrow=aLen ,ncol=length(cName) )	;colnames(infoMtx) <- cName
			infoMtx[,"pvSubName"] <- aObj$miIdStr
            for( pvName in names(phVP$stdMILst) ){
                infoMtx[infoMtx[,"pvSubName"]==pvName ,"pvSubHLen"] <- phVP$stdMILst[[pvName]]$mtxLen
            }
		}
		if( 0==length(aObj$miIdStr) ){
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

        for( pvName in aObj$miNames ){  # pvName <- aObj$miNames[2]
            workArea <- which(aObj$miIdStr==pvName)
            stdMI<-phVP$stdMILst[[pvName]]
            wMI <- phVP$getCodeH( stdMI )
            scrMtx <- bSMtx.sScore0LVf24( stdMI=stdMI ,workArea ,wMI=wMI ,aObj ,aZoidMtx )
            scoreMtx[workArea,] <- scrMtx
        }

        return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
    }

}

if( TRUE ){ # sScore0GS
	bS.sScore0GS.cName <- c( "rMatCnt","rExtMax","rSumCnt","rValCnt" ,"eMatCnt","eExtMax","eSumCnt","eValCnt" 
                            ,"cMatCnt","cExtMax","cSumCnt","cValCnt"
                            ,"fMatCnt","fExtMax","fSumCnt","fValCnt"
    )
    bSMtx.sScore0GS <- function( stdMI ,workArea ,wMI ,aObj ,aZoidMtx ){
		scrMtx <- matrix( 0, nrow=length(workArea), ncol=length(bS.sScore0GS.cName) )	;colnames(scrMtx) <- bS.sScore0GS.cName
        rownames(scrMtx) <- workArea
        stdMILen <- nrow(wMI$rawTail)
        if( 1>stdMILen || 0>length(workArea) ){
            return( scrMtx )
        }

        mObj <- list( available=FALSE )
        if( TRUE ){ # mObj$gsLst
            tailLen <- nrow(wMI$rawTail)

            gsLst <- list()
            # raw ---------------------------------------------------------------
            gsObj <- bUtil.scoreGS( wMI$rawTail[tailLen,] )
            gsLst[["r"]] <- gsObj
            # rem ---------------------------------------------------------------
            codeMtx <- wMI$rawTail %% 10
            gsObj <- bUtil.scoreGS( codeMtx[tailLen,] )
            gsLst[["e"]] <- gsObj
            # cStep -------------------------------------------------------------
            gsObj <- bUtil.scoreGS( wMI$cStepTail[tailLen,] )
            gsLst[["c"]] <- gsObj
            # fStep -------------------------------------------------------------
            if( 1<tailLen ){
                gsObj <- bUtil.scoreGS( wMI$fStepTail[tailLen,] )
                gsLst[["f"]] <- gsObj
            }

            mObj$gsLst <- gsLst
            mObj$available <- TRUE
        }

        for( wIdx in seq_len(length(workArea)) ){
            aIdx <- workArea[wIdx]
            aCode <- aObj$aZoidMtx[aIdx,]       ;aRem <- aCode%%10
            aCStep <- aObj$cStepMtx[aIdx,]      ;aFStep <- aObj$fStepMtx[aIdx,]

			if( !is.null(mObj$gsLst[["r"]]) ){	# raw
				matLst <- mObj$gsLst[["r"]]$check( aCode )
				scrMtx[wIdx,"rMatCnt"] <- length(matLst)
				for( idx in seq_len(scrMtx[wIdx,"rMatCnt"]) ){
					matInfo <- matLst[[idx]]$matInfo	# sumMatFlg valMatCnt  valMatCntIdx fndLst_RowIdx
					scrMtx[wIdx,"rExtMax"] <- max( scrMtx[wIdx,"rExtMax"] ,sum(matInfo>0) )
					scrMtx[wIdx,"rSumCnt"] <- scrMtx[wIdx,"rSumCnt"] + matInfo["sumMatFlg"]
					scrMtx[wIdx,"rValCnt"] <- scrMtx[wIdx,"rValCnt"] + matInfo["valMatCnt"]
				}
			}
			if( !is.null(mObj$gsLst[["e"]]) ){	# rem
				matLst <- mObj$gsLst[["e"]]$check( aRem )
				scrMtx[wIdx,"eMatCnt"] <- length(matLst)
				for( idx in seq_len(scrMtx[wIdx,"eMatCnt"]) ){
					matInfo <- matLst[[idx]]$matInfo	# sumMatFlg valMatCnt  valMatCntIdx fndLst_RowIdx
					scrMtx[wIdx,"eExtMax"] <- max( scrMtx[wIdx,"eExtMax"] ,sum(matInfo>0) )
					scrMtx[wIdx,"eSumCnt"] <- scrMtx[wIdx,"eSumCnt"] + matInfo["sumMatFlg"]
					scrMtx[wIdx,"eValCnt"] <- scrMtx[wIdx,"eValCnt"] + matInfo["valMatCnt"]
				}
			}
			if( !is.null(mObj$gsLst[["c"]]) ){	# cStep
				matLst <- mObj$gsLst[["c"]]$check( aCStep )
				scrMtx[wIdx,"cMatCnt"] <- length(matLst)
				for( idx in seq_len(scrMtx[wIdx,"cMatCnt"]) ){
					matInfo <- matLst[[idx]]$matInfo	# sumMatFlg valMatCnt  valMatCntIdx fndLst_RowIdx
					scrMtx[wIdx,"cExtMax"] <- max( scrMtx[wIdx,"cExtMax"] ,sum(matInfo>0) )
					scrMtx[wIdx,"cSumCnt"] <- scrMtx[wIdx,"cSumCnt"] + matInfo["sumMatFlg"]
					scrMtx[wIdx,"cValCnt"] <- scrMtx[wIdx,"cValCnt"] + matInfo["valMatCnt"]
				}
			}
			if( !is.null(mObj$gsLst[["f"]]) ){	# fStep
				matLst <- mObj$gsLst[["f"]]$check( aFStep )
				scrMtx[wIdx,"fMatCnt"] <- length(matLst)
				for( idx in seq_len(scrMtx[wIdx,"fMatCnt"]) ){
					matInfo <- matLst[[idx]]$matInfo	# sumMatFlg valMatCnt  valMatCntIdx fndLst_RowIdx
					scrMtx[wIdx,"fExtMax"] <- max( scrMtx[wIdx,"fExtMax"] ,sum(matInfo>0) )
					scrMtx[wIdx,"fSumCnt"] <- scrMtx[wIdx,"fSumCnt"] + matInfo["sumMatFlg"]
					scrMtx[wIdx,"fValCnt"] <- scrMtx[wIdx,"fValCnt"] + matInfo["valMatCnt"]
				}
			}

            # scrMtx[wIdx ,scrMtx[wIdx,]==1] <- 0
            # scrMtx[wIdx,"rebC.r"] <- sum(aCode==wMI$rawTail[stdMILen,])
            # scrMtx[wIdx,"rebC.c"] <- sum(aCStep==wMI$cStepTail[stdMILen,])
        }

        return( scrMtx )

    }
    bSMtxLst[["sScore0GS"]] <- function( phVP ,aZoidMtx ,makeInfoStr=F ){
        # phVP <- phVP.grp$phVPLst[[1]]
		aLen <- nrow(aZoidMtx)
        aObj <- phVP$getCodeW( aZoidMtx )

		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(bS.sScore0GS.cName) )	;colnames(scoreMtx) <- bS.sScore0GS.cName
        rownames(scoreMtx) <- aObj$miIdStr
		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c( "pvSubHLen" ,"pvSubName" )
			infoMtx <- matrix( "" ,nrow=aLen ,ncol=length(cName) )	;colnames(infoMtx) <- cName
			infoMtx[,"pvSubName"] <- aObj$miIdStr
            for( pvName in names(phVP$stdMILst) ){
                infoMtx[infoMtx[,"pvSubName"]==pvName ,"pvSubHLen"] <- phVP$stdMILst[[pvName]]$mtxLen
            }
		}
		if( 0==length(aObj$miIdStr) ){
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

        for( pvName in aObj$miNames ){  # pvName <- aObj$miNames[2]
            workArea <- which(aObj$miIdStr==pvName)
            stdMI<-phVP$stdMILst[[pvName]]
            wMI <- phVP$getCodeH( stdMI )
            scrMtx <- bSMtx.sScore0GS( stdMI=stdMI ,workArea ,wMI=wMI ,aObj ,aZoidMtx )
            scoreMtx[workArea,] <- scrMtx
        }

        return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
    }
}
if( TRUE ){ # sScore0GS3
	bS.sScore0GS3.cName <- c( "rMatCntH1","rMatCntH2","rRebMaxH1","rRebMaxH2" ,"eMatCntH1","eMatCntH2","eRebMaxH1","eRebMaxH2"
                            ,"cMatCntH1","cMatCntH2","cRebMaxH1","cRebMaxH2"
                            ,"fMatCntH1","fMatCntH2","fRebMaxH1","fRebMaxH2"
    )
    bSMtx.sScore0GS3 <- function( stdMI ,workArea ,wMI ,aObj ,aZoidMtx ){
		scrMtx <- matrix( 0, nrow=length(workArea), ncol=length(bS.sScore0GS3.cName) )	;colnames(scrMtx) <- bS.sScore0GS3.cName
        rownames(scrMtx) <- workArea
        stdMILen <- nrow(wMI$rawTail)
        if( 1>stdMILen || 0>length(workArea) ){
            return( scrMtx )
        }

        mObj <- list( available=FALSE )
        if( TRUE ){ # mObj$gsLst
            tailLen <- nrow(wMI$rawTail)

            gsLst <- list()

            # raw ---------------------------------------------------------------
            gsLst[["r"]] <- bUtil.scoreGS3( wMI$rawTail )
            # rem ---------------------------------------------------------------
            codeMtx <- wMI$rawTail %% 10
            gsLst[["e"]] <- bUtil.scoreGS3( codeMtx )
            # cStep -------------------------------------------------------------
            gsLst[["c"]] <- bUtil.scoreGS3( wMI$cStepTail )
            # fStep -------------------------------------------------------------
            if( 1<tailLen ){
                fStepTail <- wMI$fStepTail
                if( is.na(fStepTail[1,1]) ){
                    fStepTail <- fStepTail[2:tailLen,,drop=F]
                }
                gsLst[["f"]] <- bUtil.scoreGS3( fStepTail )
            }

            mObj$gsLst <- gsLst
            mObj$available <- TRUE
        }

        for( wIdx in seq_len(length(workArea)) ){
            aIdx <- workArea[wIdx]
            aCode <- aObj$aZoidMtx[aIdx,]       ;aRem <- aCode%%10
            aCStep <- aObj$cStepMtx[aIdx,]      ;aFStep <- aObj$fStepMtx[aIdx,]

			if( !is.null(gsLst[["r"]]) ){
				chkRst <- gsLst[["r"]]$check( aCode )
				scrMtx[wIdx,"rMatCntH1"]	<- chkRst$h1["matCnt"]
				scrMtx[wIdx,"rMatCntH2"]	<- chkRst$h2["matCnt"]
				scrMtx[wIdx,"rRebMaxH1"]	<- chkRst$h1["valRebMax"]
				scrMtx[wIdx,"rRebMaxH2"]	<- chkRst$h2["valRebMax"]
			}
			if( !is.null(gsLst[["e"]]) ){
				chkRst <- gsLst[["e"]]$check( aRem )
				scrMtx[wIdx,"eMatCntH1"]	<- chkRst$h1["matCnt"]
				scrMtx[wIdx,"eMatCntH2"]	<- chkRst$h2["matCnt"]
				scrMtx[wIdx,"eRebMaxH1"]	<- chkRst$h1["valRebMax"]
				scrMtx[wIdx,"eRebMaxH2"]	<- chkRst$h2["valRebMax"]
			}
			if( !is.null(gsLst[["c"]]) ){
				chkRst <- gsLst[["c"]]$check( aCStep )
				scrMtx[wIdx,"cMatCntH1"]	<- chkRst$h1["matCnt"]
				scrMtx[wIdx,"cMatCntH2"]	<- chkRst$h2["matCnt"]
				scrMtx[wIdx,"cRebMaxH1"]	<- chkRst$h1["valRebMax"]
				scrMtx[wIdx,"cRebMaxH2"]	<- chkRst$h2["valRebMax"]
			}
			if( !is.null(gsLst[["f"]]) ){
				chkRst <- gsLst[["f"]]$check( aFStep )
				scrMtx[wIdx,"fMatCntH1"]	<- chkRst$h1["matCnt"]
				scrMtx[wIdx,"fMatCntH2"]	<- chkRst$h2["matCnt"]
				scrMtx[wIdx,"fRebMaxH1"]	<- chkRst$h1["valRebMax"]
				scrMtx[wIdx,"fRebMaxH2"]	<- chkRst$h2["valRebMax"]
			}

            # scrMtx[wIdx ,scrMtx[wIdx,]==1] <- 0
            # scrMtx[wIdx,"rebC.r"] <- sum(aCode==wMI$rawTail[stdMILen,])
            # scrMtx[wIdx,"rebC.c"] <- sum(aCStep==wMI$cStepTail[stdMILen,])
        }

        return( scrMtx )
    }
    bSMtxLst[["sScore0GS3"]] <- function( phVP ,aZoidMtx ,makeInfoStr=F ){
        # phVP <- phVP.grp$phVPLst[[1]]
		aLen <- nrow(aZoidMtx)
        aObj <- phVP$getCodeW( aZoidMtx )

		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(bS.sScore0GS3.cName) )	;colnames(scoreMtx) <- bS.sScore0GS3.cName
        rownames(scoreMtx) <- aObj$miIdStr
		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c( "pvSubHLen" ,"pvSubName" )
			infoMtx <- matrix( "" ,nrow=aLen ,ncol=length(cName) )	;colnames(infoMtx) <- cName
			infoMtx[,"pvSubName"] <- aObj$miIdStr
            for( pvName in names(phVP$stdMILst) ){
                infoMtx[infoMtx[,"pvSubName"]==pvName ,"pvSubHLen"] <- phVP$stdMILst[[pvName]]$mtxLen
            }
		}
		if( 0==length(aObj$miIdStr) ){
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

        for( pvName in aObj$miNames ){  # pvName <- aObj$miNames[2]
            workArea <- which(aObj$miIdStr==pvName)
            stdMI<-phVP$stdMILst[[pvName]]
            wMI <- phVP$getCodeH( stdMI )
            scrMtx <- bSMtx.sScore0GS3( stdMI=stdMI ,workArea ,wMI=wMI ,aObj ,aZoidMtx )
            scoreMtx[workArea,] <- scrMtx
        }

        return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
    }
}

if( TRUE ){ # sScore0PSh
	bS.sScore0PSh.cName <- c(   "rSeq0" ,"rSeq1" ,"rSeqN","rNSeq" ,"rSyc0" ,"rSyc1" ,"rColCnt" ,"eSeq0" ,"eSeq1" ,"eSeqN","eNSeq" ,"eSyc0" ,"eSyc1" ,"eColCnt"
                                ,"cSeq0" ,"cSeq1" ,"cSeqN","cNSeq" ,"cSyc0" ,"cSyc1" ,"cColCnt"
                                ,"fSeq0" ,"fSeq1" ,"fSeqN","fNSeq" ,"fSyc0" ,"fSyc1" ,"fColCnt"
    )
    bSMtx.sScore0PSh <- function( stdMI ,workArea ,wMI ,aObj ,aZoidMtx ){
		scrMtx <- matrix( 0, nrow=length(workArea), ncol=length(bS.sScore0PSh.cName) )	;colnames(scrMtx) <- bS.sScore0PSh.cName
        rownames(scrMtx) <- workArea
        stdMILen <- nrow(wMI$rawTail)
        if( 2>stdMILen || 0>length(workArea) ){
            return( scrMtx )
        }

        mObj <- list( available=FALSE )
        tailLen <- nrow(wMI$rawTail)
        if( TRUE ){ # mObj$psLst

            psLst <- list()

            psObj <- bUtil.scorePSh( wMI$rawTail )
            if( 0<length(psObj$chkLst) ){
                psLst[["r"]] <- psObj
            }

            psObj <- bUtil.scorePSh( wMI$rawTail%%10 )
            if( 0<length(psObj$chkLst) ){
                psLst[["e"]] <- psObj
            }

            psObj <- bUtil.scorePSh( wMI$cStepTail )
            if( 0<length(psObj$chkLst) ){
                psLst[["c"]] <- psObj
            }

            fStepMtx <- wMI$fStepTail
            fStepMtx <- fStepMtx[!is.na(fStepMtx[,1]),,drop=F]
            psObj <- bUtil.scorePSh( fStepMtx )
            if( 0<length(psObj$chkLst) ){
                psLst[["f"]] <- psObj
            }

            if( 0<length(psLst) ){
                mObj$psLst <- psLst
                mObj$available <- TRUE
            }
        }

        mObj$scoreFromMatLst <- function( matLst ){
            # seq0 seq1 seqN nSeq syc0 syc1
            score <- c( seq0=0 ,seq1=0 ,seqN=0 ,nSeq=0 ,syc0=0 ,syc1=0 ,colCnt=0 )

            colHpnF <- rep( F ,50 )		# 속도 땜시... 설마 50 이상 쓸 일은 없겠지.
            for( nIdx in names(matLst) ){
                score[nIdx] <- nrow(matLst[[nIdx]])
                colHpnF[ matLst[[nIdx]]$idx ] <- TRUE
            }
            score["colCnt"] <- sum(colHpnF)

            return( score )
        }

		sumMtxLst <- list()
		if( !is.null(mObj$psLst[["r"]]) ){
			sumMtxLst[["r"]] <- mObj$psLst[["r"]]$getSumMtx( aObj$aZoidMtx[workArea,,drop=F] )
		}
		if( !is.null(mObj$psLst[["e"]]) ){
			sumMtxLst[["e"]] <- mObj$psLst[["e"]]$getSumMtx( aObj$aZoidMtx[workArea,,drop=F]%%10 )
		}
		if( !is.null(mObj$psLst[["c"]]) ){
			sumMtxLst[["c"]] <- mObj$psLst[["c"]]$getSumMtx( aObj$cStepMtx[workArea,,drop=F] )
		}
		if( !is.null(mObj$psLst[["f"]]) ){
			sumMtxLst[["f"]] <- mObj$psLst[["f"]]$getSumMtx( aObj$fStepMtx[workArea,,drop=F] )
		}

        for( wIdx in seq_len(length(workArea)) ){
            # sumMtxLst 자체가 이미 workArea에서 추려낸 데이터라, aIdx 적용필요가 없다.
            #   aIdx <- workArea[wIdx]
            #   aCode <- aObj$aZoidMtx[aIdx,]       ;aRem <- aCode%%10
            #   aCStep <- aObj$cStepMtx[aIdx,]      ;aFStep <- aObj$fStepMtx[aIdx,]

			if( !is.null(mObj$psLst[["r"]]) ){
				matLst <- mObj$psLst[["r"]]$check( sumMtxLst[["r"]][wIdx,] )
				score <- mObj$scoreFromMatLst( matLst )
				scrMtx[wIdx ,c("rSeq0" ,"rSeq1" ,"rSeqN")] <- score[c("seq0" ,"seq1" ,"seqN")]
				scrMtx[wIdx ,c("rNSeq" ,"rSyc0" ,"rSyc1" ,"rColCnt")] <- score[c("nSeq" ,"syc0" ,"syc1" ,"colCnt")]
			}
			if( !is.null(mObj$psLst[["e"]]) ){
				matLst <- mObj$psLst[["e"]]$check( sumMtxLst[["e"]][wIdx,] )
				score <- mObj$scoreFromMatLst( matLst )
				scrMtx[wIdx ,c("eSeq0" ,"eSeq1" ,"eSeqN")] <- score[c("seq0" ,"seq1" ,"seqN")]
				scrMtx[wIdx ,c("eNSeq" ,"eSyc0" ,"eSyc1" ,"eColCnt")] <- score[c("nSeq" ,"syc0" ,"syc1" ,"colCnt")]
			}
			if( !is.null(mObj$psLst[["c"]]) ){
				matLst <- mObj$psLst[["c"]]$check( sumMtxLst[["c"]][wIdx,] )
				score <- mObj$scoreFromMatLst( matLst )
				scrMtx[wIdx ,c("cSeq0" ,"cSeq1" ,"cSeqN")] <- score[c("seq0" ,"seq1" ,"seqN")]
				scrMtx[wIdx ,c("cNSeq" ,"cSyc0" ,"cSyc1" ,"cColCnt")] <- score[c("nSeq" ,"syc0" ,"syc1" ,"colCnt")]
			}
			if( !is.null(mObj$psLst[["f"]]) ){
				matLst <- mObj$psLst[["f"]]$check( sumMtxLst[["f"]][wIdx,] )
				score <- mObj$scoreFromMatLst( matLst )
				scrMtx[wIdx ,c("fSeq0" ,"fSeq1" ,"fSeqN")] <- score[c("seq0" ,"seq1" ,"seqN")]
				scrMtx[wIdx ,c("fNSeq" ,"fSyc0" ,"fSyc1" ,"fColCnt")] <- score[c("nSeq" ,"syc0" ,"syc1" ,"colCnt")]
			}

            # scrMtx[wIdx ,scrMtx[wIdx,]==1] <- 0
            # scrMtx[wIdx,"rebC.r"] <- sum(aCode==wMI$rawTail[stdMILen,])
            # scrMtx[wIdx,"rebC.c"] <- sum(aCStep==wMI$cStepTail[stdMILen,])
        }

        return( scrMtx )

    }
    bSMtxLst[["sScore0PSh"]] <- function( phVP ,aZoidMtx ,makeInfoStr=F ){
        # phVP <- phVP.grp$phVPLst[[1]]
		aLen <- nrow(aZoidMtx)
        aObj <- phVP$getCodeW( aZoidMtx )

		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(bS.sScore0PSh.cName) )	;colnames(scoreMtx) <- bS.sScore0PSh.cName
        rownames(scoreMtx) <- aObj$miIdStr
		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c( "pvSubHLen" ,"pvSubName" )
			infoMtx <- matrix( "" ,nrow=aLen ,ncol=length(cName) )	;colnames(infoMtx) <- cName
			infoMtx[,"pvSubName"] <- aObj$miIdStr
            for( pvName in names(phVP$stdMILst) ){
                infoMtx[infoMtx[,"pvSubName"]==pvName ,"pvSubHLen"] <- phVP$stdMILst[[pvName]]$mtxLen
            }
		}
		if( 0==length(aObj$miIdStr) ){
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

        for( pvName in aObj$miNames ){  # pvName <- aObj$miNames[2]
            workArea <- which(aObj$miIdStr==pvName)
            stdMI<-phVP$stdMILst[[pvName]]
            wMI <- phVP$getCodeH( stdMI )
            scrMtx <- bSMtx.sScore0PSh( stdMI=stdMI ,workArea ,wMI=wMI ,aObj ,aZoidMtx )
            scoreMtx[workArea,] <- scrMtx
        }

        return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
    }
}

if( TRUE ){	# sScore0PSrp       reb ptn(idx only? or even val reb?)	score3참고
	bS.sScore0PSrp.cName <- c(  "r1TotSize" ,"r1ValSize" ,"r2TotSize" ,"r2ValSize" ,"e1TotSize" ,"e1ValSize" ,"e2TotSize" ,"e2ValSize"
                                ,"c1TotSize" ,"c1ValSize" ,"c2TotSize" ,"c2ValSize"
                                ,"f1TotSize" ,"f1ValSize" ,"f2TotSize" ,"f2ValSize"
    )
    bSMtx.sScore0PSrp <- function( stdMI ,workArea ,wMI ,aObj ,aZoidMtx ){
		scrMtx <- matrix( 0, nrow=length(workArea), ncol=length(bS.sScore0PSrp.cName) )	;colnames(scrMtx) <- bS.sScore0PSrp.cName
        rownames(scrMtx) <- workArea
        stdMILen <- nrow(wMI$rawTail)
        if( 2>stdMILen || 0>length(workArea) ){
            return( scrMtx )
        }

        mObj <- list( available=FALSE )
        tailLen <- nrow(wMI$rawTail)
        if( TRUE ){ # mObj$psLst

            psLst <- list()

            psLst[["r"]] <- bUtil.scorePSrp( wMI$rawTail )
            psLst[["e"]] <- bUtil.scorePSrp( wMI$rawTail%%10 )
            psLst[["c"]] <- bUtil.scorePSrp( wMI$cStepTail )
            if( 2<tailLen ){
                fStepTail <- wMI$fStepTail
                if( is.na(fStepTail[1,1]) ){
                    fStepTail <- fStepTail[2:tailLen,,drop=F]
                }
                psLst[["f"]] <- bUtil.scorePSrp( fStepTail )
            }
            mObj$psLst <- psLst
            mObj$available <- TRUE
        }

        mObj$scoreFromMatLst <- function( matLst ){
            # seq0 seq1 seqN nSeq syc0 syc1
            score <- c( seq0=0 ,seq1=0 ,seqN=0 ,nSeq=0 ,syc0=0 ,syc1=0 ,colCnt=0 )

            colHpnF <- rep( F ,50 )		# 속도 땜시... 설마 50 이상 쓸 일은 없겠지.
            for( nIdx in names(matLst) ){
                score[nIdx] <- nrow(matLst[[nIdx]])
                colHpnF[ matLst[[nIdx]]$idx ] <- TRUE
            }
            score["colCnt"] <- sum(colHpnF)

            return( score )
        }

		sumMtxLst <- list()
		if( !is.null(mObj$psLst[["r"]]) ){
			sumMtxLst[["r"]] <- mObj$psLst[["r"]]$getSumMtx( aObj$aZoidMtx[workArea,,drop=F] )
		}
		if( !is.null(mObj$psLst[["e"]]) ){
			sumMtxLst[["e"]] <- mObj$psLst[["e"]]$getSumMtx( aObj$aZoidMtx[workArea,,drop=F]%%10 )
		}
		if( !is.null(mObj$psLst[["c"]]) ){
			sumMtxLst[["c"]] <- mObj$psLst[["c"]]$getSumMtx( aObj$cStepMtx[workArea,,drop=F] )
		}
		if( !is.null(mObj$psLst[["f"]]) ){
			sumMtxLst[["f"]] <- mObj$psLst[["f"]]$getSumMtx( aObj$fStepMtx[workArea,,drop=F] )
		}

		sizeCeil <- 5	# 최대값 제한. 값이 너무 크면 evt 지정하기 힘들어지므로.
        for( wIdx in seq_len(length(workArea)) ){
            # sumMtxLst 자체가 이미 workArea에서 추려낸 데이터라, aIdx 적용필요가 없다.
            #   aIdx <- workArea[wIdx]
            #   aCode <- aObj$aZoidMtx[aIdx,]       ;aRem <- aCode%%10
            #   aCStep <- aObj$cStepMtx[aIdx,]      ;aFStep <- aObj$fStepMtx[aIdx,]

			if( !is.null(mObj$psLst[["r"]]) ){
				matMtx <- mObj$psLst[["r"]]$check( sumMtxLst[["r"]][wIdx,] )
					#    totCnt totSize valCnt valSize
					# h1      0       0      0       0
					# h2      0       0      0       0
				matMtx[,c("totSize","valSize")] <- ifelse( matMtx[,c("totSize","valSize")]>sizeCeil ,sizeCeil ,matMtx[,c("totSize","valSize")] )

				scrMtx[wIdx ,c("r1TotSize" ,"r1ValSize")] <- matMtx["h1",c("totSize","valSize")]
				scrMtx[wIdx ,c("r2TotSize" ,"r2ValSize")] <- matMtx["h2",c("totSize","valSize")]
			}
			if( !is.null(mObj$psLst[["e"]]) ){
				matMtx <- mObj$psLst[["e"]]$check( sumMtxLst[["e"]][wIdx,] )
				matMtx[,c("totSize","valSize")] <- ifelse( matMtx[,c("totSize","valSize")]>sizeCeil ,sizeCeil ,matMtx[,c("totSize","valSize")] )

				scrMtx[wIdx ,c("e1TotSize" ,"e1ValSize")] <- matMtx["h1",c("totSize","valSize")]
				scrMtx[wIdx ,c("e2TotSize" ,"e2ValSize")] <- matMtx["h2",c("totSize","valSize")]
			}
			if( !is.null(mObj$psLst[["c"]]) ){
				matMtx <- mObj$psLst[["c"]]$check( sumMtxLst[["c"]][wIdx,] )
				matMtx[,c("totSize","valSize")] <- ifelse( matMtx[,c("totSize","valSize")]>sizeCeil ,sizeCeil ,matMtx[,c("totSize","valSize")] )

				scrMtx[wIdx ,c("c1TotSize" ,"c1ValSize")] <- matMtx["h1",c("totSize","valSize")]
				scrMtx[wIdx ,c("c2TotSize" ,"c2ValSize")] <- matMtx["h2",c("totSize","valSize")]
			}
			if( !is.null(mObj$psLst[["f"]]) ){
				matMtx <- mObj$psLst[["f"]]$check( sumMtxLst[["f"]][wIdx,] )
				matMtx[,c("totSize","valSize")] <- ifelse( matMtx[,c("totSize","valSize")]>sizeCeil ,sizeCeil ,matMtx[,c("totSize","valSize")] )

				scrMtx[wIdx ,c("f1TotSize" ,"f1ValSize")] <- matMtx["h1",c("totSize","valSize")]
				scrMtx[wIdx ,c("f2TotSize" ,"f2ValSize")] <- matMtx["h2",c("totSize","valSize")]
			}

            # scrMtx[wIdx ,scrMtx[wIdx,]==1] <- 0
            # scrMtx[wIdx,"rebC.r"] <- sum(aCode==wMI$rawTail[stdMILen,])
            # scrMtx[wIdx,"rebC.c"] <- sum(aCStep==wMI$cStepTail[stdMILen,])
        }

        return( scrMtx )

    }
    bSMtxLst[["sScore0PSrp"]] <- function( phVP ,aZoidMtx ,makeInfoStr=F ){
        # phVP <- phVP.grp$phVPLst[[1]]
		aLen <- nrow(aZoidMtx)
        aObj <- phVP$getCodeW( aZoidMtx )

		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(bS.sScore0PSrp.cName) )	;colnames(scoreMtx) <- bS.sScore0PSrp.cName
        rownames(scoreMtx) <- aObj$miIdStr
		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c( "pvSubHLen" ,"pvSubName" )
			infoMtx <- matrix( "" ,nrow=aLen ,ncol=length(cName) )	;colnames(infoMtx) <- cName
			infoMtx[,"pvSubName"] <- aObj$miIdStr
            for( pvName in names(phVP$stdMILst) ){
                infoMtx[infoMtx[,"pvSubName"]==pvName ,"pvSubHLen"] <- phVP$stdMILst[[pvName]]$mtxLen
            }
		}
		if( 0==length(aObj$miIdStr) ){
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

        for( pvName in aObj$miNames ){  # pvName <- aObj$miNames[2]
            workArea <- which(aObj$miIdStr==pvName)
            stdMI<-phVP$stdMILst[[pvName]]
            wMI <- phVP$getCodeH( stdMI )
            scrMtx <- bSMtx.sScore0PSrp( stdMI=stdMI ,workArea ,wMI=wMI ,aObj ,aZoidMtx )
            scoreMtx[workArea,] <- scrMtx
        }

        return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
    }
}

if( TRUE ){	# sScore0PSrpRaw
	bS.sScore0PSrpRaw.cName <- c(  "r1TotSize" ,"r1ValSize" ,"r2TotSize" ,"r2ValSize" ,"e1TotSize" ,"e1ValSize" ,"e2TotSize" ,"e2ValSize"
                                ,"c1TotSize" ,"c1ValSize" ,"c2TotSize" ,"c2ValSize"
                                ,"f1TotSize" ,"f1ValSize" ,"f2TotSize" ,"f2ValSize"
    )
    bSMtx.sScore0PSrpRaw <- function( stdMI ,workArea ,wMI ,aObj ,aZoidMtx ){
		scrMtx <- matrix( 0, nrow=length(workArea), ncol=length(bS.sScore0PSrpRaw.cName) )	;colnames(scrMtx) <- bS.sScore0PSrpRaw.cName
        rownames(scrMtx) <- workArea
        stdMILen <- nrow(wMI$rawTail)
        if( 2>stdMILen || 0>length(workArea) ){
            return( scrMtx )
        }

        mObj <- list( available=FALSE )
        tailLen <- nrow(wMI$rawTail)
        if( TRUE ){ # mObj$psLst

            psLst <- list()

            psLst[["r"]] <- bUtil.scorePSrpRaw( wMI$rawTail )
            psLst[["e"]] <- bUtil.scorePSrpRaw( wMI$rawTail%%10 )
            psLst[["c"]] <- bUtil.scorePSrpRaw( wMI$cStepTail )
            if( 2<tailLen ){
                fStepTail <- wMI$fStepTail
                if( is.na(fStepTail[1,1]) ){
                    fStepTail <- fStepTail[2:tailLen,,drop=F]
                }
                psLst[["f"]] <- bUtil.scorePSrpRaw( fStepTail )
            }
            mObj$psLst <- psLst
            mObj$available <- TRUE

        }

        mObj$scoreFromMatLst <- function( matLst ){
            # seq0 seq1 seqN nSeq syc0 syc1
            score <- c( seq0=0 ,seq1=0 ,seqN=0 ,nSeq=0 ,syc0=0 ,syc1=0 ,colCnt=0 )

            colHpnF <- rep( F ,50 )		# 속도 땜시... 설마 50 이상 쓸 일은 없겠지.
            for( nIdx in names(matLst) ){
                score[nIdx] <- nrow(matLst[[nIdx]])
                colHpnF[ matLst[[nIdx]]$idx ] <- TRUE
            }
            score["colCnt"] <- sum(colHpnF)

            return( score )
        }

		sizeCeil <- 5	# 최대값 제한. 값이 너무 크면 evt 지정하기 힘들어지므로.
        for( wIdx in seq_len(length(workArea)) ){
            # sumMtxLst 자체가 이미 workArea에서 추려낸 데이터라, aIdx 적용필요가 없다.
            aIdx <- workArea[wIdx]
            aCode <- aObj$aZoidMtx[aIdx,]       ;aRem <- aCode%%10
            aCStep <- aObj$cStepMtx[aIdx,]      ;aFStep <- aObj$fStepMtx[aIdx,]

			if( !is.null(mObj$psLst[["r"]]) ){
				matMtx <- mObj$psLst[["r"]]$check( aCode )
					#    totCnt totSize valCnt valSize
					# h1      0       0      0       0
					# h2      0       0      0       0
				matMtx[,c("totSize","valSize")] <- ifelse( matMtx[,c("totSize","valSize")]>sizeCeil ,sizeCeil ,matMtx[,c("totSize","valSize")] )

				scrMtx[wIdx ,c("r1TotSize" ,"r1ValSize")] <- matMtx["h1",c("totSize","valSize")]
				scrMtx[wIdx ,c("r2TotSize" ,"r2ValSize")] <- matMtx["h2",c("totSize","valSize")]
			}
			if( !is.null(mObj$psLst[["e"]]) ){
				matMtx <- mObj$psLst[["e"]]$check( aRem )
				matMtx[,c("totSize","valSize")] <- ifelse( matMtx[,c("totSize","valSize")]>sizeCeil ,sizeCeil ,matMtx[,c("totSize","valSize")] )

				scrMtx[wIdx ,c("e1TotSize" ,"e1ValSize")] <- matMtx["h1",c("totSize","valSize")]
				scrMtx[wIdx ,c("e2TotSize" ,"e2ValSize")] <- matMtx["h2",c("totSize","valSize")]
			}
			if( !is.null(mObj$psLst[["c"]]) ){
				matMtx <- mObj$psLst[["c"]]$check( aCStep )
				matMtx[,c("totSize","valSize")] <- ifelse( matMtx[,c("totSize","valSize")]>sizeCeil ,sizeCeil ,matMtx[,c("totSize","valSize")] )

				scrMtx[wIdx ,c("c1TotSize" ,"c1ValSize")] <- matMtx["h1",c("totSize","valSize")]
				scrMtx[wIdx ,c("c2TotSize" ,"c2ValSize")] <- matMtx["h2",c("totSize","valSize")]
			}
			if( !is.null(mObj$psLst[["f"]]) ){
				matMtx <- mObj$psLst[["f"]]$check( aFStep )
				matMtx[,c("totSize","valSize")] <- ifelse( matMtx[,c("totSize","valSize")]>sizeCeil ,sizeCeil ,matMtx[,c("totSize","valSize")] )

				scrMtx[wIdx ,c("f1TotSize" ,"f1ValSize")] <- matMtx["h1",c("totSize","valSize")]
				scrMtx[wIdx ,c("f2TotSize" ,"f2ValSize")] <- matMtx["h2",c("totSize","valSize")]
			}

            # scrMtx[wIdx ,scrMtx[wIdx,]==1] <- 0
            # scrMtx[wIdx,"rebC.r"] <- sum(aCode==wMI$rawTail[stdMILen,])
            # scrMtx[wIdx,"rebC.c"] <- sum(aCStep==wMI$cStepTail[stdMILen,])
        }

        return( scrMtx )

    }
    bSMtxLst[["sScore0PSrpRaw"]] <- function( phVP ,aZoidMtx ,makeInfoStr=F ){
        # phVP <- phVP.grp$phVPLst[[1]]
		aLen <- nrow(aZoidMtx)
        aObj <- phVP$getCodeW( aZoidMtx )

		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(bS.sScore0PSrpRaw.cName) )	;colnames(scoreMtx) <- bS.sScore0PSrpRaw.cName
        rownames(scoreMtx) <- aObj$miIdStr
		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c( "pvSubHLen" ,"pvSubName" )
			infoMtx <- matrix( "" ,nrow=aLen ,ncol=length(cName) )	;colnames(infoMtx) <- cName
			infoMtx[,"pvSubName"] <- aObj$miIdStr
            for( pvName in names(phVP$stdMILst) ){
                infoMtx[infoMtx[,"pvSubName"]==pvName ,"pvSubHLen"] <- phVP$stdMILst[[pvName]]$mtxLen
            }
		}
		if( 0==length(aObj$miIdStr) ){
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

        for( pvName in aObj$miNames ){  # pvName <- aObj$miNames[2]
            workArea <- which(aObj$miIdStr==pvName)
            stdMI<-phVP$stdMILst[[pvName]]
            wMI <- phVP$getCodeH( stdMI )
            scrMtx <- bSMtx.sScore0PSrpRaw( stdMI=stdMI ,workArea ,wMI=wMI ,aObj ,aZoidMtx )
            scoreMtx[workArea,] <- scrMtx
        }

        return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
    }
}










if( FALSE ){ # sScore0FV. cStepMtx,fStepMtx 등등의 폭이 일정치 않음을 반영 할 것.
}



if( FALSE ){ # "sScore0X"

	bS.sScore0X.cName <- c( "xxx","xxx" )

    bSMtx.sScore0X <- function( stdMI ,workArea ,wMI ,aObj ,aZoidMtx ){
		scrMtx <- matrix( 0, nrow=length(workArea), ncol=length(bS.sScore0X.cName) )	;colnames(scrMtx) <- bS.sScore0X.cName
        rownames(scrMtx) <- workArea
        stdMILen <- nrow(wMI$rawTail)
        if( 0==stdMILen ){
            return( scrMtx )
        }

        for( wIdx in seq_len(length(workArea)) ){
            aIdx <- workArea[wIdx]
            aCode <- aObj$aZoidMtx[aIdx,]       ;aRem <- aCode%%10
            aCStep <- aObj$cStepMtx[aIdx,]      ;aFStep <- aObj$fStepMtx[aIdx,]

            # scrMtx[wIdx,"rebC.r"] <- sum(aCode==wMI$rawTail[stdMILen,])
            # scrMtx[wIdx,"rebC.c"] <- sum(aCStep==wMI$cStepTail[stdMILen,])
        }

        return( scrMtx )

    }

    bSMtxLst[["sScore0X"]] <- function( phVP ,aZoidMtx ,makeInfoStr=F ){
        # phVP <- phVP.grp$phVPLst[[1]]
		aLen <- nrow(aZoidMtx)
        aObj <- phVP$getCodeW( aZoidMtx )

		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(bS.sScore0X.cName) )	;colnames(scoreMtx) <- bS.sScore0X.cName
        rownames(scoreMtx) <- aObj$miIdStr
		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c( "pvSubHLen" ,"pvSubName" )
			infoMtx <- matrix( "" ,nrow=aLen ,ncol=length(cName) )	;colnames(infoMtx) <- cName
			infoMtx[,"pvSubName"] <- aObj$miIdStr
            for( pvName in names(phVP$stdMILst) ){
                infoMtx[infoMtx[,"pvSubName"]==pvName ,"pvSubHLen"] <- phVP$stdMILst[[pvName]]$mtxLen
            }
		}
		if( 0==length(aObj$miIdStr) ){
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

        for( pvName in aObj$miNames ){  # pvName <- aObj$miNames[2]
            workArea <- which(aObj$miIdStr==pvName)
            stdMI<-phVP$stdMILst[[pvName]]
            wMI <- phVP$getCodeH( stdMI )
            scrMtx <- bSMtx.sScore0X( stdMI=stdMI ,workArea ,wMI=wMI ,aObj ,aZoidMtx )
            scoreMtx[workArea,] <- scrMtx
        }

        return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
    }

}


#============================================================================================================
#   bS 그룹을 개발하는 동안의 임시 코드.
if( FALSE ){
    source("./lib/bSMtx.R")             ;source("./lib/bSMtx_VP.R")         ;source("./lib/bSMtx_Util.R")
    load("Obj_allIdxF0_chkStdMIPair_928.save")    ;aZoidMtx <- gEnv$allZoidMtx[allIdxF,,drop=F]    # 4k
    phVP.grp <- bS.getPhVPGrp( gEnv ,aZoidMtx )

    phVP <- phVP.grp$phVPLst[[1]]
    aZoidMtx <- gEnv$allZoidMtx[allIdxF,]
    #  bSMtxLst[["sScore01"]]( phVP ,aZoidMtx )
} 