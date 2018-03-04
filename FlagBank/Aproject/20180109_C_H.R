# 20180109_C_H.R 교차모델
#	pEVL : encValLst, encoded value list
#	pSBC : search base code name
#	pNZC : next zoid code name
#	pCFLst : getCFLst.base(tEnv) # 비교함수를 얻기 위해.
#		pEVL<- encValLst ;pSBC<-"A0010_o3"	;pNZC<-"A0020_o3"	;pCFLst<-cfObjLst
evlScan <- function( pEVL ,pSBC ,pNZC ,pCFLst ){

	cfNames <- sapply(pCFLst,function(p){p$idStr})
	sbc.cfObj <- pCFLst[[which(cfNames==pSBC)]]
	sbcLst <- pEVL[[pSBC]]
	nzcLst <- pEVL[[pNZC]]
	
	sbc.last <- sbcLst[[length(sbcLst)]]

	nzcLst.fnd <- list()	# found nzcLst.
	idx.fnd <- list()
	for( idx in 1:(length(sbcLst)-1) ){
		dCnt <- sbc.cfObj$diffCnt( sbcLst[[idx]] ,sbc.last )
		if( (!is.na(dCnt)) && (0==dCnt) ){
			idx.fnd[[1+length(idx.fnd)]] <- idx
			nzcLst.fnd[[1+length(nzcLst.fnd)]] <- nzcLst[[(idx+1)]]
		}
	}

	rObj <- list( sbcName=pSBC ,nzcName=pNZC ,nzcLst.fnd=nzcLst.fnd )
	if( 0 < length(idx.fnd) ){
		rObj$idx.fnd <- do.call( c ,idx.fnd )
		rObj$lastZC <- nzcLst.fnd[[length(nzcLst.fnd)]]
	}

	return( rObj )
} # evlScan()

evalScan.pair <- function( pEVL ,pName ,pCFLst ,pThld=0 ){

	cfName <- sapply(pCFLst,function(p){p$idStr})
	cfObj <- pCFLst[[which(cfName==pName)]]
	sbcLst <- encValLst[[pName]]

	pairLst <- list()
	for( aIdx in 1:(length(sbcLst)-2) ){
		for( bIdx in (aIdx+1):length(sbcLst) ){
			dCnt <- cfObj$diffCnt( sbcLst[[aIdx]] ,sbcLst[[bIdx]] )
			if( (!is.null(dCnt)) && (!is.na(dCnt)) && (pThld>=dCnt) ){
				pairLst[[1+length(pairLst)]] <- c( aIdx ,bIdx ,dCnt )
			}
		}
	}

	pairMtx <- do.call( rbind ,pairLst )
	colnames(pairMtx) <- c("aIdx","bIdx","thld")
	return( pairMtx )

} # evalScan.pair()


#   동일 패턴이 그대로 재발.
#	pBanObj : getCFltObj() 리턴 값.
#   	pBanObj<-banObj ;pZoidMtx<-gEnv$zhF ;pInitZIdx=NULL ;pCodeLst=codeLst
ban.hntSameRow <- function( pBanObj ,pZoidMtx ,pCodeLst ,pInitZIdx=NULL ){

    if( is.null(pInitZIdx) ){
        pInitZIdx <- 1:nrow(pZoidMtx)
    }

    codeLst <- pCodeLst
    banLst.dup <- lapply( pBanObj$encValLst ,function(p){p[[length(p)]]})

    # --------------------------------------------------
    #	filtLst.dupRow ,filtedIdx.dupRow
    excBan=c("A0070_o3")
    filtLst <- lapply( 1:nrow(pZoidMtx) ,function(pIdx){
            # 어느 banLst.dupRow에서 걸렸는지의 flag
            flag <- sapply(pBanObj$cfNames ,function(pName){
                            if( pName %in% excBan ){
                                return( FALSE )
                            }
                            cfObj <- pBanObj$cfObjLst[[pName]]
                            dCnt <- cfObj$diffCnt( codeLst[[pName]][[pIdx]] ,banLst.dup[[pName]] )
                            return( dCnt==0 )
                        })
            return( pBanObj$cfNames[flag] )
        })
    filtedIdx <- pInitZIdx[0<sapply(filtLst ,length)]

    rstObj <- list( idStr="hntSameRow" )
    rstObj$filtLst  <- filtLst
    rstObj$filtedIdx<- filtedIdx
    return( rstObj )

} # ban.hntSameRow()

#   다른 차원끼리의 과거 연속 재발. pDepth를 과감하게 키워도 될 듯.
#	pBanObj : getCFltObj() 리턴 값.
#	pDepth : 과거 발생 패턴을 몇 개까지 검사할 것인지. 1이면 가장 최근 발생 패턴만 검색&비교
#   	pBanObj<-banObj ;pZoidMtx<-gEnv$zhF ;pInitZIdx=NULL ;pCodeLst=codeLst   ;pDepth=2
ban.hntCrossDim <- function( pBanObj ,pZoidMtx ,pCodeLst ,pInitZIdx=NULL ,pDepth=2 ){

    if( is.null(pInitZIdx) ){
        pInitZIdx <- 1:nrow(pZoidMtx)
    }

    codeLst <- pCodeLst

    cfNameMtx <- apply( permutations( length(pBanObj$cfNames) ,2 )
                         ,1 ,function(p){pBanObj$cfNames[p]}
                    )
    cfNameMtx <- t(cfNameMtx)   ;colnames(cfNameMtx) <- c("sbc.name","nzc.name")

    banLst <- list()
    evlScanLst <- list()
    for( cfIdx in 1:nrow(cfNameMtx) ){

        sbc.name <- cfNameMtx[cfIdx,1]
        nzc.name <- cfNameMtx[cfIdx,2]
        rstObj <- evlScan( pBanObj$encValLst ,sbc.name ,nzc.name ,pBanObj$cfObjLst )
        if( !is.null(rstObj$lastZC) ){
            cmDepth <- ifelse( pDepth>length(rstObj$nzcLst.fnd) ,length(rstObj$nzcLst.fnd) ,pDepth ) # current max depth
            useSpan <- (length(rstObj$nzcLst.fnd)-cmDepth+1):length(rstObj$nzcLst.fnd)
            banLst[[cfIdx]] <- rstObj$nzcLst.fnd[useSpan]
        } else {
            # banLst[[cfIdx]] <- list(noData=integer(0))   # NULL 이면 맨 끝 인덱스가 달라질 수 있어서..
            banLst[[cfIdx]] <- list()   # NULL 이면 맨 끝 인덱스가 달라질 수 있어서..
        }
        evlScanLst[[1+length(evlScanLst)]] <- rstObj
    } # cfIdx


    # --------------------------------------------------
    #	filtLst.dupRow ,filtedIdx.dupRow
    filtLst <- lapply( 1:nrow(pZoidMtx) ,function(pIdx){
            # 어느 banLst에서 걸렸는지의 flag
            flag <- sapply( 1:nrow(cfNameMtx) ,function(pRIdx){
                            if( 0==length(banLst[[pRIdx]]) ){
                                return( FALSE )	# 판단 기준자체가 없으니 ban에 걸리지 않은 걸로 처리.
                            }
                            # banCode <- banLst[[pRIdx]]
                            banFltName <- cfNameMtx[pRIdx,2]
                            zCode <- codeLst[[banFltName]][[pIdx]]
                            dCnt <- sapply( banLst[[pRIdx]] ,function(banCode){ 
                                            pBanObj$cfObjLst[[banFltName]]$diffCnt( banCode ,zCode )
                                        })
                            return( all(dCnt==0) )
                        })
            return( which(flag) )
        })
    filtedIdx <- pInitZIdx[0<sapply(filtLst ,length)]

    rstObj <- list( idStr="hntCrossDim" )
    rstObj$filtLst      <- filtLst
    rstObj$filtedIdx    <- filtedIdx
    # 디버깅용 -------
    rstObj$depth        <- pDepth
    rstObj$cfNameMtx    <- cfNameMtx
    rstObj$banLst       <- banLst
    rstObj$evlScanLst   <- evlScanLst
    return( rstObj )

} # ban.hntCrossDim()



getCFltObj <- function( pEnv ){

    cfObjLst <- NULL
    encSpan <- 50:nrow(pEnv$zhF)
    hIdx.encVal <- encSpan
    encValLst <- list()
    for( tIdx in encSpan ){	# lastZoid 기반 동작들 때문에 1부터 시작은 의미없다.
        tEnv <- pEnv
        tEnv$zhF <- pEnv$zhF[1:(tIdx-1),]
        tEnv$allZoidMtx <- pEnv$zhF[tIdx,,drop=F]
        
        cfObjLst <- getCFLst.base( tEnv )
        for( lIdx in seq_len(length(cfObjLst)) ){
            cfObj <- cfObjLst[[lIdx]]
            if( is.null(encValLst[[cfObj$idStr]]) ){
                encValLst[[cfObj$idStr]] <- list()
            }
            encValLst[[cfObj$idStr]][[1+length(encValLst[[cfObj$idStr]])]] <- cfObj$enc( tEnv$allZoidMtx )[[1]]
        }
    }
	
	# 버그 수정 중. cfObjLst는 pEnv 기준으로 다시 만들어져야 한다.(lastZoid 값을 위해..)
	cfObjLst <- getCFLst.base( pEnv )

    cfNames <- sapply(cfObjLst,function(p){p$idStr})
    encVal.len <- length(encValLst[[1]])

    cfNameMtx <- apply( permutations( length(cfNames) ,2 ) # 삭제
                         ,1 ,function(p){cfNames[p]}
                    )
    cfNameMtx <- t(cfNameMtx)   ;colnames(cfNameMtx) <- c("sbc.name","nzc.name")

    banLst <- list()	# 삭제
    for( cfIdx in 1:nrow(cfNameMtx) ){

        sbc.name <- cfNameMtx[cfIdx,1]
        nzc.name <- cfNameMtx[cfIdx,2]
        rstObj <- evlScan( encValLst ,sbc.name ,nzc.name ,cfObjLst )
        if( !is.null(rstObj$lastZC) ){
            banLst[[cfIdx]] <- rstObj$lastZC
        } else {
            banLst[[cfIdx]] <- integer(0)   # NULL 이면 맨 끝 인덱스가 달라질 수 있어서..
        }
    } # cfIdx

    banLst.dup <- lapply( encValLst ,function(p){p[[length(p)]]})	# 삭제

    if( FALSE ){    # 측정파트

        # < failHIdx.dupRow >
        #   Row가 동일 연속 재발하는 빈도 측정(발생빈도를 봐가며 사용해야 할 듯)
        failIdx <- rep( 0 ,length(encSpan) )
        dupRowCnt <- rep( 0 ,length(cfNames) ) ;names(dupRowCnt) <- cfNames
        for( idx in 2:length(encSpan) ) {
            dupFlag <- sapply( seq_len(length(encValLst)) ,function(eIdx){
                            dCnt <- cfObjLst[[eIdx]]$diffCnt( encValLst[[eIdx]][[idx-1]] ,encValLst[[eIdx]][[idx]] )
                            return( !is.na(dCnt) && (0==dCnt) )
                        })
            dupRowCnt <- dupRowCnt + dupFlag
            failIdx[idx] <- sum(dupFlag)
        }
        failHIdx.dupRow <- encSpan[failIdx>0]

        # < failHIdx.banLst >
        #   evlScan() 함수로부터의 banLst 에 의해 제거되는 hIdx
        failIdx <- rep( 0 ,length(encSpan) )
        testSpan <- 150:length(encSpan)
        for( tIdx in testSpan ){
            tValLst <- lapply( encValLst ,function(p){p[1:(tIdx-1)]})
            stdValLst <- lapply( encValLst ,function(p){p[[tIdx]]})
            failFlag <- rep( FALSE ,nrow(cfNameMtx) )
            for( rIdx in 1:nrow(cfNameMtx) ){
                nameIdx <- which( cfNames == cfNameMtx[rIdx,"nzc.name"] )
                rstObj <- evlScan( tValLst
                                ,cfNameMtx[rIdx,"sbc.name"] ,cfNameMtx[rIdx,"nzc.name"]
                                ,cfObjLst
                            )
                if( !is.null(rstObj$lastZC) ){
                    dCnt <- cfObjLst[[nameIdx]]$diffCnt( rstObj$lastZC ,stdValLst[[nameIdx]] )
                    failFlag[rIdx] <- dCnt==0
                }
            }
            failIdx[tIdx] <- sum(failFlag)
        }
        failHIdx.banLst <- encSpan[failIdx>0]

        failHIdx <- unique( c(failHIdx.banLst,intersect(failHIdx.dupRow,testSpan)) )
        length(failHIdx) / length(testSpan)

    } # if( FALSE )    # 측정파트

	# =[ rObj ]=============================================================
    rObj <- list( cfNameMtx=cfNameMtx	,banLst=banLst    ,banLst.dup=banLst.dup
                    ,cfNames=cfNames	,cfObjLst=cfObjLst
					,encValLst=encValLst,encVal.len=encVal.len
                	,hIdx.encVal=hIdx.encVal )
	names(rObj$cfObjLst) <- rObj$cfNames

	rObj$getCodeLst <- function( pZoidMtx ){
        codeLst <- list()
        for( nIdx in rObj$cfNames ){
            codeLst[[nIdx]] <- rObj$cfObjLst[[nIdx]]$enc(pZoidMtx)
        }
		return( codeLst )
	} # rObj$getCodeLst()

	rObj$getFiltedIdx <- function( pZoidMtx ,pZIdx=NULL ){

		if( is.null(pZIdx) ){
			pZIdx <- 1:nrow(pZoidMtx)
		}

		codeLst <- list()
		for( nIdx in rObj$cfNames ){
			codeLst[[nIdx]] <- rObj$cfObjLst[[nIdx]]$enc(pZoidMtx)
		}

		# --------------------------------------------------
		#	filtLst.dupRow ,filtedIdx.dupRow
		excBanLst.dupRow=c("A0070_o3")
		filtLst.dupRow <- lapply( 1:nrow(pZoidMtx) ,function(pIdx){
				# 어느 banLst.dupRow에서 걸렸는지의 flag
				flag <- sapply(rObj$cfNames ,function(pName){
								if( pName %in% excBanLst.dupRow ){
									return( FALSE )
								}
								cfObj <- rObj$cfObjLst[[pName]]
								dCnt <- cfObj$diffCnt( codeLst[[pName]][[pIdx]] ,rObj$banLst.dup[[pName]] )
								return( dCnt==0 )
							})
				return( rObj$cfNames[flag] )
			})
		filtedIdx.dupRow <- pZIdx[0<sapply(filtLst.dupRow ,length)]

		# --------------------------------------------------
		#	filtLst.cf1 ,filtedIdx.cf1
		filtLst.cf1 <- lapply( 1:nrow(pZoidMtx) ,function(pIdx){
				# 어느 banLst에서 걸렸는지의 flag
				flag <- sapply( 1:nrow(rObj$cfNameMtx) ,function(pRIdx){
								banCode <- rObj$banLst[[pRIdx]]
								if( 0==length(banCode) ){
									return( FALSE )	# 판단 기준자체가 없으니 ban에 걸리지 않은 걸로 처리.
								}
								banFltName <- rObj$cfNameMtx[pRIdx,2]
								zCode <- codeLst[[banFltName]][[pIdx]]
								dCnt <- rObj$cfObjLst[[banFltName]]$diffCnt( banCode ,zCode )
								return( dCnt==0 )
							})
				return( which(flag) )
			})
		filtedIdx.cf1 <- pZIdx[0<sapply(filtLst.cf1 ,length)]

		assObj <- list( filtLst.dupRow=filtLst.dupRow	,filtedIdx.dupRow=filtedIdx.dupRow
						,filtLst.cf1=filtLst.cf1			,filtedIdx.cf1=filtedIdx.cf1
					)

		return( assObj )
	} # rObj$getFiltedIdx()

    return( rObj )

} # getCFltObj()


# ====================================================================================

getCFLst.base <- function( pEnv ){
	cfObjLst <- list()
	cfObjLst[[1+length(cfObjLst)]] <- cf_A0010( pEnv )
	cfObjLst[[1+length(cfObjLst)]] <- cf_A0020( pEnv )
	cfObjLst[[1+length(cfObjLst)]] <- cf_A0030( pEnv )
	cfObjLst[[1+length(cfObjLst)]] <- cf_A0040( pEnv )
	cfObjLst[[1+length(cfObjLst)]] <- cf_A0050( pEnv )
	cfObjLst[[1+length(cfObjLst)]] <- cf_A0060( pEnv )
	cfObjLst[[1+length(cfObjLst)]] <- cf_A0070( pEnv )
	cfObjLst[[1+length(cfObjLst)]] <- cf_A0080( pEnv )
	cfObjLst[[1+length(cfObjLst)]] <- cf_A0090( pEnv )
	return( cfObjLst )
} # getCFLst.base()



cf_A0010 <- function( pEnv ,pBase=3 ){
	
	cfObj <- list( idStr=sprintf("A0010_o%s",pBase) )
	cfObj$enc <- function( pZoidMtx ,pLogFunc=NULL ){
		tStmp <- Sys.time()

		rLst <- lapply( seq_len(nrow(pZoidMtx)) ,function(p){ 
						(pZoidMtx[p,])%%pBase } 
					)
		if( !is.null(pLogFunc) ){
			tDiff <- Sys.time() - tStmp
			pLogFunc( sprintf("ID:%s cost:%.1f%s",cfObj$idStr,tDiff,units(tDiff)) )
		}
		
		return( rLst )
	} # cfObj$enc()
	cfObj$diffCnt <- function( p1 ,p2 ){
		return( sum(p1!=p2) )
	}
	
	return( cfObj )

} # cf_A0010()

cf_A0020 <- function( pEnv ,pBase=3 ){
	
	cfObj <- list( idStr=sprintf("A0020_o%s",pBase) )
	cfObj$lastZoid <- pEnv$zhF[nrow(pEnv$zhF),]
	cfObj$enc <- function( pZoidMtx ,pLogFunc=NULL ){
		tStmp <- Sys.time()

		rLst <- lapply( seq_len(nrow(pZoidMtx)) ,function(p){ 
						abs(cfObj$lastZoid-pZoidMtx[p,])%%pBase } 
					)
		if( !is.null(pLogFunc) ){
			tDiff <- Sys.time() - tStmp
			pLogFunc( sprintf("ID:%s cost:%.1f%s",cfObj$idStr,tDiff,units(tDiff)) )
		}
		
		return( rLst )
	} # cfObj$enc()
	cfObj$diffCnt <- function( p1 ,p2 ){
		return( sum(p1!=p2) )
	}
	
	return( cfObj )

} # cf_A0020()

cf_A0030 <- function( pEnv ,pBase=3 ){
	
	cfObj <- list( idStr=sprintf("A0030_o%s",pBase) )
	cfObj$enc <- function( pZoidMtx ,pLogFunc=NULL ){
		tStmp <- Sys.time()

		rLst <- lapply( seq_len(nrow(pZoidMtx)) ,function(p){ 
						(pZoidMtx[p,2:6]-pZoidMtx[p,1:5])%%pBase } 
					)
		if( !is.null(pLogFunc) ){
			tDiff <- Sys.time() - tStmp
			pLogFunc( sprintf("ID:%s cost:%.1f%s",cfObj$idStr,tDiff,units(tDiff)) )
		}
		
		return( rLst )
	} # cfObj$enc()
	cfObj$diffCnt <- function( p1 ,p2 ){
		return( sum(p1!=p2) )
	}
	
	return( cfObj )

} # cf_A0030()

cf_A0040 <- function( pEnv ,pBase=3 ){
	
	cfObj <- list( idStr=sprintf("A0040_o%s",pBase) )
	cfObj$zhF <- pEnv$zhF
	cfObj$enc <- function( pZoidMtx ,pLogFunc=NULL ){
		tStmp <- Sys.time()
		rebLen <- getRebLen( 1:45 ,cfObj$zhF )
		
		rLst <- lapply( seq_len(nrow(pZoidMtx)) ,function(p){ 
						rebLen[pZoidMtx[p,]] %% pBase
					} )
		if( !is.null(pLogFunc) ){
			tDiff <- Sys.time() - tStmp
			pLogFunc( sprintf("ID:%s cost:%.1f%s",cfObj$idStr,tDiff,units(tDiff)) )
		}
		
		return( rLst )
	} # cfObj$enc()
	cfObj$diffCnt <- function( p1 ,p2 ){
		return( sum(p1!=p2) )
	}
	
	return( cfObj )

} # cf_A0040()

cf_A0050 <- function( pEnv ,pBase=5 ){
	
	cfObj <- list( idStr=sprintf("A0050_o%s",pBase) )
	cfObj$enc <- function( pZoidMtx ,pLogFunc=NULL ){
		tStmp <- Sys.time()
		
		rLst <- lapply( seq_len(nrow(pZoidMtx)) ,function(p){ 
						pZoidMtx[p,]%/%pBase
					} )
		if( !is.null(pLogFunc) ){
			tDiff <- Sys.time() - tStmp
			pLogFunc( sprintf("ID:%s cost:%.1f%s",cfObj$idStr,tDiff,units(tDiff)) )
		}
		
		return( rLst )
	} # cfObj$enc()
	cfObj$diffCnt <- function( p1 ,p2 ){
		return( sum(p1!=p2) )
	}
	
	return( cfObj )

} # cf_A0050()

cf_A0060 <- function( pEnv ,pBase=7 ){
	
	cfObj <- list( idStr=sprintf("A0060_o%s",pBase) )
	cfObj$enc <- function( pZoidMtx ,pLogFunc=NULL ){
		tStmp <- Sys.time()
		
		rMtx <- getTblCnt( pZoidMtx%/%pBase ,pTblVal=0:(45%/%pBase) )
		rLst <- lapply( seq_len(nrow(rMtx)) ,function(p){rMtx[p,]} )
		if( !is.null(pLogFunc) ){
			tDiff <- Sys.time() - tStmp
			pLogFunc( sprintf("ID:%s cost:%.1f%s",cfObj$idStr,tDiff,units(tDiff)) )
		}
		
		return( rLst )
	} # cfObj$enc()
	cfObj$diffCnt <- function( p1 ,p2 ){
		return( sum(p1!=p2) )
	}
	
	return( cfObj )

} # cf_A0060()

cf_A0070 <- function( pEnv ){

	cfObj <- list( idStr=sprintf("A0070") )

	cfObj$enc <- function( pZoidMtx ,pLogFunc=NULL ){
		tStmp <- Sys.time()

		rLst <- lapply( seq_len(nrow(pZoidMtx)) ,function(p){ 
						c( (pZoidMtx[p,4]-pZoidMtx[p,1]) ,(pZoidMtx[p,6]-pZoidMtx[p,3]) ,(pZoidMtx[p,6]-pZoidMtx[p,1]) )
					} )
		if( !is.null(pLogFunc) ){
			tDiff <- Sys.time() - tStmp
			pLogFunc( sprintf("ID:%s cost:%.1f%s",cfObj$idStr,tDiff,units(tDiff)) )
		}
		
		return( rLst )
	} # cfObj$enc()
	cfObj$diffCnt <- function( p1 ,p2 ){
		return( sum(p1!=p2) )
	}

	# pastValMtx <- do.call( rbind ,cfObj$enc(pEnv$zhF) )
	return( cfObj )

} # cf_A0070()

cf_A0080 <- function( pEnv ){
	# 다음 h에서 똑같은 코드 발생.
	cfObj <- list( idStr=sprintf("A0080") )
	cfObj$lastZoid <- pEnv$zhF[nrow(pEnv$zhF),]

	cfObj$enc <- function( pZoidMtx ,pLogFunc=NULL ){
		tStmp <- Sys.time()

		rLst <- lapply( seq_len(nrow(pZoidMtx)) ,function(p){ 
						codeVal <- c( 0 ,0 ,0 ,0 )
						fndIdx <- which( cfObj$lastZoid %in% pZoidMtx[p,] )
						if( 0<length(fndIdx) ){
							codeVal[1] <- fndIdx[1]
							codeVal[2] <- cfObj$lastZoid[fndIdx[1]]
							if( 1<length(fndIdx) ){
								codeVal[3] <- fndIdx[2]
								codeVal[4] <- cfObj$lastZoid[fndIdx[2]]
							}
						}
						return( codeVal )
					} )
		if( !is.null(pLogFunc) ){
			tDiff <- Sys.time() - tStmp
			pLogFunc( sprintf("ID:%s cost:%.1f%s",cfObj$idStr,tDiff,units(tDiff)) )
		}
		
		return( rLst )
	} # cfObj$enc()
	cfObj$diffCnt <- function( p1 ,p2 ){
		dCnt <- sum(p1!=p2)
		if( all(p1==0) ){	# 발견되지 않은 경우는 너무 흔하기 때문에 같은 것으로 인정하지 않기로 한다.
			dCnt <- length(p1)
		}
		return( dCnt )
	}
	
	return( cfObj )

} # cf_A0080()

cf_A0090 <- function( pEnv ){
	#	다음 h에서 1 증가한 코드 발생.
	cfObj <- list( idStr=sprintf("A0090") )
	cfObj$lastZoid <- pEnv$zhF[nrow(pEnv$zhF),]

	cfObj$enc <- function( pZoidMtx ,pLogFunc=NULL ){
		tStmp <- Sys.time()

		rLst <- lapply( seq_len(nrow(pZoidMtx)) ,function(p){ 
						codeVal <- c( 0 ,0 ,0 ,0 )
						fndIdx <- which( cfObj$lastZoid %in% (pZoidMtx[p,]-1) )
						if( 0<length(fndIdx) ){
							codeVal[1] <- fndIdx[1]
							codeVal[2] <- cfObj$lastZoid[fndIdx[1]]
							if( 1<length(fndIdx) ){
								codeVal[3] <- fndIdx[2]
								codeVal[4] <- cfObj$lastZoid[fndIdx[2]]
							}
						}
						return( codeVal )
					} )
		if( !is.null(pLogFunc) ){
			tDiff <- Sys.time() - tStmp
			pLogFunc( sprintf("ID:%s cost:%.1f%s",cfObj$idStr,tDiff,units(tDiff)) )
		}
		
		return( rLst )
	} # cfObj$enc()
	cfObj$diffCnt <- function( p1 ,p2 ){
		dCnt <- sum(p1!=p2)
		if( all(p1==0) ){	# 발견되지 않은 경우는 너무 흔하기 때문에 같은 것으로 인정하지 않기로 한다.
			dCnt <- length(p1)
		}
		return( dCnt )
	}
	
	return( cfObj )

} # cf_A0090()


