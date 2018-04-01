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

# 최근 pDepth만큼의 동일값 연속이 있는 지 확인.
getColSeq <- function( pValMtx ,pDepth=2 ){

	valLen <- nrow(pValMtx)
	if(pDepth>valLen){
		return( NULL )
	}

	stdVal <- pValMtx[valLen-pDepth+1 ,]
	matFlag <- rep( TRUE ,length(stdVal) )
	for( idx in (valLen-pDepth+2):valLen ){
		matFlag <- matFlag & (pValMtx[idx,]==stdVal)
	}

	rObj <- list( stdVal=stdVal ,flag=matFlag ,depth=pDepth )
	return( rObj )

} # getColSeq()

getBanPtn <- function( pValMtx ,pMaxDepth=5 ,pDebug=F ){
	# val.getBanPtn() 함수 참고.

	valLen <- nrow(pValMtx)
	if( valLen < 2*2 ){	# 최소한 2번의 패턴반복이 나와야 하므로.
		return( NULL )
	}

	ptnLst <- list()
	for( dIdx in 2:pMaxDepth ){	# depthIdx
		for( psIdx in 0:(dIdx-1) ){	# pattern slide(이하 pSlide로 약칭) index
			if( valLen < (dIdx*2+psIdx) ){
				break			# 최초 오버 이후로는 계속 크므로 dIdx를 위한 break는 추가할 필요 없음.
			}
			dSpan <- dIdx:1		# depth span
			plateA <- pValMtx[valLen-(dSpan-1)		- psIdx	,]
			plateB <- pValMtx[valLen-(dSpan-1)-dIdx	- psIdx	,]
			matFlagMtx <- plateA==plateB
			matFlagMtx[is.na(matFlagMtx)] <- FALSE
			if( !any(matFlagMtx) ){
				next	# 매치 자체가 없다.
			}

			plateB[!matFlagMtx] <- NA				# 매치되어야 할 부분 빼고는 모두 NA
													# 따라서 plateB만 가지고도 패턴매치 확인 가능.
			matRowIdx	<- 1
			matRowVal	<- plateB[matRowIdx,]
			matCnt		<- 0	# pSlide 내에서 확인된 매치 수. psIdx가 0이면 자연히 0
			chkCnt		<- sum(!is.na(matRowVal))	# 다음 H에서 매치 확인해야 하는 갯수

			if( psIdx>0 ){	# pSlide 영역 존재 시
				matRowIdx <- matRowIdx + psIdx
				matRowVal	<- plateB[matRowIdx,]

				# pSlide 영역 내에서도 매치가 발견되는가? (plateB는 이미 매치될 부분만 남은 상태임을 참고)
				valMtx.B 	<- plateB[1:psIdx,,drop=F]
				valMtx.s 	<- pValMtx[(valLen-psIdx+1):valLen,,drop=F] # pSlide
				psMatFlagMtx <- valMtx.B == valMtx.s
				
				matCnt		<- sum(!is.na(valMtx.B))	# pSlide 내에서 확인된 매치 수. psIdx가 0이면 자연히 0
														# pSlide가 있음에도 불구하고 matCnt가 0인 경우에 대해 고려필요
				if( matCnt != sum(psMatFlagMtx,na.rm=T) ){
					next	# pSlide에서의 매치가 어긋났다...
				}				
				chkCnt		<- sum(!is.na(matRowVal))	# 다음 H에서 매치 확인해야 하는 갯수
			}

			if( 0==chkCnt ){
				next	# 다음 H에서는 매치 확인할 게 없다...
			}

			ptnObj <- list( matCnt=matCnt ,chkCnt=chkCnt ,matVal=plateB[matRowIdx,] )
			ptnObj$depth		<- dIdx
			ptnObj$ptnSlide		<- psIdx
			ptnObj$matFlagMtx	<- matFlagMtx
			ptnObj$matValMtx	<- plateB
			ptnObj$matFlag		<- matFlagMtx[matRowIdx,]

			ptnLst[[1+length(ptnLst)]] <- ptnObj
		}
	} # dIdx

	rObj <- list( ptnLst=ptnLst )
	rObj$chkMatchAll <- function( pValMtx ,pDebug=F ){ 

		rstLst <- list()
		for( valIdx in 1:nrow(pValMtx) ){
			ptnIdx <- integer(0)
			for( pIdx in seq_len(length(rObj$ptnLst)) ){
				matCnt <- sum(rObj$ptnLst[[pIdx]]$matVal==pValMtx[valIdx,] ,na.rm=T)
				if( matCnt==rObj$ptnLst[[pIdx]]$chkCnt ){
					ptnIdx[1+length(ptnIdx)] <- pIdx
					if( !pDebug ){
						break	# 디버그 모드일 때 만 모든 ptn 체크하자.
					}
				}
			}
			rstLst[[1+length(rstLst)]] <- ptnIdx
		} # valIdx

		rstObj <- list( rstLst=rstLst )
		if( pDebug ){
			rstObj$chkCntLst <- lapply( rstLst ,function(ptnIdx){
										sapply(rObj$ptnLst[ptnIdx] ,function(p){p$chkCnt})
									})
			rstObj$matCntLst <- lapply( rstLst ,function(ptnIdx){
										sapply(rObj$ptnLst[ptnIdx] ,function(p){p$matCnt})
									})
			rstObj$ptnSlideLst <- lapply( rstLst ,function(ptnIdx){
										sapply(rObj$ptnLst[ptnIdx] ,function(p){p$ptnSlide})
									})
		}
		return( rstObj )

	} # rObj$chkMatchAll()
	
	#	pExcCol : 체크대상에서 제외하고픈 컬럼 idx
	rObj$chkMatchAny <- function( pValMtx ,pExcCol=NULL ,pDebug=F ){

		rstLst <- list()
		for( valIdx in 1:nrow(pValMtx) ){
			ptnIdx <- integer(0)
			for( pIdx in seq_len(length(rObj$ptnLst)) ){
				flag <- rObj$ptnLst[[pIdx]]$matVal==pValMtx[valIdx,]
				if( any(flag,na.rm=T) ){
					if( !is.null(pExcCol) ){
						if( all(which(flag)%in%pExcCol) ){
							next
						}
					}
					ptnIdx[1+length(ptnIdx)] <- pIdx
				}
			}
			rstLst[[1+length(rstLst)]] <- ptnIdx
		} # valIdx

		rstObj <- list( rstLst=rstLst )
		if( pDebug ){
			rstObj$chkCntLst <- lapply( rstLst ,function(ptnIdx){
										sapply(rObj$ptnLst[ptnIdx] ,function(p){p$chkCnt})
									})
			rstObj$matCntLst <- lapply( rstLst ,function(ptnIdx){
										sapply(rObj$ptnLst[ptnIdx] ,function(p){p$matCnt})
									})
			rstObj$ptnSlideLst <- lapply( rstLst ,function(ptnIdx){
										sapply(rObj$ptnLst[ptnIdx] ,function(p){p$ptnSlide})
									})
		}
		return( rstObj )

	} # rObj$chkMatchAny()

	return( rObj )

} # getBanPtn()

#   컬럼 값에서 연속으로 대칭발생 ban
#       pValMtx <- gEnv$zhF[1:792,] ;pMaxDepth=5 ;pDebug=F
getBanSeqSym <- function( pValMtx ,pMaxDepth=5 ,pDebug=F ){

    symLst <- list()
    colBanLst <- lapply(1:ncol(pValMtx),function(p){integer(0)})
    for( colIdx in 1:ncol(pValMtx) ){
        symObj <- col.fndSymEven( pValMtx[,colIdx] )
        if( !is.null(symObj) ){
            symObj$colIdx <- colIdx
            symLst[[1+length(symLst)]] <- symObj
            colBanLst[[colIdx]][1+length(colBanLst[[colIdx]])] <- symObj$banVal
        }

        symObj <- col.fndSymOdd( pValMtx[,colIdx] )
        if( !is.null(symObj) ){
            symObj$colIdx <- colIdx
            symLst[[1+length(symLst)]] <- symObj
            colBanLst[[colIdx]][1+length(colBanLst[[colIdx]])] <- symObj$banVal
        }
    }

	rObj <- list( colBanLst=colBanLst )
    if( pDebug ){
        rObj$symLst <- symLst
    }
	return( rObj )

} # getBanSeqSym()

#   3,2,1,1,2,? 패턴
col.fndSymEven <- function( pVal ,pMaxDepth=5 ){
    
    val.len <- length(pVal)
    if( 3 > val.len ){   # 2,1,1,?
        return(NULL)
    }

    srcPtn <- integer(0)
    eadgeVal <- NA
    for( dIdx in 1:pMaxDepth ){
        srcPtn <- pVal[val.len:(val.len-dIdx+1)]
        matSpan <- (val.len-dIdx-dIdx+1):(val.len-dIdx)
        if( 2 > matSpan[1] ){ # 최소한 pVal[1]은 존재해야 ? 매칭값 할당가능
            break
        }

        if( all(srcPtn==pVal[matSpan]) ){
            eadgeVal <- pVal[ matSpan[1]-1 ]
            break
        }
    } # for(dIdx)

    if( is.na(eadgeVal) ){  return( NULL )
    } else {
        return( list(banVal=eadgeVal ,srcPtn=srcPtn) )
    }

} # col.fndSymEven()

#   3,2,1,2,? 패턴
col.fndSymOdd <- function( pVal ,pMaxDepth=5 ){

	# pVal <- c( 30,20,10,20 )
	# pVal <- c( 40,30,20,10,20,30 )

    val.len <- length(pVal)
    if( 4>length(pVal) ){   # 3,2,1,2,?
        return(NULL)
    }

    srcPtn <- integer(0)
    eadgeVal <- NA
    for( dIdx in 1:pMaxDepth ){
        srcPtn <- pVal[val.len:(val.len-dIdx+1)]
        matSpan <- (val.len-dIdx-1-dIdx+1):(val.len-dIdx-1)
        if( 2 > matSpan[1] ){ # 최소한 pVal[1]은 존재해야 ? 매칭값 할당가능
            break
        }

        if( all(srcPtn==pVal[matSpan]) ){
            eadgeVal <- pVal[ matSpan[1]-1 ]
            break
        }
    } # for(dIdx)

    if( is.na(eadgeVal) ){  return( NULL )
    } else {
        return( list(banVal=eadgeVal ,srcPtn=srcPtn) )
    }

} # col.fndSymOdd()

#	4,5,? 패턴
getBanGrad <- function( pValMtx ){

	valLen <- nrow(pValMtx)
	if( 2>valLen ){
		return( list(banVal=rep(NA,ncol(pValMtx))) )
	}

    vDiff <- pValMtx[valLen,] - pValMtx[(valLen-1),]
    banVal <- pValMtx[valLen,] + vDiff
    banVal[1!=abs(vDiff)] <- NA

	rObj <- list( banVal=banVal )
	return( rObj )

} # getBanGrad()

#	28,21,14,? 패턴
getBanGrad.n <- function( pValMtx ){

	valLen <- nrow(pValMtx)
	if( 3>valLen ){
		return( list(banVal=rep(NA,ncol(pValMtx))) )
	}

    vDiffMtx <- pValMtx[valLen-(0:1),] - pValMtx[valLen-(1:2),]
    flag <- apply( vDiffMtx ,2 ,function(p){1==length(unique(p))} )

    banVal <- pValMtx[valLen,] + vDiffMtx[1,]
    banVal[!flag] <- NA

	rObj <- list( banVal=banVal )
	return( rObj )

} # getBanGrad.n()


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
    #	filtLst ,filtedIdx
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

#   pDimCnt : 동일한 dim이 n개 이상 존재하는 것은 자른다.
#   	pBanObj<-banObj ;pZoidMtx<-gEnv$zhF ;pInitZIdx=NULL ;pCodeLst=codeLst   ;pDebug=F
ban.multiDim <-function( pBanObj ,pZoidMtx ,pCodeLst ,pInitZIdx=NULL ,pDebug=F ){

    if( is.null(pInitZIdx) ){
        pInitZIdx <- 1:nrow(pZoidMtx)
    }

	# --------------------------------------------------
	#	예외 리스트 정의
	excFiltCmbLst <- list(	cmb0103=which(pBanObj$cfNames %in% c("A0010_o3","A0030_o3"))
							,cmb0506=which(pBanObj$cfNames %in% c("A0050_o5","A0060_o7"))
							,cmbC12_23=which(pBanObj$cfNames %in% c("C0010w02","C0020w03"))
							,cmbC14_34=which(pBanObj$cfNames %in% c("C0010w04","C0030w04"))
						)
	isExcFiltCmb <- function( pFiltCmb ){
		for( idx in 1:length(excFiltCmbLst) ){
			if( length(pFiltCmb)==length(excFiltCmbLst[[idx]]) ){
				if( all(pFiltCmb==excFiltCmbLst[[idx]]) ){
					return(TRUE)
				}
			}
		}
		return(FALSE)
	} # isExcFiltCmb( )

    # --------------------------------------------------
    #	filtLst ,filtedIdx
    fndLst <- list()
    for( zIdx in 1:nrow(pZoidMtx) ){
		curCodeLst <- lapply( pCodeLst ,function(p){p[[zIdx]]})

		cfNameFndLst <- list()
		for( snIdx in pBanObj$cfNames ){ # search name index
			fndIdxLst <- list()
			for( eIdx in pBanObj$encVal.len:1 ){
				dCnt <- pBanObj$cfObjLst[[snIdx]]$diffCnt( 
								curCodeLst[[snIdx]] ,pBanObj$encValLst[[snIdx]][[eIdx]]
							)
				if( 0==dCnt ){
					fndIdxLst[[1+length(fndIdxLst)]] <- eIdx
				}
			}
			matIdxLst	<- lapply(fndIdxLst ,function(pFndIdx){
							hCodeLst <- lapply(pBanObj$encValLst,function(valLst){valLst[[pFndIdx]]})
							dCnt <- sapply(pBanObj$cfNames,function(mName){
											pBanObj$cfObjLst[[mName]]$diffCnt( curCodeLst[[mName]] ,hCodeLst[[mName]] )
										})
							return( which(dCnt==0) )
						})
			matCnt <- sapply(matIdxLst,length)
			fndIdxLst <- fndIdxLst[matCnt>1]
			matIdxLst <- matIdxLst[matCnt>1]

			if( 0<length(matIdxLst) ){
				excFlag	<- sapply(matIdxLst,isExcFiltCmb)
				fndIdxLst <- fndIdxLst[!excFlag]
				matIdxLst <- matIdxLst[!excFlag]
			}
			cfNameFndLst[[snIdx]] <- list( fndIdxLst=fndIdxLst ,matIdxLst=matIdxLst )
		} # snIdx
		fndLst[[1+length(fndLst)]] <- cfNameFndLst

    } # for(zIdx)

	nameLst <- list()
	filtLst <- list()
	for( fIdx in 1:length(fndLst) ){
		cfNameFndLst <- fndLst[[fIdx]]
		maxFnd <- 0
		for( snIdx in pBanObj$cfNames ){
			fndObj <- cfNameFndLst[[snIdx]]
			if( 0<length(fndObj$matIdxLst) ){
				if( pDebug ){
					nameCmb <- sapply( fndObj$matIdxLst ,function(p){paste(pBanObj$cfNames[p],collapse=" ")})
					nameLst[[1+length(nameLst)]] <- nameCmb
				}
				curMax <- max(sapply(fndObj$matIdxLst,length))
				maxFnd <- ifelse( curMax>maxFnd ,curMax ,maxFnd )
			}
		}
		filtLst[[1+length(filtLst)]] <- maxFnd
	}

	flag <- sapply( filtLst ,function(p){p>1})
	filtedIdx <- pInitZIdx[flag]

    rstObj <- list( idStr="multiDim" )
    rstObj$filtLst      <- filtLst
    rstObj$filtedIdx    <- filtedIdx
    # 디버깅용 -------
	if( pDebug ){
		rstObj$nameLst <- nameLst
	}
	
	return( rstObj )
	
} # ban.multiDim()

#   동일한 패턴이 여러 H동안 나타났을 때, 다음 H에도 동일하게 나타날 것인가?
#  	pBanObj<-banObj ;pZoidMtx<-gEnv$zhF ;pInitZIdx=NULL ;pCodeLst=codeLst   ;pLevel="mid"   ;pDebug=F
ban.throughH <-function( pBanObj ,pZoidMtx ,pCodeLst ,pInitZIdx=NULL ,pLevel="mid" ,pDebug=F ){

    if( is.null(pInitZIdx) ){
        pInitZIdx <- 1:nrow(pZoidMtx)
    }

    banFlagLst <- list( )
    for( depthIdx in 2:5 ){
        codeSearchSpan <- (pBanObj$encVal.len-depthIdx+1):pBanObj$encVal.len
        encValLst <- lapply( pBanObj$encValLst ,function(p){p[codeSearchSpan]})

        thld <- sapply( pBanObj$cfObjLst ,function(cfObj){
                            cfObj$throughHisMtx[
                                depthIdx==cfObj$throughHisMtx[,"depth"]
                                ,pLevel]
                        })
        names(thld) <- pBanObj$cfNames

        for( cfName in pBanObj$cfNames ){

            if( cfName %in% c("A0080","A0090") ){
                # "A0080", "A0090"에서는 0000 상태가 많아 폭주발생위험있음.
                if( all(c(0,0,0,0)==encValLst[[cfName]][[1]]) ){
                    next
                }
            }

            dupFlag <- rep( TRUE ,length(encValLst[[cfName]][[1]]) )
            for( codeIdx in 2:length(encValLst[[cfName]]) ){
                dupFlag <- dupFlag & encValLst[[cfName]][[1]]==encValLst[[cfName]][[codeIdx]]
            }
            if( thld[cfName]<=sum(dupFlag) ){
                banFlagObj <- list(depth=depthIdx,cfName=cfName,thldSize=thld[cfName])
                banFlagObj$rawCode <- encValLst[[cfName]][[1]]
                banFlagObj$rawCode.Idx <- codeSearchSpan[1]
                banFlagObj$dupFlag <- dupFlag
                banFlagObj$rawCode.chk <- banFlagObj$rawCode[dupFlag]

                banFlagLst[[1+length(banFlagLst)]] <- banFlagObj
            }
        } # cfName
    } # depthIdx

    filtLst <- list()
    for( pIdx in 1:nrow(pZoidMtx) ){
        bfIdxLst <- list()  # idx for banFlagLst
        for( banIdx in seq_len(length(banFlagLst)) ){
            banFlagObj <- banFlagLst[[banIdx]]
            chkCode <- pCodeLst[[banFlagObj$cfName]][[pIdx]][banFlagObj$dupFlag]
            if( all(banFlagObj$rawCode.chk==chkCode) ){
                bfIdxLst[[1+length(bfIdxLst)]] <- banIdx
            }
        } # banIdx
        filtLst[[1+length(filtLst)]] <- bfIdxLst
    }
    filtedIdx <- which( sapply(filtLst,length) > 0 )

    rstObj <- list( idStr="throughH" )
    rstObj$filtLst      <- filtLst
    rstObj$filtedIdx    <- filtedIdx
    # 디버깅용 -------
    rstObj$level        <- pLevel
    rstObj$banFlagLst     <- banFlagLst
    return( rstObj )

} # ban.throughH()

#   동일한 패턴이 여러 H동안 나타났을 때, 다음 H에도 동일하게 나타날 것인가?
#       단 한번씩 건너뛴 H에서의 패턴을 다룸.
#       pLevel : "hard","mid","easy"
#  	pBanObj<-banObj ;pZoidMtx<-gEnv$zhF ;pInitZIdx=NULL ;pCodeLst=codeLst   ;pLevel="mid"   ;pDebug=F
ban.throughH2 <-function( pBanObj ,pZoidMtx ,pCodeLst ,pInitZIdx=NULL ,pLevel="hard" ,pDebug=F ){

    if( is.null(pInitZIdx) ){
        pInitZIdx <- 1:nrow(pZoidMtx)
    }

    banFlagLst <- list( )
    for( depthIdx in 2:5 ){
        # 현재 pBanObj$encVal.len이 742라면 다음 차례는 743. 
        #   따라서 과거 패턴은 739, 741 스텝을 밟는다.
        stepSpan <- 2*(depthIdx:1)
        codeSearchSpan <- (pBanObj$encVal.len+1) - stepSpan

        encValLst <- lapply( pBanObj$encValLst ,function(p){p[codeSearchSpan]})

        thld <- sapply( pBanObj$cfObjLst ,function(cfObj){
                            cfObj$throughHisMtx[
                                depthIdx==cfObj$throughHisMtx[,"depth"]
                                ,pLevel]
                        })
        names(thld) <- pBanObj$cfNames

        for( cfName in pBanObj$cfNames ){

            if( cfName %in% c("A0080","A0090") ){
                # "A0080", "A0090"에서는 0000 상태가 많아 폭주발생위험있음.
                if( all(c(0,0,0,0)==encValLst[[cfName]][[1]]) ){
                    next
                }
            }

            dupFlag <- rep( TRUE ,length(encValLst[[cfName]][[1]]) )
            for( codeIdx in 2:length(encValLst[[cfName]]) ){
                dupFlag <- dupFlag & encValLst[[cfName]][[1]]==encValLst[[cfName]][[codeIdx]]
            }
            if( thld[cfName]<=sum(dupFlag) ){
                banFlagObj <- list(depth=depthIdx,cfName=cfName,thldSize=thld[cfName])
                banFlagObj$rawCode <- encValLst[[cfName]][[1]]
                banFlagObj$rawCode.Idx <- codeSearchSpan[1]
                banFlagObj$dupFlag <- dupFlag
                banFlagObj$rawCode.chk <- banFlagObj$rawCode[dupFlag]

                banFlagLst[[1+length(banFlagLst)]] <- banFlagObj
            }
        } # cfName
    } # depthIdx

    filtLst <- list()
    for( pIdx in 1:nrow(pZoidMtx) ){
        bfIdxLst <- list()  # idx for banFlagLst
        for( banIdx in seq_len(length(banFlagLst)) ){
            banFlagObj <- banFlagLst[[banIdx]]
            chkCode <- pCodeLst[[banFlagObj$cfName]][[pIdx]][banFlagObj$dupFlag]
            if( all(banFlagObj$rawCode.chk==chkCode) ){
                bfIdxLst[[1+length(bfIdxLst)]] <- banIdx
            }
        } # banIdx
        filtLst[[1+length(filtLst)]] <- bfIdxLst
    }
    filtedIdx <- which( sapply(filtLst,length) > 0 )

    rstObj <- list( idStr="throughH2" )
    rstObj$filtLst      <- filtLst
    rstObj$filtedIdx    <- filtedIdx
    # 디버깅용 -------
    rstObj$level        <- pLevel
    rstObj$banFlagLst     <- banFlagLst
    return( rstObj )

} # ban.throughH2()


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

	throughHisMtx <- matrix( 0 ,nrow=0 ,ncol=4 )
	throughHisMtx <- rbind( throughHisMtx ,c( 2 ,3 ,3 ,2 ) )
	throughHisMtx <- rbind( throughHisMtx ,c( 3 ,3 ,2 ,2 ) )
	throughHisMtx <- rbind( throughHisMtx ,c( 4 ,2 ,2 ,2 ) )
	throughHisMtx <- rbind( throughHisMtx ,c( 5 ,2 ,2 ,1 ) )
	colnames(throughHisMtx) <- c("depth","hard","mid","easy") # 1%이내, 2%부근 ,5% 부근
	cfObj$throughHisMtx <- throughHisMtx
	
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
	throughHisMtx <- matrix( 0 ,nrow=0 ,ncol=4 )
	throughHisMtx <- rbind( throughHisMtx ,c( 2 ,4 ,3 ,2 ) )
	throughHisMtx <- rbind( throughHisMtx ,c( 3 ,3 ,2 ,2 ) )
	throughHisMtx <- rbind( throughHisMtx ,c( 4 ,2 ,2 ,2 ) )
	throughHisMtx <- rbind( throughHisMtx ,c( 5 ,2 ,2 ,1 ) )
	colnames(throughHisMtx) <- c("depth","hard","mid","easy") # 1%이내, 2%부근 ,5% 부근
	cfObj$throughHisMtx <- throughHisMtx
	
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
	throughHisMtx <- matrix( 0 ,nrow=0 ,ncol=4 )
	throughHisMtx <- rbind( throughHisMtx ,c( 2 ,4 ,3 ,2 ) )
	throughHisMtx <- rbind( throughHisMtx ,c( 3 ,3 ,2 ,2 ) )
	throughHisMtx <- rbind( throughHisMtx ,c( 4 ,2 ,2 ,2 ) )
	throughHisMtx <- rbind( throughHisMtx ,c( 5 ,2 ,1 ,1 ) )
	colnames(throughHisMtx) <- c("depth","hard","mid","easy") # 1%이내, 2%부근 ,5% 부근
	cfObj$throughHisMtx <- throughHisMtx
	
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
	throughHisMtx <- matrix( 0 ,nrow=0 ,ncol=4 )
	throughHisMtx <- rbind( throughHisMtx ,c( 2 ,3 ,3 ,2 ) )
	throughHisMtx <- rbind( throughHisMtx ,c( 3 ,2 ,2 ,2 ) )
	throughHisMtx <- rbind( throughHisMtx ,c( 4 ,2 ,2 ,2 ) )
	throughHisMtx <- rbind( throughHisMtx ,c( 5 ,2 ,1 ,1 ) )
	colnames(throughHisMtx) <- c("depth","hard","mid","easy") # 1%이내, 2%부근 ,5% 부근
	cfObj$throughHisMtx <- throughHisMtx
	
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
	throughHisMtx <- matrix( 0 ,nrow=0 ,ncol=4 )
	throughHisMtx <- rbind( throughHisMtx ,c( 2 ,3 ,3 ,2 ) )
	throughHisMtx <- rbind( throughHisMtx ,c( 3 ,2 ,2 ,2 ) )
	throughHisMtx <- rbind( throughHisMtx ,c( 4 ,2 ,2 ,2 ) )
	throughHisMtx <- rbind( throughHisMtx ,c( 5 ,2 ,1 ,1 ) )
	colnames(throughHisMtx) <- c("depth","hard","mid","easy") # 1%이내, 2%부근 ,5% 부근
	cfObj$throughHisMtx <- throughHisMtx
	
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
	throughHisMtx <- matrix( 0 ,nrow=0 ,ncol=4 )
	throughHisMtx <- rbind( throughHisMtx ,c( 2 ,5 ,4 ,3 ) )
	throughHisMtx <- rbind( throughHisMtx ,c( 3 ,4 ,4 ,3 ) )
	throughHisMtx <- rbind( throughHisMtx ,c( 4 ,3 ,3 ,3 ) )
	throughHisMtx <- rbind( throughHisMtx ,c( 5 ,3 ,3 ,3 ) )
	colnames(throughHisMtx) <- c("depth","hard","mid","easy") # 1%이내, 2%부근 ,5% 부근
	cfObj$throughHisMtx <- throughHisMtx

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
	throughHisMtx <- matrix( 0 ,nrow=0 ,ncol=4 )
	throughHisMtx <- rbind( throughHisMtx ,c( 2 ,1 ,1 ,1 ) )
	throughHisMtx <- rbind( throughHisMtx ,c( 3 ,1 ,1 ,1 ) )
	throughHisMtx <- rbind( throughHisMtx ,c( 4 ,1 ,1 ,1 ) )
	throughHisMtx <- rbind( throughHisMtx ,c( 5 ,1 ,1 ,1 ) )
	colnames(throughHisMtx) <- c("depth","hard","mid","easy") # 1%이내, 2%부근 ,5% 부근
	cfObj$throughHisMtx <- throughHisMtx

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
	throughHisMtx <- matrix( 0 ,nrow=0 ,ncol=4 )
	throughHisMtx <- rbind( throughHisMtx ,c( 2 ,4 ,4 ,3 ) )
	throughHisMtx <- rbind( throughHisMtx ,c( 3 ,4 ,3 ,3 ) )
	throughHisMtx <- rbind( throughHisMtx ,c( 4 ,4 ,3 ,3 ) )
	throughHisMtx <- rbind( throughHisMtx ,c( 5 ,4 ,3 ,3 ) )
	colnames(throughHisMtx) <- c("depth","hard","mid","easy") # 1%이내, 2%부근 ,5% 부근
	cfObj$throughHisMtx <- throughHisMtx
	
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
	throughHisMtx <- matrix( 0 ,nrow=0 ,ncol=4 )
	throughHisMtx <- rbind( throughHisMtx ,c( 2 ,4 ,4 ,4 ) )
	throughHisMtx <- rbind( throughHisMtx ,c( 3 ,4 ,4 ,4 ) )
	throughHisMtx <- rbind( throughHisMtx ,c( 4 ,4 ,4 ,4 ) )
	throughHisMtx <- rbind( throughHisMtx ,c( 5 ,4 ,4 ,4 ) )
	colnames(throughHisMtx) <- c("depth","hard","mid","easy") # 1%이내, 2%부근 ,5% 부근
	cfObj$throughHisMtx <- throughHisMtx
	
	return( cfObj )

} # cf_A0090()


# ====================================================================================

getCFltCmbObj <- function( pEnv ){

    cfObjLst <- NULL
    encSpan <- 50:nrow(pEnv$zhF)
    hIdx.encVal <- encSpan
    encValLst <- list()
    for( tIdx in encSpan ){	# lastZoid 기반 동작들 때문에 1부터 시작은 의미없다.
        tEnv <- pEnv
        tEnv$zhF <- pEnv$zhF[1:(tIdx-1),]
        tEnv$allZoidMtx <- pEnv$zhF[tIdx,,drop=F]
        
        cfObjLst <- getCFLst.comb( tEnv )
        for( lIdx in seq_len(length(cfObjLst)) ){
            cfObj <- cfObjLst[[lIdx]]
            if( is.null(encValLst[[cfObj$idStr]]) ){
                encValLst[[cfObj$idStr]] <- list()
            }
            encValLst[[cfObj$idStr]][[1+length(encValLst[[cfObj$idStr]])]] <- cfObj$enc( tEnv$allZoidMtx )[[1]]
        }
    }

	# cfObjLst는 pEnv 기준으로 다시 만들어져야 한다.(lastZoid 값을 위해..)
	cfObjLst <- getCFLst.comb( pEnv )

    cfNames <- sapply(cfObjLst,function(p){p$idStr})
    encVal.len <- length(encValLst[[1]])

    cfNameMtx <- apply( permutations( length(cfNames) ,2 ) # 삭제
                         ,1 ,function(p){cfNames[p]}
                    )
    cfNameMtx <- t(cfNameMtx)   ;colnames(cfNameMtx) <- c("sbc.name","nzc.name")


	# =[ rObj ]=============================================================
    rObj <- list( cfNameMtx=cfNameMtx
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

    return( rObj )

} # getCFltCmbObj()


getCFLst.comb <- function( pEnv ){
	cfObjLst <- list()
	cfObjLst[[1+length(cfObjLst)]] <- cf_C0010w02( pEnv )
	cfObjLst[[1+length(cfObjLst)]] <- cf_C0010w04( pEnv )
	cfObjLst[[1+length(cfObjLst)]] <- cf_C0020w03( pEnv )
	cfObjLst[[1+length(cfObjLst)]] <- cf_C0030w04( pEnv )
	return( cfObjLst )
} # getCFLst.comb()


cf_CUtil.4enc <- function( pAVal ,pBVal ,pLen=NULL ){

	if( is.null(pLen) ){
		pLen <- length(pAVal)
	}
	rVal <- rep( 0 ,pLen )
	for( idx in 1:pLen ){
		if( (0==pAVal[idx])&&(0==pBVal[idx]) ){
			rVal[idx] <- 0
		} else if( (0==pAVal[idx])&&(1==pBVal[idx]) ){
			rVal[idx] <- 1
		} else if( (1==pAVal[idx])&&(0==pBVal[idx]) ){
			rVal[idx] <- 2
		} else {	# 1,1
			rVal[idx] <- 3
		}
	}

	return( rVal )

} # cf_CUtil.4enc()

cf_C0010w02 <- function( pEnv ){
	
	cfObj <- list( idStr="C0010w02" )
    cfObj$rawCfA <- cf_A0010( pEnv ,pBase=2 )
    cfObj$rawCfB <- cf_A0020( pEnv ,pBase=2 )
	cfObj$enc <- function( pZoidMtx ,pLogFunc=NULL ){

		tStmp <- Sys.time()
        aValLst <- cfObj$rawCfA$enc( pZoidMtx )
        bValLst <- cfObj$rawCfB$enc( pZoidMtx )
		val.len <- length(aValLst[[1]])
        rLst <- lapply(1:length(aValLst),function(idx){
                        cf_CUtil.4enc(aValLst[[idx]],bValLst[[idx]],pLen=val.len)
                    })

		if( !is.null(pLogFunc) ){
			tDiff <- Sys.time() - tStmp
			pLogFunc( sprintf("ID:%s cost:%.1f%s",cfObj$idStr,tDiff,units(tDiff)) )
		}
		
		return( rLst )
	} # cfObj$enc()
	cfObj$diffCnt <- function( p1 ,p2 ){
		return( sum(p1!=p2) )
	}

	throughHisMtx <- matrix( 0 ,nrow=0 ,ncol=4 )
	throughHisMtx <- rbind( throughHisMtx ,c( 2 ,3 ,3 ,2 ) )
	throughHisMtx <- rbind( throughHisMtx ,c( 3 ,3 ,2 ,2 ) )
	throughHisMtx <- rbind( throughHisMtx ,c( 4 ,2 ,2 ,2 ) )
	throughHisMtx <- rbind( throughHisMtx ,c( 5 ,2 ,2 ,1 ) )
	colnames(throughHisMtx) <- c("depth","hard","mid","easy") # 1%이내, 2%부근 ,5% 부근
	cfObj$throughHisMtx <- throughHisMtx
	
	return( cfObj )

} # cf_C0010w02()

cf_C0010w04 <- function( pEnv ){
	
	cfObj <- list( idStr="C0010w04" )
    cfObj$rawCfA <- cf_A0010( pEnv ,pBase=2 )
    cfObj$rawCfB <- cf_A0040( pEnv ,pBase=2 )
	cfObj$enc <- function( pZoidMtx ,pLogFunc=NULL ){

		tStmp <- Sys.time()
        aValLst <- cfObj$rawCfA$enc( pZoidMtx )
        bValLst <- cfObj$rawCfB$enc( pZoidMtx )
		val.len <- length(aValLst[[1]])
        rLst <- lapply(1:length(aValLst),function(idx){
                        cf_CUtil.4enc(aValLst[[idx]],bValLst[[idx]],pLen=val.len)
                    })

		if( !is.null(pLogFunc) ){
			tDiff <- Sys.time() - tStmp
			pLogFunc( sprintf("ID:%s cost:%.1f%s",cfObj$idStr,tDiff,units(tDiff)) )
		}
		
		return( rLst )
	} # cfObj$enc()
	cfObj$diffCnt <- function( p1 ,p2 ){
		return( sum(p1!=p2) )
	}

	throughHisMtx <- matrix( 0 ,nrow=0 ,ncol=4 )
	throughHisMtx <- rbind( throughHisMtx ,c( 2 ,3 ,3 ,2 ) )
	throughHisMtx <- rbind( throughHisMtx ,c( 3 ,3 ,2 ,2 ) )
	throughHisMtx <- rbind( throughHisMtx ,c( 4 ,2 ,2 ,2 ) )
	throughHisMtx <- rbind( throughHisMtx ,c( 5 ,2 ,2 ,1 ) )
	colnames(throughHisMtx) <- c("depth","hard","mid","easy") # 1%이내, 2%부근 ,5% 부근
	cfObj$throughHisMtx <- throughHisMtx
	
	return( cfObj )

} # cf_C0010w04()

cf_C0020w03 <- function( pEnv ){

	cfObj <- list( idStr="C0020w03" )
    cfObj$rawCfA <- cf_A0020( pEnv ,pBase=2 )
    cfObj$rawCfB <- cf_A0030( pEnv ,pBase=2 )
	cfObj$enc <- function( pZoidMtx ,pLogFunc=NULL ){

		tStmp <- Sys.time()
        aValLst <- cfObj$rawCfA$enc( pZoidMtx )
        bValLst <- cfObj$rawCfB$enc( pZoidMtx )
		val.len <- length(bValLst[[1]])
        rLst <- lapply(1:length(aValLst),function(idx){
                        cf_CUtil.4enc(aValLst[[idx]],bValLst[[idx]],pLen=val.len)
                    })

		if( !is.null(pLogFunc) ){
			tDiff <- Sys.time() - tStmp
			pLogFunc( sprintf("ID:%s cost:%.1f%s",cfObj$idStr,tDiff,units(tDiff)) )
		}

		return( rLst )
	} # cfObj$enc()
	cfObj$diffCnt <- function( p1 ,p2 ){
		return( sum(p1!=p2) )
	}

	throughHisMtx <- matrix( 0 ,nrow=0 ,ncol=4 )
	throughHisMtx <- rbind( throughHisMtx ,c( 2 ,3 ,3 ,2 ) )
	throughHisMtx <- rbind( throughHisMtx ,c( 3 ,3 ,2 ,2 ) )
	throughHisMtx <- rbind( throughHisMtx ,c( 4 ,2 ,2 ,2 ) )
	throughHisMtx <- rbind( throughHisMtx ,c( 5 ,2 ,2 ,1 ) )
	colnames(throughHisMtx) <- c("depth","hard","mid","easy") # 1%이내, 2%부근 ,5% 부근
	cfObj$throughHisMtx <- throughHisMtx

	return( cfObj )

} # cf_C0020w03()

cf_C0030w04 <- function( pEnv ){

	cfObj <- list( idStr="C0030w04" )
    cfObj$rawCfA <- cf_A0030( pEnv ,pBase=2 )
    cfObj$rawCfB <- cf_A0040( pEnv ,pBase=2 )
	cfObj$enc <- function( pZoidMtx ,pLogFunc=NULL ){

		tStmp <- Sys.time()
        aValLst <- cfObj$rawCfA$enc( pZoidMtx )
        bValLst <- cfObj$rawCfB$enc( pZoidMtx )
		val.len <- length(aValLst[[1]])
        rLst <- lapply(1:length(aValLst),function(idx){
                        cf_CUtil.4enc(aValLst[[idx]],bValLst[[idx]],pLen=val.len)
                    })

		if( !is.null(pLogFunc) ){
			tDiff <- Sys.time() - tStmp
			pLogFunc( sprintf("ID:%s cost:%.1f%s",cfObj$idStr,tDiff,units(tDiff)) )
		}

		return( rLst )
	} # cfObj$enc()
	cfObj$diffCnt <- function( p1 ,p2 ){
		return( sum(p1!=p2) )
	}

	throughHisMtx <- matrix( 0 ,nrow=0 ,ncol=4 )
	throughHisMtx <- rbind( throughHisMtx ,c( 2 ,3 ,3 ,2 ) )
	throughHisMtx <- rbind( throughHisMtx ,c( 3 ,3 ,2 ,2 ) )
	throughHisMtx <- rbind( throughHisMtx ,c( 4 ,2 ,2 ,2 ) )
	throughHisMtx <- rbind( throughHisMtx ,c( 5 ,2 ,2 ,1 ) )
	colnames(throughHisMtx) <- c("depth","hard","mid","easy") # 1%이내, 2%부근 ,5% 부근
	cfObj$throughHisMtx <- throughHisMtx

	return( cfObj )

} # cf_C0030w04()


cutEadge.colValCut <- function( gEnv ,allIdx ,colValLst ){

    surviveMtx <- matrix( F ,nrow=length(allIdx) ,ncol=6 )

    colIdx <- 1
    valMtx <- colValLst[[colIdx]]
    availVal <- valMtx["val", valMtx["freq",]>5 ]
    surviveMtx[,colIdx] <- apply( gEnv$allZoidMtx[allIdx,] ,1 ,function(p){p[colIdx]%in%availVal})

    colIdx <- 2
    valMtx <- colValLst[[colIdx]]
    availVal <- valMtx["val", valMtx["freq",]>5 ]
    surviveMtx[,colIdx] <- apply( gEnv$allZoidMtx[allIdx,] ,1 ,function(p){p[colIdx]%in%availVal})

    colIdx <- 3
    valMtx <- colValLst[[colIdx]]
    availVal <- valMtx["val", valMtx["freq",]>5 ]
    surviveMtx[,colIdx] <- apply( gEnv$allZoidMtx[allIdx,] ,1 ,function(p){p[colIdx]%in%availVal})

    colIdx <- 4
    valMtx <- colValLst[[colIdx]]
    availVal <- valMtx["val", valMtx["freq",]>5 ]
    surviveMtx[,colIdx] <- apply( gEnv$allZoidMtx[allIdx,] ,1 ,function(p){p[colIdx]%in%availVal})

    colIdx <- 5
    valMtx <- colValLst[[colIdx]]
    availVal <- valMtx["val", valMtx["freq",]>5 ]
    surviveMtx[,colIdx] <- apply( gEnv$allZoidMtx[allIdx,] ,1 ,function(p){p[colIdx]%in%availVal})

    colIdx <- 6
    valMtx <- colValLst[[colIdx]]
    availVal <- valMtx["val", valMtx["freq",]>5 ]
    surviveMtx[,colIdx] <- apply( gEnv$allZoidMtx[allIdx,] ,1 ,function(p){p[colIdx]%in%availVal})

    rObj <- list( idStr="cutEadge.colValCut" )
    rObj$flag <- apply( surviveMtx ,1 ,all )
    return( rObj )

} # cutEadge.colVal()

cutEadge.dup3Col <- function( gEnv ,allIdx ,colValLst ,pThld=5 ){

    surviveMtx <- matrix( TRUE ,nrow=length(allIdx) ,ncol=6 )
    for( colIdx in 1:6 ){
        lastPtnObj <- last3Ptn.ana( gEnv$zhF ,colIdx ,pThld=pThld )
        colSpan <- lastPtnObj$colSpan

        colFlag <- rep( TRUE ,length(allIdx) )
        for( aIdx in 1:length(allIdx) ){
            anaObj <- lastPtnObj$getAnaObj( allZoidMtx[aIdx,colIdx] )
            if( is.null(anaObj) ){
                next
            }
            for( stdIdx in 1:nrow(anaObj$stdCodeMtx) ){
                if( all(anaObj$stdCodeMtx[stdIdx,]==allZoidMtx[aIdx,colSpan] ) ){
                    colFlag[aIdx] <- FALSE
                    break
                }
            }
        } # aIdx
        surviveMtx[,colIdx] <- colFlag
    }

    rObj <- list( idStr="cutEadge.dup3Col" )
    rObj$flag <- apply( surviveMtx ,1 ,all )
    return( rObj )

} # cutEadge.dup3Col()

cutEadge.getCFltObj <- function( gEnv ,allIdx ){

    allZoidMtx <- gEnv$allZoidMtx[allIdx,]
    bRstObjLst <- list()

    banObj <- getCFltObj( gEnv )
    codeLst <- banObj$getCodeLst( allZoidMtx )
    bRstObj <- ban.hntSameRow(banObj ,allZoidMtx ,pCodeLst=codeLst)
    bRstObjLst[[1+length(bRstObjLst)]] <- bRstObj$filtedIdx
    bRstObj <- ban.hntCrossDim(banObj ,allZoidMtx ,pCodeLst=codeLst ,pDepth=2)
    bRstObjLst[[1+length(bRstObjLst)]] <- bRstObj$filtedIdx
    bRstObj <- ban.multiDim(banObj ,allZoidMtx ,pCodeLst=codeLst )
    bRstObjLst[[1+length(bRstObjLst)]] <- bRstObj$filtedIdx
    bRstObj <- ban.throughH(banObj ,allZoidMtx ,pCodeLst=codeLst ,pLevel="hard" )
    bRstObjLst[[1+length(bRstObjLst)]] <- bRstObj$filtedIdx
    bRstObj <- ban.throughH2(banObj ,allZoidMtx ,pCodeLst=codeLst ,pLevel="hard" )
    bRstObjLst[[1+length(bRstObjLst)]] <- bRstObj$filtedIdx

    banCmbObj <- getCFltCmbObj( gEnv )
    codeCmbLst <- banCmbObj$getCodeLst( allZoidMtx )
    bRstObj <- ban.hntSameRow(banCmbObj ,allZoidMtx ,pCodeLst=codeCmbLst)
    bRstObjLst[[1+length(bRstObjLst)]] <- bRstObj$filtedIdx
    bRstObj <- ban.hntCrossDim(banCmbObj ,allZoidMtx ,pCodeLst=codeCmbLst ,pDepth=2)
    bRstObjLst[[1+length(bRstObjLst)]] <- bRstObj$filtedIdx
    bRstObj <- ban.multiDim(banCmbObj ,allZoidMtx ,pCodeLst=codeCmbLst )
    bRstObjLst[[1+length(bRstObjLst)]] <- bRstObj$filtedIdx
    bRstObj <- ban.throughH(banCmbObj ,allZoidMtx ,pCodeLst=codeCmbLst )
    bRstObjLst[[1+length(bRstObjLst)]] <- bRstObj$filtedIdx
    bRstObj <- ban.throughH2(banCmbObj ,allZoidMtx ,pCodeLst=codeCmbLst )
    bRstObjLst[[1+length(bRstObjLst)]] <- bRstObj$filtedIdx

    filtedCnt <- rep( 0 ,length(allIdx) ) # qqe work
    for( idx in 1:length(bRstObjLst) ){
        filtedIdx <- bRstObjLst[[idx]]
        filtedCnt[filtedIdx] <- 1 + filtedCnt[filtedIdx]
    }

    rObj <- list( idStr="cutEadge.getCFltObj" )
    rObj$flag <- filtedCnt==0
    return( rObj )

} # cutEadge.getCFltObj()

cutEadge.remLstHard <- function( gEnv ,allIdx ){
	# 최저 기준
    logFile <- sprintf("./log/allZoidMtx%s_hard.log",saveId)
    tEnv <- list( allZoidMtx = gEnv$allZoidMtx[allIdx,]
                    ,zhF = gEnv$zhF
                    ,logFile = logFile
                    ,doLog = TRUE
                )
    tEnv$log <- function( pMsg ){ if(tEnv$doLog) k.FLog(pMsg ,pFile=tEnv$logFile) }
    tEnv$logStr <- function( pMsg ){ if(tEnv$doLog) k.FLogStr( pMsg ,pFile=tEnv$logFile) }

    tStmp <- Sys.time()
    filtFuncLst.hard <- getFiltLst.hard()
    remLst.hard <- list()
    for( fIdx in 1:length(filtFuncLst.hard) ){
        rstObj <- filtFuncLst.hard[[fIdx]]( tEnv )	# 소요시간 rstObj$tCost
        remLst.hard[[rstObj$filtId]] <- which( !rstObj$flag )
        tDiff <- Sys.time() - tStmp
    } # fIdx
    tDiff <- Sys.time() - tStmp

    surviveMtx <- matrix( TRUE ,nrow=length(allIdx) ,ncol=length(remLst.hard) )
    for( idx in 1:length(remLst.hard) ){
        surviveMtx[remLst.hard[[idx]] ,idx] <- FALSE
    }

    rObj <- list( idStr="cutEadge.remLstHard" )
    rObj$flag <- apply( surviveMtx ,1 ,all )    # QQE:work
    return( rObj )

} # cutEadge.remLstHard()


cutEadge.getColSeq <- function( gEnv ,allIdx ){

    colValLst <- apply( gEnv$allZoidMtx[allIdx,,drop=F] ,2 ,function(p){ sort(unique(p)) })

	# apply( gEnv$allZoidMtx[allIdx,,drop=F] ,2 ,function(p){ sort(unique(p)) } )


    fltMtx.col <- matrix( 0 ,nrow=length(allIdx) ,ncol=length(colValLst) )
    for( colIdx in 1:length(colValLst) ){
        for( valIdx in colValLst[[colIdx]] ){
            valMtx <- gEnv$zhF[gEnv$zhF[,colIdx]==valIdx , ,drop=F]
            if( 2>nrow(valMtx) ){
                next
            }

            seqObj <- getColSeq( valMtx ,pDepth=2 )
            seqIdx <- setdiff( which(seqObj$flag) ,colIdx )
            if( 0==length(seqIdx) ){  # 반복이 자기 자신뿐이라면...
                next
            }

            fltPtn <- valMtx[nrow(valMtx), seqIdx ]
            flag <- apply( gEnv$allZoidMtx[allIdx,,drop=F] ,1 ,function(zoid){ 
                            (zoid[colIdx]==valIdx) && (any(zoid[seqIdx]==fltPtn)) 
                        })
            fltMtx.col[flag,colIdx] <- 1 + fltMtx.col[flag,colIdx]
        }
    } # colIdx
    fltMtx.col.cnt <- apply( fltMtx.col ,1 ,sum )

    # zhF 에 대해서도 getColSeq() 적용.
    fltMtx.zhF <- matrix( 0 ,nrow=length(allIdx) ,ncol=6 )
    for( colIdx in 1:length(colValLst) ){
        for( valIdx in colValLst[[colIdx]] ){
            valMtx <- gEnv$zhF
            seqObj <- getColSeq( valMtx ,pDepth=2 )

            fltPtn <- valMtx[nrow(valMtx),seqObj$flag]
            flag <- apply( gEnv$allZoidMtx[allIdx,seqObj$flag,drop=F] ,1 ,function(p){ any(p==fltPtn) })
            fltMtx.zhF[flag,colIdx] <- 1 + fltMtx.zhF[flag,colIdx]
        }
    } # colIdx
    fltMtx.zhF.cnt <- apply( fltMtx.zhF ,1 ,sum )

    rObj <- list( idStr="cutEadge.getColSeq" )
    rObj$flag <- (fltMtx.col.cnt==0) & (fltMtx.zhF.cnt==0)
    return( rObj )

} # cutEadge.getColSeq()

cutEadge.getBanPtn <- function( gEnv ,allIdx ,pThldChk=1 ){

    banObj <- getBanPtn( gEnv$zhF )
    chkCnt <- sapply(banObj$ptnLst ,function(p){p$chkCnt})
    thldPtnIdx <- which(chkCnt<pThldChk)    # chkCnt에 대한 갯수 기준.

    banRst <- banObj$chkMatchAny( gEnv$allZoidMtx[allIdx,,drop=F] ,pDebug=T )
    rstLst <- banRst$rstLst
    #   chkMatchAny() 가 사용된다면 pThldChk는 의미없다.
    # rstLst <- lapply( banRst$rstLst ,function(p){ setdiff(p,thldPtnIdx) })

    rObj <- list( idStr="cutEadge.getBanPtn" )
    rObj$flag <- ( 0==sapply(rstLst,length) )
    return( rObj )

} # cutEadge.getBanPtn()

cutEadge.getBanSym <- function( gEnv ,allIdx ){

    colBanLst <- getBanSeqSym( gEnv$zhF )$colBanLst
    # colBanLst <- getBanSeqSym( gEnv$zhF[1:792,] )$colBanLst  # for test
    flagLst.base <- lapply( 1:length(allIdx) ,function(p){integer(0)})
    for( colIdx in seq_len(length(colBanLst)) ){

        if( 0==length(colBanLst[[colIdx]]) ){
            next
        }

        for( idx in seq_len(length(allIdx)) ){
            aIdx <- allIdx[idx]
            if( gEnv$allZoidMtx[aIdx,colIdx] %in% colBanLst[[colIdx]] ){
                flagLst.base[[idx]][ 1+length(flagLst.base[[idx]]) ] <- colIdx
            }
        }
    } # colIdx

    azColValLst <- apply( gEnv$allZoidMtx[allIdx,,drop=F] ,2 ,function(p){unique(p)} )
    flagLst.cv <- vector( "list", length(allIdx) )    # by column value
    for( azColIdx in 1:6 ){
        for( vIdx in azColValLst[[azColIdx]] ){
            valMtx <- gEnv$zhF[gEnv$zhF[,azColIdx]==vIdx ,]
            colBanLst <- getBanSeqSym( valMtx )$colBanLst
            for( colIdx in seq_len(length(colBanLst)) ){
                if( colIdx==azColIdx ){
                    next
                }
                if( 0==length(colBanLst[[colIdx]]) ){
                    next
                }
                for( idx in seq_len(length(allIdx)) ){
                    aIdx <- allIdx[idx]
                    if( gEnv$allZoidMtx[aIdx,azColIdx]==vIdx ){
                        if( gEnv$allZoidMtx[aIdx,colIdx] %in% colBanLst[[colIdx]] ){
                            flagLst.cv[[idx]][[ 1+length(flagLst.cv[[idx]]) ]] <- c( azColIdx ,vIdx ,colIdx )
                        }
                    }
                }
            } # colIdx
        }
    } # azColIdx

    rObj <- list( idStr="cutEadge.getBanSym" )
    rObj$flag <- sapply( seq_len(length(allIdx)) ,function(idx){ 
                            ( 0==length(flagLst.base[[idx]]) ) && ( 0==length(flagLst.cv[[idx]]) )
                        })

    return( rObj )

} # cutEadge.getBanSym()

cutEadge.getBanGrad <- function( gEnv ,allIdx ){

	valMtx <- gEnv$zhF

    # flagLst.base
    banMtx <- rbind( getBanGrad(valMtx)$banVal ,getBanGrad.n(valMtx)$banVal )
    banLst <- lapply( 1:ncol(banMtx) ,function(idx){ banMtx[,idx][!is.na(banMtx[,idx])] })
    flagLst.base <- lapply( allIdx ,function(aIdx){
                zoid <- gEnv$allZoidMtx[aIdx,]
                flag <- sapply( 1:length(zoid) ,function(idx){ zoid[idx]%in%banLst[[idx]] })
                return( which(flag) )
            })

    # flagLst.cv
    azColValLst <- apply( gEnv$allZoidMtx[allIdx,,drop=F] ,2 ,function(p){unique(p)} )
    banMtx <- matrix( NA ,nrow=2 ,ncol=ncol(valMtx) )
    flagLst.cv <- vector( "list", length(allIdx) )    # by column value
    for( azColIdx in 1:6 ){
		for( vIdx in azColValLst[[azColIdx]] ){
			tValMtx <- valMtx[valMtx[,azColIdx]==vIdx ,,drop=F]
			banMtx[1,] <- getBanGrad(tValMtx)$banVal
			banMtx[2,] <- getBanGrad.n(tValMtx)$banVal
			banLst <- lapply( 1:ncol(banMtx) ,function(idx){ banMtx[,idx][!is.na(banMtx[,idx])] })
			banLst[[ azColIdx ]] <- integer(0)
			for( idx in seq_len(length(allIdx)) ){
				zoid <- gEnv$allZoidMtx[ allIdx[idx] ,]
				if( zoid[azColIdx]!=vIdx ){
					next
				}
				flag <- sapply( 1:length(zoid) ,function(p){zoid[p]%in%banLst[[p]]})
				if( any(flag) ){
					flagLst.cv[[idx]][[ 1+length(flagLst.cv[[idx]]) ]] <- c(azColIdx,vIdx,which(flag)[1])
				}
			}
		} # vIdx
	} # azColIdx
	# tDiff <- Sys.time() - tStmp

    rObj <- list( idStr="cutEadge.getBanGrad" )
    rObj$flag <- sapply( seq_len(length(allIdx)) ,function(idx){ 
						(length(flagLst.base[[idx]])==0) && (length(flagLst.cv[[idx]])==0)
					})
    return( rObj )

} # cutEadge.getBanGrad()

cutEadge.getBanPtnColVal <- function( gEnv ,allIdx ){

	valMtx <- gEnv$zhF

	azColValLst <- apply( gEnv$allZoidMtx[allIdx,,drop=F] ,2 ,function(p){unique(p)} )
	flagLst.cv <- vector( "list" ,length(allIdx) )
	for( azColIdx in 1:6 ){
		for( vIdx in azColValLst[[azColIdx]] ){
			tValMtx <- valMtx[valMtx[,azColIdx]==vIdx ,]
			banObj <- getBanPtn( tValMtx )
			banRst <- banObj$chkMatchAny( gEnv$allZoidMtx[allIdx,,drop=F] ,pExcCol=azColIdx ,pDebug=T )
			
			for( idx in seq_len(length(allIdx)) ){
				if( 0==length(banRst$rstLst[[idx]]) ){
					next
				}
				zoid <- gEnv$allZoidMtx[ allIdx[idx] ,]
				if( zoid[azColIdx]!=vIdx ){
					next
				}
				flagLst.cv[[idx]][[ 1+length(flagLst.cv[[idx]]) ]] <- c(azColIdx,vIdx)
			}
		} # vIdx
	} # azColIdx

    rObj <- list( idStr="cutEadge.getBanPtnColVal" )
    rObj$flag <- 0==sapply(flagLst.cv,length)
    return( rObj )

} # cutEadge.getBanPtnColVal()

cutEadge.banDupSeq <- function( gEnv ,allIdx ){

	getBanDupVal <- function( pVal ){
		val.len <- length(pVal)
		if( 3>val.len ){
			return( NA )
		}
		if( pVal[val.len-2]==pVal[val.len-1] ){
			return( pVal[val.len] )
		} else {
			return( NA )
		}
	} # getBanDupVal( )

	valMtx <- gEnv$zhF

	# flagLst.base
	banVal <- apply( gEnv$zhF ,2 ,getBanDupVal )
	flagLst.base <- lapply( gEnv$allZoidMtx[allIdx,,drop=F] ,function(zoid){
							return( which(zoid==banVal) )
						})

	# flagLst.cv
	azColValLst <- apply( gEnv$allZoidMtx[allIdx,,drop=F] ,2 ,function(p){unique(p)} )
	flagLst.cv <- vector( "list" ,length(allIdx) )
	for( azColIdx in 1:6 ){
		for( vIdx in azColValLst[[azColIdx]] ){
			tValMtx <- valMtx[valMtx[,azColIdx]==vIdx ,,drop=F]
			banVal <- apply( tValMtx ,2 ,getBanDupVal )
			banVal[azColIdx] <- NA	# 현재 기준 컬럼은 제외시켜야..
			if( all(is.na(banVal)) ){
				next
			}
			for( idx in seq_len(length(allIdx)) ){
				zoid <- gEnv$allZoidMtx[allIdx[idx],]
				if( zoid[azColIdx]!=vIdx ){
					next
				}

				matIdx <- which(zoid==banVal)
				if( 0<length(matIdx) ){
					flagLst.cv[[idx]][[ 1+length(flagLst.cv[[idx]]) ]] <- c( azColIdx ,vIdx ,matIdx[1] )
				}
			}
		} # vIdx
	} # azColIdx

    rObj <- list( idStr="cutEadge.banDupSeq" )
    rObj$flag <- sapply( seq_len(length(allIdx)) ,function(idx){ 
						(length(flagLst.base[[idx]])==0) && (length(flagLst.cv[[idx]])==0)
					})

    return( rObj )

} # cutEadge.banDupSeq()

cutEadge.getBanRebBin <- function( gEnv ,allIdx ){

    stdCodeMtx <- gEnv$zhF %% 2
    stdCodeLen <- nrow(stdCodeMtx)
    allCodeMtx <- gEnv$allZoidMtx[allIdx,] %% 2
    allCodeLen <- nrow(allCodeMtx)

    # flagLst.base
    flagLst.base <- lapply( seq_len(length(allIdx)) ,function(p){integer(0)} )
    for( dIdx in 1:5 ){
        chkHIdx <- stdCodeLen +1 -(1:2*dIdx)
        if( 1>chkHIdx[2] ){
            break
        }
        if( !all(stdCodeMtx[chkHIdx[1],]==stdCodeMtx[chkHIdx[2],]) ){
            next
        }
        for( idx in seq_len(allCodeLen) ){
            if( all(stdCodeMtx[chkHIdx[1],]==allCodeMtx[idx,]) ){
                flagLst.base[[idx]][1+length(flagLst.base[[idx]])] <- dIdx
            }
        }
    } # for(dIdx)

    azColValLst <- apply( gEnv$allZoidMtx[allIdx,,drop=F] ,2 ,function(p){unique(p)} )
    flagLst.cv <- vector( "list" ,allCodeLen )
    for( azColIdx in 1:ncol(gEnv$zhF) ){
        for( vIdx in azColValLst[[azColIdx]] ){
            tCodeMtx <- (gEnv$zhF[gEnv$zhF[,azColIdx]==vIdx ,,drop=F]) %% 2
            tCodeMtxlen <- nrow( tCodeMtx )
            for( dIdx in 1:5 ){
                chkHIdx <- tCodeMtxlen +1 -(1:2*dIdx)
                if( 1>chkHIdx[2] ){
                    break
                }
                if( !all(tCodeMtx[chkHIdx[1],]==tCodeMtx[chkHIdx[2],]) ){
                    next
                }
                for( idx in seq_len(allCodeLen) ){
                    if( vIdx != gEnv$allZoidMtx[allIdx[idx],azColIdx] ){
                        next
                    }
                    if( all(tCodeMtx[chkHIdx[1],]==allCodeMtx[idx,]) ){
                        flagLst.cv[[idx]][[1+length(flagLst.cv[[idx]])]] <- c( azColIdx ,vIdx ,dIdx )
                    }
                }
            } # dIdx
        } # vIdx
    } # azColIdx

    rObj <- list( idStr="cutEadge.getBanRebBin" )
    rObj$flag <- sapply( seq_len(length(allIdx)) ,function(idx){ 
						(length(flagLst.base[[idx]])==0) && (length(flagLst.cv[[idx]])==0)
					})
    return( rObj )

} # cutEadge.getBanRebBin()

cutEadge.banDupSeqBin <- function( gEnv ,allIdx ){

    chkDup <- function( pMtx ){
        mtxLen <- nrow(pMtx)
        if( 3 > mtxLen ){
            return( NULL )
        }
        if( !all(pMtx[mtxLen-1,]==pMtx[mtxLen-2,]) ){
            return( NULL )
        }
        return( pMtx[mtxLen,] )
    }

    banCode <- chkDup( gEnv$zhF %% 2 )
    flag.base <- if( is.null(banCode) ){ rep(TRUE,length(allIdx))
                    } else { 
                        apply( gEnv$allZoidMtx[allIdx,]%%2 ,1 ,function(aCode){!all(aCode==banCode)} )
                    }

    azColValLst <- apply( gEnv$allZoidMtx[allIdx,,drop=F] ,2 ,function(p){unique(p)} )
    flagLst.cv <- vector( "list" ,length(allIdx) )
    for( azColIdx in 1:6 ){
        for( vIdx in azColValLst[[azColIdx]] ){
            tCodeMtx <- (gEnv$zhF[gEnv$zhF[,azColIdx]==vIdx ,,drop=F]) %% 2
            banCode <- chkDup( tCodeMtx )
            if( is.null(banCode) ){
                next
            }
            for( idx in seq_len(length(allIdx)) ){
                aIdx <- allIdx[idx]
                if( vIdx != gEnv$allZoidMtx[aIdx,azColIdx] ){
                    next
                }
                if( all( (gEnv$allZoidMtx[aIdx,]%%2)==banCode ) ){
                    flagLst.cv[[idx]][[ 1+length(flagLst.cv[[idx]]) ]] <- c( azColIdx ,vIdx )
                }
            }
        } # vIdx
    }

    rObj <- list( idStr="cutEadge.banDupSeqBin" )
    rObj$flag <- sapply( seq_len(length(allIdx)) ,function(idx){
                        (flag.base[idx]) && (0==length(flagLst.cv[[idx]]))
                    })
    return( rObj )

} # cutEadge.banDupSeqBin()


cutEadge.getBanSymBin <- function( gEnv ,allIdx ){

    scanSymm <- function( pMtx ){
        mtxLen <- nrow(pMtx)

        banPtnLst <- list()

        # A,B,B,? 패턴
        if( mtxLen>=3 ){
            if( all(pMtx[mtxLen,]==pMtx[mtxLen-1,]) ) {
                banPtnLst[[ 1+length(banPtnLst) ]] <- pMtx[mtxLen-2,]
            }
        }

        # A,B,C,B,? 패턴
        if( mtxLen>=4 ){
            if( all(pMtx[mtxLen,]==pMtx[mtxLen-2,]) ){
                banPtnLst[[ 1+length(banPtnLst) ]] <- pMtx[mtxLen-1,]
                banPtnLst[[ 1+length(banPtnLst) ]] <- pMtx[mtxLen-3,]
            }
        }

        return( banPtnLst )
    } # scanSymm

    # flag.base
    banPtnLst <- scanSymm( gEnv$zhF%%2 )
    flag.base <- apply( gEnv$allZoidMtx[allIdx,]%%2 ,1 ,function(aZoid){
                        flag <- sapply( banPtnLst ,function(banPtn){all(banPtn==aZoid)})
                        return( !any(flag) )
                    })

    # flagLst.cv
    azColValLst <- apply( gEnv$allZoidMtx[allIdx,,drop=F] ,2 ,function(p){unique(p)} )
    flagLst.cv <- vector( "list" ,length(allIdx) )
    for( azColIdx in 1:6 ){
        for( vIdx in azColValLst[[azColIdx]]){
            tMtx <- gEnv$zhF[gEnv$zhF[,azColIdx]==vIdx ,] %% 2
            banPtnLst <- scanSymm( tMtx )
            if( 0==length(banPtnLst) ){
                next
            }
            for( idx in seq_len(length(allIdx)) ){
                if( vIdx != gEnv$allZoidMtx[allIdx[idx],azColIdx] ){
                    next
                }
                aCode <- gEnv$allZoidMtx[allIdx[idx],] %% 2
                flag <- sapply( banPtnLst ,function(banPtn){ all(banPtn==aCode ) })
                if( any(flag) ){
                    flagLst.cv[[idx]][[ 1+length(flagLst.cv[[idx]]) ]] <- c( azColIdx ,vIdx )
                }
            } # idx
        } # vIdx
    }

    rObj <- list( idStr="cutEadge.getBanSymBin" )
    rObj$flag <- sapply( seq_len(length(allIdx)) ,function(idx){
                        (flag.base[idx]) && (0==length(flagLst.cv[[idx]]))
                    })
    return( rObj )

} # cutEadge.getBanSymBin()


