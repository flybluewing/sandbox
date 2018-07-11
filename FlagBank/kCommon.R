
# ======================================================================================
# krondor 전용 제네릭 함수들의 전언.
# --------------------------------------------------------------------------------------
getFlag <- function( pObj ,... ){
		UseMethod("getFlag")
	}
k.predict <- function( pObj ,... ){
		UseMethod("k.predict")
	}

k.report <- function( pObj ,... ){
		UseMethod("k.report")
	}


# ---------------------------------------------------------------------
# sameVal.Jidx : 벡터 내에서 동일한 값이 존재하는 지 확인.
#				단지 index만 반환한다.(맨 첫번째 위치는 제외)
#				Null이나 NA는 무시한다.
sameVal.Jidx <- function( p ){

		p.len <- length(p)
		if( 2 > p.len )
			return( integer(0) )

		dupIdx <- integer(0)
		for( idx in 1:(p.len-1) ){
			# idx <- 3
			if( is.na(p[idx]) )
				next
			dup <- which(p[idx] == p[(idx+1):p.len])
			dupIdx <- c( dupIdx ,(dup+idx))
		}

		return( unique(dupIdx) )

	}


# ---------------------------------------------------------------------
# sameRow : 동일한 Row 들을 검출.
#   pMtx에서 지정된 컬럼(pColNames)들을 기준으로 동일 데이터 조사.
sameRow <- function( pMtx ,pColNames=NULL ){

		isSame.withNA <- function( pDna1 ,pDna2 ){
				# NA 존재를 고려한 동일여부 비교.
				oneSideNA <- xor(is.na(pDna1),is.na(pDna2))
				if( any(oneSideNA) )
					return( FALSE )
				
				return( all( pDna1[!oneSideNA]==pDna2[!oneSideNA] ,na.rm=T ) )
			}

        rLst <- list()

        leftIndices <- 1:nrow(pMtx)
        if( is.null(pColNames) ){
            pColNames <- 1:ncol(pMtx)
        }

        for( rIdx in 1:nrow(pMtx) ){
            if( all(rIdx!=leftIndices) )
                next    # 이전에 동일 데이터가 발견된 row는 스킵
            if( rIdx==nrow(pMtx) )
                break;  # 어차피 마지막 row는 비교대상 없으므로.
                
            curIdx <- rIdx
            for( rrIdx in (rIdx+1):nrow(pMtx) ){
                # isSame = all(pMtx[rIdx,pColNames]==pMtx[rrIdx,pColNames])
                isSame <- isSame.withNA( pMtx[rIdx,pColNames] ,pMtx[rrIdx,pColNames] )
                if( !is.na(isSame) && isSame ){
                    curIdx <- c( curIdx ,rrIdx )
                }
            }

            if( 1 < length(curIdx) ){
                rLst[[as.character(rIdx)]] <- curIdx
            }

            leftIndices <- setdiff( leftIndices ,curIdx )
        }

        return( rLst )
    }

# ---------------------------------------------------------------------
# k.relative : 유사한 DNA 를 갖는 zoid가 얼마나...
#	- 과거의 hz들만 검색함.
k.relative <- function( pZh = FB$zh ,pLog=F ){

		if( pLog ){	k.FLogStr("k.relative start")	}
		
		# 과거로 갈수록 유사 dna가진 zoid가 몇 개 존재했는지(누적)
		cntMtx <- matrix( 0 ,nrow=nrow(pZh) ,ncol=(GZoid$dnaSize+1) )
		colnames(cntMtx) <- 0:GZoid$dnaSize

		rebLst <- list()	# 각 hZoid의 dna유사발생내역.
		rebLst[[1]] <- list()	# history 첫번째는 비교대상 없으므로
		for( rIdx in 2:nrow(pZh) ){
			# rIdx = 3
			for( bIdx in (rIdx-1):1 ){
				sCnt <- length(intersect(pZh[bIdx,],pZh[rIdx,]))
				# print(sprintf("rIdx:%d h:%d sCnt:%d",rIdx,(rIdx-bIdx),sCnt))
				cntMtx[(rIdx-bIdx),(sCnt+1)] <- cntMtx[(rIdx-bIdx),(sCnt+1)]+1		
			}
			rebLst[[rIdx]] <- apply( pZh[1:(rIdx-1),,drop=F] ,1 ,function(p){
									return(length(intersect(pZh[rIdx,],p)))
								} ) 	
		}
		if( pLog ){	k.FLogStr("k.relative rebLst")	}
		
		seqCntMtx <- NULL	# 유사 dna 규모가 연속된 내역
		for( idx in 2:length(rebLst) ){
			obj <- k.seq( rebLst[[idx]] )
			if( is.null(seqCntMtx) ){
				seqCntMtx <- obj$seqCntMtx
			} else {
				seqCntMtx <- rbind( seqCntMtx ,obj$seqCntMtx )
			}
		}
		if( pLog ){	k.FLogStr("k.relative seqCntMtx")	}

		zhMtx <- as.matrix(pZh)
		rebDna <- list() # 이전 Zoid와 공유하는 코드들의 위치.
		rebCnt <- rep( 0 ,nrow(zhMtx) ) # 유사 규모
		rebIdent <- rep( F ,nrow(zhMtx) ) # 규모뿐만 아니라 내용까지 동일한가?
		rebDna[[1]] <- list()
		for( idx in 2:nrow(zhMtx) ){
			rebDna[[idx]] <- which( zhMtx[idx,] %in% zhMtx[idx-1,] )
			rebIdent[idx] <- identical( rebDna[[idx]], rebDna[[(idx-1)]] )
			rebCnt[idx]	<- length(rebDna[[idx]])
		}
		names(rebDna) <- 1:nrow(pZh)
		names(rebIdent) <- 1:nrow(pZh)
		names(rebCnt) <- 1:nrow(pZh)
		#  rebCnt[rebIdent & rebCnt>0]

		rObj <- list()
		rObj$cntMtx <- cntMtx
		rObj$rebLst <- rebLst
		rObj$seqCntMtx <- seqCntMtx
		rObj$rebDna <- rebDna
		rObj$rebIdent <- rebIdent
		if( pLog ){	k.FLogStr("k.relative end")	}
		return( rObj )

	} # k.relative

#	pZoid와 유사 DNA를 hZoid발생내역 검토.
k.relativeSearch <- function( pZoid ,pZhMtx=NULL ){

		if( is.null(pZhMtx) )
			pZhMtx <- as.matrix( FB$zh )
		
		rebCode	<- list() # 중복발생 dna 코드
		rebPos	<- list()   # dna중복발생 위치.(pZoid기준)
		for(idx in 1:nrow(pZhMtx)){
			rebCode[[idx]]	<- intersect(pZoid,pZhMtx[idx,])
			rebPos[[idx]]	<- which( pZoid %in% pZhMtx[idx,] )
		}

		cName <- c("hIdx","dist","cnt")
		rebCntMtx	<- matrix( 0, ncol=length(cName) ,nrow=nrow(pZhMtx) )		
		colnames(rebCntMtx) <- cName
		rebCntMtx[,"hIdx"]	<- 1:nrow(pZhMtx)
		rebCntMtx[,"dist"]	<- nrow(pZhMtx):1
		rebCntMtx[,"cnt"]	<- sapply( rebPos ,length )
		
		rObj <- list( rebCode=rebCode )
		rObj$rebPos		<- rebPos
		rObj$rebCntMtx	<- rebCntMtx
		return( rObj )
		
	}



	
	
# ---------------------------------------------------------------------
# k.diff : 앞 DNA코드와의 차이
k.diff <- function( pZh=FB$zh ,pMax=7 ){

		wi <- 2:GZoid$dnaSize
		diffMtx <- pZh[,wi]-pZh[,(wi-1)]

		diffLst <- apply(diffMtx,1,function(p){return( table(p[p<=pMax]) )})

		rObj <- list( diffMtx = diffMtx )
		rObj$diffLst	<- diffLst
		
		diffCntMtx <- matrix( 0 ,ncol=pMax ,nrow=nrow(diffMtx) )
		colnames(diffCntMtx) <- 1:pMax
		for( idx in 1:length(diffLst) ){
			diffCntMtx[idx,names(diffLst[[idx]])] <- diffLst[[idx]]
		}
		rObj$diffCntMtx	<- diffCntMtx
		
		return( rObj )
	}
	
# ---------------------------------------------------------------------
# k.fillMatrix : pToMtx에 pFromMtx의 값을 채운 형태를 반환.
k.fillMatrix <- function( pFromMtx ,pToMtx ){
            # pFromMtx, pToMtx 모두 컬럼/로우 이름을 갖고 있어야 한다.
            # pToMtx 는 matrix,table,data.frame등이어야 한다.
            rNames <- rownames(pFromMtx)
            for( cnIdx in colnames(pFromMtx) ){
                pToMtx[rNames,cnIdx] <- pFromMtx[rNames,cnIdx]
            }
            return( pToMtx )
        }

# pBase로 나눈 배수 그룹별 몇 개를 갖는지?
#	- 예를들어 pBase가 10이면, DNA에서 0~9 몇 개, 10~19 몇 개...
#	- pMax 최대 몇 배수까지 그룹을 찾을 것인지?
#		pMax가 3이면 21~30구간까지 검색
k.chunk <- function( pBase ,pZh=FB$zh ,pMax=7 ){
		
		rCntMtx <- matrix( 0 ,nrow=nrow(zh) ,ncol=(pMax+1) )
		colnames(rCntMtx) <- 0:pMax

		rBaseDf <- pZh[1:10,] %/% pBase
		cntLst <- apply( rBaseDf ,1 ,function(p){return(table(p[p<=pMax]))} )

		for( rIdx in 1:nrow(rBaseDf) ){
			# rIdx <- 1
			rCntMtx[rIdx,names(cntLst[[rIdx]])] <- cntLst[[rIdx]]
		}

		rObj <- list( cntMtx = rCntMtx )
		rObj$baseDf <- rBaseDf
		
		return( rObj )
	}

#	각 포지션 별로, 동일 DNA 코드가 나온 경우 조사.
k.samePos <- function( pZh=FB$zh ){
		rObj <- list()
		for( cIdx in 1:ncol(pZh) ){
			seqCntMtx <- k.seq( pZh[,cIdx] )$seqCntMtx
			rObj[[cIdx]] <- seqCntMtx[seqCntMtx[,"cnt"]>1,]
		}
	}
	
#   pH 이전 년도에서 같은 위치 동일한 DNA 발생 갯수.
#   - pH가 3인 경우, 3년 이내에 동일위치 동일 DNA가 발생한 갯수
k.dupPosition <- function( pH ,pZh=FB$zh ){
        rObj <- rep( 0 ,nrow(pZh) )
        rObj[1:pH] <- NA

        calRange <- (pH+1):nrow(pZh)
        samePMtx <- matrix(0,ncol=0,nrow=length(calRange) )
        for( bhIdx in 1:pH ){
            mtx     <- pZh[calRange-bhIdx,]==pZh[calRange,]
            spNum   <- apply( mtx ,1 ,sum )
            samePMtx    <- cbind( samePMtx ,spNum )
        }

        rObj[calRange] <- apply(samePMtx,1,sum)
        return( rObj )
    }

# ---------------------------------------------------------------------
# k.rebound : 과거 pSH내 DNA재발 수.
#		반환 : 벡터. h에서의 값은 (h-1)~(h-pSH)에서 발생된 총 DNA수.
#				1~pSH 구간은 NA.
k.rebound <- function( pSH ,pZh ){
		nR <- nrow(pZh)
		rVec <- rep( NA ,nR )
		if( nR<=pSH )
			return( rVec )
			
		for( rIdx in (pSH+1):nR ){
			# rIdx <- 6
			sDna <- as.vector(as.matrix(pZh[rIdx-(1:pSH),]))
			sDna <- unique(sDna)
			rVec[rIdx] <- length(intersect(sDna,pZh[rIdx,]))
		}
		
		return( rVec )
	}

k.seq <- function( pFlag ){

		flagL <- length(pFlag)


		# -[hSeqCntMtx]---------------------------------------------
		# hSeqCntMtx[1,2]는 항상 0임. 제일 첫번째 원소는 연속이 없으므로
		hSeqCntMtx <- matrix( 0 ,ncol=2 ,nrow=flagL )
		rownames(hSeqCntMtx) <- as.character(1:flagL)
		colnames(hSeqCntMtx) <- c("val","seqCnt")
		hSeqCntMtx[,1] <- pFlag

		seqIdx <- 2:flagL
		for( dIdx in 1:(flagL-1) ){
			seqIdx <- seqIdx[0<(seqIdx-dIdx)]
			seqIdx <- seqIdx[ pFlag[seqIdx] == pFlag[seqIdx-dIdx] ]
			if( 0==length(seqIdx) ){
				break
			} else {
				hSeqCntMtx[seqIdx,2] <- hSeqCntMtx[seqIdx,2]+1
			}
		}

		# -[seqCntMtx]---------------------------------------------
		seqEndIdx <- which(hSeqCntMtx[,"seqCnt"]==0)   # 연속이 "끝"난 지점들
		
		seqCntMtx <- matrix( 1 ,ncol=3 ,nrow=length(seqEndIdx) )
		colnames(seqCntMtx) <- c("hIdx","val","cnt")

		seqCntMtx[,"hIdx"] <- seqEndIdx
		seqCntMtx[,"val"] <- hSeqCntMtx[seqEndIdx,"val"]
		for( dIdx in 1:(flagL-1) ){
			seqEndIdx <- seqEndIdx[flagL>=(seqEndIdx+dIdx)]
			seqEndIdx <- seqEndIdx[ pFlag[seqEndIdx]==pFlag[seqEndIdx+dIdx] ]
			if( 0==length(seqEndIdx) ){
				break
			} else {
				mtxIdx <- match( seqEndIdx, seqCntMtx[,"hIdx"] )
				seqCntMtx[mtxIdx,"cnt"] <- seqCntMtx[mtxIdx,"cnt"]+1				
			}
		}

		# -[seqPMtx]---------------------------------------------
		#	참고 : tapply( seqCntMtx[,"cnt"] ,list(seqCntMtx[,"val"],seqCntMtx[,"cnt"]) ,length )
		cdRange <- sort(unique(hSeqCntMtx[,"val"]))
		seq.max <- max(hSeqCntMtx[,"seqCnt"])
		# seqPMtx : 연속발생 확률
		seqPMtx <- matrix( 0 ,ncol=length(cdRange) ,nrow=seq.max )
		colnames(seqPMtx) <- cdRange
		# seqPMtx.cum : 연속발생 패턴 존재 인정.
		seqPMtx.cum <- matrix( 0 ,ncol=length(cdRange) ,nrow=seq.max )
		colnames(seqPMtx.cum) <- cdRange

		for( vIdx in cdRange ){
			seqCnt <- hSeqCntMtx[hSeqCntMtx[,"val"]==vIdx,"seqCnt"]

			for( cIdx in 1:seq.max ){
				#cIdx <- 1
				# 연속발생을 규칙적으로 볼것이냐, 확률적으로 볼 것이냐의 문제가 있다.
				# 	예를 들어 1연속, 4연속 발생이 규칙적인 것이라면,
				#	3연속 발생에서는 4연속이 100%이어야 한다.(한번 발생하면 우르르 발생.)
				prb <- sum(seqCnt>=cIdx)*100 / length(seqCnt) # 연속확률은 낮게 잡을 시.
				seqPMtx[cIdx,as.character(vIdx)] <- ifelse( is.nan(prb) ,NA ,prb )
				
				prb <- sum(seqCnt>=cIdx)*100 / sum(seqCnt>=(cIdx-1)) # 규칙적 발생 가정 시.
				seqPMtx.cum[cIdx,as.character(vIdx)] <- ifelse( is.nan(prb) ,NA ,prb )				
			}
		} # vIdx

		rLst <- list( hSeqCntMtx=hSeqCntMtx, seqCntMtx=seqCntMtx )
		rLst$seqPMtx <- seqPMtx
		rLst$seqPMtx.cum <- seqPMtx.cum
		rLst$cdRange <- cdRange
		return( rLst )

	}

# 두개 zoid집단간의 dna 유전 유사내역 조사
#	pSim개 이상으로 유사한 것들만 추출
k.compareZoid <- function( pZMtx1 ,pZMtx2 ,pSim=5 ,pLog=F ){

		cName <- c("zMtx1","zMtx2","cnt")
		rMtx <- matrix( 0 ,ncol=length(cName) ,nrow=0 )
		for( r1Idx in 1:nrow(pZMtx1) ){
			curZ <- pZMtx1[r1Idx,]
			lst <- apply(pZMtx2,1,function( p ){
						sum(curZ%in%p)
					})
			mtx <- matrix( 0 ,ncol=length(cName) ,nrow=sum(lst>=pSim) )
			mtx[,1] <- r1Idx
			mtx[,2] <- which(lst>=pSim)
			mtx[,3] <- lst[lst>=pSim]
			if( 0==(r1Idx%%100) ){
				k.FLogStr(sprintf("    r1Idx:%d",r1Idx))
			}
			rMtx <- rbind( rMtx ,mtx )
		} # for
		
		return( rMtx )
	}

k.payment <- function( pStdZ ,pZMtx ,pRemove=NULL ,pLog=F ){
		
		totCost=0.1*nrow(pZMtx)
		payTable <- c( 0 ,0 ,0 ,0.5 ,5*2/3 ,129*2/3 ,10000 )
		
		cName = c("hit","pay")
		relMtx <- matrix( 0 ,ncol=length(cName) ,nrow=nrow(pZMtx) )
		colnames(relMtx) <- cName
		relMtx[,"hit"] <- apply( pZMtx ,1 ,function(p){sum(p%in%pStdZ)} )
		relMtx[,"pay"] <- payTable[1+relMtx[,"hit"]] # 0 도 고려해야 하므로 +1
		
		hitV <- rep(0,7);		names(hitV) <- 0:6
		hitTbl <- table(relMtx[,"hit"])
		hitV[names(hitTbl)] <- hitTbl
		
		rObj <- list( relMtx=relMtx )
		rObj$totCost=totCost
		rObj$hitV <- hitV
		rObj$totPay = sum(relMtx[,"pay"])
		rObj$removePay = ifelse( is.null(pRemove) ,0 ,sum(relMtx[pRemove,"pay"]) )
		if(pLog){ 
			k.FLogStr(sprintf( "Pay %.1f from %.1f(%d)",rObj$totPay,totCost,nrow(pZMtx) )
				,pConsole=T )
			k.FLogStr(sprintf( "Pay lost %.1f(%.1f)",rObj$removePay,100*rObj$removePay/rObj$totPay )
				,pConsole=T )
			k.FLogStr(sprintf( "    Remove:%d from %d",ifelse(is.null(pRemove),0,length(pRemove)),nrow(pZMtx) )
				,pConsole=T )
			k.FLog( rObj$hitV ,pConsole=T )
		}

		return( rObj )
	}
	#  o <- k.payment( pStdZ ,pZMtx ,pLog=T )

# ======================================================================================
# Artificial Nerual Network
# --------------------------------------------------------------------------------------

getNnOpt <- function( ){
			nnOpt <- list( );	class(nnOpt) <- "mlpOption"

			nnOpt$size <- c(20,20);	nnOpt$maxit <- 10000
			nnOpt$initFunc="Randomiz_Weights"			
			nnOpt$initFuncParams <- c(-1.5,1.5)
			nnOpt$learnFunc="Std_Backpropagation"
			nnOpt$learnFuncParams <- c(0.2,0)
			nnOpt$updateFunc="Topological_Order"
			nnOpt$updateFuncParams <- c(0)
			nnOpt$hiddenActFunc="Act_Logistic"
			nnOpt$shufflePattern=T
			nnOpt$linOut=T
			return( nnOpt )
		}

k.standardize <- function( p ,pPer=T ){
		if( 0==sum(p) ){
			k.FLogStr("Error : k.standardize() - sum(p) is zero")
		}
		
		if(pPer){
			# % 기준으로 변환
			p.r <- 100*p/sum(p)
			# 스케일 평준화.(최대 최소값의 합이 100%가 나오도록)
			p.r <- p.r * (100/sum(range(p.r)))
		} else {
			p.r <- p/sum(p)
			p.r <- p.r / sum(range(p.r))
		}
		
		return( p.r )
	}
		
# ======================================================================================
# Logging
# --------------------------------------------------------------------------------------

# 로깅 옵션(글로벌한 디폴트 옵션.)
k.getFLogOpt <- function(){
		rObj <- list( )
		rObj$defaultLogFile	<- "./log/default.log"
		rObj$logTime	<- T
		rObj$append		<- T
		rObj$console	<- F
		return( rObj )
	}
k.FLogOpt <- k.getFLogOpt( )	# 글로벌 옵션 변경 후, 원래로 되돌리기 원할 때도 사용.


# 로깅함수
k.FLog <- function( pMsg ,pFile=k.FLogOpt$defaultLogFile 
					,pTime=k.FLogOpt$logTime
					,pAppend=k.FLogOpt$append
					,pConsole=k.FLogOpt$console
				)
	{

		if( pTime ){
			cat( sprintf("[%s] %s",Sys.time(),capture.output(pMsg)), file=pFile, sep="\n", append=pAppend ) 
		} else {
			cat( sprintf("%s",capture.output(pMsg)), file=pFile, sep="\n", append=pAppend ) 
		}
		
		if( pConsole ){
			if( pTime ){
				print( sprintf("[%s] %s",Sys.time(),capture.output(pMsg)), file=pFile, sep="\n", append=pAppend ) 
			} else {
				print( sprintf("%s",capture.output(pMsg)), file=pFile, sep="\n", append=pAppend ) 
			}
		}

	}
	
k.FLogStr <- function( pMsg ,pFile=k.FLogOpt$defaultLogFile
						,pTime=k.FLogOpt$logTime
						,pAppend=k.FLogOpt$append
						,pConsole=k.FLogOpt$console
					)
	{

		if( pTime ){
			cat( sprintf("[%s] %s",Sys.time(),pMsg), file=pFile, sep="\n", append=pAppend ) 
		} else {
			cat( sprintf("%s",pMsg), file=pFile, sep="\n", append=pAppend ) 
		}
		
		if( pConsole ){
			if( pTime ){
				print( sprintf("[%s] %s",Sys.time(),pMsg), file=pFile, sep="\n", append=pAppend ) 
			} else {
				print( sprintf("%s",pMsg), file=pFile, sep="\n", append=pAppend ) 
			}
		}

	}

#	pMtx=cvr$cvrRateMtxLst.clue[[idx]] ;pFmt="%3.0f" ;pNA.str="  ." ;pIndent="  " ;pRowName=paste("rn:",1:nrow(pMtx),sep="")
k.matrixToStr <- function( pMtx ,pFmt="%3.0f" ,pNA.str="  ." ,pIndent="  " ,pRowName=NULL ){

								if( 0==nrow(pMtx) )
									return( "" )
								
								# row name 생성 예시.
								# 	rowFmtStr <- sprintf("[%%%dd]",ceiling(log10(nrow(pMtx))))
								if( !is.null(pRowName) && (nrow(pMtx)!=length(pRowName)) )
									return( "    k.matrixToStr() error. pRowName length is not same pMtx row length" )
								
								strs <- apply(pMtx,1,function(p){
													strV <- sprintf(pFmt,p)
													strV[strV==" NA"] <- pNA.str
													return( paste(strV,collapse="") )
												})

								if( !is.null(pRowName) ){
									for( idx in 1:length(strs) ){
										strs[idx] <- sprintf("[%s] %s",pRowName[idx],strs[idx])
									}
								}

								return( paste( paste(pIndent,strs,sep="") ,collapse="\n") )
							}


k.str <- function( pObj ) {
		UseMethod("k.str")
	}

k.str.mlpOption <- function( pObj ){

			rStr				 <- sprintf("size:%s maxit:%d",paste(pObj$size,collapse=",") ,pObj$maxit)
			rStr[length(rStr)+1] <- sprintf("initFunc:%s initFuncParams:%s",pObj$initFunc,paste(pObj$initFuncParams,collapse=","))
			rStr[length(rStr)+1] <- sprintf("learnFunc:%s learnFuncParams:%s",pObj$learnFunc,paste(pObj$learnFuncParams,collapse=","))
			rStr[length(rStr)+1] <- sprintf("updateFunc:%s updateFuncParams:%s",pObj$updateFunc,paste(pObj$updateFuncParams,collapse=","))
			rStr[length(rStr)+1] <- sprintf("hiddenActFunc:%s",pObj$hiddenActFunc)
			rStr[length(rStr)+1] <- sprintf("shufflePattern:%s linOut:%s",pObj$shufflePattern,pObj$linOut)

			return( paste(rStr,collapse="\n") )
	}

kLog.getPerStr <- function( frag ,tot ,pUseName=TRUE ,pLong=FALSE ) {
    perVal <- frag*100/tot
	perStr <- sapply( 1:length(frag) ,function(idx){
						if( pUseName && !is.null(names(frag)) ){
							if( pLong ){
								sprintf("%s:%.1f%%(%d/%d)",names(frag)[idx],perVal[idx],frag[idx],tot)
							} else {
								sprintf("%s:%.1f%%",names(frag)[idx],perVal[idx])
							}
						} else {
							if( pLong ){
								sprintf("%.1f%%(%d/%d)",perVal[idx],frag[idx],tot)
							} else {
								sprintf("%.1f%%",perVal[idx])
							}
						}
					})
	return( perStr )
} # kLog.getPerStr()


