# ==============================================================
#	krondor's lib
# --------------------------------------------------------------
#	공용함수
# ==============================================================

# --------------------------------------------------------------
#	주어진 벡터의 값들을 1.0에 기준한 비율로 표준화.
#		예를들어 c(5,2,3)은 c(0.5,0.2,0.3)으로 반환.
standardize.BaseOne <- function( pVec ){
	return( pVec/sum(pVec) )
}

# --------------------------------------------------------------
#	주어진 벡터의 값들을 표준화.
#		최대값은 1.0, 최소값은 0.0으로 표현. (모두 같으면 모두 1.0)
#		예를들어 c(5,2,3)은 c(1.0,0.0,0.3)으로 반환.
standardize.Max <- function( pVec ){
	minV <- min(pVec)
	rng <- max(pVec) - minV
	if( rng==0 ){
		return( rep(1,length(pVec)) )
	} else {
		return( (pVec-minV)/rng )
	}
}


# ---------------------------------------------------------------------
# seqCount : DNA 염기 연속발생 Count
# 	return : matrix. ["값","연속발생 횟수","발생시작점"]
seqCount <- function( seqV ){
	rMtx <- matrix( ncol=3, nrow=0 )

	fva <- c( seqV[1], 0, 1 )	# 아직 Count는 0이다.
	# names(fva) <- c("value","count","startIdx")
	for( idx in 1:length(seqV) ){
		if( fva[1] != seqV[idx] ){
			rMtx <- rbind(rMtx,fva)
			fva <- c( seqV[idx], 0, idx )
		}
		fva[2] <- 1+fva[2]
	}
	rMtx <- rbind(rMtx,fva)
	colnames(rMtx) <- c("value","SeqCount","startIndex")
	
	return( rMtx )
}	# seqCount




# ---------------------------------------------------------------------
# seqBias : 염기 연속발생 통계분포
#   테스트 샘플 : c( 0,1,2,0,0, 1,1,1,2,2, 0,0,0,1,1, 1,1,2,2,2, 2,0,0,0,0 )
# 	return : class
#           $vals : pVec 내의 값 종류
#           $recurMtx : vals값 별(col) 연속발생 횟수(row)
seqDistribute <- function( pVec ){
    # 발견된 값들의 목록
    rClass <- list(vals=sort(unique( pVec )))
    rClass$valCnt <- rClass$vals
    names(rClass$valCnt) <- rClass$vals
    rClass$valCnt[names(rClass$valCnt)] <- table(pVec)

    # recurMtx 생성.
    #   - 모두 0으로 초기화.(연속발생 통계가 없으면 연속발생 불가로 간주)
    #   - curVal에 없는 값이 새로 발견된 경우, 연속발생 불가로 간주.
    rowSize <- length(pVec) # 연속발생 수(0 ~ 연속 최대가능 수)
    colSize <- length(vals)
    recurMtx <- matrix( rep(0,rowSize*colSize), nrow=rowSize, ncol=colSize )
    colnames(recurMtx) <- vals; rownames(recurMtx) <- 1:length(pVec)

    for( vIdx in 1:length(vals) ){
        curVal <- vals[vIdx]
        foundLoc <- which( pVec==curVal )

        # 연속횟수 통계
        for( rIdx in 1:(length(pVec)-1) ){
            foundLoc <- foundLoc[ (foundLoc+rIdx) <= length(pVec) ]
                # rIdx 만큼의 비교대상보다 작은 위치만 남김.
            if( 0==length(foundLoc) ){
                break;
            }
            seqFnd <- pVec[foundLoc]==pVec[foundLoc+rIdx]
                # 이전 루프에서, rIdx 이전 구간은 연속상태임이 확인된 상태임.
            foundLoc <- foundLoc[seqFnd]
            recurMtx[rIdx,vIdx] <- sum(seqFnd)
        }   # for(rIdx)

    }   # for(vIdx)

	fndSum <- apply( recurMtx, 1, sum )
	seqMax <- max(which(fndSum>0))
		
    rClass$recurMtx <- recurMtx[1:seqMax,]
    return( rClass )
}

# seqDistribute() 결과에 기반하여 재발확률 계산
seqDistClass <- function( pVals, pRecurMtx ){
    rObject <- list( vals=pVals )
    rObject$recurProbMtx <- pRecurMtx
    rObject$maxSeqCnt <- nrow(pRecurMtx)
	rObject$recurProbMtx <- standardize.Max(rObject$recurProbMtx)
	
    rObject$prob <- function( pVal, pSeq ){
        if( 0==sum(pVal==rObject$vals) ){
            return( 0.0 )   # 값 자체가 처음나온 값인 경우
        }
        if( pSeq>rObject$maxSeqCnt ){
            return( 0.0 )
        }
        cIdx <- which(rObject$vals==pVal)
        return( rObject$recurProbMtx[pSeq,cIdx] )
    } # $bias

    return( rObject )
}

#	(!) 필수사항 : jsonlite 라이브러리
DataLogFile <- function( pFileName ){
		classlst <- list()
		
		classlst$fileName <- pFileName
		
		# 파일내용 초기화
		cat( "", file=classlst$fileName, append=F )
		
		classlst$log <- function( logStr ){
				cat( logStr, file=classlst$fileName, sep="\n", append=T )
			}
		classlst$jsonLog <- function( logObj ){
				classlst$log( toJSON(logObj) )
			}
		
		return( classlst )
	}
	#	readLines() 나 read.table() 함수 사용.

	logstr <- function( p ){
	paste( p, collapse="\t" )
}

logstr.table <- function( pT ){
	if( nrow(pT)==0 )
		return("")
	
	# 일단 테이블이 한개 row값만 가질때를 처리하자.
	strs <- c(	paste(names(pT),collapse="\t"),
				paste(pT,collapse="\t")
				)
	return(paste(strs,collapse="\n"))
}

logstr.matrix <- function( pM ){
	strs <- rep("",nrow(pM))
	for( idx in 1:nrow(pM) ){
		strs[idx] <- paste( pM[idx,], collapse="\t" )
	}
	return(paste(strs,collapse="\n"))
}

# ---------------------------------------------------------------------
# sameRow : 동일한 Row 들을 검출.
#   pMtx에서 지정된 컬럼(pColNames)들을 기준으로 동일 데이터 조사.
sameRow <- function( pMtx ,pColNames ){

        rLst <- list()

        leftIndices <- 1:nrow(pMtx)
        for( rIdx in 1:nrow(pMtx) ){
            if( all(rIdx!=leftIndices) )
                next    # 이전에 동일 데이터가 발견된 row는 스킵
            if( rIdx==nrow(pMtx) )
                break;
                
            curIdx <- rIdx
            for( rrIdx in (rIdx+1):nrow(pMtx) ){
                if( all(pMtx[rIdx,pColNames]==pMtx[rrIdx,pColNames]) ){
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


