t00.case1 <- function( pSample ){
        
    }

t00.filters <- function(){
        rObj <- list()
        # fObj <- t00.sam00_Freq();       rObj[[fObj$name]] <- fObj
        # fObj <- t00.sam00_WTA();        rObj[[fObj$name]] <- fObj
        fObj <- t00.sam00_Seq(1);        rObj[[fObj$name]] <- fObj
        fObj <- t00.sam00_Seq(2);        rObj[[fObj$name]] <- fObj
        fObj <- t00.sam00_Seq(3);        rObj[[fObj$name]] <- fObj
        # fObj <- t00.sam00_MMean(50);    rObj[[fObj$name]] <- fObj
        return( rObj )
    }

t00.getProbMtx <- function( pFlag ){
		# 연속발생가능 확률 매트릭스 반환.
		# 	예를 들어서 1이 나타난 적이 6번 있었던 상태에서
		#	이중 바로 앞에 1인 경우가 3번, 바로 앞에 1이 아닌 경우가 3번 있었다면
		#	1연속이 발생할 확률은 0.5가 된다.
        
        vals <- sort(unique(pFlag))
        mtxRowNum <- length(pFlag)-1  # 최대 연속가능 수.

        seqCntMtx <- matrix( 0, ncol=length(vals), nrow=mtxRowNum ) # 연속발생 수(누적)
        colnames( seqCntMtx ) <- as.character( vals )
        rownames( seqCntMtx ) <- as.character( 1:mtxRowNum )

        seqFailCntMtx <- matrix( 0, ncol=length(vals), nrow=nrow(seqCntMtx) ) # 연속발생 실패 수
        colnames( seqFailCntMtx ) <- colnames( seqCntMtx )
        rownames( seqFailCntMtx ) <- rownames( seqCntMtx )

        for( vIdx in vals ){
            # vIdx <- 1
            vIdx.col <- as.character(vIdx)
            indexes <- which(pFlag==vIdx)
            for( dIdx in 1:(length(pFlag)-1) ){
                # dIdx <- 1
                indexes <- indexes[indexes<=(length(pFlag)-dIdx)]

                flag <- pFlag[indexes]==pFlag[indexes+dIdx]
                seqCntMtx[as.character(dIdx),vIdx.col] <- sum(flag)
                seqFailCntMtx[as.character(dIdx),vIdx.col] <- sum(!flag)

                indexes <- indexes[flag]
            } 
        } # for(vIdx)

        seqProbMtx <- ifelse( 0==(seqCntMtx+seqFailCntMtx)
                            , 0, seqCntMtx/(seqCntMtx+seqFailCntMtx) )
        colnames( seqFailCntMtx ) <- colnames( seqCntMtx )
        rownames( seqFailCntMtx ) <- rownames( seqCntMtx )

        return( seqProbMtx )
    }




t00.sam00_MMean <- function( pSize=20 ){
        rObj <- list( name=sprintf("t00.sam00_MMean%05d",pSize) )
        rObj$msSize <- pSize    # sample size for mean
        rObj$sample <- function( pHObj, pSamSize ){
                flag.L <- length(pHObj$flag)
                if( 1 > (flag.L-rObj$msSize) ){
                    myLog(sprintf( "Error:not enough flag(flag:%d msSize:%d)",flag.L,rObj$msSize ))
                }
                flag <- pHObj$flag[(flag.L-rObj$msSize):flag.L]
                    # 주어진 flag에 비해 msSize가 너무 큰 경우 처리필요.
                vals <- sort(unique(flag))
                valFreq <- table(flag)[as.character(vals)]
                sam <- sample( vals, pSamSize, replace=T, prob=valFreq )
                return( sam )
            }
        return( rObj )
    }

t00.sam00_Seq <- function( pTh=1 ){ 
        rObj <- list( name=sprintf("t00.sam00_Seq%02d",pTh) )
        rObj$th <- pTh  # threshold . 연속치를 몇 개 이상부터 적용할 것인지?
        rObj$sample <- function( pHObj, pSamSize ){
                seqProbMtx <- t00.getProbMtx( pHObj$flag )

                lastV <- pHObj$flag[length(pHObj$flag)]
                lastV.idx <- which(pHObj$vals==lastV)
                lastV.cnt <- 0  # 다음에도 연속이 계속될 수.
                                # 연속 횟수 이므로, 이전에 연달아 나온 갯수와 동일하다.

                for( idx in length(pHObj$flag):1 ){
                    if( lastV == pHObj$flag[idx] ){
                        lastV.cnt <- lastV.cnt + 1
                    } else { break }
                }

                if( lastV.cnt < rObj$th ){  # QQE
                    sam <- sample( pHObj$vals, pSamSize, replace=T, prob=pHObj$valFreq )
                    return(sam)
                }

                seqProb <- 0    # 연속발생 확률.
                if( lastV.cnt <= nrow(seqProbMtx) ){
                    seqProb <- seqProbMtx[as.character(lastV.cnt),as.character(lastV)]
                }

                sam <- rep( lastV, pSamSize )   # 일단 lastV로 도배해놓고...
                if( 1 < length(pHObj$vals) ){ # vals가 2가지 이상의 값일 때만 처리
                    lastV.nFlag <- sample( 0:1, pSamSize, replace=T, prob=c(seqProb,(1-seqProb)) )
                    sam[lastV.nFlag==1] <-
                        sample( pHObj$vals[-lastV.idx], sum(lastV.nFlag), replace=T, prob=pHObj$valFreq[-lastV.idx] )
                }

                return( sam )
            }
        return( rObj )
    }

t00.sam00_Seq.deprecated <- function(){    # 연속발생 기준 체크.(비연속은 none) <-- 폐기 고려 중.
                                #   발생빈도보다 연속발생 확률을 우선.
        rObj <- list( name="t00.sam00_Seq" )
        rObj$sample <- function( pHObj, pSamSize ){
                # 1st. 바로 이전 값이 재발할 빈도에 따른 T,F 배열 생성.
                lastV <- pHObj$flag[length(pHObj$flag)]
                lastV.idx <- which(pHObj$vals==lastV)
                lastV.seqCnt <- pHObj$seqCnt[length(pHObj$seqCnt)]
                lastV.prob <- pHObj$seqProb( lastV, lastV.seqCnt )
                lastV.flag <- sample( T:F, pSamSize, replace=T, prob=c(lastV.prob,(1-lastV.prob)) )

                # 2nd. F이면 이전 값이 아닌 다른 값을 발생빈도에 따라 적용.
                #       (T이면 바로 이전값이 적용된 상태)
                sam <- rep( lastV, pSamSize )
                sam.f.prob <- pHObj$valFreq[-lastV.idx]
                if( 0==max(sam.f.prob) ){
                    sam.f.prob <- rep(1,length(sam.f.prob))
                }
                sam[!lastV.flag] <- 
                    sample( pHObj$vals[-lastV.idx], length(sam[!lastV.flag]), replace=T, prob=sam.f.prob )
                
                return( sam )
            }

        return( rObj )
    }

t00.sam00_Freq <- function( ){
        
        rObj <- list( name="t00.sam00_Freq" )
        rObj$sample <- function( pHObj, pSampleSize ){
                freq <- table(pHObj$flag)
                sam <- with(pHObj,{ 
                        sample(vals,pSampleSize,replace=T,prob=freq[as.character(vals)]) 
                    })
                return( sam )
            }

        return( rObj )
    }

t00.sam00_WTA <- function(){   # Winner takes it all...
        rObj <- list( name="t00.sam00_WTA" )
        rObj$sample <- function( pHObj, pSampleSize ){
                freq <- table(pHObj$flag)
                freq <- freq[as.character(pHObj$vals)]  # vals와 table()출력결과 순서맞춤
                winner = (pHObj$vals[which(freq==max(freq))])[1]
                return( rep(winner,pSampleSize) )
            }
        return( rObj )
    }


t00.get_hSeq <- function( pFlag ){

        # =================================================================
        getProbMtx <- function( pVals, pFlag, pSeqCnt, pSeqCnt.0 ){ # 사용 폐기....

                seqCntMtx <- matrix( 0, ncol=length(pVals), nrow=(max(pSeqCnt)+1) ) # 연속발생 수 누적
                colnames( seqCntMtx ) <- as.character( pVals )
                rownames( seqCntMtx ) <- as.character( 0:max(pSeqCnt) )

                seqProbMtx <- matrix( 0, ncol=length(pVals), nrow=(max(pSeqCnt)+1) )
                colnames(seqProbMtx) <- as.character( pVals )
                rownames(seqProbMtx) <- as.character( 0:max(pSeqCnt) )

                for( vIdx in as.character(pVals) ){
                    # seqCntMtx ----------------------
                    seqCntMtx["0",vIdx] <- sum( pSeqCnt.0[pFlag==vIdx] )    # 연속없음 처리.

                    tt <- table( pSeqCnt[pFlag==as.integer(vIdx)] )         # 연속발생 누적횟수 처리
                                        # pSeqCnt에서의 0은 실제 연속발생 없음상태가 아님(앞 숫자만 다른 상태)
                    if( length(tt)>1 ){ # 연속발생이 존재하는 경우만 처리
                        tIdx <- 2:length(tt)
                        seqCntMtx[tIdx,vIdx] <- tt[tIdx]
                    }

                    # seqProbMtx(확률계산) -----------
                    cMax <- max(seqCntMtx[,vIdx])
                    seqProbMtx[,vIdx] <- ifelse( rep(cMax,nrow(seqCntMtx))==0, 0, seqCntMtx[,vIdx]/cMax )

                }   # for( vIdx )

                return( seqProbMtx )
            } # function

        seqProb <- function( pVal, pSeqCnt ){
                if( all(hObj$vals!=pVal) )
                    return( 0 )
                if( pSeqCnt >= nrow(hObj$seqProbMtx) )
                    return( 0 )

                return( hObj$seqProbMtx[as.character(pSeqCnt),as.character(pVal)] )
            }
        # =================================================================        

        hObj <- list( flag=pFlag )
        hObj$seqProb <- seqProb
        hObj$vals <- sort(unique( hObj$flag ))
        hObj$valFreq <- table(hObj$flag)[as.character(hObj$vals)]

        # Mtx 필요가 없지. 그냥 각 위치에서의 중복 카운트만 세면 되니까.
        # hObj$dupCntMtx <- with( hObj, { matrix(ncol=length(vals),nrow=length(flag)); })
        fL <- length(hObj$flag)

        # [seqCnt] ---------------------
        hObj$seqCnt <- rep( 0, fL ) # 맨 첫번째 요소는 중복발생 없음으로 가정한다.(0)
            # 중복발생 위치들은 업데이트 될 것이므로, 중복발생 없는 위치들은 0으로 그냥 놔둬도 됨.        
        tPos <- 2:fL
        for( rIdx in 1:(fL-1) ){ # fL-1 개까지는 진행되어야..
            tPos <- tPos[(tPos-rIdx)>0] # 검사시작 인덱스는 0보다 커야 하므로 
            tPos <- tPos[(hObj$flag[tPos]==hObj$flag[tPos-rIdx])]
            hObj$seqCnt[tPos] <- hObj$seqCnt[tPos]+1

            if( 0==length(tPos) )
                break;
        }

        hObj$seqCnt.0 <- rep( F, fL ) # 연속이 없는 위치. (맨 앞, 맨 뒤의 요소는 연속없음이 아닌 것으로 처리.)
            # 연속확률 계산시, 전체 경우의 수에 연속없음 횟수도 포함시키기 위해 구한다.
        tPos <- 2:(fL-1)
        hObj$seqCnt.0[tPos] <- with(hObj,{ (flag[tPos-1]!=flag[tPos]) & (flag[tPos]!=flag[tPos+1]) })

        hObj$seqProbMtx <- getProbMtx( hObj$vals, hObj$flag, hObj$seqCnt, hObj$seqCnt.0 )

        # [noneCnt] ---------------------
        hObj$noneCnt <- rep( 0, fL ) # 발생없음이 연속된 횟수.(맨 첫번째 요소는 0으로 처리)
        tPos <- 2:fL
        for( rIdx in 1:(fL-1) ){
            tPos <- tPos[(tPos-rIdx)>0]
            tPos <- tPos[(hObj$flag[tPos]!=hObj$flag[tPos-rIdx])]
            hObj$noneCnt[tPos] <- hObj$noneCnt[tPos]+1

            if( 0==length(tPos) )
                break;
        }

        hObj$noneCnt.0 <- rep( F, fL ) # 발생없음이 없는 횟수.(가치가 있는지 추가판단 필요.)
        tPos <- 2:fL
        hObj$noneCnt.0[tPos] <- with(hObj,{ (flag[tPos-1]==flag[tPos]) })
        hObj$noneProbMtx <- getProbMtx( hObj$vals, hObj$flag, hObj$noneCnt, hObj$noneCnt.0 )
            # hObj$noneProbMtx 사용성은 검토해볼 것.

        return(hObj)
    }


probMtx <- matrix( c(99,1), nrow=1, ncol=2 )
probMtx <- rbind( probMtx, c(95, 5) )
probMtx <- rbind( probMtx, c(90,10) )
probMtx <- rbind( probMtx, c(85,15) )
probMtx <- rbind( probMtx, c(80,20) )
probMtx <- rbind( probMtx, c(75,25) )
probMtx <- rbind( probMtx, c(70,30) )
probMtx <- rbind( probMtx, c(65,35) )
probMtx <- rbind( probMtx, c(60,40) )
probMtx <- rbind( probMtx, c(55,45) )
probMtx <- rbind( probMtx, c(50,50) )

probMtx <- rbind( probMtx, c(45,55) )
probMtx <- rbind( probMtx, c(40,60) )
probMtx <- rbind( probMtx, c(35,65) )
probMtx <- rbind( probMtx, c(30,70) )
probMtx <- rbind( probMtx, c(25,75) )
probMtx <- rbind( probMtx, c(20,80) )
probMtx <- rbind( probMtx, c(15,85) )
probMtx <- rbind( probMtx, c(10,90) )
probMtx <- rbind( probMtx, c( 5,95) )

