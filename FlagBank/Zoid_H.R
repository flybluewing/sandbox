
GZoid <- list( dnaCode=1:45 )
GZoid$dnaSize <- 6

#	pSize 만큼의 랜덤 zoid 생성
#		- pPreZoid가 주어질 경우, pPreZoid에 대한 중복체크 시행
#		- pMaxIt : 중복없는 newZoid를 찾을 때 까지 최대 반복시행
k.randomZoid <- function( pSize=10000 ,pPreZoid=NULL ,pMaxIt=1000 ,pLog=F ){

		if( pLog ){	k.FLog("k.randomZoid start") }
		rMtx <- matrix( NA ,nrow=pSize ,ncol=GZoid$dnaSize )
		rProb <- NULL
		if( !is.null(pPreZoid) ){
			distTbl <- table(as.vector(pPreZoid))
			rProb <- rep( max(distTbl)+1 ,length(GZoid$dnaCode) )
			names(rProb) <- GZoid$dnaCode
			rProb[names(distTbl)] <- rProb[names(distTbl)]-distTbl
			if( 2<max(rProb)-min(rProb) ){
				k.FLogStr("rProb applied",pConsole=T)
				k.FLog(rProb,pConsole=T)
			} else {
				rProb <- NULL
			}
		}
		mIdx.last <- 1	# pMaxIt까지 도달해버렸는지 확인용.
		mIdx.warnLevel <- (pMaxIt*9) %/% 10
		for( rIdx in 1:pSize ){
		
			if( pLog && ((rIdx>1) && (0==(rIdx%%1000))) ){
				k.FLogStr(sprintf("    rIdx:%d",rIdx))
			}
		
			for( mIdx in 1:pMaxIt ){
				if( is.null(rProb) ){
					newZoid <- sort(sample( GZoid$dnaCode ,GZoid$dnaSize ))
				} else {
					newZoid <- sort(sample( GZoid$dnaCode ,GZoid$dnaSize ,prob=rProb ))
				}
				
				chkSame <- apply( rMtx[1:rIdx,,drop=F] ,1 ,function(p){return(all(p==newZoid))} )
				if( any(chkSame,na.rm=T) ){
					if( mIdx > mIdx.warnLevel ){
						k.FLogStr(sprintf("same dna:%s %d/%d(%d)",paste(newZoid,collapse=" "),mIdx,pMaxIt,rIdx))
						mIdx.last <- mIdx
					}
					next
				}
				if( !is.null(pPreZoid) ){
					chkSame <- apply(pPreZoid,1,function(p){return(all(p==newZoid))})
					if( any(chkSame) ){
						if( mIdx > mIdx.warnLevel ){
							k.FLogStr(sprintf("same dna in preZoid:%s %d/%d(%d)",paste(newZoid,collapse=" "),mIdx,pMaxIt,rIdx))
							mIdx.last <- mIdx
						}
						next
					}
				}
				rMtx[rIdx,] <- newZoid
				break	# 새로운 Zoid 를 찾았으면 pMaxIt까지 가지않고 탈출.
			}	# mIdx
			
			if( mIdx.last==pMaxIt ){
				k.FLogStr(sprintf("Warn: reached to pMaxIt at %d",rIdx),pConsole=T)
				break
			}
			
		}	# rIdx
		if( pLog ){	k.FLog("k.randomZoid finish") }
		
		return( na.omit(rMtx)[,] )
		
	} # k.randomZoid