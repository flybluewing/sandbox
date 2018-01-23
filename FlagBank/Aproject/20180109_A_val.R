# 20180109_A_val.R filt 함수들에 대한 성능검증 코드들.

#-[AQ000.C]------------------------------------------------------
#	다음 H에서 DNA가 1개 재발
Val_AQ000.C <- function( pEnv ){	#
	
	filtId="AQ000.C";	tStmp <- Sys.time()
	zhF <- pEnv$zhF
	lastZoid <- zhF[nrow(pEnv$zhF),]
	allCodeMtx <- pEnv$allZoidMtx

	rebLst <- list()
	for( hIdx in 2:nrow(zhF) ){
		rebObj <- list( hIdx=hIdx )
		rebVal <- intersect( zhF[hIdx,] ,zhF[(hIdx-1),] )
		if( 1==length(rebVal) ){
			rebValQuo <- rebVal %/% 10
			rebObj$info <- c( rebVal 
								,which(zhF[hIdx-1,]==rebVal) 
								,which(zhF[hIdx,]==rebVal)
								,sum( (zhF[hIdx-1,]%/%10)==rebValQuo )
								,sum( (zhF[hIdx,]%/%10)==rebValQuo )
							) 
			rebLst[[1+length(rebLst)]] <- rebObj
		}
	}
	rebMtx <- do.call( rbind ,lapply(rebLst,function(p){p$info}) ) # 324

	# 1~9는 거의 맨 앞이고, 40~45는 대부분 맨 뒤라서 의미없음. 제거.
	selFlag <- (rebMtx[,1]>=10)&(rebMtx[,1]<=39)
	reb.hIdx <- do.call( c ,lapply(rebLst[selFlag],function(p){p$hIdx}) )
	rebMtx <- rebMtx[selFlag ,] # 226
	rownames(rebMtx) <- reb.hIdx
	colnames(rebMtx) <- c("val","pos.p","pos.c","quo.p","quo.c") # 값, 이전/이후 위치&그룹크기

	matMtx <- scanSameRow( rebMtx )	# 11 (11/226 5% 수준임	)

	# 비교방법
	#	1. 선택된 DNA의 값, 위치, Quo 10 그룹크기가 과거내역과 동일한 것 비교.
	#	2. 값이 pos.c 위치에 위치하고 Quo 10 그룹 크기가 quo.c 와 동일한 것은 제외.
	# hard 비교방법
	#	1. 상동이나, 동일항목이 다수 발견되면 가장 최신것만 선택.
	#	2. 상동이나, quo.c 크기는 물론 값도 동일한 것만 제외.

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_AQ000.C()



Val_AX000.X <- function( pEnv ){	#
	
	filtId="AX000.X";	tStmp <- Sys.time()
	zhF <- pEnv$zhF
	lastZoid <- zhF[nrow(pEnv$zhF),]
	allCodeMtx <- pEnv$allZoidMtx

	# flag <- apply( allZoidMtx ,1 ,function(p){ sum(lastZoid%in%p) } )
	

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_AX000.X()

