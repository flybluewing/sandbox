# 20180109_A_val.R filt 함수들에 대한 성능검증 코드들.
testSpan <- 400:nrow(zhF)


#-[AK000.C]------------------------------------------------------
#	zhF[,6]-zhF[,1] 이전 패턴의 반복. (15/391 3.8%)
val_AK000.C <- function( gEnv ){	#
	filtId="AK000.C";	tStmp <- Sys.time()

	tEnv <- gEnv
	flag <- rep( TRUE ,nrow(gEnv$zhF) )
	for( hIdx in testSpan ){
		tEnv$zhF <- gEnv$zhF[1:(hIdx-1),]
		tEnv$allZoidMtx <- gEnv$zhF[hIdx,,drop=F]
		rstObj <- filt_AK000.C( tEnv )
		flag[hIdx] <- rstObj$flag[1]
	}

	gEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # val_AK000.C()


#-[AK000.D]------------------------------------------------------
#	zhF[,6]-zhF[,1]	: 이전과 같은 간격으로 건너뛸 가능성. 20/391 5%
val_AK000.D <- function( pEnv ){
	
	filtId="AK000.D";	tStmp <- Sys.time()

	tEnv <- gEnv
	flag <- rep( TRUE ,nrow(pEnv$zhF) )
	for( hIdx in testSpan ){
		tEnv$zhF <- gEnv$zhF[1:(hIdx-1),]
		tEnv$allZoidMtx <- gEnv$zhF[hIdx,,drop=F]
		rstObj <- filt_AK000.D( tEnv )
		flag[hIdx] <- rstObj$flag[1]
	}

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # val_AK000.D()

#-[AL000.A]------------------------------------------------------
#	이전에 발생한 DNA의 첫번째가 다음에도 발생할 가능성. 31/391
val_AL000.A <- function( pEnv ){
	
	filtId="AL000.A";	tStmp <- Sys.time()

	tEnv <- gEnv
	flag <- rep( TRUE ,nrow(pEnv$zhF) )
	for( hIdx in testSpan ){
		tEnv$zhF <- gEnv$zhF[1:(hIdx-1),]
		tEnv$allZoidMtx <- gEnv$zhF[hIdx,,drop=F]
		rstObj <- filt_AL000.A( tEnv )
		flag[hIdx] <- rstObj$flag[1]
	}

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # val_AL000.A()


#-[AP000.A]------------------------------------------------------
#	zhF %/% 10 Quoatient.  이전 H와 1개 이내로 틀리는 것 24/391 6%
val_AP000.A <- function( gEnv ){
	
	filtId="AP000.A";	tStmp <- Sys.time()

	tEnv <- gEnv
	flag <- rep( TRUE ,nrow(pEnv$zhF) )
	for( hIdx in testSpan ){
		tEnv$zhF <- gEnv$zhF[1:(hIdx-1),]
		tEnv$allZoidMtx <- gEnv$zhF[hIdx,,drop=F]
		rstObj <- filt_AP000.A( tEnv )
		flag[hIdx] <- rstObj$flag[1]
	}

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # val_AP000.A()

#-[AP000.C]------------------------------------------------------
#	zhF %/% 10 Quoatient.  한가지 Quoatient가 4개 이상인 경우 49/787
val_AP000.C <- function( gEnv ){	#
	filtId="AP000.C";	tStmp <- Sys.time()

	tEnv <- gEnv
	flag <- rep( TRUE ,nrow(gEnv$zhF) )
	for( hIdx in testSpan ){
		tEnv$zhF <- gEnv$zhF[1:(hIdx-1),]
		tEnv$allZoidMtx <- gEnv$zhF[hIdx,,drop=F]
		rstObj <- filt_AP000.C( tEnv )
		flag[hIdx] <- rstObj$flag[1]
	}

	gEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # val_AP000.C()

#-[AP000.D]------------------------------------------------------
#	zhF %/% 10 Quoatient.  Quoatient패턴 Next 값.	6/391
val_AP000.D <- function( gEnv ){	#
	filtId="AP000.D";	tStmp <- Sys.time()

	tEnv <- gEnv
	flag <- rep( TRUE ,nrow(gEnv$zhF) )
	for( hIdx in testSpan ){
		tEnv$zhF <- gEnv$zhF[1:(hIdx-1),]
		tEnv$allZoidMtx <- gEnv$zhF[hIdx,,drop=F]
		rstObj <- filt_AP000.D( tEnv )
		flag[hIdx] <- rstObj$flag[1]
	}

	gEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # val_AP000.D()

#-[AP000.E]------------------------------------------------------
#	zhF %/% 10 Quoatient.  Quoatient그룹이 다음에도 반복. (77/391 20%)
val_AP000.E <- function( gEnv ){	#
	filtId="AP000.E";	tStmp <- Sys.time()

	tEnv <- gEnv
	flag <- rep( TRUE ,nrow(gEnv$zhF) )
	for( hIdx in testSpan ){
		tEnv$zhF <- gEnv$zhF[1:(hIdx-1),]
		tEnv$allZoidMtx <- gEnv$zhF[hIdx,,drop=F]
		rstObj <- filt_AP000.E( tEnv )
		flag[hIdx] <- rstObj$flag[1]
	}

	gEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # val_AP000.E()


#-[AQ000.A]------------------------------------------------------
#	DNA 코드가 다음 H에서 몇 개나 재발되는지. (3개 이상 12/391	3%)
val_AQ000.A <- function( pEnv ){
	
	filtId="AQ000.A";	tStmp <- Sys.time()

	tEnv <- gEnv
	flag <- rep( TRUE ,nrow(pEnv$zhF) )
	for( hIdx in testSpan ){
		tEnv$zhF <- gEnv$zhF[1:(hIdx-1),]
		tEnv$allZoidMtx <- gEnv$zhF[hIdx,,drop=F]
		rstObj <- filt_AQ000.A( tEnv )
		flag[hIdx] <- rstObj$flag[1]
	}

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # val_AQ000.A()


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

	# -[실측: 4% 수준]-----------------------------------------------------
	tEnv <- gEnv	# 10/391 2.5%
	dd <- 0
	flag <- rep( TRUE ,nrow(pEnv$zhF) )
	for( hIdx in testSpan ){
		dd <- hIdx
		tEnv$zhF <- gEnv$zhF[1:(hIdx-1),]
		tEnv$allZoidMtx <- gEnv$zhF[hIdx,,drop=F]
		rstObj <- filt_AQ000.C( tEnv )
		flag[hIdx] <- rstObj$flag[1]
	}

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_AQ000.C()

#-[C1000.A]------------------------------------------------------
#	abs(zhF[1:(nrow(zhF)-1),] - zhF[2:nrow(zhF),])  49/391 13%
val_C1000.A <- function( pEnv ){
	
	filtId="C1000.A";	tStmp <- Sys.time()

	tEnv <- gEnv
	flag <- rep( TRUE ,nrow(pEnv$zhF) )
	for( hIdx in testSpan ){
		tEnv$zhF <- pEnv$zhF[1:(hIdx-1),]
		tEnv$allZoidMtx <- pEnv$zhF[hIdx,,drop=F]
		rstObj <- filt_C1000.A( tEnv )
		flag[hIdx] <- rstObj$flag[1]
	}

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # val_C1000.A()


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

