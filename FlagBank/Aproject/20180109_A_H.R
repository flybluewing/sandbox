# 20180109_A_H.R 마지막 시도가 되길..

getFiltRst <- function( ){

}

filt_A0010 <- function( pEnv ){	# 7740330
	
	filtId="A0010";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF

	codeMtx <- allZoidMtx[,2:6]-allZoidMtx[,1:5]
	stepM <- apply( codeMtx ,1 ,function(p){max(table(p))})
	flag <- stepM<=2

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_A0010()


filt_A0020 <- function( pEnv ){	# 7599240
	
	filtId="A0020";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF

	codeMtx <- allZoidMtx %% 2
	cnt <- apply( codeMtx ,1 ,function(p){sum(p==0)})
	flag <- (cnt>0)&(cnt<6)

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_A0020()

filt_A0030 <- function( pEnv ){	# 7041489
	
	filtId="A0030";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF

	codeMtx <- allZoidMtx %% 3
	cnt <- apply( codeMtx ,1 ,function(p){sum(p==0)})
	flag <- cnt>0

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_A0030()


#----------------------------------------------------------------
#  A0100 : lastZoid를 기준으로 잘라낸다.

