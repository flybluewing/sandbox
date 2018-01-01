# 20171116_C_exp.R
source("20171116_A_H.R")
source("20171116_A_H_cliper.R")
source("20171116_B_H.R")
source("20171116_C_H.R")

curWd <- getwd()	;setwd("..")
FB <- getFlagBank()
FB.f <- getFlagBank("./zoidHistory/ZH_Final.csv")
setwd(curWd)
zh	<- as.matrix( FB$zh )
zhF	<- as.matrix( FB.f$zh )	;rownames(zhF) <- 1:nrow(zhF)
testSpan <- (nrow(zh)+1):nrow(zhF)
filtLst <- list()
getFiltHist <- function( pFiltId ,pTStmp ,pFlag=NULL ){
		rObj <- list( filtId=pFiltId ,tCost=(Sys.time()-pTStmp) ,flag=pFlag )
		return( rObj )
	}

# zhF ,allZoidMtx
testSpan <- 300:nrow(zhF)


#-[C0020.A]------------------------------------------------------
#	zhF[,6]-zhF[,1] 이 20 이하인 경우는 전체 5.6% 정도.. 자르자!!
stdCodeMtx <- cbind( zhF[,4]-zhF[,1] , zhF[,6]-zhF[,3] ,zhF[,6]-zhF[,1] )
k <- table(stdCodeMtx[,3])
sum(k[1:8])/sum(k)	# 0.05597964



#-[C0020.B]------------------------------------------------------
#	동일한 쌍의 발생이 42개 밖에 안됨.
stdCodeMtx <- cbind( zhF[,4]-zhF[,1] , zhF[,6]-zhF[,3] ,zhF[,6]-zhF[,1] )
compLst <- list()
for( aIdx in 1:(nrow(zhF)-1)){
	for( bIdx in (aIdx+1):nrow(zhF) ){
		if( all(stdCodeMtx[aIdx,]==stdCodeMtx[bIdx,]) ){
			compLst[[1+length(compLst)]] <- list( aIdx=aIdx ,bIdx=bIdx )
		}
	}
}



#-[C0030.A]------------------------------------------------------
#	5개 이상 일치 82개. 약 10%... 자르자.
stdCodeMtx <- zhF %% 8
compLst <- list()
for( aIdx in 1:(nrow(zhF)-1) ){
	for( bIdx in (aIdx+1):nrow(zhF)){
		cnt <- sum( stdCodeMtx[aIdx,]==stdCodeMtx[bIdx,] )
		if( 4<cnt ){
			compLst[[1+length(compLst)]] <- c( aIdx ,bIdx ,cnt )
		}
	}
}
compMtx <- do.call( rbind ,compLst )
tail( sort(unique( as.vector(compMtx[,1:2]) )) )

#-[C0040.A]------------------------------------------------------
#	5개 이상 일치하는 게 2개 밖에 없음..
stdCodeMtx <- abs(zhF[1:(nrow(zhF)-1),] - zhF[2:nrow(zhF),])
compLst <- list()
for( aIdx in 1:(nrow(stdCodeMtx)-1) ){
	for( bIdx in (aIdx+1):nrow(stdCodeMtx)){
		cnt <- sum( stdCodeMtx[aIdx,]==stdCodeMtx[bIdx,] )
		if( 4<cnt ){
			compLst[[1+length(compLst)]] <- c( aIdx ,bIdx ,cnt )
		}
	}
}
compMtx <- do.call( rbind ,compLst )

#-[C0040.B]------------------------------------------------------
#	n개 Jump.
#		stdCodeMtx에서 780번째 row이면 zhF에서의 780,783 차이값임.
jumpVal <- 2:10	# 29개 발생.
compMtxLst <- list()
for( jIdx in jumpVal ){
	stdCodeMtx <- abs(zhF[1:(nrow(zhF)-jIdx),] - zhF[(jIdx+1):nrow(zhF),])
	nrow(stdCodeMtx) <- jIdx + 1:nrow(stdCodeMtx)
	compLst <- list()
	for( aIdx in 1:(nrow(stdCodeMtx)-1) ){
		for( bIdx in (aIdx+1):nrow(stdCodeMtx)){
			cnt <- sum( stdCodeMtx[aIdx,]==stdCodeMtx[bIdx,] )
			if( 4<cnt ){
				compLst[[1+length(compLst)]] <- c( aIdx ,bIdx ,cnt )
			}
		}
	}
	compMtxLst[[1+length(compMtxLst)]] <- do.call( rbind ,compLst )
}

sapply(compMtxLst,nrow)

#-[D0010.A]------------------------------------------------------
#	최근 2개 History에서 발생한 요소가 재발생...
#		4개 이상은 26개 밖에 없었다.
bNum <- 2
flag <- rep( 0 ,nrow(zhF) )
for( hIdx in (bNum+1):nrow(zhF) ){
	val <- unique(as.vector(zhF[(hIdx-bNum):(hIdx-1),]))
	cnt <- sum( zhF[hIdx,] %in% val )
	flag[hIdx] <- cnt
}

#-[D0010.B]------------------------------------------------------
#	최근 10개 History에서 발생한 요소가 재발생...
#		1개 이하는 11개 밖에 없었다.
bNum <- 10
flag <- rep( 0 ,nrow(zhF) )
for( hIdx in (bNum+1):nrow(zhF) ){
	val <- unique(as.vector(zhF[(hIdx-bNum):(hIdx-1),]))
	cnt <- sum( zhF[hIdx,] %in% val )
	flag[hIdx] <- cnt
}


#=[SAVE]========================================================================================
deskObj <- list( allZoidMtx=allZoidMtx )
deskObj$memo <- sprintf("zhF size : %d.",nrow(zhF))
deskObj$filtLst <- filtLst
save( deskObj ,file="Obj_deskObj.ReadyGo.save" )
