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

myObj <- load("Obj_deskObj.FiltRemain.save")	# deskObj
allZoidMtx <- deskObj$allZoidMtx
filtLst <- deskObj$filtLst

rZoidMtx <- matrix( 0 ,ncol=6 ,nrow=0 )
rZoidMtx <- rbind( rZoidMtx , c( 7,14,22,26,28,43) )
rZoidMtx <- rbind( rZoidMtx , c( 8,18,19,28,29,43) )
rZoidMtx <- rbind( rZoidMtx , c( 1, 7, 9,10,34,35) )
rZoidMtx <- rbind( rZoidMtx , c( 1, 3, 8,12,18,34) )
rZoidMtx <- rbind( rZoidMtx , c( 8, 9,23,31,35,40) )

fIndices <- rep( 0 ,nrow(rZoidMtx) )
for( rIdx in 1:nrow(rZoidMtx) ){
	flag <- apply( allZoidMtx ,1 ,function(p){all(p==rZoidMtx[rIdx,])} )
	fIndices[rIdx] <- ifelse( 0==sum(flag) ,0 ,which(flag)[1] )
}

rZoidVal <- sort(unique(as.vector(rZoidMtx)))
aZoidVal <- sort(unique(as.vector(allZoidMtx)))

# 일단 검증안된 것들 직관만으로 적용하기... 찍기.
#-[D0030]---------------------------------------------------------------------------------------
#	%%10 그룹에서 1개 발생한 것이 다음에도 그 그룹에서 1개만 발생
filtVal <- c(3,6,10,18,15,31,33,38)
filtValMtx <- matrix( filtVal ,ncol=1 ,nrow=length(filtVal) )
grpName <- sprintf("%d",filtValMtx[,1] %/% 10)
flag <- rep( 0 ,nrow(allZoidMtx) )
for( aIdx in 1:nrow(allZoidMtx) ){
	tbl <- table( allZoidMtx[aIdx,] %/% 10 )
	fndName <- names(tbl)[tbl==ncol(filtValMtx)]
	rIndices <- which( grpName %in% fndName )
	if( 0==length(rIndices) )
		next

	aName <- sprintf("%d",allZoidMtx[aIdx,] %/% 10)
	chkName <- intersect( grpName ,fndName )
	for( nIdx in chkName ){
		val <- allZoidMtx[aIdx,aName==nIdx]
		chkF <- apply( filtValMtx[grpName==nIdx,,drop=F] ,1 ,function(p){ all(p==val)} )
		if( any(chkF) ){
			flag[aIdx] <- as.integer(nIdx)
		}
	}
}
#	sum(flag>0)	# 22466
allZoidMtx <- allZoidMtx[flag==0,]

#-[D0040]---------------------------------------------------------------------------------------
#	%%10 그룹에서 2개 발생한 것이 다음에도 그 그룹에서 2개만 발생
filtValMtx <- matrix( 0 ,ncol=2 ,nrow=0 )
filtValMtx <- rbind( filtValMtx ,c( 4, 6) )
filtValMtx <- rbind( filtValMtx ,c(25,26) )
filtValMtx <- rbind( filtValMtx ,c(23,24) )
filtValMtx <- rbind( filtValMtx ,c(31,39) )
grpName <- sprintf("%d",filtValMtx[,1] %/% 10)
flag <- rep( 0 ,nrow(allZoidMtx) )
for( aIdx in 1:nrow(allZoidMtx) ){
	tbl <- table( allZoidMtx[aIdx,] %/% 10 )
	fndName <- names(tbl)[tbl==ncol(filtValMtx)]
	rIndices <- which( grpName %in% fndName )
	if( 0==length(rIndices) )
		next

	aName <- sprintf("%d",allZoidMtx[aIdx,] %/% 10)
	chkName <- intersect( grpName ,fndName )
	for( nIdx in chkName ){
		val <- allZoidMtx[aIdx,aName==nIdx]
		chkF <- apply( filtValMtx[grpName==nIdx,,drop=F] ,1 ,function(p){ all(p==val)} )
		if( any(chkF) ){
			flag[aIdx] <- as.integer(nIdx)
		}
	}
}
#	sum(flag>0)	# 338
allZoidMtx <- allZoidMtx[flag==0,]


#-[D0041]---------------------------------------------------------------------------------------
#	%%10 그룹에서 2개 발생한 것이 다음에도 그 그룹에서 연속적으로 발생.
remVal <- c( 4 , 6 )
flag <- apply(allZoidMtx ,1 ,function(p){sum(p%in%remVal)} )
#	sum(flag>=2)	# 1169
allZoidMtx <- allZoidMtx[flag<2,]

remVal <- c( 31,39 )
flag <- apply(allZoidMtx ,1 ,function(p){sum(p%in%remVal)} )
#	sum(flag>=2)	# 317
allZoidMtx <- allZoidMtx[flag<2,]

#-[D0050]---------------------------------------------------------------------------------------
#	sequencial 증가가 이미 두번이나 연속발생했으므로 다음엔 없겠...지?
remZoid <- zhF[nrow(zhF),]+1
flag <- apply( allZoidMtx ,1 ,function(p){ sum(p==remZoid) })
#	sum(flag>0)	# 12882
allZoidMtx <- allZoidMtx[flag==0,]

#-[D0051]---------------------------------------------------------------------------------------
#	sequencial 증가가 이미 두번이나 연속발생했으므로 다음엔 없겠...지? 위치에 상관없이...
#		3->4->5   24->25->26
remVal <- c( 5, 26 )
flag <- apply(allZoidMtx ,1 ,function(p){sum(p%in%remVal)} )
#	sum(flag>0)	# 6790
allZoidMtx <- allZoidMtx[flag==0,]



#-[E0010]---------------------------------------------------------------------------------------
#	col 별 패턴 재발생 가능성.
jumpStepSpan <- 1:100
remMtx <- matrix( 0 ,ncol=6 ,nrow=length(jumpStepSpan) )
for( jumpStep in jumpStepSpan ){
	ptnLst <- list()
	remVal <- rep( 0 ,6 )
	for( cIdx in 1:6 ){
		ptnLst[[cIdx]] <- NULL
		for( bjIdx in 5:2 ){
			ptn <- getPastPtn( zhF[1:(nrow(zhF)-jumpStep+1),cIdx] ,pDepth=bjIdx ,pScanAll=F )
			if( !is.null(ptn) ){
				ptn$bjIdx <- bjIdx
				ptnLst[[cIdx]] <- ptn
				remVal[cIdx] <- zhF[ptn$fIdx+jumpStep,cIdx]
				break
			}
		}
	}
	remMtx[ jumpStep , ] <- remVal
}

flag <- rep( 0 ,nrow(allZoidMtx) )
for( aIdx in 1:nrow(allZoidMtx) ){
	for( remIdx in 1:nrow(remMtx) ){
		mCnt <- sum(allZoidMtx[aIdx,]==remMtx[remIdx,])
		if( remIdx <4 ){
			if( 0<mCnt ){
				flag[aIdx] <- remIdx
				break				
			}
		} else if( 1<mCnt ){
			flag[aIdx] <- remIdx
			break
		}
	} # for(remIdx)
}

allZoidMtx <- allZoidMtx[flag==0,]

# zhF[(nrow(zhF)-20):nrow(zhF),]
# 	[1,]   1  3  8 12 42 43
# 	[2,]   9 30 34 35 39 41
# 	[3,]   5 15 20 31 34 42
# 	[4,]   7 27 29 30 38 44
# 	[5,]   5  7 11 16 41 45
# 	[6,]   1  9 12 23 39 43
# 	[7,]   6 10 17 18 21 29
# 	[8,]   5  6 11 14 21 41
# 	[9,]   8 12 19 21 31 35
# 	[10,] 12 15 18 28 34 42
# 	[11,] 11 12 29 33 38 42
# 	[12,]  8  9 18 21 28 40
# 	[13,]  6 12 17 21 34 37
# 	[14,]  6 21 35 36 37 41
# 	[15,]  6 12 19 24 34 41
# 	[16,] 15 17 19 21 27 45
# 	[17,] 11 16 18 19 24 39
# 	[18,]  6 18 31 34 38 45
# 	[19,] 14 15 16 17 38 45
# 	[20,]  3 10 23 24 31 39
# 	[21,]  4  6 15 25 26 33


#=[Analyze]=====================================================================================
flagMtx <- matrix( 0 ,ncol=length(filtLst) ,nrow=nrow(allZoidMtx) )
colnames( flagMtx ) <- sapply( filtLst ,function(p){p$filtId} )
for( cIdx in seq_len(ncol(flagMtx)) ){
	flagMtx[ ,cIdx ] <- filtLst[[cIdx]]$flag
}

flagSum <- apply( flagMtx ,1 ,sum )


#=[SAVE]========================================================================================
deskObj <- list( allZoidMtx=allZoidMtx )
deskObj$memo <- sprintf("zhF size : %d.",nrow(zhF))
deskObj$filtLst <- filtLst
save( deskObj ,file="Obj_deskObj.ReadyGo.save" )
