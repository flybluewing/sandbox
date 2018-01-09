# 20171116_C_exp.R
source("20171116_A_H.R")
source("20171116_A_H_cliper.R")
source("20171116_B_H.R")
source("20171116_C_H.R")
source("20171116_D_H.R")

curWd <- getwd()	;setwd("..")
FB <- getFlagBank()
FB.f <- getFlagBank("./zoidHistory/ZH_Final.csv")
setwd(curWd)
zh	<- as.matrix( FB$zh )
zhF	<- as.matrix( FB.f$zh )	;rownames(zhF) <- 1:nrow(zhF)
lastZoid <- zhF[nrow(zhF),]
testSpan <- (nrow(zh)+1):nrow(zhF)
filtLst <- list()
getFiltHist <- function( pFiltId ,pTStmp ,pAllZoidMtx ,pFlag=NULL ){
		rObj <- list( filtId=pFiltId ,tCost=(Sys.time()-pTStmp) 
						,zoidSize=nrow(pAllZoidMtx)
						,flag=pFlag 
					)
		return( rObj )
	}

# zhF ,allZoidMtx
testSpan <- 300:nrow(zhF)

allZoidMtx <- getAllZoid() # 38sec

#=[BaseFilt]====================================================================================
#-[A0010]------------------------------------------------------
filtId <- "A0010"
codeMtx <- allZoidMtx[,2:6]-allZoidMtx[,1:5]
tStmp <- Sys.time()
stepM <- apply( codeMtx ,1 ,function(p){max(table(p))})
allZoidMtx <- allZoidMtx[stepM<=2,]		# 7740330 
filtLst[[1+length(filtLst)]] <- getFiltHist( filtId ,tStmp ,allZoidMtx )
k.FLogStr(sprintf("%s %d",filtId,nrow(allZoidMtx)))

codeMtx <- NULL # for gc

#-[A0020]------------------------------------------------------
filtId <- "A0020"
codeMtx <- allZoidMtx %% 2
tStmp <- Sys.time()
cnt <- apply( codeMtx ,1 ,function(p){sum(p==0)})
allZoidMtx <- allZoidMtx[(cnt>0)&(cnt<6),]	# 7599240
filtLst[[1+length(filtLst)]] <- getFiltHist( filtId ,tStmp ,allZoidMtx )
k.FLogStr(sprintf("%s %d",filtId,nrow(allZoidMtx)))

codeMtx <- NULL # for gc

#-[A0030]------------------------------------------------------
filtId <- "A0030"
codeMtx <- allZoidMtx %% 3
tStmp <- Sys.time()
cnt <- apply( codeMtx ,1 ,function(p){sum(p==0)})
allZoidMtx <- allZoidMtx[(cnt>0),]	# 7041489
filtLst[[1+length(filtLst)]] <- getFiltHist( filtId ,tStmp ,allZoidMtx )
k.FLogStr(sprintf("%s %d",filtId,nrow(allZoidMtx)))

codeMtx <- NULL # for gc

#----------------------------------------------------------------
#  A0100 : lastZoid를 기준으로 잘라낸다.
#-[A0100.A]------------------------------------------------------
filtId <- "A0100.A"
stdCodeMtx <- zhF
allCodeMtx <- allZoidMtx
tStmp <- Sys.time()
for( nextJump in 1:8 ){
	filtGrp <- getPtnRebGrp( stdCodeMtx ,pNextJump=nextJump )
	filtRst <- filtGrp$filt( allCodeMtx )
	surviveFlag <- sapply(filtRst ,function(p){p$survive})
	allCodeMtx <- allCodeMtx[surviveFlag,]
	tDiff <- Sys.time() - tStmp
	k.FLogStr(sprintf("nextJump:%d  past:%.1f%s left:%d"
					,nextJump,tDiff,units(tDiff),nrow(allCodeMtx) )
				,pConsole=T
			)
}
tDiff <- Sys.time() - tStmp
allZoidMtx <- allCodeMtx
filtLst[[1+length(filtLst)]] <- getFiltHist( filtId ,tStmp ,allZoidMtx )
k.FLogStr(sprintf("%s %d",filtId,nrow(allZoidMtx)))

filtRst <- NULL # for gc

#-[A0110.A]------------------------------------------------------
filtId <- "A0110.A"
stdCodeMtx <- zhF
allCodeMtx <- allZoidMtx
tStmp <- Sys.time()
for( nextJump in 1:2 ){
	filtGrp <- getPtnRebGrp2( stdCodeMtx ,pNextJump=nextJump )
	filtRst <- filtGrp$filt( allCodeMtx )
	surviveFlag <- sapply(filtRst ,function(p){p$survive})
	allCodeMtx <- allCodeMtx[surviveFlag,]
	tDiff <- Sys.time() - tStmp
	k.FLogStr(sprintf("nextJump:%d  past:%.1f%s left:%d"
					,nextJump,tDiff,units(tDiff),nrow(allCodeMtx) )
				,pConsole=T
			)
}
tDiff <- Sys.time() - tStmp
allZoidMtx <- allCodeMtx
filtLst[[1+length(filtLst)]] <- getFiltHist( filtId ,tStmp ,allZoidMtx )
k.FLogStr(sprintf("%s %d",filtId,nrow(allZoidMtx)))

filtRst <- NULL # for gc

#-[AK000.A]------------------------------------------------------
#	zhF[,6]-zhF[,1] 이 20 이하인 경우는 전체 5.6% 정도.. 자르자!!
filtId <- "AK000.A";	tStmp <- Sys.time()
flag <- apply( allZoidMtx ,1 ,function(p){p[6]-p[1]})
tDiff <- Sys.time() - tStmp
allZoidMtx <- allZoidMtx[flag>20,]
filtLst[[1+length(filtLst)]] <- getFiltHist( filtId ,tStmp ,allZoidMtx )
k.FLogStr(sprintf("%s %d",filtId,nrow(allZoidMtx)))


#-[AK000.B]------------------------------------------------------
#	zhF[,6]-zhF[,1] 이 다음에도 반복될 가능성.. 5% 미만.
filtId <- "AK000.B";	tStmp <- Sys.time()
flag <- (allZoidMtx[,6]-allZoidMtx[,1])!=(lastZoid[6]-lastZoid[1])
tDiff <- Sys.time() - tStmp
allZoidMtx <- allZoidMtx[flag,]
filtLst[[1+length(filtLst)]] <- getFiltHist( filtId ,tStmp ,allZoidMtx )
k.FLogStr(sprintf("%s %d",filtId,nrow(allZoidMtx)))

#-[AK000.C]------------------------------------------------------
#	zhF[,6]-zhF[,1] 이전 패턴의 반복. 3% 미만.
filtId <- "AK000.C";	tStmp <- Sys.time()
stdCode <- zhF[,6]-zhF[,1]
ptn <- NULL
for( dIdx in 5:1 ){
	ptn <- getPastPtn( stdCode ,pDepth=dIdx )
	if( !is.null(ptn) ){
		flag <- (allZoidMtx[,6]-allZoidMtx[,1])!=ptn$nextVal
		allZoidMtx <- allZoidMtx[flag,]
		break
	}
}
filtLst[[1+length(filtLst)]] <- getFiltHist( filtId ,tStmp ,allZoidMtx )
k.FLogStr(sprintf("%s %d",filtId,nrow(allZoidMtx)))

#-[AK000.D]------------------------------------------------------
#	zhF[,6]-zhF[,1]	: 이전과 같은 간격으로 건너뛸 가능성. 55/785
filtId <- "AK000.D";	tStmp <- Sys.time()
stdCode <- zhF[,6]-zhF[,1]
remVal <- c( stdCode[length(stdCode)-1] , 
				stdCode[length(stdCode)] + 
					abs(stdCode[length(stdCode)]-stdCode[length(stdCode)-1])
			)
flag <- apply( allZoidMtx ,1 ,function(p){ !( (p[6]-p[1])%in%remVal ) })
allZoidMtx <- allZoidMtx[flag,]
filtLst[[1+length(filtLst)]] <- getFiltHist( filtId ,tStmp ,allZoidMtx )
k.FLogStr(sprintf("%s %d",filtId,nrow(allZoidMtx)))


#-[AL000.A]------------------------------------------------------
#	이전에 발생한 DNA의 첫번째가 다음에도 발생할 가능성. 53/785
filtId <- "AL000.A";	tStmp <- Sys.time()
comVal <- intersect(zhF[nrow(zhF),],zhF[nrow(zhF)-1,])
if( 0 < length(comVal) ){
	flag <- apply( allZoidMtx ,1 ,function(p){ !(comVal[1]%in%p) } )
	allZoidMtx <- allZoidMtx[flag,]
}
filtLst[[1+length(filtLst)]] <- getFiltHist( filtId ,tStmp ,allZoidMtx )
k.FLogStr(sprintf("%s %d",filtId,nrow(allZoidMtx)))

#-[AP000.A]------------------------------------------------------
#	zhF %/% 10 Quoatient.  이전 H와 1개 이내로 틀리는 것 59/787
filtId <- "AP000.A";	tStmp <- Sys.time()
stdCode <- lastZoid %/% 10
flag <- apply( allZoidMtx %/% 10 ,1 ,function(p){ sum(p!=stdCode) })
allZoidMtx <- allZoidMtx[flag>1,]
filtLst[[1+length(filtLst)]] <- getFiltHist( filtId ,tStmp ,allZoidMtx )
k.FLogStr(sprintf("%s %d",filtId,nrow(allZoidMtx)))

#-[AP000.B]------------------------------------------------------
#	zhF %/% 10 Quoatient.  동일한 패턴의 재발간격. 7 H 이내 재발 57/787
filtId <- "AP000.B";	tStmp <- Sys.time()
stdCodeMtx <- zhF %/% 10
stdCodeMtx <- stdCodeMtx[(nrow(stdCodeMtx)-6):nrow(stdCodeMtx),]
allCodeMtx <- allZoidMtx %/% 10
flag <- rep( 0 ,nrow(allCodeMtx) )
for( aIdx in 1:nrow(allCodeMtx) ){
	for( sIdx in 1:nrow(stdCodeMtx) ){
		if( all(allCodeMtx[aIdx,]==stdCodeMtx[sIdx,]) ){
			flag[aIdx] <- sIdx
			break
		}
	}
}
allZoidMtx <- allZoidMtx[flag==0,]
filtLst[[1+length(filtLst)]] <- getFiltHist( filtId ,tStmp ,allZoidMtx )
k.FLogStr(sprintf("%s %d",filtId,nrow(allZoidMtx)))

#-[AP000.C]------------------------------------------------------
#	zhF %/% 10 Quoatient.  한가지 Quoatient가 4개 이상인 경우 49/787
filtId <- "AP000.C";	tStmp <- Sys.time()
allCodeMtx <- allZoidMtx %/% 10
flagLst <- apply(allCodeMtx ,1 ,table)
flag <- sapply(flagLst,max)
tDiff <- Sys.time() - tStmp
allZoidMtx <- allZoidMtx[flag<4,]
filtLst[[1+length(filtLst)]] <- getFiltHist( filtId ,tStmp ,allZoidMtx )
k.FLogStr(sprintf("%s %d",filtId,nrow(allZoidMtx)))

#-[AP000.D]------------------------------------------------------
#	zhF %/% 10 Quoatient.  Quoatient패턴 Next 값.	5/288
filtId <- "AP000.D";	tStmp <- Sys.time()
stdCodeMtx <- getTblCnt( zhF %/% 10 )
allCodeMtx <- getTblCnt( allZoidMtx %/% 10 )
flag <- rep( 0 ,nrow(allCodeMtx) )
ptn <- getPtnReb( stdCodeMtx )
if( !is.null(ptn) ){
	for( aIdx in 1:nrow(allCodeMtx) ){
		if( all(allCodeMtx[aIdx,]==ptn$nextRow) ){
			flag[aIdx] <- 1
		}
	}
}
allZoidMtx <- allZoidMtx[flag==0,]
filtLst[[1+length(filtLst)]] <- getFiltHist( filtId ,tStmp ,allZoidMtx )
k.FLogStr(sprintf("%s %d",filtId,nrow(allZoidMtx)))


#-[AP000.E]------------------------------------------------------
#	zhF %/% 10 Quoatient.  Quoatient그룹이 다음에도 반복.
filtId <- "AP000.E";	tStmp <- Sys.time()
stdGrpLst <- apply( zhF ,1 ,function(p){ getQGrp(p,0:4) } )
for( gIdx in 1:length(stdGrpLst) ){
	# 1~9, 40번 대역에서 1개 나오는 경우는 너무 흔하니,
	#	아예 없는 것으로 제외시킴.
	if( 1==stdGrpLst[[gIdx]]$grpCnt[1] ){
		stdGrpLst[[gIdx]]$grpCnt[1] <- 0
		stdGrpLst[[gIdx]]$grpLst[[1]] <- integer(0)
	}
	if( 1==stdGrpLst[[gIdx]]$grpCnt[5] ){
		stdGrpLst[[gIdx]]$grpCnt[5] <- 0
		stdGrpLst[[gIdx]]$grpLst[[5]] <- integer(0)
	}
}

backMtx <- matrix( c(1,2) ,ncol=2 ,nrow=1 ) # grp 크기 ,검색 H 범위
backMtx <- rbind( backMtx ,c(2,5) )
backMtx <- rbind( backMtx ,c(3,200) )

allGrpLst <- apply( allZoidMtx ,1 ,function(p){ getQGrp(p,0:4) } )

flag <- rep( TRUE ,length(allGrpLst) )
for( bIdx in 1:nrow(backMtx) ){	
	scanSpan <- (length(stdGrpLst)-backMtx[bIdx,2]+1):length(stdGrpLst)
	for( sIdx in scanSpan ){
		rstFlag <- stdGrpLst[[sIdx]]$filt( allGrpLst ,pMin=backMtx[bIdx,1] )
		flag <- (rstFlag==0) & flag
	}
}
allZoidMtx <- allZoidMtx[flag,]
filtLst[[1+length(filtLst)]] <- getFiltHist( filtId ,tStmp ,allZoidMtx )
k.FLogStr(sprintf("%s %d",filtId,nrow(allZoidMtx)))


#-[AQ000.A]------------------------------------------------------
#	DNA 코드가 다음 H에서 몇 개나 재발되는지. (3개 이상 20/787)
filtId <- "AQ000.A";	tStmp <- Sys.time()
flag <- apply( allZoidMtx ,1 ,function(p){ sum(lastZoid%in%p) } )
tDiff <- Sys.time() - tStmp
allZoidMtx <- allZoidMtx[flag<3,]
filtLst[[1+length(filtLst)]] <- getFiltHist( filtId ,tStmp ,allZoidMtx )
k.FLogStr(sprintf("%s %d",filtId,nrow(allZoidMtx)))

#-[AR000.A]------------------------------------------------------
#	remainder 근거리 재발. (3H 이내 재발 31/787)
filtId <- "AR000.A";	tStmp <- Sys.time()
stdCodeMtx <- zhF[(nrow(zhF)-2):nrow(zhF),] %% 2
allCodeMtx <- allZoidMtx %% 2
flag <- rep( 0 ,nrow(allCodeMtx) )
for( aIdx in 1:nrow(allCodeMtx) ){
	for( sIdx in 1:nrow(stdCodeMtx) ){
		if( all(allCodeMtx[aIdx,]==stdCodeMtx[sIdx,]) ){
			flag[aIdx] <- sIdx
			break
		}
	}
}
tDiff <- Sys.time() - tStmp
allZoidMtx <- allZoidMtx[flag==0,]
filtLst[[1+length(filtLst)]] <- getFiltHist( filtId ,tStmp ,allZoidMtx )
k.FLogStr(sprintf("%s %d",filtId,nrow(allZoidMtx)))

#-[AR000.B]------------------------------------------------------
#	remainder 패턴 재발 (nextVal)	17% 탈락.
filtId <- "AR000.B";	tStmp <- Sys.time()
stdCodeMtx <- zhF %% 10
ptn <- getPtnRebGrp( stdCodeMtx ,pNextJump=1 )
flag <- sapply( ptn$filt( allZoidMtx %% 10 )
				,function(p){p$survive} 
			)
allZoidMtx <- allZoidMtx[flag,]
filtLst[[1+length(filtLst)]] <- getFiltHist( filtId ,tStmp ,allZoidMtx )
k.FLogStr(sprintf("%s %d",filtId,nrow(allZoidMtx)))

#-[AS000.A]------------------------------------------------------
#	연이은 DNA코드가 다음에도 연이어서 재발 38/787
filtId <- "AS000.A";	tStmp <- Sys.time()
flag <- rep( 0 ,nrow(allZoidMtx) )
for( aIdx in 1:nrow(allZoidMtx) ){
	rebIdx.1 <- which( lastZoid %in% allZoidMtx[aIdx,] )
	if( 2!=length(rebIdx.1) ){
		next	# 3개 이상발생은 AQ000.A에서 제거되었다.
	}
	flag[aIdx] <- 2
	rebIdx.2 <- which( allZoidMtx[aIdx,] %in% lastZoid )
	if( (rebIdx.1[2]-rebIdx.1[1])==(rebIdx.2[2]-rebIdx.2[1]) ){
		flag[aIdx] <- rebIdx.1[1]
	}
}
tDiff <- Sys.time() - tStmp
allZoidMtx <- allZoidMtx[flag==0,]
filtLst[[1+length(filtLst)]] <- getFiltHist( filtId ,tStmp ,allZoidMtx )
k.FLogStr(sprintf("%s %d",filtId,nrow(allZoidMtx)))




#----------------------------------------------------------------
#-[C0000.A]------------------------------------------------------
#	zhF[,2:6]-zhF[,1:5] : 똑같은 경우는 없었고, 1개 틀린 경우는 32/787
#		백만개 당 23분 정도 소요.
filtId <- "C0000.A";	tStmp <- Sys.time()
stdCodeMtx <- zhF[,2:6]-zhF[,1:5]
allCodeMtx <- allZoidMtx[,2:6]-allZoidMtx[,1:5]
flag <- rep(0,nrow(allZoidMtx))
for( aIdx in 1:nrow(allCodeMtx) ){
	for( sIdx in 1:nrow(stdCodeMtx) ){
		cnt <- sum(stdCodeMtx[sIdx,]!=allCodeMtx[aIdx,])
		if( 1 >= cnt ){
			flag[aIdx] <- sIdx
			break
		}	
	}
}
tDiff <- Sys.time() - tStmp
allZoidMtx <- allZoidMtx[flag==0,]
filtLst[[1+length(filtLst)]] <- getFiltHist( filtId ,tStmp ,allZoidMtx )
k.FLogStr(sprintf("%s %d",filtId,nrow(allZoidMtx)))


#-[C1000.A]------------------------------------------------------
#	abs(zhF[1:(nrow(zhF)-1),] - zhF[2:nrow(zhF),])
#		2개 이하로 틀린 경우는 65개 정도
filtId <- "C1000.A";	tStmp <- Sys.time()
stdCodeMtx <- abs(zhF[1:(nrow(zhF)-1),] - zhF[2:nrow(zhF),])
flag <- rep( 0 ,nrow(allZoidMtx) )
for( aIdx in 1:nrow(allZoidMtx) ){
	chkVal <- abs(allZoidMtx[aIdx,]-lastZoid)
	for( sIdx in 1:nrow(stdCodeMtx) ){
		cnt <- sum(chkVal==stdCodeMtx[sIdx,])
		if( 4<=cnt ){
			flag[aIdx] <- sIdx
			break
		}
	}
}
tDiff <- Sys.time() - tStmp
allZoidMtx <- allZoidMtx[flag==0,]
filtLst[[1+length(filtLst)]] <- getFiltHist( filtId ,tStmp ,allZoidMtx )
k.FLogStr(sprintf("%s %d",filtId,nrow(allZoidMtx)))


#=[SAVE]========================================================================================
deskObj <- list( allZoidMtx=allZoidMtx )
deskObj$memo <- sprintf("zhF size : %d.",nrow(zhF))
deskObj$filtLst <- filtLst
save( deskObj ,file="Obj_deskObj.TMP.save" )


#=[SAVE]========================================================================================
deskObj <- list( allZoidMtx=allZoidMtx )
deskObj$memo <- sprintf("zhF size : %d.",nrow(zhF))
deskObj$filtLst <- filtLst
save( deskObj ,file="Obj_deskObj.E00.save" )







#=[SAVE.A]======================================================================================
deskObj <- list( allZoidMtx=allZoidMtx )
deskObj$memo <- sprintf("zhF size : %d.",nrow(zhF))
deskObj$filtLst <- filtLst
save( deskObj ,file="Obj_deskObj.EZZ.save" )



