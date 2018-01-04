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

#-[A0020]------------------------------------------------------
filtId <- "A0020"
codeMtx <- allZoidMtx %% 2
tStmp <- Sys.time()
cnt <- apply( codeMtx ,1 ,function(p){sum(p==0)})
allZoidMtx <- allZoidMtx[(cnt>0)&(cnt<6),]	# 7599240
filtLst[[1+length(filtLst)]] <- getFiltHist( filtId ,tStmp ,allZoidMtx )
k.FLogStr(sprintf("%s %d",filtId,nrow(allZoidMtx)))

#-[A0030]------------------------------------------------------
filtId <- "A0030"
codeMtx <- allZoidMtx %% 3
tStmp <- Sys.time()
cnt <- apply( codeMtx ,1 ,function(p){sum(p==0)})
allZoidMtx <- allZoidMtx[(cnt>0),]	# 7041489
filtLst[[1+length(filtLst)]] <- getFiltHist( filtId ,tStmp ,allZoidMtx )
k.FLogStr(sprintf("%s %d",filtId,nrow(allZoidMtx)))

#=[임의제거]====================================================================================
# tail(zhF) E1 E2 E3 E4 E5 E6
# 		781 11 16 18 19 24 39
# 		782  6 18 31 34 38 45
# 		783 14 15 16 17 38 45
# 		784  3 10 23 24 31 39
# 		785  4  6 15 25 26 33
# 		786 12 15 16 20 24 30
#		787  5  6 13 16 27 28


#-[B0010.A]------------------------------------------------------
#	중복발생 숫자가 다음에도 나오지는 않겠지.
filtId <- "B0010.A"
tStmp <- Sys.time()
rebVal <- intersect( zhF[nrow(zhF),] ,zhF[nrow(zhF)-1,] )
flag <- apply( allZoidMtx ,1 ,function(p){!any(p %in% rebVal)})
allZoidMtx <- allZoidMtx[flag,]
filtLst[[1+length(filtLst)]] <- getFiltHist( filtId ,tStmp ,allZoidMtx )
k.FLogStr(sprintf("%s %d",filtId,nrow(allZoidMtx)))

#-[B0010.B]------------------------------------------------------
#	같은 컬럼에서의 연속 값 두번이니, 다음에도 같은 컬럼에 연속 값은 어렵겠지.
filtId <- "B0010.B"
seqVal <- zhF[nrow(zhF),] + 1
tStmp <- Sys.time()
flag <- apply( allZoidMtx ,1 ,function(p){ !any(p==seqVal) })
allZoidMtx <- allZoidMtx[flag,]
filtLst[[1+length(filtLst)]] <- getFiltHist( filtId ,tStmp ,allZoidMtx )
k.FLogStr(sprintf("%s %d",filtId,nrow(allZoidMtx)))

#-[B0010.C]------------------------------------------------------
#	설마 3개 이상 값이 재발되지는 않겠지.
filtId <- "B0010.C"
lastZoid <- zhF[nrow(zhF),]
tStmp <- Sys.time()
flag <- apply( allZoidMtx ,1 ,function(p){sum(p%in%lastZoid)})
allZoidMtx <- allZoidMtx[flag<=2,]
filtLst[[1+length(filtLst)]] <- getFiltHist( filtId ,tStmp ,allZoidMtx )
k.FLogStr(sprintf("%s %d",filtId,nrow(allZoidMtx)))

#-[B0010.D]------------------------------------------------------
#	이전 zoid 코드에서 연속된 부분이 다음에도 재발하긴 어렵겠지.
filtId <- "B0010.D"
lastZoid <- zhF[nrow(zhF),]
tStmp <- Sys.time()
flag <- rep( TRUE ,nrow(allZoidMtx) )
for( aIdx in 1:nrow(allZoidMtx) ){
	rebIdx <- which(allZoidMtx[aIdx,] %in% lastZoid)
	if( length(rebIdx)!=2 || (1!=rebIdx[2]-rebIdx[1]) ){
		next
	}

	lastReb <- which(lastZoid %in% allZoidMtx[aIdx,rebIdx] )
	if( (1+lastReb[1])==lastReb[2] )
		flag[aIdx] <- FALSE
}
allZoidMtx <- allZoidMtx[flag,]
filtLst[[1+length(filtLst)]] <- getFiltHist( filtId ,tStmp ,allZoidMtx )
k.FLogStr(sprintf("%s %d",filtId,nrow(allZoidMtx)))

#-[B0010.E]------------------------------------------------------
#	여태까지 발생한 zoid들의 거리가 동일하게 떨어지지는 않겠지.
filtId <- "B0010.E"
tStmp <- Sys.time() 
flag <- rep( 0 ,nrow(allZoidMtx) )
for( aIdx in 1:nrow(allZoidMtx) ) {
	for( hIdx in 1:nrow(zhF) ){
		d <- abs(allZoidMtx[aIdx,]-zhF[hIdx,])
		if( all( d[1]==d ) ){
			flag[aIdx] <- hIdx
			break
		}
	}
}
allZoidMtx <- allZoidMtx[flag==0,]
filtLst[[1+length(filtLst)]] <- getFiltHist( filtId ,tStmp ,allZoidMtx )
k.FLogStr(sprintf("%s %d",filtId,nrow(allZoidMtx)))

#-[B0010.F]------------------------------------------------------
#	grp 구성이 동일할 수는 없겠지.
#		즉 0대 2개, 10 대 딱 2개, 20대 딱 2개 그대로 발생은..
filtId <- "B0010.F"
lastZoid <- zhF[nrow(zhF),]
grp.grp <- unique(lastZoid%/%10)
grp.dna <- lapply( grp.grp ,function(p){ 
				seg <- lastZoid[which(p==(lastZoid%/%10))]
				return(seg)
			})
grp.num <- sapply( grp.dna ,length )
tStmp <- Sys.time()
flag <- rep( 0 ,nrow(allZoidMtx) )
for( aIdx in 1:nrow(allZoidMtx) ){
	a.grp <- allZoidMtx[aIdx,]%/%10
	for( gIdx in 1:length(grp.grp) ){
		grp <- grp.grp[gIdx]
		fcIdx <- which(grp==a.grp)
		if( grp.num[gIdx]==length(fcIdx) ){
			if( all( grp.dna[[gIdx]] == allZoidMtx[aIdx,fcIdx] ) ){
				flag[aIdx] <- gIdx
				break
			}
		}
	} # for(gIdx)
}
allZoidMtx <- allZoidMtx[flag==0,]
filtLst[[1+length(filtLst)]] <- getFiltHist( filtId ,tStmp ,allZoidMtx )
k.FLogStr(sprintf("%s %d",filtId,nrow(allZoidMtx)))

#-[B0010.F2]-----------------------------------------------------
#	grp 구성과 순서가 동일할 수는 없겠지.
#		5  6 13 16 27 28 --> 2:2:2 순서가 바로 다음에도 반복될까?
#	QQE:Todo 시간이 오래걸릴 듯 하여 일단 하드코딩으로 처리한다.
#			나중에 개선 요.
filtId <- "B0010.F2"
tStmp <- Sys.time()
stdCodeMtx <- allZoidMtx %/% 10
flag <- rep( TRUE ,nrow(stdCodeMtx) )
for( aIdx in 1:nrow(stdCodeMtx) ){
	if( stdCodeMtx[aIdx,1]==stdCodeMtx[aIdx,2] && 
			stdCodeMtx[aIdx,2]!=stdCodeMtx[aIdx,3] 
	){
		if( stdCodeMtx[aIdx,3]==stdCodeMtx[aIdx,4] && 
				stdCodeMtx[aIdx,4]!=stdCodeMtx[aIdx,5] 
		) {
			if( stdCodeMtx[aIdx,5]==stdCodeMtx[aIdx,6] ){
				flag[aIdx] <- FALSE
			}
		}
	}
}
tDiff <- Sys.time() - tStmp
allZoidMtx <- allZoidMtx[flag,]
filtLst[[1+length(filtLst)]] <- getFiltHist( filtId ,tStmp ,allZoidMtx )
k.FLogStr(sprintf("%s %d",filtId,nrow(allZoidMtx)))

#-[B0010.F3]-----------------------------------------------------
#	한 개 grp에 4개 씩이나 몰리는 건 최근에 2번이나.. 피해보자.
filtId <- "B0010.F3"
tStmp <- Sys.time()
stdCodeMtx <- allZoidMtx %/% 10
flag <- rep( TRUE ,nrow(stdCodeMtx) )
for( aIdx in 1:nrow(stdCodeMtx) ){
	tbl <- table( stdCodeMtx[aIdx,] )
	if( 4<=max(tbl) ){
		flag[aIdx] <- FALSE
	}
}
tDiff <- Sys.time() - tStmp
allZoidMtx <- allZoidMtx[flag,]
filtLst[[1+length(filtLst)]] <- getFiltHist( filtId ,tStmp ,allZoidMtx )
k.FLogStr(sprintf("%s %d",filtId,nrow(allZoidMtx)))


#=[손실감수]====================================================================================
#-[C0010.A]------------------------------------------------------
#	최근 30 history 이내에서 2개 이상 매치되는 경우는 10건 정도밖에 없었다.
#		abs(allZoidMtx[aIdx,]-zhF[hIdx,])
filtId <- "C0010.A"
tStmp <- Sys.time() 
hisSpan <- (nrow(zhF)-30+1):nrow(zhF)
flag <- rep( 0 ,nrow(allZoidMtx) )
for( aIdx in 1:nrow(allZoidMtx) ) {
	for( hIdx in hisSpan ){
		d <- abs(allZoidMtx[aIdx,]-zhF[hIdx,])
		if( 5==sum(d[1]==d) ){
			flag[aIdx] <- hIdx
			break
		} else if( 5==sum(d[6]==d) ){
			flag[aIdx] <- hIdx
			break
		}
	} # for(hIdx)
}
tDiff <- Sys.time() - tStmp
allZoidMtx <- allZoidMtx[flag==0,]
filtLst[[1+length(filtLst)]] <- getFiltHist( filtId ,tStmp ,allZoidMtx )
k.FLogStr(sprintf("%s %d",filtId,nrow(allZoidMtx)))


#-[C0020.A]------------------------------------------------------
#	zhF[,6]-zhF[,1] 이 20 이하인 경우는 전체 5.6% 정도.. 자르자!!
filtId <- "C0020.A"
tStmp <- Sys.time()
flag <- apply( allZoidMtx ,1 ,function(p){p[6]-p[1]})
tDiff <- Sys.time() - tStmp
allZoidMtx <- allZoidMtx[flag>20,]
filtLst[[1+length(filtLst)]] <- getFiltHist( filtId ,tStmp ,allZoidMtx )
k.FLogStr(sprintf("%s %d",filtId,nrow(allZoidMtx)))

#-[C0020.B]------------------------------------------------------
#	일치 가능성은 전체 5.3%.. 자르자.
#		시간만 오래 걸리고 걸리는 게 없는 듯..
filtId <- "C0020.B"
stdCodeMtx <- cbind( zhF[,4]-zhF[,1] , zhF[,6]-zhF[,3] ,zhF[,6]-zhF[,1] )
allCodeMtx <- cbind( allZoidMtx[,4]-allZoidMtx[,1] ,allZoidMtx[,6]-allZoidMtx[,3] 
						,allZoidMtx[,6]-allZoidMtx[,1]
					)
flag <- rep( 0 ,nrow(allZoidMtx) )
tStmp <- Sys.time()
for( aIdx in 1:nrow(allCodeMtx) ){
	for( sIdx in 1:nrow(stdCodeMtx) ){
		if( all(allCodeMtx[aIdx,]==stdCodeMtx[sIdx,]) ) {
			flag[aIdx] <- sIdx
			break
		}
	}
}
tDiff <- Sys.time() - tStmp
allZoidMtx <- allZoidMtx[flag==0,]
filtLst[[1+length(filtLst)]] <- getFiltHist( filtId ,tStmp ,allZoidMtx )
k.FLogStr(sprintf("%s %d",filtId,nrow(allZoidMtx)))

#-[C0030.A]------------------------------------------------------
#	5개 이상 일치 82개. 약 10%... 자르자.
#		시간만 오래 걸리고 걸리는 게 없는 듯..
filtId <- "C0030.A"
stdCodeMtx <- zhF %% 8
allCodeMtx <- allZoidMtx %% 8
flag <- rep( 0 ,nrow(allZoidMtx) )
tStmp <- Sys.time()
for( aIdx in 1:nrow(allCodeMtx) ){
	for( sIdx in 1:nrow(stdCodeMtx) ){
		cnt <- sum(allCodeMtx[aIdx,]==stdCodeMtx[sIdx,])
		if( 5<=cnt ) {
			flag[aIdx] <- sIdx
			break
		}
	}
}
tDiff <- Sys.time() - tStmp
allZoidMtx <- allZoidMtx[flag==0,]
filtLst[[1+length(filtLst)]] <- getFiltHist( filtId ,tStmp ,allZoidMtx )
k.FLogStr(sprintf("%s %d",filtId,nrow(allZoidMtx)))

#-[C0040.A]------------------------------------------------------
#	5개 이상 일치하는 게 2개 밖에 없음..
filtId <- "C0040.A"
stdCodeMtx <- abs(zhF[1:(nrow(zhF)-1),] - zhF[2:nrow(zhF),])
lastZoid <- zhF[nrow(zhF),]
allCodeMtx <- allZoidMtx
for( aIdx in 1:nrow(allCodeMtx) ){
	allCodeMtx[aIdx,] <- abs(allCodeMtx[aIdx]-lastZoid)
}

flag <- apply(allZoidMtx,1,function(p){ sum(p==stdCodeMtx[1,]) })

flag <- rep( 0 ,nrow(allZoidMtx) )
tStmp <- Sys.time()
for( aIdx in 1:nrow(allCodeMtx) ){
	for( sIdx in 1:nrow(stdCodeMtx) ){
		cnt <- sum(allCodeMtx[aIdx,]==stdCodeMtx[sIdx,])
		if( 5<=cnt ) {
			flag[aIdx] <- sIdx
			break
		}
	}
}
tDiff <- Sys.time() - tStmp
allZoidMtx <- allZoidMtx[flag==0,]
filtLst[[1+length(filtLst)]] <- getFiltHist( filtId ,tStmp ,allZoidMtx )
k.FLogStr(sprintf("%s %d",filtId,nrow(allZoidMtx)))

#----------------------------------------------------------------
# QQE todo


#-[C0040.B]------------------------------------------------------
#	Jump 2~10
#		stdCodeMtx에서 780번째 row이면 zhF에서의 780,783 차이값임.
#		보류.. 효과 없는 거 같다.

#-[C0050.A]------------------------------------------------------

#=[       ]====================================================================================
#-[D0010.A]------------------------------------------------------
#	최근 2개 History에서 발생한 요소가 재발생...
#		4개 이상은 26개 밖에 없었다.
filtId <- "D0010.A"
bNum <- 2
lastPool <- as.vector(zhF[(nrow(zhF)-bNum+1):nrow(zhF),])
lastPool <- sort(unique(lastPool))
flag <- rep( 0 ,nrow(allZoidMtx) )
tStmp <- Sys.time()
for( aIdx in 1:nrow(allZoidMtx) ){
	flag[aIdx] <- sum( lastPool %in% allZoidMtx[aIdx,] )
}
tDiff <- Sys.time() - tStmp
allZoidMtx <- allZoidMtx[flag<4,]
filtLst[[1+length(filtLst)]] <- getFiltHist( filtId ,tStmp ,allZoidMtx )
k.FLogStr(sprintf("%s %d",filtId,nrow(allZoidMtx)))

#-[D0010.B]------------------------------------------------------
#	최근 10개 History에서 발생한 요소가 재발생...
#		1개 이하는 11개 밖에 없었다.
filtId <- "D0010.B"
bNum <- 10
lastPool <- as.vector(zhF[(nrow(zhF)-bNum+1):nrow(zhF),])
lastPool <- sort(unique(lastPool))
flag <- rep( 0 ,nrow(allZoidMtx) )
tStmp <- Sys.time()
for( aIdx in 1:nrow(allZoidMtx) ){
	flag[aIdx] <- sum( lastPool %in% allZoidMtx[aIdx,] )
}
tDiff <- Sys.time() - tStmp
allZoidMtx <- allZoidMtx[flag>1,]
filtLst[[1+length(filtLst)]] <- getFiltHist( filtId ,tStmp ,allZoidMtx )
k.FLogStr(sprintf("%s %d",filtId,nrow(allZoidMtx)))


#=[패턴재현 제거]==============================================================================
#-[E0010.A]------------------------------------------------------
filtId <- "E0010.A"
stdCodeMtx <- zhF
allCodeMtx <- allZoidMtx
tStmp <- Sys.time()
for( nextJump in 1:15 ){
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



# sapply( filtGrp$filtGrpLst ,function(p){length(p$filtLst)} )

filtRst <- filtGrp$filt( allZoidMtx )
tDiff <- Sys.time() - tStmp
flag <- sapply(filtRst,function(p){p$survive})
allZoidMtx <- allZoidMtx[flag,]
filtLst[[1+length(filtLst)]] <- getFiltHist( filtId ,tStmp ,allZoidMtx )
k.FLogStr(sprintf("%s %d",filtId,nrow(allZoidMtx)))





#=[       ]====================================================================================
#-[Z0010.A]------------------------------------------------------



#=[SAVE]========================================================================================
deskObj <- list( allZoidMtx=allZoidMtx )
deskObj$memo <- sprintf("zhF size : %d.",nrow(zhF))
deskObj$filtLst <- filtLst
save( deskObj ,file="Obj_deskObj.DE0.save" )









