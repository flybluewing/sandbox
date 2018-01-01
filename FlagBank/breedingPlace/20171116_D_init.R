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

allZoidMtx <- getAllZoid() # 38sec

#=[BaseFilt]====================================================================================
#-[A0010]------------------------------------------------------
codeMtx <- allZoidMtx[,2:6]-allZoidMtx[,1:5]
tStmp <- Sys.time()
stepM <- apply( codeMtx ,1 ,function(p){max(table(p))})
filtLst[[1+length(filtLst)]] <- getFiltHist( "A0010" ,tStmp )
allZoidMtx <- allZoidMtx[stepM<=2,]		# 7740330 

#-[A0020]------------------------------------------------------
codeMtx <- allZoidMtx %% 2
tStmp <- Sys.time()
cnt <- apply( codeMtx ,1 ,function(p){sum(p==0)})
filtLst[[1+length(filtLst)]] <- getFiltHist( "A0020" ,tStmp )
allZoidMtx <- allZoidMtx[(cnt>0)&(cnt<6),]	# 7599240

#-[A0030]------------------------------------------------------
codeMtx <- allZoidMtx %% 3
tStmp <- Sys.time()
cnt <- apply( codeMtx ,1 ,function(p){sum(p==0)})
filtLst[[1+length(filtLst)]] <- getFiltHist( "A0030" ,tStmp )
allZoidMtx <- allZoidMtx[(cnt>0),]	# 7041489

#=[임의제거]====================================================================================
# tail(zhF) E1 E2 E3 E4 E5 E6
# 		781 11 16 18 19 24 39
# 		782  6 18 31 34 38 45
# 		783 14 15 16 17 38 45
# 		784  3 10 23 24 31 39
# 		785  4  6 15 25 26 33
# 		786 12 15 16 20 24 30

#-[B0010.A]------------------------------------------------------
#	중복발생 나왔으니 다음에도 15가 발생하긴 어렵겠지.
tStmp <- Sys.time()
flag <- apply( allZoidMtx ,1 ,function(p){!any(p==15)})
filtLst[[1+length(filtLst)]] <- getFiltHist( "B0010.A" ,tStmp )
allZoidMtx <- allZoidMtx[flag,]

#-[B0010.B]------------------------------------------------------
#	같은 컬럼에서의 연속 값 두번이니, 다음에도 같은 컬럼에 연속 값은 어렵겠지.
seqVal <- zhF[nrow(zhF),] + 1
tStmp <- Sys.time()
flag <- apply( allZoidMtx ,1 ,function(p){ !any(p==seqVal) })
filtLst[[1+length(filtLst)]] <- getFiltHist( "B0010.B" ,tStmp )
allZoidMtx <- allZoidMtx[flag,]

#-[B0010.C]------------------------------------------------------
#	설마 3개 이상 값이 재발되지는 않겠지.
lastZoid <- zhF[nrow(zhF),]
tStmp <- Sys.time()
flag <- apply( allZoidMtx ,1 ,function(p){sum(p%in%lastZoid)})
filtLst[[1+length(filtLst)]] <- getFiltHist( "B0010.C" ,tStmp )
allZoidMtx <- allZoidMtx[flag<=2,]


#-[B0010.D]------------------------------------------------------
#	이전 zoid 코드에서 연속된 부분이 다음에도 재발하긴 어렵겠지.
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
filtLst[[1+length(filtLst)]] <- getFiltHist( "B0010.D" ,tStmp )
allZoidMtx <- allZoidMtx[flag,]


#-[B0010.E]------------------------------------------------------
#	여태까지 발생한 zoid들의 거리가 동일하게 떨어지지는 않겠지.
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
filtLst[[1+length(filtLst)]] <- getFiltHist( "B0010.E" ,tStmp )
allZoidMtx <- allZoidMtx[flag==0,]


#-[B0010.F]------------------------------------------------------
#	grp 구성이 동일할 수는 없겠지.
#		즉 30대 30 하나, 20 대 20,24 딱 2개, 10대 12,15,16 딱 세개 그대로 발생은..
lastZoid <- zhF[nrow(zhF),]
grp.grp <- unique(lastZoid%/%10)
grp.dna <- sapply( grp.grp ,function(p){ 
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
filtLst[[1+length(filtLst)]] <- getFiltHist( "B0010.F" ,tStmp )
allZoidMtx <- allZoidMtx[flag==0,]

#-[B0010.F2]-----------------------------------------------------
#	grp 구성과 순서가 동일할 수는 없겠지.
#		12 15 16 20 24 30 --> 3:2:1 순서가 바로 다음에도 반복될까?
#	QQE:Todo 시간이 오래걸릴 듯 하여 일단 하드코딩으로 처리한다.
#			나중에 개선 요.
tStmp <- Sys.time()
stdCodeMtx <- allZoidMtx %/% 10
flag <- rep( TRUE ,nrow(stdCodeMtx) )
for( aIdx in 1:nrow(stdCodeMtx) ){
	if( stdCodeMtx[aIdx,1]==stdCodeMtx[aIdx,2] && 
			stdCodeMtx[aIdx,2]==stdCodeMtx[aIdx,3] 
	){
		if( stdCodeMtx[aIdx,4]==stdCodeMtx[aIdx,5] ){
			# stdCodeMtx[aIdx,3]!=stdCodeMtx[aIdx,4] 조건이 있어야 하긴 하지만
			# 그러면 1~5까지 모두 같은 grp라는 것이 되니.. 
			
			# stdCodeMtx[aIdx,5]!=stdCodeMtx[aIdx,6] 을 체크해야 할 까?
			# 그러면 최소한 3:3 내지는 한 개 grp인데... 힘들겠지?
			flag[aIdx] <- FALSE
		}
	}
}
tDiff <- Sys.time() - tStmp
filtLst[[1+length(filtLst)]] <- getFiltHist( "B0010.F2" ,tStmp )
allZoidMtx <- allZoidMtx[flag,]

#-[B0010.F3]-----------------------------------------------------
#	한 개 grp에 4개 씩이나 몰리는 건 최근에 2번이나.. 피해보자.
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
filtLst[[1+length(filtLst)]] <- getFiltHist( "B0010.F3" ,tStmp )
allZoidMtx <- allZoidMtx[flag,]



#=[손실감수]====================================================================================
#-[C0010.A]------------------------------------------------------
#	최근 30 history 이내에서 2개 이상 매치되는 경우는 10건 정도밖에 없었다.
#		abs(allZoidMtx[aIdx,]-zhF[hIdx,])
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
filtLst[[1+length(filtLst)]] <- getFiltHist( "C0010.A" ,tStmp )
allZoidMtx <- allZoidMtx[flag==0,]


#-[C0020.A]------------------------------------------------------
#	zhF[,6]-zhF[,1] 이 20 이하인 경우는 전체 5.6% 정도.. 자르자!!
tStmp <- Sys.time()
flag <- apply( allZoidMtx ,1 ,function(p){p[6]-p[1]})
tDiff <- Sys.time() - tStmp
filtLst[[1+length(filtLst)]] <- getFiltHist( "C0020.A" ,tStmp )
allZoidMtx <- allZoidMtx[flag>20,]

#-[C0020.B]------------------------------------------------------
#	일치 가능성은 전체 5.3%.. 자르자.
#		시간만 오래 걸리고 걸리는 게 없는 듯..
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
filtLst[[1+length(filtLst)]] <- getFiltHist( "C0020.B" ,tStmp )
allZoidMtx <- allZoidMtx[flag==0,]


#-[C0030.A]------------------------------------------------------
#	5개 이상 일치 82개. 약 10%... 자르자.
#		시간만 오래 걸리고 걸리는 게 없는 듯..
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
filtLst[[1+length(filtLst)]] <- getFiltHist( "C0030.A" ,tStmp )
allZoidMtx <- allZoidMtx[flag==0,]

#-[C0040.A]------------------------------------------------------
#	5개 이상 일치하는 게 2개 밖에 없음..
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
filtLst[[1+length(filtLst)]] <- getFiltHist( "C0040.A" ,tStmp )
allZoidMtx <- allZoidMtx[flag==0,]

#----------------------------------------------------------------
# QQE todo


#-[C0040.B]------------------------------------------------------
#	Jump 2~10
#		stdCodeMtx에서 780번째 row이면 zhF에서의 780,783 차이값임.
#		보류.. 효과 없는 거 같다.
for( jIdx in 2:10 ){
	stdCodeMtx <- abs(zhF[1:(nrow(zhF)-jIdx),] - zhF[(jIdx+1):nrow(zhF),])
	stdZoid <- zhF[nrow(zhF)-jIdx+1,]	# 다음 발생 Zoid와의 차이이므로..
	allCodeMtx <- allZoidMtx	# loop동안 계속 줄어들 것이므로.
	for( aIdx in 1:nrow(allCodeMtx) ){
		# QQE
	}

}

#-[C0050.A]------------------------------------------------------

#=[       ]====================================================================================
#-[D0010.A]------------------------------------------------------
#	최근 2개 History에서 발생한 요소가 재발생...
#		4개 이상은 26개 밖에 없었다.
bNum <- 2
lastPool <- as.vector(zhF[(nrow(zhF)-bNum+1):nrow(zhF),])
lastPool <- sort(unique(lastPool))
flag <- rep( 0 ,nrow(allZoidMtx) )
tStmp <- Sys.time()
for( aIdx in 1:nrow(allZoidMtx) ){
	flag[aIdx] <- sum( lastPool %in% allZoidMtx[aIdx,] )
}
tDiff <- Sys.time() - tStmp
filtLst[[1+length(filtLst)]] <- getFiltHist( "D0010.A" ,tStmp )
allZoidMtx <- allZoidMtx[flag<4,]

#-[D0010.B]------------------------------------------------------
#	최근 10개 History에서 발생한 요소가 재발생...
#		1개 이하는 11개 밖에 없었다.
bNum <- 10
lastPool <- as.vector(zhF[(nrow(zhF)-bNum+1):nrow(zhF),])
lastPool <- sort(unique(lastPool))
flag <- rep( 0 ,nrow(allZoidMtx) )
tStmp <- Sys.time()
for( aIdx in 1:nrow(allZoidMtx) ){
	flag[aIdx] <- sum( lastPool %in% allZoidMtx[aIdx,] )
}
tDiff <- Sys.time() - tStmp
filtLst[[1+length(filtLst)]] <- getFiltHist( "D0010.B" ,tStmp )
allZoidMtx <- allZoidMtx[flag>1,]


#=[       ]====================================================================================
#-[Z0010.A]------------------------------------------------------
jumpStepSpan <- 1:100
remMtx <- matrix(NA,ncol=6,nrow=length(jumpStepSpan))
bjMtx <- matrix(NA,ncol=6,nrow=length(jumpStepSpan))
for( jumpStep in jumpStepSpan ){
	for( cIdx in 1:6 ){
		for( bjIdx in 5:2 ){
			ptn <- getPastPtn( zhF[1:(nrow(zhF)-jumpStep-1),cIdx] ,pDepth=bjIdx ,pScanAll=F )
			if( !is.null(ptn) ){
				remMtx[jumpStep,cIdx] <- zhF[ptn$fIdx+jumpStep,cIdx]
				bjMtx[jumpStep,cIdx] <- bjIdx
				break
			}
		}
	}
}

matNum <- rep(0,nrow(remMtx))
filtNum <- rep(0,nrow(remMtx))
score <- rep(0,nrow(allZoidMtx))
#for( aIdx in 1:nrow(allZoidMtx) ){
for( aIdx in 1:100 ){
	matNum[] <- 0
	filtNum[] <- 0
	for( remIdx in 1:nrow(remMtx) ){
		filtNum[remIdx] <- sum( !is.na(remMtx[remIdx,]) )
		matNum[remIdx] <- sum(allZoidMtx[aIdx,]!=remMtx[remIdx,],na.rm=T)
	}
	sum(filtNum)-sum(matNum)
	score[aIdx] <- 
}


#=[SAVE]========================================================================================
deskObj <- list( allZoidMtx=allZoidMtx )
deskObj$memo <- sprintf("zhF size : %d.",nrow(zhF))
deskObj$filtLst <- filtLst
save( deskObj ,file="Obj_deskObj.D01.save" )









