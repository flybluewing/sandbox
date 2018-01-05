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
filtLst <- list()
getFiltHist <- function( pFiltId ,pTStmp ,pFlag=NULL ){
		rObj <- list( filtId=pFiltId ,tCost=(Sys.time()-pTStmp) ,flag=pFlag )
		return( rObj )
	}
#=[missHLst]===============================================================================
#	이 파일에서의 핵심 데이터.
#	사전 필터로 인해 유실되는 실제 Zoid History Index.
missHLst <- list()

# zhF ,allZoidMtx
testSpan <- 500:nrow(zhF)


#-[A01NN.A]------------------------------------------------------
lostHLst <- list()
#-[A0100.A]------------------------------------------------------
filtId <- "A0100.A"
tStmp <- Sys.time()
failedHLst <- list()
for( nextJump in 1:8 ){
	flagLst <- list()
	for( testIdx in testSpan ){
		lastZoidMtx <- zhF[testIdx,,drop=F]
		filtGrp <- getPtnRebGrp( zhF[1:(testIdx-1),] ,pNextJump=nextJump )
		filtRst <- filtGrp$filt( lastZoidMtx )
		flagObj <- list( hIdx=testIdx ,filtRst=filtRst )
		flagObj$filtNum <- sapply(filtGrp$filtGrpLst ,function(p){length(p$filtLst)})
		flagLst[[1+length(flagLst)]] <- flagObj
	}
	surviveF <- sapply( flagLst ,function(p){ p$filtRst[[1]]$survive } )
	failedHLst[[1+length(failedHLst)]] <- sapply(flagLst[!surviveF] ,function(p){p$hIdx} )
}
tDiff <- Sys.time() - tStmp
lostHLst[[filtId]] <- failedHLst
k.FLogStr(sprintf("%s : %.1f%s",filtId,tDiff,units(tDiff)))
missHLst[[filtId]] <- sort(unique(do.call( c ,failedHLst )))

#-[A0110.A]------------------------------------------------------
filtId <- "A0110.A"
tStmp <- Sys.time()
failedHLst <- list()
for( nextJump in 1:2 ){
	flagLst <- list()
	for( testIdx in testSpan ){
		lastZoidMtx <- zhF[testIdx,,drop=F]
		filtGrp <- getPtnRebGrp2( zhF[1:(testIdx-1),] ,pNextJump=nextJump )
		filtRst <- filtGrp$filt( lastZoidMtx )
		flagObj <- list( hIdx=testIdx ,filtRst=filtRst )
		flagObj$filtNum <- sum(!is.na(filtGrp$remVal))
		flagLst[[1+length(flagLst)]] <- flagObj
	}
	surviveF <- sapply( flagLst ,function(p){p$filtRst[[1]]$survive} )
	failedHLst[[1+length(failedHLst)]] <- sapply(flagLst[!surviveF] ,function(p){p$hIdx} )
}
tDiff <- Sys.time() - tStmp
lostHLst[[filtId]] <- failedHLst
k.FLogStr(sprintf("%s : %.1f%s",filtId,tDiff,units(tDiff)))
missHLst[[filtId]] <- sort(unique(do.call( c ,failedHLst )))

#-[AK000.A]------------------------------------------------------
#	zhF[,6]-zhF[,1]	: 20 이하인 경우.
filtId <- "AK000.A"
stdCode <- zhF[,6]-zhF[,1]
missHLst[[filtId]] <- which(stdCode<=20)

#-[AK000.B]------------------------------------------------------
#	zhF[,6]-zhF[,1]	: 같은 값이 연속되는 경우. 총 35개 5%미만.
filtId <- "AK000.B"
stdCode <- zhF[,6]-zhF[,1]
flag <- stdCode[1:(length(stdCode)-1)]==stdCode[2:length(stdCode)]
missHLst[[filtId]] <- which(flag)+1

#-[AK000.C]------------------------------------------------------
#	zhF[,6]-zhF[,1]	: 이전 연속체가 다음에도 반복될 가능성은?
filtId <- "AK000.C"
stdCode <- zhF[,6]-zhF[,1]
flag <- rep( 0 ,nrow(zhF) )
for( hIdx in testSpan ){
	ptn <- NULL
	for( dIdx in 5:1 ){
		ptn <- getPastPtn( stdCode[1:(hIdx-1)] ,pDepth=dIdx )
		if( !is.null(ptn) ){
			ptn$depth <- dIdx
			break
		}
	}
	if( !is.null(ptn) ){
		flag[hIdx] <- stdCode[hIdx]==ptn$nextVal
	}
}
missHLst[[filtId]] <- which(flag>0)

#-[AK000.D]------------------------------------------------------
#	zhF[,6]-zhF[,1]	: 이전과 같은 간격으로 건너뛸 가능성. 55/785
filtId <- "AK000.D"
stdCode <- zhF[,6]-zhF[,1]
stepDiff <- abs(stdCode[2:length(stdCode)]-stdCode[1:(length(stdCode)-1)])
flag <- stepDiff[2:length(stepDiff)]==stepDiff[1:(length(stepDiff)-1)]
missHLst[[filtId]] <- which(flag)+2


#-[AL000.A]------------------------------------------------------
#	이전에 발생한 DNA의 첫번째가 다음에도 발생할 가능성. 53/785
filtId <- "AL000.A"
flag <- rep( 0 ,nrow(zhF) )
for( hIdx in 2:nrow(zhF) ){
	comVal <- intersect( zhF[hIdx,] ,zhF[hIdx-1,] )
	if( 0<length(comVal) ){
		flag[hIdx] <- comVal[1]
	}
}
sameIdx <- 1+which(flag[1:(length(flag)-1)]==flag[2:length(flag)])
missHLst[[filtId]] <- sameIdx[flag[sameIdx]>0]

#-[AP000.A]------------------------------------------------------
#	zhF %/% 10 Quoatient.  이전 H와 1개 이내로 틀리는 것 59/787
filtId <- "AP000.A"
stdCodeMtx <- zhF %/% 10
flag <- rep(0,nrow(stdCodeMtx))
for( hIdx in 2:nrow(stdCodeMtx) ){
	flag[hIdx] <- sum(stdCodeMtx[hIdx,]==stdCodeMtx[(hIdx-1),])
}
missHLst[[filtId]] <- which(flag>=5)

#-[AP000.B]------------------------------------------------------
#	zhF %/% 10 Quoatient.  동일한 패턴의 재발간격. 7 H 이내 재발 57/787
filtId <- "AP000.B"
stdCodeMtx <- zhF %/% 10
matMtx <- scanSameRow( stdCodeMtx ,pThld=6 )
matMtx <- matMtx[(matMtx[,2]-matMtx[,1])<=7,]
missHLst[[filtId]] <- sort(unique(matMtx[,"bIdx"]))

#-[AP000.C]------------------------------------------------------
#	zhF %/% 10 Quoatient.  한가지 Quoatient가 4개 이상인 경우 49/787
filtId <- "AP000.B"
stdCodeMtx <- zhF %/% 10
flag <- apply( stdCodeMtx ,1 ,function(p){ max(table(p)) } )
missHLst[[filtId]] <- which(flag>=4)


#-[AQ000.A]------------------------------------------------------
#	DNA 코드가 다음 H에서 몇 개나 재발되는지. (3개 이상 20/787)
filtId <- "AQ000.A"
flag <- rep(0,nrow(zhF))
for( hIdx in 2:nrow(zhF) ){
	flag[hIdx] <- sum(zhF[hIdx,] %in% zhF[hIdx-1,])
}
missHLst[[filtId]] <- which(flag>=3)

QQE.Todo
#-[AR000.A]------------------------------------------------------
#	remainder 재발
filtId <- "AR000.A"
stdCodeMtx <- zhF %% 2

#-[AR000.A]------------------------------------------------------
#	remainder 패턴 재발 (nextVal)


#-[AS000.B]------------------------------------------------------
#	연이은 DNA코드가 다음에도 연이어서 재발 52/787
#	3개 이상은 잘라내므로 2개 연속일때만 체크하면 된다.
#	수정. 같은 col 간격.
filtId <- "AS000.B"
flag <- rep(0,nrow(zhF))
flag <- rep(0,nrow(zhF))
for( hIdx in 2:nrow(zhF) ){
	rebIndices <- which( zhF[hIdx,] %in% zhF[hIdx-1,])
	if( 2!=length(rebIndices) ){
		next	# 3개 이상발생은 AS000.A에서 제거되었다.
	}
	rebIndices.2 <- which( zhF[hIdx-1,] %in% zhF[hIdx,])
	if( (rebIndices[2]-rebIndices[1])==(rebIndices.2[2]-rebIndices.2[1]) ) {
		flag[hIdx] <- rebIndices[1]
	}
}
missHLst[[filtId]] <- which(flag>=0)



#----------------------------------------------------------------
#-[C0000.A]------------------------------------------------------
#	zhF[,2:6]-zhF[,1:5] : 똑같은 경우는 없었고, 1개 틀린 경우는 32/787
filtId <- "C0000.A"
stdCodeMtx <- zhF[,2:6]-zhF[,1:5]
matMtx <- scanSameRow( stdCodeMtx ,pThld=4 )
missHLst[[filtId]] <- sort(unique(matMtx[,"bIdx"]))

#-[C1000.A]------------------------------------------------------
#	abs(zhF[1:(nrow(zhF)-1),] - zhF[2:nrow(zhF),])
#		2개 이하로 틀린 경우는 65개 정도
filtId <- "C1000.A"
stdCodeMtx <- abs(zhF[1:(nrow(zhF)-1),] - zhF[2:nrow(zhF),])
matMtx <- scanSameRow( stdCodeMtx ,pThld=4 )
missHLst[[filtId]] <- sort(unique(matMtx[,"bIdx"]))+1


#=[Report]=====================================================================================
allMiss <- sort( unique(do.call(c,missHLst)) )
missFlag <- rep(0,nrow(zhF))
missFlag[allMiss] <- 1
finalMiss <- missFlag[testSpan]

#=[SAVE]=======================================================================================
valObj <- list( missHLst=missHLst ,lostHLst=lostHLst )
valObj$memo <- sprintf("zhF size : %d.",nrow(zhF))
save( valObj ,file="Obj_valObj.E.save" )
