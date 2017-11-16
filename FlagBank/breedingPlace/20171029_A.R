require("XLConnect")
# 기초 데이터 로딩
curWd <- getwd()	;setwd("..")
FB <- getFlagBank()	;setwd(curWd)
# 헤더 파일
source("20170917_A_H.R")
source("20171029_A_H.R")
source("20171029_A_auxH.R")
# sfInit( parallel=T, cpus=CPU.NUM )

devMode <- TRUE

pZh <- as.matrix(FB$zh)
if( devMode )
	pZh <- pZh[1:120,] # h 10개당 2분 정도.

# devMode <- F # creFunSet 테스트용.
makeSpan <- 1:nrow(pZh)
creFunSet <- getCreateFunSet( pZh ,devMode )
	# saveCreFunSetInfo( creFunSet ,"Xls_creFunSet.xls")
eleSet <- getNewElementSet( creFunSet ,pZh=pZh[makeSpan,] )

# -------------------------------------------------------------------------------
#	eleSet 데이터 생성
#		데이터 생성 후, 반드시 eleSet$createTime <- Sys.time() !!
# -------------------------------------------------------------------------------
tStamp <- Sys.time()
for( msIdx in makeSpan ){

	if( 1==(msIdx%%50) ){
		tDiff <- Sys.time()-tStamp
		k.FLogStr(sprintf("msIdx:%d %.1f %s" ,msIdx ,tDiff ,units(tDiff) )
					,pConsole=T ,pTime=F
				)
	}

	bornEleLst <- list()
	eleSet$rowSpan <- if( 1==msIdx ) integer(0) else 1:(msIdx-1)

	# 일단 현재 Zoid에 대한 element value, 즉 bornEleLst를 계산.
	#	bornEleLst는 나중에 유전 알고리즘에서의 평가자료로도 사용될 것이다.
	for( eleIdx in seq_len(length(creFunSet)) ){
		colVal <- sapply( creFunSet[[eleIdx]] ,function( p ){
						p$output( eleSet ,pZh[msIdx,] ,bornEleLst )
						# p$ioAddr[["outLst"]]
					})
		bornEleLst[[eleIdx]] <- colVal
	} # for(eleIdx)

	# 현재는 zoid history에 대한 element value생성 중 이므로
	#	bornEleLst를 eleSet에 저장.
	for( eleIdx in seq_len(length(creFunSet)) ){
		eleSet$eleLst[[eleIdx]]$mtx[msIdx,] <- bornEleLst[[eleIdx]]
	} # for(eleIdx)

} # for(msIdx)
eleSet$createTime <- Sys.time()	# 최소한의 버전관리를 위해.

stmpDiff <- Sys.time() - tStamp
k.FLogStr(sprintf("eleSet is made. cost:%.1f%s",stmpDiff,units(stmpDiff)),pConsole=T)

# -------------------------------------------------------------------------------
#	hAnaSet 생성.
# -------------------------------------------------------------------------------

# 실행시간 엄청 오래걸림. 분할실행할 것.
# hAnaSet <- analyzeSeq( eleSet ,pDebug=T )
stmpDiff <- Sys.time() - tStamp
k.FLogStr(sprintf("hAnaSet is made. cost:%.1f%s",stmpDiff,units(stmpDiff)),pConsole=T)


# -------------------------------------------------------------------------------
#	transSet 생성.
# -------------------------------------------------------------------------------
transSet <- getTranslateSet( eleSet )
	# transSet <- transSet[1:18]
transRstLst <- lapply( hAnaSet$hLst ,function( pH ){
						rObj <- list( stdH=pH$stdH )
						rObj$transLst <- lapply( transSet ,function( pT ){ pT$translate( eleSet ,pH$anaLst ) } )
						return( rObj )
					})


# -[scoreMtx]--------------------------------------------------------------------
# 히스토리 전체를 관통하여 개별 trans 의 득실을 검토하자.
cName <- c("score","prior","isChCnt","isCh.hit","isCh.aid","isCh.kill","isCh.troll")
	# prior : 정답 앞의 다른 후보들이 몇 이나 있는지.
	#			상위 %는 의미가 별로 없어서..
scoreMtx <- matrix( 0 ,nrow=length(transRstLst) ,ncol=length(cName) )
colnames(scoreMtx) <- cName
scoreLst <- list()
for( tIdx in seq_len(length(transSet)) ){
	scoreMtx[,] <- 0
	for( rstIdx in seq_len(length(transRstLst)) ){
		trans <- transRstLst[[rstIdx]]$transLst[[tIdx]]
		stdVal<- eleSet$eleLst[[ trans$eleCord["ele"] ]]$mtx[ transRstLst[[rstIdx]]$stdH, trans$eleCord["col"] ]
		stdVal.idx <- if( is.na(stdVal) ){	which( is.na(trans$codeVal) )
						} else which( trans$codeVal == stdVal )

		scoreMtx[rstIdx	,"score"]		<- trans$prob[stdVal.idx]
		scoreMtx[rstIdx	,"prior"]		<- sum(trans$prob>=trans$prob[stdVal.idx])
		scoreMtx[rstIdx	,"isChCnt"]		<- sum(trans$isChanging!=0)
		if( 0<scoreMtx[rstIdx	,"isChCnt"] ){
			flag <- which(trans$isChanging>0)==stdVal.idx
			scoreMtx[rstIdx	,"isCh.hit"]	<- sum( flag)
			scoreMtx[rstIdx	,"isCh.troll"]	<- sum(!flag)

			flag <- which(trans$isChanging<0)==stdVal.idx
			scoreMtx[rstIdx	,"isCh.kill"]	<- sum( flag)
			scoreMtx[rstIdx	,"isCh.aid"]	<- sum(!flag)
		} # if
	} # for( rstIdx )
	scoreLst[[1+length(scoreLst)]] <- scoreMtx
} # for(tIdx)



# -[scoreMtx]--------------------------------------------------------------------
#	히스토리 별 조회임.
cName <- c("stdH","score","scorePer","isChCnt","isCh.hit","isCh.aid","isCh.kill","isCh.troll")
scoreMtx <- matrix( 0 ,nrow=length(transRstLst) ,ncol=length(cName) )
colnames(scoreMtx) <- cName
transSpan <- seq_len(length(transSet))
for( rstIdx in seq_len(length(transRstLst) ) ){

	transRst <- transRstLst[[rstIdx]]
	scoreMtx[rstIdx,"stdH"] <- transRst$stdH

	score <- 0	;scorePer <- 0
	isChCnt<-0
	isCh.hit	<-0		;isCh.aid	<-0	# 정답 맞추거나 경쟁자 제거
	isCh.kill	<-0		;isCh.troll	<-0	# 정답을 없애거나, 방해.
	for( tIdx in transSpan ){
		trans <- transRst$transLst[[tIdx]]
		stdVal<- eleSet$eleLst[[ trans$eleCord["ele"] ]]$mtx[ transRst$stdH, trans$eleCord["col"] ]
		stdVal.idx <- if( is.na(stdVal) ){	which( is.na(trans$codeVal) )
						} else which( trans$codeVal == stdVal )

		# 몇 점을 얻었는가?
		score <- score + trans$prob[stdVal.idx]
		# 상위 %는?
		scorePer <- scorePer + rank(-trans$prob)[stdVal.idx] / length(trans$prob)
		# isChanging이 몇 개나 있었는가?
		isChCnt <- isChCnt + sum( trans$isChanging!=0 )

		if( any(trans$isChanging!=0) ){
			flag <- which(trans$isChanging>0)==stdVal.idx
			isCh.hit	<- isCh.hit		+ sum( flag)
			isCh.troll	<- isCh.troll	+ sum(!flag)
			
			flag <- which(trans$isChanging<0)==stdVal.idx
			isCh.kill	<- isCh.kill	+ sum( flag)
			isCh.aid	<- isCh.aid		+ sum(!flag)
 		}

	} # for(tIdx)

	scoreMtx[rstIdx	,"score"]		<- score
	scoreMtx[rstIdx	,"scorePer"]	<- scorePer / length(transSpan)
	scoreMtx[rstIdx	,"isChCnt"]		<- isChCnt
	scoreMtx[rstIdx	,"isCh.hit"]	<- isCh.hit
	scoreMtx[rstIdx	,"isCh.aid"]	<- isCh.aid
	scoreMtx[rstIdx	,"isCh.kill"]	<- isCh.kill
	scoreMtx[rstIdx	,"isCh.troll"]	<- isCh.troll

} # for(rstIdx)




#----------------------------------------------------------------
save( hAnaSet ,file="Obj_hAnaSetDev.save" )
save( eleSet  ,file="Obj_eleSetDev.save")
save( creFunSet ,file="Obj_creFunSetDev.save" )



# -------------------------------------------------------
myObj <- load("Obj_hAnaSetDev.save")
myObj <- load("Obj_eleSetDev.save")
myObj <- load("Obj_creFunSetDev.save")


