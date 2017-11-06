# 기초 데이터 로딩
curWd <- getwd()	;setwd("..")
FB <- getFlagBank()	;setwd(curWd)
# 헤더 파일
source("20170917_A_H.R")
source("20171029_A_H.R")
CPU.NUM <- 2
sfInit( parallel=T, cpus=CPU.NUM )

devMode <- TRUE

pZh <- as.matrix(FB$zh)
if( devMode )
	pZh <- pZh[1:300,] # h 10개당 2분 정도.

makeSpan <- 1:nrow(pZh)
creFunSet <- getCreateFunSet( pZh ,devMode )
eleSet <- getNewElementSet( creFunSet ,pZh=pZh[makeSpan,] )

# -------------------------------------------------------------------------------
#	eleSet 데이터 생성
#		데이터 생성 후, 반드시 eleSet$createTime <- Sys.time() !!
# -------------------------------------------------------------------------------
tStamp <- Sys.time()
for( msIdx in makeSpan ){

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
hAnaSet <- analyzeSeq( eleSet ,pDebug=T )
stmpDiff <- Sys.time() - tStamp
k.FLogStr(sprintf("hAnaSet is made. cost:%.1f%s",stmpDiff,units(stmpDiff)),pConsole=T)


# -------------------------------------------------------------------------------
#	transSet 생성.
# -------------------------------------------------------------------------------
transSet <- getTranslateSet( eleSet )
transRstLst <- lapply( hAnaSet$hLst ,function( pH ){
						rObj <- list( stdH=pH$stdH )
						rObj$transLst <- lapply( transSet ,function( pT ){ pT$translate( eleSet ,pH$anaLst ) } )
						return( rObj )
					})




save( hAnaSet ,file="Obj_hAnaSetDev.save" )
save( eleSet  ,file="Obj_eleSetDev.save")
save( creFunSet ,file="Obj_creFunSetDev.save" )



# -------------------------------------------------------
myObj <- load("Obj_hAnaSetDev.save")
myObj <- load("Obj_eleSetDev.save")
myObj <- load("Obj_creFunSetDev.save")


