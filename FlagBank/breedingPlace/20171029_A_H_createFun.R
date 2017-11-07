# 파일명 : 20171029_A_H_createFun.R
#	실질 사용보다는 ioAddr 객체가 이렇게 구성된다는 걸 
#	전시하기 위한 코드.
getIoAddr <- function( pIn ,pOut ,pZDC=NULL	,pZEALst=NULL ){
	
	inLst <- list()	;inLst[[1]] <- pIn	;names(inLst[[1]]) <- c("ele","col")
	outLst<- list()	;outLst[[1]]<- pOut	;names(outLst[[1]])<- c("ele","col")

	rObj <- list( inLst=inLst ,outLst=outLst )
	rObj$zDC	= pZDC		# Zoid DNA Column index
							# output() 함수에서 pZoid의 어느 부분을 사용할 것인지.

	rObj$zEALst	= pZEALst	# current Zoid temporary element input address list
							# output() 함수에서 pZoid으로부터 생성한 현재 elementSet의 어느 컬럼을 사용할 것인지?
							#	c("ele","col")
							# 2차 이상의 create function을 위해 필요.

	return(rObj)
}



# =========================================================================================
#	ElementSet
# =========================================================================================

#	1세대 elementSet을 만들기 위한 함수들
#		Raw DNA Code Column 하나를 대상으로 파생되는 Set로서,
#		하나의 Column에 대한 1차 가공 결과만을 가지며,
#		Raw DNA 말고는 서로간의 의존관계가 없어야 한다.
get1stCreateFunSet <- function( pZh ,pDev=F ){
	
	generationNumber <- 1
	ioAddr <- getIoAddr( c(generationNumber,0) ,c(generationNumber,0) ,NULL ,NULL )
	funLst <- list()

	outIdxAccum <- 0
	chunk <- 6
	for( chIdx in 1:chunk ){
		ioAddr$inLst[[1]]["col"] <- chIdx
		ioAddr$outLst[[1]]["col"]<- (outIdxAccum+chIdx)
		ioAddr$zDC <- chIdx
		funLst[[ ioAddr$outLst[[1]]["col"] ]] <- cF.rawDummy( ioAddr )
	}

	outIdxAccum <- outIdxAccum + chunk
	chunk <- 6
	for( chIdx in 1:chunk ){
		ioAddr$inLst[[1]]["col"] <- chIdx
		ioAddr$outLst[[1]]["col"]<- (outIdxAccum+chIdx)
		ioAddr$zDC <- chIdx
		funLst[[ ioAddr$outLst[[1]]["col"] ]] <- cF.quotient( ioAddr ,pBase=5 )
	}

	outIdxAccum <- outIdxAccum + chunk
	chunk <- 6
	for( chIdx in 1:chunk ){
		ioAddr$inLst[[1]]["col"] <- chIdx
		ioAddr$outLst[[1]]["col"]<- (outIdxAccum+chIdx)
		ioAddr$zDC <- chIdx
		funLst[[ ioAddr$outLst[[1]]["col"] ]] <- cF.remainder( ioAddr ,pBase=5 )
	}

	if( pDev )
		return( funLst )

	outIdxAccum <- outIdxAccum + chunk
	chunk <- 6
	for( chIdx in 1:chunk ){
		ioAddr$inLst[[1]]["col"] <- chIdx
		ioAddr$outLst[[1]]["col"]<- (outIdxAccum+chIdx)
		ioAddr$zDC <- chIdx
		funLst[[ ioAddr$outLst[[1]]["col"] ]] <- cF.pastDiff( ioAddr ,pHSize=1 ,pMode="ltgt" )
	}

	outIdxAccum <- outIdxAccum + chunk
	chunk <- 6
	for( chIdx in 1:chunk ){
		ioAddr$inLst[[1]]["col"] <- chIdx
		ioAddr$outLst[[1]]["col"]<- (outIdxAccum+chIdx)
		ioAddr$zDC <- chIdx
		funLst[[ ioAddr$outLst[[1]]["col"] ]] <- cF.pastDiff( ioAddr ,pHSize=1 ,pMode="abs" )
	}

	outIdxAccum <- outIdxAccum + chunk
	chunk <- 6
	for( chIdx in 1:chunk ){
		ioAddr$inLst[[1]]["col"] <- chIdx
		ioAddr$outLst[[1]]["col"]<- (outIdxAccum+chIdx)
		ioAddr$zDC <- chIdx
		funLst[[ ioAddr$outLst[[1]]["col"] ]] <- cF.pastDiff( ioAddr ,pHSize=1 )
	}

	outIdxAccum <- outIdxAccum + chunk
	chunk <- 6
	for( chIdx in 1:chunk ){
		ioAddr$inLst[[1]]["col"] <- chIdx
		ioAddr$outLst[[1]]["col"]<- (outIdxAccum+chIdx)
		ioAddr$zDC <- chIdx
		funLst[[ ioAddr$outLst[[1]]["col"] ]] <- cF.remainder( ioAddr ,pBase=2 )
	}

	outIdxAccum <- outIdxAccum + chunk
	chunk <- 6
	for( chIdx in 1:chunk ){
		ioAddr$inLst[[1]]["col"] <- chIdx
		ioAddr$outLst[[1]]["col"]<- (outIdxAccum+chIdx)
		ioAddr$zDC <- chIdx
		funLst[[ ioAddr$outLst[[1]]["col"] ]] <- cF.pastDiff( ioAddr ,pHSize=3 ,pMode="abs" )
	}

	outIdxAccum <- outIdxAccum + chunk
	chunk <- 6
	for( chIdx in 1:chunk ){
		ioAddr$inLst[[1]]["col"] <- chIdx
		ioAddr$outLst[[1]]["col"]<- (outIdxAccum+chIdx)
		ioAddr$zDC <- chIdx
		funLst[[ ioAddr$outLst[[1]]["col"] ]] <- cF.pastDiff( ioAddr ,pHSize=5 ,pMode="abs" )
	}

	outIdxAccum <- outIdxAccum + chunk
	chunk <- 6
	for( chIdx in 1:chunk ){
		ioAddr$inLst[[1]]["col"] <- chIdx
		ioAddr$outLst[[1]]["col"]<- (outIdxAccum+chIdx)
		ioAddr$zDC <- chIdx
		funLst[[ ioAddr$outLst[[1]]["col"] ]] <- cF.pastDiff( ioAddr ,pHSize=7 ,pMode="abs" )
	}

	return( funLst )
} # get1stCreateFunSet( )

#	2 세대 elementSet을 만들기 위한 함수들
#		1세대 Set에 대한 2차 가공결과이며, 역시 Column간 의존관계는 없어야 한다.
#		1,2세대 Set을 가지고 Raw Column에서의 Code 별 성적을 계산할 수 있어야 한다.
get2ndCreateFunSet <- function( pZh ,pFunIdLst ,pFunGIdLst ,pDev=F ){

	eleGenNum <- 2
	zEALst <- list()
	for( gIdx in 1:(eleGenNum-1) )
		zEALst[[gIdx]] <- integer(0)

	ioAddr <- getIoAddr( c(1,0) ,c(eleGenNum,0) ,NULL ,zEALst )
	funLst <- list()
	outIdxAccum <- 0
	chunk <- 0

	outIdxAccum <- outIdxAccum + chunk
	inEleIdx <- 1   # input element index
	inColIdx <- which(pFunIdLst[[inEleIdx]]=="cF.remainder_B5") # input Column Idx
	chunk <- length(inColIdx)
	for( chIdx in seq_len(chunk) ){
		ioAddr$inLst[[1]]["col"] <- inColIdx[chIdx]
		ioAddr$outLst[[1]]["col"]<- (outIdxAccum+chIdx)
		funLst[[ ioAddr$outLst[[1]]["col"] ]] <- cF.seqAccum( ioAddr )
	}

	if( pDev )
		return( funLst )

	outIdxAccum <- outIdxAccum + chunk
	inEleIdx <- 1   # input element index
	inColIdx <- which(pFunIdLst[[inEleIdx]]=="cF.quotient_B5") # input Column Idx
	chunk <- length(inColIdx)
	for( chIdx in seq_len(chunk) ){
		ioAddr$inLst[[1]]["col"] <- inColIdx[chIdx]
		ioAddr$outLst[[1]]["col"]<- (outIdxAccum+chIdx)
		funLst[[ ioAddr$outLst[[1]]["col"] ]] <- cF.seqAccum( ioAddr )
			# QQE:결과 확인
	}

	outIdxAccum <- outIdxAccum + chunk
	inEleIdx <- 1   # input element index
	inColIdx <- which(pFunIdLst[[inEleIdx]]=="cF.remainder_B2") # input Column Idx
	chunk <- length(inColIdx)
	for( chIdx in seq_len(chunk) ){
		ioAddr$inLst[[1]]["col"] <- inColIdx[chIdx]
		ioAddr$outLst[[1]]["col"]<- (outIdxAccum+chIdx)
		funLst[[ ioAddr$outLst[[1]]["col"] ]] <- cF.seqAccum( ioAddr )
			# QQE:결과 확인
	}

	outIdxAccum <- outIdxAccum + chunk
	inEleIdx <- 1   # input element index
	inColIdx <- which(pFunIdLst[[inEleIdx]]=="cF.pastDiff_H1Mabs") # input Column Idx
	chunk <- length(inColIdx)
	for( chIdx in seq_len(chunk) ){
		ioAddr$inLst[[1]]["col"] <- inColIdx[chIdx]
		ioAddr$outLst[[1]]["col"]<- (outIdxAccum+chIdx)
		funLst[[ ioAddr$outLst[[1]]["col"] ]] <- cF.remainder( ioAddr ,pBase=5 ) 
			# QQE:zDC가 아닌 현재 col 사용토록.
	}

	outIdxAccum <- outIdxAccum + chunk
	inEleIdx <- 1   # input element index
	inColIdx <- which(pFunIdLst[[inEleIdx]]=="cF.remainder_B2") # input Column Idx
	chunk <- length(inColIdx)
	for( chIdx in seq_len(chunk) ){
		ioAddr$inLst[[1]]["col"] <- inColIdx[chIdx]
		ioAddr$outLst[[1]]["col"]<- (outIdxAccum+chIdx)
		funLst[[ ioAddr$outLst[[1]]["col"] ]] <- cF.remainder( ioAddr ,pBase=5 ) 
			# QQE:zDC가 아닌 현재 col 사용토록.
	}

	outIdxAccum <- outIdxAccum + chunk
	inEleIdx <- 1   # input element index
	inColIdx <- which(pFunIdLst[[inEleIdx]]=="cF.pastDiff_H3Mabs") # input Column Idx
	chunk <- length(inColIdx)
	for( chIdx in seq_len(chunk) ){
		ioAddr$inLst[[1]]["col"] <- inColIdx[chIdx]
		ioAddr$outLst[[1]]["col"]<- (outIdxAccum+chIdx)
		funLst[[ ioAddr$outLst[[1]]["col"] ]] <- cF.remainder( ioAddr ,pBase=5 ) 
			# QQE:zDC가 아닌 현재 col 사용토록.
	}

	outIdxAccum <- outIdxAccum + chunk
	inEleIdx <- 1   # input element index
	inColIdx <- which(pFunIdLst[[inEleIdx]]=="cF.pastDiff_H5Mabs") # input Column Idx
	chunk <- length(inColIdx)
	for( chIdx in seq_len(chunk) ){
		ioAddr$inLst[[1]]["col"] <- inColIdx[chIdx]
		ioAddr$outLst[[1]]["col"]<- (outIdxAccum+chIdx)
		funLst[[ ioAddr$outLst[[1]]["col"] ]] <- cF.remainder( ioAddr ,pBase=5 ) 
			# QQE:zDC가 아닌 현재 col 사용토록.
	}

	outIdxAccum <- outIdxAccum + chunk
	inEleIdx <- 1   # input element index
	inColIdx <- which(pFunIdLst[[inEleIdx]]=="cF.pastDiff_H7Mabs") # input Column Idx
	chunk <- length(inColIdx)
	for( chIdx in seq_len(chunk) ){
		ioAddr$inLst[[1]]["col"] <- inColIdx[chIdx]
		ioAddr$outLst[[1]]["col"]<- (outIdxAccum+chIdx)
		funLst[[ ioAddr$outLst[[1]]["col"] ]] <- cF.remainder( ioAddr ,pBase=5 ) 
			# QQE:zDC가 아닌 현재 col 사용토록.
	}

	return( funLst )

} # get2ndCreateFunSet( )

#	3 세대 : 1,2세대에 대한 Column간 관계.(가공없이 측정값만 도출)
get3rdCreateFunSet <- function( pZh ,pFunIdLst ,pFunGIdLst ,pDev=F ){

	generationNumber <- 3
	zEALst <- list()
	for( gIdx in 1:(generationNumber-1) ){
		zEALst[[gIdx]] <- c(NA,NA)
		names( zEALst[[gIdx]] ) <- c("ele","col")
	}

	ioAddr <- getIoAddr( c(1,0) ,c(generationNumber,0) ,NULL ,zEALst )
	funLst <- list()
	outIdxAccum <- 0
	chunk <- 0

	outIdxAccum <- outIdxAccum + chunk
	inEleIdx <- 1   # input element index
	inColIdx <- which(pFunIdLst[[inEleIdx]]=="cF.remainder_B5") # input Column Idx
	baseIdx <- 1
	if( baseIdx < length(inColIdx) ){
		ioAddr$zEALst[[1]]["ele"] <- inEleIdx
		ioAddr$zEALst[[1]]["col"] <- inColIdx[baseIdx]
		inColIdx <- inColIdx[-(1:baseIdx)] # 기준점과 이미 계산된 부분을 제외.
		chunk <- length(inColIdx)
		for( chIdx in (1:chunk) ){ 
			ioAddr$inLst[[1]]["col"] <- inColIdx[chIdx]
			ioAddr$outLst[[1]]["col"]<- (outIdxAccum+chIdx)
			funLst[[ ioAddr$outLst[[1]]["col"] ]] <- cF.pastColDiff( ioAddr ,pHSize=0 ,pMode=NULL ,pFGIdStr=baseIdx )
		}
	}

	outIdxAccum <- outIdxAccum + chunk
	inEleIdx <- 1   # input element index
	inColIdx <- which(pFunIdLst[[inEleIdx]]=="cF.remainder_B5") # input Column Idx
	baseIdx <- 2
	if( baseIdx < length(inColIdx) ){
		ioAddr$zEALst[[1]]["ele"] <- inEleIdx
		ioAddr$zEALst[[1]]["col"] <- inColIdx[baseIdx]
		inColIdx <- inColIdx[-(1:baseIdx)] # 기준점과 이미 계산된 부분을 제외.
		chunk <- length(inColIdx)
		for( chIdx in (1:chunk) ){ 
			ioAddr$inLst[[1]]["col"] <- inColIdx[chIdx]
			ioAddr$outLst[[1]]["col"]<- (outIdxAccum+chIdx)
			funLst[[ ioAddr$outLst[[1]]["col"] ]] <- cF.pastColDiff( ioAddr ,pHSize=0 ,pMode=NULL ,pFGIdStr=baseIdx )
		}
	}

	outIdxAccum <- outIdxAccum + chunk
	inEleIdx <- 1   # input element index
	inColIdx <- which(pFunIdLst[[inEleIdx]]=="cF.remainder_B5") # input Column Idx
	baseIdx <- 3
	if( baseIdx < length(inColIdx) ){
		ioAddr$zEALst[[1]]["ele"] <- inEleIdx
		ioAddr$zEALst[[1]]["col"] <- inColIdx[baseIdx]
		inColIdx <- inColIdx[-(1:baseIdx)] # 기준점과 이미 계산된 부분을 제외.
		chunk <- length(inColIdx)
		for( chIdx in (1:chunk) ){ 
			ioAddr$inLst[[1]]["col"] <- inColIdx[chIdx]
			ioAddr$outLst[[1]]["col"]<- (outIdxAccum+chIdx)
			funLst[[ ioAddr$outLst[[1]]["col"] ]] <- cF.pastColDiff( ioAddr ,pHSize=0 ,pMode=NULL ,pFGIdStr=baseIdx )
		}
	}

	if( pDev )
		return( funLst )

	# cF.rawDummy 에 대한 pHSize=0 일때의 순차 스텝
	# cF.quotient_B5 에 대한 pHSize=0 일때의 순차 스텝
	# cF.pastDiff_H1Mltgt
	# ... 모두 해 버리자.

	# 2nd ele cF.seqAccum에 대한 pHSize=0 도 처리.

	# "cF.remainder_B5" 에 대한 cF.pastColDiff() 순차/백스텝
	#			pHSize <- 1:7

	# 2개 컬럼간의 합. cF.quotient_B5, cF.remainder_B5
	# 이것도 pHSize 에 따라 변칙적용하자.

	return( funLst )
} # get3rdCreateFunSet( )

#	4 세대 : 3세대에 대한 1차 연산
get4thCreateFunSet <- function( pZh ,pFunIdLst ,pFunGIdLst ,pDev=F ){

	generationNumber <- 4
	zEALst <- list()
	for( gIdx in 1:(generationNumber-1) )
		zEALst[[gIdx]] <- c(NA,NA)

	ioAddr <- getIoAddr( c(1,0) ,c(generationNumber,0) ,NULL ,zEALst )
	funLst <- list()
	outIdxAccum <- 0
	chunk <- 0

	outIdxAccum <- 0
	chunk <- 6
	for( chIdx in 1:chunk ){
		# ioAddr$inLst[[1]]["col"] <- chIdx
		# ioAddr$outLst[[1]]["col"]<- (outIdxAccum+chIdx)
		# ioAddr$zDC <- chIdx
		# funLst[[ ioAddr$outLst[[1]]["col"] ]] <- cF.rawDummy( ioAddr )
	}

	if( pDev )
		return( funLst )

	return( funLst )

} # get4thCreateFunSet( )

#	5 세대 : 1~4 세대에 대한 컬럼간 연산
get5thCreateFunSet <- function( pZh ,pFunIdLst ,pFunGIdLst ,pDev=F ){

	generationNumber <- 5
	zEALst <- list()
	for( gIdx in 1:(generationNumber-1) )
		zEALst[[gIdx]] <- c(NA,NA)

	ioAddr <- getIoAddr( c(1,0) ,c(generationNumber,0) ,NULL ,zEALst )
	funLst <- list()
	outIdxAccum <- 0
	chunk <- 0

	outIdxAccum <- 0
	chunk <- 6
	for( chIdx in 1:chunk ){
		# ioAddr$inLst[[1]]["col"] <- chIdx
		# ioAddr$outLst[[1]]["col"]<- (outIdxAccum+chIdx)
		# ioAddr$zDC <- chIdx
		# funLst[[ ioAddr$outLst[[1]]["col"] ]] <- cF.rawDummy( ioAddr )
	}

	if( pDev )
		return( funLst )

	return( funLst )

} # get5thCreateFunSet( )

#	6 세대 : 5세대에 대한 1차 연산
get6thCreateFunSet <- function( pZh ,pFunIdLst ,pFunGIdLst ,pDev=F ){

	generationNumber <- 6
	zEALst <- list()
	for( gIdx in 1:(generationNumber-1) )
		zEALst[[gIdx]] <- c(NA,NA)

	ioAddr <- getIoAddr( c(1,0) ,c(generationNumber,0) ,NULL ,zEALst )
	funLst <- list()
	outIdxAccum <- 0
	chunk <- 0

	outIdxAccum <- 0
	chunk <- 6
	for( chIdx in 1:chunk ){
		# ioAddr$inLst[[1]]["col"] <- chIdx
		# ioAddr$outLst[[1]]["col"]<- (outIdxAccum+chIdx)
		# ioAddr$zDC <- chIdx
		# funLst[[ ioAddr$outLst[[1]]["col"] ]] <- cF.rawDummy( ioAddr )
	}

	if( pDev )
		return( funLst )

	return( funLst )

} # get6thCreateFunSet( )

# =========================================================================================
# 한정하기 어려워 규모가 큰 CodeVal을 정의
#	메모리 절약을 위해 참조형태가 되도록 하기 위해서임.
codeVal.DnaMinMax	<- -max(FB$dnaType):max(FB$dnaType)
codeVal.accumMax	<- 1000
codeVal.accumRng0	<- 0:codeVal.accumMax
codeVal.accumRng1	<- 1:codeVal.accumMax


# =========================================================================================
#	cF.Funcitons()
# =========================================================================================

# 서로 다른 2개 컬럼간의 차이.(주로 3세대)
#	pIoAddr : inLst는 2개를 갖는다. pHSize는 inLst[[2]]에 대해 적용.
#	pMode : NULL(가공없음) ,"ltgt"(크면 1, 작으면0) ,"abs"(차이값의 절대값)
cF.pastColDiff <- function( pIoAddr ,pHSize=1 ,pFGIdStr="" ,pCodeVal=NULL ,pMode=NULL ){
	rObj <- list( idStr=sprintf("cF.pastColDiff_H%dM%s",pHSize,ifelse(is.null(pMode),"",pMode) )
					,ioAddr=pIoAddr	,fGIdStr=pFGIdStr
					,optStr=sprintf("H:%d Mode:%s",pHSize,ifelse(is.null(pMode),"",pMode))
				)
	rObj$inAddr <- rObj$ioAddr$inLst[[1]]
	rObj$zEA	<- rObj$ioAddr$zEALst[[1]]
	rObj$hSize	<- pHSize # hSize가 0이면 자기 자신, 즉 bornEleLst 내에서 차이 계산.
	rObj$initNA <- ifelse( is.null(rObj$hSize) ,NA ,rObj$hSize ) # seqAnaFun() 들에게 불용구간 정보를 전달하기 위한 변수.
	
	rObj$codeValNA	<- NA
	if( is.null(pCodeVal) ){
		if( is.null(pMode) ){
			rObj$codeVal<- c( codeVal.DnaMinMax ,rObj$codeValNA)
		} else if( "abs"==pMode ){
			rObj$codeVal<- c( 0:max(FB$dnaType) ,rObj$codeValNA)
		} else if( "ltgt"==pMode ){
			rObj$codeVal<- c( 0:1 ,rObj$codeValNA)
		}
	} else {
		rObj$codeVal<- c(pCodeVal,rObj$codeValNA)
	}
	rObj$codeValNA.idx <- which(rObj$codeVal==rObj$codeValNA)

	if( is.null(pMode) ){
		rObj$cModify <- function(p){ p }
	} else if( "abs"==pMode ){
		rObj$cModify <- function(p){ abs(p) }
	} else if( "ltgt"==pMode ){
		rObj$cModify <- function(p){ ifelse(p>0,1,0) }
	}
	
	rObj$output <- function( pEleSet ,pZoid ,pBornEleLst ){

			outVal <- pBornEleLst[[ rObj$zEA["ele"] ]][ rObj$zEA["col"] ]
			if( 0<rObj$hSize ){
				workMtx <- pEleSet$eleLst[[ rObj$inAddr["ele"] ]]$mtx
				if( !is.null(pEleSet$rowSpan) ){
					workMtx <- workMtx[pEleSet$rowSpan,,drop=F]
				}

				nSize <- nrow(workMtx) 
				if( nSize==0 || rObj$hSize>nSize ){
					# check1 : rowSpan 지정으로 인해 nrow() 가 0이 될 수도 있음.
					# check2 : 현재 pZoid를 기준으로 과거계산이므로.
					return( rObj$codeValNA )
				}

				outVal <- outVal - workMtx[ (nSize-(rObj$hSize-1)) ,rObj$inAddr["col"] ]
			} else {
				# 경고!!
				#	이 시점에서 rObj$inAddr["ele"]가 가리키는 Element 차원은 
				#	pBornEleLst 내에 이미 존재해야 한다. 
				#	즉, 현재 차원보다 이전 차원만이 사용 가능 함.
				if( length(pBornEleLst)<rObj$inAddr["ele"] ) 
					k.FLogStr("Cretical Error in cF.pastColDiff. 2017.10.31",pConsole=T)

				outVal <- outVal - pBornEleLst[[ rObj$inAddr["ele"] ]][ rObj$inAddr["col"] ]
			}

			outVal <- rObj$cModify(outVal)
			# 연산 대상이 NA라고 해도, outVal이 NA이고 codeVal 내에 NA가 포함되어 있으므로 문제 없음.
			if( outVal%in%rObj$codeVal )
				return( outVal )
			else
				return( rObj$codeValNA )
		}

	return( rObj )
} # cF.pastColDiff()

# 동일 컬럼 내 연속 누적 수.
# 2세대 이상의 ElementSet 에서 사용.
cF.seqAccum <- function( pIoAddr ,pMaxAccum=codeVal.accumMax ,pFGIdStr="" ,pCodeVal=NULL ){
	rObj <- list( idStr=sprintf("cF.seqAccum")
					,ioAddr=pIoAddr	,fGIdStr=pFGIdStr
					,optStr=""
				)
	rObj$inAddr <- rObj$ioAddr$inLst[[1]] # 어차피 얘는 input이 하나 뿐인지라.
	rObj$codeValNA	<- -1
	rObj$maxAccum <- pMaxAccum
	rObj$initNA <- NA # seqAnaFun() 들에게 불용구간 정보를 전달하기 위한 변수.

	if( is.null(pCodeVal) ){
		rObj$codeVal<- c( codeVal.accumRng1 ,rObj$codeValNA)
			# 현재 자기 자신부터 카운트 하기로 한다.
			# 즉 이전 값이 자신과 다르면 1	
	} else {
		rObj$codeVal<- c(pCodeVal,rObj$codeValNA)
	}
	rObj$codeValNA.idx <- which(rObj$codeVal==rObj$codeValNA)

	rObj$output <- function( pEleSet ,pZoid ,pBornEleLst ){

			workMtx <- pEleSet$eleLst[[ rObj$inAddr["ele"] ]]$mtx
			if( !is.null(pEleSet$rowSpan) ){
				workMtx <- workMtx[pEleSet$rowSpan,,drop=F]
			}

			nSize <- nrow(workMtx)
			if( nSize==0 )
				return( 1 )

			his <- workMtx[,rObj$inAddr["col"]]
			curVal <- pBornEleLst[[ rObj$inAddr["ele"] ]][ rObj$inAddr["col"] ]
			matchFlag <- 	if( is.na(curVal) ){ is.na(his)
							} else { 
								flag <- his==curVal
								flag <- ifelse( is.na(flag) ,F ,flag )
							}
			fIndices <- which(!matchFlag)
			seqNum <-	if( 0==length(fIndices) ){
							1 + nSize # 태초부터 지금까티 동일한 값.
						} else {
							1 + ( nSize-max(fIndices) )
						}

			outVal <- ifelse( seqNum>rObj$maxAccum ,rObj$maxAccum ,seqNum )
			return( outVal ) # codeValNA 검사는 MaxAccum 체크로 대체
		}

	return( rObj )
} # cF.seqAccum


# 과거와의 차이.(Zoid DNA 한정)
#	pMode : NULL(가공없음) ,"ltgt"(크면 1, 작으면0) ,"abs"(차이값의 절대값)
cF.pastDiff <- function( pIoAddr ,pHSize=1 ,pFGIdStr="" ,pCodeVal=NULL ,pMode=NULL ){
	rObj <- list( idStr=sprintf("cF.pastDiff_H%dM%s",pHSize,ifelse(is.null(pMode),"",pMode) )
					,ioAddr=pIoAddr	,fGIdStr=pFGIdStr
					,optStr=sprintf("H:%d Mode:%s",pHSize,ifelse(is.null(pMode),"",pMode))
				)
	rObj$inAddr <- rObj$ioAddr$inLst[[1]] # 어차피 얘는 input이 하나 뿐인지라.
	rObj$zDC	<- rObj$ioAddr$zDC
	rObj$hSize	<- pHSize
	rObj$initNA <- ifelse( is.null(rObj$hSize) ,NA ,rObj$hSize ) # seqAnaFun() 들에게 불용구간 정보를 전달하기 위한 변수.

	rObj$codeValNA	<- NA
	if( is.null(pCodeVal) ){
		if( is.null(pMode) ){
			rObj$codeVal<- c( -max(FB$dnaType):max(FB$dnaType) ,rObj$codeValNA)
		} else if( "abs"==pMode ){
			rObj$codeVal<- c( 0:max(FB$dnaType) ,rObj$codeValNA)
		} else if( "ltgt"==pMode ){
			rObj$codeVal<- c( 0:1 ,rObj$codeValNA)
		}
	} else {
		rObj$codeVal<- c(pCodeVal,rObj$codeValNA)
	}
	rObj$codeValNA.idx <- which(rObj$codeVal==rObj$codeValNA)

	if( is.null(pMode) ){
		rObj$cModify <- function(p){ p }
	} else if( "abs"==pMode ){
		rObj$cModify <- function(p){ abs(p) }
	} else if( "ltgt"==pMode ){
		rObj$cModify <- function(p){ ifelse(p>0,1,0) }
	}

	
	rObj$output <- function( pEleSet ,pZoid ,pBornEleLst=NULL ){

			workMtx <- pEleSet$eleLst[[ rObj$inAddr["ele"] ]]$mtx
			if( !is.null(pEleSet$rowSpan) ){
				workMtx <- workMtx[pEleSet$rowSpan,,drop=F]
			}

			nSize <- nrow(workMtx) 
			if( nSize==0 || rObj$hSize>nSize ){
				# check1 : rowSpan 지정으로 인해 nrow() 가 0이 될 수도 있음.
				# check2 : 현재 pZoid를 기준으로 과거계산이므로.
				return( rObj$codeValNA )
			}
			
			outVal <- pZoid[ rObj$zDC ] - workMtx[ (nSize-(rObj$hSize-1)) ,rObj$inAddr["col"] ]
			outVal <- rObj$cModify(outVal)
			# 연산 대상이 NA라고 해도, outVal이 NA이고 codeVal 내에 NA가 포함되어 있으므로 문제 없음.
			if( outVal%in%rObj$codeVal )
				return( outVal )
			else
				return( rObj$codeValNA )
		}
	return( rObj )
	
} # cF.pastDiff()


cF.remainder <- function( pIoAddr ,pBase ,pFGIdStr="" ,pCodeVal=NULL ){
	rObj <- list( idStr=sprintf("cF.remainder_B%d",pBase) 
					,ioAddr=pIoAddr	,fGIdStr=pFGIdStr
					,optStr=sprintf("Base:%d",pBase)
				)
	rObj$inAddr <- rObj$ioAddr$inLst[[1]] # 어차피 얘는 input이 하나 뿐인지라.
	rObj$zDC	<- rObj$ioAddr$zDC
	rObj$base	<- pBase
	rObj$initNA <- NA # seqAnaFun() 들에게 불용구간 정보를 전달하기 위한 변수.
	
	rObj$codeValNA	<- -1
	if( is.null(pCodeVal) ){
		rObj$codeVal<- c( 0:(pBase-1) ,rObj$codeValNA)
	} else {
		rObj$codeVal<- c(pCodeVal,rObj$codeValNA)
	}
	rObj$codeValNA.idx <- which(rObj$codeVal==rObj$codeValNA)

	rObj$output <- function( pEleSet ,pZoid ,pBornEleLst=NULL ){
			outVal <- pZoid[rObj$zDC] %% rObj$base 
			if( outVal%in%rObj$codeVal )
				return( outVal )
			else
				return( rObj$codeValNA )
		}
	return( rObj )
} # cF.remainder()


cF.quotient <- function( pIoAddr ,pBase ,pFGIdStr="" ,pCodeVal=NULL ){
	rObj <- list( idStr=sprintf("cF.quotient_B%d",pBase)
					,ioAddr=pIoAddr	,fGIdStr=pFGIdStr
					,optStr=sprintf("Base:%d",pBase)
				)
	rObj$inAddr <- rObj$ioAddr$inLst[[1]] # 어차피 얘는 input이 하나 뿐인지라.
	rObj$zDC	<- rObj$ioAddr$zDC
	rObj$base	<- pBase
	rObj$initNA <- NA # seqAnaFun() 들에게 불용구간 정보를 전달하기 위한 변수.
	
	rObj$codeValNA	<- -1
	if( is.null(pCodeVal) ){
		maxQuotient <- max(FB$dnaType) %/% rObj$base
		rObj$codeVal<- c(0:maxQuotient,rObj$codeValNA)
	} else {
		rObj$codeVal<- c(pCodeVal,rObj$codeValNA)
	}
	rObj$codeValNA.idx <- which(rObj$codeVal==rObj$codeValNA)

	rObj$output <- function( pEleSet ,pZoid ,pBornEleLst=NULL ){
			outVal <- pZoid[rObj$zDC] %/% rObj$base 
			if( outVal%in%rObj$codeVal )
				return( outVal )
			else
				return( rObj$codeValNA )
		}
	return( rObj )
} # cF.quotient()


# 그냥 Raw DNA를 그대로 옮기는 더미함수
#	- pIoAddr : 
#			names(ioAddr) = c("inEle","inCol","outEle","outCol")
cF.rawDummy <- function( pIoAddr ,pFGIdStr="" ,pCodeVal=NULL ){
	rObj <- list( idStr=sprintf("cF.rawDummy") 
					,ioAddr=pIoAddr	,fGIdStr=pFGIdStr
					,optStr=""
				)
	rObj$inAddr <- rObj$ioAddr$inLst[[1]] # 어차피 얘는 input이 하나 뿐인지라.
	rObj$zDC	<- rObj$ioAddr$zDC
	rObj$initNA <- NA # seqAnaFun() 들에게 불용구간 정보를 전달하기 위한 변수.
	
	rObj$codeValNA	<- -1
	if( is.null(pCodeVal) ){
		rObj$codeVal<- c(FB$dnaType,rObj$codeValNA)
	} else {
		rObj$codeVal<- c(pCodeVal,rObj$codeValNA)
	}
	rObj$codeValNA.idx <- which(rObj$codeVal==rObj$codeValNA)

	rObj$output <- function( pEleSet ,pZoid ,pBornEleLst=NULL ){
			outVal <- pZoid[rObj$zDC]
			if( outVal%in%rObj$codeVal )
				return( outVal )
			else
				return( rObj$codeValNA )
		}
	return( rObj )
} # cF.rawDummy()





