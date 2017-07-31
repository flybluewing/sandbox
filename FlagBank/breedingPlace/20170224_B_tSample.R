#-[custFunction]-------------------------------------------------
filtF.oneIdx <- function( pMatPtn ,pIdxMtx ){ 1==nrow(pIdxMtx) }
filtF.oneRow <- function( pMatPtn ,pIdxMtx ){ rIdx <-unique(pIdxMtx[,1]); return(1>=length(rIdx)) }
filtF.lastRowMiss <- function( pMatPtn ,pIdxMtx ){
		rIdx <- unique(pIdxMtx[,1])
		return( (1>=length(rIdx)) || !(nrow(pMatPtn) %in% rIdx) )
	}
filtF.limitedRow <- function( pMatPtn ,pIdxMtx ){

		rIdx <- unique(pIdxMtx[,1])
		predNum <- sum( pIdxMtx[,1]==nrow(pMatPtn) )

		# 패턴이 없거나, 예측수가 없거나, 예측수가 전체 절반을 넘는다면.
		# || ((2*predNum)>nrow(pIdxMtx))
		return( 	1 >=length(rIdx)				# 2개 이상의 row 패턴
				||	0 == predNum					# 예측 수가 1개 이상
				||	( (2*predNum) > nrow(pIdxMtx) )	# 예측 수가 전체 절반 초과
			)
	}

buildField <- function( pFieldMtx=as.matrix(FB$zh) ){

		colN <- NULL
		vFieldMtx	<- matrix( 0 ,nrow=nrow(pFieldMtx) ,ncol=0 )
		filmMtx		<- matrix( F ,nrow=nrow(pFieldMtx) ,ncol=0 )

		# getFieldSet.dummy( ) -------------------------------------------------
		fObj <- getFieldSet.dummy( pFieldMtx=pFieldMtx )
		vFieldMtx	<- cbind( vFieldMtx	,fObj$vFieldMtx	)
		filmMtx		<- cbind( filmMtx	,fObj$film	)
		colN <- c( colN ,sprintf("%s_%02d",fObj$idStr,1:ncol(fObj$vFieldMtx)) )

		# getFieldSet.left( ) -------------------------------------------------
		fObj <- getFieldSet.left( pFieldMtx=pFieldMtx ,pBase=4 )
		#fObj <- getFieldSet.have( pFieldMtx=pFieldMtx )
		vFieldMtx	<- cbind( vFieldMtx	,fObj$vFieldMtx	)
		filmMtx		<- cbind( filmMtx	,fObj$film	)
		colN <- c( colN ,sprintf("%s_%02d",fObj$idStr,1:ncol(fObj$vFieldMtx)) )

		rObj <- list( rawFieldMtx = vFieldMtx )
		rObj$vFieldMtx <- vFieldMtx
		rObj$vFieldMtx[!filmMtx] <- NA
		rObj$filmMtx <- filmMtx
		rObj$colN <- colN
		return( rObj )
	}


#-[custFunction]-------------------------------------------------

FB <- getFlagBank()
fbObj <- buildField( pFieldMtx=as.matrix(FB$zh) )

win.height <- 3
wIdx <- 34	# 34, 42, ...
winDef <- getWindow( pField=fbObj$vFieldMtx	,pFilm=fbObj$filmMtx
						,pHeight=win.height ,pInitPos=wIdx ,pIdStr=sprintf("wObj%03d",wIdx)
					)

k.FLog(dim(winDef$film),pConsole=T,pAppend=F)


k.FLogStr("Start",pConsole=T)
rawPtn <- scanRawPattern( winDef ,pFiltF=filtF.lastRowMiss ,pBuffSize=1 ,pDebug=T )
k.FLogStr("scanRawPattern() finish.",pConsole=T)
glance.rawPtn( rawPtn )

# --------------------------------------------------------------------------------------------
# rawPtn에서 발생 횟수가 많은 매치 패턴 선별. 발생횟수가 전체적으로 낮다면 overlap() 검색
overlap <- findOverlap( rawPtn ,pFiltF=filtF.lastRowMiss ,pDebug=T )
k.FLogStr("findOverlap() finish.",pConsole=T)

ptnObjGrp <- list( winDef=winDef ,rawPtn=rawPtn ,overlap=overlap )
save( ptnObjGrp ,file="Obj_ptnObjGrp.0608.save" )

# --------------------------------------------------------------------------------------------
# 선택된 매치패턴의 최종 결과로서 나올 수 있는 경우의 수와, 경우별 발생기록 검사.
inspecIdx <- c(1,34,42)
glance.overlap( overlap ,inspecIdx )

ptnCaseLst <- list()
for( iIdx in 1:length(inspecIdx) ){
	# iIdx <- 1
	ptnObj <- createPtnObj( overlap$matchLst[[ inspecIdx[iIdx] ]]$matMtx )
	ptnCase <- scanRawPattern.ptnLst( pWin=winDef ,pPtnObj=ptnObj ,pFiltF=NULL ,pBuffSize=1 ,pDebug=T )

	ptnCaseLst[[1+length(ptnCaseLst)]] <- list( ptnObj=ptnObj ,ptnCase=ptnCase ,inspecIdx=inspecIdx[iIdx] )
	# cat(sprintf("%s\n",k.matrixToStr( ptnObj$matMtx )))
	# glance.overlap(ptnCase)
}

# --------------------------------------------------------------------------------------------
# 발생 횟수가 많은 매치 패턴 선별(일단 임의로 부여.)




excRawPtn.span <- 2:3 # 패턴 검색을 실시할 대상으로 선택된 것들.
for( idx in excRawPtn.span ){
	ptnCase	<- ptnCaseLst[[idx]]$ptnCase
	ptnObj	<- ptnCaseLst[[idx]]$ptnObj	#; ptnObj$searchDepth <- 1
	hIndice.all <- do.call( c ,lapply(ptnCase$matchLst,function(p){p$compPair}) )
	hIndice <- sort(unique(hIndices.all))
	cat(sprintf("hIndice length : %d (all:%d)\n",length(hIndice.all),length(hIndice)))
	excFilm <- !ptnObj$film
	excFilm[nrow(excFilm):(nrow(excFilm)-ptnObj$searchDepth+1),] <- T
	excRawPtn <- scanRawPattern.within( pWin=winDef ,pIndices=hIndice ,pFiltF=filtF.oneRow ,pExcFilm=excFilm ,pBuffSize=200 ,pDebug=T )
	ptnCaseLst[[idx]]$excRawPtn <- excRawPtn
}
	#	excRawPtn$matchLst[[n]]$compPair 값이 좀 큰 것들을 단서로 이용하자.
	#	만약 너무 단일 history 패턴들이라 실용성이 없다면, overlap검색을 한 번 더 해주자.
excOverlap <- findOverlap( excRawPtn ,pFiltF=NULL ,pDebug=T )
