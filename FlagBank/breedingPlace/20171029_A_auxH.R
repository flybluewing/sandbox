# 20171029_A_auxH.R


# 생성함수 정보를 excel로 저장.
#	의존 : require("XLConnect")
#	pCFS<-creFunSet		;pXlsFile<-"Xls_creFunSet.xls"
saveCreFunSetInfo <- function( pCFS ,pXlsFile ){

	if( file.exists(pXlsFile) )
		file.remove(pXlsFile)

	xlObj <- loadWorkbook( pXlsFile ,create=T )

	dfLst <- list()
	for( eIdx in seq_len(length(pCFS)) ){

		eleName <- sprintf("%d_%s",eIdx,names(pCFS)[eIdx])
		dataSize <- length(pCFS[[eIdx]])
		
		cDF <- NULL
		if( dataSize > 0 ){
			cDF <- data.frame( idStr="" ,gIdStr="" ,optStr=""
								,ioAddr.in="" ,ioAddr.out="" ,ioAddr.zDC="" ,ioAddr.zEA=""
								,codeValLen=rep(0,dataSize) 
								,stringsAsFactors=F
							)
		} else {
			cDF <- data.frame( idStr="NA" ,gIdStr="NA"	,optStr="NA"
								,ioAddr.in="NA" ,ioAddr.out="NA" ,ioAddr.zDC="NA" ,ioAddr.zEA="NA"
								,codeValLen="NA"
								,stringsAsFactors=F
							)
		}

		for( cIdx in seq_len(length(pCFS[[eIdx]])) ){
			curFun <- pCFS[[eIdx]][[cIdx]]
			cDF$idStr[cIdx]			<- curFun$idStr
			cDF$gIdStr[cIdx]		<- curFun$fGIdStr
			cDF$optStr[cIdx]		<- curFun$optStr

			addrStr <- sapply( curFun$ioAddr$inLst ,function(p){sprintf("(%d,%d)",p[1],p[2])} )
			cDF$ioAddr.in[cIdx]		<- paste( addrStr ,collapse="," )
			addrStr <- sapply( curFun$ioAddr$outLst ,function(p){sprintf("(%d,%d)",p[1],p[2])} )
			cDF$ioAddr.out[cIdx]	<- paste( addrStr ,collapse="," )

			addrStr <- sapply( curFun$ioAddr$zEALst ,function(p){sprintf("(%d,%d)",p[1],p[2])} )
			cDF$ioAddr.zEA[cIdx]	<- paste( addrStr ,collapse="," )

			cDF$ioAddr.zDC[cIdx]	<- paste(curFun$ioAddr$zDC,collapse=",")

			cDF$codeValLen[cIdx]	<- length(curFun$codeVal)
		} # cIdx

		createSheet( xlObj ,eleName )
		writeWorksheet( xlObj ,sheet=eleName ,cDF )
		dfLst[[ 1+length(dfLst) ]] <- cDF
	} # eIdx

	saveWorkbook( xlObj )

} # saveCreFunSetInfo()


# analyzeSeq() 함수 결과물의 동일여부 확인
# 	pSet1 <- hAnaSet	;pSet2 <- hAnaSet
checkSame.hAnaSet <- function( pSet1 ,pSet2 ){

	hLst.len <- length(pSet1$hLst)
	if( hLst.len != length(pSet2$hLst) ){
		return(sprintf("$hLst length"))
	}

	for( hIdx in seq_len(hLst.len) ){
		if( pSet1$hLst[[hIdx]]$stdH != pSet2$hLst[[hIdx]]$stdH ){
			return(sprintf("hIdx:%3d stdH",hIdx))
		}
		
		ele.len <- length(pSet1$hLst[[hIdx]]$anaLst)
		if( ele.len != length(pSet2$hLst[[hIdx]]$anaLst) ){
			return(sprintf("hIdx:%3d ele.len",hIdx))
		}

		for( eIdx in seq_len(ele.len) ){
			col.len <- length(pSet1$hLst[[hIdx]]$anaLst[[eIdx]])
			if( col.len != length(pSet2$hLst[[hIdx]]$anaLst[[eIdx]]) ){
				return(sprintf("hIdx:%3d eIdx:%3d col.len",hIdx,eIdx))
			}
			anaObjLst1 <- pSet1$hLst[[hIdx]]$anaLst[[eIdx]]
			anaObjLst2 <- pSet2$hLst[[hIdx]]$anaLst[[eIdx]]
			for( cIdx in seq_len(col.len) ){
				cRst <- all.equal( anaObjLst1[[cIdx]] ,anaObjLst2[[cIdx]] )
				if( "logical"!=class(cRst) || !cRst ){
					return(sprintf("hIdx:%3d eIdx:%3d cIdx:%3d anaObj",hIdx,eIdx,cIdx))
				}
			}
		} # for(eIdx)

	} # for(hIdx)

	return("all same.")
}

