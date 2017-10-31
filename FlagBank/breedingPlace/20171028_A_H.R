# 

getRandomZoidPool <- function(){

	zoidLst <- list()
	codeVal <- c( 0:9 )
	codeStr <- sprintf("%d",codeVal)
	for( fIdx in codeStr ){ # first index
		for( sIdx in codeStr ){ # second index
			for( tIdx in codeStr ){ # third index
				# zObj : dna, score, 기타 속성들...
				zoidLst[[sprintf("%s%s%s",fIdx,sIdx,tIdx)]] <- list()
			}
		}
	}

	rObj <- list( zoidLst=zoidLst )
	rObj$same <- function( pZoid1 ,pZoid2 ){
				# 나중에 NA등을 지원해야 할 가능성을 위해서.
				return( all(pZoid1==pZoid2) )
			}

	return( rObj )
} # getRandomZoidPool()

# RZP.G <- getRandomZoidPool()	# Random Zoid Pool - Global

#--------------------------------------------------------------------
# RZP.G 가 이미 존재한다는 가정하에 사용
RZP.addZoid <- function( pZoid ){

	keyStr <- paste(pZoid[1:3]%%10,collapse="")

	zLst <- RZP.G$zoidLst[[keyStr]]
	if( 0==length(zLst) ){
		RZP.G$zoidLst[[keyStr]][[1]] <<- list( dna=pZoid )
	} else {
		for( idx in 1:length(zLst) ){
			if( RZP.G$same(zLst[[idx]]$dna ,pZoid) )
				return( FALSE ) # 추가될 필요 없으므로.
		}
		RZP.G$zoidLst[[keyStr]][[ (1+length(zLst)) ]] <<- list( dna=pZoid )
	}
	return( TRUE )
} # RZP.addZoid()

RZP.size <- function(){
	return( sum(sapply(RZP.G$zoidLst,length)) )
} # RZP.size()



