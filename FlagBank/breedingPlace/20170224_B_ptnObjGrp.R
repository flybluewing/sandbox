
# myObj <- load("Obj_ptnObjGrp.0608.save") # 20170224_B_tSample.R

attach(ptnObjGrp)
# ------------------------------------------------------------------------------------
# overlap 선별 및
# Haunt List (hntPtnLst) 생성.
#
ol.IdxNum <- sapply(overlap$matchLst, function(p){ return(nrow(p$idxMtx)) } )
ol.preNum <- sapply(overlap$matchLst,function(p){ h<-nrow(p$matMtx) ;return( sum(!is.na(p$matMtx[h,])) ) })
ol.compNum <- sapply(overlap$matchLst,function(p){ cv<-unique(as.vector(p$compPair))  ; return(length(cv)) })
	# ol.compNum 은 rawPtn의 matchLst 인덱스임을 유의(winDef$field의 인덱스가 아니다.)
ol.compNum.r <- sapply(overlap$matchLst,function(p){ 
							cv <- unique(as.vector(p$compPair))
							cvL <- length(cv)
							if( 0==cvL )
								return( integer(0) )
							rawCompPairV <- NULL
							for( idx in 1:cvL ){
								rawCompPairV <- c( rawCompPairV ,ptnObjGrp$rawPtn$matchLst[[(cv[idx])]]$compPair )
							}
							return( length(unique(rawCompPairV)) )
						})
	# 실제 winDef$field 대상 compPairs
ol.numMtx <- cbind( 1:length(ol.IdxNum) ,ol.IdxNum, ol.preNum ,ol.compNum ,ol.compNum.r )
colnames(ol.numMtx) <- c("matchIdx","idxNum","preNum","compNum" ,"compNum.real")

#	compPair 검토 및 선별
plot( ol.numMtx[,"compNum"] ,ol.numMtx[,"compNum.real"] )
ol.numMtx <- ol.numMtx[order(ol.numMtx[,"preNum"],-ol.numMtx[,"compNum"]),]
ol.numMtx <- ol.numMtx[ ol.numMtx[,"compNum"]>=5 ,]

hntPtnLst <- list()
for( idx in ol.numMtx[,"matchIdx"] ){
    # idx <- ol.numMtx[,"matchIdx"][1]
	huObj <- list()
    huObj$matchLst.idx <- idx
    huObj$ptnObj <- createPtnObj(overlap$matchLst[[idx]]$matMtx) 
    huObj$rawPtn <- scanRawPattern.ptnLst( winDef ,huObj$ptnObj )
    huObj$matchNum <- sapply( huObj$rawPtn$matchLst ,function(p){length(p$compPair)} )
    hntPtnLst[[(length(hntPtnLst)+1)]] <- huObj
}






# ------------------------------------------------------------------------------------
#	Haunt 선별 및
#	Clue List (huLst.inv.clueLst) 생성.
#

k.matchLst.idx <- sapply(hntPtnLst,function(p){p$matchLst.idx})
k.hntPtnNum <- sapply( hntPtnLst ,function(p){length(p$matchNum)} ) # rawPtn$matchLst 의 수.
k.hntCntAll <- sapply( hntPtnLst ,function(p){sum(p$matchNum)})
k.hntCntMaxRate <- sapply( hntPtnLst ,function(p){ return( (max(p$matchNum)*100)%/%sum(p$matchNum) ) } )

olMtx <- cbind(ol.numMtx,k.hntPtnNum, k.hntCntAll, k.hntCntMaxRate )

#	데이터 검토.
spotCol=topo.colors( max(k.hntPtnNum) )
plot( olMtx[,"k.hntCntMaxRate"] ,olMtx[,"k.hntCntAll"] ,col=spotCol[k.hntPtnNum] )
rgndMtx <- cbind( 70 ,50+((10:1)*(250-50)/10) ,((1:10)*(55))%/%10 ) # ptnNum ,cntAll ,maxRate, 
text( x=rgndMtx[,1] ,y=rgndMtx[,2] ,labels=rgndMtx[,3] ,col=spotCol[round(rgndMtx[,3])] )

plot( olMtx[,"k.hntCntMaxRate"] ,olMtx[,"k.hntPtnNum"] ,col=spotCol[k.hntPtnNum] )

# 선별 : 발생 수량이 좀 되면서, 치우침 값도 큰 것을 골라 냄.
olMtx.inv <- olMtx[ ( olMtx[,"k.hntCntMaxRate"]>40 & olMtx[,"k.hntCntAll"]>30 ), ]
hntPtnLst.inv <- hntPtnLst[ sapply(hntPtnLst,function(p){ p$matchLst.idx %in% olMtx.inv[,"matchIdx"] }) ]

#	pHuRawPtn <- hntPtnLst.inv[[1]]$rawPtn;	pPtnObj	<- hntPtnLst.inv[[1]]$ptnObj
ana_cluePtn <- function( pHuRawPtn ,pPtnObj ,pDebug=F ){

		rObj <- list( )
		hIndiceLst <- sapply( pHuRawPtn$matchLst,function(p){ p$compPair })
		rObj$hIndiceLst <- hIndiceLst

		excFilm <- (!pPtnObj$film); excFilm[nrow(excFilm):(nrow(excFilm)-pPtnObj$searchDepth+1),]<-T
		cluePtn <- scanRawPattern.within( winDef ,do.call(c,hIndiceLst) ,pFiltF=NULL ,pExcFilm=excFilm )

		rObj$cluePtn <- cluePtn
		if( 0==length(cluePtn$matchLst) )
			return( rObj )

		for( idx in 1:length(cluePtn$matchLst) ){
			belongMtx <- matrix( NA ,nrow=2 ,ncol=length(cluePtn$matchLst[[idx]]$compPair.v) )
			belongMtx[1,] <- sort(cluePtn$matchLst[[idx]]$compPair.v)
			for( hIdx in 1:length(hIndiceLst) ){	#	idx <- 1
				belongMtx[2 ,belongMtx[1,] %in% hIndiceLst[[hIdx]] ] <- hIdx
			}
			rObj$cluePtn$matchLst[[idx]]$belongMtx <- belongMtx
		} # for(idx)

		return( rObj )
	}

huLst.inv.clueLst <- lapply( hntPtnLst.inv ,function(p){ return(ana_cluePtn(p$rawPtn,p$ptnObj)) } )

huLst.invM <- list()
for( idx in 1:length(huLst.inv.clueLst) ){
	k.FLogStr(sprintf("  huLst.inv.clueLst idx:%d",idx))
	invM <- list( matchLst.idx = hntPtnLst.inv[[idx]]$matchLst.idx )
	invM$huCompPair <- lapply(hntPtnLst.inv[[idx]]$rawPtn$matchLst ,function(p){p$compPair})
	invM$huSize <- sapply( invM$huCompPair ,length )
	invM$clueCompPair <- lapply( huLst.inv.clueLst[[idx]]$cluePtn$matchLst ,function(p){p$compPair.v} )
	invM$clueIdxNum <- sapply( huLst.inv.clueLst[[idx]]$cluePtn$matchLst ,function(p){nrow(p$idxMtx)} )
	invM$clueSize <- sapply( invM$clueCompPair ,length )

	# coverage
	rNames <- c("cvrNum","cvrRate")
	olCvr <- matrix( 0 ,ncol=length(invM$huCompPair) ,nrow=length(rNames) )
	rownames(olCvr) <- rNames
	cvrMtx <- matrix(0 ,ncol=length(invM$huCompPair) ,nrow=length(invM$clueCompPair) )
	for( hIdx in 1:length(invM$huCompPair) ){
		cvrMtx[,hIdx] <- sapply( invM$clueCompPair ,function(p){ sum( invM$huCompPair[[hIdx]] %in% p ) } )
		cvrLst <- lapply( invM$clueCompPair ,function(p){ 
											cvr<-intersect(p,invM$huCompPair[[hIdx]])
											if( 1>=length(cvr) )
												return( NULL )
											else
												return( cvr ) 
										} )
		cvr <- intersect( invM$huCompPair[[hIdx]] ,unique( do.call(c,cvrLst) ) )
		olCvr["cvrNum",hIdx] <- length(cvr)
		olCvr["cvrRate",hIdx] <- (100*olCvr["cvrNum",hIdx]) %/% invM$huSize[hIdx]
	}	
	cvrMtx.hu <- t( apply( cvrMtx ,1 ,function(p){(p*100)%/%invM$huSize} ) )
	cvrMtx.clue <- t( apply( cvrMtx ,1 ,function(p){(p*100)%/%sum(p)}) )
	
	invM$olCvr		<- olCvr
	invM$cvrMtx		<- cvrMtx
	invM$cvrMtx.hu	<- cvrMtx.hu
	invM$cvrMtx.clue <- cvrMtx.clue

	huLst.invM[[(length(huLst.invM)+1)]] <- invM
}

save( huLst.invM ,file="Obj_ptnObjGrp.0608.invM.save" )
# ana.invM


# ------------------------------------------------------------------------------------
#	invM 분석.
#
invM <- huLst.invM[[1]]
anaLst <- lapply( huLst.invM ,ana.invM )	#	anaLst[[n]]$errMsg 확인필요.



for( idx in 1:length(anaLst) ){
	valuableRowNum <- report.invMAna( anaLst[[idx]]	,pRptMsg=sprintf("%d th of anaLst",idx)
													,pRptAppend=(idx!=1)
										)
	if( 0 < valuableRowNum ){
	# if( T ){
		k.FLogStr(sprintf("  V Row found : %d (%d rows)",idx,valuableRowNum),pConsole=T)
	}
}


plot( invM$cvrMtx.hu[ ,1] ,invM$cvrMtx.clue[ ,1] ,pch=as.character(invM$clueIdxNum) )
points( invM$cvrMtx.hu[srchClue,1] , invM$cvrMtx.clue[srchClue,1] ,col="red" )

# 커버리지 확인용. 필요할때만 print() 부분 주석을 풀자.
for( idx in 1:length(invM$huCompPair[[1]]) ){
	# print( sprintf("idx:%d (%d)=================================",idx,invM$huCompPair[[1]][idx]) )
	flag <- sapply( invM$clueCompPair ,function(p){ 
							if( 2==length(p))
								return( F )
							return( invM$huCompPair[[1]][idx]%in%p ) 
						} )
	# print( sprintf("  %s",paste(which(flag),collapse=",")) )
}
