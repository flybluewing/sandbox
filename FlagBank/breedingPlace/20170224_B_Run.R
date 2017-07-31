# 실제 실행용.
source("20170224_B_H.R")
source("20170224_B_auxH.R")
source("20170224_B_Run_H.R")

#================================================================
# [custFunction]
#----------------------------------------------------------------
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


# FB <- getFlagBank()
fbObj <- buildField( pFieldMtx=as.matrix(FB$zh) )

#================================================================
# [RawPtn & Overlap]
#----------------------------------------------------------------
# library(snowfall)
sfInit( parallel=T ,cpus=2 )
sfExport("k.FLogStr");			sfExport("k.FLogOpt")
sfExport("scanRawPattern");		sfExport("scanRawPattern.filtF")
sfExport("filtF.lastRowMiss");	sfExport("findOverlap")

wIdxLst <- as.list( c(34,42) )	# seq(1,nrow(FB$zh),10)
sfExport("fbObj");	sfExport("getWindow")
k.FLogStr(sprintf("Parallel test start ========================"),pAppend=F,pConsole=T)
saveFiles <- sfLapply(wIdxLst,function(wIdx){

					saveFileName <- sprintf("Obj_ptnObjGrp%03d.save",wIdx)
					winDef <- getWindow( pField=fbObj$vFieldMtx	,pFilm=fbObj$filmMtx
											,pHeight=3 ,pInitPos=wIdx ,pIdStr=sprintf("wObj%03d",wIdx)
										)
					rawPtn <- scanRawPattern( winDef ,pFiltF=filtF.lastRowMiss ,pBuffSize=1 ,pDebug=T )
					overlap <- findOverlap( rawPtn ,pFiltF=NULL ,pDebug=T ) 
						# rawPtn 내 중복점을 찾기 위함이므로 pFiltF는 적용안함.
					ptnObjGrp <- list( winDef=winDef ,rawPtn=rawPtn ,overlap=overlap )
					save( ptnObjGrp ,file=saveFileName )
					k.FLogStr(sprintf("%s is saved",saveFileName),pConsole=T)
	
					return(saveFileName)
				})
k.FLogStr(sprintf("Parallel test end ========================"),pConsole=T)

report.ptnObjGrp( do.call(c,saveFiles) )

#================================================================
# [RawPtn 검토]
#----------------------------------------------------------------
myObj <- load("Obj_ptnObjGrp042.save")

#================================================================
# [Overlap 검토]
#----------------------------------------------------------------
fn <- "Obj_ptnObjGrp017.save"
myObj <- load(fn)
par( mfrow=c(4,2) )
png( sprintf("%s.png",fn) ,height=768*2 ,width=768 )
pMatchLst <- ptnObjGrp$overlap$matchLst
idxNum	<- sapply(pMatchLst,
					function(p){ return(nrow(p$idxMtx)) } )
preNum	<- sapply(pMatchLst,
					function(p){ h<-nrow(p$matMtx) ;return( sum(!is.na(p$matMtx[h,])) ) }
				)
compNum	<- sapply(pMatchLst,
					function(p){ cv<-unique(as.vector(p$compPair))  ; return(length(cv)) }
				)
compNum.real <- sapply( pMatchLst ,function( p ){
						# p <- ptnObjGrp$overlap$matchLst[[1]]
						rpMatchIdx <- unique( as.vector(p$compPair) )
						ml <- ptnObjGrp$rawPtn$matchLst[rpMatchIdx]
						rpIdx <- do.call( c, lapply(ml,function(p){p$compPair}) )
						return( length(unique(rpIdx)) )
					})
numMtx <- cbind( 1:length(idxNum) ,idxNum ,preNum ,compNum ,compNum.real )

	# report
	plot( numMtx[,"compNum"] ,numMtx[,"compNum.real"] ,main=fn )

selFlag.compNum <- numMtx[,"compNum"]>15
selFlag.predNum <- numMtx[,"preNum"]>0
selFlag.ratio	<- numMtx[,"idxNum"]>=(numMtx[,"preNum"]*2)
ol.numMtx <- numMtx[(selFlag.compNum&selFlag.predNum&selFlag.ratio),]# sort(numMtx[,"compNum"],decreasing=T)[1:20]
hntPtnLst <- list()
for( idx in ol.numMtx[,1] ){
    # idx <- ol.numMtx[,1][1]
	# k.FLogStr(sprintf("hntPtnLst idx : %d",idx),pConsole=T)
	huObj <- list()
    huObj$matchLst.idx <- idx
    huObj$ptnObj <- createPtnObj(ptnObjGrp$overlap$matchLst[[idx]]$matMtx) 
    huObj$rawPtn <- scanRawPattern.ptnLst( ptnObjGrp$winDef ,huObj$ptnObj )
    huObj$matchNum <- sapply( huObj$rawPtn$matchLst ,function(p){length(p$compPair)} )
    hntPtnLst[[(length(hntPtnLst)+1)]] <- huObj
}

hntPtnNum <- sapply( hntPtnLst ,function(p){length(p$matchNum)} ) # rawPtn$matchLst 의 수.
hntCntAll <- sapply( hntPtnLst ,function(p){sum(p$matchNum)})
hntCntMaxRate <- sapply( hntPtnLst ,function(p){ return( (max(p$matchNum)*100)%/%sum(p$matchNum) ) } )
olMtx <- cbind(ol.numMtx,hntPtnNum, hntCntAll, hntCntMaxRate )

	# report
	spotCol=topo.colors( max(hntPtnNum) )
	plot( olMtx[,"hntCntMaxRate"] ,olMtx[,"hntCntAll"] ,col=spotCol[hntPtnNum]  ,xlim=c(0,100) ,main=fn )
	yrng <- range(olMtx[,"hntCntAll"]);	yrng.marg <- (yrng[2]-yrng[1])%/%10;	yrng[1]<-yrng[1]+yrng.marg;	yrng[2]<-yrng[2]-yrng.marg
	rgndMtx <- cbind( 70 ,yrng[1]+((10:1)*(yrng[2]-yrng[1])/10) ,round((1:10)*(max(hntPtnNum)/10)) ) # ptnNum ,cntAll ,maxRate,
	rgndMtx[1,3] <- ifelse( rgndMtx[1,3]==0 ,1 ,rgndMtx[1,3] )
	text( x=rgndMtx[,1] ,y=rgndMtx[,2] ,labels=rgndMtx[,3] ,col=spotCol[round(rgndMtx[,3])] )

	plot( olMtx[,"hntCntMaxRate"] ,olMtx[,"hntPtnNum"] ,col=spotCol[hntPtnNum] ,xlim=c(0,100) ,main=fn )

olMtx.inv <- olMtx[ ( olMtx[,"hntCntMaxRate"]>30 & (olMtx[,"hntCntAll"]/nrow(ptnObjGrp$winDef$field))>0.4 ), ]
hntPtnLst.inv <- hntPtnLst[ sapply(hntPtnLst,function(p){ p$matchLst.idx %in% olMtx.inv[,1] }) ]

if( 0==length(hntPtnLst.inv) ){
	k.FLogStr(sprintf("No investigation in overlap in %s",fn))
	stop("No investigation in overlap")
}

invMFileLst <- list()
for( invIdx in 1:length(hntPtnLst.inv) ){ # invIdx <- 1
	p <- hntPtnLst.inv[[invIdx]]
	k.FLogStr(sprintf("scanRawPattern.within() for ol idx %d",p$matchLst.idx))
	rObj <- list( matchLst.idx = p$matchLst.idx )
	rObj$hIndiceLst <- sapply( p$rawPtn$matchLst,function(p){ p$compPair })
	excFilm <- (!p$ptnObj$film); excFilm[nrow(excFilm):(nrow(excFilm)-p$ptnObj$searchDepth+1),]<-T
	
	rObj$cluePtn <- scanRawPattern.within( ptnObjGrp$winDef ,do.call(c,rObj$hIndiceLst) ,pFiltF=NULL ,pExcFilm=excFilm )

	if( 0==length(rObj$cluePtn$matchLst) ){
		k.FLogStr(sprintf("     no clue is found (matchLst.idx:%d)",p$matchLst.idx))
		next #	return( rObj )
	}

	rObj$huCompPair		<- lapply( hntPtnLst.inv[[invIdx]]$rawPtn$matchLst ,function(p){p$compPair})
	rObj$huSize			<- sapply( rObj$huCompPair ,length )
	rObj$clueCompPair	<- lapply( rObj$cluePtn$matchLst ,function(p){p$compPair.v} )
	rObj$clueIdxNum		<- sapply( rObj$cluePtn$matchLst ,function(p){nrow(p$idxMtx)} )
	rObj$clueSize		<- sapply( rObj$clueCompPair ,length )
		# clueSize가 너무 작은 것들 뿐이라면, findOverlap() 적용필요하다.

	# coverage
	rNames <- c("cvrNum","cvrRate")
	olCvr <- matrix( 0 ,ncol=length(rObj$huCompPair) ,nrow=length(rNames) )
	rownames(olCvr) <- rNames
	cvrMtx <- matrix(0 ,ncol=length(rObj$huCompPair) ,nrow=length(rObj$clueCompPair) )
	for( hIdx in 1:length(rObj$huCompPair) ){
		cvrMtx[,hIdx] <- sapply( rObj$clueCompPair ,function(p){ sum( rObj$huCompPair[[hIdx]] %in% p ) } )
		cvrLst <- lapply( rObj$clueCompPair ,function(p){ 
											cvr<-intersect(p,rObj$huCompPair[[hIdx]])
											if( 1>=length(cvr) )
												return( NULL )
											else
												return( cvr ) 
										} )
		cvr <- intersect( rObj$huCompPair[[hIdx]] ,unique( do.call(c,cvrLst) ) )
		olCvr["cvrNum",hIdx] <- length(cvr)
		olCvr["cvrRate",hIdx] <- (100*olCvr["cvrNum",hIdx]) %/% rObj$huSize[hIdx]
	}
	cvrMtx.hu <- t( apply( cvrMtx ,1 ,function(p){(p*100)%/%rObj$huSize} ) )
	cvrMtx.clue <- t( apply( cvrMtx ,1 ,function(p){(p*100)%/%sum(p)}) )
	
	rObj$olCvr		<- olCvr
	rObj$cvrMtx		<- cvrMtx
	rObj$cvrMtx.hu	<- cvrMtx.hu
	rObj$cvrMtx.clue <- cvrMtx.clue

	invMFile <- sprintf( "%s.invM%d.save" ,fn ,rObj$matchLst.idx )
	invObj <- rObj
	save( invObj ,file=invMFile )
	invMFileLst[[(1+length(invMFileLst))]] <- invMFile
}

if( 0==length(invMFileLst) ){
	k.FLogStr(sprintf("No investigation(in %s) has clue.",fn))
	stop("No investigation has clue.")
}


for( idx in 1:length(invMFileLst) ){ # idx <- 1
	fName <- invMFileLst[[idx]]
	k.FLogStr(sprintf("loading %s",fName),pConsole=T)
	myObj <- load( fName )	# invObj
	ana <- ana.invM( invObj )
	if( !is.null(ana$errMsg) ){
		k.FLogStr(sprintf("analysis error message (%s)\n      %s",fName,ana$errMsg),pConsole=T)
		next
	}
	valuableRowNum <- report.invMAna( ana	,pRptMsg=sprintf("invObj in %s",fName) 
											,pRptFile=sprintf("./report/%s.anaRpt",fn)
											,pRptAppend=(idx!=1)
									)
	if( 0 < valuableRowNum ){
	# if( T ){
		k.FLogStr(sprintf("  Valuable Row found : %s (%d rows)",fName,valuableRowNum),pConsole=T)
	}

}





