# 20180109_C.R 교차모델
source("../breedingPlace/20171116_A_H.R")
source("../breedingPlace/20171116_A_H_cliper.R")
source("../breedingPlace/20171116_B_H.R")
source("../breedingPlace/20171116_C_H.R")
source("../breedingPlace/20171116_D_H.R")
source("20180109_A_H.R")
source("20180109_C_H.R")

saveId <- "0127_23"
load(sprintf("./save/Obj_gEnv%s.save",saveId))
load(sprintf("./save/Obj_fRstLst%s.save",saveId))
load(sprintf("./save/Obj_remLst%s.save",saveId))

stdFiltedCnt <- sapply( fRstLst ,length )
stdFilted.tbl <- table(stdFiltedCnt)
stdFilted.per <- sprintf( "%.1f" ,100*stdFilted.tbl / length(fRstLst) )
names(stdFilted.per) <- names(stdFilted.tbl)


banObj <- getCFltObj( gEnv )
allZoidMtx <- gEnv$zhF
codeLst <- banObj$getCodeLst( allZoidMtx )
# 개발 샘플 시점.

allZoid.fltCnt <- getAllZoidIdx.FltCnt( gEnv ,remLst )
allZoidMtx <- gEnv$allZoidMtx[(allZoid.fltCnt==0),]

tStmp <- Sys.time()
filtedIdxObj <- banObj$getFiltedIdx( allZoidMtx )
filtedIdx <- unique( filtedIdxObj$filtedIdx.dupRow ,filtedIdxObj$filtedIdx.cf1 )
    # filtedIdxObj$filtedIdx.dupRow 에게 모두 파묻히는 거 같은데.. 뭔가 수상타?
tDiff <- Sys.time() - tStmp
cat(sprintf("time cost %.1f%s \n",tDiff,units(tDiff)))

# ===============================================================================

#   pDimCnt : 동일한 dim이 n개 이상 존재하는 것은 자른다.
#   	pBanObj<-banObj ;pZoidMtx<-gEnv$zhF ;pInitZIdx=NULL ;pCodeLst=codeLst   ;pDimThld=2  ;pDepth=2
ban.multiDim <-function( pBanObj ,pZoidMtx ,pCodeLst ,pInitZIdx=NULL ,pDimThld=2 ,pDepth=2 ){

    if( is.null(pInitZIdx) ){
        pInitZIdx <- 1:nrow(pZoidMtx)
    }

    # --------------------------------------------------
    #	filtLst ,filtedIdx
    fndLst <- list()
    for( zIdx in 1:nrow(pZoidMtx) ){
		curCodeLst <- lapply( pCodeLst ,function(p){p[[zIdx]]})
		
		cfNameFndLst <- list()
		for( snIdx in pBanObj$cfNames ){ # search name index
			fndIdxLst <- list()
			for( eIdx in pBanObj$encVal.len:1 ){
				dCnt <- pBanObj$cfObjLst[[snIdx]]$diffCnt( 
								curCodeLst[[snIdx]] ,pBanObj$encValLst[[snIdx]][[eIdx]]
							)
				if( 0==dCnt ){
					fndIdxLst[[1+length(fndIdxLst)]] <- eIdx
				}
			}
			matLst <- lapply(fndIdxLst ,function(pFndIdx){
					hCodeLst <- lapply(pBanObj$encValLst,function(valLst){valLst[[pFndIdx]]})
					matCnt <- sapply(pBanObj$cfNames,function(mName){
									pBanObj$cfObjLst[[mName]]$diffCnt( curCodeLst[[mName]] ,hCodeLst[[mName]] )
								})
					return( pBanObj$cfNames[matCnt==0] )
				})
			cfNameFndLst[[snIdx]] <- list( fndIdxLst=fndIdxLst ,matLst=matLst )
		} # snIdx
		fndLst[[1+length(fndLst)]] <- cfNameFndLst

    } # for(zIdx)
	
	nameLst <- list()
	filtLst <- list()
	for( fIdx in 1:length(fndLst) ){
		cfNameFndLst <- fndLst[[fIdx]]
		maxFnd <- 0
		for( snIdx in pBanObj$cfNames ){
			fndObj <- cfNameFndLst[[snIdx]]
			if( 0<length(fndObj$matLst) ){
				nameCmb <- sapply( fndObj$matLst ,function(p){paste(p,collapse=" ")})
				nameLst[[1+length(nameLst)]] <- nameCmb

				curMax <- max(sapply(fndObj$matLst,length))
				maxFnd <- ifelse( curMax>maxFnd ,curMax ,maxFnd )
			}
		}
		filtLst[[1+length(filtLst)]] <- maxFnd
	}
	
	flag <- sapply( filtLst ,function(p){p>1})
	filtedIdx <- pInitZIdx[flag]

    rstObj <- list( idStr="hntCrossDim" )
    rstObj$filtLst      <- filtLst
    rstObj$filtedIdx    <- filtedIdx
    # 디버깅용 -------
	rstObj$nameLst <- nameLst
	
} # ban.multiDim()

# -- 제외 대상이..

kName <- do.call( c ,nameLst )

> table(kName)
kName
                                    A0010_o3 A0030_o3 
                                                 1944 
                           A0010_o3 A0030_o3 A0040_o3 
                                                   12 
   A0010_o3 A0030_o3 A0040_o3 A0050_o5 A0060_o7 A0070 
                                                   12 
                  A0010_o3 A0030_o3 A0050_o5 A0060_o7 
                                                    8 
            A0010_o3 A0030_o3 A0050_o5 A0060_o7 A0070 
                                                 3610 
      A0010_o3 A0030_o3 A0050_o5 A0060_o7 A0070 A0080 
                                                   54 
A0010_o3 A0030_o3 A0050_o5 A0060_o7 A0070 A0080 A0090 
                                                    7 
      A0010_o3 A0030_o3 A0050_o5 A0060_o7 A0070 A0090 
                                                   48 
                           A0010_o3 A0030_o3 A0060_o7 
                                                    6 
                              A0010_o3 A0030_o3 A0070 
                                                    3 
                              A0010_o3 A0030_o3 A0090 
                                                   12 
                                             A0020_o3 
                                                  756 
                                    A0020_o3 A0030_o3 
                                                    8 
                                    A0020_o3 A0060_o7 
                                                    8 
                                       A0020_o3 A0080 
                                                   10 
                                       A0020_o3 A0090 
                                                    4 
                                             A0030_o3 
                                                 1624 
                                    A0030_o3 A0040_o3 
                                                   14 
                                    A0030_o3 A0060_o7 
                                                   12 
                                       A0030_o3 A0070 
                                                   12 
                                       A0030_o3 A0090 
                                                    2 
                                             A0040_o3 
                                                  941 
                                    A0040_o3 A0060_o7 
                                                    6 
                                       A0040_o3 A0080 
                                                    2 
                                             A0050_o5 
                                                  238 
                                    A0050_o5 A0060_o7 
                                                  110 
                                       A0050_o5 A0080 
                                                    2 
                                             A0060_o7 
                                                 1614 
                                       A0060_o7 A0070 

