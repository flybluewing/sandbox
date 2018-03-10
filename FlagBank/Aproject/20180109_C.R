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



cf_C0010w02 <- function( pEnv ){
	
	cfObj <- list( idStr="C0010w02" )
    cfObj$rawCfA <- cf_A0010( pEnv ,pBase=2 )
    cfObj$rawCfB <- cf_A0020( pEnv ,pBase=2 )
	cfObj$enc <- function( pZoidMtx ,pLogFunc=NULL ){
		tStmp <- Sys.time()

        aValLst <- cfObj$rawCfA$enc( pZoidMtx )
        bValLst <- cfObj$rawCfB$enc( pZoidMtx )
        rLst <- lapply(1:length(aValLst),function(idx){
                        cf_CUtil.4enc(aValLst[[idx]],bValLst[[idx]])
                    })

		if( !is.null(pLogFunc) ){
			tDiff <- Sys.time() - tStmp
			pLogFunc( sprintf("ID:%s cost:%.1f%s",cfObj$idStr,tDiff,units(tDiff)) )
		}
		
		return( rLst )
	} # cfObj$enc()
	cfObj$diffCnt <- function( p1 ,p2 ){
		return( sum(p1!=p2) )
	}

	throughHisMtx <- matrix( 0 ,nrow=0 ,ncol=4 )
	throughHisMtx <- rbind( throughHisMtx ,c( 2 ,3 ,3 ,2 ) )
	throughHisMtx <- rbind( throughHisMtx ,c( 3 ,3 ,2 ,2 ) )
	throughHisMtx <- rbind( throughHisMtx ,c( 4 ,2 ,2 ,2 ) )
	throughHisMtx <- rbind( throughHisMtx ,c( 5 ,2 ,2 ,1 ) )
	colnames(throughHisMtx) <- c("depth","hard","mid","easy") # 1%이내, 2%부근 ,5% 부근
	cfObj$throughHisMtx <- throughHisMtx
	
	return( cfObj )

} # cf_C0010w02()

cf_C0010w04

cf_C0020w03

cf_C0030w04