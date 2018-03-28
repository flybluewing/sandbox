# 20180109_C.R 교차모델
source("../breedingPlace/20171116_A_H.R")
source("../breedingPlace/20171116_A_H_cliper.R")
source("../breedingPlace/20171116_B_H.R")
source("../breedingPlace/20171116_C_H.R")
source("../breedingPlace/20171116_D_H.R")
source("20180109_A_H.R")
source("20180109_C_H.R")

saveId <- "Z798"
load(sprintf("./save/Obj_gEnv%s.save",saveId))
load(sprintf("./save/Obj_fRstLst%s.save",saveId))
load(sprintf("./save/Obj_remLst%s.save",saveId))

stdFiltedCnt <- sapply( fRstLst ,length )
stdFilted.tbl <- table(stdFiltedCnt)
stdFilted.per <- sprintf( "%.1f" ,100*stdFilted.tbl / length(fRstLst) )
names(stdFilted.per) <- names(stdFilted.tbl)

allZoid.lst <- vector("list",nrow(gEnv$allZoidMtx))
for( nIdx in attributes(remLst)$name ){
    for( aIdx in remLst[[nIdx]] ){
        allZoid.lst[[aIdx]][1+length(allZoid.lst[[aIdx]])] <- nIdx
    }
}


allZoid.fltCnt <- getAllZoidIdx.FltCnt( gEnv ,remLst )
# allZoidMtx <- gEnv$allZoidMtx[(allZoid.fltCnt==0),]

lastZoid <- gEnv$zhF[nrow(gEnv$zhF),]
# 가정 : allZoid.fltCnt 는 1~3이다.
allZoid.idx0 <- which(allZoid.fltCnt==0)
allZoid.idx1 <- which(allZoid.fltCnt==1)
allZoid.idx2 <- which(allZoid.fltCnt==2)

# 가정 : zoid[1]==1
flag <- sapply( allZoid.idx0 ,function( p ){ gEnv$allZoidMtx[p,1] == 1 })
allZoid.idx0 <- allZoid.idx0[flag]
flag <- sapply( allZoid.idx1 ,function( p ){ gEnv$allZoidMtx[p,1] == 1 })
allZoid.idx1 <- allZoid.idx1[flag]
flag <- sapply( allZoid.idx2 ,function( p ){ gEnv$allZoidMtx[p,1] == 1 })
allZoid.idx2 <- allZoid.idx2[flag]

# 가정 : 22,32는 포함되지 않는다.
flag <- sapply( allZoid.idx0 ,function( p ){ !any(c(22,23) %in%gEnv$allZoidMtx[p,]) })
allZoid.idx0 <- allZoid.idx0[flag]
flag <- sapply( allZoid.idx1 ,function( p ){ !any(c(22,23) %in%gEnv$allZoidMtx[p,]) })
allZoid.idx1 <- allZoid.idx1[flag]
flag <- sapply( allZoid.idx2 ,function( p ){ !any(c(22,23) %in%gEnv$allZoidMtx[p,]) })
allZoid.idx2 <- allZoid.idx2[flag]

# 가정 : zoid[4]%%10은 2가 아님.
flag <- sapply( allZoid.idx0 ,function(p){ 2!=gEnv$allZoidMtx[p,4]%%10 } )
allZoid.idx0 <- allZoid.idx0[flag]
flag <- sapply( allZoid.idx1 ,function(p){ 2!=gEnv$allZoidMtx[p,4]%%10 } )
allZoid.idx1 <- allZoid.idx1[flag]
flag <- sapply( allZoid.idx2 ,function(p){ 2!=gEnv$allZoidMtx[p,4]%%10 } )
allZoid.idx2 <- allZoid.idx2[flag]

# 가정 : 연속 값은 1개이다.
flag <- sapply( allZoid.idx0 ,function( p ){ 1==sum(c(2,10,14,36) %in%gEnv$allZoidMtx[p,]) })
allZoid.idx0 <- allZoid.idx0[flag]
flag <- sapply( allZoid.idx1 ,function( p ){ 1==sum(c(2,10,14,36) %in%gEnv$allZoidMtx[p,]) })
allZoid.idx1 <- allZoid.idx1[flag]
flag <- sapply( allZoid.idx2 ,function( p ){ 1==sum(c(2,10,14,36) %in%gEnv$allZoidMtx[p,]) })
allZoid.idx2 <- allZoid.idx2[flag]

# 가정 : allZoid.fltCnt의 1영역에서, 바로이전 필터링 결과는 포함되지 않는다.
#   단, 과거에 일어난 필터링만 사용한다.
#   단 1영역에서 "A0110.A" 반복은 제외(반복 발생량의 60%)
stdFiltedCnt.idx1 <- which(stdFiltedCnt==1)
lastFlt <- fRstLst[[ stdFiltedCnt.idx1[[length(stdFiltedCnt.idx1)]] ]]
fName <- do.call( c ,allZoid.lst[allZoid.idx1] )
allZoid.idx1 <- allZoid.idx1[fName!=lastFlt]

neverFnd <- setdiff( attributes(remLst)$name ,unique(do.call(c,fRstLst[stdFiltedCnt==1])) )
fName <- do.call( c ,allZoid.lst[allZoid.idx1] )
flag <- sapply( allZoid.lst[allZoid.idx1] ,function(p){ p%in%neverFnd })
allZoid.idx1 <- allZoid.idx1[!flag]


# 가정 : allZoid.fltCnt의 2영역에서, 바로이전 필터링 결과는 포함되지 않는다.
#   단, 과거에 일어난 필터링 조합만 사용한다.
stdFiltedCnt.idx2 <- which(stdFiltedCnt==2)
lastFlt <- fRstLst[[ stdFiltedCnt.idx2[[length(stdFiltedCnt.idx2)]] ]]
lastFlt.name <- paste(sort(lastFlt),collapse="_")
flag <- sapply( allZoid.lst[allZoid.idx2] ,function(p){
                    lastFlt.name != paste(sort(p),collapse="_")
                })
allZoid.idx2 <- allZoid.idx2[flag]

hnt.name <- unique(sapply(fRstLst[stdFiltedCnt.idx2] ,function(p){ paste(sort(p),collapse="_") } ))
flag <- sapply( allZoid.lst[allZoid.idx2] ,function(p){
                    return( paste(sort(p),collapse="_") %in% hnt.name )
                })
allZoid.idx2 <- allZoid.idx2[flag]


#   allIdx <- allZoid.idx1  ;allIdx.bak <- allIdx
cutEadge <- function( gEnv ,allIdx ){

    allZoidMtx <- gEnv$allZoidMtx[allIdx,]
    colValLst <- apply( gEnv$zhF ,2 ,function(p){
                        val <- sort(unique(p))
                        tbl <- table(p)
                        mtx <- matrix( 0 ,ncol=length(val) ,nrow=2 )
                        mtx[1,] <- val
                        mtx[2,] <- tbl[as.character(val)]
                        rownames(mtx) <- c("val","freq")
                        return(mtx)
                    })

    rstObj <- cutEadge.colValCut( gEnv ,allIdx ,colValLst )
    allIdx <- allIdx[rstObj$flag]

    rstObj <- cutEadge.dup3Col( gEnv ,allIdx ,colValLst ,pThld=5 )  # pThld^6 에 비해 효과는 좋음.
    allIdx <- allIdx[rstObj$flag]

    rstObj <- cutEadge.getCFltObj( gEnv ,allIdx )
    allIdx <- allIdx[rstObj$flag]

    rstObj <- cutEadge.getCFltObj( gEnv ,allIdx )
    allIdx <- allIdx[rstObj$flag]

    rstObj <- cutEadge.remLstHard( gEnv ,allIdx )
    allIdx <- allIdx[rstObj$flag]

    rstObj <- cutEadge.getColSeq( gEnv ,allIdx )
    allIdx <- allIdx[rstObj$flag]

    rstObj <- cutEadge.getBanPtn( gEnv ,allIdx )
    allIdx <- allIdx[rstObj$flag]
    # cutEadge.getBanPtnRem10() %% 10 에 대한 ptn

    rstObj <- cutEadge.getBanSym( gEnv ,allIdx )
    allIdx <- allIdx[rstObj$flag]

    rstObj <- cutEadge.getBanGrad( gEnv ,allIdx )
    allIdx <- allIdx[rstObj$flag]

    allIdx.bak <- allIdx

} # cutEadge()

# ~~ 32min

    banObj <- getBanPtn( gEnv$zhF )
    chkCnt <- sapply(banObj$ptnLst ,function(p){p$chkCnt})
    thldPtnIdx <- which(chkCnt<pThldChk)    # chkCnt에 대한 갯수 기준.

    banRst <- banObj$chkMatchAny( gEnv$allZoidMtx[allIdx,,drop=F] ,pDebug=T )
    rstLst <- banRst$rstLst
    #   chkMatchAny() 가 사용된다면 pThldChk는 의미없다.
    # rstLst <- lapply( banRst$rstLst ,function(p){ setdiff(p,thldPtnIdx) })


# cutEadge.getBanPtnColVal() 컬럼 값 별
cutEadge.getBanPtnColVal <- function( gEnv ,allIdx ){

	valMtx <- gEnv$zhF

	azColValLst <- apply( gEnv$allZoidMtx[allIdx,,drop=F] ,2 ,function(p){unique(p)} )
	flagLst.cv <- vector( "list" ,length(allIdx) )
	for( azColIdx in 1:6 ){
		for( vIdx in azColValLst[[azColIdx]] ){
			tValMtx <- valMtx[valMtx[,azColIdx]==vIdx ,]
			banObj <- getBanPtn( tValMtx )			
			banRst <- banObj$chkMatchAny( gEnv$allZoidMtx[allIdx,,drop=F] ,pExcCol=azColIdx ,pDebug=T )
			
			for( idx in seq_len(length(allIdx)) ){
				if( 0==length(banRst$rstLst[[idx]]) ){
					next
				}
				zoid <- gEnv$allZoidMtx[ allIdx[idx] ,]
				if( zoid[azColIdx]!=vIdx ){
					next
				}
				flagLst.cv[[idx]][[ 1+length(flagLst.cv[[idx]]) ]] <- c(azColIdx,vIdx)
			}
		} # vIdx
	} # azColIdx
	## QQE debuging flagLst.cv last result
	
    rObj <- list( idStr="cutEadge.getBanPtnColVal" )
    rObj$flag <- QQE # 
    return( rObj )

} # cutEadge.getBanPtnColVal()





cutEadge.XXXX <- function( gEnv ,allIdx ){

    rObj <- list( idStr="cutEadge.XXXX" )
    rObj$flag <- QQE # 
    return( rObj )

} # cutEadge.XXXX()





# ===============================================================================







