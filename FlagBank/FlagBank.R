# --------------------------------------------------------------------------------------
#	기본 FB
# --------------------------------------------------------------------------------------
FB <- getFlagBank( )

FB$saf <- defineSAF( )
FB$dpf <- defineDPF( )

# 테스트용 단순화 작업.
flagNames <- attributes(FB$saf)$names
for( nIdx in flagNames ){
	FB$saf[[nIdx]]$def$nnOpt$maxit <- 5
	FB$saf[[nIdx]]$def$nnOpt$size <- c(5,5)
}

filterName <- attributes(FB$saf)$name
names(filterName) <- filterName # as.list()를 위해
# 작업에서 제외시킬 filter 는 여기서 정리해 주자.

# SAF Flag 생성
#	- 나중에는 필요한 Flag 사용하도록 한다.
#	- flag가 NULL인지 체크 필요.
for( nIdx in filterName ){
	k.FLogStr(sprintf("getFlag(%s)",nIdx),pConsole=T)
	FB$saf[[nIdx]]$flag <- getFlag( FB$saf[[nIdx]] )
	if( is.null(FB$saf[[nIdx]]$flag) )
		next
	class(FB$saf[[nIdx]]) <- class(FB$saf[[nIdx]]$flag)
}

# FlagMtx 업데이트.
#	- nrow() 는 zh와 동일.
#	- 기존 Flag가 있으면 업데이트, 아니면 추가.
newFMtx <- matrix( 0 ,ncol=0 ,nrow=nrow(FB$flagMtx) )
newFMtxCol <- NULL
for( nIdx in filterName ){
	if( is.null(FB$saf[[nIdx]]$flag) )
		next
	if( nIdx %in% colnames(FB$flagMtx) ){
		FB$flagMtx[,nIdx] <- FB$saf[[nIdx]]$flag
	} else {
		newFMtx <- cbind( newFMtx ,FB$saf[[nIdx]]$flag$flag )
		newFMtxCol <- c(newFMtxCol,nIdx)
	}
}
colnames(newFMtx) <- newFMtxCol
FB$flagMtx <- cbind( FB$flagMtx ,newFMtx )

# Simul & Predr
sfInit( parallel=T ,cpus=3 )
sfLibrary(RSNNS)
sfExport("FB");			sfExport("getMlpSimul")
sfExport("k.FLogStr");	sfExport("k.FLogOpt")
sfExport("getMlpPredr")
rLst <- sfLapply( as.list(filterName) ,function( p ){
					rObj <- list()					
					FB$saf[[p]]$simul <- getMlpSimul( FB$saf[[p]] ,pSimulNum=5 ,pSimulSetNum=5 )
					FB$saf[[p]]$predr <- getMlpPredr( FB$saf[[p]] )
					return( FB$saf[[p]] )
				}
			)

for( nIdx in filterName ){
	FB$saf[[nIdx]]$simul = rLst[[nIdx]]$simul
	FB$saf[[nIdx]]$predr = rLst[[nIdx]]$predr
}

saveObj <- list( FB=FB ,rLst=rLst )
save( saveObj ,file="Obj_saveObj_1031.save" )


# Simul
#	- flag의 NULL 여부는 simul()함수에서 감지하여 처리.
sfInit( parallel=T ,cpus=5 )
sfLibrary(RSNNS)
sfExport("FB");			sfExport("getMlpSimul")
sfExport("k.FLogStr");	sfExport("k.FLogOpt")
simulLst <- sfLapply( FB$saf ,function( p ){
						rSimul <- getMlpSimul( p ,pSimulNum=500 ,pSimulSetNum=5 )
						return( rSimul )
					}
				)

for( nIdx in attributes(simulLst)$names ){
	FB$saf[[nIdx]]$simul <- simulLst[[nIdx]]
}

sfExport("getMlpPredr")
predrLst <- sfLapply( FB$saf ,function(p){
						rPredr <- getMlpPredr( p )
						return( rPredr )
					}
				)
for( nIdx in attributes(simulLst)$names ){
	FB$saf[[nIdx]]$predr <- predrLst[[nIdx]]
}


# Report


