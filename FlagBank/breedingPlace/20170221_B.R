FB <- getFlagBank()
hMtx <- FB$zh
set.seed(42)

# 모든 DNA 가 나온 시점을 먼저 찾고.
dnaType <- 1:45
dnaSize <- ncol(hMtx)
dnaFlag <- rep( 0 ,length(dnaType) )
names(dnaFlag) <- dnaType

k.FLogStr("start",pConsole=T)
zoid1 <- matrix( 0 ,ncol=dnaSize ,nrow=8000000 )
for( rIdx in 1:nrow(zoid1) ){
	zoid1[rIdx,] <- sort(sample( dnaType, 6 ))
}
k.FLogStr("end",pConsole=T)

hMtx <- hMtx[1:2,]

matchMtx <- matrix( 0 ,ncol=nrow(zoid1) ,nrow=nrow(hMtx) )
k.FLogStr("Start matchMtx",pConsole=T)
for( hIdx in 1:nrow(hMtx) ){
	# hIdx <- 1	
	itstNum <- apply( zoid1 ,1 ,function( p ){
							return(length(intersect( p, hMtx[hIdx,] )))
						})
	k.FLogStr(sprintf("   pressing %d",hIdx))
}
k.FLogStr("End matchMtx",pConsole=T)

