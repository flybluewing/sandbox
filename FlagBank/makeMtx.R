Obj_makeMtx <- list()

workZh <- FB$zh



bh <- 7
mtx <- matrix( 0 ,nrow=nrow(workZh) ,ncol=bh )
for( bhIdx in 1:bh ){
	rObj <- k.rebound( bhIdx ,pZh=workZh )
	mtx[,bhIdx] <- rObj
}
Obj_makeMtx$mtx.k.rebound <- mtx

bh <- 7
mtx <- matrix( 0 ,nrow=nrow(workZh) ,ncol=bh )
for( bhIdx in 1:bh ){
	rObj <- k.dupPosition( bhIdx ,pZh=workZh )
	mtx[,bhIdx] <- rObj
}
Obj_makeMtx$mtx.k.dupPosition <- mtx

fileName <- sprintf("Obj_makeMtx%d.save",nrow(workZh))
save( Obj_makeMtx ,file=fileName )

