
resAreaMtx <- matrix( c(13 ,19 ,21 ,25 ,36 ,45) ,nrow=1 )
resAreaMtx <- rbind( resAreaMtx ,c(09 ,10 ,12 ,16 ,23 ,32) )
resAreaMtx <- rbind( resAreaMtx ,c(16 ,19 ,23 ,27 ,32 ,38) )
resAreaMtx <- rbind( resAreaMtx ,c(03 ,05 ,13 ,17 ,42 ,43) )
resAreaMtx <- rbind( resAreaMtx ,c(02 ,07 ,13 ,14 ,34 ,41) )
resAreaMtx <- rbind( resAreaMtx ,c(08 ,12 ,20 ,21 ,36 ,44) )
resAreaMtx <- rbind( resAreaMtx ,c(03 ,07 ,16 ,18 ,27 ,41) )
resAreaMtx <- rbind( resAreaMtx ,c(21 ,30 ,33 ,38 ,40 ,41) )
resAreaMtx <- rbind( resAreaMtx ,c(02 ,03 ,11 ,29 ,36 ,39) )
resAreaMtx <- rbind( resAreaMtx ,c(01 ,10 ,12 ,16 ,37 ,44) )

resAreaMtx <- rbind( resAreaMtx ,c(01 ,15 ,16 ,36 ,38 ,40) )
resAreaMtx <- rbind( resAreaMtx ,c(01 ,11 ,16 ,20 ,28 ,43) )
resAreaMtx <- rbind( resAreaMtx ,c(03 ,06 ,07 ,25 ,35 ,37) )
resAreaMtx <- rbind( resAreaMtx ,c(06 ,09 ,10 ,21 ,22 ,36) )
resAreaMtx <- rbind( resAreaMtx ,c(05 ,07 ,11 ,31 ,33 ,43) )
resAreaMtx <- rbind( resAreaMtx ,c(07 ,23 ,31 ,34 ,40 ,43) )
resAreaMtx <- rbind( resAreaMtx ,c(02 ,03 ,15 ,24 ,30 ,40) )
resAreaMtx <- rbind( resAreaMtx ,c(02 ,16 ,25 ,28 ,31 ,45) )
resAreaMtx <- rbind( resAreaMtx ,c(10 ,20 ,26 ,27 ,39 ,45) )
resAreaMtx <- rbind( resAreaMtx ,c(02 ,12 ,21 ,29 ,40 ,43) )



ra.cd <- sort(unique(as.vector(resAreaMtx)))

# ---------------------------------------------------------------------------------

FB <- getFlagBank()

hitRate <- apply( FB$zh ,1 ,function(p){length(intersect(p,ra.cd))} )

myCsv <- read.csv("C:/zproject/ISLR/DataSet/excel_742.csv",header=F)
myMtx <- as.matrix(myCsv)
myMtx <- myMtx[,2:7]

p <- myMtx[1,]

sc <- apply(myMtx ,1 ,function(p){
						k <- apply( resAreaMtx ,1 ,function(pp){length(intersect(pp,p))})
						return( max(k) )
					}
		)


