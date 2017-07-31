
library(ISLR)

nci.labs <- NCI60$labs
nci.data <- NCI60$data	# row:암 종류 column:genes
dim(nci.data)	# 64 rows, 6,830 columns

table( nci.labs )	# 종류 별 데이터 수 확인.

pr.out <- prcomp( nci.data, scale=T )

Cols <- function( vec ){
		cols <- rainbow(length(unique(vec)))
		return( cols[as.numeric(as.factor(vec))] )
	}

# [Principal Component]-------------------------------------
par( mfrow=c(1,2) )
plot( pr.out$x[,1:2], col=Cols(nci.labs), pch=19, xlab="Z1", ylab="Z2" )
plot( pr.out$x[,c(1,3)], col=Cols(nci.labs), pch=19, xlab="Z1", ylab="Z2" )

summary( pr.out )	# PVE(Propotion of Variance Explaned)
plot( pr.out )	# sdev^2 graph

pve <- 100*pr.out$sdev^2/sum(pr.out$sdev^2)
	# summary(pr.out)$importance[2,] 를 사용해도 됨.
	# summary(pr.out)$importance[3,] 으로 cumsum() 조회 가능

par( mfrow=c(1,2) )
plot( pve ,type="o" ,ylab="Cumulative PVE" ,xlab="Principal Component" ,col="brown" )
plot( cumsum(pve) ,type="o" ,ylab="Cumulative PVE" ,xlab="Principal Component" ,col="brown3" )

# [Heirarchical Clustering]-------------------------------------
sd.data <- scale(nci.data)

par( mfrow=c(1,3) )
data.dist <- dist(sd.data)
plot( hclust(data.dist) 
	,labels=nci.labs ,main="Complete Linkage" ,xlab="" ,sub="" ,ylab="" )
plot( hclust(data.dist,method="average") 
	,labels=nci.labs ,main="Average Linkage" ,xlab="" ,sub="" ,ylab="" )
plot( hclust(data.dist,method="single")
	,labels=nci.labs ,main="Single Linkage" ,xlab="" ,sub="" ,ylab="" )	

hc.out <- hclust( dist(sd.data) )
hc.clusters <- cutree( hc.out, 4 )
table( hc.clusters, nci.labs )
	#			   nci.labs
	#	hc.clusters BREAST CNS COLON K562A-repro K562B-repro LEUKEMIA
	#			  1      2   3     2           0           0        0
	#			  2      3   2     0           0           0        0
	#			  3      0   0     0           1           1        6
	#			  4      2   0     5           0           0        0
	# --> 단순히 Cluster 별 갯수가 아닌, nci.labs에 대응된 갯수가 표현됨.
	
plot( hc.out ,labels=nci.labs )
abline( h=139 ,col="red" )


# [K-means]-------------------------------------
set.seed(2)
km.out <- kmeans( sd.data ,4 ,nstart=20 )
km.clusters <- km.out$cluster
table( km.clusters ,hc.clusters )
	#			   hc.clusters
	#	km.clusters  1  2  3  4
	#			  1 11  0  0  9
	#			  2  0  0  8  0
	#			  3  9  0  0  0
	#			  4 20  7  0  0

# [PCA & Clustering]---------------------------------------------
hc.out <- hclust(dist(pr.out$x[,1:5]))
plot( hc.out ,labels=nci.labs ,main="Heir. Clust. on First Five score vectors")
table( cutree(hc.out,4), nci.labs )
