set.seed(2)

#-----------------------------------------
#	K-means Clustering
#-----------------------------------------

x <- matrix( rnorm(50*2) ,ncol=2 )
x[1:25,1] <- x[1:25,1]+3
x[1:25,2] <- x[1:25,2]-4

km.out <- kmeans(x,2,nstart=20)	# nstart : 초기 클러스터 랜덤부여 샘플 수
km.out$cluster	# cluster indices
plot( x ,col=(km.out$cluster+1) )
km.out$tot.withinss	# 최종 수렴된 total within-cluster sum of squares.
km.out$withinss		# 최종 수렴된 (클러스터 별)within-cluster sum of squares

#-----------------------------------------
#	Hierarchical Clustering
#-----------------------------------------

hc.complete	<- hclust( dist(x) ,method="complete" )
hc.average	<- hclust( dist(x) ,method="average" )
hc.single	<- hclust( dist(x) ,method="single" )
	# method : ISLR 395 page
	
par( mfrow=c(1,3) )
plot( hc.complete )
plot( hc.average )
plot( hc.single )

cluster <- cutree( hc.complete ,2 )	# Cluster 구하기.
		# height, cluster(K) 중 원하는대로 값을 줄 수 있음.
		# cutree(tree, k = NULL, h = NULL)
		
# scale 적용
xsc <- scale( x )
plot( hclust(dist(xsc) ,method="complete") )

# Correlation-based distance (only for data with at least three features)
x <- matrix( rnorm(30*3), ncol=3 )
dd <- as.dist(1-cor(t(x)))	# t() : 각 데이터의 좌표값을 대상으로 cor() 적용
plot( hclust(dd,method="complete"))

