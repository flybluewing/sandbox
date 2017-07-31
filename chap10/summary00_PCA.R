states = row.names(USArrests)
# data overview
apply(USArrests,2,mean)
apply(USArrests,2,var)

# PCA (with standardize)
pr.out <- prcomp( USArrests, scale=T )
names(pr.out)
	#	"sdev"     "rotation" "center"   "scale"    "x"
	#	center : mean       sdev : standard deviations
head(pr.out$rotation)
	# Principal component loadings(즉, 기울기)
	#					PC1        PC2        PC3         PC4
	#	Murder   -0.5358995  0.4181809 -0.3412327  0.64922780
	#	Assault  -0.5831836  0.1879856 -0.2681484 -0.74340748
	#	UrbanPop -0.2781909 -0.8728062 -0.3780158  0.13387773
	#	Rape     -0.5434321 -0.1673186  0.8177779  0.08902432
head(pr.out$x)
	# Principal component score vectors(PCA 적용된 결과 값)
	#					  PC1        PC2         PC3          PC4
	#	Alabama    -0.9756604  1.1220012 -0.43980366  0.154696581
	#	Alaska     -1.9305379  1.0624269  2.01950027 -0.434175454
	#	Arizona    -1.7454429 -0.7384595  0.05423025 -0.826264240
	#	Arkansas    0.1399989  1.1085423  0.11342217 -0.180973554
	#	California -2.4986128 -1.5274267  0.59254100 -0.338559240
	#	Colorado   -1.4993407 -0.9776297  1.08400162  0.001450164

biplot( pr.out ,scale=0 ,cex=0.5 )
	# 상-우 : Principal component loadings
	# 하-좌 : Principal component scores
	
pr.var <- pr.out$sdev^2  # The Variance explained by principal component (ISLR 403page)
pve <- pr.var / sum(pr.var)
	# The propotion of variance explained by each principal component.
plot( cumsum(pve) ,xlab="PComp" ,ylab="explained Var" ,ylim=0:1 ,type='b' )	

