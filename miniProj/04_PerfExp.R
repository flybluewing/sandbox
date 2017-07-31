# ---------------------------------------------------------------------------------
#	실험 : 규칙성이 있는 Flag에서, ANN은 규칙성을 제대로 잡아내는가?
# ---------------------------------------------------------------------------------

source("03_H.R")
source("04_H.R")

nnOpt <- NnOpt()
nnOpt$maxit <- 2000
nnOpt$size <- c(10,10)


pFlag <- t03.test.sample002(pSize=100)

ObjSeq2 <- list()
ObjSeq2$flag <- t04Flag( pFlag ,"seq2" )
ObjSeq2$simul <- t04_mlpSimul( ObjSeq2$flag ,nnOpt ,pSimulNum=100 ,pSimulSetNum=5 )
ObjSeq2$report <- t04_report( ObjSeq2$simul ,ObjSeq2$flag ,pReportFile="./temp/seq2" )

# save( ObjSeq2 ,file="./temp/Obj_seq2.save" )


# 정확해야 하는 위치 검색(hotP1, hotP0)
pFlag <- ObjSeq2$flag$flag
hotP1 <- which(pFlag==1)
hotP1 <- hotP1[(hotP1-1)>0]
hotP1 <- hotP1[(pFlag[hotP1]==pFlag[hotP1-1])]

hotP0 <- hotP1+1
hotP0 <- hotP0[hotP0<=length(pFlag)]

# 정확해야 하는 위치들에 대해 ANN이 제시한 확률조사
# Beyond simulRange
assDf.rownames <- rownames(p$report$assDf)
assDf.hotP1 <- assDf.rownames[assDf.rownames %in% as.character(hotP1)]
assDf.hotP0 <- assDf.rownames[assDf.rownames %in% as.character(hotP0)]

rStr <- sprintf("Spot 1 Hit %d of %d" ,sum(p$report$assDf[assDf.hotP1,"isHit"]) ,length(assDf.hotP1) )
print(rStr)
rStr <- sprintf("Spot 0 Hit %d of %d" ,sum(p$report$assDf[assDf.hotP0,"isHit"]) ,length(assDf.hotP0) )
print(rStr)

# 정확해야 하는 위치들에 대한 명중률 추정
boxplot( p$report$assDf[assDf.hotP1,"rate"] ,p$report$assDf[assDf.hotP0,"rate"]
		,names=c("hotP1","hotP0")	,ylim=c(0,1)
	)

