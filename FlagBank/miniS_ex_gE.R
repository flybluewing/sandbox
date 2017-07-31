source("miniS_H.R")

k.FLogOpt$defaultLogFile <- "./log/Gen_miniS.log"
#k.FLogOpt <- k.getFLogOpt( ) # 로깅 옵션 정상으로 돌리고자 할 때.


myObj <- load("Obj_miniS0125.save")
# miniS
mSPool <- gE.defineSpecies( pPop=400 ,pDnaType=1:length(miniS$fltRstLst) 
							,pDnaSize=20 ,pDnaReplace=F
                            ,pAsIndiF="assIndiF.mOpt"
                            ,pAsIndiF_Opt=c( 10 ,35 ,5 ,50 )
							,pClassName="miniS" 
							,pAsF="assF.mean"
						)

mSPool$pool	<- gE.createPool( mSPool ,miniS$fltRstLst )
chkNA <- apply(mSPool$pool$childMtx ,1 ,function(p){all(is.na(p))})
if( any(chkNA) ){
	k.FLog("dkdkd",pConsole=T)
}

# for Testing
# mSPool$ass	<- gE.assessPool( mSPool )
# mSPool$pool$childMtx	<- gE.nextGen( mSPool )

genLength <- 500
scoreTrend <- rep(0,genLength)
assLst <- list()
mtxLst <- list()
k.FLogStr("Start evolve",pConsole=T)
for( gIdx in 1:genLength ){
	chkNA <- apply(mSPool$pool$childMtx ,1 ,function(p){all(is.na(p))})
	if( any(chkNA) ){
		k.FLogStr(sprintf("all NA child found!! %d",sum(chkNA)),pConsole=T)
	}
	
	mSPool$ass	<- gE.assessPool( mSPool )
	k.FLogStr("    assessment done.")
	# assLst[[gIdx]] <- mSPool$ass
	# mtxLst[[gIdx]] <- mSPool$pool$childMtx
	scoreTrend[gIdx] <- mSPool$ass$survive.mean
	if( gIdx<genLength ){	# 마지막 순간에는 childMtx 보존필요.
		mSPool$pool$childMtx	<- gE.nextGen( mSPool )
	}
	k.FLogStr(sprintf("Evo Gen : %d th. score:%f",gIdx,mSPool$ass$survive.mean))
}

mSPool$scoreTrend <- scoreTrend

save( mSPool ,file="Obj_mSPool.save")


gE.rptPool.miniS( mSPool )
for( idx in mSPool$ass$survive.idx )
	gE.rptIndividual.miniS( mSPool ,idx )

plot( scoreTrend )