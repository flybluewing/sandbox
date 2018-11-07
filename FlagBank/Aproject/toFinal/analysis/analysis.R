#	개선 구조는 826 부터 적용되어 있음.
tStmp <- Sys.time()
for( hIdx in c( 809, 814, 816, 819, 820:831 ) ){
	tDiff <- Sys.time() - tStmp
	cat(sprintf("start hIdx %d (%.1f%s)\n",hIdx,tDiff,units(tDiff) ))
	source(sprintf("./toFinal/toZ%d_H.R",hIdx) )
	u0.saveStdZoidFltRst( hIdx )
}
#		trouble c( 819 ,822 ,827 ,828 ,829 ,830 ,831)


#	rpt.analyLst( analyLst[["toZ821"]] )
rpt.analyLst <- function( analy ,rptId=NULL ){
	#	analy <- analyLst[["toZ821"]]
	if( is.null(rptId) ){
		cat( sprintf( "* stdFltCnt:%d \n" ,analy$stdFltCnt )  )
	} else {
		cat( sprintf( "*<%s> stdFltCnt:%d \n" ,rptId ,analy$stdFltCnt )  )
	}
	cat( sprintf( "    default:%d \n" ,analy$fCutCnt.default )  )
	cat( sprintf( "    colValSeqNext:%d \n" ,analy$fCutCnt.colValSeqNext )  )
	cat( sprintf( "    colValSeqNext.cStep:%d \n",analy$fCutCnt.colValSeqNext.cStep	) )

	phaseNm <- attributes(analy)$names
	exctPhNmIdx <- c( grep("colValSeqNext",phaseNm) ,grep("^stdFltCnt$",phaseNm) ,grep("^fCutCnt.default$",phaseNm) )
	phaseNm <- phaseNm[-exctPhNmIdx]
	scoreMtx <- do.call( rbind, 
							lapply(phaseNm,function(nm){
										do.call( c ,analy[[nm]] )
									})
						)
	rownames(scoreMtx) <- gsub("^fCutCnt\\.","",phaseNm)
	rptStr <- sprintf("    %s",capture.output(scoreMtx))
	cat( sprintf("%s \n",paste(rptStr,collapse="\n")) )
	return( scoreMtx )
} # rpt.analyLst()


#	tgtGold <- c("toZ809","toZ814","toZ816","toZ820","toZ823","toZ825")
#	tgtLate <- c("toZ819","toZ820","toZ821","toZ822","toZ823","toZ824","toZ825")
# 	mtxLst <- lapply( tgtLate
# 						,function(nm){ rpt.analyLst( analyLst[[nm]] ,nm ) }
# 					)
# 	for( nIdx in colnames( mtxLst[[1]] ) ){
# 		cat( sprintf("name : %s\n",nIdx) )
# 		mtx <- do.call( rbind ,lapply( mtxLst ,function(mtx){mtx[,nIdx]}) )
# 		rptStr <- sprintf("  %s",capture.output(mtx))
# 		cat( sprintf("%s \n",paste(rptStr,collapse="\n")) )
# 	}

# 	fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx ,rpt=T )

# 	stdMI$rawTail
# 	fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )
# 	auxCntMtx
# 	cntMtx


analyLst <- list()
# [gold] ---------------------------------------
#	toZ800	: 0		toZ801	: 0		toZ806	: 1
analyLst[["toZ809"]] <- list(	stdFltCnt = 1
	,fCutCnt.basic = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =1	,fStep =0
				)
	,fCutCnt.colValSeqNext =NA
	,fCutCnt.colValSeqNext.cStep =NA
	,fCutCnt.nextZW = list(	commonCutCnt=0	# reb / / c23
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =0
				)
	,fCutCnt.nextQuo10 = list(	commonCutCnt=0	# reb	/	/
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =1	,fStep =0
				)
	,fCutCnt.nextBin = list(	commonCutCnt=1	# 	/ spanM	/
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =2	,cStep =0	,fStep =0
				)
	,fCutCnt.nextRebNum = list(	commonCutCnt=0	#	/	/
					,auxZW=1	,auxQuo=0
					,raw =2	,rawFV =1	# fCutU.hasPtn(c(15,NA,23),aZoid)
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.nextCStepBin = list(	commonCutCnt=1	# 	/ spanM /
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =2	,cStep =0	,fStep =2
				)
	,fCutCnt.nextFStepBin = list(	commonCutCnt=1	# reb / quoPtn /
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_1 = list(	commonCutCnt=1	# 	/ spanM	/ c23
					,auxZW=1	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_2 = list(	commonCutCnt=1	# 	/ quoPtn / 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_3 = list(	commonCutCnt=1	# 	/ zw	/
					,auxZW=0	,auxQuo=1
					,raw =1	,rawFV =0
					,rem =0	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_4 = list(	commonCutCnt=1	# 	/ zw	/ 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =2	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_5 = list(	commonCutCnt=1	# 	/ quoPtn /
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =1	,cStep =0	,fStep =1
				)
	,fCutCnt.nextColVal_6 = list(	commonCutCnt=0	#	/	/
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =1	,fStep =0
				)
) # analyLst[["toZ809"]]  check

analyLst[["toZ814"]] <- list(	stdFltCnt = 1
	,fCutCnt.basic = list(	commonCutCnt=0	# reb / 	/ c25
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =2	,cStep =0	,fStep =0
				)
	,fCutCnt.colValSeqNext =NA
	,fCutCnt.colValSeqNext.cStep =NA
	,fCutCnt.nextZW = list(	commonCutCnt=1	# reb / spanM / c25
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =1	# fStep( *2 1/2)
				)
	,fCutCnt.nextQuo10 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =1	,fStep =0	# cStep( *1 )
				)
	,fCutCnt.nextBin = list(	commonCutCnt=0	# reb /	/
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =2	,cStep =1	,fStep =0
				)
	,fCutCnt.nextRebNum = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=1
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =0
				)
	,fCutCnt.nextCStepBin = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =2	,cStep =1	,fStep =0
				)
	,fCutCnt.nextFStepBin = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_1 = list(	commonCutCnt=0	# reb /	/
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =2	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_2 = list(	commonCutCnt=2	# 	/ nbor spanM / 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_3 = list(	commonCutCnt=1	# 	/ nbor	/ 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_4 = list(	commonCutCnt=0	#	/	/ c25
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =1	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_5 = list(	commonCutCnt=0	# reb /	/ c25
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_6 = list(	commonCutCnt=0	#	/	/
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =1	,cStep =0	,fStep =0
				)
) # analyLst[["toZ814"]]

analyLst[["toZ816"]] <- list(	stdFltCnt = 0
	,fCutCnt.basic = list(	commonCutCnt=0	#	/	/	
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.colValSeqNext =NA
	,fCutCnt.colValSeqNext.cStep =NA
	,fCutCnt.nextZW = list(	commonCutCnt=0	#	/	/
					,auxZW=0	,auxQuo=1
					,raw =0	,rawFV =0
					,rem =1	,cStep =1	,fStep =0
				)
	,fCutCnt.nextQuo10 = list(	commonCutCnt=0	#	/	/ 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =2	,cStep =1	,fStep =0
				)
	,fCutCnt.nextBin = list(	commonCutCnt=0	#	/	/ 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =1
				)
	,fCutCnt.nextRebNum = list(	commonCutCnt=0	# reb /	/ 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =1	,fStep =0	# cStep(*1)
				)
	,fCutCnt.nextCStepBin = list(	commonCutCnt=0	#	/	/ 
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.nextFStepBin = list(	commonCutCnt=0	# reb /	/ c23
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =2	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_1 = list(	commonCutCnt=0	# reb /	/ 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =1	,fStep =0	# cStep(*1)
				)
	,fCutCnt.nextColVal_2 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =0	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_3 = list(	commonCutCnt=1	# reb / spanM /
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =2	# 2 * fCutU.hasPtn(c(18,19))
					,rem =1	,cStep =3	,fStep =0	# cStep(*1)
				)
	,fCutCnt.nextColVal_4 = list(	commonCutCnt=0	# reb /	/
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_5 = list(	commonCutCnt=0	# reb /	/ 
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =2	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_6 = list(	commonCutCnt=0	# reb /	/ 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =1	,fStep =0
				)
) # analyLst[["toZ816"]]

# analyLst[["toZ820"]] : 1
# analyLst[["toZ823"]] : 1



# [late] ---------------------------------------
analyLst[["toZ819"]] <- list(	stdFltCnt =2
	,fCutCnt.basic = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =0
				)
	,fCutCnt.colValSeqNext =2
	,fCutCnt.colValSeqNext.cStep =0
	,fCutCnt.nextZW = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =1	,fStep =0
				)
	,fCutCnt.nextQuo10 = list(	commonCutCnt=1	# 	/ quoPtn / 
					,auxZW=0	,auxQuo=0
					,raw =2	,rawFV =0
					,rem =0	,cStep =0	,fStep =1
				)
	,fCutCnt.nextBin = list(	commonCutCnt=1	# reb / spanM / c25
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =0
				)
	,fCutCnt.nextRebNum = list(	commonCutCnt=0	# reb /	/ 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.nextCStepBin = list(	commonCutCnt=0	# reb /	/ c25
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =1	,cStep =1	,fStep =0
				)
	,fCutCnt.nextFStepBin = list(	commonCutCnt=1	# reb / spanM / 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =2	,fStep =0
				)
	,fCutCnt.nextColVal_1 = list(	commonCutCnt=2	# 	/ zw / c25 max2
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_2 = list(	commonCutCnt=0	#	/	/ c25
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =2	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_3 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=1
					,raw =0	,rawFV =0
					,rem =0	,cStep =2	,fStep =0
				)
	,fCutCnt.nextColVal_4 = list(	commonCutCnt=1	# reb / spanM / 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =2	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_5 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =3	,fStep =1	# cStep(*1)
				)
	,fCutCnt.nextColVal_6 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =2	,fStep =0	# cStep(*1)
				)
) # analyLst[["toZ819"]] check

analyLst[["toZ820"]] <- list(	stdFltCnt = 1	# (A0110.A 필터 제외)
	,fCutCnt.basic = list(	commonCutCnt=1	# 	/ cStep2 /
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =1	,fStep =0	# cStep(*1)
				)
	,fCutCnt.colValSeqNext = 0
	,fCutCnt.colValSeqNext.cStep = 0
	,fCutCnt.nextZW = list(	commonCutCnt=1	# reb / quoPtn / 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =2	,cStep =1	,fStep =0
				)
	,fCutCnt.nextQuo10 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =2	,cStep =0	,fStep =0
				)
	,fCutCnt.nextBin = list(	commonCutCnt=1	# 	/ quoPtn / 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =0
				)
	,fCutCnt.nextRebNum = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =1	# fStep(*1)
				)
	,fCutCnt.nextCStepBin = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =0
				)
	,fCutCnt.nextFStepBin = list(	commonCutCnt=2	# / zw cStep2 / 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_1 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_2 = list(	commonCutCnt=2	# reb / zw remH1 /
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =1	# fStep( *2:1/2 )
				)
	,fCutCnt.nextColVal_3 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_4 = list(	commonCutCnt=0	# reb /	/ 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_5 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_6 = list(	commonCutCnt=0	# reb /	/ 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =1	,fStep =0
				)
) # analyLst[["toZ820"]]

analyLst[["toZ821"]] <- list(	stdFltCnt = 3
	,fCutCnt.basic = list(	commonCutCnt=3	# 	/ quoAll cStep2 cStep3 / 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.colValSeqNext =4
	,fCutCnt.colValSeqNext.cStep =0
	,fCutCnt.nextZW = list(	commonCutCnt=0	#	/	/ c21
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =1	,fStep =0
				)
	,fCutCnt.nextQuo10 = list(	commonCutCnt=0	# reb /	/ 
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =1	,cStep =0	,fStep =1
				)
	,fCutCnt.nextBin = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =0
				)
	,fCutCnt.nextRebNum = list(	commonCutCnt=1	# 	/ quoPtn / c31 c21 c22 c25
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =0
				)
	,fCutCnt.nextCStepBin = list(	commonCutCnt=1	#	/ quoAll / c21
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =1	,cStep =0	,fStep =1
				)
	,fCutCnt.nextFStepBin = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =2	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_1 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =0	,cStep =1	,fStep =0	# cStep(*1)
				)
	,fCutCnt.nextColVal_2 = list(	commonCutCnt=1	# reb / quoPtn /
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =0	,cStep =2	,fStep =0	# cStep(*1)
				)
	,fCutCnt.nextColVal_3 = list(	commonCutCnt=0	# reb /	/ c22
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_4 = list(	commonCutCnt=1	# 	/ quoPtn / 
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =2	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_5 = list(	commonCutCnt=1	# 	/ zw / c21 c25
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =3	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_6 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =2	,cStep =0	,fStep =0
				)
) # analyLst[["toZ821"]]

analyLst[["toZ822"]] <- list(	stdFltCnt = 3
	,fCutCnt.basic = list(	commonCutCnt=0	# reb /	/ c25
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =1	,fStep =0
				)
	,fCutCnt.colValSeqNext =1
	,fCutCnt.colValSeqNext.cStep =3
	,fCutCnt.nextZW = list(	commonCutCnt=2	# reb / spanM quoPtn  / c34 c25
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =2	,cStep =0	,fStep =0
				)
	,fCutCnt.nextQuo10 = list(	commonCutCnt=0	# reb /	/ 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =1	,fStep =0	# cStep(*2)
				)
	,fCutCnt.nextBin = list(	commonCutCnt=1	# 	/ quoPtn / c24
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =2	,fStep =0	# cStep(*2)
				)
	,fCutCnt.nextRebNum = list(	commonCutCnt=0	# reb /	/ 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =0
				)
	,fCutCnt.nextCStepBin = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.nextFStepBin = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =2	,fStep =0
				)
	,fCutCnt.nextColVal_1 = list(	commonCutCnt=0	#	/	/ c33
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =2,fStep =0
				)
	,fCutCnt.nextColVal_2 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =2	,cStep =2	,fStep =0	# cStep(*2)
				)
	,fCutCnt.nextColVal_3 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_4 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =0	,fStep =2
				)
	,fCutCnt.nextColVal_5 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_6 = list(	commonCutCnt=1	# 	/ cStep2 / 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =1	,fStep =0	# cStep(*1)
				)
) # analyLst[["toZ822"]]

analyLst[["toZ823"]] <- list(	stdFltCnt =1
	,fCutCnt.default =0
	,fCutCnt.basic = list(	commonCutCnt=0	# reb /	/ 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.colValSeqNext =0
	,fCutCnt.colValSeqNext.cStep =0
	,fCutCnt.nextZW = list(	commonCutCnt=2	# 	/ nbor quoAll / 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =1	# fCutU.hasPtn(c(12,NA,24),aZoid)
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.nextQuo10 = list(	commonCutCnt=1	# 	/ spanM / 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =1	,fStep =0
				)
	,fCutCnt.nextBin = list(	commonCutCnt=0	#	/	/ c25
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =3	,fStep =0	# cStep(*1)
				)
	,fCutCnt.nextRebNum = list(	commonCutCnt=1	# reb /	/ 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.nextCStepBin = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =0
				)
	,fCutCnt.nextFStepBin = list(	commonCutCnt=1	# 	/ cStep2 / 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =2	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_1 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =1,fStep =0
				)
	,fCutCnt.nextColVal_2 = list(	commonCutCnt=1	# 	/ quoAll / 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =2	,cStep =1	,fStep =1	# fStep(*2)
				)
	,fCutCnt.nextColVal_3 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =1	,cStep =2	,fStep =0
				)
	,fCutCnt.nextColVal_4 = list(	commonCutCnt=0	# reb /	/	
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =1	,fStep =1
				)
	,fCutCnt.nextColVal_5 = list(	commonCutCnt=1	# reb /	spanM / c21
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_6 = list(	commonCutCnt=1	# reb / spanM / c21
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =2	,fStep =0
				)
) # analyLst[["toZ823"]]

analyLst[["toZ824"]] <- list(	stdFltCnt =3
	,fCutCnt.default =0
	,fCutCnt.basic = list(	commonCutCnt=0	#  reb /	/ 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =0
				)
	,fCutCnt.colValSeqNext =1	# colValSeqNext( pColSize=2 ) cStep
	,fCutCnt.colValSeqNext.cStep =0
	,fCutCnt.nextZW = list(	commonCutCnt=2	# 	/spanM cStep2/ 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0	# 
					,rem =1	,cStep =2	,fStep =0
				)
	,fCutCnt.nextQuo10 = list(	commonCutCnt=0	# 	/ / 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =2	,cStep =1	,fStep =0
				)
	,fCutCnt.nextBin = list(	commonCutCnt=0	#	/	/ c21
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.nextRebNum = list(	commonCutCnt=0	# reb /	/ 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.nextCStepBin = list(	commonCutCnt=1	#	/ spanM / 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =0
				)
	,fCutCnt.nextFStepBin = list(	commonCutCnt=1	# 	/ spanM / 
					,auxZW=0	,auxQuo=1
					,raw =0	,rawFV =0
					,rem =2	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_1 = list(	commonCutCnt=0	# reb /	/ 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =0,fStep =0
				)
	,fCutCnt.nextColVal_2 = list(	commonCutCnt=0	# reb /	/ c21 c25
					,auxZW=0	,auxQuo=1
					,raw =1	,rawFV =0
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_3 = list(	commonCutCnt=1	#     / cStep2 / 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =2	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_4 = list(	commonCutCnt=0	# reb /	/ c22
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_5 = list(	commonCutCnt=1	#     /	cStep2 / 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =1	,fStep =1
				)
	,fCutCnt.nextColVal_6 = list(	commonCutCnt=0	#     /  / 
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =0	,cStep =1	,fStep =1	# fStep(*1)
				)
) # analyLst[["toZ824"]]

analyLst[["toZ825"]] <- list(	stdFltCnt =0
	,fCutCnt.default =0
	,fCutCnt.basic = list(	commonCutCnt=0	# reb  /	/ 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =1	,fStep =0
				)
	,fCutCnt.colValSeqNext =1	# 
	,fCutCnt.colValSeqNext.cStep =0
	,fCutCnt.nextZW = list(	commonCutCnt=0	# 	// 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0	# 
					,rem =2	,cStep =0	,fStep =0
				)
	,fCutCnt.nextQuo10 = list(	commonCutCnt=0	# reb / / 
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =0	,cStep =1	,fStep =1	# fStep(*2)
				)
	,fCutCnt.nextBin = list(	commonCutCnt=0	#	/	/ 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.nextRebNum = list(	commonCutCnt=1	#  /quoPtn/ 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =1	,fStep =1	# fStep(*2)
				)
	,fCutCnt.nextCStepBin = list(	commonCutCnt=0	#	/  / 
					,auxZW=0	,auxQuo=1
					,raw =0	,rawFV =0
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.nextFStepBin = list(	commonCutCnt=0	# 	/ / 
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =2	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_1 = list(	commonCutCnt=1	#  /quoPtn/ 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =1,fStep =0
				)
	,fCutCnt.nextColVal_2 = list(	commonCutCnt=1	# reb /quoPtn/ 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_3 = list(	commonCutCnt=0	# reb /  / c24
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =3	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_4 = list(	commonCutCnt=0	#  /	/ 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_5 = list(	commonCutCnt=2	# reb /(remH0,cStep2)/ c22
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =2	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_6 = list(	commonCutCnt=0	#     /  / 
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =0	,cStep =0	,fStep =0	# 
				)
) # analyLst[["toZ825"]]


analyLst[["toZ825"]] <- list(	stdFltCnt =0
	,fCutCnt.default =0
	,fCutCnt.basic = list(	commonCutCnt=0	# reb  /	/ 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =1	,fStep =0
				)
	,fCutCnt.colValSeqNext =1	# 
	,fCutCnt.colValSeqNext.cStep =0
	,fCutCnt.nextZW = list(	commonCutCnt=0	# 	// 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0	# 
					,rem =2	,cStep =0	,fStep =0
				)
	,fCutCnt.nextQuo10 = list(	commonCutCnt=0	# reb / / 
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =0	,cStep =1	,fStep =1	# fStep(*2)
				)
	,fCutCnt.nextBin = list(	commonCutCnt=0	#	/	/ 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.nextRebNum = list(	commonCutCnt=1	#  /quoPtn/ 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =1	,fStep =1	# fStep(*2)
				)
	,fCutCnt.nextCStepBin = list(	commonCutCnt=0	#	/  / 
					,auxZW=0	,auxQuo=1
					,raw =0	,rawFV =0
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.nextFStepBin = list(	commonCutCnt=0	# 	/ / 
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =2	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_1 = list(	commonCutCnt=1	#  /quoPtn/ 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =1,fStep =0
				)
	,fCutCnt.nextColVal_2 = list(	commonCutCnt=1	# reb /quoPtn/ 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_3 = list(	commonCutCnt=0	# reb /  / c24
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =3	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_4 = list(	commonCutCnt=0	#  /	/ 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_5 = list(	commonCutCnt=2	# reb /(remH0,cStep2)/ c22
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =2	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_6 = list(	commonCutCnt=0	#     /  / 
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =0	,cStep =0	,fStep =0	# 
				)
) # analyLst[["toZ85"]]


analyLst[["toZ826"]] <- list(	stdFltCnt =NA
	,fCutCnt.default =NA
	,fCutCnt.basic = list(	commonCutCnt=NA	#   /	/ 
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.colValSeqNext =NA	# 
	,fCutCnt.colValSeqNext.cStep =NA
	,fCutCnt.nextZW = list(	commonCutCnt=NA	#     /  / 
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA	# 
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextQuo10 = list(	commonCutCnt=NA	#     /  / 
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextBin = list(	commonCutCnt=NA	#     /  / 
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextRebNum = list(	commonCutCnt=NA	#     /  / 
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextCStepBin = list(	commonCutCnt=NA	#     /  / 
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextFStepBin = list(	commonCutCnt=NA	#     /  / 
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextColVal_1 = list(	commonCutCnt=NA	#     /  / 
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA,fStep =NA
				)
	,fCutCnt.nextColVal_2 = list(	commonCutCnt=NA	#     /  / 
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextColVal_3 = list(	commonCutCnt=NA	#     /  / 
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextColVal_4 = list(	commonCutCnt=NA	#     /  / 
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextColVal_5 = list(	commonCutCnt=NA	#     /  / 
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextColVal_6 = list(	commonCutCnt=NA	#     /  / 
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
) # analyLst[["toZ86"]]

