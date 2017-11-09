# 파일명 : seqAnaFun.R
#   setwd("c:/zproject/ISLRgit/FlagBank/breedingPlace/batch")
#	excId=1107		;anaId=0

curWd <- getwd()
setwd("./../..")
source("hCommon.R")
FB <- getFlagBank() # 사실 FB는 필요 없는데.. H 파일 땜시.
setwd(curWd)

setwd("./..")
source("20170917_A_H.R")
source("20171029_A_H.R")
source("20171029_A_auxH.R")
setwd(curWd)

args=(commandArgs(TRUE))
for( aIdx in seq_len(length(args)) ){
	eval(parse(text=args[aIdx]))
}
sprintf( "excId:%d anaId:%d" ,excId ,anaId )

myObj <- load("Obj_eleSet60.save")	# eleSet
myObj <- load("Obj_stdSeqObj.save")	# stdSeqObj


rem.val <- anaId	# remainder value
rem.base <- 3		# base of remainder

caf <- eleCafUtil.getNew( eleSet )
caf.part <- eleCafUtil.remainder( caf ,pRemainder=rem.val ,pBase=rem.base )

tgtSize <- slapply( caf.part ,function(p){sum(p)})
sprintf( "tgtSize:%d (%s)" ,sum(tgtSize) ,paste(tgtSize,collapse=",") )

tStmp <- Sys.time()
hAnaSet <- analyzeSeq( eleSet ,pCaf=caf.part ,pDebug=T )
tDiff <- Sys.time() - tStmp
sprintf("hAnaSet(remainder:%d/%d) %.1f %s",rem.val,rem.base,tDiff,units(tDiff))

hAnaSet$memo <- sprintf("hAnaSet(remainder:%d/%d) %.1f %s",rem.val,rem.base,tDiff,units(tDiff))
save( hAnaSet ,file=sprintf("Obj%d_hAnaSet%d.save",excId,rem.val) )

