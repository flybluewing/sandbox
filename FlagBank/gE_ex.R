source("gE_H.R")

# myObj <- load("Obj_miniS.save")	# miniS
head( miniS$fltRstLst[[1]] )
	#		hist val    score order isStd
	#	[1,]  128   1 94.73684  6.25     0
	#	[2,]  128   2 89.47368 12.50     0

mSpec <- gE.defineSpecies( pDnaType=1:length(miniS$fltRstLst) ,pClassName="miniS")

mSpec$pool <- gE.createPool( mSpec ,miniS$fltRstLst )


