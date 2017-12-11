# 20171116_A_H_pin.R

pin.rem <- function( pZh ,pBase=2 ){
    rObj <- list( idStr="pin.rem" )
    rObj$base <- pBase
    rObj$getBaseH <- function( pSrcH ){ 
                        remMtx <- pSrcH %% rObj$base 
                        return( remMtx[1:(nrow(remMtx)-1),]==remMtx[2:nrow(remMtx),] )
    } # rObj$getBaseH()
    rObj$baseH <- rObj$getBaseH( pZh )
        
    rObj$byLate <- function( pZoidMtx ,pZoidH ,pthld=8 ,pDebugInfo=F ){
        
    }

} # pin.rem()


