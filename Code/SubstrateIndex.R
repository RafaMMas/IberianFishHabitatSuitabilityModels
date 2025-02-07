SubstrateIndex <- function(Substrate = NA,
                           vegetation = NA, 
                           silt = NA, 
                           sand = NA, 
                           fine.gravel = NA, 
                           gravel = NA, 
                           cobbles = NA, 
                           boulders = NA,
                           bed.rock = NA){
  
  if(!is.na(Substrate) & 
     any(!is.na(vegetation), 
         !is.na(silt), 
         !is.na(sand), 
         !is.na(fine.gravel), 
         !is.na(gravel), 
         !is.na(cobbles),
         !is.na(boulders),
         !is.na(bed.rock))){
    stop("Data may be duplicated")
  }
  
  if(!is.na(Substrate)){
    Substrate.index <- apply(Substrate[,c("sand", "fine.gravel", "gravel", "cobbles", "boulders", "bed.rock")], 1, function(x){
     sum((x/100)*c(3:8), na.rm = T)
    })
  } else {
    Substrate <- data.frame(sand, fine.gravel, gravel, cobbles, boulders, bed.rock)
    Substrate.index <- apply(Substrate, 1, function(x){
      sum((x/100)*c(3:8), na.rm = T)
  })
  }
  return(data.frame(Substrate.index = Substrate.index))
}

SubstrateIndex(Substrate = NA,
               vegetation = NA,
               silt = NA,
               sand = NA,
               fine.gravel = NA, 
               gravel = NA, 
               cobbles = NA, 
               boulders = NA,
               bed.rock = 100)

