SubstrateIndex <- function(Substrate = NA,
                           Vegetation = NA,
                           Silt = NA,
                           Sand = NA,
                           Fine.gravel = NA,
                           Gravel = NA,
                           Cobbles = NA,
                           Boulders = NA,
                           Bed.rock = NA, check.completeness = TRUE) {
  if (!is.na(Substrate) &
    any(
      !is.na(Vegetation),
      !is.na(Silt),
      !is.na(Sand),
      !is.na(Fine.gravel),
      !is.na(Gravel),
      !is.na(Cobbles),
      !is.na(Boulders),
      !is.na(Bed.rock)
    )) {
    stop("Data may be duplicated")
  }

  if (!is.na(Substrate)) {
    if(check.completeness)
    {
      if(any(apply(Substrate,1,function(x){sum(x, na.rm=T)})!=100))
        stop("Some records do not sum up to 100")
    }
    
    Substrate.index <- apply(Substrate[, c("sand", "fine.gravel", "gravel", "cobbles", "boulders", "bed.rock")], 1, function(x) {
      sum((x / 100) * c(3:8), na.rm = T)
    })
  } else {
    Substrate <- data.frame(sand, fine.gravel, gravel, cobbles, boulders, bed.rock)
    
    if(check.completeness)
    {
      if(any(apply(Substrate,1,function(x){sum(x, na.rm=T)})!=100))
        stop("Some records do not sum up to 100")
    }
    
    Substrate.index <- apply(Substrate, 1, function(x) {
      sum((x / 100) * c(3:8), na.rm = T)
    })
  }
  return(data.frame(Substrate.index = Substrate.index))
}

SubstrateIndex(
  Substrate = NA,
  vegetation = NA,
  silt = NA,
  sand = 20,
  fine.gravel = NA,
  gravel = 25,
  cobbles = NA,
  boulders = NA,
  bed.rock = 75,
  check.completeness = TRUE
)
