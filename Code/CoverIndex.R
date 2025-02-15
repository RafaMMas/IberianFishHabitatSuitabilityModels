Cover.dataset.example <- read.table("K:/Revisión curvas de idoneidad/IberianFishHabitatSuitabilityModels functions/IberianFishHabitatSuitabilityModels/Hydraulics/Serpis.cover.txt",
                                    header = T, sep = "\t")


Selected.cover.types


Cover.index <- function(Covers = NA,
                        Leaves = NA,
                        Algae = NA,
                        Root = NA,
                        Aquatic.vegetation = NA,
                        Reed = NA,
                        Wood = NA,
                        Sand = NA,
                        Rock = NA,
                        Cave  = NA,              
                        Shade = NA, Species, Size.class = c("Large", "Medium", "Small", "Very small", "Spawning"), Model){
  
  if (!is.na(Covers) &
      any(
        !is.na(Leaves),
        !is.na(Algae),
        !is.na(Root),
        !is.na(Aquatic.vegetation),
        !is.na(Reed),
        !is.na(Wood),
        !is.na(Sand),
        !is.na(Rock),
        !is.na(cave),
        !is.na(Shade)
      )) {
    stop("Data may be duplicated")
  }  

  data(Selected.cover.types)
  
  Selected.cover.types <- Selected.cover.types[Selected.cover.types$Species == Species &
                                               Selected.cover.types$Size.class == Size.class  &
                                               Selected.cover.types$Model == Model  , c("Leaves",
                                                                                                  "Algae",
                                                                                                  "Root",
                                                                                                  "Aquatic.vegetation",
                                                                                                  "Reed",
                                                                                                  "Wood",
                                                                                                  "Sand",
                                                                                                  "Rock",
                                                                                                  "Cave",              
                                                                                                  "Shade")]
  remove("Selected.cover.types")
  
  if (!is.na(Covers)) {
    Cover <- apply(Covers[,Selected.cover.types], 1, sum)
  } else {
    Covers <- data.frame(Leaves,
                         Algae,
                         Root,
                         Aquatic.vegetation,
                         Reed,
                         Wood,
                         Sand,
                         Rock,
                         Cave,              
                         Shade)
    Cover <- apply(Covers[,Selected.cover.types], 1, sum)
  }  
  
  return(Cover)
  
}



Cover.index <- function(Covers = NA,
                        Leaves = NA,
                        Algae = NA,
                        Root = NA,
                        Aquatic.vegetation = NA,
                        Reed = NA,
                        Wood = NA,
                        Sand = NA,
                        Rock = NA,
                        Cave  = NA,              
                        Shade = NA, Species, Size.class, model)


