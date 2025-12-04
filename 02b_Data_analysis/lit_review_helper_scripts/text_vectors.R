# Define words to remove
filler_words <- c(
  #Prepositions
  "in", "on", "at", "by", "between", "of", "to", "over",
  "for", "with", "within", "from", "across", "among",
  #Articles
  "a", "the", "that", "this", "these", "those",
  #Conjunctions
  "and", "or",
  #Numbers
  "one", "single", "both", "three", "multiple", "various", "most",
  "either", "every", "multi", "2", "3", "4", "individual", "first", "second", 
  "two", "four", "all", "some", "several", "particular", "successive", 
  #Comparison
  "as","different","same","distinct",
  #Other
  "each","per", 
  "any", "given", "following",
  "subsequent","during","climatic",
  "consecutive", "specific","than","certain","contrasting",
  "previous", "other")

temporal_phases <- c("early", "mid", "late", 
                     "end-of", "whole", "entire", "normal", "peak")

#Define season types
temperate <- c("spring", "summer", "autumn", "winter", "fall",
               "winter-spring", "summer-autumn")
flora <- c("wintering", "growth", "growing", "non-growing", 
           "vegetative", "productive", "non-productive",
           "dormant", "bloom", "blooming", "vegetation", "fire")
fauna <- c("breeding", "spawning", "reproductive", "mating",
           "nesting","non-breeding", "migration","active", "migratory",
           "transmission")
ice <- c("ice-covered", "ice-free", "open-water", "melt", 
         "melting", "thaw", "snowmelt", "ice", "ice-cover", 
         "snow", "snow-free", "ablation", "thawing")
stratification <- c("stratified", "mixing")
temperature <- c("cold", "cool", "warm", "hot", "warmer", "heating","colder")
hydrology <- c("dry", "rainy", "wet","runoff", "hydrological", "postmonsoon",
               "post-rainy", "flood", "flooding", "high-water", "low-water", 
               "high-flow", "low-flow", "non-flood", "rainfall", "monsoon",
               "post-monsoon", "pre-monsoon", "non-monsoon", "drought")
agriculture <- c("irrigation", "rice-growing", "cropping", "crop",
                 "planting", "harvest", "wheat", "rice", "farming")
economic <- c("fishing", "angling", "closed","trapping",
              "hunting", "tourist", "bathing", "swimming")
research <- c("field", "sampling")