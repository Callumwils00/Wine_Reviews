if(!require(tidyverse)){library(tidyverse)}
Data <- read_csv("winemag-data_first150k.csv")
Data$url <- paste0("https://en.wikipedia.org/wiki/str_to_title", "/", eval(str_replace_all(Data$variety, " ", "_")))

wine_words <- c("Acidity","Acid", "Angular", "Austere", "Barnyard", "Big", "Bright",
  "Buttery", "Butter", "Cassis", "Charcoal", "Chewy Tannins", "Chewy",
  "Tannin", "Cigar Box", "Cigar", "complex", "creamy", "cream",
  "crisp", "dense", "earthy", "earth", "elegant", "fat", "flabby", "flab",
  "flamboyant", "fleshy", "flesh", "food friendly", "food", "grippy tannins",
  "grip", "Intellectually satisfying", "intellectual", "satisfying",
  "jammy", "juicy", "Laser-like", "Laser", "Lees", "Minerally", "Mineral",
  "Oaked", "Oak", "Opulent", "Refined", "Silky", "Silk", "Steely", "Steel",
  "Structured", "Structure", "Tight", "Toasty", "Toast", "Unctuous", "Unoaked",
  "velvety")

Data$similarity_count <- 0
Data <- Data[,c("description", "variety", "similarity_count")]
Data$variety <- as.character(Data$variety)
Data2 <- Data %>% 
  mutate(Description2 = if_else(str_detect(description,paste0(wine_words, collapse = "|")),
                               as.list(str_extract_all(description, paste0(wine_words, collapse = "|"))),
                               as.list(NA_character_))) %>%
  filter(is.na(Description2) == FALSE)
description_one <- unlist(paste(toupper(Data2$Description2)))
similarity_count_one <- numeric(length(description_one))
