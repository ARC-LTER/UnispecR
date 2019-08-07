## 2013 % cover plots 

require(tidyverse)

cover_data <- read_csv("2013jmcover.csv") %>% slice(-1, -2, -3) %>% 
  mutate(cover = as.numeric(`Relative Cover`))



## Functional Groups 
species <- cover_data$Species %>% unique()
decid_shrubs <- c("Bet nan", "Sal pul", "Vac uli", "Rho lap", "Sal arc", "Sal gla", "Sal ret")
evgreen_shrubs <- c("And pol", "Led pal", "Cas tet", "Emp nig", "Vac vit", "Ped lap")
st_dead_shrubs <- c("St. D. Bet.", "St. D. Sal pul")
graminoids <- c("Poa arc", "Eri vag", "Car big", "Car mic","Car rot", "Cal lap", "Eri ang", "Arc lat")
forbs <- c("Arc alp", "Ped lab", "Pol bis", "Pol viv", "Rub cha", "Sax ang", "Sax dav", "Sax pun",
           "Ped cap", "Ped kan", "Ped oed", "Ped sp.", "Ped sud", "Pyr Sec", "Dry int",
           "Epi ang", "Tofieldia", "Tof coc", "Draba sp.", "Equ arv")
identity <- c("litter", "lichen", "moss", "frost boil", "bare", "vole litter")

other <- species[!species %in% c(decid_shrubs, evgreen_shrubs, graminoids, forbs, st_dead_shrubs,
                                 identity)]

other


## Calculate Means & Tag Functional Groups 
cover_means <- cover_data %>% 
  mutate(Treatment = factor(Treatment, levels = c("CT", "N", "P", "NP"))) %>% 
  group_by(Site, Block, Treatment, Plot, Species) %>% 
  summarize(cover = mean(cover, na.rm = T)) %>% filter(!is.na(cover)) %>% 
  group_by(Site, Treatment, Species) %>% 
  group_by(N = n(), add = TRUE) %>% # add number of blocks per site to get Standard Error
  summarize_at(vars(cover), funs(mean, sd), na.rm = T) %>% 
  mutate(Type = ifelse(Species %in% decid_shrubs, "Deciduous Shrubs", 
                       ifelse(Species %in% evgreen_shrubs, "Evergreen Shrubs",
                              ifelse(Species %in% st_dead_shrubs, "St. Dead Shrubs", 
                                     ifelse(Species %in% forbs, "Forbs",
                                            ifelse(Species %in% graminoids, "Graminoids",
                                                   ifelse(Species %in% identity, Species, "other"))))))) %>% 
  mutate(Type = factor(Type, levels = c("Forbs", "Evergreen Shrubs", "Deciduous Shrubs", "Graminoids", "St. Dead Shrubs",
                                        "litter", "lichen", "moss", "bare"))) 



ggplot(cover_means, aes(x=Treatment, y = mean)) + 
  geom_bar(aes(fill = Type), stat = "identity") + 
  scale_fill_manual(values = c("brown", "grey", "blue", "magenta", "dark blue", "orange", "dark green", "red", "black")) +
  facet_wrap(vars(Site))
