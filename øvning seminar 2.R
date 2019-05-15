library(tidyverse)
%>% 
library(vegan) # datasett

data("BCI", package = "vegan")

x11() # plot kommer i nytt vindu

# bra måte å skrive koden på
plot(  
  sort(
    colSums(BCI)
    , decreasing = TRUE
    )
  )

# bra måte å skrive koden på, i små steg istdenfor alle samtidig.
x1 <- colSums(BCI) 
x2 <- sort(x1, decreasing = TRUE)
plot(x2)

# pipes, bedre alternativ for å skrive koden. Pipes istedenfor parantes. Begynner på toppen og jobber seg nedover.
BCI %>% # datasettet som skal inn i parantesene under.
  colSums() %>% 
  sort(decreasing = TRUE) %>% # hvis du ikke bruker denne vil grafen gå andre veien.
  plot()

####one table functions ####

iris <- as_tibble(iris) #datasett. Tibble er for å organisere dataene bedre. Forteller hvilken data som er i hver kolonne.
iris

#select
iris %>% select(Sepal.Length, Species)
.Last.value %>% view()

iris %>% select(-Sepal.Width) #fjerne sepal.witdh fra analysen/datasettet.
iris %>% select(Sepal.Width:Petal.Length) # Sepal.Width er start-kolonnen og Petal.Length er slutt-kolonnen.

iris %>% rename(Sepal.length = Sepal.Length, spp = Species) # nytt navn = gammelt navn. Må være "lovlig" navn.
iris %>% rename(`sepal length` = Sepal.Length) # bruk `` kun når du skal lage en tabell og liksom er ferdig med kodingen. 
iris %>%  rename(`1` = Sepal.Length)

#filtrere noen rekker
iris %>%  filter(Sepal.Length > 5, Petal.Length < 2) %>% 
  select(Species) 

# mutate
iris %>% mutate(petal.area = Petal.Length * Petal.Width) # lage ny kolonne eller forandre den eksisterende.
iris %>%  mutate(Species = toupper(Species))

#
iris %>% group_by(Species) %>% # gruppering
  summarise(mean_petal_length = mean(Petal.Length), sd_petal_length = sd(Petal.Length))
iris %>% group_by(Species) %>% 
  mutate(mean_petal_length = mean(Petal.Length)) %>% 
  ungroup() # ta vekk grupperingen

iris %>% arrange(Petal.Length)
iris %>% arrange(desc(pental.length))

iris %>% group_by(Species) %>% arrange(Petal.Length) %>%  slice(1:3)

iris %>% group_by(Species) %>%  nest() # tibble inside tibble. 

iris %>% 
  group_by(Species) %>%  
  nest() %>% # fitted a model to each species. 
  mutate(mod = map (data, ~lm(Sepal.Length ~ Sepal.Width, data = .))) %>% # ~tilde
  mutate(coef = map(mod, broom::tidy)) %>% # trekker intercept etc. tabellen ut av modellen. 
  unnest(coef)

# gather and spread
iris
iris %>% #gjør det om til en kolonne:
  rownames_to_column() %>% 
  gather(key = variable, value = measurement, -Species, -rowname) %>%
  group_by(Species, variable) %>% 
  summarise(mean = mean(measurement))
x11()
iris %>% 
  gather(key = variable, value = measurement, - Species, -rowname) %>%
  ggplot(aes(x = variable, y = measurement, fill = Species)) + geom_violin()

  



