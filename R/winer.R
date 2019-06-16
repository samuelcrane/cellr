library(tidyverse)
library(summarytools)
library(httr)

# usethis::edit_r_environ()
url_list <- sprintf("https://www.cellartracker.com/xlquery.asp?User=%s&Password=%s&Format=csv&Table=List", 
               Sys.getenv("CELLAR_TRACKER_USER"), 
               Sys.getenv("CELLAR_TRACKER_PASS"))
url_availability <- sprintf("https://www.cellartracker.com/xlquery.asp?User=%s&Password=%s&Format=csv&Table=Availability", 
                    Sys.getenv("CELLAR_TRACKER_USER"), 
                    Sys.getenv("CELLAR_TRACKER_PASS"))

response_list <- httr::GET(url_list)
response_availability <- httr::GET(url_availability)

wine_cellar <- content(response_list, type = "text/csv", encoding = "ISO-8859-1")
availability <- content(response_availability, type = "text/csv", encoding = "ISO-8859-1")

#wine_cellar <- read_csv("My Cellar.csv", na = c("", "NA", 9999), locale = locale(encoding = "ISO-8859-1"))
#view(dfSummary(wine_cellar))

wine_cellar <- 
  wine_cellar %>% 
  unite("RegionCountry", c(Region, Country), sep = ", ")


# Inventory by Type
wine_cellar %>% 
  mutate(Type = fct_rev(fct_infreq(Type))) %>%
  ggplot(aes(x = Type)) +
  geom_bar() +
  #coord_flip() +
  theme_minimal()

# Inventory by Varietal
wine_cellar %>% 
  mutate(MasterVarietal = fct_rev(fct_infreq(MasterVarietal))) %>%
  ggplot(aes(x = MasterVarietal)) +
  geom_bar() +
  coord_flip() +
  theme_minimal()

# Inventory by Region
wine_cellar %>% 
  mutate(RegionCountry = fct_rev(fct_infreq(RegionCountry))) %>%
  ggplot(aes(x = RegionCountry)) +
  geom_bar() +
  coord_flip() +
  theme_minimal()

# Inventory by Vintage
wine_cellar %>% 
  ggplot(aes(x = Vintage)) +
  geom_bar() +
  theme_minimal()

# Consume Range
wine_cellar %>% 
  mutate(Wine = fct_rev(fct_reorder(Wine, EndConsume))) %>%
  ggplot() +
  geom_segment( aes(x=Wine, xend=Wine, y=BeginConsume, yend=EndConsume), color="grey") +
  geom_point( aes(x=Wine, y=BeginConsume), color=rgb(0.2,0.7,0.1,1), size=2 ) +
  geom_point( aes(x=Wine, y=EndConsume), color=rgb(0.7,0.2,0.1,1), size=2 ) +
  theme_minimal() +
  coord_flip() +
  geom_hline(yintercept = 2019) +
  xlab("") +
  ylab("Consume between")
  
# Next 5
availability %>% 
  arrange(desc(Available)) %>% 
  slice(1:5) %>% 
  select(Available, Wine, Vintage, MasterVarietal, Producer, Country, Region, CScore, ComBegin, ComEnd)

plotly::ggplotly(
availability %>% 
  drop_na(Available, CScore) %>% 
  ggplot(aes(x = Available, y = CScore, fill = Wine)) +
  geom_point()
)

# Unreported Availability
availability %>% 
  filter(is.na(Available)) %>% 
  select(Available, Wine, Vintage, MasterVarietal, Producer, Country, Region, CScore)
