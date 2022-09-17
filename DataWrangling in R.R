rm(list=ls())

library(tidyverse)

#' Data Transformation Cheat Sheet
browseURL("https://rstudio.com/resources/cheatsheets/")
#' dplyr
browseURL("https://cran.r-project.org/web/packages/dplyr/vignettes/dplyr.html")

#' Load data
data(gapminder, package = "gapminder")

#' Data is now a `tibble`
gapminder

#' dplyr basics  
#'   Basic data manipulation  

#' * Pick observations (rows) by their values using `filter()`  
#' * Pick variables (columns) by their names using `select()`  
#' * Create new variables (columns) with functions of existing variables using `mutate()`  
#' * Reorder rows using `arrange()`  
#' * Collapse many values down to a single summary using `summarise()`  

#' `filter()` rows  

#' old way: function_verb(object, option)
filter(gapminder, country == "Norway")

#' new way: pipe, %>% (shift+ctrl+M); then
gapminder %>%
  filter(country == "Norway")

#' & = AND
gapminder %>% 
  filter(country == "Norway" & year <= 1977) 

#' , = AND
gapminder %>% 
  filter(country == "Norway" , year <= 1977) 

#' | = OR
gapminder %>% 
  filter(year <= 1970) %>% 
  filter(country == "Norway" | country == "Sweden") 

#' combine using %in% and a vector
gapminder %>%
  filter(year <= 1970) %>%
  filter(country %in% c("Norway","Sweden","Denmark")) 

#' filter, select & rename a variable
gapminder %>%
  filter(year <= 1970) %>%
  filter(country %in% c("Norway","Sweden","Denmark")) %>%
  select(country, year, gdpPercap) %>%
  rename(gdp=gdpPercap)

#' arrange rows with arrange()  
#' ascending
gapminder %>%
  filter(year <= 1970) %>%
  filter(country %in% c("Norway","Sweden","Denmark")) %>% 
  select(country, year, gdpPercap) %>%
  rename(gdp=gdpPercap) %>% 
  arrange(gdp)

#' descending
gapminder %>%
  filter(year <= 1970) %>%
  filter(country %in% c("Norway","Sweden","Denmark")) %>% 
  select(country, year, gdpPercap) %>%
  rename(gdp=gdpPercap) %>% 
  arrange(desc(gdp))

#' within each year, arrange gdp
gapminder %>%
  filter(year <= 1970) %>%
  filter(country %in% c("Norway","Sweden","Denmark")) %>% 
  select(country, year, gdpPercap) %>% 
  rename(gdp=gdpPercap) %>% 
  arrange(year, gdp)

#' recode variable using mutate & case_when
gapminder %>%
  filter(year <= 1970) %>%
  filter(country %in% c("Norway","Sweden","Denmark")) %>% 
  select(country, year, gdpPercap) %>%
  rename(gdp=gdpPercap) %>% 
  mutate(ccode = case_when(
    country == "Norway" ~ 1,
    country == "Denmark" ~ 2,
    country == "Sweden" ~ 3))

#' recode variable using mutate & ifelse
gapminder %>%
  filter(year <= 1970) %>%
  filter(country %in% c("Norway","Sweden","Denmark")) %>% 
  select(country, year, gdpPercap) %>%
  rename(gdp=gdpPercap) %>% 
  mutate(ccode = ifelse(country == "Norway", 1,
                        ifelse(country == "Denmark", 2, 3)))

#' data structure: from "long" to "wide"  

#' our "long" starting point
gapminder %>%
  filter(year <= 1970) %>%
  filter(country %in% c("Norway","Sweden","Denmark")) %>% 
  select(country, year, gdpPercap) %>%
  rename(gdp=gdpPercap)

#' wide, years as variables
gapminder %>%
  filter(year <= 1970) %>%
  filter(country %in% c("Norway","Sweden","Denmark")) %>% 
  select(country, year, gdpPercap) %>%
  rename(gdp=gdpPercap) %>%
  pivot_wider(names_from = year,
              values_from = gdp)

#' wide, countries as variables
gapminder %>%
  filter(year <= 1970) %>%
  filter(country %in% c("Norway","Sweden","Denmark")) %>% 
  select(country, year, gdpPercap) %>%
  rename(gdp=gdpPercap) %>%
  pivot_wider(names_from = country,
              values_from = gdp)

#' wide, countries as variables & back again (long)
gapminder %>%
  filter(year <= 1970) %>%
  filter(country %in% c("Norway","Sweden","Denmark")) %>% 
  select(country, year, gdpPercap) %>%
  rename(gdp=gdpPercap) %>%
  pivot_wider(names_from = country,
              values_from = gdp) %>% 
  pivot_longer(-year,
               names_to = "country",
               values_to = "gdp")

#' joining dataframes  

#' first dataframe, gdp
df1 <- gapminder %>%
  filter(year <= 1970) %>%
  filter(country %in% c("Norway","Sweden","Denmark")) %>% 
  select(country, year, gdpPercap) %>%
  rename(gdp=gdpPercap)

#' second dataframe, pop
df2 <- gapminder %>%
  filter(year <= 1970) %>%
  filter(country %in% c("Norway","Sweden","Denmark")) %>% 
  select(country, year, pop)

df1 ; df2

#' this works
left_join(df1, df2)

#' however, this is good practice
left_join(df1, df2, by=c("year","country"))

#' third dataframe, lifeExp
df3 <- gapminder %>%
  filter(year <= 1970) %>%
  filter(country %in% c("Norway","Sweden","Denmark")) %>% 
  select(country, year, lifeExp)

df3

#' adding a third data frame
left_join(df1, df2, by=c("year","country")) %>% left_join(df3, by=c("year","country"))

#' remove dataframes
rm(df1,df2,df3)

#' first dataframe, some rows
df1 <- gapminder %>%
  filter(year <= 1980) %>%
  filter(country == "Norway") %>% 
  select(country, year, gdpPercap) %>%
  rename(gdp=gdpPercap)

#' second dataframe, some more rows, overlap with first dataframe
df2 <- gapminder %>%
  filter(country == "Norway") %>% 
  select(country, year, gdpPercap) %>%
  rename(gdp=gdpPercap)

df1 ; df2

#' third dataframe, with duplicate rows
df3 <- bind_rows(df1,df2)
df3

#' remove duplicate rows
df3 %>% distinct()

#' remove dataframes
rm(df1,df2,df3)


#' create summaries of a dataframe using summarise()  

#' average of all 3 countries
gapminder %>%
  filter(country %in% c("Norway","Sweden","Denmark")) %>% 
  summarise(mean(lifeExp))

#' average per country, name variable
gapminder %>%
  filter(country %in% c("Norway","Sweden","Denmark")) %>% 
  group_by(country) %>% 
  summarise(avlfexp=mean(lifeExp))

#' calculate mean life excpectancy by continent
gapminder %>% 
  group_by(continent) %>%
  summarise(avlfexp=mean(lifeExp))

#' calculate mean life excpectancy per country
gapminder %>% 
  group_by(country) %>%
  summarise(avlfexp=mean(lifeExp))

#' calculate mean life excpectancy by continent & country
gapminder %>% 
  group_by(continent, country) %>%
  summarise(avlfexp=mean(lifeExp))

#' calculate mean life excpectancy by continent & add min and max lifeExp
gapminder %>%
  group_by(continent) %>% 
  summarise(avlfexp=mean(lifeExp), min=min(lifeExp), max=max(lifeExp))

#' data from Norway  
#' use mutate() and create a new variable that is GDP from gdpPercap & pop
gapminder %>%
  filter(country == "Norway") %>%
  mutate(gdp=gdpPercap*pop)

#' use mutate() and create a new variable that is GDP per billion NOK (1 000 000 000 NOK) (1 USD=9 NOK)
gapminder %>%
  filter(country == "Norway") %>%
  mutate(gdp=gdpPercap*pop,
         gdpNOK=gdp*9/1e9)

#' change order of columns
gapminder %>%
  filter(country == "Norway") %>%
  mutate(gdp=gdpPercap*pop,
         gdpNOK=gdp*9/1e9) %>% 
  select(country:year, gdpNOK, everything())

#' categorise
gapminder %>%
  filter(country == "Norway") %>%
  mutate(gdp=gdpPercap*pop,
         gdpNOK=gdp*9/1e9,
         gdpcat = ifelse(gdpNOK <= 999, "less than a billion",
                         ifelse(gdpNOK > 1000 & gdpNOK <= 1999, "between a billion and two billions",
                                ifelse(gdpNOK > 2000, "larger than two billions", NA))))

#' or
gapminder %>%
  filter(country == "Norway") %>%
  mutate(gdp=gdpPercap*pop,
         gdpNOK=gdp*9/1e9,
         gdpcat = ifelse(gdpNOK <= 999, "less than a billion",
                         ifelse(between(gdpNOK, 1000, 1999), "between a billion and two billions",
                                ifelse(gdpNOK >= 2000, "larger than two billions", NA))))

#' histogram
gapminder %>%
  ggplot(aes(x = lifeExp)) + 
  geom_histogram(binwidth=2) +
  facet_wrap(~continent, ncol=3) +
  ggtitle("Life Expectancy by Continent")

#' scatterplot
gapminder %>%
  ggplot(aes(x = gdpPercap, y = lifeExp)) + 
  geom_point() +
  facet_wrap(~continent, ncol=3) +
  ggtitle("Life Expectancy and GDP by Continent")

#' scatterplot - one plot
gapminder %>%
  ggplot(aes(x = gdpPercap, y = lifeExp, color=continent, shape=continent)) + 
  geom_point(alpha=0.4) +
  ggtitle("Life Expectancy and GDP by Continent")

#' 3 countries
gapminder %>%
  filter(country %in% c("Norway","Sweden","Denmark")) %>% 
  ggplot(aes(x = lifeExp, y = gdpPercap, colour = country)) + geom_point()

#' more bells and whistles
gapminder %>%
  filter(country %in% c("Norway","Sweden","Denmark")) %>% 
  ggplot(aes(x = lifeExp, y = gdpPercap, colour = country)) + geom_point() + 
  geom_text(aes(label=as.character(year), hjust=0, vjust=1)) + geom_line() 

#' Find the correlation for all countries
browseURL("https://cran.r-project.org/web/packages/broom/vignettes/broom_and_dplyr.html")

#' sorted ascending
gapminder %>%
  group_by(country) %>%
  summarise(correlation = cor(lifeExp, gdpPercap)) %>% 
  arrange(correlation)

#' sorted descending
gapminder %>%
  group_by(country) %>%
  summarise(correlation = cor(lifeExp, gdpPercap)) %>% 
  arrange(desc(correlation))

#' top two and bottom two
gapminder %>%
  filter(country %in% c("Kuwait","Madagascar","France","Austria")) %>% 
  ggplot(aes(x = lifeExp, y = gdpPercap, colour = country)) + geom_point()

#' scale is important in plots
gapminder %>%
  filter(country %in% c("Madagascar")) %>% 
  ggplot(aes(x = lifeExp, y = gdpPercap, colour = country)) + geom_point()

library(broom)
#' note that reference to data and variables is .$
gapminder %>%
  group_by(country) %>%
  do(tidy(cor.test(.$lifeExp, .$gdpPercap)))

#' calculate the percentage and logaritmic growth & their compounds (cumulative change)
gapminder %>%
  filter(country == "Norway") %>%
  select(country, year, gdpPercap) %>%
  rename(gdp=gdpPercap) %>% 
  mutate(perc_diff = (gdp-lag(gdp))/lag(gdp),
         log_diff = c(NA, diff(log(gdp))),
         comp_perc = c(1, 1+cumsum(na.omit(perc_diff))),
         comp_log = c(1, 1+cumsum(na.omit(log_diff))))

#' what if we wanted to change 1982 to the base year (=100) for comp_log?
df1 <- gapminder %>%
  filter(country == "Norway") %>%
  select(country, year, gdpPercap) %>%
  rename(gdp=gdpPercap) %>% 
  mutate(log_diff = c(NA, diff(log(gdp))),
         comp_log = c(1, 1+cumsum(na.omit(log_diff))))

df2 <- df1 %>% filter(year==1982)

df1 %>%
  mutate(rebase=100*comp_log/df2$comp_log)

rm(df1,df2)

#' calculate the logaritmic growth in gdp for all countries
gapminder %>%
  select(country, year, gdpPercap) %>%
  rename(gdp=gdpPercap) %>%
  group_by(country) %>%
  mutate(log_diff = c(NA, diff(log(gdp)))) %>% head(.,20)

#' what country has the highest and lowest average logaritmic growth in gdp?  

#' lowest
gapminder %>%
  select(country, year, gdpPercap) %>%
  rename(gdp=gdpPercap) %>%
  group_by(country) %>%
  mutate(log_diff = c(NA, diff(log(gdp)))) %>%
  group_by(country) %>% 
  summarise(meanGDP=mean(na.omit(log_diff))) %>% arrange(meanGDP)

#' highest
gapminder %>%
  select(country, year, gdpPercap) %>%
  rename(gdp=gdpPercap) %>%
  group_by(country) %>%
  mutate(log_diff = c(NA, diff(log(gdp)))) %>%
  group_by(country) %>% 
  summarise(meanGDP=mean(na.omit(log_diff))) %>% arrange(desc(meanGDP))

#' density plot
gapminder %>%
  select(continent, country, year, gdpPercap) %>%
  rename(gdp=gdpPercap) %>%
  group_by(country) %>%
  mutate(log_diff = c(NA, diff(log(gdp)))) %>% 
  ggplot() + 
  geom_density(aes(x = log_diff, group=continent, fill=continent), alpha=0.3) + 
  ggtitle("Average GDP by Continent") +
  xlim(c(-0.25,0.5)) +
  xlab("% average growth in GDP")


#' Some useful links:
browseURL("http://r4ds.had.co.nz/transform.html")

browseURL("https://info201.github.io/")