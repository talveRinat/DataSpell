
first_name <- 'Rinat'
last_name <-  'Mahmutov'
email_address <- 'mail@mail'
postal_address <- 'must 289-32'
date_added <- '04.04.2020'
df <- data.frame(first_name, last_name, email_address, postal_address, date_added)
df

select(df, -contains("_add"))
select(df, matches("_.{4,5}$"))
select(df, first_name, last_name, date_added)
select(df, -3:4)
select(df, contains("name"), date_added)
df %>% select(c(1:2, 5))


??transmute
??sample_n
??inner_join
?transform
??'%>%'

avian <- read.csv('https://raw.githubusercontent.com/tonytonov/Rcourse/master/R%20programming/avianHabitat.csv')
avian %>%
  select(Site, Observer, contains("Ht")) %>%
  mutate(Site = factor(str_replace(Site, "[:digit:]+", ""))) %>%
  group_by(Site, Observer) %>%
  summarise_if(is.numeric, funs(sum(.>0)))

df <- read.csv('https://raw.githubusercontent.com/tonytonov/Rcourse/master/R%20programming/avianHabitat.csv')

result <- df %>%
  select(Site, Observer, contains("Ht")) %>%
  transform(Site = as.factor(sub("\\d+","",.$Site))) %>%
  group_by(Site, Observer) %>%
  summarise_each(funs(sum(.>0)))
View(result)

cat <- function(...) {
  if (!require(httr)) install.packages("httr")
  if (!require(jpeg)) install.packages("jpeg")
  library(httr)
  library(jpeg)
  dev.new()
  plot(0:1, 0:1, type = "n")
  rasterImage(content(GET("http://placekitten.com/g/250/250")), 0, 0, 1, 1)
}

cat("Meow!")