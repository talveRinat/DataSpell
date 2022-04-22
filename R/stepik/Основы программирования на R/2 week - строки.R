# Факторы и строки

a <- 'Аэрофотосъёмка ландшафта уже выявила земли богачей и процветающих крестьян.'
n <- nchar(a)
str_length(a)

toupper(tolower(a))
tolower(a)
?strsplit
?paste0


hamlet <- "To be, or not to be: that is the question:
Whether 'tis nobler in the mind to suffer
The slings and arrows of outrageous fortune,
Or to take arms against a sea of troubles,
And by opposing end them?"

hamlet1 <- str_replace_all(hamlet, "[:punct:]", "")
hamlet2 <- tolower(unlist(str_split(hamlet, "[:space:]")))

# решение 1
sum(str_count(hamlet1, "to" ))
b <- (str_extract_all(hamlet1, "[fqw]"))
c <- (str_extract_all(hamlet1, "b."))

# решение 2
sum(hamlet=="to")
sum(grepl("[fqw]", hamlet))
sum(grepl("b.", hamlet))
sum(nchar(hamlet)==7)

# решение 3
sum(hamlet == 'to')
length(grep("[fqw]", hamlet))
sum(str_detect(hamlet, "b."))
sum(str_length(hamlet)==7)


?gsub
quakes$mag
f <- hist(quakes$mag)
f$counts
# решение 1
q <- sort(table(cut(quakes$mag, seq(4, 6.5, by = 0.5), r = F)), d = T)
# решение 2
e <- table(cut(quakes$mag,seq(4,7,0.5),rig=F))
# решение 3
cat(names(sort(table(cut(quakes$mag, seq(4, 6.5, by = 0.5), right = F)), decreasing = T)))


# Importing and inspecting data
#getwd() #setwd if needed
#list.files()
#list.files(pattern = ".*\\.csv$")
#readLines("avianHabitat.csv", 5)
#options(stringsAsFactors = F)
avian <- read.csv("https://raw.githubusercontent.com/tonytonov/Rcourse/master/R%20programming/avianHabitat.csv")
#avian <- read.table("avianHabitat.csv", head = T, sep = ",")
str(avian)
head(avian)
summary(avian)

# Checking data
any(!complete.cases(avian))
any(avian$PDB < 0)
any(avian$PDB > 100)

check_percent_range <- function(x) {
  any(x < 0 | x > 100)
}

check_percent_range(avian$PW)

library(stringr)
coverage_variables <- names(avian)[str_detect(names(avian), "^P")]
sapply(coverage_variables, function(name) check_percent_range(avian[[name]]))

# Transforming variables
#names(avian)
#coverage_variables <- names(avian)[-(1:4)][c(T, F)]
#coverage_variables

avian$total_coverage <- rowSums(avian[, coverage_variables])
summary(avian$total_coverage)

avian$site_name <- factor(str_replace(avian$Site, "[:digit:]+", ""))

#решение 1
df<-read.table("avianHabitat .txt", header = T, sep = ",", dec=".")
apply(df[str_detect(names(df), "Ht")], 2, function(x) df$Observer[which.max(x)])
# решение 2
hight_variables <- names(avian)[str_detect(names(avian), "Ht")]
hight_max <- sapply(avian[,hight_variables], max)
data.frame(hight_max, avian$Observer[hight_max])