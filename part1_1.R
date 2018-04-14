setwd('/Users/lisakim/Desktop/EDAV Final/')
data <- read.csv('Restaurants.csv', header = T)
names(data)

library(tidyverse)
keep_cols <- c('DBA', 'BORO', 'ZIPCODE','CUISINE.DESCRIPTION','INSPECTION.DATE','VIOLATION.CODE','SCORE', 'GRADE','INSPECTION.TYPE')
df <- data %>% select(keep_cols)

# new restaurants
nrow(df[df$INSPECTION.DATE == '1/1/1900',])
nrow(df)

# remove new restaurants and restaurants with no scores
df <- filter(df,!is.na(SCORE))
nrow(df)

colnames(df) <- c("name", "boro", "zipcode", "cuisine", "inspection data", 
                  "violation code", "score", "grade",
                  "inspection type")

convert_to_grade <- function(x){
  if (x < 14){
    return("A")
  }
  else if(x > 28){
    return("C")
  }
  else {
    return("B")
  }
}

grade <- sapply(df$score, convert_to_grade)
df$grade <- grade

write.csv(df, file = "combined_data.csv")

df <- filter(df,!is.na(score))
df$boro <- factor(df$boro)

library(vcd)
mosaic(grade~boro, df, labeling = labeling_border(rot_labels = c(0, 0)))
list(unique(df$cuisine))
# cuisine desciption - preprocess and code
# looking at map and the code
# selina is working on violation code, so let her know
df$cuisine['Chinese']
res <- list(unique(df$cuisine))

type <- df %>% select(cuisine) %>% mutate(ifelse(grepl("Chinese", .), "Chinese", .))

# click the code, and can see the distribution
# mosiac plot: look at distribution of grade based on the inspection type; see it improves; also the critical vs non-critical