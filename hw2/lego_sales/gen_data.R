library(randomNames)
library(stringr)
library(dplyr)
library(jsonlite)

set.seed(21817)
n = 250

df = data.frame(gender=sample(c("Female", "Male"), n, replace=TRUE),
                ethnicity=sample(c("African American", "Hispanic", "Asian", "White", "Native American"), 
                                 n, replace=TRUE, prob = c(0.132,0.171,0.056,0.631,0.01)),
                stringsAsFactors = FALSE)

full_names = randomNames(gender=df$gender, ethnicity=df$ethnicity) %>% str_split(", ")

df$first_name = sapply(full_names, `[[`, 2)
df$last_name = sapply(full_names, `[[`, 1)

df$ethnicity = NULL

df$age = rnorm(2*n, 30, 15) %>% .[. >= 16] %>% floor() %>% head(n)


area_codes = unlist(read.csv("area_codes.csv",header=TRUE))
npa = "555"
lines = formatC(0:9999, width = 4, format = "d", flag = "0")

df$phone_number = paste(sample(area_codes,n,replace=TRUE),npa,sample(lines,n,replace=TRUE),sep="-")



data = lapply(1:nrow(df), function(j) as.list(df[j,]))

for(i in sample(1:n, 0.15*n, replace=FALSE))
  data[[i]]$phone_number = NULL


hobbies = unlist(read.csv("hobbies.csv",header=FALSE, stringsAsFactors=FALSE), use.names=FALSE)
n_hobbies = rbinom(n,3,0.5)

for(i in seq_along(data))
{
  if(n_hobbies[i] == 0)
  {
    data[[i]]$hobbies = character()
  } else {
    data[[i]]$hobbies = sample(hobbies, n_hobbies[i], replace=FALSE)
  }
}

legos = read.csv("legos.csv",stringsAsFactors=FALSE)
legos = legos %>% select(-Variant, -Minifigs, -UKPrice, -CAPrice, -EUPrice) %>% filter(!is.na(USPrice))

n_purchases = 1+rbinom(n, size = 30, prob = 0.05)

for(i in seq_along(data))
{
  data[[i]]$purchases = sample(1:nrow(legos), n_purchases[i], replace = FALSE) %>%
                        lapply(function(j) as.list(legos[j,]))
  
  for(j in seq_along(data[[i]]$purchases))
  {
    data[[i]]$purchases[[j]]$Quantity = rt(5,8) %>% abs() %>% ceiling() %>% .[. <= 10] %>% .[1]
  }
}

sales = data

i = sample(1:n,1)
sales[[i]]$first_name = "Jenny"
sales[[i]]$last_name = "Tutone"
sales[[i]]$gender = "Female"
sales[[i]]$phone_number = "919-867-5309"
sales[[i]]$age = 35L


save(sales, file="lego_sales.RData")

toJSON(sales, pretty=TRUE, auto_unbox=TRUE) %>% writeLines("lego_sales.json")
