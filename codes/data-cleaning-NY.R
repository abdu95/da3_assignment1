# prepare environment 

rm(list=ls())

library(tidyverse)

rm(list=ls())

# CHANGE TO YOUR WORKING DIRECTORY
# setwd("")

setwd("D:/CEU/winter semester/data-analysis-3/")
dir = "da3_assignment1"

#location folders
data_in  <- paste0(dir,"/data/raw/")
data_out <- paste0(dir,"/data/clean/")


data <- read.csv(paste0(data_in,"listings.csv"))

drops <- c("host_thumbnail_url","host_picture_url","listing_url","thumbnail_url","medium_url","picture_url","xl_picture_url","host_url","last_scraped","description", "experiences_offered", "neighborhood_overview", "notes", "transit", "access", "interaction", "house_rules", "host_about", "host_response_time", "name", "summary", "space", "host_location")
data<-data[ , !(names(data) %in% drops)]
write.csv(data,file=paste0(data_in,"airbnb_ny_listing.csv"))


#####################################

# opening dataset
df <- read.csv(paste0(data_in,"airbnb_ny_listing.csv"),
             sep=",",header = TRUE, stringsAsFactors = FALSE)

#drop broken lines - where id is not a character of numbers
df$junk<-grepl("[[:alpha:]]", df$id)
df<-subset(df,df$junk==FALSE)
df<-df[1:ncol(df)-1]

#####################
#formatting columns

#remove percentage signs
for (perc in c("host_response_rate","host_acceptance_rate")){
  df[[perc]]<-gsub("%","",as.character(df[[perc]]))
}


#remove dollar signs from price variables
for (pricevars in c("price")){
  df[[pricevars]]<-gsub("\\$","",as.character(df[[pricevars]]))
  df[[pricevars]]<-as.numeric(as.character(df[[pricevars]]))
}

#format binary variables
for (binary in c("host_is_superhost","host_has_profile_pic","host_identity_verified","instant_bookable")){
  df[[binary]][df[[binary]]=="f"] <- 0
  df[[binary]][df[[binary]]=="t"] <- 1
}

# array of elements converted into vector

#amenities
df$amenities<-gsub("\\[","",df$amenities)
df$amenities<-gsub("\\]","",df$amenities)
df$amenities<-gsub('\\"',"",df$amenities)
df$amenities<-as.list(strsplit(df$amenities, ","))

#define levels and dummies 
levs <- levels(factor(unlist(df$amenities)))

#############
# something <- cbind(df,as.data.frame(do.call(rbind, lapply(lapply(df$amenities, factor, levs), table))))

for (element in 1:length(df$amenities)){
  df$amenities[[element]] <- trimws(df$amenities[[element]])
}

for (element in 1:length(df$amenities)){
  result <- str_detect(tolower(df$amenities[[element]]), 'stove') 
  something2$stove[[element]] <- ifelse(any(result, na.rm = FALSE), 1, 0)
}

something2$stove <- NA
result <- str_detect(tolower(something2$amenities[[1]]), 'stove') 
result

ifelse(any(result, na.rm = FALSE), 1, 0)

'Hot water' %in% something2$amenities[[1]]

if(any(result, na.rm = FALSE)){
  print("hey")
}


# something2$soap <- ifelse('soap' %in% something2$amenities, 1, 0)
something2$stove <- ifelse('Stove' %in% something2$amenities, 1, 0)
something2$amenities

x = strsplit(something2$amenities, ",")

levs
print()
for (element in 1:length(something2$amenities)){
  print(something2$amenities[element])
}

for (element in 1:3){
  print(something2$amenities[[1]])
}
typeof(something2$amenities)







#########

drops <- c("amenities")
df<-df[ , !(names(df) %in% drops)]


#write csv
write.csv(df,file=paste0(data_out,"airbnb_ny_cleaned.csv"))



to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]
##  bathrooms - 36923, bedrooms - 3588
# beds - 477

#  data$license

# unique(data[c("license")])

