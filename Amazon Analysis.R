# 1/27/2019: Dani Reed and CJ Arayata
# Monell Chemical Senses Center
# Amazon Food Reviews R-Script
# Dani revisits in May 2019

# Set up: Load libraries and data ####
#setwd("C:/Users/cj/Desktop/Dani Amazon Project")
#Toggle to Dani
setwd("~/Dropbox/Talks/2018/8. SSIB/Manuscript - Amazon/amazon-fine-foods")

# Pacman is a useful package manager that will install packages on the fly

if (!require("pacman")) install.packages("pacman")
pacman::p_load(rvest, readr, tidyr, plyr, dplyr, stringr, tidytext, ggplot2,knitr)#wordVectors)
#devtools::install_github("bmschmidt/wordVectors")

#Dani cannot install wordVectors - not in Cran for my version of R and github download fails
#need more developer software beyond devtools

# Load raw review data
Reviews <- read_csv("Reviews.csv")

# Clean up: all review text to lower case
Reviews$Text <- tolower(Reviews$Text)

# ## There are several cases of duplicate text, for example:
# subset(Reviews,UserId=="A3HDKO7OW0QNK4")
# ## This is very surprising to me--I only see 393,568 unique, which means over 30% are duplicates. Some of these are reviews for two different sizes of the same product
# length(unique(Reviews$Text))

## Let's remove duplicates
Reviews.withDup <- Reviews
Reviews.dedup <- Reviews[!duplicated(Reviews$Text),]
Reviews <- Reviews.dedup 


# Number of unique reviewers: 256,043
length(unique(Reviews$UserId))

# Number of unique products: 67,553
length(unique(Reviews$ProductId))

# New Table 1/2: Setup the multi-word analyses #####
# Important to use 'str_detect' here. If any word is detected at least once, it counts only once. Using 'str_count' means that if a word is repeated in a review, it counts twice, which can lead to inaccurate counts.
# CJ is using this to also produce a master Supplemental Table 1 that contains all words and categories

test <- c("taste taste aftertaste")

sum(str_detect(test, "taste")) # 1
sum(str_count(test, "taste")) # 3


# Word2Vec to subjectively get clusters
Reviews$Text <- tolower(Reviews$Text)
Reviews.noPunct <- gsub('[[:punct:] ]+',' ',Reviews$Text) #Strip punctuation
write.table(Reviews.noPunct,"amazonReviews.txt",sep=" ",row.names=FALSE)

#Train model
#Initial call:
#model = train_word2vec("amazonReviews.txt","amazonReviews.bin",vectors=200,threads=4,window=12,iter=5,negative_samples=0)
model = read.vectors("amazonReviews.bin")

#We start with a seed word:
kable(model %>% closest_to("smell"))
#Gather list of terms based on above output
kable(model %>% closest_to(model[[c("smell","odor","fragrance","aroma","smells", "scent")]],50))
#Continue to grow the list until we don't see any useful additions
kable(model %>% closest_to(model[[c("smoky","minty","chamomile","fruity","burnt","spearmint","jasmine","chocolaty")]],30))


#Repeat for other seed words
kable(model %>% closest_to(model[[c("price","value","savings","deal","cheaper","discount","bargain","pricing","offers","pricey","sale")]],50))
kable(model %>% closest_to(model[[c("healthy","nutritious","unhealthy","wholesome","safe","vitamins","nutrients","healthier","harmful","diet")]],50))
kable(model %>% closest_to(model[[c("order", "service", "arrived", "ordering","expired","shipment","shipped","damaged","seller","promptly","vendor","refunded","ship","condition","broken","refund","fulfilled","timely")]],50))
#There are at least two taste clusters: unpleasant and pleasant. I followed both.
kable(model %>% closest_to(model[[c("taste","aftertaste","acidic","bitterness","bitter","sour","metallic","salty")]],50))
kable(model %>% closest_to(model[[c("sweet","savory","sugary","umami","salty","sweetness")]],50))
kable(model %>% closest_to(model[[c("spicy","kick","bland","spiciness","tabasco")]],30))


# Taste ####
# Removed double-count words, e.g. bitter/bitterness, taste/aftertaste
tasteList <- c("taste", "acidic","bitter","sour","metallic","salty","sweet","savory","sugary","umami")

taste.counts <- data.frame(word = tasteList)
taste.counts$count <- NA
Reviews$taste <- NA

for (i in 1:length(tasteList)){
        taste.counts$count[i] <- sum(str_detect(Reviews$Text, tasteList[i]))
}


taste.counts$percent <- round((taste.counts$count / length(Reviews$Text)) * 100, 2)
taste.counts <- taste.counts %>% 
        arrange(desc(count)) %>% 
        filter(count > 0)

taste.counts$category <- "Taste"

# Price ####
costList <- c("price","value","savings","deal","cheaper","discount","bargain","pricing","offers","sale")

price.counts <- data.frame(word = costList)
price.counts$count <- NA

for (i in 1:length(costList)){
        price.counts$count[i] <- sum(str_detect(Reviews$Text, costList[i]))
}

price.counts$percent <- round((price.counts$count / length(Reviews$Text)) * 100, 2)
price.counts <- price.counts %>% 
        arrange(desc(count)) %>% 
        filter(count > 0)

price.counts$category <- "Price"

# Texture ####
# Removed some words that were duplicated, e.g. chewy, moist, brittle, oily
textureList <- c("texture","crumbly","gritty","airy","consistency","mushy","chalky","grainy","oily","brittle","chewy","dense","crispness","cripsy","dry", "tough", "firm","cloggy","squashy", "crunchy", "fish eyes",	"gummy","melts", "feathery", "smooth", "hard", "moist", "slick", "fine", "juicy","greasy", "glue",	"wet","pulpy", "heavy", "creamy","lumpy", "lean", "biting", "rubbery", "pasty", "soft","light", "runny","crackling","soggy","powdery", "slimy", "watery", "puffy", "stringy", "noise", "slippery", "delicate", "binding", "rough", "fluffy", "thick", "flaky", "sounds", "tender", "gooey", "chunky", "chokes me", "sponginess","silky")


texture.counts <- data.frame(word = textureList)
texture.counts$count <- NA

for (i in 1:length(textureList)){
        texture.counts$count[i] <- sum(str_detect(Reviews$Text, textureList[i]))
}

texture.counts$percent <- round((texture.counts$count / length(Reviews$Text)) * 100, 2)
texture.counts <- texture.counts %>% 
        arrange(desc(count)) %>% 
        filter(count > 0)

texture.counts$category <- "Texture"

# Smell - bigger list ####
odorList2 <- c("smell","odor","fragrance","aroma", "scent","smoky","minty","chamomile","fruity","burnt","spearmint","jasmine","chocolaty")

odor.counts <- data.frame(word = odorList2)
odor.counts$count <- NA

for (i in 1:length(odorList2)){
        odor.counts$count[i] <- sum(str_detect(Reviews$Text, odorList2[i]))
}

odor.counts$percent <- round((odor.counts$count / length(Reviews$Text)) * 100, 2)
odor.counts <- odor.counts %>% 
        arrange(desc(count)) %>% 
        filter(count > 0)

odor.counts$category <- "Olfaction"

# Health ####
healthList <- c("healthy","nutritious","unhealthy","wholesome","safe","vitamins","nutrients","healthier","harmful","diet","organic")

health.counts <- data.frame(word = healthList)
health.counts$count <- NA

for (i in 1:length(healthList)){
        health.counts$count[i] <- sum(str_detect(Reviews$Text, healthList[i]))
}

health.counts$percent <- round((health.counts$count / length(Reviews$Text)) * 100, 2)
health.counts <- health.counts %>% 
        arrange(desc(count)) %>% 
        filter(count > 0)

health.counts$category <- "Health"

# Customer Service ####
# Removed some words that were duplicated, order/ordering, ship/shipment/shipped
serviceList <- c("order", "service", "arrived","expired","damaged","seller","promptly","vendor","refunded","ship","condition","broken","refund","fulfilled","timely")

service.counts <- data.frame(word = serviceList)
service.counts$count <- NA

for (i in 1:length(serviceList)){
        service.counts$count[i] <- sum(str_detect(Reviews$Text, serviceList[i]))
}

service.counts$percent <- round((service.counts$count / length(Reviews$Text)) * 100, 2)
service.counts <- service.counts %>% 
        arrange(desc(count)) %>% 
        filter(count > 0)

service.counts$category <- "Customer Service"

# Trigeminal ####
trigemList <- c("spicy","kick","bland","spiciness","tabasco")

trigem.counts <- data.frame(word = trigemList)
trigem.counts$count <- NA

for (i in 1:length(trigemList)){
        trigem.counts$count[i] <- sum(str_detect(Reviews$Text, trigemList[i]))
}

trigem.counts$percent <- round((trigem.counts$count / length(Reviews$Text)) * 100, 2)
trigem.counts <- trigem.counts %>% 
        arrange(desc(count)) %>% 
        filter(count > 0)

trigem.counts$category <- "Chemesthesis"

# Rbind all together
category.master <- rbind(taste.counts, texture.counts, service.counts,
                         price.counts, health.counts, odor.counts, trigem.counts)

# Supplemental Table 1: Write out
write.csv(category.master, file = "Supplemental_Table_1.csv", row.names = F)

# For each category, get the top 5 or so words as a new Table 1
category.top <- category.master %>% 
        group_by(category) %>% 
        top_n(5, count)

# Table 1: Write out #####
# Feel free to trim rows that are irrelevant
write.csv(category.top, file = "New_Table_1.csv", row.names = F)

# Figure 1 #####
# Now we aggregate for each category to make the graph, using OR so we don't double count
category.counts <- data.frame(category = c("Taste","Price","Texture","Olfaction","Health","Customer Service","Chemesthesis"))
category.counts$count <- NA

#I don't want to learn about lists of lists at the moment, so here is the ugly version
category.counts$count[1] <- sum(str_detect(Reviews$Text, paste(tasteList,collapse = '|')))
category.counts$count[2] <- sum(str_detect(Reviews$Text, paste(costList,collapse = '|')))
category.counts$count[3] <- sum(str_detect(Reviews$Text, paste(textureList,collapse = '|')))
category.counts$count[4] <- sum(str_detect(Reviews$Text, paste(odorList2,collapse = '|')))
category.counts$count[5] <- sum(str_detect(Reviews$Text, paste(healthList,collapse = '|')))
category.counts$count[6] <- sum(str_detect(Reviews$Text, paste(serviceList,collapse = '|')))
category.counts$count[7] <- sum(str_detect(Reviews$Text, paste(trigemList,collapse = '|')))

category.counts$percent <- round((category.counts$count / length(Reviews$Text)) * 100, 2)
category.counts <- category.counts %>% 
  arrange(desc(count)) %>% 
  filter(count > 0)

# Plot and save
ggplot(category.counts,aes(x=reorder(category,-percent),y=percent))+geom_col(fill="cornflowerblue")+xlab("Category")+ylab("Percentage of reviews containing a word from each category")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_blank())
ggsave("Categories.pdf", dpi = 300, width = 6, height = 6, units = "in")


# Write out single word counts #####
#Need to fix this to read 
#Write out the sentence
paste0("taste was mentioned in over ",floor(percent_taste*100),"% of the reviews (N=",calculate_taste_percent[2],"), followed by sweet (",
      round(percent_sweet_percent*100,1),"%, N=",calculate_sweet_percent[2],"), bitter (",round(percent_bitter_percent*100,1),"%, N=",11,424,
      "), sour (",round(percent_sour_percent*100,1),"%, N=",calculate_sour_percent[2],"), and salty (",round(percent_salty_percent*100,1),"%, N=",calculate_salty_percent[2],")." 
      )

# "taste was mentioned in over 30% of the reviews (N=121447), followed by sweet (10.8%, N=42315), bitter (2.9%, N=11424), sour (2.1%, N=8252), and salty (1.4%, N=5688)."

paste0("‘Taste’ was mentioned ",round(calculate_taste_percent[2]/calculate_smell_percent[2])," times more frequently than ‘smell’ and ",round(calculate_taste_percent[2]/calculate_texture_solo_percent[2])," times more frequently than ‘texture’." )


# Table 2: sweet phrases #####
# First, get all reviews that contain the word sweet

sweet.string <- c("sweet")
sweet.reviews <- Reviews %>%
        filter(str_detect(Text, paste(sweet.string, collapse = "|")))

# Trim the review text to just be 5 words before and after "sweet"
sweet.review.text <- str_extract(sweet.reviews$Text, "([^\\s]+\\s){1,5}sweet.?(\\s[^\\s]+){1,5}")


# Making it tidy
sweet.review.text <- tibble(review = 1:length(sweet.review.text),
                            text = sweet.review.text)

# From the sweet.review.text - remove 'sweet tooth' and 'sweet potato' - data cleaning
sweet.review.text <- filter(sweet.review.text, !is.na(text) &
                                    !str_detect(sweet.review.text$text, "sweet tooth") &
                                    !str_detect(sweet.review.text$text, "sweet potato"))

# Make a table of the words in order of frequency
sweet.review.wordcount <- sweet.review.text %>%
        unnest_tokens(word, text) %>%
        count(word, sort = T)

# Based on skimming, let's make a vector of most common phrases
# Positive versions
phrases.v3 <- c("too sweet", "sweet enough", "really sweet", "overly sweet", "excessively sweet", "sweet as i would prefer", "so sweet", "slightly sweet", "very sweet",
  "sweet enough for me", 'disgustingly sweet', "sickeningly sweet", "pretty sweet", "overpoweringly sweet", 
  "overwhelmingly sweet", "sickly sweet", "couldn't stand how sweet it was", "ultra sweet", "cloyingly sweet",
  "to sweet", "on the sweet side")

# Let's make parallel negation versions, e.g. "not sweet enough"
phrases.v3.neg <- outer("not", phrases.v3, paste)[1,]

# Doing counts for the positive
phrase.counts.v3 <- data.frame(phrase = phrases.v3)
phrase.counts.v3$count <- NA

for (i in 1:length(phrases.v3)){
        phrase.counts.v3$count[i] <- sum(str_count(Reviews$Text, phrases.v3[i]))
}

# Doing counts for the negative
phrase.counts.v3.neg <- data.frame(phrase = phrases.v3.neg)
phrase.counts.v3.neg$count <- NA

for (i in 1:length(phrases.v3.neg)){
        phrase.counts.v3.neg$count[i] <- sum(str_count(Reviews$Text, phrases.v3.neg[i]))
}

# Because these are parallel versions, we can subtract them:
## 7613 "too sweets" actually includes 3352 "not too sweets"; same for all other phrases
sweet.phrase.master <- data.frame(phrase = phrases.v3,
                                  count = phrase.counts.v3$count - phrase.counts.v3.neg$count)

# Rbind both together and sort
sweet.phrase.master <- rbind(sweet.phrase.master, phrase.counts.v3.neg)

sweet.phrase.master$percent <- round((sweet.phrase.master$count / length(Reviews$Text)) * 100, 2)
sweet.phrase.master <- sweet.phrase.master %>% 
        arrange(desc(count)) %>% 
        filter(count > 0)

# Assign categories and tabulate
sweet.phrase.master$Type <- NA

under <- c("not very sweet", "not sweet enough", "not sweet enough for me")

over <- c("too sweet", "very sweet", "to sweet", "overly sweet", "so sweet", "really sweet", "sickly sweet", "pretty sweet", "cloyingly sweet", "sickeningly sweet", "overwhelmingly sweet", "overpoweringly sweet", "excessively sweet", "disgustingly sweet", "ultra sweet", "couldn't stand how sweet it was")

# Recode
sweet.phrase.master$Type <- ifelse(sweet.phrase.master$phrase %in% under, "Under", sweet.phrase.master$Type)
sweet.phrase.master$Type <- ifelse(sweet.phrase.master$phrase %in% over, "Over", sweet.phrase.master$Type)
sweet.phrase.master$Type <- ifelse(is.na(sweet.phrase.master$Type), "Neutral", sweet.phrase.master$Type)

sweet.phrase.master <- sweet.phrase.master %>% 
        arrange(desc(Type), desc(count))

# Here is Supplemental Table 2:
write.csv(sweet.phrase.master, file = "Supplemental_Table_2.csv")

# Group by categories
sweet.summary <- ddply(sweet.phrase.master, ~Type,
                       summarise,
                       total = sum(count))

sweet.summary$Percent <- (sweet.summary$total/sum(sweet.summary$total)) * 100

## Here is Table 2:
sweet.summary <- arrange(sweet.summary, desc(Percent))
write.csv(sweet.summary, file = "Table_2.csv", row.names = F)

# To assess face validity, pull all reviews that contain any of these phrases and read them
# Also keep in mind that str_detect will find "sweeter", "sweetness", e.g. "slightly sweeter" or "[a] little sweetness"
refined.reviews <- Reviews %>% 
        filter(str_detect(Text, paste(phrases.v3, collapse = "|")))

# Pull a sample to read
set.seed(555)
sampled.reviews <- refined.reviews[sample(1:nrow(refined.reviews), 50), ]

# Table 3: Polarizing Products (Part 1) #####

# First, trim down to all reviews with greater than 50 reviews
Reviews50 <- Reviews %>% group_by(ProductId) %>% filter(n() >50)

# Number of products, redux. 109,698 reviews of 908 products
length(unique(Reviews50$ProductId)) # 908

# Get the standard deviation in the star rating
sd <- aggregate(Reviews50$Score, by=list(Reviews50$ProductId), FUN = sd)
#Dani add ins - 
mean <- aggregate(Reviews50$Score, by=list(Reviews50$ProductId), FUN = mean)
count_temp <- aggregate(Reviews50$Score, by=list(Reviews50$ProductId), FUN = length)


# Arrange by sd; larger sd is more polarizing
sd <- arrange(sd, desc(x))

names(sd) <- c("ASIN", "std.dev")

#Table 4: Dani checking code - I could not get code below to run so workaround to check Table 4
target.products <- c("B001M08YZA", "B004TJF3BE", "B00507A02Q",
                     "B002OMV09W", "B000HDKZKU", "B000EM9E2Y",
                     "B000F6SNPS", "B000CRIBCA", "B000AQJRWG",
                     "B002CENRLG", "B002EDEMLY")
# we lost B004TJF3BE probably during the de-duplication process
# add ghost chili pepper B002OMV09W

temp <- merge(sd, mean, by.x = "Group.1", by.y = "Group.1", all = TRUE)
temp1 <- merge(temp, count_temp, by.x = "Group.1", by.y = "Group.1", all = TRUE)
temp2 <- temp1[temp1$Group.1 %in% target.products, ]
temp2 <- arrange(temp2, desc(x.x))
test1 <- temp1[temp1$Group.1 %in% test, ]
write_csv(temp2, "Table_4_Checkingv1.csv")



# Table 4: Polarizing Products (Part 2) #####
# Map product codes (Amazon ID "ASIN") to actual product name

# Vector of top 100 items (most polarizing)
first.100 <- sd$ASIN[1:100]

# Initialize blank matrix
product.codebook <- matrix(nrow = 100, ncol = 2)

# Go to product page, and get the title from the website
for (i in 1:100){
      
        # read a url page
        try({
        url <- paste0('https://www.amazon.com/dp/', first.100[i])
        webpage <- read_html(url)
        
        # get the title (after cleaning a bit)
        title <- html_nodes(webpage,'#title') %>% 
                html_text()
        title <- gsub("\n", "", title)
        title <- gsub(" ", "", title)

        # write it to our blank matrix that matches the product key
        product.codebook[i] <- first.100[i]
        product.codebook[i, 2] <- title
        })
}


# Polish a bit and write
polished.product.codebook <- as.data.frame(product.codebook)
names(polished.product.codebook) <- c("ASIN", "Description")

write.csv(polished.product.codebook, "polished.codebook.results.csv", row.names = F)

# Cut to complete cases
polished.product.codebook <- polished.product.codebook[complete.cases(polished.product.codebook), ]

# Merge back in the polarizing data (standard deviation)
products <- merge(polished.product.codebook, sd, all.x = T) %>% 
        arrange(desc(std.dev))

# Tie back to all reviews
product.reviews <- merge(Reviews50, products, by.x = "ProductId", by.y = "ASIN", all.y = T)

# Create manual list of the ASINs I want (products in Table 4)
## The top 11 we picked are the human-edible items (i.e not petfood)
target.products <- c("B001M08YZA", "B004TJF3BE", "B00507A02Q",
"B002OMV09W", "B000HDKZKU", "B000EM9E2Y",
"B000F6SNPS", "B000CRIBCA", "B000AQJRWG",
"B002CENRLG", "B002EDEMLY")

# These are all the reviews for the Top 11 products: For manual coding
target.reviews <- product.reviews[product.reviews$ProductId %in% target.products, ]
write.csv(target.reviews, "target_reviews_for_reading.csv", row.names = F)

# Summary table for the Top 11 products
summary.reviews <- ddply(target.reviews, .(ProductId, Description), summarise,
                         review.count = length(UserId),
                         mean.rating = mean(Score),
                         std.dev = mean(std.dev),
                         taste.count = sum(taste_keyword),
                         bitter.count = sum(bitter_keyword),
                         sweet.count = sum(sweet_keyword),
                         sour.count = sum(sour_keyword),
                         salty.count = sum(salty_keyword)) %>% 
        arrange(desc(std.dev))

most.polarizing.scores <- subset(Reviews,ProductId %in% summary.reviews$ProductId)[,c(2,7)]
names(most.polarizing.scores)[1]="ASIN"
most.polarizing.scores <- merge(most.polarizing.scores,polished.product.codebook)
most.polarizing.scores[,"Description"] <- as.character(most.polarizing.scores[,"Description"])
most.polarizing.scores[,"Description"] <- as.factor(most.polarizing.scores[,"Description"])
levels(most.polarizing.scores$Description) <- c("Alkaline Water","Enjoy Life Chewy Bars","Ghost chili pepper","Good Earth Herbal Tea","Popped Cheese","Red Vines","Shirataki Noodles","Special K Cereal","Think! Protein Bars","Tofu Shiratake Noodles")
ggplot(most.polarizing.scores,aes(x=Score))+geom_bar(fill="cornflowerblue")+facet_wrap(.~Description)
ggsave("Most Polarizing.pdf", dpi = 300, width = 6, height = 6, units = "in")

# Sorting for least polarizing (control) foods #####
# Arrange by sd, ascending
sd <- arrange(sd, x)

names(sd) <- c("ASIN", "std.dev")

# Least Polarizing Products #####
# Map product codes (Amazon ID "ASIN") to actual product name

# Vector of top 100 items (least polarizing)
first.100 <- sd$ASIN[1:100]

# Initialize blank matrix
product.codebook <- matrix(nrow = 100, ncol = 2)

# Go to product page, and get the title from the website
for (i in 1:100){
        
        # read a url page
        try({
                url <- paste0('https://www.amazon.com/dp/', first.100[i])
                webpage <- read_html(url)
                
                # get the title (after cleaning a bit)
                title <- html_nodes(webpage,'#title') %>% 
                        html_text()
                title <- gsub("\n", "", title)
                title <- gsub(" ", "", title)
                
                # write it to our blank matrix that matches the product key
                product.codebook[i] <- first.100[i]
                product.codebook[i, 2] <- title
        })
}


# Polish a bit
polished.product.codebook <- as.data.frame(product.codebook)
names(polished.product.codebook) <- c("ASIN", "Description")

# Cut to complete cases
polished.product.codebook <- polished.product.codebook[complete.cases(polished.product.codebook), ]

# Merge back in the polarizing data (standard deviation)
products <- merge(polished.product.codebook, sd, all.x = T) %>%
        arrange(std.dev)

# Now have list of potential control foods to test
write.csv(products, "least.polarizing.results.csv", row.names = F)


###Some extra stuff to find Ghost Chili Pepper quotes
chili <- c("B002OMV09W")
test2 <- Reviews[Reviews$ProductId %in% chili, ]
write.csv(test2, "chili.csv")