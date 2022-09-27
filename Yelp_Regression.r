rm(list = ls())

install.packages("tidytext")
library("tidytext")
library("ggplot2")

traindata = read.csv("train_Madison.csv") #Read data into R
attach(traindata)
summary(traindata)

testdata = read.csv("test_Madison.csv") #Read data into R
attach(testdata)
summary(testdata)

traindatadf <- data.frame(traindata)

train <- read.csv("train_Madison.csv")

train$text <- as.character(train$text)
train$name <- as.character(train$name )
train$city <- as.character(train$city)

library(stringr)
library(jsonlite)
library("tidytext")
require("tidytext")
require("stringer")
require("jsonlite")
require(dplyr)
review_words <- train %>%
select(Id, star, text) %>%
unnest_tokens(word, text) %>%
filter(!word %in% stop_words$word,
 str_detect(word, "^[a-z']+$"))

AFINN <- sentiments %>%
filter(lexicon == "AFINN") %>%
select(word, afinn_score = score)

AFINN


#tidy sentiment analysis, separates good and bad words
#generate AFINN score on all words, plot and then filter by usage
#what AFINN separators do we want to choose, -5 to -3, -3 to -1, -1 to 1, 1 to 3, 3 to 5?

#what is the end model supposed to look like, do we want to group things into those categories


#take x columns and transform them into new variables

c(sort(names(traindata[,c(-1 : -8)])))
#look at different cities use of words
#include city in the model

#all of the words
c(sort(names(traindata[,c(-1 : -8)])))

#all of the reviews that have each word in it
#How many times the word appears more than once

traindata


glm(star ~ city, data = traindata)

plot(traindata$city, traindata$star)

impdata <- traindata[,c(-1, -3, -4, -5, -6)]
total.lm <- lm(star ~ ., data = impdata)
summary(total.lm)

plot(traindata$star, traindata$incredible)

names(traindata)

sumcol <- colSums(Filter(is.numeric, traindata[,c(-1, -2, -3, -4, -5, -6, -7, -8)]))
sumcol

sort(sumcol)

functiondata <- traindata

#need some kind of file that returns IDno and star prediction
starprediction <- data.frame(functiondata[1], Expected)

write.csv(starprediction, file = "Project2Group1.csv", row.names = FALSE)


starsetup <- data.frame(traindata[,c(-1, -3, -4, -5, -6)])

fivestarrating <- starsetup[starsetup$star == 5, ]
fivestarrating
wordsoffivestar <- colSums(fivestarrating)
fivewords <- sort(wordsoffivestar, decreasing = TRUE)
fivewords

fourstarrating <- starsetup[starsetup$star == 4, ]
fourstarrating
wordsoffourstar <- colSums(fourstarrating)
fourwords <- sort(wordsoffourstar, decreasing = TRUE)

threestarrating <- starsetup[starsetup$star == 3, ]
threestarrating
wordsofthreestar <- colSums(threestarrating)
threewords <- sort(wordsofthreestar, decreasing = TRUE)

twostarrating <- starsetup[starsetup$star == 2, ]
twostarrating
wordsoftwostar <- colSums(twostarrating)
twowords <- sort(wordsoftwostar, decreasing = TRUE)

onestarrating <- starsetup[starsetup$star == 1, ]
onestarrating
wordsofonestar <- colSums(onestarrating)
onewords <- sort(wordsofonestar, decreasing = TRUE)
onewords

fivewords[1:50]
fourwords[1:50]
threewords[1:50]
twowords[1:50]
onewords[1:50]

write.csv(fivewords, file = "fivestarwords.csv")
write.csv(fourwords, file = "fourstarwords.csv")
write.csv(threewords, file = "threestarwords.csv")
write.csv(twowords, file = "twostarwords.csv")
write.csv(onewords, file = "onestarwords.csv")

#sorted them into alphabetical order in excel

v = read.csv("fivestarwords.csv")
w = read.csv("fourstarwords.csv")
x = read.csv("threestarwords.csv")
y = read.csv("twostarwords.csv")
z = read.csv("onestarwords.csv")

v$X

#this takes every word and gives it a value of sd of usage at each star rating divided my mean usage at each star rating
#then it splits it into 5 quantile groups
#then it splits between positive and negative words
m <- data.frame()

m <- data.frame(v$x, w$x, x$x, y$x, z$x)
m
n <- rowMeans(m)
m <- transform(m, SD=apply(m, 1, sd, na.rm = TRUE))
m$mean <- n
m
o <- m$SD/m$mean
m$sdmean <- o
m
m[order(m$sdmean, decreasing = TRUE),]
#decides if its a positive or negative word
m$slope <- m$v.x - m$z.x

m$word <- v$X

quantile(m$sdmean, probs=seq(0, 1, .2))

high <- quantile(m$sdmean, probs=1)
medhigh <- quantile(m$sdmean, probs=.8)
med <- quantile(m$sdmean, probs=.6)
medlow <- quantile(m$sdmean, probs=.4)
low <- quantile(m$sdmean, probs=.2)

highimp <- m[m$sdmean > medhigh, ]
medhighimp <- m[m$sdmean > med & m$sdmean <= medhigh, ]
medimp <- m[m$sdmean > medlow & m$sdmean <= med, ]
medlowimp <- m[m$sdmean > low & m$sdmean <= medlow, ]
lowimp <- m[m$sdmean < low, ]

highimpposwords <- highimp$word[highimp$slope > 0]
highimpnegwords <- highimp$word[highimp$slope < 0]
medhighposwords <- medhighimp$word[medhighimp$slope > 0]
medhighnegwords <- medhighimp$word[medhighimp$slope < 0]
medimpposwords <- medimp$word[medimp$slope > 0]
medimpnegwords <- medimp$word[medimp$slope < 0]
medlowimpposwords <- medlowimp$word[medlowimp$slope > 0]
medlowimpnegwords <- medlowimp$word[medlowimp$slope < 0]
lowimpposwords <- lowimp$word[lowimp$slope > 0]
lowimpnegwords <- lowimp$word[lowimp$slope < 0]

#what i could do is assign values to the ratings so if at least one of the words from the highimp category
#comes up that could be worth +5 or -5

lowimpposwords
lowimpnegwords

functiondata <- traindata


verynegative <- ((functiondata$horrible + functiondata$manager + functiondata$rude + functiondata$worst) / functiondata$nword)
verypositive <- ((functiondata$absolutely + functiondata$always + functiondata$amazing + functiondata$atmosphere + functiondata$authentic + functiondata$awesome + functiondata$bakery + functiondata$beautiful + functiondata$best + functiondata$brunch + functiondata$capitol + functiondata$casual + functiondata$chocolate + functiondata$classic + functiondata$cocktail + functiondata$cocktails + functiondata$complaint+ functiondata$cozy+ functiondata$creamy+ functiondata$crispy+ functiondata$cute + functiondata$definitely+ functiondata$delicious + functiondata$desserts+ functiondata$enjoyed + functiondata$excellent + functiondata$fan + functiondata$fantastic+ functiondata$fashioned + functiondata$favorite+ functiondata$favorites + functiondata$flavorful + functiondata$flavors + functiondata$foods + functiondata$fresh+ functiondata$friendly+ functiondata$generous+ functiondata$glad+ functiondata$great + functiondata$hands + functiondata$happy + functiondata$healthy+ functiondata$helpful + functiondata$highly+ functiondata$homemade+ functiondata$incredible+ functiondata$italian + functiondata$knowledgeable + functiondata$lived+ functiondata$local + functiondata$love+ functiondata$loved + functiondata$lovely+ functiondata$madison + functiondata$market+ functiondata$meats+ functiondata$mexican + functiondata$notch + functiondata$outdoor + functiondata$outstanding + functiondata$perfect + functiondata$perfectly + functiondata$pleased+ functiondata$reasonable+ functiondata$reasonably+ functiondata$recommend + functiondata$recommended + functiondata$rich+ functiondata$roasted + functiondata$savory + functiondata$selection + functiondata$shared+ functiondata$solid + functiondata$specials+ functiondata$specialty + functiondata$spicy + functiondata$spot + functiondata$summer+ functiondata$super + functiondata$tasty + functiondata$town + functiondata$traditional + functiondata$truly + functiondata$unique + functiondata$variety + functiondata$vegan + functiondata$vegetarian + functiondata$vibe + functiondata$visiting + functiondata$weekend + functiondata$wine + functiondata$wisconsin + functiondata$wonderful + functiondata$world + functiondata$yum + functiondata$yummy) / functiondata$nword)
midnegative <- ((functiondata$awful + functiondata$mediocre + functiondata$terrible)/ functiondata$nword)
#got rid of TRUE because of formatting issues
midpositive <- ((functiondata$along + functiondata$also + functiondata$ambiance + functiondata$anywhere + functiondata$attentive + functiondata$avocado  + functiondata$bacon  + functiondata$baked  + functiondata$beer  + functiondata$beers  + functiondata$belly  + functiondata$bloody  + functiondata$blue + functiondata$breakfast  + functiondata$burgers  + functiondata$cafe  + functiondata$choices  + functiondata$choose  + functiondata$city.1  + functiondata$coffee  + functiondata$consistently  + functiondata$cool  + functiondata$craving  + functiondata$cream  + functiondata$crisp  + functiondata$crowded + functiondata$curds  + functiondata$curry  + functiondata$dessert  + functiondata$die  + functiondata$downtown  + functiondata$dumplings  + functiondata$enjoy  + functiondata$environment  + functiondata$especially  + functiondata$every  + functiondata$everything  + functiondata$family  + functiondata$fancy  + functiondata$fast  + functiondata$filling  + functiondata$fruit  + functiondata$garlic  + functiondata$gluten  + functiondata$help  + functiondata$house  + functiondata$huge  + functiondata$including + functiondata$ingredients  + functiondata$lamb  + functiondata$light  + functiondata$located  + functiondata$lots  + functiondata$lunch  + functiondata$makes + functiondata$milk  + functiondata$morning  + functiondata$mouth  + functiondata$must  + functiondata$neighborhood  + functiondata$nicely  + functiondata$nights  + functiondata$often  + functiondata$options  + functiondata$owners  + functiondata$packed  + functiondata$pancakes  + functiondata$patio  + functiondata$pizzas  + functiondata$plenty  + functiondata$pork + functiondata$portions  + functiondata$pot  + functiondata$prepared  + functiondata$quick  + functiondata$sandwiches  + functiondata$sauces  + functiondata$seating  + functiondata$share + functiondata$shop  + functiondata$soft  + functiondata$space  + functiondata$spice  + functiondata$spinach  + functiondata$split  + functiondata$spring  + functiondata$square  + functiondata$stop  + functiondata$stuffed  + functiondata$style  + functiondata$sweet  + functiondata$tacos  + functiondata$tap  + functiondata$tofu + functiondata$week  + functiondata$well) / functiondata$nword)
negative <- ((functiondata$bland + functiondata$called + functiondata$dirty + functiondata$disappointing + functiondata$minutes + functiondata$okay + functiondata$overpriced + functiondata$paid + functiondata$poor + functiondata$told + functiondata$waited)/functiondata$nword)
positive <- ((functiondata$addition + functiondata$ago  + functiondata$average + functiondata$cake + functiondata$case + functiondata$cheese + functiondata$chicago + functiondata$chili + functiondata$clean + functiondata$comfortable + functiondata$corn + functiondata$crust + functiondata$cut + functiondata$date + functiondata$decent + functiondata$easily + functiondata$easy + functiondata$eggs + functiondata$ended + functiondata$everyone + functiondata$except + functiondata$expected + functiondata$expensive + functiondata$forget + functiondata$french + functiondata$fun + functiondata$gem + functiondata$grab + functiondata$green + functiondata$greens + functiondata$hear + functiondata$however + functiondata$indian + functiondata$kept + functiondata$lemon + functiondata$list + functiondata$live + functiondata$loud + functiondata$mac + functiondata$menu + functiondata$mix + functiondata$mostly + functiondata$mushroom + functiondata$music + functiondata$nchar + functiondata$never + functiondata$nword + functiondata$overly + functiondata$past + functiondata$perhaps + functiondata$pie + functiondata$plan + functiondata$please + functiondata$potato + functiondata$prices + functiondata$reason + functiondata$reservation + functiondata$rest + functiondata$salmon + functiondata$seafood + functiondata$seems + functiondata$serving + functiondata$somewhat + functiondata$sort + functiondata$st + functiondata$staff + functiondata$star.1 + functiondata$state + functiondata$stay + functiondata$strong + functiondata$tea + functiondata$tender + functiondata$thai + functiondata$thank + functiondata$toast + functiondata$topped + functiondata$treat + functiondata$trip + functiondata$try + functiondata$trying + functiondata$us + functiondata$veggie + functiondata$veggies + functiondata$way + functiondata$west + functiondata$wi + functiondata$window + functiondata$wish + functiondata$worth + functiondata$wow)/functiondata$nword)
lownegative <- ((functiondata$asked + functiondata$barely + functiondata$bill + functiondata$card + functiondata$charge + functiondata$customer + functiondata$customers + functiondata$drive + functiondata$employees + functiondata$minute + functiondata$money + functiondata$phone + functiondata$received + functiondata$returning + functiondata$seemed + functiondata$soggy + functiondata$sorry + functiondata$walked) / functiondata$nword)
lowpositive <- ((functiondata$actually + functiondata$almost + functiondata$already + functiondata$anyway + functiondata$away + functiondata$bartender + functiondata$box + functiondata$call + functiondata$care + functiondata$change + functiondata$cook + functiondata$counter + functiondata$decided + functiondata$delivered + functiondata$despite + functiondata$done + functiondata$due + functiondata$early + functiondata$establishment + functiondata$even + functiondata$expecting + functiondata$experience + functiondata$extremely + functiondata$fact + functiondata$felt + functiondata$fine + functiondata$going + functiondata$got + functiondata$gotten + functiondata$greasy + functiondata$guy + functiondata$hard + functiondata$heard + functiondata$hours + functiondata$idea + functiondata$immediately + functiondata$lack + functiondata$last + functiondata$let + functiondata$lettuce + functiondata$long + functiondata$maybe + functiondata$mean + functiondata$medium + functiondata$mentioned + functiondata$middle + functiondata$multiple + functiondata$needed + functiondata$oil + functiondata$ok + functiondata$ordering + functiondata$part + functiondata$person + functiondata$rather + functiondata$rating + functiondata$ready + functiondata$review + functiondata$reviews + functiondata$salt + functiondata$salty + functiondata$sat + functiondata$seem + functiondata$seen + functiondata$sign + functiondata$similar + functiondata$stand + functiondata$stick + functiondata$table + functiondata$tables + functiondata$tell + functiondata$thought + functiondata$three + functiondata$tiny + functiondata$today + functiondata$together + functiondata$tonight + functiondata$twice + functiondata$two + functiondata$view + functiondata$weird + functiondata$went + functiondata$working) / functiondata$nword)
verylownegative <- ((functiondata$another + functiondata$arrived + functiondata$ask + functiondata$asking + functiondata$avoid + functiondata$bad + functiondata$behind + functiondata$clearly + functiondata$cold + functiondata$completely + functiondata$dry + functiondata$empty + functiondata$finally + functiondata$forgot + functiondata$given + functiondata$guess + functiondata$hostess + functiondata$instead + functiondata$issue + functiondata$later + functiondata$left + functiondata$nothing + functiondata$obviously + functiondata$orders + functiondata$pay + functiondata$paying + functiondata$problem + functiondata$said + functiondata$saw + functiondata$saying + functiondata$slow + functiondata$someone + functiondata$took + functiondata$turned + functiondata$understand + functiondata$unfortunately + functiondata$waiting + functiondata$waitress) / functiondata$nword)
#got rid of else because of formatting issues
verylowpositive <- ((functiondata$anything + functiondata$attention + functiondata$basically + functiondata$better + functiondata$brought + functiondata$business + functiondata$came + functiondata$chance + functiondata$cost + functiondata$delivery + functiondata$disappointed + functiondata$either + functiondata$entire + functiondata$excited + functiondata$finished + functiondata$floor + functiondata$front + functiondata$frozen + functiondata$gave + functiondata$give + functiondata$giving + functiondata$gone + functiondata$half + functiondata$hope + functiondata$hour + functiondata$kitchen + functiondata$least + functiondata$leave + functiondata$less + functiondata$literally + functiondata$longer + functiondata$looked + functiondata$nearly + functiondata$needs + functiondata$none + functiondata$noticed + functiondata$offered + functiondata$order + functiondata$ordered + functiondata$party + functiondata$piece + functiondata$pieces + functiondata$point + functiondata$put + functiondata$return + functiondata$sad + functiondata$seated + functiondata$second + functiondata$server + functiondata$sitting + functiondata$taken + functiondata$taking + functiondata$talk + functiondata$tasted + functiondata$tip + functiondata$unless + functiondata$used + functiondata$waiter + functiondata$wanted + functiondata$water + functiondata$write + functiondata$wrong)/ functiondata$nword)


testdata

#the best one right now is verynegative, verypositive, midnegative, midpositive, negative, positive

glm(traindata$star ~ functiondata$city + verynegative + verypositive + midnegative + midpositive + negative + lownegative + lowpositive + verylownegative + verylowpositive)
Expected <- 3.778 + -41.911*verynegative + 8.42*verypositive + -42.894*midnegative + 3.254*midpositive + -24.69*negative + -10.752*lownegative + -3.259*lowpositive + -14.642*verylownegative + -6.272*verylowpositive

#best so far, "positive" words were found to have no impact
bestglm <- glm(traindata$star ~ verynegative + verypositive + midnegative + midpositive + negative + lownegative + lowpositive + verylownegative + verylowpositive)
summary(bestglm)
Expected <- 3.778 + -41.911*verynegative + 8.42*verypositive + -42.894*midnegative + 3.254*midpositive + -24.69*negative + -10.752*lownegative + -3.259*lowpositive + -14.642*verylownegative + -6.272*verylowpositive

Expected[Expected > 5] <- 5
Expected[Expected < 1] <- 1

#need some kind of file that returns IDno and star prediction
starprediction <- data.frame(functiondata[1], Expected)

write.csv(starprediction, file = "Project2Group1.csv", row.names = FALSE)

Expected <- y1
Expected[Expected > 5] <- 5
Expected[Expected < 1] <- 1


veryneg.lm
verypos.lm
k <- glm(starrate ~verypos.lm + veryneg.lm)

glm(functiondata$star ~ )
highimpposwords
highimpnegwords

plot(m[282])
m[220,]
v[220,]

#in order for a word to be significant, its usage in a rating has to be different from the others, 
#H1 = H2 = H3 = H4 = H5




#now that we can see what the most used words are for each rating, we can try to recognize patterns
#words that are common in all stars; star, st, us, wi, order






#######not using right now

#this is a file with each word sorted into alphabetical order and by usage in each star rating
overalluse = read.csv("words and usage project 2.csv") #Read data into R
attach(overalluse)
summary(overalluse)


v = overalluse$Appear
w = overalluse$Appear.1
x = overalluse$Appear.2
y = overalluse$Appear.3
z = overalluse$Appear.4

m <- matrix(c(v, w, x, y, z), nrow = 1, ncol = 5, byrow = TRUE)
m
barplot(m, beside = TRUE)




averagerating <- (5*v + 4*w + 3*x + 2*y + z) / (v+w+x+y+z)
averagerating
worduse <- (v+w+x+y+z)
alphabeticalorderwords <- c(sort(names(traindata[,c(-1 : -8)])))
alphabeticalorderwords
averageratingdataframe <- data.frame(alphabeticalorderwords, averagerating, worduse)
averageratingdataframe
averageratingdataframe$alphabeticalorderwords

#outliers in usage st, us, wi

howtoadjdataframe <- which(averageratingdataframe$alphabeticalorderwords %in% c("st", "us", "wi"))
howtoadjdataframe
adjdataframe <- data.frame(averageratingdataframe[c(-406, -462, -486)])
adjdataframe


plot(averageratingdataframe$worduse, averageratingdataframe$averagerating)


f <- jitter(traindata$incredible, 1)
starjit <- jitter(traindata$star, .3)
plot(f, starjit)







#"Positive" words
#gem, incredible, die 


#"Negative" words





#find the average star rating of each word'

gemstar <- sum(traindata$star[traindata$gem])/sum(traindata$gem[traindata$gem > 0])
gemstar

gemstar0 <- traindata$gem[traindata$gem > 0]
gemstar0
length(gemstar0)

gemstar1 <- traindata$star[traindata$gem == 1]
gemstar1
sum(gemstar1)/length(gemstar1)

gemstar2 <- traindata$star[traindata$gem == 2]
gemstar2
sum(gemstar2)/length(gemstar2)

gemstar3 <- traindata$star[traindata$gem == 3]
gemstar3
sum(gemstar3)/length(gemstar3)

gemstar4 <- traindata$star[traindata$gem == 4]
gemstar4
sum(gemstar4)/length(gemstar4)


lm(traindata$star ~ traindata$gem)






#Change the value of this to be the prediction
Expected <- 

#need some kind of file that returns IDno and star prediction
starprediction <- data.frame(traindata[1], Expected)

write.csv(starprediction, file = "Project2Group1.csv", row.names = FALSE)


