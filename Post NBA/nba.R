library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)

nba <- read_excel("NBA.xlsx")
View(nba)

#Let's review data generally
summary(nba)
head(nba)
tail(nba)
names(nba)
summary(nba$LikeCount)

#Start Clean
#####################################
#First to do that change unpractical columns to categorical variable
nba$PostType.f = as.factor(nba$PostType)
nba$PostItemType.f = as.factor(nba$PostItemType)
summary(nba$PostItemType.f)
summary(nba$PostType.f)

#First column is unnecessary
nba = nba[,-1]

#Height and Width is unnecessary as seperated we unite these two columns
nba = unite(nba,"Height-Width",c("Height","Width"),sep = "x")
names(nba)[5] = "Resolution"


#Differ by post code
nba_by_postcode= nba[,-5]
nba_by_postcode = unique(nba_by_postcode)
nba_by_postcode = nba_by_postcode[,-12]

#Like/View column
nba_by_postcode$l_v_percent=if_else(nba_by_postcode$LikeCount/nba_by_postcode$ViewCount != Inf,
                                    nba_by_postcode$LikeCount/nba_by_postcode$ViewCount,
                                    NULL)
#like count by time
nba_by_postcode$ViewCount_null=if_else(nba_by_postcode$ViewCount!=0,
                                       nba_by_postcode$ViewCount,
                                       NULL) 

#First Question

#first question
##################################### 
#How often they send a post? Is there any schedule or are they send randomly? Is there any time they stop posting? 
PostDateDıff = difftime((head(nba_by_postcode$PostDate, -1)), (tail(nba_by_postcode$PostDate, -1)), units = "mins")
PostDateDıff[31963]= NA
tail(PostDateDıff,50)

PostDateDıff=ifelse(PostDateDıff<0,NA,PostDateDıff)
mean(PostDateDıff,na.rm = TRUE)
median(PostDateDıff,na.rm = TRUE)
sd(PostDateDıff,na.rm = TRUE)
dfDate = data.frame(PostDateDıff,nba_by_postcode$PostDate)

dfDate=na.omit(dfDate)

ggplot(data=dfDate,aes(x=nba_by_postcode.PostDate,y=PostDateDıff))+ geom_jitter(alpha = 1/15) +xlab("Post Date") + 
  ylab("Time Difference (hours)") +
  ggtitle("Time Difference Between Two Posts") +
  scale_y_continuous(breaks = seq(0,17500,2500))



id1 <- boxplot.stats(dfDate$PostDateDıff,coef = 2)	
id1$stats	
lowerwhisker =id1$stats[1]	
upperwhisker =id1$stats[5]

dfDate_out<-dfDate[dfDate>lowerwhisker & dfDate<upperwhisker]
dfDate_out=as.data.frame(dfDate_out)
colnames(dfDate_out)[1] = "PostDateDıff"
dfDate_out$PostDateDıff = as.numeric(dfDate_out$PostDateDıff)

dfDate_out$PostDate<-dfDate$nba_by_postcode.PostDate[dfDate$PostDateDıff>lowerwhisker & dfDate$PostDateDıff<upperwhisker]

ggplot(data=dfDate_out,aes(x=PostDate,y=PostDateDıff))+ geom_jitter(alpha = 1/15) +xlab("Post Date") + 
  ylab("Time Difference (min)") +
  ggtitle("Time Difference Between Two Posts") 

mean(dfDate_out$PostDateDıff)
median(dfDate_out$PostDateDıff)
sd(dfDate_out$PostDateDıff)


dfDate$nba_by_postcode.PostDate[dfDate$PostDateDıff>15000]



##sidecar

#sidecar tryings
############# 


summary(nba_by_postcode$PostDate)

summary(nba_by_postcode$l_v_percent)
#only sidecar post type data
nba_sidecar = nba[nba$PostType == "sidecar"]  #BURAYA BAK SONRA ! ! ! ! !


ggplot(data=nba_by_postcode,aes(x=PostDate,y=LikeCount,color=PostType.f)) + 
  geom_jitter(alpha=1/10) 
  +geom_jitter(aes(y=nba_by_postcode$ViewCount_null),color = 'red',na.rm = TRUE,alpha=1/10)
  #Add after run 

library(lubridate)
#like / view graph
ggplot(data=nba_by_postcode,aes(x=PostDate,l_v_percent,color=PostType.f)) + 
  geom_jitter(alpha=1/10) +
  scale_x_date(limits = ymd(c("2015-07-01","2020-01-01" )))


#textmining





#textmining second question

#textmining second question
########################
#Here, I'm going to try introduce into text mining :)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

text_caption = nba_by_postcode$PostCaption
text_caption=na.omit(text_caption)
text_tags = nba_by_postcode$Tags
textMining=function(list) {
  

text_caption = list
docs_caption <- Corpus(VectorSource(text_caption))
inspect(docs_caption)

#Text Transformation
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs_caption <- tm_map(docs_caption, toSpace, "/")
docs_caption <- tm_map(docs_caption, toSpace, "@")
docs_caption <- tm_map(docs_caption, toSpace, "\\|")
docs_caption <- tm_map(docs_caption, toSpace, ",")

#Cleaning the tex
# Convert the text to lower case
docs_caption <- tm_map(docs_caption, content_transformer(tolower))
# Remove numbers
docs_caption <- tm_map(docs_caption, removeNumbers)
# Remove english common stopwords
docs_caption <- tm_map(docs_caption, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs_caption <- tm_map(docs_caption, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs_caption <- tm_map(docs_caption, removePunctuation)
# Eliminate extra white spaces
docs_caption <- tm_map(docs_caption, stripWhitespace)
# Text stemming
#docs_caption <- tm_map(docs_caption, stemDocument)

#Build a term-document matrix
dtm <- TermDocumentMatrix(docs_caption)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
d$dtm = dtm
return(d)
}

head(d, 10)

mention_freq = textMining(na.omit(nba_by_postcode$Mentions))
head(mention_freq,10)[,c(1,2)]

summary(mention_freq$freq)

#Generate the Word cloud!!!
set.seed(1234)
wordcloud(words = mention_freq$word, freq = mention_freq$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

findAssocs(mention_freq$dtm, terms = "warriors", corlimit = 0.1)

#barplot
barplot(mention_freq[1:10,]$freq, las = 2, names.arg = mention_freq[1:10,]$word,
        col ="blue", main ="Most frequent mentions",
        ylab = "Mention frequencies")




#Third Question

#Third Quest.

#third question
#########
library(magick)
library(lubridate)


nba_by_postcode=mutate(nba_by_postcode, date = ymd_hms(PostDate), PostYear = year(date))


img <- image_graph(600, 340, res = 96)
datalist <- split(nba_by_postcode, nba_by_postcode$PostYear)

out <- lapply(datalist, function(data){
  p <- ggplot(data, aes(PostType.f, round(LikeCount,-3)/1000)) + geom_boxplot() +
    ggtitle(data$PostYear) + theme_classic() + xlab("Post Type") + ylab("Like Count(in thousand)")
  print(p)
})

dev.off()
animation <- image_animate(img, fps = 1)
print(animation)

ggplot(nba_by_postcode, aes(PostType.f, round(LikeCount,-3)/1000)) + geom_boxplot() +
  theme_classic() + xlab("Post Type") + ylab("Like Count(in thousand)")+
  facet_wrap(~nba_by_postcode$PostYear,ncol = 8) + scale_y_continuous(breaks = seq(0,1500,100),limits = c(0,1000))



#some extras
########
tag_freq = textMining(nba_by_postcode$Tags)
head(tag_freq)

mention_freq = textMining(nba_by_postcode$Mentions)
head(mention_freq)

detach("package:magick", unload=TRUE)
library(plotrix)
install.packages("plotrix")
sum_total_freq=sum(mention_freq$freq)
mention_freq$percentage = (mention_freq$freq/sum_total_freq) * 100

pie3D(head(mention_freq$freq),labels=head(mention_freq$word),height= 0.1,labelcex=1,
      explode=.1,
      main = "TOP MENTİONS")

#Again bar plot for mention
barplot(mention_freq[1:10,]$freq, las = 2, names.arg = mention_freq[1:10,]$word,
        col ="blue", main ="Most frequent mention",
        ylab = "Mention frequencies")

#Some trial on mention freq by year gif
mention_freq_2012 = textMining(nba_by_postcode$Mentions[nba_by_postcode$PostYear == 2012])
mention_freq_2013 = textMining(nba_by_postcode$Mentions[nba_by_postcode$PostYear == 2013])
mention_freq_2014 = textMining(nba_by_postcode$Mentions[nba_by_postcode$PostYear == 2014])
mention_freq_2015 = textMining(nba_by_postcode$Mentions[nba_by_postcode$PostYear == 2015])
mention_freq_2016 = textMining(nba_by_postcode$Mentions[nba_by_postcode$PostYear == 2016])
mention_freq_2017 = textMining(nba_by_postcode$Mentions[nba_by_postcode$PostYear == 2017])
mention_freq_2018 = textMining(nba_by_postcode$Mentions[nba_by_postcode$PostYear == 2018])
mention_freq_2019 = textMining(nba_by_postcode$Mentions[nba_by_postcode$PostYear == 2019])

mention_freq_total = rbind(mention_freq_2012,
                           mention_freq_2013,
                           mention_freq_2014,
                           mention_freq_2015,
                           mention_freq_2016,
                           mention_freq_2017,
                           mention_freq_2018,
                           mention_freq_2019)



#machinel 
##########


