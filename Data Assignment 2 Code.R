library(janitor)

#Load in csv files
star1 <- read.csv("/Users/allisonking/Downloads/STARD Demo 1.csv")
star2 <- read.csv("/Users/allisonking/Downloads/STARD Demo 2.csv")
qids <- read.csv("/Users/allisonking/Downloads/STARD QIDS.csv")

#Problem 1 Part A
#Merge the two demographic datasets
demo <- star1 %>% left_join(star2)

#Problem 1 Part B
#Count number of observations per subject in QIDS file
countID <- qids %>% count(ID)

#Table of counts
with.counts <- qids %>%
  group_by(ID) %>%
  mutate(var1_count = n()) 

tabyl(with.counts, var1_count)

#Create new dataset of only subjects with 3 or more visits
qids.3 <- qids %>%
  group_by(ID) %>%
  mutate(var1_count = n()) %>%
  filter(var1_count >= 3)

#Delete any .1 visits/weeks
nodec <- subset(qids.3, qids.3$WEEK != 0.1 & qids.3$WEEK != 0.2 & qids.3$WEEK != 2.1 & qids.3$WEEK != 2.2 & qids.3$WEEK != 4.1 & 
                  qids.3$WEEK != 4.2 & qids.3$WEEK != 6.1 & qids.3$WEEK != 6.2 & qids.3$WEEK != 9.1 & qids.3$WEEK != 9.2 & qids.3$WEEK != 12.1 & 
                  qids.3$WEEK != 14.1 & qids.3$WEEK != 14.2)

tabyl(nodec, var1_count)

#Problem 1 Part C
#Create a vector of unique IDs from nodec
id.vec <- unique(nodec$ID)

#Check which vars are skewed
plot(demo[,c(2,3,4)])

demo1 <- demo[which(id.vec %in% demo$ID),]


#All skewed - report median & IQR for cont. vars 
median(demo1$episode_date)
median(demo1$epino)
median(demo1$dage)
IQR(demo1$episode_date)
IQR(demo1$epino)
IQR(demo1$dage)

#Replace negatves with NAs
demo1$epino <- replace(demo1$epino, which(demo1$epino < 0), NA)
demo1$dage <- replace(demo1$dage, which(demo1$dage < 0), NA)








