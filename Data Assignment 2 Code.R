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

#Categorical percentages
demo1 %>% group_by(DEP) %>% summarise(Percentage=n()/nrow(.))
demo1 %>% group_by(BIP) %>% summarise(Percentage=n()/nrow(.))
demo1 %>% group_by(ALCOHOL) %>% summarise(Percentage=n()/nrow(.))
demo1 %>% group_by(DRUG_PHX) %>% summarise(Percentage=n()/nrow(.))
demo1 %>% group_by(SUIC_PHX) %>% summarise(Percentage=n()/nrow(.))
demo1 %>% group_by(SEX) %>% summarise(Percentage=n()/nrow(.))
demo1 %>% group_by(HSPNC) %>% summarise(Percentage=n()/nrow(.))
demo1 %>% group_by(WHITE) %>% summarise(Percentage=n()/nrow(.))
demo1 %>% group_by(BLACK) %>% summarise(Percentage=n()/nrow(.))
demo1 %>% group_by(ASIAN) %>% summarise(Percentage=n()/nrow(.))
demo1 %>% group_by(AMIND) %>% summarise(Percentage=n()/nrow(.))
demo1 %>% group_by(HAWAI) %>% summarise(Percentage=n()/nrow(.))

#Replace negatives with NAs
demo1$epino <- replace(demo1$epino, which(demo1$epino < 0), NA)
demo1$dage <- replace(demo1$dage, which(demo1$dage < 0), NA)
demo1$DEP <- replace(demo1$DEP, which(demo1$DEP < -3), NA)
demo1$BIP <- replace(demo1$BIP, which(demo1$BIP < -3), NA)
demo1$ALCOHOL <- replace(demo1$ALCOHOL, which(demo1$ALCOHOL < -3), NA)
demo1$DRUG_PHX <- replace(demo1$DRUG_PHX, which(demo1$DRUG_PHX < -3), NA)
demo1$SUIC_PHX <- replace(demo1$SUIC_PHX, which(demo1$SUIC_PHX < -3), NA)
demo1$HSPNC <- replace(demo1$HSPNC, which(demo1$HSPNC < 0), NA)

#Problem 1 Part D
#Based on counts, split up into white and other
demo1$OTHER <- ifelse(demo1$ASIAN == 1, 1, ifelse(demo1$AMIND == 1, 1, ifelse(demo1$BLACK == 1, 1, ifelse(demo1$HAWAI == 1, 1, 0))))

#Problem 1 Part E
demo1$Any.Fam.Psyc.Hist <- ifelse(demo1$DEP == 1, 1, ifelse(demo1$BIP == 1, 1, ifelse(demo1$ALCOHOL == 1, 1, ifelse(demo1$DRUG_PHX == 1, 1, 
                           ifelse(demo1$SUIC_PHX == 1, 1, 0)))))

#Problem 2 Part A
#Spaghetti plot 
ggplot(data = qids.3, aes(x = WEEK, y = QSTOT, group = ID)) + geom_line(linetype="dashed", color = "blue") + 
  theme_bw() + scale_x_continuous(breaks = c(0,2,4,6,9,12,14), name = "Week") + scale_y_continuous(name = "QIDS Score") + 
  ggtitle("Spaghetti Plot of QIDS Score by Week for Each Subject")

#Problem 2 Part B
#Jitter the data
qids.3$jweek <- jitter(qids.3$WEEK)
with(qids.3, head(cbind(ID, WEEK, jweek), n = 14))

ggplot(data = qids.3, aes(x = jweek, y = QSTOT)) + geom_point(shape = 1) + scale_x_continuous(breaks = c(0,2,4,6,9,12,14), name = "Week") + 
  scale_y_continuous(name = "QIDS Score") + stat_summary(fun = mean, geom = "point", size=2, shape = 19) + 
  stat_smooth(se = FALSE, color = "purple") + ggtitle("QIDS Score by Week for Each Subject with Loess Curve")






