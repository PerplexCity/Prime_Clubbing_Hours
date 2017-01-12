library(ggplot2)
bbi <- element_text(face="bold.italic", color="black")

nysc_combo<- read.csv("~/nysc_big.csv")
nysc_combo[nysc_combo==0] <- NA

x <- 0:167
#grabs average of every hour and normalizes 75-pixel scale
y <- colMeans(nysc_combo[,2:169], na.rm = TRUE)/75

weekly_means <- data.frame(x,y)

times = c("Mon 6AM", "Mon 6PM",
          "Tue 6AM", "Tue 6PM",
          "Wed 6AM", "Wed 6PM",
          "Thu 6AM", "Thu 6PM",
          "Fri 6AM", "Fri 6PM",
          "Sat 6AM", "Sat 6PM",
          "Sun 6AM", "Sun 6PM")

weekly_graf<- ggplot(weekly_means, aes(x,y)) + geom_line(size=2, col="red") +
  scale_x_continuous(limits=c(0,168), breaks=seq(6,168,12), 
                     labels=times) +
  scale_y_continuous(limits=c(0,1), breaks=c(0.25, 0.5, 0.75, 1),
                     labels = c("25%", "50%", "75%", "100%")) +
  labs(x="", y="Crowdedness", title="When is the Average
       New York Sports Club Crowded?") + 
  theme(title=bbi, 
        axis.text.x = element_text(size=12,angle = 45, hjust=1),
        axis.text.y = element_text(size=12),
        axis.title.y = element_text(size=15))

#first graph spans 20 hours
x <- 0:19

#isolates correct date/location
third <- colMeans(nysc_combo[12,102:121])/75
varick <- colMeans(nysc_combo[47,102:121])/75

friday_comp <- data.frame(x, third, varick)

friday_graf <- ggplot(friday_comp, aes(x)) + geom_line(aes(y=third, col="41st & Third Ave"), size=2) + 
  geom_line(aes(y=varick, col="Varick Street"), size=2) +
  scale_colour_manual("", breaks=c("41st & Third Ave", "Varick Street"), 
                      values=c("red", "blue")) +
  scale_y_continuous(limits=c(0,1), breaks=c(0.25, 0.5, 0.75, 1),
                     labels = c("25%", "50%", "75%", "100%")) + 
  scale_x_continuous(breaks=c(2, 8, 14),
                     labels=c("6AM", "12PM", "6PM")) +
  labs(x="", y="Crowdedness", title="NYSC Deep Dive #1:
       41st & Third Ave vs. Varick Street on Fridays") +
  theme(title=bbi, 
        axis.text.x = element_text(size=12,angle = 45, hjust=1),
        axis.text.y = element_text(size=12),
        axis.title.y = element_text(size=15),
        legend.text = element_text(size=10))

#second graph spans 13 hours
x <- 6:18
times_sq <- colMeans(nysc_combo[13,152:164])/75
atlas <- colMeans(nysc_combo[36,152:164])/75

sunday_comp <- data.frame(x, times_sq, atlas)

sunday_graf <- ggplot(sunday_comp, aes(x)) + geom_line(aes(y=times_sq, col="41st & Seventh Ave"), size=2) + 
  geom_line(aes(y=atlas, col="Atlas Park"), size=2) +
  scale_colour_manual("", breaks=c("41st & Seventh Ave", "Atlas Park"), 
                      values=c("red", "blue")) +
  scale_y_continuous(limits=c(0,1), breaks=c(0.25, 0.5, 0.75, 1),
                     labels = c("25%", "50%", "75%", "100%")) + 
  scale_x_continuous(breaks=c(6, 12, 18),
                     labels=c("6AM", "12PM", "6PM")) +
  labs(x="", y="Crowdedness", title="NYSC Deep Dive #2:
       41st & Seventh Ave vs. Atlas Park on Sundays") +
  theme(title=bbi, 
        axis.text.x = element_text(size=12,angle = 45, hjust=1),
        axis.text.y = element_text(size=12),
        axis.title.y = element_text(size=15),
        legend.text = element_text(size=10))


#loops through columns looking for big jumps
#Set if to ">35" for increase, "<-30" for decrease
for (i in 3:169){
  for (j in 1:48){
    diff <- nysc_combo[j,i] - nysc_combo[j,i-1]
    if (!is.na(diff) & diff < -30){
      print(diff)
      print(nysc_combo[j,1])
      print(names(nysc_combo)[i])
    }
  }
}

#finds column with highest and lowest standard deviations
max(apply(nysc_combo, 1, sd, na.rm=TRUE))
min(apply(nysc_combo, 1, sd, na.rm=TRUE))

#for final ratio superlatives, sort the last column in nysc_combo
