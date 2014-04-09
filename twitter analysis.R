library(reshape2)
library(ggplot2)
library(Cairo)

#reads csv into R
twitter1 <- read.csv("C:/Users/Matthew.Mikuni/My Documents/janmfinal.csv")

#makes a list by date 
#http://stackoverflow.com/questions/17179334/count-of-events-per-datetime-r
twitterdate <- aggregate(twitter1, by = list(twitter1$date), length)
twitterday <- aggregate(twitter1, by = list(twitter1$day), length)

#takes the average of the twitter day because it is a two week sum
twitterday$date <- twitterday$date/2

#orders the table from Monday to Sunday
twitterday$Group.1 <- factor(twitterday$Group.1, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

#creates lineplot
#singe line plot ggplot2 http://stackoverflow.com/questions/15978836/ggplot2-geom-line-for-single-observations-x-factor-y-numeric
twitterdayline <- ggplot(twitterday, aes(Group.1, date, group=1)) + geom_line() + geom_point(size=3) + 
  labs(x = "Day of the Week", y = "Tweets received per day") +
  theme(axis.text = element_text(size = 20), strip.text.y = element_text(size = 20), axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 20))

#formats the time column into a character string to strip it next
twitter1$time <- as.character(twitter1$time)

#strips the last three characters from the string
twitter1$time <- substr(twitter1$time, 1, nchar(twitter1$time)-3)

#creates a table based on days of the week and hours
hourtable <- table(twitter1$time, twitter1$day)

#creates a character variable with the days of the week in order
dayorder <- factor(dayorder, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

#reorders the columns into a the correct order then creates a data frame
hourtable <- hourtable[, dayorder]
hourtable <- data.frame(hourtable)

#melts data frame for ggplot, turning hourtable into a matrix lets melt capture the rownames
hourmelt <- melt(as.matrix(hourtable))

hourmelt$Var2 <- factor(hourmelt$Var2, levels = dayorder)

#creates facet table for each day of the week and layers shaded regions over each
hourgraphs <- ggplot(hourmelt, aes(Var1, value)) + geom_line(aes(group = Var2, color = Var2), size = 1) + 
  facet_grid(Var2~.) + geom_point(size = 3) + scale_x_discrete() + 
  geom_rect(aes(xmin=0, xmax=4, ymin=0, ymax=Inf), fill= "green", alpha = .01) + 
  geom_rect(aes(xmin=20, xmax=23, ymin=0, ymax=Inf), fill= "green", alpha = .01) +
  geom_rect(aes(xmin=4, xmax=12, ymin=0, ymax=Inf), fill= "blue", alpha = .01) + 
  geom_rect(aes(xmin=12, xmax=20, ymin=0, ymax=Inf), fill = "red", alpha = .01) + 
  labs(x = "Hour (Pacific Time)", y = "Number of Tweets received per hour", size = 25) +
  theme(axis.text.x = element_text(size = 20), strip.text.y = element_text(size = 20), axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25))
twitterdayline
hourgraphs

