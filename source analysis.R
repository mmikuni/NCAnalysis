library(MASS)
require(COUNT)
library(COUNT)
library(plyr)
library(ggplot2)

analysis <- read.csv("asia sources.csv")

#subtracts NA column from dataset
analysis <- analysis[,-6]

#orders the bars for source quality
analysis$source_quality <- factor(analysis$source_quality, levels = c("Media", "Government", "Unconfirmed", "Utility"))

#subsets by country Thailand
tlsource <- analysis[analysis$country == "Thailand",]
aussource <- analysis[analysis$country == "Australia",]

tlpie <-ggplot(tlsource, aes(factor(1), tlsource$source_quality, fill = 'tlsource$source_origin'))

#counts the number of values and subdivides by country, source_quality, source_impact, source_origin
awesomecount <- count(analysis, c("country", "source_quality", "source_impact", "source_origin"))
auscount <- count(aussource, c("source_quality", "source_origin"))
tlcount <- count(tlsource, c("source_quality", "source_origin"))



#creates stacked bar plots
tlsourcebar <- ggplot(tlcount, aes(source_quality, freq, fill = source_origin)) + geom_bar(stat="identity") + 
  labs(x = "Source Quality", y = "Number of Sources", title = "Source Breakdown") + 
  scale_fill_discrete(name = "Source Type") +
  coord_flip() +
  theme_set(theme_gray(base_size = 12))+
  scale_color_brewer(palette="Accent")+
  theme(axis.text = element_text(size = 12), strip.text.y = element_text(size = 15), axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20), plot.title = element_text(size = 20))


aussourcebar <- ggplot(auscount, aes(source_quality, freq, fill = source_origin)) + geom_bar(stat="identity") +
  labs(x = "Source Quality", y = "Number of Sources", title = "Source Breakdown") + 
  scale_fill_discrete(name = "Source Type")

