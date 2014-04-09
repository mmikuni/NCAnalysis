library(xts)
library(reshape2)
library(ggplot2)

#reads the Thailand incidents and assigns it to "tlcsv"
tlcsv <- read.csv("C:/Users/Matthew.Mikuni/Downloads/thailandfull.csv", header = TRUE)


#formats the dates in tlcsv$date occurred to get it to be recognized
tlcsv$Updated <- as.Date(tlcsv$Updated, format = '%m/%d/%Y')

#creates a subset from tlcsv for all incidents that occur after 1/1/2013
tl2013 <- subset(tlcsv, tlcsv$Updated >"2013-01-01")

#gets the number of rows for incidents in JUST 2013
getrows <- subset(tlcsv, tlcsv$Updated >= "2013-01-01" & tlcsv$Updated <= "2013-12-31")


#creates a subset and gets the rows from incidents that took place after 2013 and are Security incidents
tlsec <- subset(tlcsv, tlcsv$Updated >"2013-01-01" & tlcsv$Category == "Security")
getrows2 <- subset(tlcsv, tlcsv$Updated >"2013-01-01" & tlcsv$Category == "Security")


#whole months shortcuts
tjan = tlcsv$Updated >="2013-01-01" & tlcsv$Updated <= "2013-01-31"
tfeb = tlcsv$Updated >="2013-02-01" & tlcsv$Updated <= "2013-02-28"
tmarch = tlcsv$Updated >="2013-03-01" & tlcsv$Updated <= "2013-03-31"
tapril = tlcsv$Updated >="2013-04-01" & tlcsv$Updated <= "2013-04-30"
tmay = tlcsv$Updated >="2013-05-01" & tlcsv$Updated <= "2013-05-31"
tjune = tlcsv$Updated >="2013-06-01" & tlcsv$Updated <= "2013-06-30"
tjuly = tlcsv$Updated >="2013-7-01" & tlcsv$Updated <= "2013-07-31"
taug = tlcsv$Updated >="2013-8-01" & tlcsv$Updated <= "2013-08-31"
tsept = tlcsv$Updated >="2013-9-01" & tlcsv$Updated <= "2013-09-30"
toct = tlcsv$Updated >="2013-10-01" & tlcsv$Updated <= "2013-10-31"
tnov = tlcsv$Updated >="2013-11-01" & tlcsv$Updated <= "2013-11-30"
tdec = tlcsv$Updated >="2013-12-01" & tlcsv$Updated <= "2013-12-31"
t14jan = tlcsv$Updated >="2014-01-01" & tlcsv$Updated <= "2014-01-31"
t14feb = tlcsv$Updated >="2014-02-01" & tlcsv$Updated <= "2014-02-28"

#incident shortcuts
security = tlcsv$Category == "Security" | tlcsv$Category == "Terrorism"
fire = tlcsv$Category == "Fire"
aviation = tlcsv$Category == "Aviation"
infrastructure = tlcsv$Category == "Infrastructure"
meteorological = tlcsv$Category == "Meteorological"
transportation = tlcsv$Category == "Transportation"
advisory = tlcsv$Category == "Advisory"
earthquake = tlcsv$Category == "Geophysical"
other = tlcsv$Category == "Cargo Disruption" | tlcsv$Category == "Other" | tlcsv$Category == "Public Holiday"
hazmat_health = tlcsv$Category == "Hazmat" | tlcsv$Category == "Health"
labor = tlcsv$Category == "Labor"

a1 <- nrow(subset(tlcsv, tjan & security))

a2 <- nrow(subset(tlcsv, tfeb & security))

a3 <- nrow(subset(tlcsv, tmarch & security))

a4 <- nrow(subset(tlcsv, tapril & security))

a5 <- nrow(subset(tlcsv, tmay & security))

a6 <- nrow(subset(tlcsv, tjune & security))

a7 <- nrow(subset(tlcsv, tjuly & security))

a8 <- nrow(subset(tlcsv, taug & security))

a9 <- nrow(subset(tlcsv, tsept & security))

a10 <- nrow(subset(tlcsv, toct & security))

a11 <- nrow(subset(tlcsv, tnov & security))

a12 <- nrow(subset(tlcsv, tdec & security))

a13 <- nrow(subset(tlcsv, t14jan & security))

a14 <- nrow(subset(tlcsv, t14feb & security))


b1 <- nrow(subset(tlcsv, tjan & fire))

b2 <- nrow(subset(tlcsv, tfeb & fire))

b3 <- nrow(subset(tlcsv, tmarch & fire))

b4 <- nrow(subset(tlcsv, tapril & fire))

b5 <- nrow(subset(tlcsv, tmay & fire))

b6 <- nrow(subset(tlcsv, tjune & fire))

b7 <- nrow(subset(tlcsv, tjuly & fire))

b8 <- nrow(subset(tlcsv, taug & fire))

b9 <- nrow(subset(tlcsv, tsept & fire))

b10 <- nrow(subset(tlcsv, toct & fire))

b11 <- nrow(subset(tlcsv, tnov & fire))

b12 <- nrow(subset(tlcsv, tdec & fire))

b13 <- nrow(subset(tlcsv, t14jan & fire))

b14 <- nrow(subset(tlcsv, t14feb & fire))


c1 <- nrow(subset(tlcsv, tjan & transportation))

c2 <- nrow(subset(tlcsv, tfeb & transportation))

c3 <- nrow(subset(tlcsv, tmarch & transportation))

c4 <- nrow(subset(tlcsv, tapril & transportation))

c5 <- nrow(subset(tlcsv, tmay & transportation))

c6 <- nrow(subset(tlcsv, tjune & transportation))

c7 <- nrow(subset(tlcsv, tjuly & transportation))

c8 <- nrow(subset(tlcsv, taug & transportation))

c9 <- nrow(subset(tlcsv, tsept & transportation))

c10 <- nrow(subset(tlcsv, toct & transportation))

c11 <- nrow(subset(tlcsv, tnov & transportation))

c12 <- nrow(subset(tlcsv, tdec & transportation))

c13 <- nrow(subset(tlcsv, t14jan & transportation))

c14 <- nrow(subset(tlcsv, t14feb & transportation))


d1 <- nrow(subset(tlcsv, tjan & labor))

d2 <- nrow(subset(tlcsv, tfeb & labor))

d3 <- nrow(subset(tlcsv, tmarch & labor))

d4 <- nrow(subset(tlcsv, tapril & labor))

d5 <- nrow(subset(tlcsv, tmay & labor))

d6 <- nrow(subset(tlcsv, tjune & labor))

d7 <- nrow(subset(tlcsv, tjuly & labor))

d8 <- nrow(subset(tlcsv, taug & labor))

d9 <- nrow(subset(tlcsv, tsept & labor))

d10 <- nrow(subset(tlcsv, toct & labor))

d11 <- nrow(subset(tlcsv, tnov & labor))

d12 <- nrow(subset(tlcsv, tdec & labor))

d13 <- nrow(subset(tlcsv, t14jan & labor))

d14 <- nrow(subset(tlcsv, t14feb & labor))


e1 <- nrow(subset(tlcsv, tjan & earthquake))

e2 <- nrow(subset(tlcsv, tfeb & earthquake))

e3 <- nrow(subset(tlcsv, tmarch & earthquake))

e4 <- nrow(subset(tlcsv, tapril & earthquake))

e5 <- nrow(subset(tlcsv, tmay & earthquake))

e6 <- nrow(subset(tlcsv, tjune & earthquake))

e7 <- nrow(subset(tlcsv, tjuly & earthquake))

e8 <- nrow(subset(tlcsv, taug & earthquake))

e9 <- nrow(subset(tlcsv, tsept & earthquake))

e10 <- nrow(subset(tlcsv, toct & earthquake))

e11 <- nrow(subset(tlcsv, tnov & earthquake))

e12 <- nrow(subset(tlcsv, tdec & earthquake))

e13 <- nrow(subset(tlcsv, t14jan & earthquake))

e14 <- nrow(subset(tlcsv, t14feb & earthquake))


f1 <- nrow(subset(tlcsv, tjan & meteorological))

f2 <- nrow(subset(tlcsv, tfeb & meteorological))

f3 <- nrow(subset(tlcsv, tmarch & meteorological))

f4 <- nrow(subset(tlcsv, tapril & meteorological))

f5 <- nrow(subset(tlcsv, tmay & meteorological))

f6 <- nrow(subset(tlcsv, tjune & meteorological))

f7 <- nrow(subset(tlcsv, tjuly & meteorological))

f8 <- nrow(subset(tlcsv, taug & meteorological))

f9 <- nrow(subset(tlcsv, tsept & meteorological))

f10 <- nrow(subset(tlcsv, toct & meteorological))

f11 <- nrow(subset(tlcsv, tnov & meteorological))

f12 <- nrow(subset(tlcsv, tdec & meteorological))

f13 <- nrow(subset(tlcsv, t14jan & meteorological))

f14 <- nrow(subset(tlcsv, t14feb & meteorological))


g1 <- nrow(subset(tlcsv, tjan & infrastructure))

g2 <- nrow(subset(tlcsv, tfeb & infrastructure))

g3 <- nrow(subset(tlcsv, tmarch & infrastructure))

g4 <- nrow(subset(tlcsv, tapril & infrastructure))

g5 <- nrow(subset(tlcsv, tmay & infrastructure))

g6 <- nrow(subset(tlcsv, tjune & infrastructure))

g7 <- nrow(subset(tlcsv, tjuly & infrastructure))

g8 <- nrow(subset(tlcsv, taug & infrastructure))

g9 <- nrow(subset(tlcsv, tsept & infrastructure))

g10 <- nrow(subset(tlcsv, toct & infrastructure))

g11 <- nrow(subset(tlcsv, tnov & infrastructure))

g12 <- nrow(subset(tlcsv, tdec & infrastructure))

g13 <- nrow(subset(tlcsv, t14jan & infrastructure))

g14 <- nrow(subset(tlcsv, t14feb & infrastructure))


h1 <- nrow(subset(tlcsv, tjan & advisory))

h2 <- nrow(subset(tlcsv, tfeb & advisory))

h3 <- nrow(subset(tlcsv, tmarch & advisory))

h4 <- nrow(subset(tlcsv, tapril & advisory))

h5 <- nrow(subset(tlcsv, tmay & advisory))

h6 <- nrow(subset(tlcsv, tjune & advisory))

h7 <- nrow(subset(tlcsv, tjuly & advisory))

h8 <- nrow(subset(tlcsv, taug & advisory))

h9 <- nrow(subset(tlcsv, tsept & advisory))

h10 <- nrow(subset(tlcsv, toct & advisory))

h11 <- nrow(subset(tlcsv, tnov & advisory))

h12 <- nrow(subset(tlcsv, tdec & advisory))

h13 <- nrow(subset(tlcsv, t14jan & advisory))

h14 <- nrow(subset(tlcsv, t14feb & advisory))


#testing using '[' instead of subset
whytypedec = tlcsv$Updated >="2013-12-01" & tlcsv$Updated <= "2013-12-31"

whytype <- tlcsv[whytypedec & tlcsv$Category == "Security", ]

tlsectest <- tlcsv[tlcsv$Updated >="2013-1-01" & tlcsv$Updated <= "2013-12-31" & 
                     (tlcsv$Category =="Security" | tlcsv$Category == "Fire"),]



#says what the 'x' data frame will contain
a = c(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) #security
b = c(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14) #fire
c = c(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14) #transportation
d = c(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14) #Labor
e = c(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14) #earthquake
f = c(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14) #meteorlogical
g = c(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, g13, g14) #infrastructure
h = c(h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14) #advisory

#finds the means of each section, from http://stackoverflow.com/questions/3497464/element-wise-mean-in-r
allmeans <- colMeans(cbind(a,b,c,d,e,f,g,h))

month = c('Jan 2013', 'Feb 2013', 'Mar 2013', 'Apr 2013', 'May 2013', 'Jun 2013', 'Jul 2013', 'Aug 2013', 'Sep 2013', 'Oct 2013', 'Nov 2013', 'Dec 2013', 'Jan 2014', 'Feb 2014')
month <- factor(month, levels = month)
incidenttype = c('Security')

#oh my glob data frame
tlyes <- data.frame('Advisory' = h, 'Security' = a, 'Fire' = b, 'Transportation' = c, 'Labor' = d, 'Earthquakes' = e, 'Meteorological' = f, 'Infrastructure' = g, row.names = month)


#adds months to an actual column in the data frame instead of colnames
tlyes2 <- data.frame(month = month, tlyes)
nosectlyes <- tlyes2[, -3]
sectlyes<- tlyes2[, c(1,3)]

#melts the data frame by each object
tlyes3 <- melt(tlyes2, id='month', variable_name='series')
nosectlyes3 <- melt(nosectlyes, id='month', variable_name='series')
sectlyes3 <- melt(sectlyes, id='month', variable_name='series')


#plots everything on single graph
tlline <- ggplot(tlyes3, aes(month, value)) + geom_line(aes(group = variable, color = variable, size=.1))
sectlline <-ggplot(sectlyes3, aes(month, value)) + 
  geom_line(aes(group = variable), color = "black", size = 3) + facet_grid(variable~.) +
  labs(x = "Month", y = "Number of Incidents", title = "Thailand Incident Breakdown", colour = "Incident Category")+
  theme(axis.text = element_text(size = 12), strip.text.y = element_text(size = 15), axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 20), plot.title = element_text(size = 25))

#plots everything on separate graphs
tlsepline <- ggplot(tlyes3, aes(month, value)) + geom_line(aes(group = variable, color = variable), size = 3) + 
  facet_grid(variable~.) +
  scale_color_brewer(palette="Accent") + 
  labs(x = "Month", y = "Number of Incidents", title = "Thailand Incident Breakdown", colour = "Incident Category")

nosectlsepline <- ggplot(nosectlyes3, aes(month, value)) + 
  geom_line(aes(group = variable, color = variable), size = 3) + facet_grid(variable~.) +
  labs(x = "Month", y = "Number of Incidents", title = "Thailand Incident Breakdown", colour = "Incident Category", size = 25) +
  theme(axis.text = element_text(size = 12), strip.text.y = element_text(size = 15), axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25), plot.title = element_text(size = 25)) +
  guides(colour = FALSE)



#barplot for incidents
tbar <- barplot(tlyes$Security, names = month)




library(zoo)
library(chron)


#-----------------------
#trying http://stackoverflow.com/questions/11369167/partitioning-data-set-by-time-intervals-in-r
tlcsv3 <-read.csv("C:/Users/Matthew.Mikuni/Downloads/thailandfull.csv", header = TRUE, as.is = TRUE)



#remove uncessary rows
tlcsv3 <- tlcsv3[-554:-555, ]

#formats the date as month/year
#http://stackoverflow.com/questions/9749598/r-obtaining-month-and-year-from-a-date
tlcsv$Updated <- as.yearmon(tlcsv$Updated)

library(plyr)
awesomecount2 <- count(tlcsv, c("tlcsv$Updated", "tlcsv$Category"))
