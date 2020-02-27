# # install.packages("tidyverse")
# # install.packages("pacman")
# # install.packages("rio")
# # install.packages("party")
# # 
# # pacman::p_load(pacman, rio, party, tidyverse)
# # 
# # df <- import("C:/Users/mohit/Downloads/data/StateData.xlsx")%>% 
# #   as_tibble()%>%
# #   select(state_code, 
# #          region, 
# #          psychRegions)%>%
# #   mutate(psychRegions = as.factor(psychRegions))%>%
# #   print ()
# # 
# # str(UCBAdmissions)
# # UCBAdmissions
# # plot(UCBAdmissions)
# # 
# # admit.fail <- (UCBAdmissions)
# # admit.fail
# # plot(UCBAdmissions)
# # 
#Reading .csv file
#str provides information on one line for each basic structure
#Summary provides summary about each column
read.csv("C:/Users/mohit/Downloads/data/StateData.csv") -> Data_science
str(Data_science)
summary(Data_science)
 
#Data Cleaning (we can change data in any column)
#These symbols '%>%' are used to pipe command
library(dplyr)
Data_science%>%select(c(-1,-2)) -> Data_science
Data_science$extraversion<-factor(Data_science$extraversion,labels=c("Yes","No"))
 
?aes
library(ggplot2)
ggplot(data = Data_science,aes(x=agreeableness))+geom_histogram(bins = 40)
ggplot(data = Data_science,aes(x=agreeableness))+geom_histogram(bins= 40,fill="Lightblue",col="blue")
ggplot(data = Data_science,aes(x=agreeableness)) #no histo just blank
ggplot(data = Data_science,aes(y=agreeableness,x=gdpr,fill="agreeableness"))+geom_boxplot()

#splitting Data
library(caTools)
sample.split(Data_science$agreeableness,SplitRatio = 0.5)->split_index
train<-subset(Data_science,split_index==T)
test<-subset(Data_science,split_index==F)
nrow(train)
nrow(test)

#model building
lm(agreeableness~.,data = train)-> mod1
predict(mod1,test)-> result
cbind(actual=test$agreeableness,predicted=result) -> compare_result
as.data.frame(compare_result)->compare_result
compare_result$actual-compare_result$predicted->error
cbind(compare_result,error)
sqrt(mean(compare_result$error^2))->rmse1


plot("state_code,region")
 
#Quits Rstudio
q()
 
#demo is used for graphically sense about R graphics
#demo(Hershey)
#demo(persp())
#demo(image())
# 
#preinstalled dataset , mpg=miles per gallon, wt = cars weight
#running simple linear regression
lm(mpg~wt, data = mtcars)
 
#results displayed in console but no info saved.
# 
#now info saved but no output in console
#list object is created
lmfit <- lm(mpg~wt, data = mtcars)
 
#now summary of results are displayed
summary(lmfit)
 
#diagnostic plots are displayed
#we can run this many times it will change the graph structure
plot(lmfit)
 
 

cook <- cooks.distance(lmfit)
plot(cook)
 
predict(lmfit, mynewdata)
 
#help function opens a browser window with introduction, advanced manuals, FAQs, reference materials
help(lm)
?lm
# 
#ls functions lists objects in workspace
ls()

#select all code then ctrl+shift+C will comment all out
 
#Displays last commands used
history()
 
#Save function is used to save object, history, image)
savehistory("makora")
 
#vectors is one dimensional array that holds numeric,character and logical data
#c() This is combine function used to form vector
a<- c(1,2,3,4,5) #numeric
b<- c("Mohit","leena","nikhil","varun","kito","priya") #character
c<- c(TRUE, FALSE) #Logical

#DataFrame
StudentID <- c(111, 222, 333, 444, 555)
StudentName <- c("Maria", "picolo", "zian", "Goku", "suzuka")
CourseID <- c(123, 456, 789, 132, 465)
CourseName <- c("Probandstats" , "DBMS", "Analytics", "ML", "Datamining")
status <- c("Done", "Pending", "Not Submitted", "Pending", "Inappropriate")
StudentsData <- data.frame(StudentID, StudentName, CourseID, CourseName, status)
StudentsData

StudentsData$CourseID

table(StudentsData$StudentName, StudentsData$CourseName)
table(StudentsData$StudentID, StudentsData$CourseID)

#With Function is used instead of Attach and Detach when multiple objects with same name takes place:
with(StudentsData,{
  mydata <- summary(StudentsData)
  mydata <<- summary(StudentsData)
})
mydata

#Case Identifiers
StudentsData <- data.frame(StudentID, StudentName, CourseID, CourseName, row.names = StudentID)
StudentsData

#factor function is used to assign levels such as 1 = Done, 2 = Pending, 3 = Not submitted, 4 = Inappropriate
status <- factor(status, ordered = TRUE)
status

#List is created with 4components: String, numeric vector, matrix and character vector
m <- "Shopping_List"
l <- c(120, 90, 45, 152, 450)
n <- matrix(1:10, nrow = 5)
o <- c("Snow_jacket", "Snow_shoe", "Gloves", "Sprint_plan", "i_watch")
shopping_List <- list(title=m, price=l, n, o)
shopping_List

#Webscraping
#Getting the page source
web_page <- readLines("https://www.kaggle.com/c/bluebook-for-bulldozers/data")

author_lines <- web_page[grep("<science<", web_page)]

# Get the page's source
web_page <- read.table("http://www.programmingr.com/jan09rlist.html")

# Pull out the appropriate line
author_lines <- web_page[grep("<I<", web_page)]

# Delete unwanted characters in the lines we pulled out
authors <- gsub("<I<", "", author_lines, fixed = TRUE)

# Present only the ten most frequent posters
author_counts <- sort(table(authors), decreasing = TRUE)
author_counts[1:10]

#Importing Data from Stata (Failed importing due to error in read.dta)
library(foreign)
mydataframe <- read.dta("mydata.dta")


#Importing Data from Stata:
mydata <- read.dta("http://dss.princeton.edu/training/students.dta")
mydata.dta <- read.dta("http://dss.princeton.edu/training/mydata.dta",
           convert.factors=TRUE,
           convert.dates=TRUE,
           convert.underscore=TRUE,
           warn.missing.labels=TRUE)
mydata.dta
  
#lists first six rows of the data frame
head(StudentsData)

#lists last six rows of the data frame
tail(StudentsData)


cells <- c(4,5,6,7)
matrix(cells)


str(UCBAdmissions)
UCBAdmissions

plot(UCBAdmissions)

#Get marginal frequencies from new table
margin.table(UCBAdmissions, 1)
margin.table(UCBAdmissions, 2)
margin.table(UCBAdmissions, 3)
margin.table(UCBAdmissions)

#Save Marginals as new table
admit.dept <- margin.table(UCBAdmissions, 3)
str(admit.dept)
barplot(admit.dept)
admit.dept #show frequencies

prop.table(admit.dept) # show as proportions

round(prop.table(admit.dept),2) #round functions roundoffs value to less decimals

round(prop.table(admit.dept),2) * 100 #gives percentages(removes decimals)


#Go from table to one row per case
admit1 <- as.data.frame.table(UCBAdmissions)

#Repeats each row by frequencies
admit2 <- lapply(admit1, function(x)rep(x, admit1$Freq))

#converts from list back to data frame
admit3 <- as.data.frame(admit2)

#Removes column with frequencies
admit4 <- admit3[,-3]

#webpage with pdfs of colors in R
browseURL("http://research.stowers-institute.org/efg/R/Color/Chart/")

#gives list of color names
colors()
#R has names for 657 colors

?rgb
?col2rgb #can convert color names to rgb

col2rgb("navyblue")

x <- c(3,12,7,9,15,20)
#RGB Hexcodes
barplot(x, col = '#FFEBCD') #blanchedalmond

#colorpalettes
help(package="colorspace")
palette()
barplot(x, col = 1:6)
barplot(x, col = rainbow(6))
barplot(x, col = heat.colors(6))
barplot(x, col = terrain.colors(6))
barplot(x, col = topo.colors(6))
barplot(x, col = cm.colors(6))
palette("default")

rm(list = ls()) #cleanup environment

library(ncdf4)
nc <- nc_open("mynetcdffile")
myarray <- get.var.ncdf()

head(mtcars)
#plotting and saving Pdf

pdf("mygraph.pdf") 
attach(mtcars)
plot(wt, mpg)
abline(lm(mpg~wt))
title("Regression of MPG on weight")
detach(mtcars)
dev.off()

#plotting and saving jpeg
jpeg("mygraph.jpeg")
attach(mtcars)
plot(wt, mpg)
abline(lm(mpg~wt))
title("Regression of MPG on weight")
detach(mtcars)
dev.off()

dev.new()
attach(mtcars) #Do we need to attach everytime while creating new graph?
hist(mpg, col = 1:5, col.main = "orange",col.sub = "white", bg = "Blue", fg = "black") #col function colors the graph 
detach(mtcars)
dev.off()

dev.next()
boxplot(mpg, qsec)
title("BOXPLOT")
dev.off()

#Brewer Package
install.packages("RColorBrewer")
library(RColorBrewer)
m <- 7
mycolors <- brewer.pal(m, "Set1")
barplot(rep(1,m), col=mycolors) #returns a vector 7brewer colors


brewer.pal.info #provides info about brewer colors
display.brewer.all() #displays all brewer colors

gray(0:10/10) #produces 10gray levels

# below code reflects 10diff gray colors
l <- 10
mycolors <- rainbow(l)
pie(rep(1, l), labels = mycolors, col=mycolors)
mygrays <- gray(0:l/l)
pie(rep(1, l), labels = mygrays, col = mygrays)
par(font.lab=3, cex.lab=1.5, font.main=4, cex.main=2)

#Graph Appearance
Vans <- c(90, 45, 75, 65, 55)
Skate <- c(39, 49, 59, 69, 79)
Canvas <- c(29, 35, 47, 52, 68)

opar <- par(no.readonly = TRUE)
par(pin=c(2, 3))
par(lwd=2, cex=1.5)
par(cex.axis=0.75, font.axis=100)
plot(Vans, Skate, main = "Shoe range", type = "b", pch=19, lty=2, col="red")
plot(Vans, Canvas, main = "Shoe range", type = "b",pch=23, lty=6, col="blue", bg="red")
par(opar)

#lty changes line type and pch changes the default symbol for plotting points to a solid triangle or whichever have selected
# cex is used in increasing size of plotting symbols
#lwd is used for increasing line width
#Pch specifies symbol to use when plotting points
windowsFonts()

#font is used to change font style for font.sub, font.axis, font.lab, font.main, fg, bg


attach(mtcars)
plot(wt, mpg,
     main="Mileage vs. Car Weight",
     xlab="Weight", ylab="Mileage",
     pch=18, col="blue")
text(wt, mpg,
     row.names(mtcars),
     cex=0.6, pos=4, col="red")
detach(mtcars)
# In aforemention code 'text' function plays a role of labelled points 


opar <- par(no.readonly=TRUE)
par(cex=1.5)
plot(1:7,1:7,type="n")
text(3,3,"Example of default text")
text(4,4,family="mono","Example of mono-spaced text")
text(5,5,family="serif","Example of serif text")
par(opar)
#aforementioned code displays example of font family
plot.new()
demo(plotmath)

#Below code displays combined graphs with the help of matrix (mf)
attach(mtcars)
opar <- par(no.readonly=TRUE)
par(mfrow=c(2,2))
plot(wt,mpg, main="Scatterplot of wt vs. mpg")
plot(wt,disp, main="Scatterplot of wt vs. disp")
hist(wt, main="Histogram of wt")
boxplot(wt, main="Boxplot of wt")
par(opar)
detach(mtcars)

#3graphs combined in one 
attach(mtcars)
opar <- par(no.readonly=TRUE)
par(mfrow=c(3,1))
hist.(wt, ann=FALSE)
hist(mpg, ann=FALSE)
hist(disp, ann=FALSE)
par(opar)
detach(mtcars)

#layout function to combine 3graphs (mat)matrix object is used to specify location of multiple plots to combine

attach(mtcars)
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
hist(wt)
hist(mpg)
hist(disp)
detach(mtcars)

install.packages("Hmisc")
library(Hmisc)

x <- c(1:10)
y <- x
z <- 10/x
opar <- par(no.readonly = TRUE)

par(mar=c(5, 4, 4, 8) + 0.1)
plot(x, y, type="b", pch=21, col="red", yaxt="n", lty=3, ann = FALSE)
lines(x, z, type = "b", pch=22, col="blue", lty=2)
axis(2, at=x, labels = x, col.axis="red", las=2)
axis(4, at=z, labels=round(z, digits=2),
     col.axis="blue", las=2, cex.axis=0.7, tck=-.01)
mtext("y=1/x", sides=4, line=3, cex.lab=1, las=2, col="blue")
title("An Example of creative axis",
      xlab="X values",
      ylab="Y=X")
minor.tick(nx=2, ny=3, tick.ratio = 0.5)
par(opar)

#minor.tick adds tick line on x and y axis

today <- Sys.Date()
dob <- as.Date("1994-08-12")
difftime(today, dob, units = "weeks")
#difftime function is used to calculate difference b/w dates

help("as.Date")
help("strftime")
help("ISOdatetime")

install.packages("sqldf")
library(sqldf)

newdf <- sqldf("select * from mtcars where carb=1 order by mpg",
               row.names=TRUE)
sqldf("select avg(mpg) as avg_mpg, avg(disp) as avg_disp, gear from
      mtcars where cyl in (4, 6) group by gear")

UCBAdmissions
admit1 <- as.data.frame.table(UCBAdmissions)
admit2 <- lapply(admit1, function(x)rep(x, admit1$Freq))


t = c(12, 4, 18, 22, 27, 9)
barplot(t)

browseURL("http://colorbrewer2.org/")
install.packages("RColorBrewer")
require(RColorBrewer) #loads required package

display.brewer.all() #shows all of the palletes in graphic window

#displays colors in separate window, give number of desired color and name of palette in quotes
display.brewer.pal(8, "Accent")
display.brewer.pal(4, "Spectral")

#can save a palette as vector
blues <- brewer.pal(6, "Blues")
barplot(t, col = blues)

barplot(t, col = brewer.pal(6, "Greens"))
barplot(t, col = brewer.pal(6, "RdGy"))

#cleanup
palette("default") #return to default
detach("package:RColorBrewer", unload = TRUE)
rm(list = ls()) #clears the environment
#ctrl+l clears the console

n <- 5 #number of bars
o <- c(rep(10,n)) #create and number of bars of uniform height
barplot(o, col = rainbow(n))

# 1.show 5diff categories
n <- 5
o <- c(rep(10,n))
barplot(o, col = rainbow(n))

# 2.show 8 sequential values
n <- 8
o <- c(rep(10,n))
barplot(o, col = heat.colors(n))

# 3. show 11 divergent values
n <- 11
o <- c(rep(10,n))
barplot(o, col = cm.colors(n))

# 4. show 7 different categories
require(RColorBrewer)
n <- 7
o <- c(rep(10,n))
barplot(o, col = brewer.pal(n, "Set1"))
display.brewer.pal(n, "Set1")

#5. Show 6 sequential values
n <- 6
o <- c(rep(10,n))
barplot(o, col = brewer.pal(n, "BuPu"))
display.brewer.pal(n, "BuPu")

#6. Show 9 divergent values
n <- 9
o <- c(rep(10,n))
barplot(o, col = brewer.pal(n, "PRGn"))
display.brewer.pal(n, "PRGn")

help("CRAN.packages")
?scan

#creating barchats for categorical variables
require(datasets)
chickwts
data("chickwts")


plot(chickwts$feed)


feeds <- table(chickwts$feed)
feeds
barplot(feeds)

#To put the bars in descending order, add "order": (BOX brackets)
barplot(feeds[order(feeds, decreasing = TRUE)])

#customize the chart (par=parameter, oma=Outsidemargin, mar=plotsmargin)
par(oma = c(1, 1, 1, 1)) #sets outside margins: b, l, t, r
par(mar = c(4, 5, 2, 1)) # sets plot margin
barplot(feeds[order(feeds)],
        horiz = TRUE,
        las = 1, #las gives orientation of axis labels
        col = c("beige", "blanchedalmond", "bisque1", "bisque2",
                "bisque3", "bisque4"),
        border = NA, #No borders on bars
        main = "Frequencies of Different Feeds\nin chickwts Datasets",
        #\n =line break
        xlab = "Number of chicks")

#piechart
pie(feeds)

#modifying Piechart
pie(feeds[order(feeds, decreasing = TRUE)],
    init.angle = 90, # starts as 12'o clock instead of 3
    clockwise = TRUE, #default is FALSE
    col = c("seashell", "cadetblue2", "lightpink", "lightcyan", "plum1",
            "papayawhip"),
    main = "pie chart of feeds from chickwts")

pie.a <- c(22, 14, 18, 20, 14, 12)
pie.b <- c(20, 18, 16, 18, 16, 12)
pie.c <- c(12, 14, 20, 16, 18, 12)

#changing graphical parameters
oldpar <- par()
par(mfrow = c(1,3),# Number of rows, columns
    cex.main=3) #main title 3x bigger
colors <- c("grey98", "grey90", "lightskyblue", "lightgreen", "grey98", "grey90")

#three pie charts side by side
pie(pie.a, main = "Pie A", col = colors)
pie(pie.a, main = "Pie B", col = colors)
pie(pie.a, main = "Pie C", col = colors)

#piecharts are very bad way of displaying data


#histogram
lynx
hist(lynx)

#modify histogram
h <- hist(lynx, breaks = 11, freq = FALSE, col = "thistle1", 
          main = "Histogram of Annual Canadian Lynx Trappings\n1821-1934")

#curve function will draw a normal distribution with the same mean and standard deviation on top of histogram
#dnorm is density normal
curve(dnorm(x, mean=mean(lynx), sd = sd(lynx)),
      col="thistle4",
      lwd=2,
      add=TRUE) #add will apply this function in existing chart

#Boxplot
boxplot(USJudgeRatings)
boxplot(USJudgeRatings$RTEN)

#modify Boxplot
boxplot(USJudgeRatings$RTEN,
        horizontal = TRUE,
        lwd = 2,
        las = 2, #makes all labels horizontal
        notch = TRUE, #notches for CI for median
        ylim = c(0, 10), #specify range on Y axis
        col = "slategray3", 
        boxwex = 0.5, #width of box as proportion of original
        xlab = "Lawyer's Ratings",
        whisklty = 1, #whisker line type = 1 is solid line
        staplelty = 0, #staple (line at end) 0=none will remove lines
        outcol = "slategray3") #outcol fills color in outliers 

boxplot(USJudgeRatings,
        horizontal = TRUE,
        lwd = 2,
        las = 2, #makes all labels horizontal
        notch = TRUE, #notches for CI for median
        ylim = c(0, 10), #specify range on Y axis
        col = "slategray3", 
        boxwex = 0.5, #width of box as proportion of original
        xlab = "Lawyer's Ratings",
        whisklty = 1, #whisker line type = 1 is solid line
        staplelty = 0, #staple (line at end) 0=none will remove lines
        outcol = "slategray3") #outcol fills color in outliers 



swiss
str(swiss)
fertility = c(swiss$Fertility)

jpeg("FertilityHIstogram.jpeg")
par(oma = c(1, 1, 1, 1))
par(mar = c(4, 5, 3, 2))
#overlaying plots
h <- hist(fertility,
          prob = TRUE, #Flipside of "freq = FALSE"
          ylim = c(0, 0.04),
          xlim = c(20, 100),
          breaks = 11,
          col = "slategray3",
          border = 0,
          main = "Fertility for 47 french-speaking\nSwiss Provinces, c. 1888")

#plot2: normal curve (if prob = TRUE)
curve(dnorm(x, mean = mean(fertility), sd = sd(fertility)),
      col = 'red',
      lwd = 3,
      add = TRUE)

#plot3&4: kernel density lines
lines(density(fertility), col = "blue")
lines(density(fertility, adjust = 3), col = "darkgreen")

#plot 5: rug plot
rug(fertility, col = "black")

dev.off()

iris

par(mar=c(1,1,1,1))
petal_length = c(iris$Petal.Length)
hist(petal_length, col = "slategray3", breaks = 12, prob = TRUE,
     border = 0)

lines(density(petal_length), col = "darkred", lwd = 2)
rug(petal_length, col = "darkgreen")

groups <- c(rep("blue", 3990),
            rep("red", 4141),
            rep("purple", 5100),
            rep("Maroon", 6123),
            rep("mergenda", 8328))

groups.t1 <- table(groups)
groups.t1

groups.t2 <- sort(groups.t1, decreasing = TRUE)
groups.t2

prop.table(groups.t2)
round(prop.table(groups.t2), 2) * 100

summary(cars) #summary for entire table
summary(cars$speed) #summary for one variable
summary(cars$dist) #summary for one variable

fivenum(cars$speed) #TUkeys five number summary: minimum, lower-hinge, median, upper-hinge, maximum. No labels
fivenum(cars$dist)

#boxplot : hinges, n, CI, outliers
boxplot.stats(cars$speed)

install.packages("psych")
require(psych)
describe(cars)

#Using single proportin: Hypothesis test and COnfidence INterval
prop.test(98, 178)

prop.test(98, 178, alt="greater", conf.level = .90)

#using single categorical value: one sample chi-square test
HairEyeColor
str(HairEyeColor)
HairEyeColor

#get Marginal Frequencies from eye color
margin.table(HairEyeColor, 2)

#save eye color to data frame
eyes <- margin.table(HairEyeColor, 2)
eyes

#show as proportions with 2digits
round(prop.table(eyes), 2) *100

#Use pearsons chi-squared 
#Need one-dimensional goodness of fit test
#default test(assume equal distribution)

chi1 <- chisq.test(eyes)
chi1

#Quantitative data
#see outliers in boxplots
?rivers

data(rivers)
hist(rivers)
boxplot(rivers)

boxplot(rivers, horizontal = TRUE)
boxplot.stats(rivers)

#removing outliers from data
rivers.low <- rivers[rivers < 1210] #remove outliers
rivers.low
boxplot(rivers.low, horizontal = TRUE)
boxplot.stats(rivers.low)
rivers.out <- rivers.low[rivers.low < 1171]
boxplot(rivers.out, horizontal = TRUE)

#Tranforming Variables
?islands
islands
hist(islands, breaks = 15)
boxplot(islands)


islands.z <- scale(islands) #mean = 0,  sd = 1
islands.z #makes matrix with attribute information

hist(islands.z, breaks = 15) #histogram of z-scores
boxplot(islands.z) #boxplot of z-scores
mean(islands.z) #M should be equal 0
round(mean(islands.z), 2) #roundoff to see mean=0

sd(islands.z) #SD = 1
attr(islands.z, "scaled:center") #show original mean
attr(islands.z, "scaled:scale") #show original sd

islands.z <- as.numeric(islands.z) #converts from matrix back to numeric
islands.z

#logarithmic Transformations
islands.ln <- log(islands) #natural log (base=e)
hist(islands.ln)
#islands.log10 <- log10(islands) #common log (base=10)
#hist(islands.log10)
#islands.log2 <- log2(islands) #binary log (base=2)

boxplot(islands.ln)

#note: Add 1 to avoid undefined logs when x = 0
x.ln <- log(x + 1)

#Ranking
islands.rank2 <- rank(islands)
hist(islands.rank2)
#If there ties in data then there several different methods to deal with them
#ties.method = c("average","first","random","max","min")

islands.rank3 <- rank(islands, ties.method = "random")
hist(islands.rank3)

#dichotomizing
#use wisely and purposefull!
#split at 1000 (=1,00,000 square miles)
#ifelse is the conditional element selection

continent <- ifelse(islands>1000, 1, 0)
continent


#computing composite variables
#create variable rn1 with 1million random normal values
#will vary from one time to another
rn1 <- rnorm(1000000)
hist(rn1)
summary(rn1)

#create variable rn2 with 1million random normal values
rn2 <- rnorm(1000000)
hist(rn2)
summary(rn2)

#Average scores across two variables
rn.mean <- (rn1 + rn2)/2
hist(rn.mean)
summary(rn.mean)


#multiply scores across two variables
rn.prod <- rn1 * rn2
hist(rn.prod)
summary(rn.prod)

require(psych)
#kurtosis comparison
kurtosi(rn1)
kurtosi(rn2)
kurtosi(rn.mean)
kurtosi(rn.prod)

#coding missing Data
x1 <- c(1, 2, 3, 4, NA, 6)
summary(x1)
mean(x1) #results in NA (doesn't work)

#to find missing values
which(is.na(x1)) #gives index number

#Ignore missing values with na.rm = T
mean(x1, na.rm = T)

#Replace missing values with 0
#opt 1 : using "is.na"
x2 <- x1
x2[is.na(x2)] <- 0
x2

#opt2: using "ifelse"
x3 <- ifelse(is.na(x1), 0, x1)
x3

#for data frames, R has many packages to deal intelligently with missing data via imputation.

#missing data imputation and model checking:
browseURL("https://cran.r-project.org/web/packages/mice/index.html")

#imputation
browseURL("https://cran.r-project.org/web/packages/imputation/index.html")

read.csv("C:/Users/mohit/Downloads/Ex_Files_RStats_EssT/Ex_Files_RStats_EssT/Exercise Files/Ch04/04_05_Challenge/xskew.csv")


#practice
xskew <- (read.csv("C:/Users/mohit/Downloads/Ex_Files_RStats_EssT/Ex_Files_RStats_EssT/Exercise Files/Ch04/04_05_Challenge/xskew.csv"))
x <- xskew[,2]
x

summary(x)
hist(x)
boxplot(x)
boxplot.stats(x)

#squaring data
x2 <- x^2
hist(x2)
boxplot(x2)

#4th power
x4 <- x^4
hist(x4)
boxplot(x4)

data(mtcars)
mtcars

#SELECTING CASES
#mean quartermile time(for all cars)
mean(mtcars$qsec)

#mean quarter mile time (for 8cylinder cars) use square brackets to indicate for what to select in this format: [rows]
mean(mtcars$qsec[mtcars$cyl == 8])

#median horsepower (for all cars)
median(mtcars$hp)

#mean MPG for cars above median horsepower
mean(mtcars$mpg[mtcars$hp > median(mtcars$hp)])

#create new data frame for 8cylinder cars 
#To create a new data frame, must indicate whch rows n cols to copy
#in this format:[rows, columns]. To select all
#cols, leave second part blank

cyl.8 <- mtcars[mtcars$cyl == 8, ]

#select 8 cylinder cars with 4+ barrel carburetors
mtcars[mtcars$cyl == 8 & mtcars$carb >= 4,]

# '~' - Tilde it means as a function of

#ANALYZING BY SUBGROUP
data("iris")
iris
mean(iris$Petal.Width)

#split the data file and repeat analysis with "aggregate" Func
#compare groups on mean of one variable
aggregate(iris$Petal.Width ~ iris$Species, FUN = mean)

#compare groups on several variables
#use cbind to list outcome variables
aggregate(cbind(iris$Petal.Width, iris$Petal.Length) ~ iris$Species, FUN = mean)

#Splitting and merging files
?longley
data(longley)

#split up data
a1 <- longley[1:14, 1:6] #starting data
a2 <- longley[1:14, 6:7] #New column to add (with "Year" to match)

b <- longley[15:16, ] #new rows to add

write.table(a1, "C:/Users/mohit/OneDrive/Desktop/RLanguage/a1.txt", sep = "\t")
write.table(a2, "C:/Users/mohit/OneDrive/Desktop/RLanguage/a2.txt", sep = "\t")
write.table(b, "C:/Users/mohit/OneDrive/Desktop/RLanguage/b.txt", sep = "\t")

#Import data
a1t <- read.table("C:/Users/mohit/OneDrive/Desktop/RLanguage/a1.txt", sep="\t")
a2t <- read.table("C:/Users/mohit/OneDrive/Desktop/RLanguage/a2.txt", sep="\t")
#b <- read.table("C:/Users/mohit/OneDrive/Desktop/RLanguage/b.txt", sep="\t")

a.1.2 <- merge(a1t, a2t, by = "Year") #Merge two data frames

a.1.2 #check results

#Add two more cases at bottom
b <- read.table("C:/Users/mohit/OneDrive/Desktop/RLanguage/b.txt", sep = "\t")
all.data <- rbind(a.1.2, b) #Row bind
all.data

row.names(all.data) <- NULL #Reset Rownames
all.data #check data


?InsectSprays
sprays <- InsectSprays

#to plt means, first get the means for the groups
means <- aggregate(sprays$count ~ sprays$spray, FUN = mean)
means
plot(means) #gets lines for means

#CREATING BAR CHARTS OF GROUP MEANS
#to get a barplot, need to extract means and reorganize
mean.data <- t(means[-1]) #Removes first columns, transposes second

colnames(mean.data) <-means[,1]

#Basic barplot for means
barplot(mean.data)

#Modify barplot
barplot(mean.data,
        col = "lightblue",
        main = "Effectiveness of Insect sprays",
        xlab = "sprays Used",
        ylab = "Insect count")

#CREATING GROUPED BOX PLOTS
#using mass package for dataset "painters"
require(MASS)
?painters


#For interesting Follow up on this data, see "Taste Endures! The rankings of ROGER de piles
#(???1709) and 3centuries of ART prices" by kathryn GRAddy at
browseURL("http://people.brandeis.edu/~kgraddy/published%20papers/
Depiles_complete.pdf")

data(painters)
painters

#Draw boxplots of outcome (Expression) by group (school)
#Basic Version
boxplot(painters$Expression ~ painters$School)

require(RColorBrewer)

boxplot(painters$Expression ~ painters$School,
        col = brewer.pal(8, "Pastel2"),
        names = c("renais.",
                  "Mannerist",
                  "seicento",
                  "venetian",
                  "Lombard",
                  "16th C.",
                  "17th C.",
                  "French"),
        #notch = TRUE, #Not good because of small samples; don't use
        boxwex = 0.5, #width of box
        whisklty = 1, #Whisker line type; 1= solid line
        staplelty = 0,
        outpch = 16,
        outcol = brewer.pal(8, "Pastel2"),
        main = "Expression Ratings of painters by school\nFrom\"Mass\" Package",
        xlab = "Painter's School",
        ylab = "Expression Ratings")

#CREATING SCATTER PLOT

data("cars")


plot(cars)

#MODIFIED SCATTER PLOT
plot(cars,
     pch = 16,
     col = "gray",
     main = "speed vs. stopping distance",
     xlab = "speed (MPH)",
     ylab = "Stopping Distance (feet)")

#Adding linear regression line
abline(lm(cars$dist ~ cars$speed),
       col = "darkred",
       lwd = 2)

#Locally weighted scatterplot smoothing
lines(lowess(cars$speed, cars$dist),
      col = "blue",
      lwd = 2)


#("COMPANION TO APPLIED REGRESSION")
install.packages("car")
require(car)
help(package = "car")

#"Scatterplot" has marginal boxplots, smoothers, and quantile regression intervals

scatterplot(cars$dist ~ cars$speed,
            pch = 16,
            col = "dark blue",
            main = "speed vs. stopping distance",
            xlab = "speed(MPH",
            ylab = "stopping distance (feet)")


mora <- read.csv("C:/Users/mohit/Downloads/Ex_Files_RStats_EssT/Ex_Files_RStats_EssT/Exercise Files/Ch06/06_04_Challenge/SearchData.csv")

require(RColorBrewer)
boxplot(mora$nfl ~ mora$region,
        col = brewer.pal(8, "Pastel2"),
        #notch = TRUE, #Not good because of small samples; don't use
        boxwex = 0.5, #width of box
        whisklty = 1, #Whisker line type; 1= solid line
        staplelty = 0, #staple Type
        outpch = 16, #outlier symbol
        outcol = brewer.pal(8, "Pastel2"),
        main = "Google Search Interest in NFL by region of US",
        xlab = "Region's of US",
        ylab = "Search Interest")
mora[mora$region == "midwest", ]

#Calculating Correlations

#Load Data
data(swiss)

#correlation matrix for data frame
cor(swiss)
round(cor(swiss), 2) #Rounded to 2 decimals

#can test one pair of variables at a time
#Gives r, hypothesis test and confidence interval
cor.test(swiss$Fertility, swiss$Education)

require(Hmisc)

#need to coerce from data frame to matix
#to get correlation matrix and p-values
rcorr(as.matrix(swiss))

#COMPUTING BIVARIATE REGRESSION
data("trees")
trees[1:5, ] #show first 5lines

#Quick graphical check on data
hist(trees$Girth)
hist(trees$Height)
hist(trees$Volume)
plot(trees$Height ~ trees$Girth)
abline(lm(trees$Height ~ trees$Girth))

#linear regression model
reg1 <- lm(Height ~ Girth, data = trees)
reg1

summary(reg1)

#confidence intervals for coefficients
confint(reg1)

#Predict values based on regression equation
predict(reg1) #predicted height based on girth
predict(reg1, interval = "prediction") #CI for predicted height

#Regression Diagnostics
lm.influence(reg1)
influence.measures(reg1)

#Comparing means with t-test
data(sleep)
sleep[1:5, ]
sd <- sleep [ , 1:2] #save just the first 2variables
sd[1:5, ] #show the first 5cases

#some quick plots to check data
hist(sd$extra, col = "darkred")
boxplot(extra ~ group, data = sd) # ~ means broken down or by group

#Independent 2-group t-test (with defaults)
t.test(extra ~ group, data = sd) #runs welch 2-sample t-test

#t-test with options
t.test(extra ~ group,
       data = sd,
       alternative = "less", #one-tailed test
       conf.level = 0.80) #80% CI (vs. 95%)

#Create 2groups of random data in separate variables
# good because actual difference is known
x <- rnorm(30, mean = 20, sd = 5)
y <- rnorm(30, mean = 22, sd = 5)
t.test(x, y)

#COMPARING PAIRED MEANS: Paired t-test

#load data
#create random data
t1 <- rnorm(50, mean = 52, sd = 6) # Time 1
dif <- rnorm(50, mean = 6, sd = 12) #Difference
t2 <- t1 + dif

#some quick plots to check data
hist(t1)
hist(dif)
hist(t2)
boxplot(t1, t2)

#save variable in dataframe and use "MASS"
# to create parallel cordinate plot

pairs <- data.frame(t1, t2)
require("MASS")
parcoord(pairs, var.label = TRUE)

#Paired t-test (with defaults)
t.test(t2, t1, paired = TRUE)

#Paired t-test with options
t.test(t2, t1,
       paired = TRUE,
       mu = 6, #Specifynon-0 null value
       alternative = "greater", #one-tailed test
       conf.level = 0.99) # 99% confidence interval (vs. 95%)

#Comparing means with one factor analysis of variance ANOVA

#load data 
#Each group in separate variable
x1 <- rnorm(30,mean=40, sd = 8)
x2 <- rnorm(30,mean=41, sd = 8)
x3 <- rnorm(30,mean=45, sd = 8)
x4 <- rnorm(30,mean=45, sd = 8)

#formula result is F(3, 116) = 3.24, p = .025
boxplot(x1, x2, x3, x4) #Quick graphical check
#combine vectors into single data frame
xdf = data.frame(cbind(x1,x2,x3,x4))
summary(xdf)

#stack data to get one column with outcome
# and second columns with group

xs <- stack(xdf)

#conduct one-way anova
anova1 <- aov(values ~ ind, data = xs)
anova1
summary(anova1)

#post-hoc comparisons
TukeyHSD(anova1)
?pairwise.t.test #other post-hoc tests
?p.adjust


my_function <- function(x) {       
  x <- ifelse(x %% 2 < 1, 1, x ^ 2)        
  return(x)
}
x <- my_function(20)
my_function(x)

#Comparing Proportions

#Need 2vectors:
#One specifies the total number of people in each group
#This creates a vector with 5 100s in it, for 5groups
#same as "number of trials"
n5 <- c(rep(100,5))
?rep
#Another specifies the number of people who are in category
#same as "number of successes"

x5 <- c(65, 60, 60, 50, 45)
prop.test(x5, n5)

#If there are only 2groups, then it gives a confidence interval for the difference between groups;
#the default CI is .95

n2 <- c(40, 40) #Number of trials
x2 <- c(30, 20) #Number of Successes
prop.test(x2, n2, conf.level=.80)

#creating crosstabs for categorical values
str(Titanic)
Titanic
ftable(Titanic) #Makes "flat" table

#Convert table to data frame with one row per observation
tdf <- as.data.frame(lapply(as.data.frame.table(Titanic), 
                            function(x)rep(x, as.data.frame.table(Titanic)$Freq)))[,-5]
tdf[1:5,] #check first 5rows of data

#create contingency table
ttab <- table(tdf$Class, tdf$Survived)
ttab

#call also get cell, row, and column %
#with rounding to get just 2decimal places
#multiplied by 100 to make %
round(prop.table(ttab, 1), 2)* 100 #row %
round(prop.table(ttab, 2), 2)* 100 #column %
round (prop.table(ttab), 2) * 100 #cell %

#chi-squared test

tchi <- chisq.test(ttab)
tchi
?chisq.test
#Additional tables
tchi$observed #observed frequencies (same as tab)
tchi$expected #expected frequencies
tchi$residuals #pearson's residual
tchi$stdres #standardized residual

#Computing ROBUST statistics for bivariate associations

#ROBUST regression: A Sampling of packages

help(package = "robust")
help(package = "robustbase")
help(package = "MASS") #see rlm (Robust Linear Model)
help(package = "quantreg") #Quantile Regression

#example from quantreg package
install.packages("quantreg")
require(quantreg)

?rq #(quantile regression in quantreg package)

data(engel)
?engel
attach(engel)

plot(income, 
     foodexp,
     xlab = "Household Income",
     ylab = "Food Expenditure",
     type = "n",
     cex = .5)
points(income, foodexp, #POints in plot
       pch = 16, col = "lightgray")
taus <- c(0.5, .1, .25, .75, .9, .95) #Quantiles
xx <- seq(min(income), max(income), 100) #X values
f <- coef(rq((foodexp)~(income), tau = taus)) #coefficients
yy = cbind(1, xx)%*%f#Y values
for(i in 1:length(taus)){# for each quantile value...
  lines(xx, yy[, i], col = "darkgray") #Draw regression
}
abline(lm(foodexp~income), #standard LS regression
       col = "darkred",
       lwd = 2)
abline(rq(foodexp~income), #Median Regression
       col ="blue",
       lwd = 3)
legend(3000, 1500, #plot legend
       c("mean fit", "median fit"),
       col = c("darkred", "blue"),
       lty = 1,
       lwd =2)

#CHALLENGE
ml <- read.csv("C:/Users/mohit/Downloads/Ex_Files_RStats_EssT/Ex_Files_RStats_EssT/Exercise Files/Ch07/07_09_Challenge/mlb2011.csv")

#Homewins as proportion of all wins


#compare all 30teams at once
prop.test(ml$HomeWins, ml$AllWins)

#compare just highest and lowest

#milwaukee brewers : 57/96 = 59%
#florida Marlins: 31/72 = 43%

x2 <- c(31, 57) #number of successes
n2 <- c(72, 96) #number of trials
prop.test(x2, n2)
