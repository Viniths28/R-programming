library(ggplot2)
data("diamonds")
diam <- data.frame(diamonds)
dim(diam)
head(diam)
levels(diam$cut)
cut_levels <- c("Fair", "Good", "Very Good", "Premium", "Ideal")
diam$cut <- factor(diam$cut, levels = cut_levels)
levels(diam$cut)

# lab2.3..histogram----


qplot(x = carat, data = diam, geom = "histogram")

qplot(x = carat, data = diam, geom = "histogram", binwidth = 0.1)

qplot(
  x = carat, data = diam, geom = "histogram",
  binwidth = 0.25
) + facet_wrap(~cut, ncol = 3) + theme_light()

x <- sort(diam$carat)
length(x) * 1 / 2
x[26970]
median(diam$carat)
median(x)

x[c(0.25, 0.75) * length(x)]
quantile(diam$carat, c(0.25, 0.50, 0.75))

qplot(x = carat, y = price, data = diam, geom = "point")

qplot(
  x = carat, y = price, data = diam,
  geom = "point"
) + facet_wrap(~cut, ncol = 5)

qplot(
  x = carat, y = price, data = diam,
  geom = "point"
) + facet_wrap(~cut, ncol = 5)

qplot(
  x = carat, y = price, data = diam,
  geom = "point"
) + facet_wrap(~clarity, ncol = 3)

qplot(x = carat, y = price, data = diam, geom = "point", colour = clarity)

qplot(
  x = carat, y = price, data = diam, geom = "point",
  colour = clarity
) + facet_wrap(~cut, ncol = 3) + theme_dark()

pclarity <- qplot(x = clarity, data = diam, geom = "bar", fill = cut)
qplot(x = clarity, data = diam, geom = "bar", fill = cut)
# ggsave(pclarity, file="p_carat.png", width=6, height=4)

ggplot(diam) +
  geom_boxplot(aes(x = cut, y = carat),
    colour = "red", fill = "NA",
    outlier.colour = "blue"
  ) +
  geom_jitter(aes(x = cut, y = carat), alpha = 0.01) +
  theme_grey()

# Titanic----
install.packages("titanic")
library(titanic)
#data("Titanic")

train <- read.csv(
  file = "C://Users//vs0432//Documents//Rfolder//Lab2//train.csv")

test <- read.csv(
  file = "C:\\Users\\vs0432\\Documents\\Rfolder\\Lab2\\test.csv")

#test
kkk <- levels(test$Sex)
test$Sex <- factor(test$Sex, 
                     levels =kkk,labels=c("f","m"))
#train

levels(train$Sex)[1] <-"f"
levels(train$Sex)[2] <- "m"


#Charts-titanic----
#taking train data all over the excercise
#
qplot(x = Survived, data = train, geom = "bar")
library(ggplot2)
train$Survived <- as.character(train$Survived)

ggplot(train)+geom_bar(aes(x=train$Survived))

#Last ex:Q5
library(ggplot2)
train$Pclass <- as.character(train$Pclass)

ggplot(train)+
  geom_bar(aes(x=Survived,fill=Pclass))+
  theme(legend.position = "top")

train$Parch <- as.character(train$Parch)
ggplot(train)+
  geom_bar(aes(x=Survived,fill=Parch))+
  theme(legend.position = "top")


train$Age <- round(train$Age)

train$Survived <- as.character(train$Survived)
ggplot(train)+geom_bar(aes(x=Survived, y=..prop..))

#ggplot(train) +
  #geom_bar(aes(x=as.factor(Survived),y = (..count..)/sum(..count..)))
train$Survived <- as.character(train$Survived)

ggplot(train)+
  geom_bar(aes(x=Embarked,y=..prop..,group=1,fill=factor(..x..)),
           stat="count")+ 
  scale_y_continuous(labels = scales::percent_format())+
  labs(y = "Percent", fill="Embarked") +
  geom_text(aes( label = scales::percent(..prop..)))


plt <- ggplot(data = train) +
  geom_bar(mapping = aes(x = Pclass))
plt_b <- ggplot_build(plt)
plt_b$data[[1]]

plt <- ggplot(data = train) +
  geom_bar(mapping = aes(x = Survived, y = ..prop.., group = 1))
plt_b <- ggplot_build(plt)
plt_b$data[[1]]


ggplot(train)+ 
  geom_bar(aes(x= Embarked,y = ..prop..,group=Survived,
               fill = factor(..x..)), stat="count")+facet_grid(~Survived)+
  scale_y_continuous(labels=scales::percent)+
  labs(y = "Percent", fill="embarked")

library(ggplot2)
ggplot(train, aes(x = Embarked, y = ..prop.., group = 1)) +
  geom_bar(aes(
   fill = factor(..x.., labels = c("NA", "C", "Q", "S"))), 
    stat = "count") + 
  facet_grid(~Survived) + 
  geom_text(aes(
    label = scales::percent(..prop..)),
    stat = "count",
    vjust = -.5) + 
  labs(y = "Percent", fill = "Embarked") + 
  theme_bw() + theme(legend.position = "top") -> plot1
  

plot1 + scale_y_continuous(labels = scales::percent_format(accuracy = 1))

train$Survived <- as.character(train$Survived)

library(ggplot)
ggplot(train, aes(x = as.character(Survived), y = ..prop.., group = 1)) +
  geom_bar(aes(
    fill = factor(..x.., labels = c("Yes","No"))), 
    stat = "count") + 
  facet_grid(~Pclass) + 
  geom_text(aes(
    label = scales::percent(..prop..)),
    stat = "count",
    vjust = -.5) + 
  labs(y = "Percent", fill = "Survived") + 
  theme_bw() + theme(legend.position = "top") -> plot2


plot2 + scale_y_continuous(labels = scales::percent_format(accuracy = 1))      


library(ggplot)
ggplot(train)+
  geom_bar(aes(x=Age,col="red"),fill="orange",binwidth =10)+
  theme_light()

train <- read.csv
(file = "C:\\Users\\vs0432\\Documents\\Rfolder\\Lab2\\train.csv")



 




  


















