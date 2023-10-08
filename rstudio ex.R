install.packages("readxl")
library(readxl)
data <- read_excel("C:/Users/JOTHSNA/Desktop/dataset.xlsx",)
plot(x=data$x,y = data$y,
     xlab="x-axis",
     ylab="y-axis",
     main="plot"
)
A <- c(17,32,8,53,1)
barplot(A,xlab="X-axis",ylab="Y-axis",main="Bar-Chart")
A <- c(17,32,8,53,1)
barplot(A,horiz=TRUE ,xlab="X-axis",ylab="Y-axis",main="Bar-Chart")

# creating the data for the chart
v <-c(17,25,38,13,41)
t <-c(22,19,36,19,23)
m <-c(25,14,16,34,29)

#plotting the bar chart
plot
barplot(v,type="p",col="red",
     xlab="Month",ylab="Article Written",
     main="Article Written chart")

lines(t,type="l",col="blue")
lines(m,type="o",col="green")

#create data for the graph,
v <-c(19,23,11,5,16,21,32,14,19,27,50,30)
#creating the histogram

m<-hist(v,xlab="Weight",ylab="Frequency",col="brown",border="white",xlim=c(0,50),ylim=c(0,5),breaks=5)

#setting Labels

text(m$mids,m$counts,labels=m$counts,adj=c(0.5,-0.5))
library(ggplot2)

# set theme
theme_set(theme_bw(20))

xAxis <- read_excel("C:/Users/JOTHSNA/Desktop/dataset2.xlsx")

x_ax = xAxis$AGE

hist(x_ax,col='red',xlim=c(13,81))

group <- rep(13, 81)            
group[x_ax < 15] <- 1
group[x_ax > 30] <- 6
group[x_ax > 50] <- 4
group[x_ax > 70] <- 2
sample_data <- data.frame(x_ax, group)
ggplot(sample_data, aes(x=x_ax, fill = as.factor(group)))+
  geom_histogram( color='purple', alpha=1.5, position='identity')+
  geom_freqpoly(color="purple",alpha=1.5)+
  geom_density(color="black",alpha=1.5)+geom_boxplot(color="yellow",alpha=1.5)

  ggplot(sample_data, aes(x=x_ax,y=group, fill = as.factor(group)))+
  geom_violin(color="green",alpha=1.5)+geom_violin(trim=FALSE)+
    geom_violin(scale="count")+geom_violin(adjust=10)+geom_violin(adjust=.5)
  smokers <- c(69.3, 56.0, 22.1, 47.6, 53.2, 48.1, 52.7, 34.4, 60.2, 43.8, 23.2, 13.8)
  df <- data.frame(Value = c(smokers),
                   Cat = c(rep("smokers",length(smokers))),
                   xseq = c(seq_along(smokers)))
  library(ggplot2)
  ggplot(df, aes(x = Cat, y = Value, color = Cat)) + geom_point(color="blue")+xlab("")
  
  ggplot(df, aes(x = Cat, y = Value)) + geom_point(color="blue")+xlab("")
  
  #12
  smokers <- c(69.3, 56.0, 22.1, 47.6, 53.2, 48.1, 52.7, 34.4, 60.2, 43.8, 23.2, 13.8)
  #15
  nonsmokers <- c(28.6, 25.1, 26.4, 34.9, 29.8, 28.4, 38.5, 30.2, 30.6, 31.8, 41.6, 21.1, 36.0, 37.9, 13.9)
  df <- data.frame(Value = c(smokers,nonsmokers),
                   Cat = c(rep("smokers",length(smokers)), rep("nonsmokers",length(nonsmokers))),
                   xseq = c(seq_along(smokers),seq_along(nonsmokers)))
  library(ggplot2)
  ggplot(df, aes(x = Cat, y = Value, color = Cat)) + geom_point()+xlab("")
  
  
  ggplot(df, aes(x = "points", y = Value, color = Cat)) + geom_point()+xlab("")
  
  library(ggplot2)
  library(readxl)
  theme_set(theme_bw(12))

  
  xAxis <- read_excel("C:/Users/JOTHSNA/Desktop/dataset2.xlsx")
  
  x_ax = xAxis$AGE
  
  hist(x_ax,col='red',xlim=c(13,81))
  
  group <- rep(13, 81)            
  group[x_ax < 15] <- 1
  group[x_ax > 30] <- 6
  group[x_ax > 50] <- 4
  group[x_ax > 70] <- 2
  
  
  sample_data <- data.frame(x_ax, group)
  ggplot(sample_data, aes(x=x_ax,y=group))+
    geom_dotplot(binaxis='y', stackdir='center')
  
  ggplot(sample_data, aes(x=x_ax,y=group))+
    geom_dotplot(binaxis='y')
  
  ggplot(sample_data, aes(x=x_ax,y=group))+
    geom_dotplot(binaxis='y', stackdir='center',
                 stackratio=1.5, dotsize=1.2)
  faithful_p <- ggplot(faithful, aes(x = eruptions, y = waiting))
  
  faithful_p +
    geom_point() +
    stat_density2d()
  
  
  
  
  
  
  
  data = read.table(text="P1 -1 0 4\nP2 0 0 2\nP3 2 1 8\nP4 -2 -2 6\nP5 0.5 2 12")
  data2 = read.table(text="Q1 1 1 3\nQ2 1 -1 2\nQ3 -1 1 8")
  colnames(data) = c("name","x","y","score")
  colnames(data2) = c("name","x","y","score")
  
  ggplot(data, aes(x=x,y=y)) +
    stat_density2d(data=data,geom="tile", aes(fill = ..density..,alpha=..density..), contour=FALSE) +
    theme(legend.position="none") + scale_fill_gradient (low = "#FF4FFF", high = "blue") +
    xlim(-3,3) + ylim(-3,3) +
    geom_point()
  
  ggplot(data2, aes(x=x,y=y)) +
    stat_density2d(data=data2,geom="tile", aes(fill = ..density..,alpha=..density..), contour=FALSE) +
    theme(legend.position="none") +
    scale_fill_gradient (low = "blue", high = "#00FF00") +
    xlim(-3,3) + ylim(-3,3) +
    geom_point()
  
  
  ggplot(rbind(data.frame(data, group="a"), data.frame(data2, group="b")), aes(x=x,y=y)) +
    stat_density2d(geom="tile", aes(fill = group, alpha=..density..), contour=FALSE) +
    scale_fill_manual(values=c("a"="#FF0000", "b"="#00FF00")) +
    geom_point() +
    theme_minimal() +
    xlim(-3.3, 3.3) + ylim(-3.3, 3.3) +
    coord_cartesian(xlim = c(-3.2, 3.2), ylim = c(-3.2, 3.2))
  
  
  
  df <- structure(list(Lat = c(-24.1871741, -24.2069615, -24.2022726,
                               -24.2016188, -24.2152107, -24.1939073, -24.1913561, -24.198409,
                               -24.2088875, -24.2121186),
                       Long = c(30.8839167, 30.8814249, 30.8788437,
                                30.8903969, 30.8883906, 30.8784664, 30.870561, 30.8800543,
                                30.8818679, 30.8914805)), row.names = c(NA, 10L),
                  class = "data.frame")
  
  ggplot(df, aes(Long, Lat)) +
    stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE) +
    geom_point(colour = "white",size=9)
  
  
  
  
  
  
  
  #transform our data using binning, smoothing, descriptive, intermediate
  ggplot(df, aes(Long, Lat)) +
    geom_point() +
    stat_smooth(method = lm, col = "purple") +
    labs(title = "")
  
  
  
  
  



