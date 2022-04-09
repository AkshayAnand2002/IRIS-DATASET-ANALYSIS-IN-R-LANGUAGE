##iris dataset
install.packages("Hmisc")
install.packages("Corrplot")
install.packages("PerformanceAnalytics")
install.packages("ggpubr")
install.packages("ggplot2")
install(tseries)
data = read.csv('C:/iris.csv')
str(data) ##describes data
subset(data, target == "Iris-setosa")[1:5,]
### above subset function gives first five values of setosa target
### we can get 7,8 etc values also by changing index.
subset(data, target == "Iris-versicolor")[1:8,]
### we get first 8 values 
## indexing in R language starts from 1 .
## from subset function we can get starting value of specific variety.
## == is relational operator used for comparison .
boxplot(Sepal.Length~Species,
        data=iris,
        main='Sepal Length by Species',
        xlab='Species',
        ylab='Sepal Length',
        col='steelblue',
        border='black')
##boxplot to find how well distributed is data in dataset.
hist(data$petal.length..cm.)
##$- used to access variable in dataset.
head(iris) ### for first six rows of iris dataset.
summary(iris)
##summarizes iris datset
#Min: The minimum value.
#1st Qu: The value of the first quartile (25th percentile).
#Median: The median value.
#Mean: The mean value.
#3rd Qu: The value of the third quartile (75th percentile).
#Max: The maximum value.
#For the only categorical variable in the dataset (Species) 
#we see a frequency count of each value:
#setosa: This species occurs 50 times.
#versicolor: This species occurs 50 times.
#virginica: This species occurs 50 times.
#We can use the dim() function to get the dimensions of the dataset 
#in terms of number of rows and number of columns.
dim(iris)
#the names() function to display the column names of the data frame
names(iris)
## we used iris directly in above few r statements as iris is built-in
#dataset.
#create histogram of values for sepal length
hist(iris$Sepal.Length,
     col='steelblue',
     main='Histogram', ##heading- Histogram
     xlab='Length',
     ylab='Frequency')
## boxplot for sepal length
boxplot(iris$Sepal.Length)
#create scatterplot of sepal width vs. sepal length
plot(iris$Sepal.Width, iris$Sepal.Length,
     col='steelblue',
     main='Scatterplot',
     xlab='Sepal Width',
     ylab='Sepal Length',
     pch=19)
##pch value varies from 0 to 25 with different values for different
##shapes.
###boxplot of species vs sepal.length
boxplot(Sepal.Length~Species,
        data=iris,
        main='Sepal Length by Species',
        xlab='Species',
        ylab='Sepal Length',
        col='steelblue',
        border='black')
##This type of plot allows us to quickly see that the sepal length tends 
##to be largest for the virginica species and smallest for the setosa species.
PL <- iris$Petal.Length
PW <- iris$Petal.Width
plot(PL, PW)
## the previous 3 lines together used for plotting functions.
plot(PL, PW, pch = 2) # pch = 2 means the symbol is triangle
? plot ##documentation of plot.
plot(PL, PW, pch = 2, col = "green")
# change the symbol color to green
iris$Species
#the data type of the Species column is character. 
#We need to convert this column into a factor.
iris$Species <- as.factor(iris$Species)
str(iris$Species)
#Once convertetd into a factor, each observation is represented by one of the 
#three levels of the three species setosa, versicolor, and virginica.
#factors are used to store categorical variables as levels. To completely 
#convert this factor to numbers for plotting, we use the as.numeric function. 
#Since we do not want to change the data frame, we will define a new variable 
#called speciesID.
speciesID <- as.numeric(iris$Species)
speciesID
#We can assign different markers to different species by letting 
#pch = speciesID.
plot(PL, PW, pch = speciesID, col = "green")
#The first 50 data points (setosa) are represented by open circles (pch = 1). 
#The next 50 (versicolor) are represented by triangles (pch = 2), 
#while the last 50 (virginica) are in crosses (pch = 3).
# assign 3 colors red, green, and blue to 3 species *setosa*, *versicolor*,
# and *virginica* respectively
plot(PL, PW, pch = speciesID, col = speciesID)
plot(PL, PW, # x and y
     pch = speciesID, # symbol type
     col = speciesID, # color
     xlab = "Petal length (cm)", # x label
     ylab = "Petal width (cm)", # y label
     main = "Petal width vs. length"  # title
) 
#We notice a strong linear correlation between petal length and width. Let's 
#add a trend line using abline(), a low level graphics function.
abline(lm(PW ~ PL)) # the order is reversed as we need y ~ x.
#The lm(PW ~ PL) generates a linear model (lm) of petal width as a function 
#petal length. y ~ x is formula notation that used in many different situations.
PCC <- cor(PW, PL) # Pearson's correlation coefficient
PCC <- round(PCC, 2) # round to the 2nd place after decimal point.
paste("R =", PCC)
ma <- as.matrix(iris[, 1:4]) # convert to matrix
colMeans(ma) # columnwise means for matrix
colSums(ma)## columnwise sums
#The same thing can be done with rows via rowMeans(x) and rowSums(x).
#We can generate a matrix of scatter plot by pairs() function.
pairs(ma)
pairs(ma, col = rainbow(3)[speciesID]) # set colors by species
#Star plot uses stars to visualize multidimensional data. Radar chart is a 
#useful way to display multivariate observations with an arbitrary number of
#variables. Each observation is represented as a star-shaped figure with one 
#ray for each variable.
df <- iris[, 1:4]
stars(df) 
stars(df, key.loc = c(17, 0)) 
#The stars() function can also be used to generate segment diagrams, where each 
#variable is used to generate colorful segments. The sizes of the segments 
#are proportional to the measurements.
stars(df, key.loc = c(20, 0.5), draw.segments = TRUE)
install.packages("ggplot2")
library(ggplot2) # load the ggplot2 package
ggplot(data = iris)#define a canvas
# map data to x and y coordinates
ggplot(data = iris) +
  aes(x = Petal.Length, y = Petal.Width)
# add data points
ggplot(data = iris) +
  aes(x = Petal.Length, y = Petal.Width) +
  geom_point()
# change color & symbol type
ggplot(data = iris) +
  aes(x = Petal.Length, y = Petal.Width) +
  geom_point(aes(color = Species, shape = Species))
# add trend line
ggplot(data = iris) +
  aes(x = Petal.Length, y = Petal.Width) +
  geom_point(aes(color = Species, shape = Species)) +
  geom_smooth(method = lm)##lm means linear model.
# add annotation text to a specified location by setting coordinates x = , y =
ggplot(data = iris) +
  aes(x = Petal.Length, y = Petal.Width) +
  geom_point(aes(color = Species, shape = Species)) +
  geom_smooth(method = lm) +
  annotate("text", x = 5, y = 0.5, label = "R=0.96")
ggplot(iris) +
  aes(x = Petal.Length, y = Petal.Width) + # define space
  geom_point(aes(color = Species, shape = Species)) + # add points
  geom_smooth(method = lm) + # add trend line
  annotate("text", x = 5, y = 0.5, label = "R=0.96") + # annotate with text
  xlab("Petal length (cm)") + # x-axis labels
  ylab("Petal width (cm)") + # y-axis labels
  ggtitle("Correlation between petal length and width") # title
#Box plots can be generated by ggplot.
library(ggplot2)
ggplot(data = iris) +
  aes(x = Species, y = Sepal.Length, color = Species) +
  geom_boxplot()
##Jitter plot with boxplot
ggplot(data = iris) +
  aes(x = Species, y = Sepal.Length, color = Species) +
  geom_boxplot() +
  geom_jitter(position = position_jitter(0.2))
#Density plot
ggplot(data = iris) +
  aes(x = Petal.Length, fill = Species) +
  geom_density(alpha = 0.3)
## from below code made 3 rows of diffeerent species by facet_wrap
#Density plot by subgroup using facets
ggplot(data = iris) +
  aes(x = Petal.Length, fill = Species) +
  geom_density(alpha = 0.3) +
  facet_wrap(~Species, nrow = 3)
#Dynamite Plot
ggplot(iris) +
  aes(x = Species, y = Sepal.Width, fill = Species) +
  stat_summary(geom = "bar", fun = "mean") +
  stat_summary(geom = "errorbar", fun.data = "mean_se", width = .3)
#Hierarchical clustering summarizes observations into trees representing the overall similarities.
ma <- as.matrix(iris[, 1:4]) # convert to matrix
disMatarix <- dist(ma)
plot(hclust(disMatarix))
#We first calculate a distance matrix using the dist() function with the default 
#Euclidean distance method. The distance matrix is then used by the hclust1()
#function to generate a hierarchical clustering tree with the default complete
#linkage method, which is then plotted in a nested command.
#The rows and columns are reorganized based on hierarchical clustering, and the 
#values in the matrix are coded by colors. Heat maps can directly visualize 
#millions of numbers in one plot. 
heatmap(ma,
        scale = "column",
        RowSideColors = rainbow(3)[iris$Species]
)
install.packages("pheatmap")
library(pheatmap)
ma <- as.matrix(iris[, 1:4]) # convert to matrix
row.names(ma) <- row.names(iris) # assign row names in the matrix
pheatmap(ma,
         scale = "column",
         clustering_method = "average", # average linkage
         annotation_row = iris[, 5, drop = FALSE], # the 5th column as color bar
         show_rownames = FALSE
)
#Heatmap for iris flower dataset.
