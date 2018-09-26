### Data Wrangling Assignment 1
## Group: Alexander Uitendaal , Milad Ehsan(10732918) , Daan Uittenhout() and Isabel Slot()

 

rm(list=ls())         # remove working memory
library("mlbench")    #install the right package
data("BreastCancer")  #install th? right dataset: BreastCancer
View(BreastCancer)

## Missing values 
missing <- is.na(BreastCancer) #Check whether there are missing values
View(missing) 

#There are missing values, so delete these:
BreastCancer2<-na.omit(BreastCancer)
view(BreastCancer2)
?# 16 out of 700 (+/- 2%)observations where lost

# install mice pack
# add mice pack to lib
# use imputing to fill the missing data
# create class BreastcancerImputted
BreastcancerImputted <- mice(BreastCancer)
View(BreastcancerImputted)
# sumarize data an? class
md.pattern(BreastcancerImputted)
summary(BreastcancerImputted)
BreastCancer2 <- BreastcancerImputted $data



## Visualise the variables and see their relation with the Class type of the Cancer
## Also look for outliers, 
library(ggplot2)
# Variable?1 : cell thickness

ggplot() +
  geom_bar(data = BreastCancer2,
           aes(x = Cl.thickness,
               fill = BreastCancer2 $Class),
           position = "Stack") + scale_x_discrete("Clump Thickness")+scale_y_continuous("Frequency")+
  guides(fil?=guide_legend(title="Class type"))+scale_fill_manual(values=c("red", "blue"))
# No outliers spotted 
# Variable 2 : cell size

ggplot() +
  geom_bar(data = BreastCancer2,
           aes(x = Cell.size,
               fill = BreastCancer2$Class),
           ?osition = "Stack") + scale_x_discrete("Clump Thickness")+scale_y_continuous("Frequency")+
  guides(fill=guide_legend(title="Class type"))+scale_fill_manual(values=c("red", "blue"))
# No outliers spotted 

# Variable 3 : cell shape

ggplot() +
  geom_bar(da?a = BreastCancer2,
           aes(x = Cell.shape,
               fill = BreastCancer2$Class),
           position = "Stack") + scale_x_discrete("Clump Thickness")+scale_y_continuous("Frequency")+
  guides(fill=guide_legend(title="Class type"))+scale_fill_m?nual(values=c("red", "blue"))
# No outliers spotted 

# Variable 4 : cell marginal adhesion

ggplot() +
  geom_bar(data = BreastCancer2,
           aes(x = Marg.adhesion,
               fill = BreastCancer2$Class),
           position = "Stack") + scale_x_?iscrete("Clump Thickness")+scale_y_continuous("Frequency")+
  guides(fill=guide_legend(title="Class type"))+scale_fill_manual(values=c("red", "blue"))
# No outliers spotted 

# Variable 5 : Single Epithelial Cell Sizehesion

ggplot() +
  geom_bar(data = Br?astCancer2,
           aes(x = Epith.c.size,
               fill = BreastCancer2$Class),
           position = "Stack") + scale_x_discrete("Clump Thickness")+scale_y_continuous("Frequency")+
  guides(fill=guide_legend(title="Class type"))+scale_fill_manual?values=c("red", "blue"))
# No outliers spotted 

# Variable 6 Bare Nuclei
ggplot() +
  geom_bar(data = BreastCancer2,
           aes(x = Bare.nuclei,
               fill = BreastCancer2$Class),
           position = "Stack") + scale_x_discrete("Clump Thick?ess")+scale_y_continuous("Frequency")+
  guides(fill=guide_legend(title="Class type"))+scale_fill_manual(values=c("red", "blue"))
# No outliers spotted 

# Variable 7 bland Chromatin
ggplot() +
  geom_bar(data = BreastCancer2,
           aes(x = Bl.cromati?,
               fill = BreastCancer2$Class),
           position = "Stack") + scale_x_discrete("Clump Thickness")+scale_y_continuous("Frequency")+
  guides(fill=guide_legend(title="Class type"))+scale_fill_manual(values=c("red", "blue"))
# No outliers spo?ted 

# Variable 8 Normal Nucleoli
ggplot() +
  geom_bar(data = BreastCancer2,
           aes(x = Normal.nucleoli,
               fill = BreastCancer2$Class),
           position = "Stack") + scale_x_discrete("Clump Thickness")+scale_y_continuous("Frequenc?")+
  guides(fill=guide_legend(title="Class type"))+scale_fill_manual(values=c("red", "blue"))
# No outliers spotted 

# Variable 9 Mitoses

ggplot() +
  geom_bar(data = BreastCancer2,
           aes(x = Mitoses,
               fill = BreastCancer2$Class),?           position = "Stack") + scale_x_discrete("Clump Thickness")+scale_y_continuous("Frequency")+
  guides(fill=guide_legend(title="Class type"))+scale_fill_manual(values=c("red", "blue"))
# No outliers spotted 

## All the variables are on an ordinal ?cale from 1 to 10. Therefore there are not clear 
## outliers in the data to be spotted 
## For all the variable seems  that the higher the value, the more likely the obervervations 
## class type is 'malignant' 


# make a correlation matrix colored, so w? can have a quick overview of strong and weak correlating variables
# first make the variables numeric so we can plot the correlation matrix
#install.packages("corrplot")
#install.packages("taRifx")

#library( taRifx )
#library(corrplot)

breastcancer3 <- ?reastCancer2[2:10]
breastcancer3 <- sapply( breastcancer3, as.numeric )
correlation_matrix <- cor(breastcancer3)
correlation_plot <- corrplot(correlation_matrix, method = 'color', col = (200), type = 'upper', addCoef.col = "black")
# strong correlation bet?een cell shape and cell size 
# all variables have a positive correlation




# TODO(Daan): subset vinden van de data waarbij de kans op 'malignant' extra groot is 
with(BreastCancer2, CrossTable(colnames(BreastCancer2), colnames(BreastCancer2)))
     
# T?DO(Daan): style guide kiezen en toepassen
# TODO(Daan): 

#contingency table Clump thickness
install.packages("descr")
library("descr")
tabel1 <-CrossTable(BreastCancer2$Class, BreastCancer2$Cl.thickness,prop.c = FALSE,prop.chisq = FALSE, prop.t = FALSE)
p?int(tabel1)
#shows that the thinkess of the Clump is not bigger than 8 for a benign tumor. 
#Moreover, if the thickness of the clump is smaller than 6, the tumor is mostly benign, vicaversa for thickness >= 6 


#contingency table  Cell Size
tabel2 <-Cross?able(BreastCancer2$Class, BreastCancer2$Cell.size,prop.c = FALSE,prop.chisq = FALSE, prop.t = FALSE)
print(tabel2)
#shows that, except for a few unique cases, the cell size usually does not get bigger than 4 for a benign tumor. 

#contingency table Cell Sh?pe
tabel3 <-CrossTable(BreastCancer2$Class, BreastCancer2$Cell.shape,prop.c = FALSE,prop.chisq = FALSE, prop.t = FALSE)
print(tabel3)
#cell shape is decreasing for bening tumors. Malignant cases show a less clear pattern.

#contingency table Marginal Adhes?on
tabel4 <-CrossTable(BreastCancer2$Class, BreastCancer2$Marg.adhesion,prop.c = FALSE,prop.chisq = FALSE, prop.t = FALSE)
print(tabel4)
# except for one benign tumor, the marginal adhesion has a decreasing pattern for benign tumors and doesnt get bigger t?an 6.
# the malignant cases show a less abrupt decrease and do not really show a consistent pattern. 

#contingency table  Epith Size
tabel5 <-CrossTable(BreastCancer2$Class, BreastCancer2$Epith.c.size,prop.c = FALSE,prop.chisq = FALSE, prop.t = FALSE)
pri?t(tabel5)
#  benign cases are decreasing, whereas the malignant cases do not really show a consistent pattern.

#contingency table Bare Nuclei
tabel6 <-CrossTable(BreastCancer2$Class, BreastCancer2$Bare.nuclei,prop.c = FALSE,prop.chisq = FALSE, prop.t = FA?SE)
print(tabel6)
# benign cases are decreasing, malignant cases are quite consistent, except for the great amountin class 10. 
#One can say that there is a high  probability of a malignant tumor when the Bare nuclei class is 10 and visa versa for class 1.?
#contingency table Bl Cromatin
tabel7 <-CrossTable(BreastCancer2$Class, BreastCancer2$Bl.cromatin,prop.c = FALSE,prop.chisq = FALSE, prop.t = FALSE)
print(tabel7)
#decreasing pattern for benign cases, with no Bland Chromatin bigger than 7 for a benign cas?. 


#contingency table Normal Nucleoli
tabel8 <-CrossTable(BreastCancer2$Class, BreastCancer2$Normal.nucleoli,prop.c = FALSE,prop.chisq = FALSE, prop.t = FALSE)
print(tabel8)
#benign class shows a decreasing pattern, with no Normal nucleoli bigger than 8
?

#contingency table Mitoses
tabel9 <-CrossTable(BreastCancer2$Class, BreastCancer2$Mitoses,prop.c = FALSE,prop.chisq = FALSE, prop.t = FALSE)
print(tabel9)
#decreasing pattern for benign cases. Allmost all benign cases have a Mitoses class of 1. 
#The mal?gnant cases are also decreasing, but mitoses bigger than 1 are less rare. 






# OUD werk van Milad

# Comparing variables
table1 <- table(BreastCancer2$Cell.size, BreastCancer2$Cell.shape)
View(table1)
#In table we observe that a combination of Cell Sha?e 1 and Cell Size 1 is most frequent in this
#dataset.

#install.packages("gmodels")
library(gmodels)
table2 <- CrossTable(BreastCancer2$Cell.size, BreastCancer2$Cell.shape)
View(table2)
#????

str(BreastCancer2$Cl.thickness)
#Since all the values are fact?rvalues/levels, there won't be any outliers.
#install.packages("ggplot2")
library(ggplot2)
#Histogram of all Bland Chromatin:

plot(BreastCancer2$Bl.cromatin, main='Histogram 1: Bland Chromatin', ylab='Frequency', xlab='Level') 

#Overlayed bar chart of th? clump thickness and the classtype. We observe in this bar chart that when the clump thickness
# is in a higher category, the cancer will more frequently be malignant. Even so, that in all cases with clump thickness
# of category 10 the cancer is malignant?
ggplot() +
  geom_bar(data = BreastCancer2,
           aes(x = Cl.thickness,
               fill = Class),
           position = "stack") + scale_x_discrete("Clump Thickness")+scale_y_continuous("Frequency")+
  guides(fill=guide_legend(title="Class type")?+scale_fill_manual(values=c("red", "blue"))

#In this plot, we observe that the categories of cell size do correlate with the categories of cell shape, e.g. most of 
#observations with category 1 in Cell Size are also category 1 in Cell Shape:
ggplot() +
 ?geom_bar(data = BreastCancer2,
           aes(x = Cell.size,
               fill = Cell.shape),
           position = "stack") + scale_x_discrete("Cell Size")+ scale_y_continuous("Frequency") 
+ guides(fill=guide_legend(title="Cell shape"))

#In this scatt?r we observe that certain combinations of Cell Size and Cell Shape indicate whether a cancer is 
# benign or malagnant. For example: a Cell Shape 1 in combination with a Cell Size 1 is likely to indicate a begign cancer.
ggplot(data=BreastCancer2) + geom_p?int(mapping = aes(x=Cell.size, y=Cell.shape), col = ifelse(BreastCancer2$Class=="benign",
                                                                                               "red","blue"))
legend("topright", c("benign", "malignant"), col = c("re?", "blue"), pch = 1, title = "milad")




