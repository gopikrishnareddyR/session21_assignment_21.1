#session21_assignment_21.1

#1Use the below given data set
getwd()
path<-"C:/Users/DELL/Documents"

setwd(path)
dataset<-read.csv("C:/Users/DELL/Documents/epi_r.csv", sep = ",", header = TRUE)

View(dataset)
library(psych)
View(describe(dataset))


#2.Perform the below given activities:


library(factoextra)

head(dataset)

summary(dataset)

str(dataset)
sum(is.na(dataset))
sum(is.na(dataset[,2:6]))
epi_r<-dataset[,1:6]
summary(epi_r)


# thisis not working imputing using mice, showing error

library(mice)
md.pattern(epi_r)
epi_rr <- mice(epi_r,m=5,maxit=10,meth='pmm',seed=500)
summary(epi_rr)

library(VIM)
aggr_plot <- aggr(epi_r, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(epi_r), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
marginplot(epi_r[c(1,2)])

#######################

epi_r<-dataset[ ,1:6]
View(epi_r)


class(epi_r)
data1<-na.exclude(epi_r)
View(data1)
sum(is.na(data1))

dataepi<-na.omit(epi_r)
View(dataepi)
na.omit(data1)
sum(is.na(dataepi))

data2 <-dataepi[1:100,2:6]
sum(is.na(data2))
View(data2)
head(data2)



library(factoextra)
res.pca<-princomp(data2, scale = FALSE) # using princomp() function
res.pca


#a.Apply PCA to the dataset and show proportion of variance


library(factoextra)
res.pca<-prcomp(data2, scale = TRUE)
res.pca

summary(res.pca)
fviz_eig(res.pca)

# Eigenvalues
eig.val <- get_eigenvalue(res.pca)
eig.val

# Results for Variables
res.var <- get_pca_var(res.pca)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 



#b.Perform PCA using SVD approach

library(factoextra)
res.pca<-prcomp(data2, p=10)
res.pca


#c.Show the graphs of PCA components
fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)
