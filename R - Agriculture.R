library(readxl)
Data <- read_excel("ข้อมูล/Data.xlsx", 
                   sheet = "Data")

str(Data)
head(Data)
summary(Data)

#ตรวจสอบค่า Outlier ของ X
Q1_x <- quantile(Data$Tomatoes,0.25,na.rm = TRUE)
Q3_x <- quantile(Data$Tomatoes,0.75,na.rm = TRUE)
IQR <- Q3_x - Q1_x
print(IQR)

lower_bound_x <- Q1_x -1.5*IQR
upper_bound_x <- Q3_x +1.5*IQR

outliers_x <- Data$Tomatoes[Data$Tomatoes < lower_bound_x|Data$Tomatoes>upper_bound_x]

#ตรวจสอบค่า Outlier ของ y
Q1_y <- quantile(Data$Emissions,0.25,na.rm = TRUE)
Q3_y <- quantile(Data$Emissions,0.75,na.rm = TRUE)
IQR <- Q3_y - Q1_y
print(IQR)

lower_bound_y <- Q1_y -1.5*IQR
upper_bound_y <- Q3_y +1.5*IQR

outliers_y <- Data$Emissions[Data$Emissions < lower_bound_y|Data$Emissions>upper_bound_y]

library(ggplot2)
#กราฟกระจายความสัมพันธ์ระหว่างปริมาณการผลิตมันฝรั่งและปริมาณการใช้พลัง
ggplot(Data, aes(x = Data$Tomatoes, y = Data$Emissions,color = "blue")) +
  geom_point(size = 1, alpha = 0.7) +
  labs(title = "Emission and Tomatoes",x = "Tomatoes", y = "Emission")

#Boxplot x
ggplot(Data, aes(y = Data$Tomatoes)) + geom_boxplot() + 
  geom_boxplot(outlier.color = "red", outlier.size = 2)

#Boxplot y
ggplot(Data, aes(y = Data$Emissions)) + geom_boxplot() + 
  geom_boxplot(outlier.color = "red", outlier.size = 2)

#Boxplot แยกตามประเทศ
#ปริมาณการผลิตมันฝรั่ง
ggplot(Data, aes(x = Data$Area, y = Data$Tomatoes,fill = Data$Area)) + 
  geom_boxplot(outlier.color = "red", outlier.size = 2) +
  labs(title = "Tomatoes and Area") + theme_minimal()

#ปริมาณการใช้พลังงาน
ggplot(Data, aes(x = Data$Area, y = Data$Emissions,fill = Data$Area)) + 
  geom_boxplot(outlier.color = "red", outlier.size = 2) +
  labs(title = "Emissions and Area") + theme_minimal()

#ค่า Correlation
correlation <- cor(Data$Tomatoes, Data$Emissions, method = "spearman")

cor_test <- cor.test(Data$Tomatoes, Data$Emissions, method = "pearson")

---------------------------------------------------------------------------------------------------

#สร้างการถดถอยเชิงเส้นอย่างง่าย
model <- lm(Emissions ~ Tomatoes,data = Data)
summary(model)

plot(Data$Tomatoes,Data$Emissions)
abline(model, col="red")

par(mfrow = c(2,2))
plot(model)

shapiro.test(residuals(model))
library(car)
ncvTest(model)

predictions <- predict(model,interval = "confidence")
head(predictions)

model2 <- lm(Emissions ~ poly(Tomatoes, 2),data = Data)
coef(model2)
summary(model2)

plot(Data$Tomatoes,Data$Emissions)
abline(model2, col="red")

par(mfrow = c(2,2))
plot(model2)

shapiro.test(residuals(model2))
library(car)
ncvTest(model2)

--------------------------------------------------------------------------------------------------

install.packages("ggplot2")
library(ggplot2)

model_poly2 <- lm(Emissions ~ poly(Tomatoes,2,raw = TRUE),data = Data)
summary(model_poly2)
summary(model_poly2)$coefficients

model_poly3 <- lm(Emissions ~ poly(Tomatoes,3,raw = TRUE),data = Data)
summary(model_poly3)
summary(model_poly3)$coefficients

model_poly4 <- lm(Emissions ~ poly(Tomatoes,4,raw = TRUE),data = Data)
summary(model_poly4)
summary(model_poly4)$coefficients

model_poly5 <- lm(Emissions ~ poly(Tomatoes,5,raw = TRUE),data = Data)
summary(model_poly5)
summary(model_poly5)$coefficients

model_poly6 <- lm(Emissions ~ poly(Tomatoes,6,raw = TRUE),data = Data)
summary(model_poly6)
summary(model_poly6)$coefficients

model_poly7 <- lm(Emissions ~ poly(Tomatoes,7,raw = TRUE),data = Data)
summary(model_poly7)
summary(model_poly7)$coefficients

model_poly8 <- lm(Emissions ~ poly(Tomatoes,8,raw = TRUE),data = Data)
summary(model_poly8)
summary(model_poly8)$coefficients

model_poly9 <- lm(Emissions ~ poly(Tomatoes,9,raw = TRUE),data = Data)
summary(model_poly9)
summary(model_poly9)$coefficients

new_data <- data.frame(Tomatoes = seq(min(Data$Tomatoes), max(Data$Tomatoes), length.out = 100) )
pred2 <- predict(model_poly2, newdata = new_data) 
pred3 <- predict(model_poly3, newdata = new_data) 
pred4 <- predict(model_poly4, newdata = new_data)
pred5 <- predict(model_poly5, newdata = new_data)
pred6 <- predict(model_poly6, newdata = new_data)
pred9 <- predict(model_poly9, newdata = new_data)

new_data$quad <- pred2 
new_data$cubic <- pred3 
new_data$quintic <- pred4
new_data$sextic <- pred5
new_data$septic <- pred6
new_data$decic <- pred9

install.packages("ggplot2")
library(ggplot2)
ggplot(Data, aes(x = Tomatoes,y = Emissions)) +
  geom_point(alpha = 0.5) +
  geom_line(data = new_data, aes(x = Tomatoes, y = quad, color = "Quadratic")) +
  geom_line(data = new_data, aes(x = Tomatoes, y = cubic, color = "Cubic")) + 
  geom_line(data = new_data, aes(x = Tomatoes, y= quartic, color = "Quartic")) +
  geom_line(data = new_data, aes(x = Tomatoes, y= sextic, color = "Sextic")) +
  labs(title = "เปรียบเทียบ Polynomial Regression ระดับต่างๆ",
       x = "ปริมาณกาารผลิตมันฝรั่ง",
       y = "ปริมาณการใช้พลังงาน",
       color = "ระดับของ Polynomial") +
  theme_minimal() + 
  theme(legend.position = "bottom")

#เปรียบเทียบค่า R-squared ของแต่ละโมเดล
summary_stats <- data.frame(Model = c("Quadratic","Cubic","Quartic","Sextic"),R_squared = c(
summary(model_poly2)$r.squared, summary(model_poly3)$r.squared,
summary(model_poly4)$r.squared,summary(model_poly5)$r.squared), Adj_R_squared = c(
summary(model_poly2)$adj.r.squared, summary(model_poly3)$adj.r.squared,
summary(model_poly4)$adj.r.squared,summary(model_poly5)$adj.r.squared))

print(summary_stats)

summary(model_poly2)$adj.r.squared
summary(model_poly3)$adj.r.squared
summary(model_poly4)$adj.r.squared

install.packages("Metrics")
install.packages("caret")
install.packages("splines")
library(Metrics)
library(caret)
library(splines)

mse_quad_metrics <- mse(Data$Emissions,predict(model_poly2))
mse_cubic_metrics <- mse(Data$Emissions,predict(model_poly3))
mse_quartic_metrics <- mse(Data$Emissions,predict(model_poly4))
mse_sextic_metrics <- mse(Data$Emissions,predict(model_poly5))

rmse_quad_metrics <- rmse(Data$Emissions,predict(model_poly2))
rmse_cubic_metrics <- rmse(Data$Emissions,predict(model_poly3))
rmse_quartic_metrics <- rmse(Data$Emissions,predict(model_poly4))
rmse_sextic_metrics <- rmse(Data$Emissions,predict(model_poly5))

comparison_table <- data.frame(Model = c("Quadratic","Cubic","Quartic","Sextic"), MSE = c(mse_quad_metrics,mse_cubic_metrics,mse_quartic_metrics,mse_sextic_metrics), RMSE = c(rmse_quad_metrics,rmse_cubic_metrics,rmse_quartic_metrics,rmse_sextic_metrics))

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------

install.packages(c("tidyverse","cluster","factoextra"))
library(tidyverse)
library(cluster)
library(factoextra)

set.seed(1234)

clustering_data <- Data%>%
select(Tomatoes,Emissions)%>%
na.omit()

scaled_data <- scale(clustering_data)

fviz_nbclust(scaled_data,kmeans,method = "wss") + 
  labs(title = "Elbow Mathod")

fviz_nbclust(scaled_data,kmeans,method = "silhouette") +
  labs(title = "Silhouette Method")

kmeans_result <- kmeans(scaled_data, centers = 3, nstart = 25)
fviz_cluster(kmeans_result,data = scaled_data,
             geom = "point",
             ellipse.type = "convex",
             main = "K-means Clustering Results")

print(kmeans_result)

install.packages("tidyverse")
install.packages("cluster")
install.packages("factoextra")
install.packages("dplyr")

library(dplyr)
library(tidyverse)
library(cluster)
library(factoextra)

clustering_data <- Data[,c("Area","Year","Tomatoes","Emissions")]
kmeans_result <- kmeans(clustering_data[,c("Tomatoes","Emissions")], centers = 3, nstart = 25)

clustering_data$Cluster <- kmeans_result$cluster

cluster_counts <- table(clustering_data$Cluster)
print("จำนวนข้อมูลในแต่ละกลุ่ม")
print(cluster_counts)

cluster_means <- aggregate(cbind(Emissions,Tomatoes)~Cluster,
                           data = clustering_data,
                           mean)
print("\nค่าเฉลี่ยของแต่ละกลุ่ม")
print(cluster_means)

for(i in 1:4){
  cat("\nประเทศในกลุ่ม",i,":\n")
  print(unique(clustering_data$Area[clustering_data$Cluster== i]))
}

for(i in 1:4){cat("\nกลุ่มที่",i,":\n")
  cluster_data <- clustering_data[clustering_data$Cluster == i,]
  countries <- unique(cluster_data$Area)
  for(Area in countries){
    country_data <- cluster_data[cluster_data$Area == Area,]
    cat("\nประเทศ:",Area)
    cat("\nปี",min(country_data$Year),"ถึง",max(country_data$Year))
    cat("\nค่าเฉลี่ยปริมาณการผลิตมันฝรั่ง",mean(country_data$Tomatoes))
    cat("\nค่าเฉลี่ยปริมาณการปล่อยมลพิษ",mean(country_data$Emissions))
    cat("\n--------------------------")
  }
}

install.packages("clValid")
library(clValid)

install.packages("fpc")
library(fpc)

metrics <- cluster.stats(
  d = dist(clustering_data[,c("Tomatoes","Emissions")]),
  clustering = kmeans_result$cluster
)

cat("Dunn Index:",metrics$dunn,"\n")
cat("Silhouette Score:",metrics$avg.silwidth,"\n")
cat("Calinski-Harabasz Index:",metrics$ch, "\n")

ggplot(clustering_data, aes(x = Tomatoes, y = Emissions, color = factor(Cluster))) +
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ poly(x,3), se = FALSE) +
  facet_wrap(~Cluster) +
  labs(title = "Polynomial Regression by Cluster",
       x = "Tomatoes",
       y = "Emission",
       color = "Cluster") +
  theme_minimal()

for(i in 1:3){
  cluster_data <- clustering_data[clustering_data$Cluster == i,]
  model <- lm(Emissions~poly(Tomatoes,3,raw = TRUE),data = cluster_data)
  cat("\nCluster",i,"Polynomial Model:\n")
  print(summary(model)$coefficients)
  cat("R-squared:",summary(model)$r.squared,"\n")
}

-------------------------------------------------------------------------------------------------
library(caret)
set.seed(123)

train_data <-Data[Data$Year < 2011,]
test_data <-Data[Data$Year > 2010,]

model_polynew5 <- lm(Emissions ~ poly(Tomatoes,5,raw = TRUE),data = train_data)
summary(model_polynew5)
summary(model_polynew5)$coefficients

predictions <- predict(model_polynew5, newdata = test_data)

error <- test_data$Emissions-predictions

df <- data.frame(test_data, Predicted = predictions, Error = error)

mse <- mse(test_data$Emissions - predictions)
rmse <- rmse(test_data$Emissions,predictions)
