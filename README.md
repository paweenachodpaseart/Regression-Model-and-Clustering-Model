<div id="header" align="center">
 <img src="https://github.com/paweenachodpaseart/Regression-Model-and-Clustering-Model/blob/main/tomato.jpg?raw=true"width="900"/>
</div>

# Regression-Model-and-Clustering-Model
Analyze the impact of tomato production on agricultural energy consumption

### 📌 Project Overview 
#### 1. Dataset
- Study Areas: 10 countries, including Argentina, Brazil, China, France, Germany, Indonesia, Japan, Mexico, Türkiye, and the United States.
- Time Period: 1991 - 2021
- Data Size: 310 rows, 4 columns
#### 2. Data Analysis Methods
##### 1. Outlier Detection
- Analyzed outliers in tomato production volume and energy consumption for each country.
##### 2. Relationship Analysis
- Used scatter plots to examine the relationship between tomato production and energy consumption.
##### 3. Polynomial Regression Model
- Tested different polynomial degrees to find the most suitable model.
- Evaluated model performance using Adjusted R-squared and RMSE.
##### 4. K-Means Clustering for Country Classification
- Tested clustering into 2, 3, and 4 groups.
- Assessed clustering quality using Dunn Index, Silhouette Score, and Calinski-Harabasz Index.

#### 4. Results and Conclusion
- Polynomial Regression Model
  - Adjusted R-squared = 0.7215
  - RMSE = 110412.8
  - Indicates a correlation between tomato production volume and energy consumption in agriculture.
-  K-Means Clustering
  - 2 clusters: Show the best separation between groups.
  - 4 clusters: Exhibit the highest density within groups and the best inter-group separation.
  - Countries with similar production and energy consumption patterns were grouped accordingly.

--- 

### Conclusion
- There is a correlation between tomato production volume and energy consumption in the agricultural sector.
- The Polynomial Regression Model effectively explains data trends.
- K-Means Clustering successfully classifies countries with similar production and energy usage behaviors.
- These insights can be utilized to optimize energy management in the agricultural sector for improved efficiency.

### 🛠️ Tool: R
