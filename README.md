# Yelp_Regression_Analysis
<b> Applied R, statistical models in this project </b>

## Data Background
Yelp allows consumers/customers to provide reviews of major local businesses/restaurants. </br>
The Yelp challenge dataset includes Wisconsin restaurants reviews written in English. </br>
Also, the businesses included in the data must have at least 3 reviews older than 14 days. </br>
Also, only reviews that were recommended at the time of the data collection are included. </br>

## Project Goal
The dataset has 85,543 reviews from Madison restaurants. </br>
My goal is to create a reliable model to predict a reviewer’s rating based on text and other relevant attributes.

## Step 1 : Grouping Data
Group the data and end up with 10 groups, sum each independently. </br>
5 Levels in Negative, 5 Levels in Positive.

## Step 2 : Model Building 
Divide each factor by Characters/Words, then take the Square Root. </br>
Control effects of outliers. </br>
Wanted Better, Added Interaction and Location. </br>

## Step 3 : Model Selection
I choose the multiple linear regression model. </br>
Round Predictions so that they are between 1 and 5. </br>
Backward Search with AIC as Criterion. </br>
As the number of predictors increases, the adjusted R square value does not change a lot. </br>

## Step 4 : Model Analysis
Adjusted  R Square Value of 51.44%. </br>
P value of 2.2*10^(-16) (less than .0001), thus at least one of the predictors used is significant. </br>

## Step 5 : Model Diagnostic
The final model provides good prediction, with good Kaggle score. </br>
It has an adjusted R-squared value of 0.5144. 51% of variance in the data can be explained. </br>
The 500 numeric predictors in the dataset train are combined and classified into 10 categories. </br>
The model includes city, 10 categories of words and interactions of 10 categories of words. </br>

## Step 6 :Final Model
Star rating = 3.5 - Very Negative + Very Positive -1.5 * Mostly Negative + -0.5 * Negative

![alt text](https://github.com/gracexin98/Yelp_Regression_Analysis/blob/main/graph_1.png)
![alt text](https://github.com/gracexin98/Yelp_Regression_Analysis/blob/main/graph_2.png)
![alt text](https://github.com/gracexin98/Yelp_Regression_Analysis/blob/main/example.png)

## Summary
The final Multiple Linear Regression Model is a good tool to predict yelp restaurant ratings by using city, 10  categories of words and interactions of those 10 categories as predictors in our model. 
