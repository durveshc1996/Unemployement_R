# About the project:
The project presents time-series analysis on annual unemployment rate, an important driver of monetary policy decisions, persisting in Australia for over a decade, 2010-2021. Due to economic downturn faced over the last year, unemployment rate has been at all-time high to 7.4% which aggravates the significance of this analysis. It is an important summary statistic to monitor economic progress which necessitates making monetary decisions, such as wage rates, monetary policy settings. (Education, 2021) The long-term unemployment often increases the well-being costs of Australia, leading to public policy interventions. The aim of this project is to identify the best fitted time-series model on the univariate time-series dataset to understand unemployment component and forecast the rate in the future for next 4 months. The report is primarily divided into three sections. First, the exploratory data analysis suggested the presence of trend, AR and MA components in the series. The stationary component was removed by performing first order differencing, evidenced by stationary tests. The report proceeds into model specification by using model specification tools like ACF and PACF plots along with EACF and BIC. The possible set of candidate stochastic trend models are fitted to the series, to identify the best fitted model, based on residual analysis. Finally, the best fitted model was used to forecast the variable for the next 10 months.

# Screenshots: Some snips of the process for quick overview :)

##### The flow of the project:

![image](https://user-images.githubusercontent.com/86138415/122757750-59ba8980-d2db-11eb-9b8e-6a5e2bec6824.png)

##### Time-Series Plot of Unemployment Rate (2010-2021):

![image](https://user-images.githubusercontent.com/86138415/122758850-a0f54a00-d2dc-11eb-949e-158233908d00.png)
 
##### McLeod Li Test and Normal Q-Q Plot(For Garch Model):

![image](https://user-images.githubusercontent.com/86138415/122758383-157bb900-d2dc-11eb-8958-9cbdfb7cd1c4.png)

##### First differencing:

![image](https://user-images.githubusercontent.com/86138415/122759452-4a3c4000-d2dd-11eb-9fcb-181fd5037c2f.png)


##### ARIMA(3,1,2) residuals: 

![image](https://user-images.githubusercontent.com/86138415/122758501-35ab7800-d2dc-11eb-908c-6e651b5cf3db.png)


##### Forecast: 

![image](https://user-images.githubusercontent.com/86138415/122758797-8f13a700-d2dc-11eb-8ff6-71c467e63953.png)




# Setup:
Download the R file and dataset
Download R studio to open the R file.
##### Note: Please change the path to load the dataset


Thank you for visiting my repository.
