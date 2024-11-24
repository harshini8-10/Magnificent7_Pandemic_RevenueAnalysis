# Title of the Project : Time Series Forecasting - Analysis of  Revenue Trends of the Magnificent Seven Companies During and Post-COVID-19 Pandemic

# Description : 
This project estimates the impact of the COVID-19 pandemic on the revenues of the Magnificent Seven (M7) companies, using quarterly revenue data from 2010 to 2023. Specific time series ensemble models were developed to forecast quarterly revenues during the pandemic period, and these models were applied in counterfactual analysis to evaluate revenue trends, including revenue losses, gains, or stability during the COVID pandemic.

# Goals :
•	 Develop accurate time series forecasting models to analyze quarterly revenues of the M7 companies.

•	 Conduct counterfactual analysis to estimate revenue trends during the COVID pandemic.

•	 Quantify the impact of the COVID pandemic on M7 companies' revenues by comparing forecasted and actual revenues.

# Technologies Used :
•	R Programming : For data preprocessing, model development, visualization and analysis.

•	Time Series Models : Naïve model, Seasonal Naïve model, Two-level model (Regression + Trailing MA), Automated Holt Winter’s Model, Regression Models, Two-level model (Automated Holt Winter’s Model + Autoregressive Model of Order 1), and Automated Autoregressive Integrated Moving Average (ARIMA) Model, Ensemble of best models

# Challenges and Future Work :
•	Challenges: Ensuring accurate forecasts for companies with differing revenue trends, handling data anomalies, and selecting ensemble weights.

•	Future Work: Explore additional external factors impacting revenue and test advanced machine learning models (e.g., neural networks) to further improve forecasting accuracy.

# Table of Contents :
1.	Introduction
2.	Project Objectives
3.	Project Scope
4.	Methodology
5.	How to Install and Run the Project
6.	How to Use the Project
7.	Forecasting Process and Analysis
8.	Results
9.	Conclusion

# 1.	Introduction
This project explores the effects of the COVID pandemic on the revenue trends of the Magnificent Seven (M7) companies— Alphabet, Amazon, Apple, Meta, Microsoft, NVIDIA and Tesla. Using quarterly data, time series models were developed to forecast revenue trends for the pandemic period (2020-2023) and perform counterfactual analysis to quantify the pandemic’s impact.

# 2.	Project Objectives
•	Identify the best time series forecasting models using historical data (2010-2019 for most companies) and develop ensemble of those models.

•	Forecast quarterly revenues for 2020-2023 and compare actual revenue trends against forecasts.

•	Apply counterfactual analysis to assess the impact of COVID-19 on the revenue trends of M7 companies.


# 3.	Project Scope
•	Time Period: Quarterly revenue data from Q1 2010 to Q4 2023,  (Q1 2014 to Q4 2023 for Tesla)

•	Data Source: https://www.macrotrends.net

•	Procedure: Develop various time series analysis models, determine the best models using accuracy metrics to develop ensemble of those models deploying the same to forecast and compare revenue for pandemic and post-pandemic periods.

# 4.	Methodology
Data Preparation

•	Data Transformation: Convert raw revenue data into CSV format suitable for analysis.

•	Data Partitioning: Split data into training (2010-2016), validation (2017-2019), and future (2020-2023) sets.

## Forecasting Models
i.	Naïve Model

ii.	Seasonal Naïve Model

iii.	Two-Level Forecasting models

      a.	Regression Model with Linear Trend + Trailing Moving Average Models for window widths k = 2, 3, and 4
      
      b.	Regression Model with Quadratic Trend + Trailing Moving Average Models for window widths k = 2, 3, and 4
      
iv.	Automated Holt-Winter’s Model

v.	Regression Models

    a.	Regression Model with Linear Trend
    
    b.	Regression Model with Quadratic Trend
    
    c.	Regression Model with No Trend but Seasonality
    
    d.	Regression Model with Linear Trend but Seasonality
    
    e.	Regression Model with Quadratic Trend but Seasonality
    
vi.	Two-Level Forecasting model : Automated Holt-Winter’s Model + Autoregressive Model

vii.	Automated ARIMA Model

viii.	Ensemble of Best 3 Models with weights

# 5.	How to Install and Run the Project
•	Install R : Ensure R is installed on your machine.

•	Install Required Packages : 

install.packages("forecast") 

install.packages("zoo") 

•	Clone the Repository: Clone this GitHub repository to your local machine.

git clone https://github.com/harshini8-10/Magnificent7_Pandemic_RevenueAnalysis.git

•	Run the Analysis Script : Navigate to the project directory and run the R scripts.

    Alphabet_upd.R
    Amazon_upd.R
    Apple_upd.R
    Meta_upd.R
    Microsoft_upd.R
    NVIDIA_upd.R
    Tesla_upd.R




