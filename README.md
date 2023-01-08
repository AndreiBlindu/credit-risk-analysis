# Credit Risk Analysis - Financial Data Science
Credit risk modeling is a financial analysis technique used by lenders to determine the level of credit risk associated with extending credit to a borrower. In our case we aim to determine the default risk associated with a company based on different past metrics of that company.

## Dataset
The analysis was performed on the dataset, that you can find in the Complete_Data.xlsx file, containing the following features from year 2015 to 2020:

- Company name
- Turnover (2015-2020): total sales made by a business in a certain period, how quickly the company runs its business
- EBIT (2015-2020): Earnings Before Interest and Taxes is an indicator of a company's profitability
- PLTax (2015-2020): company tax based on their cumulative income over their lifetime
- MScore (2015-2020): credit risk associated with the company
- Region: location of the company inside its country
- Country
- NACE code: statistical classification of economic activities in the European Community
- Sector 1: detailed description of the company's activity
- Sector 2: general sector
- Leverage (2015-2020): refers to the use of debt (borrowed funds) to amplify returns from an investment or project
- ROE (2015-2020): measure of a company's net income divided by its shareholders' equity
- TAsset (2015-2020): total value of assets owned by the company

I considered the MScore as the response variable that I wanted to predict and explain based on the other features.
As part of the pre-processing this variable was binarized considering the values from level A to B as low level of risk (value 0) and the values from C to D as high level of risk (value 1).

## Analysis steps and code
At first, with the code in [brainstorming.R](https://github.com/AndreiBlindu/credit-risk-analysis/blob/main/brainstorming.R) I've performed some pre-processing, an exploratory analysis and tested some machine learning models on different sets of features in order to gain some insights from my data. 

Then I came up with the idea of building models that predict the MScore of a generic year (MScore_Curr) based on the company attributes (NACE, Country ...) and metrics of the previous year. The code implementation for this analysis can be found in the [credit-risk-analysis.R](https://github.com/AndreiBlindu/credit-risk-analysis/blob/main/credit-risk-analysis.R) file.

## Presentation
The presentation for this project can be found [here](https://www.canva.com/design/DAFWsXvUJS8/8uQ4A2cNgiZu-38-Y3djIQ/view?utm_content=DAFWsXvUJS8&utm_campaign=designshare&utm_medium=link2&utm_source=sharebutton)


 
