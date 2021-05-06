# FinancialShenanigans
Repository for Predicting Financial Shenanigans from Financial Statements

From the forthcoming preprint on ArXiv: [add link]

# Abstract
Every investor’s primary concern is (_or should be_) the prevention of the permanent loss of capital. There are many proprietary models available to investing professionals to identify companies that are engaging in financial shenanigans. In this project I develop a model that the average investor can use. With a step-wise AUC procedure in a Bayesian hieararchical modeling framework, I identified 3 Generally Accepted Accounting Principles items that predict false disclosures. These items include: basic shares outstanding from the income sheet, net purchases of PP&E on the cash flow sheet, and other equity from the balance sheet. The prediction model works over arbitrary time windows and had an accuracy of 0.9 when predicting fraudulent disclosure on a test data set. Interactions between these 3 variables revealed a high probability of fraudulent disclosure when all 3 are being manipulated. Investors may find this model useful when building a stock portfolio or to check on the status of their existing holdings. All code, data, and models are available on github for future researchers.
