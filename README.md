# Forecasting tax revenues with Buoyancy and Elasticity Approach

The orthodox approach to forecasting tax revenues is usually based on forecasting revenues based on tax buoyancy and tax elasticity. Tax buoyancy measures the gross elasticity of tax revenues in relation to the respective macroeconomic variable (e.g GDP ). 

The main characteristic of this approach is that it measures the overall elasticity of taxes in relation to their base. In the tax elasticity approach, the time series needs to be first excluded from the discretionary measures of the fiscal policy to calculate the coefficient of the net tax elasticity in relation to the respective macroeconomic variable. Tax elasticity, according to Jenkins et al. (2000, p. 39), is a relevant factor for forecasting and is most often used by ministries of finance when forecasting tax revenues. Furthermore, to obtain more robust forecasts when estimating the elasticities, it is necessary to harmonise them with the business cycle in the economy, which has significant effects on revenue collection.

Collection of revenues
![Test picture](https://github.com/jordans78/Forecasting-tax-revenues/blob/main/Documentation/CollectionStructureOfRevenues.PNG)


Share of taxes in GDP
![Stucture of TAX](https://github.com/jordans78/Forecasting-tax-revenues/blob/main/Documentation/ShareOfGDP.png)


Estimated tax buoyancy and tax elasticity coeffcients
![Test picture](https://github.com/jordans78/Forecasting-tax-revenues/blob/main/Documentation/Coefficients.png)

|   	| Buoyancy 	| Elasticity 	| Short_run_effect 	| Long_run_effect 	| Error_correction_coefficient 	|
|---	|---	|---	|---	|---	|---	|
| PIT 	| 0.47 	| 1.00 	| 0.86 	| 1.06 	| -0.21 	|
| CIT 	| 1.83 	| 1.47 	| 1.59 	| 1.41 	| -0.59 	|
| VAT 	| 0.83 	| 1.02 	| 1.05 	| 1.01 	| -0.49 	|
| EXCISE 	| 0.70 	| 0.85 	| 0.77 	| 0.84 	| -0.71 	|
| Customs duties 	| -1.47 	| 1.00 	| 0.59 	| 0.95 	| -0.70 	|
| SSC 	| 0.67 	| 0.97 	| 0.89 	| 0.96 	| -1.14 	|
| TAX 	| 0.60 	| 1.00 	| 1.00 	| 1.00 	| -0.66 	|
| TAX_SSC 	| 0.64 	| 1.01 	| 1.00 	| 1.01 	| -0.88 	|


Dashboard produced from the model
![Dasboard](https://github.com/jordans78/Forecasting-tax-revenues/blob/main/Documentation/PIT.PNG)



Data used in this model are publicly available on the web pages of MoF and SSO in Republic of North Macedonia. Hoewer this model can be used on diffrent countries with 
GRD - Government revenue dataset from UNU-WIDER https://www.wider.unu.edu/about-grd

