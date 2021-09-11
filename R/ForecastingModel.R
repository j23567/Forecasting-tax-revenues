
#  Forecasting tax revenues ######################################################################################################################################################################
#                                                                                                                                                                                                #
#                                                                                                                                                                                                #
#                                       # CALCULATING ELASTICITY AND FORECASTING REVENUE                                                                                                         #
#                                                                                                                                                                                                #
#                                                   9-10-2021                                                                              by Jordan Simonov                                    #
#                                                                                                                                                                                                #


# clear memory
rm(list=ls())

            library(readxl)
            library(dplyr)
            library(forecast)
            library(ggplot2)
            library(gridExtra)
            library(bizdays)
            library(zoo)





#  Module (I.) ----------------------------------------------------------------
       
        #  B r i e f    e x p l a n a t i o n

          #  The main goal of the module is: estimation of coefficients of elasticity (gross elasticity - buoyancy and net elasticity - tax elasticity) with multiple models (OLS, ECM and ARDL). 
          #  From what coefficient will be made a projection based on the nominal growth of GDP and resilience coefficients. 
          #  There is also an estimate of the error made by each of these models based on the coefficients of elasticity.

                # 1.Input parameters and functions ----------------------------------------
                
                            INPUT_FORECASTING_YEAR<-2021      # <------ Mandatory input parameter
                            MACRO_FISCAL_INDICATORS <- read_excel("INPUT-DATA/MACRO_FISCAL_INDICATORS/MACRO_FISCAL_INDICATORS.xlsx")
                
                # Subset of table with discretionary measures
                            DS_NEXT_YEARS<-MACRO_FISCAL_INDICATORS%>%
                            select(Year,DS_PIT,DS_CIT,DS_VAT_NET,DS_VAT_GROSS,DS_EXCISE,DS_CUSTOMS_DUTIES,DS_SSC,DS_TAX,DS_TAX_SSC,DS_NON_TAX_REVENUES,DS_REVENUES)
                            DS_NEXT_YEARS[is.na(DS_NEXT_YEARS)] <- 0
                            MACRO_FISCAL_INDICATORS[16:26]<-NULL
                               
                
                # Define simple functions for easy estimation
                            fun1 <- function(x){ ((x - lag(x))/lag(x))*100}
                            fun1a <- function(x){ ((lead(x) - lag(x))/lag(x))*100}
                            fun1ab <- function(x){ ((x) /lead(x))*100-100}
                            fun2 <- function(x,y,z){((x-(y*lag(z))))}
                            fun3 <- function(x,y){(lag(x)*lag(y))}
                
                # 2.Base estimation  ----------------------------------------          
                
                                
                            library(magrittr)
                            library(rlang)
                            library(tidyverse)
                                          
                          
                          
                          
                                BASE_ESTIMATION<-na.omit(MACRO_FISCAL_INDICATORS) #Removes empty fields
                                
                                          
                                ESTIMATION_0<-mutate(BASE_ESTIMATION,
                                                # Nominal rate growth
                                                     Nominal_growth_PIT=fun1(Nominal_PIT),
                                                     Nominal_growth_CIT=fun1(Nominal_CIT),
                                                     Nominal_growth_VAT_NET=fun1(Nominal_VAT_NET),
                                                     Nominal_growth_VAT_GROSS=fun1(Nominal_VAT_GROSS),
                                                     Nominal_growth_EXCISE=fun1(Nominal_EXCISE),
                                                     Nominal_growth_CUSTOMS_DUTIES=fun1(Nominal_CUSTOMS_DUTIES),
                                                     Nominal_growth_SSC=fun1(Nominal_SSC),
                                                     Nominal_growth_TAX=fun1(Nominal_TAX),
                                                     Nominal_growth_TAX_SSC=fun1(Nominal_TAX_SSC),
                                                     Nominal_growth_NON_TAX_REVENUES=fun1(Nominal_NON_TAX_REVENUES),
                                                     Nominal_growth_Nominal_REVENUES=fun1(Nominal_REVENUES),
                                                # Effective tax rates like percentage of GDP
                                                     ETR_PIT=(Nominal_PIT/Nominal_GDP),
                                                     ETR_CIT=(Nominal_CIT/Nominal_GDP),
                                                     ETR_VAT_NET=(Nominal_VAT_NET/Nominal_GDP),
                                                     ETR_VAT_GROSS=(Nominal_VAT_GROSS/Nominal_GDP),
                                                     ETR_EXCISE=(Nominal_EXCISE/Nominal_GDP),
                                                     ETR_CUSTOMS_DUTIES=(Nominal_CUSTOMS_DUTIES/Nominal_GDP),
                                                     ETR_SSC=(Nominal_SSC/Nominal_GDP),
                                                     ETR_TAX=(Nominal_TAX/Nominal_GDP),
                                                     ETR_TAX_SSC=(Nominal_TAX_SSC/Nominal_GDP),
                                                     ETR_NON_TAX_REVENUES=(Nominal_NON_TAX_REVENUES/Nominal_GDP),
                                                     ETR_Nominal_REVENUES=(Nominal_REVENUES/Nominal_GDP),
                                                #  GDP
                                                     Real_GDP=Nominal_GDP/GDP_Deflator*100,
                                                # PIT
                                                     DS_PIT=fun2(Nominal_PIT,Nominal_GDP,ETR_PIT),
                                                     DS_PIT_1=round((abs(DS_PIT)/Nominal_GDP)*100,2),
                                                     DS_FINAL_PIT=ifelse(DS_PIT_1>0.1,round(DS_PIT,0),0),
                                                     Real_PIT=Nominal_PIT/GDP_Deflator*100,
                                                     Real_DS_PIT=round(DS_FINAL_PIT/GDP_Deflator*100),
                                                # CIT
                                                     DS_CIT=fun2(Nominal_CIT,Nominal_GDP,ETR_CIT),
                                                     DS_CIT_1=round((abs(DS_CIT)/Nominal_GDP)*100,2),
                                                     DS_FINAL_CIT=ifelse(DS_CIT_1>0.1,round(DS_CIT,0),0),
                                                     Real_CIT=Nominal_CIT/GDP_Deflator*100,
                                                     Real_DS_CIT=round(DS_FINAL_CIT/GDP_Deflator*100),
                                                # VAT_NET
                                                     DS_VAT_NET=fun2(Nominal_VAT_NET,Nominal_GDP,ETR_VAT_NET),
                                                     DS_VAT_NET_1=round((abs(DS_VAT_NET)/Nominal_GDP)*100,2),
                                                     DS_FINAL_VAT_NET=ifelse(DS_VAT_NET_1>0.1,round(DS_VAT_NET,0),0),
                                                     Real_VAT_NET=Nominal_VAT_NET/GDP_Deflator*100,
                                                     Real_DS_VAT_NET=round(DS_FINAL_VAT_NET/GDP_Deflator*100),
                                                # VAT_GROSS
                                                     DS_VAT_GROSS=fun2(Nominal_VAT_GROSS,Nominal_GDP,ETR_VAT_GROSS),
                                                     DS_VAT_GROSS_1=round((abs(DS_VAT_GROSS)/Nominal_GDP)*100,2),
                                                     DS_FINAL_VAT_GROSS=ifelse(DS_VAT_GROSS_1>0.1,round(DS_VAT_GROSS,0),0),
                                                     Real_VAT_GROSS=Nominal_VAT_GROSS/GDP_Deflator*100,
                                                     Real_DS_VAT_GROSS=round(DS_FINAL_VAT_GROSS/GDP_Deflator*100),
                                                # EXCISE
                                                     DS_EXCISE=fun2(Nominal_EXCISE,Nominal_GDP,ETR_EXCISE),
                                                     DS_EXCISE_1=round((abs(DS_EXCISE)/Nominal_GDP)*100,2),
                                                     DS_FINAL_EXCISE=ifelse(DS_EXCISE_1>0.1,round(DS_EXCISE,0),0),
                                                     Real_EXCISE=Nominal_EXCISE/GDP_Deflator*100,
                                                     Real_DS_EXCISE=round(DS_FINAL_EXCISE/GDP_Deflator*100),
                                                # CUSTOMS_DUTIES
                                                     DS_CUSTOMS_DUTIES=fun2(Nominal_CUSTOMS_DUTIES,Nominal_GDP,ETR_CUSTOMS_DUTIES),
                                                     DS_CUSTOMS_DUTIES_1=round((abs(DS_CUSTOMS_DUTIES)/Nominal_GDP)*100,2),
                                                     DS_FINAL_CUSTOMS_DUTIES=ifelse(DS_CUSTOMS_DUTIES_1>0.1,round(DS_CUSTOMS_DUTIES,0),0),
                                                     Real_CUSTOMS_DUTIES=Nominal_CUSTOMS_DUTIES/GDP_Deflator*100,
                                                     Real_DS_CUSTOMS_DUTIES=round(DS_FINAL_CUSTOMS_DUTIES/GDP_Deflator*100),
                                                # SSC
                                                     DS_SSC=fun2(Nominal_SSC,Nominal_GDP,ETR_SSC),
                                                     DS_SSC_1=round((abs(DS_SSC)/Nominal_GDP)*100,2),
                                                     DS_FINAL_SSC=ifelse(DS_SSC_1>0.1,round(DS_SSC,0),0),
                                                     Real_SSC=Nominal_SSC/GDP_Deflator*100,
                                                     Real_DS_SSC=round(DS_FINAL_SSC/GDP_Deflator*100),
                                                # TAX
                                                     DS_TAX=fun2(Nominal_TAX,Nominal_GDP,ETR_TAX),
                                                     DS_TAX_1=round((abs(DS_TAX)/Nominal_GDP)*100,2),
                                                     DS_FINAL_TAX=ifelse(DS_TAX_1>0.1,round(DS_TAX,0),0),
                                                     Real_TAX=Nominal_TAX/GDP_Deflator*100,
                                                     Real_DS_TAX=round(DS_FINAL_TAX/GDP_Deflator*100),
                                               # TAX and SSC
                                                     DS_TAX_SSC=fun2(Nominal_TAX_SSC,Nominal_GDP,ETR_TAX_SSC),
                                                     DS_TAX_SSC_1=round((abs(DS_TAX_SSC)/Nominal_GDP)*100,2),
                                                     DS_FINAL_TAX_SSC=ifelse(DS_TAX_SSC_1>0.1,round(DS_TAX_SSC,0),0),
                                                     Real_TAX_SSC=Nominal_TAX_SSC/GDP_Deflator*100,
                                                     Real_DS_TAX_SSC=round(DS_FINAL_TAX_SSC/GDP_Deflator*100),
                                              # NON TAX REVENUES
                                                     DS_NON_TAX_REVENUES=fun2(Nominal_NON_TAX_REVENUES,Nominal_GDP,ETR_NON_TAX_REVENUES),
                                                     DS_NON_TAX_REVENUES_1=round((abs(DS_NON_TAX_REVENUES)/Nominal_GDP)*100,2),
                                                     DS_FINAL_NON_TAX_REVENUES=ifelse(DS_NON_TAX_REVENUES_1>0.1,round(DS_NON_TAX_REVENUES,0),0),
                                                     Real_NON_TAX_REVENUES=Nominal_NON_TAX_REVENUES/GDP_Deflator*100,
                                                     Real_DS_NON_TAX_REVENUES=round(DS_FINAL_NON_TAX_REVENUES/GDP_Deflator*100),
                                              # Nominal_REVENUES
                                                     DS_Nominal_REVENUES=fun2(Nominal_REVENUES,Nominal_GDP,ETR_Nominal_REVENUES),
                                                     DS_Nominal_REVENUES_1=round((abs(DS_Nominal_REVENUES)/Nominal_GDP)*100,2),
                                                     DS_FINAL_Nominal_REVENUES=ifelse(DS_Nominal_REVENUES_1>0.1,round(DS_Nominal_REVENUES,0),0),
                                                     Real_Nominal_REVENUES=Nominal_REVENUES/GDP_Deflator*100,
                                                     Real_DS_Nominal_REVENUES=round(DS_FINAL_Nominal_REVENUES/GDP_Deflator*100)
                                                      )
                                ESTIMATION_0<-ESTIMATION_0 %>%
                                              arrange(desc(Year))
                
                                
                                ESTIMATION_1<-mutate(ESTIMATION_0,          
                                                # PIT
                                                     Coefficent_change_PIT=round(Real_PIT/(Real_PIT-Real_DS_PIT),3),
                                                     Adjusted_real_PIT=round(Real_PIT*lag(cumprod(Coefficent_change_PIT), k=1, default=1),0),
                                                # CIT
                                                     Coefficent_change_CIT=round(Real_CIT/(Real_CIT-Real_DS_CIT),3),
                                                     Adjusted_real_CIT=round(Real_CIT*lag(cumprod(Coefficent_change_CIT), k=1, default=1),0),
                                                # VAT_NET
                                                     Coefficent_change_VAT_NET=round(Real_VAT_NET/(Real_VAT_NET-Real_DS_VAT_NET),3),
                                                     Adjusted_real_VAT_NET=round(Real_VAT_NET*lag(cumprod(Coefficent_change_VAT_NET), k=1, default=1),0),
                                                # VAT_GROSS
                                                     Coefficent_change_VAT_GROSS=round(Real_VAT_GROSS/(Real_VAT_GROSS-Real_DS_VAT_GROSS),3),
                                                     Adjusted_real_VAT_GROSS=round(Real_VAT_GROSS*lag(cumprod(Coefficent_change_VAT_GROSS), k=1, default=1),0),
                                                # EXCISE
                                                     Coefficent_change_EXCISE=round(Real_EXCISE/(Real_EXCISE-Real_DS_EXCISE),3),
                                                     Adjusted_real_EXCISE=round(Real_EXCISE*lag(cumprod(Coefficent_change_EXCISE), k=1, default=1),0),
                                                # CUSTOMS_DUTIES
                                                     Coefficent_change_CUSTOMS_DUTIES=round(Real_CUSTOMS_DUTIES/(Real_CUSTOMS_DUTIES-Real_DS_CUSTOMS_DUTIES),3),
                                                     Adjusted_real_CUSTOMS_DUTIES=round(Real_CUSTOMS_DUTIES*lag(cumprod(Coefficent_change_CUSTOMS_DUTIES), k=1, default=1),0),
                                                # SSC
                                                     Coefficent_change_SSC=round(Real_SSC/(Real_SSC-Real_DS_SSC),3),
                                                     Adjusted_real_SSC=round(Real_SSC*lag(cumprod(Coefficent_change_SSC), k=1, default=1),0),
                                                # TAX
                                                     Coefficent_change_TAX=round(Real_TAX/(Real_TAX-Real_DS_TAX),3),
                                                     Adjusted_real_TAX=round(Real_TAX*lag(cumprod(Coefficent_change_TAX), k=1, default=1),0),
                                                # TAX_SSC
                                                     Coefficent_change_TAX_SSC=round(Real_TAX_SSC/(Real_TAX_SSC-Real_DS_TAX_SSC),3),
                                                     Adjusted_real_TAX_SSC=round(Real_TAX_SSC*lag(cumprod(Coefficent_change_TAX_SSC), k=1, default=1),0),
                                                # NON_TAX_REVENUES
                                                     Coefficent_change_NON_TAX_REVENUES=round(Real_NON_TAX_REVENUES/(Real_NON_TAX_REVENUES-Real_DS_NON_TAX_REVENUES),3),
                                                     Adjusted_real_NON_TAX_REVENUES=round(Real_NON_TAX_REVENUES*lag(cumprod(Coefficent_change_NON_TAX_REVENUES), k=1, default=1),0),
                                                # Nominal_REVENUES
                                                     Coefficent_change_Nominal_REVENUES=round(Real_Nominal_REVENUES/(Real_Nominal_REVENUES-Real_DS_Nominal_REVENUES),3),
                                                     Adjusted_real_Nominal_REVENUES=round(Real_Nominal_REVENUES*lag(cumprod(Coefficent_change_Nominal_REVENUES), k=1, default=1),0)
                                                     )%>%
                                               arrange((Year))
                
                                
                                
                                
                                
                # 3.Estimation of coefficients with OLS (Tax buoyancy & tax elasticity approach)--------------------------------------------------
                
                                      # Tax buoyancy 
                                                # PIT
                                                    model_buoyancy_PIT<-lm(log(Real_PIT)~log(Real_GDP),data=ESTIMATION_1)
                                                  ##  summary(model_buoyancy_PIT)
                                                # CIT
                                                    model_buoyancy_CIT<-lm(log(Real_CIT)~log(Real_GDP),data=ESTIMATION_1)
                                                # VAT_NET
                                                    model_buoyancy_VAT_NET<-lm(log(Real_VAT_NET)~log(Real_GDP),data=ESTIMATION_1)
                                                # VAT_GROSS
                                                    model_buoyancy_VAT_GROSS<-lm(log(Real_VAT_GROSS)~log(Real_GDP),data=ESTIMATION_1)
                                                # EXCISE
                                                    model_buoyancy_EXCISE<-lm(log(Real_EXCISE)~log(Real_GDP),data=ESTIMATION_1)
                                                # CUSTOMS_DUTIES
                                                    model_buoyancy_CUSTOMS_DUTIES<-lm(log(Real_CUSTOMS_DUTIES)~log(Real_GDP),data=ESTIMATION_1)
                                                # SSC
                                                    model_buoyancy_SSC<-lm(log(Real_SSC)~log(Real_GDP),data=ESTIMATION_1)
                                                # TAX
                                                    model_buoyancy_TAX<-lm(log(Real_TAX)~log(Real_GDP),data=ESTIMATION_1)
                                                # TAX_SSC
                                                    model_buoyancy_TAX_SSC<-lm(log(Real_TAX_SSC)~log(Real_GDP),data=ESTIMATION_1)
                                                # NON_TAX_REVENUES
                                                    model_buoyancy_NON_TAX_REVENUES<-lm(log(Real_NON_TAX_REVENUES)~log(Real_GDP),data=ESTIMATION_1)
                                                # Nominal_REVENUES
                                                    model_buoyancy_Nominal_REVENUES<-lm(log(Real_Nominal_REVENUES)~log(Real_GDP),data=ESTIMATION_1)
                                
                                          # Summary table with coefficients from tax buoyancy approach
                                TAX_BUOYANCY_SUMMARY<-data.frame(
                                                      PIT=model_buoyancy_PIT$coefficients[2],
                                                      CIT=model_buoyancy_CIT$coefficients[2],
                                                      VAT_NET=model_buoyancy_VAT_NET$coefficients[2],
                                                      VAT_GROSS=model_buoyancy_VAT_GROSS$coefficients[2],
                                                      EXCISE=model_buoyancy_EXCISE$coefficients[2],
                                                      CUSTOMS_DUTIES=model_buoyancy_CUSTOMS_DUTIES$coefficients[2],
                                                      SSC=model_buoyancy_SSC$coefficients[2],
                                                      TAX=model_buoyancy_TAX$coefficients[2],
                                                      TAX_SSC=model_buoyancy_TAX_SSC$coefficients[2],
                                                      NON_TAX_REVENUES=model_buoyancy_NON_TAX_REVENUES$coefficients[2],
                                                      Nominal_REVENUES=model_buoyancy_NON_TAX_REVENUES$coefficients[2])
                                
                                
                                # TAX ELASTICITY
                                              # PIT
                                                    model_elasticity_PIT<-lm(log(Adjusted_real_PIT)~log(Real_GDP),data=ESTIMATION_1)
                                              # CIT
                                                    model_elasticity_CIT<-lm(log(Adjusted_real_CIT)~log(Real_GDP),data=ESTIMATION_1)
                                              # VAT_NET
                                                    model_elasticity_VAT_NET<-lm(log(Adjusted_real_VAT_NET)~log(Real_GDP),data=ESTIMATION_1)
                                              # VAT_GROSS
                                                    model_elasticity_VAT_GROSS<-lm(log(Adjusted_real_VAT_GROSS)~log(Real_GDP),data=ESTIMATION_1)
                                              # EXCISE
                                                    model_elasticity_EXCISE<-lm(log(Adjusted_real_EXCISE)~log(Real_GDP),data=ESTIMATION_1)
                                              # CUSTOMS_DUTIES
                                                    model_elasticity_CUSTOMS_DUTIES<-lm(log(Adjusted_real_CUSTOMS_DUTIES)~log(Real_GDP),data=ESTIMATION_1)
                                              # SSC
                                                    model_elasticity_SSC<-lm(log(Adjusted_real_SSC)~log(Real_GDP),data=ESTIMATION_1)
                                              # TAX
                                                    model_elasticity_TAX<-lm(log(Adjusted_real_TAX)~log(Real_GDP),data=ESTIMATION_1)
                                              # TAX_SSC
                                                    model_elasticity_TAX_SSC<-lm(log(Adjusted_real_TAX_SSC)~log(Real_GDP),data=ESTIMATION_1)
                                              # NON_TAX_REVENUES
                                                    model_elasticity_NON_TAX_REVENUES<-lm(log(Adjusted_real_NON_TAX_REVENUES)~log(Real_GDP),data=ESTIMATION_1)
                                              # Nominal_REVENUES
                                                    model_elasticity_Nominal_REVENUES<-lm(log(Adjusted_real_Nominal_REVENUES)~log(Real_GDP),data=ESTIMATION_1)
                                
                                # Summary table with coefficients from tax elasticity  approach 
                                TAX_ELASTICITY_SUMMARY<-data.frame(
                                                      PIT=model_elasticity_PIT$coefficients[2],
                                                      CIT=model_elasticity_CIT$coefficients[2],
                                                      VAT_NET=model_elasticity_VAT_NET$coefficients[2],
                                                      VAT_GROSS=model_elasticity_VAT_GROSS$coefficients[2],
                                                      EXCISE=model_elasticity_EXCISE$coefficients[2],
                                                      CUSTOMS_DUTIES=model_elasticity_CUSTOMS_DUTIES$coefficients[2],
                                                      SSC=model_elasticity_SSC$coefficients[2],
                                                      TAX=model_elasticity_TAX$coefficients[2],
                                                      TAX_SSC=model_elasticity_TAX_SSC$coefficients[2],
                                                      NON_TAX_REVENUES=model_elasticity_NON_TAX_REVENUES$coefficients[2],
                                                      Nominal_REVENUES=model_elasticity_Nominal_REVENUES$coefficients[2])
                                                      
                                # Merging  tables 
                                                      FINAL_BUOYANCY_ELASTICITY<-rbind(TAX_BUOYANCY_SUMMARY,TAX_ELASTICITY_SUMMARY)
                                                      row.names(FINAL_BUOYANCY_ELASTICITY)<-c("OLS_Buoyancy","OLS_Elasticity")
                                                      FINAL_BUOYANCY_ELASTICITY<-t(FINAL_BUOYANCY_ELASTICITY)
                
                                                      
                
                # 4.Estimation of coefficients with ECM&ARDL -----------
                                                                                                                                                                     #
                                
                                BASE_REGRESSION<-data.frame(
                                        #GDP          
                                            Real_GDP=log(ESTIMATION_1$Real_GDP)[2:18],
                                            Real_GDP_diff=diff(log(ESTIMATION_1$Real_GDP))[1:17],
                                            Real_GDP_lag1=log((ESTIMATION_1$Real_GDP))[1:17],
                                        # PIT
                                            Adjusted_real_PIT=log(ESTIMATION_1$Adjusted_real_PIT)[2:18],
                                            Adjusted_real_PIT_diff=diff(log(ESTIMATION_1$Adjusted_real_PIT))[1:17],
                                            Adjusted_real_PIT_lag1=log((ESTIMATION_1$Adjusted_real_PIT))[1:17],
                                        # CIT
                                            Adjusted_real_CIT=log(ESTIMATION_1$Adjusted_real_CIT)[2:18],
                                            Adjusted_real_CIT_diff=diff(log(ESTIMATION_1$Adjusted_real_CIT))[1:17],
                                            Adjusted_real_CIT_lag1=log((ESTIMATION_1$Adjusted_real_CIT))[1:17],
                                        # VAT_NET
                                            Adjusted_real_VAT_NET=log(ESTIMATION_1$Adjusted_real_VAT_NET)[2:18],
                                            Adjusted_real_VAT_NET_diff=diff(log(ESTIMATION_1$Adjusted_real_VAT_NET))[1:17],
                                            Adjusted_real_VAT_NET_lag1=log((ESTIMATION_1$Adjusted_real_VAT_NET))[1:17],
                                        # VAT_GROSS
                                            Adjusted_real_VAT_GROSS=log(ESTIMATION_1$Adjusted_real_VAT_GROSS)[2:18],
                                            Adjusted_real_VAT_GROSS_diff=diff(log(ESTIMATION_1$Adjusted_real_VAT_GROSS))[1:17],
                                            Adjusted_real_VAT_GROSS_lag1=log((ESTIMATION_1$Adjusted_real_VAT_GROSS))[1:17],
                                        # EXCISE
                                            Adjusted_real_EXCISE=log(ESTIMATION_1$Adjusted_real_EXCISE)[2:18],
                                            Adjusted_real_EXCISE_diff=diff(log(ESTIMATION_1$Adjusted_real_EXCISE))[1:17],
                                            Adjusted_real_EXCISE_lag1=log((ESTIMATION_1$Adjusted_real_EXCISE))[1:17],
                                        # CUSTOMS_DUTIES
                                            Adjusted_real_CUSTOMS_DUTIES=log(ESTIMATION_1$Adjusted_real_CUSTOMS_DUTIES)[2:18],
                                            Adjusted_real_CUSTOMS_DUTIES_diff=diff(log(ESTIMATION_1$Adjusted_real_CUSTOMS_DUTIES))[1:17],
                                            Adjusted_real_CUSTOMS_DUTIES_lag1=log((ESTIMATION_1$Adjusted_real_CUSTOMS_DUTIES))[1:17],
                                        # SSC
                                            Adjusted_real_SSC=log(ESTIMATION_1$Adjusted_real_SSC)[2:18],
                                            Adjusted_real_SSC_diff=diff(log(ESTIMATION_1$Adjusted_real_SSC))[1:17],
                                            Adjusted_real_SSC_lag1=log((ESTIMATION_1$Adjusted_real_SSC))[1:17],
                                        # TAX
                                            Adjusted_real_TAX=log(ESTIMATION_1$Adjusted_real_TAX)[2:18],
                                            Adjusted_real_TAX_diff=diff(log(ESTIMATION_1$Adjusted_real_TAX))[1:17],
                                            Adjusted_real_TAX_lag1=log((ESTIMATION_1$Adjusted_real_TAX))[1:17],
                                        # TAX_SSC
                                            Adjusted_real_TAX_SSC=log(ESTIMATION_1$Adjusted_real_TAX_SSC)[2:18],
                                            Adjusted_real_TAX_SSC_diff=diff(log(ESTIMATION_1$Adjusted_real_TAX_SSC))[1:17],
                                            Adjusted_real_TAX_SSC_lag1=log((ESTIMATION_1$Adjusted_real_TAX_SSC))[1:17],
                                        # NON_TAX_REVENUES
                                            Adjusted_real_NON_TAX_REVENUES=log(ESTIMATION_1$Adjusted_real_NON_TAX_REVENUES)[2:18],
                                            Adjusted_real_NON_TAX_REVENUES_diff=diff(log(ESTIMATION_1$Adjusted_real_NON_TAX_REVENUES))[1:17],
                                            Adjusted_real_NON_TAX_REVENUES_lag1=log((ESTIMATION_1$Adjusted_real_NON_TAX_REVENUES))[1:17],
                                        # Nominal_REVENUES
                                            Adjusted_real_Nominal_REVENUES=log(ESTIMATION_1$Adjusted_real_Nominal_REVENUES)[2:18],
                                            Adjusted_real_Nominal_REVENUES_diff=diff(log(ESTIMATION_1$Adjusted_real_Nominal_REVENUES))[1:17],
                                            Adjusted_real_Nominal_REVENUES_lag1=log((ESTIMATION_1$Adjusted_real_Nominal_REVENUES))[1:17])
                                             
                                
                                # Coefficients from ECM models
                                        # PIT		 
                                            ECM_regression_PIT<-lm(Adjusted_real_PIT_diff ~  Real_GDP_diff+Adjusted_real_PIT_lag1+Real_GDP_lag1,data=BASE_REGRESSION) 			 
                                        # CIT
                                            ECM_regression_CIT<-lm(Adjusted_real_CIT_diff ~  Real_GDP_diff+Adjusted_real_CIT_lag1+Real_GDP_lag1,data=BASE_REGRESSION) 			 	 
                                        # VAT_NET
                                            ECM_regression_VAT_NET<-lm(Adjusted_real_VAT_NET_diff ~  Real_GDP_diff+Adjusted_real_VAT_NET_lag1+Real_GDP_lag1,data=BASE_REGRESSION) 			 	 	 
                                        # VAT_GROSS
                                            ECM_regression_VAT_GROSS<-lm(Adjusted_real_VAT_GROSS_diff ~  Real_GDP_diff+Adjusted_real_VAT_GROSS_lag1+Real_GDP_lag1,data=BASE_REGRESSION) 			 
                                        # EXCISE
                                            ECM_regression_EXCISE<-lm(Adjusted_real_EXCISE_diff ~  Real_GDP_diff+Adjusted_real_EXCISE_lag1+Real_GDP_lag1,data=BASE_REGRESSION) 				 
                                        # CUSTOMS_DUTIES
                                            ECM_regression_CUSTOMS_DUTIES<-lm(Adjusted_real_CUSTOMS_DUTIES_diff ~  Real_GDP_diff+Adjusted_real_CUSTOMS_DUTIES_lag1+Real_GDP_lag1,data=BASE_REGRESSION)			 
                                        # SSC
                                            ECM_regression_SSC<-lm(Adjusted_real_SSC_diff ~  Real_GDP_diff+Adjusted_real_SSC_lag1+Real_GDP_lag1,data=BASE_REGRESSION)			 
                                        # TAX
                                            ECM_regression_TAX<-lm(Adjusted_real_TAX_diff ~  Real_GDP_diff+Adjusted_real_TAX_lag1+Real_GDP_lag1,data=BASE_REGRESSION)			 			 
                                        # TAX_SSC
                                            ECM_regression_TAX_SSC<-lm(Adjusted_real_TAX_SSC_diff ~  Real_GDP_diff+Adjusted_real_TAX_SSC_lag1+Real_GDP_lag1,data=BASE_REGRESSION)			 
                                            #NON_TAX_REVENUES
                                            ECM_regression_NON_TAX_REVENUES<-lm(Adjusted_real_NON_TAX_REVENUES_diff ~  Real_GDP_diff+Adjusted_real_NON_TAX_REVENUES_lag1+Real_GDP_lag1,data=BASE_REGRESSION)	
                                        # Nominal_REVENUES
                                            ECM_regression_Nominal_REVENUES<-lm(Adjusted_real_Nominal_REVENUES_diff ~  Real_GDP_diff+Adjusted_real_Nominal_REVENUES_lag1+Real_GDP_lag1,data=BASE_REGRESSION)			 
                                            
                                # Summary table of coefficients of elasticity
                                       # PIT  
                                          COEFFICENT_ECM_PIT<-data.frame(
                                                    Short_run_effect=ECM_regression_PIT$coefficients[2],
                                                    Long_run_effect=ECM_regression_PIT$coefficients[4]/-ECM_regression_PIT$coefficients[3], #OVDE DODADEN E ZNAK MINIS
                                                    Error_correction_coefficient=ECM_regression_PIT$coefficients[3])
                                          row.names(COEFFICENT_ECM_PIT)<-c("PIT")
                                        # CIT
                                          COEFFICENT_ECM_CIT<-data.frame(
                                                    Short_run_effect=ECM_regression_CIT$coefficients[2],
                                                    Long_run_effect=ECM_regression_CIT$coefficients[4]/-ECM_regression_CIT$coefficients[3],
                                                    Error_correction_coefficient=ECM_regression_CIT$coefficients[3])
                                          row.names(COEFFICENT_ECM_CIT)<-c("CIT")
                                        # VAT_NET
                                          COEFFICENT_ECM_VAT_NET<-data.frame(
                                                    Short_run_effect=ECM_regression_VAT_NET$coefficients[2],
                                                    Long_run_effect=ECM_regression_VAT_NET$coefficients[4]/-ECM_regression_VAT_NET$coefficients[3],
                                                    Error_correction_coefficient=ECM_regression_VAT_NET$coefficients[3])
                                          row.names(COEFFICENT_ECM_VAT_NET)<-c("VAT_NET")
                                        # VAT_GROSS
                                          COEFFICENT_ECM_VAT_GROSS<-data.frame(
                                                    Short_run_effect=ECM_regression_VAT_GROSS$coefficients[2],
                                                    Long_run_effect=ECM_regression_VAT_GROSS$coefficients[4]/-ECM_regression_VAT_GROSS$coefficients[3],
                                                    Error_correction_coefficient=ECM_regression_VAT_GROSS$coefficients[3])
                                          row.names(COEFFICENT_ECM_VAT_GROSS)<-c("VAT_GROSS")
                                        # EXCISE
                                          COEFFICENT_ECM_EXCISE<-data.frame(
                                                    Short_run_effect=ECM_regression_EXCISE$coefficients[2],
                                                    Long_run_effect=ECM_regression_EXCISE$coefficients[4]/-ECM_regression_EXCISE$coefficients[3],
                                                    Error_correction_coefficient=ECM_regression_EXCISE$coefficients[3])
                                          row.names(COEFFICENT_ECM_EXCISE)<-c("EXCISE")
                                        # CUSTOMS_DUTIES
                                          COEFFICENT_ECM_CUSTOMS_DUTIES<-data.frame(
                                                    Short_run_effect=ECM_regression_CUSTOMS_DUTIES$coefficients[2],
                                                    Long_run_effect=ECM_regression_CUSTOMS_DUTIES$coefficients[4]/-ECM_regression_CUSTOMS_DUTIES$coefficients[3],
                                                    Error_correction_coefficient=ECM_regression_CUSTOMS_DUTIES$coefficients[3])
                                          row.names(COEFFICENT_ECM_CUSTOMS_DUTIES)<-c("CUSTOMS_DUTIES")
                                        # SSC
                                          COEFFICENT_ECM_SSC<-data.frame(
                                                    Short_run_effect=ECM_regression_SSC$coefficients[2],
                                                    Long_run_effect=ECM_regression_SSC$coefficients[4]/-ECM_regression_SSC$coefficients[3],
                                                    Error_correction_coefficient=ECM_regression_SSC$coefficients[3])
                                          row.names(COEFFICENT_ECM_SSC)<-c("SSC")
                                        # TAX
                                          COEFFICENT_ECM_TAX<-data.frame(
                                                    Short_run_effect=ECM_regression_TAX$coefficients[2],
                                                    Long_run_effect=ECM_regression_TAX$coefficients[4]/-ECM_regression_TAX$coefficients[3],
                                                    Error_correction_coefficient=ECM_regression_TAX$coefficients[3])
                                          row.names(COEFFICENT_ECM_TAX)<-c("TAX")
                                        # TAX_SSC
                                          COEFFICENT_ECM_TAX_SSC<-data.frame(
                                                    Short_run_effect=ECM_regression_TAX_SSC$coefficients[2],
                                                    Long_run_effect=ECM_regression_TAX_SSC$coefficients[4]/-ECM_regression_TAX_SSC$coefficients[3],
                                                    Error_correction_coefficient=ECM_regression_TAX_SSC$coefficients[3])
                                          row.names(COEFFICENT_ECM_TAX_SSC)<-c("TAX_SSC")
                                        # NON_TAX_REVENUES
                                          COEFFICENT_ECM_NON_TAX_REVENUES<-data.frame(
                                                    Short_run_effect=ECM_regression_NON_TAX_REVENUES$coefficients[2],
                                                    Long_run_effect=ECM_regression_NON_TAX_REVENUES$coefficients[4]/-ECM_regression_NON_TAX_REVENUES$coefficients[3],
                                                    Error_correction_coefficient=ECM_regression_NON_TAX_REVENUES$coefficients[3])
                                          row.names(COEFFICENT_ECM_NON_TAX_REVENUES)<-c("NON_TAX_REVENUES")
                                        # Nominal_REVENUES
                                          COEFFICENT_ECM_Nominal_REVENUES<-data.frame(
                                                    Short_run_effect=ECM_regression_Nominal_REVENUES$coefficients[2],
                                                    Long_run_effect=ECM_regression_Nominal_REVENUES$coefficients[4]/-ECM_regression_Nominal_REVENUES$coefficients[3],
                                                    Error_correction_coefficient=ECM_regression_Nominal_REVENUES$coefficients[3])
                                          row.names(COEFFICENT_ECM_Nominal_REVENUES)<-c("Nominal_REVENUES")
                                          
                                          
                                # FINAL_ECM_COEFFICIENTS
                                          FINAL_ECM_COEFFICIENTS<-rbind(COEFFICENT_ECM_PIT,COEFFICENT_ECM_CIT,COEFFICENT_ECM_VAT_NET,COEFFICENT_ECM_VAT_GROSS,COEFFICENT_ECM_EXCISE,COEFFICENT_ECM_CUSTOMS_DUTIES,COEFFICENT_ECM_SSC,COEFFICENT_ECM_TAX,COEFFICENT_ECM_TAX_SSC,COEFFICENT_ECM_NON_TAX_REVENUES,COEFFICENT_ECM_Nominal_REVENUES)
                                          colnames(FINAL_ECM_COEFFICIENTS)<-c("ECM_Short_run_effect","ECM_Long_run_effect","ECM_Error_correction_coefficient")
                                        
                                # Long-term coefficients with ARDL model
                                        # PIT				
                                          ARDL_PIT<-lm(Adjusted_real_PIT ~  Adjusted_real_PIT_lag1+ Real_GDP + Real_GDP_lag1,data=BASE_REGRESSION) 
                                        # CIT				
                                          ARDL_CIT<-lm(Adjusted_real_CIT ~  Adjusted_real_CIT_lag1+ Real_GDP + Real_GDP_lag1,data=BASE_REGRESSION) 
                                        # VAT_NET				
                                          ARDL_VAT_NET<-lm(Adjusted_real_VAT_NET ~  Adjusted_real_VAT_NET_lag1+ Real_GDP + Real_GDP_lag1,data=BASE_REGRESSION) 
                                        # VAT_GROSS				
                                          ARDL_VAT_GROSS<-lm(Adjusted_real_VAT_GROSS ~  Adjusted_real_VAT_GROSS_lag1+ Real_GDP + Real_GDP_lag1,data=BASE_REGRESSION) 
                                        # EXCISE				
                                          ARDL_EXCISE<-lm(Adjusted_real_EXCISE ~  Adjusted_real_EXCISE_lag1+ Real_GDP + Real_GDP_lag1,data=BASE_REGRESSION) 
                                        # CUSTOMS_DUTIES				
                                          ARDL_CUSTOMS_DUTIES<-lm(Adjusted_real_CUSTOMS_DUTIES ~  Adjusted_real_CUSTOMS_DUTIES_lag1+ Real_GDP + Real_GDP_lag1,data=BASE_REGRESSION) 
                                        # SSC				
                                          ARDL_SSC<-lm(Adjusted_real_SSC ~  Adjusted_real_SSC_lag1+ Real_GDP + Real_GDP_lag1,data=BASE_REGRESSION) 
                                        # TAX				
                                          ARDL_TAX<-lm(Adjusted_real_TAX ~  Adjusted_real_TAX_lag1+ Real_GDP + Real_GDP_lag1,data=BASE_REGRESSION) 
                                        # TAX_SSC				
                                          ARDL_TAX_SSC<-lm(Adjusted_real_TAX_SSC ~  Adjusted_real_TAX_SSC_lag1+ Real_GDP + Real_GDP_lag1,data=BASE_REGRESSION) 
                                        # NON_TAX_REVENUES				
                                          ARDL_NON_TAX_REVENUES<-lm(Adjusted_real_NON_TAX_REVENUES ~  Adjusted_real_NON_TAX_REVENUES_lag1+ Real_GDP + Real_GDP_lag1,data=BASE_REGRESSION) 
                                        # Nominal_REVENUES				
                                          ARDL_Nominal_REVENUES<-lm(Adjusted_real_Nominal_REVENUES ~  Adjusted_real_Nominal_REVENUES_lag1+ Real_GDP + Real_GDP_lag1,data=BASE_REGRESSION) 
                                          
                                          
                                # Summary table of coefficients of elasticity
                                      # PIT  
                                          COEFFICENT_ARDL_PIT<-data.frame(
                                              Short_run_effect=ARDL_PIT$coefficients[2],
                                              Long_run_effect=((ARDL_PIT$coefficients[3]+ARDL_PIT$coefficients[4])/(1-ARDL_PIT$coefficients[2])),
                                              Error_correction_coefficient=ARDL_PIT$coefficients[2]-1)
                                          row.names(COEFFICENT_ARDL_PIT)<-c("PIT")
                                      # CIT
                                          COEFFICENT_ARDL_CIT<-data.frame(
                                            Short_run_effect=ARDL_CIT$coefficients[2],
                                            Long_run_effect=((ARDL_CIT$coefficients[3]+ARDL_CIT$coefficients[4])/(1-ARDL_CIT$coefficients[2])),
                                            Error_correction_coefficient=ARDL_CIT$coefficients[2]-1)
                                          row.names(COEFFICENT_ARDL_CIT)<-c("CIT")
                                      # VAT_NET
                                          COEFFICENT_ARDL_VAT_NET<-data.frame(
                                            Short_run_effect=ARDL_VAT_NET$coefficients[2],
                                            Long_run_effect=((ARDL_VAT_NET$coefficients[3]+ARDL_VAT_NET$coefficients[4])/(1-ARDL_VAT_NET$coefficients[2])),
                                            Error_correction_coefficient=ARDL_VAT_NET$coefficients[2]-1)
                                          row.names(COEFFICENT_ARDL_VAT_NET)<-c("VAT_NET")
                                      # VAT_GROSS
                                          COEFFICENT_ARDL_VAT_GROSS<-data.frame(
                                            Short_run_effect=ARDL_VAT_GROSS$coefficients[2],
                                            Long_run_effect=((ARDL_VAT_GROSS$coefficients[3]+ARDL_VAT_GROSS$coefficients[4])/(1-ARDL_VAT_GROSS$coefficients[2])),
                                            Error_correction_coefficient=ARDL_VAT_GROSS$coefficients[2]-1)
                                          row.names(COEFFICENT_ARDL_VAT_GROSS)<-c("VAT_GROSS")
                                      # EXCISE
                                          COEFFICENT_ARDL_EXCISE<-data.frame(
                                            Short_run_effect=ARDL_EXCISE$coefficients[2],
                                            Long_run_effect=((ARDL_EXCISE$coefficients[3]+ARDL_EXCISE$coefficients[4])/(1-ARDL_EXCISE$coefficients[2])),
                                            Error_correction_coefficient=ARDL_EXCISE$coefficients[2]-1)
                                          row.names(COEFFICENT_ARDL_EXCISE)<-c("EXCISE")
                                    # CUSTOMS_DUTIES
                                          COEFFICENT_ARDL_CUSTOMS_DUTIES<-data.frame(
                                            Short_run_effect=ARDL_CUSTOMS_DUTIES$coefficients[2],
                                            Long_run_effect=((ARDL_CUSTOMS_DUTIES$coefficients[3]+ARDL_CUSTOMS_DUTIES$coefficients[4])/(1-ARDL_CUSTOMS_DUTIES$coefficients[2])),
                                            Error_correction_coefficient=ARDL_CUSTOMS_DUTIES$coefficients[2]-1)
                                          row.names(COEFFICENT_ARDL_CUSTOMS_DUTIES)<-c("CUSTOMS_DUTIES")
                                      # SSC
                                          COEFFICENT_ARDL_SSC<-data.frame(
                                            Short_run_effect=ARDL_SSC$coefficients[2],
                                            Long_run_effect=((ARDL_SSC$coefficients[3]+ARDL_SSC$coefficients[4])/(1-ARDL_SSC$coefficients[2])),
                                            Error_correction_coefficient=ARDL_SSC$coefficients[2]-1)
                                          row.names(COEFFICENT_ARDL_SSC)<-c("SSC")
                                      # TAX
                                          COEFFICENT_ARDL_TAX<-data.frame(
                                            Short_run_effect=ARDL_TAX$coefficients[2],
                                            Long_run_effect=((ARDL_TAX$coefficients[3]+ARDL_TAX$coefficients[4])/(1-ARDL_TAX$coefficients[2])),
                                            Error_correction_coefficient=ARDL_TAX$coefficients[2]-1)
                                          row.names(COEFFICENT_ARDL_TAX)<-c("TAX")
                                      # TAX_SSC
                                          COEFFICENT_ARDL_TAX_SSC<-data.frame(
                                            Short_run_effect=ARDL_TAX_SSC$coefficients[2],
                                            Long_run_effect=((ARDL_TAX_SSC$coefficients[3]+ARDL_TAX_SSC$coefficients[4])/(1-ARDL_TAX_SSC$coefficients[2])),
                                            Error_correction_coefficient=ARDL_TAX_SSC$coefficients[2]-1)
                                          row.names(COEFFICENT_ARDL_TAX_SSC)<-c("TAX_SSC")
                                      # NON_TAX_REVENUES
                                          COEFFICENT_ARDL_NON_TAX_REVENUES<-data.frame(
                                            Short_run_effect=ARDL_NON_TAX_REVENUES$coefficients[2],
                                            Long_run_effect=((ARDL_NON_TAX_REVENUES$coefficients[3]+ARDL_NON_TAX_REVENUES$coefficients[4])/(1-ARDL_NON_TAX_REVENUES$coefficients[2])),
                                            Error_correction_coefficient=ARDL_NON_TAX_REVENUES$coefficients[2]-1)
                                            row.names(COEFFICENT_ARDL_NON_TAX_REVENUES)<-c("NON_TAX_REVENUES")
                                      # Nominal_REVENUES
                                            COEFFICENT_ARDL_Nominal_REVENUES<-data.frame(
                                              Short_run_effect=ARDL_Nominal_REVENUES$coefficients[2],
                                              Long_run_effect=((ARDL_Nominal_REVENUES$coefficients[3]+ARDL_Nominal_REVENUES$coefficients[4])/(1-ARDL_Nominal_REVENUES$coefficients[2])),
                                              Error_correction_coefficient=ARDL_Nominal_REVENUES$coefficients[2]-1)
                                            row.names(COEFFICENT_ARDL_Nominal_REVENUES)<-c("Nominal_REVENUES")
                                
                                # Final ARDL coefficients
                                            FINAL_ARDL_COEFFICIENTS<-rbind(COEFFICENT_ARDL_PIT,COEFFICENT_ARDL_CIT,COEFFICENT_ARDL_VAT_NET,COEFFICENT_ARDL_VAT_GROSS,COEFFICENT_ARDL_EXCISE,COEFFICENT_ARDL_CUSTOMS_DUTIES,COEFFICENT_ARDL_SSC,COEFFICENT_ARDL_TAX,COEFFICENT_ARDL_TAX_SSC,COEFFICENT_ARDL_NON_TAX_REVENUES,COEFFICENT_ARDL_Nominal_REVENUES)
                                            colnames(FINAL_ARDL_COEFFICIENTS)<-c("ARDL_Short_run_effect","ARDL_Long_run_effect","ARDL_Error_correction_coefficient")
                                              
                                            FINAL_ELASTICITY_COEFFICIENTS<-cbind(FINAL_BUOYANCY_ELASTICITY,FINAL_ECM_COEFFICIENTS,FINAL_ARDL_COEFFICIENTS)
                                              
                
                # 5.Evaluation of forecasting ---------------------------------------------
                                
                                
                                ESTIMATION_2<-data.frame(ESTIMATION_1)
                                ESTIMATION_2<-ESTIMATION_2 %>%
                                  arrange(desc(Year))
                                
                                Evaluation_forecasting<-mutate(ESTIMATION_2,
                                                  # GDP
                                                               Nominal_GDP_GROWTH=fun1a(Real_GDP),
                                                  # PIT
                                                              # OLS-Buoyancy
                                                               CALCULATION_PIT_Buoyancy=1+(FINAL_ELASTICITY_COEFFICIENTS[1,1]/100*Nominal_GDP_GROWTH),
                                                               FORECASTING_PIT_Buoyancy=lag(Adjusted_real_PIT)*CALCULATION_PIT_Buoyancy,
                                                               ERROR_PIT_Buoyancy=(Adjusted_real_PIT-FORECASTING_PIT_Buoyancy)/Adjusted_real_PIT,
                                                               ABSOULTE_ERROR_PIT_Buoyancy=abs(ERROR_PIT_Buoyancy),
                                                               SQ_ERROR_PIT_Buoyancy=abs(ERROR_PIT_Buoyancy)^2,
                                                                # OLS-Elasticity
                                                                CALCULATION_PIT_Elasticity_OLS=1+(FINAL_ELASTICITY_COEFFICIENTS[1,2]/100*Nominal_GDP_GROWTH),
                                                                FORECASTING_PIT_Elasticity_OLS=lag(Adjusted_real_PIT)*CALCULATION_PIT_Elasticity_OLS,
                                                                ERROR_PIT_Elasticity_OLS=(Adjusted_real_PIT-FORECASTING_PIT_Elasticity_OLS)/Adjusted_real_PIT,
                                                                ABSOULTE_ERROR_PIT_Elasticity_OLS=abs(ERROR_PIT_Elasticity_OLS),
                                                                SQ_ERROR_PIT_Elasticity_OLS=abs(ERROR_PIT_Elasticity_OLS)^2,
                                                                # ECM-Elasticity
                                                                CALCULATION_PIT_Elasticity_ECM=1+(FINAL_ELASTICITY_COEFFICIENTS[1,4]/100*Nominal_GDP_GROWTH),
                                                                FORECASTING_PIT_Elasticity_ECM=lag(Adjusted_real_PIT)*CALCULATION_PIT_Elasticity_ECM,
                                                                ERROR_PIT_Elasticity_ECM=(Adjusted_real_PIT-FORECASTING_PIT_Elasticity_ECM)/Adjusted_real_PIT,
                                                                ABSOULTE_ERROR_PIT_Elasticity_ECM=abs(ERROR_PIT_Elasticity_ECM),
                                                                SQ_ERROR_PIT_Elasticity_ECM=abs(ERROR_PIT_Elasticity_ECM)^2,                    
                                                                # ARDL-Elasticity
                                                                CALCULATION_PIT_Elasticity_ARDL=1+(FINAL_ELASTICITY_COEFFICIENTS[1,7]/100*Nominal_GDP_GROWTH),
                                                                FORECASTING_PIT_Elasticity_ARDL=lag(Adjusted_real_PIT)*CALCULATION_PIT_Elasticity_ARDL,
                                                                ERROR_PIT_Elasticity_ARDL=(Adjusted_real_PIT-FORECASTING_PIT_Elasticity_ARDL)/Adjusted_real_PIT,
                                                                ABSOULTE_ERROR_PIT_Elasticity_ARDL=abs(ERROR_PIT_Elasticity_ARDL),
                                                                SQ_ERROR_PIT_Elasticity_ARDL=abs(ERROR_PIT_Elasticity_ARDL)^2,       
                                                  # CIT
                                                                # OLS-Buoyancy
                                                                CALCULATION_CIT_Buoyancy=1+(FINAL_ELASTICITY_COEFFICIENTS[2,1]/100*Nominal_GDP_GROWTH),
                                                                FORECASTING_CIT_Buoyancy=lag(Adjusted_real_CIT)*CALCULATION_CIT_Buoyancy,
                                                                ERROR_CIT_Buoyancy=(Adjusted_real_CIT-FORECASTING_CIT_Buoyancy)/Adjusted_real_CIT,
                                                                ABSOULTE_ERROR_CIT_Buoyancy=abs(ERROR_CIT_Buoyancy),
                                                                SQ_ERROR_CIT_Buoyancy=abs(ERROR_CIT_Buoyancy)^2,
                                                                # OLS-Elasticity
                                                                CALCULATION_CIT_Elasticity_OLS=1+(FINAL_ELASTICITY_COEFFICIENTS[2,2]/100*Nominal_GDP_GROWTH),
                                                                FORECASTING_CIT_Elasticity_OLS=lag(Adjusted_real_CIT)*CALCULATION_CIT_Elasticity_OLS,
                                                                ERROR_CIT_Elasticity_OLS=(Adjusted_real_CIT-FORECASTING_CIT_Elasticity_OLS)/Adjusted_real_CIT,
                                                                ABSOULTE_ERROR_CIT_Elasticity_OLS=abs(ERROR_CIT_Elasticity_OLS),
                                                                SQ_ERROR_CIT_Elasticity_OLS=abs(ERROR_CIT_Elasticity_OLS)^2,
                                                                # ECM-Elasticity
                                                                CALCULATION_CIT_Elasticity_ECM=1+(FINAL_ELASTICITY_COEFFICIENTS[2,4]/100*Nominal_GDP_GROWTH),
                                                                FORECASTING_CIT_Elasticity_ECM=lag(Adjusted_real_CIT)*CALCULATION_CIT_Elasticity_ECM,
                                                                ERROR_CIT_Elasticity_ECM=(Adjusted_real_CIT-FORECASTING_CIT_Elasticity_ECM)/Adjusted_real_CIT,
                                                                ABSOULTE_ERROR_CIT_Elasticity_ECM=abs(ERROR_CIT_Elasticity_ECM),
                                                                SQ_ERROR_CIT_Elasticity_ECM=abs(ERROR_CIT_Elasticity_ECM)^2,                    
                                                                # ARDL-Elasticity
                                                                CALCULATION_CIT_Elasticity_ARDL=1+(FINAL_ELASTICITY_COEFFICIENTS[2,7]/100*Nominal_GDP_GROWTH),
                                                                FORECASTING_CIT_Elasticity_ARDL=lag(Adjusted_real_CIT)*CALCULATION_CIT_Elasticity_ARDL,
                                                                ERROR_CIT_Elasticity_ARDL=(Adjusted_real_CIT-FORECASTING_CIT_Elasticity_ARDL)/Adjusted_real_CIT,
                                                                ABSOULTE_ERROR_CIT_Elasticity_ARDL=abs(ERROR_CIT_Elasticity_ARDL),
                                                                SQ_ERROR_CIT_Elasticity_ARDL=abs(ERROR_CIT_Elasticity_ARDL)^2,            
                                                # VAT_NET
                                                            # OLS-Buoyancy
                                                              CALCULATION_VAT_NET_Buoyancy=1+(FINAL_ELASTICITY_COEFFICIENTS[3,1]/100*Nominal_GDP_GROWTH),
                                                                FORECASTING_VAT_NET_Buoyancy=lag(Adjusted_real_VAT_NET)*CALCULATION_VAT_NET_Buoyancy,
                                                                ERROR_VAT_NET_Buoyancy=(Adjusted_real_VAT_NET-FORECASTING_VAT_NET_Buoyancy)/Adjusted_real_VAT_NET,
                                                                ABSOULTE_ERROR_VAT_NET_Buoyancy=abs(ERROR_VAT_NET_Buoyancy),
                                                                SQ_ERROR_VAT_NET_Buoyancy=abs(ERROR_VAT_NET_Buoyancy)^2,
                                                                # OLS-Elasticity
                                                                CALCULATION_VAT_NET_Elasticity_OLS=1+(FINAL_ELASTICITY_COEFFICIENTS[3,2]/100*Nominal_GDP_GROWTH),
                                                                FORECASTING_VAT_NET_Elasticity_OLS=lag(Adjusted_real_VAT_NET)*CALCULATION_VAT_NET_Elasticity_OLS,
                                                                ERROR_VAT_NET_Elasticity_OLS=(Adjusted_real_VAT_NET-FORECASTING_VAT_NET_Elasticity_OLS)/Adjusted_real_VAT_NET,
                                                                ABSOULTE_ERROR_VAT_NET_Elasticity_OLS=abs(ERROR_VAT_NET_Elasticity_OLS),
                                                                SQ_ERROR_VAT_NET_Elasticity_OLS=abs(ERROR_VAT_NET_Elasticity_OLS)^2,
                                                                # ECM-Elasticity
                                                                CALCULATION_VAT_NET_Elasticity_ECM=1+(FINAL_ELASTICITY_COEFFICIENTS[3,4]/100*Nominal_GDP_GROWTH),
                                                                FORECASTING_VAT_NET_Elasticity_ECM=lag(Adjusted_real_VAT_NET)*CALCULATION_VAT_NET_Elasticity_ECM,
                                                                ERROR_VAT_NET_Elasticity_ECM=(Adjusted_real_VAT_NET-FORECASTING_VAT_NET_Elasticity_ECM)/Adjusted_real_VAT_NET,
                                                                ABSOULTE_ERROR_VAT_NET_Elasticity_ECM=abs(ERROR_VAT_NET_Elasticity_ECM),
                                                                SQ_ERROR_VAT_NET_Elasticity_ECM=abs(ERROR_VAT_NET_Elasticity_ECM)^2,                    
                                                                # ARDL-Elasticity
                                                                CALCULATION_VAT_NET_Elasticity_ARDL=1+(FINAL_ELASTICITY_COEFFICIENTS[3,7]/100*Nominal_GDP_GROWTH),
                                                                FORECASTING_VAT_NET_Elasticity_ARDL=lag(Adjusted_real_VAT_NET)*CALCULATION_VAT_NET_Elasticity_ARDL,
                                                                ERROR_VAT_NET_Elasticity_ARDL=(Adjusted_real_VAT_NET-FORECASTING_VAT_NET_Elasticity_ARDL)/Adjusted_real_VAT_NET,
                                                                ABSOULTE_ERROR_VAT_NET_Elasticity_ARDL=abs(ERROR_VAT_NET_Elasticity_ARDL),
                                                                SQ_ERROR_VAT_NET_Elasticity_ARDL=abs(ERROR_VAT_NET_Elasticity_ARDL)^2,
                                                # VAT_GROSS
                                                                # OLS-Buoyancy
                                                                CALCULATION_VAT_GROSS_Buoyancy=1+(FINAL_ELASTICITY_COEFFICIENTS[4,1]/100*Nominal_GDP_GROWTH),
                                                                FORECASTING_VAT_GROSS_Buoyancy=lag(Adjusted_real_VAT_GROSS)*CALCULATION_VAT_GROSS_Buoyancy,
                                                                ERROR_VAT_GROSS_Buoyancy=(Adjusted_real_VAT_GROSS-FORECASTING_VAT_GROSS_Buoyancy)/Adjusted_real_VAT_GROSS,
                                                                ABSOULTE_ERROR_VAT_GROSS_Buoyancy=abs(ERROR_VAT_GROSS_Buoyancy),
                                                                SQ_ERROR_VAT_GROSS_Buoyancy=abs(ERROR_VAT_GROSS_Buoyancy)^2,
                                                                # OLS-Elasticity
                                                                CALCULATION_VAT_GROSS_Elasticity_OLS=1+(FINAL_ELASTICITY_COEFFICIENTS[4,2]/100*Nominal_GDP_GROWTH),
                                                                FORECASTING_VAT_GROSS_Elasticity_OLS=lag(Adjusted_real_VAT_GROSS)*CALCULATION_VAT_GROSS_Elasticity_OLS,
                                                                ERROR_VAT_GROSS_Elasticity_OLS=(Adjusted_real_VAT_GROSS-FORECASTING_VAT_GROSS_Elasticity_OLS)/Adjusted_real_VAT_GROSS,
                                                                ABSOULTE_ERROR_VAT_GROSS_Elasticity_OLS=abs(ERROR_VAT_GROSS_Elasticity_OLS),
                                                                SQ_ERROR_VAT_GROSS_Elasticity_OLS=abs(ERROR_VAT_GROSS_Elasticity_OLS)^2,
                                                                # ECM-Elasticity
                                                                CALCULATION_VAT_GROSS_Elasticity_ECM=1+(FINAL_ELASTICITY_COEFFICIENTS[4,4]/100*Nominal_GDP_GROWTH),
                                                                FORECASTING_VAT_GROSS_Elasticity_ECM=lag(Adjusted_real_VAT_GROSS)*CALCULATION_VAT_GROSS_Elasticity_ECM,
                                                                ERROR_VAT_GROSS_Elasticity_ECM=(Adjusted_real_VAT_GROSS-FORECASTING_VAT_GROSS_Elasticity_ECM)/Adjusted_real_VAT_GROSS,
                                                                ABSOULTE_ERROR_VAT_GROSS_Elasticity_ECM=abs(ERROR_VAT_GROSS_Elasticity_ECM),
                                                                SQ_ERROR_VAT_GROSS_Elasticity_ECM=abs(ERROR_VAT_GROSS_Elasticity_ECM)^2,                    
                                                                # ARDL-Elasticity
                                                                CALCULATION_VAT_GROSS_Elasticity_ARDL=1+(FINAL_ELASTICITY_COEFFICIENTS[4,7]/100*Nominal_GDP_GROWTH),
                                                                FORECASTING_VAT_GROSS_Elasticity_ARDL=lag(Adjusted_real_VAT_GROSS)*CALCULATION_VAT_GROSS_Elasticity_ARDL,
                                                                ERROR_VAT_GROSS_Elasticity_ARDL=(Adjusted_real_VAT_GROSS-FORECASTING_VAT_GROSS_Elasticity_ARDL)/Adjusted_real_VAT_GROSS,
                                                                ABSOULTE_ERROR_VAT_GROSS_Elasticity_ARDL=abs(ERROR_VAT_GROSS_Elasticity_ARDL),
                                                                SQ_ERROR_VAT_GROSS_Elasticity_ARDL=abs(ERROR_VAT_GROSS_Elasticity_ARDL)^2,
                                                # EXCISE
                                                                # OLS-Buoyancy
                                                                CALCULATION_EXCISE_Buoyancy=1+(FINAL_ELASTICITY_COEFFICIENTS[5,1]/100*Nominal_GDP_GROWTH),
                                                                FORECASTING_EXCISE_Buoyancy=lag(Adjusted_real_EXCISE)*CALCULATION_EXCISE_Buoyancy,
                                                                ERROR_EXCISE_Buoyancy=(Adjusted_real_EXCISE-FORECASTING_EXCISE_Buoyancy)/Adjusted_real_EXCISE,
                                                                ABSOULTE_ERROR_EXCISE_Buoyancy=abs(ERROR_EXCISE_Buoyancy),
                                                                SQ_ERROR_EXCISE_Buoyancy=abs(ERROR_EXCISE_Buoyancy)^2,
                                                                # OLS-Elasticity
                                                                CALCULATION_EXCISE_Elasticity_OLS=1+(FINAL_ELASTICITY_COEFFICIENTS[5,2]/100*Nominal_GDP_GROWTH),
                                                                FORECASTING_EXCISE_Elasticity_OLS=lag(Adjusted_real_EXCISE)*CALCULATION_EXCISE_Elasticity_OLS,
                                                                ERROR_EXCISE_Elasticity_OLS=(Adjusted_real_EXCISE-FORECASTING_EXCISE_Elasticity_OLS)/Adjusted_real_EXCISE,
                                                                ABSOULTE_ERROR_EXCISE_Elasticity_OLS=abs(ERROR_EXCISE_Elasticity_OLS),
                                                                SQ_ERROR_EXCISE_Elasticity_OLS=abs(ERROR_EXCISE_Elasticity_OLS)^2,
                                                                # ECM-Elasticity
                                                                CALCULATION_EXCISE_Elasticity_ECM=1+(FINAL_ELASTICITY_COEFFICIENTS[5,4]/100*Nominal_GDP_GROWTH),
                                                                FORECASTING_EXCISE_Elasticity_ECM=lag(Adjusted_real_EXCISE)*CALCULATION_EXCISE_Elasticity_ECM,
                                                                ERROR_EXCISE_Elasticity_ECM=(Adjusted_real_EXCISE-FORECASTING_EXCISE_Elasticity_ECM)/Adjusted_real_EXCISE,
                                                                ABSOULTE_ERROR_EXCISE_Elasticity_ECM=abs(ERROR_EXCISE_Elasticity_ECM),
                                                                SQ_ERROR_EXCISE_Elasticity_ECM=abs(ERROR_EXCISE_Elasticity_ECM)^2,                    
                                                                # ARDL-Elasticity
                                                                CALCULATION_EXCISE_Elasticity_ARDL=1+(FINAL_ELASTICITY_COEFFICIENTS[5,7]/100*Nominal_GDP_GROWTH),
                                                                FORECASTING_EXCISE_Elasticity_ARDL=lag(Adjusted_real_EXCISE)*CALCULATION_EXCISE_Elasticity_ARDL,
                                                                ERROR_EXCISE_Elasticity_ARDL=(Adjusted_real_EXCISE-FORECASTING_EXCISE_Elasticity_ARDL)/Adjusted_real_EXCISE,
                                                                ABSOULTE_ERROR_EXCISE_Elasticity_ARDL=abs(ERROR_EXCISE_Elasticity_ARDL),
                                                                SQ_ERROR_EXCISE_Elasticity_ARDL=abs(ERROR_EXCISE_Elasticity_ARDL)^2,
                                                # CUSTOMS_DUTIES
                                                                # OLS-Buoyancy
                                                                CALCULATION_CUSTOMS_DUTIES_Buoyancy=1+(FINAL_ELASTICITY_COEFFICIENTS[6,1]/100*Nominal_GDP_GROWTH),
                                                                FORECASTING_CUSTOMS_DUTIES_Buoyancy=lag(Adjusted_real_CUSTOMS_DUTIES)*CALCULATION_CUSTOMS_DUTIES_Buoyancy,
                                                                ERROR_CUSTOMS_DUTIES_Buoyancy=(Adjusted_real_CUSTOMS_DUTIES-FORECASTING_CUSTOMS_DUTIES_Buoyancy)/Adjusted_real_CUSTOMS_DUTIES,
                                                                ABSOULTE_ERROR_CUSTOMS_DUTIES_Buoyancy=abs(ERROR_CUSTOMS_DUTIES_Buoyancy),
                                                                SQ_ERROR_CUSTOMS_DUTIES_Buoyancy=abs(ERROR_CUSTOMS_DUTIES_Buoyancy)^2,
                                                                # OLS-Elasticity
                                                                CALCULATION_CUSTOMS_DUTIES_Elasticity_OLS=1+(FINAL_ELASTICITY_COEFFICIENTS[6,2]/100*Nominal_GDP_GROWTH),
                                                                FORECASTING_CUSTOMS_DUTIES_Elasticity_OLS=lag(Adjusted_real_CUSTOMS_DUTIES)*CALCULATION_CUSTOMS_DUTIES_Elasticity_OLS,
                                                                ERROR_CUSTOMS_DUTIES_Elasticity_OLS=(Adjusted_real_CUSTOMS_DUTIES-FORECASTING_CUSTOMS_DUTIES_Elasticity_OLS)/Adjusted_real_CUSTOMS_DUTIES,
                                                                ABSOULTE_ERROR_CUSTOMS_DUTIES_Elasticity_OLS=abs(ERROR_CUSTOMS_DUTIES_Elasticity_OLS),
                                                                SQ_ERROR_CUSTOMS_DUTIES_Elasticity_OLS=abs(ERROR_CUSTOMS_DUTIES_Elasticity_OLS)^2,
                                                                # ECM-Elasticity
                                                                CALCULATION_CUSTOMS_DUTIES_Elasticity_ECM=1+(FINAL_ELASTICITY_COEFFICIENTS[6,4]/100*Nominal_GDP_GROWTH),
                                                                FORECASTING_CUSTOMS_DUTIES_Elasticity_ECM=lag(Adjusted_real_CUSTOMS_DUTIES)*CALCULATION_CUSTOMS_DUTIES_Elasticity_ECM,
                                                                ERROR_CUSTOMS_DUTIES_Elasticity_ECM=(Adjusted_real_CUSTOMS_DUTIES-FORECASTING_CUSTOMS_DUTIES_Elasticity_ECM)/Adjusted_real_CUSTOMS_DUTIES,
                                                                ABSOULTE_ERROR_CUSTOMS_DUTIES_Elasticity_ECM=abs(ERROR_CUSTOMS_DUTIES_Elasticity_ECM),
                                                                SQ_ERROR_CUSTOMS_DUTIES_Elasticity_ECM=abs(ERROR_CUSTOMS_DUTIES_Elasticity_ECM)^2,                    
                                                                # ARDL-Elasticity
                                                                CALCULATION_CUSTOMS_DUTIES_Elasticity_ARDL=1+(FINAL_ELASTICITY_COEFFICIENTS[6,7]/100*Nominal_GDP_GROWTH),
                                                                FORECASTING_CUSTOMS_DUTIES_Elasticity_ARDL=lag(Adjusted_real_CUSTOMS_DUTIES)*CALCULATION_CUSTOMS_DUTIES_Elasticity_ARDL,
                                                                ERROR_CUSTOMS_DUTIES_Elasticity_ARDL=(Adjusted_real_CUSTOMS_DUTIES-FORECASTING_CUSTOMS_DUTIES_Elasticity_ARDL)/Adjusted_real_CUSTOMS_DUTIES,
                                                                ABSOULTE_ERROR_CUSTOMS_DUTIES_Elasticity_ARDL=abs(ERROR_CUSTOMS_DUTIES_Elasticity_ARDL),
                                                                SQ_ERROR_CUSTOMS_DUTIES_Elasticity_ARDL=abs(ERROR_CUSTOMS_DUTIES_Elasticity_ARDL)^2,
                                                # SSC
                                                                # OLS-Buoyancy
                                                                CALCULATION_SSC_Buoyancy=1+(FINAL_ELASTICITY_COEFFICIENTS[7,1]/100*Nominal_GDP_GROWTH),
                                                                FORECASTING_SSC_Buoyancy=lag(Adjusted_real_SSC)*CALCULATION_SSC_Buoyancy,
                                                                ERROR_SSC_Buoyancy=(Adjusted_real_SSC-FORECASTING_SSC_Buoyancy)/Adjusted_real_SSC,
                                                                ABSOULTE_ERROR_SSC_Buoyancy=abs(ERROR_SSC_Buoyancy),
                                                                SQ_ERROR_SSC_Buoyancy=abs(ERROR_SSC_Buoyancy)^2,
                                                                # OLS-Elasticity
                                                                CALCULATION_SSC_Elasticity_OLS=1+(FINAL_ELASTICITY_COEFFICIENTS[7,2]/100*Nominal_GDP_GROWTH),
                                                                FORECASTING_SSC_Elasticity_OLS=lag(Adjusted_real_SSC)*CALCULATION_SSC_Elasticity_OLS,
                                                                ERROR_SSC_Elasticity_OLS=(Adjusted_real_SSC-FORECASTING_SSC_Elasticity_OLS)/Adjusted_real_SSC,
                                                                ABSOULTE_ERROR_SSC_Elasticity_OLS=abs(ERROR_SSC_Elasticity_OLS),
                                                                SQ_ERROR_SSC_Elasticity_OLS=abs(ERROR_SSC_Elasticity_OLS)^2,
                                                                # ECM-Elasticity
                                                                CALCULATION_SSC_Elasticity_ECM=1+(FINAL_ELASTICITY_COEFFICIENTS[7,4]/100*Nominal_GDP_GROWTH),
                                                                FORECASTING_SSC_Elasticity_ECM=lag(Adjusted_real_SSC)*CALCULATION_SSC_Elasticity_ECM,
                                                                ERROR_SSC_Elasticity_ECM=(Adjusted_real_SSC-FORECASTING_SSC_Elasticity_ECM)/Adjusted_real_SSC,
                                                                ABSOULTE_ERROR_SSC_Elasticity_ECM=abs(ERROR_SSC_Elasticity_ECM),
                                                                SQ_ERROR_SSC_Elasticity_ECM=abs(ERROR_SSC_Elasticity_ECM)^2,                    
                                                                # ARDL-Elasticity
                                                                CALCULATION_SSC_Elasticity_ARDL=1+(FINAL_ELASTICITY_COEFFICIENTS[7,7]/100*Nominal_GDP_GROWTH),
                                                                FORECASTING_SSC_Elasticity_ARDL=lag(Adjusted_real_SSC)*CALCULATION_SSC_Elasticity_ARDL,
                                                                ERROR_SSC_Elasticity_ARDL=(Adjusted_real_SSC-FORECASTING_SSC_Elasticity_ARDL)/Adjusted_real_SSC,
                                                                ABSOULTE_ERROR_SSC_Elasticity_ARDL=abs(ERROR_SSC_Elasticity_ARDL),
                                                                SQ_ERROR_SSC_Elasticity_ARDL=abs(ERROR_SSC_Elasticity_ARDL)^2,
                                              # TAX
                                                                # OLS-Buoyancy
                                                                CALCULATION_TAX_Buoyancy=1+(FINAL_ELASTICITY_COEFFICIENTS[8,1]/100*Nominal_GDP_GROWTH),
                                                                FORECASTING_TAX_Buoyancy=lag(Adjusted_real_TAX)*CALCULATION_TAX_Buoyancy,
                                                                ERROR_TAX_Buoyancy=(Adjusted_real_TAX-FORECASTING_TAX_Buoyancy)/Adjusted_real_TAX,
                                                                ABSOULTE_ERROR_TAX_Buoyancy=abs(ERROR_TAX_Buoyancy),
                                                                SQ_ERROR_TAX_Buoyancy=abs(ERROR_TAX_Buoyancy)^2,
                                                                # OLS-Elasticity
                                                                CALCULATION_TAX_Elasticity_OLS=1+(FINAL_ELASTICITY_COEFFICIENTS[8,2]/100*Nominal_GDP_GROWTH),
                                                                FORECASTING_TAX_Elasticity_OLS=lag(Adjusted_real_TAX)*CALCULATION_TAX_Elasticity_OLS,
                                                                ERROR_TAX_Elasticity_OLS=(Adjusted_real_TAX-FORECASTING_TAX_Elasticity_OLS)/Adjusted_real_TAX,
                                                                ABSOULTE_ERROR_TAX_Elasticity_OLS=abs(ERROR_TAX_Elasticity_OLS),
                                                                SQ_ERROR_TAX_Elasticity_OLS=abs(ERROR_TAX_Elasticity_OLS)^2,
                                                                # ECM-Elasticity
                                                                CALCULATION_TAX_Elasticity_ECM=1+(FINAL_ELASTICITY_COEFFICIENTS[8,4]/100*Nominal_GDP_GROWTH),
                                                                FORECASTING_TAX_Elasticity_ECM=lag(Adjusted_real_TAX)*CALCULATION_TAX_Elasticity_ECM,
                                                                ERROR_TAX_Elasticity_ECM=(Adjusted_real_TAX-FORECASTING_TAX_Elasticity_ECM)/Adjusted_real_TAX,
                                                                ABSOULTE_ERROR_TAX_Elasticity_ECM=abs(ERROR_TAX_Elasticity_ECM),
                                                                SQ_ERROR_TAX_Elasticity_ECM=abs(ERROR_TAX_Elasticity_ECM)^2,                    
                                                                # ARDL-Elasticity
                                                                CALCULATION_TAX_Elasticity_ARDL=1+(FINAL_ELASTICITY_COEFFICIENTS[8,7]/100*Nominal_GDP_GROWTH),
                                                                FORECASTING_TAX_Elasticity_ARDL=lag(Adjusted_real_TAX)*CALCULATION_TAX_Elasticity_ARDL,
                                                                ERROR_TAX_Elasticity_ARDL=(Adjusted_real_TAX-FORECASTING_TAX_Elasticity_ARDL)/Adjusted_real_TAX,
                                                                ABSOULTE_ERROR_TAX_Elasticity_ARDL=abs(ERROR_TAX_Elasticity_ARDL),
                                                                SQ_ERROR_TAX_Elasticity_ARDL=abs(ERROR_TAX_Elasticity_ARDL)^2,
                                                # TAX_SSC
                                                                # OLS-Buoyancy
                                                                CALCULATION_TAX_SSC_Buoyancy=1+(FINAL_ELASTICITY_COEFFICIENTS[9,1]/100*Nominal_GDP_GROWTH),
                                                                FORECASTING_TAX_SSC_Buoyancy=lag(Adjusted_real_TAX_SSC)*CALCULATION_TAX_SSC_Buoyancy,
                                                                ERROR_TAX_SSC_Buoyancy=(Adjusted_real_TAX_SSC-FORECASTING_TAX_SSC_Buoyancy)/Adjusted_real_TAX_SSC,
                                                                ABSOULTE_ERROR_TAX_SSC_Buoyancy=abs(ERROR_TAX_SSC_Buoyancy),
                                                                SQ_ERROR_TAX_SSC_Buoyancy=abs(ERROR_TAX_SSC_Buoyancy)^2,
                                                                # OLS-Elasticity
                                                                CALCULATION_TAX_SSC_Elasticity_OLS=1+(FINAL_ELASTICITY_COEFFICIENTS[9,2]/100*Nominal_GDP_GROWTH),
                                                                FORECASTING_TAX_SSC_Elasticity_OLS=lag(Adjusted_real_TAX_SSC)*CALCULATION_TAX_SSC_Elasticity_OLS,
                                                                ERROR_TAX_SSC_Elasticity_OLS=(Adjusted_real_TAX_SSC-FORECASTING_TAX_SSC_Elasticity_OLS)/Adjusted_real_TAX_SSC,
                                                                ABSOULTE_ERROR_TAX_SSC_Elasticity_OLS=abs(ERROR_TAX_SSC_Elasticity_OLS),
                                                                SQ_ERROR_TAX_SSC_Elasticity_OLS=abs(ERROR_TAX_SSC_Elasticity_OLS)^2,
                                                                # ECM-Elasticity
                                                                CALCULATION_TAX_SSC_Elasticity_ECM=1+(FINAL_ELASTICITY_COEFFICIENTS[9,4]/100*Nominal_GDP_GROWTH),
                                                                FORECASTING_TAX_SSC_Elasticity_ECM=lag(Adjusted_real_TAX_SSC)*CALCULATION_TAX_SSC_Elasticity_ECM,
                                                                ERROR_TAX_SSC_Elasticity_ECM=(Adjusted_real_TAX_SSC-FORECASTING_TAX_SSC_Elasticity_ECM)/Adjusted_real_TAX_SSC,
                                                                ABSOULTE_ERROR_TAX_SSC_Elasticity_ECM=abs(ERROR_TAX_SSC_Elasticity_ECM),
                                                                SQ_ERROR_TAX_SSC_Elasticity_ECM=abs(ERROR_TAX_SSC_Elasticity_ECM)^2,                    
                                                                # ARDL-Elasticity
                                                                CALCULATION_TAX_SSC_Elasticity_ARDL=1+(FINAL_ELASTICITY_COEFFICIENTS[9,7]/100*Nominal_GDP_GROWTH),
                                                                FORECASTING_TAX_SSC_Elasticity_ARDL=lag(Adjusted_real_TAX_SSC)*CALCULATION_TAX_SSC_Elasticity_ARDL,
                                                                ERROR_TAX_SSC_Elasticity_ARDL=(Adjusted_real_TAX_SSC-FORECASTING_TAX_SSC_Elasticity_ARDL)/Adjusted_real_TAX_SSC,
                                                                ABSOULTE_ERROR_TAX_SSC_Elasticity_ARDL=abs(ERROR_TAX_SSC_Elasticity_ARDL),
                                                                SQ_ERROR_TAX_SSC_Elasticity_ARDL=abs(ERROR_TAX_SSC_Elasticity_ARDL)^2,
                                                # NON_TAX_REVENUES
                                                                # OLS-Buoyancy
                                                                CALCULATION_NON_TAX_REVENUES_Buoyancy=1+(FINAL_ELASTICITY_COEFFICIENTS[10,1]/100*Nominal_GDP_GROWTH),
                                                                FORECASTING_NON_TAX_REVENUES_Buoyancy=lag(Adjusted_real_NON_TAX_REVENUES)*CALCULATION_NON_TAX_REVENUES_Buoyancy,
                                                                ERROR_NON_TAX_REVENUES_Buoyancy=(Adjusted_real_NON_TAX_REVENUES-FORECASTING_NON_TAX_REVENUES_Buoyancy)/Adjusted_real_NON_TAX_REVENUES,
                                                                ABSOULTE_ERROR_NON_TAX_REVENUES_Buoyancy=abs(ERROR_NON_TAX_REVENUES_Buoyancy),
                                                                SQ_ERROR_NON_TAX_REVENUES_Buoyancy=abs(ERROR_NON_TAX_REVENUES_Buoyancy)^2,
                                                                # OLS-Elasticity
                                                                CALCULATION_NON_TAX_REVENUES_Elasticity_OLS=1+(FINAL_ELASTICITY_COEFFICIENTS[10,2]/100*Nominal_GDP_GROWTH),
                                                                FORECASTING_NON_TAX_REVENUES_Elasticity_OLS=lag(Adjusted_real_NON_TAX_REVENUES)*CALCULATION_NON_TAX_REVENUES_Elasticity_OLS,
                                                                ERROR_NON_TAX_REVENUES_Elasticity_OLS=(Adjusted_real_NON_TAX_REVENUES-FORECASTING_NON_TAX_REVENUES_Elasticity_OLS)/Adjusted_real_NON_TAX_REVENUES,
                                                                ABSOULTE_ERROR_NON_TAX_REVENUES_Elasticity_OLS=abs(ERROR_NON_TAX_REVENUES_Elasticity_OLS),
                                                                SQ_ERROR_NON_TAX_REVENUES_Elasticity_OLS=abs(ERROR_NON_TAX_REVENUES_Elasticity_OLS)^2,
                                                                # ECM-Elasticity
                                                                CALCULATION_NON_TAX_REVENUES_Elasticity_ECM=1+(FINAL_ELASTICITY_COEFFICIENTS[10,4]/100*Nominal_GDP_GROWTH),
                                                                FORECASTING_NON_TAX_REVENUES_Elasticity_ECM=lag(Adjusted_real_NON_TAX_REVENUES)*CALCULATION_NON_TAX_REVENUES_Elasticity_ECM,
                                                                ERROR_NON_TAX_REVENUES_Elasticity_ECM=(Adjusted_real_NON_TAX_REVENUES-FORECASTING_NON_TAX_REVENUES_Elasticity_ECM)/Adjusted_real_NON_TAX_REVENUES,
                                                                ABSOULTE_ERROR_NON_TAX_REVENUES_Elasticity_ECM=abs(ERROR_NON_TAX_REVENUES_Elasticity_ECM),
                                                                SQ_ERROR_NON_TAX_REVENUES_Elasticity_ECM=abs(ERROR_NON_TAX_REVENUES_Elasticity_ECM)^2,                    
                                                                # ARDL-Elasticity
                                                                CALCULATION_NON_TAX_REVENUES_Elasticity_ARDL=1+(FINAL_ELASTICITY_COEFFICIENTS[10,7]/100*Nominal_GDP_GROWTH),
                                                                FORECASTING_NON_TAX_REVENUES_Elasticity_ARDL=lag(Adjusted_real_NON_TAX_REVENUES)*CALCULATION_NON_TAX_REVENUES_Elasticity_ARDL,
                                                                ERROR_NON_TAX_REVENUES_Elasticity_ARDL=(Adjusted_real_NON_TAX_REVENUES-FORECASTING_NON_TAX_REVENUES_Elasticity_ARDL)/Adjusted_real_NON_TAX_REVENUES,
                                                                ABSOULTE_ERROR_NON_TAX_REVENUES_Elasticity_ARDL=abs(ERROR_NON_TAX_REVENUES_Elasticity_ARDL),
                                                                SQ_ERROR_NON_TAX_REVENUES_Elasticity_ARDL=abs(ERROR_NON_TAX_REVENUES_Elasticity_ARDL)^2,
                                                # Nominal_REVENUES
                                                                # OLS-Buoyancy
                                                                CALCULATION_Nominal_REVENUES_Buoyancy=1+(FINAL_ELASTICITY_COEFFICIENTS[9,1]/100*Nominal_GDP_GROWTH),
                                                                FORECASTING_Nominal_REVENUES_Buoyancy=lag(Adjusted_real_Nominal_REVENUES)*CALCULATION_Nominal_REVENUES_Buoyancy,
                                                                ERROR_Nominal_REVENUES_Buoyancy=(Adjusted_real_Nominal_REVENUES-FORECASTING_Nominal_REVENUES_Buoyancy)/Adjusted_real_Nominal_REVENUES,
                                                                ABSOULTE_ERROR_Nominal_REVENUES_Buoyancy=abs(ERROR_Nominal_REVENUES_Buoyancy),
                                                                SQ_ERROR_Nominal_REVENUES_Buoyancy=abs(ERROR_Nominal_REVENUES_Buoyancy)^2,
                                                                # OLS-Elasticity
                                                                CALCULATION_Nominal_REVENUES_Elasticity_OLS=1+(FINAL_ELASTICITY_COEFFICIENTS[9,2]/100*Nominal_GDP_GROWTH),
                                                                FORECASTING_Nominal_REVENUES_Elasticity_OLS=lag(Adjusted_real_Nominal_REVENUES)*CALCULATION_Nominal_REVENUES_Elasticity_OLS,
                                                                ERROR_Nominal_REVENUES_Elasticity_OLS=(Adjusted_real_Nominal_REVENUES-FORECASTING_Nominal_REVENUES_Elasticity_OLS)/Adjusted_real_Nominal_REVENUES,
                                                                ABSOULTE_ERROR_Nominal_REVENUES_Elasticity_OLS=abs(ERROR_Nominal_REVENUES_Elasticity_OLS),
                                                                SQ_ERROR_Nominal_REVENUES_Elasticity_OLS=abs(ERROR_Nominal_REVENUES_Elasticity_OLS)^2,
                                                                # ECM-Elasticity
                                                                CALCULATION_Nominal_REVENUES_Elasticity_ECM=1+(FINAL_ELASTICITY_COEFFICIENTS[9,4]/100*Nominal_GDP_GROWTH),
                                                                FORECASTING_Nominal_REVENUES_Elasticity_ECM=lag(Adjusted_real_Nominal_REVENUES)*CALCULATION_Nominal_REVENUES_Elasticity_ECM,
                                                                ERROR_Nominal_REVENUES_Elasticity_ECM=(Adjusted_real_Nominal_REVENUES-FORECASTING_Nominal_REVENUES_Elasticity_ECM)/Adjusted_real_Nominal_REVENUES,
                                                                ABSOULTE_ERROR_Nominal_REVENUES_Elasticity_ECM=abs(ERROR_Nominal_REVENUES_Elasticity_ECM),
                                                                SQ_ERROR_Nominal_REVENUES_Elasticity_ECM=abs(ERROR_Nominal_REVENUES_Elasticity_ECM)^2,                    
                                                                # ARDL-Elasticity
                                                                CALCULATION_Nominal_REVENUES_Elasticity_ARDL=1+(FINAL_ELASTICITY_COEFFICIENTS[9,7]/100*Nominal_GDP_GROWTH),
                                                                FORECASTING_Nominal_REVENUES_Elasticity_ARDL=lag(Adjusted_real_Nominal_REVENUES)*CALCULATION_Nominal_REVENUES_Elasticity_ARDL,
                                                                ERROR_Nominal_REVENUES_Elasticity_ARDL=(Adjusted_real_Nominal_REVENUES-FORECASTING_Nominal_REVENUES_Elasticity_ARDL)/Adjusted_real_Nominal_REVENUES,
                                                                ABSOULTE_ERROR_Nominal_REVENUES_Elasticity_ARDL=abs(ERROR_Nominal_REVENUES_Elasticity_ARDL),
                                                                SQ_ERROR_Nominal_REVENUES_Elasticity_ARDL=abs(ERROR_Nominal_REVENUES_Elasticity_ARDL)^2)
                
                              Evaluation_forecasting<-Evaluation_forecasting %>%	
                                                                arrange((Year))
                        
                                ERROR_FORECASTING_PIT<-data.frame(
                                                              #OLS-Buoyancy
                                                              Average_error_Buoyancy=mean(Evaluation_forecasting$ERROR_PIT_Buoyancy*100,na.rm = TRUE),
                                                              Root_average_squared_error_Buoyancy=mean(Evaluation_forecasting$ABSOULTE_ERROR_PIT_Buoyancy*100,na.rm = TRUE),
                                                              Average_absolute_error_Buoyancy=(sqrt(mean(Evaluation_forecasting$SQ_ERROR_PIT_Buoyancy,na.rm = TRUE))*100),
                                                              #OLS-Elasticity
                                                              Average_error_Elasticity_OLS=mean(Evaluation_forecasting$ERROR_PIT_Elasticity_OLS*100,na.rm = TRUE),
                                                              Root_average_squared_error_Elasticity_OLS=mean(Evaluation_forecasting$ABSOULTE_ERROR_PIT_Elasticity_OLS*100,na.rm = TRUE),
                                                              Average_absolute_error_Elasticity_OLS=(sqrt(mean(Evaluation_forecasting$SQ_ERROR_PIT_Elasticity_OLS,na.rm = TRUE))*100),
                                                              # ECM-Elasticity
                                                              Average_error_Elasticity_ECM=mean(Evaluation_forecasting$ERROR_PIT_Elasticity_OLS*100,na.rm = TRUE),
                                                              Root_average_squared_error_Elasticity_ECM=mean(Evaluation_forecasting$ABSOULTE_ERROR_PIT_Elasticity_ECM*100,na.rm = TRUE),
                                                              Average_absolute_error_Elasticity_ECM=(sqrt(mean(Evaluation_forecasting$SQ_ERROR_PIT_Elasticity_ECM,na.rm = TRUE))*100),
                                                              # ARDL-Elasticity
                                                              Average_error_Elasticity_ARDL=mean(Evaluation_forecasting$ERROR_PIT_Elasticity_ARDL*100,na.rm = TRUE),
                                                              Root_average_squared_error_Elasticity_ARDL=mean(Evaluation_forecasting$ABSOULTE_ERROR_PIT_Elasticity_ARDL*100,na.rm = TRUE),
                                                              Average_absolute_error_Elasticity_ARDL=(sqrt(mean(Evaluation_forecasting$SQ_ERROR_PIT_Elasticity_ARDL,na.rm = TRUE))*100)
                                                            )
                                                            row.names(ERROR_FORECASTING_PIT)<-c("PIT")
                
                                
                                ERROR_FORECASTING_CIT<-data.frame(
                                                            #OLS-Buoyancy
                                                            Average_error_Buoyancy=mean(Evaluation_forecasting$ERROR_CIT_Buoyancy*100,na.rm = TRUE),
                                                            Root_average_squared_error_Buoyancy=mean(Evaluation_forecasting$ABSOULTE_ERROR_CIT_Buoyancy*100,na.rm = TRUE),
                                                            Average_absolute_error_Buoyancy=(sqrt(mean(Evaluation_forecasting$SQ_ERROR_CIT_Buoyancy,na.rm = TRUE))*100),
                                                            #OLS-Elasticity
                                                            Average_error_Elasticity_OLS=mean(Evaluation_forecasting$ERROR_CIT_Elasticity_OLS*100,na.rm = TRUE),
                                                            Root_average_squared_error_Elasticity_OLS=mean(Evaluation_forecasting$ABSOULTE_ERROR_CIT_Elasticity_OLS*100,na.rm = TRUE),
                                                            Average_absolute_error_Elasticity_OLS=(sqrt(mean(Evaluation_forecasting$SQ_ERROR_CIT_Elasticity_OLS,na.rm = TRUE))*100),
                                                            # ECM-Elasticity
                                                            Average_error_Elasticity_ECM=mean(Evaluation_forecasting$ERROR_CIT_Elasticity_OLS*100,na.rm = TRUE),
                                                            Root_average_squared_error_Elasticity_ECM=mean(Evaluation_forecasting$ABSOULTE_ERROR_CIT_Elasticity_ECM*100,na.rm = TRUE),
                                                            Average_absolute_error_Elasticity_ECM=(sqrt(mean(Evaluation_forecasting$SQ_ERROR_CIT_Elasticity_ECM,na.rm = TRUE))*100),
                                                            # ARDL-Elasticity
                                                            Average_error_Elasticity_ARDL=mean(Evaluation_forecasting$ERROR_CIT_Elasticity_ARDL*100,na.rm = TRUE),
                                                            Root_average_squared_error_Elasticity_ARDL=mean(Evaluation_forecasting$ABSOULTE_ERROR_CIT_Elasticity_ARDL*100,na.rm = TRUE),
                                                            Average_absolute_error_Elasticity_ARDL=(sqrt(mean(Evaluation_forecasting$SQ_ERROR_CIT_Elasticity_ARDL,na.rm = TRUE))*100)
                                                          )
                                                          row.names(ERROR_FORECASTING_CIT)<-c("CIT")
                                
                                ERROR_FORECASTING_VAT_NET<-data.frame(
                                                           #OLS-Buoyancy
                                                           Average_error_Buoyancy=mean(Evaluation_forecasting$ERROR_VAT_NET_Buoyancy*100,na.rm = TRUE),
                                                           Root_average_squared_error_Buoyancy=mean(Evaluation_forecasting$ABSOULTE_ERROR_VAT_NET_Buoyancy*100,na.rm = TRUE),
                                                           Average_absolute_error_Buoyancy=(sqrt(mean(Evaluation_forecasting$SQ_ERROR_VAT_NET_Buoyancy,na.rm = TRUE))*100),
                                                           #OLS-Elasticity
                                                           Average_error_Elasticity_OLS=mean(Evaluation_forecasting$ERROR_VAT_NET_Elasticity_OLS*100,na.rm = TRUE),
                                                           Root_average_squared_error_Elasticity_OLS=mean(Evaluation_forecasting$ABSOULTE_ERROR_VAT_NET_Elasticity_OLS*100,na.rm = TRUE),
                                                           Average_absolute_error_Elasticity_OLS=(sqrt(mean(Evaluation_forecasting$SQ_ERROR_VAT_NET_Elasticity_OLS,na.rm = TRUE))*100),
                                                           # ECM-Elasticity
                                                           Average_error_Elasticity_ECM=mean(Evaluation_forecasting$ERROR_VAT_NET_Elasticity_OLS*100,na.rm = TRUE),
                                                           Root_average_squared_error_Elasticity_ECM=mean(Evaluation_forecasting$ABSOULTE_ERROR_VAT_NET_Elasticity_ECM*100,na.rm = TRUE),
                                                           Average_absolute_error_Elasticity_ECM=(sqrt(mean(Evaluation_forecasting$SQ_ERROR_VAT_NET_Elasticity_ECM,na.rm = TRUE))*100),
                                                           # ARDL-Elasticity
                                                           Average_error_Elasticity_ARDL=mean(Evaluation_forecasting$ERROR_VAT_NET_Elasticity_ARDL*100,na.rm = TRUE),
                                                           Root_average_squared_error_Elasticity_ARDL=mean(Evaluation_forecasting$ABSOULTE_ERROR_VAT_NET_Elasticity_ARDL*100,na.rm = TRUE),
                                                           Average_absolute_error_Elasticity_ARDL=(sqrt(mean(Evaluation_forecasting$SQ_ERROR_VAT_NET_Elasticity_ARDL,na.rm = TRUE))*100)
                                                         )
                                                         row.names(ERROR_FORECASTING_VAT_NET)<-c("VAT_NET")
                                
                                  
                                ERROR_FORECASTING_VAT_GROSS<-data.frame(
                                                        #OLS-Buoyancy
                                                        Average_error_Buoyancy=mean(Evaluation_forecasting$ERROR_VAT_GROSS_Buoyancy*100,na.rm = TRUE),
                                                        Root_average_squared_error_Buoyancy=mean(Evaluation_forecasting$ABSOULTE_ERROR_VAT_GROSS_Buoyancy*100,na.rm = TRUE),
                                                        Average_absolute_error_Buoyancy=(sqrt(mean(Evaluation_forecasting$SQ_ERROR_VAT_GROSS_Buoyancy,na.rm = TRUE))*100),
                                                        #OLS-Elasticity
                                                        Average_error_Elasticity_OLS=mean(Evaluation_forecasting$ERROR_VAT_GROSS_Elasticity_OLS*100,na.rm = TRUE),
                                                        Root_average_squared_error_Elasticity_OLS=mean(Evaluation_forecasting$ABSOULTE_ERROR_VAT_GROSS_Elasticity_OLS*100,na.rm = TRUE),
                                                        Average_absolute_error_Elasticity_OLS=(sqrt(mean(Evaluation_forecasting$SQ_ERROR_VAT_GROSS_Elasticity_OLS,na.rm = TRUE))*100),
                                                        # ECM-Elasticity
                                                        Average_error_Elasticity_ECM=mean(Evaluation_forecasting$ERROR_VAT_GROSS_Elasticity_OLS*100,na.rm = TRUE),
                                                        Root_average_squared_error_Elasticity_ECM=mean(Evaluation_forecasting$ABSOULTE_ERROR_VAT_GROSS_Elasticity_ECM*100,na.rm = TRUE),
                                                        Average_absolute_error_Elasticity_ECM=(sqrt(mean(Evaluation_forecasting$SQ_ERROR_VAT_GROSS_Elasticity_ECM,na.rm = TRUE))*100),
                                                        # ARDL-Elasticity
                                                        Average_error_Elasticity_ARDL=mean(Evaluation_forecasting$ERROR_VAT_GROSS_Elasticity_ARDL*100,na.rm = TRUE),
                                                        Root_average_squared_error_Elasticity_ARDL=mean(Evaluation_forecasting$ABSOULTE_ERROR_VAT_GROSS_Elasticity_ARDL*100,na.rm = TRUE),
                                                        Average_absolute_error_Elasticity_ARDL=(sqrt(mean(Evaluation_forecasting$SQ_ERROR_VAT_GROSS_Elasticity_ARDL,na.rm = TRUE))*100)
                                                       )
                                                      row.names(ERROR_FORECASTING_VAT_GROSS)<-c("VAT_GROSS")
                                
                                  
                                  
                                ERROR_FORECASTING_EXCISE<-data.frame(
                                                      #OLS-Buoyancy
                                                      Average_error_Buoyancy=mean(Evaluation_forecasting$ERROR_EXCISE_Buoyancy*100,na.rm = TRUE),
                                                      Root_average_squared_error_Buoyancy=mean(Evaluation_forecasting$ABSOULTE_ERROR_EXCISE_Buoyancy*100,na.rm = TRUE),
                                                      Average_absolute_error_Buoyancy=(sqrt(mean(Evaluation_forecasting$SQ_ERROR_EXCISE_Buoyancy,na.rm = TRUE))*100),
                                                      #OLS-Elasticity
                                                      Average_error_Elasticity_OLS=mean(Evaluation_forecasting$ERROR_EXCISE_Elasticity_OLS*100,na.rm = TRUE),
                                                      Root_average_squared_error_Elasticity_OLS=mean(Evaluation_forecasting$ABSOULTE_ERROR_EXCISE_Elasticity_OLS*100,na.rm = TRUE),
                                                      Average_absolute_error_Elasticity_OLS=(sqrt(mean(Evaluation_forecasting$SQ_ERROR_EXCISE_Elasticity_OLS,na.rm = TRUE))*100),
                                                      # ECM-Elasticity
                                                      Average_error_Elasticity_ECM=mean(Evaluation_forecasting$ERROR_EXCISE_Elasticity_OLS*100,na.rm = TRUE),
                                                      Root_average_squared_error_Elasticity_ECM=mean(Evaluation_forecasting$ABSOULTE_ERROR_EXCISE_Elasticity_ECM*100,na.rm = TRUE),
                                                      Average_absolute_error_Elasticity_ECM=(sqrt(mean(Evaluation_forecasting$SQ_ERROR_EXCISE_Elasticity_ECM,na.rm = TRUE))*100),
                                                      # ARDL-Elasticity
                                                      Average_error_Elasticity_ARDL=mean(Evaluation_forecasting$ERROR_EXCISE_Elasticity_ARDL*100,na.rm = TRUE),
                                                      Root_average_squared_error_Elasticity_ARDL=mean(Evaluation_forecasting$ABSOULTE_ERROR_EXCISE_Elasticity_ARDL*100,na.rm = TRUE),
                                                      Average_absolute_error_Elasticity_ARDL=(sqrt(mean(Evaluation_forecasting$SQ_ERROR_EXCISE_Elasticity_ARDL,na.rm = TRUE))*100)
                                                    )
                                                    row.names(ERROR_FORECASTING_EXCISE)<-c("EXCISE")
                                
                                  
                                ERROR_FORECASTING_CUSTOMS_DUTIES<-data.frame(
                                                        #OLS-Buoyancy
                                                        Average_error_Buoyancy=mean(Evaluation_forecasting$ERROR_CUSTOMS_DUTIES_Buoyancy*100,na.rm = TRUE),
                                                        Root_average_squared_error_Buoyancy=mean(Evaluation_forecasting$ABSOULTE_ERROR_CUSTOMS_DUTIES_Buoyancy*100,na.rm = TRUE),
                                                        Average_absolute_error_Buoyancy=(sqrt(mean(Evaluation_forecasting$SQ_ERROR_CUSTOMS_DUTIES_Buoyancy,na.rm = TRUE))*100),
                                                        #OLS-Elasticity
                                                        Average_error_Elasticity_OLS=mean(Evaluation_forecasting$ERROR_CUSTOMS_DUTIES_Elasticity_OLS*100,na.rm = TRUE),
                                                        Root_average_squared_error_Elasticity_OLS=mean(Evaluation_forecasting$ABSOULTE_ERROR_CUSTOMS_DUTIES_Elasticity_OLS*100,na.rm = TRUE),
                                                        Average_absolute_error_Elasticity_OLS=(sqrt(mean(Evaluation_forecasting$SQ_ERROR_CUSTOMS_DUTIES_Elasticity_OLS,na.rm = TRUE))*100),
                                                        # ECM-Elasticity
                                                        Average_error_Elasticity_ECM=mean(Evaluation_forecasting$ERROR_CUSTOMS_DUTIES_Elasticity_OLS*100,na.rm = TRUE),
                                                        Root_average_squared_error_Elasticity_ECM=mean(Evaluation_forecasting$ABSOULTE_ERROR_CUSTOMS_DUTIES_Elasticity_ECM*100,na.rm = TRUE),
                                                        Average_absolute_error_Elasticity_ECM=(sqrt(mean(Evaluation_forecasting$SQ_ERROR_CUSTOMS_DUTIES_Elasticity_ECM,na.rm = TRUE))*100),
                                                        # ARDL-Elasticity
                                                        Average_error_Elasticity_ARDL=mean(Evaluation_forecasting$ERROR_CUSTOMS_DUTIES_Elasticity_ARDL*100,na.rm = TRUE),
                                                        Root_average_squared_error_Elasticity_ARDL=mean(Evaluation_forecasting$ABSOULTE_ERROR_CUSTOMS_DUTIES_Elasticity_ARDL*100,na.rm = TRUE),
                                                        Average_absolute_error_Elasticity_ARDL=(sqrt(mean(Evaluation_forecasting$SQ_ERROR_CUSTOMS_DUTIES_Elasticity_ARDL,na.rm = TRUE))*100)
                                                      )
                                                  row.names(ERROR_FORECASTING_CUSTOMS_DUTIES)<-c("CUSTOMS_DUTIES")
                                
                                                  
                                ERROR_FORECASTING_SSC<-data.frame(
                                                        #OLS-Buoyancy
                                                        Average_error_Buoyancy=mean(Evaluation_forecasting$ERROR_SSC_Buoyancy*100,na.rm = TRUE),
                                                        Root_average_squared_error_Buoyancy=mean(Evaluation_forecasting$ABSOULTE_ERROR_SSC_Buoyancy*100,na.rm = TRUE),
                                                        Average_absolute_error_Buoyancy=(sqrt(mean(Evaluation_forecasting$SQ_ERROR_SSC_Buoyancy,na.rm = TRUE))*100),
                                                        #OLS-Elasticity
                                                        Average_error_Elasticity_OLS=mean(Evaluation_forecasting$ERROR_SSC_Elasticity_OLS*100,na.rm = TRUE),
                                                        Root_average_squared_error_Elasticity_OLS=mean(Evaluation_forecasting$ABSOULTE_ERROR_SSC_Elasticity_OLS*100,na.rm = TRUE),
                                                        Average_absolute_error_Elasticity_OLS=(sqrt(mean(Evaluation_forecasting$SQ_ERROR_SSC_Elasticity_OLS,na.rm = TRUE))*100),
                                                        # ECM-Elasticity
                                                        Average_error_Elasticity_ECM=mean(Evaluation_forecasting$ERROR_SSC_Elasticity_OLS*100,na.rm = TRUE),
                                                        Root_average_squared_error_Elasticity_ECM=mean(Evaluation_forecasting$ABSOULTE_ERROR_SSC_Elasticity_ECM*100,na.rm = TRUE),
                                                        Average_absolute_error_Elasticity_ECM=(sqrt(mean(Evaluation_forecasting$SQ_ERROR_SSC_Elasticity_ECM,na.rm = TRUE))*100),
                                                        # ARDL-Elasticity
                                                        Average_error_Elasticity_ARDL=mean(Evaluation_forecasting$ERROR_SSC_Elasticity_ARDL*100,na.rm = TRUE),
                                                        Root_average_squared_error_Elasticity_ARDL=mean(Evaluation_forecasting$ABSOULTE_ERROR_SSC_Elasticity_ARDL*100,na.rm = TRUE),
                                                        Average_absolute_error_Elasticity_ARDL=(sqrt(mean(Evaluation_forecasting$SQ_ERROR_SSC_Elasticity_ARDL,na.rm = TRUE))*100)
                                                      )
                                                      row.names(ERROR_FORECASTING_SSC)<-c("SSC")
                                
                                
                                ERROR_FORECASTING_TAX<-data.frame(
                                                        #OLS-Buoyancy
                                                        Average_error_Buoyancy=mean(Evaluation_forecasting$ERROR_TAX_Buoyancy*100,na.rm = TRUE),
                                                        Root_average_squared_error_Buoyancy=mean(Evaluation_forecasting$ABSOULTE_ERROR_TAX_Buoyancy*100,na.rm = TRUE),
                                                        Average_absolute_error_Buoyancy=(sqrt(mean(Evaluation_forecasting$SQ_ERROR_TAX_Buoyancy,na.rm = TRUE))*100),
                                                        #OLS-Elasticity
                                                        Average_error_Elasticity_OLS=mean(Evaluation_forecasting$ERROR_TAX_Elasticity_OLS*100,na.rm = TRUE),
                                                        Root_average_squared_error_Elasticity_OLS=mean(Evaluation_forecasting$ABSOULTE_ERROR_TAX_Elasticity_OLS*100,na.rm = TRUE),
                                                        Average_absolute_error_Elasticity_OLS=(sqrt(mean(Evaluation_forecasting$SQ_ERROR_TAX_Elasticity_OLS,na.rm = TRUE))*100),
                                                        # ECM-Elasticity
                                                        Average_error_Elasticity_ECM=mean(Evaluation_forecasting$ERROR_TAX_Elasticity_OLS*100,na.rm = TRUE),
                                                        Root_average_squared_error_Elasticity_ECM=mean(Evaluation_forecasting$ABSOULTE_ERROR_TAX_Elasticity_ECM*100,na.rm = TRUE),
                                                        Average_absolute_error_Elasticity_ECM=(sqrt(mean(Evaluation_forecasting$SQ_ERROR_TAX_Elasticity_ECM,na.rm = TRUE))*100),
                                                        # ARDL-Elasticity
                                                        Average_error_Elasticity_ARDL=mean(Evaluation_forecasting$ERROR_TAX_Elasticity_ARDL*100,na.rm = TRUE),
                                                        Root_average_squared_error_Elasticity_ARDL=mean(Evaluation_forecasting$ABSOULTE_ERROR_TAX_Elasticity_ARDL*100,na.rm = TRUE),
                                                        Average_absolute_error_Elasticity_ARDL=(sqrt(mean(Evaluation_forecasting$SQ_ERROR_TAX_Elasticity_ARDL,na.rm = TRUE))*100)
                                                      )
                                                  row.names(ERROR_FORECASTING_TAX)<-c("TAX")
                                
                                
                                ERROR_FORECASTING_TAX_SSC<-data.frame(
                                                        #OLS-Buoyancy
                                                        Average_error_Buoyancy=mean(Evaluation_forecasting$ERROR_TAX_SSC_Buoyancy*100,na.rm = TRUE),
                                                        Root_average_squared_error_Buoyancy=mean(Evaluation_forecasting$ABSOULTE_ERROR_TAX_SSC_Buoyancy*100,na.rm = TRUE),
                                                        Average_absolute_error_Buoyancy=(sqrt(mean(Evaluation_forecasting$SQ_ERROR_TAX_SSC_Buoyancy,na.rm = TRUE))*100),
                                                        #OLS-Elasticity
                                                        Average_error_Elasticity_OLS=mean(Evaluation_forecasting$ERROR_TAX_SSC_Elasticity_OLS*100,na.rm = TRUE),
                                                        Root_average_squared_error_Elasticity_OLS=mean(Evaluation_forecasting$ABSOULTE_ERROR_TAX_SSC_Elasticity_OLS*100,na.rm = TRUE),
                                                        Average_absolute_error_Elasticity_OLS=(sqrt(mean(Evaluation_forecasting$SQ_ERROR_TAX_SSC_Elasticity_OLS,na.rm = TRUE))*100),
                                                        # ECM-Elasticity
                                                        Average_error_Elasticity_ECM=mean(Evaluation_forecasting$ERROR_TAX_SSC_Elasticity_OLS*100,na.rm = TRUE),
                                                        Root_average_squared_error_Elasticity_ECM=mean(Evaluation_forecasting$ABSOULTE_ERROR_TAX_SSC_Elasticity_ECM*100,na.rm = TRUE),
                                                        Average_absolute_error_Elasticity_ECM=(sqrt(mean(Evaluation_forecasting$SQ_ERROR_TAX_SSC_Elasticity_ECM,na.rm = TRUE))*100),
                                                        # ARDL-Elasticity
                                                        Average_error_Elasticity_ARDL=mean(Evaluation_forecasting$ERROR_TAX_SSC_Elasticity_ARDL*100,na.rm = TRUE),
                                                        Root_average_squared_error_Elasticity_ARDL=mean(Evaluation_forecasting$ABSOULTE_ERROR_TAX_SSC_Elasticity_ARDL*100,na.rm = TRUE),
                                                        Average_absolute_error_Elasticity_ARDL=(sqrt(mean(Evaluation_forecasting$SQ_ERROR_TAX_SSC_Elasticity_ARDL,na.rm = TRUE))*100)
                                                      )
                                                  row.names(ERROR_FORECASTING_TAX_SSC)<-c("TAX_SSC")
                                
                                
                                ERROR_FORECASTING_NON_TAX_REVENUES<-data.frame(
                                                        #NON_TAX_REVENUES
                                                        #OLS-Buoyancy
                                                        Average_error_Buoyancy=mean(Evaluation_forecasting$ERROR_NON_TAX_REVENUES_Buoyancy*100,na.rm = TRUE),
                                                        Root_average_squared_error_Buoyancy=mean(Evaluation_forecasting$ABSOULTE_ERROR_NON_TAX_REVENUES_Buoyancy*100,na.rm = TRUE),
                                                        Average_absolute_error_Buoyancy=(sqrt(mean(Evaluation_forecasting$SQ_ERROR_NON_TAX_REVENUES_Buoyancy,na.rm = TRUE))*100),
                                                        #OLS-Elasticity
                                                        Average_error_Elasticity_OLS=mean(Evaluation_forecasting$ERROR_NON_TAX_REVENUES_Elasticity_OLS*100,na.rm = TRUE),
                                                        Root_average_squared_error_Elasticity_OLS=mean(Evaluation_forecasting$ABSOULTE_ERROR_NON_TAX_REVENUES_Elasticity_OLS*100,na.rm = TRUE),
                                                        Average_absolute_error_Elasticity_OLS=(sqrt(mean(Evaluation_forecasting$SQ_ERROR_NON_TAX_REVENUES_Elasticity_OLS,na.rm = TRUE))*100),
                                                        # ECM-Elasticity
                                                        Average_error_Elasticity_ECM=mean(Evaluation_forecasting$ERROR_NON_TAX_REVENUES_Elasticity_OLS*100,na.rm = TRUE),
                                                        Root_average_squared_error_Elasticity_ECM=mean(Evaluation_forecasting$ABSOULTE_ERROR_NON_TAX_REVENUES_Elasticity_ECM*100,na.rm = TRUE),
                                                        Average_absolute_error_Elasticity_ECM=(sqrt(mean(Evaluation_forecasting$SQ_ERROR_NON_TAX_REVENUES_Elasticity_ECM,na.rm = TRUE))*100),
                                                        # ARDL-Elasticity
                                                        Average_error_Elasticity_ARDL=mean(Evaluation_forecasting$ERROR_NON_TAX_REVENUES_Elasticity_ARDL*100,na.rm = TRUE),
                                                        Root_average_squared_error_Elasticity_ARDL=mean(Evaluation_forecasting$ABSOULTE_ERROR_NON_TAX_REVENUES_Elasticity_ARDL*100,na.rm = TRUE),
                                                        Average_absolute_error_Elasticity_ARDL=(sqrt(mean(Evaluation_forecasting$SQ_ERROR_NON_TAX_REVENUES_Elasticity_ARDL,na.rm = TRUE))*100)
                                                      )
                                                      row.names(ERROR_FORECASTING_NON_TAX_REVENUES)<-c("NON_TAX_REVENUES")
                                
                                
                                ERROR_FORECASTING_Nominal_REVENUES<-data.frame(
                                                                        #OLS-Buoyancy
                                                        Average_error_Buoyancy=mean(Evaluation_forecasting$ERROR_Nominal_REVENUES_Buoyancy*100,na.rm = TRUE),
                                                        Root_average_squared_error_Buoyancy=mean(Evaluation_forecasting$ABSOULTE_ERROR_Nominal_REVENUES_Buoyancy*100,na.rm = TRUE),
                                                        Average_absolute_error_Buoyancy=(sqrt(mean(Evaluation_forecasting$SQ_ERROR_Nominal_REVENUES_Buoyancy,na.rm = TRUE))*100),
                                                        #OLS-Elasticity
                                                        Average_error_Elasticity_OLS=mean(Evaluation_forecasting$ERROR_Nominal_REVENUES_Elasticity_OLS*100,na.rm = TRUE),
                                                        Root_average_squared_error_Elasticity_OLS=mean(Evaluation_forecasting$ABSOULTE_ERROR_Nominal_REVENUES_Elasticity_OLS*100,na.rm = TRUE),
                                                        Average_absolute_error_Elasticity_OLS=(sqrt(mean(Evaluation_forecasting$SQ_ERROR_Nominal_REVENUES_Elasticity_OLS,na.rm = TRUE))*100),
                                                        # ECM-Elasticity
                                                        Average_error_Elasticity_ECM=mean(Evaluation_forecasting$ERROR_Nominal_REVENUES_Elasticity_OLS*100,na.rm = TRUE),
                                                        Root_average_squared_error_Elasticity_ECM=mean(Evaluation_forecasting$ABSOULTE_ERROR_Nominal_REVENUES_Elasticity_ECM*100,na.rm = TRUE),
                                                        Average_absolute_error_Elasticity_ECM=(sqrt(mean(Evaluation_forecasting$SQ_ERROR_Nominal_REVENUES_Elasticity_ECM,na.rm = TRUE))*100),
                                                        # ARDL-Elasticity
                                                        Average_error_Elasticity_ARDL=mean(Evaluation_forecasting$ERROR_Nominal_REVENUES_Elasticity_ARDL*100,na.rm = TRUE),
                                                        Root_average_squared_error_Elasticity_ARDL=mean(Evaluation_forecasting$ABSOULTE_ERROR_Nominal_REVENUES_Elasticity_ARDL*100,na.rm = TRUE),
                                                        Average_absolute_error_Elasticity_ARDL=(sqrt(mean(Evaluation_forecasting$SQ_ERROR_Nominal_REVENUES_Elasticity_ARDL,na.rm = TRUE))*100)
                                                      )
                                row.names(ERROR_FORECASTING_Nominal_REVENUES)<-c("Nominal_REVENUES")
                                
                                
                                # Summary table for evaluation of errors
                                                      FINAL_ERRORS<-rbind(ERROR_FORECASTING_PIT,ERROR_FORECASTING_CIT,ERROR_FORECASTING_VAT_NET,ERROR_FORECASTING_VAT_GROSS,ERROR_FORECASTING_EXCISE,
                                                                          ERROR_FORECASTING_CUSTOMS_DUTIES,ERROR_FORECASTING_SSC,ERROR_FORECASTING_TAX,ERROR_FORECASTING_TAX_SSC,ERROR_FORECASTING_NON_TAX_REVENUES,ERROR_FORECASTING_Nominal_REVENUES)
                                    
                                                      
                
                # 6.Forecasting ---------------------------------------------------
                                              
                                                      
                            MACRO_FISCAL_INDICATORS_BASE<-MACRO_FISCAL_INDICATORS%>%   
                                   dplyr::filter(between(Year,2000,INPUT_FORECASTING_YEAR))
                
                                 FINAL_FORECASTING0<-data.frame(select(MACRO_FISCAL_INDICATORS_BASE,Year,Nominal_GDP,Nominal_PIT,Nominal_CIT,Nominal_VAT_NET,Nominal_VAT_GROSS,Nominal_EXCISE,Nominal_CUSTOMS_DUTIES,Nominal_SSC,Nominal_TAX,Nominal_TAX_SSC,Nominal_NON_TAX_REVENUES,Nominal_REVENUES))%>%
                                   arrange(desc(Year))
                
                                 FINAL_FORECASTING1<-mutate(FINAL_FORECASTING0,
                                                            Nominal_GDP_GROWTH=fun1ab(Nominal_GDP)
                                                            ) %>%
                                                            arrange((Year))
                                
                                FINAL_FORECASTING2<-mutate(FINAL_FORECASTING1,
                                                                CALCULATION_PIT=1+(FINAL_ELASTICITY_COEFFICIENTS[1,1]/100*Nominal_GDP_GROWTH),
                                                                CALCULATION_CIT=1+(FINAL_ELASTICITY_COEFFICIENTS[2,2]/100*Nominal_GDP_GROWTH),##<--Estimation with elasticity
                                                                CALCULATION_VAT_NET=1+(FINAL_ELASTICITY_COEFFICIENTS[3,1]/100*Nominal_GDP_GROWTH),
                                                                CALCULATION_VAT_GROSS=1+(FINAL_ELASTICITY_COEFFICIENTS[4,1]/100*Nominal_GDP_GROWTH),
                                                                CALCULATION_EXCISE=1+(FINAL_ELASTICITY_COEFFICIENTS[5,1]/100*Nominal_GDP_GROWTH),
                                                                CALCULATION_CUSTOMS_DUTIES=1+(FINAL_ELASTICITY_COEFFICIENTS[6,2]/100*Nominal_GDP_GROWTH),##<---Estimation with elasticity
                                                                CALCULATION_SSC=1+(FINAL_ELASTICITY_COEFFICIENTS[7,1]/100*Nominal_GDP_GROWTH),
                                                                CALCULATION_TAX=1+(FINAL_ELASTICITY_COEFFICIENTS[8,1]/100*Nominal_GDP_GROWTH),
                                                                CALCULATION_TAX_SSC=1+(FINAL_ELASTICITY_COEFFICIENTS[9,1]/100*Nominal_GDP_GROWTH),
                                                                CALCULATION_NON_TAX_REVENUES=1+(FINAL_ELASTICITY_COEFFICIENTS[10,2]/100*Nominal_GDP_GROWTH),##<---Estimation with elasticity
                                                                CALCULATION_Nominal_REVENUES=1+(FINAL_ELASTICITY_COEFFICIENTS[11,1]/100*Nominal_GDP_GROWTH))
                    
                                                
                              FINAL_FORECASTING2<-inner_join(FINAL_FORECASTING2,DS_NEXT_YEARS, by = "Year")
                
                                    
                                                   
                                      # PIT
                                                for (i in 1:length(which(is.na(FINAL_FORECASTING2$Nominal_PIT)))){
                                                                FINAL_FORECASTING2=FINAL_FORECASTING2%>%mutate(Nominal_PIT=if_else(is.na(Nominal_PIT), (CALCULATION_PIT*lag(Nominal_PIT)+ DS_PIT),Nominal_PIT))
                                                }
                                      # CIT
                                                for (i in 1:length(which(is.na(FINAL_FORECASTING2$Nominal_CIT)))){
                                                  FINAL_FORECASTING2=FINAL_FORECASTING2%>%mutate(Nominal_CIT=if_else(is.na(Nominal_CIT),(CALCULATION_CIT*lag(Nominal_CIT)+ DS_CIT),Nominal_CIT))
                                                }
                                      # VAT_NET
                                                for (i in 1:length(which(is.na(FINAL_FORECASTING2$Nominal_VAT_NET)))){
                                                  FINAL_FORECASTING2=FINAL_FORECASTING2%>%mutate(Nominal_VAT_NET=if_else(is.na(Nominal_VAT_NET),(CALCULATION_VAT_NET*lag(Nominal_VAT_NET)+ DS_VAT_NET),Nominal_VAT_NET))
                                                }
                                      # VAT_GROSS 
                                                for (i in 1:length(which(is.na(FINAL_FORECASTING2$Nominal_VAT_GROSS)))){
                                                  FINAL_FORECASTING2=FINAL_FORECASTING2%>%mutate(Nominal_VAT_GROSS=if_else(is.na(Nominal_VAT_GROSS),(CALCULATION_VAT_GROSS*lag(Nominal_VAT_GROSS)+ DS_VAT_GROSS),Nominal_VAT_GROSS))
                                                }
                                      # EXCISE
                                                for (i in 1:length(which(is.na(FINAL_FORECASTING2$Nominal_EXCISE)))){
                                                  FINAL_FORECASTING2=FINAL_FORECASTING2%>%mutate(Nominal_EXCISE=if_else(is.na(Nominal_EXCISE), (CALCULATION_EXCISE*lag(Nominal_EXCISE)+ DS_EXCISE),Nominal_EXCISE))
                                                }
                
                                     # CUSTOMS_DUTIES
                                                for (i in 1:length(which(is.na(FINAL_FORECASTING2$Nominal_CUSTOMS_DUTIES)))){
                                                  FINAL_FORECASTING2=FINAL_FORECASTING2%>%mutate(Nominal_CUSTOMS_DUTIES=if_else(is.na(Nominal_CUSTOMS_DUTIES),(CALCULATION_CUSTOMS_DUTIES*lag(Nominal_CUSTOMS_DUTIES)+ DS_CUSTOMS_DUTIES) ,Nominal_CUSTOMS_DUTIES))
                                                }
                
                                      # SSC
                                                for (i in 1:length(which(is.na(FINAL_FORECASTING2$Nominal_SSC)))){
                                                  FINAL_FORECASTING2=FINAL_FORECASTING2%>%mutate(Nominal_SSC=if_else(is.na(Nominal_SSC), (CALCULATION_SSC*lag(Nominal_SSC)+ DS_SSC),Nominal_SSC))
                                                }
                                      
                                      # TAX
                                                for (i in 1:length(which(is.na(FINAL_FORECASTING2$Nominal_TAX)))){
                                                  FINAL_FORECASTING2=FINAL_FORECASTING2%>%mutate(Nominal_TAX=if_else(is.na(Nominal_TAX), (CALCULATION_TAX*lag(Nominal_TAX)+ DS_TAX),Nominal_TAX))
                                                }
                
                                      # TAX_SSC
                                                for (i in 1:length(which(is.na(FINAL_FORECASTING2$Nominal_TAX_SSC)))){
                                                  FINAL_FORECASTING2=FINAL_FORECASTING2%>%mutate(Nominal_TAX_SSC=if_else(is.na(Nominal_TAX_SSC),(CALCULATION_TAX_SSC*lag(Nominal_TAX_SSC)+ DS_TAX_SSC),Nominal_TAX_SSC))
                                                }
                
                                      # NON_TAX_REVENUES
                                                for (i in 1:length(which(is.na(FINAL_FORECASTING2$Nominal_NON_TAX_REVENUES)))){
                                                  FINAL_FORECASTING2=FINAL_FORECASTING2%>%mutate(Nominal_NON_TAX_REVENUES=if_else(is.na(Nominal_NON_TAX_REVENUES), (CALCULATION_NON_TAX_REVENUES*lag(Nominal_NON_TAX_REVENUES)+ DS_NON_TAX_REVENUES),Nominal_NON_TAX_REVENUES))
                                                }
                
                                      # Nominal_REVENUES
                                                for (i in 1:length(which(is.na(FINAL_FORECASTING2$Nominal_REVENUES)))){
                                                  FINAL_FORECASTING2=FINAL_FORECASTING2%>%mutate(Nominal_REVENUES=if_else(is.na(Nominal_REVENUES),(CALCULATION_Nominal_REVENUES*lag(Nominal_REVENUES)+ DS_REVENUES),Nominal_REVENUES))
                                                }
                           
                                     # FINAL FORECASTING    
                                             FINAL_FORECASTING<-data.frame(select(FINAL_FORECASTING2,Year,Nominal_GDP,Nominal_PIT,Nominal_CIT,Nominal_VAT_NET,Nominal_VAT_GROSS,Nominal_EXCISE,Nominal_CUSTOMS_DUTIES,Nominal_SSC,Nominal_TAX,Nominal_TAX_SSC,Nominal_NON_TAX_REVENUES,Nominal_REVENUES))
                             
                                    
                                      FINAL_FORECASTING<-FINAL_FORECASTING%>%
                                                  dplyr:: mutate(Nominal_EXCISE_GROSS= Nominal_EXCISE* 1.141334764)%>%
                                                   dplyr:: rename(c(
                                                    "Year"="Year",
                                                    "Nominal_GDP"="Nominal_GDP",
                                                    "Nominal_PIT"="Nominal_PIT",
                                                    "Nominal_CIT"="Nominal_CIT",	
                                                    "Nominal_VAT_NET"	="Nominal_VAT_NET",
                                                   "Nominal_VAT_GROSS"="Nominal_VAT_GROSS",
                                                    "Nominal_EXCISE"="Nominal_EXCISE",	
                                                    "Nominal_CUSTOMS_DUTIES"="Nominal_CUSTOMS_DUTIES",
                                                    "Nominal_SSC"="Nominal_SSC"	,
                                                   "Nominal_EXCISE_GROSS"="Nominal_EXCISE_GROSS",
                                                    "Nominal_TAX_REVENUES"="Nominal_TAX",	
                                                    "Nominal_TAX_SSC"="Nominal_TAX_SSC",
                                                    "Nominal_NON_TAX_REVENUES"="Nominal_NON_TAX_REVENUES",
                                                    "Nominal_REVENUES"="Nominal_REVENUES"))%>%
                                                    select("Year","Nominal_GDP","Nominal_PIT","Nominal_CIT","Nominal_VAT_NET", "Nominal_VAT_GROSS","Nominal_EXCISE",
                                                      "Nominal_CUSTOMS_DUTIES","Nominal_SSC","Nominal_EXCISE_GROSS","Nominal_TAX_REVENUES","Nominal_TAX_SSC",
                                                      "Nominal_NON_TAX_REVENUES","Nominal_REVENUES"
                                                    )
                                
                                                    
                                                View(FINAL_FORECASTING)
                
                                          


#  Module (II.) (Correction of error)--------------------------------

                                                
                # 1.Input data (data from three tables) for simulation ----------------------------------------------------
                 
                  # FORECASTING_YEAR<- as.numeric(2020) 
                  FORECASTING_YEAR<-INPUT_FORECASTING_YEAR # Add 11/02/2021
                
                
            # Determining the structure of revenues so that it can be applied in the coming years
                  INPUT_REVENUE <- read_excel("INPUT-DATA/MACRO_FISCAL_INDICATORS/MACRO_FISCAL_INDICATORS.xlsx", 
                                                      sheet = "INPUT_REVENUE") # <----This series must contain annually data.Amounts are in millions 

                
       
                  FORECASTING_NEXT_YEARS<-FINAL_FORECASTING  # Adding projection from Module 1 in order to making adjustment with revenue collection form current year
                  
                  
                  Revenues_D <- read_excel("INPUT-DATA/REVENUE REALIZATION/DATA/UPDATE_DATA_forTaxDept.xlsx",
                                         sheet = "FINAL_R", col_types = c("numeric",
                                                                          "date", "numeric", "numeric", "numeric",
                                                                          "numeric", "numeric", "numeric",
                                                                          "numeric", "numeric", "numeric",
                                                                          "numeric", "numeric", "numeric",
                                                                          "numeric", "numeric", "numeric",
                                                                          "numeric", "numeric", "numeric",
                                                                          "numeric", "numeric", "numeric",
                                                                          "numeric", "numeric"))

                
                
                # Input of realization and evaluation of ERROR
                 REVENUE_REALIZATION <- read_excel("INPUT-DATA/MACRO_FISCAL_INDICATORS/MACRO_FISCAL_INDICATORS.xlsx", 
                                                      sheet = "REVENUE_REALISATION_CURRENT")

                # Official budget projection
                BUDGET_PROJECTIONS <- read_excel("INPUT-DATA/MACRO_FISCAL_INDICATORS/MACRO_FISCAL_INDICATORS.xlsx", 
                                                      sheet = "BUDGET_PROJECTIONS")
                
                
                
    # II. Estimation of             
                
                
                # 2.Estimation of tax collection by days -------------------------------------
                
                # Structure of collected taxes
                REVENUE_STRUCTURE_MONTLY<-select(INPUT_REVENUE,M,Y,PIT,CIT,SSC,VAT_NET,VAT_GROSS,CUS,EXCISE_GROSS,EXCISE_NET1,EXCISE_NET2,O_TAX,NON_TAX_REVENUES,TAX_REVENUES,TAX_SSC,REVENUES )%>%
                  dplyr:: filter(Y %in% c("2019","2017","2016"))%>%  # <=== The range of years must be changed at each new screening of the year !
                  dplyr:: group_by(M,Y)%>%
                  dplyr:: summarize(PIT=sum(PIT),CIT=sum(CIT),SSC=sum(SSC),VAT_NET=sum(VAT_NET),VAT_GROSS=sum(VAT_GROSS),CUS=sum(CUS),EXCISE_GROSS=sum(EXCISE_GROSS),EXCISE_NET1=sum(EXCISE_NET1),EXCISE_NET2=sum(EXCISE_NET2),O_TAX=sum(O_TAX),NON_TAX_REVENUES=sum(NON_TAX_REVENUES),TAX_REVENUES=sum(TAX_REVENUES),TAX_SSC=sum(TAX_SSC),REVENUES=sum(REVENUES)) %>%
                  dplyr:: arrange(Y)
                        
                REVENUE_STRUCTURE_ANNUAL<-select(INPUT_REVENUE,M,Y,PIT,CIT,SSC,VAT_NET,VAT_GROSS,CUS,EXCISE_GROSS,EXCISE_NET1,EXCISE_NET2,O_TAX,NON_TAX_REVENUES,TAX_REVENUES,TAX_SSC,REVENUES)%>%
                  dplyr:: group_by(Y)%>%
                  dplyr:: summarize(PIT=sum(PIT),CIT=sum(CIT),SSC=sum(SSC),VAT_NET=sum(VAT_NET),VAT_GROSS=sum(VAT_GROSS),CUS=sum(CUS),EXCISE_GROSS=sum(EXCISE_GROSS),EXCISE_NET1=sum(EXCISE_NET1),EXCISE_NET2=sum(EXCISE_NET2),O_TAX=sum(O_TAX),NON_TAX_REVENUES=sum(NON_TAX_REVENUES),TAX_REVENUES=sum(TAX_REVENUES),TAX_SSC=sum(TAX_SSC),REVENUES=sum(REVENUES)) %>%
                  dplyr:: arrange(Y)
                        
                 REVENUE_STRUCTURE_FINAL0<-left_join(REVENUE_STRUCTURE_MONTLY,REVENUE_STRUCTURE_ANNUAL,by = c("Y"))%>%
                  dplyr:: mutate(
                          STRUCTURE_PIT=(PIT.x/PIT.y)*100,
                          STRUCTURE_CIT=(CIT.x/CIT.y)*100,
                          STRUCTURE_SSC=(SSC.x/SSC.y)*100,
                          STRUCTURE_VAT_NET=(VAT_NET.x/VAT_NET.y)*100,
                          STRUCTURE_VAT_GROSS=(VAT_GROSS.x/VAT_GROSS.y)*100,
                          STRUCTURE_CUS=(CUS.x/CUS.y)*100,
                          STRUCTURE_EXCISE_GROSS=(EXCISE_GROSS.x/EXCISE_GROSS.y)*100,
                          STRUCTURE_EXCISE_NET1=(EXCISE_NET1.x/EXCISE_NET1.y)*100,
                          STRUCTURE_EXCISE_NET2=(EXCISE_NET2.x/EXCISE_NET2.y)*100,
                          STRUCTURE_O_TAX=(O_TAX.x/O_TAX.y)*100,
                          STRUCTURE_NON_TAX_REVENUES=(NON_TAX_REVENUES.x/NON_TAX_REVENUES.y)*100,
                          STRUCTURE_TAX_REVENUES=(TAX_REVENUES.x/TAX_REVENUES.y)*100,
                          STRUCTURE_TAX_SSC=(TAX_SSC.x/TAX_SSC.y)*100,
                          STRUCTURE_REVENUES=(REVENUES.x/REVENUES.y)*100
                         )%>%
                data.frame()

       
                REVENUE_STRUCTURE_FINAL1<-REVENUE_STRUCTURE_FINAL0[,23:44]
                
                
                REVENUE_STRUCTURE_FINAL2<-REVENUE_STRUCTURE_FINAL0[,1:2]
                REVENUE_STRUCTURE_FINAL3<-(cbind(REVENUE_STRUCTURE_FINAL2,REVENUE_STRUCTURE_FINAL1))
                
                
            # Estimation of structure of revenues
                REVENUE_STRUCTURE_FINAL<-REVENUE_STRUCTURE_FINAL3%>%
                  dplyr:: group_by(M)%>%
                  dplyr::summarize(PIT_S=mean(STRUCTURE_PIT),CIT_S=mean(STRUCTURE_CIT),SSC_S=mean(STRUCTURE_SSC),VAT_NET_S=mean(STRUCTURE_VAT_NET),VAT_GROSS_S=mean(STRUCTURE_VAT_GROSS),CUS_S=mean(STRUCTURE_CUS),EXCISE_GROSS_S=mean(STRUCTURE_EXCISE_GROSS),EXCISE_NET1_S=mean(STRUCTURE_EXCISE_NET1),EXCISE_NET2_S=mean(STRUCTURE_EXCISE_NET2),O_TAX_S=mean(STRUCTURE_O_TAX),NON_TAX_REVENUES_S=mean(STRUCTURE_NON_TAX_REVENUES),TAX_REVENUES_S=mean(STRUCTURE_TAX_REVENUES),TAX_SSC_S =mean(STRUCTURE_TAX_SSC),REVENUES_S=mean(STRUCTURE_REVENUES))%>%
                  dplyr::arrange(M)
                
                rm(REVENUE_STRUCTURE_FINAL1,REVENUE_STRUCTURE_FINAL2,REVENUE_STRUCTURE_FINAL0,REVENUE_STRUCTURE_MONTLY,REVENUE_STRUCTURE_ANNUAL,REVENUE_STRUCTURE_FINAL3)

                # 3.Distribution of the projection by months -------------------------------------
                ANNUAL_PROJECTION<-select(FORECASTING_NEXT_YEARS,Year,Nominal_PIT,Nominal_CIT,Nominal_SSC,Nominal_VAT_NET,Nominal_VAT_GROSS,Nominal_EXCISE,Nominal_CUSTOMS_DUTIES,Nominal_EXCISE_GROSS,Nominal_TAX_REVENUES,Nominal_TAX_SSC,Nominal_NON_TAX_REVENUES,Nominal_REVENUES)%>%
                  dplyr::filter(Year==FORECASTING_YEAR)
                colnames(ANNUAL_PROJECTION)<-c("Y","PIT","CIT","SSC","VAT_NET","VAT_GROSS","EXCISE_NET","CUSTOMS_DUTIES","EXCISE_GROSS","TAX_REVENUES","TAX_SSC","NON_TAX_REVENUES","REVENUES")

                #  Revenue structure
                MONTLY_FORECAST<-mutate(REVENUE_STRUCTURE_FINAL,
                                        PIT_MONTLY=PIT_S*ANNUAL_PROJECTION$PIT/100,
                                        CIT_MONTLY=CIT_S*ANNUAL_PROJECTION$CIT/100,
                                        SSC_MONTLY=SSC_S*ANNUAL_PROJECTION$SSC/100,
                                        VAT_NET_MONTLY=VAT_NET_S*ANNUAL_PROJECTION$VAT_NET/100,
                                        VAT_GROSS_MONTLY=VAT_GROSS_S*ANNUAL_PROJECTION$VAT_GROSS/100,
                                        EXCISE_MONTLY=EXCISE_NET1_S*ANNUAL_PROJECTION$EXCISE_NET/100,
                                        CUS_MONTLY=CUS_S*ANNUAL_PROJECTION$CUSTOMS_DUTIES/100,
                                        EXCISE_MONTLY_GROSS=EXCISE_GROSS_S*ANNUAL_PROJECTION$EXCISE_GROSS/100,
                                        TAX_REVENUES_MONTLY=TAX_REVENUES_S*ANNUAL_PROJECTION$TAX_REVENUES/100,
                                        NON_TAX_REVENUES_MONTLY=NON_TAX_REVENUES_S*ANNUAL_PROJECTION$NON_TAX_REVENUES/100,
                                        TAX_SSC_MONTLY=TAX_SSC_S*ANNUAL_PROJECTION$TAX_SSC/100,
                                        REVENUES_MONTLY=REVENUES_S*ANNUAL_PROJECTION$REVENUES/100
                                         )
        
                MONTLY_FORECAST1<-MONTLY_FORECAST[,12:27]
                MONTLY_FORECAST2<-MONTLY_FORECAST[,1:1]
                MONTLY_FORECAST_FINAL<-cbind(MONTLY_FORECAST2,MONTLY_FORECAST1)
                rm(MONTLY_FORECAST1,MONTLY_FORECAST2,MONTLY_FORECAST)
                
                # Making working days
                bizz_days <-  ts(rnorm(12), start = c(FORECASTING_YEAR, 1), frequency = 12) #Ovozmozuva pravanje na kalendar so rabotni denovi
                forecast::bizdays(bizz_days, FinCenter ="Zurich") # da SE PROVERI DALI E VO KONFLIKT SO NEKOJ OD DRUGIVE PAKETI????
                
                data_bizz_days<-data.frame(
                  DAYS_IN_M=forecast::bizdays(bizz_days, FinCenter = "Zurich"))
                
                # Macedonian working days in 2020. This table sould be updated with data from MTSP each year in accordance with working days
                data_bizz_days[1,1]<-20
                data_bizz_days[4,1]<-20
                data_bizz_days[8,1]<-20
                data_bizz_days[10,1]<-20
                
     
                
                # Estimation of daily averages
                DAILY_AVERAGES<-cbind(data_bizz_days,MONTLY_FORECAST_FINAL)%>%   # <----OVDE VO PRVATA KOLONA SE SODRZANI DATUMU KOI PONATAMU MOZE DA SE RAZCLENAT NA NEDELI A POTOA I NA RABOTNI DENOVI
                  dplyr::mutate(PIT_DAILY=PIT_MONTLY/DAYS_IN_M,
                         CIT_DAILY=CIT_MONTLY/DAYS_IN_M,
                         SSC_DAILY=SSC_MONTLY/DAYS_IN_M,
                         VAT_NET_DAILY=VAT_NET_MONTLY/DAYS_IN_M,
                         VAT_GROSS_DAILY=VAT_GROSS_MONTLY/DAYS_IN_M,
                         EXCISE_DAILY_NET=EXCISE_MONTLY/DAYS_IN_M,
                         CUS_DAILY=CUS_MONTLY/DAYS_IN_M,
                         EXCISE_DAILY_GROSS=EXCISE_MONTLY_GROSS/DAYS_IN_M,
                         TAX_REVENUES_DAILY=TAX_REVENUES_MONTLY/DAYS_IN_M,
                         NON_TAX_REVENUES_DAILY=NON_TAX_REVENUES_MONTLY/DAYS_IN_M,
                         TAX_SSC_DAILY=TAX_SSC_MONTLY/DAYS_IN_M,
                         REVENUES_DAILY=REVENUES_MONTLY/DAYS_IN_M
                         #EXCISE_DAILY_GROSS=EXCISE_MONTLY_GROSS/DAYS_IN_M
                         )
                #View(DAILY_AVERAGES)  #<--------------------------Ovaa tabela sodrzi podatoci koi se odnesuvaat na mesecna ditribucija plus dnevni proseci
                
 
                # 4.Distribution of projection by week --------------------------------------
                DATA_0<-select(Revenues_D,Y,date,PIT,CIT,VAT_NET,EXCISE_NET,CUS,SSC,TAX_REVENUES,EXCISE_GROSS,VAT_IMPORT,NON_TAX_REVENUES)
                DATA_0<-filter(DATA_0,Y %in% c("2019","2018","2017")) #<----------- # <=== The range of years must be changed at each new screening of the year !
               
                DATA_1a<-select(DATA_0,date)
                DATA_1a<-as.Date(DATA_1a$date) # Conversion in ts
                DATA_1b<-select(DATA_0,PIT,CIT,VAT_NET,EXCISE_NET,CUS,SSC,TAX_REVENUES,EXCISE_GROSS,VAT_IMPORT,NON_TAX_REVENUES)
                
                # Adding weeks 
                library(zoo)
                
                vals<- data.frame(DATA_1b)
                z <- zoo(vals, DATA_1a)
                
                # Determing numbers of weeks
                week_year <- function(x)format(x, '%Y.%W') #<--Sum by years and months 
                DATA_1ab<-aggregate(z, by=week_year, FUN=sum,na.rm = TRUE)
                week <- function(x)format(x, '%W')  #<-Sum by multiple years and months 
                DATA_1abc<-aggregate(z, by=week, FUN=sum,na.rm = TRUE)
                
                # Cumulative values for the table
                CUMMULATIVE_PIT<-sum(DATA_1abc$PIT,na.rm = TRUE)
                CUMMULATIVE_CIT<-sum(DATA_1abc$CIT,na.rm = TRUE)
                CUMMULATIVE_VAT_NET<-sum(DATA_1abc$VAT_NET,na.rm = TRUE)
                CUMMULATIVE_EXCISE_NET<-sum(DATA_1abc$EXCISE_NET,na.rm = TRUE)
                CUMMULATIVE_CUS<-sum(DATA_1abc$CUS,na.rm = TRUE)
                CUMMULATIVE_SSC<-sum(DATA_1abc$SSC,na.rm = TRUE)
                CUMMULATIVE_TAX_REVENUES<-sum(DATA_1abc$TAX_REVENUES,na.rm = TRUE)
                CUMMULATIVE_EXCISE_GROSS<-sum(DATA_1abc$EXCISE_GROSS,na.rm = TRUE)
                CUMMULATIVE_VAT_IMPORT<-sum(DATA_1abc$VAT_IMPORT,na.rm = TRUE)
                CUMMULATIVE_NON_TAX_REVENUES<-sum(DATA_1abc$NON_TAX_REVENUES,na.rm = TRUE)
                
  
                # Weekly structure
                TEST<-data.frame(coredata(DATA_1abc)) #<---Extract data from time-series

                
                # Percentage structure by weeks
                WEEKLY_STRUCTURE<-data.frame(
                  PIT=TEST$PIT/CUMMULATIVE_PIT*100,  #<----Extract data from time-series
                  CIT=TEST$CIT/CUMMULATIVE_CIT*100,
                  VAT_NET=TEST$VAT_NET/CUMMULATIVE_VAT_NET*100,
                  EXCISE=TEST$EXCISE_NET/CUMMULATIVE_EXCISE_NET*100,
                  CUS=TEST$CUS/CUMMULATIVE_CUS*100,
                  SSC=TEST$SSC/CUMMULATIVE_SSC*100,
                  TAX_REVENUES=TEST$TAX_REVENUES/CUMMULATIVE_TAX_REVENUES*100,
                  EXCISE_GROSS=TEST$EXCISE_GROSS/CUMMULATIVE_EXCISE_GROSS*100,
                  VAT_IMPORT=TEST$VAT_IMPORT/CUMMULATIVE_VAT_IMPORT*100,
                  NON_TAX_REVENUES=TEST$NON_TAX_REVENUES/CUMMULATIVE_NON_TAX_REVENUES*100)
                

                rm(DATA_0,DATA_1a,DATA_1b,vals,z,week_year,week,DATA_1abc,CUMMULATIVE_PIT,CUMMULATIVE_CIT,CUMMULATIVE_VAT_NET,CUMMULATIVE_EXCISE_NET,CUMMULATIVE_CUS,TEST,
                   CUMMULATIVE_EXCISE_GROSS,CUMMULATIVE_SSC,CUMMULATIVE_TAX_REVENUES)


        # Annual projections
                WEEKLY_PROJECTION<-mutate(WEEKLY_STRUCTURE,
                                          PIT_W_F=PIT*ANNUAL_PROJECTION$PIT/100,
                                          CIT_W_F=CIT*ANNUAL_PROJECTION$CIT/100,
                                          VAT_NET_W_F=VAT_NET*ANNUAL_PROJECTION$VAT_NET/100,
                                          EXCISE_NET_W_F=EXCISE*ANNUAL_PROJECTION$EXCISE_NET/100,
                                          CUS_W_F=CUS*ANNUAL_PROJECTION$CUSTOMS_DUTIES/100,
                                          SSC_W_F=SSC*ANNUAL_PROJECTION$SSC/100,
                                          TAX_REVENUES_W_F=TAX_REVENUES*ANNUAL_PROJECTION$TAX_REVENUES/100, 
                                          EXCISE_GROSS_W_F=EXCISE_GROSS*ANNUAL_PROJECTION$EXCISE_GROSS/100,
                                          NON_TAX_REVENUES_W_F=NON_TAX_REVENUES*ANNUAL_PROJECTION$NON_TAX_REVENUES/100)
                
                rm(WEEKLY_STRUCTURE)
                detach(package:zoo)
               
                # 5.Input of realization and evaluation of ERROR ----------------------------

         
               # Define the number of months for which a comparison of projected and planned is made. 
               # This is done by first counting the number of fields with NA and then subtracting from the number 12.
                Define_months<-length(which(is.na(REVENUE_REALIZATION$PIT)))
                Define_months_final<-12-Define_months
                
                # Range of months
                Range<-seq(1:Define_months_final)
                
                # Data set for comparison
                
                # Projected
                SUBSET_PROJECTION<-filter(DAILY_AVERAGES,M %in% c(Range))%>%
                  select(PIT_MONTLY,CIT_MONTLY,SSC_MONTLY,VAT_NET_MONTLY,VAT_GROSS_MONTLY,EXCISE_MONTLY,CUS_MONTLY,EXCISE_MONTLY_GROSS,TAX_REVENUES_MONTLY,NON_TAX_REVENUES_MONTLY,TAX_SSC_MONTLY,REVENUES_MONTLY)
                
                # Realized
                SUBSET_REALIZATION<-filter(REVENUE_REALIZATION,M %in% c(Range))                           
                
                # Merging data set Projected and Realized
                REVENUE_COMPARATION<-cbind(SUBSET_PROJECTION,SUBSET_REALIZATION)
                rm(SUBSET_PROJECTION,SUBSET_REALIZATION)
                
                # Evaluation of errors
                
                library(Metrics)
                
                MONTLY_ERROR<-data.frame(
                                PIT=round((((REVENUE_COMPARATION$PIT/REVENUE_COMPARATION$PIT_MONTLY)*100)-100),2),
                                CIT=round((((REVENUE_COMPARATION$CIT/REVENUE_COMPARATION$CIT_MONTLY)*100)-100),2),
                                SSC=round((((REVENUE_COMPARATION$SSC/REVENUE_COMPARATION$SSC_MONTLY)*100)-100),2),
                                VAT_NET=round((((REVENUE_COMPARATION$VAT_NET/REVENUE_COMPARATION$VAT_NET_MONTLY)*100)-100),2),
                                VAT_GROSS=round((((REVENUE_COMPARATION$VAT_GROSS/REVENUE_COMPARATION$VAT_GROSS_MONTLY)*100)-100),2),
                                EXCISE_NET=round((((REVENUE_COMPARATION$EXCISE_NET/REVENUE_COMPARATION$EXCISE_MONTLY)*100)-100),2),
                                CUS=round((((REVENUE_COMPARATION$CUS/REVENUE_COMPARATION$CUS_MONTLY)*100)-100),2),
                                EXCISE_GROSS=round((((REVENUE_COMPARATION$EXCISE_GROSS/REVENUE_COMPARATION$EXCISE_MONTLY_GROSS)*100)-100),2),
                                TAX_REVENUES_MONTLY=round((((REVENUE_COMPARATION$TAX_REVENUES/REVENUE_COMPARATION$TAX_REVENUES_MONTLY)*100)-100),2),
                                REVENUES_MONTLY=round((((REVENUE_COMPARATION$REVENUES/REVENUE_COMPARATION$REVENUES_MONTLY)*100)-100),2))
                          

                
    # Cumulative errors
                CUMULATIVE_ERROR_EVALUATION<-data.frame(
                                BIAS_PERCENT_PIT=round(percent_bias(REVENUE_COMPARATION$PIT,REVENUE_COMPARATION$PIT_MONTLY),4)*100,
                                BIAS_PERCENT_CIT=round(percent_bias(REVENUE_COMPARATION$CIT,REVENUE_COMPARATION$CIT_MONTLY),4)*100,
                                BIAS_PERCENT_SSC=round(percent_bias(REVENUE_COMPARATION$SSC,REVENUE_COMPARATION$SSC_MONTLY),4)*100,
                                BIAS_PERCENT_VAT_NET=round(percent_bias(REVENUE_COMPARATION$VAT_NET,REVENUE_COMPARATION$VAT_NET_MONTLY),4)*100,
                                BIAS_PERCENT_VAT_GROSS=round(percent_bias(REVENUE_COMPARATION$VAT_GROSS,REVENUE_COMPARATION$VAT_GROSS_MONTLY),4)*100,
                                BIAS_PERCENT_EXCISE_NET=round(percent_bias(REVENUE_COMPARATION$EXCISE_NET,REVENUE_COMPARATION$EXCISE_MONTLY),4)*100,
                                BIAS_PERCENT_CUS=round(percent_bias(REVENUE_COMPARATION$CUS,REVENUE_COMPARATION$CUS_MONTLY),4)*100,
                                BIAS_PERCENT_EXCISE_GROSS=round(percent_bias(REVENUE_COMPARATION$EXCISE_GROSS,REVENUE_COMPARATION$EXCISE_MONTLY_GROSS),4)*100,
                                BIAS_PERCENT_TAX_REVENUES=round(percent_bias(REVENUE_COMPARATION$TAX_REVENUES,REVENUE_COMPARATION$TAX_REVENUES),4)*100,
                                BIAS_PERCENT_REVENUES=round(percent_bias(REVENUE_COMPARATION$REVENUES,REVENUE_COMPARATION$REVENUES_MONTLY),4)*100)
                              CUMULATIVE_ERROR_EVALUATION<-t(CUMULATIVE_ERROR_EVALUATION)
                              colnames(CUMULATIVE_ERROR_EVALUATION)<-c("Error")
              
                
        
        # Average deviation obtained as an average of the monthly deviation in millions
                AVERAGE_BIAS_EVALUATION<-data.frame(
                              BIAS_PIT= bias(REVENUE_COMPARATION$PIT,REVENUE_COMPARATION$PIT_MONTLY),
                              BIAS_CIT= bias(REVENUE_COMPARATION$CIT,REVENUE_COMPARATION$CIT_MONTLY),  
                              BIAS_SSC= bias(REVENUE_COMPARATION$SSC,REVENUE_COMPARATION$SSC_MONTLY),
                              BIAS_VAT_NET= bias(REVENUE_COMPARATION$VAT_NET,REVENUE_COMPARATION$VAT_NET_MONTLY),
                              BIAS_VAT_GROSS= bias(REVENUE_COMPARATION$VAT_GROSS,REVENUE_COMPARATION$VAT_GROSS_MONTLY),
                              BIAS_EXCISE= bias(REVENUE_COMPARATION$EXCISE_NET,REVENUE_COMPARATION$EXCISE_MONTLY),
                              BIAS_CUS= bias(REVENUE_COMPARATION$CUS,REVENUE_COMPARATION$CUS_MONTLY),
                              BIAS_EXCISE_GROSS= bias(REVENUE_COMPARATION$EXCISE_GROSS,REVENUE_COMPARATION$EXCISE_MONTLY_GROSS),
                              BIAS_TAX_REVENUES= bias(REVENUE_COMPARATION$TAX_REVENUES,REVENUE_COMPARATION$TAX_REVENUES_MONTLY),
                              BIAS_REVENUES= bias(REVENUE_COMPARATION$REVENUES,REVENUE_COMPARATION$REVENUES_MONTLY))
                        
                        detach(package:Metrics)
        

                # 6.New revisited projection(Correction of current projections) --------------------------

                # This part of the code defines the number of months left until the end of the year in order to reassess the current projection, 
                # i.e to increase or decrease it according to the analyzed period. increase or decrease in that case that increase or decrease will be taken into account in order to 
                # revise the remaining months up or down here is another example: Current realization + projection according to a model by the end of the year adapted with a cumulative percentage 
                # increase or decrease to end of the year.
                
                # Range of months
                Range1<-seq(from=Define_months_final+1, to=12)
                
                # Data set for comparison
                
                # Projected
                SUBSET_NEW_PROJECTION<-filter(DAILY_AVERAGES,M %in% c(Range1))%>%
                  select(M,PIT_MONTLY,CIT_MONTLY,SSC_MONTLY,VAT_NET_MONTLY,VAT_GROSS_MONTLY,EXCISE_MONTLY,CUS_MONTLY,EXCISE_MONTLY_GROSS,TAX_REVENUES_MONTLY,NON_TAX_REVENUES_MONTLY,TAX_SSC_MONTLY,REVENUES_MONTLY)
                
               # This code also include daily average.Is need to check does is need here daily averages ???
                Montly_Projection_Revisited<-mutate(SUBSET_NEW_PROJECTION,
                                                    PIT_NEW=PIT_MONTLY+(PIT_MONTLY*CUMULATIVE_ERROR_EVALUATION[1,1]/100),
                                                    CIT_NEW=CIT_MONTLY+(CIT_MONTLY*CUMULATIVE_ERROR_EVALUATION[2,1]/100),
                                                    SSC_NEW=SSC_MONTLY+(SSC_MONTLY*CUMULATIVE_ERROR_EVALUATION[3,1]/100),
                                                    VAT_NET_NEW=VAT_NET_MONTLY+(VAT_NET_MONTLY*CUMULATIVE_ERROR_EVALUATION[4,1]/100),
                                                    VAT_GROSS_NEW=VAT_GROSS_MONTLY+(VAT_GROSS_MONTLY*CUMULATIVE_ERROR_EVALUATION[5,1]/100),
                                                    EXCISE_NEW=EXCISE_MONTLY+(EXCISE_MONTLY*CUMULATIVE_ERROR_EVALUATION[6,1]/100),
                                                    CUS_NEW=CUS_MONTLY+(CUS_MONTLY*CUMULATIVE_ERROR_EVALUATION[7,1]/100),
                                                    EXCISE_MONTLY_GROSS_NEW=EXCISE_MONTLY_GROSS+(EXCISE_MONTLY_GROSS*CUMULATIVE_ERROR_EVALUATION[8,1]/100),
                                                    TAX_REVENUES_NEW=TAX_REVENUES_MONTLY+(TAX_REVENUES_MONTLY*CUMULATIVE_ERROR_EVALUATION[9,1]/100),
                                                    REVENUES_NEW=REVENUES_MONTLY+(REVENUES_MONTLY*CUMULATIVE_ERROR_EVALUATION[10,1]/100))
                                                    
            # New projections based on error of CUMULATIVE_ERROR_EVALUATION
                
                Montly_Projection_Revisited0<- Montly_Projection_Revisited[,14:23]
                Montly_Projection_Revisited1<-data.frame(colSums(Montly_Projection_Revisited0,na.rm = TRUE))
                Montly_Projection_Revisited2<-t(Montly_Projection_Revisited1)
                colnames(Montly_Projection_Revisited2)<-c("PIT","CIT","SSC","VAT_NET","VAT_GROSS","EXCISE","CUS","EXCISE_GROSS","TAX_REVENUES","REVENUES")
                Montly_Projection_Revisited_FINAL<-Montly_Projection_Revisited2[,1:10]
                
                
                              
          # Current realisation of revenues
                Current_realizations<-data.frame(colSums(REVENUE_REALIZATION,na.rm = TRUE))
                Current_realizations1<-t(Current_realizations)
          
                
                Current_realizations_FINAL<-data.frame(Current_realizations1)%>%
                  rename(c(
                    "EXCISE"="EXCISE_NET"
                  ))%>%
                  select( "PIT","CIT","SSC","VAT_NET","VAT_GROSS","EXCISE","CUS", "EXCISE_GROSS", "TAX_REVENUES","REVENUES")
                
 
                FINAL_NEW_PROJECTIONS_0<-rbind(Montly_Projection_Revisited_FINAL,Current_realizations_FINAL)
                FINAL_NEW_PROJECTIONS<-data.frame(colSums(FINAL_NEW_PROJECTIONS_0))%>%
                  round(0)
                colnames(FINAL_NEW_PROJECTIONS)<-c("NEW_PROJECTIONS")
                
                rm(Montly_Projection_Revisited0,Montly_Projection_Revisited1,Montly_Projection_Revisited2,Montly_Projection_Revisited_FINAL,Current_realizations,Current_realizations1,Current_realizations_FINAL)
                


             # Forecast from GDP Approach (ELASTICITY&BUOYANCY)
                MONTLY_FORECAST_FINAL1<-select(MONTLY_FORECAST_FINAL,M)
                MONTLY_FORECAST_FINAL2<-MONTLY_FORECAST_FINAL
                type<-rep(c("NewForecast"),12)
                MONTLY_FORECAST_FINAL3<-cbind(MONTLY_FORECAST_FINAL1,MONTLY_FORECAST_FINAL2,type)
                MONTLY_FORECAST_FINAL3[,2:9]<-MONTLY_FORECAST_FINAL3[,2:9]%>%
                  round(0)
                
                
                MONTLY_FORECAST_FINAL3<-MONTLY_FORECAST_FINAL3%>%
                        select("M","PIT_MONTLY","CIT_MONTLY","SSC_MONTLY","VAT_NET_MONTLY","VAT_GROSS_MONTLY","EXCISE_MONTLY","CUS_MONTLY","EXCISE_MONTLY_GROSS","TAX_REVENUES_MONTLY",
                               "NON_TAX_REVENUES_MONTLY","TAX_SSC_MONTLY","REVENUES_MONTLY","type")%>%
                        dplyr:: rename(c(
                          "M"="M",
                          "Nominal_PIT"="PIT_MONTLY",
                          "Nominal_CIT"="CIT_MONTLY",	
                          "Nominal_VAT_NET"	="VAT_NET_MONTLY",
                          "Nominal_VAT_GROSS"="VAT_GROSS_MONTLY",
                          "Nominal_EXCISE"="EXCISE_MONTLY",	
                          "Nominal_CUSTOMS_DUTIES"="CUS_MONTLY",
                          "Nominal_SSC"="SSC_MONTLY"	,
                          "Nominal_EXCISE_GROSS"="EXCISE_MONTLY_GROSS",
                          "Nominal_TAX_REVENUES"="TAX_REVENUES_MONTLY",	
                          "Nominal_TAX_SSC"="TAX_SSC_MONTLY",
                          "Nominal_NON_TAX_REVENUES"="NON_TAX_REVENUES_MONTLY",
                          "Nominal_REVENUES"="REVENUES_MONTLY"))

                 
                Montly_Projection_Revisited_GRAPH<-Montly_Projection_Revisited %>%
                        select("PIT_MONTLY","CIT_MONTLY","SSC_MONTLY","VAT_NET_MONTLY","VAT_GROSS_MONTLY","EXCISE_MONTLY","CUS_MONTLY","EXCISE_MONTLY_GROSS","TAX_REVENUES_MONTLY",
                               "NON_TAX_REVENUES_MONTLY","TAX_SSC_MONTLY","REVENUES_MONTLY")%>%
                          dplyr:: rename(c(
                            "Nominal_PIT"="PIT_MONTLY",
                            "Nominal_CIT"="CIT_MONTLY",	
                            "Nominal_VAT_NET"	="VAT_NET_MONTLY",
                            "Nominal_VAT_GROSS"="VAT_GROSS_MONTLY",
                            "Nominal_EXCISE"="EXCISE_MONTLY",	
                            "Nominal_CUSTOMS_DUTIES"="CUS_MONTLY",
                            "Nominal_SSC"="SSC_MONTLY"	,
                            "Nominal_EXCISE_GROSS"="EXCISE_MONTLY_GROSS",
                            "Nominal_TAX_REVENUES"="TAX_REVENUES_MONTLY",	
                            "Nominal_TAX_SSC"="TAX_SSC_MONTLY",
                            "Nominal_NON_TAX_REVENUES"="NON_TAX_REVENUES_MONTLY",
                            "Nominal_REVENUES"="REVENUES_MONTLY"))

                
                SUBSET_PROJECTION1<-filter(REVENUE_REALIZATION,M %in% c(Range)) %>%# NEW 15-10-2020
                          select("PIT","CIT","SSC","VAT_NET","VAT_GROSS","EXCISE_NET","CUS","EXCISE_GROSS",
                                 "NON_TAX_REVENUES","TAX_REVENUES","TAX_SSC","REVENUES")%>%
                          dplyr:: rename(c(
                            "Nominal_PIT"="PIT",
                            "Nominal_CIT"="CIT",	
                            "Nominal_VAT_NET"	="VAT_NET",
                            "Nominal_VAT_GROSS"="VAT_GROSS",
                            "Nominal_EXCISE"="EXCISE_NET",	
                            "Nominal_CUSTOMS_DUTIES"="CUS",
                            "Nominal_SSC"="SSC"	,
                            "Nominal_EXCISE_GROSS"="EXCISE_GROSS",
                            "Nominal_TAX_REVENUES"="TAX_REVENUES",	
                            "Nominal_TAX_SSC"="TAX_SSC",
                            "Nominal_NON_TAX_REVENUES"="NON_TAX_REVENUES",
                            "Nominal_REVENUES"="REVENUES"))
                
     
                
                NEW_MONTLY_PROJECTION_FINAL<-rbind(SUBSET_PROJECTION1,Montly_Projection_Revisited_GRAPH)
                M<-seq(1:12)
                NEW_MONTLY_PROJECTION_FINAL<-cbind(M,NEW_MONTLY_PROJECTION_FINAL)
                type<-rep(c("NewForecast1"),12)
                
                
                NEW_MONTLY_PROJECTION_FINAL_GRAPH<-cbind(NEW_MONTLY_PROJECTION_FINAL,type)
                
           
                
                ##### Obedineta baza od tri tabeli  za grafikon (oficijalna proekcija,moja proekcija od FS i koregirana proekcija)
                
                
                
                BUDGET_PROJECTIONS$type<-as.factor(BUDGET_PROJECTIONS$type)
                
                FINAL_BASE_GRAPH<-rbind(MONTLY_FORECAST_FINAL3,NEW_MONTLY_PROJECTION_FINAL_GRAPH,BUDGET_PROJECTIONS)
                View(FINAL_BASE_GRAPH)
                

                # Annual graph 
                FINAL_BASE_GRAPH_YEARLY<-FINAL_BASE_GRAPH%>%
                  group_by(type)%>%
                  summarize(PIT=sum(Nominal_PIT),CIT=sum(Nominal_CIT),SSC=sum(Nominal_SSC),VAT_NET=sum(Nominal_VAT_NET ),
                            VAT_GROSS=sum(Nominal_VAT_GROSS),EXCISE=sum(Nominal_EXCISE),CUS=sum(Nominal_CUSTOMS_DUTIES),
                            EXCISE_GROSS=sum(Nominal_EXCISE_GROSS),TAX_REVENUES=sum(Nominal_TAX_REVENUES),NON_TAX_REVENUES=sum(Nominal_NON_TAX_REVENUES),
                            TAX_SSC=sum(Nominal_TAX_SSC),REVENUES=sum(Nominal_REVENUES))
                N<-seq(1:3)
                FINAL_BASE_GRAPH_YEARLY<-cbind(FINAL_BASE_GRAPH_YEARLY,N)
                FINAL_BASE_GRAPH_YEARLY[,2:9]<-FINAL_BASE_GRAPH_YEARLY[,2:9]%>%
                  round(0)
                
                View(FINAL_BASE_GRAPH_YEARLY)
                
       # Explanation of graph
                # NewForecast-Current custom projection based on GDP-Forecast approach (custom model)
                # NewForecast1-New projection based on current realization and re-evaluation of revenues by the end of the year
                # BUDGET PROJECTION-Projection based on official figures published in the budget and distributed monthly

                


# Module (III.) (Iteration and making new projections)----------------------
    
    # B r i e f    e x p l a n a t i o n
    
    # Main purpose of this code is to make new projections for the following years.Namely collection of revenues in current yeaar have influence of collection of revenue for next years.
    # In order to capture this effects with this module we make itteration again over code from Module 1 but now with new projection for current year adjusted for montly deviation (explanitd in Module 2)
                
                
          # Adjusting the first base
                MACRO_FISCAL_INDICATORS_MERGE<-MACRO_FISCAL_INDICATORS%>%
                  filter(Year==FORECASTING_YEAR)%>%
                  data.frame()
                
                MACRO_FISCAL_SUBSET<-MACRO_FISCAL_INDICATORS_MERGE%>%
                  select(Year,GDP_Deflator,Nominal_GDP,Real_GDP_growth)

                
            # Adjusting the second base
                MACRO_FISCAL_INDICATORS_BASE_UPDATED<-FINAL_BASE_GRAPH_YEARLY%>%
                filter(type=="NewForecast1")%>%
                  select("PIT","CIT","SSC","VAT_NET","VAT_GROSS","EXCISE","CUS",
                         "NON_TAX_REVENUES","TAX_REVENUES","TAX_SSC","REVENUES")%>%
                  dplyr:: rename(c(
                    "Nominal_PIT"="PIT",
                    "Nominal_CIT"="CIT",	
                    "Nominal_VAT_NET"	="VAT_NET",
                    "Nominal_VAT_GROSS"="VAT_GROSS",
                    "Nominal_EXCISE"="EXCISE",	
                    "Nominal_CUSTOMS_DUTIES"="CUS",
                    "Nominal_SSC"="SSC"	,
                    "Nominal_TAX_REVENUES"="TAX_REVENUES",	
                    "Nominal_TAX_SSC"="TAX_SSC",
                    "Nominal_NON_TAX_REVENUES"="NON_TAX_REVENUES",
                    "Nominal_REVENUES"="REVENUES"))

                MACRO_FISCAL_INDICATORS_BASE_UPDATED1<-cbind(MACRO_FISCAL_SUBSET,MACRO_FISCAL_INDICATORS_BASE_UPDATED)%>%
                  dplyr:: rename(c(
                    "Nominal_TAX"="Nominal_TAX_REVENUES"	
                    ))
                

                MACRO_FISCAL_INDICATORS_BASE_UPDATED2<-rbind(BASE_ESTIMATION,MACRO_FISCAL_INDICATORS_BASE_UPDATED1)
                
                
                
                MACRO_FISCAL_INDICATORS_BASE_UPDATED3<-MACRO_FISCAL_INDICATORS%>%   ### PROMENA 15-10-2020
                  dplyr::filter(Year>INPUT_FORECASTING_YEAR)
                
                
                MACRO_FISCAL_INDICATORS_BASE_UPDATED_FINAL<-rbind(MACRO_FISCAL_INDICATORS_BASE_UPDATED2,MACRO_FISCAL_INDICATORS_BASE_UPDATED3)
                View(MACRO_FISCAL_INDICATORS_BASE_UPDATED_FINAL)
                
              
                
                FINAL_FORECASTING0<-data.frame(select(MACRO_FISCAL_INDICATORS_BASE_UPDATED_FINAL,Year,Nominal_GDP,Nominal_PIT,Nominal_CIT,Nominal_VAT_NET,Nominal_VAT_GROSS,Nominal_EXCISE,Nominal_CUSTOMS_DUTIES,Nominal_SSC,Nominal_TAX,Nominal_TAX_SSC,Nominal_NON_TAX_REVENUES,Nominal_REVENUES))%>%
                  arrange(desc(Year))

                
                # ARDL and ECM coeffcients
                FINAL_FORECASTING1<-mutate(FINAL_FORECASTING0,
                                           Nominal_GDP_GROWTH=fun1ab(Nominal_GDP)) %>%
                  arrange((Year))
                FINAL_FORECASTING2<-mutate(FINAL_FORECASTING1,
                                           CALCULATION_PIT=1+(FINAL_ELASTICITY_COEFFICIENTS[1,1]/100*Nominal_GDP_GROWTH),
                                           CALCULATION_CIT=1+(FINAL_ELASTICITY_COEFFICIENTS[2,2]/100*Nominal_GDP_GROWTH),##<---Estimation with elasticity
                                           CALCULATION_VAT_NET=1+(FINAL_ELASTICITY_COEFFICIENTS[3,1]/100*Nominal_GDP_GROWTH),
                                           CALCULATION_VAT_GROSS=1+(FINAL_ELASTICITY_COEFFICIENTS[4,1]/100*Nominal_GDP_GROWTH),
                                           CALCULATION_EXCISE=1+(FINAL_ELASTICITY_COEFFICIENTS[5,1]/100*Nominal_GDP_GROWTH),
                                           CALCULATION_CUSTOMS_DUTIES=1+(FINAL_ELASTICITY_COEFFICIENTS[6,2]/100*Nominal_GDP_GROWTH),##<---Estimation with elasticity
                                           CALCULATION_SSC=1+(FINAL_ELASTICITY_COEFFICIENTS[7,1]/100*Nominal_GDP_GROWTH),
                                           CALCULATION_TAX=1+(FINAL_ELASTICITY_COEFFICIENTS[8,1]/100*Nominal_GDP_GROWTH),
                                           CALCULATION_TAX_SSC=1+(FINAL_ELASTICITY_COEFFICIENTS[9,1]/100*Nominal_GDP_GROWTH),
                                           CALCULATION_NON_TAX_REVENUES=1+(FINAL_ELASTICITY_COEFFICIENTS[10,2]/100*Nominal_GDP_GROWTH),##<--Estimation with elasticity
                                           CALCULATION_Nominal_REVENUES=1+(FINAL_ELASTICITY_COEFFICIENTS[11,1]/100*Nominal_GDP_GROWTH))
               
                
                FINAL_FORECASTING2<-inner_join(FINAL_FORECASTING2,DS_NEXT_YEARS, by = "Year")
                

                # PIT
                for (i in 1:length(which(is.na(FINAL_FORECASTING2$Nominal_PIT)))){
                  FINAL_FORECASTING2=FINAL_FORECASTING2%>%mutate(Nominal_PIT=if_else(is.na(Nominal_PIT), (CALCULATION_PIT*lag(Nominal_PIT)+ DS_PIT),Nominal_PIT))
                }
                # CIT
                for (i in 1:length(which(is.na(FINAL_FORECASTING2$Nominal_CIT)))){
                  FINAL_FORECASTING2=FINAL_FORECASTING2%>%mutate(Nominal_CIT=if_else(is.na(Nominal_CIT),(CALCULATION_CIT*lag(Nominal_CIT)+ DS_CIT),Nominal_CIT))
                }
                # VAT_NET
                for (i in 1:length(which(is.na(FINAL_FORECASTING2$Nominal_VAT_NET)))){
                  FINAL_FORECASTING2=FINAL_FORECASTING2%>%mutate(Nominal_VAT_NET=if_else(is.na(Nominal_VAT_NET),(CALCULATION_VAT_NET*lag(Nominal_VAT_NET)+ DS_VAT_NET),Nominal_VAT_NET))
                }
                # VAT_GROSS 
                for (i in 1:length(which(is.na(FINAL_FORECASTING2$Nominal_VAT_GROSS)))){
                  FINAL_FORECASTING2=FINAL_FORECASTING2%>%mutate(Nominal_VAT_GROSS=if_else(is.na(Nominal_VAT_GROSS),(CALCULATION_VAT_GROSS*lag(Nominal_VAT_GROSS)+ DS_VAT_GROSS),Nominal_VAT_GROSS))
                }
                # EXCISE
                for (i in 1:length(which(is.na(FINAL_FORECASTING2$Nominal_EXCISE)))){
                  FINAL_FORECASTING2=FINAL_FORECASTING2%>%mutate(Nominal_EXCISE=if_else(is.na(Nominal_EXCISE), (CALCULATION_EXCISE*lag(Nominal_EXCISE)+ DS_EXCISE),Nominal_EXCISE))
                }
                
                # CUSTOMS_DUTIES
                for (i in 1:length(which(is.na(FINAL_FORECASTING2$Nominal_CUSTOMS_DUTIES)))){
                  FINAL_FORECASTING2=FINAL_FORECASTING2%>%mutate(Nominal_CUSTOMS_DUTIES=if_else(is.na(Nominal_CUSTOMS_DUTIES),(CALCULATION_CUSTOMS_DUTIES*lag(Nominal_CUSTOMS_DUTIES)+ DS_CUSTOMS_DUTIES) ,Nominal_CUSTOMS_DUTIES))
                }
                
                # SSC
                for (i in 1:length(which(is.na(FINAL_FORECASTING2$Nominal_SSC)))){
                  FINAL_FORECASTING2=FINAL_FORECASTING2%>%mutate(Nominal_SSC=if_else(is.na(Nominal_SSC), (CALCULATION_SSC*lag(Nominal_SSC)+ DS_SSC),Nominal_SSC))
                }
                
                # TAX
                for (i in 1:length(which(is.na(FINAL_FORECASTING2$Nominal_TAX)))){
                  FINAL_FORECASTING2=FINAL_FORECASTING2%>%mutate(Nominal_TAX=if_else(is.na(Nominal_TAX), (CALCULATION_TAX*lag(Nominal_TAX)+ DS_TAX),Nominal_TAX))
                }
                
                # TAX_SSC
                for (i in 1:length(which(is.na(FINAL_FORECASTING2$Nominal_TAX_SSC)))){
                  FINAL_FORECASTING2=FINAL_FORECASTING2%>%mutate(Nominal_TAX_SSC=if_else(is.na(Nominal_TAX_SSC),(CALCULATION_TAX_SSC*lag(Nominal_TAX_SSC)+ DS_TAX_SSC),Nominal_TAX_SSC))
                }
                
                # NON_TAX_REVENUES
                for (i in 1:length(which(is.na(FINAL_FORECASTING2$Nominal_NON_TAX_REVENUES)))){
                  FINAL_FORECASTING2=FINAL_FORECASTING2%>%mutate(Nominal_NON_TAX_REVENUES=if_else(is.na(Nominal_NON_TAX_REVENUES), (CALCULATION_NON_TAX_REVENUES*lag(Nominal_NON_TAX_REVENUES)+ DS_NON_TAX_REVENUES),Nominal_NON_TAX_REVENUES))
                }
                
                # Nominal_REVENUES
                for (i in 1:length(which(is.na(FINAL_FORECASTING2$Nominal_REVENUES)))){
                  FINAL_FORECASTING2=FINAL_FORECASTING2%>%mutate(Nominal_REVENUES=if_else(is.na(Nominal_REVENUES),(CALCULATION_Nominal_REVENUES*lag(Nominal_REVENUES)+ DS_REVENUES),Nominal_REVENUES))
                }
                
                # FINAL FORECASTING    
                FINAL_FORECASTING<-data.frame(select(FINAL_FORECASTING2,Year,Nominal_GDP,Nominal_PIT,Nominal_CIT,Nominal_VAT_NET,Nominal_VAT_GROSS,Nominal_EXCISE,Nominal_CUSTOMS_DUTIES,Nominal_SSC,Nominal_TAX,Nominal_TAX_SSC,Nominal_NON_TAX_REVENUES,Nominal_REVENUES))
 
                
                FINAL_FORECASTING_0<-FINAL_FORECASTING%>%
                  dplyr:: mutate(Nominal_EXCISE_GROSS= Nominal_EXCISE* 1.141334764)%>%
                  dplyr:: rename(c(
                    "Year"="Year",
                    "GDP"="Nominal_GDP",
                    "PIT"="Nominal_PIT",
                    "CIT"="Nominal_CIT",	
                    "VAT_NET"	="Nominal_VAT_NET",
                    "VAT_GROSS"="Nominal_VAT_GROSS",
                    "EXCISE"="Nominal_EXCISE",	
                    "CUSTOMS_DUTIES"="Nominal_CUSTOMS_DUTIES",
                    "SSC"="Nominal_SSC"	,
                    "EXCISE_GROSS"="Nominal_EXCISE_GROSS",
                    "TAX_REVENUES"="Nominal_TAX",	
                    "TAX_SSC"="Nominal_TAX_SSC",
                    "NON_TAX_REVENUES"="Nominal_NON_TAX_REVENUES",
                    "REVENUES"="Nominal_REVENUES"))%>%
                  select("Year","GDP","PIT","CIT","VAT_NET", "VAT_GROSS","EXCISE",
                         "CUSTOMS_DUTIES","SSC","EXCISE_GROSS","TAX_REVENUES","TAX_SSC",
                         "NON_TAX_REVENUES","REVENUES"
                  )
                
                
                View(FINAL_FORECASTING_0)
                
                
                
        gc(TRUE)
              
        # FINAL_FORECASTING1<-mutate(FINAL_FORECASTING,
        #                            
        #                            
        #                            )
        
        FINAL_FORECASTING_1 <- FINAL_FORECASTING_0 %>% 
        dplyr::mutate(across(everything()), . / GDP) %>% 
        dplyr::select(PIT,CIT,VAT_NET,VAT_GROSS,EXCISE,CUSTOMS_DUTIES,SSC,EXCISE_GROSS,TAX_REVENUES,TAX_SSC,NON_TAX_REVENUES,REVENUES)%>% 
        dplyr:: rename(c(
          "PIT_pct"="PIT",
          "CIT_pct"="CIT",	
          "VAT_NET_pct"	="VAT_NET",
          "VAT_GROSS_pct"="VAT_GROSS",
          "EXCISE_pct"="EXCISE",	
          "CUSTOMS_DUTIES_pct"="CUSTOMS_DUTIES",
          "SSC_pct"="SSC"	,
          "EXCISE_GROSS_pct"="EXCISE_GROSS",
          "TAX_REVENUES_pct"="TAX_REVENUES",	
          "TAX_SSC_pct"="TAX_SSC",
          "NON_TAX_REVENUES_pct"="NON_TAX_REVENUES",
          "REVENUES_pct"="REVENUES")) 
            
        
        
        FINAL_FORECASTING<-cbind(FINAL_FORECASTING_0,FINAL_FORECASTING_1)
        
        
      #   "C:/Users/hp/Documents/HOME WORK/ZIP/0-FISKALNA STRATEGIJA/FS 2021/1.Forecasting Model/PLOTING DAHSBOARDS/

        

# Module (IV.) Output in Excel (Please set output path in your computer !!!!) ---------------------------------------------------------
        
               gc(TRUE)
        
        # C:/Users/User/Documents/DataScience/GitHub/Project 7/R/INPUT-DATA
        
        path<-"C:/Users/User/Documents/DataScience/GitHub/Project 7/R/INPUT-DATA/"
        path1<-"C:/Users/User/Documents/DataScience/GitHub/Project 7/R/INPUT-DATA/OUTPUT/"

        
        library(xlsx)
#       
        # Path File output.xlsx
        write.xlsx(as.data.frame(FINAL_ELASTICITY_COEFFICIENTS),file=paste(path,"output.xlsx",sep=""),sheetName="Coefficients", row.names=TRUE)
        write.xlsx(as.data.frame(FINAL_ERRORS),file=paste(path,"output.xlsx",sep=""),sheetName="Accurancy", row.names=TRUE, append=TRUE)
        write.xlsx(as.data.frame(FINAL_FORECASTING), file=paste(path,"output.xlsx",sep=""),col.names=TRUE,sheetName="Forecast", row.names=FALSE, append=TRUE)
        write.xlsx(as.data.frame(REVENUE_STRUCTURE_FINAL), file=paste(path,"output.xlsx",sep=""),sheetName="REVENUE_STRUCTURE", row.names=TRUE)
        
        # Path 1 File new_projections
        write.xlsx(as.data.frame(MONTLY_FORECAST_FINAL), file=paste(path1,"new_projections.xlsx",sep=""),sheetName="REVENUE_STRUCTURE",sheetName="MONTLY_FORECAST_FINAL", row.names=TRUE,append=TRUE)
        write.xlsx(as.data.frame(data_bizz_days), file=paste(path1,"new_projections.xlsx",sep=""),col.names=TRUE,sheetName="Bizz_days", row.names=TRUE, append=TRUE)
        write.xlsx(as.data.frame(DAILY_AVERAGES),  file=paste(path1,"new_projections.xlsx",sep=""),sheetName="DAILY_AVERAGES", row.names=TRUE, append=TRUE)
        write.xlsx(as.data.frame(WEEKLY_PROJECTION), file=paste(path1,"new_projections.xlsx",sep=""),col.names=TRUE,sheetName="WEEKLY_PROJECTION", row.names=TRUE, append=TRUE)
        write.xlsx(as.data.frame(MONTLY_ERROR), file=paste(path1,"new_projections.xlsx",sep=""),col.names=TRUE,sheetName="MONTLY_ERROR", row.names=TRUE, append=TRUE)
        write.xlsx(as.data.frame(CUMULATIVE_ERROR_EVALUATION), file=paste(path1,"new_projections.xlsx",sep=""),col.names=TRUE,sheetName="CUMULATIVE_ERROR_EVALUATION", row.names=TRUE, append=TRUE)
        write.xlsx(as.data.frame(AVERAGE_BIAS_EVALUATION), file=paste(path1,"new_projections.xlsx",sep=""),col.names=TRUE,sheetName="AVERAGE_BIAS_EVALUATION", row.names=TRUE, append=TRUE)
        write.xlsx(as.data.frame(Montly_Projection_Revisited),file=paste(path1,"new_projections.xlsx",sep=""),col.names=TRUE,sheetName="Montly_Projection_Revisited", row.names=TRUE, append=TRUE)
        write.xlsx(as.data.frame(FINAL_NEW_PROJECTIONS), file=paste(path1,"new_projections.xlsx",sep=""),col.names=TRUE,sheetName="FINAL_NEW_PROJECTIONS", row.names=TRUE, append=TRUE)
        write.xlsx(as.data.frame(FINAL_BASE_GRAPH),file=paste(path1,"new_projections.xlsx",sep=""),col.names=TRUE,sheetName="FINAL_BASE_GRAPH", row.names=TRUE, append=TRUE)
        write.xlsx(as.data.frame(FINAL_BASE_GRAPH_YEARLY), file=paste(path1,"new_projections.xlsx",sep=""),col.names=TRUE,sheetName="FINAL_BASE_GRAPH_YEARLY", row.names=TRUE)
        write.xlsx(as.data.frame(FINAL_FORECASTING), file=paste(path1,"new_projections.xlsx",sep=""),col.names=TRUE,sheetName="MediumTermForecast", row.names=FALSE)
        
        
    