# Expansion-of-renewables
The code in this replication package constructs the analysis file from the N data sources () using R, Stata, Python, and Excel. Five main files run all of the code to generate the data for the 14 figures and 8 tables in the paper. The replicator should expect the code to run for about X hours.

1. Data on conventional net plant capacities were downloaded from the Open Power System (OPS, 2018). We use data on conventional power plants for Germany (DE). Data can be downloaded from https://data.open-power-system-data.org/conventional_power_plants/2017-07-07 and https://doi.org/10.25832/conventional_power_plants/2018-12-20, select conventional_power_plants_DE.xlsx. A copy of the data is provided as part of this archive. The data are in the public domain.
Raw datafiles: conventional_power_plants_DE_17.xlsx and conventional_power_plants_DE_18.xlsx. When the net capacity is lower than production, we use maximum production values from the period of analysis from the Strommarktdaten (SMARD, 2020***). To calculate the total capacity in the market, we use data from https://www.smard.de/en. Raw datafile: Installed_generation_capacity_201701010000_201812312359.xlsx
Data on wind plant capacities were sourced from Aurora Energy Research (AER, 2018) and are confidential, but may be obtained by contacting Prof. Dr. Andreas Löschel at andreas.loeschel@rub.de. It can take some months to negotiate data use agreements and gain access to the data. The author will assist with any reasonable replication attempts for two years following publication.

2. Data on Forced outage failure (fof) is obtained from Entso-e and can be downloaded from https://transparency.entsoe.eu/outage-domain/r2/unavailabilityOfProductionAndGenerationUnits/show A copy of the data is provided as part of this archive. The data are in the public domain, previous registration.

3. 




This datalist is awesome 

| Data file                                                  | Type                                           | Source|  Provided                     |
| ---------------------------------------------------------- |------------------------------------------------| ------| ----------------------------- |
| 1.namechp.xlsx                                               |- plant capacitites                             | Combines data from OPP, SMARD and AER   |  yes |  
| 2.tout.xlsx                                                  |- outage all plants                             | Entso-e                                 |  yes |
| 3.chp.xlsx                                                   |- chp data consumption and probabilities        |       |                   |
| 4.Marginalcosts.xlsx                                         |- fuel and Co2 costs                            |       |                   |
| 5.Realisierter_Stromverbrauch_201701020000_201809302345.csv  |- Actual electricity demand                     |       |                   |
| 6.Realisierte_Erzeugung_201701020000_201809302345.csv        |- Actual electricity production                 |       |                   |
| 7.inst.xlsx                                                  |- prices of electricity, coal, gas, oil         |       |                   |
| 8.wind.xlsx                                                  |- Hourly wind data per plant                    |       |                   |
| 9.solar.xlsx                                                 |- Hourly wind data per plant                    |       |                   |
| 10.I.xlsx                                                     |- Electricity prod per plant                    |       |                   |
| 11.firmpy.xlsx                                                |- firms and plants match                        |       |                   |
| 12.resdm5.xlsx                                                | - Electricity prod per tech, demand, renewables|       |                   |
| 13.loadfa.xlsx                                                |- load factors as actual prod in hour           |       |                   |
| 14.temper.xlsx                                                |- temperatures per plant location               |       |                   |
| 15.ramp.xlsx                                                  |- Ramping costs                                 |       |                   |



Codes for "Expansion of Intermittent Renewables: Strategies, pass-through costs, and welfare distribution"

**************************************************************************************
Use codes in file *R_paneldata_**
**************************************************************************************
1. Download all files [here](https://mega.nz/folder/eckSFQYK#smoBcJIp-vna-XvnHdCZQg)
2. Run *R_paneldata_SFE* until line 1041 to obtain files sfe_one.xlsx and sfe_two.xlsx
3. Run Stata do file in *Stata_SFE* using excel files in step 2
4. Return to *R_paneldata_SFE* and run lines 1055-1186 to obtain Tables and Figures
5. Run *R_paneldata_BLP* until line 553 to obtain file i1rv.cvs
6. For aditional data for Table 1 run lines 563-569

**************************************************************************************
Use codes in file *BLP_python_**
**************************************************************************************
7. Run each case separately until line 2376 (each case takes around 20 hours)
8. Go to *BLP_python_cf2* and run the entire code, each case separately (similar to step 7)
9. Go to *BLP_python* and run lines 2378-2426 to gather all results in files pt.xlsx, cs.xlsx, curva.xlsx, ps.csv
10. Return to *R_paneldata_BLP* and run lines 576-946 using files in step 9 to obtain Tables and Figures
