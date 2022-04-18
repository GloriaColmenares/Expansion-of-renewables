# Expansion-of-renewables
The code in this replication package constructs the analysis file from the six data sources () using R, Stata, Python, and Excel. Five main files run all of the code to generate the data for the 14 figures and 8 tables in the paper. The replicator should expect the code to run for about X hours.

Data on plant capacities were downloaded from the Open Power System (OPS, 2018). We use data on conventional power plants for Germany (DE). Data can be downloaded from https://data.open-power-system-data.org/conventional_power_plants/2017-07-07 and https://doi.org/10.25832/conventional_power_plants/2018-12-20, select conventional_power_plants_DE.xlsx. A copy of the data is provided as part of this archive. The data are in the public domain.
Datafiles: conventional_power_plants_DE_17.xlsx and conventional_power_plants_DE_18.xlsx





This datalist is awesome 

| Data file                                                  | Source                                         |  Notes|  Provided                     |
| ---------------------------------------------------------- |------------------------------------------------| ------| ----------------------------- |
| namechp.xlsx                                               |- plant capacitites                             | Combines data from OPP and ENTSO-E    |                   |
| tout.xlsx                                                  |- outage ENTSOE of all plants                   |       |                   |
| chp.xlsx                                                   |- chp data consumption and probabilities        |       |                   |
| Marginalcosts.xlsx                                         |- fuel and Co2 costs                            |       |                   |
| Realisierter_Stromverbrauch_201701020000_201809302345.csv  |- Actual electricity demand                     |       |                   |
| Realisierte_Erzeugung_201701020000_201809302345.csv        |- Actual electricity production                 |       |                   |
| inst.xlsx                                                  |- prices of electricity, coal, gas, oil         |       |                   |
| wind.xlsx                                                  |- Hourly wind data per plant                    |       |                   |
| solar.xlsx                                                 |- Hourly wind data per plant                    |       |                   |
| inst.xlsx                                                  |- prices of electricity, coal, gas, oil         |       |                   |
| wind.xlsx                                                  |- Hourly wind data per plant                    |       |                   |
| solar.xlsx                                                 |- Hourly wind data per plant                    |       |                   |
| inst.xlsx                                                  |- prices of electricity, coal, gas, oil         |       |                   |
| wind.xlsx                                                  |- Hourly wind data per plant                    |       |                   |
| solar.xlsx                                                 |- Hourly wind data per plant                    |       |                   |
| I.xlsx                                                     |- Electricity prod per plant                    |       |                   |
| firmpy.xlsx                                                |- firms and plants match                        |       |                   |
| resdm5.xlsx                                                | - Electricity prod per tech, demand, renewables|       |                   |
| loadfa.xlsx                                                |- load factors as actual prod in hour           |       |                   |
| temper.xlsx                                                |- temperatures per plant location               |       |                   |
| ramp.xlsx                                                  |- Ramping costs                                 |       |                   |



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
