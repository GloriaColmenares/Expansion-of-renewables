# Expansion-of-renewables
The code in this replication package constructs the analysis file from the 8 data sources described in the table below using R, Stata, Python, and Excel. Five main files run all of the code to generate the data for the 14 figures and 8 tables in the paper. The replicator should expect the code to run for about X hours.

1. Data on conventional net plant installed capacities were downloaded from the Open Power System (OPS, 2018). We use data on conventional power plants for Germany (DE). Data can be downloaded from https://data.open-power-system-data.org/conventional_power_plants/2017-07-07 and https://doi.org/10.25832/conventional_power_plants/2018-12-20, select conventional_power_plants_DE.xlsx. A copy of the data is provided as part of this archive. The data are in the public domain.
Raw datafiles: conventional_power_plants_DE_17.xlsx and conventional_power_plants_DE_18.xlsx. When the net capacity is lower than production, we use maximum production values from the period of analysis from the Strommarktdaten (SMARD, 2020***). To calculate the total capacity in the market, we use data from https://www.smard.de/en. Raw datafile: Installed_generation_capacity_201701010000_201812312359.xlsx
Data on wind plant capacities were sourced from Aurora Energy Research (AER, 2018) may be obtained by contacting Prof. Dr. Andreas Löschel at andreas.loeschel@rub.de. It can take some months to negotiate data use agreements and gain access to the data. The author will assist with any reasonable replication attempts for two years following publication. A copy of the data is provided as part of this archive and cannot be redistributed. 

2. Data on Forced outage failure (fof) is obtained from Entso-e and can be downloaded from https://transparency.entsoe.eu/content/static_content/Static%20content/knowledge%20base/SFTP-Transparency_Docs.html. The files are under the numerals 15.1.A&B, 15.1.C&D. A copy of the data is provided as part of this archive. The data are in the public domain, previous registration.

3. Data on Combined heat power (chp) were downloaded from the Genesis Database of the Statisches Bundesamt Deutschland (SBD, 2022), Ergebnis 43311-0002 and 43351-0004, which can be downloaded from https://www-genesis.destatis.de/genesis/online?operation=themes&code=4#abreadcrumb item 43. A copy of the data is provided as part of this archive. The data are in the public domain.
Heat profiles for commercial and residential loads have been sourced from OPS, and can be found in the public domain https://data.open-power-system-data.org/when2heat/2019-08-06. A copy of the data is provided as part of this archive. 

4. Data on fuel prices is sourced from from Aurora Energy Research (AER, 2018) may be obtained by contacting Prof. Dr. Andreas Löschel at andreas.loeschel@rub.de. It can take some months to negotiate data use agreements and gain access to the data. The author will assist with any reasonable replication attempts for two years following publication. A copy of the data is provided as part of this archive and cannot be redistributed. Data on CO2 prices are sourced from the European Energy Exchange (EEX) emission market (EUA), contract EUSP. The data can be bought, but a copy is provided as part of this archive and cannot be redistributed. Raw datafiles: EmissionSpotHistory_2017.xls and EmissionSpotHistory_2018.xls.

5. Data on heat, emissions rate and O&M data are sourced from OPS, the Umweltbundesamt and the Öko-Institut e.V. A copy of the data is provided as part of this archive. This file also uses data from file 4.

6. Data on actual demand consumption (Realisierter Stromverbrauch) can be obtained from the SMARD database and downloaded from https://www.smard.de/en. The data are in the public domain and is provided as part of this archive.

7. Actual electricity production data per energy source (Realisierter Erzeugung) can be obtained from the SMARD database and downloaded from https://www.smard.de/en. The data are in the public domain and is provided as part of this archive.

8. Hourly data on wind speed, solar radiation (8.1.) and air temperature (8.2.) can be found on the Deutscher Wetterdienst database (DWD) under the Climate Data Center (wind, solar, and air temperature) from https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/. The data are in the public domain and is provided as part of this archive.

9. Actual electricity production data per plant is sourced from AER and may be obtained by contacting Prof. Dr. Andreas Löschel at andreas.loeschel@rub.de. It can take some months to negotiate data use agreements and gain access to the data. The author will assist with any reasonable replication attempts for two years following publication. A copy of the data is provided as part of this archive and cannot be redistributed. 

10. Data on plant ownership can be found on OPS. A copy of the data is provided as part of this archive. The data are in the public domain. Same raw datafiles as in 1.

11. Data on ramping times for each fossil fuel plant are sourced from Boldt et al (2012).

---
References

1. Boldt, Jenny, Lisa Hankel, Lilian Laurisch, Felix Lutterbeck, Pao-Yu Oei, Aram Sander, Andreas Schr ̈oder, Helena Schweter, Philipp Sommer, and Jasmin
Sulerz. 2012. “Renewables in the Grid Modeling the German Power Market of the Year 2030.” Electricity Markets Working Papers, 48: 1–91.
---
#This datalist is awesome 

| Data file                                                    | Type                                           | Source                                |Provided |
| ------------------------------------------------------------ |------------------------------------------------| --------------------------------------|-------- |
| 1.namechp.xlsx                                               |- plant capacitites                             | Combines data from OPS, SMARD and AER | yes |  
| 2.tout.xlsx                                                  |- fof per plant                                 | Combines data from Entso-e and 1.     | yes |
| 3.chp.xlsx                                                   |- chp data: consumption and probabilities       | Combines data from SBD and OPS        | yes |
| 4.inst.xlsx                                                  |- prices of electricity, coal, gas, oil         | AER and EEX                           | yes |
| 5.Marginalcosts.xlsx                                         |- fuel and Co2 costs                            | Combines data from various sources    | yes |
| 6.Realisierter_Stromverbrauch_201701020000_201809302345.csv  |- Actual electricity demand                     |  SMARD                                | yes |
| 7.Realisierte_Erzeugung_201701020000_201809302345.csv        |- Actual electricity production                 |  SMARD                                | yes |
| 8.wind.xlsx                                                  |- Hourly wind data per plant location           |  DWD                                  | yes |
| 8.1.solar.xlsx                                               |- Hourly wind data per plant location           |  DWD                                  | yes |
| 8.2 temper.xlsx                                              |- Hourly air temperatures per plant location    |  DWD                                  | yes |
| 9.I.xlsx                                                     |- Electricity prod per plant                    |  AER and Entso-e                      | yes |
| 10.firmpy.xlsx                                               |- matching firms and plants                     |  OPS                                  | yes |
| 11.ramp.xlsx                                                 |- assigned hourly ramping costs                 |  Combines data from 5. and Boldt et al (2012)| yes |

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
