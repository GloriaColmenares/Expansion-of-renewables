# Expansion-of-renewables
The code in this replication package constructs the analysis file from the 8 data sources described in the table below using R, Stata, Python, and Excel. Five main files run all of the code to generate the data for the 14 figures and 8 tables in the paper. The replicator should expect the code to run for about X hours.

1. Data on conventional net plant installed capacities were downloaded from the Open Power System (OPS, 2018). We use data on conventional power plants for Germany (DE). Data can be downloaded from https://data.open-power-system-data.org/conventional_power_plants/2017-07-07 and https://doi.org/10.25832/conventional_power_plants/2018-12-20, select conventional_power_plants_DE.xlsx. A copy of the data is provided as part of this archive. The data are in the public domain.
Raw datafiles: conventional_power_plants_DE_17.xlsx and conventional_power_plants_DE_18.xlsx. When the net capacity is lower than production, we use maximum production values from the period of analysis from the Strommarktdaten (SMARD, 2020***). To calculate the total capacity in the market, we use data from https://www.smard.de/en. Raw datafile: Installed_generation_capacity_201701010000_201812312359.xlsx
Data on wind plant capacities were sourced from Aurora Energy Research, EOS database (AER, 2019) which may be obtained by contacting Prof. Dr. Andreas Löschel at andreas.loeschel@rub.de. It can take some months to negotiate data use agreements and gain access to the data. The author will assist with any reasonable replication attempts for two years following publication. A copy of the data is provided as part of this archive and cannot be redistributed. 

2. Data on Forced outage failure (fof) is obtained from Entso-e and can be downloaded from https://transparency.entsoe.eu/content/static_content/Static%20content/knowledge%20base/SFTP-Transparency_Docs.html. The files are under the numerals 15.1.A&B, 15.1.C&D. A copy of the data is provided as part of this archive. The data are in the public domain, previous registration.

3. Data on Combined heat power (chp) were downloaded from the Genesis Database of the Statisches Bundesamt Deutschland (SBD, 2022), Ergebnis 43311-0002 and 43351-0004, which can be downloaded from https://www-genesis.destatis.de/genesis/online?operation=themes&code=4#abreadcrumb item 43. A copy of the data is provided as part of this archive. The data are in the public domain.
Heat profiles for commercial and residential loads have been sourced from OPS, and can be found in the public domain https://data.open-power-system-data.org/when2heat/2019-08-06. A copy of the data is provided as part of this archive. 

4. Data on fuel prices is sourced from from Aurora Energy Research, EOS database (AER, 2019) which may be obtained by contacting Prof. Dr. Andreas Löschel at andreas.loeschel@rub.de. It can take some months to negotiate data use agreements and gain access to the data. The author will assist with any reasonable replication attempts for two years following publication. A copy of the data is provided as part of this archive and cannot be redistributed. Data on CO2 prices are sourced from the European Energy Exchange (EEX) emission market (EUA), contract EUSP. The data can be bought, but a copy is provided as part of this archive and cannot be redistributed. Raw datafiles: EmissionSpotHistory_2017.xls and EmissionSpotHistory_2018.xls.

5. Data on heat, emissions rate and O&M data are sourced from OPS, the Umweltbundesamt and the Öko-Institut e.V. A copy of the data is provided as part of this archive. This file also uses data from file 4.

6. Data on actual demand consumption (Realisierter Stromverbrauch) can be obtained from the SMARD database and downloaded from https://www.smard.de/en. The data are in the public domain and is provided as part of this archive.

7. Actual electricity production data per energy source (Realisierter Erzeugung) can be obtained from the SMARD database and downloaded from https://www.smard.de/en. The data are in the public domain and is provided as part of this archive.

8. Hourly data on wind speed, solar radiation (8.1.) and air temperature (8.2.) can be found on the Deutscher Wetterdienst database (DWD) under the Climate Data Center (wind, solar, and air temperature) from https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/. The data are in the public domain and is provided as part of this archive.

9. Actual electricity production data per plant is sourced from AER and may be obtained by contacting Prof. Dr. Andreas Löschel at andreas.loeschel@rub.de. It can take some months to negotiate data use agreements and gain access to the data. The author will assist with any reasonable replication attempts for two years following publication. A copy of the data is provided as part of this archive and cannot be redistributed. 

10. Data on plant ownership can be found on OPS. A copy of the data is provided as part of this archive. The data are in the public domain. Same raw datafiles as in 1.

11. Data on ramping times for each fossil fuel plant are sourced from Boldt et al (2012).

<!---This datalist is awesome---> 

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

---
# Computational requirements

RStudio 2021.09.0+351 

To install all dependencies described below, run codes at the beginining of the R files and run with the "Ghost Orchid" Release (077589bcad3467ae79f318afe8641a1899a51606, 2021-09-20) for Windows Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) QtWebEngine/5.12.8 Chrome/69.0.3497.128 Safari/537.36
  - tidyverse 
  - lubridate
  - data.table
  - readxl
  - reshape2
  - xts
  - tibbletime
  - writexl
  - dummies
  - caret
  - psych
  - gridExtra
  - pastecs
  - openxlsx
 as of 2022-04-25

Stata/IC 15.1 for Windows (64-bitx86-64), revision 2018-06-06. To install all dependencies described below, run codes at the beginining of the Stata files.
- ivreg2 (as of 2019-11-22, vr. 4.1.11)
- ranktest (as of 2010-09-21, vr. 2.0.04)
- ivreg210 (as of 2015-01-19, vr. 3.1.10)
- xtivreg2 (as of 2015-02-19, vr. 1.0.17)
- estout (as of 2022-03-22, vr. 3.30)

Python 3.8.8 | Anaconda Navigator 2.1.1 |Spyder 4.2.5. To install all dependencies described below, run codes at the beginining of the Python files.
- pyblp 0.12.0
- numpy 1.21.2
- pandas 1.3.4
- scipy 1.7.1

Excel 2018

Random seeds are used at line 657-658 and 664 in R, file SFE.R
Random seeds are used at line 185 in R, file BLP.R
Random seeds are used at "pyblp.Integration" lines in python, files Base_Ramp.py and CF2.py

The approximate time needed to reproduce the analyses on a standard desktop machine (2022) is about X days. Codes were last run on a core i7 10th generation Intel-based laptop with Windows 10 Pro version 21H1 and 16.0 GB of RAM and 476.0 GB of local storage. Computation time took X hours.


---
General Instructions to run codes for "Expansion of Intermittent Renewables: Strategies, pass-through costs, and welfare distribution"

**************************************************************************************

1. Go to file <R_paneldata> run *SFE.R* until line 1041 to obtain files sfe_one.xlsx and sfe_two.xlsx
2. Go to file <Stata> run *SFE.do* using excels in 1. as inputs
3. Return to <R_paneldata> and run lines 1055-1186 to obtain Tables and Figures
4. Run *R_paneldata_BLP* until line 553 to obtain file i1rv.cvs
5. For aditional data for Table 1 run lines 563-569

**************************************************************************************
Use codes in file *BLP_python_**

7. Run each case separately until line 2376 (each case takes around 20 hours)
8. Go to *BLP_python_cf2* and run the entire code, each case separately (similar to step 7)
9. Go to *BLP_python* and run lines 2378-2426 to gather all results in files pt.xlsx, cs.xlsx, curva.xlsx, ps.csv
10. Return to *R_paneldata_BLP* and run lines 576-946 using files in step 9 to obtain Tables and Figures

---
The provided codes reproduce:

| Figure/Table        | Program                                  | Lines          | Output file                    |Note |
| ------------------- |------------------------------------------| ---------------| -------------------------------|---- |
| Figure A1           |- Excel                                   |                | 3.png                          | EEX |
| Figure A2           |- fuel and Co2 costs                      |    a           | Combines data from various sou | yes |
| Figure A3           |- SFE.R                                   | 1050-1075      | FA3                            |     |
| Figure A4           |- SFE.R                                   | 1080-1114      | FA4                            |     |
| Figure A5           |- fuel and Co2 costs                      |    a           | Combines data from various sou | yes |
| Figure A6           |- fuel and Co2 costs                      |    a           | Combines data from various sou | yes |
| Figure A7           |- fuel and Co2 costs                      |    a           | Combines data from various sou | yes |
| Figure A8           |- fuel and Co2 costs                      |    a           | Combines data from various sou | yes |
| Figure A9           |- fuel and Co2 costs                      |    a           | Combines data from various sou | yes |
| Figure A10          |- fuel and Co2 costs                      |    a           | Combines data from various sou | yes |
| Figure A11          |- fuel and Co2 costs                      |    a           | Combines data from various sou | yes |
| Figure A12          |- fuel and Co2 costs                      |    a           | Combines data from various sou | yes |
| Figure A13          |- fuel and Co2 costs                      |    a           | Combines data from various sou | yes |
| Figure A14          |- fuel and Co2 costs                      |    a           | Combines data from various sou | yes |
| Table A1            |- SFE.R                                   | 43-873         | TA1.xlsx                       | Various sources |
|                     |- BLP.R                                   | 31-552         | TA1_blp.xlsx
| Table A2            |- namechp.xlsx                            | sheet Table A2 | -                              | Various sources |
| Table A3            |- I.xlsx                                  | sheet Table A3 | -                              | Various sources |
| Table A4            |- SFE.do +                                | 26-123         | (TA4_1).cvs+ (TA4_2).cvs       |     |
| Table A5            |- SFE.do                                  | 130-266        | (TA5_1,2,3,4,5,6).cvs          |     |
| Table A6            |- fuel and Co2 costs                      |    a           | Combines data from various sou | yes |
| Table A7            |- chp data: consumption and probabilities |    a           | Combines data from SBD and OPS | yes |
| Table A8            |- SFE.R                                   | 1136-1170      | TA8_SFE.xlsx                   |     |

---
References

1. Aurora Energy Research (AER). 2019. "Wind installed capacitites, Electricity production per plant, and fuel costs [dataset]," https://eos.auroraer.com/dragonfly/login, 2019-12-02.
2. Boldt, Jenny, Lisa Hankel, Lilian Laurisch, Felix Lutterbeck, Pao-Yu Oei, Aram Sander, Andreas Schr ̈oder, Helena Schweter, Philipp Sommer, and Jasmin
Sulerz. 2012. “Renewables in the Grid Modeling the German Power Market of the Year 2030.” Electricity Markets Working Papers, 48: 1–91.

---

