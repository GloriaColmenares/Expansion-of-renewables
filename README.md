# Expansion-of-renewables
Codes for "Expansion of Intermittent Renewables: Strategies, pass-through costs, and welfare distribution"

**************************************************************************************
Use codes in file *R_paneldata_**
**************************************************************************************
1. Download all files [here](https://mega.nz/folder/eckSFQYK#smoBcJIp-vna-XvnHdCZQg)
2. Run *R_paneldata_SFE* until line 1041 to obtain files sfe_one.xlsx and sfe_two.xlsx
3. Run Stata do file in *Stata_SFE* using excel files in 2.
4. Return to *R_paneldata_SFE* and run lines 1055-1186 to obtain Tables and Figures
5. Run *R_paneldata_BLP* until line 553 to obtain file i1rv.cvs
6. For aditional data for Table 1 run lines 563-569

**************************************************************************************
Use codes in file *BLP_python_**
**************************************************************************************
7. Run each case separately until line 2376 (each case takes around 20 hours)
8. Go to *BLP_python_cf2* and run the entire code, each case separately (similar to 7).
9. Go to *BLP_python* and run lines 2378-2426 to gather all results in files pt.xlsx, cs.xlsx, curva.xlsx, ps.csv
10. Return to *R_paneldata_BLP* and run lines 576-946 using files in 9 to obtain Tables and Figures
