
* ===================================================================
* Date: 17.08.21 - last revised 05.05.2022
*
* Paper: Expansion of Intermittent Renewables: Strategies, pass-through costs, and welfare distribution
*
* Period: 020117 to 300918

*run to install dependencies
ssc install ivreg2
ssc install ranktest
ssc install ivreg210
ssc install xtivreg2
ssc install estout

* ===================================================================
* Regressions
* ===================================================================

clear all 

* ===================================================================
*NO RAMPING CASE (1) - Table A4
* ===================================================================

* Define directory, add here own directory "..."
cd "C:\Users\...\Replication\R_paneldata\Output"
import excel using "sfe_one.xlsx", firstrow

cd "C:\Users\...\Replication\Stata"

* Variable generation

gen drd=0
replace drd=1 if rd<0

gen dchp=0

replace dchp=1 if fcost>=1 & fcost< 14.99


encode block, gen(bl)
encode period, gen(semester)
encode week, gen(wk)
encode day, gen(dia)

save regression1, replace 

************************************************************

clear all
use regression1, clear
xtset hour
keep if block == "off" | block == "peak3"
xtivreg2 price dchp drd wk month (fcost ghgc = coal gas coa ), fe r orthog(coa) savefprefix(a1) saverfprefix(a1) 
est store a1

use regression1, clear
xtset hour
keep if block == "peak1" | block == "sppeak1" 
xtivreg2 price dchp drd wk month (fcost ghgc = coal  gas coa ), fe r orthog(coa) savefprefix(b1) saverfprefix(b1) 
est store b1

use regression1, clear
xtset hour
keep if  block == "sppeak2" | block == "peak2"  
xtivreg2 price dchp drd wk month (fcost ghgc = coal  gas coa ), fe r orthog(coa) savefprefix(c1) saverfprefix(c1) 
est store c1

estout* using "(TA4_1).cvs", replace stats(instd  exexog inexog collin r2_a F widstat bw j jp cstat cstatp N) cells(b(star fmt(3)) se(par fmt(2)))


* ===================================================================
*RAMPING CASE (2) - Table A4
* ===================================================================
clear all
* Define directory, add here own directory "..."
cd "C:\Users\...\Replication\R_paneldata\Output"
import excel using "sfe_two.xlsx", firstrow

cd "C:\Users\...\Replication\Stata"

* Variable generation

gen drd=0
replace drd=1 if rd<0

gen dchp=0

replace dchp=1 if fcost>=1 & fcost< 14.99


encode block, gen(bl)
encode period, gen(semester)
encode week, gen(wk)
encode day, gen(dia)


save regression2, replace


* ===================================================================

clear all
use regression2, clear
xtset hour
keep if block == "off" | block == "peak3"
xtivreg2 price wk month (fcost ghgc rampc = coal gas coa ), fe r orthog(coa) savefprefix(a2) saverfprefix(a2) 
est store a2

use regression2, clear
xtset hour
keep if block == "peak1" | block == "sppeak1" 
xtivreg2 price wk month (fcost ghgc rampc = coal  gas coa ), fe r orthog(coa) savefprefix(b2) saverfprefix(b2) 
est store b2

use regression2, clear
xtset hour
keep if  block == "sppeak2" | block == "peak2"  
xtivreg2 price wk month (fcost ghgc rampc = coal  gas coa ), fe r orthog(coa) savefprefix(c2) saverfprefix(c2) 
est store c2

estout* using "(TA4_2).cvs", replace stats(instd  exexog inexog collin r2_a F widstat bw j jp cstat cstatp N) cells(b(star fmt(3)) se(par fmt(2)))


* ===================================================================
*ADDITIONAL REGRESSIONS (1-6) - Table A5 / (1-4) NO RAMPING | (5-6) RAMPING
* ===================================================================

cd "C:\Users\...\Replication\Stata"
clear all
use regression1, clear
xtset hour
keep if block == "off" | block == "peak3"
ivreg2 price dchp drd  (fcost ghgc = coal gas coa ), r orthog(coa) savefprefix(a1) saverfprefix(a1) 
est store a1

use regression1, clear
xtset hour
keep if block == "peak1" | block == "sppeak1" 
ivreg2 price dchp drd  (fcost ghgc = coal gas coa ), r orthog(coa) savefprefix(b1) saverfprefix(b1) 
est store b1

use regression1, clear
xtset hour
keep if  block == "sppeak2" | block == "peak2"  
ivreg2 price dchp drd  (fcost ghgc = coal gas coa ), r orthog(coa) savefprefix(c1) saverfprefix(c1) 
est store c1

estout* using "(TA5_1).cvs", replace stats(instd  exexog inexog collin r2_a F widstat bw j jp cstat cstatp N) cells(b(star fmt(3)) se(par fmt(2)))

* ===================================================================

clear all
use regression1, clear
xtset hour
keep if block == "off" | block == "peak3"
xtivreg2 price dchp drd (fcost ghgc = coal gas coa ), fe r orthog(coa) savefprefix(a2) saverfprefix(a2) 
est store a2

use regression1, clear
xtset hour
keep if block == "peak1" | block == "sppeak1" 
xtivreg2 price dchp drd (fcost ghgc = coal  gas coa ), fe r orthog(coa) savefprefix(b2) saverfprefix(b2) 
est store b2

use regression1, clear
xtset hour
keep if  block == "sppeak2" | block == "peak2"  
xtivreg2 price dchp drd (fcost ghgc = coal  gas coa ), fe r orthog(coa) savefprefix(c2) saverfprefix(c2) 
est store c2

estout* using "(TA5_2).cvs", replace stats(instd  exexog inexog collin r2_a F widstat bw j jp cstat cstatp N) cells(b(star fmt(3)) se(par fmt(2)))

* ===================================================================

clear all
use regression1, clear
xtset hour
keep if block == "off" | block == "peak3"
xtivreg2 price muwind wk month (fcost ghgc = coal gas coa ), fe r orthog(coa) savefprefix(a3) saverfprefix(a3) 
est store a3

use regression1, clear
xtset hour
keep if block == "peak1" | block == "sppeak1" 
xtivreg2 price muwind wk month (fcost ghgc = coal  gas coa ), fe r orthog(coa) savefprefix(b3) saverfprefix(b3) 
est store b3

use regression1, clear
xtset hour
keep if  block == "sppeak2" | block == "peak2"  
xtivreg2 price muwind wk month (fcost ghgc = coal  gas coa ), fe r orthog(coa) savefprefix(c3) saverfprefix(c3) 
est store c3

estout* using "(TA5_3).cvs", replace stats(instd  exexog inexog collin r2_a F widstat bw j jp cstat cstatp N) cells(b(star fmt(3)) se(par fmt(2)))

* ===================================================================

clear all
use regression1, clear
xtset hour
keep if block == "off" | block == "peak3"
xtivreg2 price musolar wk month (fcost ghgc = coal gas coa ), fe r orthog(coa) savefprefix(a4) saverfprefix(a4) 
est store a4

use regression1, clear
xtset hour
keep if block == "peak1" | block == "sppeak1" 
xtivreg2 price musolar wk month (fcost ghgc = coal  gas coa ), fe r orthog(coa) savefprefix(b4) saverfprefix(b4) 
est store b4

use regression1, clear
xtset hour
keep if  block == "sppeak2" | block == "peak2"  
xtivreg2 price musolar wk month (fcost ghgc = coal  gas coa ), fe r orthog(coa) savefprefix(c4) saverfprefix(c4) 
est store c4

estout* using "(TA5_4).cvs", replace stats(instd  exexog inexog collin r2_a F widstat bw j jp cstat cstatp N) cells(b(star fmt(3)) se(par fmt(2)))

* ===================================================================

clear all
use regression2, clear
xtset hour
keep if block == "off" | block == "peak3"
ivreg2 price (fcost ghgc rampc = coal gas coa ), r orthog(coa) savefprefix(a5) saverfprefix(a5) 
est store a5

use regression2, clear
xtset hour
keep if block == "peak1" | block == "sppeak1" 
ivreg2 price (fcost ghgc rampc = coal gas coa ), r orthog(coa) savefprefix(b5) saverfprefix(b5) 
est store b5

use regression2, clear
xtset hour
keep if  block == "sppeak2" | block == "peak2"  
ivreg2 price  (fcost ghgc rampc = coal gas coa ), r orthog(coa) savefprefix(c5) saverfprefix(c5) 
est store c5

estout* using "(TA5_5).cvs", replace stats(instd  exexog inexog collin r2_a F widstat bw j jp cstat cstatp N) cells(b(star fmt(3)) se(par fmt(2)))


* ===================================================================

clear all
use regression2, clear
xtset hour
keep if block == "off" | block == "peak3"
xtivreg2 price (fcost ghgc rampc = coal gas coa), fe r orthog(coa) savefprefix(a6) saverfprefix(a6) 
est store a6

use regression2, clear
xtset hour
keep if block == "peak1" | block == "sppeak1" 
xtivreg2 price (fcost ghgc rampc = coal gas coa ), fe r orthog(coa) savefprefix(b6) saverfprefix(b6) 
est store b6

use regression2, clear
xtset hour
keep if  block == "sppeak2" | block == "peak2"  
xtivreg2 price (fcost ghgc rampc = coal gas coa ), fe r orthog(coa) savefprefix(c6) saverfprefix(c6) 
est store c6

estout* using "(TA5_6).cvs", replace stats(instd  exexog inexog collin r2_a F widstat bw j jp cstat cstatp N) cells(b(star fmt(3)) se(par fmt(2)))



* ===================================================================
*ADDITIONAL REGRESSIONS - TOTAL COSTS
* ===================================================================

cd "C:\Users\...\Replication\Stata"

clear all
use regression2, clear
xtset hour
keep if block == "off" | block == "peak3"
xtivreg2 price wk month (mumc = coal gas coa ), fe r orthog(coa) savefprefix(a2) saverfprefix(a2) 
est store a2

use regression2, clear
xtset hour
keep if block == "peak1" | block == "sppeak1" 
xtivreg2 price wk month (mumc = coal  gas coa ), fe r orthog(coa) savefprefix(b2) saverfprefix(b2) 
est store b2

use regression2, clear
xtset hour
keep if  block == "sppeak2" | block == "peak2"  
xtivreg2 price wk month (mumc = coal  gas coa ), fe r orthog(coa) savefprefix(c2) saverfprefix(c2) 
est store c2

estout* using "(TOTAL).cvs", replace stats(instd  exexog inexog collin r2_a F widstat bw j jp cstat cstatp N) cells(b(star fmt(3)) se(par fmt(2)))

* ===================================================================
* no Dummies of time (ndum)

clear all
use regression2, clear
xtset hour
keep if block == "off" | block == "peak3"
xtivreg2 price  (mumc = coal gas coa ), fe r orthog(coa) savefprefix(a2) saverfprefix(a2) 
est store a2

use regression2, clear
xtset hour
keep if block == "peak1" | block == "sppeak1" 
xtivreg2 price (mumc = coal  gas coa ), fe r orthog(coa) savefprefix(b2) saverfprefix(b2) 
est store b2

use regression2, clear
xtset hour
keep if  block == "sppeak2" | block == "peak2"  
xtivreg2 price  (mumc = coal  gas coa ), fe r orthog(coa) savefprefix(c2) saverfprefix(c2) 
est store c2

estout* using "(TOTAL_ndum).cvs", replace stats(instd  exexog inexog collin r2_a F widstat bw j jp cstat cstatp N) cells(b(star fmt(3)) se(par fmt(2)))


