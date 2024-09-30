**This file replicates all of the tables and figures in both the paper and appendix for Michler et al. (2019).
**To run the code, one must install ivreg2, ranktest, and coefplot.

set matsize 11000

use "Michler_et_al_JEEM_data.dta", clear

****************************************************
****Figure A1: Frequency by crop and year***********
****************************************************

graph hbar (count), over(crops) over(year) ///
	asyvars bar(1, color(navy)) bar(2, color(maroon)) ///
	bar(3, color(forest_green)) bar(4, color(teal)) ///
	bar(5, color(dkorange)) ytitle("Frequency") ///
	blabel(bar, format(%6.1gc))

graph export "frequency.eps", as(eps) replace


****************************************************
****Figure A1: Percentage of crop in each year******
****************************************************

graph hbar, percentages over(crops) over(year) ///
	asyvars bar(1, color(navy)) bar(2, color(maroon)) ///
	bar(3, color(forest_green)) bar(4, color(teal)) ///
	bar(5, color(dkorange)) ytitle("Percentage") ///
	blabel(bar, format(%4.1gc))

graph export "percentage.eps", as(eps) replace


****************************************************
****Figure A3: Distribution of yields by year*******
****************************************************

*All Crops
twoway (kdensity lnyield if year == 2008, color(gray%30) recast(area)) ///
		(kdensity lnyield if year == 2009, color(vermillion%30) recast(area)) ///
		(kdensity lnyield if year == 2010, color(turquoise%30) recast(area)) ///
		(kdensity lnyield if year == 2011, color(sky%30) recast(area) ///
		ytitle("ln(yield)") ylabel(, nogrid labsize(small)) yscale(r(0 0.4)) ylabel(0 0.2 0.4, labsize(small)) ///
		xlabel(, nogrid labsize(small)) xscale(r(0 10)) xlabel(0 5 10, labsize(small))), ///
		title("All Crops") legend( col(1) label(1 "2008") label(2 "2009") ///
		label(3 "2010") label(4 "2011")) saving(alny, replace)

*Maize
twoway (kdensity lnyield if year == 2008 & crops == 1, color(gray%30) recast(area)) ///
		(kdensity lnyield if year == 2009 & crops == 1, color(vermillion%30) recast(area)) ///
		(kdensity lnyield if year == 2010 & crops == 1, color(turquoise%30) recast(area)) ///
		(kdensity lnyield if year == 2011 & crops == 1, color(sky%30) recast(area) ///
		ytitle("ln(yield)") ylabel(, nogrid labsize(small)) yscale(r(0 0.4)) ylabel(0 0.2 0.4, labsize(small)) ///
		xlabel(, nogrid labsize(small)) xscale(r(0 10)) xlabel(0 5 10, labsize(small))), ///
		title("Maize") legend( col(1) label(1 "2008") label(2 "2009") ///
		label(3 "2010") label(4 "2011")) saving(mlny, replace)

*Sorghum
twoway (kdensity lnyield if year == 2008 & crops == 2, color(gray%30) recast(area)) ///
		(kdensity lnyield if year == 2009 & crops == 2, color(vermillion%30) recast(area)) ///
		(kdensity lnyield if year == 2010 & crops == 2, color(turquoise%30) recast(area)) ///
		(kdensity lnyield if year == 2011 & crops == 2, color(sky%30) recast(area) ///
		ytitle("ln(yield)") ylabel(, nogrid labsize(small)) yscale(r(0 0.4)) ylabel(0 0.2 0.4, labsize(small)) ///
		xlabel(, nogrid labsize(small)) xscale(r(0 10)) xlabel(0 5 10, labsize(small))), ///
		title("Sorghum") legend( col(1) label(1 "2008") label(2 "2009") ///
		label(3 "2010") label(4 "2011")) saving(slny, replace)

*Millet
twoway (kdensity lnyield if year == 2008 & crops == 4, color(gray%30) recast(area)) ///
		(kdensity lnyield if year == 2009 & crops == 4, color(vermillion%30) recast(area)) ///
		(kdensity lnyield if year == 2010 & crops == 4, color(turquoise%30) recast(area)) ///
		(kdensity lnyield if year == 2011 & crops == 4, color(sky%30) recast(area) ///
		ytitle("ln(yield)") ylabel(, nogrid labsize(small)) yscale(r(0 0.4)) ylabel(0 0.2 0.4, labsize(small)) ///
		xlabel(, nogrid labsize(small)) xscale(r(0 10)) xlabel(0 5 10, labsize(small))), ///
		title("Millet") legend( col(1) label(1 "2008") label(2 "2009") ///
		label(3 "2010") label(4 "2011")) saving(milny, replace)

*Groundnut
twoway (kdensity lnyield if year == 2008 & crops == 5, color(gray%30) recast(area)) ///
		(kdensity lnyield if year == 2009 & crops == 5, color(vermillion%30) recast(area)) ///
		(kdensity lnyield if year == 2010 & crops == 5, color(turquoise%30) recast(area)) ///
		(kdensity lnyield if year == 2011 & crops == 5, color(sky%30) recast(area) ///
		ytitle("ln(yield)") ylabel(, nogrid labsize(small)) yscale(r(0 0.4)) ylabel(0 0.2 0.4, labsize(small)) ///
		xlabel(, nogrid labsize(small)) xscale(r(0 10)) xlabel(0 5 10, labsize(small))), ///
		title("Goundnut") legend(col(1) label(1 "2008") label(2 "2009") ///
		label(3 "2010") label(4 "2011")) saving(glny, replace)

*Cowpea
twoway (kdensity lnyield if year == 2008 & crops == 6, color(gray%30) recast(area)) ///
		(kdensity lnyield if year == 2009 & crops == 6, color(vermillion%30) recast(area)) ///
		(kdensity lnyield if year == 2010 & crops == 6, color(turquoise%30) recast(area)) ///
		(kdensity lnyield if year == 2011 & crops == 6, color(sky%30) recast(area) ///
		ytitle("ln(yield)") ylabel(, nogrid labsize(small)) yscale(r(0 0.4)) ylabel(0 0.2 0.4, labsize(small)) ///
		xlabel(, nogrid labsize(small)) xscale(r(0 10)) xlabel(0 5 10, labsize(small))), ///
		title("Cowpea") legend( col(1) label(1 "2008") label(2 "2009") ///
		label(3 "2010") label(4 "2011")) saving(clny, replace)

gr combine alny.gph mlny.gph slny.gph milny.gph glny.gph clny.gph, col(2) iscale(.5) commonscheme
		
graph export "nlydist.png", as(png) replace


****************************************************
****Table 1: Summary Stats by crop******************
****************************************************


label var yieldkgha "Yield (kg/ha)"
label var CA "CA $(=1)$"
label var basalkg "Basal applied fertilizer (kg)"
label var topkg "Top applied fertilizer (kg)"
label var seedkg "Seed (kg)"
label var aream2 "Area (m$^2$)"
label var shock "Rainfall shock"
label var wardNGO "HH in ward with NGO support"

estpost tabstat yieldkgha yieldkgha, by(crops) ///
	statistics(mean sd) columns(statistics) listwise

	esttab . using table1.tex, replace ///
	main(mean) aux(sd) nostar unstack label booktabs nonum collabels(none) f noobs ///
	cells(mean(fmt(1)) sd(fmt(1)par))

estpost tabstat CA basalkg topkg seedkg, by(crops) ///
	statistics(mean sd) columns(statistics) listwise

	esttab . using table1.tex, append ///
	main(mean) aux(sd) nostar unstack label booktabs nonum collabels(none) f noobs ///
	cells(mean(fmt(3)) sd(fmt(3)par))

estpost tabstat aream2 aream2, by(crops) ///
	statistics(mean sd) columns(statistics) listwise

	esttab . using table1.tex, append ///
	main(mean) aux(sd) nostar unstack label booktabs nonum collabels(none) f noobs ///
	cells(mean(fmt(0)) sd(fmt(0)par))
	
estpost tabstat shock shock, by(crops) ///
	statistics(mean sd) columns(statistics) listwise

	esttab . using table1.tex, append ///
	main(mean) aux(sd) nostar unstack label booktabs nonum collabels(none) f noobs ///
	cells(mean(fmt(3)) sd(fmt(3)par))

estpost tabstat wardNGO wardNGO, by(crops) ///
	statistics(mean sd) columns(statistics) listwise

	esttab . using table1.tex, append ///
	main(mean) aux(sd) nostar unstack label booktabs nonum collabels(none) f noobs ///
	cells(mean(fmt(2)) sd(fmt(2)par))

	
****************************************************
****Table 2: Summary Stats by year*****************
****************************************************


estpost tabstat yieldkgha yieldkgha, by(year) nototal ///
	statistics(mean sd) columns(statistics) listwise

	esttab . using table2.tex, replace ///
	main(mean) aux(sd) nostar unstack label booktabs nonum collabels(none) f noobs ///
	cells(mean(fmt(1)) sd(fmt(1)par))

estpost tabstat CA basalkg topkg seedkg, by(year) nototal ///
	statistics(mean sd) columns(statistics) listwise

	esttab . using table2.tex, append ///
	main(mean) aux(sd) nostar unstack label booktabs nonum collabels(none) f noobs ///
	cells(mean(fmt(3)) sd(fmt(3)par))

estpost tabstat aream2 aream2, by(year) nototal ///
	statistics(mean sd) columns(statistics) listwise

	esttab . using table2.tex, append ///
	main(mean) aux(sd) nostar unstack label booktabs nonum collabels(none) f noobs ///
	cells(mean(fmt(0)) sd(fmt(0)par))

estpost tabstat shock shock, by(year) nototal ///
	statistics(mean sd) columns(statistics) listwise

	esttab . using table2.tex, append ///
	main(mean) aux(sd) nostar unstack label booktabs nonum collabels(none) f noobs ///
	cells(mean(fmt(3)) sd(fmt(3)par))

estpost tabstat wardNGO wardNGO, by(year) nototal ///
	statistics(mean sd) columns(statistics) listwise

	esttab . using table2.tex, append ///
	main(mean) aux(sd) nostar unstack label booktabs nonum collabels(none) f noobs ///
	cells(mean(fmt(2)) sd(fmt(2)par))
	
by year, sort: distinct plot_id rc ward_id


****************************************************
****Table 3: Summary Stats by cultivation***********
****************************************************


by crops CA, sort: sum yieldkgha basalkg topkg seedkg aream2 shock wardNGO, separator(7)

sort CA
*Tests of equality

*Frist test for normality
levelsof crops, local(levels) 
foreach v of local levels {
	by CA, sort : swilk yieldkgha if crops==`v'
	}
levelsof crops, local(levels) 
foreach v of local levels {
	by CA, sort : swilk basalkg if crops==`v'
	}
levelsof crops, local(levels) 
foreach v of local levels {
	by CA, sort : swilk topkg if crops==`v'
	}
levelsof crops, local(levels) 
foreach v of local levels {
	by CA, sort : swilk seedkg if crops==`v'
	}
levelsof crops, local(levels) 
foreach v of local levels {
	by CA, sort : swilk aream2 if crops==`v'
	}
levelsof crops, local(levels) 
foreach v of local levels {
	by CA, sort : swilk shock if crops==`v'
	}
levelsof crops, local(levels) 
foreach v of local levels {
	by CA, sort : swilk wardNGO if crops==`v'
	}
	
*We reject normality for all
levelsof crops, local(levels) 
foreach v of local levels {
	ranksum yieldkgha if crops==`v', by(CA)
	}
levelsof crops, local(levels) 
foreach v of local levels {
	ranksum basalkg if crops==`v', by(CA)
	}
levelsof crops, local(levels) 
foreach v of local levels {
	ranksum topkg if crops==`v', by(CA)
	}
levelsof crops, local(levels) 
foreach v of local levels {
	ranksum seedkg if crops==`v', by(CA)
	}
levelsof crops, local(levels) 
foreach v of local levels {
	ranksum aream2 if crops==`v', by(CA)
	}
levelsof crops, local(levels) 
foreach v of local levels {
	ranksum shock if crops==`v', by(CA)
	}
levelsof crops, local(levels) 
foreach v of local levels {
	ranksum wardNGO if crops==`v', by(CA)
	}
		
by crops CA, sort: distinct plot_id rc ward_id 


****************************************************
****Figure 2: Average Adoption**********************
****************************************************


by crops year, sort: egen CA_bar = mean(CA)
by year, sort: egen sub_bar = mean(wardNGO/nhhs_ward)
by year, sort: egen CA_mean = mean(CA)
by year crop, sort: egen yield_bar = mean(yieldkgha)
sort year

twoway 	(line CA_bar year if crops==1, lcolor(navy) lpattern(solid)) || (scatter CA_bar year if crops==1, mcolor(navy)) || ///
		(line CA_bar year if crops==2, lcolor(maroon) lpattern(longdash_dot)) || (scatter CA_bar year if crops==2, mcolor(maroon)) || ///
		(line CA_bar year if crops==4, lcolor(forest_green) lpattern(dash)) || (scatter CA_bar year if crops==4, mcolor(forest_green)) || ///
		(line CA_bar year if crops==5, lcolor(teal) lpattern("--..")) || (scatter CA_bar year if crops==5, mcolor(teal)) || ///
		(line CA_bar year if crops==6, lcolor(dkorange) lpattern(shortdash_dot)) || (scatter CA_bar year if crops==6, mcolor(dkorange) ///
		xtitle("Year") ytitle("Average Adoption Rate of CA")  ylabel(, nogrid labsize(small))  xlabel(, nogrid labsize(small))), ///
		legend(order(1 3 5 7 9) label(1 "Maize") label(3 "Sorghum") label(5 "Millet") label(7 "Groundnut") ///
		label(9 "Cowpea") position(6) col(3)  )

graph export "mean_CA.eps", as(eps) replace


****************************************************
****Figure 5: Kdensity of yields********************
****************************************************


sum shock
gen norm = 1 if shock < (.4759452 + .2737359) & shock > (.4759452 - .2737359)
replace norm = 0 if norm==.

twoway (kdensity lnyield if CA==1 & norm==1, ytitle("Density") lcolor(navy) ///
lpattern(solid) lwidth(medthick) )  || (kdensity lnyield if CA==0 & norm==1, ///
xtitle("Ln Yield (kg/ha)") lcolor(maroon) lpattern(longdash_dot) lwidth(medthick) ), ///
legend(label(1 "CA normal rainfall") label(2 "TC normal rainfall") position(6) ///
rows(1)) title("") title("Yields During Normal Rainfall") saving(norm, replace)

twoway (kdensity lnyield if CA==1 & norm==0, ytitle("Density") lcolor(forest_green) ///
lpattern(dash) lwidth(medthick) )  || (kdensity lnyield if CA==0 & norm==0, ///
xtitle("Ln Yield (kg/ha)") lcolor(dkorange) lpattern("--..") lwidth(medthick) ), ///
legend(label(1 "CA abnormal rainfall") label(2 "TC abnormal rainfall") position(6) ///
rows(1)) title("") title("Yields During Abnormal Rainfall") saving(abnorm, replace)

twoway (kdensity lnyield if CA==1 & norm==1, ytitle("Density") lcolor(navy) ///
lpattern(solid) lwidth(medthick) )  || (kdensity lnyield if CA==1 & norm==0, ///
xtitle("Ln Yield (kg/ha)") lcolor(forest_green) lpattern(dash) lwidth(medthick) ), ///
legend(label(1 "CA normal rainfall") label(2 "CA abnormal rainfall") position(6) ///
rows(1)) title("")  title("Yields Using Conservation Agriculture") saving(CA, replace)

twoway (kdensity lnyield if CA==0 & norm==1, ytitle("Density") lcolor(maroon) ///
lpattern(longdash_dot) lwidth(medthick) )  || (kdensity lnyield if CA==0 & norm==0, ///
xtitle("Ln Yield (kg/ha)") lcolor(dkorange) lpattern("--..") lwidth(medthick) ), ///
legend(label(1 "TC normal rainfall") label(2 "TC abnormal rainfall") position(6) ///
rows(1)) title("")  title("Yields Using Traditional Cultivation") saving(TC, replace)

gr combine norm.gph abnorm.gph CA.gph TC.gph, col(2) ycommon xcommon iscale(.5) commonscheme

graph export "4way.eps", as(eps) replace

ksmirnov lnyield if norm==1, by(CA)
ksmirnov lnyield if norm==0, by(CA)

ksmirnov lnyield if CA==1, by(norm)
ksmirnov lnyield if CA==0, by(norm)


****************************************************
****Figure 6: Yield Response Curves*****************
****************************************************


**CA-Input use
twoway 	(scatter yieldkgha seedkgha if CA==0, mcolor(orangebrown%25) msize(vsmall) msymbol(circle) mlwidth(none)) || ///
		(scatter yieldkgha seedkgha if CA == 1, mcolor(sea%25) msize(vsmall) msymbol(D) mlwidth(none)) || ///
		(qfitci yieldkgha seedkgha if CA ==0, level(90) lcolor(orangebrown%70) clpattern(solid) fcolor(gs15%70) alcolor(orangebrown%70)) || ///
		(qfitci yieldkgha seedkgha if CA ==1, level(90) lcolor(sea%70) clpattern(dash) fcolor(gs15%70) alcolor(sea%70) ///
		xtitle("Seed (kg/ha)") xlabel(, nogrid) ytitle("Yield (kg/ha)") ylabel(, nogrid)), title("(1)") ///
		legend(order(5 3) label(3 "TC") label(5 "CA") col(2) pos(6)) saving(seed, replace)

twoway 	(scatter yieldkgha basalkgha if CA==0, mcolor(orangebrown%25) msize(vsmall) msymbol(circle) mlwidth(none)) || ///
		(scatter yieldkgha basalkgha if CA == 1, mcolor(sea%25) msize(vsmall) msymbol(D) mlwidth(none)) || ///
		(qfitci yieldkgha basalkgha if CA ==0, level(90) lcolor(orangebrown%50) clpattern(solid) fcolor(gs15%50) alcolor(orangebrown%50)) || ///
		(qfitci yieldkgha basalkgha if CA ==1, level(90) lcolor(sea%50) clpattern(dash) fcolor(gs15%50) alcolor(sea%50) ///
		xtitle("Basal Fertilizer (kg/ha)") xlabel(, nogrid) ytitle("Yield (kg/ha)") ylabel(, nogrid)), title("(2)") ///
		legend(order(5 3) label(3 "TC") label(5 "CA") col(2) pos(6)) saving(basal, replace)

twoway 	(scatter yieldkgha topkgha if CA==0, mcolor(orangebrown%25) msize(vsmall) msymbol(circle) mlwidth(none)) || ///
		(scatter yieldkgha topkgha if CA == 1, mcolor(sea%25) msize(vsmall) msymbol(D) mlwidth(none)) || ///
		(qfitci yieldkgha topkgha if CA ==0, level(90) lcolor(orangebrown%50) clpattern(solid) fcolor(gs15%50) alcolor(orangebrown%50)) || ///
		(qfitci yieldkgha topkgha if CA ==1, level(90) lcolor(sea%50) clpattern(dash) fcolor(gs15%50) alcolor(sea%50) ///
		xtitle("Top Fertilizer (kg/ha)") xlabel(, nogrid) ytitle("Yield (kg/ha)") ylabel(, nogrid)), title("(3)") ///
		legend(order(5 3) label(3 "TC") label(5 "CA") col(2) pos(6)) saving(top, replace)

**Yield-Input graphs control for Ward
reg lnyield i.ward_id
	predict yhat, residuals

reg lnseed i.ward_id
	predict shat, residuals

reg lnbasal i.ward_id
	predict bhat, residuals
	
reg lntop i.ward_id
	predict that, residuals

twoway 	(scatter yhat shat if CA==0, mcolor(orangebrown%25) msize(vsmall) msymbol(circle) mlwidth(none)) || ///
		(scatter yhat shat if CA == 1, mcolor(sea%25) msize(vsmall) msymbol(D) mlwidth(none)) || ///
		(qfitci yhat shat if CA ==0, level(90) lcolor(orangebrown%50) clpattern(solid) fcolor(gs15%50) alcolor(orangebrown%50)) || ///
		(qfitci yhat shat if CA ==1, level(90) lcolor(sea%50) clpattern(dash) fcolor(gs15%50) alcolor(sea%50) ///
		xtitle("ln(Seed) (kg/ha)") xlabel(, nogrid) ytitle("Yield (kg/ha)") ylabel(, nogrid)), title("(4)") ///
		legend(order(5 3) label(3 "TC") label(5 "CA") col(2) pos(6)) saving(shat, replace)

twoway 	(scatter yhat bhat if CA==0, mcolor(orangebrown%25) msize(vsmall) msymbol(circle) mlwidth(none)) || ///
		(scatter yhat bhat if CA == 1, mcolor(sea%25) msize(vsmall) msymbol(D) mlwidth(none)) || ///
		(qfitci yhat bhat if CA ==0, level(90) lcolor(orangebrown%50) clpattern(solid) fcolor(gs15%50) alcolor(orangebrown%50)) || ///
		(qfitci yhat bhat if CA ==1, level(90) lcolor(sea%50) clpattern(dash) fcolor(gs15%50) alcolor(sea%50) ///
		xtitle("ln(Basal Fert) (kg/ha)") xlabel(, nogrid) ytitle("Yield (kg/ha)") ylabel(, nogrid)), title("(5)") ///
		legend(order(5 3) label(3 "TC") label(5 "CA") col(2) pos(6)) saving(bhat, replace)
		
twoway 	(scatter yhat that if CA==0, mcolor(orangebrown%25) msize(vsmall) msymbol(circle) mlwidth(none)) || ///
		(scatter yhat that if CA == 1, mcolor(sea%25) msize(vsmall) msymbol(D) mlwidth(none)) || ///
		(qfitci yhat that if CA ==0, level(90) lcolor(orangebrown%50) clpattern(solid) fcolor(gs15%50) alcolor(orangebrown%50)) || ///
		(qfitci yhat that if CA ==1, level(90) lcolor(sea%50) clpattern(dash) fcolor(gs15%50) alcolor(sea%50) ///
		xtitle("ln(Top Fert) (kg/ha)") xlabel(, nogrid) ytitle("Yield (kg/ha)") ylabel(, nogrid)), title("(6)") ///
		legend(order(5 3) label(3 "TC") label(5 "CA") col(2) pos(6)) saving(that, replace)

gr combine seed.gph basal.gph top.gph shat.gph bhat.gph that.gph, col(3) iscale(.5) commonscheme

graph export "input_resp.png", as(png) replace


****************************************************
****Figure 7: Yield, CA, and Inputs*****************
****************************************************


replace sub_bar = sub_bar*100
replace CA_mean = CA_mean*100

twoway 	(line yield_bar year if crop == 1, lcolor(forest_green) lpattern(dash)) || (scatter yield_bar year if crop ==1, mcolor(forest_green)) || ///
		(line sub_bar year, lcolor(navy) lpattern(solid) yaxis(2)) || (scatter sub_bar year, mcolor(navy) yaxis(2)) || ///
		(line CA_mean year, lcolor(maroon) lpattern(longdash_dot) yaxis(2)) || (scatter CA_mean year , mcolor(maroon) yaxis(2) ///
		xtitle("Year") ytitle("Percentage", axis(2)) ytitle("Yields (kg/ha)") ylabel(, nogrid labsize(small))  xlabel(, nogrid labsize(small))), ///
		legend(order(1 3 5) label(1 "Maize Yield") label(3 "Input Subsidies") label(5 "CA Adoption") col(3) pos(6))

graph export "CA_sub.eps", as(eps) replace


****************************************************
****Table 4: Input Change Over Time*****************
****************************************************


use "Michler_et_al_JEEM_data.dta", clear

duplicates tag plot_id year, gen(pcount)
tab pcount

keep if pcount==0
drop pcount

duplicates tag plot_id, generate(pcount)
tab pcount

drop if pcount==0

sort plot_id year

*replace in terms of per hectare units
replace lnbasal = asinh(basalkgha)
replace lntop = asinh(topkgha)
replace lnseed = asinh(seedkgha)

*Generate wide data set
keep rc- crc
reshape wide rc CA- plot crop- crc, i(plot_id) j(year)

gen always_adopt = 1 if CA2008 == 1 & CA2009 == 1 & CA2010 == 1 & CA2011 == 1
replace always_adopt = 1 if CA2008 == 1 | CA2008 == . & CA2009 == 1 | CA2009 == . & CA2010 == 1 | CA2010 == . & CA2011 == 1 | CA2011 == . 
replace always_adopt = . if CA2008 == 0 | CA2009 == 0 | CA2010 == 0 | CA2011 == 0

gen never_adopt = 1 if CA2008 == 0 & CA2009 == 0 & CA2010 == 0 & CA2011 == 0
replace never_adopt = 1 if CA2008 == 0 | CA2008 == . & CA2009 == 0 | CA2009 == . & CA2010 == 0 | CA2010 == . & CA2011 == 0 | CA2011 == . 
replace never_adopt = . if CA2008 == 1 | CA2009 == 1 | CA2010 == 1 | CA2011 == 1

gen adopter = 1
replace adopter = . if CA2008 == 1
replace adopter = . if CA2011 == 0
replace adopter = . if always_adopt == 1 | never_adopt == 1
replace adopter = . if CA2008 == 1 & CA2009 == 0
replace adopter = . if CA2008 == 1 & CA2010 == 0
replace adopter = . if CA2008 == 1 & CA2011 == 0
replace adopter = . if CA2009 == 1 & CA2010 == 0
replace adopter = . if CA2009 == 1 & CA2011 == 0
replace adopter = . if CA2010 == 1 & CA2011 == 0

gen disadopter = 1
replace disadopter = . if CA2008 == 0
replace disadopter = . if CA2011 == 1
replace disadopter = . if always_adopt == 1 | never_adopt == 1 | adopter == 1
replace disadopter = . if CA2008 == 0 & CA2009 == 1
replace disadopter = . if CA2008 == 0 & CA2010 == 1
replace disadopter = . if CA2008 == 0 & CA2011 == 1
replace disadopter = . if CA2009 == 0 & CA2010 == 1
replace disadopter = . if CA2009 == 0 & CA2011 == 1
replace disadopter = . if CA2010 == 0 & CA2011 == 1

order adopter disadopter never_adopt, after(always_adopt)
mvencode always_adopt-never_adopt, mv(0)
			
*Always adopt versus dis-adopters
gen always_dis = 1 if always_adopt == 1
replace always_dis = 0 if disadopter == 1

gen never_ad = 1 if never_adopt == 1
replace never_ad = 0 if adopter == 1

**Remove zero yield or fert observations
replace lnyield2008 = . if lnyield2008 == 0
replace lnbasal2008 = . if lnbasal2008 == 0
replace lntop2008 = . if lntop2008 == 0
replace lnaream22008 = . if lnaream22008 == 0

replace lnyield2011 = . if lnyield2011 == 0
replace lnbasal2011 = . if lnbasal2011 == 0
replace lntop2011 = . if lntop2011 == 0
replace lnaream22011 = . if lnaream22011 == 0

*Regressions
reg lnyield2008 i.ward_id2008
	predict yhat2008, residuals

reg lnyield2011 i.ward_id2011
	predict yhat2011, residuals

reg lnseed2008 i.ward_id2008
	predict shat2008, residuals

reg lnseed2011 i.ward_id2011
	predict shat2011, residuals
	
reg lnbasal2008 i.ward_id2008
	predict bhat2008, residuals

reg lnbasal2011 i.ward_id2011
	predict bhat2011, residuals
	
reg lntop2008 i.ward_id2008
	predict that2008, residuals

reg lntop2011 i.ward_id2011
	predict that2011, residuals
	
reg lnaream22008 i.ward_id2008
	predict ahat2008, residuals

reg lnaream22011 i.ward_id2011
	predict ahat2011, residuals
	
*Predicted differences based on Ward FE

by always_dis, sort: sum yhat2008 shat2008 bhat2008 that2008 ahat2008
ranksum yhat2008, by(always_dis)
ranksum shat2008, by(always_dis)
ranksum bhat2008, by(always_dis)
ranksum that2008, by(always_dis)
ranksum ahat2008, by(always_dis)

by always_dis, sort: sum yhat2011 shat2011 bhat2011 that2011 ahat2011 
ranksum yhat2011, by(always_dis)
ranksum shat2011, by(always_dis)
ranksum bhat2011, by(always_dis)
ranksum that2011, by(always_dis)
ranksum ahat2011, by(always_dis)

by never_ad, sort: sum yhat2008 shat2008 bhat2008 that2008 ahat2008
ranksum yhat2008, by(never_ad)
ranksum shat2008, by(never_ad)
ranksum bhat2008, by(never_ad)
ranksum that2008, by(never_ad)
ranksum ahat2008, by(never_ad)

by never_ad, sort: sum yhat2011 shat2011 bhat2011 that2011 ahat2011
ranksum yhat2011, by(never_ad)
ranksum shat2011, by(never_ad)
ranksum bhat2011, by(never_ad)
ranksum that2011, by(never_ad)
ranksum ahat2011, by(never_ad)


****************************************************
****Table 5: All crops, CA exogenous****************
****************************************************

use "Michler_et_al_JEEM_data.dta", clear

*CA, without rain, without households fixed effects
local maize CAcroXCA_1 BcroXlnbas_1 TcroXlntop_1 ScroXlnsee_1 AcroXlnare_1 
local sorghum CAcroXCA_2 BcroXlnbas_2 TcroXlntop_2 ScroXlnsee_2 AcroXlnare_2 
local millet CAcroXCA_4 BcroXlnbas_4 TcroXlntop_4 ScroXlnsee_4 AcroXlnare_4 
local gnut CAcroXCA_5 BcroXlnbas_5 TcroXlntop_5 ScroXlnsee_5 AcroXlnare_5 
local ppea CAcroXCA_6 BcroXlnbas_6 TcroXlntop_6 ScroXlnsee_6 AcroXlnare_6 

xi: reg lnyield `maize' `sorghum' `millet' `gnut' `ppea' crop_type1- crop_type5 i.year, noconstant vce(cluster crc)

est store CA

*CA, without rain, with households fixed effects
local maize CAcroXCA_1 BcroXlnbas_1 TcroXlntop_1 ScroXlnsee_1 AcroXlnare_1 
local sorghum CAcroXCA_2 BcroXlnbas_2 TcroXlntop_2 ScroXlnsee_2 AcroXlnare_2 
local millet CAcroXCA_4 BcroXlnbas_4 TcroXlntop_4 ScroXlnsee_4 AcroXlnare_4 
local gnut CAcroXCA_5 BcroXlnbas_5 TcroXlntop_5 ScroXlnsee_5 AcroXlnare_5 
local ppea CAcroXCA_6 BcroXlnbas_6 TcroXlntop_6 ScroXlnsee_6 AcroXlnare_6 

xi: reg lnyield `maize' `sorghum' `millet' `gnut' `ppea' crop_type1- crop_type5 i.year i.rc, noconstant vce(cluster crc)

est store CAFE

*CA, with rain shock, without househould fixed effects
local maize CAcroXCA_1 SHKcroXshock_1 CAScroXca_sh_1 BcroXlnbas_1 TcroXlntop_1 ScroXlnsee_1 AcroXlnare_1 
local sorghum CAcroXCA_2 SHKcroXshock_2 CAScroXca_sh_2 BcroXlnbas_2 TcroXlntop_2 ScroXlnsee_2 AcroXlnare_2 
local millet CAcroXCA_4 SHKcroXshock_4 CAScroXca_sh_4 BcroXlnbas_4 TcroXlntop_4 ScroXlnsee_4 AcroXlnare_4 
local gnut CAcroXCA_5 SHKcroXshock_5 CAScroXca_sh_5 BcroXlnbas_5 TcroXlntop_5 ScroXlnsee_5 AcroXlnare_5 
local ppea CAcroXCA_6 SHKcroXshock_6 CAScroXca_sh_6 BcroXlnbas_6 TcroXlntop_6 ScroXlnsee_6 AcroXlnare_6 

xi: reg lnyield `maize' `sorghum' `millet' `gnut' `ppea' crop_type1- crop_type5 i.year, noconstant vce(cluster crc)

est store CAshk

*CA, with rain shock, with househould fixed effects
local maize CAcroXCA_1 SHKcroXshock_1 CAScroXca_sh_1 BcroXlnbas_1 TcroXlntop_1 ScroXlnsee_1 AcroXlnare_1 
local sorghum CAcroXCA_2 SHKcroXshock_2 CAScroXca_sh_2 BcroXlnbas_2 TcroXlntop_2 ScroXlnsee_2 AcroXlnare_2 
local millet CAcroXCA_4 SHKcroXshock_4 CAScroXca_sh_4 BcroXlnbas_4 TcroXlntop_4 ScroXlnsee_4 AcroXlnare_4 
local gnut CAcroXCA_5 SHKcroXshock_5 CAScroXca_sh_5 BcroXlnbas_5 TcroXlntop_5 ScroXlnsee_5 AcroXlnare_5 
local ppea CAcroXCA_6 SHKcroXshock_6 CAScroXca_sh_6 BcroXlnbas_6 TcroXlntop_6 ScroXlnsee_6 AcroXlnare_6 

xi: reg lnyield `maize' `sorghum' `millet' `gnut' `ppea' crop_type1- crop_type5 i.year i.rc, noconstant vce(cluster crc)

est store CAshkFE

*Limited coefficients for Table 5
esttab CA CAFE CAshk CAshkFE using table5.tex, replace f ///
	label booktabs b(3) se(3) eqlabels(none) alignment(S)  ///
	drop(BcroXlnbas_* TcroXlntop_* ScroXlnsee_* AcroXlnare_* crop_type* _Iyear_* _Irc_*) ///
	star(* 0.10 ** 0.05 *** 0.01) nogaps ///
	order(CAcroXCA_1 SHKcroXshock_1 CAScroXca_sh_1 CAcroXCA_2 SHKcroXshock_2 CAScroXca_sh_2 ///
	CAcroXCA_4 SHKcroXshock_4 CAScroXca_sh_4 CAcroXCA_5 SHKcroXshock_5 CAScroXca_sh_5  ///
	CAcroXCA_6 SHKcroXshock_6 CAScroXca_sh_6 ) ///
	refcat(CAcroXCA_1 "\emph{Maize}" CAcroXCA_2 "\emph{Sorghum}" CAcroXCA_4 "\emph{Millet}" CAcroXCA_5 "\emph{Groundnut}" CAcroXCA_6 "\emph{Cowpea}", nolabel) ///
	stats(N r2, fmt(0 3) layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{S}{@}") labels(`"Observations"' `"\(R^{2}\)"'))


*All coefficients for Appendix Table A2
esttab CA CAFE CAshk CAshkFE using tableA2.tex, replace f ///
	label booktabs b(3) se(3) eqlabels(none) alignment(S)  ///
	drop(crop_type* _Iyear_* _Irc_*) ///
	star(* 0.10 ** 0.05 *** 0.01) nogaps ///
	order(CAcroXCA_1 SHKcroXshock_1 CAScroXca_sh_1 BcroXlnbas_1 TcroXlntop_1 ScroXlnsee_1 AcroXlnare_1 ///
	CAcroXCA_2 SHKcroXshock_2 CAScroXca_sh_2 BcroXlnbas_2 TcroXlntop_2 ScroXlnsee_2 AcroXlnare_2 ///
	CAcroXCA_4 SHKcroXshock_4 CAScroXca_sh_4 BcroXlnbas_4 TcroXlntop_4 ScroXlnsee_4 AcroXlnare_4 ///
	CAcroXCA_5 SHKcroXshock_5 CAScroXca_sh_5 BcroXlnbas_5 TcroXlntop_5 ScroXlnsee_5 AcroXlnare_5 ///
	CAcroXCA_6 SHKcroXshock_6 CAScroXca_sh_6 BcroXlnbas_6 TcroXlntop_6 ScroXlnsee_6 AcroXlnare_6 ) ///
	refcat(CAcroXCA_1 "\emph{Maize}" CAcroXCA_2 "\emph{Sorghum}" CAcroXCA_4 "\emph{Millet}" CAcroXCA_5 "\emph{Groundnut}" CAcroXCA_6 "\emph{Cowpea}", nolabel) ///
	stats(N r2, fmt(0 3) layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{S}{@}") labels(`"Observations"' `"\(R^{2}\)"'))


*******************************************
****Table 6: IMR Wooldridge IV method******
*******************************************


** Generate variables for CRE
local z11 BcroXlnbas_1-AcroXlnare_1 BcroXlnbas_2-AcroXlnare_2 BcroXlnbas_4-AcroXlnare_4 BcroXlnbas_5-AcroXlnare_5 BcroXlnbas_6-AcroXlnare_6
local z11bar
local i=1
foreach var of varlist `z11' {
	qui egen `var'bar=mean(`var'), by(rc)
	local z11bar `z11bar' `var'bar
local i=`i'+1
}

*IV without rain, without households CRE
local maize BcroXlnbas_1 TcroXlntop_1 ScroXlnsee_1 AcroXlnare_1 
local sorghum BcroXlnbas_2 TcroXlntop_2 ScroXlnsee_2 AcroXlnare_2 
local millet BcroXlnbas_4 TcroXlntop_4 ScroXlnsee_4 AcroXlnare_4 
local gnut BcroXlnbas_5 TcroXlntop_5 ScroXlnsee_5 AcroXlnare_5 
local ppea BcroXlnbas_6 TcroXlntop_6 ScroXlnsee_6 AcroXlnare_6 

xi: probit CA wardNGO `maize' `sorghum' `millet' `gnut' `ppea' crop_type1- crop_type5 i.year, vce(cluster crc)

predict p1, xb
gen IMR1 = normalden(p1) / normal(p1)     /* gets the inverse mills ratio*/

est store  CAp1IV

*IV without rain, with households CRE
local maize BcroXlnbas_1 TcroXlntop_1 ScroXlnsee_1 AcroXlnare_1 
local sorghum BcroXlnbas_2 TcroXlntop_2 ScroXlnsee_2 AcroXlnare_2 
local millet BcroXlnbas_4 TcroXlntop_4 ScroXlnsee_4 AcroXlnare_4 
local gnut BcroXlnbas_5 TcroXlntop_5 ScroXlnsee_5 AcroXlnare_5 
local ppea BcroXlnbas_6 TcroXlntop_6 ScroXlnsee_6 AcroXlnare_6 

xi: probit CA wardNGO `maize' `sorghum' `millet' `gnut' `ppea' crop_type1- crop_type5 i.year BcroXlnbas_1bar-AcroXlnare_6bar, vce(cluster crc)

predict p2, xb
gen IMR2 = normalden(p2) / normal(p2)     /* gets the inverse mills ratio*/

est store CAp2IV

*IV with rain shock, without househould CRE
local maize SHKcroXshock_1 BcroXlnbas_1 TcroXlntop_1 ScroXlnsee_1 AcroXlnare_1 
local sorghum SHKcroXshock_2 BcroXlnbas_2 TcroXlntop_2 ScroXlnsee_2 AcroXlnare_2 
local millet SHKcroXshock_4 BcroXlnbas_4 TcroXlntop_4 ScroXlnsee_4 AcroXlnare_4 
local gnut SHKcroXshock_5 BcroXlnbas_5 TcroXlntop_5 ScroXlnsee_5 AcroXlnare_5 
local ppea SHKcroXshock_6 BcroXlnbas_6 TcroXlntop_6 ScroXlnsee_6 AcroXlnare_6 

xi: probit CA wardNGO `maize' `sorghum' `millet' `gnut' `ppea' crop_type1- crop_type5 i.year, vce(cluster crc)

predict p3, xb
gen IMR3 = normalden(p3) / normal(p3)     /* gets the inverse mills ratio*/

est store CAp3IV

*IV with rain shock, with househould CRE
local maize SHKcroXshock_1 BcroXlnbas_1 TcroXlntop_1 ScroXlnsee_1 AcroXlnare_1 
local sorghum SHKcroXshock_2 BcroXlnbas_2 TcroXlntop_2 ScroXlnsee_2 AcroXlnare_2
local millet SHKcroXshock_4 BcroXlnbas_4 TcroXlntop_4 ScroXlnsee_4 AcroXlnare_4 
local gnut SHKcroXshock_5 BcroXlnbas_5 TcroXlntop_5 ScroXlnsee_5 AcroXlnare_5 
local ppea SHKcroXshock_6 BcroXlnbas_6 TcroXlntop_6 ScroXlnsee_6 AcroXlnare_6 

xi: probit CA wardNGO `maize' `sorghum' `millet' `gnut' `ppea' crop_type1- crop_type5 i.year BcroXlnbas_1bar-AcroXlnare_6bar, vce(cluster crc)

predict p4, xb
gen IMR4 = normalden(p4) / normal(p4)     /* gets the inverse mills ratio*/

est store CAp4IV

esttab CAp1IV CAp2IV CAp3IV CAp4IV using table6.tex, replace f ///
	label booktabs b(3) se(3) eqlabels(none) alignment(S)  ///
	drop(SHKcroXshock_* BcroXlnbas_* TcroXlntop_* ScroXlnsee_* AcroXlnare_* crop_type* _Iyear_* _cons) ///
	star(* 0.10 ** 0.05 *** 0.01) nogaps ///
	stats(N ll, fmt(0 0) layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}") labels(`"Observations"' `"Log Likelihood"'))


****************************************************
****Table 7: All crops, CA endogenous***************
****************************************************

*Generate crop specific IV for equation 1
xi I.crops|IMR1, prefix(IV) noomit
drop IVcrops_1 IVcrops_2 IVcrops_4 IVcrops_5 IVcrops_6

*Generate crop specific IV for equation 2
xi I.crops|IMR2, prefix(CtIV) noomit
drop CtIVcrops_1 CtIVcrops_2 CtIVcrops_4 CtIVcrops_5 CtIVcrops_6

*Generate crop specific IV for equation 3
xi I.crops|IMR3, prefix(CsIV) noomit
drop CsIVcrops_1 CsIVcrops_2 CsIVcrops_4 CsIVcrops_5 CsIVcrops_6

gen sIV_shk = IMR3*shock
xi I.crops|sIV_shk, prefix(sIV) noomit
drop sIVcrops_1 sIVcrops_2 sIVcrops_4 sIVcrops_5 sIVcrops_6

*Generate crop specific IV for equation 4
xi I.crops|IMR4, prefix(CbIV) noomit
drop CbIVcrops_1 CbIVcrops_2 CbIVcrops_4 CbIVcrops_5 CbIVcrops_6

gen bIV_shk = IMR4*shock
xi I.crops|bIV_shk, prefix(bsIV) noomit
drop bsIVcrops_1 bsIVcrops_2 bsIVcrops_4 bsIVcrops_5 bsIVcrops_6


*Endogenous CA, without rain, without households fixed effects 
local maize BcroXlnbas_1 TcroXlntop_1 ScroXlnsee_1 AcroXlnare_1 
local sorghum BcroXlnbas_2 TcroXlntop_2 ScroXlnsee_2 AcroXlnare_2 
local millet BcroXlnbas_4 TcroXlntop_4 ScroXlnsee_4 AcroXlnare_4 
local gnut BcroXlnbas_5 TcroXlntop_5 ScroXlnsee_5 AcroXlnare_5 
local ppea BcroXlnbas_6 TcroXlntop_6 ScroXlnsee_6 AcroXlnare_6 

ivreg2 lnyield ( CAcroXCA_1 CAcroXCA_2 CAcroXCA_4 CAcroXCA_5 CAcroXCA_6 = IVcroXIMR1_1 IVcroXIMR1_2 IVcroXIMR1_4 IVcroXIMR1_5 IVcroXIMR1_6 ) ///
`maize' `sorghum' `millet' `gnut' `ppea' crop_type1- crop_type5 i.year, first noconstant cluster(crc) partial(crop_type1- crop_type5 i.year) 

est store CAend

	*Calculate p-value with n = 2 & K = 5
	local K2 = min(100,e(exexog_ct))
	gen pval1_10 = 1-nchi2(`K2',`K2'*1.84,`K2'*e(widstat))
	gen pval1_15 = 1-nchi2(`K2',`K2'*0.61,`K2'*e(widstat))
	gen pval1_20 = 1-nchi2(`K2',`K2'*0.35,`K2'*e(widstat))
	gen pval1_25 = 1-nchi2(`K2',`K2'*0.22,`K2'*e(widstat))

	sum pval1_10 pval1_15 pval1_20 pval1_25

*Endogenous CA, without rain, with households fixed effects 
local maize BcroXlnbas_1 TcroXlntop_1 ScroXlnsee_1 AcroXlnare_1 
local sorghum BcroXlnbas_2 TcroXlntop_2 ScroXlnsee_2 AcroXlnare_2 
local millet BcroXlnbas_4 TcroXlntop_4 ScroXlnsee_4 AcroXlnare_4 
local gnut BcroXlnbas_5 TcroXlntop_5 ScroXlnsee_5 AcroXlnare_5 
local ppea BcroXlnbas_6 TcroXlntop_6 ScroXlnsee_6 AcroXlnare_6 

ivreg2 lnyield ( CAcroXCA_1 CAcroXCA_2 CAcroXCA_4 CAcroXCA_5 CAcroXCA_6 = CtIVcroXIMR2_1 CtIVcroXIMR2_2 CtIVcroXIMR2_4 CtIVcroXIMR2_5 CtIVcroXIMR2_6 ) ///
`maize' `sorghum' `millet' `gnut' `ppea'  crop_type1- crop_type5 i.year i.rc, first noconstant cluster(crc) partial(crop_type1- crop_type5 i.year i.rc)

est store CAtotend 

	*Calculate p-value with n = 2 & K = 5
	local K2 = min(100,e(exexog_ct))
	gen pval2_10 = 1-nchi2(`K2',`K2'*1.84,`K2'*e(widstat))
	gen pval2_15 = 1-nchi2(`K2',`K2'*0.61,`K2'*e(widstat))
	gen pval2_20 = 1-nchi2(`K2',`K2'*0.35,`K2'*e(widstat))
	gen pval2_25 = 1-nchi2(`K2',`K2'*0.22,`K2'*e(widstat))

	sum pval2_10 pval2_15 pval2_20 pval2_25

*Endogenous CA, with rain shock, without househould fixed effects
local maize SHKcroXshock_1 BcroXlnbas_1 TcroXlntop_1 ScroXlnsee_1 AcroXlnare_1 
local sorghum SHKcroXshock_2 BcroXlnbas_2 TcroXlntop_2 ScroXlnsee_2 AcroXlnare_2 
local millet SHKcroXshock_4 BcroXlnbas_4 TcroXlntop_4 ScroXlnsee_4 AcroXlnare_4 
local gnut SHKcroXshock_5 BcroXlnbas_5 TcroXlntop_5 ScroXlnsee_5 AcroXlnare_5 
local ppea SHKcroXshock_6 BcroXlnbas_6 TcroXlntop_6 ScroXlnsee_6 AcroXlnare_6 

ivreg2 lnyield ( CAcroXCA_1 CAcroXCA_2 CAcroXCA_4 CAcroXCA_5 CAcroXCA_6 CAScroXca_sh_1 CAScroXca_sh_2 CAScroXca_sh_4 CAScroXca_sh_5 CAScroXca_sh_6 = ///
 CsIVcroXIMR3_1 CsIVcroXIMR3_2 CsIVcroXIMR3_4 CsIVcroXIMR3_5 CsIVcroXIMR3_6 sIVcroXsIV_s_1 sIVcroXsIV_s_2 sIVcroXsIV_s_4 sIVcroXsIV_s_5 sIVcroXsIV_s_6 ) ///
 `maize' `sorghum' `millet' `gnut' `ppea' crop_type1- crop_type5 i.year, first noconstant cluster(crc) partial(crop_type1- crop_type5 i.year)

est store CAshkend

	*Calculate p-value with n = 2 & K = 10
	local K2 = min(100,e(exexog_ct))
	gen pval3_10 = 1-nchi2(`K2',`K2'*1.84,`K2'*e(widstat))
	gen pval3_15 = 1-nchi2(`K2',`K2'*0.61,`K2'*e(widstat))
	gen pval3_20 = 1-nchi2(`K2',`K2'*0.35,`K2'*e(widstat))
	gen pval3_25 = 1-nchi2(`K2',`K2'*0.22,`K2'*e(widstat))

	sum pval3_10 pval3_15 pval3_20 pval3_25

*Endogenous CA, with rain shock, with househould fixed effects
local maize SHKcroXshock_1 BcroXlnbas_1 TcroXlntop_1 ScroXlnsee_1 AcroXlnare_1 
local sorghum SHKcroXshock_2 BcroXlnbas_2 TcroXlntop_2 ScroXlnsee_2 AcroXlnare_2 
local millet SHKcroXshock_4 BcroXlnbas_4 TcroXlntop_4 ScroXlnsee_4 AcroXlnare_4 
local gnut SHKcroXshock_5 BcroXlnbas_5 TcroXlntop_5 ScroXlnsee_5 AcroXlnare_5 
local ppea SHKcroXshock_6 BcroXlnbas_6 TcroXlntop_6 ScroXlnsee_6 AcroXlnare_6 

ivreg2 lnyield ( CAcroXCA_1 CAcroXCA_2 CAcroXCA_4 CAcroXCA_5 CAcroXCA_6 CAScroXca_sh_1 CAScroXca_sh_2 CAScroXca_sh_4 CAScroXca_sh_5 CAScroXca_sh_6 = ///
 CbIVcroXIMR4_1 CbIVcroXIMR4_2 CbIVcroXIMR4_4 CbIVcroXIMR4_5 CbIVcroXIMR4_6 bsIVcroXbIV_s_1 bsIVcroXbIV_s_2 bsIVcroXbIV_s_4 bsIVcroXbIV_s_5 bsIVcroXbIV_s_6 ) ///
 `maize' `sorghum' `millet' `gnut' `ppea' crop_type1- crop_type5 i.year i.rc, first noconstant cluster(crc) partial(crop_type1- crop_type5 i.year i.rc)

est store  CAtotshkend

	*Calculate p-value with n = 2 & K = 10
	local K2 = min(100,e(exexog_ct))
	gen pval4_10 = 1-nchi2(`K2',`K2'*1.84,`K2'*e(widstat))
	gen pval4_15 = 1-nchi2(`K2',`K2'*0.61,`K2'*e(widstat))
	gen pval4_20 = 1-nchi2(`K2',`K2'*0.35,`K2'*e(widstat))
	gen pval4_25 = 1-nchi2(`K2',`K2'*0.22,`K2'*e(widstat))

	sum pval4_10 pval4_15 pval4_20 pval4_25

*Limited coefficients for Table 7
esttab CAend CAtotend CAshkend CAtotshkend using table7.tex, replace f ///
	label booktabs b(3) se(3) eqlabels(none) alignment(S)  ///
	drop(BcroXlnbas_* TcroXlntop_* ScroXlnsee_* AcroXlnare_*) ///
	star(* 0.10 ** 0.05 *** 0.01) nogaps ///
	order(CAcroXCA_1 SHKcroXshock_1 CAScroXca_sh_1 CAcroXCA_2 SHKcroXshock_2 CAScroXca_sh_2 CAcroXCA_4 SHKcroXshock_4 CAScroXca_sh_4  ///
	CAcroXCA_5 SHKcroXshock_5 CAScroXca_sh_5  CAcroXCA_6 SHKcroXshock_6 CAScroXca_sh_6  ) ///
	refcat(CAcroXCA_1 "\emph{Maize}" CAcroXCA_2 "\emph{Sorghum}" CAcroXCA_4 "\emph{Millet}" CAcroXCA_5 "\emph{Groundnut}" CAcroXCA_6 "\emph{Cowpea}", nolabel) ///
	stats(N ll idstat archi2 widstat idp archi2p , fmt(0 0 3 2 3 3 3) layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" ///
	"\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}") labels(`"Observations"' `"Log Likelihood"' ///
	`"Kleibergen-Paap LM stat"' `"Anderson-Rubin Wald stat"' `"Kleibergen-Paap Wald stat"' `"Kleibergen-Paap p-value"' `"Anderson-Rubin p-value"' ))

*All coefficients for Appendix Table A3	
esttab CAend CAtotend CAshkend CAtotshkend using tableA3.tex, replace f ///
	label booktabs b(3) se(3) eqlabels(none) alignment(S)  ///
	star(* 0.10 ** 0.05 *** 0.01) nogaps ///
	order(CAcroXCA_1 SHKcroXshock_1 CAScroXca_sh_1 BcroXlnbas_1 TcroXlntop_1 ScroXlnsee_1 AcroXlnare_1 ///
	CAcroXCA_2 SHKcroXshock_2 CAScroXca_sh_2 BcroXlnbas_2 TcroXlntop_2 ScroXlnsee_2 AcroXlnare_2 ///
	CAcroXCA_4 SHKcroXshock_4 CAScroXca_sh_4 BcroXlnbas_4 TcroXlntop_4 ScroXlnsee_4 AcroXlnare_4 ///
	CAcroXCA_5 SHKcroXshock_5 CAScroXca_sh_5 BcroXlnbas_5 TcroXlntop_5 ScroXlnsee_5 AcroXlnare_5 ///
	CAcroXCA_6 SHKcroXshock_6 CAScroXca_sh_6 BcroXlnbas_6 TcroXlntop_6 ScroXlnsee_6 AcroXlnare_6 ) ///
	refcat(CAcroXCA_1 "\emph{Maize}" CAcroXCA_2 "\emph{Sorghum}" CAcroXCA_4 "\emph{Millet}" CAcroXCA_5 "\emph{Groundnut}" CAcroXCA_6 "\emph{Cowpea}", nolabel) ///
	stats(N ll idstat archi2 widstat idp archi2p , fmt(0 0 3 2 3 3 3) layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" ///
	"\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}") labels(`"Observations"' `"Log Likelihood"' ///
	`"Kleibergen-Paap LM stat"' `"Anderson-Rubin Wald stat"' `"Kleibergen-Paap Wald stat"' `"Kleibergen-Paap p-value"' `"Anderson-Rubin p-value"' ))

	
	
****************************************************
****Table 8: All crops, shock & surplus*************
****************************************************


*IMR Wooldridge IV method, results are presented in Appendix

*IV with rain shortage, with househould CRE
local maize SRTcroXshort_1 BcroXlnbas_1 TcroXlntop_1 ScroXlnsee_1 AcroXlnare_1 
local sorghum SRTcroXshort_2 BcroXlnbas_2 TcroXlntop_2 ScroXlnsee_2 AcroXlnare_2 
local millet SRTcroXshort_4 BcroXlnbas_4 TcroXlntop_4 ScroXlnsee_4 AcroXlnare_4 
local gnut SRTcroXshort_5 BcroXlnbas_5 TcroXlntop_5 ScroXlnsee_5 AcroXlnare_5 
local ppea SRTcroXshort_6 BcroXlnbas_6 TcroXlntop_6 ScroXlnsee_6 AcroXlnare_6 

xi: probit CA wardNGO `maize' `sorghum' `millet' `gnut' `ppea' crop_type1- crop_type5 i.year BcroXlnbas_1bar-AcroXlnare_6bar, vce(cluster crc)

predict psrt, xb
gen IMRsrt = normalden(psrt) / normal(psrt)     /* gets the inverse mills ratio*/

est store CAsIV1

*IV with rain surplus, with househould CRE
local maize SRPcroXsurpl_1 BcroXlnbas_1 TcroXlntop_1 ScroXlnsee_1 AcroXlnare_1 
local sorghum SRPcroXsurpl_2 BcroXlnbas_2 TcroXlntop_2 ScroXlnsee_2 AcroXlnare_2 
local millet SRPcroXsurpl_4 BcroXlnbas_4 TcroXlntop_4 ScroXlnsee_4 AcroXlnare_4 
local gnut SRPcroXsurpl_5 BcroXlnbas_5 TcroXlntop_5 ScroXlnsee_5 AcroXlnare_5 
local ppea SRPcroXsurpl_6 BcroXlnbas_6 TcroXlntop_6 ScroXlnsee_6 AcroXlnare_6 

xi: probit CA wardNGO `maize' `sorghum' `millet' `gnut' `ppea' crop_type1- crop_type5 i.year BcroXlnbas_1bar-AcroXlnare_6bar, vce(cluster crc)

predict psrp, xb
gen IMRsrp = normalden(psrp) / normal(psrp)     /* gets the inverse mills ratio*/

est store CAsIV2

*IV with rain shortage and surplus, with househould CRE
local maize SRTcroXshort_1 SRPcroXsurpl_1 BcroXlnbas_1 TcroXlntop_1 ScroXlnsee_1 AcroXlnare_1 
local sorghum SRTcroXshort_2 SRPcroXsurpl_2 BcroXlnbas_2 TcroXlntop_2 ScroXlnsee_2 AcroXlnare_2 
local millet SRTcroXshort_4 SRPcroXsurpl_4 BcroXlnbas_4 TcroXlntop_4 ScroXlnsee_4 AcroXlnare_4 
local gnut SRTcroXshort_5 SRPcroXsurpl_5 BcroXlnbas_5 TcroXlntop_5 ScroXlnsee_5 AcroXlnare_5 
local ppea SRTcroXshort_6 SRPcroXsurpl_6 BcroXlnbas_6 TcroXlntop_6 ScroXlnsee_6 AcroXlnare_6 

xi: probit CA wardNGO `maize' `sorghum' `millet' `gnut' `ppea' crop_type1- crop_type5 i.year BcroXlnbas_1bar-AcroXlnare_6bar, vce(cluster crc)

predict prtrp, xb
gen IMRrtrp = normalden(prtrp) / normal(prtrp)     /* gets the inverse mills ratio*/

est store CAssIV3


*Second stage IV regressions that are used in the paper
	
*Generate crop specific IV for shortage equation
xi I.crops|IMRsrt, prefix(Csrt) noomit
drop Csrtcrops_1 Csrtcrops_2 Csrtcrops_4 Csrtcrops_5 Csrtcrops_6

gen IV_srt = IMRsrt*short
xi I.crops|IV_srt, prefix(stIV) noomit
drop stIVcrops_1 stIVcrops_2 stIVcrops_4 stIVcrops_5 stIVcrops_6

*Generate crop specific IV for shortage equation
xi I.crops|IMRsrp, prefix(Csrp) noomit
drop Csrpcrops_1 Csrpcrops_2 Csrpcrops_4 Csrpcrops_5 Csrpcrops_6

gen IV_srp = IMRsrp*surpl
xi I.crops|IV_srp, prefix(spIV) noomit
drop spIVcrops_1 spIVcrops_2 spIVcrops_4 spIVcrops_5 spIVcrops_6

*Generate crop specific IV for shortage and surplus equation
xi I.crops|IMRrtrp, prefix(rprt) noomit
drop rprtcrops_1 rprtcrops_2 rprtcrops_4 rprtcrops_5 rprtcrops_6

gen IV_rp = IMRrtrp*surpl
xi I.crops|IV_rp, prefix(rpIV) noomit
drop rpIVcrops_1 rpIVcrops_2 rpIVcrops_4 rpIVcrops_5 rpIVcrops_6

gen IV_rt = IMRrtrp*short
xi I.crops|IV_rt, prefix(rtIV) noomit
drop rtIVcrops_1 rtIVcrops_2 rtIVcrops_4 rtIVcrops_5 rtIVcrops_6


*Endogenous CA, with rain shortage, with househould fixed effects
local maize SRTcroXshort_1 BcroXlnbas_1 TcroXlntop_1 ScroXlnsee_1 AcroXlnare_1 
local sorghum SRTcroXshort_2 BcroXlnbas_2 TcroXlntop_2 ScroXlnsee_2 AcroXlnare_2 
local millet SRTcroXshort_4 BcroXlnbas_4 TcroXlntop_4 ScroXlnsee_4 AcroXlnare_4 
local gnut SRTcroXshort_5 BcroXlnbas_5 TcroXlntop_5 ScroXlnsee_5 AcroXlnare_5 
local ppea SRTcroXshort_6 BcroXlnbas_6 TcroXlntop_6 ScroXlnsee_6 AcroXlnare_6 

xi: ivreg2 lnyield ( CAcroXCA_1 CAcroXCA_2 CAcroXCA_4 CAcroXCA_5 CAcroXCA_6 CARcroXca_sr_1 CARcroXca_sr_2 CARcroXca_sr_4 CARcroXca_sr_5 CARcroXca_sr_6 = ///
 CsrtcroXIMRsr_1 CsrtcroXIMRsr_2 CsrtcroXIMRsr_4 CsrtcroXIMRsr_5 CsrtcroXIMRsr_6 stIVcroXIV_sr_1 stIVcroXIV_sr_2 stIVcroXIV_sr_4 stIVcroXIV_sr_5 stIVcroXIV_sr_6 ) ///
 `maize' `sorghum' `millet' `gnut' `ppea' crop_type1- crop_type5 i.year i.rc, first noconstant cluster(crc) partial(crop_type1- crop_type5 i.year i.rc)

est store  CAsrtend	

	*Calculate p-value with n = 2 & K = 10
	local K2 = min(100,e(exexog_ct))
	gen pval5_10 = 1-nchi2(`K2',`K2'*1.84,`K2'*e(widstat))
	gen pval5_15 = 1-nchi2(`K2',`K2'*0.61,`K2'*e(widstat))
	gen pval5_20 = 1-nchi2(`K2',`K2'*0.35,`K2'*e(widstat))
	gen pval5_25 = 1-nchi2(`K2',`K2'*0.22,`K2'*e(widstat))

	sum pval5_10 pval5_15 pval5_20 pval5_25

*Endogenous CA, with rain shortage, with househould fixed effects
local maize SRPcroXsurpl_1 BcroXlnbas_1 TcroXlntop_1 ScroXlnsee_1 AcroXlnare_1 
local sorghum SRPcroXsurpl_2 BcroXlnbas_2 TcroXlntop_2 ScroXlnsee_2 AcroXlnare_2 
local millet SRPcroXsurpl_4 BcroXlnbas_4 TcroXlntop_4 ScroXlnsee_4 AcroXlnare_4 
local gnut SRPcroXsurpl_5 BcroXlnbas_5 TcroXlntop_5 ScroXlnsee_5 AcroXlnare_5 
local ppea SRPcroXsurpl_6 BcroXlnbas_6 TcroXlntop_6 ScroXlnsee_6 AcroXlnare_6 

xi: ivreg2 lnyield ( CAcroXCA_1 CAcroXCA_2 CAcroXCA_4 CAcroXCA_5 CAcroXCA_6 CAPcroXca_sr_1 CAPcroXca_sr_2 CAPcroXca_sr_4 CAPcroXca_sr_5 CAPcroXca_sr_6 = ///
 CsrpcroXIMRsr_1 CsrpcroXIMRsr_2 CsrpcroXIMRsr_4 CsrpcroXIMRsr_5 CsrpcroXIMRsr_6 spIVcroXIV_sr_1 spIVcroXIV_sr_2 spIVcroXIV_sr_4 spIVcroXIV_sr_5 spIVcroXIV_sr_6 ) ///
 `maize' `sorghum' `millet' `gnut' `ppea' crop_type1- crop_type5 i.year i.rc, first noconstant cluster(crc) partial(crop_type1- crop_type5 i.year i.rc)

est store  CAsrpend

	*Calculate p-value with n = 2 & K = 10
	local K2 = min(100,e(exexog_ct))
	gen pval6_10 = 1-nchi2(`K2',`K2'*1.84,`K2'*e(widstat))
	gen pval6_15 = 1-nchi2(`K2',`K2'*0.61,`K2'*e(widstat))
	gen pval6_20 = 1-nchi2(`K2',`K2'*0.35,`K2'*e(widstat))
	gen pval6_25 = 1-nchi2(`K2',`K2'*0.22,`K2'*e(widstat))

	sum pval6_10 pval6_15 pval6_20 pval6_25

*Endogenous CA, with rain shortage and surplus, with househould fixed effects
local maize SRTcroXshort_1 SRPcroXsurpl_1 BcroXlnbas_1 TcroXlntop_1 ScroXlnsee_1 AcroXlnare_1 
local sorghum SRTcroXshort_2 SRPcroXsurpl_2 BcroXlnbas_2 TcroXlntop_2 ScroXlnsee_2 AcroXlnare_2 
local millet SRTcroXshort_4 SRPcroXsurpl_4 BcroXlnbas_4 TcroXlntop_4 ScroXlnsee_4 AcroXlnare_4 
local gnut SRTcroXshort_5 SRPcroXsurpl_5 BcroXlnbas_5 TcroXlntop_5 ScroXlnsee_5 AcroXlnare_5 
local ppea SRTcroXshort_6 SRPcroXsurpl_6 BcroXlnbas_6 TcroXlntop_6 ScroXlnsee_6 AcroXlnare_6 

xi: ivreg2 lnyield ( CAcroXCA_1 CAcroXCA_2 CAcroXCA_4 CAcroXCA_5 CAcroXCA_6 CARcroXca_sr_1 CARcroXca_sr_2 CARcroXca_sr_4 CARcroXca_sr_5 ///
 CARcroXca_sr_6 CAPcroXca_sr_1 CAPcroXca_sr_2 CAPcroXca_sr_4 CAPcroXca_sr_5 CAPcroXca_sr_6 = rprtcroXIMRrt_1 rprtcroXIMRrt_2 rprtcroXIMRrt_4 rprtcroXIMRrt_5 rprtcroXIMRrt_6 ///
 rpIVcroXIV_rp_1 rpIVcroXIV_rp_2 rpIVcroXIV_rp_4 rpIVcroXIV_rp_5 rpIVcroXIV_rp_6 rtIVcroXIV_rt_1 rtIVcroXIV_rt_2 rtIVcroXIV_rt_4 rtIVcroXIV_rt_5 rtIVcroXIV_rt_6 ) ///
 `maize' `sorghum' `millet' `gnut' `ppea' crop_type1- crop_type5 i.year i.rc, first noconstant cluster(crc) partial(crop_type1- crop_type5 i.year i.rc)

est store  CArprtend

	*Calculate p-value with n = 2 & K = 15
	local K2 = min(100,e(exexog_ct))
	gen pval7_10 = 1-nchi2(`K2',`K2'*1.84,`K2'*e(widstat))
	gen pval7_15 = 1-nchi2(`K2',`K2'*0.61,`K2'*e(widstat))
	gen pval7_20 = 1-nchi2(`K2',`K2'*0.35,`K2'*e(widstat))
	gen pval7_25 = 1-nchi2(`K2',`K2'*0.22,`K2'*e(widstat))

	sum pval7_10 pval7_15 pval7_20 pval7_25

save "JEEM_results.dta", replace
	
****************************************************
****Figure 8****************************************
****************************************************

*Endogenous CA, with rain shortage and surplus, with househould fixed effects
local maize SRTcroXshort_1 SRPcroXsurpl_1 BcroXlnbas_1 TcroXlntop_1 ScroXlnsee_1 AcroXlnare_1 
local sorghum SRTcroXshort_2 SRPcroXsurpl_2 BcroXlnbas_2 TcroXlntop_2 ScroXlnsee_2 AcroXlnare_2 
local millet SRTcroXshort_4 SRPcroXsurpl_4 BcroXlnbas_4 TcroXlntop_4 ScroXlnsee_4 AcroXlnare_4 
local gnut SRTcroXshort_5 SRPcroXsurpl_5 BcroXlnbas_5 TcroXlntop_5 ScroXlnsee_5 AcroXlnare_5 
local ppea SRTcroXshort_6 SRPcroXsurpl_6 BcroXlnbas_6 TcroXlntop_6 ScroXlnsee_6 AcroXlnare_6 

xi: ivreg2 lnyield ( CAcroXCA_1 CAcroXCA_2 CAcroXCA_4 CAcroXCA_5 CAcroXCA_6 CARcroXca_sr_1 CARcroXca_sr_2 CARcroXca_sr_4 CARcroXca_sr_5 ///
 CARcroXca_sr_6 CAPcroXca_sr_1 CAPcroXca_sr_2 CAPcroXca_sr_4 CAPcroXca_sr_5 CAPcroXca_sr_6 = rprtcroXIMRrt_1 rprtcroXIMRrt_2 rprtcroXIMRrt_4 rprtcroXIMRrt_5 rprtcroXIMRrt_6 ///
 rpIVcroXIV_rp_1 rpIVcroXIV_rp_2 rpIVcroXIV_rp_4 rpIVcroXIV_rp_5 rpIVcroXIV_rp_6 rtIVcroXIV_rt_1 rtIVcroXIV_rt_2 rtIVcroXIV_rt_4 rtIVcroXIV_rt_5 rtIVcroXIV_rt_6 ) ///
 `maize' `sorghum' `millet' `gnut' `ppea' crop_type1- crop_type5 i.year i.rc, noconstant cluster(crc)

	label variable CAcroXCA_1 "conservation ag"
	label variable CARcroXca_sr_1 "CA*shortage"
	label variable CAPcroXca_sr_1 "CA*surplus"
	label variable CAcroXCA_2 "conservation ag"
	label variable CARcroXca_sr_2 "CA*shortage"
	label variable CAPcroXca_sr_2 "CA*surplus"
	label variable CAcroXCA_4 "conservation ag"
	label variable CARcroXca_sr_4 "CA*shortage"
	label variable CAPcroXca_sr_4 "CA*surplus"
	label variable CAcroXCA_5 "conservation ag"
	label variable CARcroXca_sr_5 "CA*shortage"
	label variable CAPcroXca_sr_5 "CA*surplus"
	label variable CAcroXCA_6 "conservation ag"
	label variable CARcroXca_sr_6 "CA*shortage"
	label variable CAPcroXca_sr_6 "CA*surplus"

coefplot (CArprtend, label(maize)), keep(CAcroXCA_1 CAcroXCA_2 CAcroXCA_4 CAcroXCA_5 CAcroXCA_6 CARcroXca_sr_1 ///
CARcroXca_sr_2 CARcroXca_sr_4 CARcroXca_sr_5 CARcroXca_sr_6 CAPcroXca_sr_1 CAPcroXca_sr_2 CAPcroXca_sr_4 CAPcroXca_sr_5 CAPcroXca_sr_6)  ///
xline(0, lcolor(maroon)) xtitle("Effect on yield (%)") levels(95) msymbol(D) mfcolor(white) pstyle(p2) ciopts(lwidth(*3) lcolor(*3)) ///
order(CAcroXCA_1 CARcroXca_sr_1 CAPcroXca_sr_1 CAcroXCA_2 CARcroXca_sr_2 CAPcroXca_sr_2 CAcroXCA_4 CARcroXca_sr_4 CAPcroXca_sr_4 ///
CAcroXCA_5 CARcroXca_sr_5 CAPcroXca_sr_5 CAcroXCA_6 CARcroXca_sr_6 CAPcroXca_sr_6) headings(CAcroXCA_1 = "{bf:Maize (3,827)}" ///
CAcroXCA_2 = "{bf:Sorghum (1,264)}" CAcroXCA_4 = "{bf:Millet (488)}" CAcroXCA_5 = "{bf:Groundnut (1,397)}" CAcroXCA_6 = "{bf:Cowpea (667)}", labcolor(maroon))

graph export "coef.eps", as(eps) replace

 
predict lnyCA_hat if CA == 1, xb
predict lnyTC_hat if CA == 0, xb

gen CA_maize = _b[CAcroXCA_1]
gen CA_sorghum = _b[CAcroXCA_2]
gen CA_millet = _b[CAcroXCA_4]
gen CA_gnut = _b[CAcroXCA_5]
gen CA_ppea = _b[CAcroXCA_6]

gen CA_SRT_maize =  _b[CARcroXca_sr_1]
gen CA_SRT_sorghum = _b[CARcroXca_sr_2]
gen CA_SRT_millet = _b[CARcroXca_sr_4]
gen CA_SRT_gnut = _b[CARcroXca_sr_5]
gen CA_SRT_ppea = _b[CARcroXca_sr_6]

gen CA_SRP_maize = _b[CAPcroXca_sr_1]
gen CA_SRP_sorghum = _b[CAPcroXca_sr_2]
gen CA_SRP_millet = _b[CAPcroXca_sr_4]
gen CA_SRP_gnut = _b[CAPcroXca_sr_5]
gen CA_SRP_ppea = _b[CAPcroXca_sr_6]

gen r_shk = surpl - short 

gen rtrn = CA_maize + CA_SRP_maize*surpl + CA_SRT_maize*short if crops == 1
replace rtrn = CA_sorghum + CA_SRP_sorghum*surpl + CA_SRT_sorghum*short if crops == 2
replace rtrn = CA_millet + CA_SRP_millet*surpl + CA_SRT_millet*short if crops == 4
replace rtrn = CA_gnut + CA_SRP_gnut*surpl + CA_SRT_gnut*short if crops == 5
replace rtrn = CA_ppea + CA_SRP_ppea*surpl + CA_SRT_ppea*short if crops == 6

gen avg_rtn = (3827*(CA_maize + CA_SRP_maize*surpl + CA_SRT_maize*short) + 1264*(CA_sorghum + CA_SRP_sorghum*surpl + CA_SRT_sorghum*short) + ///
488*(CA_millet + CA_SRP_millet*surpl + CA_SRT_millet*short) + 1397*(CA_gnut + CA_SRP_gnut*surpl + CA_SRT_gnut*short) ///
+ 667*(CA_ppea + CA_SRP_ppea*surpl + CA_SRT_ppea*short) )/7643

twoway (scatter avg_rtn r_shk, sort mcolor(ebblue) msize(small)  ///
ytitle("Returns to CA") ylabel(, nogrid labsize(small)) yscale(r(-2 3.5)) ylabel(-2 -1 0 1 2 3, labsize(small)) ///
xtitle("Rainfall Deviation") xlabel(1.4259347 "+2.5" 1.1531545 "+2.0" 0.8803743 "+1.5" 0.6075941 "+1.0" 0.3348139 "+0.5" .0620337 "0" ///
-0.2107465 "-0.5" -0.4835267 "-1.0" -0.7563069 "-1.5" -1.0290871 "-2.0" -1.3018673 "-2.5", nogrid labsize(small))), ///
xline(1.1531545 0.8803743 0.6075941 0.3348139 -0.2107465 -0.4835267 -0.7563069 -1.0290871, lcolor(erose)) yline(0, lcolor(gs12)) ///
title("Average") legend(off) saving(avg, replace)

graph export "avg.eps", as(eps) replace

twoway (scatter rtrn r_shk if crops == 1, sort mcolor(navy) msize(small) ///
ytitle("Returns to CA") ylabel(, nogrid labsize(small)) yscale(r(-2 3.5)) ylabel(-2 -1 0 1 2 3, labsize(small)) ///
xtitle("Rainfall Deviation") xlabel(1.4259347 "+2.5" 1.1531545 "+2.0" 0.8803743 "+1.5" 0.6075941 "+1.0" 0.3348139 "+0.5" .0620337 "0" ///
-0.2107465 "-0.5" -0.4835267 "-1.0" -0.7563069 "-1.5" -1.0290871 "-2.0" -1.3018673 "-2.5", nogrid labsize(small))), ///
xline(1.1531545 0.8803743 0.6075941 0.3348139 -0.2107465 -0.4835267 -0.7563069 -1.0290871, lcolor(erose)) yline(0, lcolor(gs12)) ///
title("Maize") legend(off) saving(maz, replace)

graph export "maize.eps", as(eps) replace

twoway (scatter rtrn r_shk if crops == 2, sort mcolor(maroon) msize(small) ///
ytitle("Returns to CA") ylabel(, nogrid labsize(small)) yscale(r(-2 3.5)) ylabel(-2 -1 0 1 2 3, labsize(small)) ///
xtitle("Rainfall Deviation") xlabel(1.4259347 "+2.5" 1.1531545 "+2.0" 0.8803743 "+1.5" 0.6075941 "+1.0" 0.3348139 "+0.5" .0620337 "0" ///
-0.2107465 "-0.5" -0.4835267 "-1.0" -0.7563069 "-1.5" -1.0290871 "-2.0" -1.3018673 "-2.5", nogrid labsize(small))), ///
xline(1.1531545 0.8803743 0.6075941 0.3348139 -0.2107465 -0.4835267 -0.7563069 -1.0290871, lcolor(erose)) yline(0, lcolor(gs12)) ///
title("Sorghum") legend(off) saving(sor, replace)

graph export "sorghum.eps", as(eps) replace

twoway (scatter rtrn r_shk if crops == 4, sort mcolor(forest_green) msize(small) ///
ytitle("Returns to CA") ylabel(, nogrid labsize(small)) yscale(r(-2 3.5)) ylabel(-2 -1 0 1 2 3, labsize(small)) ///
xtitle("Rainfall Deviation") xlabel(1.4259347 "+2.5" 1.1531545 "+2.0" 0.8803743 "+1.5" 0.6075941 "+1.0" 0.3348139 "+0.5" .0620337 "0" ///
-0.2107465 "-0.5" -0.4835267 "-1.0" -0.7563069 "-1.5" -1.0290871 "-2.0" -1.3018673 "-2.5", nogrid labsize(small))), ///
xline(1.1531545 0.8803743 0.6075941 0.3348139 -0.2107465 -0.4835267 -0.7563069 -1.0290871, lcolor(erose)) yline(0, lcolor(gs12)) ///
title("Millet") legend(off) saving(mil, replace)

graph export "millet.eps", as(eps) replace

twoway (scatter rtrn r_shk if crops == 5, sort mcolor(teal) msize(small) ///
ytitle("Returns to CA") ylabel(, nogrid labsize(small)) yscale(r(-2 3.5)) ylabel(-2 -1 0 1 2 3, labsize(small)) ///
xtitle("Rainfall Deviation") xlabel(1.4259347 "+2.5" 1.1531545 "+2.0" 0.8803743 "+1.5" 0.6075941 "+1.0" 0.3348139 "+0.5" .0620337 "0" ///
-0.2107465 "-0.5" -0.4835267 "-1.0" -0.7563069 "-1.5" -1.0290871 "-2.0" -1.3018673 "-2.5", nogrid labsize(small))), ///
xline(1.1531545 0.8803743 0.6075941 0.3348139 -0.2107465 -0.4835267 -0.7563069 -1.0290871, lcolor(erose)) yline(0, lcolor(gs12)) ///
title("Groundnut") legend(off) saving(nut, replace)

graph export "gnut.eps", as(eps) replace

twoway (scatter rtrn r_shk if crops == 6, sort mcolor(dkorange) msize(small) ///
ytitle("Returns to CA") ylabel(, nogrid labsize(small)) yscale(r(-2 3.5)) ylabel(-2 -1 0 1 2 3, labsize(small)) ///
xtitle("Rainfall Deviation") xlabel(1.4259347 "+2.5" 1.1531545 "+2.0" 0.8803743 "+1.5" 0.6075941 "+1.0" 0.3348139 "+0.5" .0620337 "0" ///
-0.2107465 "-0.5" -0.4835267 "-1.0" -0.7563069 "-1.5" -1.0290871 "-2.0" -1.3018673 "-2.5", nogrid labsize(small))), ///
xline(1.1531545 0.8803743 0.6075941 0.3348139 -0.2107465 -0.4835267 -0.7563069 -1.0290871, lcolor(erose)) yline(0, lcolor(gs12)) ///
title("Cowpea") legend(off) saving(pea, replace)

graph export "cow.eps", as(eps) replace

gr combine avg.gph maz.gph sor.gph mil.gph nut.gph pea.gph, col(2) iscale(.5) commonscheme

graph export "rtrn.eps", as(eps) replace


*Calculate CA counterfactual
gen lnynoCA_hat = lnyCA_hat - CA_maize*CA - CA_SRT_maize*short*CA - CA_SRP_maize*surpl*CA if crops == 1
replace lnynoCA_hat = lnyCA_hat - CA_sorghum*CA - CA_SRT_sorghum*short*CA - CA_SRP_sorghum*surpl*CA if crops == 2
replace lnynoCA_hat = lnyCA_hat - CA_millet*CA - CA_SRT_millet*short*CA - CA_SRP_millet*surpl*CA if crops == 4
replace lnynoCA_hat = lnyCA_hat - CA_gnut*CA - CA_SRT_gnut*short*CA - CA_SRP_gnut*surpl*CA if crops == 5
replace lnynoCA_hat = lnyCA_hat - CA_ppea*CA - CA_SRT_ppea*short*CA - CA_SRP_ppea*surpl*CA if crops == 6
replace lnynoCA_hat = lnyTC_hat if CA == 0

replace lnyCA_hat = lnyTC_hat + CA_maize*CA + CA_SRT_maize*short*CA + CA_SRP_maize*surpl*CA if crops == 1 & CA == 0
replace lnyCA_hat = lnyTC_hat + CA_sorghum*CA + CA_SRT_sorghum*short*CA + CA_SRP_sorghum*surpl*CA if crops == 2 & CA == 0
replace lnyCA_hat = lnyTC_hat + CA_millet*CA + CA_SRT_millet*short*CA + CA_SRP_millet*surpl*CA if crops == 4 & CA == 0
replace lnyCA_hat = lnyTC_hat + CA_gnut*CA + CA_SRT_gnut*short*CA + CA_SRP_gnut*surpl*CA if crops == 5 & CA == 0
replace lnyCA_hat = lnyTC_hat + CA_ppea*CA + CA_SRT_ppea*short*CA + CA_SRP_ppea*surpl*CA if crops == 6 & CA == 0

gen rev_CA = (390/1000)*exp(lnyCA_hat) if crops == 1 | crops == 2 | crops == 4
replace rev_CA = (500/1000)*exp(lnyCA_hat) if crops == 5
replace rev_CA = (900/1000)*exp(lnyCA_hat) if crops == 6

gen rev_noCA = (390/1000)*exp(lnynoCA_hat) if crops == 1 | crops == 2 | crops == 4
replace rev_noCA = (500/1000)*exp(lnynoCA_hat) if crops == 5
replace rev_noCA = (900/1000)*exp(lnynoCA_hat) if crops == 6

twoway (lpolyci rev_CA r_shk, sort lcolor(ebblue%90) fcolor(gs15%70) alcolor(ebblue%90) level(90) ///
ytitle("Revenue (USD)") ylabel(, nogrid labsize(small)) yscale(r(0 2000)) ylabel(0 500 1000 1500 2000, labsize(small))) || ///
(lpolyci rev_noCA r_shk, sort lcolor(gray%90) fcolor(gs15%70) alcolor(gray%90) level(90) ///
xtitle("Rainfall Deviation") xlabel(1.4259347 "+2.5" 1.1531545 "+2.0" 0.8803743 "+1.5" 0.6075941 "+1.0" 0.3348139 "+0.5" .0620337 "0" ///
-0.2107465 "-0.5" -0.4835267 "-1.0" -0.7563069 "-1.5" -1.0290871 "-2.0" -1.3018673 "-2.5", nogrid labsize(small))), ///
xline(1.1531545 0.8803743 0.6075941 0.3348139 -0.2107465 -0.4835267 -0.7563069 -1.0290871, lcolor(erose)) ///
title("Average") legend(order(2 4) label(2 "CA") label(4 "TC") pos(6) col(2)) saving(pavg, replace)

graph export "pavg.png", as(png) replace

twoway (lpolyci rev_CA r_shk if crops == 1, sort lcolor(navy%90) fcolor(gs15%70) alcolor(navy%90) level(90)  ///
ytitle("Revenue (USD)") ylabel(, nogrid labsize(small)) yscale(r(0 2000)) ylabel(0 500 1000 1500 2000, labsize(small))) || ///
(lpolyci rev_noCA r_shk if crops == 1, sort lcolor(gray%90) fcolor(gs15%70) alcolor(gray%90) level(90) ///
xtitle("Rainfall Deviation") xlabel(1.4259347 "+2.5" 1.1531545 "+2.0" 0.8803743 "+1.5" 0.6075941 "+1.0" 0.3348139 "+0.5" .0620337 "0" ///
-0.2107465 "-0.5" -0.4835267 "-1.0" -0.7563069 "-1.5" -1.0290871 "-2.0" -1.3018673 "-2.5", nogrid labsize(small))), ///
xline(1.1531545 0.8803743 0.6075941 0.3348139 -0.2107465 -0.4835267 -0.7563069 -1.0290871, lcolor(erose)) ///
title("Maize") legend(order(2 4) label(2 "CA") label(4 "TC") pos(6) col(2)) saving(pmaz, replace)

graph export "pmaz.png", as(png) replace

twoway (lpolyci rev_CA r_shk if crops == 2, sort lcolor(maroon%90) fcolor(gs15%70) alcolor(maroon%90) level(90)  ///
ytitle("Revenue (USD)") ylabel(, nogrid labsize(small)) yscale(r(0 2000)) ylabel(0 500 1000 1500 2000, labsize(small))) || ///
(lpolyci rev_noCA r_shk if crops == 2, sort lcolor(gray%90) fcolor(gs15%70) alcolor(gray%90) level(90)  ///
xtitle("Rainfall Deviation") xlabel(1.4259347 "+2.5" 1.1531545 "+2.0" 0.8803743 "+1.5" 0.6075941 "+1.0" 0.3348139 "+0.5" .0620337 "0" ///
-0.2107465 "-0.5" -0.4835267 "-1.0" -0.7563069 "-1.5" -1.0290871 "-2.0" -1.3018673 "-2.5", nogrid labsize(small))),  ///
xline(1.1531545 0.8803743 0.6075941 0.3348139 -0.2107465 -0.4835267 -0.7563069 -1.0290871, lcolor(erose)) ///
title("Sorghum") legend(order(2 4) label(2 "CA") label(4 "TC") pos(6) col(2)) saving(psor, replace)

graph export "psor.png", as(png) replace

twoway (lpolyci rev_CA r_shk if crops == 4, sort lcolor(forest_green%70) fcolor(gs15%70) alcolor(forest_green%70) level(90) ///
ytitle("Revenue (USD)") ylabel(, nogrid labsize(small)) yscale(r(0 2000)) ylabel(0 500 1000 1500 2000, labsize(small))) || ///
(lpolyci rev_noCA r_shk if crops == 4, sort lcolor(gray%70) fcolor(gs15%70) alcolor(gray%90) level(90) ///
xtitle("Rainfall Deviation") xlabel(1.4259347 "+2.5" 1.1531545 "+2.0" 0.8803743 "+1.5" 0.6075941 "+1.0" 0.3348139 "+0.5" .0620337 "0" ///
-0.2107465 "-0.5" -0.4835267 "-1.0" -0.7563069 "-1.5" -1.0290871 "-2.0" -1.3018673 "-2.5", nogrid labsize(small))), ///
xline(1.1531545 0.8803743 0.6075941 0.3348139 -0.2107465 -0.4835267 -0.7563069 -1.0290871, lcolor(erose)) ///
title("Millet") legend(order(2 4) label(2 "CA") label(4 "TC") pos(6) col(2)) saving(pmil, replace)

graph export "pmil.png", as(png) replace

twoway (lpolyci rev_CA r_shk if crops == 5, sort lcolor(teal%90) fcolor(gs15%70) alcolor(teal%90) level(90) ///
ytitle("Revenue (USD)") ylabel(, nogrid labsize(small)) yscale(r(0 2000)) ylabel(0 500 1000 1500 2000, labsize(small))) || ///
(lpolyci rev_noCA r_shk if crops == 5, sort lcolor(gray%90) fcolor(gs15%70) alcolor(gray%90) level(90) ///
xtitle("Rainfall Deviation") xlabel(1.4259347 "+2.5" 1.1531545 "+2.0" 0.8803743 "+1.5" 0.6075941 "+1.0" 0.3348139 "+0.5" .0620337 "0" ///
-0.2107465 "-0.5" -0.4835267 "-1.0" -0.7563069 "-1.5" -1.0290871 "-2.0" -1.3018673 "-2.5", nogrid labsize(small))), ///
xline(1.1531545 0.8803743 0.6075941 0.3348139 -0.2107465 -0.4835267 -0.7563069 -1.0290871, lcolor(erose)) ///
title("Groundnut") legend(order(2 4) label(2 "CA") label(4 "TC") pos(6) col(2)) saving(pnut, replace)

graph export "pnut.png", as(png) replace

twoway (lpolyci rev_CA r_shk if crops == 6, sort lcolor(dkorange%90) fcolor(gs15%70) alcolor(dkorange%90) level(90) ///
ytitle("Revenue (USD)") ylabel(, nogrid labsize(small)) yscale(r(0 2000)) ylabel(0 500 1000 1500 2000, labsize(small))) || ///
(lpolyci rev_noCA r_shk if crops == 6, sort lcolor(gray%90) fcolor(gs15%70) alcolor(gray%90) level(90) ///
xtitle("Rainfall Deviation") xlabel(1.4259347 "+2.5" 1.1531545 "+2.0" 0.8803743 "+1.5" 0.6075941 "+1.0" 0.3348139 "+0.5" .0620337 "0" ///
-0.2107465 "-0.5" -0.4835267 "-1.0" -0.7563069 "-1.5" -1.0290871 "-2.0" -1.3018673 "-2.5", nogrid labsize(small))), ///
xline(1.1531545 0.8803743 0.6075941 0.3348139 -0.2107465 -0.4835267 -0.7563069 -1.0290871, lcolor(erose)) ///
title("Cowpea") legend(order(2 4) label(2 "CA") label(4 "TC") pos(6) col(2)) saving(ppea, replace)

graph export "ppea.png", as(png) replace

gr combine pavg.gph pmaz.gph psor.gph pmil.gph pnut.gph ppea.gph, col(2) iscale(.5) commonscheme

graph export "revenue.png", as(png) replace

***************************************************
****Table 8: Plot FE and CRE***********************
***************************************************

duplicates tag plot_id year, gen(pcount)
tab pcount

keep if pcount==0
drop pcount

duplicates tag plot_id, generate(pcount)
tab pcount

drop if pcount==0

sort plot_id year

*Plot CRE with CA as endogenous
drop BcroXlnbas_1bar-AcroXlnare_6bar

** Generate variables for CRE
local z11 BcroXlnbas_1-AcroXlnare_1 BcroXlnbas_2-AcroXlnare_2 BcroXlnbas_4-AcroXlnare_4 BcroXlnbas_5-AcroXlnare_5 BcroXlnbas_6-AcroXlnare_6
local z11bar
local i=1
foreach var of varlist `z11' {
	qui egen `var'bar=mean(`var'), by(plot_id)
	local z11bar `z11bar' `var'bar
local i=`i'+1
}

*IMR Wooldridge IV method, results are not presented in paper

*IV with rain shortage and surplus, with plot CRE
local maize SRTcroXshort_1 SRPcroXsurpl_1 BcroXlnbas_1 TcroXlntop_1 ScroXlnsee_1 AcroXlnare_1 
local sorghum SRTcroXshort_2 SRPcroXsurpl_2 BcroXlnbas_2 TcroXlntop_2 ScroXlnsee_2 AcroXlnare_2 
local millet SRTcroXshort_4 SRPcroXsurpl_4 BcroXlnbas_4 TcroXlntop_4 ScroXlnsee_4 AcroXlnare_4 
local gnut SRTcroXshort_5 SRPcroXsurpl_5 BcroXlnbas_5 TcroXlntop_5 ScroXlnsee_5 AcroXlnare_5 
local ppea SRTcroXshort_6 SRPcroXsurpl_6 BcroXlnbas_6 TcroXlntop_6 ScroXlnsee_6 AcroXlnare_6 

xi: probit CA wardNGO `maize' `sorghum' `millet' `gnut' `ppea' crop_type1- crop_type5 i.year BcroXlnbas_1bar-AcroXlnare_6bar, noconstant vce(cluster plot_id)

predict pl3, xb
gen IMRpl3 = normalden(pl3) / normal(pl3)     /* gets the inverse mills ratio*/

est store CAssIV4

*First stage regression results for Appendix Table A4
esttab CAsIV1 CAsIV2 CAssIV3 CAssIV4 using tableA4.tex, replace f ///
	label booktabs b(3) se(3) eqlabels(none) alignment(S)  ///
	drop(SRTcroXshort_* SRPcroXsurpl_* BcroXlnbas_* TcroXlntop_* ScroXlnsee_* AcroXlnare_* crop_type* _Iyear_* _cons) ///
	star(* 0.10 ** 0.05 *** 0.01) nogaps ///
	stats(N ll, fmt(0 0) layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}") labels(`"Observations"' `"Log Likelihood"'))

*Second stage IV regressions that are used in the paper

*Generate crop specific IV with rain shortage and surplus
xi I.crops|IMRpl3, prefix(pl3) noomit
drop pl3crops_1 pl3crops_2 pl3crops_4 pl3crops_5 pl3crops_6

gen pl3IV_srp = IMRpl3*surpl
xi I.crops|pl3IV_srp, prefix(plrp) noomit
drop plrpcrops_1 plrpcrops_2 plrpcrops_4 plrpcrops_5 plrpcrops_6

gen pl3IV_srt = IMRpl3*short
xi I.crops|pl3IV_srt, prefix(plrt) noomit
drop plrtcrops_1 plrtcrops_2 plrtcrops_4 plrtcrops_5 plrtcrops_6

*Endogenous CA, with rain shortage and surplus, with plot fixed effects
local maize SRTcroXshort_1 SRPcroXsurpl_1 BcroXlnbas_1 TcroXlntop_1 ScroXlnsee_1 AcroXlnare_1 
local sorghum SRTcroXshort_2 SRPcroXsurpl_2 BcroXlnbas_2 TcroXlntop_2 ScroXlnsee_2 AcroXlnare_2 
local millet SRTcroXshort_4 SRPcroXsurpl_4 BcroXlnbas_4 TcroXlntop_4 ScroXlnsee_4 AcroXlnare_4 
local gnut SRTcroXshort_5 SRPcroXsurpl_5 BcroXlnbas_5 TcroXlntop_5 ScroXlnsee_5 AcroXlnare_5 
local ppea SRTcroXshort_6 SRPcroXsurpl_6 BcroXlnbas_6 TcroXlntop_6 ScroXlnsee_6 AcroXlnare_6 

xi: ivreg2 lnyield ( CAcroXCA_1 CAcroXCA_2 CAcroXCA_4 CAcroXCA_5 CAcroXCA_6 CARcroXca_sr_1 CARcroXca_sr_2 CARcroXca_sr_4 CARcroXca_sr_5 CARcroXca_sr_6 ///
 CAPcroXca_sr_1 CAPcroXca_sr_2 CAPcroXca_sr_4 CAPcroXca_sr_5 CAPcroXca_sr_6 = pl3croXIMRpl_1 pl3croXIMRpl_2 pl3croXIMRpl_4 pl3croXIMRpl_5 pl3croXIMRpl_6 ///
 plrpcroXpl3IV_1 plrpcroXpl3IV_2 plrpcroXpl3IV_4 plrpcroXpl3IV_5 plrpcroXpl3IV_6 plrtcroXpl3IV_1 plrtcroXpl3IV_2 plrtcroXpl3IV_4 plrtcroXpl3IV_5 plrtcroXpl3IV_6 ) ///
 `maize' `sorghum' `millet' `gnut' `ppea' crop_type1- crop_type5 i.year BcroXlnbas_1bar-AcroXlnare_6bar, first noconstant cluster(plot_id)

est store  pl3end

	*Calculate p-value with n = 2 & K = 15
	local K2 = min(100,e(exexog_ct))
	gen pval8_10 = 1-nchi2(`K2',`K2'*1.84,`K2'*e(widstat))
	gen pval8_15 = 1-nchi2(`K2',`K2'*0.61,`K2'*e(widstat))
	gen pval8_20 = 1-nchi2(`K2',`K2'*0.35,`K2'*e(widstat))
	gen pval8_25 = 1-nchi2(`K2',`K2'*0.22,`K2'*e(widstat))

	sum pval8_10 pval8_15 pval8_20 pval8_25

	sum pval*
	
*Limited coefficients for Table 8
esttab CAsrtend CAsrpend CArprtend pl3end using table8.tex, replace f ///
	label booktabs b(3) se(3) eqlabels(none) alignment(S)  ///	
	drop(BcroXlnbas_* TcroXlntop_* ScroXlnsee_* AcroXlnare_* crop_type* _Iyear_* ) ///
	star(* 0.10 ** 0.05 *** 0.01) nogaps ///
	order(CAcroXCA_1 SRTcroXshort_1 CARcroXca_sr_1 SRPcroXsurpl_1 CAPcroXca_sr_1 BcroXlnbas_1 TcroXlntop_1 ScroXlnsee_1 AcroXlnare_1 ///
	CAcroXCA_2 SRTcroXshort_2 CARcroXca_sr_2 SRPcroXsurpl_2 CAPcroXca_sr_2 BcroXlnbas_2 TcroXlntop_2 ScroXlnsee_2 AcroXlnare_2 ///
	CAcroXCA_4 SRTcroXshort_4 CARcroXca_sr_4 SRPcroXsurpl_4 CAPcroXca_sr_4 BcroXlnbas_4 TcroXlntop_4 ScroXlnsee_4 AcroXlnare_4 ///
	CAcroXCA_5 SRTcroXshort_5 CARcroXca_sr_5 SRPcroXsurpl_5 CAPcroXca_sr_5 BcroXlnbas_5 TcroXlntop_5 ScroXlnsee_5 AcroXlnare_5 ///
	CAcroXCA_6 SRTcroXshort_6 CARcroXca_sr_6 SRPcroXsurpl_6 CAPcroXca_sr_6 BcroXlnbas_6 TcroXlntop_6 ScroXlnsee_6 AcroXlnare_6 ) ///
	refcat(CAcroXCA_1 "\emph{Maize}" CAcroXCA_2 "\emph{Sorghum}" CAcroXCA_4 "\emph{Millet}" CAcroXCA_5 "\emph{Groundnut}" CAcroXCA_6 "\emph{Cowpea}", nolabel) ///
	stats(N ll idstat archi2 widstat idp archi2p , fmt(0 0 3 2 3 3 3) layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" ///
	"\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}") labels(`"Observations"' `"Log Likelihood"' ///
	`"Kleibergen-Paap LM stat"' `"Anderson-Rubin Wald stat"' `"Kleibergen-Paap Wald stat"' `"Kleibergen-Paap p-value"' `"Anderson-Rubin p-value"' ))

*All coefficients for Appendix Table A5	
esttab CAsrtend CAsrpend CArprtend pl3end using tableA5.tex, replace f ///
	label booktabs b(3) se(3) eqlabels(none) alignment(S)  ///
	drop(crop_type* _Iyear_* ) ///
	star(* 0.10 ** 0.05 *** 0.01) nogaps ///
	order(CAcroXCA_1 SRTcroXshort_1 CARcroXca_sr_1 SRPcroXsurpl_1 CAPcroXca_sr_1 BcroXlnbas_1 TcroXlntop_1 ScroXlnsee_1 AcroXlnare_1 ///
	CAcroXCA_2 SRTcroXshort_2 CARcroXca_sr_2 SRPcroXsurpl_2 CAPcroXca_sr_2 BcroXlnbas_2 TcroXlntop_2 ScroXlnsee_2 AcroXlnare_2 ///
	CAcroXCA_4 SRTcroXshort_4 CARcroXca_sr_4 SRPcroXsurpl_4 CAPcroXca_sr_4 BcroXlnbas_4 TcroXlntop_4 ScroXlnsee_4 AcroXlnare_4 ///
	CAcroXCA_5 SRTcroXshort_5 CARcroXca_sr_5 SRPcroXsurpl_5 CAPcroXca_sr_5 BcroXlnbas_5 TcroXlntop_5 ScroXlnsee_5 AcroXlnare_5 ///
	CAcroXCA_6 SRTcroXshort_6 CARcroXca_sr_6 SRPcroXsurpl_6 CAPcroXca_sr_6 BcroXlnbas_6 TcroXlntop_6 ScroXlnsee_6 AcroXlnare_6 ) ///
	refcat(CAcroXCA_1 "\emph{Maize}" CAcroXCA_2 "\emph{Sorghum}" CAcroXCA_4 "\emph{Millet}" CAcroXCA_5 "\emph{Groundnut}" CAcroXCA_6 "\emph{Cowpea}", nolabel) ///
	stats(N ll idstat archi2 widstat idp archi2p , fmt(0 0 3 2 3 3 3) layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" ///
	"\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}") labels(`"Observations"' `"Log Likelihood"' ///
	`"Kleibergen-Paap LM stat"' `"Anderson-Rubin Wald stat"' `"Kleibergen-Paap Wald stat"' `"Kleibergen-Paap p-value"' `"Anderson-Rubin p-value"' ))


****************************************************
****Appendix Table A1: Alternative Rainfall Spec****
****************************************************


*First, production function without CA interaction
use "Michler_et_al_JEEM_data.dta", clear

*CA, with rain shock, with househould fixed effects
local maize CAcroXCA_1 SHKcroXshock_1 BcroXlnbas_1 TcroXlntop_1 ScroXlnsee_1 AcroXlnare_1 
local sorghum CAcroXCA_2 SHKcroXshock_2 BcroXlnbas_2 TcroXlntop_2 ScroXlnsee_2 AcroXlnare_2 
local millet CAcroXCA_4 SHKcroXshock_4  BcroXlnbas_4 TcroXlntop_4 ScroXlnsee_4 AcroXlnare_4 
local gnut CAcroXCA_5 SHKcroXshock_5 BcroXlnbas_5 TcroXlntop_5 ScroXlnsee_5 AcroXlnare_5 
local ppea CAcroXCA_6 SHKcroXshock_6 BcroXlnbas_6 TcroXlntop_6 ScroXlnsee_6 AcroXlnare_6 

xi: reg lnyield `maize' `sorghum' `millet' `gnut' `ppea' crop_type1- crop_type5 i.year i.rc, noconstant vce(cluster crc)

est store rains

xi I.crops|oct_shock, prefix(OCT) noomit
xi I.crops|nov_shock, prefix(NOV) noomit
xi I.crops|dec_shock, prefix(DEC) noomit
xi I.crops|jan_shock, prefix(JAN) noomit
xi I.crops|feb_shock, prefix(FEB) noomit
xi I.crops|mar_shock, prefix(MAR) noomit
xi I.crops|apr_shock, prefix(APR) noomit

local maize CAcroXCA_1 OCTcroXoct_s_1 NOVcroXnov_s_1 DECcroXdec_s_1 JANcroXjan_s_1 FEBcroXfeb_s_1 MARcroXmar_s_1 APRcroXapr_s_1 BcroXlnbas_1 TcroXlntop_1 ScroXlnsee_1 AcroXlnare_1 
local sorghum CAcroXCA_2 OCTcroXoct_s_2 NOVcroXnov_s_2 DECcroXdec_s_2 JANcroXjan_s_2 FEBcroXfeb_s_2 MARcroXmar_s_2 APRcroXapr_s_2 BcroXlnbas_2 TcroXlntop_2 ScroXlnsee_2 AcroXlnare_2 
local millet CAcroXCA_4 OCTcroXoct_s_4 NOVcroXnov_s_4 DECcroXdec_s_4 JANcroXjan_s_4 FEBcroXfeb_s_4 MARcroXmar_s_4 APRcroXapr_s_4 BcroXlnbas_4 TcroXlntop_4 ScroXlnsee_4 AcroXlnare_4 
local gnut CAcroXCA_5 OCTcroXoct_s_5 NOVcroXnov_s_5 DECcroXdec_s_5 JANcroXjan_s_5 FEBcroXfeb_s_5 MARcroXmar_s_5 APRcroXapr_s_5 BcroXlnbas_5 TcroXlntop_5 ScroXlnsee_5 AcroXlnare_5 
local ppea CAcroXCA_6 OCTcroXoct_s_6 NOVcroXnov_s_6 DECcroXdec_s_6 JANcroXjan_s_6 FEBcroXfeb_s_6 MARcroXmar_s_6 APRcroXapr_s_6 BcroXlnbas_6 TcroXlntop_6 ScroXlnsee_6 AcroXlnare_6 

xi: reg lnyield `maize' `sorghum' `millet' `gnut' `ppea' crop_type1- crop_type5 i.year i.rc, noconstant vce(cluster crc)

est store rainm

*Limited coefficients for Appendix Table A1
esttab rains using tableA1.tex, replace f unstack ///
	label booktabs b(3) se(3) eqlabels(none) alignment(S)  ///
	drop(CAcroXCA_* BcroXlnbas_* TcroXlntop_* ScroXlnsee_* AcroXlnare_* crop_type* _Iyear_* _Irc_*) ///
	star(* 0.10 ** 0.05 *** 0.01) nogaps ///
	refcat(OCTcroXoct_s_1 "\emph{Maize}" OCTcroXoct_s_2 "\emph{Sorghum}" OCTcroXoct_s_4 "\emph{Millet}" ///
	OCTcroXoct_s_5 "\emph{Groundnut}" OCTcroXoct_s_6 "\emph{Cowpea}", nolabel) ///
	stats(N r2, fmt(0 3) layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{S}{@}") labels(`"Observations"' `"\(R^{2}\)"'))

esttab rainm using tableA1.tex, append f unstack ///
	label booktabs b(3) se(3) eqlabels(none) alignment(S)  ///
	drop(CAcroXCA_* BcroXlnbas_* TcroXlntop_* ScroXlnsee_* AcroXlnare_* crop_type* _Iyear_* _Irc_*) ///
	star(* 0.10 ** 0.05 *** 0.01) nogaps ///
	refcat(OCTcroXoct_s_1 "\emph{Maize}" OCTcroXoct_s_2 "\emph{Sorghum}" OCTcroXoct_s_4 "\emph{Millet}" ///
	OCTcroXoct_s_5 "\emph{Groundnut}" OCTcroXoct_s_6 "\emph{Cowpea}", nolabel) ///
	stats(N r2, fmt(0 3) layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{S}{@}") labels(`"Observations"' `"\(R^{2}\)"'))

	
*******************************************************
****Appendix Table A6: Alternative Rainfall Measure****
*******************************************************


use "Michler_et_al_JEEM_data.dta", clear

*one half standard deviation
replace shock = 0 if shock<=0.61281315 & shock>=0.33907725

*crop-rain interactions
xi I.crops|lntot, prefix(TOT) noomit
xi I.crops|shock, prefix(SHK) noomit
xi I.crops|short, prefix(SRT) noomit
xi I.crops|surpl, prefix(SRP) noomit
drop TOTcrops_1- TOTcrops_6 SHKcrops_1- SHKcrops_6 SRTcrops_1- SRTcrops_6 SRPcrops_1- SRPcrops_6
order TOTcroXlntot_1 SHKcroXshock_1 SRTcroXshort_1 SRPcroXsurpl_1, after(CAcroXCA_1)
order TOTcroXlntot_2 SHKcroXshock_2 SRTcroXshort_2 SRPcroXsurpl_2, after(CAcroXCA_2)
order TOTcroXlntot_4 SHKcroXshock_4 SRTcroXshort_4 SRPcroXsurpl_4, after(CAcroXCA_4)
order TOTcroXlntot_5 SHKcroXshock_5 SRTcroXshort_5 SRPcroXsurpl_5, after(CAcroXCA_5)
order TOTcroXlntot_6 SHKcroXshock_6 SRTcroXshort_6 SRPcroXsurpl_6, after(CAcroXCA_6)

*Generate rainfall-CA-crop intereaction
replace ca_tot = CA*lntot
replace ca_shk = CA*shock
replace ca_srt = CA*short
replace ca_srp = CA*surpl

xi I.crops|ca_tot, prefix(CAT) noomit
xi I.crops|ca_shk, prefix(CAS) noomit
xi I.crops|ca_srt, prefix(CAR) noomit
xi I.crops|ca_srp, prefix(CAP) noomit
drop CATcrops_1- CATcrops_6 CAScrops_1- CAScrops_6 CARcrops_1- CARcrops_6 CAPcrops_1- CAPcrops_6
order CATcroXca_to_1, after(TOTcroXlntot_1)
order CATcroXca_to_2, after(TOTcroXlntot_2)
order CATcroXca_to_4, after(TOTcroXlntot_4)
order CATcroXca_to_5, after(TOTcroXlntot_5)
order CATcroXca_to_6, after(TOTcroXlntot_6)

order CAScroXca_sh_1, after(SHKcroXshock_1)
order CAScroXca_sh_2, after(SHKcroXshock_2)
order CAScroXca_sh_4, after(SHKcroXshock_4)
order CAScroXca_sh_5, after(SHKcroXshock_5)
order CAScroXca_sh_6, after(SHKcroXshock_6)

order CARcroXca_sr_1, after(SRTcroXshort_1)
order CARcroXca_sr_2, after(SRTcroXshort_2)
order CARcroXca_sr_4, after(SRTcroXshort_4)
order CARcroXca_sr_5, after(SRTcroXshort_5)
order CARcroXca_sr_6, after(SRTcroXshort_6)

order CAPcroXca_sr_1, after(SRPcroXsurpl_1)
order CAPcroXca_sr_2, after(SRPcroXsurpl_2)
order CAPcroXca_sr_4, after(SRPcroXsurpl_4)
order CAPcroXca_sr_5, after(SRPcroXsurpl_5)
order CAPcroXca_sr_6, after(SRPcroXsurpl_6)


*******************************************
****Label everything***********************
*******************************************

	
	*Labels
	label variable CA "\hspace{0.1cm} CA $(=1)$"
	label variable ca_shk "\hspace{0.1cm} CA $\times$ rainfall shock"
	label variable ca_srt "\hspace{0.1cm} CA $\times$ rainfall shortage"
	label variable ca_srp "\hspace{0.1cm} CA $\times$ rainfall surplus"
	label variable lnbasal "\hspace{0.1cm} $\ln$(basal)"
	label variable lntop "\hspace{0.1cm} $\ln$(top)"
	label variable lnseed "\hspace{0.1cm} $\ln$(seed)"
	label variable lnaream2 "\hspace{0.1cm} $\ln$(area)"
	label variable topkg "\hspace{0.1cm} top applied fertilizer (kg)"
	label variable basalkg "\hspace{0.1cm} basal applied fertilizer (kg)"
	label variable seedkg "\hspace{0.1cm} seed planted (kg)"
	label variable aream2 "\hspace{0.1cm} area planted (m2)"
	label variable pdate "\hspace{0.1cm} planting date (weeks)"
	label variable pdate2 "\hspace{0.1cm} planting date$^2$ (weeks)"
	label variable tot_season "\hspace{0.1cm} culmulative rainfall (mm)"
	label variable shock "\hspace{0.1cm} rainfall shock"
	label variable surpl "\hspace{0.1cm} rainfall surplus"
	label variable short "\hspace{0.1cm} rainfall shortage"
	label variable CAcroXCA_1 "\hspace{0.1cm} CA $(=1)$"
	label variable TOTcroXlntot_1 "\hspace{0.1cm} cumulative rainfall"
	label variable SHKcroXshock_1 "\hspace{0.1cm} rainfall shock"
	label variable CATcroXca_to_1 "\hspace{0.1cm} CA $\times$ rainfall"
	label variable CAScroXca_sh_1 "\hspace{0.1cm} CA $\times$ rainfall shock"
	label variable SRTcroXshort_1 "\hspace{0.1cm} rainfall shortage"
	label variable CARcroXca_sr_1 "\hspace{0.1cm} CA $\times$ rainfall shortage"
	label variable SRPcroXsurpl_1 "\hspace{0.1cm} rainfall surplus"
	label variable CAPcroXca_sr_1 "\hspace{0.1cm} CA $\times$ rainfall surplus"
	label variable BcroXlnbas_1 "\hspace{0.1cm} $\ln$(basal)"
	label variable TcroXlntop_1 "\hspace{0.1cm} $\ln$(top)"
	label variable ScroXlnsee_1 "\hspace{0.1cm} $\ln$(seed)"
	label variable AcroXlnare_1 "\hspace{0.1cm} $\ln$(area)"
	label variable pcroXpdate_1 "\hspace{0.1cm} planting date"
	label variable p2croXpdate_1 "\hspace{0.1cm} planting date$^2$"
	label variable CAcroXCA_2 "\hspace{0.1cm} CA $(=1)$"
	label variable TOTcroXlntot_2 "\hspace{0.1cm} cumulative rainfall"
	label variable SHKcroXshock_2 "\hspace{0.1cm} rainfall shock"
	label variable CATcroXca_to_2 "\hspace{0.1cm} CA $\times$ rainfall"
	label variable CAScroXca_sh_2 "\hspace{0.1cm} CA $\times$ rainfall shock"
	label variable CAScroXca_sh_2 "\hspace{0.1cm} CA $\times$ rainfall shock"
	label variable SRTcroXshort_2 "\hspace{0.1cm} rainfall shortage"
	label variable CARcroXca_sr_2 "\hspace{0.1cm} CA $\times$ rainfall shortage"
	label variable SRPcroXsurpl_2 "\hspace{0.1cm} rainfall surplus"
	label variable CAPcroXca_sr_2 "\hspace{0.1cm} CA $\times$ rainfall surplus"
	label variable BcroXlnbas_2 "\hspace{0.1cm} $\ln$(basal)"
	label variable TcroXlntop_2 "\hspace{0.1cm} $\ln$(top)"
	label variable ScroXlnsee_2 "\hspace{0.1cm} $\ln$(seed)"
	label variable AcroXlnare_2 "\hspace{0.1cm} $\ln$(area)"
	label variable pcroXpdate_2 "\hspace{0.1cm} planting date"
	label variable p2croXpdate_2 "\hspace{0.1cm} planting date$^2$"
	label variable CAcroXCA_4 "\hspace{0.1cm} CA $(=1)$"
	label variable TOTcroXlntot_4 "\hspace{0.1cm} cumulative rainfall"
	label variable SHKcroXshock_4 "\hspace{0.1cm} rainfall shock"
	label variable CATcroXca_to_4 "\hspace{0.1cm} CA $\times$ rainfall"
	label variable CAScroXca_sh_4 "\hspace{0.1cm} CA $\times$ rainfall shock"
	label variable CAScroXca_sh_4 "\hspace{0.1cm} CA $\times$ rainfall shock"
	label variable SRTcroXshort_4 "\hspace{0.1cm} rainfall shortage"
	label variable CARcroXca_sr_4 "\hspace{0.1cm} CA $\times$ rainfall shortage"
	label variable SRPcroXsurpl_4 "\hspace{0.1cm} rainfall surplus"
	label variable CAPcroXca_sr_4 "\hspace{0.1cm} CA $\times$ rainfall surplus"
	label variable BcroXlnbas_4 "\hspace{0.1cm} $\ln$(basal)"
	label variable TcroXlntop_4 "\hspace{0.1cm} $\ln$(top)"
	label variable ScroXlnsee_4 "\hspace{0.1cm} $\ln$(seed)"
	label variable AcroXlnare_4 "\hspace{0.1cm} $\ln$(area)"
	label variable pcroXpdate_4 "\hspace{0.1cm} planting date"
	label variable p2croXpdate_4 "\hspace{0.1cm} planting date$^2$"
	label variable CAcroXCA_5 "\hspace{0.1cm} CA $(=1)$"
	label variable TOTcroXlntot_5 "\hspace{0.1cm} cumulative rainfall"
	label variable SHKcroXshock_5 "\hspace{0.1cm} rainfall shock"
	label variable CATcroXca_to_5 "\hspace{0.1cm} CA $\times$ rainfall"
	label variable CAScroXca_sh_5 "\hspace{0.1cm} CA $\times$ rainfall shock"
	label variable CAScroXca_sh_5 "\hspace{0.1cm} CA $\times$ rainfall shock"
	label variable SRTcroXshort_5 "\hspace{0.1cm} rainfall shortage"
	label variable CARcroXca_sr_5 "\hspace{0.1cm} CA $\times$ rainfall shortage"
	label variable SRPcroXsurpl_5 "\hspace{0.1cm} rainfall surplus"
	label variable CAPcroXca_sr_5 "\hspace{0.1cm} CA $\times$ rainfall surplus"
	label variable BcroXlnbas_5 "\hspace{0.1cm} $\ln$(basal)"
	label variable TcroXlntop_5 "\hspace{0.1cm} $\ln$(top)"
	label variable ScroXlnsee_5 "\hspace{0.1cm} $\ln$(seed)"
	label variable AcroXlnare_5 "\hspace{0.1cm} $\ln$(area)"
	label variable pcroXpdate_5 "\hspace{0.1cm} planting date"
	label variable p2croXpdate_5 "\hspace{0.1cm} planting date$^2$"
	label variable CAcroXCA_6 "\hspace{0.1cm} CA $(=1)$"
	label variable TOTcroXlntot_6 "\hspace{0.1cm} cumulative rainfall"
	label variable SHKcroXshock_6 "\hspace{0.1cm} rainfall shock"
	label variable CATcroXca_to_6 "\hspace{0.1cm} CA $\times$ rainfall"
	label variable CAScroXca_sh_6 "\hspace{0.1cm} CA $\times$ rainfall shock"
	label variable CAScroXca_sh_6 "\hspace{0.1cm} CA $\times$ rainfall shock"
	label variable SRTcroXshort_6 "\hspace{0.1cm} rainfall shortage"
	label variable CARcroXca_sr_6 "\hspace{0.1cm} CA $\times$ rainfall shortage"
	label variable SRPcroXsurpl_6 "\hspace{0.1cm} rainfall surplus"
	label variable CAPcroXca_sr_6 "\hspace{0.1cm} CA $\times$ rainfall surplus"
	label variable BcroXlnbas_6 "\hspace{0.1cm} $\ln$(basal)"
	label variable TcroXlntop_6 "\hspace{0.1cm} $\ln$(top)"
	label variable ScroXlnsee_6 "\hspace{0.1cm} $\ln$(seed)"
	label variable AcroXlnare_6 "\hspace{0.1cm} $\ln$(area)"
	label variable pcroXpdate_6 "\hspace{0.1cm} planting date"
	label variable p2croXpdate_6 "\hspace{0.1cm} planting date$^2$"
	label variable CAres "residue"
	label variable CAbsn "basin"
	label variable CArtn "rotation"

** Generate variables for CRE
local z11 BcroXlnbas_1-AcroXlnare_1 BcroXlnbas_2-AcroXlnare_2 BcroXlnbas_4-AcroXlnare_4 BcroXlnbas_5-AcroXlnare_5 BcroXlnbas_6-AcroXlnare_6
local z11bar
local i=1
foreach var of varlist `z11' {
	qui egen `var'bar=mean(`var'), by(rc)
	local z11bar `z11bar' `var'bar
local i=`i'+1
}

*IV with 1/2 s.d. rain shock, with househould CRE
local maize SHKcroXshock_1 BcroXlnbas_1 TcroXlntop_1 ScroXlnsee_1 AcroXlnare_1 
local sorghum SHKcroXshock_2 BcroXlnbas_2 TcroXlntop_2 ScroXlnsee_2 AcroXlnare_2
local millet SHKcroXshock_4 BcroXlnbas_4 TcroXlntop_4 ScroXlnsee_4 AcroXlnare_4 
local gnut SHKcroXshock_5 BcroXlnbas_5 TcroXlntop_5 ScroXlnsee_5 AcroXlnare_5 
local ppea SHKcroXshock_6 BcroXlnbas_6 TcroXlntop_6 ScroXlnsee_6 AcroXlnare_6 

xi: probit CA wardNGO `maize' `sorghum' `millet' `gnut' `ppea' crop_type1- crop_type5 i.year BcroXlnbas_1bar-AcroXlnare_6bar, vce(cluster crc)

predict p4, xb
gen IMR4 = normalden(p4) / normal(p4)     /* gets the inverse mills ratio*/

*Generate crop specific IV for equation 4
xi I.crops|IMR4, prefix(CbIV) noomit
drop CbIVcrops_1 CbIVcrops_2 CbIVcrops_4 CbIVcrops_5 CbIVcrops_6

gen bIV_shk = IMR4*shock
xi I.crops|bIV_shk, prefix(bsIV) noomit
drop bsIVcrops_1 bsIVcrops_2 bsIVcrops_4 bsIVcrops_5 bsIVcrops_6

*Endogenous CA, with 1/2 s.d. rain shock, with househould fixed effects
local maize SHKcroXshock_1 BcroXlnbas_1 TcroXlntop_1 ScroXlnsee_1 AcroXlnare_1 
local sorghum SHKcroXshock_2 BcroXlnbas_2 TcroXlntop_2 ScroXlnsee_2 AcroXlnare_2 
local millet SHKcroXshock_4 BcroXlnbas_4 TcroXlntop_4 ScroXlnsee_4 AcroXlnare_4 
local gnut SHKcroXshock_5 BcroXlnbas_5 TcroXlntop_5 ScroXlnsee_5 AcroXlnare_5 
local ppea SHKcroXshock_6 BcroXlnbas_6 TcroXlntop_6 ScroXlnsee_6 AcroXlnare_6 

xi: ivreg2 lnyield ( CAcroXCA_1 CAcroXCA_2 CAcroXCA_4 CAcroXCA_5 CAcroXCA_6 CAScroXca_sh_1 CAScroXca_sh_2 CAScroXca_sh_4 CAScroXca_sh_5 CAScroXca_sh_6 = ///
 CbIVcroXIMR4_1 CbIVcroXIMR4_2 CbIVcroXIMR4_4 CbIVcroXIMR4_5 CbIVcroXIMR4_6 bsIVcroXbIV_s_1 bsIVcroXbIV_s_2 bsIVcroXbIV_s_4 bsIVcroXbIV_s_5 bsIVcroXbIV_s_6 ) ///
 `maize' `sorghum' `millet' `gnut' `ppea' crop_type1- crop_type5 i.year i.rc, noconstant cluster(crc) partial(crop_type1- crop_type5 i.year i.rc)

est store  CAhsd

drop ca_tot- _Irc_831517

*one standard deviation
replace shock = 0 if shock<=0.7496811 & shock>=0.2022093

*crop-rain interactions
xi I.crops|lntot, prefix(TOT) noomit
xi I.crops|shock, prefix(SHK) noomit
xi I.crops|short, prefix(SRT) noomit
xi I.crops|surpl, prefix(SRP) noomit
drop TOTcrops_1- TOTcrops_6 SHKcrops_1- SHKcrops_6 SRTcrops_1- SRTcrops_6 SRPcrops_1- SRPcrops_6
order TOTcroXlntot_1 SHKcroXshock_1 SRTcroXshort_1 SRPcroXsurpl_1, after(CAcroXCA_1)
order TOTcroXlntot_2 SHKcroXshock_2 SRTcroXshort_2 SRPcroXsurpl_2, after(CAcroXCA_2)
order TOTcroXlntot_4 SHKcroXshock_4 SRTcroXshort_4 SRPcroXsurpl_4, after(CAcroXCA_4)
order TOTcroXlntot_5 SHKcroXshock_5 SRTcroXshort_5 SRPcroXsurpl_5, after(CAcroXCA_5)
order TOTcroXlntot_6 SHKcroXshock_6 SRTcroXshort_6 SRPcroXsurpl_6, after(CAcroXCA_6)

*Generate rainfall-CA-crop intereaction
gen ca_tot = CA*lntot
gen ca_shk = CA*shock
gen ca_srt = CA*short
gen ca_srp = CA*surpl

xi I.crops|ca_tot, prefix(CAT) noomit
xi I.crops|ca_shk, prefix(CAS) noomit
xi I.crops|ca_srt, prefix(CAR) noomit
xi I.crops|ca_srp, prefix(CAP) noomit
drop CATcrops_1- CATcrops_6 CAScrops_1- CAScrops_6 CARcrops_1- CARcrops_6 CAPcrops_1- CAPcrops_6
order CATcroXca_to_1, after(TOTcroXlntot_1)
order CATcroXca_to_2, after(TOTcroXlntot_2)
order CATcroXca_to_4, after(TOTcroXlntot_4)
order CATcroXca_to_5, after(TOTcroXlntot_5)
order CATcroXca_to_6, after(TOTcroXlntot_6)

order CAScroXca_sh_1, after(SHKcroXshock_1)
order CAScroXca_sh_2, after(SHKcroXshock_2)
order CAScroXca_sh_4, after(SHKcroXshock_4)
order CAScroXca_sh_5, after(SHKcroXshock_5)
order CAScroXca_sh_6, after(SHKcroXshock_6)

order CARcroXca_sr_1, after(SRTcroXshort_1)
order CARcroXca_sr_2, after(SRTcroXshort_2)
order CARcroXca_sr_4, after(SRTcroXshort_4)
order CARcroXca_sr_5, after(SRTcroXshort_5)
order CARcroXca_sr_6, after(SRTcroXshort_6)

order CAPcroXca_sr_1, after(SRPcroXsurpl_1)
order CAPcroXca_sr_2, after(SRPcroXsurpl_2)
order CAPcroXca_sr_4, after(SRPcroXsurpl_4)
order CAPcroXca_sr_5, after(SRPcroXsurpl_5)
order CAPcroXca_sr_6, after(SRPcroXsurpl_6)

sort rc crops
egen crc = group (rc crops)


*******************************************
****Label everything***********************
*******************************************

	
	*Labels
	label variable CA "\hspace{0.1cm} CA $(=1)$"
	label variable ca_shk "\hspace{0.1cm} CA $\times$ rainfall shock"
	label variable ca_srt "\hspace{0.1cm} CA $\times$ rainfall shortage"
	label variable ca_srp "\hspace{0.1cm} CA $\times$ rainfall surplus"
	label variable lnbasal "\hspace{0.1cm} $\ln$(basal)"
	label variable lntop "\hspace{0.1cm} $\ln$(top)"
	label variable lnseed "\hspace{0.1cm} $\ln$(seed)"
	label variable lnaream2 "\hspace{0.1cm} $\ln$(area)"
	label variable topkg "\hspace{0.1cm} top applied fertilizer (kg)"
	label variable basalkg "\hspace{0.1cm} basal applied fertilizer (kg)"
	label variable seedkg "\hspace{0.1cm} seed planted (kg)"
	label variable aream2 "\hspace{0.1cm} area planted (m2)"
	label variable pdate "\hspace{0.1cm} planting date (weeks)"
	label variable pdate2 "\hspace{0.1cm} planting date$^2$ (weeks)"
	label variable tot_season "\hspace{0.1cm} culmulative rainfall (mm)"
	label variable shock "\hspace{0.1cm} rainfall shock"
	label variable surpl "\hspace{0.1cm} rainfall surplus"
	label variable short "\hspace{0.1cm} rainfall shortage"
	label variable CAcroXCA_1 "\hspace{0.1cm} CA $(=1)$"
	label variable TOTcroXlntot_1 "\hspace{0.1cm} cumulative rainfall"
	label variable SHKcroXshock_1 "\hspace{0.1cm} rainfall shock"
	label variable CATcroXca_to_1 "\hspace{0.1cm} CA $\times$ rainfall"
	label variable CAScroXca_sh_1 "\hspace{0.1cm} CA $\times$ rainfall shock"
	label variable SRTcroXshort_1 "\hspace{0.1cm} rainfall shortage"
	label variable CARcroXca_sr_1 "\hspace{0.1cm} CA $\times$ rainfall shortage"
	label variable SRPcroXsurpl_1 "\hspace{0.1cm} rainfall surplus"
	label variable CAPcroXca_sr_1 "\hspace{0.1cm} CA $\times$ rainfall surplus"
	label variable BcroXlnbas_1 "\hspace{0.1cm} $\ln$(basal)"
	label variable TcroXlntop_1 "\hspace{0.1cm} $\ln$(top)"
	label variable ScroXlnsee_1 "\hspace{0.1cm} $\ln$(seed)"
	label variable AcroXlnare_1 "\hspace{0.1cm} $\ln$(area)"
	label variable pcroXpdate_1 "\hspace{0.1cm} planting date"
	label variable p2croXpdate_1 "\hspace{0.1cm} planting date$^2$"
	label variable CAcroXCA_2 "\hspace{0.1cm} CA $(=1)$"
	label variable TOTcroXlntot_2 "\hspace{0.1cm} cumulative rainfall"
	label variable SHKcroXshock_2 "\hspace{0.1cm} rainfall shock"
	label variable CATcroXca_to_2 "\hspace{0.1cm} CA $\times$ rainfall"
	label variable CAScroXca_sh_2 "\hspace{0.1cm} CA $\times$ rainfall shock"
	label variable CAScroXca_sh_2 "\hspace{0.1cm} CA $\times$ rainfall shock"
	label variable SRTcroXshort_2 "\hspace{0.1cm} rainfall shortage"
	label variable CARcroXca_sr_2 "\hspace{0.1cm} CA $\times$ rainfall shortage"
	label variable SRPcroXsurpl_2 "\hspace{0.1cm} rainfall surplus"
	label variable CAPcroXca_sr_2 "\hspace{0.1cm} CA $\times$ rainfall surplus"
	label variable BcroXlnbas_2 "\hspace{0.1cm} $\ln$(basal)"
	label variable TcroXlntop_2 "\hspace{0.1cm} $\ln$(top)"
	label variable ScroXlnsee_2 "\hspace{0.1cm} $\ln$(seed)"
	label variable AcroXlnare_2 "\hspace{0.1cm} $\ln$(area)"
	label variable pcroXpdate_2 "\hspace{0.1cm} planting date"
	label variable p2croXpdate_2 "\hspace{0.1cm} planting date$^2$"
	label variable CAcroXCA_4 "\hspace{0.1cm} CA $(=1)$"
	label variable TOTcroXlntot_4 "\hspace{0.1cm} cumulative rainfall"
	label variable SHKcroXshock_4 "\hspace{0.1cm} rainfall shock"
	label variable CATcroXca_to_4 "\hspace{0.1cm} CA $\times$ rainfall"
	label variable CAScroXca_sh_4 "\hspace{0.1cm} CA $\times$ rainfall shock"
	label variable CAScroXca_sh_4 "\hspace{0.1cm} CA $\times$ rainfall shock"
	label variable SRTcroXshort_4 "\hspace{0.1cm} rainfall shortage"
	label variable CARcroXca_sr_4 "\hspace{0.1cm} CA $\times$ rainfall shortage"
	label variable SRPcroXsurpl_4 "\hspace{0.1cm} rainfall surplus"
	label variable CAPcroXca_sr_4 "\hspace{0.1cm} CA $\times$ rainfall surplus"
	label variable BcroXlnbas_4 "\hspace{0.1cm} $\ln$(basal)"
	label variable TcroXlntop_4 "\hspace{0.1cm} $\ln$(top)"
	label variable ScroXlnsee_4 "\hspace{0.1cm} $\ln$(seed)"
	label variable AcroXlnare_4 "\hspace{0.1cm} $\ln$(area)"
	label variable pcroXpdate_4 "\hspace{0.1cm} planting date"
	label variable p2croXpdate_4 "\hspace{0.1cm} planting date$^2$"
	label variable CAcroXCA_5 "\hspace{0.1cm} CA $(=1)$"
	label variable TOTcroXlntot_5 "\hspace{0.1cm} cumulative rainfall"
	label variable SHKcroXshock_5 "\hspace{0.1cm} rainfall shock"
	label variable CATcroXca_to_5 "\hspace{0.1cm} CA $\times$ rainfall"
	label variable CAScroXca_sh_5 "\hspace{0.1cm} CA $\times$ rainfall shock"
	label variable CAScroXca_sh_5 "\hspace{0.1cm} CA $\times$ rainfall shock"
	label variable SRTcroXshort_5 "\hspace{0.1cm} rainfall shortage"
	label variable CARcroXca_sr_5 "\hspace{0.1cm} CA $\times$ rainfall shortage"
	label variable SRPcroXsurpl_5 "\hspace{0.1cm} rainfall surplus"
	label variable CAPcroXca_sr_5 "\hspace{0.1cm} CA $\times$ rainfall surplus"
	label variable BcroXlnbas_5 "\hspace{0.1cm} $\ln$(basal)"
	label variable TcroXlntop_5 "\hspace{0.1cm} $\ln$(top)"
	label variable ScroXlnsee_5 "\hspace{0.1cm} $\ln$(seed)"
	label variable AcroXlnare_5 "\hspace{0.1cm} $\ln$(area)"
	label variable pcroXpdate_5 "\hspace{0.1cm} planting date"
	label variable p2croXpdate_5 "\hspace{0.1cm} planting date$^2$"
	label variable CAcroXCA_6 "\hspace{0.1cm} CA $(=1)$"
	label variable TOTcroXlntot_6 "\hspace{0.1cm} cumulative rainfall"
	label variable SHKcroXshock_6 "\hspace{0.1cm} rainfall shock"
	label variable CATcroXca_to_6 "\hspace{0.1cm} CA $\times$ rainfall"
	label variable CAScroXca_sh_6 "\hspace{0.1cm} CA $\times$ rainfall shock"
	label variable CAScroXca_sh_6 "\hspace{0.1cm} CA $\times$ rainfall shock"
	label variable SRTcroXshort_6 "\hspace{0.1cm} rainfall shortage"
	label variable CARcroXca_sr_6 "\hspace{0.1cm} CA $\times$ rainfall shortage"
	label variable SRPcroXsurpl_6 "\hspace{0.1cm} rainfall surplus"
	label variable CAPcroXca_sr_6 "\hspace{0.1cm} CA $\times$ rainfall surplus"
	label variable BcroXlnbas_6 "\hspace{0.1cm} $\ln$(basal)"
	label variable TcroXlntop_6 "\hspace{0.1cm} $\ln$(top)"
	label variable ScroXlnsee_6 "\hspace{0.1cm} $\ln$(seed)"
	label variable AcroXlnare_6 "\hspace{0.1cm} $\ln$(area)"
	label variable pcroXpdate_6 "\hspace{0.1cm} planting date"
	label variable p2croXpdate_6 "\hspace{0.1cm} planting date$^2$"
	label variable CAres "residue"
	label variable CAbsn "basin"
	label variable CArtn "rotation"
	
** Generate variables for CRE
local z11 BcroXlnbas_1-AcroXlnare_1 BcroXlnbas_2-AcroXlnare_2 BcroXlnbas_4-AcroXlnare_4 BcroXlnbas_5-AcroXlnare_5 BcroXlnbas_6-AcroXlnare_6
local z11bar
local i=1
foreach var of varlist `z11' {
	qui egen `var'bar=mean(`var'), by(rc)
	local z11bar `z11bar' `var'bar
local i=`i'+1
}

*IV with one s.d. rain shock, with househould CRE
local maize SHKcroXshock_1 BcroXlnbas_1 TcroXlntop_1 ScroXlnsee_1 AcroXlnare_1 
local sorghum SHKcroXshock_2 BcroXlnbas_2 TcroXlntop_2 ScroXlnsee_2 AcroXlnare_2
local millet SHKcroXshock_4 BcroXlnbas_4 TcroXlntop_4 ScroXlnsee_4 AcroXlnare_4 
local gnut SHKcroXshock_5 BcroXlnbas_5 TcroXlntop_5 ScroXlnsee_5 AcroXlnare_5 
local ppea SHKcroXshock_6 BcroXlnbas_6 TcroXlntop_6 ScroXlnsee_6 AcroXlnare_6 

xi: probit CA wardNGO `maize' `sorghum' `millet' `gnut' `ppea' crop_type1- crop_type5 i.year BcroXlnbas_1bar-AcroXlnare_6bar, vce(cluster crc)

predict p4, xb
gen IMR4 = normalden(p4) / normal(p4)     /* gets the inverse mills ratio*/

*Generate crop specific IV for equation 4
xi I.crops|IMR4, prefix(CbIV) noomit
drop CbIVcrops_1 CbIVcrops_2 CbIVcrops_4 CbIVcrops_5 CbIVcrops_6

gen bIV_shk = IMR4*shock
xi I.crops|bIV_shk, prefix(bsIV) noomit
drop bsIVcrops_1 bsIVcrops_2 bsIVcrops_4 bsIVcrops_5 bsIVcrops_6

*Endogenous CA, with one s.d. rain shock, with househould fixed effects
local maize SHKcroXshock_1 BcroXlnbas_1 TcroXlntop_1 ScroXlnsee_1 AcroXlnare_1 
local sorghum SHKcroXshock_2 BcroXlnbas_2 TcroXlntop_2 ScroXlnsee_2 AcroXlnare_2 
local millet SHKcroXshock_4 BcroXlnbas_4 TcroXlntop_4 ScroXlnsee_4 AcroXlnare_4 
local gnut SHKcroXshock_5 BcroXlnbas_5 TcroXlntop_5 ScroXlnsee_5 AcroXlnare_5 
local ppea SHKcroXshock_6 BcroXlnbas_6 TcroXlntop_6 ScroXlnsee_6 AcroXlnare_6 

xi: ivreg2 lnyield ( CAcroXCA_1 CAcroXCA_2 CAcroXCA_4 CAcroXCA_5 CAcroXCA_6 CAScroXca_sh_1 CAScroXca_sh_2 CAScroXca_sh_4 CAScroXca_sh_5 CAScroXca_sh_6 = ///
 CbIVcroXIMR4_1 CbIVcroXIMR4_2 CbIVcroXIMR4_4 CbIVcroXIMR4_5 CbIVcroXIMR4_6 bsIVcroXbIV_s_1 bsIVcroXbIV_s_2 bsIVcroXbIV_s_4 bsIVcroXbIV_s_5 bsIVcroXbIV_s_6 ) ///
 `maize' `sorghum' `millet' `gnut' `ppea' crop_type1- crop_type5 i.year i.rc, noconstant cluster(crc) partial(crop_type1- crop_type5 i.year i.rc)

est store  CAonesd

*Limited coefficients for Appendix Table A6
esttab CAonesd CAhsd using tableA6.tex, replace f ///
	label booktabs b(3) se(3) eqlabels(none) alignment(S)  ///
	drop(BcroXlnbas_* TcroXlntop_* ScroXlnsee_* AcroXlnare_*) ///
	star(* 0.10 ** 0.05 *** 0.01) nogaps ///
	order(CAcroXCA_1 SHKcroXshock_1 CAScroXca_sh_1 CAcroXCA_2 SHKcroXshock_2 CAScroXca_sh_2 CAcroXCA_4 SHKcroXshock_4 CAScroXca_sh_4  ///
	CAcroXCA_5 SHKcroXshock_5 CAScroXca_sh_5  CAcroXCA_6 SHKcroXshock_6 CAScroXca_sh_6  ) ///
	refcat(CAcroXCA_1 "\emph{Maize}" CAcroXCA_2 "\emph{Sorghum}" CAcroXCA_4 "\emph{Millet}" CAcroXCA_5 "\emph{Groundnut}" CAcroXCA_6 "\emph{Cowpea}", nolabel) ///
	stats(idstat N ll, fmt(3 0 0) layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}") ///
	labels(`"Kleibergen-Paap rk LM statistic"' `"Observations"' `"Log Likelihood"'))
	
	
*********************************************************
****Appendix Table A7 and A8: Alternative First Stage****
*********************************************************


*Tobit IV with rain shortage and surplus, with househould FE
local maize SRTcroXshort_1 SRPcroXsurpl_1 BcroXlnbas_1 TcroXlntop_1 ScroXlnsee_1 AcroXlnare_1 
local sorghum SRTcroXshort_2 SRPcroXsurpl_2 BcroXlnbas_2 TcroXlntop_2 ScroXlnsee_2 AcroXlnare_2 
local millet SRTcroXshort_4 SRPcroXsurpl_4 BcroXlnbas_4 TcroXlntop_4 ScroXlnsee_4 AcroXlnare_4 
local gnut SRTcroXshort_5 SRPcroXsurpl_5 BcroXlnbas_5 TcroXlntop_5 ScroXlnsee_5 AcroXlnare_5 
local ppea SRTcroXshort_6 SRPcroXsurpl_6 BcroXlnbas_6 TcroXlntop_6 ScroXlnsee_6 AcroXlnare_6 

xi: tobit CA wardNGO `maize' `sorghum' `millet' `gnut' `ppea' crop_type1- crop_type5 i.year i.rc, ll vce(cluster crc)

predict t_fe, xb
gen IMRt = normalden(t_fe) / normal(t_fe)     /* gets the inverse mills ratio*/

est store TFE_f

*Generate crop specific IV for LPM FE
xi I.crops|t_fe, prefix(TE) noomit
xi I.crops|IMRt, prefix(IMR) noomit

gen tfIV_rp = t_fe*surpl
xi I.crops|tfIV_rp, prefix(rpIV) noomit
gen tIMR_rp = IMRt*surpl
xi I.crops|tIMR_rp, prefix(rpIM) noomit

gen tfIV_rt = t_fe*short
xi I.crops|tfIV_rt, prefix(rtIV) noomit
gen tIMR_rt = IMRt*short
xi I.crops|tIMR_rt, prefix(rtIM) noomit

*Endogenous CA, with rain shortage and surplus, with househould fixed effects
local maize SRTcroXshort_1 SRPcroXsurpl_1 BcroXlnbas_1 TcroXlntop_1 ScroXlnsee_1 AcroXlnare_1 
local sorghum SRTcroXshort_2 SRPcroXsurpl_2 BcroXlnbas_2 TcroXlntop_2 ScroXlnsee_2 AcroXlnare_2 
local millet SRTcroXshort_4 SRPcroXsurpl_4 BcroXlnbas_4 TcroXlntop_4 ScroXlnsee_4 AcroXlnare_4 
local gnut SRTcroXshort_5 SRPcroXsurpl_5 BcroXlnbas_5 TcroXlntop_5 ScroXlnsee_5 AcroXlnare_5 
local ppea SRTcroXshort_6 SRPcroXsurpl_6 BcroXlnbas_6 TcroXlntop_6 ScroXlnsee_6 AcroXlnare_6 

xi: ivreg2 lnyield ( CAcroXCA_1 CAcroXCA_2 CAcroXCA_4 CAcroXCA_5 CAcroXCA_6 CARcroXca_sr_1 CARcroXca_sr_2 CARcroXca_sr_4 CARcroXca_sr_5 ///
 CARcroXca_sr_6 CAPcroXca_sr_1 CAPcroXca_sr_2 CAPcroXca_sr_4 CAPcroXca_sr_5 CAPcroXca_sr_6 = TEcroXt_fe_1 TEcroXt_fe_2 TEcroXt_fe_4 ///
 TEcroXt_fe_5 TEcroXt_fe_6 IMRcroXIMRt_1 IMRcroXIMRt_2 IMRcroXIMRt_4 IMRcroXIMRt_5 IMRcroXIMRt_6 rpIVcroXtfIV__1 rpIVcroXtfIV__2 ///
 rpIVcroXtfIV__4 rpIVcroXtfIV__5 rpIVcroXtfIV__6 rpIMcroXtIMR__1 rpIMcroXtIMR__2 rpIMcroXtIMR__4 rpIMcroXtIMR__5 rpIMcroXtIMR__6 ///
 rtIVcroXtfIV__1 rtIVcroXtfIV__2 rtIVcroXtfIV__4 rtIVcroXtfIV__5 rtIVcroXtfIV__6 rtIMcroXtIMR__1 rtIMcroXtIMR__2 rtIMcroXtIMR__4 ///
 rtIMcroXtIMR__5 rtIMcroXtIMR__6 ) `maize' `sorghum' `millet' `gnut' `ppea' crop_type1- crop_type5 i.year i.rc, ///
 noconstant cluster(crc) partial(crop_type1- crop_type5 i.year i.rc)

est store TFE

*Tobit IV with rain shortage and surplus, with househould CRE
local maize SRTcroXshort_1 SRPcroXsurpl_1 BcroXlnbas_1 TcroXlntop_1 ScroXlnsee_1 AcroXlnare_1 
local sorghum SRTcroXshort_2 SRPcroXsurpl_2 BcroXlnbas_2 TcroXlntop_2 ScroXlnsee_2 AcroXlnare_2 
local millet SRTcroXshort_4 SRPcroXsurpl_4 BcroXlnbas_4 TcroXlntop_4 ScroXlnsee_4 AcroXlnare_4 
local gnut SRTcroXshort_5 SRPcroXsurpl_5 BcroXlnbas_5 TcroXlntop_5 ScroXlnsee_5 AcroXlnare_5 
local ppea SRTcroXshort_6 SRPcroXsurpl_6 BcroXlnbas_6 TcroXlntop_6 ScroXlnsee_6 AcroXlnare_6 

xi: tobit CA wardNGO `maize' `sorghum' `millet' `gnut' `ppea' crop_type1- crop_type5 i.year BcroXlnbas_1bar-AcroXlnare_6bar, ll vce(cluster crc)

predict t_cre, xb
gen IMRc = normalden(t_cre) / normal(t_cre)     /* gets the inverse mills ratio*/

est store TCE_f

*Generate crop specific IV for LPM FE
xi I.crops|t_cre, prefix(TRE) noomit
xi I.crops|IMRc, prefix(IMR) noomit

gen trIV_rp = t_cre*surpl
xi I.crops|trIV_rp, prefix(rpIV) noomit
gen trIMR_rp = IMRc*surpl
xi I.crops|trIMR_rp, prefix(rpIM) noomit

gen trIV_rt = t_cre*short
xi I.crops|trIV_rt, prefix(rtIV) noomit
gen trIMR_rt = IMRc*short
xi I.crops|trIMR_rt, prefix(rtIM) noomit

*Endogenous CA, with rain shortage and surplus, with househould fixed effects
local maize SRTcroXshort_1 SRPcroXsurpl_1 BcroXlnbas_1 TcroXlntop_1 ScroXlnsee_1 AcroXlnare_1 
local sorghum SRTcroXshort_2 SRPcroXsurpl_2 BcroXlnbas_2 TcroXlntop_2 ScroXlnsee_2 AcroXlnare_2 
local millet SRTcroXshort_4 SRPcroXsurpl_4 BcroXlnbas_4 TcroXlntop_4 ScroXlnsee_4 AcroXlnare_4 
local gnut SRTcroXshort_5 SRPcroXsurpl_5 BcroXlnbas_5 TcroXlntop_5 ScroXlnsee_5 AcroXlnare_5 
local ppea SRTcroXshort_6 SRPcroXsurpl_6 BcroXlnbas_6 TcroXlntop_6 ScroXlnsee_6 AcroXlnare_6 

xi: ivreg2 lnyield (CAcroXCA_1 CAcroXCA_2 CAcroXCA_4 CAcroXCA_5 CAcroXCA_6 CARcroXca_sr_1 CARcroXca_sr_2 CARcroXca_sr_4 CARcroXca_sr_5 ///
 CARcroXca_sr_6 CAPcroXca_sr_1 CAPcroXca_sr_2 CAPcroXca_sr_4 CAPcroXca_sr_5 CAPcroXca_sr_6 = TREcroXt_cre_1 TREcroXt_cre_2 TREcroXt_cre_4 ///
 TREcroXt_cre_5 TREcroXt_cre_6 IMRcroXIMRc_1 IMRcroXIMRc_2 IMRcroXIMRc_4 IMRcroXIMRc_5 IMRcroXIMRc_6 rpIVcroXtrIV__1 rpIVcroXtrIV__2 ///
 rpIVcroXtrIV__4 rpIVcroXtrIV__5 rpIVcroXtrIV__6 rpIMcroXtrIMR_1 rpIMcroXtrIMR_2 rpIMcroXtrIMR_4 rpIMcroXtrIMR_5 rpIMcroXtrIMR_6 ///
 rtIVcroXtrIV__1 rtIVcroXtrIV__2 rtIVcroXtrIV__4 rtIVcroXtrIV__5 rtIVcroXtrIV__6 rtIMcroXtrIMR_1 rtIMcroXtrIMR_2 rtIMcroXtrIMR_4 ///
 rtIMcroXtrIMR_5 rtIMcroXtrIMR_6 ) `maize' `sorghum' `millet' `gnut' `ppea' crop_type1- crop_type5 i.year i.rc, ///
 noconstant cluster(crc) partial(crop_type1- crop_type5 i.year i.rc)

est store TRE

*First stage regression results for Appendix Table A7
esttab TFE_f TCE_f using tableA7.tex, replace f ///
	label booktabs b(3) se(3) eqlabels(none) alignment(S)  ///
	drop(SRTcroXshort_* SRPcroXsurpl_* BcroXlnbas_* TcroXlntop_* ScroXlnsee_* AcroXlnare_* crop_type* _Iyear_* _Irc_* _cons) ///
	star(* 0.10 ** 0.05 *** 0.01) nogaps ///
	stats(N r2_p ll, fmt(0 3 0) layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}") labels(`"Observations"' `"pseudo-\(R^{2}\)"' `"Log Likelihood"'))

*Second stage regression results for Appendix Table A8
esttab TFE TRE using tableA8.tex, replace f ///
	label booktabs b(3) se(3) eqlabels(none) alignment(S)  ///	
	drop(BcroXlnbas_* TcroXlntop_* ScroXlnsee_* AcroXlnare_*) ///
	star(* 0.10 ** 0.05 *** 0.01) nogaps ///
	order(CAcroXCA_1 SRTcroXshort_1 CARcroXca_sr_1 SRPcroXsurpl_1 CAPcroXca_sr_1 BcroXlnbas_1 TcroXlntop_1 ScroXlnsee_1 AcroXlnare_1 ///
	CAcroXCA_2 SRTcroXshort_2 CARcroXca_sr_2 SRPcroXsurpl_2 CAPcroXca_sr_2 BcroXlnbas_2 TcroXlntop_2 ScroXlnsee_2 AcroXlnare_2 ///
	CAcroXCA_4 SRTcroXshort_4 CARcroXca_sr_4 SRPcroXsurpl_4 CAPcroXca_sr_4 BcroXlnbas_4 TcroXlntop_4 ScroXlnsee_4 AcroXlnare_4 ///
	CAcroXCA_5 SRTcroXshort_5 CARcroXca_sr_5 SRPcroXsurpl_5 CAPcroXca_sr_5 BcroXlnbas_5 TcroXlntop_5 ScroXlnsee_5 AcroXlnare_5 ///
	CAcroXCA_6 SRTcroXshort_6 CARcroXca_sr_6 SRPcroXsurpl_6 CAPcroXca_sr_6 BcroXlnbas_6 TcroXlntop_6 ScroXlnsee_6 AcroXlnare_6 ) ///
	refcat(CAcroXCA_1 "\emph{Maize}" CAcroXCA_2 "\emph{Sorghum}" CAcroXCA_4 "\emph{Millet}" CAcroXCA_5 "\emph{Groundnut}" CAcroXCA_6 "\emph{Cowpea}", nolabel) ///
	stats(N idstat ll, fmt(0 3 0) layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{S}{@}" "\multicolumn{1}{c}{@}" ) ///
	labels(`"Observations"' `"Kleibergen-Paap LM"'  `"Log Likelihood"'))
