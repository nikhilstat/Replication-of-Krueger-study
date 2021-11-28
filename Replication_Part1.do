
*Bring data into Stata
use "STAR_Students.dta", clear


* recode free lunch to 1 and 0
* understand the values of gkfreelunch
tab gkfreelunch
tab gkfreelunch, nolabel
* change the values: students qualify for free lunch =1, non-free lunch =0
replace gkfreelunch=gkfreelunch-2 if gkfreelunch==2

* recode free lunch to 1 and 0
* understand the values of g1freelunch
tab g1freelunch
tab g1freelunch, nolabel
* change the values: students qualify for free lunch =1, non-free lunch =0
replace g1freelunch=g1freelunch-2 if g1freelunch==2

* recode free lunch to 1 and 0
* understand the values of g2freelunch
tab g2freelunch
tab g2freelunch, nolabel
* change the values: students qualify for free lunch =1, non-free lunch =0
replace g2freelunch=g2freelunch-2 if g2freelunch==2

* recode free lunch to 1 and 0
* understand the values of g3freelunch
tab g3freelunch
tab g3freelunch, nolabel
	* change the values: students qualify for free lunch =1, non-free lunch =0
replace g3freelunch=g3freelunch-2 if g3freelunch==2


* generate entering year variable
*students who enter in kindergarten
gen entersgk=0
replace entersgk=1 if flagsgk==1
		
* generate entering year variable
*students who enter in grade 1
gen entersg1=0 
replace entersg1=1 if flagsgk==0 & flagsg1 ==1

* generate entering year variable
*students who enter in grade 2
gen entersg2=0 
replace entersg2=1 if flagsgk==0 & flagsg1 ==0 & flagsg2 ==1

* generate entering year variable
*students who enter in grade 3
gen entersg3=0 
replace entersg3=1 if flagsgk==0 & flagsg1 ==0 & flagsg2 ==0 & flagsg3 ==1

*Code class type in grade kindergarten
tab gkclasstype if entersgk ==1,m
gen smallgk = (gkclasstype ==1) if entersgk ==1
gen regulargk = (gkclasstype ==2) if entersgk ==1
gen aidegk = (gkclasstype ==3) if entersgk ==1

*Code class type in grade 1
tab g1classtype if entersg1 ==1,m
gen smallg1 = (g1classtype ==1) if entersg1 ==1
gen regularg1 = (g1classtype ==2) if entersg1 ==1
gen aideg1 = (g1classtype ==3) if entersg1 ==1

*Code class type in grade 2
tab g2classtype if entersg2 ==1,m
gen smallg2 = (g2classtype ==1) if entersg2 ==1
gen regularg2 = (g2classtype ==2) if entersg2 ==1
gen aideg2 = (g2classtype ==3) if entersg2 ==1

*Code class type in grade 3
tab g3classtype if entersg3 ==1,m
gen smallg3 = (g3classtype ==1) if entersg3 ==1
gen regularg3 = (g3classtype ==2) if entersg3 ==1
gen aideg3 = (g3classtype ==3) if entersg3 ==1




* generate race indicator for White/Asian
	* race is encoded with lables
tab race
tab race, nolabel
* white and asian numbers corresponding to race are 1 and 3

gen White_Asian=0
replace White_Asian=1 if race==1
replace White_Asian=1 if race==3
	
* generate dob and age as of 9/1/1985
gen dob=mdy(birthmonth,birthday,birthyear)
format dob %td
gen age_1985=(mdy(9,1,1985)-dob)/365
replace age_1985=1985-birthyear if mi(age_1985)

* generate percentile score by averaging the 3 SAT scores (or 2, or 1 if any are missing)
	* kindergarten
	foreach sub in ktread ktmath kwordskill {
		* cumulative distribution for regular and regular/aide class
		cumul g`sub'ss if inrange(gkclasstype,2,3), gen(g`sub'xt)
		sort g`sub'ss
		* assign percentile for small calss
		qui replace g`sub'xt=g`sub'xt[_n-1] if g`sub'ss==g`sub'ss[_n-1] & gkclasstype==1
		/* ipolate yvar xvar: creates in newvar a linear interpolation of 	
		yvar on xvar for missing values of yvar.*/
		qui ipolate g`sub'xt g`sub'ss, gen(ipo)
		qui replace g`sub'xt=ipo if gkclasstype==1 & mi(g`sub'xt)
		drop ipo
		}
	* calculate the average of three test
	egen gkSATxt = rmean(gktreadxt gktmathxt gkwordskillxt)
	qui replace gkSATxt=100*gkSATxt
	
	

* generate percentile score by averaging the 3 SAT scores (or 2, or 1 if any are missing)
	* Grade 1
	foreach sub in 1tread 1tmath 1wordskill {
		* cumulative distribution for regular and regular/aide class
		cumul g`sub'ss if inrange(g1classtype,2,3), gen(g`sub'xt)
		sort g`sub'ss
		* assign percentile for small calss
		qui replace g`sub'xt=g`sub'xt[_n-1] if g`sub'ss==g`sub'ss[_n-1] & g1classtype==1
		/* ipolate yvar xvar: creates in newvar a linear interpolation of 	
		yvar on xvar for missing values of yvar.*/
		qui ipolate g`sub'xt g`sub'ss, gen(ipo)
		qui replace g`sub'xt=ipo if g1classtype==1 & mi(g`sub'xt)
		drop ipo
		}
	* calculate the average of three test
	egen g1SATxt = rmean(g1treadxt g1tmathxt g1wordskillxt)
	qui replace g1SATxt=100*g1SATxt
	
* generate percentile score by averaging the 3 SAT scores (or 2, or 1 if any are missing)
	* Grade 2
	foreach sub in 2tread 2tmath 2wordskill {
		* cumulative distribution for regular and regular/aide class
		cumul g`sub'ss if inrange(g2classtype,2,3), gen(g`sub'xt)
		sort g`sub'ss
		* assign percentile for small calss
		qui replace g`sub'xt=g`sub'xt[_n-1] if g`sub'ss==g`sub'ss[_n-1] & g2classtype==1
		/* ipolate yvar xvar: creates in newvar a linear interpolation of 	
		yvar on xvar for missing values of yvar.*/
		qui ipolate g`sub'xt g`sub'ss, gen(ipo)
		qui replace g`sub'xt=ipo if g2classtype==1 & mi(g`sub'xt)
		drop ipo
		}
	* calculate the average of three test
	egen g2SATxt = rmean(g2treadxt g2tmathxt g2wordskillxt)
	qui replace g2SATxt=100*g2SATxt
		
* generate percentile score by averaging the 3 SAT scores (or 2, or 1 if any are missing)
	* Grade 3
	foreach sub in 3tread 3tmath 3wordskill {
		* cumulative distribution for regular and regular/aide class
		cumul g`sub'ss if inrange(g3classtype,2,3), gen(g`sub'xt)
		sort g`sub'ss
		* assign percentile for small calss
		qui replace g`sub'xt=g`sub'xt[_n-1] if g`sub'ss==g`sub'ss[_n-1] & g3classtype==1
		/* ipolate yvar xvar: creates in newvar a linear interpolation of 	
		yvar on xvar for missing values of yvar.*/
		qui ipolate g`sub'xt g`sub'ss, gen(ipo)
		qui replace g`sub'xt=ipo if g3classtype==1 & mi(g`sub'xt)
		drop ipo
		}
	* calculate the average of three test
	egen g3SATxt = rmean(g3treadxt g3tmathxt g3wordskillxt)
	qui replace g3SATxt=100*g3SATxt
	

* generate attrition variable for each entering year (3rd grade N.A.)
* replace exit variable with 1 if student entering STAR in Kindergarten is not in STAR in grades 1-3
gen exit_STARk=0 if entersgk==1
replace exit_STARk=1 if flagsg1==0 & entersgk==1
replace exit_STARk=1 if flagsg2==0 & entersgk==1
replace exit_STARk=1 if flagsg3==0 & entersgk==1
		
* replace exit variable with 1 if student entering STAR in grade 1 is not in STAR in grades 2-3	
gen exit_STAR1=0 if entersg1==1
replace exit_STAR1=1 if flagsg2==0 & entersg1==1
replace exit_STAR1=1 if flagsg3==0 & entersg1==1		

* replace exit variable with 2 if student entering STAR in grade 2 is not in STAR in grades 3	
gen exit_STAR2=0 if entersg2==1
replace exit_STAR2=1 if flagsg3==0 & entersg2==1

* Find Means for subpopulations

* For Students entering STAR in kindergarten
*mean gkfreelunch White_Asian age_1985 exit_STARk gkclasssize percentile_scoreavgk if entersgk==1, over(gkclasstype)
tabstat gkfreelunch White_Asian age_1985 exit_STARk gkclasssize gkSATxt if entersgk==1, by(gkclasstype) save
return list

* combine vector or matrix: using "\" to combine by row, use "," to combine by column
matrix kindergarten=(r(Stat1) \ r(Stat2) \ r(Stat3))
matrix list kindergarten
matrix rownames kindergarten= "Small Class" "Regular Class" "Regular + Aid Class"
matrix list kindergarten

* use eststo and esttab
eststo smallk: estpost sum gkfreelunch White_Asian age_1985 exit_STARk gkclasssize gkSATxt if entersgk==1 & gkclasstype== 1
eststo regulark: estpost sum gkfreelunch White_Asian age_1985 exit_STARk gkclasssize gkSATxt if entersgk==1 & gkclasstype== 2
eststo aidek: estpost sum gkfreelunch White_Asian age_1985 exit_STARk gkclasssize gkSATxt if entersgk==1 & gkclasstype== 3

esttab smallk regulark aidek, cell(mean(fmt(2))) title (Comparison of Mean Characteristics of Treatments and Controls: Unadjusted Data) mtitle("Small" "Regular" "Regular + Aide") noobs

	
* For Students entering STAR in grade 1
*mean g1freelunch White_Asian age_1985 exit_STAR1 g1classsize percentile_scoreavg1 if entersg1==1, over(g1classtype)
tabstat g1freelunch White_Asian age_1985 exit_STAR1 g1classsize g1SATxt if entersg1==1, by(g1classtype) save
return list

* combine vector or matrix: using "\" to combine by row, use "," to combine by column
matrix grade1=(r(Stat1) \ r(Stat2) \ r(Stat3))
matrix list grade1
matrix rownames grade1= "Small Class" "Regular Class" "Regular + Aid Class"
matrix list grade1

* use eststo and esttab
eststo small1: estpost sum g1freelunch White_Asian age_1985 exit_STAR1 g1classsize g1SATxt if entersg1==1 & g1classtype== 1
eststo regular1: estpost sum g1freelunch White_Asian age_1985 exit_STAR1 g1classsize g1SATxt if entersg1==1 & g1classtype== 2
eststo aide1: estpost sum g1freelunch White_Asian age_1985 exit_STAR1 g1classsize g1SATxt if entersg1==1 & g1classtype== 3

esttab small1 regular1 aide1, cell(mean(fmt(2))) title (Comparison of Mean Characteristics of Treatments and Controls: Unadjusted Data) mtitle("Small" "Regular" "Regular + Aide") noobs

* For Students entering STAR in grade 2
*mean g2freelunch White_Asian age_1985 exit_STAR2 g2classsize percentile_scoreavg2 if entersg2==1, over(g2classtype)
tabstat g2freelunch White_Asian age_1985 exit_STAR2 g2classsize g2SATxt if entersg2==1, by(g2classtype) save
return list

* combine vector or matrix: using "\" to combine by row, use "," to combine by column
matrix grade2=(r(Stat1) \ r(Stat2) \ r(Stat3))
matrix list grade2
matrix rownames grade2= "Small Class" "Regular Class" "Regular + Aid Class"
matrix list grade2

* use eststo and esttab
eststo small2: estpost sum g2freelunch White_Asian age_1985 exit_STAR2 g2classsize g2SATxt if entersg2==1 & g2classtype== 1
eststo regular2: estpost sum g2freelunch White_Asian age_1985 exit_STAR2 g2classsize g2SATxt if entersg2==1 & g2classtype== 2
eststo aide2: estpost sum g2freelunch White_Asian age_1985 exit_STAR2 g2classsize g2SATxt if entersg2==1 & g2classtype== 3

esttab small2 regular2 aide2, cell(mean(fmt(2))) title (Comparison of Mean Characteristics of Treatments and Controls: Unadjusted Data) mtitle("Small" "Regular" "Regular + Aide") noobs

* For Students entering STAR in grade 3
*mean g3freelunch White_Asian age_1985 exit_STAR3 g3classsize percentile_scoreavg3 if entersg3==1, over(g3classtype)
tabstat g3freelunch White_Asian age_1985 g3classsize g3SATxt if entersg3==1, by(g3classtype) save
return list

* combine vector or matrix: using "\" to combine by row, use "," to combine by column
matrix grade3=(r(Stat1) \ r(Stat2) \ r(Stat3))
matrix list grade3
matrix rownames grade3= "Small Class" "Regular Class" "Regular + Aid Class"
matrix list grade3

* use eststo and esttab
eststo small3: estpost sum g3freelunch White_Asian age_1985  g3classsize g3SATxt if entersg3==1 & g3classtype== 1
eststo regular3: estpost sum g3freelunch White_Asian age_1985  g3classsize g3SATxt if entersg3==1 & g3classtype== 2
eststo aide3: estpost sum g3freelunch White_Asian age_1985  g3classsize g3SATxt if entersg3==1 & g3classtype== 3

esttab small3 regular3 aide3, cell(mean(fmt(2))) title (Comparison of Mean Characteristics of Treatments and Controls: Unadjusted Data) mtitle("Small" "Regular" "Regular + Aide") noobs
	
*Joint pvalues
*kindergarten
qui reg gkfreelunch smallgk aidegk if entersgk ==1
test smallgk aidegk 
qui reg White_Asian smallgk aidegk if entersgk ==1
test smallgk aidegk 
qui reg age_1985 smallgk aidegk if entersgk ==1
test smallgk aidegk 
qui reg exit_STARk smallgk aidegk if entersgk ==1
test smallgk aidegk 
qui reg gkclasssize smallgk aidegk if entersgk ==1
test smallgk aidegk 
qui reg gkSATxt smallgk aidegk if entersgk ==1
test smallgk aidegk 

*Grade 1
qui reg g1freelunch smallg1 aideg1 if entersg1 ==1
test smallg1 aideg1
qui reg White_Asian smallg1 aideg1 if entersg1 ==1
test smallg1 aideg1
qui reg age_1985 smallg1 aideg1 if entersg1 ==1
test smallg1 aideg1
qui reg exit_STAR1 smallg1 aideg1 if entersg1 ==1
test smallg1 aideg1
qui reg g1classsize smallg1 aideg1 if entersg1 ==1
test smallg1 aideg1
qui reg g1SATxt smallg1 aideg1 if entersg1 ==1
test smallg1 aideg1

*Grade 2
qui reg g2freelunch smallg2 aideg2 if entersg2 ==1
test smallg2 aideg2
qui reg White_Asian smallg2 aideg2 if entersg2 ==1
test smallg2 aideg2
qui reg age_1985 smallg2 aideg2 if entersg2 ==1
test smallg2 aideg2
qui reg exit_STAR2 smallg2 aideg2 if entersg2 ==1
test smallg2 aideg2
qui reg g2classsize smallg2 aideg2 if entersg2 ==1
test smallg2 aideg2
qui reg g2SATxt smallg2 aideg2 if entersg2 ==1
test smallg2 aideg2

*Grade 3
qui reg g3freelunch smallg3 aideg3 if entersg3 ==1
test smallg3 aideg3
qui reg White_Asian smallg3 aideg3 if entersg3 ==1
test smallg3 aideg3
qui reg age_1985 smallg3 aideg3 if entersg3 ==1
test smallg3 aideg3
qui reg g3classsize smallg3 aideg3 if entersg3 ==1
test smallg3 aideg3
qui reg g3SATxt smallg3 aideg3 if entersg3 ==1
test smallg3 aideg3

/*
test smallgk aidegk 
return list
*Save p value using the following:
local p_gkfreelunch = r(p)

*Use anova
anova gkfreelunch smallgk aidegk if entersgk ==1
ereturn list

*Ftail: the reverse cumulative (upper tail or survivor) F distribution
local p_anova_gkfreelunch = Ftail(e(df_m),e(df_r),e(F))
di "p-value (regression) = "`p_gkfreelunch'
di "p-value(anova) = "`p_anova_gkfreellunch'*/

*Table 2
*Kindergarten
qui areg gkfreelunch smallgk aidegk if entersgk ==1, abs(gkschid)
test smallgk aidegk
qui areg White_Asian smallgk aidegk if entersgk ==1, abs(gkschid)
test smallgk aidegk
qui areg age_1985 smallgk aidegk if entersgk ==1, abs(gkschid)
test smallgk aidegk
qui areg exit_STARk smallgk aidegk if entersgk ==1, abs(gkschid)
test smallgk aidegk
qui areg gkclasssize smallgk aidegk if entersgk ==1, abs(gkschid)
test smallgk aidegk
qui areg gkSATxt smallgk aidegk if entersgk ==1, abs(gkschid)
test smallgk aidegk


*GRADE 1
qui areg g1freelunch smallg1 aideg1 if entersg1 ==1, abs(g1schid)
test smallg1 aideg1
qui areg White_Asian smallg1 aideg1 if entersg1 ==1, abs(g1schid)
test smallg1 aideg1
qui areg age_1985 smallg1 aideg1 if entersg1 ==1, abs(g1schid)
test smallg1 aideg1
qui areg exit_STAR1 smallg1 aideg1 if entersg1 ==1, abs(g1schid)
test smallg1 aideg1
qui areg g1classsize smallg1 aideg1 if entersg1 ==1, abs(g1schid)
test smallg1 aideg1
qui areg g1SATxt smallg1 aideg1 if entersg1 ==1, abs(g1schid)
test smallg1 aideg1

*GRADE2
qui areg g2freelunch smallg2 aideg2 if entersg2 ==1, abs(g2schid)
test smallg2 aideg2
qui areg White_Asian smallg2 aideg2 if entersg2 ==1, abs(g2schid)
test smallg2 aideg2
qui areg age_1985 smallg2 aideg2 if entersg2 ==1, abs(g2schid)
test smallg2 aideg2
qui areg exit_STAR2 smallg2 aideg2 if entersg2 ==1, abs(g2schid)
test smallg2 aideg2
qui areg g2classsize smallg2 aideg2 if entersg2 ==1, abs(g2schid)
test smallg2 aideg2
qui areg g2SATxt smallg2 aideg2 if entersg2 ==1, abs(g2schid)
test smallg2 aideg2

*GRADE 3
qui areg g3freelunch smallg3 aideg3 if entersg3 ==1, abs(g3schid)
test smallg3 aideg3
qui areg White_Asian smallg3 aideg3 if entersg3 ==1, abs(g3schid)
test smallg3 aideg3
qui areg age_1985 smallg3 aideg3 if entersg3 ==1, abs(g3schid)
test smallg3 aideg3
qui areg g3classsize smallg3 aideg3 if entersg3 ==1, abs(g3schid)
test smallg3 aideg3
qui areg g3SATxt smallg3 aideg3 if entersg3 ==1, abs(g3schid)
test smallg3 aideg3

*Table 3
asdoc tabulate g1classsize g1classtype, replace











