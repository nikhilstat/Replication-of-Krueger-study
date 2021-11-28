
* Bring data into Stata
use "STAR_Students.dta", clear

* erase Table5.tex  


gen WhiteAsian=0
gen dob = 0
gen age_1985 = 0
scalar k=0
gen Girl=0
gen Small=0
gen RegAide=0
gen FreeLunch = 0
gen MastersDegree = 0
gen enteringclass=0
gen exitclass=4
gen Small_init=0
gen RegAide_init=0
gen Reg_init =0

foreach grade in 3 2 1 k{
	replace enteringclass=`grade' if flagsg`grade'==1
}

foreach grade in 3 1 2 k{
	replace exitclass=`grade' if flagsg`grade'==0 &  `grade'> enteringclass
}




foreach grade in k 1 2 3{
	* change the values: students qualify for free lunch =1, non-free lunch =0
		replace g`grade'freelunch=g`grade'freelunch-2 if g`grade'freelunch==2
	
	* generate race indicator for White/Asian
		replace WhiteAsian=1 if race==1
		replace WhiteAsian=1 if race==3

	* generate gender indicator for girls
		replace Girl= gender==2

	* generate race indicator for White Teachers
		gen WhiteTeacherg`grade'= g`grade'trace == 1

	* generate teacher degree indicator for masters holders
		gen Mastersg`grade' = g`grade'thighdegree >=3

	* Teachers' years of expereince 
		gen TeacherExperienceg`grade'=0
			replace TeacherExperienceg`grade'=g`grade'tyears if g`grade'tyears>0

	* generate dob and age as of 9/1/1985
		replace dob=mdy(birthmonth,birthday,birthyear)
		format dob %td
		replace age_1985=(mdy(9,1,1985)-dob)/365
		replace age_1985=1985-birthyear if mi(age_1985)

	* generate percentile score by averaging the 3 SAT scores (or 2, or 1 if any are missing)
		* kindergarten
		foreach sub in `grade'tread `grade'tmath `grade'wordskill {
			* cumulative distribution for regular and regular/aide class
			cumul g`sub'ss if inrange(g`grade'classtype,2,3), gen(g`sub'xt)
			sort g`sub'ss
			* assign percentile for small calss
			qui replace g`sub'xt=g`sub'xt[_n-1] if g`sub'ss==g`sub'ss[_n-1] & g`grade'classtype==1
			/* ipolate yvar xvar: creates in newvar a linear interpolation of 	
			yvar on xvar for missing values of yvar.*/
			qui ipolate g`sub'xt g`sub'ss, gen(ipo)
			qui replace g`sub'xt=ipo if g`grade'classtype==1 & mi(g`sub'xt)
			drop ipo
			}
		* calculate the average of three test
		egen g`grade'SATxt = rmean(g`grade'treadxt g`grade'tmathxt g`grade'wordskillxt)
		qui replace g`grade'SATxt=100*g`grade'SATxt

			egen g`grade'SATss = rmean(g`grade'treadss g`grade'tmathss g`grade'wordskillss)
		qui replace g`grade'SATss=100*g`grade'SATss


		
	/* flag  students in small and w/ aide classes */
		gen Smallg`grade' = g`grade'classtype==1 if !mi(g`grade'classtype)
		gen RegAideg`grade' = g`grade'classtype==3 if !mi(g`grade'classtype)


	/* flag  students initially in small and w/ aide classes */
		gen Small_initg`grade'=0
		gen RegAide_initg`grade'=0
		foreach initgrade in k 1 2 3{
			replace Small_initg`grade' = 1 if g`initgrade'classtype==1   & enteringclass==`initgrade'
			replace RegAide_initg`grade' = 1 if g`initgrade'classtype==3 & enteringclass==`initgrade'		
		}

		
		replace Reg_init=1 if enteringclass==`grade' & inrange(g`grade'classtype,2,3) 
		replace Small_init=1 if enteringclass==`grade' & g`grade'classtype==1 
		
}



foreach grade in 3 2 1 k{

/* create percentile scores*/

* generate percentile score by averaging the 3 SAT scores (or 2, or 1 if any are missing)
    * kindergarten
    foreach sub in `grade'tread `grade'tmath `grade'wordskill {
        drop g`sub'xt
		cumul g`sub'ss if inrange(g`grade'classtype,2,3), gen(g`sub'xt)
        sort g`sub'ss
        qui replace g`sub'xt=g`sub'xt[_n-1] if g`sub'ss==g`sub'ss[_n-1] & g`grade'classtype==1
        qui ipolate g`sub'xt g`sub'ss, gen(ipo)
        qui replace g`sub'xt=ipo if g`grade'classtype==1 & mi(g`sub'xt)
        drop ipo
        }
	drop g`grade'SATxt
    egen g`grade'SATxt = rmean(g`grade'treadxt g`grade'tmathxt g`grade'wordskillxt)
    qui replace g`grade'SATxt=100*g`grade'SATxt
	
/* use -kdensity- to plot densities of SAT scores for small-class students (solid line) versus regular-class students. 
- lc() for line color 
- lp() for line pattern
- addplot() add another plot to the graph
- scheme: https://www.stata.com/manuals13/g-4schemesintro.pdf
*/

kdensity g`grade'SATxt if Smallg`grade', lc(black) lp(solid) addplot(kdensity g`grade'SATxt if Smallg`grade'==0, lc(black) lp(dash)) scheme(s1mono) xtitle("Stanford Achievement Test Percentile") ytitle("Density") legend(off) title("") note("")

/* save as .png */
graph export g`grade'density.png, replace

}




* TABLE V

foreach grade in k 1 2 3 {
		
replace Small = Smallg`grade'
replace RegAide = RegAideg`grade'
replace FreeLunch = g`grade'freelunch


eststo t5c1g`grade': reghdfe g`grade'SATxt Small RegAide , noabsorb vce(cluster g`grade'schid)

eststo t5c2g`grade': reghdfe g`grade'SATxt Small RegAide, a(g`grade'schid) vce(cluster g`grade'schid)

eststo t5c3g`grade': reghdfe g`grade'SATxt Small RegAide WhiteAsian Girl FreeLunch, a(g`grade'schid) vce(cluster g`grade'schid)

eststo t5c4g`grade': reghdfe g`grade'SATxt Small RegAide WhiteAsian Girl FreeLunch WhiteTeacherg`grade' TeacherExperienceg`grade' Mastersg`grade', a(g`grade'schid) vce(cluster g`grade'schid)
		
replace Small = Small_initg`grade'
replace RegAide = RegAide_initg`grade'
replace FreeLunch = g`grade'freelunch

eststo t5c5g`grade': reghdfe g`grade'SATxt Small RegAide, noabsorb vce(cluster g`grade'schid)

eststo t5c6g`grade': reghdfe g`grade'SATxt Small RegAide, a(g`grade'schid) vce(cluster g`grade'schid)

eststo t5c7g`grade': reghdfe g`grade'SATxt Small RegAide WhiteAsian Girl FreeLunch, a(g`grade'schid) vce(cluster g`grade'schid)

eststo t5c8g`grade': reghdfe g`grade'SATxt Small RegAide WhiteAsian Girl FreeLunch WhiteTeacherg`grade' TeacherExperienceg`grade' Mastersg`grade', a(g`grade'schid) vce(cluster g`grade'schid)
* report results in latex
esttab  t5c1g`grade' t5c2g`grade' t5c3g`grade'  t5c4g`grade' t5c5g`grade' t5c6g`grade' t5c7g`grade' t5c8g`grade' using Table5, append keep(Small RegAide WhiteAsian Girl FreeLunch WhiteTeacherg`grade' TeacherExperienceg`grade' Mastersg`grade' ) scalars(r2) label cells(b(fmt(3)) se(par star fmt(3)))  fragment booktabs starlevels(* 0.1 ** 0.05 *** 0.01) varwidth(15) mlabels(, none) collabels(none, none) noobs nonumbers plain substitute(_ \_ ) style(tex) 
		
}



* Table 7

foreach grade in k 1 2 3{
	
	* OLS
	reg g`grade'SATxt g`grade'classsize WhiteAsian Girl FreeLunch WhiteTeacherg`grade' TeacherExperienceg`grade' Mastersg`grade' i.g`grade'schid, vce(cluster g`grade'tchid)
	matrix KOLSg`grade' = _b[g`grade'classsize]
	matrix KseOLSg`grade' = _se[g`grade'classsize]
	matrix Ksamg`grade' = e(N)
		
	*2SLS
	ivregress 2sls g`grade'SATxt WhiteAsian Girl FreeLunch WhiteTeacherg`grade' TeacherExperienceg`grade' Mastersg`grade' i.g`grade'schid (g`grade'classsize = Small_init Reg_init) , vce(cluster g`grade'tchid)

	matrix K2SLSg`grade' = _b[g`grade'classsize]
	matrix Kse2SLSg`grade' = _se[g`grade'classsize]


}

	matrix K = KOLSgk, K2SLSgk, Ksamgk \ KseOLSgk, Kse2SLSgk, . \ KOLSg1, K2SLSg1, Ksamg1 \ KseOLSg1, Kse2SLSg1, . \ KOLSg2, K2SLSg2, Ksamg2 \ KseOLSg2, Kse2SLSg2, . \ KOLSg3, K2SLSg3, Ksamg3 \ KseOLSg3, Kse2SLSg3, .
	matrix list K
	
	matrix rownames K = "K" "se(K)" "1" "se(1)" "2" "se(2)" "3" "se(3)"
	matrix colnames K = "OLS" "2SLS" "Sample Size"
	matrix list K

	

