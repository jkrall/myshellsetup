*! version 1.0.0 March 24, 2006 @ 14:42:07
*! doesnt do anything but work for syntax testing
program def syntax_tester, eclass
	"does it understand strings?"
	`"does it understand "nested" strings?"'
	here is a `macro' that could be used
	/* what about comments? */
pro def fummel
version 1.0
version 2.0
version 2b0
version 2.1
version 2b1 
version 3.0
version 3.1
version 3.2
version 4
version 4.0
version 5
version 5.0
version 6
version 6.0
version 7
version 8
version 8.1
version 8.2
version 8.3
version 9
version 9.1
version 9.2
version 9.5
version 10
   
	/* this program does nothing - it merely has some things for testing syntax coloring */
	/* working with the syntax table */
	local dingle = this - that 
	
	/* functions in the order of the manuals (for checking for obsolete functions... */
	/* first... the general reference manuals */
	about
	adjust
	ameans
	alpha
	an ano anov anova
	/* estat syntax testing under regression postestimation below regression */
	areg
	asmprobit
	binreg
	biprobit
	bitest bitesti
	bootstrap
	boxcox
	brier
	bsample
	bstat
	centile
	ci cii
	clog clogi clogit
	cloglog
	cnr cnre cnreg
	cnsreg
	/* constraint commands */
	cons de 
	const defin 
	constr d
	constr dir
	constra drop
	cons l
	constrai list
	constrain get
	constraint free
	/* bad constraints -- should not highlight, except as mata reserves */
	const ge
	const fre
	/* end constraint */
	copyright
	cor corr corre correl correla correlat correlate pwcorr
	cumul
	cusum
	db
	/* diagnostic plots */
	symplot
	quantile
	qqplot
	qnorm
	pnorm
	pchi
	qchi
	/* end... */
	di dis disp displ displa display
	do ru run
	doed doedi doedit
	dotplot
	dstdize 	istdize
	dydx
	integ
	eivreg
	/* estat commands (new to Stata 9) */
	estat clas
	estat gof
	estat ic
	estat su
	estat summa
	estat summarize
	estat vce
	/* estat from multivariate manual under multivariate */
	/* end estat */
	/* estimates */
	est sto
	esti store
	estimates r
	estimates replay
	estimates t
	estimates table
	estimates f
	estimates for
	estimates st
	estimates stats
	estimates q
	estimates query
	estimates d
	estimates dir
	estimat res
	estimate restore
	estimates drop
	estimates clear
	estim ch
	estima change
	/* end estimates */
	e
	ex
	exi
	exit
	fracpoly
	fracgen
	fracplot
	fracpred
	frontier
	gllamm
	glm
	blogit
	bprobit
	glogit
	gprobit
	grmeanby
	hausman
	heckman
	heckprob
	h he hel help
	ch che chel chelp
	whelp
	hetprob
	hist histo histog histogr histogra histogram
	intreg
	ivprobit
	ivreg
	ivtobit
	jackknife
	kappa
	kdensity
	ksmirnov
	kwallis
	ladder
	gladder
	qladder
	set l
	set level
	lincom
	linktest
	lnskew0
	bcskew0
	lo
	log
	cmdlog
	set logtype t
	set logtype s
	set linesize
	logistic
	estat clas
	estat gof
	lroc lsens
	logi logit
	loneway
	lowess
	lrtest
	lv
	set mat
	set matsize
	set maxiter
	mean
	/* mfp commands */
	mfp clogit
	mfp glm
	mfp logistic
	mfp logit
	mfp poisson
	mfp probit
	mfp qreg
	mfp regress
	mfp stcox
	mfp streg
	mfp xtgee
	/* obsolete */
	mfp ologit
	mfp oprobit
	/* end mfp */
	fracplot
	fracpred
	mfx
	mfx c
	mfx compute
	mfx r
	mfx replay
	mkspline
	/* ml commands */
	ml mod
	ml model
	ml clear
	ml q
	ml query
	ml check
	ml sea
	ml search
	ml pl
	ml plot
	ml init
	ml rep
	ml report
	ml trace
	ml count
	ml max
	ml maximize
	ml gr
	ml graph
	ml di
	ml display
	ml foot
	ml footnote
	/* end ml */
	mlog mlogi mlogit
	set mo on
	set more off
	set pag
	set pagesize
	mprobit
	mvreg
	nbreg
	gnbreg
	/* net commands */
	net from
	net cd
	net link
	net search
	net
	net d
	net describe
	net set ado
	net set other
	net q
	net query
	net ins
	net install
	net get
	net sj
	net stb
	ado
	ado dir
	ado d
	ado describe
	ado uninstall
	/* end net */
	/* netio, which is also in set */
	set httpproxy on
	set httpproxyhost
	set httpproxyport
	set httpproxyauth on
	set httpproxypw
	set timeout1
	set timeout2
	news
	nl
	nlinit
	nlcom
	nlogit
	nptrend
	olog ologi ologit 
	on one onew onewa oneway
	oprob oprobi oprobit
	orthog
	orthpoly
	pcorr
	permute
	pkexamine pksumm pkshape pkcross pkequiv pkcollapse
	poisson
	poisgof
	predict
	predictnl
	prob probi probit
	dprobit
	proportion
	prtest prtesti
	/* qc commands */
	cchart
	pchart
	rchart
	xchart
	shewhart
	/* end qc */
	qreg iqreg sqreg bsqreg _qreg
	q mem
	qu memory
	que out
	quer output
	query inter
	query interface
	query graph
	query graphics
	query eff
	query efficiency
	quer net
	que network
	query up
	query update
	qu trace
	q mata
	q oth
	q other
	query `foo'
	ranksum
	median
	ratio
	reg3
	reg regr regre regres regress
	/* regression diagnostics */
	
	/* this is a comment */
	this is how Stata corp likes to indent 
	#r #re #rev #revi #revie #review
	_qreg _rmcoll _huber
	roctab roccomp rocgold
	rocfit
	rocplot 
	rologit
	rreg
	runtest
	sampsi

	/* these should be in the programming section... */
	ret li
	retu list
	return `foo'
	eret li
	eretur li
	sret li
	sreturn list
	scobit
	sdtest sdtesti robvar
	search
	findit
	serrbar
	/* set commands */
	set a
	set adosize
	set checksum on
	set checksum off
	set conren
	set conren clear
	set conren sf
	set conren bf
	set copycolor auto
	set copycolor automatic
	set copycolor asis
	set copycolor gs1
	set copycolor gs2
	set copycolor gs3
	set dockable on
	set dockable off
	set dockingg on
	set dockingguides off
	set `foo'
	set dp comma
	set dp period
	set eolch mac
	set eolchar unix
	set fastscroll on
	set fastscroll off
	set g on
	set graphics off
	set httpproxy on
	set httpproxy off
	set httpproxya on
	set httpproxyauth off
	set httpproxyhost
	set httpproxyport
	set httpproxypw
	set httpproxyuser
	set icmap on
	set icmap off
	set level
	set lineg
	set linegap
	set linesize
	set locksplit on
	set locksplitters off
	set logt t
	set logty te
	set logtyp tex
	set logtype text
	set logtype smcl
	set macgph quartz
	set macgphengine quickdraw
	set mat
	set matsize
	set maxdb
	set maxiter
	set maxvar
	set mem
	set memory
	set mo on
	set more off
	set ob
	set obs
	set ou proc
	set output p
	set outpu i
	set output inform
	set outp e
	set output error
	set pa
	set pagesize
	set piccom on
	set piccomments off
	set persistfv on
	set persistvtopic off
	set printcolor auto
	set printcolor automatic
	set printcolor asis
	set printcolor grayscale /* obsolete in Stata 9 */
	set reventr
	set reventries
	set revwin nofloat
	set revwindow float
	set r on
	set rmsg off
	set scheme
	set scrollbufsize
	set searchd local
	set searchdefault net
	set searchdefault all
	set se
	set seed
	set smalldlg on
	set smoothfonts off
	set smoothsize 12
	set timeout1
	set timeout2
set tr on
set trace off
	set traced
	set tracedepth
	set tracee on
	set traceexpand off
	set traceh
	set tracehilite
	set tracei off
	set traceindent on
	set tracen on
	set tracenumber off
	set traces off
	set tracesep on
	set ty
	set typ
	set type
	set update_interval
	set update_prompt on
	set update_prompt off
	set update_query on
	set update_query off
	set varabbrev on
	set varlabelpos
	set varwin float
	set varwindow nofloat
	set virt on
	set virtual off
	set xptheme on
	set_defaults
	/* end set commands */
	signrank
	signtest
	simulate
	sktest
	slogit
	smooth
	spearman
	ktau
	spikeplot
	ssc what
	ssc whatsnew
	ssc d
	ssc `foo'
	ssc describe
	ssc inst
	ssc install
	ssc uninstall
	ssc type
	ssc copy
	stem
	/* stepwise or sw now has a syntax bad for highlighting */
	stepwise
	sw 
	/* sw commands no longer exist, due to syntax changes */
	sw clogit
	sw cloglog
	sw `foo'
	sw clogit
	sw cloglog
	sw cnreg
	sw glm
	sw logistic
	sw logit
	sw nbreg
	sw ologit
	sw oprobit
	sw poisson
	sw probit
	sw qreg
	sw reg
	sw regr
	sw regre
	sw regres
	sw regress
	sw stcox
	sw streg
	sw tobit
	sw weibull
	sw gompertz 								 /* out of date */
	/* end sw commands */
	suest
	su sum summ summa summar summari summariz summarize
	sureg
	sunflower
	swilk
	sfrancia
	symmetry
	symmi
	
	table
	tabstat
	ta tab tabu tabul tabula tabulat tabulate
	tab1
	tab2
	tabi
	te tes test
	testparm
	testnl
	tetrachoric
	tob tobi tobit
	total
	print
	translate
	translator q
	translator query
	translator set
	translator reset
	translator `foo'
	transmap q
	transmap query
	transmap def
	transmap define
	transmap `foo'
	treatreg
	truncreg
	ttest
	ttesti

	update
	update from
	update q
	update query
	update ado
	update executable
	update swap
	update all
	update `foo'
	
	view
	view file
 	view browse
	view help
	view search
	view `foo'
	view news
	view net
	view ado
	view update
	view view_d
	view help_d
	view search_d
	view net_d
	view ado_d
	view update_d
	vwls
	which
	xi
	zinb
	zip
	ztnb
	ztb

	/* postestimation stuff */
	/*  don't know a good way to organize, could be repeats */
	estat alternatives
	estat archlm
	estat bgodfrey
	estat bootstrap
	estat clas
	estat covariance
	estat correlation
	estat durbinalt
	estat dwatson
	estat gof
	estat hettest
	estat imtest
	estat ovtest
	estat szroeter
	estat vif
	acprplot
	avplot
	avplots
	cprplot
	dfbeta
	fracplot
	fracpred
	lroc
	lsens
	lvr2plot
	rocplot
	rvfplot
	rvpplot

	adjust
	/* types of commands, which have their own highlighting */
	estat 
	estimates
	/* end subcommand using postestimation */
	hausman
	lincom
	linktest
	lrtest
	mfx
	nlcom
	predict
	predictnl
	suest
	test
	testnl
	
	/* some obsolete stuff */
	ovtest
	hettest
	szroeter
	imtest
	vif
	/* end postestimation */
	
	/* from data management manual */
	ap app appe appen append
	append using
	as ass asse asser assert
	by bys byso bysor bysort
	cd pwd
	cf
	checksum
	clonevar
	codebook
	collapse
	compare
	compress
	contract
	copy
	corr2data
	cou coun count
	cross
	byte int long float double
	/* only gives legal status to str throught str80 --- not for str81 through str 244 for SE */
	str str1 str80 str99 str24444 
	d de des desc descr descri describ describe
	ds lookfor
	destring
	dir ls man
	drawnorm
	drop keep clear
	/* duplicates */
	duplicates r
	duplicates report
	duplicates e
	duplicates examples
	duplicates l
	duplicates list
	duplicates b
	duplicates browse
	duplicates t
	duplicates tag
	duplicates drop
	/* end dup */
	ed edi edit
	b br bro brow brows browse
	/* endless egen & options */
	egen
	/* any() has been mapped to anyvalue() but is not listed as obsolete as of 19.oct.05 */
	egen = any()
	egen = anycount()
	egen = anymatch()
	egen = anyvalue()
	egen = concat() 
	egen = count()
	egen foo = cut( fie )
	egen = diff()
	/* eqany is now anymatch */
	egen = eqany()
	egen = ends()
	egen = fill()
	egen = group()
	egen = iqr()
	egen = kurt()
	/* ma still works, though undocumented through what could be a mistake*/
	egen = ma()
	egen = mad()
	egen = max()
	egen = mdev()
	egen = mean()
	egen = median()
	egen = min()
	egen = mode()
	egen = mtr()
	/* neqany mapped to anycount */
	egen = neqany()
	egen = pc()
	egen = pctile()
	egen = rank()
	/* all the rxxx have been renamed */
	egen = rfirst()
	egen = rlast()
	egen = rmax()
	egen = rmean()
	egen = rmin()
	egen = rmiss()
	egen = robs()
	egen = rsd()
	egen = rsum()
	/* end of obsolete names */
	egen = rowfirst()
	egen = rowlast()
	egen = rowmax()
	egen = rowmean()
	egen = rowmin()
	egen = rowmiss()
	egen = rownonmiss()
	egen = rowsd()
	egen = rowtotal()
	egen = sd()
	egen = seq()
	egen = skew()
	egen = std()
	/* replaced with total */
	egen = sum()
	egen = total()
	egen = tag()
	/* end egen */
 	en enc enco encod encode
	dec deco decod decode
	erase rm
	expand
	expandcl
	fdasav fdasave
	fdause
	fdades fdadesc fdadescr fdadescri fdadescrib fdadescribe
	filef filefi filefil filefilt filefilte filefilter 
	fillin
	format
	/* functions */
	/* math functions */
	abs()
	acos()
	asin()
	atan()
	atan2()
	atanh()
	ceil()
	cloglog()
	comb()
	cos()
	digamma()
	exp()
	floor()
	int()
	invcloglog()
	invlogit()
	ln()
	/* should this count as obsolete? */
	lnfact()
	lnfactorial()
	lngamma()
	log()
	log10()
	logit()
	max()
	min()
	mod()
	reldif()
	round()
	sign()
	sin()
	sqrt()
	sum()
	tan()
	tanh()
	trigamma()
	trunc()
	/* probability functions */
	betaden()
	Binomial()
	binorm()
	binormal()
	chi2()
	chi2tail()
	dgammapda()
	dgammapdada()
	dgammapdadx()
	dgammapdx()
	dgammapdxdx()
	F()
	Fden()
	Ftail()
	gammaden()
	gammap()
	ibeta()
	invbinomial()
	invchi2()
	invchi2tail()
	invF()
	invFtail()
	invgammap()
	invibeta()
	invnchi2()
	invnFtail()
	invnibeta()
	invnorm()
	invnormal()
	invttail()
	nbetaden()
	nchi2()
	nFden()
	nFtail()
	nibeta()
	norm()
	normal()
	normden()
	normalden()
	npnchi2()
	tden()
	ttail()
	/* random number function */
	uniform()
	/* string functions */ 
	abbrev()
	char()
	index()
	indexnot()
	length()
	lower()
	ltrim()
	match()
	plural()
	proper()
	real()
	regexm()
	regexr()
	regexs()
	reverse()
	rtrim()
	string()
	strlen()
	strmatch()
	strofreal()
	strpos()
	subinstr()
	subinword()
	substr()
	trim()
	upper()
	word()
	wordcount()
	/* programming functions */
	autocode()
	byteorder()
	c()
	_caller()
	chop()
	clip()
	cond()
	e()
	epsdouble()
	epsfloat()
	float()
	group()
	has_eprop()
	inlist()
	inrange()
	irecode()
	matrix()
	maxbyte()
	maxdouble()
	maxfloat()
	maxint()
	maxlong()
	mi()
	minbyte()
	mindouble()
	minfloat()
	minint()
	minlong()
	missing()
	r(this should not really highlight)
	recode()
	replay()
	return()
	s()
	scalar()
	/* date functions */
	d()
	date()
	day()
	dow()
	doy()
	halfyear()
	mdy()
	month()
	quarter()
	week()
	year()
	/* time series functions */
	d()
	daily()
	halfyearly()
	monthly()
	quarterly()
	weekly()
	yearly()
	yh()
	ym()
	yq()
	yw()
	d() yet again
	h()
	m()
	q()
	w()
	y()
	dofd()
	dofh()
	dofm()
	dofq()
	dofw()
	dofy()
	hofd()
	mofd()
	qofd()
	wofd()
	yofd()
	tin()
	twithin()
	wofd()
	yofd()
	/* matrix to matrix */
	cholesky()
	corr()
	diag()
	get()
	hadamard()
	I()
	inv()
	invsym()
	J()
	matuniform()
	nullmat()
	sweep()
	syminv()
	vec()
	vecdiag()
	/* matrix to scalar */
	colnumb()
	colsof()
	det()
	diag0cnt()
	el()
	issym()
	matmissing()
	mreldif()
	rownumb()
	rowsof()
	trace()
	/* end functions */
	g ge gen gene gener genera generat generate
	replace
	gsort
	hexdump
	/* icd9 commands */
	icd9 check
	icd9p check
	icd9 clean
	icd9p clean
	icd9 gen
	icd9p generate
	icd9 l
	/* got to figure out how to fix the lookup here */
	icd9p lookup
	icd9 sea
	icd9p search
	icd9 q
	icd9 query
	impute
	infile
	infix
	inp inpu	input
	insheet
	ins insp inspe inspec inspect
	ipolate
	isid
	joinby
	/* label */
	la da
	label data
	la de
	label define
	la di
	label dir
	lab drop
	labe save
	la val
	lab val
	label values
	la var
	label variable
	/* end label */
	la lang
	label language
	labelbook
	numlabel
	uselabel
	l li lis list
	fl fli flis flist
	lookfor
	memory
	mer merg merge
	mkdir
	mvencode
	mvdecode
	note notes
	/* odbc */
	odbc li
	odbc list
	odbc q
	odbc query
	odbc des
	odbc describe
	odbc lo
	odbc load
	odbc ins
	odbc insert
	odbc exe
	odbc exec
	odbc sql
	odbc sqlfile
	/* end odbc */
	order mov move aorder
	ou out outf outfi outfil outfile
	outs outsh outshe outshee outsheet
	pctile
	xtile
	_pctile
	range integ
	recast
	recode
	ren rena renam rename renpfix
	/* reshape ... */
	reshape long
	reshape wide
	reshape error
	reshape i
	reshape j
	reshape xij
	reshape xi
	reshape
	reshape q
	reshape query
	reshape clear
	rmdir
	sample
	save saveold u us use
	separate
	sh she shel shell
	xsh xshe xshel xshell
	so sor sort
	split
	stack
	statsby
	sysuse
	ty typ type
	u us use
	webuse
	webuse query
	webuse set
	xmlsav xmlsave
	xmluse
	xpose
	/* end data management */

	/* from multivariate model */
	biplot
	ca
	cabiplot
	caprojection
	estat coordinates
	estat distances
	estat inertia
	estat profiles
	estat table

	canon
	estat correlations
	estat loadings

	/* cluster commands */
	/* in Stata 8 these were in  the cluster analysis manual */
	/* initially ordered by the intro (so that all the linkage subcommands are together */
	
	cluster k
	cluster kmeans
	cluster kmed
	cluster kmedians
	cluster s
	cluster singlelinkage
	cluster a
	cluster averagelinkage
	cluster c
	cluster completelinkage
	cluster wav
	cluster waveragelinkage
	cluster med
	cluster `foo'
	cluster medianlinkage
	cluster ward
	cluster wardslinkage
	cluster stop
	cluster dend
	cluster dendrogram
	/* cluster tree is listed in online help as synonym for cluster dendogram */
	cluster tr
	cluster tree
	cluster gen
	cluster generate
	cluster note
	cluster notes
	cluster dir
	cluster list
	cluster drop
	cluster rename
	cluster renamevar
	cluster query
	cluster set
	cluster del
	cluster delete
	cluster parsedist
	cluster parsedistance
	cluster measures
	/* not in the front for some reason */
	cluster cent
	cluster centroidlinkage
	/* clustermat commands */
	clustermat s
	clustermat singlelinkage
	clustermat a
	clustermat averagelinkage
	clustermat c
	clustermat completelinkage
	clustermat wav
	clustermat waveragelinkage
	clustermat med
	clustermat `foo'
	clustermat medianlinkage
	clustermat ward
	clustermat wardslinkage
	/* end clustermat commands */

	fac fact facto factor
	factormat
	/* uh oh, factor estat stuff */
	estat anti
	estat common
	estat factors
	estat kmo
	estat residuals
	estat rotatecompare
	estat smc
	estat structure
	loadingplot
	rotate
	scoreplot
	screeplot
	/* end factor estat stuff */
	hotelling
	mano manov manova
	manovatest
	mat dis
	matrix dissimilarity
	mds
	/* mds postestimation */
	estat config
	estat correlations
	estat pairwise
	estat quantiles
	estat stress
	mdsconfig
	mdsshepard
	/* end mds postestimation */
	mdslong
	mdsmat
	
	/* score is now obsolete */
	sco scor score
	pca
	pcamat
	/* pca postestimation */
	estat anti
	estat kmo
	estat loadings
	estat residuals
	estat rotatecompare
	estat smc
	loadingplot
	rotate
	scoreplot
	screeplot
	/* end pca postestimation */
	procrustes
	/* procrustes postestimation */
	estat compare
	estat mvreg
	estat summarize
	procoverlay
	rot rota rotat rotate
	rotatemat
	scoreplot
	loadingplot
	greigen

	/* from the survival analysis manual */ 

	ctset
	cttost
	ir
	iri
	cs
	csi
	cc
	cci
	tabodds
	mhodds
	mcc
	mcci
	ltable
	snapspan
	st_is 2
	st_show
	st_ct
	stbase
	stci
	stcox
	/* stcox diagnostics */
	stphplot
	stcoxkm
	/* stcox postestimation */
	estat con
	estat phtest
	stcurve
	/* end stcox postestimation */
	stphtest
	stcurve
	stdes
	stfill
	stgen
	stir
	stptime
	strate
	stmh
	stmc
	streg
	sts
	sts g
	sts graph
	sts l
	sts list
	sts `foo'
	sts t
	sts test
	sts gen
	sts generate
	stset
	streset
	stsplit
	stjoin
	stsum
	sttocc
	sttoct
	stvary

	/* from the survey data manual */
	/* post estimation commands */
	estat svyset
	estat eff
	estat effects
	estat lceff
	estat lceffects
	estat size
	estat vce
	/* end survey post estimation commands */
	lincom
	svy: heckman
	svy: heckprob
	svy: ivreg
	svy: intreg
	svy jack: logistic
	svy: logit
	svy: mean
	svy: mlogit
	svy: nbreg
	svy: gnbreg
	svy linear: gnbreg 
	svy brr: gnbreg
	svy: ologit
	svy: oprobit
	svy: poisson
	svy: probit
 	svy: proportion
	svy: ratio
	svy: reg
	svy: regress
	svy: tab
	svy: tabul
	svy: tabulate
	svy:total
	svydes
	svymarkout
	svyset
	/* still can be used, but not listed anywhere */
	testparm

	/* end of the survey stats book */

	/* time series */
	arch
	arima
	corrgram
	ac
	pac
	cumsp
	dfgls
	dfuller
	fcast c
	fcast com
	fcast compute
	fcast g
	fcast graph
	haver des
	haver describe
	haver use
	/* irf commands ... starting Stata 8.2 */
	irf a
	irf add
	irf
	irf cg
	irf cgraph
	irf cr
	irf create
	irf ct
	irf ctable
	irf d
	irf describe
	/* irf dir seems to be dead */
	irf di
	irf dir
	irf drop
	irf erase
	irf g
	irf graph
	irf og
	irf ograph
	irf ren
	irf rename
	irf set
	irf t
	irf table
	newey
	pergram
	pperron
	prais
	rolling
	dwstat
	durbina
	bgodfrey
	archlm
	/* should these options be required?? */
	tsappend, add(4) last(foo) tsfmt(string)
	tsfill
	tsline
	twoway tsline
	graph twoway tsline
	tsrline
	tsreport
	tsrevar
	tsset
	tssmooth ma
	tssmooth `foo'
	tssmooth e
	tssmooth exponential
	tssmooth d
	tssmooth dexponential
	tssmooth h
	tssmooth hwinters
	tssmooth s
	tssmooth shwinters
	tssmooth nl
	var
	/* var post estimation commands */
	/* these seem to be common to var and svar */
	fcast compute
	fcast graph
	irf
	vargranger
	varlmar
	varnorm
	varsoc
	varstable
	varwle
	/* end var post estimation commands */
	svar
	varbasic
	
	/* varfcast is obsolete as of July 23, 2004 */
	/* varirf is obsolete as of July 23, 2004 */
	vec
	/* vec post estimation commands */
	veclmar
	vecnorm
	vecrank
	vecstable
	wntestb
	wntestq
	xcorr
	/* end time-series */
   
	
	/* stuff from the crossectional timeseries book */
	/* now called the longitudinal/panel data book */
	

	iis
	tis
	quadchk
	xtabond
	xtcloglog
	xtdata
	xtdes
	xtfrontier
	xtgee
	estat wcorrelation
	/* estat wcorrelation replaced xtcorr */
	xtcorr
	xtgls
	xthaus
	xthtaylor
	xtintreg
	xtivreg
	xtline
	xtlogit
	xtmixed
	/* xtmixed post estimation commands */
	estat group
	estat recovariance
	/* end xtmixed post estimation commands */
	xtnbreg
	xtpcse				/*  */
	xtpois                               /* old but not out of date */
	xtpoisson
	xtprobit
	xtrc
	xtreg
	xttest0
	xtregar
	xtsum
	xttab
	xttrans
	xttobit

	/* end stuff from crossectionaltime-series */

	/* programming manual */

	nobreak
	break
program dingle, rclass byable(recall)
program foobar, sclass byable(onecall)
	cap
	capture
	char
	char l
	char list
	char ren
	char rename
	*** class added for saving programs with correct names ***
	a.bc.new
	ab.b.copy
	a.compress.ref
	a.objtype
	a.b.isa
	a.b.classname
	a.b.isofclass
	a.b.objkey
	a.c.uname
	a.b.ref_n
	a.b.arrnels
	a.b.arrindexof
	a.b.classmv
	a.b.instancemv
	a.b.dynamicmv
	a.b.superclass

	a.b.Declare
	a.b.Arrdropel
	a.b.Arrdropall
	a.b.Arrpop
	a.c.Arrpush

	/* cannot seem to get these to work */
	.Global.foo.gringo
	.Local.d.e.f
	.Super.q.e.d
	4.2

	class fooey {
		classwide:
		instance:
		instancespecific:
		} 
	
	class exit
   
	classutil drop
	classutil d
	classutil describe
	classutil dir
	classutil cdir
	classutil which
	classutil `foo'

	/* confirm commands */
	conf e
	confi existence
	confir new f
	confirm file
	/* won't highlight because of need for a subcommand */
	confirm `foo'
	conf name
	confi names
	/* should not work */
	confirm int number
	confir integer n
	confirm number
	conf mat
	confirm matrix
	conf sca
	conf scalar
	conf new v
	conf numeric va
	confirm str var
	confirm string vari
	confirm byte varia
	confirm int variab
	conf long variabl
	conf float variable
	confirm str11 v

	continue

	/* oh no! the cclass stuff */
	cret l
	creturn list

	c( current_date )
	c(current_date)
	c(current_time)
	c(rmsg_time)
	c(stata_version)
	c(version)
	c(born_date)
	c(flavor)
	c(SE)
	c(mode)
	c(console)
	c(os)
	c(osdtl)
	c(machine_type)
	c(byteorder)

	c(sysdir_stata)
	c(sysdir_updates)
	c(sysdir_base)
	c(sysdir_site)
	c(sysdir_plus)
	c(sysdir_personal)
	c(sysdir_oldplace)
	c(adopath)
	c(pwd)
	c(dirsep)

	c(max_N_theory)
	c(max_k_theory)
	c(max_width_theory)
	c(max_matsize)
	c(max_macrolen)
	c(max_cmdlen)
	c(namelen)

	c(mindouble)
	c(maxdouble)
	c(epsdouble)
	c(minfloat)
	c(maxfloat)
	c(epsfloat)
	c(minlong)
	c(maxlong)
	c(minint)
	c(maxint)
	c(minbyte)
	c(maxbyte)
	c(maxstrvarlen)

	c(N)
	c(k)
	c(width)
	c(changed)
	c(filename)
	c(filedate)

	c(memory)
	c(maxvar)
	c(matsize)

	c(more)
	c(rmsg)
	c(dp)
	c(linesize)
	c(pagesize)
	c(logtype)
	c(eolchar)
	c(icmap)

	/* should have something for platform-specific */
	c(dockable)
	c(dockingguides)
	c(locksplitters)
	c(persistfv)
	c(persistvtopic)
	c(reventries)
	c(smalldlg)
	c(xptheme)
	c(fastscroll)
	c(revwindow)
	c(varwindow)
	c(smoothfonts)
	c(smoothsize)
	/* not specific */
	c(linegap)
	c(scrollbufsize)
	c(varlabelpos)
	c(maxdb)

	c(graphic)
	c(scheme)
	c(printcolor)
	c(copycolor)
	c(macgphengine)
	c(piccomments)

	c(adosize)
	c(virtual)

	c(checksum)
	c(timeout1)
	c(timeout2)
	c(httpproxy)
	c(httpproxyhost)
	c(httpproxyport)
	c(httpproxyauth)
	c(httpproxyuser)
	c(httpproxypw)

	c(trace)
	c(tracedepth)
	c(tracesep)
	c(traceindent)
	c(traceexpand)
	c(tracenumber)
	c(tracehilite)

	c(matastrict)
	c(matalnum)
	c(mataoptimize)
	c(matafavor)
	c(matacache)
	c(matalibs)
	c(matamofirst)

	c(type)
	c(level)
	c(maxiter)
	c(searchdefault)
	c(seed)
	c(varabbrev)
	c(`foo')

	c(pi)
	c(alpha)
	c(ALPHA)
	c(Mons)
	c(Months)
	c(Wdays)
	c(Weekdays)
	c(rc)

	/* end of that mess */
	#d cr
#delimit ;

	this is another command;
	this is fine
	  this is fine, too;
	this is ok;
#delimit cr
	this is ok /// this should look like a comment
	  why is this a comment
	here is something // this is a comment
	this is fine
	
#delimit ;
	
	this is funny?;
	this
	  
	  /* this */
	  command
	  continuation;
	
	this is a test /// this is a comment
	  this is not a comment /// some more comments
	  this is fine!;
	
	this is OK;
	
	commands 
	  this should behave as a continuation?;
	this is a new line;
	if this==that {;
		this is an if clause
		  another continuation;
		};  
	foo;       glue; silly;
	this is a continuation, it should indent properly
	  this is ok;
	
	if this | that {;
		indent;
		}; 
	whooie!
	  continuation;
	
	
#delimit cr
	
	this "#delim ;" is inside quotations, and hence is invalid

	if {
		test
		}   
	
#delimit cr

	/* all the dialog stuff is in syntax_tester.dlg, because the dlg stuff should really
	be a separate mode ... ugh */
	discard
	di dis disp displ displa display
	display as text
	display as txt
	display as res
	display as result
	display as err
	display as error
	display as inp
	display as input
	display in smcl
	display _asis
	display _s(4)
	display _skip(3)
	display _col(4)
	display _column(2)
	display _new(3)
	display _newline(3)
	display _newline 
	display _c
	display _continue
	display _dup(3)
	display _r(fuggy)
	display _request(jiminy)
	display _char(4)
	display in blue
	

	/* ereturn... */
	eret loc
	eret loca
	eretu sca
	eretur scalar
	ereturn mat
	eret matrix
	eretu clear
	/* should fail to highlight */
	ereturn `foo'
	eretur li
	ereturn list
	eret post
	eretu repost
	eretur di
	ereturn display

	err 444
	error 666

	_est h
	_esti hold
	_estim u
	_estima unhold
	_estimat dir
	_estimate clear
	_estimates drop
	_estimates `foo'
	
	e
	exit

	file open
	file r
	file read
	file w
	file write
	file seek
	file set
	file close
	file q
	file query

	findfile


	foreach grue in dingle {
		}
	foreach bleen of loc hooie {
		}
	foreach mike of loc frantie {
		}
	foreach small of glo biggie {
		}
	foreach big of global smallie {
		}
	foreach var of var thevars {
		}
	foreach var of varlist thevars {
		}
	foreach makeme of new newvarlist {
		}
	foreach makeme of newlist newvarlist {
		}
	foreach number of num somenumlist {
		}
	foreach number of numlist somenumlist {
		}
	forvalues fooie = 1/4 {
		}

	gettoken foo : griminy, parse(" ,")
	gettoken bleeble bauble: foo, parse(",")
	hexdump

	if foo fuggy
	if `this' that
	if `those' {
		something
		}
	if foo {
		fuggy
		}
	else `fortuna'
	else frantabulous
	else {
		frantabulous
		}

	levelsof

	/* macro stuff */
	gl fooie
	global fooie
	loc fooie
	local fooie
	tempvar ding
	tempname dong
	tempfile the
	/* right */
	loc ++witch
	loc which--
	local --is
	ma di
	macro dir
	macro drop 
	ma l
	macro list
	ma s
	macro shift

	glo fooey : properties
	glo dingle : ty
	global dingle : type
	loc dingle : f
	local dingle : format
	gl h : val lab
	global h : value label
	loc h : var lab
	local h: variable label
	gl h : data l
	global h: data label
	local h: sort
	local h: sortedby
	loc h : lab
	local h : label
	gl h : constraint
	global h: constraint
	loc h : char
	local h: char
	gl h : permname
	global h : permname
	loc h : dir
	local h: sysdir
	gl h : env
	global h : environment
	loc h : r(scalars)
	local h : r(macros)
	gl h: r(matrices)
	global h: r(functions)
	loc h : e(scalars)
	local h : e(macros)
	gl h: e(matrices)
	global h: e(functions)
	loc h: s(macros)
	global h: all globals
	global h: all scalars
	loc h: all matrices
	local h: all numeric scalars
	local h: all string scalars
	local h: all scalars
	local h: di
	local h: display
	gl h : list
	global h : rown
	gl h : rownames
	local h : coln
	local h :colnames
	local h : rowf
	local h : rowfullnames
	local h : colf
	local h : colfullnames
	local h : rowe
	local h : roweq
	local h : cole
	local h : coleq
	glo foo: tsnorm 
	local h : word
	local h : word count
	loc h : word
	local h : piece
	local h : length loc
	local h : length local
	local h : length gl
	local h : length global
	local h : subinstr gl
	local h : subinstr global
	local h : subinstr loc
	local h : subinstr local
	local h : tempv
	local h : tempvar
	local h : tempf
	local h : tempfile

	/* macro lists */

	loc foo : list uniq bar
	global foo : list dups bar
	glob foo: list sort bar
	loca foo : list retok bar
	local foo:list retokenize bar
	glo foo : list clean bar
	glob foo : list a | b
	globa foo: list c & d
	global foo : list ding - dong
	global foo: list this == that
	global foo: list this === that
	loc foo: list hey in ho
	local foo: list sizeof hey
	local foo: list posof "this is something" in hooie

	/* ahh the macros are over */

	makecns a
	matcproc a b c
	
	marksample hooie
	mark
	markout
	markin
	svymarkout fiem

	matlist
	
	/* matrix commands */
	mat ac m
	matr accum matt
	matri glsa matt
	matrix glsaccum matt
	mat opaccum matt
	matrix veca matt
	matrix vecaccum matt
	/* not listed but still accepted */
	matr makeCns foo 
	matri dispCns
	matcproc a b c

	/* dangerous keyword highlighting which is unavoidable */
	mat def foo
	mat defin
	matrix in
	mat input bling
	matrix jjj

	mat diss foo
	matrix dissimilarity bleen
	
	mat eigenval vgy mmk
	mat eigenvalues bleeble blob
	/* nothing for matrix get */
	mat_put_rr bling
	mkmat
	svmat noodle
	matname foo
	mat rown njk = kjj
	matrix rownames rrr = ccc
	mat coln ccc = rrr
	matrix colnames ccc = rrr
	mat roweq hi = ho
	mat coleq ho = hi
	mat sco fooey
	matrix score fooey
	mat svd g h j
	matrix syme jwjwk foo
	matrix `foo'
	
	/* phew, matrix is finally done */

	mor
	more

	numlist

pause on
pause off
pause "fuggy"

	/* don't know what would be different here */
program fooey, plugin
	/* hmm.... */
	
	postfile
	post
	postclose
	postutil dir
	postutil clear

	_predict
	preserve
	restore

pr def foo
pro def foo

	/* should fix the following (doesn't need to be flush left
		the problem really is that since define is now optional, it is hard
	for the syntax to be corrected */
program dir 

program drop fooie
program list fooie 

	qui blah
	quietly {
		n bling
		noisily blang
		}
	set ou p
	set output proc
	set output i
	set ou inform
	set ou e
	set output error

	_ret hold
	_retu res
	_retur restore
	_return drop
	_return dir
	_return `foo'

	return `foo'
	ret clear
	retu sca foo
	return sca foo
	ret loc foo
	return local foo
	ret mat matt
	return matrix matt
	ret add
	return add

	eret clear
	ereturn clear
	eret post m1 m2
	ereturn post
	eret sca
	ereturn scalar
	eret loc foo
	ereturn local foo
	ereturn matrix bleen
	eret repost
	ereturn repost

	sret clear
	sret loc foo
	sreturn local foo
	/* end return commands */
   
	_rmcoll
	_rmdcoll
	set rmsg on
	set rmsg off

	_robust

	/* sca is an ambiguous abbreviation sc for scatterplot and sca for scalar ! */
	sca foo
	scalar define foo
	scalar foo
	scalar di
	scalar dir
	sca l
	sca list
	scalar drop

	/* new serset commands */

	serset cr
	serset create
	serset create_xmedians
	serset create_cspline
	serset set
	serset sort
	serset su
	serset summarize
	serset
	serset use
	serset reset_id
	serset drop
	serset clear
	serset dir
	file sersetwrite 
	file sersetwrite

	/* oops - Stata has extended macro functions just for serset */

	loc foo: serset id
	loc foo: serset k
	loc foo: serset N
	loc foo: serset varnum
	glo foo: serset type
	glo foo: serset format
	glo foo: serset varnames
	glo foo: serset min
	glo foo: serset max

	sleep

	/* smcl */

	INCLUDE help
	/* syntax 1 and 2 */
	{sf}
	{sf:foo}
	{it}
	{it:foo}
	{bf}
	{bf:bar}
	/* should fail */
	{sf should fail}
	{sf should:fail}
   
	{input}
	{input:foo}
	{error}
	{error:hahah}
	{result}
	{result:shocking}
	{text}
	{text:for later reading}

	{inp}
	{inp:foo}
	{err}
	{err:hahah}
	{res}
	{res:shock}
	{txt}
	{txt:later}

	{cmd}
	{cmd:Go Home!}
	/* hybrid syntax */
	{cmdab:this:that}

	/* no checking for bad opt syntax */
	{opt fooey}
	{opt foo(bar)}
	{opt foo(bar,yah)}
	{opt foo(bar|yah)}
	{opt foo:bar}
	{opt foo:bar(3)}
	{opt foo:bar(from,to)}
	{opt foo:bar(this|that)}

	/* syntax 1 & 2 */
	{hilite}
	{hilite:of the day}
	{hi}
	{hi:how are you}

	/* syntax 2 & 3 */
	{ul on}
	{ul:is no. 1 in basketball}
	{ul off}
	/* should fail */
	{ul bogus}

	/* syntax 2 & 3 (book says 2 & 4 but illustrates with 2 & 3) */
	{*:comment}
	{* this is a comment}

	{hline}
	{hline 20}
	{hline bogus}
	{.-}
	{hline `this'}

	{dup 23:some}
	{dup bogus:some}

	{c 666}
	{char 333}
	{char bogus}

	{reset}

	b
	boldb
	/* link commands.... */
	{help someword}
	{help someword:clickable}
   {helpb bold}
	{helpb bold:hack}
	{manhelp unix}
	{manhelp unix:unix}
	{manhelp damn G:its own syntax}
	{manhelp this should fail:if I had time}
	{manhelpi fooey}
	{manhelpi fooey:cakes}
	/* need yet another @#@#$@ syntax for this hack */
	{help stata##anchors}
	{help stata##anchor|viewer}
	{help stata##anchor:subtext}
	{help stata##anchor|viewer:subtext}
	{help_d:fooie}

	{newvar}
	{newvar:13}
	{var}
	{var:fooey}
	{varname}
	{varname:fooey}
	{vars}
	{vars:huey duey looie}
	{varlist}
	{varlist: huey duey looie}
	{depvar}
	{depvar: fooey gooey}
	{depvars}
	{depvars: ha ho}
	{depvarlist}
	{depvarlist: hee high ho}
	{indepvars}
	{indepvars: hoo who}
	{ifin}
	{weight}
	{dtype}
	{search goofay}
	{search goofy:clickable}

	{search_d:fooey}
	{dialog hello}
	{dialog hellp:clickable}
	{browse fooey}
	{browse fooey:click}
	{view fooey}
	{view fooey:click}
	{view_d:hahah}
	{news:is bad}
	{net fishing}
	{net fishing:wide}
	{net_d:hello}
	{netfrom_d:howdydoody}
	{ado foo}
	{ado foo:bar}
	{ado_d : bar}
	{update howdy}
	{update howdy:doody}
	{update_d:morning}
	{back:and forth}
	{clearmore:fooey}
	{stata corp}
	{stata corp:click}
	{matacmd arrg}
	{matacmd arrg:ahoy}
	/* for line mode */
	{title:howdy doody}
	{center:middle}
	{centre:muddle}
	{rcenter:teehee}
	{rcenter 33:friday!}
	{center 23:fiddle}
	{center 43:fuddle}
	{center bogus:haha}
	{centre 59:foo!}
	{right:wing neocon}
	{lalign 69:ihtfp}
	{ralign 666:nationalist}
	{dlgtab 34:fooey}
	{dlgtab : fooey}
	{...}
	{col bogus}
	{col 32}
	{space bogus}
	{space 43}
	{tab}

	/* for paragraph mode */
	{p}
	{p 4}
	{p bogus}
	{p `hoo'}
	{p 3 4}
	{p 3 4 5}
	{p 3 4 5 oh no}
	/* uh oh, all sorts of equivalent directives */
	{pstd}
	{psee}
	{phang}
	{pmore}
	{pin}
	{phang2}
	{pmore2}
	{pin2}
	{phang3}
	{pmore3}
	{pin3}
	{p_end}
	{p2colset 1 2 3 4}
	{p2col 2 3 4 5: fooey}
	{p2col : first col}
	{p2colreset}
	{synoptset}
	{synoptset 5}
	{synoptset 5 tabbed}
	{synopthdr}
	{synopthdr: damn}
	{syntab: this}
	{synopt: is}
	{p2coldent: no fun}
	{synoptline}

	{bind:all this together}
	{break}
	{asis}
	{s6hlp}
	{ccl}
	{char 7}

	/* end smcl */

	args mac1 mactheknife
	args foo
	args foo1 foo2 foo3 foo4, bogus

	/* syntax */
	/* no attempt to get this to fontify properly, sadly enough */
	syntax

	varlist
	varname
	newvarlist
	newvarname

	/* back to things I can handle */
	sysdir
	sysdir l
	sysdir list
	sysdir set
	personal
	personal dir
	adopath
	adopath + dingle
	adopath ++ freeble
	adopath - foo
	set a 30
	set adosize 99
	tabdisp

	token tokeni tokeniz tokenize

set tr on
set trace off
	set traced 44
	set tracedepth 34
	set tracee on
	set traceexpand off
	set tracesep on
	set traces off
	set tracei on
	set traceindent off
	set tracen on
	set tracenumber off
	set traceh "fooey"
	set tracehilite "hahaha"

	unab lfoo : dingle
	tsunab lfoo : dongle

	unabcmd

	/* more complicated version commands :<( */
version 8
version 8: fooie
	viewsource

	while foo {
		this is some stuff
		}
	/* window commands... seem to have been moved out of the manual?!?*/
	win fop
	window fopen
	win fs
	window fsave
	win man minimize
	window manage restore
	win man prefs load
	win man prefs save
	win man prefs default
	win man update variable
	win man associate
	window man maintitle "fooey"
	windo manag maintitle reset
	window manage forward command
	window manage forward doeditor
	window manage forward graph
	window manage forward help
	window manage forward results
	window manage forward review
	window manage forward variables
	window manage forward viewer
	
	win man print graph
	win man close graph
	win man rename graph

	win man print viewer
	win man forward viewer
	win man close viewer

	window m clear
	win menu append submenu
	win m append item
	window menu append separator
	window menu refresh
	window menu add_recentfiles

	/* obsolete?? */
	window menu popout
	window menu set
	window menu append   popout
	window menu append   string

	window push
	window stop stop
	window stopbox note
	window stop rusure
	/* end programming manual */

	/* the miserable graph commands... */

	gr7 using foo
	graph7 this that

	gr bar
	graph bar
	gr hbar
	graph hbar

	graph box
	graph hbox

	graph combine

	gr des
	graph describe

	graph dir

	graph di
	gr display

	graph dot

	graph drop
	graph drop _all

	gr export
	graph export
	
	graph matrix

	/* these /should/ be previous testing lines, since they are documented in other */
	/*  manuals. Still... they are repeated here */

	histogram
	symplot
	quantile
	qnorm
	pnorm
	qchi
	pchi
	qqplot
	gladder
	qladder
	spikeplot
	dotplot
	sunflower
	kdensity
	lowess
	avplot
	cprplot
	lvr2plot
	rvfplot
	rvpplot
	ac
	pac
	pergram
	cumsp
	xcorr
	wntestb
	varfcast graph
	varirf graph
	varirf ograph
	varirf cgraph
	fcast graph
	varstable
	vecstable
	irf graph
	irf ograph
	irf cgraph

	xtline

	sts graph
	strate
	ltable
	stci
	stphtest
	stphplot
	stcoxkm
	estat phtest
	stcurve
	
	roctab
	rocplot
	roccomp
	lroc
	lsens

	biplot
	cluster dendrogram
	screeplot
	scoreplot
	loadingplot
	procoverlay
	cabiplot
	caprojection
	mdsconfig
	mdsshepard

	cusum
	cchart
	pchart
	rchart
	xchart
	shewhart
	serrbar
	
	tabodds
	pkexamine

	/* end of so-called graph other */
	graph pie

	graph print
	gr q
	graph query

	graph rename
	gr save

	gr set

	gr twoway fee fie fo
	gr twoway (bar foo) (bar fee)
	twoway bar foo || bar fee

	/* forget the stuff under twoway */
	/* redone in the order of the commands themselves to accommodate abbrevs */

	gr tw scatter
	
	graph twoway area y
	twoway area 
	graph twoway bar y
	tw con
	two connected
	graph twoway dot y
	graph twoway dropline y
	graph twoway fpfit y
	graph twoway fpfitci y
	graph twoway function y
	graph twoway hist
	tw histogram
	graph twoway kdensity
	/* this is a problem because lfit is obsolete, but also OK */
	graph twoway lfit
	graph twoway lfitci
	gr two line
	twow line
	line foo bar
	graph twoway lowess
	graph twoway mband
	graph twoway mspline
	twoway pcarrow
	twoway pcbarrow
	tw pcbarrowi
	two pccapsym
	twoway pci
	two pcscatter
	two pcspike
	graph twoway qfit
	graph twoway qfitci
	graph twoway rarea
	graph twoway rbar
	graph twoway rcap
	graph twoway rcapsym
	graph twoway rconnected
	tw rcon
	twow rl
	graph twoway rline
	twowa rsc
	graph twoway rscatter
	graph twoway rspike
	graph tw scatter
	two sc
	scat
	graph twowa scatteri
	graph twoway spike
	twoway tsline
	two tsrline
	graph use

	palette color
	palette line
	palette linepalette
	palette symbol
	palette symbolpalette

	set scheme
	q graph
	query graphics
	set graphics on
	set graphics off
	set printcolor auto
	set printcolor automatic
	set copycolor auto

	/* now for some Mata content */
	/* first - all the reserved words */
	aggregate
	array
	boolean
	break /* used elsewhere */
	byte /* used elsewhere */
	case
	catch
	class
	colvector
	complex
	const
	continue /* used elsewhere */
	default
	delegate
	delete
	do /* used elsewhere */ 
	double /* used elsewhere */
	else /* used elsewhere */
	eltypedef
end /* used elsewhere */
	enum
	explicit
	export
	external
	float /* used elsewhere */
	for /* used elsewhere */
	friend
	function
	global /* used elsewhere */ 
	goto
	if /* used elsewhere */
	inline
	int /* used elsewhere */
	local /* used elsewhere */
	long /* used elsewhere */
	mata
	matrix /* used elsewhere */
	namespace
	new
	NULL
	numeric
	operator
	orgtypedef
	pointer
	polymorphic
	pragma
	private
	protected
	public
	quad
	real
	return
	rowvector
	scalar /* used elsewhere */
	short
	signed
	sizeof
	static
	string
	struct
	super
	switch
	template
	this
	throw
	transmorphic
	try
	typedef
	typename
	union
	unsigned
	using /* used elsewhere */
	vector
version /* used elsewhere */
	virtual
	volatile
	void
	while /* used elsewhere */ 
	
	/* mata building blocks */
	for(hey; ho; wego) {
		hmmmm
		}

	/* trying to have some mata stuff */
	pragma unset
	pragma unused
	
	/* from "commands for controlling mata" */
	mata help
	mata clear
	mata d
	mata describe
	mata drop
	mata help

	mata mlib create foo
	mata mlib add bar
	mata mlib index
	mata mlib q
	mata mlib query

	mata matsave foo
	mata matuse bar
	mata matd breeble
	mata matdescribe box
	mata memory
	mata mosave momoney()
	mata query
	mata set matacache
	mata set matalnum on
	mata set matalnum off
	mata set mataoptimize on
	mata set mataoptimize off
	mata set matafavor space
	mata set matafavor speed
	mata set matastrict on
	mata set matastrict off
	mata set matalibs
	mata set matamofirst on
	mata set matamofirst off
	mata stata
	mata which

	/* mata functions */
	/* first, the functions added in the update of 20jan2006 */
	adosubdir()
	ghk()
	halton()
	_halton()
	ghalton()
	lnnormal()
	lnnormalden()
	pathsearchlist()

	/* now for the stuff in the manuals */
	printf()
	errprintf()
	display()
	displayas()
	displayflush()
	more()
	direxists()
	dir()
	chdir()
	findfile()
	fileexists()
	cat()
	unlink()
	fopen()
	fclose()
	fget()
	fgetnl()
	fread()
	fput()
	fwrite()
	fgetmatrix()
	fputmatrix()
	fstatus()
	ftell()
	fseek()
	ftruncate()
	pathjoin()
	pathsplit()
	pathbasename()
	pathsuffix()
	pathrmsuffix()
	pathisurl()
	pathisabs()
	pathasciisuffix()
	pathstatasuffix()
	pathsubsysdir()
	
	transposeonly()
	_transpose()
	diag()
	diagonal()
	lowertriangle()
	uppertriangle()
	makesymmetric()
	sort()
	jumble()
	order()
	invorder()
	revorder()
	_collate()
	uniqrows()
	_fillmissing()
	editmissing()
	editvalue()
	edittozero()
	edittozerotol()
	edittoint()
	edittointtol()
	vec()
	vech()
	invvech()
	rowshape()
	colshape()

	fft()
	invfft()
	convolve()
	Corr()
	ftperiodogram()
	ftpad()
	ftwrap()
	ftunwrap()
	ftretime()
	ftfreqs()
	spline3()
	spline3val()
	polyeval()
	polysolve()
	polytrim()
	polyderiv()
	polyinteg()
	polyadd()
	polymult()
	polydiv()
	polyroots()

	trace() /* already regular function */
	det()   /* already regular function */
	dettriangular()
	norm() /* great - Stata reused an obsolete command */
	cond()
	rank()
	cholesky() /* already regular function */
	cholsolve()
	cholinv()
	invsym() /* already regular function */
	lud()
	lusolve()
	luinv()
	qrd()
	qrdp()
	hqrd()
	hqrdp()
	hqrdmultq()
	hqrdmultqlt()
	hqrdq()
	hqrdq1()
	hqrdr()
	hqrdr1()
	qrsolve()
	qrinv()
	svd()
	svdsv()
	fullsvd()
	fullsdiag()
	svsolve()
	pinv()
	solvelower()
	solveupper()
	eigensystem()
	eigenvalues()
	symeigensystem()
	symeigenvalues()
	lefteigensystem()
	matpowersym()
	matexpsym()
	matlogsym()
	_equilrc()
	_equilr()
	_equilc()
	_perhapsequilrc()
	_perhapsequilr()
	_perhapsequilc()
	rowscalefactors()
	colscalefactors()

	args()
	isfleeting()
	callersversion()
	favorspeed()
	findexternal()
	crexternal()
	rmexternal()
	direxternal()
	valofexternal()
	setbreakintr()
	querybreakintr()
	breakkey()
	breakkeyreset()
	assert()
	asserteq()
	c() /* already regular function */
	sizeof()
	swap()
	exit()
	error()
	_error()

	Re()
	Im()
	C()
	abs() /* already regular function */
	sign() /* already regular function */
	quadrant()
	dsign()
	conj()
	exp() /* already regular function */
	ln() /* already regular function */
	log() /* already regular function */
	log10() /* already regular function */
	sqrt() /* already regular function */
	/* piles of other functions ... skipped here */
	factorial()
	lnfactorial() /* already regular function */
	gamma()
	lngamma() /* already regular function */
	digamma() /* already regular function */
	/* once again, regular functions */

	/* solvers (all defined earlier) */
	cholsolve()
	lusolve()
	qrsolve()
	svsolve()
	invsym()
	cholinv()
	luinv()
	qrinv()
	pinv()

	/* mata standard */
	I() /* already regular function */ 
	e() /* already regular function */ 
	J() /* already regular function */ 
	designmatrix()
	blockdiag()
	range()
	unitcircle()
	uniform() /* already regular function */ 
	uniformseed()
	Hilbert()
	Toeplitz()
	Vandermonde()

	/* mata stata interface */
	st_nvar()
	st_nobs()
	st_data()
	st_sdata()
	st_store()
	st_sstore()
	st_view()
	st_sview()
	st_subview()
	st_viewvars()
	st_viewobs()
	st_varindex()
	st_varname()
	st_varrename()
	st_vartype()
	st_isnumvar()
	st_isstrvar()
	st_varformat()
	st_varlabel()
	st_varvaluelabel()
	st_vlexists()
	st_vldrop()
	st_vlmap()
	st_vlsearch()
	st_vlload()
	st_vlmodify()
	st_tempname()
	st_tempfilename()
	st_tsrevar()
	st_addobs()
	st_addvar()
	st_dropvar()
	st_dropobsin()
	st_dropobsif()
	st_keepvar()
	st_keepobsin()
	st_keepobsif()
	st_update()

	stata()
	st_macroexpand()
	st_global()
	st_local()
	st_numscalar()
	st_strscalar()
	st_matrix()
	st_matrixrowstripe()
	st_matrixcolstripe()
	st_replacematrix()
	st_dir()
	st_rclear()
	st_eclear()
	st_sclear()

	st_isname()
	st_islmname()
	st_isfmt()
	st_isnumfmt()
	st_isstrfmt()

	/* mata statistical */
	uniform()
	uniformseed()

	mean()
	variance()
	quadvariance()
	meanvariance()
	correlation()
	quadcorrelation()
	cross()
	corr() /* already regular function */
	crossdev()
	quadcross()
	quadcrossdev()
	/* /* factorial stuff done already */ */
	/* /* densities and distributions already done */ */

	/* mata string */
	tokens()
	strmatch() /* already regular function */
	strlen() /* already regular function */
	strpos() /* already regular function */
	indexnot() /* already regular function */
	/* /* bunch of all the string functions */ */
	strdup()
	ascii()
	char()

	/* mata utility */
	/* /* complex already done */ */
	rows()
	cols()
	length() /* already regular function */
	eltype()
	orgtype()
	isreal()
	iscomplex()
	isstring()
	ispointer()
	isrealvalues()
	isview()
	issymmetric()
	issymmetriconly()
	isdiagonal()
	diag0cnt() /* already regular function */

	missing() /* already regular function */ 
	rowmissing()
	colmissing()
	nonmissing()
	rownonmissing()
	colnonmissing()
	missingof()

	rowmin()
	colmin()
	min() /* already regular function */
	rowmax()
	colmax()
	max() /* already regular function */
	rowminmax()
	colminmax()
	minmax()
	rowmaxabs()
	colmaxabs()
	rowsum()
	colsum()
	sum() /* already regular function */
	quadrowsum()
	quadcolsum()
	quadsum()

	reldif() /* already regular function */
	mreldif() /* already regular function */
	mreldifsym()
	mreldifre()
	all()
	any()
	allof()
	anyof()
	panelsetup()
	panelstats()
	panelsubmatrix()
	panelsubview()

	mindouble() /* already regular function */ 
	maxdouble() /* already regular function */ 
	smallestdouble()
	epsilon()
	solve_tol()

	/* really getting mad at stata corp now */
	/* wtf is the point of all the extra underscore functions */
	_chdir()
	_mkdir()
	_rmdir()
	_cholesky()
	_cholinv()
	_cholsolve()
	_corr()
	_editmissing()
	_edittoint()
	_edittointtol()
	_edittozero()
	_edittozerotol()
	_editvalue()
	_eigensystem()
	_lefteigensystem()
	_eigenvalues()
	_symeigensystem()
	_symeigenvalues()
	_fft()
	_invfft()
	_fillmissing()
	_fopen()
	_fclose()
	_fget()
	_fgetnl()
	_fread()
	_fput()
	_fwrite()
	_fgetmatrix()
	_fputmatrix()
	_ftell()
	_fseek()
	_ftruncate()

	_fullsvd()
	_svd_la()
	invHilbert()

	_invsym()

	_lowertriangle()
	_uppertriangle()

	_lud()
	_lud_la()
	_luinv()
	_luinv_la()

	_lusolve()
	_lusolve_la()
	_makesymmetric()
	_matexpsym()
	_matlogsym()
	_matpowersym()
	_pinv()

	_hqrd()
	_hqrdp()
	_hqrdp_la()
	_qrinv()
	_qrsolve()

	rangen()
	_solvelower()
	_solveupper()
	_sort()
	_jumble()

	_st_addobs()
	_st_addvar()
	_st_data()
	_st_sdata()
	_st_macroexpand()
	_st_store()
	_st_sstore()
	_st_varindex()
	_stata()
	_svd()
	_svdsv()
	_svsolve()
	_conj()
	_transposeonly()
	_unlink()

	/* now for some things which give trouble... */
	
	/* macros showing up inside other constructions */
	local ding `r(foo)'
	local dong "this is `bramble' and this is `r(foo)'"
	display as text "this is `r(foo)'"
	/* checking long wrapping */
#delimit ;
	reshape this whole thing as this is
	  the start of a long command. ;
#delimit cr
	/* wrapping when comments wipe out the end of a line */
	reshape this whole thing as this is /* foo
	*/ the start of a long command.
	
	here is a command with /* a following comment */
	here is - somethin

	/* now there appears to be no way to have the doubleslash */
	
	here is a comment... // fooey
	
	/* end a line properly, because the newline ends the comment... */
	
	here is something followed by a comment // this is a comment
	and this continues the command /// with an extended comment
	  with, finally, a conclusion /// and a second long cmment
	  hahaha
	// did this end /* this */
	
	here is a partial // this is ok
	hie
	  
	reshape this whole thing /// hahah
	  /// here is a following comment
	  this is the test with the strange intervening line! ///
	  worked just fine
	this is
	this is something new
	this has stuff // this is a comment
	* this is a comment, just one that is not recognized

	/* start here */
	/* macro highlighting problem */

	append using `file`filenum'`fooie'' 
	append using ``file'' 
	this is a `2'
	split this line properly	

	/* should nesting be allowed? */
	/*  start multi line comment
		/* inline comment */
		stuff which does not recognize its commentness
		the end of multi line comment
	*/

	ddd

	/* new stuff from the 8.2 July 23, 2004 upgrade */
	vec foo
	veclmar foo
	vecnorm
	vecrank bar
	vecstable

	

	/* varfcast made obsolete */
	varfcast clear
 	varfcast c
	varfcast compute
	varfcast g
	varfcast graph
	/* replaced by fcast; some subcommands disappeared */
 	fcast c
	fcast com
	fcast compu
	fcast compute
	fcast g
	fcast gra
	fcast graph
	

	/* varirf is obsolete as of July 23, 2004 */
	varirf a
	varirf add
	varirf
	varirf cg
	varirf cgraph
	varirf cr
	varirf create
	varirf ct
	varirf ctable
	varirf d
	varirf describe
	varirf di
	varirf dir
	varirf drop
	varirf erase
	varirf g
	varirf graph
	varirf og
	varirf ograph
	varirf ren
	varirf rename
	varirf set
	varirf t
	varirf table


	veclmar
	vecnorm
	vecstable

	/* comments and conditionals */
	if this {
		then this {
			though this
			}
		}

	/* the acid test for comments --- a legal stata command! */
	display 8*9 + /// comment text
	  14 - 10 *	/* comment mid line
		  /* oops, a nested comment */
		  /* and another
			  which lasts
			  and lasts
		  */
	  which continues */ 1 + /*
	  */ 14 /* on and on */ /2 ///
	  + 5 /*
	  */ +1 
	* this is a comment which has ///
	  strange continuation
	display "hah"

	
	/* this is a test
		
		of how the indents would work
		/* this is much better */
		ahhh, this is great.
	*/ this will screw up, but it is legal in stata
	is odd
	
	oops
	darn /* what will happen here? */
	/* this is a comment */ and here
	
	
	
	/*
		
		what happens now
		this
	
		would be nice to have this indented farther
		this is confusing
		/* Tricky start of new comment
			ahhh, indentation is ok
		*/
		
	
		
	
		back to where it should be
	*/ fooey
	outside the comment.
end
