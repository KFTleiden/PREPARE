#
#	PREPAREStat.R
#	Creation: Tue Jan 28 15:32:50 2020
#

source('~/src/Rprivate/RgenericFresh.R'); loadLibraries();
Library(c('knitr', 'magrittr', 'metafor', 'lme4', 'reshape2', 'dfoptim', 'cluster'));
# ToxReview
Library(c('psych', 'dplyr', 'reshape2'));
resultsDate = '2022-11';
.fn.set(prefix = join(c('results', resultsDate, ''), '/'));

varsNum = c('AGE', 'BMI', 'HEIGHT', 'WEIGHT');
readData = function(path = 'data/', prefix = '', varsNumeric = varsNum) {
	#path1 = con(prefix, path);
	path1 = with(splitPath(path), Sprintf('%{qualifierFull}s%{prefix}s%{path}s'));
	d = readTable(path1);
	d2N = Df_(do.call(cbind, lapply(varsNumeric, function(n) {
		as.numeric(levels(d[[n]]))[as.integer(d[[n]])]
	})), names = varsNumeric);
	d[, varsNumeric] = d2N;
	#d = Df_(d, as_numeric = varsNumeric);

	# recode INDDRUG1
	d$INDDRUG1orig = d$INDDRUG1;
	levels(d$INDDRUG1) = c(levels(d$INDDRUG1), 'Fluorouracil cutaneous')
	fluorouracilCut = d$INDDRUG1 == 'Fluorouracil' & (d$ADMROUTE1 %in% c('Topical', 'Transdermal'))
	d$INDDRUG1[ fluorouracilCut ] = 'Fluorouracil cutaneous'

	# create CENTER variable
	d$CENTER = as.factor(substr(d$STUDYCODE, 1, 3));
	
	# reverse reference level
	d$STUDYARMC = recodeLevels(d$STUDYARM, list(`Control arm` = 'Control', `Study arm` = 'Intervention'))
	d
}


imputationPrefx = 'results/2022-04/'
preparePaths = c(list(
	data = '[DATE=GENRESTOPHDATE;RECRDATE]:data/202202/PATIENT_RECORDS.sav',
	dataE = 'data/202202/Events_1.sav',
	dataG = 'data/genotypes/exported_samples_genotypes.rds',
	drug2gene = 'data/Actionable drug-gene interactions-date-6.ods',
	dataN = 'data/202202/Nurse_assessments.sav'
	), listKeyValue(c('pathEQ5D', 'pathNaller', 'pathCOME'),
		paste0(imputationPrefx, c(
			'dataEQ5D.sav',
			'Naller.sav',
			'COME.sav'
	))),
	dataCo = 'data/202202/CoMorbAllergies.sav'
)

resavePrepareData = function(paths = preparePaths, doSave = FALSE) {
	resaveTableRds(paths, doSave = doSave);
}

readEventData = function(eventPath) {
	dataE = readTable(eventPath);

	dataE$evtTime = as.numeric(dataE$ADESTARDAT - dataE$INITDATEDR1)/(86400 * 7);
	minDate = min(dataE$ADESTARDAT, na.rm = T);
	dataE$evtTimeAbs = as.numeric(dataE$ADESTARDAT - minDate)/(86400 * 7);
	dataE$CENTER = as.factor(substr(dataE$STUDYCODE, 1, 3));
	return(dataE);
}

dataAnalysisComputeFollowUp = function(dataN) {
	fu = by(dataN, dataN$STUDYCODE, function(d){ Df(
		STUDYCODE = d$STUDYCODE[1],
		followup = max(as.integer(d$TIMINGASSDR1)),
		ELIGIBLE = d$ELIGIBLE[1],
		WITHDRAWREASON = any(as.integer(d$WITHDRAWREASON) == 1, na.rm = TRUE)
	)});
	dataFu = Df_(do.call(rbind, fu));
	return(dataFu);
}

dataAnalysisMergePrecomputed = function(dataPrim0,
	pathEQ5D = .fn('dataEQ5D', 'sav'),
	pathNaller = .fn('Naller', 'sav'),
	pathCOME = .fn('COME', 'sav')
	) {
	dataPrim1 = dataPrim0;
	if (notE(pathEQ5D)) {
		dataEQ = readTable(pathEQ5D);
		dataPrim1 = merge(dataPrim1, dataEQ, by = 'STUDYCODE', all.x = TRUE);
		dataPrim1$EQ5D[is.na(dataPrim1$EQ5D)] = median(dataPrim1$EQ5D);
	}
	if (notE(pathNaller)) {
		dataNaller = readTable(pathNaller);
		dataPrim1 = merge(dataPrim1, dataNaller, by = 'STUDYCODE', all.x = TRUE);
		dataPrim1$Naller[is.na(dataPrim1$Naller)] = 0;
	}
	if (notE(pathCOME)) {
		dataCOME = readTable(pathCOME);
		dataPrim1 = merge(dataPrim1, dataCOME, by = 'STUDYCODE', all.x = TRUE);
		dataPrim1$Ncome[is.na(dataPrim1$Ncome)] = 0;
	
	}
	return(dataPrim1);
}

#
#	<p> Helpers
#

centerCountry = function(data)SetNames(unique(data[, c('CENTER', 'COUNTRY')]), rnames = NULL)

#
#	<p> Event funnel
#

dataAnalysisRecodeCenters = function(dataPrim2) {
	dataPrim2$COUNTRY1 = dataPrim2$COUNTRY;
	dataPrim3 = dataPrim2;
	dataPrim3$COUNTRY1 = dataPrim3$COUNTRY;

	# recode smallest centers
	dataPrim3$CENTER[dataPrim3$CENTER == '121'] = '114';
	dataPrim3$CENTER[dataPrim3$CENTER == '122'] = '114';
	dataPrim3$CENTER[dataPrim3$CENTER == '123'] = '114';
	dataPrim3$CENTER[dataPrim3$CENTER == '124'] = '114';
	dataPrim3$CENTER[dataPrim3$CENTER == '125'] = '114';
	
	dataPrim3 = droplevels(dataPrim3)
	return(dataPrim3);
}

intersectList = function(l) {
	base = l[[1]]
	if (length(l) == 1) return(base);
	i = lapply(l[-1], function(set) base %in% set);
	inter = if (length(i) > 1) apply(sapply(i, identity), 1, all) else i[[1]];
	return(base[inter]);
}

idsControlsFromPosNeg = function(data, idsPos, idsNeg) {
	idsControl = setdiff(intersectList(c(list(data$STUDYCODE), idsPos)), idsNeg);
	return(idsControl);
}

resultErr = matrix(NA, ncol = 2, nrow = 1, dimnames = list('arm', c('Estimate', 'Std. Error')))
primaryAnalysis = function(dA, f1 = ADESEVGR1 ~ arm + SEX) {
	# stratified regression
	r = by(dA, dA$COUNTRY, function(d) {
		r = try(summary(glm(f1, data = d, family = binomial())))
		return(if (class(r) == 'try-error') resultErr else coefficients(r));
	})
	# extraction of information for meta-analysis
	#cfs = lapply(r, coefficients);
	cfs = r;
	rMetaInp = lapply(cfs, function(r)r['arm', c('Estimate', 'Std. Error')]);
	rMetaInp = lapply(rMetaInp, as.list);	# <A> work around list.kp -> tb changed
	# meta-analysis
	m = rma.uni(
		yi = list.kpu(rMetaInp, 'Estimate'), 
		sei = list.kpu(rMetaInp, 'Std. Error'),
		slab = names(rMetaInp)
	);
	return(m);
}

secondaryAnalysis = function(dA, f1 = ADESEVGR1 ~ arm + SEX) {
	# stratified regression
	r = by(dA, dA$COUNTRY, function(d) {
		r = try(summary(glm(f1, data = d, family = binomial())))
		return(if (class(r) == 'try-error') resultErr else coefficients(r));
	})
	# extraction of information for meta-analysis
	#cfs = lapply(r, coefficients);
	cfs = r;
	rMetaInp = lapply(cfs, function(r)r['arm', c('Estimate', 'Std. Error')]);
	rMetaInp = lapply(rMetaInp, as.list);	# <A> work around list.kp -> tb changed
	# meta-analysis
	m = rma.uni(
		yi = list.kpu(rMetaInp, 'Estimate'), 
		sei = list.kpu(rMetaInp, 'Std. Error'),
		slab = names(rMetaInp)
	);
	return(m);
}

#pathRules = 'data/Actionable drug-gene interactions-date-3.ods';
rulesValueMap = valueMap = list(Gene = list('HLA B*5701 ' = 'HLA-B5701', 'DPD' = 'DPYD'));
rulesHeaderMap = list(
	Gene.of.interest = 'Gene', Actionable.phenotype.s. = 'phenotype',
	Date = 'date', Drug = 'drug'
);

readActionabilityRules = function(pathRules,
	valueMap = rulesValueMap, headerMap = rulesHeaderMap) {
	dataRules0 = readTable(pathRules);
	dataRules1 = Df_(dataRules0, headerMap = headerMap, valueMap = valueMap);
	dataDI = dataRules1;

	# <p> fake, example rule data frame for devlopment
	#dataDI = Df(drug = c('Clopidrogel','Clopidrogel'), gene = c('CYP2C19', 'CYP2C19'), phenotype = c('PM, IM', 'PM'), date = c(NA, '2019-01-01 UTC'));

	# <p> prepare rules data frame
	start = strptime('2010-01-01', '%F', 'UTC');
	dataDI = Df_(dataDI, as_date = 'date', date_format = '%F', date_tz = 'UTC');
	dataDI$date[is.na(dataDI$date)] = start;

	# <N> empty cells are encoded as NA -> empty list, ie. no actionable phenotype
	splSel = function(l, re = '(; ?)')na.omit(Select(splitString(l, re = re), function(e)e != ''));
	drug2gene = by(dataDI, dataDI$drug, function(ddD) {
		#if (any(ddD$drug == 'Voriconazole')) browser();
		by(ddD, ddD$Gene, function(dd) {
			ddO = dd[order(dd$date), , drop = F];
			# split gene name from phenotype
			#phenotype = gsub(Sprintf('%{Gene}s ', as.list(ddO[1, ])), '', ddO$phenotype);
			pts = lapply(ddO$phenotype, splSel);
			if (any(is.na(pts))) stop('Invalid phenotype coding');
			list(date = ddO$date, gene = ddO$Gene, phenotypes = pts)
		});
	});
	return(drug2gene);
}

# r: patient row
# drug2gene: actionability guidelines
actionabilityCheckRules = function(r, drug2gene, doSow = TRUE, drugColumn = 'INDDRUG1')with(as.list(r), {
	DrugName = r[[drugColumn]]
	drug = drug2gene[[DrugName]];
	if (is.null(drug)) return(FALSE);
	actions = lapply(drug, function(d) {
		#if (length(d$date) > 1) browser();
		# which actionability policy is in place?
		Ipt = if (is.na(dateDrugpol)) 1 else max(dateDrugpol >= d$date);
		gene = d$gene[Ipt]
		#print(list(Ipt, gene, gene %in% names(r)))
		if (Ipt == 0 || !(gene %in% names(r))) stop(Sprintf('Unknown gene %{gene}s, or no guideline %{Ipt}d', d));
		ab = r[[gene]] %in% d$phenotypes[[Ipt]];
		if (doSow) if (!ab)
			Sow(non = listKeyValue(DrugName, list(listKeyValue(gene, r[[gene]])))) else
			Sow(act = listKeyValue(DrugName, list(listKeyValue(gene, r[[gene]]))))
		#if (DrugName == 'Capecitabine') { print(r[[gene]]) }
		return(ab);
	})
	return(any(unlist(actions), na.rm = TRUE));
})
actionabilityCheckGenotypeOnly = function(r, drug2gene, doSow = TRUE, drugColumn = 'INDDRUG1')with(as.list(r), {
	DrugName = r[[drugColumn]]
	drug = drug2gene[[DrugName]];
	if (is.null(drug)) return(FALSE);
	actions = lapply(drug, function(d) {
		#if (length(d$date) > 1) browser();
		# which actionability policy is in place?
		Ipt = if (is.na(dateDrugpol)) 1 else max(dateDrugpol >= d$date);
		gene = d$gene[Ipt]
		#print(list(Ipt, gene, gene %in% names(r)))
		if (Ipt == 0 || !(gene %in% names(r))) stop(Sprintf('Unknown gene %{gene}s, or no guideline %{Ipt}d', d));
		ab = r[[gene]] %in% d$phenotypes[[Ipt]];
		if (doSow) if (!ab)
			Sow(non = listKeyValue(DrugName, list(listKeyValue(gene, r[[gene]])))) else
			Sow(act = listKeyValue(DrugName, list(listKeyValue(gene, r[[gene]]))))
		#if (DrugName == 'Capecitabine') { print(r[[gene]]) }
		return(ab);
	})
	return(any(unlist(actions), na.rm = TRUE));
})
computeActionabilityGtOnly = function(dataG, drug2gene) {
	genes = unique(list.kpu(drug2gene, '*$gene'));
	phenotypes = unlist(lapply(drug2gene, function(dr)lapply(dr, function(dg)listKeyValue(dg$gene, dg$phenotypes))));
	actionability = apply(dataG, 1, function(gts)sum(gts[genes] %in% phenotypes));
	return(actionability);
}

computeActionabilityPerGene = function(dataG, drug2gene) {
	genes = unique(list.kpu(drug2gene, '*$gene'));
	phenotypes = unlist(lapply(drug2gene, function(dr)lapply(dr, function(dg)listKeyValue(dg$gene, dg$phenotypes))));
	actT = t(apply(dataG, 1, \(.).[genes] %in% phenotypes));
	actionability = Df_(actT, names = genes);
	return(actionability);
}

pathGenotypes = 'data/genotypes/exported_samples_genotypes.rds';
#pathClinical = '[DATE=GENRESTOPHDATE;RECRDATE]:data/202103/PATIENT_RECORDS.sav';
pathClinical = '[DATE=GENRESTOPHDATE;RECRDATE]:data/202202/PATIENT_RECORDS.sav';
genotypesHm = list(upgx.sample.id = 'STUDYCODE', haplotypename = 'ht', gstandaard.phenotype.name = 'pt');

readGenotypes = function(pathGts = pathGenotypes, pathClin = pathClinical,
	genotypesHeaderMap = genotypesHm) {
	dataGts = Df_(readRDS(pathGts), headerMap = genotypesHeaderMap);
	dataPts = d1 = dcast(dataGts[, c('STUDYCODE', 'gene', 'pt')], STUDYCODE ~ gene);
	data = readData(pathClinical);
	dataG = merge(data, dataPts, by = 'STUDYCODE', all.x = TRUE);
	# at which date to decide drug policy
	dataG$dateDrugpol = as.POSIXct(ifelse(is.na(dataG$GENRESTOPHDATE),
		dataG$RECRDATE, dataG$GENRESTOPHDATE
	), origin = "1970-01-01", tz = "UTC");
	return(dataG);
}

computeActionability = function(dataG, drug2gene, check = actionabilityCheckRules, drugColumns = 'INDDRUG1', simplify = TRUE) {
	#drugs = names(drug2gene);
	#print(table(with(dataG, INDDRUG1 %in% drugs)));
	#genes = list.kpu(drug2gene, 'gene');
	actionability = Reap(sapply(drugColumns, function(drugColumn)
		apply(dataG, 1, check, drug2gene = drug2gene, drugColumn = drugColumn)
	), returnResult = T);
	if (simplify && notE(dim(actionability$result))) {
		actionability$result = apply(actionability$result, 1, any);
	}
	#actionability2 = apply(dataG, 1, actionabilityCheckRules, drug2gene = drug2gene, doSow = FALSE);
	return(actionability);
}

ComputeActionability = function(dataG, drug2gene, check = actionabilityCheckRules, drugColumns = 'INDDRUG1') {
	actionability = computeActionability(dataG, drug2gene, drugColumns = drugColumns, simplify = TRUE);
	idsActionable = dataG$STUDYCODE[actionability$result];
	return(idsActionable);
}

actionTable = function(ab, type = 'non') {
	nonact = sapply(ab[which(names(ab) == type)], unlist)
	nonacttab = Df(gene = names(nonact), nonact)
	tableT = function(..., transpose = TRUE) if (transpose) t(table(...)) else table(...)
	tableSet = function(...)SetNames(Df_(tableT(...)), c('allele', 'Druggene', 'count'))
	bygene = by(nonacttab, nonacttab$gene, tableSet, useNA = 'ifany');
	return(bygene);
}

adrCodingCols = c(
	"ADEBLLYMPHX", "Blood and lymphatic system disorders - Other, specify",
	"ADECARDDISX", "Cardiac disorders - Other, specify",
	"ADEEARLABYRX", "Ear and labyrinth disorders - Other, specify",
	"ADEENDDISX", "Endocrine disorders - Other, specify",
	"ADEEYEX", "Eye disorders - Other, specify",
	"ADEGASTRX", "Gastrointestinal disorders - Other, specify",
	"ADEGENDISX", "General disorders and administration site conditions - Other, specify",
	"ADEHEPAX", "Hepatobiliary disorders - Other, specify",
	"ADEIMMUNEX", "Immune system disorders - Other, specify",
	"ADEINFX", "Infections and infestations - Other, specify",
	"ADEINJPOISPROCX", "Injury, poisoning and procedural complications - Other, specify",
	"ADEINVOTHCATX", "Investigations, other category",
	"ADEINVX", "Investigations - other, specify",
	"ADEMETNUTRX", "Metabolism and nutrition disorders - Other, specify",
	"ADEMUSKCONTISX", "Musculoskeletal and connective tissue disorder - Other, specify",
	"ADENEOPLASMX", "Neoplasms benign, malignant and unspecified (incl cysts and polyps) - Other, specify",
	"ADENERVSYSTDISX", "Nervous system disorders - Other, specify",
	"ADEPREGNX", "Pregnancy, puerperium and perinatal conditions - Other, specify",
	"ADEPSYCHX", "Psychiatric disorders - Other, specify",
	"ADERENENALURINX", "Renal and urinary disorders - Other, specify",
	"ADEREPRBREASTX", "Reproductive system and breast disorders - Other, specify",
	"ADERESPTHMEDIASX", "Respiratory, thoracic and mediastinal disorders - Other, specify",
	"ADESKINX", "Skin and subcutaneous tissue disorders - Other, specify",
	"ADESOCCIRCX", "Social circumstances - Other, specify",
	"ADEVASCX", "Vascular disorders - Other, specify"
);

if (T) {
	# columns containing free text, should correspond to 'Other' coding
	colsAdrT = matrix(adrCodingCols, ncol = 2, byrow = T)[, 1];
	# columns containing codes
	colsAdr = sapply(colsAdrT, function(s)substr(s, 1, nchar(s) - 1));
}

#
#	<p> Utrecht
#


# canonicalize name of phenotypes (tabel Heshu and genotype data)
trimpts = function(s) {
	s = gsub('[: ()/*,-]', '.', s);
	s = sub('(^[.]+)', '', s);
	s = sub('([.]+$)', '', s);
	s = gsub('[.]+', '.', s);
	return(s);
}


harmonyDict = list(
	drug = list(
		Halperidol = 'Haloperidol',
		Clopidrogel = 'Clopidogrel',
		Doxepine = 'Doxepin',
		Fluoruracil = 'Fluorouracil',
		Nortryptiline = 'Nortriptyline'
	),
	phenotype = list(
		X5B.FACTOR.V.LEIDEN.ABSCENT.YES.NO = 'FACTOR.V.LEIDEN.ABSENT',
		X5B.NUDT15.EM.YES.NO = 'NUDT15.EM',
		X5C.NUDT15.IM.YES.NO = 'NUDT15.IM'
	),
	organ = list(),
	sympt = list()
);

codeHarmonize = function(drug, phenotype, organ, sympt, code2ade) {
	drug = trimString(drug);
	phenotype = toupper(trimpts(ifelse(length(phenotype) >= 1, phenotype, 'NA')));
	organ = firstDef(organ, 'NA');
	if (length(sympt) != 1) browser();
	if (length(organ) != 1) browser();
	sympt = ifelse(length(sympt) >= 1, ifelse(organ == 'NA', 'NA', code2ade[[organ]][as.integer(sympt)]), 'NA');

	codeL = list(drug = drug, phenotype = phenotype, organ = organ, sympt = sympt);
	r = nelapply(codeL, function(n, e) {
		if (length(e) == 0 || is.na(e)) NA else firstDef(harmonyDict[[n]][[as.character(e)]], e);
	});
	return(r)
}

# actionability codes
# drug:organ:sympt:phenotype
#	phenotype . for space, uppercase
actionabilityCode = function(drug, phenotype, organ, sympt, code2ade) {
	#print(list(drug, phenotype, organ, sympt));
	#Sprintf('%{drug}s:%{phenotype}s:%{organ}s:%{sympt}s', codeHarmonize(drug, phenotype, organ, sympt))
	Sprintf('%{drug}s:%{phenotype}s:%{sympt}s', codeHarmonize(drug, phenotype, organ, sympt, code2ade))
}
ActionabilityCode = Vectorize(actionabilityCode, c('drug', 'phenotype', 'organ', 'sympt'))

actionabilityCodeForRow = function(drugNm, r, cond, CodeTransL, code2ade) {
	act = names(which(cond));
	if (length(act) == 0) return(NULL);
	organ = CodeTransL[[r['eCRForgan']]]
	acs = actionabilityCode(drugNm, act, organ, r['eCRFade'], code2ade);
}

stdS = function(s)toupper(gsub('( |[^a-zA-Z])', '', s))
is.infix = function(a, b)a == substr(b, 1, nchar(a))
is.infixSymm = function(a, b, minLen = 10) {
	if (min(nchar(a), nchar(b)) < minLen) return(FALSE);
	return(is.infix(a, b) || is.infix(b, a));
}
adeCmp = function(a, b)(stdS(a) == stdS(b)) || is.infixSymm(stdS(a), stdS(b))


collapseCode = function(code, index = 1)apply(codeIndex(code, index), 2, join, sep = ':')
setdiffsymm = function(x, y)union(setdiff(x, y), setdiff(y, x))
codeIndex = function(code, index) sapply(splitString(':', code), identity)[index, , drop = F]
#codeSub1 = unique(list.kpu(splitString(':', code1), Sprintf('[[%{index}d]]')))
checkCodeIndex = function(code1, code2, index = 1) {
	codeSub1 = collapseCode(code1, index)
	codeSub2 = collapseCode(code2, index)
	return(list(
		diff = sort(setdiffsymm(codeSub1, codeSub2)),
		u1 = sort(unique(codeSub1)),
		u2 = sort(unique(codeSub2))));
}

codesCollapse = function(code, idcs = 1:3) {
	apply(sapply(splitString(':', code), identity)[idcs, , drop = F], 2, join, sep = ':')
}

ade2combo = function(dataE, CodeTrans) {
	combos = apply(dataE, 1, \(.)with(as.list(.), {
		adeCol = CodeTrans$Column[which(CodeTrans$Organ == CATEGORY)];
		if (is.na(CATEGORY) || is.na(adeCol)) return('NA:NA');
		Sprintf('%{CATEGORY}s:%{ADE}s', ADE = .[[adeCol]]);
	}));
}

#
#	<p> causality wrapper functions
#

# Drugs: Utrecht input
# dataE: used to extract coding of ADEs
# CodeTrans: translation table
extractInsertCombos = function(Drugs, dataE, CodeTrans) {
	CodeTransL = c(listKeyValue(CodeTrans$organUtr, CodeTrans$eCRForgan), list('0' = NA));
	CodeOrganL = c(listKeyValue(CodeTrans$`Organ system`, CodeTrans$eCRForgan));
	AdeColL = c(listKeyValue(CodeTrans$`Organ system`, CodeTrans$Column));
	organCode2col = listKeyValue(CodeTrans$organUtr, CodeTrans$Column);
	code2ade = nelapply(organCode2col, function(n, e)levels(dataE[[e]]));

	rAct = Reap(nelapply(Drugs, function(drugNm, drug) {
		#print(drugNm)
		#print(names(drug))
		ns = names(drug)[DfColsAfter(drug, 'causalAny')]
		nsPhen = setdiff(ns, ns[grep('^Var', ns)]);
		#print(nsPhen);
		colADEstring = grep.infixes('(?s)^X4B((?!original).)*$', names(drug));
		#print(colADEstring)
		colADEstring = last(colADEstring)
		codes = apply(drug, 1, function(r) {
			# <p> plasubility check
			adeCode = CodeTransL[[r[['eCRForgan']]]];
			if (is.null(adeCode)) {
				print(list(`missing code` = r[['eCRForgan']]));
			} else if (!is.na(adeCode)) {
				lvls = levels(dataE[[organCode2col[[r['eCRForgan']]]]]);
				if (!adeCmp(lvls[as.integer(r[['eCRFade']])], r[[colADEstring]])) {
					print(list(insert = r[[colADEstring]], ade = lvls[as.integer(r[['eCRFade']])]));
				}
			}
			# </p>
			Sow(non = actionabilityCodeForRow(drugNm, r, r[nsPhen] != 'Y', CodeTransL, code2ade));
			actionabilityCodeForRow(drugNm, r, r[nsPhen] == 'Y', CodeTransL, code2ade);
		})
	}), returnResult = T);
	return(rAct);
}

# <N> based on extractInsertCombos
# <N> used for debugging purposes
extractInsertTerms = function(Drugs, dataE, CodeTrans) {
	CodeTransL = c(listKeyValue(CodeTrans$organUtr, CodeTrans$eCRForgan), list('0' = NA));
	organCode2Nm = listKeyValue(CodeTrans$organUtr, CodeTrans$`Organ system`);
	organCode2col = listKeyValue(CodeTrans$organUtr, CodeTrans$Column);
	code2ade = nelapply(organCode2col, function(n, e)levels(dataE[[e]]));

	terms = nelapply(Drugs, function(drugNm, drug) {
		codes = apply(drug, 1, function(r) {
			# <p> plasubility check
			organ = CodeTransL[[r['eCRForgan']]]
			adeCode = CodeTransL[[r[['eCRForgan']]]];
			if (is.null(adeCode)) {
				print(list(`missing code` = r[['eCRForgan']]));
				return(list(organ = NA, ade = NA));
			} else if (!is.na(adeCode)) {
				lvls = levels(dataE[[organCode2col[[r['eCRForgan']]]]]);
				return(c(organ = organCode2Nm[[r['eCRForgan']]], ade = lvls[as.integer(r[['eCRFade']])]))
			}
		})
	});
	ttab = do.call(rbind, lapply(terms, function(t)do.call(rbind, filterList(t, function(e)!is.null(e)))))
	return(ttab);
}


extractPatientCombos = function(dataG, dataE, CodeTrans) {
	CodeTransL = c(listKeyValue(CodeTrans$organUtr, CodeTrans$eCRForgan), list('0' = NA));
	CodeOrganL = c(listKeyValue(CodeTrans$`Organ system`, CodeTrans$eCRForgan));
	AdeColL = c(listKeyValue(CodeTrans$`Organ system`, CodeTrans$Column));
	organCode2col = listKeyValue(CodeTrans$organUtr, CodeTrans$Column);
	code2ade = nelapply(organCode2col, function(n, e)levels(dataE[[e]]));

	# causal actionability
	Igenes = DfColsBetween(dataG, 'CYP2B6', 'VKORC1')
	lvls = lapply(AdeColL, function(e)levels(dataE[[ e ]]))	# same as code2gene

	actCodesData = apply(dataE, 1, function(ev) with(as.list(ev), {
		dataPat = dataG[which(dataG$STUDYCODE == STUDYCODE), ];
		drugNm = dataPat$INDDRUG1;
		#print(STUDYCODE)
		phenotypes = dataPat[Igenes];
		if (is.na(CATEGORY) || is.null(lvls[[CATEGORY]])) {
			print(CATEGORY);
			return(actionabilityCode(drugNm, phenotypes, 'NA', 'NA', code2ade));
		}
		if (is.null(CodeOrganL[[CATEGORY]])) print(CATEGORY);
		if (is.null(AdeColL[[CATEGORY]])) browser();
		ade = which(lvls[[CATEGORY]] == ev[[ AdeColL[[CATEGORY]] ]]);
		if (length(ade) == 0) return(actionabilityCode(drugNm, phenotypes, CodeOrganL[[CATEGORY]], 'NA', code2ade));
		if (length(phenotypes) == 0) browser();
		code = ActionabilityCode(drugNm, phenotypes, CodeOrganL[[CATEGORY]], ade, code2ade);
		#print(code);
		return(code);
	}))
	return(setNames(actCodesData, dataE$RECORD_));
}

#
#	<p> Event funnel
#

# length unique cases, helper for table generation
luc = function(data)length(unique(data$STUDYCODE))

# Eligibiligy
eventFunnelEligibility = eventFunnelStep0 = function(dataE) {
	# as of 14.7.2022: ITT population is decided to be the full database
	return(dataE)

# 	# withdraw columns both in data, dataE 1.3.2022
# 	inclusion = with(dataE,
# 		ELIGIBLE == 'Yes' &
# 		!(
# 			nif(as.integer(WITHDRAWREASON) == 1)
# 			# removed as of 17.3.2022 due to redundancy with later checks
# 			#| nif(as.integer(WITHDRAWREASON) == 3)
# 		)
# 	)
# 	dataE_0 = dataE[inclusion, ]
# 	return(dataE_0);
}

eventFunnelFollowup = eventFunnelStep1 = function(dataE_0, timeHorizonDays = 14 * 7, dateColumns = 'INITDATEDR1', checkInit = FALSE) {
	spd = 86400	# seconds per day
	# tolerance of 2 weeks -> threshold 14 (instead of 12)
	# matrix cannot hold dates: apply -> lapply <N>
	#dates = apply(dataE_0[, dateColumns, drop = F], 2, function(initdateadr) {
	dates = lapply(dateColumns, function(dateCol) with(list(initdateadr = dataE_0[[ dateCol ]]), {
		initCheck = if (checkInit) dataE_0$ADESTARDAT >= initdateadr else TRUE;
		nif(initCheck & dataE_0$ADESTARDAT <= initdateadr + timeHorizonDays * spd)
	}));
	date = if (length(dates) > 1) apply(sapply(dates, identity), 1, any) else dates[[1]];
	dataE_1 = dataE_0[date, ]
	return(dataE_1);
}


eventFunnelActionability = eventFunnelStep2 = function(dataE_1, data, dataG, drug2gene, drugColumns = 'INDDRUG1') {
	#actionability = computeActionability(dataG, drug2gene);
	# actionable patients' STUDYCODE
	#idsPatAct = dataG$STUDYCODE[actionability$result]
	idsPatAct = ComputeActionability(dataG, drug2gene, drugColumns = drugColumns);
	dataE_2 = dataE_1[ dataE_1$STUDYCODE %in% idsPatAct, ]
	return(dataE_2);
}

advLvlsADEBLLYMPH = qw('Anemia "Bone marrow hypocellular" "Disseminated intravascular coagulation" "Febrile neutropenia" Hemolysis "Hemolytic uremic syndrome" Leukocytosis "Thrombotic thrombocytopenic purpura"');
advLvlsADEINV = qw('"Activated partial thromboplastin time prolonged" "CD4 lymphocytes decreased" "Hemoglobin increased" "INR increased" "Lymphocyte count decreased" "Lymphocyte count increased" "Neutrophil count decreased" "Platelet count decreased" "White blood cell decreased"')

eventFunnelSeverity = eventFunnelStep3 = function(dataE_2) {

	advHem1 = dataE_2$ADEBLLYMPH %in% advLvlsADEBLLYMPH;
	advHem2 = dataE_2$ADEINV %in% advLvlsADEINV;
	advHem = advHem1 | advHem2;

	severityI = as.integer(dataE_2$ADESEVGR1)
	toxHematological = nif(advHem & severityI >= 4)

	# Identify non-hematological, oncological toxicity
	oncoDrugs = c('Capecitabine', 'Fluorouracil', 'Tegafur', 'Irinotecan');
	toxOnco = nif(dataE_2$INDDRUG1 %in% oncoDrugs)

	dataE_3 = dataE_2[nif(
		  (toxOnco & advHem   & severityI >= 4)
		| (toxOnco & !advHem  & severityI >= 3)
		| (          !toxOnco & severityI >= 2)
	), ]
	return(dataE_3);
}

eventFunnelCausality = eventFunnelStep4 = function(dataE_3) {
	isLiverpoolCausal = as.integer(dataE_3$CAUSSCORE1) <= 3
	dataE_4 = dataE_3[isLiverpoolCausal, ]
	return(dataE_4);
}


eventFunnelPhenGen = eventFunnelStep5 = function(dataE_4, dataG, Drugs, CodeTrans) {
	rAct = extractInsertCombos(Drugs, dataE_4, CodeTrans)
	actCodes = avu(unlist(rAct$result));
	nonActCodes = avu(unlist(rAct$yield));

	# Phenotyping of patient DNA is combined with ADE data to extract potentially causal combos.
	adePat = extractPatientCombos(dataG, dataE_4, CodeTrans)

	# Determine causal ADEs.
	eventCausal = unlist(lapply(adePat, function(combos)any(combos %in% actCodes)))
	if (any(na.omit(names(eventCausal) != dataE_4$RECORD_))) stop('misaligned features');
	dataE_5 = dataE_4[eventCausal, ]

	return(dataE_5);
}

mostSevereByPatient = eventFunnelByPatient = eventFunnelStep6 = function(dataE_5) {
	records = by(dataE_5, dataE_5$STUDYCODE, function(evs) {
		record = evs$RECORD_[which.max(evs$ADESEVGR1)] }
	);
	dataE_6 = dataE_5[ dataE_5$RECORD_ %in% records, ]
	return(dataE_6);
}

eventFunnelGenPhen = function(dataE, data, dataG, drug2gene, Drugs, CodeTrans, timeHorizonDays = 14 * 7, dateColumns = 'INITDATEDR1', checkInit = FALSE) {
	dataE_0 = eventFunnelEligibility(dataE)	# eligibility
	dataE_1 = eventFunnelFollowup(dataE_0, timeHorizonDays = timeHorizonDays, dateColumns = dateColumns, checkInit = checkInit)
	dataE_2 = eventFunnelSeverity(dataE_1)	# severity
	dataE_3 = eventFunnelCausality(dataE_2)	# causality
	dataE_4 = eventFunnelPhenGen(dataE_3, dataG, Drugs, CodeTrans);	# drug-gene association
	dataE_5 = eventFunnelActionability(dataE_4, data, dataG, drug2gene)	# actionability
	dataE_6 = eventFunnelByPatient(dataE_5);	# most severe per patient
	return(dataE_6);
}

# does not include actionability filtering
eventFunnelFull = eventFunnelGate2 = eventFunnelFullA = function(dataE, data, dataG, drug2gene, Drugs, CodeTrans, timeHorizonDays = 14 * 7, dateColumns = 'INITDATEDR1', checkInit = FALSE) {
	dataE_0 = eventFunnelEligibility(dataE)
	dataE_1 = eventFunnelFollowup(dataE_0, timeHorizonDays = timeHorizonDays, dateColumns = dateColumns, checkInit = checkInit);	# followup
	dataE_2 = eventFunnelSeverity(dataE_1)
	dataE_3 = eventFunnelCausality(dataE_2)
	dataE_4 = eventFunnelByPatient(dataE_3);
	return(dataE_4);
}

eventsMostSevere = function(dataE) {
	#
	#	<p> recode severity
	#
	advHem1 = dataE$ADEBLLYMPH %in% advLvlsADEBLLYMPH;
	advHem2 = dataE$ADEINV %in% advLvlsADEINV;
	advHem = advHem1 | advHem2;

	severityI = as.integer(dataE$ADESEVGR1)

	# Identify non-hematological, oncological toxicity
	oncoDrugs = c('Capecitabine', 'Fluorouracil', 'Tegafur', 'Irinotecan');
	toxOnco = nif(dataE$INDDRUG1 %in% oncoDrugs)

	dataE$ADESEVGR1N = ifelse( (toxOnco & advHem   & severityI >= 4),
		severityI - 2, ifelse( (toxOnco & !advHem  & severityI >= 3),
		severityI - 1,
		severityI
	))

	cols = c('STUDYCODE', 'STUDYARM', 'RECORD_', 'ADESEVGR1', 'ADESEVGR1N', 'ADESTARDAT', 'INITDATEDR1', 'CAUSSCORE1');
	rs = by(dataE, dataE$STUDYCODE, function(evs) {
		evs[which.max(evs$ADESEVGR1), cols, drop = F]
	});
	return(do.call(rbind, rs));
}

evtsUseCols = c('STUDYCODE', 'STUDYARM', 'ADESEVGR1N')
eventsTable = function(data, dataE, idsEv, idsNonEv, project = evtsUseCols) {
	dataEev = dataE[ dataE$STUDYCODE %in% idsEv , ]
	dataEevP = eventsMostSevere(dataEev);
	dataEne = merge(Df(STUDYCODE = idsNonEv, ADESEVGR1N = 0), data[, c('STUDYCODE', 'STUDYARM')], by = 'STUDYCODE')
	allIds = c(idsEv, idsNonEv);

	return(rbind(dataEevP[, evtsUseCols, drop = F], dataEne));
}

#
#	<p> secondary analyses
#

eventFunnelSecondaryFull = function(dataE, data, dataG, drug2gene, Drugs, CodeTrans, timeHorizonDays = 14 * 7, dateColumns = 'INITDATEDR1', checkInit = FALSE) {
	dataE_0 = eventFunnelEligibility(dataE)	# eligibility
	dataE_1 = eventFunnelFollowup(dataE_0, timeHorizonDays = timeHorizonDays, dateColumns = dateColumns, checkInit = checkInit);	# followup
	dataE_2 = eventFunnelSeverity(dataE_1);	# severity
	dataE_3 = eventFunnelByPatient(dataE_2)	# most severe per patient
	return(dataE_3);
}

# do not filter for severity
eventFunnelSecondaryAllEvents = function(dataE, data, dataG, drug2gene, Drugs, CodeTrans, timeHorizonDays = 14 * 7, dateColumns = 'INITDATEDR1', checkInit = FALSE) {
	dataE_0 = eventFunnelEligibility(dataE);	# eligibility
	dataE_1 = eventFunnelFollowup(dataE_0, timeHorizonDays = timeHorizonDays, dateColumns = dateColumns, checkInit = checkInit);	# followup
	dataE_2 = eventFunnelCausality(dataE_1);	# causaility
	dataE_3 = eventFunnelByPatient(dataE_2);	# most severe per patient
	return(dataE_3);
}

# do not filter for severity

eventFunnelSecondaryAllEventsNoCausByEvent = function(dataE, data, dataG, drug2gene, Drugs, CodeTrans, timeHorizonDays = 14 * 7, dateColumns = 'INITDATEDR1', checkInit = FALSE) {
	dataE_0 = eventFunnelEligibility(dataE);	# eligibility
	dataE_1 = eventFunnelFollowup(dataE_0, timeHorizonDays = timeHorizonDays, dateColumns = dateColumns, checkInit = checkInit);	# followup
	return(dataE_1);
}

eventFunnelSecondaryAllEventsNoCaus = function(dataE, data, dataG, drug2gene, Drugs, CodeTrans, timeHorizonDays = 14 * 7, dateColumns = 'INITDATEDR1', checkInit = FALSE) {
	dataE_1 = eventFunnelSecondaryAllEventsNoCausByEvent(dataE, data, dataG, drug2gene, Drugs, CodeTrans, timeHorizonDays, dateColumns, checkInit);
	dataE_2 = eventFunnelByPatient(dataE_1);	# most severe per patient
	return(dataE_2);
}


#
#	<p> global health score
#

listFromPairs = function(prs) {
	kvm = matrix(prs, ncol = 2, byrow = T);
	return(listKeyValue(kvm[, 1], kvm[, 2]));
}

# global1..global10
#global = toupper(qw('Genhealth Genqol Genphyshealth Genmenthealth Gensatsoc Evdayphysact Painaver Fatigueaver Gensocact Past7daysemprobl'));
ghsMap = listFromPairs(toupper(qw('Global01 Genhealth Global02 Genqol Global03 Genphyshealth Global04 Genmenthealth Global05 Gensatsoc Global06 Evdayphysact Global07 Painaver Global08 Fatigueaver Global09 Gensocact Global10 Past7daysemprobl')));
ghsMapI = listInverse(ghsMap);

# use nurse assessment data
computeGlobalHealthScore = function(dataN) {
	# NA check: no NA's in data 12.10.2021
	for (v in ghsMap) {
		#print(c(v, ghsMapI[[v]]));
		#print(table(dataN[[v]]));
		dataN[[ v ]] = droplevels( dataN[[ v ]] )
		print(summary(dataN[[v]]));
		#dataN[[v]][ dataN[[v]] == 'Unknown' ] = NA;
	}
	# <p> rename columns, recode to numeric
	dataN1 = Df_(dataN, headerMap = ghsMapI, as_numeric = names(ghsMap))
	# <p> data recoding
	# Global08, Global10 already coded according to PROMIS Scale 1.2
	
	#dataN1$GLOBAL10R = with(dataN1, 6 - as.integer(GLOBAL10));
	# recoe Global07: PAINAVER
	# range: (1, 10) -> rescale to (0, 10) -> remap to (5, 1)
	dataN1$GLOBAL07 = with(dataN, 5 - (as.numeric(PAINAVER) - 1) * 10/9 * 4/10);
	#print(sapply(1:10, function(p)(5 - (p - 1) * 10/9 * 4/10)));
	# slight recoding mismatch to table 2, promis guide
	# -> do not discretize (12.10.2021 SB)
	# Appendix 3, promis manual
	EQ5D = with(dataN1, 0.19123 + (0.00672 * GLOBAL02) + (0.00527 * GLOBAL03) +	(0.00830 * GLOBAL04) + (0.04550 * GLOBAL06) + (0.02713 * GLOBAL07) + (0.01305 * GLOBAL08) + (0.00613 * GLOBAL09) + (0.02502 * GLOBAL10));
	return(EQ5D);
}

imputeGlobalHealthScore = function(data, dataN, output = .fn('dataEQ5D', 'sav'), Nimp = 30,
	reduceToBaseline = TRUE) {
	Library('mice')
	#data = readData();
	# number of comedications missing
	#dataImp = merge(dataN, data[, c('STUDYCODE', 'AGE')], by = 'STUDYCODE');
	dataImp0 = dataN;	# age already present in dataN

	vars = c(avu(ghsMap), c('SEX', 'AGE', 'COUNTRY', 'CENTER'));
	dataImp = Df_(dataImp0[, vars], as_numeric = 'AGE')
	dataImputed = mice(dataImp, m = Nimp);

	# <p> create data sets to export
	dataI = lapply(1:dataImputed$m, function(i) {
		# <p> get complete data
		d = complete(dataImputed, action = i);
		EQ5D = computeGlobalHealthScore(d);
		Df(imputation = i, EQ5D = EQ5D)
	});

	# <p> compute single imputation
	ghsMedian = apply(sapply(dataI, function(d)d$EQ5D), 1, median);

	dataNEQ5D = Df(dataN, EQ5D = ghsMedian);
	if (reduceToBaseline) {
		# <N> assume stable calculation of EQ5D
		dataEQ5Dby = by(dataNEQ5D, dataN$STUDYCODE, function(d) {
			return(d[which.min(d$COLDATE), c('STUDYCODE', 'EQ5D'), drop = F])
		});
		dataNEQ5D = dataEQ5Dbase = do.call(rbind, dataEQ5Dby);
	}
	if (notE(output)) writeTable(dataNEQ5D, output);
	return(dataNEQ5D);
}

#
#	<p> create data set
#

dataForCasesControls = function(data, idsCases, idsControls, outcome = 'outcomePrim', seed = 5528891,
	pathEQ5D = .fn('dataEQ5D', 'sav'), pathNaller = .fn('Naller', 'sav'), pathCOME = .fn('COME', 'sav')) {
	idsIncluded = c(idsCases, idsControls)
	dataPrim0 = data[ data$STUDYCODE %in% idsIncluded, ];
	# add outcome
	dataPrim0[[outcome]] = as.integer(dataPrim0$STUDYCODE %in% idsCases);

	#Merge pre-computed variables: global health score, number allergies, number co-medications.
	dataPrim1 = dataAnalysisMergePrecomputed(dataPrim0,
		pathEQ5D = pathEQ5D, pathNaller = pathNaller, pathCOME = pathCOME);

	#Add arm column in preparation of primary analysis.

	# <p> fake arm column
	dataPrim2 = Df(dataPrim1, STUDYARMfake = fixSeedForExpr(seed, rbinom(nrow(dataPrim1), 1, .5)));

	if (nrow(dataPrim0) != nrow(dataPrim2) && FALSE) {
		stop('Missing data in merge');
	}
	dataPrim3 = dataAnalysisRecodeCenters(dataPrim2);
	return(dataPrim3);
}

#
#	<p> descriptives
#

drugByFactor = function(tabCDW, catName = 'CENTER') {
	tabCD = apply(tabCDW, 1, function(cr, Ndrugs = 3) {
		counts = as.integer(cr);
		O = order(counts, decreasing = T)[1:Ndrugs];
		drCounts = vector.intercalate(c(dimnames(tabCDW)[[2]][O], 'Other'), c(counts[O], sum(counts[-O])))
		return(drCounts)
	}, simplify = FALSE);
	return(Df(CAT = dimnames(tabCDW)[[1]], do.call(rbind, tabCD), headerMap = list(CAT = catName)));
}

sampleTable = function(Data, outcome = 'outcomePrim', arm = 'STUDYARMC') {
	SampleSize =  c(nrow(Data), NA)
	Outcome = table(Data[[ outcome ]])
	OutcomeByArm = t(table(Data[, c(outcome, arm)]));
	OutcomeByArmFreq = apply(t(table.freq(Data[, c(outcome, arm)])), 1:2, sprintf, fmt = '%.2f')
	sampleTable = rbind(SampleSize, Outcome, OutcomeByArm, OutcomeByArmFreq);
	return(sampleTable);
}

caseMix = function(Data) {
	centerMap = unique(Data[, c('CENTER', 'COUNTRY')])

	tableCD = drugByFactor(table(Data[, c('CENTER', 'INDDRUG1')]));

	tableCDA0 = table(Data[, c('CENTER', 'INDDRUG1', 'STUDYARM')]);
	tableCDA1 = do.call(cbind, apply( tableCDA0, 3, drugByFactor, simplify = FALSE))
	names(tableCDA1) = gsub('Control arm', 'C', gsub('Study arm', 'S', names(tableCDA1)))
	tableCDA2 = Df_(tableCDA1, min_ = 'C.CENTER', headerMap = list(S.CENTER = 'CENTER'));
	tableCDA = merge(centerMap, tableCDA2);

	tableDbyCL = table(Data[, c('COUNTRY', 'INDDRUG1', 'STUDYARM')]);
	tableDbyC0 = do.call(cbind, apply( tableDbyCL, 3, drugByFactor, catName = 'COUNTRY', simplify = FALSE))
	names(tableDbyC0) = gsub('Control arm', 'C', gsub('Study arm', 'S', names(tableDbyC0)))
	tableDbyC1 = Df_(tableDbyC0, min_ = 'C.COUNTRY', headerMap = list(S.COUNTRY = 'COUNTRY'), row.names = NULL);

	r = list(DbyCe = tableCD, DbyCeA = tableCDA[order(tableCDA$COUNTRY), ], DbyCo = tableDbyC1);
	return(r);
}

descriptiveTables = function(dataN, column, output = NULL, formats = c('csv', 'xls')) {
	tableRaw = table(dataN[, column ])
	if (notE(output)) writeTable(tableRaw, path = .fn(Sprintf('%{output}s-counts'), formats));
	cat(Kable(Df(tableRaw)));
	cat("\n\n");

	tableFreq = table.freq(dataN[, column ])
	if (notE(output)) writeTable(tableFreq, path = .fn(Sprintf('%{output}s-freqs'), formats));
	cat(Kable(Df(tableFreq)))
	cat("\n\n");

	if (!('COUNTRY' %in% names(dataN))) return();

	tableByC = table(dataN[, c(column, 'COUNTRY')])
	if (notE(output)) writeTable(tableByC, path = .fn(Sprintf('%{output}s-countsByCountry'), formats));
	cat(Kable(Df(tableByC)))
	cat("\n\n");

	tableFreqByC = table.freq(dataN[, c(column, 'COUNTRY')])
	if (notE(output)) writeTable(tableFreqByC, path = .fn(Sprintf('%{output}s-freqsByCountry'), formats));
	cat(Kable(Df(tableFreqByC)))
	cat("\n\n");
}

#
#	<p> markdown
#

runAndPublishMarkdown = function(input, outputDir = .fn('md'), unfold = F) {
	markdownRenderIn(input, outputDir,
		c('PREPAREStat.R', 'RgenericAllRaw.R', 'data', 'results',
		'results/2022-04/dataEQ5D.sav', 'results/2022-04/Naller.sav', 'results/2022-04/COME.sav'), unfold = unfold,
		output_format = c('html_document')
	);
	.fn.set(prefix = join(c('results', resultsDate, ''), '/'));

	initPublishing('PREPAREStat', resultsDate);
	r = publishFile(con(outputDir, '/', splitPath(input)$base, '.html'));
	gc();
	return(r);
}

#
#	<p> case mix correction
#

soften = function(x, range = 1)((plogis(x/range) - .5) * range * 4)

drugEffects = function(f1 = outcome ~ AGE + EQ5D + STUDYARMC + Naller + Ncome + COUNTRY, data, effectVar = '(Intercept)') {
	ctrl = glmerControl(optimizer = 'bobyqa', boundary.tol = 0, tol = 1e-3, optCtrl = list(maxfun = 1e5));

	r = by(data, data$INDDRUG1, function(d) {
		#print(dim(d));
		N = dim(d)[1];
		rEff = try(glm(f1, data = d, family = binomial()), silent = TRUE);
		if (any(class(rEff) == 'try-error')) return(c(N, NA));
		#if (all(d$INDDRUG1 == 'Haloperidol')) browser();
		#if ((class(rEff) == 'try-error')) return(c(N, NA));
		Cs = coefficients(summary(rEff));
		return(c(N, if ('STUDYARMCIntervention' %in% row.names(Cs)) Cs[effectVar, 'Estimate'] else NA));
	});
	drugEff = do.call(rbind, r);
	drugEff[is.na(drugEff[, 2]), 2] = 0;
	off = drugEff[which.indeces(data$INDDRUG1, row.names(drugEff), ret.na = TRUE), 2];
	off[is.na(off)] = 0
	return(list(drugEffects = drugEff, offset = off));
}


clusterDrugEffects = function(drugEff, data, centers = 5, recode = list(), nstart = 50) {
	km = kmeans(drugEff[, 2, drop = F], centers = centers, nstart = nstart)
	#print(table(km$cluster[dataG2Prim$INDDRUG1]))

	clusters = SetNames(inverseOrder(km$centers[, 1])[ km$cluster ], names(km$cluster));
	# cluster recoding: guarantee minimum cluster size, two-step approach
	for (ri in seq_along(recode)) {
		clusters[clusters == as.integer(names(recode[ri]))] = recode[[ri]]
	}

	DrugCluster = as.factor(clusters[data$INDDRUG1]);
	print(table(data$DrugCluster))
	return(list(clusters = clusters, DrugCluster = DrugCluster));
}


prepareDrugVars = c('INDDRUG1', 'INDDRUG2', 'INDDRUG3', 'INDDRUG4', 'INDDRUG5');
computeDrugColumns = function(data, drugVars = prepareDrugVars, NdrugCutoff = 100) {
	drugsAll = Union(lapply(drugVars, function(v)levels(data[[v]])), as.list = T);
	drugsN = length(drugsAll);
	drugIs = do.call(rbind, apply(data, 1, function(r) {
		I = avu(lapply(drugVars, function(dv)which(r[[dv]] == drugsAll)));
	}, simplify = F));
	drugInd = Df_(do.call(rbind,
		apply(drugIs, 1, function(r)vector.assign(0, r, 1, N = drugsN), simplify = F))
	, names = drugsAll);
	drugN = lapply(drugInd, sum);
	drugIndPruned = drugInd[, drugN >= NdrugCutoff];
	sum(drugN < NdrugCutoff)
	dataDrugs = cbind(data, drugIndPruned);
	return(list(data = dataDrugs, drugs = names(drugIndPruned)));
}

computePeriod = function(data, crossOverTime = 153e7) {
	periods = by(data, data$CENTER, function(d) {
		dates = by(d, d$STUDYARM, function(d0)min(d0$RECRDATE, na.rm = T));
		#print(dates);
		studyFirst = if (any(is.na(dates))) {
			if (!is.na(dates[1]) && dates[1] < crossOverTime) TRUE else
			if (!is.na(dates[1]) && dates[1] >= crossOverTime) FALSE else
			if (!is.na(dates[2]) && dates[2] < crossOverTime) FALSE else NA;
		} else dates[1] < dates[2];
		d0 = Df(STUDYCODE = d$STUDYCODE,
			period = ifelse (d$STUDYARM == 'Study arm', studyFirst, !studyFirst));
		return(d0);
	});
	dPeriod = do.call(rbind, periods);
	dataP = merge(data, dPeriod, by = 'STUDYCODE');
	return(dataP);
}

coefficientsTableRaw = function(r, level = .95, doExp = FALSE) {
	s = summary(r);
	cfs = coefficients(s);
	cis = do.call(cbind, ciFromSummary(s, level = level));
	cfsTab0 = Df_(cbind(cfs, cis), min_ = c('effect', 'Std..Error', 'z.value', 't.value'), headerMap = list(Pr...z.. = 'P'))
	cfsTab = cfsTab0[, intersect(c('Estimate', 'lower', 'upper', 'P'), names(cfsTab0))];
	if (doExp) cfsTab[, c('Estimate', 'lower', 'upper')] = exp(cfsTab[, c('Estimate', 'lower', 'upper')]);
	return(cfsTab);
}


coefficientsTable = function(r, output = NULL, level = .95, doExp = FALSE, formats = c('csv', 'xls')) {
	s = summary(r);
	cfs = coefficients(s);
	cis = do.call(cbind, ciFromSummary(s, level = level));
	cfsTab = Df_(cbind(cfs, cis), min_ = c('effect', 'Std..Error', 'z.value'), headerMap = list(Pr...z.. = 'P'))[, c('Estimate', 'lower', 'upper', 'P')];
	if (doExp) cfsTab[, c('Estimate', 'lower', 'upper')] = exp(cfsTab[, c('Estimate', 'lower', 'upper')]);
	#print(.fn(output, formats));
	if (notE(output)) writeTable(cfsTab, path = .fn(output, formats));
	return(cfsTab);
}

lmerTable = function(r, output = NULL, level = .95, doExp = FALSE, formats = c('csv', 'xls')) {
	cfs = coefficientsTableRaw(r)
	Ps = lmer2p(r, 'Wald')
	cfsTab = MergeByRowNames(cfs, Df_(Ps, names = 'P'))
	if (notE(output)) writeTable(cfsTab, path = .fn(output, formats));
	return(cfsTab);
}

QoLvalueMap = list(arm = list(`Study arm` = 'Intervention', `Control arm` = 'Control'));
QoLheaderMap = list(STUDYARM = 'arm');
QoLcolorMap =  c(Intervention = "#E69F00", Control = "#56B4E9", `Study arm` = "#F0E442", `Control arm` = "#0072B2")
Mean = function(.)mean(., na.rm = TRUE)
Se = function(.)sd(., na.rm = TRUE)/sqrt(length(na.omit(.)))
MeanCi = function(., alpha = .05) {
	m = Mean(.)
	se = Se(.)
	c(m, m - qnorm(1 - alpha/2) * se, m + qnorm(1 - alpha/2) * se)
}
Ci = function(., alpha = .05)MeanCi(., alpha)[-1]
Get = function(x, ...)lapply(x, get, ...)
QoLspaghettiPlot = function(data, output = NULL, outcome, time = 'TIMINGASSDR1', group = 'STUDYARM', id = 'STUDYCODE', headerMap = QoLheaderMap, valueMap = QoLvalueMap, colorMap = QoLcolorMap, ylab = 'Self-rated Health') {
	Library('dplyr');
	d = na.omit(data[, c(outcome, time, group, id)]);
	dSummary = d %>% group_by_(time, group) %>% summarise(outcome = Mean(get(outcome)));
	dSummary = Df_(dSummary, headerMap = c(headerMap, list(outcome = outcome)), valueMap = valueMap);
	pSpaghetti = ggplot(d,
		aes_string(x = time, y = outcome, color = group)) +
		geom_line(aes_string(group = id), show.legend = FALSE) +
		geom_line(data = dSummary, aes_string(color = headerMap[[group]], group = headerMap[[group]]), lwd = 2) +
		scale_color_manual(values = colorMap) +

		xlab('Visit') + ylab(ylab) +
		theme(legend.position = 'none') + theme_bw();
	if (notE(output)) plot_save(pSpaghetti, plot_path = output, width = 20, height = 15);
	return(pSpaghetti);
}

QoLspaghettiPlotMean = function(data, output = NULL, outcome, time = 'TIMINGASSDR1', group = 'STUDYARM', headerMap = QoLheaderMap, valueMap = QoLvalueMap, colorMap = QoLcolorMap, ylab = 'Self-rated Health', ylim = c(0, 100)) {
	d = Df_(na.omit(data[, c(outcome, time, group)]), as_numeric = outcome);
	dSummary = by(d, list(d[[time]], d[[group]]), \(.)MeanCi(.[[outcome]]));
	myNumerics = c(outcome, paste0(outcome, c('L', 'U')));
	dSummary = Df_(by2df(dSummary), names = c(time, group, myNumerics));
	dSummary = Df_(dSummary, headerMap = c(headerMap, list(outcome = outcome)), valueMap = valueMap);
	dSummary = Df_(dSummary, as_numeric = myNumerics, as_factor = c(time, headerMap[[group]]));
	myGroup = headerMap[[group]]
	myColor = 'black';
	# reset levels
	dSummary[[time]] = recodeLevels(dSummary[[time]], listKeyValue(levels(data[[time]]), levels(data[[time]])))
	pSpaghetti = ggplot(dSummary,
		aes_string(x = time, y = outcome, color = myGroup, group = myGroup)) +
		geom_line(lwd = 1.5) + scale_color_manual(values = colorMap[1:2]) +
		geom_errorbar(aes_string(ymin = myNumerics[2], ymax = myNumerics[3]), width = .2) +
		#geom_errorbar(aes_string(ymin = myNumerics[2], ymax = myNumerics[3]), aes(colour = 'black'), width = .1, position = position_dodge(0.1)) +
		xlab('Visit') + ylab(ylab) +
		coord_cartesian(ylim = ylim) + 
		theme(legend.position = 'none') + theme_bw();
	if (notE(output)) plot_save(pSpaghetti, plot_path = output, width = 20, height = 15);
	return(pSpaghetti);
}


#
#	Figures
#

bernoulliCI = function(d, outcome = 'outcomePrim', alpha = 0.05) {
	out = c(d[[ outcome ]]);
	p = mean(out);
	N = length(out);
	q = qnorm(1 - alpha/2)
	c(p, q * sqrt(p * (1-p) / N), N, sum(out))
}

# add one observation to each group
# pseudo: c(1) or  c(0, 1)
bernoulliCIpseudo = function(d, outcome = 'outcomePrim', alpha = 0.05, pseudo = c(1)) {
	out = c(d[[ outcome ]], pseudo);
	p = mean(out);
	N = length(out);
	q = qnorm(1 - alpha/2)
	c(p, q * sqrt(p * (1-p) / N), N - length(pseudo), sum(out) - sum(pseudo))
}


plotEventRate = function(dPlot, strat = NULL, output = NULL) {
	p1 = if (notE(strat))
		ggplot(dPlot, aes_string(x = 'Arm', y = 'Events', fill = strat)) else
		ggplot(dPlot, aes(x = Arm, y = Events));
	p1 = p1 +
		geom_bar(stat = "identity", position=position_dodge()) +
		geom_errorbar(aes(ymin = Events - CI, ymax = Events + CI, width = .2),
			position = position_dodge(.9)) +
		theme(axis.text.x = element_text(angle = 45, vjust = 1,  hjust = 1)) +
		scale_fill_manual(values = c('blue', 'yellow'))
	if (notE(output)) plot_save(p1, width = 15, height = 15, plot_path = paste0(output, c('.pdf', '.jpeg')));
	return(p1)
}

plotEventRateStrat = function(dPlot, strat = NULL, output = NULL) {
	p1 = if (notE(strat))
		ggplot(dPlot, aes_string(x = strat, y = 'Events', fill = 'Arm')) else
		ggplot(dPlot, aes(x = Arm, y = Events));
	p1 = p1 +
		geom_bar(stat = "identity", position=position_dodge()) +
		geom_errorbar(aes(ymin = Events - CI, ymax = Events + CI, width = .2),
			position = position_dodge(.9)) +
		theme(axis.text.x = element_text(angle = 45, vjust = 1,  hjust = 1)) +
		scale_fill_manual(values = c('blue', 'yellow')) +
		labs(y = 'Event Rate')
	if (notE(output)) plot_save(p1, width = 15, height = 15, plot_path = paste0(output, c('.pdf', '.jpeg')));
	return(p1)
}
