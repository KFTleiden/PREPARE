#
#	PREPAREmdExp.R
#Tue 12 Apr 2022 01:57:22 PM CEST

source('PREPAREStat.R');
.fn.set(prefix = '../');
Library(c('magrittr', 'knitr', 'metafor', 'reshape2', 'lme4', 'dfoptim'));

#	To run individual scripts the conditions of the if-statements can be chosen TRUE/FALSE

#
#	<p> primary analysis
#

if (0) {
	input = 'PREPARE-primary.Rmd';
	runAndPublishMarkdown(input);
}

#
#	<p> Negative controls
#

if (1) {
	input = 'PREPARE-NegativeControls.Rmd';
	runAndPublishMarkdown(input);
}
