#
#	Rmeta.R
#Wed Jun  3 15:11:27 CEST 2015
#Mon 27 Jun 2005 10:49:06 AM CEST
#system("~/src/Rprivate/exportR.sh");
#system("~/src/Rprivate/exportR.sh"); source("RgenericAllRaw.R"); source("Rgenetics.R"); loadLibraries();
#system('. ~/src/Rprivate/exportR.sh ; cp ~/src/Rprivate/RgenericAllRaw.R .');

#
#	<p> Meta-helpers
#

#
#	level dependend logging
# moved from Rsystem.R to break dependency cylce (22.3.2017)
#Global..Log..Level = 4;
#Default..Log..Level = 4;
#assign(Default..Log..Level, 4, envir = .GlobalEnv);
Log_env__ <- new.env();
assign('DefaultLogLevel', 4, envir = Log_env__);

# <p> work-arounds for CRAN submissions
# capture.output replaces \n with space
Capture.output = function(expr, envir = parent.frame()) {
	tf = tempfile();
	sink(tf);
	on.exit(sink());
	r = eval(expr, envir = envir);
	return(readFile(tf));
}
Print = function(..., envir = parent.frame())Capture.output(print(...), envir = envir)

#' Log a message to stderr.
#' 
#' Log a message to stderr. Indicate a logging level to control verbosity.
#' 
#' This function prints a message to stderr if the condition is met that a
#' global log-level is set to greater or equal the value indicated by
#' \code{level}. \code{Log.level} returns the current logging level.
#' 
#' @aliases Log Log.setLevel Log.level
#' @param o Message to be printed.
#' @param level If \code{Log.setLevel} was called with this value, subsequent
#' calls to \code{Log} with values of \code{level} smaller or equal to this
#' value will be printed.
#' @param doPrint additional object that will be output using \code{print}.
#' @author Stefan Böhringer <r-packages@@s-boehringer.org>
#' @seealso \code{\link{Log.setLevel}}
#' @return inivisble formatted message or NULL if nothing was logged
#' @keywords io logging
# #' @examples
# #' \dontrun{
# #' 	Log.setLevel(4);
# #' 	Log('hello world', 4);
# #' 	Log.setLevel(3);
# #' 	Log('hello world', 4);
# #' }
Log = function(o, level = get('DefaultLogLevel', envir = Log_env__), doPrint = NULL) {
	if (level <= get('GlobalLogLevel', envir = Log_env__)) {
		#cat(sprintf("R %s: %s\n", date(), as.character(o)));
		message(sprintf("R %s: %s\n", date(), as.character(o)));
		if (!is.null(doPrint)) message(Print(doPrint));
	}
}
Logs = function(o, level = get('DefaultLogLevel', envir = Log_env__), ..., envir = parent.frame()) {
	Log(Sprintf(o, ..., envir = envir), level = level);
}
LogS = function(level, s, ..., envir = parent.frame()) {
	Log(Sprintf(s, ..., envir = envir), level = level);
}

Log.level = function()get('GlobalLogLevel', envir = Log_env__);
Log.setLevel = function(level = get('GlobalLogLevel', envir = Log_env__)) {
	assign("GlobalLogLevel", level, envir = Log_env__);
}
Log.expr = function(level, expr, envir = parent.frame()) {
	oldLevel = Log.level();
	on.exit(Log.setLevel(oldLevel));
	Log.setLevel(level);
	return(eval(expr, envir = envir));
}

Log.setLevel(4);	# default


Stop = function(..., call. = TRUE, domain = NULL, envir = parent.frame()) {
	stop(Sprintf(list(...)[[1]], envir = envir), call., domain)
}

#
#	<p> Meta-functions
#

#
#		Environments
#

# copy functions code adapted from restorepoint R package
object.copy = function(obj) {
	# Dealing with missing values
	if (is.name(obj)) return(obj);
	obj_class = class(obj);

	copy =
		if ('environment' %in% obj_class) environment.copy(obj) else
		if (all('list' == class(obj))) list.copy(obj) else
		#if (is.list(obj) && !(is.data.frame(obj))) list.copy(obj) else
		obj;
	return(copy)
}
list.copy = function(l)lapply(l, object.copy);
environment.restrict = function(envir__, restrict__= NULL) {
	if (!is.null(restrict__)) {
		envir__ = as.environment(List_(as.list(envir__), min_ = restrict__));
	}
	envir__
}
environment.copy = function(envir__, restrict__= NULL) {
	as.environment(eapply(environment.restrict(envir__, restrict__), object.copy));
}

bound_vars = function(f, functions = FALSE) {
	fms = formals(f);
	# variables bound in default arguments
	vars_defaults = unique(unlist(sapply(fms, function(e)all.vars(as.expression(e)))));
	# variables used in the body
	vars_body = setdiff(all.vars(body(f)), names(fms));
	vars = setdiff(unique(c(vars_defaults, vars_body)), c('...', '', '.GlobalEnv'));
	if (functions) {
		vars = vars[!sapply(vars, function(v)is.function(rget(v, envir = environment(f))))];
	}
	vars
}
bound_fcts_std_exceptions = c('Lapply', 'Sapply', 'Apply');
bound_fcts = function(f, functions = FALSE, exceptions = bound_fcts_std_exceptions) {
	fms = formals(f);
	# functions bound in default arguments
	fcts_defaults = unique(unlist(sapply(fms, function(e)all.vars(as.expression(e), functions = TRUE))));
	# functions bound in body
	fcts = union(fcts_defaults, all.vars(body(f), functions = TRUE));
	# remove variables
	#fcts = setdiff(fcts, c(bound_vars(f, functions), names(fms), '.GlobalEnv', '...'));
	fcts = setdiff(fcts, c(bound_vars(f, functions = functions), names(fms), '.GlobalEnv', '...'));
	# remove functions from packages
	fcts = fcts[
		sapply(fcts, function(e) {
			f_e = rget(e, envir = environment(f));
			!is.null(f_e) && environmentName(environment(f_e)) %in% c('R_GlobalEnv', '') && !is.primitive(f_e)
	})];
	fcts = setdiff(fcts, exceptions);
	fcts
}


environment_evaled = function(f, functions = FALSE, recursive = FALSE) {
	vars = bound_vars(f, functions);
	e = nlapply(vars, function(v) rget(v, envir = environment(f)));
	#Log(sprintf('environment_evaled: vars: %s', join(vars, ', ')), 7);
	#Log(sprintf('environment_evaled: functions: %s', functions), 7);
	if (functions) {
		fcts = bound_fcts(f, functions = TRUE);
		fcts_e = nlapply(fcts, function(v){
			#Log(sprintf('environment_evaled: fct: %s', v), 7);
			v = rget(v, envir = environment(f));
			#if (!(environmentName(environment(v)) %in% c('R_GlobalEnv')))
			v = environment_eval(v, functions = TRUE);
		});
		#Log(sprintf('fcts: %s', join(names(fcts_e))));
		e = c(e, fcts_e);
	}
	#Log(sprintf('evaled: %s', join(names(e))));
	r = new.env();
	lapply(names(e), function(n)assign(n, e[[n]], envir = r));
	#r = if (!length(e)) new.env() else as.environment(e);
	parent.env(r) = .GlobalEnv;
	#Log(sprintf('evaled: %s', join(names(as.list(r)))));
	r
}
environment_eval = function(f, functions = FALSE, recursive = FALSE) {
	environment(f) = environment_evaled(f, functions = functions, recursive = recursive);
	f
}

#
#	Parsing, evaluation
#

Parse = function(text, ...) {
	parse(text = text, ...)
}
Eval = function(e, ..., envir = parent.frame(), autoParse = TRUE) {
	if (autoParse && is.character(e)) e = Parse(e, ...);
	eval(e, envir = envir)
	
}

#
#		Freeze/thaw
#
# -> moved to Ext


#
#	</p> freeze/thaw functions
#

#
#	<p> calls
#
# <!> assume matched call
# <A> we only evaluate named args
callEvalArgs = function(call_, env_eval = FALSE) {
	#if (is.null(call_$envir__) || is.null(names(call_$args))) return(call_);
	#if (is.null(call_$envir) || !length(call_$args)) return(call_);

	# <p> evaluate args
	if (length(call_$args)) {
		args = call_$args;
		callArgs = lapply(1:length(args), function(i)eval(args[[i]], envir = call_$envir));
		# <i> use match.call instead
		names(callArgs) = setdiff(names(call_$args), '...');
		call_$args = callArgs;
	}

	if (env_eval) {
		call_$fct = environment_eval(call_$fct, functions = FALSE, recursive = FALSE);
	}
	# <p> construct return value
	#callArgs = lapply(call_$args, function(e){eval(as.expression(e), call_$envir)});
	call_
}

#callWithFunctionArgs = function(f, args, envir__ = parent.frame(), name = NULL) {
callWithFunctionArgs = function(f__, args__, envir__ = environment(f__), name = NULL, env_eval = FALSE) {
	if (env_eval) f = environment_eval(f__, functions = FALSE, recursive = FALSE);
	call_ = list(
		fct = f__,
		envir = environment(f__),
		args = args__,
		name = name
	);
	call_
}
#
#	</p> calls
#

encapsulateCall = function(.call, ..., envir__ = environment(.call), do_evaluate_args__ = FALSE,
	unbound_functions = FALSE) {
	# function body of call
	name = as.character(.call[[1]]);
	fct = get(name, envir = envir__);
	callm = if (!is.primitive(fct)) {
		callm = match.call(definition = fct, call = .call);
		as.list(callm)[-1]
	} else as.list(.call)[-1];
	args = if (do_evaluate_args__) {
		nlapply(callm, function(e)eval(callm[[e]], envir = envir__))
	} else {
		#nlapply(callm, function(e)callm[[e]])
		# <A> changed 1.12.2022
		callm
	}
	# unbound variables in body fct
	#unbound_vars = 

	call_ = list(
		fct = fct,
		envir = envir__,

		#args = as.list(sys.call()[[2]])[-1],
		args = args,

		name = name
	);
	call_
}
# fct_: symbol of the function, envir_get: environmen to get the function from using get
encapsulateFunction = function(fct_, envir__ = envir__, envir_get = envir__) {
	list(
		fct = get(fct_, envir = envir_get),
		envir = envir__,
		args = list(),
		name = as.character(fct_)
	)
}

encapsulateCallOrFunction = function(call_, envir__) {
	if ((is.symbol(call_)))
		encapsulateFunction(call_, envir__) else
		encapsulateCall(call_, envir__ = envir__)
}

Compose = function(F2, F1) {
	function(...) {
		r1 = do.call(F1$fct, args = c(F1$args, list(...)), envir = F1$envir);
		r2 = do.call(F2$fct, args = c(list(r1), F2$args), envir = F2$envir);
		return(r2);
	}
}

`%.%` <- compose <- function(f2, f1, envir__ = parent.frame()) {
	F2  = encapsulateCallOrFunction(sys.call()[[2]], envir__ = envir__);
	F1  = encapsulateCallOrFunction(sys.call()[[3]], envir__ = envir__);
	return(Compose(F2, F1));
}

#' Deparsing of expression
#'
#' Create single character string from R expression
#'
#' Calls deparse on argument and pastes together the return value of \code{deparse()} resulting
#' in a single character string. Operates very similar to \code{dput()}, except, it cannot write to
#' a file.
#'
#' @param o Expression/Object to be deparsed
#'
#' @return single character vector with the deparsed expression
#' @seealso [deparse()] which this function wraps
#' @seealso [eval()] for the inverse operation
#' @seealso [dput()] similar function
#' @seealso [dget()] similar to eval of character string
#' @return character string with the deparsed R-object
#' @examples
#' \dontrun{
#'	Deparse(3)
#'	Deparse(1 + 2)
#'	Deparse(matrix(1:10, ncol = 5))
#'	eval(Deparse(matrix(1:10, ncol = 5)))
#' }
Deparse = function(o)join(deparse(o));
#
#	RmetaExt.R
#Wed Feb 26 13:21:51 CET 2020

#
#		Freeze/thaw
#

delayed_objects_env = new.env();
delayed_objects_attach = function() {
	attach(delayed_objects_env);
}
delayed_objects_detach = function() {
	detach(delayed_objects_env);
}

thaw_list = function(l)lapply(l, thaw_object, recursive = T);
thaw_environment = function(e) {
	p = parent.env(e);
	r = as.environment(thaw_list(as.list(e)));
	parent.env(r) = p;
	r
}

# <i> sapply
thaw_object_internal = function(o, recursive = T, envir = parent.frame()) {
	r = 		 if (class(o) == 'ParallelizeDelayedLoad') thaw(o) else
	#if (recursive && class(o) == 'environment') thaw_environment(o) else
	if (recursive && class(o) == 'list') thaw_list(o) else o;
	r
}

thaw_object = function(o, recursive = T, envir = parent.frame()) {
	if (all(search() != 'delayed_objects_env')) delayed_objects_attach();
	thaw_object_internal(o, recursive = recursive, envir = envir);
}

#
#	<p> backend classes
#

setGeneric('thaw', function(self, which = NA) standardGeneric('thaw'));

setClass('ParallelizeDelayedLoad',
	representation = list(
		path = 'character'
	),
	prototype = list(path = NULL)
);
setMethod('initialize', 'ParallelizeDelayedLoad', function(.Object, path) {
	.Object@path = path;
	.Object
});

setMethod('thaw', 'ParallelizeDelayedLoad', function(self, which = NA) {
	if (0) {
	key = sprintf('%s%s', self@path, ifelse(is.na(which), '', which));
	if (!exists(key, envir = delayed_objects_env)) {
		Log(sprintf('Loading: %s; key: %s', self@path, key), 4);
		ns = load(self@path);
		object = get(if (is.na(which)) ns[1] else which);
		assign(key, object, envir = delayed_objects_env);
		gc();
	} else {
		#Log(sprintf('Returning existing object: %s', key), 4);
	}
	#return(get(key, envir = delayed_objects_env));
	# assume delayed_objects_env to be attached
	return(as.symbol(key));
	}

	delayedAssign('r', {
		gc();
		ns = load(self@path);
		object = get(if (is.na(which)) ns[1] else which);
		object
	});
	return(r);
});

RNGuniqueSeed = function(tag) {
	if (exists('.Random.seed')) tag = c(.Random.seed, tag);
	md5 = md5sumString(join(tag, ''));
	r = list(
		kind = RNGkind(),
		seed = hex2int(substr(md5, 1, 8))
	);
	r
}

RNGuniqueSeedSet = function(seed) {
	RNGkind(seed$kind[1], seed$kind[2]);
	#.Random.seed = freeze_control$rng$seed;
	set.seed(seed$seed);
}

FreezeThawControlDefaults = list(
	dir = '.', sourceFiles = c(), libraries = c(), objects = c(), saveResult = T,
	freeze_relative = F, freeze_ssh = T, logLevel = Log.level()
);

freezeObjectsCooked = function(objects, envir = parent.frame(), defaultEnv = 0) {
	if (is.null(objects)) return(NULL);
	ns = names(objects);
	o = lapply(seq_along(objects), function(i) {
		if (ns[i] != '') {
			lapply(objects[[i]], function(n)setNames(list(get(n, envir = get(ns[i], envir))), n));
		} else setNames(list(get(objects[[i]], envir = envir)), objects[[i]])
	});
	return(setNames(o, ns));
}
thawObjectsCooked = function(objects, envir = parent.frame(), assignPos = 1) {
	if (is.null(objects)) return(NULL);
	ns = names(objects);
	freeze = lapply(seq_along(objects), function(i) {
		if (ns[i] != '') {
			os = sapply(objects[[i]], function(o) {
				this = mget(ns[i], envir, ifnotfound = NA)[[1]];
				if (!is.environment(this)) {
					this = new.env();
					assign(ns[i], this, pos = assignPos);
				}
				assign(names(o[1]), o[[1]], envir = this);
				return(names(o[1]));
			});
			return(os);
		} else {
			assign(names(objects[[i]]), objects[[i]][[1]], pos = assignPos);
			return(names(objects[[i]]));
		}
	});
	return(setNames(freeze, ns));
}

thawCall = function(
	freeze_control = FreezeThawControlDefaults,
	freeze_tag = 'frozenFunction', freeze_file = sprintf('%s/%s.RData', freeze_control$dir, freeze_tag),
	envir = .GlobalEnv) {

	load(freeze_file, envir = envir);
	# <!> untested change [addition of line] 15.1.2020
	callSpecification = get('callSpecification');
	r = with(callSpecification, {
		for (library in freeze_control$libraries) {
			eval(parse(text = sprintf('library(%s)', library)));
		}
		for (s in freeze_control$sourceFiles) source(s, chdir = T);
		Log.setLevel(freeze_control$logLevel);
		if (!is.null(freeze_control$rng)) RNGuniqueSeed(freeze_control$rng);
		thawObjectsCooked(freeze_objects, envir);

		if (is.null(callSpecification$freeze_envir)) freeze_envir = .GlobalEnv;
		# <!> freeze_transformation must be defined by the previous source/library calls
		transformation = eval(parse(text = freeze_control$thaw_transformation));
		r = do.call(eval(parse(text = f)), transformation(args), envir = freeze_envir);
		#r = do.call(f, args);
		if (!is.null(freeze_control$output)) save(r, file = freeze_control$output);
		r
	});
	r
}

frozenCallWrap = function(freeze_file, freeze_control = FreezeThawControlDefaults,
	logLevel = Log.level(), remoteLogLevel = logLevel)
	with(merge.lists(FreezeThawControlDefaults, freeze_control), {
	sp = splitPath(freeze_file, ssh = freeze_ssh);
	file = if (freeze_relative) sp$file else sp$path;
	#wrapperPath = sprintf("%s-wrapper.RData", splitPath(file)$fullbase);
	r = sprintf("R.pl --template raw --no-quiet --loglevel %d --code 'eval(get(load(\"%s\")[[1]]))' --",
		logLevel, file);
	r
})

frozenCallResults = function(file) {
	callSpecification = NULL;	# define callSpecification
	load(file);
	get(load(callSpecification$freeze_control$output)[[1]]);
}

freezeCallEncapsulated = function(call_,
	freeze_control = FreezeThawControlDefaults,
	freeze_tag = 'frozenFunction', freeze_file = sprintf('%s/%s.RData', freeze_control$dir, freeze_tag),
	freeze_save_output = F, freeze_objects = NULL, freeze_objects_envir = parent.frame(),
	thaw_transformation = identity)
	with(merge.lists(FreezeThawControlDefaults, freeze_control), {

	sp = splitPath(freeze_file, ssh = freeze_ssh);
	outputFile = if (freeze_save_output)
		sprintf("%s_result.RData", if (freeze_relative) sp$base else sp$fullbase) else
		NULL;

	callSpecification = list(
		f = deparse(call_$fct),
		#f = freeze_f,
		args = call_$args,
		freeze_envir = if (is.null(call_$envir)) new.env() else call_$envir,
		freeze_control = list(
			sourceFiles = sourceFiles,
			libraries = libraries,
			output = outputFile,
			rng = freeze_control$rng,
			logLevel = freeze_control$logLevel,
			thaw_transformation = deparse(thaw_transformation)
		)
	);
	thawFile = if (freeze_relative) sp$file else sp$path;
	callWrapper = call('thawCall', freeze_file = thawFile);
	#Save(callWrapper, callSpecification, thawCall, file = file);
	#Save(c('callWrapper', 'callSpecification', 'thawCall', objects),
	#	file = freeze_file, symbolsAsVectors = T);
	#Save(c(c('callWrapper', 'callSpecification', 'thawCall'), objects),
	freeze_objects = freezeObjectsCooked(freeze_objects, freeze_objects_envir);
	Save(c('callWrapper', 'callSpecification', 'thawCall', 'freeze_objects'),
		file = freeze_file, symbolsAsVectors = T);
	freeze_file
})


freezeCall = function(freeze_f, ...,
	freeze_control = FreezeThawControlDefaults,
	freeze_tag = 'frozenFunction', freeze_file = sprintf('%s/%s.RData', freeze_control$dir, freeze_tag),
	freeze_save_output = F, freeze_envir = parent.frame(), freeze_objects = NULL, freeze_env_eval = F,
	thaw_transformation = identity) {

	# args = eval(list(...), envir = freeze_envir)
	call_ = callWithFunctionArgs(f__ = freeze_f, args__ = list(...),
		envir__ = freeze_envir, name = as.character(sys.call()[[2]]), env_eval = freeze_env_eval);

	freezeCallEncapsulated(call_,
		freeze_control = freeze_control, freeze_tag = freeze_tag,
		freeze_file = freeze_file, freeze_save_output = freeze_save_output,
		freeze_objects = freeze_objects, freeze_objects_envir = freeze_envir,
		thaw_transformation = thaw_transformation
	);
}

#
# compact saving for delayed loading
#
# <i> make 
# object: list with single element
freezeObject = function(object, env) {
	dir = attr(env, 'path');
	name = names(object);
	file = Sprintf('%{dir}s/%{name}s.Rdata');
	#print(file);
	save(list = name, envir = as.environment(object), file = file);
	eval(substitute(delayedAssign(OBJECT, get(load(file = FILE)[1])),
		list(OBJECT = name, FILE = file)), envir = env);
}
freezeObjectsList = function(objects, pos = 2, parent = parent.frame(), freezeObjectDir = NULL) {
	td = firstDef(freezeObjectDir, tempdir());
	env = new.env(parent = parent);
	attr(env, "path") = td;

	if (any(names(objects) == '')) stop('Only named objects can be frozen. Check for naming conflicts.');
	#n = names(objects) == '';
	#if (sum(n) > 0) names(objects)[n] = paste('ARG_ANON__', 1:sum(n), sep = '');
	nlapply(objects, function(n)freezeObject(objects[n], env = env));
	env
}
freezeObjects = function(..., pos = 2, parent = parent.frame(), freezeObjectDir = NULL) {
	freezeObjectsList(list(...), pos = pos, parent, freezeObjectDir)
}
#
#	Rmeta-classes.R
#Wed May 17 17:53:44 2017
#
# split of classes from Rmeta.R to avoid warnings from pullins

#
#	<p> delayed loading for matrix-like data structures
#

# missing2null
#	needed to work around callSuper's refusal to pass missing arguments
m2n = function(e)(if (missing(e)) NULL else e)

DelayedDataRefClass = setRefClass('DelayedDataRef',
	fields = list(
		# heuristics to correct numerical index
		'initialIndex' = 'list',
		'Data' = 'ANY'
	),
	methods = list(

	initialize = function(vivifier, ...) {
		.self
	},
	vivify = function(i, j, ...) {
		initialIndex <<- list(i = m2n(i)[1], j = m2n(j)[1]);
	},
	correctIndexI = function(i) { i },
	correctIndexJ = function(j) { j },
	at = function(i, j, ..., drop = TRUE) {
		if (class(Data) == 'uninitializedField') .self$vivify(i, j, ...);
		# <!> only handles two dimensional case
		# <!> does not cover all corner cases
		# <i> correct index, if numeric, if necessary
		if (!missing(i) && is.numeric(i)) i = correctIndexI(i);
		if (!missing(j) && is.numeric(j)) j = correctIndexJ(j);

		# can we fetch requested values; assume all indexed values present?
		if ((!missing(i) && (
				(is.numeric(i) && any(abs(i) > nrow(Data))) ||
				(is.character(i) && !all(i %in% dimnames(Data)[[1]]))
			)) ||
			(!missing(j) && (
				(is.numeric(j) && any(abs(j) > ncol(Data))) ||
				(is.character(j) && !all(j %in% dimnames(Data)[[2]]))
			))) {
			stop('delayed object could not provide requested values');
		}
		# <i> re-vivify
		r = Data[i, j, ..., drop = drop];
		r
	},
	# <A> assumes complete vivification as in load
	el = function(i, j, ..., exact = TRUE) {
		if (class(Data) == 'uninitializedField') .self$vivify(i, j, ...);
		# <i> re-vivify
		r = if (missing(j))
			Data[[i, ..., exact = exact]] else
			Data[[i, j, ..., exact = exact]];
		r
	}


	)
);
DelayedDataRefClass$accessors(names(DelayedDataRefClass$fields()));

DelayedDataLookupClass = setRefClass('DelayedDataLookup',
	fields = list(
		'path' = 'character',
		'lookup' = 'logical'
	),
	contains = 'DelayedDataRef',
	methods = list(

	initialize = function(path, lookup = TRUE, ...) {
		callSuper(...);
		.self$initFields(path = path, lookup = lookup);
		.self
	},
	vivify = function(i, j, ..., logLevel__ = 4) {
		callSuper(i, j, ...);
		# this depends on package parallelize.dynamic
		if (lookup) {
			path0 = path;
			path <<- parallelize_lookup(path);
			Logs('Vivifying name "%{path0}s" from "%{path}s"', logLevel = logLevel__);
		}
	}

	)
);
DelayedDataLookupClass$accessors(names(DelayedDataLookupClass$fields()));

# collapse: on access, remove all other keys and garbage collect
# <i> extract elements from list
loadVivifierDefaultOptions = list(collapse = FALSE);

DelayedDataLoadClass = setRefClass('DelayedDataLoad',
	fields = list(
		options = 'list',
		'transform' = 'function'
	),
	contains = 'DelayedDataLookup',
	methods = list(

	initialize = function(path, lookup = TRUE, options = list(), ...) {
		callSuper(path, lookup, ...);
		.self$initFields(options = options);
		.self
	},
	vivify = function(i, j, ..., exact = TRUE, logLevel__ = 4) {
		callSuper(...);
		o = merge.lists(loadVivifierDefaultOptions, options);
		Logs("Vivifying generic Rdata path '%{path}q'", level = logLevel__);
		Data <<- get(load(path)[1]);
		# if not null function: notE(body(function()NULL)) == FALSE
		if (notE(body(transform))) Data <<- transform(Data);
	}
	)
);
DelayedDataLoadClass$accessors(names(DelayedDataLoadClass$fields()));

setClass('DelayedData', representation(ref = 'DelayedDataRef'));
setMethod('initialize', 'DelayedData', function(.Object, ref) {
	.Object = callNextMethod(.Object);
	.Object@ref = ref;
	.Object
});
DelayedDataAt = function(x, i, j, ..., drop = TRUE)x@ref$at(i, j, ..., drop = drop)
DelayedDataEl = function(x, i, j, ..., exact = TRUE)x@ref$el(i, j, ..., exact = exact)
DelayedDataNm = function(x, name)x@ref$el(name, exact = FALSE)
setMethod('[', signature('DelayedData', 'ANY', 'ANY', 'ANY'), DelayedDataAt);
# findMethods("[[")
setMethod('[[', signature('DelayedData', 'ANY', 'ANY'), DelayedDataEl);
# findMethods("$")
setMethod('$', signature('DelayedData'), DelayedDataNm);

delayedLoadFromPath = function(name, transform = NULL, ..., logLevel = 4, class = 'DelayedDataLoad') {
	ref = new(class,  path = name, lookup = T, transform = transform, ...);
	v = new('DelayedData', ref = ref);
	v
}

#
#	</p> delayed loading
#
#
#	Rlibraries.R
#Wed Oct 31 19:00:40 CET 2012

loadLibraries = function() {
	Require('geepack');
	Require('glmnet');
	Require('ggplot2'); if (exists('theme_set')) theme_set(theme_bw());
	Require('grid');
	Require('magrittr');
	Require('set');
	Require('compare');
	#Require('plyr');	#is.formula
}

recoverOn = function(sw = TRUE)if (sw) options(error = recover) else options(error = NULL)
#
#	Rdata.R
#Mon 27 Jun 2005 10:49:06 AM CEST
#system("~/src/Rprivate/exportR.sh");
#system("~/src/Rprivate/exportR.sh"); source("RgenericAll.R"); source("Rgenetics.R"); loadLibraries();
#system('~/src/Rprivate/exportR.sh ; cp ~/src/Rprivate/RgenericAllRaw.R .');
# <!> copied to Rmeta.R as being the first file to be exported by now (26.3.2017)

#
#	<§> abstract data functions
#

defined = function(x) exists(as.character(substitute(x)));
defined.by.name = function(name) { class(try(get(name), silent = TRUE)) != 'try-error' }
# equivalent to i %in% v
is.in = function(i, v)(length((1:length(v))[v == i])>0)
rget = function(name, default = NULL, ..., pos = -1, envir = as.environment(pos)) {
	#obj = try(get(name, ...), silent = TRUE);
	#r = if(class(obj) == 'try-error') default else obj;
	#r = if (exists(name, where = pos, envir = envir)) get(name, ..., pos = pos, envir = envir) else default;
	r = if (exists(name, envir = envir)) get(name, ..., envir = envir) else default;
	r
}
# .fdE: use notE
firstDef = function(..., .fdInterpolate = FALSE, .fdIgnoreErrors = FALSE, .fdE = FALSE) {
	l = if (.fdInterpolate) c(...) else list(...);
	for (i in l) {
		if ((!is.null(i) && (!.fdE || notE(i))) && (!.fdIgnoreErrors || class(i) != 'try-error'))
			return(i)
	};
	NULL
}
FirstDef = function(..., .fdInterpolate = FALSE, .fdIgnoreErrors = FALSE, .fdE = TRUE)
	firstDef(..., .fdInterpolate = .fdInterpolate, .fdIgnoreErrors = .fdIgnoreErrors, .fdE = .fdE)
firstDefNA = function(..., .fdInterpolate = FALSE) {
	l = if (.fdInterpolate) c(...) else list(...);
	for (i in l) { if (!is.na(i)) return(i)};
	NULL
}
# <N> NULL behaviour
to.list = function(..., .remove.factors = TRUE){
	r = if(is.null(...)) NULL else if (is.list(...)) c(...) else list(...);
	if (.remove.factors) {
		r = sapply(r, function(e)ifelse(is.factor(e), levels(e)[e], e));
	}
	r
}
# clean list/vector
Avu = function(v)as.vector(unlist(v))
is.Null = nullOrLengthNull = function(e)(is.null(e) || length(e) == 0);
# pretty much force everything to be a vector
# toNA: length(0) or NULL converted to NA
avu = function(v, recursive = TRUE, toNA = TRUE) {
	transform = if (toNA)
		function(e, condition)(if (condition) NA else avu(e, toNA = TRUE, recursive = TRUE)) else
		function(e, ...)avu(e, toNA = FALSE, recursive = TRUE);

	r = if (is.list(v)) {
		nls = sapply(v, is.Null);	# detects nulls
		# unlist removes NULL values -> NA
		unlist(sapply(seq_along(v), function(i)transform(v[[i]], nls[i])));
	} else as.vector(v);
	if (!length(r)) return(NULL);
	r
}
#pop = function(v)rev(rev(v)[-1]);

assign.list = function(l, pos = -1, envir = as.environment(pos), inherits = FALSE, immediate = TRUE) {
	for (n in names(l)) {
		assign(n, l[[n]], pos, envir, inherits, immediate);
	}
}
eval.text = function(text, envir = parent.frame())eval(parse(text = text), envir= envir);

nullomit = function(r)r[!sapply(r, is.null)]

# replace elements base on list
# l may be a list of lists with elements f (from) and t (to), when f is replaced with t
# if both, f and t arguments are not NULL, l will be ignored and f is replaced with t
vector.replace = function(v, l, regex = FALSE, ..., f = NULL, t = NULL) {
# 	if (!is.null(f) & !is.null(t)) l = list(list(f = f, t = t));
# 	# replacments are given in f/t pairs
# 	if (all(sapply(l, length) == 2)) {
# 		from = list.key(l, "f");
# 		to = list.key(l, "t");
# 	} else {
# 		from = names(l);
# 		to = unlist(l);
# 	}
# 	for (i in 1:length(from)) {
# 		if (regex) {
# 			idcs = which(sapply(v, function(e)(length(fetchRegexpr(from[i], e, ...)) > 0)));
# 			v[idcs] = sapply(v[idcs], function(e)gsub(from[i], to[i], e));
# 		} else v[which(v == from[i])] = to[i];
# 	}
 	repl = if (!is.null(f) & !is.null(t)) listKeyValue(f, t) else l;
	# <!> tb tested
 	v = if (!regex) {
		raw = repl[v];
		unlist(ifelse(sapply(repl[v], is.null), v, raw))
	} else {
		sapply(v, function(e){
			# first match takes precedent
			j = which(sapply(names(repl), function(f)length(fetchRegexpr(f, e, ...)) > 0))[1];
			if (is.na(j)) e else gsub(names(repl)[j], repl[[j]], e)
		})
	}
	v
}

vector.with.names = function(v, all_names, default = 0) {
	r = rep(default, length(all_names));
	names(r) = all_names;
	is = which.indeces(names(v), all_names, ret.na = TRUE);
	r[is[!is.na(is)]] = v[!is.na(is)];
	r
}
vector.named = function(v, Names) {
	names(v) = Names;
	v
}

# dir: direction of selection: 1: select rows, 2: select columns
mat.sel = function(m, v, dir = 1) {
	r = if (dir == 1)
		sapply(1:length(v), function(i)m[v[i], i]) else
		sapply(1:length(v), function(i)m[i, v[i]]);
	r
}

# rbind on list
simplify = sapplyId = function(l)sapply(l, identity);
Simplify = function(l)unlist(simplify(l));

listFind = function(lsed, lsee) {
	values = sapply(names(lsee), function(n)list.key(lsed, n), simplify = FALSE, USE.NAMES = FALSE);
	values = sapply(values, identity);
	found = apply(values, 1, function(r) all(r == lsee));
	r = unlist.n(lsed[found], 1);
	r
}

#same.vector = function(v)(unique(v) == 1)
same.vector = function(v)all(v == v[1])

# in vector v, find index min j \in 1, ..., N so that v[1:j] contains at least U unique elements
uniqueIndex = function(v, U) {
	#Nu = sapply(seq_along(v), function(i)length(unique(data$chr[1:i])));
	# more efficient version
	u = c();
	for (i in seq_along(v)) {
		u = unique(c(u, v[i]));
		if (length(u) == U) return(i);
	}
	return(NA);
}


#
#	<§> string manipulation
#

#join = function(v, sep = " ")if (length(v) == 0) '' else paste(v, collapse = sep);
join = function(v, sep = " ")paste(v, collapse = sep);
con = function(..., Sep_ = '')paste(..., sep = Sep_);
Con = function(..., Sep_ = '')paste(unlist(list(...)), collapse = Sep_);
# pastem = function(a, b, ..., revsort = TRUE) {
# 	if (revsort)
# 		as.vector(apply(merge(data.frame(a = b), data.frame(b = a), sort = FALSE), 1,
# 			function(e)paste(e[2], e[1], ...))) else
# 		as.vector(apply(merge(data.frame(a = a), data.frame(b = b), sort = FALSE), 1,
# 			function(e)paste(e[1], e[2], ...)))
# }
pastem = function(a, b, ..., revsort = TRUE) {
	df = merge.multi.list(list(Df(a = a), Df(b = b)), .first.constant = revsort);
	paste(df[, 1], df[, 2], ...)
}

r.output.to.vector.int = function(s) {
	matches = gregexpr("(?<![\\[\\d])\\d+", s, perl=TRUE);
	starts = as.vector(matches[[1]]);
	lengthes = attr(matches[[1]], "match.length");
	v = sapply(1:length(starts), function(i){ substr(s, starts[i], starts[i] + lengthes[i] -1) });
	as.integer(v)
}
r.output.to.vector.numeric = function(s) {
	matches = gregexpr("\\d*\\.\\d+", s, perl=TRUE);
	starts = as.vector(matches[[1]]);
	lengthes = attr(matches[[1]], "match.length");
	v = sapply(1:length(starts), function(i){ substr(s, starts[i], starts[i] + lengthes[i] -1) });
	as.numeric(v)
}
readFile = function(path) { join(scan(path, what = "raw", sep = "\n", quiet = TRUE), sep = "\n") };
circumfix = function(s, post = NULL, pre = NULL) {
	if (is.null(s) || length(s) == 0) return('');
	sapply(s, function(s)if (s == '') s else con(pre, s, post))
}
abbr = function(s, Nchar = 20, ellipsis = '...') {
	ifelse(nchar(s) > Nchar, paste(substr(s, 1, Nchar - nchar(ellipsis)), ellipsis, sep = ''), s)
}
wrapStr = function(s, Nchar = 60, regex = '\\s+', indent = "\n") {
	r = '';
	while (nchar(s) > Nchar) {
		R = gregexpr('\\s+', s, perl = TRUE);
		Iws = R[[1]][R[[1]] <= Nchar];
		Ichr = max(Iws);
		# <i> handle Ichr = 1
		r = con(r, substr(s, 1, Ichr - 1), indent);
		s = substr(s, Ichr + attr(R[[1]], 'match.length')[length(Iws)], nchar(s));

	}
	r = con(r, s);
	return(r);
}

Which.max = function(l, last.max = TRUE, default = NA) {
	if (is.logical(l) && all(!l)) return(default);
	r = if (last.max) (length(l) - which.max(rev(l)) + 1) else which.max(l);
	r
}
Which.min = function(l, last.min = FALSE, default = NA) {
	if (is.logical(l) && all(!l)) return(default);
	r = if (last.min) (length(l) - which.min(rev(l)) + 1) else which.min(l);
	r
}

is_ordered = function(v)all(order(v) == seq(length(v)))

# capturesN: named captures; for each name in captureN put the captured value assuming names to be ordered
# captures: fetch only first capture per match <!> deprecated
# capturesAll: fetch all caputers for each match
fetchRegexpr = function(re, str, ..., ret.all = FALSE, globally = TRUE, captures = FALSE, captureN = c(),
	capturesAll = FALSE, maxCaptures = 9, returnMatchPositions = FALSE) {
	if (length(re) == 0) return(c());
	r = if (globally)
		gregexpr(re, str, perl = TRUE, ...)[[1]] else
		regexpr(re, str, perl = TRUE, ...);
	if (all(r < 0)) return(NULL);
	l = sapply(1:length(r), function(i)substr(str, r[i], r[i] + attr(r, "match.length")[i] - 1));
	if (captures) {
		l = sapply(l, function(e)gsub(re, '\\1', e, perl = TRUE, fixed = FALSE));
	} else if (length(captureN) > 0) {
		l = lapply(l, function(e) {
			r = sapply(1:length(captureN), function(i) {
				list(gsub(re, sprintf('\\%d', i), e, perl = TRUE, fixed = FALSE))
			});
			names(r) = captureN;
			r
		});
	} else if (capturesAll) {
		l = lapply(l, function(e) {
			cs = c();	# captures
			# <!> hack to remove zero-width assertions (no nested grouping!)
			#re = gsub('(\\(\\?<=.*?\\))|(\\(\\?=.*?\\))', '', re, perl = TRUE, fixed = FALSE);
			for (i in 1:maxCaptures) {
				n = gsub(re, sprintf('\\%d', i), e, perl = TRUE, fixed = FALSE);
				cs = c(cs, n);
			}
			cs
		});

		# trim list
		#maxEls = maxCaptures - min(c(maxCaptures + 1, sapply(l, function(e)Which.max(rev(e != ''))))
		#	, na.rm = TRUE) + 1;
		maxEls = max(c(sapply(l, function(e)Which.max(e != '', default = 1)), 1));
		l = lapply(l, function(e)(if (maxEls > 0) e[1:maxEls] else NULL));
	}
	if (!ret.all) l = l[l != ""];
	ret = if (returnMatchPositions) list(match = l, positions = r) else l;
	ret
}
# improved multistring version
FetchRegexpr = function(re, str, ..., ret.all = FALSE, globally = TRUE, captures = FALSE, captureN = c(),
	capturesAll = FALSE, maxCaptures = 9, returnMatchPositions = FALSE) {
	if (length(re) == 0) return(c());
	r = if (globally)
		gregexpr(re, str, perl = TRUE, ...) else
		list(regexpr(re, str, perl = TRUE, ...));
	if (all(unlist(r) < 0)) return(NULL);
	l = sapply(seq_along(r),
		function(j) {
			r0 = r[[j]];
			sapply(1:length(r0),
				function(i)substr(str[j], r0[i], r0[i] + attr(r0, "match.length")[i] - 1))
	});
	if (captures) {
		l = sapply(l, function(e)gsub(re, '\\1', e, perl = TRUE, fixed = FALSE));
		#print(l);
	} else if (length(captureN) > 0) {
		l = lapply(l, function(e) {
			r = sapply(1:length(captureN), function(i) {
				list(gsub(re, sprintf('\\%d', i), e, perl = TRUE, fixed = FALSE))
			});
			names(r) = captureN;
			r
		});
	} else if (capturesAll) {
		l = lapply(l, function(e) {
			cs = c();	# captures
			# <!> hack to remove zero-width assertions (no nested grouping!)
			#re = gsub('(\\(\\?<=.*?\\))|(\\(\\?=.*?\\))', '', re, perl = TRUE, fixed = FALSE);
			for (i in 1:maxCaptures) {
				n = gsub(re, sprintf('\\%d', i), e, perl = TRUE, fixed = FALSE);
				cs = c(cs, n);
			}
			cs
		});

		# trim list
		#maxEls = maxCaptures - min(c(maxCaptures + 1, sapply(l, function(e)Which.max(rev(e != ''))))
		#	, na.rm = TRUE) + 1;
		maxEls = max(c(sapply(l, function(e)Which.max(e != '', default = 1)), 1));
		l = lapply(l, function(e)(if (maxEls > 0) e[1:maxEls] else NULL));
	}
	if (!ret.all) l = l[l != ""];
	ret = if (returnMatchPositions) list(match = l, positions = r) else l;
	ret
}

regex = Vectorize(fetchRegexpr, 'str', SIMPLIFY = TRUE, USE.NAMES = TRUE);
Regex = Vectorize(FetchRegexpr, 're', SIMPLIFY = TRUE, USE.NAMES = TRUE);
RegexL = Vectorize(FetchRegexpr, 're', SIMPLIFY = FALSE, USE.NAMES = TRUE);
regexIdcs = function(re, s, ...)vectorIdcs(regex(re, s, ...), is.null, not = TRUE)

# unify capture extraction for gregexpr, regexpr
# pos == 0: grexepr, regexpr else by iterating pos as index into str
matchRegexCapture = function(reg, str, pos = NULL) {
	if (is.null(attr(reg, 'capture.start'))) return(NULL);
	if (!is.null(pos)) str = str[pos] else pos = seq_along(reg);
	captures = lapply(1:ncol(attr(reg, 'capture.start')), function(i) {
		vs = sapply(pos, function(j)Substr(str,
			attr(reg, 'capture.start')[j, i], attr(reg, 'capture.length')[j, i]))
		vs
	});
	names(captures) = attr(reg, 'capture.names');
	captures
}
MatchRegexExtract = function(m, s, pos = seq_along(m)) {
	matches = ifelse(m[pos] < 0, character(0),
		sapply(pos, function(i)Substr(s[i], m[i], attr(m, 'match.length')[i])));
	matches
}
matchRegexExtract = function(reg, str, pos = NULL) {
	if (!is.null(pos)) str = str[pos] else pos = seq_along(reg);
	matches = ifelse(reg[pos] < 0, character(0),
		sapply(pos, function(i)Substr(str, reg[i], attr(reg, 'match.length')[i])));
	matches
}
# <i> re nested list with sub-res for named captures
# <!> globally == FALSE, removeNonMatch == FALSE
matchRegex = function(re, str, ..., globally = TRUE, simplify = TRUE,
	positions = FALSE, removeNonMatch = FALSE) {
	if (length(re) == 0) return(NULL);
	reg = if (globally) gregexpr(re, str, perl = TRUE, ...) else regexpr(re, str, perl = TRUE, ...);
	ms = if (globally)
		lapply(seq_along(reg), function(i)matchRegexExtract(reg[[i]], str[i])) else
		lapply(seq_along(str), function(i)matchRegexExtract(reg, str, pos = i));
	#	regmatches(str, reg);
	captures = if (globally)
		lapply(seq_along(reg), function(i)matchRegexCapture(reg[[i]], str[i])) else
		lapply(seq_along(str), function(i)matchRegexCapture(reg, str, pos = i));
	if (removeNonMatch) {
		nonmatch = sapply(ms, length) == 0 | is.na(ms);
		ms = ms[!nonmatch];
		captures = captures[!nonmatch];
		reg = reg[!nonmatch];
	}
	if (simplify && length(str) == 1) {
		ms = ms[[1]];
		captures = captures[[1]];
		reg = reg[[1]];
	}
	r = if(positions) list(match = ms, capture = captures, positions = reg) else
		list(match = ms, capture = captures);
	r
}

#
#	<p> final interface as of 2016/04
#
MatchRegex = function(re, str, mode = 'return') {
	r = regexpr(re, str);
	if (mode == 'return') {
		r = str[which(r > 0)];
	}
	r
}
# handle attributes
# As.list assumes attributes and vector elements to be paired
#	corresponding values/attributes will be put into the list
As.list = function(v) {
	as = Recycle(attributes(v));
	l = lapply(seq_along(v), function(i) {
		# does not preserve matrices
		#attrs = list.kp(as, Sprintf('[[%{i}d]]'));
		# should become <i>
		#attrs = list.kp(as, Sprintf('[[%{i}d]]', accessor = function(e)accessIdx(e, i)));
		attrs = lapply(seq_along(as), function(j)accessIdx(as[[j]], i));
		Attr(v[i], SetNames(attrs, names(as)))
	});
	l
}

# transform results from Regexpr captures = TRUE
list.transpose = function(l)lapply(seq_along(l[[1]]), function(i)list.kp(l, Sprintf('[[%{i}d]]')));

# interface as of 2018/06
# if re is vector, iterate over
# by default, return matches
RegexprSingle = function(re, s, captures = FALSE, global = TRUE, simplify = TRUE, concatMatches = TRUE, drop = TRUE) {
	matches = if (global) gregexpr(re, s, perl = TRUE) else As.list(regexpr(re, s, perl = TRUE));
	#print(gregexpr(re, s, perl = TRUE));
	#print(regexpr(re, s, perl = TRUE));
	#print(matches);

	#matches = if (global) gregexpr(re, s, perl = TRUE) else list(regexpr(re, s, perl = TRUE));
	r = pairslapply(matches, s, function(m, s) {	# iterate strings
		if (captures) {
			r = matchRegexCapture(m, s);
			if (concatMatches) r = apply(do.call(cbind, r), 1, join, sep = '');
		} else {
			r = MatchRegexExtract(m, s);
			if (drop) r = r[!is.na(r)];
		}
		r
	});
	if (simplify && (
		(length(s) == 1 && captures && concatMatches)
	)) r = r[[1]];
	if (simplify && !global) r = Simplify(r);
	return(r);
}

Regexpr = function(re, s, ..., reSimplify = TRUE) {
	r = lapply(re, RegexprSingle, s = unlist(s), ...);
	if (length(re) == 1 && reSimplify) r = r[[1]];
	return(r);
}
RegexprM = function(re, s, ..., reSimplify = TRUE) {
	r = sapply(Regexpr(re, s, ..., reSimplify = reSimplify), function(e)length(e) > 0);
	return(r);
}

splitString = function(re, str, ..., simplify = TRUE) {
	l = lapply(str, function(str) {
		if (is.na(str)) return(NA);
		r = gregexpr(re, str, perl = TRUE, ...)[[1]];
		if (r[1] < 0) return(str);
		l = sapply(1:(length(r) + 1), function(i) {
			substr(str, ifelse(i == 1, 1, r[i - 1] + attr(r, "match.length")[i - 1]),
				ifelse(i > length(r), nchar(str), r[i] - 1))
		});
	});
	if (length(l) == 1 && simplify) l = l[[1]];
	l
}
# modeled after perl's qq
reString = '(?:([_\\/\\-a-zA-Z0-9.]+)|(?:\\"((?:\\\\\\\\.)*(?:[^"\\\\]+(?:\\\\\\\\.)*)*)\\"))';
# use reSep = '\\s+' to split based on a separator RE
qw = function(s, re = reString, reSep = NULL, names = NULL, byrow = TRUE) {
	r = if (notE(reSep)) unlist(splitString(reSep, s)) else {
	#r = if (TRUE) unlist(splitString('\\s+', s)) else
		unlist(Regexpr(re, unlist(s), captures = TRUE));
	}
	if (notE(names)) r = Df_(matrix(r, ncol = length(names), byrow = byrow), names = names);
	r
}
quoteString = function(s)sprintf('"%s"', s)
trimString = function(s) {
	sapply(s, function(e)
		if (is.na(e)) NA else FetchRegexpr('^\\s*(.*?)\\s*$', e, captures = TRUE)
	)
}
qwi = function(...)as.integer(qw(...))
qwn = function(...)as.numeric(qw(...))

valueMapperRaw = function(n, d)d[[n]]
valueMapperStandard = function(n, d) {
	if (is.na(d[[n]])) '{\\bf Value missing}' else (if (is.null(d[[n]])) n else d[[n]])
}

# <N> maxIterations needs to be large as a new iteration is entered after each successful substitution
#	this is necessary, as 
mergeDictToString = function(d, s,
	valueMapper = valueMapperStandard,
	#valueMapper = function(s)ifelse(is.na(d[[n]]), '{\\bf Value missing}', d[[n]]),
	iterative = FALSE, re = FALSE, maxIterations = 1e4, doApplyValueMap = TRUE, doOrderKeys = TRUE, maxLength = 1e7) {
	ns = names(d);
	# proceed in order of decreasing key lengthes
	if (doOrderKeys) ns = ns[rev(order(sapply(ns, nchar)))];
	for (i in 1:maxIterations) {
		s0 = s;
		for (n in ns) {
			# counteract undocumented string interpolation
			subst = if (doApplyValueMap)
				gsub("[\\\\]", "\\\\\\\\", valueMapper(n, d), perl = TRUE)
				else d[[n]];
			# <!> quoting
			if (!re) n = sprintf("\\Q%s\\E", n);
			s = gsub(n, firstDef(subst, ""), s, perl = TRUE, fixed = FALSE);
			# <A> if any substitution was made, it is nescessary to reiterate ns to preserver order
			#	of substitutions
			if (iterative && s != s0) break;
		}
		if (!iterative || s == s0 || nchar(s) > maxLength) break;
	}
	s
}
mergeDictToStringV = Vectorize(mergeDictToString, 's', SIMPLIFY = TRUE, USE.NAMES = TRUE);

mergeDictToVector = function(d, v) { unlist(ifelse(is.na(names(d[v])), v, d[v])) }

mergeDictToDict = function(dMap, dValues, ..., recursive = TRUE) {
	r = lapply(dValues, function(v) {
		r = if (class(v) == 'list') {
			if (recursive) mergeDictToDict(dMap, v, ...) else v
		} else if (class(v) == 'character') mergeDictToString(dMap, v, ...) else v;
		r
	});
	r
}

# double quote if needed
qsSingle = function(s, force = FALSE) {
	# <N> better implementation possible: detect unquoted white-space
	if (force || length(fetchRegexpr('[ \t"()\\[\\]:,]', s)) > 0) {
		s = gsub('([\\"])', '\\\\\\1', s);
		s = sprintf('"%s"', s);
	} else {
		s0 = gsub("([\\'])", '\\\\\\1', s);
		if (s0 != s) s = sprintf("$'%s'", s0);
	}
	s
}
qs = function(s, ...)sapply(s, qsSingle, ...)
# single quote if needed
qssSingle = function(s, force = FALSE) {
	# <N> better implementation possible: detect unquoted white-space
	if (force || nchar(s) == 0 || length(fetchRegexpr("[ \t'\"()\\[\\]:,]", s)) > 0) {
		s = gsub("(['])", "'\"'\"'", s);
		s = sprintf("'%s'", s);
	}
	s
}
qss = function(s, ...)sapply(s, qssSingle, ...)
# include special case for home folder expansion: do not quote initial '~'
qsSinglePath = function(s, ...) {
	if (s == '~')
		s else
	if (nchar(s) >= 2 && substring(s, 1, 2) == '~/')
		con('~/', qsSingle(substring(s, 3), ...)) else
		qsSingle(s, ...)
}
# include special case for home folder expansion"
qsPath = function(s, ...)sapply(s, qsSinglePath, ...)

#' Return sub-strings indicated by positions or produce a string by substituting those strings with
#'	replacements
#'
#' The function behaves similar to sprintf, except that character sequences to be substituted are
#' indicated by name.
#'
#' @param s template string
#' @param start vector of start positions of substrings to substitute
#' @param length vector of lengthes of substrings to substitute
#' @param replacement vector of strings to subsitute. If missing, \code{Substr} returns sub-strings indicated
#'	by start/length
#' @return character vector containing extracted sub-strings
#'
# #' @examples
# #' \dontrun{
# #' print(Substr("abc", c(2, 3), c(1, 1), c("def", 'jkl')));
# #' print(Substr("abcdef", c(2, 3, 5), c(1, 1, 1), c("123", '456', '789')));
# #' print(Substr("abcdef", c(1, 3, 5), c(1, 1, 1), c("123", '456', '789')));
# #' print(Substr("abcdef", c(1, 3, 5), c(0, 1, 0), c("123", '456', '789')));
# #' }
Substr = function(s, start, length, replacement) {
	if (missing(replacement)) return(substr(s, start, start + length - 1));
	start = c(start, nchar(s) + 1);
	l = sapply(seq_along(replacement), function(i)c(
		replacement[i],
		substr(s, start[i] + length[i], start[i + 1] - 1)
	));
	l = c(substr(s, 1, start[1] - 1), as.vector(l));
	r = join(as.vector(l), sep = '');
	r
}

sprintfIgnoreEscapes = function(r) {
	m = r$match;
	L = attr(r$positions, 'capture.length');
	if (!(any(L[, 1] == 0 & L[, 2] == 0))) return(r);
	Is = which(L[, 1] == 0 & L[, 2] == 0);
	r0 = r;
	r$match = r0$match[-Is];
	r$positions = r0$positions[-Is];
	attr(r$positions, 'match.length') = attr(r0$positions, 'match.length')[-Is];
	attr(r$positions, 'capture.start') = attr(r0$positions, 'capture.start')[-Is, , drop = FALSE];
	attr(r$positions, 'capture.length') = attr(r0$positions, 'capture.length')[-Is, , drop = FALSE];
	attr(r$positions, 'capture.names') = attr(r0$positions, 'capture.names')[-Is];
	return(r);
}

# <!> quoting
#'	Produce string by substituting placeholders
#'
#' The function behaves similar to sprintf, except that character sequences to be substituted are
#' indicated by name. To be implemented: *-specifications
#'
#' #@param s template string
#' #@param d values to substitute into \code{s}
#' #@param template template for substitution pattern. Within this pattern \code{__DICT_KEY__} is
#' # substituted for a key in \code{d}. This string \code{k} is substituted in \code{s} with \code{d[[k]]}.
#' @param .fmt formatting string into which values are interpolated (see details)
#' @param values list or vector of values to be used for interpolation
#' @param sprintf_cartesian boolean to indicate whether cartesian product of values should be used.
#'   Otherwise standard recyling rules apply.
#' @param envir environment in which values are to be evaluated
#' @return Interpolated character string
#'
# #' @examples
# #' \dontrun{
# #' Sprintf('These are N %{N} characters.', list(N = 10));
# #' Sprintf('These are N %{N}d characters.', list(N = 10));
# #' Sprintf('These are N %{N}02d characters.', list(N = 10));
# #' }
Sprintfl = function(.fmt, values, sprintf_cartesian = FALSE, envir = parent.frame()) {
	dict = extraValues = list();
	for (i in seq_along(values)) {
		if (is.list(values[[i]]))
			dict = merge.lists(dict, values[[i]]) else
		if (!is.null(names(values)[i]) && names(values)[i] != '')
			dict = merge.lists(dict, values[i]) else
			extraValues = c(extraValues, values[i]);
	}
# 	re = '(?x)(?:
# 		(?:^|[^%]|(?:%%)+)\\K
# 		[%]
# 			(?:[{]([^{}\\*\'"]*)[}])?
# 		((?:[-]?[*\\d]*[.]?[*\\d]*)?(?:[sdfegG]|))(?=[^%sdfegG]|$)
# 	)';
	# <!> new, untested regexpr as of 22.5.2014
	# un-interpolated formats do no longer work
# 	re = '(?xs)(?:
# 		(?:[^%]+|(?:%%)+)*\\K
# 		[%]
# 			(?:[{]([^{}\\*\'"]*)[}])?
# 		((?:[-]?[*\\d]*[.]?[*\\d]*)?(?:[sdfegGDQqu]|))(?=[^sdfegGDQqu]|$)
# 	)';

	re = '(?xs)(?:
		(?:[^%]+|(?:%%)+)*
		\\K[%]
			(?:[{]([^{}\\*\'"]*)[}])?
		((?:[-]?[*\\d]*[.]?[*\\d]*)?(?:[stdfegGDQqu]|))(?=[^stdfegGDQqu]|$)
	)';

# 	re = '(?xs)(?:
# 		(?:(?:[^%]+)(?:(?:%%)+(?:[^%]+))*)
# 		[%]
# 			(?:[{]([^{}\\*\'"]*)[}])?
# 		((?:[-]?[*\\d]*[.]?[*\\d]*)?(?:[sdfegGDQqu]|))(?=[^sdfegGDQqu]|$)
# 	)';

	r = fetchRegexpr(re, .fmt, capturesAll = TRUE, returnMatchPositions = TRUE);
	r = sprintfIgnoreEscapes(r);
	# <p> nothing to format
	if (length(r$match) == 0) return(.fmt);
	typesRaw = sapply(r$match, function(m)ifelse(m[2] == '', 's', m[2]));
	types = ifelse(typesRaw %in% c('D', 'Q'), 's', typesRaw);
	fmts = sapply(r$match, function(m)sprintf('%%%s',
		ifelse(m[2] %in% c('', 'D', 'Q', 'q', 't', 'u'), 's', m[2])));
	fmt1 = Substr(.fmt, r$positions, attr(r$positions, 'match.length'), fmts);

	keys = sapply(r$match, function(i)i[1]);
	nonKeysI = cumsum(keys == '');	# indeces of values not passed by name
	nonKeysIdcs = which(keys == '');

	# <p> collect all values
	allValues = c(extraValues, dict);
	# get interpolation variables
	interpolation = nlapply(keys[keys != ''], function(k)
		if (!is.null(allValues[[k]])) NULL else rget(k, default = NA, envir = envir)
	);
	# <p> handle %D: current day
	keys[typesRaw == 'D'] = '..Sprintf.date..';
	dateValue = if (sum(typesRaw == 'D'))
		list(`..Sprintf.date..` = format(Sys.time(), '%Y%m%d')) else
		list();
	allValues = c(allValues, dateValue, List_(interpolation, rm.null = TRUE));

	# 14.9.2015 -> convert to indeces
	# build value combinations
	listedValues = lapply(keys, function(k)allValues[[k]]);
	dictDf = if (!sprintf_cartesian) Df_(listedValues) else merge.multi.list(listedValues);
	# fill names of anonymous formats
	keys[keys == ''] = names(dictDf)[Seq(1, sum(nonKeysI != 0))];
	# due to repeat rules of R vectors might have been converted to factors
	#dictDf = Df_(dictDf, as_character = unique(keys[types == 's']));
	dictDf = Df_(dictDf, as_character = which(types == 's'));
	
	# <p> conversion <i>: new function
	#colsQ = keys[typesRaw == 'Q'];
	# <!> switch to index based transformation on account of duplicate keys
	colsQ = which(typesRaw == 'Q');
	dictDf[, colsQ] = apply(dictDf[, colsQ, drop = FALSE], 2, qsPath, force = TRUE);
	#colsq = keys[typesRaw == 'q'];
	colsq = which(typesRaw == 'q');;
	dictDf[, colsq] = apply(dictDf[, colsq, drop = FALSE], 2, qss);
	colst = which(typesRaw == 't');;
	dictDf[, colst] = apply(dictDf[, colst, drop = FALSE], 2, qss, force = TRUE);

	colsu = which(typesRaw == 'u');;
	dictDf[, colsu] = apply(dictDf[, colsu, drop = FALSE], 2, uc.first);

	colsd = which(typesRaw == 'd');;
	dictDf[, colsd] = apply(dictDf[, colsd, drop = FALSE], 2, as.integer);
	s = sapply(1:nrow(dictDf), function(i) {
		valueDict = as.list(dictDf[i, , drop = FALSE]);
# 		sprintfValues = lapply(seq_along(keys), function(i)
# 			ifelse(keys[i] == '', extraValues[[nonKeysI[i]]],
# 				firstDef(valueDict[[keys[i]]], rget(keys[i], default = '__no value__'), pos = -2)));
# 		sprintfValues = lapply(seq_along(keys), function(i)
# 			firstDef(valueDict[[keys[i]]], rget(keys[i], default = '__no value__', envir = envir)));
		#sprintfValues = lapply(seq_along(keys), function(i)valueDict[[keys[i]]]);
		#do.call(sprintf, c(list(fmt = fmt1), sprintfValues))
		# <!> simplify above two lines, now robust against duplicated entries -> <i> needs unit tests
		names(valueDict) = NULL;
		do.call(sprintf, c(list(fmt = fmt1), valueDict))
	});
	s
}

# 18.10.2019: fmt -> .fmt to avoid confusion with abbreviated named arguments (e.g. f = x substitutes fmt)
Sprintf = sprintd = function(.fmt, ..., sprintf_cartesian = FALSE, envir = parent.frame(),
	resetNames = TRUE, drop = TRUE) {
	r = sapply(.fmt, function(.fmt)
		Sprintfl(.fmt, list(...), sprintf_cartesian = sprintf_cartesian, envir = envir),
		USE.NAMES = !resetNames);
	# <!> special case when a single .fmt is provided -> do not return matrix for several values
	if (drop && length(.fmt) == 1) r = avu(r);
	r
}

#r = getPatternFromStrings(DOC, '(?:\\nDOCUMENTATION_BEGIN:)([^\\n]+)\\n(.*?)(?:\\nDOCUMENTATION_END\\n)');
getPatternFromStrings = function(strings, pattern, keyIndex = 1) {
	r = lapply(strings, function(s) {
		ps = fetchRegexpr(pattern, s, capturesAll = TRUE);
		listKeyValue(sapply(ps, function(e)e[[keyIndex]]), sapply(ps, function(e)e[-keyIndex]));
	});
	r
}

getPatternFromFiles = function(files, locations = NULL, ...) {
	strings = sapply(files, function(f)readFile(f, prefixes = locations));
	getPatternFromStrings(strings, ...);
}

#
#	hex strings
#

asc = function(x)strtoi(charToRaw(x), 16L);
character.as.characters = function(str) {
	sapply(str, function(s) sapply(1:nchar(s), function(i)substr(str, i, i)));
}

# bit_most_sig in bits
hex2int = function(str, bit_most_sig = 32) {
	cs = rev(sapply(character.as.characters(tolower(str)), asc));
	cms = bit_most_sig / 4;	# character containing most significant bit
	is = ifelse(cs >= asc('a'), cs - asc('a') + 10, cs - asc('0'));
	flipSign = (length(is) >= cms && is[cms] >= 8);
	if (flipSign) is[cms] = is[cms] - 8;
	r = sum(sapply(1:length(is), function(i)(is[i] * 16^(i-1))));
	if (flipSign) r = r - 2^(bit_most_sig - 1);
	r = if (r == - 2^(bit_most_sig - 1)) NA else as.integer(r);
	r
}

# chunk_size in bits
hex2ints = function(str, chunk_size = 32) {
	l = nchar(str);
	csc = chunk_size / 4;	# chunk_size in characters
	chunks = (l + csc - 1) %/% csc;
	r = sapply(1:chunks, function(i)hex2int(substr(str, (i - 1)*csc + 1, min(l, i*csc))));
	r
}

#
#	<§> binary numbers/n-adic numbers
#

ord2base = dec2base = function(o, digits = 5, base = 2) {
	sapply(1:digits, function(i){(o %/% base^(i-1)) %% base})
}
base2ord = base2dec = function(v, base = 2) {
	sum(sapply(1:length(v), function(i)v[i] * base^(i-1)))
}

ord2bin = dec.to.bin = function(number, digits = 5) ord2base(number, digits, base = 2);
bin2ord = bin.to.dec = function(bin) base2ord(bin, base = 2);

# mixed base calculations
#sapply(1:length(base), function(i)((n %/% div[i]) %% base[i]));
cumprod1 = function(v)c(1, cumprod(pop(v)))
# ord2adic = function(n, base = rep(2, 5)) {
# 	div = cumprod1(base);
# 	(n %/% div) %% base
# }
# adic2ord = function(v, base = rep(2, 5)) {
# 	mult = cumprod1(base);
# 	(v %*% mult)[1, 1]
# }
ord2adic = function(n, base = rep(2, 5))((n %/% cumprod1(base)) %% base)
adic2ord = function(v, base = rep(2, 5))((v %*% cumprod1(base))[1, 1])

#
#	<Par> sequences
#

#'	Produce constrained sequences
#'
#' This is a wrapper around seq that adds constraints. Setting ascending, descending to NA reverts to
#' standard \code{seq} behaviour.
#'
#' @param ascending restrict sequences to be ascending; return empty list if to < from
#' @param descending restrict sequences to be descending; return empty list if from < to
#' @param from starting value
#' @param to ending value
#' @param neg boolean to indicate wheter sequence should be negated before return
#' @param ... parameters passed on to \code{seq}
#' @return sequence from \code{from} to \code{to}
# #' @examples
# #' \dontrun{
# #' Seq(1, 10, ascending = TRUE)
# #' Seq(1, 10, descending = TRUE)
# #' Seq(10, 1, ascending = NA)
# #' }
Seq = function(from, to, ..., ascending = TRUE, descending = !ascending, neg = FALSE) {
	# <!> order matters: if called with only descending == TRUE
	if (nif(descending) && to > from) return(if (neg) TRUE else c()) else
	if (nif(ascending) && from > to) return(if (neg) TRUE else c());
	s = seq(from, to, ...);
	r = if (neg) -s else s;
	r
}
SeqRows = function(o)Seq(1, nrow(o))

#' Produce index pairs for vector of counts
#'
#' @param counts vector of integers specifying counts
#' @return vector of pairs of indeces indicating the first and last element in a vector for the blocks 
#'  specified by \code{counts}
#' @keywords internal
# #' @examples
# #' \dontrun{
# #' count2blocks(c(1, 5, 3))
# #' }
count2blocks = function(counts) {
	ccts = cumsum(counts);
	fidcs = c(1, ccts[-length(ccts)] + 1);
	blks = as.vector(rbind(fidcs, fidcs + counts - 1));
	blks
}

#
#	expand a block list - for example as from count2blocks - to a list of integers
#
expandBlocks = function(blks) {
	applyL(matrix(blks, ncol = 2, byrow = TRUE), 1, function(r) { r[1]:r[2] } )
}

# split 1:M into N partitions, return row-wise range
splitListIndcs = function(M, N = 1, .compact = FALSE, .truncate = TRUE) {
	if (.truncate & M < N) N = M;
	if (.compact) {
		n = rep(ceiling(M / N), N);	# size of parts
		idcs = c(0, cumsum(n));
		idcs = idcs[idcs < M];
		idcs = c(idcs, M);
	} else {
		n = rep(floor(M / N), N);		# size of parts
		R = M - n[1] * N;
		n = n + c(rep(1, R), rep(0, N - R));
		idcs = c(0, cumsum(n));
	}
	idcs = cbind(idcs + 1, c(idcs[-1], 0))[-length(idcs), ];	# from:to in a row
	# <!> usual R degeneracy
	if (!is.matrix(idcs)) idcs = matrix(idcs, nrow = 1);
	idcs
}
splitListEls = function(l, N, returnElements = FALSE) {
	idcs = splitListIndcs(length(l), N);
	li = apply(idcs, 1, function(r)(if (returnElements) l[r[1]:r[2]] else r[1]:r[2]));
	# <!> R ambiguity of apply return type
	if (is.matrix(li)) li = lapply(1:(dim(li)[2]), function(i)li[, i]);
	if (is.vector(li)) li = as.list(li);;
	li
}

# @param l list of index positions from another object
# @return return vector indicating to which list element an index was assigned
# Example: glmnet accepts fold numbers per index (as opposed to a partitioning of elements)
index2listPosition = function(l) {
	N = sum(sapply(l, length));
	na = rep(NA, N);
	m = sapply(1:length(l), function(i)vector.assign(na, l[[i]], i, na.rm = NA));
	r = apply(m, 1, na.omit);
	r
}

# idcs start positions in ragged list, converted to ranges
idcsStart2range = function(idcs, N = max(idcs)) {
	if (length(idcs) == 0) return(NULL);
	vector.intercalate(idcs, c(shift(idcs - 1), N))
}

# splitting based on fractions
# voting percentages to seats
#	simple algorithm based on size of residuals
# tiePreferHigh: for tied residuals add/subtract seats to high indeces (TRUE) or low ones (FALSE)
splitSeatsForFractions = function(Nseats, fractions = vn(rep(1, Nfractions)), Nfractions,
	tiePreferHigh = TRUE) {
	# number of parties
	Nparties = length(fractions);
	# fractional seats
	Nseats0 = fractions * Nseats;
	# garuantee one seat, otherwise round to nearest
	Nseats1 = ifelse (Nseats0 < 1, 1, round(Nseats0));
	# individual mismatch
	Nresid = Nseats0 - Nseats1;
	# mismatch total
	diff = sum(Nseats1) - Nseats;
	# redistribute deficit/overshoot
	if (diff != 0) {
		Nresid1 = ifelse(Nresid < 0, 1, Nresid);	# too few vs too many, too few -> maximal value of 1
		# take seats from whom? We need abs(diff) seats.
		#subtr = order(Nresid1, decreasing = diff < 0)[1:abs(diff)];
		prio = if (tiePreferHigh) 1:Nparties else rev(1:Nparties);
		subtr = Order(Df(Nresid1, prio))[1:abs(diff)];
		# assume one round of correction is always sufficient <!>
		Nseats1[subtr] = Nseats1[subtr] - sign(diff);
	}
	Nseats1
}

# tranform number of elements (as from splitSeatsForFractions) into from:to per row in a matrix
counts2idcs = function(counts) {
	idcs = c(0, cumsum(counts));
	idcs = cbind(idcs + 1, c(idcs[-1], 0))[-length(idcs), ];
	if (is.null(counts)) return(idcs);	# matrix w/ 0 rows
	t2r(idcs)	# fails on counts == NULL
}

# N is partitioned into fractions from p, where each element of p partitions the remaining part of N
# procedure makes sure to leave space for length(p) elements
cumpartition = function(N, p) {
	I = c();	# indeces within 1:N
	for (i in 1:length(p)) {
		# partition remaining space (ifelse), leave room for subsequent indeces
		Ii = floor(p[i] * (ifelse(i == 1, N, N - I[i - 1]) - (length(p) - i))) + 1;
		I = c(I, ifelse(i == 1, Ii, I[i - 1] + Ii));
	}
	as.integer(I)
}

#' Extract parts of a nested structure based on the range from..to
#'
#'
#' @param Ns Vector of integers that specify the size of the substructures
#' @param from absolute index where to start extraction
#' @param to absolute index where to stop extraction
#' @return Return list of lists, where each basic list contains key \code{segment}
#'  (which of the elements of Ns) and key \code{range}, a list with elements \code{from} and \code{to},
#'  specifying which elements to use from
#'  that segment.
# #' @examples
# #' \dontrun{
# #'    # TestMe: TRUE1
# #'    subListFromRaggedIdcs(c(2, 4, 10, 15), 1, 20)
# #' }
subListFromRaggedIdcs = function(Ns, from = 1, to) {
	NsCS = cumsum(Ns);
	NsCSs = c(0, pop(NsCS));	# shifted cumsum
	segments = which(from <= NsCS & to > NsCSs);
	if (missing(to)) to = sum(segments);
	r = lapply(segments, function(segment){
		N = Ns[segment];	# list-call
		from_ = 1;
		to_ = N;
		if (segment == segments[1]) from_ = from - NsCSs[segment];
		if (segment == rev(segments)[1]) to_ = to - NsCSs[segment];
		r = list(segment = segment, range = list(from = from_, to = to_));
		r
	});
	r
}

#' Extract parts of nested lists based on the range from..to
#'
#'
#' @param from absolute index where to start extraction
#' @param to absolute index where to stop extraction
#' @param ls nested list structure (currently only two levels supported)
#' @return Return list of list, where each basic list contains key \code{segment}
#'  (which of the elements of Ns) and key \code{range}, a list with elements \code{from} and \code{to},
#'  specifying which elements to use from
#'  that segment.
subListFromRaggedLists = function(ls, from = 1, to = sum(sapply(ls, length))) {
	sl = subListFromRaggedIdcs(sapply(ls, length), from = from, to = to);
	r = lapply(sl, function(s) with(s, {
		r = ls[[segment]][range$from: range$to];
		r
	}));
	r = unlist.n(r, 1);
	r
}


#
#	<§> vector functions
#

# does the position exists in vector v
exists.pos = function(v, i)(is.vector(v) && !is.na(v[i]))

# for a vector blocked by blockSize N, return indeces of elements of block i
rangeBlock = function(i, N)(((i - 1)*N + 1):(i * N))

#
#	<par> lists
#

merge.lists = function(..., ignore.nulls = TRUE, listOfLists = FALSE, concat = FALSE, useIndeces = FALSE) {
	lists = if (listOfLists) c(...) else list(...);
	l1 = lists[[1]];
	if (length(lists) > 1) for (i in 2:length(lists)) {
		l2 = lists[[i]];
		ns = if (useIndeces) 1L:length(l2) else names(l2);
		for(n in ns) {
			if (is.null(n)) print("Warning: tried to merge NULL key");
			if (!is.null(n) & (!ignore.nulls | !is.null(l2[[n]]))) {
				if (concat) l1[[n]] = c(l1[[n]], l2[[n]]) else l1[[n]] = l2[[n]];
			}
		}
	}
	l1
}

merge.lists.recursive = function(..., ignore.nulls = TRUE, listOfLists = FALSE, concat = FALSE) {
	lists = if (listOfLists) c(...) else list(...);
	l1 = lists[[1]];
	if (length(lists) > 1) for (i in 2:length(lists)) {
		l2 = lists[[i]];
		for(n in names(l2)) {
			if (is.null(n)) print("Warning: tried to merge NULL key");
			if (!is.null(n) & (!ignore.nulls | !is.null(l2[[n]])))
				l1[[n]] = if (is.list(l1[[n]]))
					merge.lists.recursive(l1[[n]], l2[[n]]) else
					(if (concat) c(l1[[n]], l2[[n]]) else l2[[n]])
		}
	}
	l1
}

unshift = function(l, listOfList = TRUE) {
	if (!listOfList) l = list(l);
	e1 = lapply(l, function(l0)if (is.list(l0)) l0[[1]] else l0[1]);
	r1 = lapply(l, function(l0)l0[-1]);
	r = list(elements = e1, remainder = r1);
	r
}

Merge.lists.raw = function(lists, ignore.nulls = TRUE, recursive = FALSE, keys = NULL) {
	if (!is.null(keys)) keys = unshift(keys);

	l1 = lists[[1]];
	if (length(lists) > 1) for (i in 2:length(lists)) {
		l2 = lists[[i]];
		for(n in names(l2)) {
			if (is.null(n)) print("Warning: tried to merge NULL key");
			if (!is.null(n) & (!ignore.nulls | !is.null(l2[[n]])))
				l1[[n]] = if (recursive && is.list(l1[[n]]) && (is.null(keys) || n %in% keys$elements))
					Merge.lists.raw(list(l1[[n]], l2[[n]]), ignore.nulls, recursive,
						if (is.null(keys)) NULL else keys$remainder) else
					l2[[n]]
		}
	}
	l1
}

Merge.lists = function(..., ignore.nulls = TRUE, listOfLists = FALSE, recursive = FALSE, keyPathes = NULL) {
	lists = if (listOfLists) c(...) else list(...);
	keys = if (!is.null(keyPathes)) splitString("[$]", keyPathes, simplify = FALSE) else NULL; 
	l = Merge.lists.raw(lists, ignore.nulls = ignore.nulls, recursive = recursive, keys = keys);
	l
}

# l: list of lists
# take parallel elements from l (1, ...) after recycling
list.combine = function(l, byRow = TRUE, names = NULL, doMerge = FALSE) {
	lR = Recycle(l, byRow = byRow);
	# <p> number of final elements
	N =	length(lR[[1]]);
	lC = lapply(1:N, function(i) {
		lol = list.kp(lR, Sprintf('[[%{i}d]]'));
		if (notE(names)) names(lol) = names;
		return(if (doMerge) merge.lists(lol, listOfLists = TRUE) else lol);
	});
	return(lC);
}
# inverse of unlist.n(, 1)
list.embed = function(l, key = 'key')lapply(l, function(e)SetNames(list(e), key));

# use.names preserves names and concatenates with lower level names
# reset sets names to top level names
unlist.n = function(l, n = 1, use.names = TRUE, reset = FALSE) {
	if (n > 0) for (i in 1:n) {
		ns = names(l);
		#names(l) = rep(NULL, length(l));	# <!> untested removal Tue Oct 19 17:11:53 2010
		l = unlist(l, recursive = FALSE, use.names = use.names);
		if (reset) names(l) = ns;
	}
	l
}

# <N> obsolete, better: with(l, { ...})
instantiate.list = function(l, n = 1) {
	for (nm in names(l)) {
 		eval.parent(parse(file = "", text = sprintf("%s = %s", nm, deparse(l[[nm]]))), n = n);
# 		if (is.integer(l[[nm]])) {
# 			eval.parent(parse(file = "", text = sprintf("%s = %d", nm, l[[nm]])), n = n);
# 		} else if (is.numeric(l[[nm]])) {
# 			eval.parent(parse(file = "", text = sprintf("%s = %f", nm, l[[nm]])), n = n);
# 		} else {
# 			eval.parent(parse(file = "", text = sprintf("%s = \"%s\"", nm, l[[nm]])), n = n);
# 		};
	}
}
# for use in testing code
instantiate = function(l, ..., envir = parent.frame()) {
	l0 = c(l, list(...));
	for (i in seq_along(l0)) assign(names(l0)[i], l0[[i]], envir = envir);
	invisible(l0)
}

# assume a list of lists (aka vector of dicts) and extract a certain key from each of the lists
list.key = function(v, key, unlist = TRUE, template = NULL, null2na = FALSE) {
	l = lapply(v, function(i){
		if (is.list(i)) {
			if (is.null(i[[key]])) { if (null2na) NA else NULL } else i[[key]]
		} else template});
	if (unlist) l = unlist(l);
	l
}

# iterative gsub
# substs: list with pairs re, sub
gsubI = function(substs, s, ...) {
	for (r in substs) s = gsub(r[[1]], r[[2]], s, ...);
	s
}

# concatenate lists, leave out lists only containing a single NULL
cList = function(...) {
	r = list(...);
	isNull = sapply(r, is.null);
	do.call(c, r[!isNull])
}

keyPathParse = function(kp) {
	kp = gsubI(
		list(c('\\*', '.'), c('\\[\\[(\\d+)\\]\\]', 'INDEX__\\1'), c('(\\$|^)\\s*[(]', '\\1PAR__('))
	, kp);
	Parse(kp)
}
keyPathExpression2key = function(e) {
	s = as.character(e);
	i = FetchRegexpr('INDEX__(\\d+)', s, captures = TRUE);
	r = if (s == '.') '*' else
		if (s == 'PAR__') NULL else
		if (!is.null(i)) return(as.integer(i)) else s;
	if (is.null(r)) NULL else list(r)
}

# level: level of nesting for parallel key pathes
# output: list or vector, a parallel pattern induces two more levels of list nesting
#	the parallel keyPathes each of which is forced to be a list
keyPathAstRaw = function(e, level = 0) {
	r = if (is.call(e)) {
		isPar = e[[1]] == '|';
		isPlain = e[[1]] == '$';
		levelN = ifelse(isPar, level + 1, 0);
		# simple walk through
		r = if (isPlain | isPar)
			cList(keyPathAstRaw(e[[2]], level = levelN), keyPathAstRaw(e[[3]], level = levelN))
		# this is a fake call generated by the PAR__ construct, the path leading to PAR__ is seen as an
		#	anonymous function
		else cList(keyPathAstRaw(e[[1]]), list(keyPathAstRaw(e[[2]], level = 1)));
		if (level & isPlain) list(r) else r
	} else if (is.name(e)) keyPathExpression2key(e) else {
		stop('malformed keyPath');
	}
	r
}

keyPathAst = function(kp) {
	unlist.n(keyPathAstRaw(keyPathParse(kp)[[1]]), n = 0);
}

list.kp.unquote = function(key) {
	# un-quote: remove single backslashes
	key = sub('(?<![\\\\])[\\\\](?![\\\\])', '', key, perl = TRUE);
	# de-quote: double backslashes become single backslashes
	key = sub('\\\\', '\\', key, fixed = TRUE);
	as.character(key)
}

# extract key path from list, general, recursive version
#	key path recursive worker
list.kprw = function(l, keys, unlist.pats, template, null2na, carryNames, test, keyAccess) {
	if (!length(keys)) return(l);
	key = keys[1];
	# <p> extract key
	r = if (key != "*") {
		index = fetchRegexpr("\\A\\[\\[(\\d+)\\]\\]\\Z", key, captures = TRUE);
		if (length(index) > 0) key = as.integer(index[[1]]);
		if (keyAccess[1] == '@') {
			r = slot(l, key);
			list.kprw(r, keys[-1], unlist.pats[-1], template, null2na, carryNames, test, keyAccess[-1]);
		} else if (is.list(l)) {
			# <N> logical(0) seen as NULL by second condition
			r = if (is.null(l[[key]]) || length(l[[key]]) == 0) {
					if (null2na) { NA } else firstDef(template, NULL)
				} else l[[key]];
			if (length(keys) > 1)
				list.kprw(r, keys[-1], unlist.pats[-1], template, null2na, carryNames, test, keyAccess[-1]) else
				if (test) !(is.null(r) || all(is.na(r))) else r;
		} else if (class(l) %in% c('character')) {
			if (notE(names(l))) l[names(l) %in% key] else l[key]
		} else if (class(l) %in% c('data.frame', 'matrix')) {
			l[, key]
		} else if (class(l) %in% c('numeric', 'integer')) {
			l[key]
		} else return(template);
# 		{
# 			r = template;
# 			attr(r, 'names') = keys[last(keys)];
# 			print(c(keys, r));
# 			return(r);
# 		}
	} else {
		if (length(keys) > 1)
			lapply(l, function(sl)
				list.kprw(sl, keys[-1], unlist.pats[-1], template, null2na, carryNames, test, keyAccess[-1])
			) else l;
	}
	# <p> unlisting
	if (notE(unlist.pats)) if (unlist.pats[1]) r = unlist.n(r, 1, reset = carryNames);
	r
}

# extract key path from list, general, recursive version
#	key path recursive worker: parallel keys
#	iterate over recursive keys
list.kprwPar = function(l, keys, ...) {
	key = keys[1];
	r = if (length(fetchRegexpr("\\|", key)) > 0) {
		parKeys = sapply(splitString('\\|', key), list.kp.unquote);
		r = lapply(parKeys, function(key)list.kprwkp(l, c(key, keys[-1]), ...));
		unlist.n(r, 1);
	} else list.kprw(l, keys, ...);
	r
}

# worker: keypath
list.kprwkp = function(l, keyPath, ..., keyAccess) {
	keysNew = fetchRegexpr("(?:[a-zA-Z0-9_.|\\[\\]*]+(?:\\\\[$@])?)+", keyPath[1]);
	keys = c(keysNew, keyPath[-1]);
	r = list.kprwPar(l, keys, ...);
	r
}

list.kp.keys = function(keyPath) fetchRegexpr("[^$@]+", keyPath);
list.kp.method = function(keyPath) fetchRegexpr("[$@]", keyPath);

# wrapper for list.kprw
# keyPath obeys EL1 $ EL2 $ ..., where ELn is '*' or a literal
# unlist.pat is pattern of truth values TR1 $ TR2 $..., where TRn is in 'T|F' and specifies unlist actions
# carryNames determines names to be carried over from the top level in case of unlist
list.kpr = function(l, keyPath, do.unlist = FALSE, template = NULL,
	null2na = FALSE, unlist.pat = NULL, carryNames = TRUE, as.matrix = FALSE, test = FALSE) {
	keys = list.kp.keys(keyPath);
	# list or slot?
	keyAccess = list.kp.method(keyPath);
	# if first element is '*', assume list
	if (length(keyAccess) < length(keys)) keyAccess = c('$', keyAccess);
	unlist.pats = if (notE(unlist.pat)) as.logical(fetchRegexpr("[^$]+", unlist.pat)) else NULL;

	# parallel keys
	#r = list.kprwkp(l, keyPath, unlist.pats, template, null2na, carryNames, test = test);
	r = list.kprw(l, keys, unlist.pats, template, null2na, carryNames, test = test, keyAccess = keyAccess);
	if (do.unlist) { r = unlist(r); }
	if (as.matrix) r = t(sapply(r, function(e)e));
	r
}
# extract key path from list
# <!> interface change: unlist -> do.unlist (Wed Sep 29 18:16:05 2010)
# test: test existance instead of returning value
list.kp = function(l, keyPath, do.unlist = FALSE, template = NULL, null2na = FALSE, test = FALSE, n,
	pathAsIs = FALSE) {
	fullPath = if (pathAsIs) keyPath else sprintf("*$%s", keyPath);
	r = list.kpr(l, fullPath, do.unlist = do.unlist,
		template = template, null2na = null2na, test = test);
	if (!missing(n)) r = unlist.n(r, n);
	r
}

list.kpu = function(..., do.unlist = TRUE)list.kp(..., do.unlist = do.unlist);
# allow for slot access
list.Kpu = function(..., do.unlist = TRUE)list.kp(..., do.unlist = do.unlist, pathAsIs = TRUE);

list.keys = function(l, keys, default = NA) {
	l = as.list(l);
	r = lapply(unlist(keys), function(key) if (is.null(l[[key]])) default else l[[key]]);
	r
}

# make A > B into B > A
listReverseHierarchy = function(l, unlist = FALSE) {
	ns = names(l[[1]]);
	r = lapply(ns, function(n)list.kp(l, n, do.unlist = unlist));
	names(r) = ns;
	return(r);
}

# return pair of lists: keys to access element, value
list.flatten_raw = function(lol, prefix = list(), ignoreIndeces = TRUE) {
	#ns = names(lol);
	#if (is.null(ns)) ns = 1:length(lol);
	# <A> duplicate names, handled by nelapply
	r = unlist.n(Nelapply(lol, function(n, e) {
		# generate list of lists, each entry being list(key path), value
		if (is.list(e)) {
			prefix = if (ignoreIndeces && is.integer(n)) prefix else c(prefix, list(n));
			list.flatten_raw(e, prefix = prefix)
		} else list(list(c(prefix, list(n)), e));
	}), 1);
	return(r);
}

# return flat list with keys representing keyPath
#	alternative: return pair with keys, values, keys joined or list
list.flatten = function(lol, joinby = NULL, aspairs = TRUE, ignoreIndeces = TRUE) {
	r = list.flatten_raw(lol, ignoreIndeces = ignoreIndeces);
	if (aspairs && is.null(joinby)) return(r);
	keys = list.kp(r, '[[1]]');
	values = list.kp(r, '[[2]]');
	if (is.null(joinby)) return(list(keys = keys, values = values));
	return(listKeyValue(sapply(keys, join, sep = joinby), values));
}


# analogous to list.merge; instead of merging, append entries
list.accrue = function(lol, sep = ':', ignoreIndeces = TRUE) {
	es = list.flatten(lol, joinby = sep, ignoreIndeces = ignoreIndeces);
	ns = names(es)
	r = list();
	for (i in seq_along(es)) {
		r[[ns[i]]] = union(r[[ns[i]]], es[[i]])
	}
	return(r);
}

null2na = function(l) {
	if (!length(l)) return(l);
	l[sapply(l, is.null)] = NA;
	return(l);
}


# return list without listed keys
list.min  = function(l, keys) {
	l[-which.indeces(keys, names(l))]
}
# list generation on steroids (wraps other functions)
.list = function(l, .min = NULL) {
	if (!is.null(.min)) l = list.min(l, .min);
	l
}
# get apply
gapply = function(l, key, unlist = FALSE)list.key(l, key, unlist)
# construct list as a dictionary for given keys and values
listKV = listKeyValue = function(keys, values, doRecycle = TRUE) {
	if (length(keys) != length(values) && doRecycle) {
		r = recycle(keys, values);
		keys = r[[1]];
		values = r[[2]];
	}
	if (length(keys) != length(values))
		stop("listKeyValue: number of provided keys does not match that of values");

	l = as.list(values);
	names(l) = keys;
	l
}
listNamed = function(l, names)setNames(l, names)
vectorNamed = function(v, names) {
	if (length(names) > length(v)) stop("vectorNamed: more names than vector elements");
	names(v) = names;
	v
}

vn = vectorNormed = function(v, type = 'O') {
	v0 = as.matrix(v);
	v0n = apply(v0, 2, function(v)norm(as.matrix(v), type = type));
	r = if (!is.matrix(v)) v/v0n else sapply(1:ncol(v), function(i) v[, i] / v0n[i]);
	r
}

#listInverse = function(l)listKeyValue(avu(l), names(l));
listInverse = function(l, toNA = FALSE) {
	n = sapply(l, length);
	# <p> values of inverse map
	vs = rep.each(names(l), n);
	# <p> construct list
	r = listKeyValue(avu(l, recursive = FALSE, toNA = toNA), vs);
	r
}

# name the list elements by the iterated vector elements ns (names)
nlapply = function(ns, f, ...) {
	if (is.list(ns)) ns = names(ns);
	r = lapply(ns, f, ...);
	names(r) = ns;
	r
}
nelapply = function(l, f, ..., name = '') {
	ns = names(l);
	if (is.null(ns)) ns = ( if (notE(name)) rep(name, length(l)) else seq_along(l) );
	r = lapply(seq_along(l), function(i, ...)f(ns[i], l[[i]], ...), ...);
	names(r) = ns;
	r
}
Nelapply = function(l, f, ..., name = NULL)nelapply(l, f, ..., name = name)

ilapply = function(l, f, ...) {
	r = lapply(1:length(l), function(i)f(l[[i]], i, ...));
	if (!is.null(names(l))) names(r) = names(l);
	r
}
einXapply = function(v, f, ..., einXapplyIterator = lapply) {
	l = as.list(v);
	ns = names(l);
	r = einXapplyIterator(seq_along(l), function(i)f(l[[i]], i, ns[i], ...));
	if (length(r) > 0) names(r) = ns;
	r
}

# pass element, index, name
einlapply = function(l, f = Identity, ...)einXapply(l, f, ..., einXapplyIterator = lapply);

# pass element, index
eilapply = function(l, f, ...) {
	r = lapply(seq_along(l), function(i)f(l[[i]], i, ...));
	names(r) = names(l);
	r
}
eisapply = function(v, f, ...) {
	l = as.list(v);
	r = sapply(seq_along(l), function(i)f(l[[i]], i, ...));
	names(r) = names(v);
	r
}
ensapply = function(l0, f, ...) {
	l = as.list(l0);
	ns = names(l);
	r = sapply(seq_along(l), function(i, ...)f(l[[i]], ns[i], ...), ...);
	names(r) = ns;
	r
}
einsapply = function(v, f = Identity, ...)einXapply(v, f, ..., einXapplyIterator = sapply)

kvlapply = function(l, f, ...) {
	ns = names(l);
	r = lapply(1:length(l), function(i)f(ns[i], l[[i]], ...));
	names(r) = ns;
	r
}
pairsapply = pairsapplyVL = function(l1, l2, f, ..., simplify = TRUE, USE.NAMES = TRUE) {
	if (length(l1) != length(l2)) stop('pairsapply: pair of collections of unequal length.');
	r = sapply(seq_along(l1), function(i)f(l1[i], l2[[i]], ...),
		simplify = simplify, USE.NAMES = USE.NAMES);
	r
}
pairsapplyLV = function(l1, l2, f, ..., simplify = TRUE, USE.NAMES = TRUE) {
	if (length(l1) != length(l2)) stop('pairsapply: pair of collections of unequal length.');
	r = sapply(seq_along(l1), function(i)f(l1[[i]], l2[i], ...),
		simplify = simplify, USE.NAMES = USE.NAMES);
	r
}
pairslapply = function(l1, l2, f, ...) {
	if (length(l1) != length(l2)) stop('pairslapply: pair of collections of unequal length.');
	r = lapply(seq_along(l1), function(i)f(l1[[i]], l2[[i]], ...));
	names(r) = names(l1);
	r
}

# d: data.frame, v: row to pick per colum
GetCombination = function(d, v)sapply(1:length(v), \(.)d[[.]][v[.]])
applyCartesian = function(l, f, ...) {
	idcs = merge.multi.list(lapply(l, \(.)1:length(.)));
	nsL = if (notE(names(l))) names(l) else rep('', length(l));
	ns0 = eisapply(l, \(., i)if (is.character(.)) . else paste0(nsL[i], 1:length(.)));
	ns = t(apply(idcs, 1, \(.)join(GetCombination(ns0, avu(.)), ':')));
	r = apply(idcs, 1, \(i) do.call(f, List.takenFrom(l, i)), simplify = FALSE);
	return(SetNames(r, ns));
}

sapplyWoI = function(v, f, ...)sapply(v, function(i, ...)f(...), ...)
lapplyWoI = function(v, f, ...)lapply(v, function(i, ...)f(...), ...)

dfapply = function(Df__, f__) {
	r = lapply(1:nrow(Df__), function(i) {
		r = Df__[i, ];
		return(Df_(f__(as.list(r))));
	});
	Dfr = do.call(rbind, r);
	return(Dfr);
}

list.grep = sublist = filterList = function(o, f, ...) {
	l = if (!is.function(f)) f else sapply(o, f, ...);
	if (length(l) == 0) l = NULL;	#list corner case
	r = o[l];
	return(r);
}



# <i> copy MARGIN handling from apply (aperm)
lapplyDir = function(m, MARGIN, f_, ..., drop = FALSE) {
	selector = if (MARGIN == 1)
		function(m, i)m[i, , drop = drop] else
		function(m, i)m[, i, drop = drop];
	setNames(lapply(1:dim(m)[MARGIN], function(i)f_(selector(m, i), ...)), Dimnames(m, MARGIN))
}

# <!> as matrix to avoid warning
#lapplyRows = function(m, ...)lapply(split(as.matrix(m), row(m)), ...)
# lapplyRows = function(m, f_, ..., drop = FALSE)
# 	setNames(lapply(1:nrow(m), function(i)f_(m[i, , drop = drop], ...), ...), Row.names(m))
lapplyRows = function(m, f_, ..., drop = FALSE)lapplyDir(m, 1, f_ = f_, ..., drop = drop)
lapplyCols = function(m, f_, ..., drop = FALSE)lapplyDir(m, 2, f_ = f_, ..., drop = drop)

getElement = function(v, i)if (is.list(v)) v[[i]] else v[i];
# unify w/ list.takenFrom -> tests
List.takenFrom = function(listOfLists, v)
	lapply(1:length(listOfLists), function(j)getElement(listOfLists[[j]], v[j]));
# tuple-apply
tuapply = function(..., fct = Identity, args = list(), names = NULL) {
	tupels = list(...);
	M = length(tupels);
	Ns = sapply(tupels, length);
	N = Ns[1];
	if (any(Ns != N)) stop('Indexable elements not of same length');
	r = lapply(1:N, function(i)do.call(fct, c(List.takenFrom(tupels, rep(i, M)), args)));
	if (is.null(names) && !is.null(base::names(tupels[[1]]))) names = base::names(tupels[[1]]);
	if (!is.null(names)) base::names(r) = names;
	r
}

undrop2row = function(e)(if (is.vector(e)) matrix(e, ncol = length(e)) else e);
Lundrop2row = function(l)lapply(l, undrop2row);

undrop2col = function(e)(if (is.vector(e)) matrix(e, nrow = length(e)) else e);
Lundrop2col = function(l)lapply(l, undrop2col);

# return list from apply (undo simplify)
applyL = function(X, MARGIN, FUN, ...) {
	r = apply(X, MARGIN, FUN, ...);
	if (is.matrix(r)) return(lapply(1:ncol(r), function(i)r[, i]));
	if (!is.list(r) && is.vector(r)) return(lapply(1:length(r), function(i)r[i]));
	return(r);
}
# USE.NAMES logic reversed for sapply
sapplyn = function(l, f, ...)sapply(l, f, ..., USE.NAMES = FALSE);
list.with.names = function(..., .key = 'name') {
	l = list(...);
	ns = names(l);
	r = nlapply(l, function(n) c(l[[n]], listKeyValue(.key, n)));
	r
}

#
#	<p> names
#

Row.names = function(o, vivify = TRUE) {
	rn = row.names(o);
	if (is.null(rn) && vivify) 1:nrow(o) else rn
}
Col.names = function(o, vivify = TRUE) {
	rn = if (is.matrix(o)) dimnames(o)[[2]] else names(o);
	if (is.null(rn) && vivify) 1:ncol(o) else rn
}
# <i> implement general MARGINs
Dimnames = function(o, MARGIN, vivify = TRUE) {
	if (MARGIN == 1) Row.names(o, vivify) else Col.names(o, vivify)
}

SetNames = function(o, names, rnames, cnames, Dimnames, embed = FALSE) {
	if (!missing(Dimnames)) dimnames(o) = Dimnames;
	if (!missing(rnames)) row.names(o) = rnames;
	if (!missing(cnames)) dimnames(o)[[2]] = cnames;
	if (!missing(names)) {
		if (any(class(o) == 'matrix')) {
			if (embed) dimnames(o)[[2]][seq_along(names)] = names else {
				ns = if (is.list(names)) vector.replace(dimnames(o)[[2]], names) else names;
				if (is.null(dimnames(o))) dimnames(o) = list(NULL, ns) else dimnames(o)[[2]] = ns;
			}
		} else {
			if (embed) names(o)[seq_along(names)] = names else {
				names(o) = if (is.list(names)) vector.replace(names(o), names) else names;
			}
		}
	}
	o
}


#
#	<p> attributes
#

Attr = function(o, plus_, min_ = NULL) {
	if (!missing(plus_)) for (n in names(plus_)) { attr(o, n) = plus_[[n]]; }
	if (Nif(min_)) for (a in min_) { attr(o, a) = NULL; }
	o
}

#
#	<par> data type conversions
#

# assure m has at least 1 column
to.col = function(m) { if (is.null(dim(m))) t(t(m)) else m }
col.frame = function(l, col.name = 'value', minus = NULL, ignore.null = TRUE,
	do.paste = NULL, do.format = TRUE, digits = 3, plus = NULL) {
	if (ignore.null) { for (n in names(l)) { if (is.null(l[[n]])) l[[n]] = NULL; } }
	if (!is.null(minus)) { for (n in minus) { l[[n]] = NULL; } }
	my.names = if (!is.null(plus)) plus else names(l);
	digits = if (length(digits) > 1) digits else rep(digits, length(l));
	if (!is.null(do.paste)) {
		if (do.format) {
			i = 1;
			for (n in my.names) { if (is.vector(l[[n]])) {
				l[[n]] = paste(sapply(l[[n]],
						function(e){if (is.numeric(e)) sprintf("%.*f", digits[i], e) else e}
					), collapse = do.paste)
				i = i + 1;
			}}
		} else {
			for (n in my.names) { if (is.vector(l[[n]])) l[[n]] = paste(l[[n]], collapse = do.paste) }
		}
	}
	f = as.data.frame(l);
	if (dim(f)[2] > length(col.name) && length(col.name) == 1)
		row.names(f) = paste(col.name, 1:dim(f)[1], sep = "")
	else row.names(f) = c(col.name);
	t(f)
}

# <i> collect recursively until list or data.frame
# convert list of lists to data frame (assuming identical keys for each sub list)
#	also works on list of vectors
listOfLists2data.frame = function(l, idColumn = "id", .names = NULL) {
	# collect keys
	keys = if (is.list(l[[1]]))
		sort(unique(as.vector(unlist(sapply(l, function(e)names(e)))))) else 1:length(l[[1]]);
	if (is.null(.names)) .names = keys;
	# row names
	rows = names(l);
	if (is.null(rows)) rows = 1:length(l);
	# build df

	#df = t(sapply(rows, function(r) { unlist(l[[r]][keys]) }));
	df = t(sapply(rows, function(r)list2df(l[[r]], keys)));
	df = if (!is.null(idColumn)) {
		data.frame.types(data.frame(..idColumn.. = rows, df),
			row.names = 1:length(rows), names = c(idColumn, .names));
	} else {
		data.frame.types(df, row.names = rows, names = .names);
	}
	df
}

# resetColNames: reset column names to names of first data frame
# colsFromFirstDf: take columns from the first data frame
# <i> improved algorithm: unlist everything, bind together: cave: data types,
#	strictly valid only for matrices
# Use cases:
#	list with named vectors: get data frame that contains all vectors with all possible names represented
#		listOfDataFrames2data.frame(cfs, colsFromUnion = TRUE, do.transpose = TRUE, idColumn = NULL);
listOfDataFrames2data.frame = function(l, idColumn = "id", do.unlist = TRUE, direction = rbind,
	resetColNames = TRUE, colsFromFirstDf = FALSE, colsFromUnion = FALSE, do.transpose = FALSE, idAsFactor = FALSE,
	row.names = FALSE) {
	# row names
	# <!> 2009-11-20 changed from: rows = firstDef(names(l), list(1:length(l)));
	rows = firstDef(names(l), 1:length(l));
	# columns
	ns = NULL;
	if (colsFromUnion) {
		ns = unique(unlist(lapply(l, names)));
		# get data.frame names
		ns = names(do.call(data.frame, listKeyValue(ns, rep(NA, length(ns)))));
		resetColNames = FALSE;	# <!> mutually exclusive
	}
	# build df
	df = NULL;
	for (i in 1:length(rows)) {
		if (is.null(l[[i]])) next;	# ignore empty entries
		# <p> force to data frame
		df0 = if (do.transpose) as.data.frame(t(l[[i]])) else as.data.frame(l[[i]]);
		# <p> homogenize columns
		if (colsFromUnion) {
			# add missing columns
			ns0 = setdiff(ns, names(df0));
			df0 = do.call(data.frame, c(list(df0), listKeyValue(ns0, rep(NA, length(ns0)))));
			# correct order of columns
			df0 = df0[, ns];
		}
		if (!is.null(df)) {
			if (colsFromFirstDf) df0 = df0[, names(df)] else
			if (resetColNames) {
				names(df0) = if (is.null(idColumn)) names(df) else names(df)[-1];
			}
		}
		# <p> add id column
		df0 = if (is.null(idColumn)) df0 else cbind(rep(rows[i], dim(df0)[1]), df0);
		# <A> case differentiation should not me necessary
		df = if (i == 1) df0 else direction(df, df0);
	}
	if (!is.null(idColumn)) names(df)[1] = idColumn;
	if (do.unlist) for (n in names(df)) { df[[n]] = unlist(df[[n]]); }
	if (idAsFactor) df[[idColumn]] = as.factor(df[[idColumn]]);
	if (!row.names) row.names(df) = NULL;
	df
}
cbindDataFrames = function(l, do.unlist = FALSE, colsFromUnion = FALSE) {
	listOfDataFrames2data.frame(l, idColumn = NULL, do.unlist = do.unlist, direction = cbind,
		resetColNames = FALSE, colsFromUnion = colsFromUnion)
}
# @param embed corresponds to colsFromUnion in listOfDataFrames2data.frame
RbindDfs = function(dfl, namesFromFirst = TRUE, embed = FALSE) {
	if (namesFromFirst && !embed) dfl = lapply(dfl, setNames, nm = names(dfl[[1]]));
	if (embed) {
		ns = unique(unlist(sapply(dfl, names)));
		df0 = Df_(listKeyValue(ns, rep(NA, length(ns))));
		dfl = lapply(dfl, function(d)cbind(d, df0[, setdiff(ns, names(d)), drop = FALSE]));
	}
	do.call(rbind, dfl)
}

rbindDataFrames = function(l, do.unlist = FALSE, useDisk = FALSE, idColumn = NULL, transpose = FALSE,
	resetColNames = FALSE, colsFromFirstDf = FALSE, idAsFactor = FALSE) {
	r = if (useDisk) {
		tempTable = tempfile();
		for (i in 1:length(l)) {
			d0 = l[[i]];
			if (class(d0) != 'data.frame') d0 = as.data.frame(d0);
			if (transpose) d0 = t(d0);
			if (!is.null(idColumn)) {
				d0 = data.frame(idColumn = names(l)[i], d0);
				names(d0)[1] = idColumn;
			}
			write.table(d0, file = tempTable, col.names = i == 1, append = i != 1, row.names = FALSE);
		}
		read.table(tempTable, header = TRUE, as.is = TRUE);
	} else {
		listOfDataFrames2data.frame(l, idColumn = idColumn, do.unlist = do.unlist,
			direction = rbind, resetColNames = resetColNames, colsFromFirstDf = colsFromFirstDf,
			idAsFactor = idAsFactor)
	}
	r
}

# names2col assigns names of the list to a column of the data frame and values to the valueCol
list2df = function(l, cols = names(l), row.name = NULL, names2col = NULL, valueCol = 'value') {
	idcs = if (is.null(cols)) 1:length(l) else
		if (all(is.integer(cols))) cols else which.indeces(names(l), cols);
	if (is.null(cols) || all(is.integer(cols))) cols = paste('C', 1:length(l), sep = '');
	r = as.list(rep(NA, length(cols)));
	names(r) = cols;
	r[idcs] = l;
	r = as.data.frame(r, stringsAsFactors = FALSE);
	if (!is.null(row.name)) row.names(r)[1] = row.name;
	if (!is.null(names2col)) {
		r = data.frame(name = names(r), value = unlist(r[1, ]), row.names = NULL, stringsAsFactors = FALSE);
		names(r) = c(names2col, valueCol);
	}
	r
}

be.numeric = function(v)
	sapply(v, function(e)grepl('^-?\\d*(\\.\\d+)?(e-?\\d+)?$', e, ignore.case = TRUE, perl = TRUE));

list2df.print = function(l, valueCol = 'value', names2col = NULL, ..., digits = 3, scientific = 3) {
	l1 = list2df(l, valueCol = valueCol, names2col = names2col, ...);
	numericRows = be.numeric(l1[[valueCol]]);
	numbers = as.numeric(l1[[valueCol]][numericRows]);
	log10range = max(floor(log10(numbers))) - min(floor(log10(numbers)));
	#fmt = if (log10range > digits + 1) '%.*e' else '%.*f';
	numbers = sprintf(ifelse(abs(floor(log10(numbers))) > scientific, '%.*e', '%.*f'), digits, numbers);
	#numbers = sapply(numbers, function(n)sprintf(fmt, digits, n));
	separators = as.vector(names(l) == '' & is.na(l));
	l1[separators, names2col] = '-';
	l1[separators, valueCol] = '';
	l1[numericRows, valueCol] = numbers;
	print(l1);
}


rbind.list2df = function(d, l, row.name = NULL) {
	d = as.data.frame(d);
	r = list2df(l, names(d), row.name);
	r0 = rbind(d, r);
	r0
}

# take list of lists
#	names of list elements become column-names
listOfLists2df = function(l, columnNames = names(l[[1]])) {
	colV = lapply(columnNames, function(n)Df_(list.kp(l, n, do.unlist = TRUE)));
	r = Df_(do.call(cbind, colV), names = columnNames);
	r
}

ListOfLists2df_extract = function(l, kp, template) {
	l1 = list.kp(l, kp, null2na = TRUE, do.unlist = FALSE, template = template);
	do.call(rbind, l1);
}
# advanced version of the above
ListOfLists2df = function(l,
	keyPath = '*', columnNames = names(list.kp(l[1], keyPath)[[1]]),
	reverseKeys = FALSE, keySep = '-', template = NA) {
	colV = lapply(columnNames, function (n) {
		kp = Sprintf('%{keyPath}s$%{n}s');
		# <A> robustly choose name (assume first element is proper template)
		#name = if (collapse) names(ListOfLists2df_extract(l[1], kp, template, collapse)) else NULL;
		r = ListOfLists2df_extract(l, kp, template);
		# names
		kpk = list.kp.keys(Sprintf('%{n}s'));
		cns = Col.names(r, vivify = FALSE);
		if (is.null(cns)) keySep = '';
		ns = if (reverseKeys)
			paste(cns, join(rev(kpk), keySep), sep = keySep) else
			paste(join(kpk, keySep), cns, sep = keySep);
		r = SetNames(r, ns);
		r
	});
	r = do.call(cbind, colV);
	# <!> Df_ applies as.data.frame -> normalization of column names
	#r = if (collapse == 0) Df_(r0, names = columnNames) else r0;
	r
}

# select elements from iterable (<=> perl grep)
Select = function(l, f, ...)return(l[sapply(l, f, ...)]);

# # d: data frame, l: list with names corresponding to cols, values to be searched for in columns
searchDataFrame = function(d, l, .remove.factors = TRUE) {
	ns = names(l);
	d = d[, ns, drop = FALSE];
	if (.remove.factors) {
		l = sapply(l, function(e)ifelse(is.factor(e), levels(e)[e], e));
		#d = apply(d, 2, function(col)(if (is.factor(col)) levels(col)[col] else col));
	}
	rs = which(as.vector(apply(apply(d, 1, function(r)(r == l)), 2, all)));
	rs
}

Which.cols = function(d, cols, regex = FALSE) {
	which.indeces(cols[is.character(cols)], names(d), regex = regex)
}
.df.cols = which.cols = function(d, cols, regex = FALSE) {
	cols[is.numeric(cols)] = as.integer(cols[is.numeric(cols)]);
	cols[is.character(cols)] = which.indeces(cols[is.character(cols)], names(d), regex = regex);
	as.integer(cols)
}
DfColsAfter = function(d, col, regex = FALSE, inclusive = FALSE) {
	I = which.cols(d, col, regex = regex);
	if (is.null(I)) return(NULL);
	return( (I + ifelse(inclusive, 0, 1)):ncol(d));
}
DfColsBetween = function(d, col1, col2, regex = FALSE, inclusive = TRUE) {
	I1 = if (missing(col1)) 1 else which.cols(d, col1, regex = regex);
	I2 = if (missing(col2)) ncol(d) else which.cols(d, col2, regex = regex);
	if (is.null(I1) || is.null(I2)) return(NULL);
	return( (I1 + ifelse(inclusive, 0, 1)):(I2 + ifelse(inclusive, 0, -1)) );
}

# select columns by name
.df = function(d, names, regex = TRUE, as.matrix = FALSE) {
	cols = which.indeces(names, names(d), regex = regex);
	d0 = d[, cols, drop = FALSE];
	# <t> simpler version:
	# d0 = d[, .df.cols(d, names, regex)];
	if (as.matrix) d0 = as.matrix(d0);
	d0
}
.df.reorder = function(d, names, regex = TRUE) {
	cols = .df.cols(d, names, regex);
	d0 = d[, c(cols, setdiff(1:dim(d)[2], cols))];
	d0
}
# remove columns by name
.dfm = function(d, names, regex = FALSE, as.matrix = FALSE) {
	cols = if (all(is.numeric(names))) as.integer(names) else which.indeces(names, names(d), regex = regex);
	d0 = d[, -cols, drop = FALSE];
	if (as.matrix) d0 = as.matrix(d0);
	d0
}
# remove rows by name
.dfrmr = function(d, names, regex = FALSE, as.matrix = FALSE) {
	rows = if (all(is.numeric(names)))
		as.integer(names) else
		which.indeces(names, row.names(d), regex = regex);
	d0 = d[-rows, , drop = FALSE];
	if (as.matrix) d0 = as.matrix(d0);
	d0
}

# remove rows/columns by name
.dfrm = function(d, rows = NULL, cols = NULL, regex = FALSE, as.matrix = FALSE) {
	d = as.data.frame(d);	# enforce data frame
	rows = if (is.null(rows)) 1:dim(d)[1] else
		-(if (all(is.numeric(rows))) as.integer(rows) else which.indeces(rows, row.names(d), regex = regex));
	cols = if (is.null(cols)) 1:dim(d)[2] else 
		-(if (all(is.numeric(cols))) as.integer(cols) else which.indeces(cols, names(d), regex = regex));
	d0 = d[rows, cols, drop = FALSE];
	if (as.matrix) d0 = as.matrix(d0);
	d0
}

# alignByRowNames: logical: use row.names from first element, else use provided vector
Cbind = function(..., stringsAsFactors = FALSE, deparse.level = 0, alignByRowNames = NULL) {
	l = list(...);
	if (notE(alignByRowNames)) {
		if (is.null(row.names(l[[1]]))) stop('Cbind[alignByRowNames]: No row names @ 1');
		ref = if (is.logical(alignByRowNames) && alignByRowNames)
			row.names(l[[1]]) else
			alignByRowNames;
		l = pairslapply(l, seq_along(l), function(e, i) {
			if (is.null(row.names(e))) stop('Cbind[alignByRowNames]: No row names @ %{i}d');
			e[order_align(ref, row.names(e)), , drop = FALSE]
		});
	}
	if (length(l) == 1)
		# <p> special case vector
		t_(l[[1]]) else
		# <p> standard invocation
		do.call(cbind, c(l, list(deparse.level = deparse.level)))
}
Rbind = function(..., stringsAsFactors = FALSE) {
	l = list(...);
	r = if (length(l) == 1) t_(t_(l[[1]])) else {
		if (class(l[[1]]) == 'data.frame')
			rbind(..., stringsAsFactors = stringsAsFactors) else
			rbind(...);
	}
	r
}


subsetTop = function(obj, sel, N = 1) {
	d0 = subset(obj, sel);
	d1 = d0[1:min(nrow(d0), N), ];
	d1
}

# transpose to create column vector for vector
t_ = function(m)(if (is.vector(m)) t(t(m)) else t(m))
# double transpose aka transpose to row -> vector to 1 x N matrix, otherwise identity
t2r = function(m)t(t_(m))

# convert strings to data frame names
#	<i> create a data frame and extract names
.dfns = function(ns)gsub(':', '.', ns);

# manipulate list of vectors
# vectors i = 1,.., n with entries v_ij are represented as vector v_11, ..., v_n1, v_21, ...
vector.intercalate = meshVectors = function(...) {
	l = list(...);
	if (length(l) == 1) l = l[[1]];
	v = as.vector(t(sapply(l, function(v)unlist(v))));
	# <N> preferred implementation
	# No unlist -> should be part of input sanitization
	# v = as.vector(do.call(rbind, l));
	v
}

is.sorted = function(...)(!is.unsorted(...))
is.ascending = function(v) {
	if (length(v) < 2) return(TRUE);
	for (i in 2:length(v)) if (v[i] <= v[i - 1]) return(FALSE);
	return(TRUE);
}

# pad a vector to length N
pad = function(v, N, value = NA)c(v, rep(value, N - length(v)));

#
#	<par> number sequences
#

rep.each.vector = function(v, n)as.vector(matrix(rep(v, n), n, byrow = TRUE))
rep.each = function(l, n, simplify = unlist) {
	l = Avu(l);
	if (length(n) == 1) rep.each.vector(l, n) else simplify(pairsapply(l, n, rep))
}
factorWithLevels = function(f, levels_) {
	f = as.factor(f);
	levels(f) = levels_;
	f
}
Rep.each = function(v, n) {
	r = rep.each(v, n);
	return(if (is.factor(v)) factorWithLevels(r, levels(v)) else r)
}
copyFactorStructure = function(dS, dD) {
	factors = which(lapply(dS, class) == 'factor');
	for (f in factors) dD[[f]] = factorWithLevels(dD[[f]], levels(dS[[f]]));
	dD
}
rep.each.row = function(m, n) {
# 	r = matrix(rep.each(m, n), ncol = ncol(m));
# 	if (class(m) == 'data.frame') {
# 		r = Df_(r, names = names(m));
# 		r = copyFactorStructure(m, r);
# 	}
	r = if (is.data.frame(m))
		Df_(lapply(m, Rep.each, n = n)) else
		m[rep.each(Seq(1, nrow(m)), n), , drop = FALSE]
	r
}

#rep.list = function(l, n) lapply(1:length(l), function(e)l);
# <!> changed as of 23.8.2016; n not used before
rep.each.list = rep.list = function(l, n) lapply(1:n, function(e)l);

matrix.intercalate = function(..., direction = 1, listOfMatrices = FALSE) {
	l = list(...);
	if (listOfMatrices) l = l[[1]];
	# <!> assume same dimension
	d = dim(l[[1]]);
	N = prod(d);
	# <p> create new matrix
	v = c(if (direction == 1) sapply(l, as.vector) else sapply(sapply(l, t), as.vector), recursive = TRUE);
	vN = as.vector(matrix(v, ncol = N, byrow = TRUE));
	r = if (direction == 1)
		matrix(vN, nrow = d[1] * length(l)) else
		matrix(vN, ncol = d[2] * length(l), byrow = TRUE);
	# <p> return value
	if (class(l[[1]]) == 'data.frame') r = Df_(r, names = names(l[[1]]));
	r
}

matrixSearch = function(mSearch, mSearched, cols = 1:ncol(mSearch)) {
	df1 = Df_(mSearch, names = paste0('c', cols));
	df2 = Df_(mSearched[, cols, drop = FALSE], names = paste0('c', cols));
	return(DfSearch(df1, df2, returnIdcs = TRUE));
}

arrayFromRowPairs = function(m, halves = FALSE) {
	if (halves)
		aperm(array(t(m), dim = c(2, dim(m)[1]/2, dim(m)[2])), c(2, 1, 3)) else
		# adjecent pairs
		aperm(array(t(m), dim = c(2, dim(m)[2], dim(m)[1]/2)), c(3, 1, 2))
}

data.frame.expandWeigths = function(data, weights = 'weights') {
	w = data[[weights]];
	weightsCol = which(names(data) == weights);
	df0 = lapply(1:length(w), function(i) {
		if (w[i] > 0) rep.each.row(data[i, -weightsCol], w[i]) else list();
	});
	df1 = rbindDataFrames(df0);
	df1
}

# spread/fill vector to indeces
vector.spread = function(v, idcs, N, default = 0) {
	r = rep(default, N);
	r[idcs] = v;
	r
}

# search vector for value, fill value elements with elements prior to it
#	e.g. 1, NA, NA, 2, NA -> 1, 1, 1, 2, 2
vector.propagateValuesForward = function(v, value = NA, vs) {
	idcs = if (is.na(value)) which(!is.na(v)) else which(v != value);
	Idcs = c(idcs, length(v) + 1);	# padded version
	# assign positions
	iA = lapply(seq_along(idcs), function(i)Seq(idcs[i] + 1, Idcs[i + 1] - 1));
	# indeces of values tb assigned
	iV = lapply(seq_along(idcs), function(i)rep(idcs[i], length(iA[[i]])));
	# fill in values
	#r = vector.assign(v, unlist(iA), v[unlist(iV)]);
	v[unlist(iA)] = v[unlist(iV)];
	return(v);
}

# create new vector with length == length(v) + length(idcs)
# idcs are positions in the final vector
vector.embed = function(v, idcs, e, idcsResult = TRUE) {
	if (!idcsResult) idcs = idcs + (1:length(idcs)) - 1;
	N = length(v) + length(idcs);
	r = rep(NA, N);
	r[setdiff(1:N, idcs)] = v;
	r[idcs] = e;

	# <p> names
	if (!is.null(names(v)) || !is.null(names(e))) {
		ns = rep(NA, N);
		if (!is.null(names(v))) ns[setdiff(1:N, idcs)] = names(v);
		if (!is.null(names(e))) ns[idcs] = names(e);
		names(r) = ns;
	}
	r
}
# set values at idcs
vector.assign = function(v, idcs, e, na.rm = 0, N) {
	if (!missing(N)) v = rep(v, N);
	v[idcs] = e;
	if (!is.na(na.rm)) v[is.na(v)] = na.rm;
	v
}
# names based assignment
Vector.assign = function(v, e, na.rm = NA) {
	idcs = which.indeces(names(e), names(v));
	vector.assign(v, idcs, e, na.rm = na.rm)
}

matrix.assign = function(m, idcs, e, byrow = TRUE) {
	if (length(dim(idcs)) > 1) {
		m[as.matrix(idcs)] = e
	} else if (byrow)
		m[idcs, ] = e else
		m[, idcs] = e
	m
}

# extract elements from array/matrix indexed in a row-wise manner by ...
#	array.extract(m, c(1, 2), c(1, 2)) -> c(m[1, 1], m[2, 2])
array.extract = function(a, ...) {
	r = mapply(function(...)do.call('[', c(list(a), list(...))), ...);
	return(r);
}

# are columns/rows same values in matrix
matrix.same = function(m, direction = 1) {
	apply(m, direction, function(e)all(e[1] == e))
}

vectorIdcs = function(v, f, ..., not = FALSE) {
	r = sapply(v, f, ...);
	which(if (not) !r else r)
}

is.seq = function(v, offset = 1)all( (v - offset + 1) == seq_along(v))

# produce indeces for indeces positioned into blocks of blocksize of which count units exists
# example: expand.block(2, 10, 1:2) == c(1, 2, 11, 12)
expand.block = function(count, blocksize, indeces) {
	blks = Seq(1,count);
	if (is.null(blks)) return(NULL);
	as.vector(apply(to.col(blks), 1,
		function(i){ (i - 1) * blocksize + t(to.col(indeces)) }
	));
}

search.block = function(l, s) {
	b.sz = length(s);
	which(sapply(
		1:(length(l)/b.sz), function(i){all(l[((i - 1) * b.sz + 1):(i * b.sz)] == s)}
	));
}

#
#	<par> matrix functions
#

# <!> assumes same indeces for rows/columns
matrixFromIndexedDf = function(df, idx.r = 'idx.r', idx.c = 'idx.c', value = 'value', referenceOrder = NULL) {
	id = unique(c(df[[idx.r]], df[[idx.c]]));
	# matrix indeces
	# <A> canonical order is by repeating vector id for row index, constant for columns within repetition
	#	-> matrix filled by columns
	midcs = merge(data.frame(id = id), data.frame(id = id), by = NULL);
	midcs = data.frame(midcs, mfid.i = 1:nrow(midcs));
	map = merge(df[, c(idx.r, idx.c, value)], midcs,
		by.x = c(idx.r, idx.c), by.y = c('id.x', 'id.y'), all.y = TRUE);
	# return to midcs order
	map = map[order(map$mfid.i), ];
	# filled by rows
	m = matrix(map[[value]], nrow = length(id));
	# reorder matrix
	o = order_align(firstDef(referenceOrder, id), id);
	# reorder in two steps -> out of mem otherwise
	m1 = m[o, ];
	m2 = m1[, o];
	m2
}

symmetrizeMatrix = function(m) {
	m[is.na(m)] = t(m)[is.na(m)];
	m
}

which.row = function(m, row) {
	cols = names(as.list(row));
	if (is.null(cols)) cols = 1:length(row);
	rows = 1:(dim(m)[1]);
	rows.found = rows[sapply(rows, function(i){ all(m[i, cols] == row) })];
	rows.found
}

# lsee:	list with searchees
# lsed:	list with searched objects
# inverse: lsed are regexes matched against lsee; pre-condition: length(lsee) == 1
# ret.list: for match.multi return list by lsee
# <!><t> cave: semantics changed as of 17.8.2009: return NA entries for unfound lsee-entries
# <!> match multi only implemented for merge = TRUE
which.indeces = function(lsee, lsed, regex = FALSE, ret.na = FALSE, merge = TRUE, match.multi = FALSE, ...,
	inverse = FALSE, ret.list = FALSE) {
	if (!length(lsed) || !length(lsee)) return(c());
	v = if (is.list(lsed)) names(lsed) else lsed;
	idcs = if (regex) {
		which(sapply(lsed, function(e)(
			if (inverse) length(fetchRegexpr(e, lsee, ...)) > 0 else
				any(sapply(lsee, function(see)(length(fetchRegexpr(see, e, ...)) > 0)))
		)))
	} else if (merge) {
		d0 = merge(
			data.frame(d = lsed, ix = 1:length(lsed)),
			data.frame(d = lsee, iy = 1:length(lsee)), all.y = TRUE);
		d0 = d0[order(d0$iy), ];
		idcs = if (match.multi) {
				#d0$ix[unlist(sapply(lsee, function(e)which(d0$d == e)))]
				#na.omit(sort(d0$ix))
				r = if (ret.list)
					unlist.n(by(d0, d0$d, function(d)list(na.omit(d$ix)), simplify = FALSE)) else
					na.omit(d0$ix);
				r
			} else {
				d0$ix[pop(which(c(d0$iy, 0) - c(0, d0$iy) != 0))];
			}
		# less efficient version
#		} else d0$ix[unlist(sapply(lsee, function(e)which(d0$d == e)[1]))];
#		} else d0$ix[order(d0$iy)]
		if (!ret.na) idcs = idcs[!is.na(idcs)];
		idcs
	} else {
		unlist(as.vector(sapply(lsee, function(e){
			w = which(e == v);
			if (!ret.na) return(w);
			ifelse(length(w), w, NA)
		})))
	};
	r = if (ret.list) idcs else as.integer(idcs);
	r
}

grep.vector = function(lsee, lsed, regex = FALSE, ret.na = FALSE, merge = TRUE, match.multi = FALSE, ..., inverse = FALSE) {
	lsed[which.indeces(lsee, lsed, regex, ret.na, merge, match.multi, ..., inverse = inverse)]
}
grep.infixes = function(lsee, lsed, ...) {
	r = grep.vector(sapply(lsee, function(v)sprintf('^%s.*', v)), lsed, regex = TRUE, inverse = FALSE, ... );
	r
}

# force structure to be matrix (arrange vector into a row)
MR = function(m) {
	if (!is.matrix(m)) m = matrix(m, byrow = TRUE, ncol = length(m));
	m
}
# force structure to be matrix (arrange vector into a columns)
MC = function(m) {
	if (!is.matrix(m)) m = matrix(m, byrow = FALSE, nrow = length(m));
	m
}

#
#	<par> data processing
#

# like table but produce columns for all numbers 1..n (not only for counts > 0)
# cats are the expected categories
table.n = function(v, n, min = 1, categories = NULL, useNA = 'no') {
	if (is.null(categories)) categories = min:n;
	t = as.vector(table(c(categories, v), useNA = useNA) - rep(1, length(categories)));
	t
}

tableFreqMarg = function(tab, margin = 2)apply(as.matrix(tab), margin, vn)

table.freq = function(v, byCol = TRUE, useNA = 'no') {
	t0 = table(v, useNA = useNA);
	r = if (is.vector(v) || is.factor(v) || is.numeric(v) || ncol(v) == 1) { t0 / sum(t0) } else {
		if (byCol) tableFreqMarg(t0) else tableFreqMarg(t0, 1)
	}
	r
}
table.n.freq = function(...) {
	t0 = table.n(...);
	r = t0 / sum(t0);
	r
}
table2df = function(tab, perc = FALSE, total = FALSE) {
	df0 = Df_(tab);
	nms = setdiff(names(df0), 'Freq');
	f = do.call(formulaWith, as.list(nms));
	df1 = dcast(df0, f, value.var= 'Freq');
	tot = apply(df1[, -1], 1, sum, na.rm = T);
	df2 = Df_(df1);
	if (perc) df2 = cbind(df2, Df_(df1[, -1] / tot, names = paste0(names(df1)[-1], 'perc')));
	if (total) df2$Total = tot;
	return(df2);
}
Table = function(v, min, max, ..., cats, asDf = FALSE, perc = FALSE, total = FALSE) {
	if (missing(min) && missing(max) && missing(cats)) {
		t0 = table(v);
		return(if (asDf) table2df(t0, perc, total) else t0);
	}
	if (!missing(cats)) {
		d = Df_(lapply(v, Avu));
		catsV = SetNames(Df_(merge.multi.list(cats)), names(d));
		t0 = table(rbind(d, catsV)) - 1;
		return(if (asDf) table2df(t0) else t0);
	} else {
		if (missing(min)) min = min(v);
		if (missing(max)) max = max(v);
		t0 = table.n(v, n = max, min = min);
		return(if (asDf) table2df(t0, perc, total) else t0);
	}
}
TableDf = function(v, min, max, ..., cats, asDf = TRUE, perc = FALSE, total = FALSE)
	Table(v, min, max, ..., cats = cats, asDf = asDf, perc = perc, total = total);
v2freq = function(v)(v/sum(v))

TableReshaped = function(df, reshape = names(df), replace = list(Freq. = '')) {
	tab = table(df)
	tabReshaped = reshape.wide(Df_(tab), reshape[1], reshape[-1]);
	if (notE(replace)) names(tabReshaped) = vector.replace(names(tabReshaped), list(Freq. = ''), regex = T);
	return(tabReshaped);
}


#
#	<p> numeric function
#

to.numeric = function(x) { SetNames(suppressWarnings(as.numeric(x)), names(x)) }
minFloor = function(x)(x - floor(x))

#
#	<par> data types
#


# set types for columns: numeric: as.numeric
data.frame.types = function(df, numeric = c(), character = c(), factor = c(), integer = c(),
	do.unlist = TRUE, names = NULL, row.names = NULL, reset.row.names = FALSE, do.rbind = FALSE, do.transpose = FALSE,
	stringsAsFactors = FALSE) {
	if (do.rbind) {
		#old code: df = t(sapply(df, function(e)e));
		lengthes = sapply(df, length);
		maxL = max(lengthes);
		df = t(sapply(1:length(df), function(i)c(df[[i]], rep(NA, maxL - lengthes[i]))));
	}
	if (do.transpose) df = t(df);
	df = as.data.frame(df, stringsAsFactors = stringsAsFactors);
	# set or replace column names
	if (!is.null(names)) {
		if (class(names) == "character") names(df)[1:length(names)] = names;
		if (class(names) == "list") names(df) = vector.replace(names(df), names);
	}
	if (do.unlist) for (n in names(df)) { df[[n]] = unlist(df[[n]]); }
	for (n in numeric) { df[[n]] = as.numeric(df[[n]]); }
	for (n in integer) { df[[n]] = as.integer(df[[n]]); }
	for (n in character) { df[[n]] = as.character(df[[n]]); }
	for (n in factor) { df[[n]] = as.factor(df[[n]]); }
	if (reset.row.names) row.names(df) = NULL;
	if (length(row.names) > 0) row.names(df) = row.names;
	df
}

DfStack = function(df0, N)do.call(rbind, rep.list(df0, N));
DfClasses = function(dataFrame)nlapply(dataFrame, function(n)class(dataFrame[[n]]));
DfAsInteger = function(dataFrame, as_integer) {
	#dfn = apply(dataFrame[, as_integer, drop = FALSE], 2, function(col)as.integer(avu(col)));
	# <!> 6.6.2016 as.integer first needed to retain factor status on factors
	dfn = nlapply(as_integer, function(col)avu(as.integer(dataFrame[[col]])));
	dataFrame[, as_integer] = as.data.frame(do.call(cbind, dfn));
	dataFrame
}
DfAsLogical = function(dataFrame, as_logical) {
	dfn = nlapply(as_logical, function(n) {
		col = dataFrame[[n]];
		if (is.factor(col)) (col == levels(col)[1]) else avu(as.logical(col));
	});
	dataFrame[, as_logical] = as.data.frame(do.call(cbind, dfn));
	dataFrame
}
DfAsCharacter = function(dataFrame, as_character) {
	#dfn = apply(dataFrame[, as_character, drop = FALSE], 2, function(col)as.character(avu(col)));
	#dataFrame[, as_character] = as.data.frame(dfn, stringsAsFactors = FALSE);
	dfn = nlapply(as_character, function(col)avu(as.character(dataFrame[[col]])));
	dataFrame[, as_character] = as.data.frame(do.call(cbind, dfn), stringsAsFactors = FALSE);
	dataFrame
}
DfFac2num = function(dataFrame) {
	return(do.call(data.frame, lapply(dataFrame, function(e)if (is.factor(e)) as.numeric(e) else e)))
}
DfApplyValueMap = function(r, valueMap, Df_doTrimValues = FALSE,
	Df_mapping_value = '__df_mapping_value__',
	Df_mapping_empty = '__DF_EMPTY__', Do_Df_mapping_empty = TRUE) {

	for (n in names(valueMap)) {
		vs = if (Df_doTrimValues && class(r[[n]]) %in% c('character', 'factor'))
			nina(trimString(as.character(r[[n]])), Df_mapping_value) else
			as.character(r[[n]]);
		vm = valueMap[[n]];
		if (Do_Df_mapping_empty) {
			vs = ifelse(nit(vs == ''), Df_mapping_empty, vs);
			if (!(Df_mapping_empty %in% names(vm)))
				vm = c(vm, listKeyValue(Df_mapping_empty, NA));
		}
		vs = nina(valueMap[[n]][vs], Df_mapping_value);
		vs = ifelse(vs == Df_mapping_value, as.character(r[[n]]), vs);
		r[[n]] = vs;
	}
	return(r);
}
# copy over factor structure from other data frame (tentamen/bw for example)
DfEmbed = function(d, dSource) {
	cols = nlapply(d, function(n) {
		if (class(dSource[[n]]) == 'factor')factor(d[[n]], levels(dSource[[n]]))else d[[n]]
	})
	return(Df_(cols));
}

#	r = strptime(col, dateFormat[1], tz = firstDef(dateFormat[2], defaultTz));

DfDate = function(dataFrame, as_date, format = '%F', tz = 'UTC') {
	dfn = nlapply(as_date, function(n)Df_(strptime(dataFrame[[n]], format, tz), names = n));
	dataFrame[, as_date] = do.call(cbind, dfn);
	dataFrame
}

# as of 22.7.2013 <!>: min_ applied before names/headerMap
# as of 19.12.2013 <!>: as.numeric -> as_numeric
# as of 22.5.2014 <!>: t -> t_
# as of 13.11.2014 <!>: sapply -> simplify_
# Create data frames with more options than \code{data.frame}
Df_ = function(df0, headerMap = NULL, names = NULL, min_ = NULL,
	as_numeric = NULL, as_character = NULL, as_factor = NULL, as_integer = NULL, as_logical = NULL,
	row.names = NA, valueMap = NULL, Df_as_is = TRUE, simplify_ = FALSE,
	deep_simplify_ = FALSE, t_ = FALSE, unlist_cols = FALSE, transf_log = NULL, transf_m1 = NULL,
	Df_doTrimValues = FALSE, Df_mapping_value = '__df_mapping_value__',
	Df_mapping_empty = '__DF_EMPTY__', Do_Df_mapping_empty = TRUE, apply_ = FALSE,
	as_date = NULL, date_format = '%F', date_tz = 'UTC') {
	# <p> input sanitation
	#r = as.data.frame(df0);
	# for a vector with identical names for each entry, use this as a column name
	if (length(unique(names(df0))) == 1 && !Nif(names)) names = unique(names(df0));
	# sanitize row.names
	dn = dimnames(df0);
	if (Nif(dn) && any(duplicated(dn[[1]]))) dimnames(df0)[[1]] = NULL;
	# <!> commented out on 4.4.2019, test implemented to fix this behavior
	#if (length(row.names) == 0 || !all(is.na(row.names))) base::row.names(df0) = row.names;

	if (apply_) df0 = as.data.frame(apply(df0, 2, identity));
	#if (!Nif(Apply_)) df0 = as.data.frame(apply(df0, 2, Apply_));
	if (t_) df0 = t(df0);
	# reset_row_names breaks unit tests (27.9.2017)
	#r = data.frame(df0, stringsAsFactors = !Df_as_is, row.names = if (reset_row_names) NA else NULL);
	r = data.frame(df0, stringsAsFactors = !Df_as_is);
	if (notE(min_)) {
		is = which.indeces(min_, names(r));
		if (length(is) > 0) r = r[, -is, drop = FALSE];
	}
	if (simplify_) r = as.data.frame(sapply(r, identity));
	if (deep_simplify_) r = as.data.frame(
		nlapply(r, function(col)sapply(r[[col]], unlist)), stringsAsFactors = !Df_as_is
	);

	#
	#	<p> column names
	#
	if (notE(names)) {
		if (class(names) == 'character') names(r)[1:length(names)] = names;
		if (class(names) == 'list') names(r) = vector.replace(names(r), names);
	}
	if (notE(headerMap)) names(r) = vector.replace(names(r), headerMap);
	#
	#	<p> column types
	#
#if (class(df0) == 'data.frame' && ncol(df0) >= 3) browser();
	if (notE(as_numeric)) {
		#dfn = apply(r[, as_numeric, drop = FALSE], 2, function(col)as.numeric(avu(col)));
		dfn = lapply(r[, as_numeric, drop = FALSE], function(col)avu(as.numeric(col)));
		r[, as_numeric] = as.data.frame(do.call(cbind, dfn));
	}
	if (notE(as_logical)) r = DfAsLogical(r, as_logical);
	if (notE(as_integer)) r = DfAsInteger(r, as_integer);
	if (notE(as_character)) r = DfAsCharacter(r, as_character);
	if (notE(as_factor)) {
		# <N> does not work
		#dfn = apply(r[, as_factor, drop = FALSE], 2, function(col)as.factor(col));
		#r[, as_factor] = dfn;
		for (f in as_factor) r[, f] = as.factor(r[[f]]);
	}
	if (notE(as_date)) r = DfDate(r, as_date, date_format, date_tz);
	#
	#	<p> value map
	#
	if (notE(valueMap)) {
# 		for (n in names(valueMap)) {
# 			vs = if (Df_doTrimValues && class(r[[n]]) %in% c('character', 'factor'))
# 				nina(trimString(as.character(r[[n]])), Df_mapping_value) else
# 				as.character(r[[n]]);
# 			vm = valueMap[[n]];
# 			if (Do_Df_mapping_empty) {
# 				vs = ifelse(nit(vs == ''), Df_mapping_empty, vs);
# 				if (!(Df_mapping_empty %in% names(vm)))
# 					vm = c(vm, listKeyValue(Df_mapping_empty, NA));
# 			}
# 			vs = nina(valueMap[[n]][vs], Df_mapping_value);
# 			vs = ifelse(vs == Df_mapping_value, as.character(r[[n]]), vs);
# 			r[[n]] = vs;
# 		}
		r = DfApplyValueMap(r, valueMap,
			Df_doTrimValues, Df_mapping_value, Df_mapping_empty, Do_Df_mapping_empty);
	}
	#
	#	<p> transformations
	#
	if (notE(transf_log)) r[, transf_log] = log(r[, transf_log, drop = FALSE]);
	if (notE(transf_m1)) r[, transf_m1] = r[, transf_m1, drop = FALSE] - 1;
	if (length(row.names) == 0 || !all(is.na(row.names))) base::row.names(r) = row.names;
	if (unlist_cols) for (n in names(r)) r[[n]] = avu(r[[n]]);
	r
}

Df = function(..., headerMap = NULL, names = NULL, min_ = NULL, row.names = NA, Df_as_is = TRUE,
	as_numeric = NULL, as_character = NULL, as_factor = NULL, t_ = FALSE, unlist_cols = FALSE) {
	r = data.frame(...);
	Df_(r, headerMap = headerMap, names = names, min_ = min_, row.names = row.names,
		as_numeric = as_numeric,
		as_character = as_character,
		as_factor = as_factor,
		Df_as_is = Df_as_is,
		t_ = t_,
		unlist_cols = unlist_cols
	);
}
Df2list = function(df) {
	df = as.data.frame(df);
	nlapply(names(df), function(n)df[[n]]);
}
Dfselect = function(data, l, na.rm = nif) {
	sel = apply(sapply(nlapply(l, function(n)data[[n]] == l[[n]]), identity), 1, all);
	r = data[na.rm(sel), ];
	r
}
DfSearch = function(dfSearch, dfSearched,
	colNamesReset = 'col', colNameIdx = '.dfSearchIdx', returnIdcs = FALSE) {

	if (is.null(dfSearched)) return(NULL);
	nms = if (notE(colNamesReset)) {
		nms = paste(colNamesReset, 1:ncol(dfSearched), sep = '');
		names(dfSearch) = names(dfSearched) = nms;
	} else names(dfSearched);
	dfm = merge(
		Df(1:nrow(dfSearched), dfSearched, names = colNameIdx),
		Df(1:nrow(dfSearch), dfSearch, names = colNameIdx), by = nms);
	if (returnIdcs)
		return(dfm[, paste0(colNameIdx, c('.x', '.y')), drop = FALSE]) else
		return(dfm[[paste0(colNameIdx, '.x')]]);
}

DfDiff = function(d1, d2) {
	dC = rbind(d2, d1);
	row.names(dC) = NULL;
	dCu = unique(dC);
	# d2 comes first, non-unique rows left out from d1, sames as ones diffed out
	r = if (nrow(dCu) == nrow(d2)) dCu[c(), ] else dCu[(nrow(d2) + 1):nrow(dCu), , drop = FALSE];
	r
}
# replace columns in data.frame
DfRepl = function(d0, d1) {
	d0[, names(d1)] = d1;
	return(d0);
}

DfRound = function(df0, cols_round = names(df0), digits = 2, as_numeric = FALSE) {
	rounder = if (as_numeric)
		function(col)round(as.numeric(df0[[col]]), digits) else
		function(col)round(df0[[col]], digits)
	df0[, cols_round] = do.call(cbind, lapply(cols_round, rounder));
	df0
}


# standardize df names using formulas
# f: formula with names used in the Dataframe ~ x1 + x2 ... or x1 ~ x2 + ...
#	corresponding positionally to the standard names
# nmsStd: formula or vector with standard names
# d: data.frame with column names tb transformed
dfNmsStd = function(f, nmsStd, d) {
	nmsUsed = all.vars(f);
	#if (is.formula(nmsStd)) nmsStd = all.vars(nmsStd);
	# import from plyr (is.formula) leads to test failures <!>
	if (class(nmsStd) == 'formula') nmsStd = all.vars(nmsStd);
	if (length(nmsUsed) != length(nmsStd))
		stop(Sprintf('Formula names [%{f}s] do not match standard names [%{nm}s]',
			f = formula.to.character(f), nm = join(nmsStd, ', ')));
	d1 = Df_(d, headerMap = listKeyValue(nmsUsed, nmsStd));
	return(d1);
}
# DfNames2std = function(d, nmsFormula, nmsStandard) {
# 	d1 = Df_(d, headerMap = listKeyValue(all.vars(nmsFormula), nmsStandard));
# 	d1
# }
DfNames2std = function(d, nmsFormula, nmsStandard)dfNmsStd(nmsFormula, nmsStandard, d)

charRange = characterRange = function(ns, range, indeces = TRUE, invert = FALSE) {
	N = length(ns);
	r = if (class(range) == 'character') {
		(if (is.na(range)[1])1 else which(range[1] == ns)) :
			(if (is.na(range)[2])N else which(range[2] == ns))
	} else if (class(range) == 'integer') {
		(if (is.na(range)[1])1 else range[1]) :
			(if (is.na(range)[2])N else range[2])
	} else c();
	if (invert) r = setdiff(1:length(ns), r);
	if (!indeces) r = ns[r];
	return(r);
}

DfCol = function(d, range) {
	d = d[, characterRange(names(d), range), drop = F];
	return(d);
}

List_ = .List = function(l, min_ = NULL, sel_ = NULL,
	rm.null = FALSE, names_ = NULL, null2na = FALSE, simplify_ = FALSE, rm.na = FALSE) {
	if (!is.null(min_)) {
		i = which.indeces(min_, names(l));
		if (length(i) > 0) l = l[-i];
	}
	if (!is.null(sel_)) {
		i = which.indeces(sel_, names(l));
		if (length(i) > 0) l = l[i];
	}
	if (rm.null) {
		remove = -which(sapply(l, is.null));
		if (length(remove) > 0) l = l[remove];
	}
	if (null2na) {
		nullI = which(sapply(l, is.null));
		l[nullI] = NA;
	}
	if (rm.na) {
		l = l[!is.na(l)];
	}
	if (notE(names_)) {
		if (is.character(names_)) names(l)[Seq(1, length(names_))] = names_;
		if (is.list(names_)) names(l) = vector.replace(names(l), names_);
		if (is.na(names_)) names(l) = NULL;
	}
	if (simplify_) l = sapply(l, identity);
	l
}
List = function(..., min_ = NULL, envir = parent.frame(), names_ = NULL) {
	l = eval(list(...), envir = envir);
	.List(l, min_ = min_, names_ = names_);
}

Unlist = function(l, ..., null2na_ = FALSE) {
	if (null2na_) l[sapply(l, is.null)] = NA;
	unlist(l, ...)
}

#last = function(v)(rev(v)[1])
last = function(v)(v[length(v)])
pop = function(v)(v[-length(v)])
shift = function(v)(v[-1])
# differences between successive elements, first diff is first element with start
vectorLag = function(v, start = 0)pop(c(v, start) - c(start, v))
splitN = function(N, by = 4) vectorLag(round(cumsum(rep(N/by, by))));
splitToMax = function(N, max = 4) vectorLag(round(cumsum(rep(N/ceiling(N/max), ceiling(N/max)))));
# split into fixed block sizes + last incomplete block
splitBy = function(N, NperBlock = 4) {
	Nlast = N %% NperBlock;
	return(c(rep(NperBlock, N %/% NperBlock), if (Nlast == 0) c() else Nlast));
}

# cumsum returning indeces for numbers given in Ns
cumsumI = function(Ns, offset = 1, do.pop = FALSE) {
	cs = vectorNamed(c(0, cumsum(Ns)) + offset, c(names(Ns), 'N'));
	if (do.pop) cs = pop(cs);
	cs
}
# recursive cumsum (one level)
cumsumR = function(l, offset = 1) {
	cs0 = if (is.list(l)) lapply(l, cumsumR, offset = 0) else rev(cumsum(l))[1];
	cs = vectorNamed(c(0, pop(unlist(cs0))) + offset, names(cs0));
	cs
}

countsExtract = function(v, Ns, simplify = FALSE) {
	cnts = counts2idcs(Ns);
	r = apply(cnts, 1, function(r) {
		r = v[ r[1] : r[2] ];
		if (simplify) r else list(r)
	});
	return(if (!simplify) unlist.n(r, 1) else r);
}

#
#	<par> sets and permutations
#

#' @title wrapper for order to allow multivariate ordering
#'
#' @param v object (vector or data frame) for which order is to be calculated
#' @param ... additional arguemnts passed on to \code{order}
#' @return order of the object
#' @seealso {order{}} which this function wraps around
Order = function(v, ...) {
	if (is.data.frame(v)) do.call(order, lapply(v, identity), ...) else
	if (is.list(v)) do.call(order, v, ...) else
	order(v, ...)
}

#' @title Return all value combinations appearing in a data frame
#'
#' @param d data frame for which value combinations are to be caclulated
#' @return list with all value combinations present in \code{d}
# #' @examples
# #'
# #' combs = valueCombinations(iris);
# #'
valueCombinations = function(d) merge.multi.list(dimnames(table(d)));

#' @title Computes order so that inverseOrder after order is the identity
#'
#' Caculate ranks for arguemnt \code{p}. Works on vactors and data frames.
#'
#' @param p object for which ranks are to be comptued
#' @return vector of ranks of elements of \code{p}
#'
# #' @examples
# #' v = runif(1e2);
# #' print(all(sort(v)[inverseOrder(v)] == v))
Rank = inverseOrder = inversePermutation = function(p) {
	## <p> naive version
	# 	o = order(p);
	# 	i = rep(NA, length(o));
	# 	for (j in 1:length(o)) { i[o[j]] = j};
	# 	i
	## <p> build-in version (not working for multivariate case)
	#rank(v, ties.method = 'first')
	## <p> better version
	which.indeces(1:(if (class(p) == 'data.frame') nrow(p) else length(p)), Order(p))
}

#' @title Calculates inverseOrder, assuming that the argument is already an \code{order}-vector.
#'
#' @param p obect for which the inverse order is to be calculated
#' @return vector with integers representing the inverse order
inverseOrder_fromOrder = function(p)which.indeces(1:length(p), p)

#' @title Return vector that reorders v to equal reference.
#'
#' Assuming that two arguments are permutaions of each other, return a vector of indeces such that \code{all(reference == v[order_align(reference, v)]) == TRUE} for all vectors \code{reference, v}.
#'
#' @param reference vector with the reference ordering
#' @param v vector that is to be ordered the same way as \code{reference}
#' @return vector of indeces so that \code{v[return_value]} is the same as \code{reference}
#'
# #' @examples
# #' sapply(1:10, function(i){v = sample(1:5); v[order_align(5:1, v)]})
# #' sapply(1:10, function(i){
# #'    v = runif(1e2); v1 = sample(v, length(v));
# #'    all(v1[order_align(v, v1)] == v)
# #' })
order_align = function(reference, v)Order(v)[inverseOrder(reference)];

#' @title Calculates \code{order_align}, assuming that the both arguments are already orders.
#'
#' Analogous to \code{order_align} under the assumption that provided arguments are orders.
#'
#' @param reference order of a reference vector
#' @param v order of vector that is to be brought into the order of \code{reference}
#' @return order that can be applied to the orignal vector (from which \code{v} was calculated) to make it identical to the vector underlying \code{reference}
#'
# # ' @examples
# # ' \dontrun{
# # '   sapply(1:40, function(i){
# # '     v = runif(1e2);
# # '     v1 = sample(v, length(v));
# # '     all(v1[order_align_fromOrder(order(v), order(v1))] == v)
# # '   })
# # ' }
order_align_fromOrder = function(reference, v)v[inverseOrder_fromOrder(reference)];

# permutation is in terms of elements of l (not indeces)

applyPermutation = function(l, perm, from = 'from', to = 'to', returnIndeces = TRUE) {
	# 1. bring perm[[from]] in the same order as l
	# 2. apply this order to perm[[to]]
	r0 = perm[[to]][order(perm[[from]])[inverseOrder(l)]];
	# 3. determine permutation going from l to r0
	r = order(l)[inverseOrder(r0)]
	if (!returnIndeces) r = l[r];
	r
}

order.df = function(df, cols = NULL, decreasing = FALSE, na.last = FALSE) {
	if (is.null(cols)) cols = 1:ncol(df);
	if (!is.numeric(cols)) cols = which.indeces(cols, names(df));
	orderText = sprintf("order(%s, decreasing = %s, na.last = %s)",
		paste(sapply(cols, function(i) { sprintf("df[, %d]", i) }), collapse = ", "
		), as.character(decreasing), as.character(na.last)
#		paste(sapply(cols, function(i) {
#			if (is.numeric(i)) sprintf("df[, %d]", i) else sprintf("df$%s", i) }), collapse = ", "
#		), as.character(decreasing), as.character(na.last)
	);
	o = eval(parse(text = orderText));
	#print(list(text = orderText, order = o, df=df));
	o
}

order.df.maps = function(d, maps, ..., regex = FALSE) {
	cols = NULL;
	for (i in 1:length(maps)) {
		m = names(maps)[i];
		map = maps[[i]];
		keys = names(map);
		cols = c(cols, if (is.list(map)) {
			tempColName = sprintf("..order.df.maps.%04d", i);
			col = if (regex)
				sapply(d[[m]], function(e){ j = which.indeces(e, keys, regex = TRUE, inverse = TRUE)
					if (length(j) == 0) NA else map[[j]]
				}) else	as.character(map[d[[m]]]);
			col[col == "NULL"] = NA;
			d = data.frame(col, d, stringsAsFactors = FALSE);
			names(d)[1] = tempColName;
		} else { m });
	}
	o = order.df(d, cols, ...);
	o
}

data.frame.union = function(l) {
	dfu = NULL;
	for (n in names(l)) {
		df = l[[n]];
		factor = rep(n, dim(df)[1]);
		dfu = rbind(dfu, cbind(df, factor));
	}
	dfu
}

#
#	<p> factors
#

# levels: take levels in that order, unmentioned levels are appended
# setLevels: restrict to these levels, else set to NA
# setLevelsTo: set names of levels to argument, set excess levels to NA
# group: group levels, set names to concatenations
#	recodeLevels(as.factor(c('AA', 'AG', 'GG')), group = list(1:2, 3))
recodeLevels = function(f, map = NULL, others2na = TRUE, levels = NULL, setLevels = NULL,
	setLevelsTo = NULL, sortLevelsByMap = TRUE, group = NULL) {
	r = f;
	# <!> overwrites map
	# <!><i> does not implement grouping by level spec
	if (notE(group)) {
		lvls = levels(f);
		map = unlist.n(lapply(group, function(e) {
			newLevel = join(lvls[e], ' ');
			mapEl = recycle(lvls[e], newLevel);
			listKeyValue(mapEl[[1]], mapEl[[2]])
		}), 1);
	}
	if (!is.null(map)) {
		# map others to NA
		if (others2na) {
			nonmentioned = setdiff(if (is.factor(f)) levels(f) else unique(f), c(names(map), NA));
			map = c(map, listKeyValue(nonmentioned, rep(NA, length(nonmentioned))));
		}
		v = vector.replace(as.character(f), map);
		# test for integer before and after
		# special case eliminated as of 14.9.2018
		#if (is.integer(f)) v = as.integer(v);
		#if (is.factor(f)) v = factor(v, levels = unique(as.character(map)));
		#v = factor(v, levels = unique(as.character(map)));
		r = if (sortLevelsByMap)
			factor(v, levels = union(unique(map), setdiff(unique(v), unique(map)))) else
			as.factor(v);
		# <!> r = v, r <- v do not work here, remain local
	}
	if (!is.null(levels) || !is.null(setLevels)) {
		# <p> preparation
		fact0 = as.factor(r);
		levls = levels(fact0);
		r = levls[fact0];

		# <p> new levels
		levlsN0 = firstDef(setLevels, levels, levls);
		levlsN = c(levlsN0, setdiff(levls, levlsN0));

		# <p> remove unwanted levels
		if (!is.null(setLevels)) r = ifelse(r %in% setLevels, r, NA);
		# <p> rename levels
		if (!is.null(setLevelsTo)) {
			#r = drop.levels(ifelse(as.integer(r) <= length(setLevels), r, NA));
			# 14.1.2020
			r = droplevels(ifelse(as.integer(r) <= length(setLevels), r, NA));
			levels(r) = setLevelsTo;
		}
		r = factor(r, levels = if (!is.null(setLevels)) levlsN0 else levlsN);
	}
	r
}
levelsSort = function(fac)recodeLevels(fac, listKeyValue(sort(levels(fac)), sort(levels(fac))))


factor2int = function(f)as.integer(as.character(f))
factor2numeric = function(f)as.numeric(as.character(f))

#
#	</p> factors
#

Union = function(..., .drop = TRUE, as.list = FALSE) {
	l = if (as.list) list(...)[[1]] else list(...);
	l = list(...);
	# auto-detect list of values
	if (.drop && length(l) == 1 && is.list(l[[1]])) l = l[[1]];
	r = NULL;
	for (e in l) { r = union(r, e); }
	r
}
Intersect = function(..., .drop = TRUE, as.list = FALSE) {
	l = if (as.list) list(...)[[1]] else list(...);
	# auto-detect list of values
	if (.drop && length(l) == 1 && is.list(l[[1]])) l = l[[1]];
	r = l[[1]];
	for (e in l[-1]) { r = intersect(r, e); }
	r
}

intersectSetsCount = function(sets) {
	i = iterateModels(list(s1 = names(sets), s2 = names(sets)), function(s1, s2) {
		length(intersect(sets[[s1]], sets[[s2]]))
	}, lapply__ = lapply);
	#r = reshape.wide(Df(i$models_symbolic, count = unlist(i$results)), 's1', 's2');
	rM = matrix(i$results, nrow = length(sets), byrow = TRUE);
	dimnames(rM) = list(names(sets), names(sets));
	rM
}
unionCum = function(..., .drop = TRUE) {
	l = list(...);
	# auto-detect list of values
	if (.drop && length(l) == 1 && is.list(l[[1]])) l = l[[1]];
	r = l[1];
	if (length(l) > 1)
		for (n in names(l)[-1]) { r = c(r, List(union(r[[length(r)]], l[[n]]), names_ = n)); }
	r
}

# row bind of data.frames/matrices with equal number of cols
lrbind = function(l, as.data.frame = FALSE, names = NULL) {
	d = dim(l[[1]])[2];
	v = unlist(sapply(l, function(m) unlist(t(m))));
	m = matrix(v, byrow = TRUE, ncol = d);
	dimnames(m) = list(NULL, names(l[[1]]));
	if (as.data.frame) {
		m = data.frame(m);
		if (!is.null(names)) names(m) = names;
	}
	m
}

#
#	logic arrays/function on list properties
#

# old versions:
#	if (na.rm) v = v[!is.na(v)];
#	sum(v)	# old version: length((1:length(v))[v])
# same as in Rlab
count = function(v, na.rm = TRUE)sum(v, na.rm = na.rm)
# old versions:
#	if (na.rm) v = v[!is.na(v)]; (sum(v)/length(v))
#	{ length(v[v]) / length(v) }
# v assumed to be logical
fraction = function(v, na.rm = TRUE)mean(v, na.rm = na.rm);
# treat v as set
set.card = function(v)count(unique(v))
# cardinality of a set
size = function(set)length(unique(set));

# null is false
#nif = function(b)(!(is.null(b) | is.na(b) | !b))
#nif = function(b)sapply(b, function(b)(!(is.null(b) || is.na(b) || !b)))
nif = function(b) {
	if (length(b) == 0) return(FALSE);
	if (class(b) %in% c('formula', 'function', 'list', 'data.frame')) return(TRUE);
	!(is.null(b) | is.na(b) | !b)
}
Nif = function(b, allnif = TRUE, nonLogicalIsTrue = TRUE) {
	if (is.null(b)) return(FALSE);
	if (class(b) %in% c('formula', 'function')) return(TRUE);
	bLog = sapply(b, as.logical);
	b = ifelse(is.na(b) | sapply(b, class) == 'logical', bLog, nonLogicalIsTrue);
	summ = (if (allnif) all else any);
	r = summ(sapply(b, nif));
	r
}
# null is true
#nit = function(b)(is.null(b) | is.na (b) | b)
#nit = function(b)sapply(b, function(b)(is.null(b) || is.na (b) || b))
nit = function(b) {
	if (length(b) == 0) return(TRUE);
	is.null(b) | is.na (b) | b
}
# null is zero
#niz = function(e)ifelse(is.null(e) | is.na(e), 0, e)
niz = function(e)ifelse(is.null(e) | is.na(e), 0, e)

# null is na (or other special value
#niz = function(e)ifelse(is.null(e) | is.na(e), 0, e)
nina = function(e, value = NA)sapply(e, function(e)ifelse(is.null(e), value, e))
Nina = function(e, value = NA)if (length(e) == 0) value else nina(e, value);

# not empty
notE = function(e)(length(e) > 0);

plus = function(x)ifelse(x > 0, x, 0)
minus = function(x)ifelse(x < 0, x, 0)

#
#	<p> complex structures
#

#
# Averaging a list of data frames per entry over list elements
#

# meanMatrices = function(d) {
# 	df = as.data.frame(d[[1]]);
# 	ns = names(df);
# 	# iterate columns
# 	dfMean = sapply(ns, function(n) {
# 		m = sapply(d, function(e)as.numeric(as.data.frame(e)[[n]]));
# 		mn = apply(as.matrix(m), 1, mean, na.rm = TRUE);
# 		mn
# 	});
# 	dfMean
# }
meanMatrices = function(d) {
	dm = dim(d[[1]]);
	good = sapply(d, function(m)(length(dim(m)) == 2 && all(dim(m) == dm)));
	if (any(!good)) warning('meanMatrices: malformed/incompatible matrices in list, ignored');
	d = d[good];
	m0 = sapply(d, function(e)avu(e));
	m1 = apply(m0, 1, mean, na.rm = TRUE);
	r = matrix(m1, ncol = dm[2], dimnames = dimnames(d[[1]]));
	r
}
meanVectors = function(d) {
	ns = names(d[[1]]);
	mn = apply(as.matrix(sapply(d, function(e)e)), 1, mean, na.rm = TRUE);
	mn
}
meanList = function(l)mean(as.numeric(l));

meanStructure = function(l) {
	r = nlapply(names(l[[1]]), function(n) {
		meanFct =
			if (is.matrix(l[[1]][[n]])) meanMatrices else
			if (length(l[[1]][[n]]) > 1) meanVectors else
				meanList;
		meanFct(list.key(l, n, unlist = FALSE));
	});
	r
}

matrixCenter = function(m, direction = 2, centerBy = median) {
	center = apply(m, direction, centerBy, na.rm = TRUE);
	m = if (direction == 1) (m - center) else t(t(m) - center);
	list(matrix = m, center = center)
}

matrixDeCenter = function(m, center, direction = 2) {
	m = if (direction == 1) t(t(m) + center) else (m + center);
	m
}


#
#	<p> combinatorial functions
#

# form all combinations of input arguments as after being constraint to lists
# .first.constant designates whether the first list changes slowest (TRUE) or fastest (FALSE)
#	in the resulting data frame,
#	i.e. all other factors are iterated for a fixed value of l[[1]] (TRUE) or not
# .constraint provides a function to filter the resulting data frame
merge.multi.list = function(l, .col.names = NULL, .col.names.prefix = "X",
	.return.lists = FALSE, .first.constant = TRUE, stringsAsFactors = FALSE, .cols.asAre = FALSE, .constraint = NULL, ...) {
	# <p> determine column names of final data frame
	.col.names.generic = paste(.col.names.prefix, 1:length(l), sep = "");
	if (is.null(.col.names)) .col.names = names(l);
	if (is.null(.col.names)) .col.names = .col.names.generic;
	.col.names[.col.names == ""] = .col.names.generic[.col.names == ""];
	names(l) = .col.names;		# overwrite names
	# <p> construct combinations
	if (.first.constant) l = rev(l);
	df0 = data.frame();
	if (length(l) >= 1) for (i in 1:length(l)) {
		newNames = if (.cols.asAre) names(l[[i]]) else names(l)[i];
		# <p> prepare data.frame: handle lists as well as data.frames
		# <!> changed 22.3.2016
		#dfi = if (is.list(l[[i]])) unlist(l[[i]]) else l[[i]];
		dfi = if (!is.data.frame(l[[i]])) unlist(l[[i]]) else l[[i]];
		df1 = data.frame.types(dfi, names = newNames, stringsAsFactors = stringsAsFactors);
		# <p> perform merge
		df0 = if (i > 1) merge(df0, df1, ...) else df1;
	}
	if (.first.constant) df0 = df0[, rev(names(df0)), drop = FALSE];
	if (.return.lists) df0 = apply(df0, 1, as.list);
	if (!is.null(.constraint)) {
		df0 = df0[apply(df0, 1, function(r).do.call(.constraint, as.list(r))), ];
	}
	df0
}

# list of list, vector contains index for each of these lists to select elements from
#	these elements are merged and return
#	if sub-element is not a list, take name of sub-element and contruct list therefrom
#	namesOfLists controls whether, if a selected element is a list, its name is used instead
#		can be used to produce printable summaries
list.takenFrom = function(listOfLists, v) {
	ns = names(listOfLists);
	if (any(ns != names(v))) v = v[order_align(ns, names(v))];
	l = lapply(1:length(v), function(i) {
		new = if (!is.list(listOfLists[[i]]))
			listKeyValue(ns[i], listOfLists[[i]][v[i]]) else {
				t = listOfLists[[i]][[v[i]]];
				# list of vectors
				t = (if (!is.list(t)) {
					# define name from higher level
					listKeyValue(firstDef(
						names(listOfLists[[i]])[v[i]], ns[i]
					), list(t))
					# <A> probably better and correct
					#listKeyValue(ns[i], list(t))
				} else if (is.null(names(t))) listKeyValue(ns[i], t) else t);
				t
			}
	});
	names(l) = names(v);
	l
}
# simplified version of list.takenFrom
list.extract = function(lol, idcs)pairsapplyLV(lol, idcs, function(l, i)l[i])
list.extractRows = function(lol, idcs)t(pairsapplyLV(lol, idcs, function(l, i)l[i, ]))


merge.lists.takenFrom = function(listOfLists, v) {
	merge.lists(list.takenFrom(listOfLists, v), listOfLists = TRUE);
}

merge.lists.takenFrom_old = function(listOfLists, v) {
	l = list();
	ns = names(listOfLists);
	if (any(ns != names(v))) v = v[order_align(ns, names(v))];
	for (i in 1:length(v)) {
		new = if (!is.list(listOfLists[[i]]))
			listKeyValue(ns[i], listOfLists[[i]][v[i]]) else {
				t = listOfLists[[i]][[v[i]]];
				# list of vectors
				t = (if (!is.list(t)) {
					# define name from higher level
					listKeyValue(firstDef(
						names(listOfLists[[i]])[v[i]], ns[i]
					), list(t))
					# <A> probably better and correct
					#listKeyValue(ns[i], list(t))
				} else if (is.null(names(t))) listKeyValue(ns[i], t) else t);
				t
			}
		l = merge.lists(l, new);
	}
	l
}

# take indeces given by v from a nested list
# namesOfLists: take the name of the list at the position in v
#	if null, take first element or leave aggregation to the function aggregator
# aggregator: called with the final result, should flatten existing lists into characters
lists.splice = function(listOfLists, v, namesOfLists = FALSE, aggregator = NULL, null2na = TRUE) {
	ns = names(listOfLists);
	l = lapply(1:length(ns), function(i) {
		name = ns[i];
		e = listOfLists[[i]][v[i]];
		r = if (!is.list(e)) e else {
			f = if (namesOfLists) {
				g = names(e)[1];
				# handle name == NULL
				if (is.null(g)) {
					# make an attempt later to print element
					#if (!is.null(aggregator)) e[[1]] else e[[1]][[1]]
					if (!is.null(aggregator))
						e[[1]] else
						join(as.character(e[[1]][[1]]), ", ")
				} else g
			} else e[[1]];
		}
		r
	});
	if (null2na) l = lapply(l, function(e)ifelse(is.null(e), NA, e));
	if (!is.null(aggregator)) l = aggregator(listKeyValue(ns, l), v, l);
	l
}

# dictionary produced by lists.splice, v: splice vector, l: aggregated list (w/o names)
merge.multi.symbolizer = function(d, v, l)unlist.n(d, 1);

merge.multi.list.symbolic = function(modelList, ..., symbolizer = NULL) {
	modelSize = lapply(modelList, function(m)1:length(m));
	models = merge.multi.list(modelSize, ...);
	namesDf = if (is.null(symbolizer)) names(modelList) else NULL;
	df0 = sapply(1:nrow(models), function(i, ...) {
		r = lists.splice(modelList, unlist(models[i, ]),
			namesOfLists = TRUE, aggregator = symbolizer);
		r
	});
	r = Df_(df0, t_ = TRUE, names = namesDf);
	r
}

inlist = function(l)lapply(l, function(e)list(e));
Inlist = function(...)inlist(list(...));

Do.callIm = function(im__f, args, ..., restrictArgs = TRUE, callMode = 'inline') {
	if (callMode == 'inlist') {
		.do.call(im__f, c(args, list(...)), restrictArgs = restrictArgs)
	} else if (callMode == 'list') {
		im__f(unlist.n(args, 1, reset = TRUE), ...)
	} else if (callMode == 'inline') {
		args = c(merge.lists(args, listOfLists = TRUE), list(...));
		.do.call(im__f, args, restrictArgs = restrictArgs)
	} else stop('Unknown call mode');
}

Kronecker = function(l, ...) {
	if (length(l) == 1) return(l[[1]]);
	kronecker(l[[1]], Kronecker(l[-1], ...), ...);
}


# <!> should be backwards compatible with iterateModels_old, not tested
# modelList: list of lists/vectors; encapuslate blocks of parameters in another level of lists
# Example:
#
#' Iterate combinations of parameters
#'
#' This function takes a list of parameters for which several values are to be evaluated. These values can be vectors of numbers or lists that contain blocks of parameters. All combinations are formed and passed to a user supplied function \code{f_iterate()}. This functions takes an index of the combination together with parameter values. Argument \code{callWithList} controls whether there is exactly one argument per parameter position or wether one more step of unlisting takes place. In case that a block of parameters is supplied, all values of the block are passed as individual arguments to \code{f_iterate()} in case \code{callWithList == FALSE}.
#'
#' #@param selectIdcs restrict models to the given indeces
#' @param modelList list specifying the models (see details)
#' @param models matrix containing indeces to sub-models (see details)
#' @param f_iterate function to be iterated across models
#' @param callWithList boolean to indicate whether model combination is to be supplied as a list.
#'   Otherwise model specification is inlined as arguments (see details)
#' @param callMode 'inline', 'list', 'inlist'
#' @param restrictArgs boolean to indicate whether over-supplied arguments (with respect to \code{f_iterate()})
#"   should be ignored. Otherwise, an error will be raised.
#' @param parallel boolean to inidcate whether iteration should be parallelized with
#'    \code{parallelize.dynamic}
#' @param lapply__ the iterator to be used (ignored at this moment)
#' @param ... extra arguments to be passed to \code{f_iterate()}
#' @return list containing the result of \code{f_iterate()} for all paramter combinations
#'
# #' @examples
# #' \dontrun{
# #' modelList = list(global = list(list(a=1, b=2)), N = c(1, 2, 3));
# #' print(iterateModels(modelList));
# #' modelList = list(N = c(1, 2, 3), parsAsBlock = list(list(list(c = 1, d = 2)),
# #'   list(list(c = 3, d = 4))));
# #' print(iterateModels(modelList));
# #' # ensure elements on A are given as a block (list)
# #' A = list(list(a = 1, b = 2), list(a = 3, b = 5));
# #' modelList = list(N = inlist(A), parsAsBlock = list(list(list(c = 1, d = 2)),
# #'   list(list(c = 3, d = 4))));
# #' print(iterateModels(modelList));
# #' # shorter version of the above
# #' modelList = list(N = Inlist(list(a = 1, b = 2), list(a = 3, b = 5)),
# #'   parsAsBlock = Inlist(list(c = 1, d = 2), list(c = 3, d = 4)));
# #' print(iterateModels(modelList));
# #' # inline calling
# #' modelList = list(N = list(list(a = 1, b = 2), list(a = 3, b = 5)),
# #'   parsAsBlock = list(list(c = 1, d = 2), list(c = 3, d = 4)));
# #' print(iterateModels(modelList));
# #' }
iterateModels_raw = function(modelList, models, f_iterate = function(...)list(...), ...,
	callWithList = FALSE, callMode = NULL, restrictArgs = TRUE, parallel = FALSE, lapply__) {
	if (!parallel) Lapply = lapply;
	if (is.null(callMode)) callMode = if (callWithList) 'list' else 'inline';
	# model indeces contains the original positions in models
	# this allows reordering of execution, eg with reverseEvaluationOrder
	r = Lapply(1:nrow(models), function(i, ..., im__f, im__model_idcs) {
		args = c(list(i = list(i = im__model_idcs[i])), list.takenFrom(modelList, unlist(models[i, ])));
#if (callMode == 'list') browser();
		Do.callIm(im__f, args, ..., restrictArgs = restrictArgs, callMode = callMode);
	}, ..., im__f = f_iterate, im__model_idcs = as.integer(row.names(models)));
	r
}

# <i> refactor iterateModels to use iterateModels_prepare
iterateModels_prepare = function(modelList, .constraint = NULL,
	callWithList = FALSE, callMode = NULL, restrictArgs = TRUE, selectIdcs = NULL, .first.constant = TRUE) {
	# <p> preparation
	if (is.null(callMode)) callMode = if (callWithList) 'list' else 'inline';

	modelSize = lapply(modelList, function(m)1:length(m));
	models = merge.multi.list(modelSize, .first.constant = .first.constant);

	# <p> handle constraints
	selC = if (is.null(.constraint)) TRUE else
		unlist(iterateModels_raw(modelList, models, f_iterate = .constraint,
			parallel = FALSE, callMode = callMode, restrictArgs = restrictArgs));
	selI = if (is.null(selectIdcs)) TRUE else 1:nrow(models) %in% selectIdcs;
	#	apply constraints
	models = models[selC & selI, , drop = FALSE];
	r = list(
		modelsRaw = models,
		selection = selC & selI,
		models = models
	);
	r
}

iterateModelsDefaultSymbolizer = function(i, ...) {
	l = list(...);
	r = lapply(l, function(e)unlist(as.character(unlist(e)[1])));
	r
}
iterateModelsJoinSymbolizer = function(i, ..., sep = ':') {
	l = list(...);
	r = lapply(l, function(e)join(unlist(as.character(unlist(e))), sep));
	r
}
iterateModelsSymbolizer = function(i, ..., im_symbolizer, im_symbolizerMode) {
	l = list(...);
	l0 = iterateModelsDefaultSymbolizer(i, ...);
	l1 = .do.call(im_symbolizer, c(list(i = i), list(...)), restrictArgs = TRUE);
	r = merge.lists(l0, l1);
	r
}

# <i> make name of supplied model index, currently 'i', configurable
iterateModels = function(modelList, f = function(...)list(...), ...,
	.constraint = NULL, .clRunLocal = TRUE, .resultsOnly = FALSE, .unlist = 0,
	callWithList = FALSE, callMode = NULL,
	symbolizer = iterateModelsDefaultSymbolizer, symbolizerMode = 'inlist',
	restrictArgs = TRUE, selectIdcs = NULL,
	.first.constant = TRUE, parallel = FALSE, lapply__, reverseEvaluationOrder = TRUE,
	modelTags = FALSE) {
	# <p> pre-conditions
	nsDupl = duplicated(names(modelList));
	if (any(nsDupl))
		stop(con('iterateModels: duplicated modelList entries: ', join(names(modelList)[nsDupl], ', ')));

	# <p> preparation
	if (is.null(callMode)) callMode = if (callWithList) 'list' else 'inline';

	# <p> produce raw combinations
	modelSize = lapply(modelList, function(m)1:length(m));
	models = merge.multi.list(modelSize, .first.constant = .first.constant);
# 	models_symbolic = merge.multi.list.symbolic(modelList,
# 		symbolizer = symbolizer, .first.constant = .first.constant);
	models_symbolic = do.call(rbind, iterateModels_raw(modelList, models, iterateModelsSymbolizer,
		callMode = 'inlist', parallel = FALSE,
		im_symbolizerMode = symbolizerMode, im_symbolizer = symbolizer));

	# <p> handle constraints
	selC = if (is.null(.constraint)) TRUE else
		unlist(iterateModels_raw(modelList, models, f_iterate = .constraint,
			callMode = callMode, restrictArgs = restrictArgs, ..., parallel = FALSE));
	selI = if (is.null(selectIdcs)) TRUE else 1:nrow(models) %in% selectIdcs;
	# <p> apply constraints
	models = models[selC & selI, , drop = FALSE];
	models_symbolic = models_symbolic[selC & selI, , drop = FALSE];

	# <p> models to be iterated
	modelsIt = if (reverseEvaluationOrder) models[rev(1:nrow(models)), , drop = FALSE] else models;
	metaStartTime = Sys.time();
	r = iterateModels_raw(modelList, modelsIt, f_iterate = f,
		callMode = callMode, restrictArgs = restrictArgs, ..., parallel = parallel);
	metaStopTime = Sys.time();
	if (reverseEvaluationOrder) r = rev(r);
	if (modelTags) {
		ns = dimnames(models_symbolic)[[2]]
		names(r) = apply(models_symbolic, 1, function(mr)
			join(apply(cbind(ns, mr), 1, join, sep = ':'), sep = '-'));
		#dimnames(models_symbolic)[[1]] = names(r);
	}
	r = if (.resultsOnly) r else list(
		models = models,
		results = r,
		models_symbolic = models_symbolic,
		meta = list(
			time = list(start = metaStartTime, stop = metaStopTime, duration = metaStopTime - metaStartTime)
		)
	);
	r = unlist.n(r, .unlist);
	r
}

iterateModelsExpand = function(modelList, .constraint = NULL) {
	modelSize = lapply(modelList, function(m)1:length(m));
	models = merge.multi.list(modelSize, .constraint = .constraint);
	r = list(
		models = models,
		models_symbolic = merge.multi.list.symbolic(modelList, .constraint = .constraint)
	);
	r
}

IterateModelsExpand = function(modelList, .constraint = NULL) {
	iterateModels(modelList, identity, .constraint = .constraint, callWithList = TRUE)$results
}

# reverse effect of .retern.lists = TRUE
#	list.to.df(merge.multi.list(..., .return.lists = TRUE)) === merge.multi.list(..., .return.lists = FALSE)
list.to.df = function(l)t(sapply(l, function(e)e))

merge.multi = function(..., .col.names = NULL, .col.names.prefix = "X",
	.return.lists = FALSE, stringsAsFactors = FALSE, .constraint = NULL, .first.constant = TRUE) {
	merge.multi.list(list(...), .col.names = .col.names, .return.lists = .return.lists,
		stringsAsFactors = stringsAsFactors, .constraint = .constraint, .first.constant = .first.constant)
}

merge.multi.dfs = function(l, .first.constant = TRUE, all = TRUE, stringsAsFactors = FALSE, ...) {
	if (.first.constant) l = rev(l);
	if (length(l) >= 1) for (i in 1:length(l)) {
		df1 = data.frame.types(l[[i]], stringsAsFactors = stringsAsFactors);
		df0 = if (i > 1) merge(df0, df1, all = all, ...) else df1;
	}
	if (.first.constant) df0 = df0[, rev(names(df0)), drop = FALSE];
	df0
}

Merge = function(x, y, by = intersect(names(x), names(y)), ..., safemerge = TRUE, stableByX = FALSE) {
	if (stableByX) x = data.frame(x, MergeStableByX = 1:nrow(x));
	if (safemerge && length(by) == 0) {
		stop(sprintf('Merge: safemerge triggered. No common columns between "%s" and "%s"',
			join(names(x), sep = ','), join(names(y), sep = ',')))
	}
	r = merge(x = x, y = y, by = by, ...);
	if (stableByX) {
		indexCol = which(names(r) == 'MergeStableByX');
		r = r[order(r$MergeStableByX), -indexCol, drop = FALSE];
	}
	r
}

MergeByRowNames = function(x, y, ...) {
	dMerge = Merge(Df(x, ROW_NAMES__ = row.names(x)), Df(y, ROW_NAMES__ = row.names(y)),
		by = 'ROW_NAMES__', ...)
	Df_(dMerge, min_ = 'ROW_NAMES__', row.names = dMerge$ROW_NAMES__)
}

# ids: variables identifying rows in final table
# vars: each combination of vars gets transformed to an own column
# <!> not tested for length(ids) > 1 || length(rvars) > 1
# blockVars: should the repeated vars go in blocks or be meshed for vars
#
# Examples:
# intersection table
# i = intersectSetsCount(sets);
# reshape.wide(Df(i$models_symbolic, count = unlist(i$results)), 's1', 's2');
reshape.wide = function(d, ids, vars, blockVars = FALSE, reverseNames = FALSE, sort.by.ids = TRUE) {
	# remaining vars
	rvars = setdiff(names(d), union(ids, vars));
	# levels of variables used in the long expansion
	levls = lapply(vars, function(v)unique(as.character(d[[v]])));
	# combinations at the varying vars as passed to vars
	cbs = merge.multi.list(levls, .col.names = vars, .first.constant = !blockVars);
	# repvars: repeated variables
	repvars = merge.multi.list(c(list(rvars), levls),
		.first.constant = !blockVars, .col.names = c("..var", vars));
	varnames = apply(repvars, 1, function(r)join(if (reverseNames) rev(r) else r, "."));

	r0 = data.frame.types(unique(d[, ids], drop = FALSE), names = ids);
	r1 = data.frame.types(apply(r0, 1, function(r) {
		# <p> isolate rows which match to current id columns
		ids = which(apply(d[, ids, drop = FALSE], 1, function(id)all(id == r)));
		d1 = d[ids, ];
		# <p> construct vector of repeated values
		vs = sapply(1:dim(cbs)[1], function(i) {
			# <A> should be equal to one
			row = which(apply(d1[, vars, drop = FALSE], 1, function(r)all(r == cbs[i, ])));
			v = if (length(row) != 1) rep(NA, length(rvars)) else d1[row, rvars];
			v
		});
		# heed blockVars
		vs = as.vector(unlist(if (!blockVars) t(vs) else vs));
		vs
	}), do.transpose = TRUE, names = varnames);
	r = data.frame(r0, r1);
	if (sort.by.ids) r = r[order.df(r, ids), ];
	row.names(r) = NULL;
	r
}

#' Convert data in wide format to long format
#' 
#' Long format duplicates certain columns and adds rows for which one new column hold values coming
#' from a set of columns in wide format. Does not allow for parallel reshaping.
#'
#' @param d data frame with columns in wide format
#' @param vars columns in wide format by name or index
#' @param factors \code{vars} can be grouped. For each level of \code{factor} a new row is created. Implies
#'			that \code{length(vars)} is a multiple of \code{length(levels(factor))}
#' @param factorColumn name of the column to be created for the factor
#' @param valueColumn name of the new column of values that were in wide format
# factors: provide factor combinations explicitly for vars (otherwise split by '.', <i>)
#' @param rowNamesAs name of the column that should contain row names
#' @return data frame in long format
# #' @examples
# #' \dontrun{
# #'	#reshape variables 2:9 (forming two groups: case/ctr), value of which is named 'group'
# #'	# the shortened columns will get names valueColumn
# #'	d0 = reshape.long(d, vars = 2:9, factors = c('case', 'ctr'), factorColumn = 'group',
# #'		valueColumn = c('AA', 'AG', 'GG', 'tot'));
# #'
# #'	# reshape several grouped columns
# #' 	d2 = reshape.long(d1, vars = avu(vs),
# #'		factorColumn = 'time', valueColumn = valueNames, factors = as.factor(1:3));
# #'	}
reshape.long = function(d, vars = NULL, factorColumn = 'factor', valueColumn = 'value',
	factors = as.factor(vars), rowNamesAs = NULL) {
	if (is.null(vars)) vars = names(d);
	# make rownames an extra column
	if (!is.null(rowNamesAs)) {
		d = data.frame(reshape_row_names__ = rownames(d), d);
		names(d)[1] = rowNamesAs;
	}
	# indeces of columns vars
	Ivars = .df.cols(d, vars);
	# remaining vars
	rvars = setdiff(1:length(names(d)), Ivars);
	# names thereof
	Nrvars = names(d)[rvars];

	# how wide are the blocks?
	S = length(vars) / length(factors);
	# columns of intermediate data.frame
	N = length(rvars);
	# create list of data frames
	dfs = lapply(1:nrow(d), function(i) {
		st = d[i, rvars];	# start of the new row
		df0 = data.frame(factors, value =  matrix(d[i, vars], nrow = length(factors), byrow = TRUE));
		df1 = data.frame(st, df0, row.names = NULL);
		names(df1) = c(Nrvars, factorColumn, valueColumn);
		df1
	});
	#r = rbindDataFrames(dfs, do.unlist = TRUE, useDisk = useDisk);
	r = do.call(rbind, dfs);
	r
}

DfUniqueRowsByCols = function(d, cols) {
	row.names(d) = NULL;
	as.integer(row.names(unique(d[, cols, drop = FALSE])))
}

#' Reduce data frame to be unique on subset of columns
#'
#' Reduce data frame by picking the first row of blocks for which \code{cols} has the same values.
#'
#' @param d data frame to be made unique
#' @param cols columns for which the reduced data frame has to be unique
#' @param drop argument passed to subset selection \code{`[`}
#' @return the reduced data frame
DfUniqueByCols = uniqueByCols = function(d, cols, drop = FALSE) {
	d[DfUniqueRowsByCols(d, cols), , drop = drop]
}

# robustly access columns: if column name is NA, add column of NAs
# changed as of 28.11.2018 <!> rely on only use by Reshape.long.raw
DfSelectCols = function(d, vars) {
	# changed as of 28.11.2018
	#d0 = do.call(cbind, lapply(vars, function(v)if (is.na(v)) NA else d[, v]));
	d0 = do.call(cbind, lapply(vars, function(v)if (is.na(v)) NA else d[, v, drop = FALSE]));
	d0
}

#	vars:	columns for which are to be in long format
#	lvMap:	mapping from levels to columns in d (wide columns)
Reshape.long.raw = function(d, vars, lvMap, factorColumn = 'repeat',
	valuePostfix = '_long', varsLong = paste(vars, valuePostfix, sep = '')) {
	if (any(sapply(lvMap, length) != length(vars)))
		stop('selected variables per level of different length to result columns');
	# remaining vars
	rvars = setdiff(names(d), na.omit(Avu(lvMap)));
	# levels
	lvls = names(lvMap);
	# create list of data frames
	dfs = lapply(1:nrow(d), function(i) {
		dR = d[i, rvars, drop = FALSE];	# fixed, repeated part of the data set
		d0L = lapply(lvls, function(l)DfSelectCols(d[i, , drop = FALSE], lvMap[[l]]));
		d0 = do.call(rbind, lapply(d0L, setNames, varsLong));
		d1 = Df(index = lvls,
			Df_(d0, row.names = NULL), Df_(dR, row.names = NULL), names = c(factorColumn, varsLong));
		#d1 = Df(index = lvls, d0, dR, names = c(factorColumn, varsLong));
		d1
	});
	r = do.call(rbind, dfs);
	r
}

Reshape.levelMap_re = function(ns, vars, factorsRe) {
	# regular expressions for columns to be reshaped
	Res = sapply(vars, function(v)Sprintf(factorsRe, COLIDENT = v));
	# perform RE search
	lvlsRaw = Regex(Res, ns);
	lvlsRawL = sapply(lvlsRaw, length);

	# levels of index/reshape column
	#cols = Df_(lvlsRaw, names = vars);
	# level belonging to column (non-simplifying Regex)
	lvCol_old = RegexL(Res, ns, captures = TRUE);
	# allow to concat matches (several captures per Re)
	lvCap = lapply(lapply(Regexpr(Res, ns, captures = TRUE, reSimplify = FALSE), setNames, ns), unlist);
	lvCol = lapply(lvCap, filterList, f = function(e)e != '');
	# prepare level -> column mapping
	names(lvCol) = vars;
	lvls = unique(Avu(lvCol));
	if (any(sapply(lvCol, length) < sapply(lvlsRaw, length))) {
		print(list(lvlsRaw = lvlsRaw, lvls = lvCol));
		stop('Could not extract values for levels for all variables');
	}
	# 	# 28.11.2018: allow levels to be embedded
	# 	if (!all(lvlsRawL == lvlsRawL[1])) {
	# 		print(lvlsRaw);
	# 		stop('Different number of levels per factor');
	# 	}
	# map from level to columns
	lvMap = nlapply(lvls, function(l)Avu(nina(lapply(lvCol, function(c)names(c)[which(c == l)]))));
	return(lvMap);
}

Reshape.levelMap_list = function(ns, vars, factorsRe) {
	lels = sapply(vars, is.list);	# list elements
	vL = vars[lels];

	# <p> check input
	unmatched = unlist(vL)[which(!(unlist(vL) %in% ns))];
	if (length(unmatched) > 0)
		stop(Sprintf('reshape variables [%{r}s] do not exist in data', r = join(unmatched, ', ')));

	# <p> process re-matched variables
	lvMapRe = if (any(!lels)) Reshape.levelMap_re(ns, unlist(vars[!lels]), factorsRe) else list();
	# <p> take levels of repetition from Re variables if available, else enumerate
	levels = if (length(lvMapRe) > 0) names(lvMapRe) else as.character(1:length(vL[[1]]));

	# <p> construct level-map for explicit variable names
	lvMapL = lapply(Df(sapply(vL, identity), t_ = TRUE), unlist);
	names(lvMapL) = levels;
	lvMap = merge.lists(lvMapRe, lvMapL, concat = TRUE)
	lvMap
}

Reshape.levelMap = function(ns, vars, factorsRe) {
	if (is.character(vars)) return(Reshape.levelMap_re(ns, vars, factorsRe));
	if (!is.list(vars)) stop('invalid variable specification');
	lvMap = Reshape.levelMap_list(ns, vars, factorsRe);
	lvMap
}

# allow parallel re-shaping, i.e. take columns of form 'prefix.\d' and take \d as the value for the new
#	index column (reshape-column)
#	vars: prefix of columns to be reshaped
#	factorsRe: re to append to vars to identify wide columns
Reshape.long = function(d, vars, factorColumn = 'repeat', valuePostfix = '_long',
	factors = NULL, factorsRe = '^%{COLIDENT}s[._]?(\\d+)', useDisk = FALSE, rowNamesAs = NULL,
	varsLong = paste(vars, valuePostfix, sep = '')) {

	lvMap = Reshape.levelMap(names(d), vars, factorsRe);
	if (is.list(vars)) {
		nsVars = names(vars);
		varsLong[nsVars != ''] = nsVars[nsVars != ''];
	}
	Reshape.long.raw(d, vars, lvMap,
		factorColumn = factorColumn, valuePostfix = valuePostfix, varsLong = varsLong);
}

# reshape rows in blocks to avoid memory exhaustion
Reshape.long.byParts = function(d, ..., N = 1e4, path = tempfile(), filter = NULL) {
	Nrow = nrow(d);
	Nparts = ceiling(Nrow / N);

	#Nparts = 2;
	for (i in 1:Nparts) {
		dP = d[ (N*(i - 1) + 1):min((N*i), Nrow), ];
		dL = Reshape.long(dP, ...);
		if (notE(filter)) dL = filter(dL);
		write.table(dL, file = path, col.names = i == 1, append = i != 1, row.names = F);
	}
	gc();
	return(readTable(Sprintf('[SEP=S,HEADER=T]:%{path}s')));
}

byParts = function(d, fn, ..., N = 1e4, path = tempfile(), filter = NULL) {
	Nrow = nrow(d);
	Nparts = ceiling(Nrow / N);

	#Nparts = 2;
	for (i in 1:Nparts) {
		dP = d[ (N*(i - 1) + 1):min((N*i), Nrow), ];
		dL = fn(dP, ...);
		if (notE(filter)) dL = filter(dL);
		write.table(apply(dL, 2, as.character), file = path, col.names = i == 1, append = i != 1, row.names = F);
	}
	gc();
	return(readTable(Sprintf('[SEP=S,HEADER=T]:%{path}s')));
}

reshape.long.byParts = function(d, ..., N = 1e4, path = tempfile(), filter = NULL) {
	byParts(d, reshape.long, ..., N = N, path = path, filter = filter);
}

#
# <p> string functions
#

uc.first = firstUpper = function(s) {
	paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "", collapse = "");
}
substrM1 = function(s)substr(s, 1, nchar(s) - 1)
strAbbr = function(s, N = 15, ellipsis = '...') {
	r = if (nchar(s) <= N) s else join(c(substr(s, 1, N - nchar(ellipsis)), ellipsis));
	r
}

deduplicateLabels = function(v, labels = v[duplicated(v)], sep = '-', firstUntouched = TRUE) {
	for (label in labels) {
		idcs = which(v == label);
		if (firstUntouched) idcs = shift(idcs);
		v[idcs] = paste(label, 1:length(idcs), sep = sep);
	}
	v
}

Trimws = function(s)join(sub('^\t', '', splitString("\n", s)), '\n');

# non-empty string
nonEmpty = function(e)nif(e != '')
# non-empty, unique
vecCharNEU = function(v)unique(Filter(nonEmpty, v))

#
#	<p> factor transformations for data frames
#

dataExpandedNames = function(data) {
	dnames = unlist(lapply(names(data), function(v){
		if (is.factor(data[[v]])) paste(v, 1:(length(levels(data[[v]])) - 1), sep = "") else v;
	}));
	dnames
}
# model.matrix removes missing columns and could not be tweaked into working
dataExpandFactors = function(data, vars =  NULL) {
	if (is.null(vars)) vars = names(data);
	d0 = lapply(vars, function(v) {
		if (is.factor(data[[v]])) {
			ls = levels(data[[v]]);
			dcNa = rep(NA, length(ls) - 1);	# missing data coding
			dc = rep(0, length(ls) - 1);	# dummy coding
			sapply(data[[v]], function(e) {
				if (is.na(e)) return(dcNa);
				i = which(e == ls);
				if (i == 1) return(dc);
				dc[i - 1] = 1;
				return(dc);
			});
		} else data[[v]];
	});
	d0names = dataExpandedNames(data[, vars]);
	# re-transform data
	d1 = data.frame(matrix(unlist(lapply(d0, function(e)t(e))), ncol = length(d0names), byrow = FALSE));
	names(d1) = d0names;
	d1
}
coefficientNamesForData = function(vars, data) {
	lnames = dataExpandedNames(data);	# names of levels of factors
	cnames = lnames[unlist(sapply(vars, function(v)which.indeces(v, lnames, regex = TRUE)))];
	cnames
}

#
# <p> statistic oriented data frame manipulation
#

variableIndecesForData = function(d, vars, varsArePrefixes = TRUE, varRegex = '%s.*') {
	if (varsArePrefixes) vars = sapply(vars, function(e)sprintf(varRegex, e));
	which.indeces(vars, names(d), regex = TRUE, match.multi = TRUE)
}
variablesForData = function(d, vars, varsArePrefixes = TRUE, varRegex = '%s.*') {
	names(d)[variableIndecesForData(d, vars, varsArePrefixes, varRegex)]
}

subData = function(d, vars, varsArePrefixes = TRUE) {
	dfr = d[, variableIndecesForData(d, vars, varsArePrefixes), drop = FALSE];
	dfr
}

subDataFromFormula = function(d, formula, responseIsPrefix = TRUE, covariateIsPrefix = TRUE) {
	resp = formula.response(formula);
	cov = formula.covariates(formula);
	ns = names(d);
	r = list(
		response = subData(d, resp, responseIsPrefix),
		covariate = subData(d, cov, covariateIsPrefix)
	);
	r
}

#
#	<p> graph functions
#

sub.graph.merge = function(df, leader, follower) {
	# next transitive step
	r0 = merge(df, data.frame(leader = leader, follower = follower), by = 'follower');
	# add new connections
	r1 = rbind(df, data.frame(follower = r0$leader.y, leader = r0$leader.x, cluster = r0$cluster));
	# symmetric closure
	r1 = rbind(r1, data.frame(follower = r1$leader, leader = r1$follower, cluster = r1$cluster))
	# form clusters by selecting min cluster number per connection
	r1 = r1[order(r1$cluster), ];
	row.names(r1) = 1:dim(r1)[1];
	r2 = unique(r1[, c('leader', 'follower')]);
	# select unique rows (first occurunce selects cluster)
	r = r1[as.integer(row.names(r2)), ];
	# pretty sort data frame
	r = r[order(r$cluster), ];
	r
}
# form clusters from a relationally defined hierarchy
sub.graph = function(df) {
	df = as.data.frame(df);
	names(df)[1:2] = c('follower', 'leader');
	df = df[order(df$follower), ];
	# seed clusters
	ids = sort(unique(df$follower));
	idsC = as.character(ids);
	counts = lapply(ids, function(id)sum(df$follower == id));
	names(counts) = idsC;
	clusters = unlist(sapply(idsC, function(id){ rep(as.integer(id), counts[[id]]) }));

	df = cbind(df, data.frame(cluster = rep(clusters, 2)));
	df = unique(rbind(df, data.frame(follower = df$leader, leader = df$follower, cluster = df$cluster)));
	# receiving frame
	df0 = df;
	# results with clusters
	i = 1;
	repeat {
		Nrows = dim(df0)[1];
		cls = df0$clusters;
		# add transitive connections
		df0 = sub.graph.merge(df0, follower = df0$leader, leader = df0$follower);
		if (dim(df0)[1] == Nrows && all(cls == df0$clusters)) break();
	}
	df0 = df0[order(df0$cluster), ];
	cIds = unique(df0$cluster);
	cls = lapply(cIds, function(id)unique(avu(df0[df0$cluster == id, c('follower', 'leader')])));
	cls
}

#
#	<p> formulas
#

# formula: formula as a character string with wildcard character '%'
# 	<!>: assume whitespace separation in formula between terms
#	<!>: write interaction with spaces <!> such as in:
#		f = 'MTOTLOS_binair ~ ZRES% + sq(ZRes%) + ( ZRES% )^2';
formula.re = function(formula, data, ignore.case = FALSE, re.string = '.*') {
	vars = names(data);
	#regex = '(?:([A-Za-z_.]+[A-Za-z0-9_.]*)[(])?([A-Za-z.]+[%][A-Za-z0-9.%_]*)(?:[)])?';
	#			function names				(    regex						   )
	#regex = '(?:([A-Za-z_.]+[A-Za-z0-9_.]*)[(])?([A-Za-z%.]+[A-Za-z0-9.%_]*)(?:[)])?';
	# allow backslash quoting
	regex = '(?:([A-Za-z_.\\\\]+[A-Za-z0-9_.\\\\]*)[(])?([A-Za-z%.\\\\]+[A-Za-z0-9.%_\\\\]*)(?:[)])?';
	patterns = unique(fetchRegexpr(regex, formula, ignore.case = ignore.case));
	subst = nlapply(patterns, function(p) {
		comps = fetchRegexpr(regex, p, captureN = c('fct', 'var'), ignore.case = ignore.case)[[1]];
		p = sprintf("^%s$", gsub('%', re.string, comps$var));
		mvars = vars[sapply(vars, function(v)regexpr(p, v, perl = TRUE, ignore.case = ignore.case)>=0)];
		if (comps$fct != '') {
			varf = sprintf('%s', paste(sapply(mvars, function(v)sprintf('%s(%s)', comps$fct, v)),
				collapse = " + "));
		} else {
			varf = sprintf('%s', paste(mvars, collapse = " + "));
		}
		varf
	});
	formula1 = mergeDictToString(subst, formula);
	formulaExp = as.formula(formula1);
	formulaExp
}

formula.response = function(f) {
	#r = fetchRegexpr('[^\\s~][^~]*?(?=\\s*~)', if (is.formula(f)) deparse(f) else f);
	f = if (class(f) == 'formula') Deparse(f) else f;
	r = as.character(fetchRegexpr('^\\s*([^~]*?)(?:\\s*~)', f, captures = TRUE));
	# <p> version 2
	#fs = as.character(as.formula(as.character(f)));	# "~" "response" "covs"
	#r = fs[2];
	# <p> version 1
	#f = as.formula(f);
	#r = all.vars(f)[attr(terms(f), "response")];	# fails to work on 'response ~ .'
	r
}
formula.rhs = function(f, noTilde = FALSE, as_character = FALSE) {
	rhs = fetchRegexpr('[~](.*)', if (!is.character(f)) formula.to.character(f) else f, captures = TRUE);
	r = if (noTilde) rhs else con('~', rhs);
	r = if (as_character) r else as.formula(r);
	r
}
formula.covariates = function(f) {
	covs = all.vars(formula.rhs(f));
	#covs = setdiff(all.vars(as.formula(f)), formula.response(f));
	covs
}
formula.vars = function(f)union(formula.response(f), formula.covariates(f));
#formula.vars = function(f)all.vars(as.formula(f));

formula.nullModel = function(f) {
	r = formula.response(f);
	fn = as.formula(sprintf("%s ~ 1", r));
	fn
}
formula.to.character = function(f)join(deparse(as.formula(f)), '');
Formula.to.character = function(f)ifelse(is.character(f), f, formula.to.character(f));

formula.expand = function(f, data) {
	if (is.null(f)) return(NULL);
	if (any(all.vars(formula.rhs(f)) == '.')) {
		covs = setdiff(names(data), as.character(formula.response(f)));
		f = formula.set.rhs(f, vars.as.rhs(covs));
	}
	return(f);
}

formula2filename = function(f) {
	fs = join(f, sep = '');
	filename = mergeDictToString(list(
		`\\s+` = '',
		`_` = '-',
		`%` = ':',
		`Surv\\(.*\\)` = 'surv',
		MARKER = 'snp'
		# other components
	), fs, re = TRUE, doApplyValueMap = FALSE, doOrderKeys = FALSE);
	filename
}
data.vars = function(data, formula, re.string = '.*', ignore.case = FALSE) {
	all.vars(formula.re(formula = formula, data = data, re.string = re.string, ignore.case = ignore.case));
}
data.vars.after = function(data, col, skip = TRUE) {
	ns = names(data);
	ns[(which(ns == col) + skip):length(ns)]
}

dataColRange = function(data, from = NULL, to = NULL) {
	ns = names(data);
	start = if (is.integer(from)) from else (if (notE(from)) which(ns == from) else 1);
	stop = if (is.integer(to)) to else (if (notE(to)) which(ns == to) else ncol(data));
	data[, start:stop, drop = FALSE]
}


# select column names based on res, negation or literal names
# dataSelectVars(data, ~ cg + ab, ~ 0)
# dataSelectVars(data, list(~ cg, ~ !ab))
dataSelectVars = function(data, prefix = list(), fixed = list()) {
	# <p> input sanitation
	ns = names(data);
	if (class(prefix) == 'formula') prefix = list(prefix);
	if (class(fixed) == 'formula') fixed = list(fixed);

	# <p> fixed
	vsF = do.call(Union, lapply(fixed, function(e)setdiff(all.vars(e), '0')));
	# <p> prefix
	vsP = do.call(Union, lapply(prefix, function(e) {
		negation = class(as.list(e)[[2]]) == 'call' && as.list(e)[[2]][[1]] == '!';
		formula = join(c('~', join(sapply(all.vars(e), function(v)sprintf('%s%%', v)), ' + ')));
		vars = all.vars(formula.re(formula, data));
		if (negation) setdiff(ns, vars) else vars
	}));

	r = Union(vsF, vsP);
	r
}
dataSelectCols = function(data, prefix, fixed = ~ 0) {
	data[, dataSelectVars(data, prefix, fixed), drop = FALSE]
}


formula.add.rhs = function(f0, f1, envir = parent.frame()) {
	if (is.character(f1)) f1 = as.formula(paste0(c('~', paste(f1, collapse = ' + ')), collapse = ''));
	as.formula(join(c(
		formula.to.character(f0),
		formula.rhs(f1, noTilde = TRUE, as_character = TRUE)), '+'), env = envir)
}
vars.as.rhs = function(v)as.formula(Sprintf('~ %{vars}s', vars = join(v, '+')))
formula.set.rhs = function(f0, f1, envir = parent.frame()) {
	as.formula(join(c(formula.response(f0), formula.rhs(f1, as_character = TRUE))), env = envir)
}
formula.add.responseByName = function(f0, response, envir = parent.frame()) {
	formula = join(c(response, formula.rhs(f0, noTilde = FALSE)), ' ');
	as.formula(formula, env = envir)
}
formula.add.response = function(f0, f1, envir = parent.frame()) {
	formula.add.responseByName(f0, formula.response(f1), envir = envir)
}
# <!><t> w/ transformations in formula
formula.predictors = function(f, data, dataFrameNames = TRUE) {
	if (formula.rhs(f) == ~ 1) return('(Intercept)');
	#mm = model.matrix(model.frame(formula.rhs(f), data), data);
	mm = model.matrix(formula.rhs(f), data);
	ns = dimnames(mm)[[2]];

	# <p> create data frame to extract proper names
# 	if (dataFrameNames) {
# 		df0 = as.data.frame(t(rep(1, length(ns))));
# 		names(df0) = ns;
#		ns = names(df0);
# 	}
	ns
}

# <!> cave survival
formulaRemoveTransformation = function(model) {
	respVar = setdiff(all.vars(model), all.vars(formula.rhs(model)));
	formula.add.response(formula.rhs(model), as.formula(Sprintf('%{respVar}s ~ 1')))
}

formulas.free = function(f1, f0, data) {
	setdiff(formula.predictors(f1, data), formula.predictors(f0, data))
}


# <i> use terms.formula from a (a + ... + z)^2 formula
# <i> merge.multi.list(rep.list(covs, 2), .constraint = is.ascending)
covariatePairs = function(covs) {
	pairs = merge(data.frame(c1 = 1:length(covs)), data.frame(c2 = 1:length(covs)));
	pairs = pairs[pairs[, 1] > pairs[ ,2], ];
	df = data.frame(c1 = covs[pairs[, 1]], c2 = covs[pairs[, 2]]);
	df
}

formulaWith = function(response = "y", covariates = "x") {
	if (!Nif(response)) response = '';
	as.formula(sprintf("%s ~ %s", response,  paste(covariates, collapse = "+")))
}

#
#	<p> set operations
#

minimax = function(v, min = -Inf, max = Inf) {
	r = ifelse(v < min, min, ifelse(v > max, max, v));
	r
}

#
#	<p> recycling
#

accessIdx = function(e, i, byRow = TRUE) {
	if (class(e) != 'matrix' || is.na(byRow)) e[i] else
		(if (byRow) e[i, , drop = FALSE] else e[, i, drop = FALSE])
}

# fixed as of 10.8.2018: different types not correctly handles <f>
Recycle = function(l, byRow = TRUE) {
	# determine recyling pattern
	# old version would not preserve type
	# Recycle = function(l)lapply(apply(do.call(cbind, l), 2, as.list), unlist)
	lTmp = lapply(l, function(e)1:length(e));
	rTmp = 	lapply(apply(do.call(cbind, lTmp), 2, as.list), unlist)
	# extract values per component
	r = lapply(seq_along(l), function(i)accessIdx(l[[i]], rTmp[[i]], byRow = byRow));
	return(setNames(r, names(l)));
}
recycle = function(...)Recycle(list(...));
recycleTo = function(..., to, simplify = TRUE) {
	r = recycle(to, ...)[-1];
	if (simplify && length(r) == 1) r[[1]] else r
}
#
#	RdataExt.R
#Wed Jan 15 11:56:40 CET 2020

# non pull-in funcitons

#
#	<§> string manipulation
#

say = function(...)cat(..., "\n");
printf = function(fmt, ...)cat(sprintf(fmt, ...));

#
#	<p> iteration
#

ByIndices = function(data, INDICES, USE.NAMES = FALSE) {
	if (class(INDICES) == 'formula') {
		rhs = all.vars(formula.rhs(INDICES));
		INDICES = if (USE.NAMES) Df_(data[, rhs, drop = F], as_character = rhs) else {
			combs = model_matrix_from_formula(INDICES, data, remove.intercept = length(rhs) > 0)$mm;
			setNames(lapply(1:ncol(combs), function(i)combs[, i]), names(combs));
		}
	# change on 13.1.2020; <!><i> test
	#} else if (class(INDICES) == 'data.frame') INDICES = Df_(idcs, as_character = names(idcs));
	} else if (class(INDICES) == 'data.frame') INDICES = Df_(INDICES, as_character = names(INDICES));
	INDICES
}

By = function(data, INDICES, FUN, ..., simplify = TRUE, RBIND = FALSE, USE.NAMES = FALSE, SEP = ':') {
	idcs = ByIndices(data, INDICES, USE.NAMES);
	r = by(data, idcs, FUN, ..., simplify = simplify);
	if (USE.NAMES) {
		ns = sapply(by(idcs, idcs, unique), join, sep = SEP);
		names(r) = ns;
	}
	if (RBIND) r = do.call(rbind, Lundrop2col(r));
	r
}

by2df = function (x, ..., Ncol = length(x[[1]])) {
    d <- dim(x)
    dn <- dimnames(x)
    dnn <- names(dn)
	df = lapply(X = seq_along(x), FUN = function(i, x, ...) {
        ii <- i - 1L
        nms = c()
        for (j in seq_along(dn)) {
            iii <- ii%%d[j] + 1L
            ii <- ii%/%d[j]
			nms = c(nms, dn[[j]][iii]);
		}
		# <!> not equivalent
		#nms = sapply(seq_along(dn), function(j)dn[[j]][ii %% d[j] + 1L])
        v = x[[i]]
        r = c(nms, if (is.null(v)) rep(NA, Ncol) else unlist(x[[i]]))
		r
	}, x, ...)
	df0 = Df_(do.call(rbind, df));
	names(df0) = c(names(dimnames(x)), names(x[[1]]));
	return(df0);
}

# analysis pattern using merge.multi.list
# i needs not to be an argument to f as .do.call strips excess arguments
iterateModels_old = function(modelList, f, ...,
	.constraint = NULL, .clRunLocal = T, .resultsOnly = F, .unlist = 0, lapply__ = clapply) {
	models = merge.multi.list(modelList, .constraint = .constraint);

	r = lapply__(1:dim(models)[1], function(i, ..., f__, models__) {
		args = c(list(i = i), as.list(models__[i, , drop = F]), list(...));
		.do.call(f__, args)
	}, ..., f__ = f, models__ = models);
	r = if (.resultsOnly) r else list(models = models, results = r);
	r = unlist.n(r, .unlist);
	r
}


#
#	<p> factors
#

factorFromFactors = function(d, sep = ';', safeNames = TRUE) {
	combs = unique(d[completeRows(d), , drop = F]);
	combsO = combs[order.df(combs), , drop = F];
	levels = apply(combsO, 1, function(comb)join(comb, sep));
	if (safeNames) levels = gsub(' ', '.', levels);
	combsI = Df(combsO, i = 1:nrow(combsO));
	combsM = merge(Df(d, j = 1:nrow(d)), combsI, all.x = T, sort = F);
	factorN = as.factor((levels[combsM$i])[order(combsM$j)]);
	factorN
}
# ~ cat1 + cat2
# create combinations from cat1/cat2, enumerate
factorFromFormula = function(d, form, sep = ';', safeNames = TRUE) {
	vars = formula.covariates(form);
	factorFromFactors(d[, vars, drop = F], sep = sep, safeNames = safeNames)
}
factorFromModelMatrix = function(mm, sep = ';') {
	combs = unique(mm);
	combsO = combs[order.df(combs), , drop = F];
	levels = apply(combsO, 1, function(comb)join(dimnames(mm)[[2]][comb], sep));
	combsI = Df(combsO, i = 1:nrow(combsO));
	#combsM = merge(Df(d, j = 1:nrow(d)), combsI, all.x = T, sort = F);
	combsM = merge(Df(mm, j = 1:nrow(mm)), combsI, all.x = T, sort = F);
	factorN = as.factor((levels[combsM$i])[order(combsM$j)]);
	factorN
}

#
#	<p> Attic
#

# better versions in TestMe
compare_print = function(r, e) {
	#require('compare');
	cmp = compare(model = r, comparison = e);
	if (!cmp$result) {
		print("Expectation not met (result != expectation):");
		print(r);
		print(e);
	}
	cmp$result
}

#
#	Rsystem.R
#Mon 27 Jun 2005 10:51:30 AM CEST 

#
#	<par> file handling
#

# <!><N> works only on atomic path
# <!> 5.1.2016: trailing slash leads to basename of ""
splitPath = function(path, removeQualifier = TRUE, ssh = FALSE, skipExists = FALSE) {
	if (is.null(path)) return(NULL);
	if (removeQualifier) {
		q = fetchRegexpr('(?<=^\\[).*?(?=\\]:)', path);
		if (length(q) > 0) path = substr(path, nchar(q) + 4, nchar(path));
	}
	sshm = list(user = '', host = '', userhost = '');
	if (ssh) {
		sshm = fetchRegexpr('^(?:(?:([a-z]\\w*)(?:@))?([a-z][\\w.]*):)?(.*)', path,
			ignore.case = TRUE, captureN = c('user', 'host', 'path'))[[1]];
		sshm$userhost = if (sshm$user != '') sprintf('%s@%s', sshm$user, sshm$host) else sshm$host;
		path = sshm$path;
	}

	#path = "abc/def.ext";
	#r.base = basename(path);
	#re = "([^.]*$)";
	#r = gregexpr(re, r.base)[[1]];
	#ext = substr(r.base, r[1], r[1] + attr(r, "match.length")[1] - 1);
	#ext = firstDef(fetchRegexpr('(?<=\\.)[^/.]+\\Z', path), '');
	ext = fetchRegexpr('(?<=\\.)[^/.]+\\Z', path);
	# take everything before ext and handle possible absence of '.'
	#base = substr(r.base, 1, r[1] - 1 - (ifelse(substr(r.base, r[1] - 1, r[1] - 1) == '.', 1, 0)));
	# reduce to file.ext
	Nchar = nchar(path);
	# replace leading '~'
	if (path == '~') {
		path = Sys.getenv('HOME');
	} else if (Nchar > 1 && substr(path, 1, 2) == '~/') {
		path = join(c(Sys.getenv('HOME'), substring(path, 2)), '');
	}
	Nchar = nchar(path);
	if (Nchar != 0 && substr(path, Nchar, Nchar) == '/') {
		base = '';
		dir = substr(path, 1, Nchar - 1);
	} else {
		base = basename(path);
		dir = dirname(path);
	}
	# base as yet still contains the file extension
	file = base;
	# chop off extension if present
	if (length(fetchRegexpr('\\.', base)) > 0) base = fetchRegexpr('\\A.*(?=\\.)', base);
	
	#pieces = regexpr(re, path, perl = TRUE);
	pieces = fetchRegexpr('([^.]+)', path);
	isAbsolute = Nchar != 0 && substr(path, 1, 1) == '/';
	# <N> disk is accessed
	exists = if (!skipExists) File.exists(path, host = sshm$userhost, ssh = FALSE) else NA;
	nonempty = exists && (file.info(path)$size > 0);
	ret = c(list(
		dir = dir,
		base = base,
		path = path,
		fullbase = sprintf("%s/%s", dir, base),
		ext = ext,
		file = file,
		isAbsolute = isAbsolute,
		absolute = if (isAbsolute) path else sprintf('%s/%s', getwd(), path),
		# fs properties
		exists = exists, nonempty = nonempty,
		# remote
		is.remote = !(sshm$user == '' && sshm$host == ''),
			user = sshm$user, host = sshm$host, userhost = sshm$userhost
	), if (removeQualifier && length(q) > 0)
		list(qualifier = q, qualifierFull = Sprintf('[%{q}s]:')) else
		list(qualifier = NA, qualifierFull = ''));
	ret
}
path.absolute = absolutePath = function(path, home.dir = TRUE, ssh = TRUE) {
	path = splitPath(path, ssh = ssh)$path;
	if (home.dir && nchar(path) >= 2 && substr(path, 1, 2) == "~/")
		path = sprintf("%s/%s", Sys.getenv('HOME'), substr(path, 3, nchar(path)));
	if (nchar(path) > 0 && substr(path, 1, 1) == "/") path else sprintf("%s/%s", getwd(), path)
}
pathSimplify = function(path) {
	path = gsub('(^./|/.(?=/|$))', '', path, perl = TRUE);
	return(path);
}

tempFileName = function(prefix, extension = NULL, digits = 6, retries = 5, inRtmp = FALSE,
	createDir = FALSE, home.dir = TRUE, doNotTouch = FALSE) {
	ext = if (is.null(extension)) '' else sprintf('.%s', extension);
	path = NULL;
	if (inRtmp) prefix = sprintf('%s/%s', tempdir(), prefix);
	if (home.dir) prefix = path.absolute(prefix, home.dir = home.dir);
	for (i in 1:retries) {
		path = sprintf('%s%0*d%s', prefix, digits, floor(runif(1) * 10^digits), ext);
		LogS(5, 'tempFileName trying path: %{path}s');
		if (!File.exists(path)) break;
	}
	if (File.exists(path))
		stop(sprintf('Could not create tempfile with prefix "%s" after %d retries', prefix, retries));
	# potential race condition <N>
	if (createDir)
		Dir.create(path, recursive = TRUE) else
		if (!doNotTouch) writeFile(path, '', mkpath = TRUE, ssh = TRUE);
	# # old implementation
	#path = tempfile(prefix);
	#cat('', file = path);	# touch path to lock name
	#path = sprintf("%s%s%s", path, ifelse(is.null(extension), "", "."),
	#	ifelse(is.null(extension), "", extension));
	Log(sprintf('Tempfilename:%s', path), 5);
	path
}
dirList = function(dir, regex = TRUE, case = TRUE, path = FALSE, absolute = FALSE) {
	base = splitPath(dir)$dir;
	files = list.files(base);
	if (regex) {
		re = splitPath(dir)$file;
		files = files[grep(re, files, perl = TRUE, ignore.case = !case)];
	}
	prefix = if (absolute) splitPath(base)$absolute else base;
	if (absolute || path) files = paste(prefix, files, sep = '/');
	files
}
list_files_with_exts = function(path, exts, full.names = TRUE)
	list.files(path, pattern = Sprintf('.(%{Exts}s)$', Exts = join(exts, '|')), full.names = full.names);

list_files_with_base = function(path, exts, full.names = TRUE) {
	sp = splitPath(path);
	list.files(sp$dir,
		pattern = Sprintf('^%{base}s.(%{Exts}s)$', base = sp$base, Exts = join(exts, '|')),
		full.names = full.names
	);
}

#
#	<p> file manipulation
#

File.exists = function(path, host = '', agent = 'ssh', ssh = TRUE) {
	if (ssh) {
		sp = splitPath(path, skipExists = TRUE, ssh = TRUE);
		host = sp$userhost;
		path = sp$path;
	}
	r = if (!is.null(host) && host != '') {
		ret = system(sprintf('%s %s stat %s >/dev/null 2>&1', agent, host, qs(path)));
		ret == 0
	} else file.exists(path);
	r
}

File.copy_raw = function(from, to, ...,
	overwrite = FALSE, recursive = FALSE, agent = 'scp', logLevel = 6, ignore.shell = TRUE,
	symbolicLinkIfLocal = TRUE) {
	spF = splitPath(from, ssh = TRUE);
	spT = splitPath(to, ssh = TRUE);
	is.remote.f = spF$is.remote || spF$host == 'localhost';
	is.remote.t = spT$is.remote || spT$host == 'localhost';

	r = if (!is.remote.f && !is.remote.t) {
		if (symbolicLinkIfLocal) {
			LogS(4, 'Symlinking "%{from}s --> %{to}s', from = spF$path, to = spT$path);
			file.symlink(spF$path, spT$path, ...);
		} else {
			LogS(4, 'Copy "%{from}s --> %{to}s', from = spF$path, to = spT$path);
			file.copy(spF$path, spT$path, recursive = recursive, ..., overwrite = overwrite);
		}
	} else {
		# <A> assume 'to' to be atomic
		cmd = sprintf('%s %s %s %s %s',
			agent,
			ifelse(recursive, '-r', ''),
			paste(sapply(from, qs), collapse = ' '),
			qs(to),
			ifelse(ignore.shell, '>/dev/null', '')
		);
		System(cmd, logLevel);
	}
	r
}

File.copy = function(from, to, ..., recursive = FALSE, agent = 'scp', logLevel = 6, ignore.shell = TRUE,
	symbolicLinkIfLocal = TRUE) {
	if (is.null(from)) return(NULL);
	pairs = cbind(from, to);
	r = apply(pairs, 1, function(r) {
		File.copy_raw(r[1], r[2], ...,
			recursive = recursive, agent = agent, logLevel = logLevel,
			ignore.shell = ignore.shell, symbolicLinkIfLocal = symbolicLinkIfLocal)
	})
	r
}

File.remove = function(path, ..., agent = 'ssh', ssh = TRUE, logLevel = 6) {
	r = if (ssh) {
		sp = splitPath(path, skipExists = TRUE, ssh = TRUE);
		host = sp$userhost;
		rpath = sp$path;
		if (File.exists(path, ssh = TRUE))
			System(sprintf('rm %s', join(sapply(rpath, qs))), pattern = agent,
				ssh_host = host, logLevel = logLevel);
	} else if (file.exists(path)) file.remove(path, ...);
	r
}

# <i> remote operations
File.symlink = function(from, to, replace = TRUE, agent = 'ssh', ssh = FALSE, logLevel = 6,
	warnings = FALSE) {
	r = if (ssh) {
		sp = splitPath(from, skipExists = TRUE, ssh = TRUE);
		host = sp$userhost;
		rpath = sp$path;
		# <!><i>
		stop('not implmenented');
	} else {
		Log(sprintf('symlink %s -> %s', qs(from), qs(to)), logLevel);
		if (replace && file.exists(to)) file.remove(to);
		if (warnings)
			file.symlink(from, to) else
			suppressWarnings(file.symlink(from, to))
	}
	r
}


# <!> only atomic path
#	treatAsFile: causes Dir.create to split off last path-component
Dir.create = function(path, ..., recursive = FALSE, agent = 'ssh', logLevel = 6,
	ignore.shell = TRUE, allow.exists = TRUE, treatPathAsFile = FALSE) {
	sp = splitPath(path, ssh = TRUE);
	# ignore last path-component
	if (treatPathAsFile) {
		sp$path = sp$dir;
		Log(sprintf('creating path %s', sp$path), 4);
	}
	if (sp$is.remote) {
		System(sprintf('ssh %s mkdir %s %s %s',
			sp$userhost,
			if (recursive) '--parents' else '',
			paste(sapply(sp$path, qs), collapse = ' '),
			if (ignore.shell) '2>/dev/null' else ''
		), logLevel);
	} else {
		if (allow.exists && !file.exists(sp$path)) dir.create(sp$path, ..., recursive = recursive);
	}
}

Save = function(..., file = NULL, symbolsAsVectors = FALSE, mkpath = TRUE, envir = parent.frame(1)) {
	sp = splitPath(file, ssh = TRUE);
	localPath = if (sp$is.remote) tempfile() else file;
	if (mkpath) { Dir.create(file, recursive = TRUE, treatPathAsFile = TRUE); }
	r = if (symbolsAsVectors) {
		do.call('save', c(as.list(c(...)), list(file = localPath)), envir = envir);
	} else save(..., file = localPath, envir = envir);
	if (sp$is.remote) File.copy(localPath, file);
	r
}
Load = function(..., file = NULL, Load_sleep = 0, Load_retries = 3, envir = parent.frame(1), logLevel = 6) {
	sp = splitPath(file, ssh = TRUE);
	localPath = if (sp$is.remote) tempfile() else file;
	r = NULL;
	for (i in 1:Load_retries) {
		if (sp$is.remote) {
			if (!File.exists(file)) {
				Sys.sleep(Load_sleep);
				next;
			}
			File.copy(file, localPath, logLevel = logLevel);
		}
		r = try(load(..., file = localPath, envir = envir));
		if (class(r) == 'try-error' && Load_sleep > 0) Sys.sleep(Load_sleep) else break;
	}
	if (is.null(r)) stop(sprintf('could not Load %s', file));
	if (class(r) == 'try-error') stop(r[1]);
	r
}

#
#	create output file names
# output = list(prefix = "results/pch", extension = "pdf", tag = "20100727");
fileName = function(output, extension = NULL, subtype = NULL) {
	if (is.null(output)) return(NULL);
	if (is.null(output$prefix)) return(NULL);
	subtype = firstDef(subtype, output$subtype, "");
	if (subtype != "") subtype =  sprintf("%s-", subtype);
	r = sprintf("%s-%s%s.%s", output$prefix, subtype, output$tag,
		firstDef(extension, output$extension, ""));
	Log(r, 4);
	r
}
#.globalOutput = list(prefix = 'results/20120126-');
#save(r, file = .fn('simulation', 'RData'))
.globalOutputDefault = .globalOutput = list(prefix = '', tag = NULL, tagFirst = FALSE);
GlobalOutput_env__ = new.env();
# .fn.set(prefix = 'results/predictionTesting-')
# @par prefix character, start path name with this character string
# @par tag character, add dashed string to all files (defaults to appending to filename)
# @par tagFirst boolean, put tag as a prefix to the file name instead
.fn.set = function(...) {
	.globalOutput = merge.lists(.globalOutputDefault, list(...));
	assign('.globalOutput', .globalOutput, envir = GlobalOutput_env__);
}
# create output file name on globalOptions
.fn = function(name, extension = '', options = NULL) {
	o = merge.lists(.globalOutputDefault, .globalOutput,
		get('.globalOutput', envir = GlobalOutput_env__), options);
	# construct plain filename
	pathes = sprintf('%s%s%s%s', o$prefix, name, ifelse(extension == '', '', '.'), extension);
	fn = sapply(pathes, function(path) {
		sp = splitPath(path);
		# <p> dir
		if (!file.exists(sp$dir)) dir.create(sp$dir);
		# <p> tag
		ext = firstDef(sp$ext, '');
		fn = if (!is.null(o$tag)) {
			if (o$tagFirst) {
				sprintf('%s/%s-%s%s%s', sp$dir, o$tag, sp$base, ifelse(ext == '', '', '.'), ext)
			} else { sprintf('%s/%s-%s%s%s', sp$dir, sp$base, o$tag, ifelse(ext == '', '', '.'), ext) };
		} else sprintf('%s/%s%s%s', sp$dir, sp$base, ifelse(ext == '', '', '.'), ext);
		fn
	});
	avu(fn)
}
.fn.pushPrefix = function(prefix) {
	output = merge.lists(.globalOutput, list(prefix = sprintf('%s%s', .globalOutput$prefix, prefix)));
	assign('.globalOutput', output, envir = GlobalOutput_env__);
	.globalOutput
}
.fn.popPrefix = function(prefix) {
	output = merge.lists(.globalOutput, list(prefix = sprintf('%s/', splitPath(.globalOutput$prefix)$dir)));
	assign('.globalOutput', output, envir = GlobalOutput_env__);
	.globalOutput
}

exprInDir = function(expr, dir = '.', envir = parent.frame()) {
	prev = setwd(dir);
	on.exit(setwd(prev));
	return(eval(expr, envir = envir));
}

#
#	create consecutive files
#
# findNextFile = function(path, N = 1e2) {
# 	sp = splitPath(path);
# 	for (i in 0:N) {
# 		path = if (i > 0) with(sp, Sprintf('%{fullbase}s-%{i}d.%{ext}')) else path;
# 		if (!file.exists(path)) return(path);
# 	}
# 	stop(Sprintf('No path could be crated from base path: %{path}s'));
# }

findLastVersion = function(path, retAll = FALSE) {
	sp = splitPath(path);
	E = if (is.null(sp$ext)) '' else with(sp, Sprintf('.%{ext}s'));
	re = with(sp, Sprintf('^%{base}s-(\\d+)%{E}s$'));
	files = list.files(sp$dir, pattern = re);
	i = max(c(0, as.integer(Regexpr(re, files, captures = TRUE))));
	highest = if (i == 0) path else with(sp, Sprintf('%{fullbase}s-%{i}d%{E}s'));
	return(if (retAll) list(path = highest, version = i, ext = E) else highest);
}

findNextFile = function(path, Nmax = 1e2) {
# 	sp = splitPath(path);
# 	re = with(sp, Sprintf('^%{base}s-(\\d+).%{ext}s$'));
# 	files = list.files(sp$dir, pattern = re);
	lv = findLastVersion(path, retAll = TRUE);
	v = lv$version;
	if (v == 0 && !file.exists(path)) return(path);
	if (v >= Nmax)
		stop(Sprintf('No path could be crated from base path: %{path}s [Maximum versions exhausted: %{Nmax}d]'));
	r = with(splitPath(path), Sprintf('%{fullbase}s-%{i}d%{ext}s', i = v + 1, ext = lv$ext));
	LogS(6, 'findNextFile: %{r}s');
	return(r);
}


#
#	command argument handling
#

# default args: command line call minus command
evaluateArgs = function(c = commandArgs()[-1]) {
	is.no.option = is.na(as.integer(sapply(c, function(a)grep("^--", a))));
	#c = c[!(c == "--vanilla")];	# eliminate '--vanilla' arguments
	c = c[is.no.option];
	if (length(c) > 0) {
		eval.parent(parse(text = c[1]));
		argListString = gsub(";", ",", gsub(";$", "", c[1]));
		print(argListString);
		return(eval(parse(text = sprintf("list(%s)", argListString))));
	}
	return(NULL);
}

# default args: command line call minus command
getCommandOptions = function(c = commandArgs()[-1]) {
	is.no.option = is.na(as.integer(sapply(c, function(a)grep("^--", a))));
	#c = c[!(c == "--vanilla")];	# eliminate '--vanilla' arguments
	c = c[is.no.option];
	o = lapply(c, function(e) {
		eval(parse(text = e));
		nlapply(setdiff(ls(), 'e'), function(n)get(n))
	});
	o = unlist.n(o, 1);
	o
}

# R.pl interface

handleTriggers = function(o, triggerDefinition = NULL) {
	if (is.null(triggerDefinition)) triggerDefinition = rget('.globalTriggers');
	if (!is.list(o) || is.null(triggerDefinition)) return(NULL);
	for (n in names(triggerDefinition)) {
		if (!is.null(o[[n]])) triggerDefinition[[n]](o$args, o);
	}

}

#
#	<p> extended system call
#

# Example of patterns:
# 	System(cmd, 5, patterns = c('cwd', 'qsub', 'ssh'),
# 		cwd = sp$path, ssh_host = sp$userhost,
# 		qsubPath = sprintf('%s/qsub', sp$path), qsubMemory = self@config$qsubRampUpMemory);

.systemSeps = list(Linux = ';', Windows = '&');
JoinCmds = function(cmds, system = Sys.info()['sysname']) {
	sep = Sprintf(' %{sep}s ', sep = .systemSeps[[ system ]]);
	return(join(cmds, sep));
}

.System.fileSystem = list(
	#tempfile = function(prefix, ...)tempfile(splitPath(prefix)$base, tmpdir = splitPath(prefix)$dir, ...),
	tempfile = function(prefix, ...)tempFileName(prefix, ...),
	readFile = function(...)readFile(...)
);
.System.patterns = list(
	default = list(pre = function(cmd, ...)cmd, post = function(spec, ret, ...)list()	),
	qsub = list(pre = function(cmd, spec,
		jidFile = spec$fs$tempfile(sprintf('/tmp/R_%s/qsub_pattern', Sys.getenv('USER'))),
		qsubOptions = '',
		waitForJids = NULL, ...) {
		Dir.create(jidFile, treatPathAsFile = TRUE);
		waitOption = if (is.null(waitForJids)) '' else
			sprintf('--waitForJids %s', join(waitForJids, sep = ','));
		message(cmd);
		ncmd = sprintf('qsub.pl --type ogs --jidReplace %s %s --unquote %s -- %s',
			jidFile, waitOption, qsubOptions, qs(cmd));
		message(ncmd);
		spec = list(cmd = ncmd, jidFile = jidFile);
		spec
	},
	post = function(spec, ret, ...) { list(jid = as.integer(spec$fs$readFile(spec$jidFile))) }
	),

	qsub_slurm = list(pre = function(cmd, spec,
		jidFile = spec$fs$tempfile(sprintf('/tmp/R_%s/qsub_pattern', Sys.getenv('USER'))),
		qsubOptions = '',
		waitForJids = NULL, ...) {
		Dir.create(jidFile, treatPathAsFile = TRUE);
		waitOption = if (is.null(waitForJids)) '' else
			sprintf('--waitForJids %s', join(waitForJids, sep = ','));
		message(cmd);
		ncmd = sprintf('qsub.pl --type slurm --jidReplace %s %s --unquote %s -- %s',
			jidFile, waitOption, qsubOptions, qs(cmd));
		message(ncmd);
		spec = list(cmd = ncmd, jidFile = jidFile);
		spec
	},
	post = function(spec, ret, ...) { list(jid = as.integer(spec$fs$readFile(spec$jidFile))) }
	),

	cwd = list(pre = function(cmd, spec, cwd = '.', ...) {
		ncmd = JoinCmds(c(Sprintf('cd %{cwd}q'), cmd));
		spec = list(cmd = ncmd);
		spec
	},
	post = function(spec, ret, ...) { list() }
	),
	# <i> stdout/stderr handling
	ssh = list(pre = function(cmd, spec, ssh_host = 'localhost', ssh_source_file = NULL, ...,
		ssh_single_quote = TRUE) {
		if (!is.null(ssh_source_file)) {
			#cmd = sprintf('%s ; %s',
			#	join(paste('source', qs(ssh_source_file), sep = ' '), ' ; '), cmd);
			cmd = JoinCmds(c(paste('source', qs(ssh_source_file), sep = ' '), cmd))
		}
		fmt = if (ssh_single_quote) 'ssh %{ssh_host}s %{cmd}q' else 'ssh %{ssh_host}s %{cmd}Q';
		spec = list(cmd = Sprintf(fmt));
		spec
	},
	fs = function(fs, ..., ssh_host) {
		list(
			tempfile = function(prefix, ...) {
				Log(sprintf('tempfile ssh:%s', prefix), 1);
				r = splitPath(tempFileName(sprintf('%s:%s', ssh_host, prefix), ...), ssh = TRUE)$path;
				Log(sprintf('tempfile ssh-remote:%s', r), 1);
				r
			},
			readFile = function(path, ...)readFile(sprintf('%s:%s', ssh_host, path), ..., ssh = TRUE)
		);
	},
	post = function(spec, ret, ...) { list() }
	)
);
#
#	a system call (c.f. privatePerl/TempFilenames::System)
#
System_env__ <- new.env();
assign(".system.doLogOnly", FALSE, envir = System_env__);

System = function(cmd, logLevel = get('DefaultLogLevel', envir = Log_env__),
	doLog = TRUE, printOnly = NULL, return.output = FALSE,
	pattern = NULL, patterns = NULL, ..., return.cmd = FALSE, return.error = FALSE, wd = NULL) {
	# prepare
	if (!exists(".system.doLogOnly", envir = System_env__))
		assign(".system.doLogOnly", FALSE, envir = System_env__);
	doLogOnly = ifelse (!is.null(printOnly), printOnly, get('.system.doLogOnly', envir = System_env__));

	# pattern mapping
	fs = .System.fileSystem;
	if (!is.null(patterns)) {
		spec = list();
		# map file accesses
		for (pattern in rev(patterns)) {
			fsMapper = .System.patterns[[pattern]]$fs;
			if (!is.null(fsMapper)) fs = fsMapper(fs, ...);
			spec[[length(spec) + 1]] = list(fs = fs);
		}
		# wrap commands into each other
		for (i in 1:length(patterns)) {
			spec[[i]] = merge.lists(spec[[i]], .System.patterns[[patterns[[i]]]]$pre(cmd, spec[[i]], ...));
			cmd = spec[[i]]$cmd;
		}
	} else if (!is.null(pattern)) {
		spec = .System.patterns[[pattern]]$pre(cmd, list(fs = fs), ...);
		spec$fs = fs;	# manually install fs
		cmd = spec$cmd;
	}
	# redirection (after patterns) <A>
	if (return.output & !doLogOnly) {
		tmpOutput = tempfile();
		cmd = sprintf("%s > %s", cmd, tmpOutput);
	}
	if (return.error & !doLogOnly) {
		tmpError = tempfile();
		cmd = sprintf("%s 2> %s", cmd, tmpError);
	}
	# logging
	if (doLog){ Log(sprintf("system: %s", cmd), logLevel); }
	# system call
	ret = NULL;
	if (!doLogOnly) ret = if (notE(wd)) exprInDir(system(cmd), wd) else system(cmd);
	# return value
	r = list(error = ret);
	if (return.output & !doLogOnly) {
		r = merge.lists(r, list(output = readFile(tmpOutput)));
	}
	if (return.error & !doLogOnly) {
		r = merge.lists(r, list(output.err = readFile(tmpError)));
	}
	# postprocess
	if (!doLogOnly) if (!is.null(patterns)) {
		for (i in rev(1:length(patterns))) {
			r = merge.lists(r, .System.patterns[[patterns[[i]]]]$post(spec[[i]], ret, ...));
		}
	} else if (!is.null(pattern)) {
		r = merge.lists(r, .System.patterns[[pattern]]$post(spec, ret, ...));
	}
	if (return.cmd) r$command = cmd;
	# simplified output
	if (!return.output && !return.cmd && !return.error && is.null(pattern)) r = r$error;
	r
}
SystemS = function(cmd, logLevel = get('DefaultLogLevel', envir = Log_env__),
	doLog = TRUE, printOnly = NULL, return.output = FALSE, return.cmd = FALSE, ..., envir = parent.frame(), wd = NULL) {

	cmd = Sprintf(cmd, ..., envir = envir);
	System(cmd, logLevel, doLog, printOnly, return.output, return.cmd = return.cmd, wd = wd);
}

qsub_wait_function = function(r, ...) {
	ids = if (is.list(r[[1]]) & !is.null(r[[1]]$jid)) list.kp(r, 'jid', do.unlist = TRUE) else r$jid;
	idsS = if (length(ids) == 0) '' else paste(ids, collapse = ' ');
	System(sprintf('qwait.pl %s', idsS), ...);
}
	
# wait on job submitted by system
.System.wait.patterns = list(
	default = function(r, ...)(NULL),
	qsub = qsub_wait_function,
	qsub_slurm = qsub_wait_function
);
System.wait = function(rsystem, pattern = NULL, ...) {
	r = if (!is.null(pattern)) .System.wait.patterns[[pattern]](rsystem, ...) else NULL;
	r
}

System.SetDoLogOnly = function(doLogOnly = FALSE) {
	assign(".system.doLogOnly", doLogOnly, envir = System_env__);
}

#
#	<p> io
#

# Capture.ouput(..., type = c('input', 'output', 'merged', 'all', 'none', 'discard'), split = c('input', 'output', 'all', 'none'), append = c('input', 'output', 'all', 'none'), return = TRUE)
silence = function(expr, verbose = FALSE) {
	if (verbose || Sys.info()['sysname'] == 'Windows') eval(expr) else {
		sink('/dev/null', type = 'output');
		sink(stdout(), type = 'message');
		on.exit({ sink(type = 'message'); sink(type = 'output'); });
		r = eval(expr);
		r
	}
}

#
#	<p> calls
#

evalCall = function(call) {
	call = callEvalArgs(call);
	do.call(call$f, call$args, envir = call$envir)
}

# envirArgs: non-functional, depracated
Do.call = function(what, args, quote = FALSE, envir = parent.frame(),
	defaultEnvir = .GlobalEnv, envirArgs = NULL, do_evaluate_args = FALSE) {
	if (is.null(envir)) envir = defaultEnvir;
	if (do_evaluate_args) args = nlapply(args, function(e)eval(args[[e]], envir = envir));
	do.call(what = what, args = args, quote = quote, envir = envir)
}

#
#	<p> file operations
#

# <A> overlap with Source; avoid dependecy with RCurl
SourceLocal = function(file, ...,
	locations = c('', '.', sprintf('%s/src/Rscripts', Sys.getenv('HOME'))),
	envir = NULL) {
	sapply(file, function(file) {
		file0 = file.locate(file, prefixes = locations);
			if (notE(envir)) sys.source(file = file0, envir = envir, ...) else source(file = file0, ...)
	})
}

# on the fly activation of package w/o installation
#	SourcePackage('~/src/Rprivate/Packages/plausibility/plausibility.R');
SourcePackage = function(defFile, ...,
	locations = c('', '.', sprintf('%s/src/Rscripts', Sys.getenv('HOME'))),
	envir = NULL) {

	dir = splitPath(defFile)$dir;
	tmpEnv = new.env();
	SourceLocal(defFile, locations = locations, envir = tmpEnv);
	files = c(defFile, get('packageDefinition', tmpEnv)$files);

	SourceLocal(files, locations = c(dir, locations), envir = envir);
}


#' Return absolute path for name searched in search-pathes
#'
#' Search for pathes.
#'
#' @param path path (segment) to be located in standard locations
#' @param prefixes prefixes to be prepended to path to check existance
#' @param normalize boolean to inidcate whether a normalized path should be returned (absolute)
#' @param home boolean to indicate whether starting prefix '~' should be interpolated to the home folder
#' @param as.dirs assume that prefixes are pathes, i.e. a slash will be put between path and prefix
#' @param force enforces that path and prefix are always joined, otherwise if path is absolute no prefixing is performed
#' @return character vector with path to file or NULL if no file could be located
file.locate = function(path, prefixes = NULL, normalize = TRUE, as.dirs = TRUE, force = FALSE, home = TRUE) {
	if (!force && substr(path, 1, 1) == '/') return(path);
	if (substr(path, 1, 1) == '~' && home) {
		path = path.absolute(path, home.dir = TRUE);
		if (!force) return(path);
	}
	if (is.null(prefixes)) prefixes = if (as.dirs) '.' else '';
	sep = ifelse(as.dirs, '/', '');
	for (prefix in prefixes) {
		npath = sprintf('%s%s%s', prefix, sep, path);
		if (normalize) npath = path.absolute(npath);
		if (file.exists(npath)) return(npath);
	}
	NULL
}

#' Read content of file and return as character object.
#' 
#' @param path Path to the file to be read.
#' @param prefixes Search for file by prepending character strings from
#' prefixes.
#' @param normalize Standardize pathes.
#' @param ssh Allow pathes to remote files in \code{scp} notation.
#' @author Stefan Böhringer <r-packages@@s-boehringer.org>
#' @return character vector containing the file content
#' @keywords io input
# #' @examples
# #' \dontrun{
# #'   parallel8 = function(e) log(1:e) %*% log(1:e);
# #'   cat(readFile(tempcodefile(parallel8)));
# #' }
# prefixes only supported locally <!>
readFile = function(path, prefixes = NULL, normalize = TRUE, ssh = FALSE) {
	s = splitPath(path, ssh = ssh);
	r = if (s$is.remote) {
		tf = tempfile();
		File.copy(path, tf);
		readChar(tf, nchars = as.list(file.info(tf)[1,])$size);
	} else {
		if (!is.null(prefixes)) path = file.locate(path, prefixes, normalize);
		readChar(path, nchars = as.list(file.info(path)[1,])$size);
	}
	r
}

writeFile = function(path, str, mkpath = FALSE, ssh = FALSE) {
	s = splitPath(path, ssh = ssh);
	if (s$is.remote) {
		Dir.create(sprintf('%s:%s', s$userhost, s$dir), recursive = mkpath);
		tf = tempfile();
		out = file(description = tf, open = 'w', encoding='UTF-8');
			cat(str, file = out, sep = "");
		close(con = out);
		File.copy(tf, path);
	} else {
		if (mkpath) {
			if (!file.exists(s$dir)) dir.create(s$dir, recursive = TRUE);
		}
		out = file(description = path, open = 'w', encoding='UTF-8');
			cat(str, file = out, sep = "");
		close(con = out);
	}
	path
}

isURL = function(path)(length(grep("^(ftp|http|https|file)://", path)) > 0L)

#
#	<p> helper functions readTable/writeTable
#

compressPathBz2 = function(pathRaw, path, doRemoveOrig = TRUE) {
	cmd = Sprintf("cat %{pathRaw}q | bzip2 -9 > %{path}q");
	r = System(cmd, 5);
	if (doRemoveOrig && !get('.system.doLogOnly', envir = System_env__)) file.remove(pathRaw);
	r
}
compressPath = function(pathRaw, path, extension = NULL, doRemoveOrig = TRUE) {
	if (is.null(extension)) return(path);
	compressor = get(Sprintf('compressPath%{extension}u'));
	r = compressor(pathRaw, path, doRemoveOrig = doRemoveOrig);
	r
}
decompressPathBz2 = function(path, pathTmp, doRemoveOrig = FALSE) {
	cmd = Sprintf("cat %{path}q | bunzip2 > %{pathTmp}q");
	r = System(cmd, 5);
	if (doRemoveOrig && !get('.system.doLogOnly', envir = System_env__)) file.remove(path);
	r
}
decompressPath = function(path, pathTmp, extension = NULL, doRemoveOrig = FALSE) {
	if (is.null(extension)) return(path);
	decompressor = get(Sprintf('decompressPath%{extension}u'));
	r0 = decompressor(path, pathTmp, doRemoveOrig = doRemoveOrig);
	r = list(destination = pathTmp, pathOrig = path, return = r0);
	r
}

compressedConnectionBz2 = function(path, mode = '') {
	#r = Sprintf('%{path}s.bz2');
	bzfile(path, open = mode)
}
compressedConnectionGz = function(path, mode = '') {
	gzfile(path, open = mode)
}
compressedConnection = function(path, extension = NULL, mode = '') {
	if (is.null(extension)) return(path);
	compressor = get(Sprintf('compressedConnection%{extension}u'));
	compressor(path, mode = mode)
}
compressedConnectionPath = function(conn) {
	if ('connection' %in% class(conn)) summary(conn)$description else conn
}

#
#	<p> readTable
#


#
#	<p> print
#

stdOutFromCall = function(call_) {
	tf = tempfile();
	sink(tf);
		eval.parent(call_, n = 2);
	sink();
	readFile(tf)
}

#
#	crypotgraphy/checksumming
#

# md5sumString = function(s, prefix = 'md5generator') {
# 	Require('tools');
# 	path = tempfile('md5generator');
# 	writeFile(path, s);
# 	md5 = avu(md5sum(path));
# 
# 	md5
# }
# same as above, less dpendencies
md5sumString = function(s, length = 32, ..., logLevel = 5)
	substr(
		SystemS('echo -n %{s}q | md5sum', return.output = TRUE, logLevel = logLevel)$output
	, 1, min(length, 32))
sha256sumString = function(s, length = 32, ..., logLevel = 5)
	substr(
		SystemS('echo -n %{s}q | sha256sum', return.output = TRUE, logLevel = logLevel)$output
	, 1, min(length, 64))
sha256sumPath = function(path, length = 64, ..., logLevel = 5)
	substr(
		SystemS('sha256sum %{path}q', return.output = TRUE, logLevel = logLevel)$output
	, 1, min(length, 64))

hashPathContent = function(path, type = 'sha256', length = 64,..., logLevel = 5) {
	f = get(paste0(type, 'sumPath'));
	f(path, length, ..., logLevel = logLevel)
}
hashStringContent = function(path, type = 'sha256', length = 64,..., logLevel = 5) {
	f = get(paste0(type, 'sumString'));
	f(path, length, ..., logLevel = logLevel)
}

#
#	<p> package documentation
#

#
#	<p> Rcpp helpers
#

#
#	<p> sqlite
#

#
#	<p> publishing
#

#
#	<p> quick pdf generation
#

#
#	<p> workarounds
#

clearWarnings = function()assign('last.warning', NULL, envir = baseenv())

# fix broken install from dir: create tarball -> install_local
Install_local = function(path, ..., tarDir = tempdir()) {
	sp = splitPath(path);
	pkgPath = Sprintf('%{tarDir}s/%{base}s.tar.gz', sp);
	# dir component is containing folder
	#System(Sprintf('cd %{dir}Q ; tar czf %{pkgPath}Q %{file}Q', sp), 2);
	LogS(2, 'Creating archive: %{pkgPath}s from: %{file}s in dir %{dir}s', sp);
	exprInDir(tar(pkgPath, sp$file, compression = 'gzip'), sp$dir);
	#lib = list(...)$lib;
	#libLocation = if (is.null(lib)) 'default location' else lib;
	#LogS(4, 'Installing to lib:%{libLocation}s');
	#print(Sprintf('Installing to lib:%{libLocation}s'));
	install_local(pkgPath, ...);
}

#
#	<p> packages
#

#
#	<p> misc linux system stuff
#

#
#	<p> random numbers
#

getRandomSeed = function(tag = date()) {
	md5 = md5sumString(join(c(getwd(), tag)));
	is = hex2ints(md5);
	seed = is[1];
	for (i in 2:length(is)) { seed = bitwXor(seed, is[i]); }
	seed
}

#
#	<p> Reporting
#

#
#	<p> stop
#

stopS = function(str, ...)stop(Sprintf(str, ...));

#
#	<p> debugging
#

# r__: return printed values as list
dprint = function(..., r__ = TRUE) {
	vs = as.character(as.list(substitute(list(...)))[-1]);
	ns = names(list(...));
	Ns = if (is.null(ns)) vs else ifelse(ns == '', vs, ns);
	l = listKeyValue(Ns, c(...));
	print(list2df(l));
	if (r__) return(l);
}

debugOn = function()options(error = recover);

#
#	<p> file system
#

normalizePath = function(p) {
	p = gsub('^~', Sys.getenv('HOME'), p);
	p = gsub('(?:g)//', '/', p, perl = TRUE);
	return(p);
}

# recursive version of gsbu
gsubR = function(pattern, replacement, x, ..., Nmax = 1e3) {
	for (i in 1:Nmax) {
		xNew = gsub(pattern, replacement, x, ...);
		if (all(xNew == x)) return(x);
		x = xNew;
	}
	return(NA);	# trigger special case (consider stop) <N>
}

# <i><!> unify with normalizePath after testing 8/2020
NormalizePath = function(p) {
	p = gsub('^~', Sys.getenv('HOME'), p);
	p = gsub('//+', '/', p, perl = TRUE);
	p = gsubR('(^|/)[^/]+/[.][.]/', '/', p, perl = TRUE);
	return(p);
}
pathToHome = function(path)
	gsub(Sprintf('^%{home}s((?=/)|$)', home = Sys.getenv('HOME')), '~', path, perl = TRUE)

# how to refer to to from within from
relativePathSingle = function(from, to) {
	from = normalizePath(from);
	to = normalizePath(to);
	spF = splitPath(from);
	spT = splitPath(to);
	if (spT$isAbsolute) return(to);
	join(c(rep('..', length(splitString('/', spF$dir)) + 0), to), '/');
}
relativePath = Vectorize(relativePathSingle, c('from', 'to'));
SplitPath = function(path, ...)lapply(path, splitPath, ...);
absolutePathSingle = function(path)splitPath(path)$absolute
absolutePath = Vectorize(absolutePathSingle, c('path'));
pathSimplify = function(p)gsub('[:]', '_', p)
pathInsertPostfix = function(path, postfix, sep = '-')
	Sprintf('%{fullbase}s%{sep}s%{postfix}s.%{ext}s', splitPath(path))

# keys of input-list are folder names, use folderstring in key to create subfolders
# 	createZip(list(results = c('r/ref1.html', 'r/ref2.html')), 'r/myZip.zip', doCopy = TRUE);
# 	createZip(list(`results::sub` = c('r/ref1.html', 'r/ref2.html')), 'r/myZip.zip', doCopy = TRUE);
#	values are slash-dependend dest = 'source' copies into dest/source; dest = source/, copies into dest

createZip = function(input, output, pword, doCopy = FALSE, readmeText, readme, logOnly = FALSE,
	absoluteSymlink = FALSE, simplifyFileNames = FALSE, folderString = '::') {
	destDir = splitPath(output)$fullbase;
	Dir.create(destDir);
	if (!missing(readmeText)) writeFile(Sprintf('%{destDir}s/README'), readmeText);
	nelapply(input, function(n, e) {
		if (notE(folderString)) n = gsub(folderString, '/', n);
		subdir = join(c(destDir, n, ''), '/');
		Dir.create(subdir);
		toFiles = list.kpu(SplitPath(e), 'file');
		if (simplifyFileNames) toFiles = sapply(toFiles, pathSimplify);
		to = paste(subdir, toFiles, sep = '/');
		LogS(4, 'Copy: %{e}s --> %{to}s');
		if (doCopy) file.copy(e, to, recursive = TRUE) else {
			#from = relativePath(subdir, e);
			from = absolutePath(e);
			if (absoluteSymlink) from = NormalizePath(paste(splitPath(subdir)$absolute, from, sep = '/'));
			print(list(from = from, to = to));
			file.symlink(from, to);
		}
	});
	dir = splitPath(output)$dir;
	zip = splitPath(output)$base;
	options = '';
	if (!missing(pword)) options = Sprintf('%{options}s -P %{pword}q');
	SystemS('cd %{dir}q ; zip %{options}s -r %{zip}q.zip %{zip}q', logLevel = 1, printOnly = logOnly);
}
#
#	RsystemExt.R
#Wed Jan  8 10:54:20 CET 2020
# split off of Rsystem to allow simpler pullIns into other packages

#
#	<par> file handling
#

write.csvs = function(t, path, semAppend = "-sem", ...) {
	s = splitPath(path);
	write.csv(t, path);
	pathSem = sprintf("%s%s.%s", s$fullbase, semAppend, s$ext);
	# make sure t is a data.frame or dec option will not take effect <A>
	#write.csv2(t, pathSem);
	write.table(t, file = pathSem, row.names = F, col.names = T, dec = ",", sep = ";");
}

#
#	<p> file manipulation
#

#
#	command argument handling
#

#
#	<p> extended system call
#


ipAddress = function(interface = "eth0") {
	o = System(sprintf("/sbin/ifconfig %s", interface), logLevel = 6, return.output = T);
	ip = fetchRegexpr("(?<=inet addr:)[^ ]+", o$output);
	ip
}

#
#	<p> io
#

#
#	<p> cluster abstraction
#
# Example:
#specifyCluster(localNodes = 8, sourceFiles = c('RgenericAll.R', 'dataPreparation.R'));
#.clRunLocal = F;
#data.frame.types(clapply(l, f, arg1 = 1), rbind = T, do.transpose = T);

# default cluster configuration
.defaultClusterConfig = list(
	hosts = list(list(host = "localhost", count = 2, type = "PSOCK")), local = F,
	provideChunkArgument = F, reverseEvaluationOrder = T, splitN = 4, reuseCluster = F,
	nestingLevel = 0,	# records the nesting of clapply calls
	splittingLevel = 1,	# specifies at which level clapply should parallelize
	evalEnvironment = F	# call environment_eval on function before passing on
);
Snow_cluster_env__ = new.env();
specifyCluster = function(localNodes = 8, sourceFiles = NULL, cfgDict = list(), hosts = NULL,
	.doSourceLocally = F, .doCopy = T, splitN = NULL, reuseCluster = F, libraries = NULL,
	evalEnvironment = F, varsEnv = NULL, doSinkOutput = NULL) {
	#<!> might not be available/outdated
	Require('parallel');
	cfg = merge.lists(.defaultClusterConfig,
		cfgDict,
		list(splitN = splitN, reuseCluster = reuseCluster, evalEnvironment = evalEnvironment),
		list(local = F, source = sourceFiles, libraries = libraries, varsEnv = varsEnv,
			doSinkOutput = doSinkOutput,
			hosts = (if(is.null(hosts))
			list(list(host = "localhost", count = localNodes, type = "PSOCK",
				environment = list(setwd = getwd()))) else
				hosts)
	));
	assign(".globalClusterSpecification", cfg, envir = Snow_cluster_env__);
	.globalClusterSpecification = get('.globalClusterSpecification', envir = Snow_cluster_env__);
	if (.doCopy) {
		for (h in .globalClusterSpecification$hosts) {
			if (h$host != "localhost" & !is.null(h$env$setwd)) {
				System(sprintf("ssh %s mkdir '%s' 2>/dev/null", h$host, h$env$setwd), 5);
				System(sprintf("scp '%s' %s:'%s' >/dev/null", paste(sourceFiles, collapse = "' '"),
					h$host, h$env$setwd), 5);
			}
		}
	}
	if (.doSourceLocally) {
		sourceFiles = setdiff(sourceFiles, "RgenericAll.R");	# assume we have been sourced
		eval(parse(text =
			paste(sapply(sourceFiles, function(s)sprintf("source('%s', chdir = TRUE);", s)), collapse = "")));
	}
}

# l: list, f: function, c: config
# <i><!> test clCfg$reverseEvaluationOrder before uncommenting
clapply_cluster = function(l, .f, ..., clCfg = NULL) {
	#if (clCfg$reverseEvaluationOrder) l = rev(l);

	# only support SOCK type right now <!><i>
	hosts = as.vector(unlist(sapply(clCfg$hosts, function(h){
		if (h$type == "PSOCK") rep(h$host, h$count) else NULL})));
	master = ifelse(all(hosts == "localhost"), "localhost", ipAddress("eth0"));
	establishEnvironment = T;
	cl = if (clCfg$reuseCluster) {
		if (!exists(".globalClusterObject")) {
			assign(".globalClusterObject", makeCluster(hosts, type = "PSOCK", master = master),
				envir = Snow_cluster_env__);
		} else establishEnvironment = FALSE;
		get('.globalClusterObject', envir = Snow_cluster_env__)
	} else makeCluster(hosts, type = "PSOCK", master = master);
	#clusterSetupRNG(cl);	# snow
	clusterSetRNGStream(cl, iseed = NULL);	# parallel

	clusterExport(cl, clCfg[['vars']]);

	# <p> establish node environment
	envs = listKeyValue(list.key(clCfg$hosts, "host"), list.key(clCfg$hosts, "environment", unlist = F));
	if (Log.level() >= 7) print(clCfg);

	if (establishEnvironment) {
		r = clusterApply(cl, seq_along(hosts), function(i, environments, cfg){
			host = hosts[i];
			env = environments[[host]];
			if (!is.null(env$setwd)) setwd(env$setwd);
			if (!is.null(cfg$source)) for (s in cfg$source) source(s, chdir = TRUE);
			if (!is.null(cfg$libraries))
				#for (package in cfg$libraries) library(package, character.only = TRUE);
				# <N> shiny hack
				for (package in cfg$libraries) do.call('library', list(package, character.only = TRUE));
			# <!> as of 3.4.2013: stop support of exporting global variables to enable CRAN submission
			#if (!is.null(env$globalVars))
			#	for (n in names(env$globalVars)) assign(n, env$globalVars[[n]], pos = .GlobalEnv);
			#sprintf("%s - %s - %s", host, hapmap, getwd());
			if (Nif(cfg$doSinkOutput)) sink(sprintf('%s-%d.out', cfg$doSinkOutput, i));
			NULL
		}, environments = envs, cfg = clCfg);
		nlapply(clCfg[['varsEnv']], function(envName) {
			envir = get(envName);
			clusterExport(cl, clCfg$varsEnv[[envName]], envir = envir);
		});
	}

	# <p> iterate
	N = clCfg$splitN * length(hosts);	# No of splits
	idcs = splitListIndcs(length(l), N);
	exportNames = c();
	iterator__ = if (clCfg$provideChunkArgument) {
		function(.i, ...) {
			r = lapply(idcs[.i, 1]:idcs[.i, 2], function(j)try(.f(l[[j]], .i, ...)));
			if (class(r) == "try-error") r = NULL;
			r
		}
	} else {
		function(.i, ...){
			r = lapply(idcs[.i, 1]:idcs[.i, 2], function(j)try(.f(l[[j]], ...)));
			if (class(r) == "try-error") r = NULL;
			r
		}
	}
	if (clCfg$evalEnvironment) {
		iterator__ = environment_eval(iterator__, functions = T);
		#clusterExport(cl, varlist = names(as.list(environment(iterator__))), envir = environment(iterator__));
	}
	r = clusterApplyLB(cl, 1:dim(idcs)[1], iterator__, ...);
	# <p> finish up
	if (!clCfg$reuseCluster) stopCluster(cl)
	r = unlist(r, recursive = F);
	#if (clCfg$reverseEvaluationOrder) r = rev(r);
	r
}

# wrapper (as of 3.12.8: I seem to have lost a previous change)
clapply = function(l, .f, ..., clCfg = NULL, .clRunLocal = rget(".clRunLocal", F, envir = .GlobalEnv)) {
	# <p> get cluster specification
	clCfg = merge.lists(
		rget(".globalClusterSpecification", default = list(), envir = Snow_cluster_env__),
		firstDef(clCfg, list())
	);
	# <p> update cluster specification
	clCfg$nestingLevel = clCfg$nestingLevel + 1;
	assign(".globalClusterSpecification", clCfg, envir = Snow_cluster_env__);

	# <p> choose/decline parallelization
	r = if (firstDef(.clRunLocal, clCfg$local, F) || clCfg$nestingLevel != clCfg$splittingLevel) {
		if (clCfg$provideChunkArgument) lapply(X = l, FUN = .f, 1, ...)
		else lapply(X = l, FUN = .f, ...)
	} else {
		clapply_cluster(l, .f, ..., clCfg = clCfg);
	};

	# <p> update cluster specification
	clCfg$nestingLevel = clCfg$nestingLevel - 1;
	assign(".globalClusterSpecification", clCfg, envir = Snow_cluster_env__);
	r
}

#
#	<p> file operations
#

Source_url = function(url, ...) {
	requireNamespace('RCurl');
	request = getURL(url, followlocation = TRUE,
		cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"));
	tf = tempfile();
	writeFile(tf, request);
    source(tf, ...)
}

GlobalCollections_env__ = new.env();
SourceAppendPath = function(path)
	assign('sourceFiles',
		unique(unique(c(unlist(mget('sourceFiles', GlobalCollections_env__, ifnotfound = list(NULL))), path))),
			envir = GlobalCollections_env__);
SourceAppendReset = function()assign('sourceFiles', c(), envir = GlobalCollections_env__);
SourceCollected = function()
	mget('sourceFiles', GlobalCollections_env__, ifnotfound = list(NULL))$sourceFiles;
LibraryAppend = function(lib)
	assign('libraries',
		unique(c(unlist(mget('libraries', GlobalCollections_env__, ifnotfound = list(NULL))), lib)),
			envir = GlobalCollections_env__);
LibraryAppendReset = function()assign('libraries', c(), envir = GlobalCollections_env__);
LibraryCollected = function()
	mget('libraries', GlobalCollections_env__, ifnotfound = list(NULL))$libraries;

SourceSingle = function(file, ..., locations, envir, sourceCollect) {
	if (sourceCollect && file == '__RESET__')return(SourceAppendReset());
	file0 = file;
	if (isURL(file)) Source_url(file, ...) else {
		file0 = file.locate(file, prefixes = locations);
		if (notE(envir)) sys.source(file = file0, envir = envir, ...) else source(file = file0, ...);
	}
	if (sourceCollect) SourceAppendPath(file0);
}
	
# <!> local = T does not work
Source = function(file, ...,
	locations = c('', '.', sprintf('%s/src/Rscripts', Sys.getenv('HOME'))),
	envir = NULL, sourceCollect = TRUE) {
	sapply(file, SourceSingle, ..., locations = locations, envir = envir, sourceCollect = sourceCollect)
}

#
#	<p> helper functions readTable/writeTable
#
# -> Rsystem.R

#
#	<p> readTable
#

readTableSplitToDict = function(e){
	r = lapply(splitString(';', e), function(e) {
		r = splitString(':', e);
		listNamed(list(shift(r)), r[1])
	});
	unlist.n(r, 1)
}

# complete: return only complete data with respect to specified colums
# NA: specify 'NA'-values
readTableSepMap = list(T = "\t", S = ' ', C = ',', `;` = ';', `S+` = '');
optionParser = list(
	SEP = function(e)readTableSepMap[[e]],
	DEC = identity,
	QUOTE = function(e)(if (e == 'F') '' else e),
	HEADER = function(e)list(T = T, F = F)[[e]],
	ROW.NAMES = function(e)list(T = T, F = F)[[e]],
	NAMES = function(e)splitString(';', e),
	FACTORS = function(e)splitString(';', e),
	NUMERIC = function(e)splitString(';', e),
	AS_CHARACTER = function(e)splitString(';', e),
	DATE = function(e)splitString(';', e),
	# key=value pairs: column name:date format
	DATEF = readTableSplitToDict,
	PROJECT = function(e)splitString(';', e),
	`NA` = function(e)splitString(';', e),
	complete = function(e)splitString(';', e),
	CONST = function(e){ r = lapply(splitString(';', e), function(e){
			r = splitString(':', e);
			v = if (length(fetchRegexpr('^\\d+$', r[2])) > 0) r[2] = as.integer(r[2]) else r[2];
			listKeyValue(r[1], v)
		});
		unlist.n(r, 1)
	},
	HEADERMAP = readTableSplitToDict,
	# tb implemented: <i>: merge.lists recursive
	VALUEMAP = readTableSplitToDict,
	COLNAMESFILE = identity,
	SHEET = as.integer,
	SKIP = as.integer,
	DELETE = Eval,
	ENCODING = identity
);

splitExtendedPath = function(path) {
	q = fetchRegexpr('(?<=^\\[).*?(?=\\]:)', path);
	options = list();
	if (length(q) > 0 && nchar(q) > 0) {
		path = substr(path, nchar(q) + 4, nchar(path));
		os = sapply(splitString(',', q), function(e)splitString('=', e));
		os = listKeyValue(os[1, ], os[2, ]);
		os = nlapply(names(os), function(n)optionParser[[n]](os[[n]]));
		options = merge.lists(options, os);
	}
	r = list(path = path, options = options)
}
path2simple = function(pathes)sapply(pathes, function(path)splitExtendedPath(path)$path);

readTable.ods = function(path, options = NULL, verbose = F) {
	Require('readODS');
	sheet = firstDef(options$SHEET, 1);
	silence(read_ods(path, sheet = sheet, col_names = options$HEADER),
		verbose = firstDef(options$VERBOSE, verbose)
	);
}

notEC = function(e)(length(e) > 0 && (length(e) > 1 || e[1] != ''))

# <!> changed SEP default "\t" -> ",", 20.5.2015
#readTable.csv.defaults = list(HEADER = T, SEP = "\t", `NA` = c('NA'), QUOTE = '"');
readTable.csv.defaults = list(HEADER = T, SEP = ",", `NA` = c('NA'), QUOTE = '"', SKIP = 0);
readTable.txt = readTable.csv = function(
	path, options = readTable.csv.defaults, headerMap = NULL, setHeader = NULL, ...) {

	options = merge.lists(readTable.csv.defaults, options);
	t = read.table(path, header = options$HEADER, sep = options$SEP, as.is = T,
		na.strings = options$`NA`, comment.char = '', quote = options$QUOTE, skip = options$SKIP, ...);
	if (!is.null(options$NAMES)) names(t)[1:length(options$NAMES)] = options$NAMES;
	if (!is.null(headerMap)) names(t) = vector.replace(names(t), headerMap);
	if (!is.null(setHeader))
		names(t) =  c(setHeader, names(t)[(length(setHeader)+1): length(names(t))]);
	if (notEC(options$FACTORS)) t = Df_(t, as_factor = options$FACTORS);
	if (notEC(options$AS_CHARACTER)) t = Df_(t, as_character = options$AS_CHARACTER);
	t
}

spssDate = function(date, tz = 'MET', spss.origin = as.POSIXct('2003/02/11', tz = tz) - 13264300800)
	(spss.origin + date)

# convert "labelled" columns to factors
haven2earth = function(d)Df_(lapply(d, function(c) if (is.labelled(c)) as_factor(c) else c))

readTable.SAV = readTable.sav = function(path, options = NULL, headerMap = NULL, stringsAsFactors = F) {
	# read file
	#Require('foreign');
	#r0 = suppressWarnings(read.spss(path));
	Library('haven');
	rRaw = if (nif(options) && nif(options$VERBOSE))
		# read_spss -> read_sav (28.2.2023)
		haven::read_sav(path, encoding = options$ENCODING) else
		#read_spss(path) else
		suppressWarnings(haven::read_sav(path, encoding = options$ENCODING));
	r0 = haven2earth(rRaw);
	r1 = as.data.frame(r0, stringsAsFactors = stringsAsFactors);
	# <!> uncommented on 5.10.2021, no prior comment as to why commented out
	# as of 20.10.2021: some dates alredy converted -> check
	dateAlready = sapply(options$DATE, function(e)any(class(r1[[e]]) == 'POSIXct'));
	dates = options$DATE[!dateAlready];
	if (notE(dates)) r1[, dates] = do.call(data.frame, lapply(r1[, dates, drop = F], spssDate));
	r1
}

readTable.dta = function(path, options = NULL, headerMap = NULL, stringsAsFactors = F) {
	Library('haven');
	rRaw = if (nif(options) && nif(options$VERBOSE))
		read_stata(path) else
		suppressWarnings(read_stata(path));
	r1 = as.data.frame(rRaw, stringsAsFactors = stringsAsFactors);
	r1
}


readTable.RData = function(path, options = NULL, headerMap = NULL) {
	t = as.data.frame(get(load(path)[1]), stringsAsFactors = F);
	#print(t);
	t
}

readTable.rds = function(path, options = NULL, headerMap = NULL) {
	t = readRDS(path);
	#print(t);
	t
}


readTable.xls = function(path, options = NULL, ..., row.names = NULL, sheet = 1) {
	Library('gdata', quietly = TRUE);
	read.xls(path, sheet = sheet, ..., row.names = row.names, verbose = FALSE);
}

#tableFunctionConnect = c('csv', 'RData', 'gz');
tableFunctionConnect = c('bz2', 'RData', 'gz');
readersAcceptingFilesOnly = c('xls');

tableFunctionForPathMeta = function(path, template = 'readTable.%{ext}s', default = readTable.csv,
	forceReader = NULL) {
	sp = splitPath(path);
	compression = NULL;
	tmpFile = NULL;
	if (firstDef(forceReader, sp$ext) %in% c('bz2', 'gz')) {
		compression = sp$ext;
		sp = splitPath(sp$fullbase);
		tmpFile = Sprintf('%{file}s.%{ext}s', file = tempfile(), ext = sp$ext);
	}
	name = Sprintf(template, ext = firstDef(forceReader, sp$ext));
	f = if (exists(name)) get(name) else default;
	r = list(
		fct = f, name = name, ext = sp$ext,
		compression = compression, tempfile = tmpFile, path = path
	);
	r
}
tableFunctionForPath = function(path, template = 'readTable.%{ext}s',
	default = readTable.csv, forceReader = NULL) {
	tableFunctionForPathMeta(path, template, default, forceReader)$fct
}

# forceReader: force readerFunction
tableFunctionForPathReader = function(path, template = 'readTable.%{ext}s', default = readTable.csv,
	forceReader = NULL) {
	m = m0 = tableFunctionForPathMeta(path, template = 'readTable.%{ext}s', default = default, forceReader);
	if (!is.null(m$compression)) {
		path = if (m0$compression %in% tableFunctionConnect &&
				 !(m0$ext %in% readersAcceptingFilesOnly))
			compressedConnection(m0$path, m0$compression) else
			decompressPath(m0$path, m0$tempfile, m0$compression)$destination
		m = merge.lists(m0, list(path = path));
	}
	m
}

readTableDate = function(o, colName, col, defaultTz = 'MET') {
	dateFormat = o$DATEF[[colName]];
	r = strptime(col, dateFormat[1], tz = firstDef(dateFormat[2], defaultTz));
	r
}

readTableRecodeNAs = function(tab, NAs) {
	ns = names(tab);
	for (n in ns) if (class(tab[[n]]) == 'character') tab[[n]][ tab[[n]] %in% NAs] = NA;
	return(tab);
}

readTable.defaultOptions = list(HEADER = TRUE, hash = list(do = FALSE, type = 'sha256', hashFile = NULL));
# <!> as of 23.5.2014: headerMap after o$NAMES assignment
# <i> use tableFunctionForPath
#	options:
#		hashFile:	store file hash in propertyList. Create entry if not present, check otherwise
readTableSingle = function(path, autodetect = T, headerMap = NULL, extendedPath = T, colnamesFile = NULL, ...,
	as_factor = NULL, stringsAsFactors = F, defaultReader = readTable.csv, doRemoveTempFile = TRUE,
	forceReader = NULL, ssh = F, options = list()) {
	# <p> preparation
	pathOrig = path = join(path, '');
	#o = merge.lists(readTable.defaultOptions, options);
	o = merge.lists.recursive(readTable.defaultOptions, options);
	if (extendedPath) {
		r = splitExtendedPath(path);
		path = r$path;
		o = merge.lists(o, r$options);
	}

	sp = splitPath(path, ssh = ssh);
	# <p> copy remote files
	if (sp$host != '') {
		tmpPath = tempFileName('readTable', sp$ext, inRtmp = T);
		File.copy(path, tmpPath);
		path = tmpPath;
		sp = splitPath(path);
	}
	# <!> code not re-entrant, possible race-condition
	if (notE(o$hash) && notE(o$hash$hashFile)) {
		h = if (File.exists(o$hash$hashFile))
			propertyFromString(readFile(o$hash$hashFile)) else list(hashes = list());
		hash = hashPathContent(path, o$hash$type);
		# <A> use only filename to identify path -> add option for full path <i>
		hashS = h$hashes[[ sp$file ]]$hash;
		if (notE(hashS)) {
			if (hashS != hash) {
				LogS(5, 'File: %{file}s hash failure: %{hash}s [cmputed != %{hashS}s [stored]', sp);
				stop('File hash failure on readTable');
			}
			LogS(5, 'Hash check passed: %{file}s: %{hash}s', sp);
		} else {
			h$hashes[[ sp$file ]] = list(hash = hash, type = o$hash$type);
			writeFile(o$hash$hashFile, stringFromProperty(h));
			LogS(5, 'Hash initialized: %{file}s: %{hash}s', sp);
		}
	}
	# <p> read table raw
	reader = if (autodetect && !is.null(sp$ext)) 
		tableFunctionForPathReader(path, 'readTable.%{ext}s', readTable.csv, forceReader) else
		list(fct = defaultReader, path = path);
	r = reader$fct(reader$path, options = o, ...);

	# <p> cleanup
	if (doRemoveTempFile && !get('.system.doLogOnly', envir = System_env__) && !is.null(reader$tempfile))
		file.remove(reader$tempfile);

	# <p> table transformations
	if (!is.null(o$NAMES) && length(o$NAMES) <= ncol(r)) names(r)[1:length(o$NAMES)] = o$NAMES;
	colnamesFile = firstDef(o$COLNAMESFILE, colnamesFile);
	headerMap = c(headerMap, o$HEADERMAP);
	if (!is.null(headerMap)) names(r) = vector.replace(names(r), headerMap);
	if (!is.null(colnamesFile)) {
		ns = read.table(colnamesFile, header = F, as.is = T)[, 1];
		names(r)[1:length(ns)] = ns;
	}
	if (!is.null(o$PROJECT)) r = r[, o$PROJECT, drop = FALSE];
	if (!is.null(o$complete)) r = r[apply(r[, o$complete], 1, function(e)!any(is.na(e))), ];
	if (!is.null(o$CONST)) { for (n in names(o$CONST)) r[[n]] = o$CONST[[n]]; }
	if (notEC(as_factor)) r = Df_(r, as_factor = as_factor);
	if (notEC(o$FACTORS)) r = Df_(r, as_factor = o$FACTORS);
	if (Nif(o$NUMERIC)) r = Df_(r, as_numeric = o$NUMERIC);
	if (!is.null(o$DELETE)) r = r[-o$DELETE, , drop = F];
	if (notE(o[['DATEF']])) { for (n in names(o$DATEF)) r[[n]] = readTableDate(o, n, r[[n]]); }
	if (notE(o[['NA']]) && !(sp$ext %in% c('csv', 'txt', 'csv2'))) r = readTableRecodeNAs(r, o[['NA']]);
	r
}

# <i> joining operation on multiple provided tables -> interface for join var
readTable = function(path, autodetect = T, headerMap = NULL, extendedPath = T, colnamesFile = NULL, ...,
	as_factor = NULL, stringsAsFactors = F, defaultReader = readTable.csv, doRemoveTempFile = TRUE,
	forceReader = NULL, ssh = F, options = list(), noJoin = FALSE) {

	if (noJoin) {
		lapply(path, readTableSingle,
			autodetect = autodetect, headerMap = headerMap, extendedPath = extendedPath,
			colnamesFile = colnamesFile,
			...,
			as_factor = as_factor, stringsAsFactors = stringsAsFactors,
			defaultReader = defaultReader, doRemoveTempFile = doRemoveTempFile,
			forceReader = forceReader, ssh = ssh, options = options
		)
	} else readTableSingle(path, autodetect, headerMap, extendedPath, colnamesFile, ...,
		as_factor = as_factor, stringsAsFactors = stringsAsFactors,
		defaultReader = defaultReader, doRemoveTempFile = doRemoveTempFile,
		forceReader = forceReader, ssh = ssh, options = options)
}

resaveTableRdsSingle = function(path, ..., doSave = FALSE) {
	if (tolower(splitPath(path)$ext) == 'rds') return(path);
	output = Sprintf('%{fullbase}s.rds', splitPath(path));
	if (doSave) {
		tab = readTable(path, ...);
		saveRDS(tab, output);
	}
	return(output)
}

resaveTableRds = function(paths, ..., doSave = FALSE)
	lapply(paths, resaveTableRdsSingle, ..., doSave = doSave);

resaveTableSingle = function(path, ..., outputDir = NULL, extension = 'rds', doSave = FALSE, createDir = TRUE) {
	sp = splitPath(path);

	if (tolower(sp$ext) == 'rds') return(path);
	outputDir = ifelse(notE(outputDir), outputDir, sp$dir)
	output = Sprintf('%{outputDir}s/%{base}s.%{extension}s', sp);
	LogS(5, 'Resaving "%{path}s" -> "%{output}s"');
	if (doSave) {
		tab = readTable(path, ...);
		writeTable(tab, output, createDir = createDir);
	}
	return(output)
}

resaveTable = function(paths, ..., outputDir = NULL, extension = 'rds', doSave = FALSE, createDir = TRUE)
	lapply(paths, resaveTableSingle, ..., outputDir = outputDir, extension = extension, doSave = doSave, createDir = createDir);


#
#	<p> writeTable
#

writeTable.defaults = list(
	global = list(
	SEP = ' ', DEC = '.',
	ROW.NAMES = FALSE,
	HEADER = TRUE,
	QUOTE = TRUE
	),
	csv = list(SEP = ',', DEC = '.'),
	csv2 = list(SEP = ';', DEC = ',')
);
writeTable.table = function(dataFrame, path, ..., doCompress = NULL, row.names = TRUE, options = list()) {
	o = merge.lists(writeTable.defaults$global, list(ROW.NAMES = row.names), options);
	conn = compressedConnection(path, doCompress, mode = 'w');
	with(o, write.table(dataFrame, file = conn, ...,
		row.names = ROW.NAMES, col.names = HEADER, sep = SEP, quote = (QUOTE != '')));
}

writeTable.compression = function(object, path, doCompress = NULL, row.names = TRUE,
	doRemoveOrig = TRUE, options = list(), writerFunction) {

	pathRaw = if (!is.null(doCompress)) Sprintf('%{path}s_raw_') else path;
	writerFunction(object, pathRaw, row.names = row.names, options = options);
	r1 = compressPath(pathRaw, path, doCompress, doRemoveOrig);
	r1
}

writeTable.sav_raw = function(object, path, doCompress = NULL, row.names = TRUE, doRemoveOrig = TRUE,
	options = list()) {
	# <p> required package
	options( java.parameters = "-Xmx4g" );
	Require('haven');

	# sanitize row names
	d0 = as.data.frame(object);
	names(d0) = gsub("[.\\$@#]", "\\_", names(d0))
	write_sav(d0, path);
}

writeTable.sav = function(object, path, doCompress = NULL, row.names = TRUE,
	doRemoveOrig = TRUE, options = list()) {
	writeTable.compression(object, path, writerFunction = writeTable.sav_raw,
		doCompress = doCompress, row.names = row.names, doRemoveOrig = doRemoveOrig, options = options);
}

writeTable.rds = function(object, path, doCompress = NULL, row.names = TRUE,
	doRemoveOrig = TRUE, options = list()) {
	saveRDS(object, path);
}

writeTable.xlsx = function(object, path, doCompress = NULL, row.names = TRUE,
	doRemoveOrig = TRUE, options = list()) {
	options( java.parameters = "-Xmx4g" );
	Require('xlsx');
	r = path;
	# <p> save non-data frame-constrained column names
	colNames = if (is.matrix(object)) dimnames(object)[[2]] else NULL;
	dataFrame = as.data.frame(object);
	r0 = pathRaw = if (!is.null(doCompress)) Sprintf('%{path}s_raw_') else path;

	# <p> construct spreadsheet
	wb = createWorkbook();
	cs = CellStyle(wb, dataFormat = DataFormat("#,##0.00"));
	styles = listKeyValue(1:ncol(dataFrame), rep.list(cs, ncol(dataFrame)));
    sheet = createSheet(wb, 'dataFrame')
    addDataFrame(dataFrame, sheet, col.names = TRUE, row.names = row.names, 
        startRow = 1, startColumn = 1, colStyle = styles, colnamesStyle = NULL, 
        rownamesStyle = NULL);
    saveWorkbook(wb, pathRaw);

    #r0 = write.xlsx2(dataFrame, file = pathRaw, row.names = row.names, showNA = FALSE,
	#	colStyle = styles);
	r1 = compressPath(pathRaw, path, doCompress, doRemoveOrig);
	r0
}

writeTable.xls = function(object, path, doCompress = NULL, row.names = TRUE,
	doRemoveOrig = TRUE, options = list()) {
	Library('WriteXLS');
	r = path;
	dataFrame = as.data.frame(object);
	pathRaw = if (!is.null(doCompress)) Sprintf('%{path}s_raw_') else path;
	r0 = WriteXLS(dataFrame, ExcelFileName = pathRaw, row.names = row.names);
	r1 = compressPath(pathRaw, path, doCompress, doRemoveOrig);
	r0
}

writeTable.csv2 = writeTable.csv = function(dataFrame, path, ..., doCompress = NULL,
	row.names = TRUE, options = list()) {
	conn = compressedConnection(path, doCompress, mode = 'w');
	with(options, LogS(5, 'Write table: SEP="%{SEP}s", HEADER=%{HEADER}s, quote=%{QUOTE}s'));
	with(options, write.table(dataFrame, file = conn, ...,
		sep = SEP, col.names = HEADER, row.names = ROW.NAMES, quote = (QUOTE != ''), dec = DEC));
}
# doCompress = 'bz2' to write bz2
# <i><!> determine from path
writeTableRaw = function(object, path, ..., doCompress = NULL, row.names = TRUE, autodetect = TRUE,
	defaultWriter = writeTable.csv, options = list(), createDir = FALSE) {
	sp = splitPath(path);
	if (createDir) Dir.create(sp$dir);
	if (is.null(doCompress) && !is.null(sp$ext) && sp$ext %in% c('bz2', 'gz')) {
		doCompress = sp$ext;
	}
	writer = if (autodetect && !is.null(sp$ext)) 
		tableFunctionForPath(path, 'writeTable.%{ext}s', writeTable.csv) else defaultWriter;
	if (is.null(writer))
		stop(Sprintf("Writing table to extension '%{ext}s' not supported", ext = sp$ext));
	options = merge.lists(writeTable.defaults$global,
		writeTable.defaults[[firstDef(sp$ext, 'global')]], options);
	r0 = writer(object, path = path, ..., doCompress = doCompress, row.names = row.names, options = options);
	r = list(path = path, return = r0);
	r
}

writeTable = function(object, path, ..., doCompress = NULL, row.names = TRUE, autodetect = TRUE,
	defaultWriter = writeTable.csv, simplify = TRUE, extendedPath = TRUE) {
	o = list();
	if (extendedPath) {
		r = splitExtendedPath(path);
		path = r$path;
		o = r$options;
		defaultWriter = writeTable.table;
	}
	if (!is.null(o$PROJECT)) object = object[, o$PROJECT, drop = FALSE];
	r = lapply(path, function(p)
		writeTableRaw(object, p, ...,
			doCompress = doCompress, row.names = row.names, autodetect = autodetect,
			defaultWriter = defaultWriter, options = o)
	);
	if (simplify && length(path) == 1) r = r[[1]];
	r
}

#
#	<p> swig
#

swigIt = function(interface, code, moduleName = NULL) {
	dir = tempdir();	# will be constant across calls
	if (is.null(moduleName)) {
		t = tempFileName("swig");
		moduleName = splitPath(t)$base;
	}

	ifile = sprintf("%s/%s.%s", dir, moduleName, "i");
	interface = sprintf("
		%%module %s
		%%inline %%{
			%s;
		%%}
	", moduleName, paste(interface, collapse = ";\n\t\t\t"));

	ifile = sprintf("%s/%s.%s", dir, moduleName, "i");
	base = splitPath(ifile)$fullbase;
	writeFile(ifile, interface);
	cfile = sprintf("%s.c", base);
	writeFile(cfile, code);
	#print(list(i = ifile, c = cfile, so = sprintf("%s.so", base)));
	system(sprintf("swig -r %s", ifile));
	#cat(code);

	system(sprintf("cd %s ; gcc -O2 -D__USE_BSD -D__USE_GNU -std=c99 -c -fpic %s.c %s_wrap.c -I/usr/local/lib64/R/include -lm ",
		splitPath(ifile)$dir, base, base));
	system(sprintf("cd %s ; gcc -shared %s.o %s_wrap.o -o %s.so", splitPath(ifile)$dir, base, base, base));
	#dyn.unload(sprintf("%s.so", base));
	dyn.load(sprintf("%s.so", base));
	source(sprintf("%s/%s.R", splitPath(ifile)$dir, moduleName));
}

#
#	<p> print
#

fprint = function(..., file = NULL, append = F) {
	if (!is.null(file)) sink(file = file, append = append);
	r = print(...);
	if (!is.null(file)) sink();
	r
}


#
#	crypotgraphy/checksumming
#

#
#	<p> package documentation
#

#	docFile = sprintf('%s/tmp/docOut.Rd', Sys.getenv('HOME'));
#	docDir = sprintf('%s/src/Rpackages/parallelize.dynamic/parallelize.dynamic/man', Sys.getenv('HOME'));
#	docs = RdocumentationSkeleton('Rparallel.back.R', 'parallelize.dynamic', output = docFile);
#	writeRdocumentationToDir(docFile, docDir);

RdocumentationForObjects = function(items, envir, unparser = function(item, envir)item) {
	files = suppressMessages({
		sapply(items, function(item)unparser(item, envir));
	});
	docs = lapply(files, readFile);
	names(docs) = sapply(files, function(f)splitPath(f)$base);
	docs
}
RdocumentationForFunctions = function(items, envir) {
	docs = RdocumentationForObjects(items, envir, unparser = function(item, envir) {
		file = file.path(tempdir(), sprintf("%s.Rd", item));
		prompt(get(item, envir = envir), name = item, filename = file);
		file
	});
	docs
}
RdocumentationForClasses = function(items, envir) {
	docs = RdocumentationForObjects(items, envir, unparser = function(item, envir) {
		file = file.path(tempdir(), sprintf("%s-class.Rd", item));
		methods::promptClass(item, filename = file, where = envir);
		file
	});
	docs
}
RdocumentationForMethods = function(items, envir) {
	docs = RdocumentationForObjects(items, envir, unparser = function(item, envir) {
		file = file.path(tempdir(), sprintf("%s-methods.Rd", item));
		methods::promptMethods(item, filename = file, findMethods(item, where = envir));
		file
	});
	docs
}


# code from packages.skeleton
objectsFromCodeFiles = function(R_files, packageName = 'generic') {
	e = new.env(hash = T);
	methods::setPackageName(packageName, e);
	for (f in R_files) sys.source(f, envir = e);
	classes = getClasses(e);
	methods = getGenerics(e);
	others = ls(e, all.names = T);
	others = others[grep('^\\.', others, invert = T)];

	r = list(envir = e, classes = classes, methods = methods,
		others = setdiff(setdiff(others, classes), methods));
	r
}

RdocumentationSkeleton = function(R_files, output = NULL, packageName = 'generic') {
	os = objectsFromCodeFiles(R_files, packageName = packageName);
	docs = c(
		RdocumentationForFunctions(os$others, os$envir),
		RdocumentationForClasses(os$classes, os$envir),
		RdocumentationForMethods(os$methods, os$envir)
	);

	doc = join(nlapply(docs, function(n) {
		sprintf("\nDOCUMENTATION_BEGIN:%s\n%s\nDOCUMENTATION_END\n", n, docs[[n]])
	}), "\n");
	if (!is.null(output)) {
		if (File.exists(output)) {
			Log(sprintf("Move away file '%s' before writing new skeleton", output), 2);
		} else {
			writeFile(output, doc);
		}
	}
	doc
}

writeRdocumentationToDir = function(pathesIn, pathOut, cleanOut = F) {
	doc = sapply(pathesIn, readFile, USE.NAMES = F);
	r = unlist.n(getPatternFromStrings(doc, '(?s)(?:\\nDOCUMENTATION_BEGIN:)([^\\n]+)\\n(.*?)(?:\\nDOCUMENTATION_END\\n)'), 1);
	Dir.create(pathOut, recursive = T);
	if (cleanOut) {
		files = list_files_with_exts(pathOut, 'Rd');
		file.remove(files);
	}
	nlapply(r, function(n) {
		output = file.path(pathOut, sprintf('%s.Rd', n));
		Log(sprintf('Writing to %s', output), 3);
		writeFile(output, r[[n]]);
	});
	names(r)
}

reDoc = function(package = 'parallelize.dynamic',
	docFile = sprintf('./%s.doc.Rd', package), docDir = sprintf('./%s/man', package)) {
	writeRdocumentationToDir(docFile, docDir, cleanOut = T);
	install.packages(sprintf('./%s', package), repos = NULL);
	#detach(package);
	#library(package)
}

#
#	<p> Rcpp helpers
#

# mod = createModule('module_parametrizer', 'build/libhaplotypes_rcpp.so',
#	headers = c('parameterhelper.h', 'parametrizer.h', 'parametrizerrcpp.h', 'parametrizerrcpp_module.h'),
#	output = 'mod'
#);

#mod = moduleCreate('Rmmap.h', 'build/libmmap_rcpp.so', output = 'mod_mmap');

# interface: C++ header file w/ RCPP_MODULE declaration
#	code block between '// -- begin inline Rcpp' and '// -- end inline Rcpp' is used
moduleCreate = function(interface, libpathes = c(), headers = c(), output = NULL) {
	Require('Rcpp');
	Require('inline');

	# <p> libs tb linked against
	dirs = sapply(libpathes, function(e)splitPath(e)$dir);
	libs = sapply(libpathes, function(e)fetchRegexpr('(?<=lib)(.*)(?=.so)', splitPath(e)$file));
	.libPaths(c(.libPaths(), dirs));
	libincludes = join(sapply(seq_along(dirs), function(i)sprintf('-L"%s" -l%s', splitPath(dirs[i])$absolute, libs[i])), ' ');

	# <p> prepare environment
	Sys.setenv(`PKG_LIBS` = sprintf('%s %s', Sys.getenv('PKG_LIBS'), libincludes));
	# <!> hardcoded include path as of 2/2021
	Sys.setenv(`PKG_CXXFLAGS` = sprintf('%s %s -I/usr/include/c++/9 -I%s', Sys.getenv('PKG_LIBS'),
		stdOutFromCall(Rcpp:::CxxFlags()), getwd()));
	for (lib in libpathes) { dyn.load(lib, local = F) }

	# <p> create definition file
	moduleRegex = '(?s:(?<=// -- begin inline Rcpp\n)(.*?)(?=// -- end inline Rcpp))';
	inc = join(sapply(interface, function(f)fetchRegexpr(moduleRegex, readFile(f))), "\n");
	inc = join(c(sapply(headers, function(f)Sprintf('#include "%{f}s"\n')), inc), "\n");
	print(inc);
	moduleName = FetchRegexpr('RCPP_MODULE[(](.*?)[)]', inc, captures = T);
	LogS(2, 'Creating module %{moduleName}s');

	# <p> create module
	#rcpp = cxxfunction( signature(), '' , includes = inc, plugin = 'Rcpp', verbose = T );
	rcpp = cxxfunction( , '' , includes = inc, plugin = 'Rcpp', verbose = T );
	mod = Module( moduleName,  getDynLib(rcpp) );
	if (!is.null(output)) {
		Dir.create(output, recursive = T);
		libfiles = sapply(libpathes, function(lib) {
			File.copy(lib, sprintf('%s/%s', output, splitPath(lib)$file), symbolicLinkIfLocal = F);
			splitPath(lib)$file
		});
		glue = sprintf('%s/%s.so', output, moduleName);
		File.copy(getDynLib(rcpp)[['path']], glue, symbolicLinkIfLocal = F);
		module_descriptor = list(
			name = moduleName,
			libs = c(libfiles, splitPath(glue)$file)
		);
		save(module_descriptor, file = sprintf('%s/module.RData', output));
	}
	mod
}

moduleActivate = function(path) {
	Require('Rcpp');
	module_descriptor = get(load(sprintf('%s/module.RData', path))[1]);
	r = lapply(module_descriptor$libs, function(lib)try(dyn.unload(sprintf('%s/%s', path, lib)), silent = T));
	r = lapply(module_descriptor$libs, function(lib)dyn.load(sprintf('%s/%s', path, lib), local = F));
print(r);
	mod = Module( module_descriptor$name, rev(r)[[1]] );
	#mod = Module( module_descriptor$name, r[[1]] );
	mod
}



#
#	<p> sqlite
#
sqlCreateTable = function(columns, types = list, index = NULL, createAt = NULL, unique = list(),
	tableName = 'data') {
	# <p> types
	types = merge.lists(listKeyValue(columns, rep('text', length(columns))), types);
	# <p> cols
	cols = join(sep = ', ', sapply(columns, function(e)sprintf('%s %s', qs(e, force = T), types[e])));
	# <p> unique constraints
	#UNIQUE (col_name1, col_name2) ON CONFLICT REPLACE)
	unique = circumfix(join(sapply(unique, function(u) {
		Sprintf('UNIQUE (%{cols}s) ON CONFLICT FAIL', cols = join(qs(u, force = T), ', '))
	}), ', '), pre = ' , ');
	createDbSql = join(sep = "\n", c(
		Sprintf('CREATE TABLE %{tableName}s (%{cols}s%{unique}s);'),
		if (is.null(index)) c() else sapply(1:length(index), function(i)
			Sprintf('CREATE INDEX index_%{tableName}s_%{i}d ON %{tableName}s (%{cols}s);',
				cols = join(qs(index[[i]], force = T), sep = ', '))),
		'.quit', ''
	));
	if (!is.null(createAt)) {
		Dir.create(createAt, treatPathAsFile = T);
		System(sprintf('echo \'%s\' | sqlite3 %s', createDbSql, qs(createAt)), 5);
	}
	createDbSql
}

# Create sqlite database with contents of csv-file
# @par index: list of columns to index
# @par type: sqlite types: integer, real, text, blob, not specified assumes text
# @par rawFilter pipable command call to filter rows

sqlTypeMap = list(integer = 'integer', numeric = 'real', character = 'text');
csv2sqlitSepMap = readTableSepMap;
sepMap = list(T = '\\t', S = ' ', C = ',', `;` = ';', `S+` = ' ');
sepMapCut = list(T = '\\t', S = '" "', C = ',', `;` = ';', `S+` = '" "');
csv2sqlite = function(path, output = tempfile(),
	columnsNames = NULL, columnsSelect = NULL,
	index = NULL,
	inputSep = 'T', inputHeader = T, inputSkip = 0,
	NULLs = NULL, types = list(), newDb = T, unique = list(),
	tableName = 'data', removeLeadingWS = T, headerMap = NULL,
	rawFilter = NULL) {
	if (is.null(output)) stop(Sprintf('No output path given for sqlite DB [NULL]@csv2sqlite'));
	if (newDb && file.exists(output)) file.remove(output);
	# <!> cave: does not heed skip
	if (inputHeader && is.null(columnsNames)) {
		columnsNames = read.table(path, header = F, nrows = 1, sep = csv2sqlitSepMap[[inputSep]]);
	}
	# <p> select columns
	#	<!> cave cut sorts provided indeces
	prefix = if (removeLeadingWS) "sed 's/^\\s\\s*//' |" else '';
	cut = if (notE(columnsSelect)) {
		Isel = which.indeces(columnsSelect, avu(columnsNames));
		columnsSelect = columnsSelect[order(Isel)];	# align ordering
		# <i> S+
		#  cat | sed 's/\s\s*/ /g' | cut -d' ' -f5
		cut = if (inputSep == 'S+')
			Sprintf("%{prefix}s sed 's/\\s\\s*/ /g' | cut") else Sprintf('%{prefix}s cut');
		Sprintf('| %{cut}s %{sep}s -f %{columns}s ',
			sep = if (inputSep == 'T') '' else sprintf('-d %s', sepMapCut[[inputSep]]),
			columns = join(sort(Isel), ',')
		)
	} else '';
	columns = if (notE(columnsSelect)) columnsSelect else columnsNames;
	columnsDb = if (notE(headerMap)) avu(headerMap[columns]) else columns;
	types = merge.lists(listKeyValue(columnsDb, rep('text', length(columns))), types);
	sqlCreateTable(columnsDb, types, index, createAt = output, unique = unique, tableName = tableName);

	# <p> import data
	#skipCommand = if (is.null(inputSkip)) '' else sprintf('| tail -n +%d ', inputSkip + 1);
	# <N> always skip header as column re-mapping might have occured
	skipCommand = sprintf('| tail -n +%d ', inputSkip + 2);
	sp = splitPath(path);
	reader = Sprintf(if (notE(sp$ext) && sp$ext == 'gz') 'zcat %{path}Q ' else 'cat %{path}Q ');
	if (notE(rawFilter)) reader = Sprintf('%{reader}s | %{rawFilter}s ');
	sqlStatements = c(
		#Sprintf(".separator %{sep}Q", sep = sepMap[[inputSep]]),
		Sprintf(".separator \"%{sep}s\"", sep = sepMap[[inputSep]]),
		Sprintf(".import \"/dev/stdin\" %{tableName}s")
	);
	importSqlite = join(sep = "\n", sqlStatements);
	LogS(5, 'Sqlite import statements: %{importSqlite}s');
	importSql = writeFile(tempfile(), importSqlite);

	sepText = sepMap[[inputSep]];
	filter = if (is.null(NULLs)) '' else
		sprintf("| perl -pe 's/((?<=%s)(?:%s)(?=%s|$)|(?<=^)(?:%s)(?=%s|$))//g'",
			sepText, join(NULLs, '|'), sepText, sepText, sepText);
	cmd = Sprintf(con(
		"%{reader}s %{skipCommand}s %{cut}s %{filter}s",
		" | sqlite3 -init %{importSql}Q %{output}Q"));
	System(cmd, 5);
	output
}
# <!> unfinished, siphones code from old csv2sqlite function
url2sqlite = function(url, output = tempfile(), header = NULL, skip = NULL, selectColumns = NULL,
	index = NULL, sep = 'T',
	NULLs = NULL, types = list()) {
	
	# <p> determine header
 	tmp1 = tempfile();
 	ret = download.file(url, tmp1, method, quiet = FALSE, mode = "w", cacheOK = TRUE);
	#if (ret) stop(sprintf("Download of '%s' failed.", url));
	if (is.null(header)) {
		tmpHeader = tempfile();
	}
}

# <!> 7.1.2015: was qq, but conflicts with QQ-plot function
qquote = function(s)as.character(fetchRegexpr('([^ ]+)', s, captures = T))

sqlite2sqlite = function(dbS, dbD, query, cols, types = list(), index = NULL) {
	sqlCreateTable(cols, types, index, createAt = dbD);
	cmd = sprintf("echo %s | sqlite3 -init %s %s | sqlite3 -init %s %s",
		qs(query),
		qs(writeFile(tempfile(), ".mode csv")),
		qs(dbS),
		qs(writeFile(tempfile(), ".separator ,\n.import \"/dev/stdin\" data")),
		qs(dbD)
	);
	System(cmd, 5);
	dbD
}

sqliteOpen = function(path) {
	Require('RSQLite');
	dbConnect(SQLite(), dbname = path);
}

sqliteQueryParticle = function(q) {
	Nq = length(q);
	opRaw = if(Nq > 2) q[[2]] else if (q[[1]] == 'Statement') q[[1]] else 'equal';
	op = switch(opRaw,
		equal = '=',
		`<` = '<',
		`>` = '>',
		`< Int` = '<',
		`> Int` = '>',
		`< Num` = '<',
		`> Num` = '>',
		`< Char` = '<',
		`> Char` = '>',
		'like' = 'LIKE',
		Statement = NA
	);
	suff = if (opRaw %in% c('< Int', '> Int', '< Num', '> Num')) ' + 0' else '';
	value = if (Nq > 2) q[[3]] else q[[2]];
	if (!(opRaw %in% c('< Int', '> Int', '< Num', '> Num', 'Statement'))) value = Sprintf('%{value}Q');
	if (opRaw %in% c('Statement'))
		with(q, Sprintf('%{value}s')) else
		with(q, Sprintf('%{name}Q%{suff}s %{op}s %{value}s'))
}
sqliteBuildQuery = function(table, query = NULL, distinct = TRUE) {
	if (is.null(query)) return(Sprintf('SELECT * FROM %{table}Q'));
	ns = names(query);
	qs = ilapply(query, function(q, i)sqliteQueryParticle(c(list(name = ns[i]), q)));
	condition = join(unlist(qs), sep = ' AND ');
	isDistinct = if (distinct) 'DISTINCT' else '';
	query = Sprintf('SELECT %{isDistinct}s * FROM %{table}Q WHERE %{condition}s');
	query
}

# query: list(chr = info$chr, mapPhy = list('> Int', pos - range), mapPhy = list('< Int', pos + range));
#	list(id = list(like = 'rs%'))

sqliteQuery = function(db, query = list(Statement = '1'), table = NULL, N = -1) {
	if (is.null(table)) table = dbListTables(db)[1];
# 	query = con(sapply(names(query), function(n)Sprintf('%{n}Q = %{v}s', v = qs(query[[n]], force = T))));
# 	query1 = Sprintf('SELECT * FROM %{table}Q WHERE %{query}s');
	query1 = sqliteBuildQuery(table, query);
	Log(query1, 5);
	dbGetQuery(db, query1, n = N);
}

#
#	<p> publishing
#

# if (1) {
#	.fn.set(prefix = 'results/201404/expressionMonocytes-')
# 	initPublishing('expressionMonocytes201404', '201405');
# 	publishFile('results/expressionMonocytesReportGO.pdf');
# }
# # force as subdir in the reporting dir
# publishDir('results/BAP1IHCgekoppeldaanWelofGeenMonosomie3', asSubdir = T);

Publishing_env__ <- new.env();
initPublishing = function(project, currentIteration, publicationPath = '/home/Library/ProjectPublishing',
	logLevel = 5) {
	md5 = md5sumString(project);
	assign('project', project, Publishing_env__);
	assign('projectMd5', md5, Publishing_env__);
	assign('currentIteration', currentIteration, Publishing_env__);
	assign('publicationPath', publicationPath, Publishing_env__);
	LogS(logLevel, "Project tag: %{project}s Md5: %{md5}s");
}
publishFctEnv = function(path, into = NULL, as = NULL) with(as.list(Publishing_env__), {
	if (!exists('project')) stop('Publishing system not yet initialized.');

	projectFolder = Sprintf('%{publicationPath}s/%{projectMd5}s');
	prefix = if (is.null(into)) '' else Sprintf('%{into}s/');
	destinationPrefix = Sprintf('%{projectFolder}s/%{currentIteration}s/%{prefix}s');
	destination = Sprintf('%{destinationPrefix}s%{path}s',
		path = if (is.null(as)) splitPath(path)$file else as);
	r = list(projectFolder = projectFolder, prefix = prefix, destination = destination,
		destinationPrefix = destinationPrefix);
	r
})


publishFileRaw = function(file, into = NULL, as = NULL) with(publishFctEnv(file, into, as), {
	if (!is.null(into)) Dir.create(destination, treatPathAsFile = T, recursive = T);
	Logs('Publishing %{file} --> "%{destination}s', 3);
	Dir.create(splitPath(destination)$dir, recursive = T);
	System(Sprintf("chmod -R a+rX %{dir}s", dir = qs(projectFolder)), 4);
	r = file.copy(file, destination, overwrite = T);
	if (any(!r)) Logs("Copying of '%{file}s' failed.", 3);
	Sys.chmod(destination, mode = '0755', use_umask = F);
	destination
})

publishUnit = function(u, into = NULL, as = NULL, asSubdir = TRUE) {
	if (!file.exists(u)) {
		LogS(4, 'publishUnit: "%{u}s" does not exist');
		return(NULL);
	}
	if (file.info(u)$isdir)
		publishDir(u, into, as, asSubdir) else
		publishFileRaw(u, into, as)
}

publishFiles = publishFile = function(files, into = NULL, as = NULL) {
	lapply(files, publishUnit, into = into, as = as)
}

publishCsv = function(table, as, ..., into = NULL) {
	file = tempfile('publish', fileext = 'csv');
	write.csv(table, file = file, ...);
	publishFile(file, into, as);
}

publishDir = function(dir, into = NULL, as = NULL, asSubdir = FALSE) with(publishFctEnv('', into, as), {
	sp = splitPath(dir);
	# if 'dir' is a slashed dir itself, use the last dir-component
	if (asSubdir) into = if (sp$file == '') splitPath(sp$dir)$file else sp$file;
	if (!is.null(into)) {
		destination = splitPath(Sprintf('%{destination}s/%{into}s/'))$fullbase;	# remove trailing slash
	}
	Dir.create(destination);
	Logs('Publishing %{dir} --> %{destination}s', 3);
	Dir.create(destination, recursive = T);
	System(Sprintf("chmod -R a+rX %{projectFolder}Q"), 4);
	System(Sprintf("cp -r %{dir}Q/* %{destination}Q"), 4);
	System(Sprintf("chmod -R a+rX %{projectFolder}Q"), 4);
	destination
})

publishAsZip = function(files, as, into = NULL, recursive = FALSE) {
	tmp = tempFileName('publishAsZip', createDir = T, inRtmp = T);
	output = tempFileName('publishAsZip', 'zip', inRtmp = T, doNotTouch = T);
	sapply(files, function(file) {
		File.symlink(splitPath(file)$absolute, Sprintf("%{tmp}s"), replace = F);
		NULL
	});
	recursiveOption = ifelse(recursive, '-r', '');
	System(Sprintf("zip -j %{recursiveOption}s %{output}s %{tmp}s/*"), 2);
	publishFile(output, into = into, as = as);
}

#
#	<p> quick pdf generation
#

print2pdf = function(elements, file) {
	es = elements;
	tf = tempfile();
	sink(tf);
		nlapply(es, function(n) {
			cat(n);
			cat('\n-------------------------------------------\n');
			print(es[[n]]);
			cat('\n\n');
		})
	sink();
	System(Sprintf('a2ps %{tf}s --columns 1 --portrait --o - | ps2pdf - - > %{output}s', output = qs(file)));
}

#
#	<p> workarounds
#

# misc

#
#	<p> packages
#

LibraryRaw = function(name, ..., repos, repoNoInteraction, repoDefault, quietly,
	dependencies, reposSupplementary) {
	# force evaluation
	#if (!Eval(Sprintf('require(%{name}s)'))) {
	if (!Require(name, character.only = TRUE, quietly = TRUE)) {
		if (repoNoInteraction) repos[repos == '@CRAN@'] = repoDefault[1];
		repos = c(repos, reposSupplementary);
		#expr = Sprintf('install.packages(%{name}s)');
		r = try(install.packages(name, repos = repos, ..., dependencies = dependencies));
		# if installation from CRAN fails, try bioconductor
		Log(Sprintf('Trying to install "%{name}s" from bioconductor'), 5);
		if (is.null(r) || class(r) == 'try-error') {
			#if (!exists('biocLite')) source("https://bioconductor.org/biocLite.R");
			#biocLite(name, suppressUpdates = TRUE, suppressAutoUpdate = TRUE, dependencies = dependencies)
			# > 3.5.0
			Require('BiocManager');
			BiocManager::install(name, update = FALSE, dependencies = dependencies);
		}
		#Eval(Sprintf('library(%{name}s)'));
		#library(name, character.only = TRUE, quietly = quietly);
		# <N> hack to get shiny running
		do.call('library', list(name, character.only = TRUE, quietly = quietly));
	}
}

# name has to be character of length 1
LibrarySingle = function(name, ...,
	repos = getOption('repos'), repoNoInteraction = TRUE, repoDefault = "http://cran.rstudio.com",
	quietly = TRUE, dependencies = TRUE, reposSupplementary = 'http://www.rforge.net/', LibraryCollect = TRUE) {

	if (LibraryCollect && name == '__RESET__')return(LibraryAppendReset());
	if (LibraryCollect) LibraryAppend(name);
	wrapper = if (quietly) suppressWarnings else eval;
	for (name in name) {
		wrapper(LibraryRaw(name, ...,
			repos = repos, repoNoInteraction = repoNoInteraction, repoDefault = repoDefault,
			quietly = quietly, dependencies = dependencies, reposSupplementary = reposSupplementary));
	}
}
Library = function(name, ...,
	repos = getOption('repos'), repoNoInteraction = TRUE, repoDefault = "http://cran.rstudio.com",
	quietly = TRUE, dependencies = TRUE, reposSupplementary = 'http://www.rforge.net/', LibraryCollect = TRUE) {

	for (name in name) {
		LibrarySingle(name, ..., repos = repos, repoNoInteraction = repoNoInteraction,
			repoDefault = repoDefault, quietly = quietly,
			dependencies = dependencies, reposSupplementary = reposSupplementary,
			LibraryCollect = LibraryCollect
		);
	}
}
Require = function(..., quietly = TRUE) {
	wrapper = if (quietly) function(call_)suppressWarnings(suppressPackageStartupMessages(call_)) else eval
	#wrapper(require(..., quietly = quietly))
	# hack to get shiny running
	wrapper(do.call('require', c(list(...), list(quietly = quietly))))
}

DumpPackageNames = function(path = '~/Documents/AdminComputer/R/packages-%{version}s.csv',
	version = with(R.Version(), Sprintf('%{major}s.%{minor}s')), n = 1) {
	Logs('Writing package names to file:%{path}s', path = Sprintf(path), logLevel = 2);
	lib = .libPaths()[n];
	writeTable(Df(pkg = row.names(installed.packages(lib))), path = Sprintf(path));
}
ReadPackageNames = function(path = '~/Documents/AdminComputer/R/packages-%{version}s.csv',
	version = R.Version()$version.string) {
	Logs('Reading package names to file:%{path}s', path = Sprintf(path), logLevel = 2);
	readTable(Sprintf(path))$pkg;
}
ReinstallPackages = function(packages, n = 1, type = 'source') {
	lib = lib <- .libPaths()[n];
	install.packages(lib  = lib, pkgs = packages, type = type)
}
Reinstall = function(n = 1, type = 'source') {
	ReinstallPackages(row.names(installed.packages(lib)), n = n , type = type);
}
ReinstallFromVersion = function(version, n = 1, type = 'source') {
	ReinstallPackages(ReadPackageNames(version = version), n = n , type = type);
}

#
#	<p> misc linux system stuff
#

agenda = function()system('bash -l', input = c('shopt -s expand_aliases', 'agenda'))
agenda_stop = function()system('bash -l', input = c('shopt -s expand_aliases', 'agenda-stop'))
# 10.2.2020: re-implemented in RtestingHelpers.R
#runTests = function()system('RrunTests')

#
#	<p> random numbers
#

#
#	<p> Reporting
#

# Knit('dir/input.Rmd')
Knit = function(input, output = NULL, ..., format = 'html') {
	Require('knitr');
	Require('markdown');
	sp = splitPath(input);
# 	knit(input, output = with(sp, Sprintf('%{fullbase}s.md')));
# 	switch(format,
# 		'html' = markdownToHTML(with(sp, Sprintf('%{fullbase}s.md')), with(sp, Sprintf('%{fullbase}s.html')))
# 	)
	owd = getwd();
	setwd(sp$dir);
	for (f in format) {
		switch(f,
			'html' = rmarkdown::render(sp$file,
				output_format = rmarkdown::html_document(),
				output_file = with(sp, Sprintf('%{base}s.html')), ...)
		)
	}
	setwd(owd);
}

#
#	<p> stop
#

#
#	<p> debugging
#

#
#	<p> file system
#

#
#	<p> IO
#

sinkStop = function(env = parent.frame()) {
	if (get('sinkOpen', envir = env)) {
		sink(type = 'output');
		sink(type = 'message');
	}
	assign('sinkOpen', FALSE, envir = env)
}

Capture = function(expr, tee = TRUE, envir = parent.frame()) {
	# <p> setup
	tO = tempfile()
	tE = tempfile()
	sink(file(tO, open = 'w'), type = 'output', split = tee);
	sink(file(tE, open = 'w'), type = 'message', split = FALSE);
	sinkOpen = TRUE;
	on.exit( sinkStop() );

	# <p> evaluate
	r = eval(expr, envir = envir);
	sinkStop();

	# <p> read output, print, return
	rCap = list(return = r, stdout = readFile(tO), stderr = readFile(tE));
	if (tee) cat(rCap$stderr, file = 'stderr');
	return(rCap);
}

#
#	<p> low-level
#

renice = function(pid = Sys.getpid(), nicity = 20, logLevel = 4) {
	SystemS('renice %{nicity}d %{pid}d', logLevel);
}

#
#	Rgraphics.R
#Mon 27 Jun 2005 10:52:17 AM CEST

#Require('grid');	# -> Rlibraries.R

#
#	<p> unit model
#
#	base unit is cm
setGeneric("factorToBase", function(this) standardGeneric("factorToBase"));
setGeneric("fromUnitToUnit", function(thisA, thisB) standardGeneric("fromUnitToUnit"));
setClass('unitGeneric', representation = list(value = 'numeric'), prototype = list(value = as.numeric(NA)));
setMethod('initialize', 'unitGeneric', function(.Object, value = as.numeric(NA)) {
	.Object@value = value;
	.Object
});
setMethod('fromUnitToUnit', c('unitGeneric', 'unitGeneric'), function(thisA, thisB)
	new(class(thisB), value = thisA@value * factorToBase(thisA) / factorToBase(thisB)));

setClass('unitCm', contains = 'unitGeneric');
setMethod('initialize', 'unitCm', function(.Object, value)callNextMethod(.Object, value = value));
setMethod('factorToBase', 'unitCm', function(this)1);

setClass('unitInch', contains = 'unitGeneric');
setMethod('initialize', 'unitInch', function(.Object, value)callNextMethod(.Object, value = value));
setMethod('factorToBase', 'unitInch', function(this)cm(1));

setClass('unitDpi150', contains = 'unitGeneric');
setMethod('initialize', 'unitDpi150', function(.Object, value)callNextMethod(.Object, value = value));
setMethod('factorToBase', 'unitDpi150', function(this)cm(1)/150);

setClass('unitDpi200', contains = 'unitGeneric');
setMethod('initialize', 'unitDpi200', function(.Object, value)callNextMethod(.Object, value = value));
setMethod('factorToBase', 'unitDpi200', function(this)cm(1)/200);

setClass('unitDpi300', contains = 'unitGeneric');
setMethod('initialize', 'unitDpi300', function(.Object, value)callNextMethod(.Object, value = value));
setMethod('factorToBase', 'unitDpi300', function(this)cm(1)/300);

setClass('unitPoints', contains = 'unitGeneric');
setMethod('initialize', 'unitPoints', function(.Object, value)callNextMethod(.Object, value = value));
setMethod('factorToBase', 'unitPoints', function(this)cm(1)/72);

valueU = valueUnited = function(value, unit) {
	class = getClass(Sprintf('unit%{unit}u'));
	new(class, value = value)
}
toUnit = function(value, unit)fromUnitToUnit(value, valueU(as.numeric(NA), unit));
ToUnit = function(value, unit)toUnit(value, unit)@value;

#
#	</p> unit model
#

#
#	<p> saving of plots
#


# base unit is 600dpi
units_conv = list(
	cm = list(from = function(cm)(cm/2.54*600), to = function(b)(b/600*2.54)),
	points = list(from = function(points)(points/72*600), to = function(b)(b/600*72)),
	inch = list(from = function(i)(i*600), to = function(b)(b/600)),
	dpi150 = list(from = function(dpi)(dpi/150*600), to = function(b)(b*150/600)),
	dpi300 = list(from = function(dpi)(dpi/300*600), to = function(b)(b*300/600))
);
units_default = list(jpeg = 'dpi300', pdf = 'cm', png = 'points');
#
#	RgraphicsExt.R
#Wed Jan 15 12:07:47 CET 2020
# moved most of Rgraphcs.R 
#Mon 27 Jun 2005 10:52:17 AM CEST

#Require('grid');	# -> Rlibraries.R

#
#	<p> unit model
#
#	base unit is cm
setGeneric("factorToBase", function(this) standardGeneric("factorToBase"));
setGeneric("fromUnitToUnit", function(thisA, thisB) standardGeneric("fromUnitToUnit"));
setClass('unitGeneric', representation = list(value = 'numeric'), prototype = list(value = as.numeric(NA)));
setMethod('initialize', 'unitGeneric', function(.Object, value = as.numeric(NA)) {
	.Object@value = value;
	.Object
});
setMethod('fromUnitToUnit', c('unitGeneric', 'unitGeneric'), function(thisA, thisB)
	new(class(thisB), value = thisA@value * factorToBase(thisA) / factorToBase(thisB)));

setClass('unitCm', contains = 'unitGeneric');
setMethod('initialize', 'unitCm', function(.Object, value)callNextMethod(.Object, value = value));
setMethod('factorToBase', 'unitCm', function(this)1);

setClass('unitInch', contains = 'unitGeneric');
setMethod('initialize', 'unitInch', function(.Object, value)callNextMethod(.Object, value = value));
setMethod('factorToBase', 'unitInch', function(this)cm(1));

setClass('unitDpi150', contains = 'unitGeneric');
setMethod('initialize', 'unitDpi150', function(.Object, value)callNextMethod(.Object, value = value));
setMethod('factorToBase', 'unitDpi150', function(this)cm(1)/150);

setClass('unitDpi200', contains = 'unitGeneric');
setMethod('initialize', 'unitDpi200', function(.Object, value)callNextMethod(.Object, value = value));
setMethod('factorToBase', 'unitDpi200', function(this)cm(1)/200);

setClass('unitDpi300', contains = 'unitGeneric');
setMethod('initialize', 'unitDpi300', function(.Object, value)callNextMethod(.Object, value = value));
setMethod('factorToBase', 'unitDpi300', function(this)cm(1)/300);

setClass('unitPoints', contains = 'unitGeneric');
setMethod('initialize', 'unitPoints', function(.Object, value)callNextMethod(.Object, value = value));
setMethod('factorToBase', 'unitPoints', function(this)cm(1)/72);

valueU = valueUnited = function(value, unit) {
	class = getClass(Sprintf('unit%{unit}u'));
	new(class, value = value)
}
toUnit = function(value, unit)fromUnitToUnit(value, valueU(as.numeric(NA), unit));
ToUnit = function(value, unit)toUnit(value, unit)@value;

#
#	</p> unit model
#

cm2in = function(i) (i/2.54)

plotPoints = function(f=sin, interval=c(0,1), count = 1e2, steps = NULL, ...) {
	if (!is.null(steps))
		count = as.integer((interval[2] - interval[1]) / steps) else
		steps = (interval[2] - interval[1]) / (count + 1);
	xs = c(interval[1] + (0:(count - 1)) * steps, interval[2]);
	#ys = apply(t(xs), 2, function(x)(f(x)));
	#ys = Vectorize(function(x)f(x, ...))(xs);
	ys = Vectorize(function(x)f(x))(xs);
	data.frame(x = xs, y = ys)
}

plotRobust = function(f=sin, interval=c(0,1), count = 1e2, steps = NULL, points = F, ...) {
	pts = plotPoints(f, interval, count, steps, points, ...);
	if (points) {
		points(pts$x, pts$y, type="l");
	} else {
		plot(pts$x, pts$y, type="l");
	}
}

robustPlot = function(f=sin, interval=c(0,1), steps = 0.05, points = F, ...) {
	plotRobust(f, interval, steps = steps, points = points, ...);
}

#
# <p> vector functions
#

vNorm = function(v)sqrt(sum(v^2))
vToNorm = toNorm = function(v) {
	l = vNorm(v);
	if (l == 0) NA else v/l
}

# orthogonal vector in 2D
vPerp = function(v)rev(v) * c(-1, 1)

# the normal of a vector (in 2D), i.e. the perpendicular unit vector
vNormal = function(v)vToNorm(vPerp(v))

#
#	<p> graph drawing
#

# draw wedges
# x: x-coordinates
# y: y-coordinates
# w: widthes
wedge = function(x0, y0 = NULL, x1 = NULL, y1 = NULL, width = NULL, col = "black", ..., defaultWidth = .1) {
	d = if (!is.null(y0)) data.frame(x0, y0, x1, y1) else x0;
	if (is.null(width)) width = matrix(defaultWidth, ncol = 2, nrow = dim(x0)[1]);

	pts = matrix(sapply(1:dim(d)[1], function(i) {
		p1 = d[i, c("x0", "y0")];
		p2 = d[i, c("x1", "y1")];
		w = width[i, ];

		n = vNormal(p2 - p1); # normal of line
		c(p1 + n * w[1]/2, p1 - n * w[1]/2, p2 - n * w[2]/2, p2 + n * w[2]/2)
	}), ncol = 2, byrow = T);
	grid.polygon(x = pts[, 1], y = pts[, 2], id.lengths = rep(4, dim(d)[1]), gp = gpar(fill=1, col = col))
}

#
#	<p> ggplot2
#

#library('ggplot2');

qplotFaceted = function(f, from = 0, to = 1, data, facets, geom = 'line', ..., by = 0.02) {
	qplot.call = match.call(qplot);
	vars = formula.vars(facets);
	varLevels = unique(data[, vars, drop = F]);
	print(varLevels);
	xs = seq(from, to, by = by);
	r = apply(varLevels, 1, function(r) {
		environment(f) = f.env = new.env(parent = environment(f));
		fl = as.list(r);
		for (n in names(fl)) assign(n, fl[[n]], envir = f.env);
		ys = f(xs);
		d = data.frame(x = xs, y = ys, fl);
		d
	});
	d = rbindDataFrames(r);
	qplotArgs = c(as.list(qplot.call[-1]));
	p = qplot(x, y, data = d, facets = facets, geom = geom, ...);
	p
}

#
#	plot to file
#

plot_file_DefaultOptions = list(width = 12, height = 12, dpi = 200);

plot_file = function(code_or_object, file = NULL, options = list(), ..., envir = parent.frame()) {
	call = sys.call()[[2]];
	if (is.null(file)) file = tempFileName('plot_file', 'pdf', inRtmp = T);
	p = if (any(class(code_or_object) == 'ggplot')) {
		o = merge.lists(plot_file_DefaultOptions, options, list(...));
		with(o, { ggsave(code_or_object, file = file, width = width, height = height, dpi = dpi) });
		code_or_object
	} else {
		device = get(splitPath(file)$ext);
		device(file, ...);
			eval(call, envir = envir);
		dev.off();
		encapsulateCall(call, envir__ = envir);
	}
	p
}

#
#	<p> special plots
#

ggplot_qqunif = function(p.values, alpha = .05, fontsize = 6,
	tr = function(x)-log(x, 10), trName = '-log10(P-value)', colorCI = "#000099") {

	# <p> preparation
	if (any(p.values < 0 | p.values > 1)) stop("P.values not in interval (0, 1)");
	p.values = tr(sort(na.omit(p.values)));

	N = length(p.values);
	Ns = 1:N;
	# j-th order statistic from a uniform(0,1) sample has beta(j,n-j+1) distribution
	# (Casella & Berger, 2002, 2nd edition, pg 230, Duxbury)
	ciU = tr(qbeta(1 - alpha/2, Ns, N - Ns + 1));
	ciL = tr(qbeta(    alpha/2, Ns, N - Ns + 1));
	d = data.frame(theoretical = tr(Ns/N), ciU = ciU, ciL = ciL, p.value = p.values, colorCI = colorCI);
	p = ggplot(d) +
		geom_line(aes(x = theoretical, y = ciU, colour = colorCI)) +
		geom_line(aes(x = theoretical, y = ciL, colour = colorCI)) +
		geom_point(aes(x = theoretical, y = p.value), size = 1) +
		theme_bw() + theme(legend.position = 'none') + coord_cartesian(ylim = c(0, max(p.values)*1.1)) +
		scale_y_continuous(name = trName) +
		theme(text = element_text(size = fontsize));
	p
}

QQunifPlotDf = function(p.values, alpha = .05, tr = function(P)-log10(P), trName = Deparse(body(tr)),
	bins = c(2e2, 2e2), Nrep = 10, p.bottom = 1e-300, na.rm = T) {

	# <p> filter
	Logs('#p-values being NA: %{Nna}d', Nna = sum(is.na(p.values)), logLevel = 4);
	if (na.rm) p.values = p.values[!is.na(p.values)];
	Logs('#p-values < %{p.bottom}.1e: %{Nceil}d', Nceil = sum(p.values < p.bottom), logLevel = 4);
	p.values = ifelse(p.values < p.bottom, p.bottom, p.values);

	# <p> data frame
	if (any(p.values < 0 | p.values > 1)) stop("P.values not in interval (0, 1)");
	o = order(p.values)
	p.values = tr(p.values[o]);

	# <p> data frame
	N = length(p.values);
	d = data.frame(theoretical = tr((1:N)/N), p.value = p.values);

	# <p> binning
	Ns = if (!is.null(bins)) binPlot(d, bins = bins, Nrep = Nrep, returnIdcs = T) else Ns = 1:N;
	d = d[Ns, ];

	# j-th order statistic from a uniform(0,1) sample has beta(j,n-j+1) distribution
	# (Casella & Berger, 2002, 2nd edition, pg 230, Duxbury)
	dPlot = data.frame(d,
		ciU = tr(qbeta(1 - alpha/2, Ns, N - Ns + 1)), ciL = tr(qbeta(alpha/2, Ns, N - Ns + 1)));
	dPlot
}

#ggplot_qqunif(seq(1e-2, 3e-2, length.out = 1e2))
QQunif = Qqunif = function(p.values, alpha = .05, fontsize = 12,
	tr = function(P)-log10(P), trName = Deparse(body(tr)), colorCI = "#000099",
	bins = c(2e2, 2e2), Nrep = 10, p.bottom = 0) {

	dPlot = QQunifPlotDf(p.values, alpha, tr, trName, bins, Nrep, p.bottom, na.rm = T);
	dPlot = Df(dPlot, colorCI = colorCI);

	p = ggplot(dPlot) +
		geom_line(aes(x = theoretical, y = ciU, colour = colorCI)) +
		geom_line(aes(x = theoretical, y = ciL, colour = colorCI)) +
		geom_point(aes(x = theoretical, y = p.value), size = 1) +
		theme_bw() + theme(legend.position = 'none') + coord_cartesian(ylim = c(0, max(dPlot$p.value)*1.1)) +
		scale_y_continuous(name = trName) +
		theme(text = element_text(size = fontsize));

	p
}

cbPalette1 = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7");
cbPalette2 = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7");
QQunifDfPalette = c(cbPalette1, cbPalette2);

QQunifDf = function(f1, data, alpha = .05, fontsize = 12,
	tr = function(P)-log10(P), trName = Deparse(body(tr)), trI = function(Ptr)exp(Ptr * log(10)),
	colorCI = "#000099", bins = c(2e2, 2e2), Nrep = 10, p.bottom = 0, byCutoff = 1, legendTitle = 'Test') {
	# <p> variable names
	p.value = formula.response(f1);

	cats0 = factorFromFormula(data, formula.rhs(f1));
	plotDfs = by(data, cats0, function(d0)
		QQunifPlotDf(d0[[p.value]], alpha, tr, trName, bins, Nrep, p.bottom, na.rm = T));
	#color = recodeLevels(as.character(cats0), levels = c('odd', 'even'));
	dPlot = do.call(rbind, nlapply(plotDfs, function(n)Df(name = n, plotDfs[[n]])));
	dPlot = dPlot[!is.na(dPlot[[p.value]]),, drop = F];
	dPlot = dPlot[order(dPlot[[p.value]]), ];
	myBreaks = function()cats0;	# work around for ggplot2 behavior of lexically scoping to data set only
	palette = c(colorCI, QQunifDfPalette)[1:(length(cats0) + 1)];

	# <p> data frame CI
	pRangeTh = range(dPlot$theoretical);
	N = ceiling(trI(pRangeTh[2]));	# <!> inverse of tr
	dCi = data.frame(
		theoretical = tr((1:N - .5)/N),
		ciU = tr(qbeta(1 - alpha/2, 1:N, N - (1:N) + 1)), ciL = tr(qbeta(alpha/2, 1:N, N - (1:N) + 1))
	);

	p = ggplot(dPlot) +
		geom_point(aes(x = theoretical, y = p.value, colour = name), size = 1) +
		scale_color_manual(name = get('legendTitle'), breaks = get('cats0'), values = get('palette')) + 
		#theme_bw() + theme(legend.position = 'none') + coord_cartesian(ylim = c(0, max(dPlot$p.value)*1.1)) +
		theme_bw() + coord_cartesian(ylim = c(0, max(dPlot$p.value)*1.1)) +
		scale_y_continuous(name = trName) +
		theme(text = element_text(size = fontsize)) +
		# CI
		geom_line(data = dCi, aes(x = theoretical, y = ciU, colour = colorCI)) +
		geom_line(data = dCi, aes(x = theoretical, y = ciL, colour = colorCI))

	p
}


vp_at = function(x, y)viewport(layout.pos.row = x, layout.pos.col = y);
plot_grid_grid = function(plots, coords) {
	# <p> do plotting
	grid.newpage();
	# <!> layout might not be respected
	nrow = max(coords[, 1]);
	ncol = max(coords[, 2]);
	pushViewport(viewport(layout = grid.layout(nrow, ncol)));

	sapply(1:length(plots), function(i) {
		print(plots[[i]], vp = vp_at(coords[i, 1], coords[i, 2]));
	});
}

plot_error = function(msg, errN = 10) {
	plot(1:errN, type = 'n', xlab = '', ylab = '', axes = FALSE, frame.plot = FALSE);
	text(errN/2, errN/2, msg);
}

plot_grid_base = function(plots, coords, envirPlotVar = 'plot', verbose = F, errN = 10) {
	# <p> do plotting
	coordMat0 = matrix(0, nrow = max(coords[, 1]), ncol = max(coords[, 2]));
	coordMat = matrix.assign(coordMat0, coords, 1:prod(dim(coordMat0)));
	layout(coordMat);

	sapply(1:length(plots), function(i) {
		if (verbose) LogS(2, 'plot_grid#: %{i}d');
		this = try({
		cl = class(plots[[i]]);
			if ('environment' %in% cl) eval(get(envirPlotVar, plots[[i]])) else
			if ('function' %in% cl) do.call(plots[[i]], list()) else
			eval(plots[[i]])
		});
		if ('try-error' %in% class(this)) plot_error(this)
	});
# 			if (is.environment(plots[[i]])) eval(get(envirPlotVar, plots[[i]])) else print(plots[[i]]);
}

# plot_grid(list(
# 	quote(plot(t0, col = d$sex)),
# 	quote(plot(pca0$x[, 1:2], col = d$sex)),
# 	function() {
# 		plot(1:10, type = 'n', xlab = '', ylab = '');
# 		text(5, 5, join(c(Sprintf('Family: %{myFid}d'), pedPed), "\n")) }
# ), nrow = 2);
# plot_save(plts, file = .fn('tsne-pca', 'pdf'));


plot_grid = function(plots, nrow, ncol, byrow = T, mapper = NULL, envirPlotVar = 'plot', verbose = F) {
	if (missing(nrow)) {
		if (missing(ncol)) {
			ncol = 1;
			nrow = length(plots);
		} else {
			nrow = ceiling(length(plots) / ncol);
		}
	} else if (missing(ncol)) ncol = ceiling(length(plots) / nrow);

	coords = if (is.null(mapper))
		merge.multi(1:nrow, 1:ncol, .first.constant = byrow) else
		mapper(1:length(plots));

	cl = class(plots[[1]]);
	if ('viewport' %in% cl) plot_grid_grid(plots, coords) else
	plot_grid_base(plots, coords, envirPlotVar, verbose = verbose)
}

plot_grid_to_path =  function(plots, ..., path,
	width = valueU(21, 'cm'), height = valueU(29.7, 'cm'), NperPage = NULL, pdfOptions = list(paper = 'a4'),
	verbose = F) {

	if (class(width) == 'numeric') width = valueU(width, 'inch');
	if (class(height) == 'numeric') height = valueU(height, 'inch');
	Nplots = length(plots);

	pages = if (!is.null(NperPage)) {
		Npages = ceiling(Nplots / NperPage);
		lapply(1:Npages, function(i) {
			Istrt = (i - 1) * NperPage + 1;
			Istop = min(i * NperPage, Nplots);
			Istrt:Istop
		})
	} else list(1:length(plots));

	pdfArgs = c(list(
		file = path, onefile = TRUE, width = ToUnit(width, 'inch'), height = ToUnit(height, 'inch')
	), pdfOptions);
	do.call(pdf, pdfArgs);

	ilapply(pages, function(plotIdcs, i) {
		if (verbose) LogS(2, 'Plotting page: %{i}d');
		plot_grid(plots[plotIdcs], ..., verbose = verbose);
	});

	dev.off();
}

plot_adjacent = function(fts, factor, N = ncol(fts)) {
	ns = names(fts);
	ps = lapply(1:(N - 1), function(i){
		x = eval({fts[, i]});
		y = eval({fts[, i + 1]});
		qplot(x, y, color = as.factor(factor), xlab = ns[i], ylab = ns[i + 1]);
	});
}

plot_grid_pdf = function(plots, file, nrow, ncol, NperPage, byrow = T, mapper = NULL,
	pdfOptions = list(paper = 'a4'), verbose = F) {
	Nplots = length(plots);
	if (missing(nrow)) nrow = NperPage / ncol;
	if (missing(ncol)) ncol = NperPage / nrow;
	if (missing(NperPage)) NperPage = ncol * nrow;
	Npages = ceiling(Nplots / NperPage);

	do.call(pdf, c(list(file = file), pdfOptions));
	sapply(1:Npages, function(i) {
		if (verbose) LogS(2, 'Plotting page: %{i}d');
		Istrt = (i - 1) * NperPage + 1;
		Istop = min(i * NperPage, Nplots);
		plot_grid(plots[Istrt:Istop], nrow, ncol, byrow = byrow, mapper = mapper, verbose = verbose);
	});
	dev.off();
}

#
#	<p> Kaplan-Meier with ggplot
#

# stolen from the internet
createSurvivalFrame <- function(f.survfit){
	# initialise frame variable
	f.frame <- NULL
	# check if more then one strata
	if(length(names(f.survfit$strata)) == 0){
		# create data.frame with data from survﬁt
		f.frame <- data.frame(time=f.survfit$time, n.risk=f.survfit$n.risk, n.event=f.survfit$n.event,
			n.censor = f.survfit$n.censor, surv=f.survfit$surv, upper=f.survfit$upper, lower=f.survfit$lower)
		# create ﬁrst two rows (start at 1)
		f.start <- data.frame(time=c(0, f.frame$time[1]), n.risk=c(f.survfit$n, f.survfit$n), n.event=c(0,0),
		n.censor=c(0,0), surv=c(1,1), upper=c(1,1), lower=c(1,1))
		# add ﬁrst row to dataset
		f.frame <- rbind(f.start, f.frame)
		# remove temporary data
		rm(f.start)
	} else {
		# create vector for strata identiﬁcation
		f.strata <- NULL
		for(f.i in 1:length(f.survfit$strata)){
			# add vector for one strata according to number of rows of strata
			f.strata <- c(f.strata, rep(names(f.survfit$strata)[f.i], f.survfit$strata[f.i]))
		}
		# create data.frame with data from survﬁt (create column for strata)
		f.frame <- data.frame(time=f.survfit$time, n.risk=f.survfit$n.risk, n.event=f.survfit$n.event, n.censor = f.survfit
		$n.censor, surv=f.survfit$surv, upper=f.survfit$upper, lower=f.survfit$lower, strata=factor(f.strata))
		# remove temporary data
		rm(f.strata)
		# create ﬁrst two rows (start at 1) for each strata
		for(f.i in 1:length(f.survfit$strata)){
			# take only subset for this strata from data
			f.subset <- subset(f.frame, strata==names(f.survfit$strata)[f.i])
			f.start <- data.frame(time=c(0, f.subset$time[1]), n.risk=rep(f.survfit[f.i]$n, 2), n.event=c(0,0), n.censor=c(0,0), surv=c(1,1), upper=c(1,1), lower=c(1,1), strata=rep(names(f.survfit$strata)[f.i], 2))	
			# add ﬁrst two rows to dataset
			f.frame <- rbind(f.start, f.frame)
			# remove temporary data
			rm(f.start, f.subset)
		}
		# reorder data
		f.frame <- f.frame[order(f.frame$strata, f.frame$time), ]
		# rename row.names
		rownames(f.frame) <- NULL
	}
	# return frame
	return(f.frame)
}

# deﬁne custom function to draw kaplan-meier curve with ggplot
qplot_survival = function(f.frame, f.CI = "default", f.shape = 3, ..., title = NULL, layers = NULL,
	axes = NULL, legendTitle = T){

	strata = avu(Regexpr('=(.*)', levels(f.frame$strata), captures = T));
	factorNm = avu(Regexpr('(.*)=', levels(f.frame$strata)[1], captures = T));
	levels(f.frame$strata) = strata;

	# use different plotting commands dependig whether or not strata's are given
	p = if("strata" %in% names(f.frame) == FALSE) {
		# conﬁdence intervals are drawn if not speciﬁed otherwise
		if(f.CI == "default" | f.CI == TRUE ){
			# create plot with 4 layers (ﬁrst 3 layers only events, last layer only censored)
			# hint: censoring data for multiple censoring events at timepoint are overplotted
			# (unlike in plot.survﬁt in survival package)
			ggplot(data=f.frame, ...) +
			geom_step(aes(x=time, y=surv), direction="hv") +
			geom_step(aes(x=time, y=upper), directions="hv", linetype=2) +
			geom_step(aes(x=time,y=lower), direction="hv", linetype=2) +
			geom_point(data=subset(f.frame, n.censor==1), aes(x=time, y=surv), shape=f.shape)
		} else {
			# create plot without conﬁdence intervalls
			ggplot(data=f.frame) +
			geom_step(aes(x=time, y=surv), direction="hv") +
			geom_point(data=subset(f.frame, n.censor==1), aes(x=time, y=surv), shape=f.shape)
		}
	} else {
		# without CI
		if(f.CI == "default" | f.CI == FALSE){
			ggplot(data=f.frame, aes(group=strata, colour=strata), ...) +
			geom_step(aes(x=time, y=surv), direction="hv") +
			geom_point(data=subset(f.frame, n.censor==1), aes(x=time, y=surv), shape=f.shape)
		} else {
			ggplot(data=f.frame, aes(colour=strata, group=strata), ...) +
			geom_step(aes(x=time, y=surv), direction="hv") +
			geom_step(aes(x=time, y=upper), directions="hv", linetype=2, alpha=0.5) +
			geom_step(aes(x=time,y=lower), direction="hv", linetype=2, alpha=0.5) +
			geom_point(data=subset(f.frame, n.censor==1), aes(x=time, y=surv), shape=f.shape)
		}
	}
	if (!is.null(title)) p = p + labs(title = title);
	if (Nif(axes) && Nif(axes$x)) p = p + xlab(axes$x);
	if (Nif(axes) && Nif(axes$y)) p = p + ylab(axes$y);
	if (!is.null(layers)) p = p + layers;
	#p = p + theme(legend.title = if (legendTitle) factorNm else element_blank());
	legendNm = if (legendTitle) factorNm else NULL;
	p = p + scale_colour_discrete(name = legendNm, breaks = strata, labels = strata)
	p
}

quantileBinning = function(x, Nbins) {
	cut(x, quantile(x, seq(0, 1, length = Nbins + 1)), labels = seq_len(Nbins), include.lowest = TRUE)
}

kaplanMeierStrat = function(d1, f1, levels = NULL,
	title = '%{formula}s, [P = %{p}.2e]', axes = NULL, legendTitle = T) {
	# <i> only allow one covariate
	titlePre = stratVar = all.vars(formula.rhs(f1))[1];
	titleFormula = Sprintf('%{O}s ~ %{titlePre}s', O = all.vars(as.formula(f1))[1])
	if (!is.null(levels)) {
		d1[[stratVar]] = as.factor(quantileBinning(d1[[stratVar]], levels));
	}
	stratValue = levels(droplevels(d1[[stratVar]]));
	# <p> log-rank test
	lr = survdiff(as.formula(f1), data = d1);
	p.lr = pchisq(lr$chisq, df = dim(lr$n) - 1, lower.tail = F)
	# <p> kaplan-meyer
	fit = survfit(as.formula(f1), data = d1);
	fit.frame = createSurvivalFrame(fit);
	titleCooked = Sprintf(title, strata = titlePre, p = p.lr, formula = titleFormula)
	p = qplot_survival(fit.frame, F, 20, title = titleCooked, axes = axes, legendTitle = legendTitle,
		layers = theme_bw());
	list(plot = p, level = stratValue)
}

kaplanMeierNested = function(d, f1, strata, combine = FALSE) {
	Require('gdata');
	# <!><b> fix special case of length(strata) == 1
	d = Df(d, dummy_comb__kaplanMeierNested = 1);
	dStrat = d[, c(strata, 'dummy_comb__kaplanMeierNested'), drop = F];
	cbs = Df_(valueCombinations(dStrat), min_ = 'dummy_comb__kaplanMeierNested');
	dStrat = Df_(dStrat, min_ = 'dummy_comb__kaplanMeierNested');

	plots = apply(cbs, 1, function(r) {
		sel1 = nif(apply(dStrat == r, 1, all));
		sel = nif(sapply(1:nrow(d), function(i)all(dStrat[i, ] == r)));
		if (sum(sel) == 0) {
			warning('empty group selected');
			return(NULL);
		}
		#if (sum(sel) == 0) return(NULL);
		dSel = d[sel, , drop = F];
		N = sum(sel);
		title = Sprintf('Stratum: %{stratum}s, N = %{N}d',
			stratum = paste(names(r), r, sep = '=', collapse = ', '));
		kaplanMeierStrat(dSel, f1, title = title);
	});
	plots
}

# groups levels by group argument
kaplanMeierCovariate = function(covariate, outcome, groups = NA, data,
	title = '%{strata}s, [P = %{p}.2e]', axes = NULL, legendTitle = T) {
	pred = if (!nif(groups)) data[[covariate]] else recodeLevels(data[[covariate]], group = groups);
	LogS(5, 'Number of levels: %{N}d', N = length(levels(droplevels(pred))));
	#if (length(levels(droplevels(pred))) < 2) browser();
	d = Df(pred, data[, avu(outcome), drop = F], names = covariate);
	f1 = with(outcome, Sprintf('Surv(%{time}s, %{status}s) ~ %{covariate}s'))
	p = kaplanMeierStrat(d, f1, title = title, axes = axes, legendTitle = legendTitle);
	return(p);
}


#
#	<p> histograms
#


histogram_colors = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7");
histogram_colors = c('red', 'blue', 'green', 'yellow');
#dayColor = list(`0` = 'red', `1` = 'blue', `3` = 'green', `8` = 'yellow');

histogram_overlayed = function(data, f1,
	groupNames = levels(groups), palette = histogram_colors, log10 = T,
	x_lab = formula.response(f1), title = 'histogram', alpha = .3, breaks = 30) {

	# <p> column names, range
	xn = formula.response(f1);
	gn = formula.covariates(f1);
	lvls = levels(data[[gn]]);
	tab = table(cut(data[[xn]], breaks));
	#mx = if (log10) 10^ceiling(log10(max(tab))) else max(tab);
	mx = max(tab);

	# <p>  create legend using pseudo data (shifted out of view)
	dp = Df(x = rep(0, length(lvls)), y = rep(mx + 1, length(lvls)), group = lvls);
	p = ggplot(dp, aes(x = x)) +
		geom_rect(data = dp, aes(xmin = x, xmax = x + 1, ymin = y, ymax = y + 1, fill = group)) +
		scale_fill_manual(name = gn, values = palette);

	# <p> histograms
	for (i in 1:length(lvls)) {
		p = p + geom_histogram(data = data.frame(x = data[[xn]][data[[gn]] == lvls[i]]),
			fill = palette[i], alpha = alpha);
	}

	# <p> log transform
	if (log10) p = p + scale_y_continuous(trans = 'log10') + coord_cartesian(ylim = c(1, mx));

	# <p> final formatting
	p = p + ggtitle(title) + xlab(x_lab);
	p

}

#'@param data:	data frame or list
histograms_alpha = function(data, palette = histogram_colors, log10 = F,
	x_lab = '', title = 'histogram', alpha = .3, origin = NULL, binwidth = NULL, relative = FALSE,
	textsize = 20) {
	# <p> preparation
	N = length(as.list(data));
	columns = names(data);
	mx = max(unlist(as.list(data)), na.rm = T);
	mn = min(unlist(as.list(data)), na.rm = T);

	# <p>  create legend using pseudo data (shifted out of view)
	dp = Df(x = rep(2*mx + 2, N), y = rep(0, N), group = columns);
	p = ggplot(dp, aes(x = x)) +
		geom_rect(data = dp, aes(xmin = x, xmax = x + .01, ymin = y, ymax = y + .01, fill = group)) +
		scale_fill_manual(name = dp$group, values = palette);

	# <p> histograms
	for (i in 1:N) {
		col = columns[i];
		dfH = data.frame(x = data[[col]]);
		p = p + if (relative)
			geom_histogram(data = dfH, aes(y=..count../sum(..count..)),
				fill = palette[i], alpha = alpha, binwidth = binwidth, origin = origin
			) else
			geom_histogram(data = dfH, fill = palette[i], alpha = alpha, binwidth = binwidth, origin = origin)
	}

	# <p> log transform
	if (log10) p = p + scale_y_continuous(trans = 'log10') + coord_cartesian(ylim = c(1, mx));

	# <p> final formatting
	p = p + coord_cartesian(xlim = c(mn - 1, mx + 1)) + ggtitle(title) + xlab(x_lab) + theme_bw() +
		theme(text = element_text(size = textsize));
	if (relative) p = p + ylab('percentage');
	p

}

#
#	<p> saving of plots
#

# autoscale scales canvas sizes to make scaling uniform between devices
#	increases the pixel resolution for pixel devices
deviceTypes = list(
	jpeg = list(hasDpi = TRUE, autoScale = 1),
	png = list(hasDpi = TRUE, autoScale = 1),
	pdf = list(forceUnit = 'inch', autoScale = 1)
);

Device = function(type, plot_path, width, height, ..., units = 'cm', dpi = NA, autoScale = FALSE) {
	device = get(type);
	o = deviceTypes[[type]];
	if (!is.null(o$forceUnit)) {
		width = toUnit(width, o$forceUnit);
		height = toUnit(height, o$forceUnit);
	}
	width = width@value;
	height = height@value;
	if (autoScale && !is.null(o$autoScale)) {
		width = width * o$autoScale;
		height = height * o$autoScale;
	}
	args = c(list(plot_path, width = width, height = height), list(...));
	if (nif(o$hasDpi)) do.call(device, c(args, list(units = units, res = dpi))) else do.call(device, args);
		#device(plot_path, width = width, height = height, units = units, res = dpi, ...) else
		#device(plot_path, width = width, height = height, ...);
}

# prepare plotting expression for later plotting
quotePlot = function(e, envir = parent.frame()) {
	list(plot = substitute(e), envir = envir)
}

# use 'cm' as output unit
# if unit_out is from a class beginning with unitDpi, produce a resolution argument correpsonding
#	to that many dpi

plot_draw = function(object, envir) {
	ret = if (any(class(object) == 'grob')) {
		grid.draw(object)
	} else if (any(class(object) %in% c('ggplot', 'plot'))) {
		print(object)
	} else if (any(class(object) %in% c('histogram'))) {
		plot(object)
	} else if (any(class(object) %in% 'list') && notE(plot$envir)) {
		eval(object$plot, envir = plot$envir);
	} else if (any(class(object) == 'function')) {
		do.call(object, list(), envir = envir);
	} else {
		eval(object, envir = envir);
	}
}

plot_save_raw = function(object, ..., width = 20, height = 20, plot_path = NULL,
	type = NULL, options = list(), unit = 'cm', unit_out = NULL, autoScale = FALSE, envir = parent.frame()) {

	if (is.null(unit_out)) unit_out = units_default[[type]];
	#width = toUnit(width, unit_out)@value;
	#height = toUnit(height, unit_out)@value;
	width = toUnit(width, 'cm');
	height = toUnit(height, 'cm');
	dpi = Nina(as.integer(FetchRegexpr('dpi(\\d+)', unit_out, captures = T)));
 	Logs('Saving %{type}s to "%{plot_path}s"  [width: %{w}f %{h}f dpi: %{dpi}d]',
		w = width@value, h = height@value, logLevel = 5);
	Device(type, plot_path, width = width, height = height, units = 'cm', dpi = dpi, ...,
		autoScale = autoScale);
		#ret = eval(object, envir = envir);
# 		ret = if (any(class(object) == 'grob')) {
# 			grid.draw(object)
# 		} else if (any(class(object) %in% c('ggplot', 'plot'))) {
# 			print(object)
# 		} else {
# 			eval(object, envir = envir);
# 		}
		ret = plot_draw(object, envir = envir);
	dev.off();
}
# <i> refactor with plot_save_raw
plot_save_eval = function(object, ..., width = 20, height = 20, plot_path = NULL,
	type = NULL, options = list(), unit = 'cm', unit_out = 'dpi150', autoScale = FALSE, envir = parent.frame()) {

	if (is.null(type)) type = splitPath(plot_path)$ext;
	if (is.null(unit_out)) unit_out = units_default[[type]];
	if (class(width) == 'numeric') width = valueU(width, 'cm');
	if (class(height) == 'numeric') height = valueU(height, 'cm');
	width = toUnit(width, 'cm');
	height = toUnit(height, 'cm');
	dpi = Nina(as.integer(FetchRegexpr('dpi(\\d+)', unit_out, captures = T)));
 	Logs('Saving %{type}s to "%{plot_path}s"  [width: %{w}f %{h}f dpi: %{dpi}d]',
		w = width@value, h = height@value, logLevel = 5);
	Device(type, plot_path, width = width, height = height, units = 'cm', dpi = dpi, ...,
		autoScale = autoScale);
		ret = eval(object, envir = envir);
	dev.off();
	return(ret);
}


plotSizes = list(
	a4 = list(width = valueU(21, 'cm'), height = valueU(21 * sqrt(2), 'cm')),
	a4Sq = list(width = valueU(21, 'cm'), height = valueU(21, 'cm')),
	a4R = list(width = valueU(21*sqrt(2), 'cm'), height = valueU(21, 'cm')),
	a4_15 = list(width = valueU(1.5*21, 'cm'), height = valueU(1.5 * 21 * sqrt(2), 'cm')),
	a4R_15 = list(width = valueU(1.5*21*sqrt(2), 'cm'), height = valueU(1.5 * 21, 'cm'))
);
#
#	Examples:
#	plot_save(object, c('a.jpeg', 'a.png'), options = list(jpeg = list(unit_out = 'dpi300')));
#	plot_save(quote(plot(res)), unit_out = 'cm', plot_path = 'resources/Q3-residuals.png',
#		width = 15, height = 15, options = list(png = list(unit_out = 'dpi150')));
#	plot_save(quote(plot(res)), plot_path = 'resources/Q3-residuals.png', width = 15, height = 15);
#		plot_save(\(.)plot(mtcars[, 1:2]), plot_path = 'results/mtcars.jpg')
plot_typeMap = list(jpg = 'jpeg');
plot_optionsDefault = list(
	png = list(unit_out = 'dpi150'),
	jpeg = list(unit_out = 'dpi150')
);
plot_save = function(object, ..., size, width, height, plot_path = NULL,
	#type = firstDef(plot_typeMap[[splitPath(plot_path)$ext]], splitPath(plot_path)$ext),
	type = NULL,
	envir = parent.frame(), options = plot_optionsDefault,
	simplify = T, unit_out = NULL, createDir = TRUE) {

	# <p> plot size
	# default size
	if (missing(size) && (missing(width) || missing(height))) size = 'a4R';
	if (!missing(size)) {
		width = plotSizes[[size]]$width;
		height = plotSizes[[size]]$height;
	}
	if (class(width) == 'numeric') width = valueU(width, 'cm');
	if (class(height) == 'numeric') height = valueU(height, 'cm');

	if (is.null(plot_path)) file = tempFileName('plot_save', 'pdf', inRtmp = T);
	ret = lapply(plot_path, function(plot_path) {
		if (createDir) Dir.create(plot_path, recursive = T, treatPathAsFile = T);
		if (is.null(type) && !is.null(plot_path)) {
			ext = splitPath(plot_path)$ext;
			type = firstDef(plot_typeMap[[ext]], ext);
		}
		uo = firstDef(unit_out, options[[type]]$unit_out, units_default[[type]]);
		Logs(con("plot_path: %{plot_path}s, device: %{type}s, unit_out: %{uo}s "), logLevel = 5);
		args = merge.lists(list(object),
			list(type = type, plot_path = plot_path,
				width = width, height = height, unit_out = uo,
				options = options, envir = envir),
			list(...)
		);
		do.call(plot_save_raw, args);
	});
	if (length(plot_path) == 1 && simplify) ret = ret[[1]];
	r = list(path = plot_path, ret = ret);
	r
}

# USAGE:
# plts = exprR1$Eapply(function(data, probe_name) {
# 	delayedPlot({
# 		boxplot(model, data, main = main);
# 		beeswarm(model, data, add = T)
# 	})
# });
# eval(plts[[1]])

delayedPlot = function(plotExpr, envir = parent.frame()) {
	e = new.env(parent = envir);
	delayedAssign('plot', plotExpr, assign.env = e)
	e
}

#
#	<p> legacy function from other packages
#

# gridExtra
ebimageGrob = function (pic, x = 0.5, y = 0.5, scale = 1, raster = FALSE, angle = NULL, ...) {
    dims <- dim(pic)
    colours = t(channel(pic, "x11"))
    width = unit(scale * dims[1], "points")
    height = unit(scale * dims[2], "points")
    angle <- if (is.null(angle)) 
        0
    else angle
    vp <- viewport(x = x, y = y, width = width, height = height, 
        angle = angle)
    if (raster) {
        child <- rasterGrob(colours, vp = vp, ...)
    }
    else {
        colours <- colours[rev(seq_len(nrow(colours))), ]
        Require('RGraphics')
        child <- imageGrob(dims[2], dims[1], cols = colours, 
            gp = gpar(col = colours), byrow = FALSE, vp = vp, 
            ...)
    }
    gTree(width = width[[1]], height = height[[1]], children = gList(child), 
        cl = "ebimage")
}

#
#	<p> transformations
#

# <p> helper functions

# apply transformations on coordinates (given as row-wise points of a nx2 matrix)
applyT = function(coords, transf)t(transf %*% t(cbind(coords, 1)))[, -3]
# rectangle of width/height
rectWH = function(width, height)
	t(c(width, height) * (c(-.5, -.5) + t(matrix(c(0,0, 1,0, 1,1, 0,1), byrow = T, ncol = 2))))
# caculate circumference radius of rectangle
rectRad = function(width, aspectRatio, margin) {
	width = width * (1 + margin);
	height = width/aspectRatio;
	radius = norm(matrix(c(width, height)), '2') / 2;
}

# <p> actual transformations
# transformations are applied right to left (see applyT)

transform2dTranslate = function(delta) {
	matrix(c(1, 0, 0, 0, 1, 0, delta[1], delta[2], 1), ncol = 3)
}

transform2dRotation = function(alpha) {
	matrix(c(cos(alpha), sin(alpha), 0, -sin(alpha), cos(alpha), 0, 0, 0, 1), ncol = 3)
}

# tranlation after rotation
transform2dRotTrans = function(alpha, delta) {
	transform2dTranslate(delta) %*% transform2dRotation(alpha)
}

# move in direction of rotation after rotation
transform2dMoveYRot = function(dist, alpha) {
	transl = transform2dTranslate(c(0, dist));
	rot = transform2dRotation(alpha);
	r = rot %*% transl;
	r
}

# move in direction of rotation after rotation
transform2dRotMove = transform2dRotMoveY = function(alpha, dist) {
	rot = transform2dRotation(alpha);
	v = rot %*% c(dist, 0, 1);
	transform2dTranslate(v[1:2]) %*% rot
}


# create transformation that is created by rotating a points
#	the destination of the point is used to create a translation
transform2dRot2Move = function(alpha, point) {
	p = applyT(matrix(point, ncol = 2), transform2dRotation(alpha));
	transform2dTranslate(p);
}

dfTop = function(d, N)d[1:min(nrow(d), N),, drop = F]
vTop = function(v, N)v[1:min(length(v), N)]

#
#	<p> binning
#

# partition range of data into bins according to bins and choose Nrep representatives for each bin
binPlot = function(data, formula = NULL, bins = c(1e2, 1e2), Nrep = 3, eps = 1e-5, permute = TRUE,
	plotRange = NULL, returnIdcs = FALSE) {
	cols = if (is.null(formula)) 1:2 else match(all.vars(formula), dimnames(data)[[2]]);
	rangeX = if (is.null(plotRange$rangeX)) range(data[, cols[1]]) else plotRange$rangeX;
	rangeY = if (is.null(plotRange$rangeX)) range(data[, cols[2]]) else plotRange$rangeY;
	# bin number defined by numbering in 1st quadrant within range, row-first
	# i.e. (0, 0): origin, bins[1]: (0, 1), ...
	RX = (rangeX[2] - rangeX[1] + eps);
	RY = (rangeY[2] - rangeY[1] + eps);
	bin = as.vector(apply(data[, cols], 1, function(e) {
		binX = as.integer(floor(bins[1] * ((e[1] - rangeX[1]) / RX)));
		binY = as.integer(floor(bins[2] * ((e[2] - rangeY[1]) / RY)));
		bin = binY * bins[1] + binX;
		bin
	}));

	# <p> select data
	idcs = unlist(lapply(unique(bin), function(b) {
		idcs = which(bin == b)
		if (permute) idcs = Sample(idcs);
		vTop(idcs, Nrep)
	}));
	r = if (returnIdcs) idcs else data[idcs, , drop = F];
	r
}

binPlot_1 = function(data, formula = NULL, bins = c(1e2, 1e2), Nrep = 3, eps = 1e-5, permute = TRUE,
	plotRange = NULL, returnIdcs = FALSE) {
	cols = if (is.null(formula)) 1:2 else match(all.vars(formula), dimnames(data)[[2]]);
	rangeX = if (is.null(plotRange$rangeX)) range(data[, cols[1]]) else plotRange$rangeX;
	rangeY = if (is.null(plotRange$rangeX)) range(data[, cols[2]]) else plotRange$rangeY;
	# bin number defined by numbering in 1st quadrant within range, row-first
	# i.e. (0, 0): origin, bins[1]: (0, 1), ...
	RX = (rangeX[2] - rangeX[1] + eps);
	RY = (rangeY[2] - rangeY[1] + eps);
	bin = as.vector(apply(data[, cols], 1, function(e) {
		binX = as.integer(floor(bins[1] * ((e[1] - rangeX[1]) / RX)));
		binY = as.integer(floor(bins[2] * ((e[2] - rangeY[1]) / RY)));
		bin = binY * bins[1] + binX;
		bin
	}));

	# <p> select data
	dataSL = lapply(unique(bin), function(b) {
		d0 = data[which(bin == b), , drop = F];
		d1 = if (permute) d0[sample.int(nrow(d0)), , drop = F] else d0;
		dfTop(d1, Nrep)
	});
	dataS = do.call(rbind, dataSL);
	dataS
}
binPlot_0 = function(data, formula = NULL, bins = c(1e2, 1e2), Nrep = 3, eps = 1e-5, permute = TRUE) {
	cols = if (is.null(formula)) 1:2 else match(all.vars(formula), dimnames(data)[[2]]);
	rangeX = range(data[, cols[1]]);
	rangeY = range(data[, cols[2]]);
	# bin number defined by numbering in 1st quadrant within range, row-first
	# i.e. (0, 0): origin, bins[1]: (0, 1), ...
	bin = as.vector(apply(data[, cols], 1, function(e) {
		binX = as.integer(floor(bins[1] * ((e[1] - rangeX[1]) / (rangeX[2] - rangeX[1] + eps))));
		binY = as.integer(floor(bins[2] * ((e[2] - rangeY[1]) / (rangeY[2] - rangeY[1] + eps))));
		bin = binY * bins[1] + binX;
		bin
	}));
	data1 = cbind(data, bin);
	if (permute) data1 = data1[sample.int(nrow(data1)), ];

	# <p> ordered data
	# <A> assume order to be stable
	dataO = data1[order(data1[, 'bin']), ];

	# <p> selected data
	binsU = unique(dataO[, 'bin']);
	dataDf = as.data.frame(dataO);
	dataSL = lapply(binsU, function(b)subsetTop(dataO, with(dataDf, bin == b), Nrep));
	dataS = do.call(rbind, dataSL);
	dataS
}

#
#	<p> label annotation
#

# patch[XY]: multiply by constants to add a shift
labelAlignmentsStd = list(
	std = list(x = 0.5, y = 0, patchX = 0, patchY = 1, hjust = 0.5, vjust = 0, penalty = 0),
	leftL = list(x = 0, y = -1, patchX = -1, patchY = 0, hjust = 1, vjust = 1, penalty = 5),
	rightL = list(x = 1, y = -1, patchX = 1, patchY = 0, hjust = 0, vjust = 1, penalty = 5),
	left = list(x = 0, y = 0, patchX = 0, patchY = 1, hjust = 1, vjust = 0, penalty = 5),
	right = list(x = 1, y = 0, patchX = 0, patchY = 1, hjust = 0, vjust = 0, penalty = 5),
	high = list(x = 0.5, y = 1, patchX = 0, patchY = 1, hjust = 0.5, vjust = 0, penalty = 15)
);
layoutStd = list(extend = list(x = 20, y = 2, patchX = .1, patchY = .1, size = 1));

layoutForExtend = function(x = 50, y = 50, labelSzX = x/10, labelSzY = y/25) {
	Mx = max(x, y);
	return(list(extend = list(x = labelSzX, y = labelSzY, patchX = x/100, patchY = y/100, size = Mx / 15)));
}

labelBoundingBox = function(l, layout = layoutStd, alignments = labelAlignmentsStd) {
	a = FirstDef(alignments[[l$align]], alignments$std);
	e = FirstDef(l$extend, layout$extend);
	p = l$pos;
	s = layout$extend;

	x1 = p$x + (a$x - 1) * e$x + a$patchX * s$patchX;
	y1 = p$y + a$y * e$y + a$patchY * s$patchY;
	coords = matrix(c(x1, y1, x1 + e$x, y1, x1 + e$x, y1 + e$y, x1, y1+e$y), byrow = T, ncol = 2);
	return(coords);
}
labelsDf = function(labels, layout = layoutStd, alignments = labelAlignmentsStd) {
	rows = lapply(labels, function(l) {
		e = FirstDef(l$extend, layout$extend);
		a = FirstDef(alignments[[l$align]], alignments$std);
		with(l$pos, Df(x = x + a$patchX * e$patchX, y = y + a$patchY * e$patchY,
			annotation = l$annotation, hjust = a$hjust, vjust = a$vjust));
	});
	d = do.call(rbind, rows);
	return(d);
}

# close polygon, convert to SpatialPolygon, cartesian coordinates
labelBB2Spatial = function(coords) {
	poly = rbind(coords, coords[1, , drop = F]);
	return(SpatialPolygons(list(Polygons(list(Polygon(poly)), 'BB')), proj4string = CRS('+proj=cart')));
}

labelIntersections = function(bb, bbList) {
	lSpat = labelBB2Spatial(bb);
	diffs = sapply(bbList, function(e)area(intersect(lSpat, labelBB2Spatial(e))));
	return(diffs);
}

Area = function(p)if (is.null(p)) return(0) else area(p);
Intersect = function(p1, p2)suppressWarnings(intersect(p1, p2));
labelIntersectionsWeighted = function(bb, bbListW) {
	weights = list.kpu(bbListW, 'weight');
	lSpat = labelBB2Spatial(bb);
	diffs = sapply(bbListW, function(e)Area(Intersect(lSpat, labelBB2Spatial(e$coords))));
	return(diffs * weights);
}

# minmize overlap with labels in lList
labelFindBestAlignment = function(l, lList, alignments = labelAlignmentsStd, layout = layoutStd) {
	bbList = lapply(lList, labelBoundingBox, layout = layout);
	bbListL = list.embed(bbList, 'coords');
	weights = list.embed(list.kpu(lList, 'weight'), 'weight');
	bbListW = list.combine(list(bbListL, weights), doMerge = T);

	overlap = sapply(names(alignments), function(a) {
		bb = labelBoundingBox(merge.lists(l, list(align = a)), layout = layout);
		inter = labelIntersectionsWeighted(bb, bbListW);
		sum(inter);
	});
	o = overlap[which.min(overlap)];
	return(list(align = names(o), overlap = o));
}

labelsRearrangeSingle = function(labels, layout = layoutStd, alignments = labelAlignmentsStd) {
	overlap = 0;
	penalty = 0;
	for (i in 1:length(labels)) {
		a = labelFindBestAlignment(labels[[i]], labels[-i], layout = layout);
		labels[[i]]$align = a$align;
		overlap = overlap + a$overlap;
		penalty = penalty + alignments[[a$align]]$penalty;
	}
	return(list(labels = labels, overlap = overlap, penalty = penalty));
}

labelsRearrangeRaw = function(labels, layout = layoutStd, NiterMax = 2e2) {
	overlap = Inf;
	for (i in 1:NiterMax) {
		lsRearr = labelsRearrangeSingle(labels, layout = layout);
		if (lsRearr$overlap == overlap) break;
		overlap = lsRearr$overlap;
		labels = lsRearr$labels;
	}
	return(lsRearr);
}

labelsRearrange = function(labels, layout = layoutStd, Nperm = 3) {
	arrangements = lapply(1:Nperm, function(i) {
		labelsRearrangeRaw(if (i > 1) labels[Sample(1:length(labels))] else labels, layout = layout);
	});
	Iperm = which.min(list.kpu(arrangements, 'overlap'));
	LogS(3, "labelsRearrange: permuation with minimal overlap: %{Iperm}d");
	return(arrangements[[Iperm]]);
}

plotLabels = function(labels, layout = layoutStd) {
	dPts = Df_(do.call(rbind, list.kp(labels, 'pos')), deep_simplify = T);
	p = ggplot(data = dPts, aes(x = x, y = y)) + geom_point(size = 1) + theme_bw();
	dL = labelsDf(labels, layout = layout);
	print(dL);
	p = p + annotate('text', x = dL$x, y = dL$y, label = dL$annotation,
		hjust = dL$hjust, vjust = dL$vjust, size = lo$extend$size
	);

}

#
#	<p> longitudinal data
#

Aes = function(...)structure(list(...),  class = "uneval")
plotSpaghetti = function(f1 = y ~ group, data, fSpaghetti = ~ time + cluster) {
	vs = c(all.vars(f1), all.vars(fSpaghetti));
	#dPairs = completeData(vs, data, collapse = T);
	data = droplevels(completeData(vs, data, collapse = T));

	time = all.vars(fSpaghetti)[1];
	cluster = all.vars(fSpaghetti)[2];

	response = formula.response(f1);
	group = all.vars(f1)[2];
	pSp = ggplot(data, Aes(x = data[[time]], y = data[[response]], group = data[[cluster]])) +
		geom_line(aes(color = data[[group]])) +
		xlab(time) + ylab(response) + theme_bw(); #theme(legend.position = "none");
	return(pSp);
}

#
#	<p> Boxplot
#

Boxplot = function(data, formula, path = NULL, box_width = .2, dot_dotzise = 1) {
	x = formula.covariates(formula);
	y = formula.response(formula);
	p = ggplot(data, aes_string(x = x, y = y)) + geom_boxplot(width = box_width) +
		geom_violin(trim = FALSE, fill = NA) +
		geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = dot_dotzise) +
		theme_bw();
	if (notE(path)) ggsave(path, p)
	return(p);
}

#
#	<p> Contour plot
#

Contour = function(grid, fContour, plot_path = NULL, ..., width = 15, height = 15) {
	Z = outer(grid[[1]], grid[[2]], FUN = fContour);
	p = plot_save_eval(
		contour(grid[[1]], grid[[2]], z = Z)
	, plot_path = plot_path, ..., width = width, height = height);
	return(p);
}
#
#	Rreporting.R
#Mon 06 Feb 2006 11:41:43 AM EST

#
#	<p> documentation (by example
#

# Example:
# 	create a Reporter instance to report to LaTeX
# r = new("Rreporter", final.path = "/tmp/output.pdf", patterns = "latex");

#
#	</p> end documentation
#

#
#	<p> generic reporting functions
#

formatNumber = function(e, digits) {
	ifelse(-floor(log10(abs(e))) >= digits,
		sprintf("%.*e", digits, e),
		sprintf("%.*f", digits, e))
}

row.standardFormatter = function(e, digits = NA) {
	f = if (is.na(digits) || substring(digits, 1, 1) == 'p') {
		e
	} else {
		e = as.numeric(e);
		if (substring(digits, 1, 1) == "#") {
			sprintf("%.*e", as.numeric(substring(digits, 2)), e)
		} else if (substring(digits, 1, 1) == "%") {
			sprintf('%.*f\\%%', as.numeric(substring(digits, 2)), e * 100)
		} else if (as.numeric(digits) < 0) {
			digits = as.integer(digits);
			ifelse(floor(log10(abs(e))) <= digits,
				sprintf("%.*g", -digits, e),
				sprintf("%.*f", -digits, e))
		} else { sprintf("%.*f", as.integer(digits), e) }
	}
	f
}

latex = list(
	# table patterns
	header = "{LONGTABLESTARTFMT\\begin{longtable}{COLUMN_FORMAT}\nLONGTABLECAPTION",
	columnNames = "%s%s %s\\hline\n",
	separator = " & ",
	hline = "\\hline\n",
	lineEnd = " \\\\\n",
	subHeading = function(h, rowSpan)
		sprintf("\\hline\n & \\multicolumn{%d}{l}{\\bf %s}\\\\\\hline\n", rowSpan, h),
	footer = "\\end{longtable}}\n\n",

	postProcess = function(s, df, row.formatters, digits, caption, na.value, subHeadings,
		ignoreRowNames, patterns, alignment, startFmt, bars) {
		if (is.null(alignment)) alignment = rep(NA, dim(df)[2]);
		alignment[is.na(alignment) & !is.na(digits)] = 'r';
		alignment[is.na(alignment)] = 'l';
		paragraphs = !is.na(digits) & substring(digits, 1, 1) == 'p';
		alignment[paragraphs] = digits[paragraphs];
		bars = c(bars, rep(F, length(alignment) - length(bars)));
		alignment = ifelse(!bars, alignment, paste(alignment, '|', sep = ''));
		colFmt = sprintf("%s%s", ifelse(ignoreRowNames, "", "r|"),
			paste(alignment, collapse = ""));
		captionPt = if (caption == '') list(LONGTABLECAPTION = '') else
			list(LONGTABLECAPTION = '\\caption{CAPTION}\\\\\n', CAPTION = caption)
		s = mergeDictToString(merge.lists(
			list(COLUMN_FORMAT = colFmt, LONGTABLESTARTFMT = startFmt),
			captionPt), s);
		s
	},
	quote = function(s, detectFormula = T) {
		s = gsub('_', '\\\\_', s, perl = T);
		s = gsub('#', '\\\\#', s, perl = T);
		s = gsub('&', '\\\\&', s, perl = T);
		s = gsub('~', '$\\\\sim$', s, perl = T);
		s = gsub('([<>])', '$\\1$', s, perl = T);
		s = gsub('\\^2', '$^2$', s, perl = T);
		#ifelse(length(grep('_', s)) > 0, gsub('_', '\\\\_', s, perl = T), s)
		s
	},

	# general text formatting
	newpage = "\n\n\\newpage\n\n",
	section = "\\section{SECTION_NAME}\n\n",
	subsection = "\\subsection{SECTION_NAME}\n\n",
	paragraph = "PARAGRAPH_TEXT\\par\n\n",

	# finalize
	document = "HEADER\n\\begin{document}\nDOC_HERE\n\\end{document}\n",
	docHeader = "\\documentclass[a4paper,oneside,11pt]{article}\n\\usepackage{setspace,amsmath,amssymb, amsthm, epsfig, epsf, amssymb, amsfonts, latexsym, rotating, longtable, setspace, natbib, a4wide,verbatim, caption}\n\\usepackage[utf8x]{inputenc}",
	docCmd = "cd TMP_DIR ; pdflatex TMP_FILE_BASE 1&>/dev/null ; cp TMP_FILE_BASE.pdf OUTPUT_FILE",

	# figure table
	figureTable = list(
		table = "\\begin{center}\\begin{tabular}{COLS}\nROWS\\end{tabular}\\end{center}",
		figure = '\\includegraphics[width=%.3f\\textwidth]{%s}',
		figureCaption = "\\begin{minipage}[b]{%.3f\\linewidth}\\centering
		\\begin{tabular}{c}
			%s\\\\
			\\includegraphics[width=\\textwidth]{%s}
		\\end{tabular}\\end{minipage}\n",
		formatTable = function(rows, cols = 2, template = latex$figureTable$table) {
			mergeDictToString(list(COLS = join(rep('c', cols), ''), ROWS = rows), template)
		},
		formatRows = function(rows, cols = 2) {
			sep = c(rep(' & ', cols - 1), "\\\\\n");
			seps = rep(sep, (length(rows) + cols - 1) %/% cols);
			seps = seps[1:length(rows)];
			rs = meshVectors(rows, seps);
			r = join(c(pop(rs), "\n"), '');
#browser();
#			texRows = sapply(1:(length(rows) - 1), function(i)sprintf('%s%s', rows[i],
#				ifelse(i %% cols == 1, ' & ', "\\\\\n")));
#			rs = join(c(texRows, rev(rows)[1], "\n"), '');
#			rs
		},
		formatFigure = function(figure, cols = 2, width = 1/cols - 0.05,
			template = latex$figureTable$figure, templateCaption = latex$figureTable$figureCaption,
			caption = '') {
			if (File.exists(figure)) figure = path.absolute(figure);
			caption = if (firstDef(caption, '') != '')
				sprintf(templateCaption, width, caption, figure) else
				sprintf(template, width, figure)
		}
	)
);

# bars: parallel structure to digits: where to insert vertical bars
report.data.frame.toString = function(df = NULL,
	row.formatters = c(row.standardFormatter), digits = NA, caption = "", na.value = "-",
	subHeadings = NULL, ignoreRowNames = F, patterns = latex, names.as = NULL, alignment = NULL,
	quoteHeader = T, quoteRows = T, quoteRowNames = quoteHeader, startFmt = '', bars = NULL) {
	with(patterns, {
	# <p> initialize
	rFmtC = length(row.formatters);
	if (length(digits) == 1) digits = rep(digits, dim(df)[2]);
	t = header;	# the nascent table as string
	if (!is.null(names.as)) names(df) = names.as;

	# <p> complete header
	header = if (quoteHeader) sapply(dimnames(df)[[2]], quote) else dimnames(df)[[2]];
	t = con(t, sprintf("%s%s%s%s", ifelse(!ignoreRowNames, separator, ""),
		paste(header, collapse = separator), lineEnd, hline));

	# <p> append rows
	for (i in Seq(1, nrow(df))) {
		row.fmt = row.formatters[[((i - 1) %% rFmtC) + 1]];

		if (i %in% subHeadings$indeces) {	# insert subheading
			j = which(subHeadings$indeces == i);
			t = con(t, subHeading(subHeadings$headings[j], dim(df)[2] - ignoreRowNames));
		}
		if (!ignoreRowNames) {
			rowName = dimnames(df)[[1]][i];
			t = con(t, sprintf("%s%s", if (quoteRowNames) quote(rowName) else rowName, separator));
		}
		# <p> formatting and quoting
		values = sapply(1:ncol(df), function(j)
			if (is.na(df[i, j])) na.value else row.fmt(as.character(df[i, j]), digits[j])
		);
		if (quoteRows) values = sapply(values, quote);
		t = con(t, sprintf("%s%s", paste(values, collapse = separator), lineEnd));
	}

	t = con(t, footer);
	t = postProcess(t, df, row.formatters, digits, caption, na.value, subHeadings,
		ignoreRowNames, patterns, alignment, startFmt, bars);
	})
}

report.figure.tableSingle = function(figures, cols = 2, width = 1/cols - 0.05, patterns = latex, captions = NULL)
	with(patterns, with(figureTable, {

	figs = sapply(1:length(figures), function(i){
		formatFigure(figures[i], cols = cols, width = width, caption = captions[i])
	});
	rows = formatRows(figs, cols = cols);
	table = formatTable(rows, cols = cols);
	table
}))
report.figure.table = function(figures, cols = 2, width = 1/cols - 0.05, patterns = latex,
	captions = NULL, maxRows = 5) with(patterns, {
	NfiguresPerPage = maxRows * cols;
	Nfigures = ceiling(ceiling(length(figures)/cols) / maxRows);
	if (Nfigures > 1) {
		tables = sapply(1:Nfigures, function(i) {
			Is = ((i - 1)*NfiguresPerPage + 1): min((i*NfiguresPerPage), length(figures));
			report.figure.tableSingle(figures[Is], cols, width, patterns, captions[Is])
		});
		join(tables, "\n")
	} else report.figure.tableSingle(figures, cols, width, patterns, captions)
})


#
#	<p> Rreporter (base on S4 methods)
#

setClass("Rreporter",
	representation(tmp.path = "character", final.path = "character", patterns = "list"),
	prototype(tmp.path = sprintf("%s.rr", tempfile()), final.path = NULL, patterns = latex)
);

setMethod("initialize", "Rreporter", function(.Object, final.path, patterns = latex) {
	.Object@final.path = final.path;
	.Object@patterns = if (is.character(patterns)) get(patterns) else patterns;
	# create temp file
	cat("", file = .Object@tmp.path);
	.Object
});

# <p> generic methods

report.data.frame = function(self, df = NULL, row.formatters = c(row.standardFormatter),
	digits = NA, caption = "", na.value = "-", subHeadings = NULL, ignoreRowNames = F, verbose = T) {
	patterns = self@patterns;
	s = report.data.frame.toString(df, row.formatters , digits, caption, na.value,
		subHeadings, ignoreRowNames, patterns);
	cat(s, file = self@tmp.path, append = T);
	if (verbose) cat(s);
	self
}

report.newpage = function(self) {
	cat(self@patterns$newpage, file = self@tmp.path, append = T);
}
report.newsection = function(self, name) {
	cat(
		mergeDictToString(list(SECTION_NAME = name), self@patterns$section),
		file = self@tmp.path, append = T
	);
}
report.newsubsection = function(self, name) {
	cat(
		mergeDictToString(list(SECTION_NAME = name), self@patterns$subsection),
		file = self@tmp.path, append = T
	);
}
report.paragraph = function(self, text) {
	cat(
		mergeDictToString(list(PARAGRAPH_TEXT = text), self@patterns$paragraph),
		file = self@tmp.path, append = T
	);
}

report.finalize = finalize = function(self) {
	cmd = sprintf("cp \"%s\" \"%s\"", self@tmp.path, absolutePath(self@final.path));
	System(cmd);
}

report.finalizeAsDocument = function(self) {
	# <p> read document to string
	doc = readFile(self@tmp.path);
	# <p> write processed document
	sp = splitPath(self@tmp.path);
	writeFile(sprintf("%s.tex", sp$fullbase),
		mergeDictToString(list(
			HEADER = self@patterns$docHeader, DOC_HERE = readFile(self@tmp.path)
		), self@patterns$document)
	);
	cmd = mergeDictToString(
		list(
			TMP_DIR = sp$dir,
			TMP_FILE = sp$path,
			TMP_FILE_BASE = sp$fullbase,
			OUTPUT_FILE = absolutePath(self@final.path)
		)
	, self@patterns$docCmd)
	System(cmd);
}

#
#	<p> end Rreporter (base on S4 methods)
#

#
#	<p> convenience methods
#

reportDataFrame2pdf = function(df, file = tempfile(), row.formatters = c(row.standardFormatter),
	digits = NA, caption = "", na.value = "-", subHeadings = NULL, ignoreRowNames = F, verbose = T) {
	r = new("Rreporter", final.path = file);
	report.data.frame(r, df,
		row.formatters, digits, caption, na.value, subHeadings, ignoreRowNames, verbose);
	report.finalizeAsDocument(r);
}

#
#	<p> sweave
#

swaeveIt = function(file = NULL, N = 1) {
	System(sprintf("R CMD Sweave '%s.Rnw'", file));
	cmd = sprintf("sh -c 'pdflatex \"./%s\"'", file);
	for (i in 1:N) System(cmd);
}

#
#	<p> Sweave replacement
#

.REP.standardTemplate = '\\input{SETUP}
\\begin{document}
TEMPLATE_MAIN
\\end{document}
';

# REP.plot('Tag', Qplot(rate, geom = 'histogram', xlab = 'heterocygosity', file = 'dest'));
# REP.plot('Tag', Qplot(sample = ps, dist = qunif, file = 'results/qc-markers-hweQQ.jpg'));

Qplot_defaults = list(
	width = 5, height = 5, dpi = 150,
	dimx = c(0, 1), dimy = c(0, 100)
);

Qplot = function(..., file = NULL, pp = Qplot_defaults, theme = theme_bw()) {
	pp = merge.lists(Qplot_defaults, pp);
	args = list(...);
	geom = firstDef(args$geom, 'default');
	# <b> workaround for QQ-plot instead of the expected qplot(...)
	p = if (any(class(args[[1]]) == 'ggplot')) {
		plot = args[[1]];
		
	} else if (
		# histogram
		(all(is.na(args[[1]])) && geom == 'histogram')
		# xy-plot
		|| (all(is.na(args[[1]]) | is.na(args[[2]])))) {
		ggplot(data = data.frame()) + geom_point() +
			xlim(pp$dimx[1], pp$dimx[2]) +
			ylim(pp$dimy[1], pp$dimx[2]);
	} else do.call(qplot, list(...));
	ggsave(p, file = file, width = pp$width, height = pp$height, dpi = pp$dpi);
	file
}
GGplot = function(p, file = NULL, pp = list(width = 5, height = 5, dpi = 150)) {
	ggsave(p, file = file, width = pp$width, height = pp$height, dpi = pp$dpi, encoding = 'AdobeStd');
	file
}
PlotDefaults = list(
	pdf = list(width = 6, height = 6),
	jpeg = list(width = 2048, height = 2048)
);
Plot = function(..., file = NULL, .plotType = 'pdf', o = NULL, f = NULL) {
	if (is.null(file)) file = tempFileName('reporter', .plotType);
	device = get(.plotType);
	plotFunction = firstDef(f, plot);
	o = merge.lists(PlotDefaults[[.plotType]], o);
	do.call(device, c(list(file = file), o));
		do.call(plotFunction, list(...));
	dev.off();
	file
}

.REP.extractFromTemplates = function(templates, re = '(?s)(?<=TEMPLATE_BEGIN).*?(?=TEMPLATE_END)',
	locations = c('.', sprintf('%s/src/Rscripts', Sys.getenv('HOME')))) {
	nst = names(templates);

	# <p> set empty template names
	if (is.null(nst)) nst = rep('', length(templates));
	nst[nst == ''] = paste('TEMPL_', 1:sum(nst == ''), sep = '');

	# <p> parse template definitions
	ts = lapply(1:length(templates), function(i) {
		# raw read templates
		templ = readFile(templates[[i]], prefixes = locations);
		tsRaw = fetchRegexpr(re, templ);
		# inline templates
		r = if (length(tsRaw) != 0) {
			ns = sapplyn(tsRaw, function(e)fetchRegexpr('(?<=^:).*?(?=\\n)', e, globally = F));
			# colon, new-line
			ts = sapply(1:length(ns), function(i)substr(tsRaw[i], nchar(ns[i]) + 3, nchar(tsRaw[i])));
			listKeyValue(ns, ts);
		} else {
			listKeyValue(nst[i], templ);
		}
		r
	});
	#r = unlist.n(ts, 1);
	r = merge.lists(ts, listOfLists = T);
	r
}

.REP.getTemplates = function(templates, locations = c('.', sprintf('%s/src/Rscripts', Sys.getenv('HOME')))) {
	nst = names(templates);

	# <p> set empty template names
	if (is.null(nst)) nst = rep('', length(templates));
	nst[nst == ''] = paste('TEMPL_', 1:sum(nst == ''), sep = '');

	# <p> parse template definitions
	ts = lapply(1:length(templates), function(i) {
		# raw read templates
		templ = readFile(templates[[i]], prefixes = locations);
		tsRaw = fetchRegexpr('(?s)(?<=TEMPLATE_BEGIN).*?(?=TEMPLATE_END)', templ);
		# inline templates
		r = if (length(tsRaw) != 0) {
			ns = sapplyn(tsRaw, function(e)fetchRegexpr('(?<=^:).*?(?=\\n)', e, globally = F));
			# colon, new-line
			ts = sapply(1:length(ns), function(i)substr(tsRaw[i], nchar(ns[i]) + 3, nchar(tsRaw[i])));
			listKeyValue(ns, ts);
		} else {
			listKeyValue(nst[i], templ);
		}
		r
	});
	#r = unlist.n(ts, 1);
	r = merge.lists(ts, listOfLists = T);
	# backward compatibility: determine wether default template should be used
	if (length(r) > 0) {
		if (names(r)[1] != 'TEMPL_1') {	# expect full document template tb specified otherwise
			# interpolate first template into standard template
			r[[1]] = mergeDictToString(list(TEMPLATE_MAIN = r[[1]]), .REP.standardTemplate);
		}
	}
	r
}

.REP.getPatterns = function(templates) {
	.REP.extractFromTemplates(templates, '(?s)(?<=KEY_BEGIN).*?(?=KEY_END)');
}

.REP.defaultParameters = list(
	copy.files = c(),
	setup = 'setup.tex',
	latex = 'pdflatex',
	useDefaultTemplate = T
);
# create new, global reporter
REP.new = function(templates = NULL, cache = NULL, parameters = list(), resetCache = F,
	latex = 'pdflatex', setup = 'setup.tex') {
	copy.files = merge.lists(.REP.defaultParameters['copy.files'],
		list(copy.files = setup), list(copy.files = parameters$copy.files), concat = TRUE);
	parameters = merge.lists(.REP.defaultParameters,
		parameters,
		list(latex = latex, setup = setup),
		copy.files,
	concat = FALSE);
	if (!is.null(cache) && file.exists(cache) && !resetCache) {
		REP.tex('SETUP', setup);
		REP.setParameters(parameters);
		load(file = cache, envir = .GlobalEnv);
	} else {
		templatePathes = c(as.list(templates), parameters$subTemplates);
		ts = .REP.getTemplates(templatePathes);
		ps = merge.lists(
			list(SETUP = setup),
			.REP.getPatterns(templatePathes)
		);
		mainPath = splitPath(as.vector(templates)[1]);
		assign('.REPORTER.ITEMS', list(
			# list of named templates
			templates = ts,
			# patterns to be interpolated
			patterns = ps,
			# housekeeping: tags for consecutively reported subtemplates
			templateTags = list(),
			# parameters passed in
			parameters = parameters,
			# path to the cache file
			cache = cache,
			# create default output name
			output = sprintf('%s.pdf', mainPath$fullbase),
			# name of the template to be used for the global, final document
			mainTemplate = names(ts)[1],
			templatePathes = templatePathes,
			# conditionals
			conditionals = list()
			), pos = .GlobalEnv
		);
	}
	NULL
}
REP.refreshTemplates = function(templates) {
	if (!exists('.REPORTER.ITEMS')) return();
	templatePathes = templates;
	ts = .REP.getTemplates(as.list(templates));
	ps = .REP.getPatterns(templatePathes);
	.REPORTER.ITEMS$templates = ts;
	.REPORTER.ITEMS$mainTemplate = names(ts)[1];
	.REPORTER.ITEMS$templatePathes = templatePathes;
	.REPORTER.ITEMS$patterns = merge.lists(.REPORTER.ITEMS$patterns, ps);
	assign('.REPORTER.ITEMS', .REPORTER.ITEMS, pos = .GlobalEnv);
	REP.save();
}
REP.save = function() {
	if (!is.null(.REPORTER.ITEMS$cache) && length(.REPORTER.ITEMS$cache) > 0) {
		dir = splitPath(.REPORTER.ITEMS$cache)$dir;
		if (!file.exists(dir)) dir.create(dir, recursive = T);
		save(.REPORTER.ITEMS, file = .REPORTER.ITEMS$cache);
	}
	NULL
}

REP.setParameters = function(parameters = .REP.defaultParameters) {
	.REPORTER.ITEMS$parameters = merge.lists(.REP.defaultParameters, parameters);
	assign('.REPORTER.ITEMS', .REPORTER.ITEMS, pos = .GlobalEnv);
	REP.save();
}
REP.unreport = function(keys) {
	l = get('.REPORTER.ITEMS', pos = .GlobalEnv);
	idcs = which.indeces(keys, names(l$patterns));
	if (!length(idcs)) return(NULL);
	l$patterns = l$patterns[-idcs];
	assign('.REPORTER.ITEMS', l, pos = .GlobalEnv);
	REP.save();
}
setREPentry = function(key, value) {
	if (!exists('.REPORTER.ITEMS')) assign('.REPORTER.ITEMS', list(), pos = .GlobalEnv);
	l = get('.REPORTER.ITEMS', pos = .GlobalEnv);
	l$patterns[[key]] = value;
	assign('.REPORTER.ITEMS', l, pos = .GlobalEnv);
	REP.save();
}
setRI = function(ri)assign('.REPORTER.ITEMS', ri, pos = .GlobalEnv);

REP.setConditional = function(name, v) {
	l = get('.REPORTER.ITEMS', pos = .GlobalEnv);
	if (is.null(l$conditionals)) l$conditionals = list();
	l$conditionals[[name]] = v;
	assign('.REPORTER.ITEMS', l, pos = .GlobalEnv);
	REP.save();
}

REP.get = function(name) {
	get('.REPORTER.ITEMS', envir = .GlobalEnv)$patterns[[name]];
}

codeForFunction = function(name) {
	str = join(as.character(attr(get(name), 'srcref')), "\n");
	str = gsub("\t", "    ", str);
	str
}
REP.function = function(name) {
	REP.tex(uc.first(name), Sprintf('%{name}s = %{code}s', code = codeForFunction(name)));
}

outputOf = function(code, print = T, envir = parent.frame()) {
	tempFile = tempFileName('reporter', inRtmp = T);
	sink(tempFile);
		if (print) print(eval(code, envir = envir)) else eval(code, envir = envir);
	sink();
	output = readFile(tempFile);
	output
}
expression2str = function(exp, removeBraces = T) {
	strs = deparse(exp);
	if (removeBraces) strs = strs[2:(length(strs) - 1)];
	sprintf("%s\n", join(strs, "\n"))
}

codeRepresentation = function(code) {
	if (is.character(code)) {
		codeExp = parse(text = code);
		codeText = gsub('^\n?(.*)', '\\1', code);	# remove leading \n
	} else {
		codeExp = code;
		codeText = expression2str(code);
	}
	r = list(code = codeExp, text = codeText);
	r
}

REP.format.sci = function(s, digits = 1) {
	e = floor(log10(as.numeric(s)));
	m = as.numeric(s) * 10^(-e);
	if (round(m, digits) == 1) {
		sprintf("$10^{%d}$", e)
	} else {
		sprintf("$%.*f \\times 10^{%d}$", digits, m, e)
	}
}

REP.formats = list(
	small = function(s)sprintf("{\n\\small %s\n}", s),
	tiny = function(s)sprintf("{\n\\tiny %s\n}", s),
 	percent = function(s)sprintf("%.1f", 100 * as.numeric(s)),
  	`.1` = function(s)sprintf("%.1f", as.numeric(s)), 
	`.2` = function(s)sprintf("%.2f", as.numeric(s)), 
 	`.3` = function(s)sprintf("%.3f", as.numeric(s)), 
 	`.4` = function(s)sprintf("%.4f", as.numeric(s)), 
 	sci0 = function(s) REP.format.sci(s, 0), 
 	sci1 = function(s) REP.format.sci(s, 1), 
 	sci2 = function(s) REP.format.sci(s, 2), 
	file = function(f) {
		ri = .REPORTER.ITEMS;
		# due to caching choose a persistent location <!> uniqueness
		tdir = sprintf('/tmp/%s/Rpreporting/%s', Sys.getenv('USER'), names(ri$templates)[1]);
		if (!file.exists(tdir)) dir.create(tdir, recursive = T);
		tf = sprintf('%s/%s', tdir, splitPath(f)$file);
		unlink(tf);	# overwrite previous version
		# <!> expect relative filename, spaces in file name not eliminated
		file.symlink(sprintf('%s/%s', getwd(), f), tf);
		tf
	}
);

REP.tex = function(name, str, print = T, quote = F, fmt = NULL) {
	if (!is.null(fmt) && !is.na(fmt)) {
		str = if (is.null(REP.formats[[fmt]])) sprintf(fmt, str) else REP.formats[[fmt]](str);
	}
	if (quote) {	#<i> use backend quoting
		#str = gsub('_', '\\\\_', str, perl = T);	# replace _
		str = latex$quote(str);
	}
	setREPentry(sprintf('%s', name), str);
	str
}
REP.texq = function(name, str, print = T, quote = T, fmt = NULL)REP.tex(name, str, print, quote, fmt)
REP.vector = function(name, v, print = T, quote = T, typewriter = T, sep = ', ', max = 50) {
	if (max > 0) v = v[1:min(max, length(v))];
	if (typewriter) {
		v = sapply(v, function(s)sprintf('\\texttt{%s}', s));
	}
	REP.tex(name, sprintf('%s%s', join(v, sep), ifelse(length(v) > max, '...', '')), quote = quote);
}

REP = function(name, code, print = T, execute = T, envir = parent.frame()) {
	c = codeRepresentation(as.list(sys.call())[[3]]);
	setREPentry(sprintf('%s_code', name), c$text);
	if (execute) {
		output = outputOf(c$code, envir = envir);
		setREPentry(sprintf('%s_out', name), output);
		if (print) cat(output);
	}
	NULL
}
REP.plotDefaultOptions = list(width = 5, height = 5, dpi = 150);
REP.plot = function(name, code, ..., file = NULL, type = 'pdf', envir = parent.frame(),
	options = list(), copyToTmp = F) {
	#c = codeRepresentation(as.list(sys.call())[[3]]);
	c = codeRepresentation(sys.call()[[3]]);	# as of version R 3.0.1
	if (is.null(file)) file = tempFileName('reporter', 'pdf', inRtmp = T, retries = 5e2);
	if (type == 'ggplot') {
		o = merge.lists(REP.plotDefaultOptions, options, list(...));
		with(o, { ggsave(code, file = file, width = width, height = height, dpi = dpi) });
	} else if (is.character(code)) {
		file = code;
	} else {
		device = get(type);
		device(file, ...);
			eval(c$code, envir = envir);
		dev.off();
	}
	pathToFile = path.absolute(file);
	if (copyToTmp || notE(grep(' ', pathToFile))) {	# spaces in filenames not possible /w latex
		fileTmp = tempFileName('reporter', splitPath(pathToFile)$ext, inRtmp = T, retries = 5e2);
		file.copy(pathToFile, fileTmp, overwrite = T);
		pathToFile = fileTmp;
	}
	if (file.info(pathToFile)$size == 0) {
		pathToFile = '';
	}
	setREPentry(sprintf('%s_plot', name), pathToFile);
	setREPentry(sprintf('%s_code', name), c$text);
	NULL
}

# tag allows to search for overloading templates (_tag). This can be used in reportSubTemplate to
#	conditionally report templates
.REP.interpolateTemplate = function(templName, conditionals = list(), tag = NULL,
	maxIterations = 1e4, iterative = T) {
	ri = .REPORTER.ITEMS;
	if (!is.null(tag) && !is.null(ri$templates[[sprintf('%s_%s', templName, tag)]]))
		templName = sprintf('%s_%s', templName, tag);
	s = ri$templates[[templName]]
	#s = readFile(tpath);
	s = mergeDictToString(.REPORTER.ITEMS$patterns, s, maxIterations = maxIterations, iterative = iterative);

	lengths = sapply(names(conditionals), nchar);
	for (n in names(conditionals)[rev(order(lengths))]) {
		s = gsub(sprintf('IF_%s(.*?)END_IF', n), if (conditionals[[n]]) '\\1' else '', s);
	}
	s
}

# initialize a series of reportSubTemplate calls followed by a finalizeSubTemplate call
REP.reportSubTemplateInitialize = function(subTemplate) {
	patterns = .REPORTER.ITEMS$patterns;
	subPatterns = sprintf('TEMPLATE:%s:subTemplates', subTemplate);
	REP.unreport(subPatterns);
}

REP.reportSubTemplate = function(subTemplate, tag = NULL, conditionals = list()) {
	ri = .REPORTER.ITEMS;
	# tag
	if (is.null(tag)) {
		tt = ri$templateTags;
		tag = ri$templateTags[[subTemplate]] =
			ifelse (is.null(tt[[subTemplate]]), 0, tt[[subTemplate]]) + 1;
		setRI(ri);
	}
	# finalize subTemplates
	patterns = ri$patterns;
	subPattern = sprintf('TEMPLATE:%s_%s', subTemplate, as.character(tag));
	subPatterns = sprintf('TEMPLATE:%s:subTemplates', subTemplate);

	# set own entry
	setREPentry(subPattern, .REP.interpolateTemplate(subTemplate, tag = tag));

	# collect all subTemplates
#	for (st in names(ri$parameters$subTemplates)) {
#		i = which.indeces(sprintf('TEMPLATE:%s_.*', st), names(.REPORTER.ITEMS$patterns), regex = T);
#		setREPentry(sprintf('TEMPLATE:%s:subTemplates', st), join(unlist(names(patterns[i])), "\n"));
#	}
	#i = which.indeces(sprintf('TEMPLATE:%s_.*', subTemplate), names(.REPORTER.ITEMS$patterns), regex = T);
	# append new element
	setREPentry(subPatterns, join(c(patterns[[subPatterns]], subPattern), "\n"));
	
	REP.save();
}

REP.finalizeSubTemplate = function(subTemplate, maxIterations = 1e4, iterative = T) {
	# finalize subTemplates
	patterns = .REPORTER.ITEMS$patterns;
	subPatterns = sprintf('TEMPLATE:%s:subTemplates', subTemplate);

	text = mergeDictToString(patterns, patterns[[subPatterns]], maxIterations = maxIterations, iterative = T);
	setREPentry(sprintf('TEMPLATE:%s', subTemplate), text);
	# remove trail
	if (is.null(subPatterns)) return(NULL);
	subPattern = splitString("\n", .REPORTER.ITEMS$patterns[[subPatterns]]);
	#print(c(subPatterns, subPattern));

	REP.unreport(c(subPatterns, subPattern));

	REP.save();
}

REP.finalize = function(conditionals = list(), verbose = FALSE, cycles = 1, output = NULL,
	maxIterations = 1e4, iterative = T) {
	# <p> vars
	ri = .REPORTER.ITEMS;
	
	# <p> prepare
	dir = tempFileName('rreporter', inRtmp = T, retries = 50);
	file.remove(dir);
	dir.create(dir);
	# <!> assume relative pathes
	for (cpath in .REPORTER.ITEMS$parameters$copy.files) {
		if (splitPath(cpath)$isAbsolute) {
			dest = sprintf('%s/%s', dir, splitPath(cpath)$file);
			Log(sprintf('Reporting: symlinking %s -> %s', cpath, dest), 4);
			file.symlink(cpath, dest);
		} else {
			for (sdir in c('', getwd(), sapply(ri$templatePathes, function(tp)splitPath(tp)$dir))) {
				source = sprintf('%s/%s/%s', getwd(), sdir, cpath);
				Log(sprintf('Reporting: dir %s', sdir), 4);
				if (file.exists(source)) {
					dest = sprintf('%s/%s', dir, splitPath(cpath)$file);
					Log(sprintf('Reporting: symlinking %s -> %s', source, dest), 4);
					file.symlink(source, dest);
					break;
				}
			}
		}
	}

	# <p> create final document
	tn = names(ri$templates)[1];
	allConditionals = merge.lists(ri$conditionals, conditionals);
	s = .REP.interpolateTemplate(ri$mainTemplate, allConditionals,
		maxIterations = maxIterations, iterative = iterative);

	# <p> run latex to produce temp file
	tmpPath = sprintf('%s/%s.tex', dir, tn);
	writeFile(tmpPath, s);
	Log(readFile(tmpPath), 5)
	latexCmd = firstDef(ri$parameters$latex, 'pdflatex');
	for (i in 1:cycles) {
		r = System(Sprintf('cd %{dir}s ; %{latexCmd}s -interaction=nonstopmode \"%{tn}s\"'),
			4, return.output = T);
		if (r$error > 0) Log(Sprintf("%{latexCmd}s exited with error."), 1);
		if (r$error > 0 || (verbose && i == 1)) Log(r$output, 1);
		#if (r$error > 0) break;
	}
	# <p> output
	postfix = join(names(conditionals[unlist(conditionals)]), '-');
	if (postfix != '') postfix = sprintf('-%s', postfix);
	#fileOut = sprintf('%s%s%s.pdf', splitPath(tpath)$base, if (postfix == '') '' else '-', postfix);
	#fileOut = sprintf('%s%s%s.pdf', tn, if (postfix == '') '' else '-', postfix);
	if (is.null(output))
		output = if (exists('.globalOutput'))
			.fn(sprintf('%s%s', splitPath(ri$output)$base, postfix), 'pdf') else ri$output;
	Log(sprintf('Writing to output %s', output), 4);
	
	file.copy(sprintf('%s.pdf', splitPath(tmpPath)$fullbase), output, overwrite = T);
	file.copy(sprintf('%s.tex', splitPath(tmpPath)$fullbase),
		sprintf('%s.tex', splitPath(output)$fullbase), overwrite = T);
}

#
#	<p> helpers
#

REP.reportFigureTable = function(nameTag, namesPlots, cols = 2, captions = NULL) {
	namesPlots = sapply(namesPlots, function(p) {
		path = if ('ggplot' %in% class(p)) {
			path = tempfile(fileext = '.pdf');
			ggsave(path, plot = p);
			path
		} else p;
		path
	});
	figureTable = report.figure.table(namesPlots, cols = cols, captions = captions);
	REP.tex(nameTag, figureTable);
}

#
#	Example code
#

# #	refresh only valid after a REP.new call
#	REP.refreshTemplates('gwas/reportGwas.tex')
# 	REP.new(
# 		'gwas/reportGwas.tex',
# 		cache = sprintf('%s/reportGWAS_cache', outputDir),
# 		resetCache = resetCache
# 	);
# # reporting
# 	REP.tex('G:DESCRIPTION', firstDef(o$studyDescription, ''));
# 	REP.tex('G:ROUNDNAME', firstDef(o$runName, 'unnamed'));
#	REP.finalize(verbose = T, output = sprintf('%s/reportGwas-%s.pdf', outputDir, o$runName), cycles = 3);

# # reporting patterns
# 	REP.tex('ASS:TABLE', report.data.frame.toString(
# 		psTop,
# 		digits = c(rep(NA, length(varsMap)), '#2', rep(2, length(Evars)), '#2', 2),
# 		names.as = rep.names, quoteHeader = F,
# 		caption = caption
# 	), fmt = 'tiny');
#	REP.tex('ASS:QQ:INFLATION', inflation, fmt = '.2');
# 	REP.plot('ASS:QQ:ASSOCIATION', Qplot(sample = ps$P, dist = qunif,
# 		file = sprintf('%s/ass-QQ-%s.jpg', outputDir, tag2fn(tag))));
#	REP.tex('QC:SAMPLE:MDS:Outlier', fraction(qcMdsOutliers), fmt = 'percent');
#
# # sub-templates
# 	REP.reportSubTemplateInitialize('association');
# 	for (m in expandedModels$models) with(m, {
#		REP.tex('ABC', 2);
#		REP.reportSubTemplate('association', tag);
# 	});
# 	REP.finalizeSubTemplate('association');

#
#	Rfunctions.R
#Tue 14 Aug 2007 01:39:42 PM CEST 

#Require('magrittr');

#
#	<§> abstract data functions
#

inverse = function(f, interval = c(-Inf, Inf)) {
	Vectorize(
	function(y, ...) {
		optimize(function(x, ...){ (y - f(x, ...))^2 }, interval = interval, ...)$minimum
	}
	)
}

#
#	<p> meta functions
#

callWithArgs = function(fctName, args) {
	#arguments = paste(sapply(names(args), function(n)sprintf("%s = %s", n, args[[n]])), collapse = ", ");
	fhead = sprintf("%s(%s)", fctName, paste(names(args), collapse = ", "));
	eval(parse(text = fhead))
}

.do.call = function(f, args, restrictArgs = TRUE, usePositional = TRUE, restrictPositional = FALSE) {
	# <p> function arguments
	fargs = names(as.list(args(f)));
	# remove spurious arguments
	fargs = fargs[fargs != ''];

	if (restrictArgs && all(fargs != '...')) {
		idcs = which.indeces(fargs, names(args));
		if (usePositional) {
			positional = which(names(args) == '');
			Npositional = (length(fargs) - length(idcs));
			if (!restrictPositional && length(positional) > Npositional)
				stop(".do.call: unmachted positional arguments");
			idcs = c(idcs, positional[1:Npositional]);
		}
		args = args[sort(idcs)];
	}
	do.call(f, args)
}

callDelegate = function(functionBase, delegation, args = list(), restrictArgs = TRUE) {
	f = get(Sprintf('%{prefix}s%{delegation}u', prefix = join(functionBase, '')));
	.do.call(f, args, restrictArgs = restrictArgs)
}

CallDelegate = function(functionBase, delegation, ..., restrictArgs = TRUE) {
	callDelegate(functionBase, delegation, args = list(...), restrictArgs = TRUE)
}

# call function with seperate arguments extracted from vector
V2A = function(f)function(x, ...)do.call(f, c(as.list(x), list(...)));
# call function with vector constructed from seperate arguments
A2V = function(f)function(...)f(c(...));
# arguments to matrix
A2M = function(f)function(...)f(do.call(cbind, list(...)));

A2VbyRow = function(f) function(...) {
	m = do.call(cbind, list(...));
	r = apply(m, 1, f);
	return(r);
}

#
#	<p> generic functions
#

Identity = function(...)list(...)
Identity1 = function(e, ...)e

#
#	<p> benchmarking
#

benchmark.timed = function(.f, ..., N__ = 1) {
	t0 = Sys.time();
	for (i in 1:N__) {
		r = .f(...);
	}
	t1 = Sys.time();
	r = list(time = (t1 - t0)/N__, lastResult = r, t0 = t0, t1 = t1);
	message(r$time);
	message(r$t0);
	message(r$t1);
	r
}

Benchmark = function(expr, N__ = 1, verbose = TRUE, returnTiming = FALSE, Nabbr = 20, logLevel = 2,
	gcFirst = FALSE, timeStat = sum, envir = parent.frame()) {
	s = Deparse(substitute(expr));

	# <p> timing
	t0 = Sys.time();
	# <i> for-loop required due to side-effects
	r0 = NULL;
	timesCal = NULL;
	timesSys = NULL;
	for (i in 1:N__) {
		t0i = Sys.time();
		t = system.time(r0 <- eval(expr, envir = envir), gcFirst = gcFirst);
		t1i = Sys.time();
		timesCal[i] = t1i - t0i;
		timesSys[i] = timeStat(t);
	}
	t1 = Sys.time();

	# <p> stats
	#timeTotal = t1 - t0;
	#timeIteration = timeTotal/N__;
	timing = list(
		timeCal = sum(timesCal), timeCalIter = mean(timesCal), timeCalSd = sd(timesCal),
		timeSys = sum(timesSys), timeSysIter = mean(timesSys), timeSysSd = sd(timesSys),
		lastResult = r0, t0 = t0, t1 = t1
	);

	if (verbose) with(timing, {
		exprStr = strAbbr(s, Nabbr);
		l = logLevel;
		Logs('Timing of %{exprStr}s', logLevel = l);
		Logs('\tCal: %{timeCal}.2f Iter: %{timeCalIter}.2f (%{timeCalSd}.1f)', logLevel = l);
		Logs("\tSys: %{timeSys}.2f Iter: %{timeSysIter}.2f (%{timeSysSd}.1f)", logLevel = l);
	})
	r = if (returnTiming) timing else r0;
	r
}

#
#	<p> optimization
#

# Ngrid should be uneven
# not used at the moment
gridFactor = function(Ngrid = 5, Step = .5, Factor = 1.5) {
	genF = function(center, min, max) {
		gridMarginal = lapply(center, function(p) {
			gridRaw = (Step * Factor ^ (0: (Ngrid - 1)));
			c(p - gridRaw, p, p + gridRaw)
		});
		grid = as.matrix(Rbind(merge.multi.list(gridMarginal)));
		return(grid);
	};
	genF
}

gridBounding = function(Ngrid = 5) {
	genF = function(center, min, max) {
		gridMarginal = lapply(seq_along(min), function(i) {
			seq(min[i], max[i], length.out = Ngrid)
		});
		grid = as.matrix(Rbind(merge.multi.list(gridMarginal)));
		return(grid);
	};
	genF
}

searchContourGridRaw = function(f, grid, v, ...,
	contour = 0.05, gridGen, eps = 1e-3, lower = TRUE, verbose = FALSE) {
	if (verbose) message(Print(cbind(grid, v)));
	# assume regular grid
	Ndim = ncol(grid);
	pts = apply(grid, 2, function(v)sort(unique(v)));
	Ns = apply(pts, 2, function(pts)pop(seq_along(pts)));
	# list of canonical points of sub-hypercubes
	cubes = as.matrix(Rbind(merge.multi.list(Df_(Ns))));
	# calculate offsets to get all vertices of hypercube, coords per column
	hyper = t2r(sapply(1:(2^Ndim) - 1, ord2bin, digits = Ndim));
	# iterate hypercubes to decide value, nd defines canonical vertex of hypercube
	sel = apply(cubes, 1, function(nd) {
		NdsCube = t(nd + hyper);
		#print(NdsCube - (nd + t_(hyper)));
		# search funcion values on vertices of hypercube
		#coords = cbind(pts[NdsCube[, 1], 1], pts[NdsCube[, 2], 2]);
		coords = sapply(1:ncol(NdsCube), function(i)pts[NdsCube[, i], i]);
		idcs = DfSearch(Df_(coords), Df_(grid));
		vs = v[idcs];
		#if (is.na(any(vs <= contour) & any(vs >= contour))) browser();
		if (any(vs <= contour) & any(vs >= contour)) { # tb persued
			mn = apply(coords, 2, min);
			mx = apply(coords, 2, max);
			center = (mn + mx) / 2;
			searchContourGrid(f, gridGen(center, mn, mx), ...,
				contour = contour, gridGen = gridGen, eps = eps, lower = lower,
				gridCache = cbind(grid, v));
		} else list()
		# } else list(matrix(rep(NA, ncol(grid), ncol = ncol(grid))))
	});
	return(unlist.n(sel, 1));
}

# <!><i> multivariate functions
applyCached = function(grid, f, gridCache, ...) {
	# <p> no cache
	if (missing(gridCache) || !notE(gridCache)) return(apply(grid, 1, f, ...));
	s = matrixSearch(grid, gridCache);
	idcs = setdiff(1:nrow(grid), s[, 2]);
	vI = apply(grid[idcs, , drop = FALSE], 1, f, ...);
	v = vector.assign(NA, c(idcs, s[, 2]), c(vI, gridCache[s[, 1], ncol(gridCache)]), N = nrow(grid));
	return(v);
}

# gridCache: matrix/df with cbind(grid, v) from previous computations to avoid double evaluations
searchContourGrid = function(f, grid, ..., contour = 0.05, gridGen, eps = 1e-3, lower = TRUE, gridCache) {
	# compute values of function on grid vertices
	#v = apply(grid, 1, f, ...);
	v = applyCached(grid, f, gridCache, ...);
	#print(cbind(grid, v));
	# determine recursion end
	mn = apply(grid, 2, min);
	mx = apply(grid, 2, max);
	# found contour elevation to desired accuracy
	#print(max(mx - mn));
	if (max(mx - mn) < eps) {
		i = if (lower) which.min(v) else which.max(v);
		return(list(grid[i, ]));
	}
	# continue searching
	r = searchContourGridRaw(f, grid, v, ...,
		contour = contour, gridGen = gridGen, eps = eps, lower = lower);
	return(r);
}

searchContourGridList = function(f, gridList, ..., contour = 0.05, gridGen, eps = 1e-2, lower = TRUE) {
	gL = lapply(gridList, searchContourGrid, ..., f = f, contour = contour, gridGen = gridGen);
	return(unlist.n(gL, 1));
}

searchContour = function(f, start, ..., contour = 0.05, delta = 3,
	gridGen = gridBounding(Ngrid = 3), eps = 1e-2, lower = TRUE) {
	grid = gridGen(start, start - delta, start + delta);
	r = searchContourGrid(f, grid, ..., contour = contour, gridGen = gridGen, eps = eps);
	return(do.call(rbind, r));
}

#
#	<p> optimization
#

searchOptimumGrid = function(f, grid, ..., delta, gridGen, eps = 1e-3, scale = 1, returnOpt = FALSE) {
	# compute values of function on grid vertices
	#print(grid);
	v = apply(grid, 1, f, ...) * scale;
	# vertex with optimum
	Iopt = which.max(v);
	# determine recursion end
	mn = apply(grid, 2, min);
	mx = apply(grid, 2, max);
	Ns = apply(grid, 2, function(v)length(unique(v)));
	#Ns = apply(grid, 2, length %.% unique);	# ought to be
	# magrittr
	#Ns = apply(grid, 2, . %>% unique %>% length);
	# found contour elevation to desired accuracy
	#print(max(mx - mn));
	if (max(mx - mn) < eps) {
		return(if (returnOpt) list(par = grid[Iopt, ], value = v[Iopt]) else grid[Iopt, ]);
	}
	# continue searching
	r = searchOptimum(f, grid[Iopt, ], ..., delta = delta / Ns, gridGen = gridGen, eps = eps, scale = scale,
		returnOpt = returnOpt);
	return(r);
}

searchOptimum = function(f, start, ..., delta = 3, gridGen = gridBounding(Ngrid = 7), eps = 1e-2, scale = 1,
	returnOpt = FALSE) {
	grid = gridGen(start, start - delta, start + delta);
	r = searchOptimumGrid(f, grid, ..., delta = delta, gridGen = gridGen, eps = eps, scale = scale,
		returnOpt = returnOpt);
	return(r);
}

..OptimizeControl = list(fnscale = -1, tol = .Machine$double.eps^0.25);
# assume unconstraint arguments
Optimize = function(p, f, method = 'BFGS', control = ..OptimizeControl, ...,
	hessian = T, ci = T, alpha = 5e-2) {
	r = if (length(p) > 1) {
		control = .list(control, .min = 'tol');
		o = optim(p, f, method = method, control = control, hessian = hessian, ...);
	} else if (length(p) == 1) {
		f0 = function(p, ...) { f(logit(p), ...) };
		o0 = try(optimize(f0, lower = 0, upper = 1,
			tol = control$tol, maximum = control$fnscale < 0, ...));
		o = if (class(o0) == 'try-error') list(par = NA, value = NA, hessian = NA) else 
			list(par = logit(o0$maximum), value = o0$objective,
				hessian = if(hessian) matrix(Dn2f(f, logit(o0$maximum), ...)/o0$objective) else NA);
	} else {
		o = list(par = c(), value = f(...));
	}
	if (ci && hessian && !is.na(r$hessian)) {
		var = -1/diag(r$hessian);	# assume sharp cramer-rao bound
		sd = sqrt(var);
		r = c(r, list(ci = list(
			ciL = qnorm(alpha/2, r$par, sd, lower.tail = T),
			ciU = qnorm(alpha/2, r$par, sd, lower.tail = F), level = alpha, var = var)));
	}
	r
}

# p: matrix of row-wise start values
OptimizeMultiStart = function(p, f, method = 'BFGS', control = ..OptimizeControl, ...) {
	r = if (is.null(p)) {	# special case of degenerate matrix (does not work in R)
		Optimize(c(), f, method = method, control = control, ...)
	} else if (!is.matrix(p)) {
		Optimize(p, f, method = method, control = control, ...)
	} else {
		os = apply(p, 1, function(s)Optimize(s, f, method = method, control = control, ...));
		# find maximum
		if (all(is.na(os))) return(NA);
		vs = list.key(os, 'value');
		arg.max = which.max(vs);
		r = os[[arg.max[1]]];
	}
	r
}

OptimControlDefault = list(fnscale = -1, tol = .Machine$double.eps^0.25, startScale = 2, hessian = F);

Optim = function(p, f, method = 'BFGS', control = list(), ...) {
	control = merge.lists(OptimControlDefault, control);
	o = if (length(p) > 1) {
		myControl = List_(control, min_ = c('tol', 'startScale', 'hessian'));
		optim(p, f, method = method, control = myControl, hessian = control$hessian, ...);
	} else if (length(p) == 1) {
		f0 = function(p, ...) { f(logit(p), ...) };
		if (!is.null(p)) {
			lower = if (p < 0) p * control$startScale else p / control$startScale;
			upper = if (p >= 0) p * control$startScale else p / control$startScale;
		} else {
			lower = -Inf;
			upper = Inf;
		}
		if (abs(lower - upper) < 1e-3) {
			lower = lower - 1;
			upper = upper + 1;
		}
		#print(c(start = p, lower = lower, upper = upper));
		o0 = try(
			optimize(f0,
				lower = plogis(lower), upper = plogis(upper),
				tol = control$tol, maximum = control$fnscale < 0, ...)
		);
		if (class(o0) == 'try-error') list(par = NA, value = NA, hessian = NA) else 
			list(
				par = logit(o0$maximum), value = o0$objective,
				hessian = if (control$hessian) matrix(Dn2f(f, logit(o0$maximum), ...)/o0$objective) else NA
			)
	} else list(par = c(), value = f(...));
	return(o);
}

optimFn = function(p, f, method = 'BFGS', control = list(), lower = -Inf, upper = Inf, ...) {
	control = merge.lists(OptimControlDefault, control);
	o = if (length(p) > 1) {
		myControl = List_(control, min_ = c('tol', 'startScale', 'hessian'));
		optim(p, f, method = method, control = myControl, hessian = control$hessian, ...);
	} else if (length(p) == 1) {
		o0 = try(
			optimize(f,
				lower = lower, upper = upper,
				tol = control$tol, maximum = control$fnscale < 0, ...)
		);
		if (class(o0) == 'try-error') list(par = NA, value = NA, hessian = NA) else 
			list(
				par = firstDef(o0$maximum, o0$minimum), value = o0$objective,
				hessian = if (control$hessian) matrix(Dn2f(f, o0$maximum, ...)/o0$objective) else NA
			)
	} else list(par = c(), value = f(...));
	return(o);
}

#
#	Rstatistic.R
#Fri 19 Jan 2007 11:06:44 PM CET 

# contains simple statistics to evaluate consulting questions

sizesDesc = function(s) {
	col.frame(list(
		mean = mean(s),
		median = median(s),
		stddev = sqrt(var(s)),
		quantiles = quantile(s)
	), do.paste = " ", digits = 1)
}

compareSamples = function(l) {
	desc = data.frame(lapply(l, function(e)sizesDesc(e)));
	print(desc);

	tests = col.frame(list(
		test.t = t.test(l[[1]], l[[2]])$p.value,
		test.wilcoxon = wilcox.test(l[[1]], l[[2]])$p.value
	));
	print(tests);
}

df2numeric = function(df) apply(df, 2, function(col)as.numeric(as.vector(col)));
expandCounts = function(tab)  unlist(apply(tab, 1, function(r){rep(r[1], r[2])}));

chisq.test.robust = function(tab, bootstrapCellCount = 5, B = 5e3) {
	# reduce table by removing 0-marginals and check for degeneration
	tab = tab[, !apply(tab, 2, function(c)all(c == 0))];
	if (is.vector(tab)) return(list(p.value = NA));
	tab = tab[!apply(tab, 1, function(r)all(r == 0)), ];
	if (is.vector(tab)) return(list(p.value = NA));
	# determine whether to bootstrap
	r = if (any(tab < bootstrapCellCount))
		chisq.test(tab, simulate.p.value = T, B = B) else
		chisq.test(tab);
	r
}

# depends on coin package <!>, unfinished
armitage.test.robust = function(formula, df, scores) {
	tab = table(df);
	# only eliminate 0-rows of table from score vector
	zRows = sapply(1:dim(tab)[1], function(i){ all(tab[i,] == 0) });
	scores[[1]] = scores[[1]][!zRows];
	r =	independence_test(formula, df, teststat = "quad", scores = scores);
	r
}

# simulations in project 2014-02-Microsatellites
logSumExpRaw = function(v, pivot = median(v))(log(sum(exp(v - pivot))) + pivot)
logSumExpPivot = logSumExpMax = function(v)logSumExpRaw(v, pivot = max(v))
logSumExp = function(x) {
	Imx = which.max(x);
	log1p(sum(exp(x[-Imx] - x[Imx]))) + x[Imx]
}
logSumExp_add = function(v, bins) {
	diff = abs(bins - v);
	diffB = pop(bins) - shift(bins);
	if (max(diff) > max(diffB)) {	# bin-reshuffeling
		Ibin = which.min(diffB);
		bins[Ibin] = logSumExp(bins[c(Ibin, Ibin + 1)]);
		bins[Ibin + 1] = v;
	} else {
		Ibin = which.min(abs(diff));
		bins[Ibin] = logSumExp(c(v, bins[Ibin]));
	}
	return(sort(bins, na.last = T, decreasing = T));
}

logSumExpIter_raw = function(x, Nbins = 30) {
	if (length(x) < Nbins) return(x);
	lse = x[1:Nbins];
	for (e in x[-(1:Nbins)]) lse = logSumExp_add(e, lse);
	return(logSumExp(lse));
}
logSumExpIter = function(x, Nbins = 30) {
	return(logSumExp(logSumExpIter_raw(x)));
}
# sumExp = function(x) {
# 	Imx = which.max(x);
# 	sum(exp(x - x[Imx])) * exp(x[Imx])
# }
sumExp = function(x)exp(logSumExp(x))
meanExp = function(x)(sumExp(x)/length(x))


# rejFrac = function(x, alpha = 0.05) {
# 	x = na.omit(x);
# 	f = count(x <= alpha) / length(x);
# 	f
# }
rejFrac = function(x, alpha = 0.05)mean(x <= alpha, na.rm = T);
vector.std = function(v, C = 1)(C * v / sum(v));
vector.std.log = function(v, C = 0)(v - (logSumExp(v) - C));

#
#	<p> ml methods
#

lhWrapperFunctions = c("initialize",
	"parsScale", "parsMap", "parsMapInv", "parsStart", "parsNames", "lh", "null2alt", "alt2null"
);

# <!> transition to S4-objects
lhGetWrapper = function(prefix, self, ...) {
	createNullWrapper = F;
	f = list();
	if (substr(prefix, nchar(prefix) - 3, nchar(prefix)) == "null") {
		createNullWrapper = T;
		prefix = substr(prefix, 1, nchar(prefix) - 5);
	}
	for (n in lhWrapperFunctions) {
		f[[n]] = mget(sprintf("%s.%s", prefix, n), envir = globalenv(), ifnotfound=list(NULL))[[1]];
	}
	f$self = if (is.null(self)) { if (is.null(f$initialize)) list(...) else f$initialize(...) } else self;
	if (createNullWrapper) {
		f1 = f;
		self = f1$self = f$self;
		f1$parsStart = function(self){ f$alt2null(self, f$parsStart(self)) };
		f1$parsScale = function(self){ f$alt2null(self, f$parsScale(self)) };
		f1$parsMap = function(self, p){ f$alt2null(self, f$parsMap(self, f$null2alt(self, p))) };
		f1$parsMapInv = function(self, p){ f$alt2null(self, f$parsMapInv(self, f$null2alt(self, p))) };
		f1$lh = function(self){ lhRaw = f$lh(self); function(p)lhRaw(f$null2alt(self, p)) };
		return(f1);
	}
	f
}
lhCopyWrapper = function(name, template) {
	for (f in lhWrapperFunctions) {
		g = mget(sprintf("%s.%s", template, f), envir = globalenv(), ifnotfound=list(NULL))[[1]];
		if (!is.null(g)) eval.parent(parse(text = sprintf("%s.%s = %s.%s;", name, f, template, f)));
	}
}

lhInit = function(lhWrapper) {
	
}

mapU = function(y){ -log(1/y - 1) }
map2U = function(z){ 1/(1 + exp(-z)) }
# one-dimensional estimation
lhMlEstimatorOD = function(lhWrapper = NULL, start = NULL, c = NULL, ...) {
	if (is.null(c)) c = list(tol = .Machine$double.eps^0.25);
	f = lhGetWrapper(lhWrapper, c$self, ...);

	lhRaw = f$lh(f$self);
	lh = function(p) { lhRaw(mapU(f$parsMap(f$self, p))) }
	o = try(optimize(lh, lower = 0, upper = 1, tol = c$tol, maximum = T));
	r = list(par = mapU(f$parsMap(f$self, o$maximum)), par.os = o$maximum, value = o$objective);
	r
}

# multi-dimensional estimation
lhMlEstimatorMD = function(lhWrapper = NULL, start = NULL, c = NULL, ...) {
	if (is.null(c)) c = list(do.sann = F, sann.cycles = 1000);
	f = lhGetWrapper(lhWrapper, c$self, ...);
	eps = 1e-5;
	#if (!is.null(start)) { starts = matrix(start, nrow = 1); }
	if (is.null(start)) start = f$parsStart(f$self);

	starts = if (!is.matrix(start)) matrix(as.numeric(unlist(start)), nrow = 1) else start;
	parscale = f$parsScale(f$self);
	lhRaw = f$lh(f$self);
	lh = function(p) { lhRaw(f$parsMap(f$self, p)) }
	os = apply(starts, 1, function(s) {
		s = f$parsMapInv(f$self, s);
		o = try(optim(s, lh, method = "Nelder-Mead",
			control = list(fnscale = -1, parscale = parscale, maxit = 1000),
		));
		if (class(o) == "try-error") return(NA);
		if (0) { # if (o$convergence > 0 || c$do.sann) {	# Nelder-Mead failed to converged
		o1 = try(optim(s, lh, method = "SANN",
			control = list(fnscale = -1, parscale = parscale, maxit = c$sann.cycles),
		));
		#if (class(o1) == "try-error") return(NA);
		if (o$convergence > 0 || o1$value > o$value) o = o1;
		}
		o$par.os = o$par;	# parameter values in optimiztation space
		o$par = f$parsMap(f$self, o$par);
		o
	});

	if (all(is.na(os))) return(NA);
	vs = sapply(os, function(o){o$value});
	arg.max = which.max(vs);
	estimate = os[[arg.max[1]]];
	fisher = list();
	#if (!is.null(c$computeFisher) & c$computeFisher)
	if (!is.null(c$computeFisher)) fisher = estimate.fisher(d, estimate, fisher.eps = 1e-1);
	r = c(estimate, fisher);
	r
}

lhMlEstimator = function(lhWrapper = NULL, start = NULL, c = NULL, ...) {
	f = lhGetWrapper(lhWrapper, c$self, ...);
	r = if (length(f$parsStart(f$self)) > 1) {
		lhMlEstimatorMD(lhWrapper, start, c, ...);
	} else if (length(f$parsStart(f$self)) == 1) {
		lhMlEstimatorOD(lhWrapper, start, c, ...);
	} else { # null hypothesis w/o nuisance parameters
		r = f$lh(f$self)();
	}
	r
}


lhLRtest = function(lhWrapper = NULL, start = NULL, c = list(do.sann = F, sann.cycles = 1000), ...) {
	f = lhGetWrapper(lhWrapper, NULL, c$self, ...);	# f$self is likelihood object and absorbs ellipsis parameters
	self = f$self;
	if (is.null(start)) start = f$parsStart(self);

	startNull = if (is.matrix(start))
		t(apply(start, 1, function(r)f$alt2null(self, r))) else
		f$alt2null(self, start);

	e.null = lhMlEstimator(sprintf("%s.%s", lhWrapper, "null"), startNull, c(c, list(self = self)));

	start = rbind(start, f$null2alt(self, e.null$par));
	e.alt = lhMlEstimator(lhWrapper, start, c(c, list(self = self)));

	# <p> calcualte degrees of freedom
	st = f$parsStart(self);
	df = length(st) - length(f$alt2null(self, st));
	stat =  2 * (e.alt$value - e.null$value);
	list(ll.null = e.null$value, ll.alt = e.alt$value,
		test.stat = stat, p = 1 - pchisq(stat, df), df = df, par.null = e.null$par, par.alt = e.alt$par
	)
}

#
#	lh-functions based on likelihood specification
#

# Example: see dataAnalysis.R in hwe project
# Example: binomial distribution
# lhBin = function(p, k, N)dbinom(k, N, p)
# spec_lhBin = list(
# 	ll = "lhBin",
# 	alt = list(
# 		start = c(.5),	# also specifies number of parameters
# 		pars = list(list(name = "rho", type = "freq"))
# 	),
# 	null = list(
# 		start = c(.5),	# assume same likelihood and therefore #pars from alternative
# 		parsFree = 0	# alternative: list free parameters or specify tail from alt
# 	)
# );
# r = lhMl(spec_lhBin)

#define a function
toF = function(expr, args, env = parent.frame()) {
	as.function(c(args, expr), env)
}

logitI = expit = function(x, min = 0, max = 1) { (max - min)/(1 + exp(-x)) + min }
expitD = toF(D(expression((max - min)/(1 + exp(-x)) + min), 'x'), list(x = NULL, min = 0, max = 1));
logit = function(x, min = 0, max = 1) { log((x - min)/(max - x)) }
logitD = toF(D(expression(log((x - min)/(max - x))), 'x'), list(x = NULL, min = 0, max = 1));
# templates assuming X as argument, p as parameter description list
lhArgMappers = list(
	freq =		"expit(X)",
	int =		"expit(X, min, max)",
	real =		"X",
	positive =	"log1p(exp(X))"
);
lhArgMappersD = list(
	freq =		NULL, #D(expression(expit(x), 'x')),
	int =		"expit(X, min, max)",
	real =		"X",
	positive =	"log1p(exp(X))"
);
lhArgMappersI = list(
	freq =	"logit(X)",
	int =	"logit(X, min, max)",
	real =	"X",
	positive =	"log(expm1(X))"
);
lhSpecificationDefaults = list(
	# embed null-parameter into alt-parameter space: variables: npars, parsFree, s (specification),
	#	p: input parameters
	#	<i>: optimization: substitute literals from start
	default = list(mapper = 'c(c(ARGS_FREE), c(ARGS_BOUND))', mapperInv = 'c(ARGS_FREE)')
);
# richest: richest parametrization of the likelihood
# lhInterface: call the likelihood function with a vector (vector) or with separate arguments formula
#	the paramters (inline)
lhSpecificationDefault = list(richest = 'alt', lhInterface = 'vector');
lhSpecificationInterfaces = list(
	vector = 'function(p, ...) { pm = mapper(p); if (any(abs(pm) > 1e10)) return(-Inf); lf(pm, ...) }',
	inline = 'function(p, ...) { pm = mapper(p); if (any(abs(pm) > 1e10)) return(-Inf); lf(ARGS_INLINE, ...) }'
);

#
#	<p> logit derivatives
#simulations in 2014-07-Borstkanker/src/borstKankerExp.R
logExpit1 = function(x)log(expit(x))
logExpit = logExpit2 = function(x)-log1p(exp(-x))
logitExp1 = function(x)logit(exp(x))
logitExp = logitExp2 = function(x)-log(expm1(-x))

logExpit1m1 = function(x)log(1 - expit(x))
logExpit1m = logExpit1m2 = function(x)-log1p(exp(x))
logit1mExp1 = function(x)logit(1 - exp(x))
logit1mExp = logit1mExp2 = function(x)log(expm1(-x))


#
#	<p> helper functions
#

# mappers for individual parameters
# ps: list of parameters
# mappers: mapper templates to used
# target: name of variable on which to apply
# idcs: indeces to iterate
lhMapperPars = function(ps, mappers, target = 'p', idcs = 1:length(ps)) {
	maps = if (length(idcs) == 0) c() else sapply(idcs, function(i) {
		p = ps[[i]];
		a = gsub("X", sprintf("%s[%s]", target,
			deparse(if (length(p$entries)) p$entries else i)), mappers[[p$type]]);
		a = mergeDictToString(ps[[i]]$args, a);
		a
	});
	r = paste(maps, collapse = ", ");
	r
}

# <!> auto inverse mapping has to heed mapperPost time of application
# mappers map individual arguments, mapper sub-sequently maps the whole vector
lhMapperFunction = function(s, mappers, mapper) {
	free = 1:s$parsFree;	# idcs of free variables
	bound = if(s$parsFree < s$npars) (s$parsFree + 1):s$npars else c(); # idcs of bound variables
	mStr = sprintf('function(p){%s}',
		mergeDictToString(list(
			ARGS_FREE = lhMapperPars(s$pars, mappers, 'p', free),
			ARGS_BOUND = lhMapperPars(s$pars, mappers, 'start', bound)
	), mapper));
	mf = with(s, eval(parse(text = mStr)));
	mf
}

lhMapperFunctions = function(s) {
	r = list(
		mapper = lhMapperFunction(s, lhArgMappers, s$mapper),
		mapperInv = lhMapperFunction(s, lhArgMappersI, s$mapperInv)
	);
	r
}

#' Build wrapper function around likelihood
#'
#' @param template parameter specification used as template (usually richest parametrization tb reduced
#'	for other hypotheses)
lhPreparePars = function(pars, defaults = lhSpecificationDefaults$default, spec = lhSpecificationDefault,
	template = pars) {
	# <p> determine free parameters
	t = merge.lists(defaults, pars);
	npars = length(template$pars);
	if (!is.null(t$parsFree)) {
		t$pars = if(t$parsFree == 0) list() else template$pars[(npars - t$parsFree): npars];
	}
	if (is.null(t$start)) t$start = template$start;
	if (is.null(t$parsFree)) t$parsFree = length(t$pars);

	# <p> construct mapped likelihood function
	fs = mergeDictToString(
		list(ARGS_INLINE =
			paste(sapply(1:npars, function(i) { sprintf("pm[%s]",
				deparse(if (length(template$pars[[i]]$entries)) template$pars[[i]]$entries else i)) }
			), collapse = ', ')),
		lhSpecificationInterfaces[[spec$lhInterface]]
	);
	t = merge.lists(t, list(npars = npars));
	t = merge.lists(t, lhMapperFunctions(t), list(lf = get(spec$ll)));
	f = with(t, eval(parse(text = fs)));
	t = merge.lists(t, list(npars = npars, lh = f));
	t
}

# types: names of specifications for which to define wrapped functions
# richest: name of specification for model that includes a superset of parameters of all other types
lhPrepare = function(s, types = c('null', 'alt')) {
	# <p> preparation
	s = merge.lists(lhSpecificationDefault, s);
	ri = s[[s$richest]];
	# number of parameter groups
	npars = length(ri$pars);
	# number of parameters of the likelihood function
	#Npar = sum(list.kp(ri$pars, 'entries', template = 1));
	# <p> build wrappers
	m = nlapply(types, function(type) {
		defaults = merge.lists(lhSpecificationDefaults$default, lhSpecificationDefaults[[type]]);
		lhPreparePars(s[[type]], defaults, s, template = ri)
	});
	m = merge.lists(s, m);
	m
}
# <N> free parameters come first
lhFreePars = function(s, p)with(s, {
	r = if (parsFree > 0) {
		idcs = unlist(list.kp(s$pars[1:parsFree], 'entries'));
		if (length(idcs) == 0) idcs = 1:parsFree;
		p[idcs]
	} else c();
	r
})

# second numeric derivative of x
Dn2f = function(f, x, ..., eps = 1e-5) {
	(f(x + 2*eps, ...) + f(x - 2*eps, ...) - 2*f(x, ...))/(4*eps^2)
}

lhEstMLRaw = function(t, start = NULL, ..., optim_method = 'BFGS') {
	if (is.null(start)) start = t$start;
	for (method in optim_method) {
		o = try(OptimizeMultiStart(t$mapperInv(start), t$lh, method = method, ...));
		if (!('try-error' %in% class(o))) break();
	}
	o$par = t$mapper(o$par);
	o$ci$ciL = t$mapper(o$ci$ciL);
	o$ci$ciU = t$mapper(o$ci$ciU);
	o
}

lhEstML = lhMl = function(s, start = NULL, type = 'alt', ..., optim_method = 'BFGS') {
	# <p> mapping of parameters
	s = lhPrepare(s, types = type);
	lhEstMLRaw(s[[type]], start = start, ..., optim_method = optim_method)
}

lfPrepare = function(s, ...) {
	lhParsOrig = list(...);
	prepare = sprintf('%s%s', s$ll, c('prepare', '_prepare'));
	prepareExists = min(which(sapply(prepare, exists)));
	lhPars = if (prepareExists < Inf) get(prepare[prepareExists])(...) else lhParsOrig;
	lhPars
}

# specification based LR-test
lhTestLR = function(s, startNull = NULL, startAlt = NULL, types = c('null', 'alt'), ...,
	optim_method = 'BFGS', addTypeArg = F) {
	# <p> general preparation
	s = lhPrepare(s, types = types);
	null = s[[types[1]]];
	alt = s[[types[2]]];

	# <p> specific preparation (user defined)
	lhPars = lfPrepare(s, ...);
	
	# <p> null hypothesis
	if (is.null(startNull))
		startNull = if(null$parsFree == 0) NULL else matrix(lhFreePars(null, null$start), nrow = 1);
	lhEstMLRawArgs = c(list(t = null, start = startNull), lhPars, list(optim_method = optim_method));
	if (addTypeArg) lhEstMLRawArgs = c(lhEstMLRawArgs, list(lh_type__ = 'null'));
	o0 = do.call(lhEstMLRaw, lhEstMLRawArgs);

	# <p> alternative hypothesis
	if (is.null(startAlt)) {
		# build from fit under the null
		parNull = lhFreePars(null, o0$par);
		startAlt = matrix(c(parNull, alt$start[(length(parNull) + 1):length(alt$start)]), nrow = 1);
	}
	lhEstMLRawArgs = c(list(t = alt, start = startAlt), lhPars, list(optim_method = optim_method));
	if (addTypeArg) lhEstMLRawArgs = c(lhEstMLRawArgs, list(lh_type__ = 'alt'));
	o1 = do.call(lhEstMLRaw, lhEstMLRawArgs);

	# <p> calcualte degrees of freedom
	df = length(alt$start) - length(lhFreePars(null, o0$par));
	stat =  2 * (o1$value - o0$value);
	r = list(ll.null = o0$value, ll.alt = o1$value,
		test.stat = stat, p = 1 - pchisq(stat, df), df = df, par.null = o0$par, par.alt = o1$par,
		lh.pars = lhPars, lh.pars.orig = lhParsOrig
	);
	r
}

#
#	<p> latest iteration of LH wrapper
#

lhPrepareFormula = function(s, type, formula, data, ...) {
	# <o> compute on subset of data <N> cave: missingness
	X = model.matrix(model.frame(formula, data = data), data = data);

	# <p> expand paramters
	t = s[[type]];
	ps = t$pars;
	fparsI = which(list.key(ps, 'name') == 'formula');
	fpars = ps[[fparsI]];	# formula pars
	ps[[fparsI]] = merge.lists(ps[[fparsI]], list(name = 'beta', count = ncol(X)));
	# <p> determine slots
	counts = cumsum(list.key(ps, 'count'));
	countsStart = pop(c(1, counts + 1));
	ps = lapply(seq_along(ps), function(i)merge.lists(ps[[i]], list(entries = countsStart[i]:counts[i])));
	# <p> determine start
	start = avu(sapply(ps, function(p)rep(p$start, p$count)));
	# <p> map pars
	t$pars = ps;
	t = lhPreparePars(t, spec = merge.lists(lhSpecificationDefault, s));
	t$start = start;
	t
}

lhMlFormula = function(s, formula, data, type = 'formula', ..., optim_method = 'BFGS') {
	# <p> mapping of parameters
	t = lhPrepareFormula(s, type, formula, data, ...);
	# <p> extra args
	lhPars = lfPrepare(s, formula = formula, data = data, ...);
	# <p> call optimizer
	lhEstMLRawArgs = c(list(t = t, start = s$start), lhPars, list(optim_method = optim_method));
	r = try(do.call(lhEstMLRaw, lhEstMLRawArgs), silent = T);
print(r);
	if (class(r) == 'try-error') r = list(par = rep(NA, length(t$start)), value = NA, convergence = 1);
	r
}

#
# <p> model manipulation
#

response.is.binary = function(r) {
	vs = sort(unique(r));
	if (length(vs) != 2) F else all(vs == c(0, 1));
}



#
#	<p> clustered data
#

#
# <p> describe relationships (genetic) given a relational (database) model
#

# given relatedness in a data frame of ids and clusterIds, return a list of clusters containing ids
# clusterRelation2list_old = function(r, idName = "id", idClusterName = "idFam", byIndex = T) {
# 	r = r[, c(idName, idClusterName)];
# 	ns = sort(unique(r[, 2]));
# 	# <p> build clusters
# 	clusters = sapply(ns, function(e)list());		# holds members of clusters
# 	names(clusters) = ns;
# 	# <!> we can iterate the list, given it is ordered lexicographically
# 	for (i in 1:(dim(r)[1])) {
# 		clN = as.character(r[i, 2]);
# 		clusters[[clN]] = unlist(c(clusters[[clN]], ifelse(byIndex, i, as.character(r[i, 1]))));
# 	}
# 	clusters
# }
clusterRelation2list = function(r, idName = "id", idClusterName = "idFam", byIndex = T) {
	r = r[, c(idName, idClusterName)];
	clusters = nlapply(sort(unique(r[[idClusterName]])), function(n) {
		idcs = which(r[[idClusterName]] == n);
		c = if (byIndex) idcs else r[[idName]][idcs];
		c
	});
	clusters
}

# permute clusters of identical size and within clusters
# cluster specification as given by clusterRelation2list assuming byIndex = T
# returned permutation is relative to refIds
permuteClusters = function(cls, refIds = NULL, selectIds = NULL) {
	# allow to filter ids from cluster specification
	if (!is.null(selectIds)) {
		cls = lapply(cls, function(cl)intersect(cl, selectIds));
		cls = clusters[sapply(cls, length) > 0];
	}
	cSizes = sapply(cls, function(e)length(e));
	# which cluster sizes are present in the data set?
	sizes = unique(cSizes);
	# indexable list of ids
	refIds = if (is.null(refIds)) sort(unlist(cls));
	# final permutation of refIds, such that refIds[perm] gives new order
	perm = 1:length(refIds);

	for (s in sort(sizes, decreasing = T)) {	# permute cluster of same size, permute within cluster
		clsS = which(cSizes == s);
		p1 = sample(1:length(clsS));	# permute clusters
		for (i in 1:length(clsS)) {
			p2 = sample(1:s);
			# <p> indeces that are to be replaced
			indT = which.indeces(cls[[clsS[i]]], refIds);
			# <p> indeces where the replacement comes from
			indF = which.indeces(cls[[clsS[p1[i]]]][p2], refIds);
			# <p> save partial permutation
			perm[indT] = indF;
		}
	}
	perm
}

# clusters is a vector with cluster ids
clustersPermute = function(cls) {
	permuteClusters(clusterRelation2list(data.frame(id = 1:length(cls), idFam = cls)))
}

#
#	<p> wrap model fitting for lm/glm/gee fitters
#

#library("geepack");	# <i> move to init method
regressionMethods = list(
	# assume formula to contain random effect
	glmr = list(
		fit = function(formula, data, clusterCol = NULL, ...) {
			glmer(formula, data = data, ...)
		},
		compare = function(m1, m0){
			a = anova(m0$r, m1$r, test = "Chisq");
			list(anova = a, m0 = m0, m1 = m1,
				#p.value = a[["P(>|Chi|)"]][2],
				p.value = a[['Pr(>Chisq)']][2],	# as of R 2.15.1
				effects0 = coefficients(summary(m0$r))[, "Estimate"],
				sdevs0 = coefficients(summary(m0$r))[, "Std. Error"],
				effects1 = coefficients(summary(m1$r))[, "Estimate"],
				sdevs1 = coefficients(summary(m1$r))[, "Std. Error"]
			)
		}
	),
	# use cluster column <!> untested
	glmrcl = list(
		fit = function(formula, data, clusterCol = NULL, ...) {
			f = update(formula, as.formula(Sprintf('~ . + (1|%{clusterCol}s)')));
			glmer(f, data = data, ...)
		},
		compare = function(m1, m0){
			a = anova(m0$r, m1$r, test = "Chisq");
			list(anova = a, m0 = m0, m1 = m1,
				#p.value = a[["P(>|Chi|)"]][2],
				p.value = a[['Pr(>Chisq)']][2],	# as of R 2.15.1
				effects0 = coefficients(summary(m0$r))[, "Estimate"],
				sdevs0 = coefficients(summary(m0$r))[, "Std. Error"],
				effects1 = coefficients(summary(m1$r))[, "Estimate"],
				sdevs1 = coefficients(summary(m1$r))[, "Std. Error"]
			)
		}
	),
	glm = list(
		fit = function(formula, data, clusterCol = NULL, ...)glm(formula, data = data, ...),
		compare = function(m1, m0){
			a = anova(m0$r, m1$r, test = "Chisq");
			list(anova = a, m0 = m0, m1 = m1,
				#p.value = a[["P(>|Chi|)"]][2],
				p.value = a[['Pr(>Chi)']][2],	# as of R 2.15.1
				effects0 = coefficients(summary(m0$r))[, "Estimate"],
				sdevs0 = coefficients(summary(m0$r))[, "Std. Error"],
				effects1 = coefficients(summary(m1$r))[, "Estimate"],
				sdevs1 = coefficients(summary(m1$r))[, "Std. Error"]
			)
		}
	),
	lm = list(
		fit = function(formula, data, clusterCol = NULL, ...)lm(formula, data = data, ...),
		compare = function(m1, m0){
			a = anova(m0$r, m1$r);
			list(anova = a, m0 = m0, m1 = m1, p.value = a[["Pr(>F)"]][2],
				effects0 = coefficients(summary(m0$r))[, "Estimate"],
				sdevs0 = coefficients(summary(m0$r))[, "Std. Error"],
				effects1 = coefficients(summary(m1$r))[, "Estimate"],
				sdevs1 = coefficients(summary(m1$r))[, "Std. Error"]
			)
		}
	),
	gee = list(
		fit = function(formula, data, clusterCol, family = binomial(), ...) {
			if (!length(formula.covariates(formula))) return(NULL);
			# geeglm needs ordered clusterIds <!> <N> now pre-processed
			#data = data[order(data[[clusterCol]]), , drop = F];
			#data = Df_(data, headerMap = listKeyValue(clusterCol, 'regrCompClusters'));
			#names(data)[which(names(data) == clusterCol)] = "regrCompClusters";	# hack to make geeglm work
			#geeClusters = as.integer(data[[clusterCol]]);
			myFamily = get(family$family)();
			r = geeglm(formula, data = data, id = regrCompClusters, ...);
			r
		},
		compare = function(m1, m0){
			#a = if (is.null(m0)) anova(m1$r) else anova.geeglm(m0$r, m1$r);
			a = if (is.null(m0)) anova(m1$r) else { anova(m0$r, m1$r); }
			list(anova = a, m0 = m0, m1 = m1, p.value = a[["P(>|Chi|)"]][1],
				effects0 = coefficients(summary(m0$r))[, "Estimate"],
				sdevs0 = coefficients(summary(m0$r))[, "Std.err"],
				effects1 = coefficients(summary(m1$r))[, "Estimate"],
				sdevs1 = coefficients(summary(m1$r))[, "Std.err"]
			)
		}
	),
	geebin = list(
		fit = function(formula, data, clusterCol, ...) {
			if (!length(formula.covariates(formula))) return(NULL);
			# <!> workaround for generic approach that gives obscure error message
			#	gee with family = binomial()
			r = geeglm(formula, data = data, id = regrCompClusters, family = binomial(), ...);
			r
		},
		compare = function(m1, m0){
			#a = if (is.null(m0)) anova(m1$r) else anova.geeglm(m0$r, m1$r);
			a = if (is.null(m0)) anova(m1$r) else { anova(m0$r, m1$r); }
			list(anova = a, m0 = m0, m1 = m1, p.value = a[["P(>|Chi|)"]][1],
				effects0 = coefficients(summary(m0$r))[, "Estimate"],
				sdevs0 = coefficients(summary(m0$r))[, "Std.err"],
				effects1 = coefficients(summary(m1$r))[, "Estimate"],
				sdevs1 = coefficients(summary(m1$r))[, "Std.err"]
			)
		}
	),
	surv = list(
		fit = function(formula, data, clusterCol, ...)
			list(r = coxph(formula, data = data), formula = formula, data = data),
		compare = function(m1, m0){
			vars0 = formula.predictors(m0$r$formula, m0$r$data);
			Nvars0 = length(vars0);
			vars1 = formula.predictors(m1$r$formula, m1$r$data);
			Nvars1 = length(vars1);
			p.value = if (Nvars0 == 0 || (Nvars1 - Nvars0) == 1) {
				v = setdiff(vars1, vars0);
				summary(m1$r$r)$coefficients[v, 'Pr(>|z|)']
			} else {
				anova(m0$r$r, m1$r$r)[2, 'P(>|Chi|)']
			}
			r = list(
				p.value = p.value,
				effects0 = coefficients(summary(m0$r$r)),
				effects1 = coefficients(summary(m1$r$r))
			);
			r
		}
	)
);

CompleteRows = function(f1, data) {
	vars = if (is.character(f1)) f1 else all.vars(as.formula(f1));
	rows = apply(data[, vars, drop = F], 1, function(r)all(!is.na(r)));
	r = which(rows);
	r
}

# <!> clusterIds is needed as argument although just forwarded
regressionFit = function(f, data, type, ...) {
	r = regressionMethods[[type]]$fit(f, data, ...);
	list(type = type, r = r)
}

regressionCompare = function(m1, m0) {
	r = regressionMethods[[m1$type]]$compare(m1, m0);
	r
}

regressionCompareModelsRaw = function(f1, f0, data, type = "lm", clusterCol = NULL, ...) {
	f0 = as.formula(f0);
	f1 = as.formula(f1);
	# <p> jointly trim data according to missing data
	#rows = which(apply(data[, c(formula.vars(f1), clusterCol)], 1, function(r)all(!is.na(r))));
	# more robust version
	row.names(data) = NULL;
	#rows = as.integer(row.names(model.frame(f1, data = data)));
	# robust for random effects
	#rows = apply(data[, all.vars(as.formula(f1)), drop = F], 1, function(r)!any(is.na(r)));
	#d0 = data[rows, ];
	# <p> general cluster processing: order (<!> required for geeglm, rename <!> required for geeglm
	if (Nif(clusterCol)) {
		data = data[order(data[[clusterCol]]), , drop = F];
		data = Df_(data, headerMap = listKeyValue(clusterCol, 'regrCompClusters'));
	}
	d0 = model_matrix_from_formula(f1, data, returnComplete = T)$data;

	# <p> fit and compare models
	m1 = regressionFit(f1, data = d0, type = type, clusterCol = clusterCol, ...);
	m0 = regressionFit(f0, data = d0, type = type, clusterCol = clusterCol, ...);
	a = regressionCompare(m1, m0);
	a
}

permuteDefault = list(
	p.value = 0, sdev.rel = .3, Nchunk = 1e3,
	nuisanceCovariates = NULL, .clRunLocal = T
);
# idCol: used for permutation: column specifying identiy of individuals: could be filled automatically <i>
# permute:
#	sdev.rel: sdev relative to p.value to decide how often to permute
regressionCompareModels = function(f1, f0, data, type = "lm", clusterCol = NULL, ...,
	permute = permuteDefault) {
	permute = merge.lists(permuteDefault, permute);
	r = regressionCompareModelsRaw(f1, f0, data, type, clusterCol, ...);

	if (!is.null(r) && !is.null(r$p.value) && !is.na(r$p.value) && r$p.value < permute$p.value)
		r = regressionCompareModelsPermuted(f1, f0, data, type, clusterCol, ..., permute = permute);
	r
}

#
#	<p> permuted cluster regression
#

regressionCompareModelsPermuted = function(f1, f0, data, type = "lm", clusterCol = "cluster", ...,
	idCol = "id", permute = permuteDefault, Nmax = 1e5) {
	# <p> data p-value
	a.data = regressionCompareModelsRaw(f1, f0, data, type, clusterCol = clusterCol, ...);
	p.data = a.data$p.value;
	# <p> logging
	Log(sprintf("Permuting Regression: %s [p = %.2e]", paste(as.character(f1), collapse = " "), p.data), 4);
	# <p> permutation variables indeces
	pvs = setdiff(formula.covariates(f1), permute$nuisanceCovariates);

	# <p> precompute cluster data structure
	cls = clusterRelation2list(data.frame(id = 1:length(data[[clusterCol]]), idFam = data[[clusterCol]]))
	ps = NULL;
	d0 = data;
	# adaptive permutation
	repeat {
		ps0 = clapply(1:permute$Nchunk, function(i, f1, f0, data, type, clusterCol, cls, pvs){
			d0[, pvs] = if (is.null(clusterCol)) d0[sample(1:(dim(data)[1])), pvs] else
				d0[permuteClusters(cls), pvs];
			r = regressionCompareModelsRaw(f1, f0, d0, type, clusterCol, ...);
			r$p.value
		}, f1 = f1, f0 = f0, data = data, type = type, clusterCol = clusterCol, cls = cls, pvs = pvs,
		.clRunLocal = permute$.clRunLocal);
		ps0 = na.exclude(as.numeric(ps0));
		ps = c(ps, ps0);
		#print(ps[1:100]);
		p.emp = fraction(ps <= p.data);
		# <p> stopping criterion
		p.break = if (p.emp == 0) 1 / length(ps) else p.emp;
		sdev.rel = sqrt(p.break * (1 - p.break) / length(ps)) / p.break;
		#print(list(sd = sdev.rel * p.break, sd.rel = sdev.rel, p = p.emp));
		if (sdev.rel <= permute$sdev.rel) break;
		# <p> final stop
		if (length(ps) >= Nmax) break;
	};
	r = list(f1 = f1, f0 = f0, p.value = p.emp, p.data = p.data, anova = a.data$anova, ps = ps);
	r
}

# permute covariates in order to obtain empirical p-values
#	f1: model formula alternative
#	f0: model formula hypothesis
#	M: number of permutations
regressionCompareModelsEmp = function(f1, f0, data, nuisanceCovariates = c(), type = "lm", M = 1e3, ...,
	idName = "id", idClusterName = "cluster", .clRunLocal = T) {
	r = regressionCompareModelsPermuted(f1, f0, type, ..., clusterCol = idClusterName, idCol = idName,
		permute = list(Nchunk = M, nuisanceCovariates = nuisanceCovariates, .clRunLocal = .clRunLocal));
	r
}

# data: data.frame
# stat: function computing test statistic
# vars: formula for permuation
# Nperm: number of permutations
# Pvalue: c('upper', 'lower', 'two.tailed')
permute = function(data, stat, vars, ..., Nperm = 5e3, Pvalue = 'lower', na.rm = T, fracBadStatThres = .01,
	returnT = TRUE) {
	perm.vars = all.vars(as.formula(vars));
	f = function(i, ...) {
	};
	Ts = Sapply(0:Nperm, function(i, data, ...) {
		if (i > 0) data[, perm.vars] = data[sample(nrow(data)), perm.vars];
		stat(data, ...)
	}, data = data, ...);

	fracBadStatistics = mean(is.na(Ts[-1]));
	if (is.na(Ts[1]) || fracBadStatistics >= fracBadStatThres) return(list(p.value = NA));
	Ts = Ts[!is.na(Ts)];
	Tdata = Ts[1];
	Ts = Ts[-1];
	Plower = (1 + sum(Ts <= Tdata)) / Nperm;
	Pupper = (1 + sum(Ts >= Tdata)) / Nperm;
	p.value = switch(Pvalue,
		lower = Plower,
		upper = Pupper,
		two.tailed = 2 * min(Plower, Pupper)
	);
	r = if (returnT)
		list(p.value = p.value, t.data = Tdata, t.perm = Ts) else
		list(p.value = p.value, t.data = Tdata);
	r
}

calibrate = function(f1, data, regression = glm, family = binomial(), quantiles = seq(0, 1, 0.25), ...) {
	stat0 = regression(f1, data, family = family);
	scores = predict(stat0, data, type = 'link');
	ps = predict(stat0, data, type = 'response');
	y = data[[formula.response(f1)]];
	groups = quantile(scores, probs = quantiles, na.rm = FALSE);

	calibration = lapply(2:length(quantiles), function(i) {
		group = scores > groups[i - 1] & scores <= groups[i];
		c(mean(y[group]), mean(ps[group]), sum(group))
	});
	c0 = Df(do.call(rbind, calibration),
		row.names = paste0(sprintf('%.1f', quantiles[-1] * 100), '%'),
			names = c('truth', 'prediction', 'N'));
	r = list(calibration = c0, model = summary(stat0), quantiles = groups[-1], scores = scores);
	r
}

#
#	<p> P-values
#

alphaLevels = function(ps, Nalpha = 40, alphas = seq(1/Nalpha, 1 - 1/Nalpha, length.out = Nalpha)) {
	lvls = setNames(sapply(alphas, function(alpha)mean(ps < alpha)), round(alphas, 2));
	lvls
}

testSizes = function(ps, levels = seq(0, 1, length.out = 21), asDf = FALSE, scale = 1, round = NULL, diff = FALSE) {
	sizes = sapply(levels, function(l) fraction(ps <= l));
	if (diff) sizes = sizes - levels;
	r = if (asDf)
		Df(level = levels, size = sizes) else
		SetNames(sizes, Sprintf("%{l}.1f%%", l = levels*100));
	r = r * scale;
	if (notE(round)) r = round(r, round);
	return(r);
}
TestSizes = function(ps, levels = seq(0, 1, length.out = 21), asDf = FALSE, scale = 100, round = 2)
	testSizes(ps, levels, asDf, scale, round)

testSizePlot = function(ps, ..., diagonal = 'red') {
	psP = testSizes(ps, ..., asDf = TRUE);
	ggplot(psP, aes(x = levels, y = size)) + geom_line() +
		geom_abline(intercept = 0, slope = 1, col = diagonal)

}

#
#	<p> error propagation
#

# as derived from the RB project and tested therein

errProd = function(x, sdx, y, sdy, covxy = 0) {
	sdp = (x * y) * sqrt((sdx/x)^2 + (sdy/y)^2 + 2 * sdx * sdy * covxy);
	sdp
}

errFrac = function(x, sdx, y, sdy, covxy = 0) {
	sdp = (x / y) * sqrt((sdx/x)^2 + (sdy/y)^2 - 2 * sdx * sdy * covxy);
	sdp
}

errSum = function(sdx, cx = 1, sdy = 0, cy = 1, covxy = 0) {
	sds = sqrt((cx *sdx)^2 + (cy * sdy)^2 + 2 * cx * cy * covxy);
	sds
}

#
#	<§> some general statistical transformations
#

# convert confidence interval to standard dev based on a normality assumption
ciToSd_old = function(ci.lo, ci.up, level = .95) {
	# upper centered limit
	ciU = ci.up - mean(c(ci.lo, ci.up));
	span = ci.up - ci.lo;
	# corresponding sd
	sd = Vectorize(inverse(function(s)qnorm(1 - (1 - level)/2, 0, s), interval = c(0, span * 8)))(ciU);
	sd
}
ciToSd = function(ci.lo, ci.up, level = .95) {
	if (missing(ci.up)) {
		if (is.null(dim(ci.lo))) {
			ci.up = ci.lo[2];
			ci.lo = ci.lo[1];
		} else {
			ci.up = ci.lo[, 2];
			ci.lo = ci.lo[, 1];
		}
	}
	alpha = 1 - level;
	widthStd = qnorm(1 - alpha/2) * 2;
	(ci.up - ci.lo)/widthStd
}
ciToP = function(ci.lo, ci.up, level = .95, one.sided = F, against = 0) {
	sd = ciToSd(ci.lo, ci.up, level)
	P = peSdToP((ci.lo + ci.up)/2 - against, sd, one.sided);
	P
}
# convert point estimate and SD to p-value (assuming normality)
betaSe2P = peSdToP = function(beta, sd, one.sided = F) {
	pnorm(-abs(beta), 0, sd, lower.tail = T) * ifelse(one.sided, 1, 2);
}

betaSdToCi = ciFromBetaSdev = function(beta, sdev, level = .95) {
	r = list(effect = beta,
		lower = qnorm((1 - level)/2, beta, sdev, lower.tail = T),
		upper = qnorm((1 - level)/2, beta, sdev, lower.tail = F)
	);
	r
}

ciFromSummary = function(s, var, level = .95) {
	cs = as.matrix(coefficients(s)[var, ]);
	ciFromBetaSdev(cs[, 'Estimate'], cs[, 'Std. Error'], level = level);
}

pFromBetaSd = function(beta, sd, null = 0)pnorm(null, abs(beta), sd)
sdFromBetaP = function(beta, p)Vectorize(inverse(function(s)peSdToP(beta, s), interval = c(0, 10)))(p);
betaPtoCi = function(beta, p) {
	sd = sdFromBetaP(beta, p);
	ciFromBetaSdev(beta, sd)
}
betaVarToCi = function(beta, var)ciFromBetaSdev(beta, sqrt(var))

coefficientsOR = function(s, level = .95) {
	cis = do.call(cbind, ciFromSummary(s, level = level));

	# <p> ORs
	cisOR = Df_(exp(cis));
	cisOR['(Intercept)', ] = NA;	# intercept does not have an OR interpretation
	names(cisOR) = paste(names(cisOR), 'OR', sep = '');

	# <p> result
	cfs = coefficients(s);
	r = cbind(cfs, cis[, -1], as.matrix(cisOR));
	r
}
coefficientsORmodel = function(model, level = .95)coefficientsOR(summary(model), level)

#
#	meta analysis
#

# meta analysis row-wise
metaPvalue = function(ps) {
	if (!is.matrix(ps)) ps = matrix(ps, nrow = 1);
	if (!all(is.numeric(ps))) ps = apply(ps, 1:2, as.numeric);
	cs = apply(ps, 1, function(r)sum(-2*log(r)));
	psM = pchisq(cs, 2*dim(ps)[2], lower.tail = F);
	psM
}

#
#	data imputation
#

Sample = function(x, ...)if (length(x) == 1)x else sample(x, ...);

mi.simple = function(data, n.imp = 20) {
	r = lapply(1:n.imp, function(i) {
		for (v in names(data)) {
			data[is.na(data[, v]), v] = Sample(na.omit(data[, v]), count(is.na(data[, v])));
		}
		data
	})
	r
}

cross.imputer = function(imputationData, imputationVars = NULL, doExpandFactors = T) {
	if (is.null(imputationVars)) imputationVars = names(imputationData);
	f = function(data) {
		d0 = data;
		for (v in imputationVars) { # cross impute from imputationData
			d0[is.na(d0[, v]), v] = Sample(na.omit(imputationData[[v]]), count(is.na(d0[, v])));
		}
		if (doExpandFactors) d0 = dataExpandFactors(d0)[, vars];
		d0
	};
	f
}

imputeMeanVar = function(col) {
	mn = mean(col, na.rm = T);
	col[is.na(col)] = mn;
	col
}
imputeMean = function(data) {
	d1 = apply(data, 2, imputeMeanVar);
	d1
}

# impute using mice
imputeVariables = function(data, formula = NULL, vars = NULL, Nimp = 10, returnOriginal = FALSE,
	imputationColumn = 'Imputation_', returnMice = FALSE) {
	require('mice');

	# <p> preparation
	if (notE(formula)) vars = all.vars(formula.re(formula, data = data));
	invalidVars = vars[!lapply(data[, vars], class) %in% c('factor', 'numeric')];
	if (length(invalidVars) > 0) {
		stop(Sprintf('some columns neither factor nor numeric: %{v}s', v = join(invalidVars, ', ')));
	}
	s = descriptives(data[, vars]);
	
	# <p> imputation
	dataImputed = mice(data[, vars, drop = F], m = Nimp);
	dataL = Df_(complete(dataImputed, "long", include = returnOriginal),
		min_ = c('.id'), headerMap = list(`.imp` = imputationColumn), as_numeric = imputationColumn);

	dataC = DfStack(data, (Nimp + returnOriginal));
	dataC[, vars] = Df_(dataL, min_ = imputationColumn);
	dataR = Df(dataL[, imputationColumn], dataC, names = imputationColumn);
	r = list(data = dataR, summary = s, vars = vars);
	if (returnMice) r = c(r, list(mice = dataImputed));
	#r = list(data = dataImputed, summary = s, vars = vars);
	r
}

missingness = function(d, f0) {
	ns = if (missing(f0)) names(d) else
		if (class(f0) == 'formula') all.vars(f0) else f0;
	r = lapply(ns, function(n) {
		list(N = sum(is.na(d[[n]])), freq = mean(is.na(d[[n]])))
	});
	return(SetNames(do.call(rbind, r), rnames = ns));
}

Missingness = function(d, f0) {
	dM = Df_(missingness(d, f0), deep_simplify_ = TRUE);
	dM$variable = row.names(dM);
	dM$perc = Sprintf('%{perc}.1f%%', perc = dM$freq * 100);
	dM$freq = Sprintf('%{freq}.3f', dM);
	row.names(dM) = NULL;
	
	return(dM[, c('variable', 'N', 'freq', 'perc')]);
}


#
#	<p> cross validation
#

# cross validation partitions for classification data
crossValidationPartitionsClassification = function(responses, K = 15, minEls = 3, maxTries = 15) {
	N = length(responses);
	cats = unique(responses);

	for (i in 1:maxTries) {
		# random permutation
		perm = sample(1:N, N);
		# compute partitions
		parts = splitListEls(perm, K, returnElements = T);
		counts = data.frame.types(
			lapply(parts, function(p)table.n(responses[-p], categories = cats)),
			names = cats, do.rbind = T
		);
		doReject = any(apply(counts, 1, function(r)any(r < minEls)));
		if (!doReject) break;
	}
	r = if (i < maxTries) parts else {
		Log("Error: failed to find suitable cross validation partition!");
		NULL
	}
	r
}

# cross validation parititions for clustered data
# return indeces into cluster vector (cluster identities assumed to be given by integers)
# so far do not heed cluster sizes
crossValidationPartitionsClusters = function(clusters, K = 20) {
	N = length(clusters);
	# unique cluster ids
	cls = unique(clusters);
	# random permutation
	perm = Sample(cls, length(cls));
	# compute partitions
	parts = splitListEls(perm, K, returnElements = T);
	r = lapply(parts, function(p)which.indeces(p, clusters, match.multi = T));
	r
}

#
#	<p> optimization
#

nested.search = function(f, ..., key = NULL, parameters = list(p1 = c(0, 10)),
	steps = 3, Ngrid = 4, rscale = 1, return.grid = F, par.as.vector = F, .clRunLocal = rget('.clRunLocal')) {
	ps = ps0 = parameters;

	for (i in 1:steps) {
		# <p> create serach grid
		pars = lapply(ps, function(p)seq(p[1], p[2], length.out = Ngrid));
		grid = merge.multi.list(pars);
		# <p> apply function
		r = clapply(1:dim(grid)[1], function(j, grid, ...) {
			args = if (par.as.vector) list(as.vector(grid[j, ]), ...) else c(as.list(grid[j, ]), list(...));
			do.call(f, args);
		}, grid = grid, ..., .clRunLocal = .clRunLocal);
		# <p> search optimum
		values = if (is.null(key)) r else list.kp(r, key, do.unlist = T);
		opt = which.min(values * rscale);
		pt = grid[opt, ];	# optimal point in the grid search

		ps = lapply(1:length(ps), function(j){
			from = max(pt[j] - (ps[[j]][2] - ps[[j]][1])/Ngrid, ps0[[j]][1]);
			to =   min(pt[j] + (ps[[j]][2] - ps[[j]][1])/Ngrid, ps0[[j]][2]);
			c(from, to)
		});
		names(ps) = names(ps0);
	}
	r = if (return.grid) list(value = values[[opt]], par = pt, grid = r) else
		list(value = values[[opt]], par = pt, r = r[[opt]]);
	r
}
optim.nested.defaults = list(steps = 5, Ngrid = 4, rscale = 1, return.grid = F);
optim.nested = function(par = NULL, f, ..., lower = -Inf, upper = Inf, control = list())
	with(merge.lists(optim.nested.defaults, control), {
	parameters = apply(cbind(lower, upper), 1, function(r)list(r));
	r = nested.search(f, ..., parameters,
		steps = steps, Ngrid = Ngrid, rscale = rscale, return.grid = return.grid, par.as.vector = T);
	r
})

#
#	<p> correlation in data
#

Df.corr = function(df, eps = 1e-2) {
	N = dim(df)[2];
	rc = rcorr(df);
	pairs = t(sapply(which(abs(rc$r) > (1 - eps)), function(e) {
		row = ((e - 1) %/% N) + 1;
		col = ((e - 1) %% N) + 1;
		r = c(row, col);
		r
	}));
	pairs = pairs[pairs[, 1] < pairs[, 2], ];
	clusters = sub.graph(pairs);
	remove = unlist(lapply(clusters, function(e)e[-1]));
	r = list(clusters = clusters, cols.remove = remove);
	r
}

identity = function(e)e
seq.transf = function(from = 0, to = 1, length.out = 1e1, ..., transf = log, transfI = exp, eps = 1e-5) {
	s = transfI(seq(from = transf(from + eps), to = transf(to - eps), length.out = length.out, ...));
	s
}

#
#	<p> bug fixes for packages
#

completeRows = function(data)!apply(data, 1, function(r)any(is.na(r)))
completeData = function(f = ~ ., data, collapse = F, varExclude = c()) {
	vsRaw = if (is.character(f)) f else all.vars(f);
	if (any(vsRaw == '.')) vsRaw = names(data);
	vs = setdiff(vsRaw, varExclude);
	data[completeRows(data[, vs, drop = F]), if (collapse) vs else 1:ncol(data), drop = F]
}

model_matrix_from_formula = function(f, data, offset = NULL, ignore.case = F, remove.intercept = F,
	returnComplete = F, formulaAsIs = F) {
	# <p> prepare data matrices
	f1 = if (formulaAsIs) f else formula.re(f, data = data, ignore.case = ignore.case);
	f1vars = all.vars(f1);
	response = formula.response(f1);
	responseVars = all.vars(as.formula(con(response, ' ~ 1')));

	# <p> complete data
	complete = completeRows(data[, f1vars, drop = F]);
	data = droplevels(data[complete, , drop = F]);
	
	# <p> response
	responseData = if (notE(responseVars)) with(data, Eval(response)) else NULL;
	# <p> offset
	offset = if (!is.null(offset)) offset[complete] else NULL;
	# <p> model matrix
	#mm = if (exists('lFormula') && length(unlist(Regex('[+]\\s*[(]', Deparse(f1)))) > 0)
	mm = if (exists('lFormula') && length(unlist(Regex('[+]\\s*[(].*?[|].*[)]', Deparse(f1)))) > 0)
		lFormula(f1, data)$X else	# lme4
		model.matrix(f1,  model.frame(f1, data = data));
	if (remove.intercept) mm = mm[, !(dimnames(mm)[[2]] == '(Intercept)'), drop = F];

	# <p> return
	r = list(mm = mm, response = responseData, offset = offset, indeces = which(complete),
		varsCov = setdiff(f1vars, responseVars), varsResponse = responseVars);
	if (returnComplete) r = c(r, list(data = data));
	r
}
complete_from_formula = function(f, data, offset = NULL, ignore.case = F, remove.intercept = F) {
	model_matrix_from_formula(f, data, offset, ignore.case, remove.intercept)$indeces
}
complete_from_vars = function(vars, data, offset = NULL, ignore.case = F, remove.intercept = F) {
	f = as.formula(Sprintf('~ %{vars}s', vars = join(vars, ' + ')));
	model_matrix_from_formula(f, data, offset, ignore.case, remove.intercept)$indeces
}

# <!><t> tb tested refactoring as of 19.2.2019

glmnet_re_mm = function(y, mm, ..., offset = NULL, ignore.case = F, remove.intercept = F,
	lambdas = NULL, cv = T, returnGlmnet = F) {
	# <p> fit model
	r = if (cv) {
		r0 = cv.glmnet(x = mm, y = y, lambda = lambdas, ..., offset = offset);
		args = c(List(..., min_ = c('foldid', 'nfolds', 'grouped')),
			list(x = mm, y = y, lambda = r0$lambda.min, offset = offset));
# 			list(x = mm, y = y, lambda = (3*r0$lambda.min + r0$lambda.1se)/4, offset = offset));
#			list(x = mm, y = y, lambda = (r0$lambda.min), offset = offset));
		do.call('glmnet', args);
	} else glmnet(x = mm, y = y, lambda = lambdas, ..., offset = offset);
	if (returnGlmnet) r = list(glmnet = r);
	return(r);
}

glmnet_re = function(f, data, ..., offset = NULL, ignore.case = F, remove.intercept = F,
	lambdas = NULL, cv = T, returnGlmnet = F) {
	d = model_matrix_from_formula(f, data, offset, ignore.case, remove.intercept);
	r = glmnet_re_mm(d$response, d$mm, ..., offset = d$offset,
		ignore.case = ignore.case, remove.intercept = remove.intercept, lambdas = lambdas,
		cv = cv, returnGlmnet = returnGlmnet);
	return(c(r, list(formula = f)));
}
glmnet_re_refit = function(model, data, ..., var_cutoff =  1e-6, intercept = '1', impute = NULL) {
	response = formula.response(model$formula);

	if (model$df <= 1) return(list());
	# <p> scrutinize model
	coefs = model$beta;
	varsSel = row.names(coefs)[abs(as.vector(coefs)) > var_cutoff];
	varsSel = setdiff(varsSel, '(Intercept)');
	
	if (!is.null(impute) && impute == 'mean') {
		# <!> use model matrix <i>
		d0 = sapply(varsSel, function(var) {
			data[[var]][is.na(data[[var]])] = mean(data[[var]], na.rm = T);
		});
		data[, varsSel] = d0;
	}
	# <p> refit
	f = as.formula(sprintf('%s ~ %s', response, paste(c(intercept, varsSel), collapse = ' + ')));
	glm1 = glm(f, data = data, ...);
	r0 = list(glm = glm1, score = as.vector(predict(glm1, data, type = 'link')))
	r0
}

#library('glmnet');
grid.glmnet.raw = function(..., glmnet.f = cv.glmnet, max.tries = 3) {
	for (i in 1:max.tries) {
		fit = try(glmnet.f(...), silent = T);
		if (all(class(fit) != 'try-error')) break();
	}
	if (any(class(fit) == 'try-error')) stop(fit[1]);
	fit
}

grid.glmnet.control = list(steps = 4, Ngrid = 50, from = .01,  to = .8, eps = 1e-5,
		transf = identity, transfI = identity);

grid.glmnet = function(..., control = grid.glmnet.control)
	with (merge.lists(grid.glmnet.control, control), {
	# initialize
	fit = NULL;
	fromO = from;
	toO = to;
	options(warn = -1);
	for (i in 1:steps) {
		lambda = seq.transf(from, to, length.out = Ngrid + 1, eps = eps,
			transf = transf, transfI = transfI);
		fit = grid.glmnet.raw(..., lambda = sort(lambda, decreasing = T));
		from = max(fit$lambda.min - (to - from)/Ngrid, 0);
		to = fit$lambda.min + (to - from)/Ngrid;
	}
	options(warn = 0);
	# choose lambdas to contain lambda.min also covering the range between from and to
	lambda = c(
		seq.transf(fromO, toO, length.out = Ngrid + 1, eps = eps,
			transf = transf, transfI = transfI),
		fit$lambda.min
	);
	fit0 = do.call('grid.glmnet.raw', c(list(...), list(lambda = sort(lambda, decreasing = T))));
	args = List(..., min_ = c('nfolds', 'grouped'));
	fit1 = do.call('grid.glmnet.raw', c(args, list(lambda = fit$lambda.min, glmnet.f = glmnet)));
	r = fit0;
	r$glmnet.fit = fit1;
	r
})

#	f: formula, passed through formula.re
#	data: data frame
grid.glmnet.re = function(f, data, ..., offset = NULL, control = grid.glmnet.control,
	ignore.case = F, remove.intercept = T)
	with (merge.lists(grid.glmnet.control, control), {

	# <p> prepare data matrices
	f1 = formula.re(f, data = data, ignore.case = ignore.case);
	f1vars = all.vars(f1);
	response = formula.response(f1);
	complete = !apply(data[, f1vars], 1, function(r)any(is.na(r)));
	d1 = data[complete, ];
	if (!is.null(offset)) offset = offset[complete];
	mm = model.matrix(f1, model.frame(f1, data = d1));
	if (remove.intercept) mm = mm[, !(dimnames(mm)[[2]] == '(Intercept)')];
	# <p> fit model
	r = grid.glmnet(x = mm, y = d1[[response]], ..., offset = offset, control = control);
	r = c(r, list(formula = f1));
	r
})

grid_glmnet_re_refit = function(model, data, ..., var_cutoff =  1e-6, intercept = '1', impute = NULL) {
	# <p> scrutinize model
	coefs = coefficients(model$glmnet.fit);
	varsSel = row.names(coefs)[abs(as.vector(coefs)) > var_cutoff];
	varsSel = setdiff(varsSel, '(Intercept)');
	response = formula.response(model$formula);

	if (!is.null(impute) && impute == 'mean') {
		# <!> use model matrix <i>
		d0 = sapply(varsSel, function(var) {
			data[[var]][is.na(data[[var]])] = mean(data[[var]], na.rm = T);
		});
		data[, varsSel] = d0;
	}
	# <p> refit
	f = as.formula(sprintf('%s ~ %s', response, paste(c(intercept, varsSel), collapse = ' + ')));
	glm1 = glm(f, data = data, ...);
	r0 = list(glm = glm1, score = as.vector(predict(glm1, data, type = 'link')))
	r0
}

refitModel = function(model, f1, f0, data, ..., var_cutoff =  1e-6, ignore.case = F, intercept = '0') {
	# <p> prepare formula and data set
	f1 = formula.re(f1, data = data, ignore.case = ignore.case);
	f0 = formula.re(f0, data = data, ignore.case = ignore.case);
	f0covs = formula.covariates(f0);
	f1vars = all.vars(f1);
	response = formula.response(f1);
	complete = complete.cases(data[, f1vars]);	#!apply(data[, f1vars], 1, function(r)(any(is.na(r))));
	d1 = data[complete, ];
	# <p> extract data set according to model
	coefs = coefficients(model);
	varsSel = row.names(coefs)[abs(as.vector(coefs)) > var_cutoff];
	varsSel = setdiff(varsSel, '(Intercept)');
	varsSel0 = intersect(varsSel, f0covs);
	if (!length(varsSel0)) return(
		list(coefficients = coefs, anova = NA, r2 = NA, r20 = NA, raw = NA, model1 = NA, model0 = NA)
	);
	# <p> re-fit glm
	f1 = as.formula(sprintf('%s ~ %s', response, paste(c(intercept, varsSel), collapse = ' + ')));
	glm1 = glm(f1, data = d1, ...);
	f0 = as.formula(sprintf('%s ~ %s', response, paste(c(intercept, varsSel0), collapse = ' + ')));
	glm0 = glm(f0, data = d1, ...);
	# <p> anova
	a = anova(glm0, glm1, test = 'Chisq');
	# <p> R^2
	mn = mean(d1[[response]]);
	#mm = model.matrix(f1, model.frame(f1, data = d1));
	pr = as.vector(predict(glm1, d1, type = 'response'));
	#r2 = cor((pr - mn), (d1[[response]] - mn));
	r2 = cor(pr, d1[[response]]);
	pr0 = as.vector(predict(glm0, d1, type = 'response'));
	#r20 = cor((pr0 - mn), (d1[[response]] - mn));
	r20 = cor(pr0, d1[[response]]);
	# <p> raw-model fit
	fScore = as.formula(sprintf('y ~ score + %s', paste(c(intercept, varsSel0), collapse = ' + ')));
	d2 = data.frame(
		d1[, varsSel0], y = d1[[response]], score = as.vector(predict(glm1, d1))
	);
	if (length(varsSel0)) names(d2)[1:length(varsSel0)] = varsSel0;
	raw = glm(fScore, data = d2, ...);
	r = list(coefficients = coefs, anova = a, r2 = r2, r20 = r20,
		raw = summary(raw), model1 = glm1, model0 = glm0);
	r
}

#
#	<p> crossvalidation
#

# <!> tb implemented
cv_summary_lm = function(model, pred, data, ...) {
	summary(r0)$fstatistic[1]
	r = mean( (pred - data)^2 );
	r
}

cv_test_glm = function(model, formula, data, ...) {
	response = formula.response(formula);
	responseP = predict(model, data, type = 'response');
	responseD = data[[response]];
	ll = sum(log(responseP));
	ll
}

cv_train_glmnet = function(f1, data, ...) {
	glmnet_re(f1, data, ..., returnGlmnet = T);
}
cv_test_glmnet = function(model, f1, data, ...) {
	preds = predict(model$glmnet, model_matrix_from_formula(f1, data)$mm, type = 'response');
	# detect logistic model, constrain probabilities
	if (notE(model$glmnet$classnames) && length(model$glmnet$classnames) == 2) preds = minimax(preds, 0, 1);
	return(preds);
}

cv_train_glmnet_mm = function(data, ...) {
	i = FirstDef(which(dimnames(data)[[2]] == 'y'), 1);
	glmnet_re_mm(data[, i], data[, -i], ..., returnGlmnet = T);
}
cv_test_glmnet_mm = function(model, data, ...) {
	i = FirstDef(which(dimnames(data)[[2]] == 'y'), 1);
	preds = predict(model$glmnet, data[, -i], type = 'response');
	# detect logistic model, constrain probabilities
	if (notE(model$glmnet$classnames) && length(model$glmnet$classnames) == 2) preds = minimax(preds, 0, 1);
	return(preds);
}


cv_train_glm = function(f1, data, ..., family = binomial()) {
	glm(f1, data = data, family = family)
}
cv_test_glm_predict = function(model, f1, data, ..., type = 'response') {
	predict(model, data, type = type)
}


# cv_prepare = function(data, argsFrom...)
# cv_train = function(data, argsFrom...)
# cv_test = function(model, data, argsFrom...)
# @arg cv_fold number of crossvalidation folds, denotes leave -cv_fold out if negative
# <!> 19.2.2019: removed cv_prepare for lack of functionality
# <i> argument routing to train/test

crossvalidate = function(cv_train, cv_test,
	data, cv_fold = 20, cv_repeats = 1, ..., parallel = F, align_order = TRUE) {
	if (cv_fold == 0) stop('crossvalidate: cv_fold must be an integer != 0');
	if (!parallel) Lapply = lapply;
	N = nrow(data);
	r = Lapply(1:cv_repeats, function(i, ...) {
			perm = Sample(1:N, N);
			# compute partitions
			fold = if (cv_fold > 0) cv_fold else as.integer(N/-cv_fold);
			parts = splitListEls(perm, fold, returnElements = T);
			o = order(unlist(parts));
			r = Lapply(parts, function(part, cv_train, cv_test, data, cv_repeats, ...) {
				d0 = data[-part, , drop = F];
				d1 = data[part, , drop = F];
				model = cv_train(..., data = d0);
				r = cv_test(model = model, ..., data = d1);
				gc();
				r
			}, cv_train = cv_train, cv_test = cv_test,
				data = data, cv_repeats = cv_repeats, ...);
			# re-establish order
			r = if (align_order
				&& all(sapply(r, class) %in% c('numeric', 'integer'))
				#&& all(sapply(r, length) == 1)) {
				&& sum(sapply(r, length)) == nrow(data)) {
				unlist(r)[o];
			} else if (align_order && all(sapply(r, class) %in% c('data.frame', 'matrix')) &&
				sum(sapply(r, nrow)) == nrow(data)) {
				#<!> untested
				#r = rbindDataFrames(r, colsFromFirstDf = T);
				r = do.call(rbind, r);
				r[o, ]
			} else if (align_order) stop("Crossvalidate: didn't know how to align order.") else r;
			gc();
			r
	}, ...);
	r
}

#
#	<p> data standardization
#

standardize = function (x, ...)UseMethod("standardize", x);
standardize.numeric = function(v, na.rm = TRUE, orig.mean = FALSE) {
	vM = mean(v, na.rm = na.rm);
	vC = v - vM;
	vS = vC / sd(vC, na.rm = na.rm);
	if (orig.mean) vS + vM else vS
}
standardize.data.frame = function(df, na.rm = TRUE)apply(df, 2, standardize.numeric, na.rm = na.rm);
standardize.matrix = function(m, na.rm = TRUE)apply(m, 2, standardize.numeric, na.rm = na.rm);

df2z = function(data, vars = names(as.data.frame(data))) {
	data = as.data.frame(data);
	df = data.frame.types(sapply(vars, function(v) {
		(data[[v]] - mean(data[[v]], na.rm = T)) / sd(data[[v]], na.rm = T)
	}), do.transpose = F);
	i = which.indeces(vars, names(data));
	d0 = data.frame(data[, -i], df);
	d0
}

lumpFactor = function(factor, minFreq = NULL, minN = 20, levelPrefix = 'l') {
	# <p> preparation
	f0 = as.factor(factor);
	t0 = table(f0);
	ls = levels(f0);
	N = length(f0);
	if (!is.null(minFreq)) minN = as.integer(minFreq * N + 0.5);
	
	# <p> lumping
	map = listKeyValue(ls, ls);
	for (i in 1:length(t0)) {
		t0 = table(factor);
		if (all(t0 >= minN) || length(t0) < 2)  break;
		# combine two smallest groups
		t1 = sort(t0);
		newLevel = sprintf('%s%d', levelPrefix, i);
		factor = as.character(factor);
		factor[factor == names(t1)[1] | factor == names(t1)[2]] = newLevel;
		map[[names(t1)[1]]] = map[[names(t1)[2]]] = newLevel;
		map[[newLevel]] = newLevel;
	}
	# <p> normalize map
	lsNew = as.character(ls);
	repeat {
		lsNew0 = lsNew;
		lsNew = as.character(map[lsNew]);
		if (all(lsNew == lsNew0)) break;
	}
	return(list(map = listKeyValue(ls, lsNew), factor = factor));
}

# lump a variable after checking other variables for non-missingness
lumpVariableOnVariables = function(data, var, vars, postfix = '_lump', minN = 20) {
	# prepare confounder afkomst
	lump = sapply(vars, function(v) {
		dvar = data[[var]][!is.na(data[[v]])];
		lump = lumpFactor(dvar, minN = minN);
		dvarNew = as.character(lump$map[as.factor(data[[var]])]);
		dvarNew[dvarNew == 'NULL'] = NA;
		as.factor(dvarNew)
	});
	d = data.frame(lump);
	names(d) = paste(var, paste(vars, postfix, sep = ''), sep = '_');
	d
}

#
#	<p> descriptive
#

compareVectors = function(l) {
	sets = names(l);
	# marginals
	r0 = nlapply(sets, function(n)c(n, length(l[[n]])));
	r1 = nlapply(sets, function(n)c(sprintf('%s-unique', n), length(unique(l[[n]]))));
	r2 = nlapply(sets, function(n)c(sprintf("%s-NA", n), sum(is.na(l[[n]]))));

	modelList = list(A = sets, B = sets);
	r3 = iterateModels(modelList, .constraint = function(A, B)(A < B), function(i, A, B) {
		r = list(
			c(sprintf("%s inter %s", A, B), length(intersect(l[[A]], l[[B]]))),
			c(sprintf("%s union %s", A, B), length(union(l[[A]], l[[B]]))),
			c(sprintf("%s min %s", A, B), length(setdiff(l[[A]], l[[B]]))),
			c(sprintf("%s min %s", B, A), length(setdiff(l[[B]], l[[A]])))
		);
		r
	}, lapply__ = lapply)$results;

	r = c(r0, r1, r2, unlist.n(r3, 1));
	r = data.frame.types(r, do.rbind = T, names = c('type', 'count'));
	r
}

pairs_std.panel.hist <- function(x, ...) {
	usr <- par("usr"); on.exit(par(usr));	# restore settings
	par(usr = c(usr[1:2], 0, 1.5) )
	h <- hist(x, plot = FALSE)
	breaks <- h$breaks; nB <- length(breaks)
	y <- h$counts; y <- y/max(y)
	rect(breaks[-nB], 0, breaks[-1], y, col = "grey", ...)
}
pairs_std.panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
	usr <- par("usr"); on.exit(par(usr));
	# <p> hack to get column names
	nsAll = eval(expression(names(x)), parent.frame());
	#nsV = nsAll[c(eval(expression(i), parent.frame()), eval(expression(j), parent.frame()))];
	# <!> seems to be order (j, i) in terms of names 16.3.2021
	nsV = nsAll[c(eval(expression(j), parent.frame()), eval(expression(i), parent.frame()))];

	par(usr = c(0, 1, 0, 1))
	c0 = cor.test(x, y);
	Sx = summary(x);
	Sy = summary(y);
	Sums = RbindDfs(list(list2df(Sx), list2df(Sy)), embed = TRUE);	# summaries
	if (!('NA.s' %in% names(Sums))) Sums = Df(Sums, NA.s = NA);
	#sum = apply(cbind(Sy, Sx), 1:2, sprintf, fmt = '%.3f');
	sum = apply(Sums, 1:2, sprintf, fmt = '%.3f');
	ns = c('Min', '1st Q', 'Median', 'Mean', '3rd Q', 'Max', 'NAs'); 
	#ns = names(summary(x));
	txt <- paste0(prefix,
		sprintf("Cor: %.2f (%.2f, %.2f)\n", c0$estimate, c0$conf.int[1], c0$conf.int[2]),
		sprintf("P: %.2e\n", c0$p.value),
		sprintf("Vars: %s  %s\n", nsV[1], nsV[2]),
		#join(apply(cbind(ns, sum), 1, join, ' '), "\n")
		join(apply(cbind(ns, t(sum)), 1, join, ' '), "\n")
	)
	if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
	text(.9, 0.5, txt, adj = c(1, .5), cex = cex.cor * 1)	# used tb cex.cor * r
}

unique_fraction = function(x) (length(unique(x))/length(x))
guess_is_factor = function(x, na.rm = T, uniqueFraction = .1, Nlevels = 10) {
	all(as.integer(x) == x, na.rm = na.rm) &&
		unique_fraction(x) < uniqueFraction &&
		length(unique(x)) <= Nlevels
}


panel.scatter = function(x, y, col = par("col"), bg = NA, pch = par("pch"), 
	cex = 1, col.smooth = "red", span = 2/3, iter = 3, blotSize = cm2in(.3), ...) 
{
	#call_ = match.call(envir = parent.frame(1L));
	# it is difficult to get information on the plotted variables, for now a heuristics is used
	#	to determine factor status
	if (guess_is_factor(x) && guess_is_factor(y)) {
		pars = par(c('usr', 'xaxt', 'yaxt')); on.exit(par(pars));
		t0 = table(x, y);
		#plot(t0);
		mx = max(as.integer(t0));
		d0 = Df(merge.multi.list(dimnames(t0)), w = sqrt(as.integer(t(t0))), as_numeric = c('x', 'y'));
		#par(new = TRUE);
		#plot(d0[, c('x', 'y')], type = 'n', xaxt='n', yaxt = 'n');
		par(new = TRUE);
		with(d0,
			symbols(x = x, y = y, circles = w, inches = blotSize, ann = F, bg = "steelblue2", fg = NULL,
			 xaxt='n', yaxt = 'n')
		)
		#return();
	} else {
		# <i> call forwarding
		panel.smooth(x, y, col, bg, pch, cex, span, iter);
	}
}

pairs_std = function(...,
	lower.panel = panel.scatter, diag.panel = pairs_std.panel.hist, upper.panel = pairs_std.panel.cor) {
	pairs(...,
		lower.panel = lower.panel, diag.panel = diag.panel, upper.panel = upper.panel)
}

pairs_std_save = function(data, ..., path = NULL) {
	if (is.null(path)) path = tempfile(fileext = '.png');
		plot_save_eval(pairs_std(data), ..., plot_path = path);
	return(path);
}

pairs_wide_list = function(blocks = NULL, NperBlock = 8) {
	# <p> names
	ns = names(blocks);
	nsAll = unlist(blocks);
	blkNs = if (notE(ns)) ns else paste('Block', seq_along(blocks));
	# <p> block tiling
	Iblocks = seq_along(blocks);
	blocksPair = merge.multi(Iblocks, Iblocks);
	# <p> reorder
	Idiag = blocksPair[, 1] == blocksPair[, 2];
	blocksPair = rbind(blocksPair[Idiag, ], blocksPair[!Idiag, ]);
	# <p> tabnames
	pairNs = apply(blocksPair, 1, function(r)paste(unique(c(blkNs[r[1]], blkNs[r[2]])), collapse = ' - '));
	# <p> indeces
	horIdcs = lapply(blocksPair[, 1], function(b)which.indeces(blocks[[b]], nsAll));
	verIdcs = lapply(blocksPair[, 2], function(b)which.indeces(blocks[[b]], nsAll));
	# <p> by pair of blocks
	r = lapply(1:nrow(blocksPair), function(i)list(
		coord = blocksPair[i, ], name = pairNs[i],
		names = list(blocks[[ blocksPair[i, 1] ]], blocks[[ blocksPair[i, 2] ]]),
		horIdcs = horIdcs[[blocksPair[i, 1]]], verIdcs = verIdcs[[blocksPair[i, 2]]]
	));
	return(r);
}

pairs_wide_old = function(data, ..., blocks = NULL, NperBlock = 8, consumer = Identity1) {
	nsPairs = unlist(blocks);
	dPairs = data[, nsPairs];
	Iblocks = seq_along(blocks);
	blocksPair = merge.multi(Iblocks, Iblocks);
	plts = apply(blocksPair, 1, function(r) {
		horInd = which.indeces(blocks[[ r[1] ]], nsPairs);
		verInd = which.indeces(blocks[[ r[2] ]], nsPairs);
		pl = consumer(pairs_std(dPairs, horInd = horInd, verInd = verInd), r);
		return(list(pl))
	});
	return(unlist.n(plts, 1));
}

pairs_wide_single = function(data, b) { pairs_std(data, horInd = b$horIdcs, verInd = b$verIdcs); }
pairs_wide = function(data, ..., blocks = NULL, NperBlock = 8, consumer = Identity1) {
	prs = pairs_wide_list(blocks, NperBlock);
	dPairs = data[, unlist(blocks)];
	plts = lapply(prs, function(b)list(consumer(pairs_wide_single(dPairs, b), b)));
	return(unlist.n(plts, 1));
}


pairs_wide_save = function(data, ..., blocks = NULL, path = NULL, ext = 'jpeg') {
	prefix = if (is.null(path)) tempfile() else path;
	r = pairs_wide(data, ..., blocks = blocks, consumer = function(plot, b) {
		plot_save_eval(plot, plot_path = Sprintf('%{prefix}s-%{name}s.%{ext}', b));
	});
	return(r);
}

#
#	<p> omics data
#

quantileData = function(d, p) {
	dS = sort(d);
	q = dS[ceiling(p * length(dS))];
	q
}

quantileReference = function(reference, direction = 2, center = TRUE) {
	if (is.matrix(reference) && center) {
		refC =  matrixCenter(reference, direction);
		reference = matrixDeCenter(refC$matrix, mean(refC$center), direction);
	}
	ref = na.omit(as.vector(as.matrix(reference)));
	ref
}

Skewness = function(x, na.rm = T) {
	x0 = if(na.rm) na.omit(x) else x;
    N = length(x0);
    x1 = x0 - mean(x0)
    y = sqrt(N) * sum(x1^3) / (sum(x1^2)^(3/2))
    s = y * ((1 - 1/N))^(3/2);
    s
}
Noutliers = function(x, coef = 1.5)length(boxplot.stats(x, coef = coef)$out)

#' Quantile normalization of frame/matrix with respect to reference distribution
#'
#' Distribution to be normalized are represented as columns or rows of a matrix/data frame.
#' Each value is replaced by the quantile of the reference distribution as given by the value of the
#' empirical distribution function of the given value.
#'
#' @param reference numeric vector with realizations from the target distribution
#' @param data data frame or matrix with data to be normalized
#' @param direction is \code{data} organized per row or column?
#'
#' @examples
#' d = sapply(1:20, rnorm(1e4));
#' dNorm = quantileNormalization(as.vector(d), d)
quantileNormalization = function(reference, data, direction = 2,
	impute = TRUE, ties = 'random', center = TRUE, referenceDirection = direction) {
	ref = quantileReference(reference, referenceDirection, center);
	if (impute) mns = apply(data, 3 - direction, median, na.rm = T);
	dN = apply(data, direction, function(d) {
		d0 = d;
		if (impute) d[is.na(d0)] = mns[is.na(d0)];
		r = quantile(ref, probs = rank(d, na.last = 'keep', ties = ties)/length(na.omit(d)))
		if (impute) r[is.na(d0)] = NA;
		r
	});
	if (direction == 1) dN = t(dN);
	dimnames(dN) = dimnames(data);
	dN
}

# quantile normalization based on samples picked on the basis of their medians (around the medians)
# Nqn: number of reference samples
quantileNormalizationMedians = function(data, direction = 2, Nqn = 5, impute = TRUE) {
	# <p> determine median of medians, corresponding median, IQR
	medians = apply(data, direction, median);
	mediansO = order(medians);
	medianOI = as.integer(length(mediansO)/2 + .5);
	medianI = mediansO[medianOI];
	refMed = summary(data[, medianI]);
	refIQR = refMed[['3rd Qu.']] - refMed[['1st Qu.']];

	# <p> reference samples
	refL = as.integer(medianOI - Nqn/2 + .5);
	refU = refL + Nqn - 1;
	refSamples = mediansO[refL:refU];
	#print(refSamples)

	# <p> standardize reference samples wrt median, IQR
	refSampleValues = sapply(refSamples, function(i) {
		refI = summary(data[, i]);
		refIIQR = refI[['3rd Qu.']] - refI[['1st Qu.']];
		E = (data[, i] - refI[['Median']]) * refIQR/refIIQR + refMed[['Median']];
		#refIE = summary(E);
		#refIEIQR = refIE[['3rd Qu.']] - refIE[['1st Qu.']];
		#print(list(refI = refI, refIIQR = refIIQR, refIE = refIE, refIEIQR = refIEIQR));
		E
	});
	eQn = quantileNormalization(refSampleValues, data,
		direction = direction, impute = impute, center = FALSE);
	eQn
}

dataCentered = function(d, na.rm = T) {
	dC = apply(d, 2, function(col)col - mean(col, na.rm = na.rm));
	dC
}

#
#	<p> distributions
#

qtruncnorm = function(p, mean = 0, sd = 1, lower = -Inf, upper = Inf) {
	plow = pnorm(lower, mean, sd);
	pupp = pnorm(upper, mean, sd);
	qnorm(p * (pupp - plow) + plow, mean, sd = sd)
}
rtruncnorm = function(N, mean = 0, sd = 1, lower = -Inf, upper = Inf) {
	qtruncnorm(runif(N), mean, sd = sd, lower, upper)
}

qqDist = function(Nqts = 1e2, qdist, ...) {
	qtls = (1:Nqtls)/(Nqtls + 1);
	qtlsExp = qdist(qtls, ...);
	qtlsObs = quantile(rtn, qtls);
	qq = qplot(qtlsExp, qtlsObs) + theme_bw();
	qq
}

qqSim = function(Nsim, dist = 'truncnorm', Nqts = Nsim/10, ...) {
	require('ggplot2');
	rdist = get(Sprintf('r%{dist}s'));
	r = rdist(Nsim, ...);
	qdist = get(Sprintf('q%{dist}s'));
	qq = qqDist(Nqts, qdist, ...);
	qq
}

#
#	<p> entropy
#

table.entropy = function(d) {
	p = table.freq(d);
	p = p[p != 0];
	H = - sum(p * log(p));
}

#
#	<p> qvalue
#

Qvalue = function(P.value, ...) {
	Require('qvalue');
	P.valuesNotNA = na.omit(P.value);
	qv = qvalue(P.valuesNotNA, ...);
	r = qv;
	r$qvalue = vector.embed(rep(NA, sum(is.na(P.value))), which(!is.na(P.value)), qv$qvalue);
	r
}

#
#	<p> math functions
#

Ceiling = function(x, N = 0)(ceiling(x * 10^N) / 10^N)
Floor = function(x, N = 0)(floor(x * 10^N) / 10^N)

#
#	<p> descriptive statistics
#

SummaryColNum = function(col, qtls = c(0, 0.25, .5, .75, 1)) {
	qAlways = c(0, .25, .5, .75, 1);
	Qtls = sort(unique(c(qAlways, qtls)));
	qs = quantile(col, Qtls, na.rm = TRUE);
	mn = mean(col, na.rm = T);
	na = fraction(is.na(col));
	Sd = sd(col, na.rm = T);
	median = qs[which(qtls == .5)];
	qs1 = qs[which.indeces(setdiff(Qtls, qAlways), Qtls)]
	N = sum(!is.na(col));
	vs = c(mn, Sd, N, na, median,
		qs[which(qtls == .25)], qs[which(qtls == .75)],
		qs[which(qtls == 0)], qs[which(qtls == 1)], qs1);
	r = Df_(t(vs),
		names = c('mean', 'sd', 'N', 'NA', 'median', 'Q25', 'Q75', 'min', 'max', names(qs1)),
		row.names = attr(col, 'label'));
	return(r);
}
SummaryColFactor = function(col) {
	na = vector.named(fraction(is.na(col)), 'NA');
	na
}
SummaryCatCol = function(col) {
	if (class(col) != 'factor') return(NULL);
	tc = t2r(table(col))[, 1];
	N = sum(tc)
	rn = paste(attr(col, 'label'), names(tc), sep = '.');
	#rn = names(tc);
	ns = c('mean', 'sd', 'N', 'NA');
	rTot = Df(NA, NA, N, mean(is.na(col)), row.names = attr(col, 'label'), names = ns);
	#rTot = Df(NA, NA, N, mean(is.na(col)), names = ns);
	freq = tc / N;
	sd = sqrt(freq * (1 - freq) / N);
	rLev = Df(freq, sd, tc, NA, row.names = rn, names = ns)
	r = rbind(rTot, rLev);
	r
}

SummaryCol = function(col, qtls = c(0, 0.25, .5, .75, 1)) {
	r = if (class(col) %in% c('numeric', 'integer')) SummaryColNum(col, qtls) else
		if (class(col) == 'factor') SummaryCatCol(col) else
			stop("Could not summarize column");
	r
}

# attach attribute label as column name to all columns
DfLabel = function(d) {
	return(Df(nelapply(d, function(n, col) {
		attr(col, 'label') = n;
		return(col);
	})));
}

Summary = function(d) {
	r0 = lapply(DfLabel(d), SummaryCol)
	r1 = RbindDfs(SetNames(r0, NULL), embed = TRUE);
	return(r1);
}

# summary of categorical data
SummaryCat = function(d) {
	r0 = lapply(DfLabel(d), SummaryCatCol)
	r = do.call(rbind, r0);
	row.names(r) = NULL
	r
}

SummaryTestCat = function(col, By, B = 1e4) {
	catTab = table(Df(col, By));
	P = chisq.test(catTab)$p.value;
	PnonPar = chisq.test(catTab, B = 1e4)$p.value;
	return(Df(P = P, PnonPar = PnonPar, row.names = attr(col, 'label')));
}
SummaryTestNum = function(col, By) {
	#grps = by(col, By, identity);	# --> t test
	rAnova = summary(aov(y ~ group, Df(y = col, group = By)));
	P = rAnova[[1]]['group', 'Pr(>F)'];
	PnonPar = kruskal.test(col, By)$p.value;
	return(Df(P = P, PnonPar = PnonPar, row.names = attr(col, 'label')));
}

SummaryTest = function(col, by) {
	r = if (class(col) == 'numeric') SummaryTestNum(col, by) else
		if (class(col) == 'factor') SummaryTestCat(col, by) else
			stop("Could not summarize column");
	r
}

SummaryBy = function(d, By, summaryVars) {
	d = DfLabel(d);
	if (missing(summaryVars)) summaryVars = setdiff(names(d), By);
	r = by(d, d[[By]], function(d0) {
		Summary(d0[, summaryVars, drop = F])
	});
	r = c(list(all = Summary(Df_(d, min_ = By))), r);
	tests = lapply(summaryVars, function(n)SummaryTest(d[[n]], d[[By]]));
	dfTests = do.call(rbind, tests);
	tab = do.call(cbind, r);
	tabTests = MergeByRowNames(tab, dfTests, all = TRUE);
	return(list(tab = tab, tabsBy = r, tabTests = tabTests, test = dfTests));
}


descriptivesNumeric = function(data) {
	bp = apply(data, 2, boxplot.stats);
	N = list.kp(bp, 'n', do.unlist = T);
	r = data.frame(
		N = N,
		Nna = nrow(data) - N,
		mean = apply(data, 2, mean, na.rm = T),
		median = apply(data, 2, median, na.rm = T),
		se = apply(data, 2, sd, na.rm = T) / sqrt(N),
		Nout = sapply(bp, function(v)length(v$out))
	);
	r
}
descriptivesFactor = function(data) {
	Nnas = apply(data, 2, function(c)sum(is.na(c)));
	r = data.frame(
		N = nrow(data) - Nnas,
		Nna = Nnas,
		Ncat = sapply(lapply(data, levels), length)
	);
	r
}

descriptives = function(data) {
	r1 = descriptivesFactor(data[, DfClasses(data) == 'factor']);
	r2 = descriptivesNumeric(data[, DfClasses(data) == 'numeric']);
	r0 = if (nrow(r2) == 0) list(r1) else if (nrow(r1) == 0) list(r2) else list(r1, r2);
	r = listOfDataFrames2data.frame(r0, idColumn = NULL, colsFromUnion = T, row.names = T);
	r$`NAperc` = with(r, Nna / nrow(data) * 100);
	r
}


#
#	<p> survival
#

# adapted from survival:::print.survfit
survfit2m = function (x, scale = 1, ..., rmean = 'none') {
	if (inherits(x, "survfitms")) {
		x$surv <- 1 - x$prev
		if (is.matrix(x$surv)) dimnames(x$surv) <- list(NULL, x$states)
		if (!is.null(x$lower)) {
			x$lower <- 1 - x$lower
			x$upper <- 1 - x$upper
		}
		rmean <- "none"
	}
	omit <- x$na.action
	temp <- survival:::survmean(x, scale = scale, rmean)
	mtemp <- if (is.matrix(temp$matrix)) 
		temp$matrix
	else matrix(temp$matrix, nrow = 1, dimnames = list(NULL, 
		names(temp$matrix)))
	if (all(mtemp[, 2] == mtemp[, 3])) {
		cname <- dimnames(mtemp)[[2]]
		mtemp <- mtemp[, -2, drop = FALSE]
		cname <- cname[-2]
		cname[2] <- "n"
		dimnames(mtemp)[[2]] <- cname
	}
	if (all(mtemp[, 1] == mtemp[, 2])) 
		mtemp <- mtemp[, -1, drop = FALSE]
	temp$matrix <- drop(mtemp)
	temp$matrix
}

#
#	<p> mixed models
#

lmer2ci = function(object, method = 'profile')confint.merMod(object, method = method);

lmer2p = function(object, method = 'profile') {
	cis = lmer2ci(object, method = method);
	apply(cis, 1, function(ci)ciToP(ci[1], ci[2]))
}

polr2p = function(rPoly) {
 	ci = t2r(confint(rPoly));
	cfs = coefficients(summary(rPoly));	
	cfs1 = cbind(cfs, P = c(ciToP(ci[, 1], ci[, 2]), rep(NA, nrow(cfs) - nrow(ci))));
	return(cfs1);
}

# confint plus P-value
confintP = function(m, level = .95) {
	ci = confint(m, level = level);
	P = apply(ci, 1, function(ci)ciToP(ci[1], ci[2], level = level));
	cbind(ci, P)
}

coefficientsLmer = function(object, method = 'Wald') {
	cfs = coefficients(summary(object));
	P = lmer2p(object, method);
	Iinter = which(names(P) == '(Intercept)');
	r = cbind(cfs, P = P[Iinter:length(P)]);
	return(r);
}


# alternative check for convergence of glmer models
#https://stackoverflow.com/questions/21344555/convergence-error-for-development-version-of-lme4
glmerFit = function(m) {
	relgrad = with(m@optinfo$derivs, solve(Hessian, gradient));
	return(max(abs(relgrad)));
}
GlmerFit = function(m, cutoff = 1e-3)(glmerFit(m) < cutoff)

#
#	<p> simulation studies
#

simulationRun = dataSimulation = function(Niteration, fSim, fStat = function(data, ...)summary(data),
	..., parSim = list(), parStat = list()) {
	parShared = list(...);
	r = Lapply(1:Niteration, function(i) {
		data = do.call(fSim, c(parSim, parShared));
		r = do.call(fStat, c(list(data = data), parStat, parShared));
	});
}

simulationRunBySpec = function(fSim, fStat = function(data, ...)summary(data),
	spec = list(Nsim = 1e1, sim = list(), stat = list(), pars = list())) {

	args = c(
		list(Niteration = spec$Nsim, fSim = fSim, fStat = fStat,
			parSim = spec$sim, parStat = spec$stat),
		spec$pars
	);
	r = do.call(simulationRun, args);
	return(r);
}

simulationDataBySpec = function(fSim,
	spec = list(Nsim = 1e1L, sim = list(), stat = list(), pars = list())) {
	data = do.call(fSim, c(spec$pars, spec$sim));
	return(data);
}
simulationStatBySpec = function(fTest, data,
	spec = list(Nsim = 1e1L, sim = list(), stat = list(), pars = list())) {
	r = do.call(fTest, c(list(data = data), spec$pars, spec$stat));
	return(r);
}

#
#	<p> delta rule
#

#
#	Numeric delta rule
#

gradient = function(f, x, ..., eps = 1e-5) {
	sapply(seq_along(x), function(i) {
		epsI = vector.assign(0, i, eps, N = length(x));
		(f(x + epsI, ...) - f(x - epsI, ...)) / (2 * eps)
	});
}

totalDeriv = function(f, x, ..., eps = 1e-5) {
	y = f(x, ...);
	t(sapply(seq_along(y), function(i)gradient(function(x)f(x, ...)[i], x, eps = eps)));
}

deltaNPseudo = function(N, pseudo = 0) {
	if (is.list(N)) N = do.call(cbind, N);
	if (pseudo != 0) {
		N[1, ] = N[1, ] + pseudo;
		N[2, ] = N[2, ] + 2*pseudo;
	}
	return(N);
}

# delta rule for functions of binomial data
#N: list of pairs Nevents, Ntotal, or nx2 matrix of these numbers
#f: function to be computed on the frequencies
deltaFreqs = function(N, f, logScale = T, ..., eps = 1e-5, alpha = .95, pseudo = 0) {
	#if (is.list(N)) N = sapply(N, identity);
	N = deltaNPseudo(N, pseudo);
	freqs = N[1, ] / N[2, ];
	var = freqs * (1 - freqs) / N[2, ];

	fDeriv = if (logScale) function(x, ...)log(f(x, ...)) else f;
	fGradient = gradient(fDeriv, freqs, ..., eps = eps);

	varf = (t(fGradient) %*% diag(var) %*% fGradient)[1, 1];
	limit = qnorm(1 - (1 - alpha)/2, 0, sqrt(varf));

	y = fDeriv(freqs, ...);
	yCi = y + c(-limit, limit);

	r = if (logScale)
		list(y = exp(y), ci = exp(yCi), var = varf, se = sqrt(varf), logCi = yCi) else
		list(y = y, ci = yCi, var = varf, se = sqrt(varf));
	r
}

delta = function(x, Cov, f, logScale = F, ..., eps = 1e-5, alpha = .95) {
	fDeriv = if (logScale) function(x, ...)log(f(x, ...)) else f;
	fGradient = gradient(fDeriv, x, ..., eps = eps);

	varf = (t(fGradient) %*% Cov %*% fGradient)[1, 1];
	y = fDeriv(x, ...);

	limit = qnorm(1 - (1 - alpha)/2, 0, sqrt(varf));
	yCi = y + c(-limit, limit);

	r = if (logScale)
		list(y = exp(y), ci = exp(yCi), var = varf, se = sqrt(varf), logCi = yCi) else
		list(y = y, ci = yCi, var = varf, se = sqrt(varf));
	r
}

# return freq-vector, cov matrix
deltaMultinomFromCnts = function(counts) {
	N = sum(counts);
	freqs = counts / N;
	Var = freqs * (1 - freqs);
	Cov = freqs %*% t(freqs);
	diag(Cov) = Var;
	return(list(par = freqs, cov = Cov));
}

# compose lists of par/cov structures
deltaCompose = function(parCov) {
	return(list(par = list.kpu(parCov, 'par'), cov = .bdiag(list.kp(parCov, 'cov'))));
}

#
#	<p> sensitvity/specificity/ROC/AUC
#


# assume TRUE/FALSE
# tab = Table(list(pred = prob >= co, truth = labels), cats = list(pred = c(F, T), truth = c(F, T)))
# Table(list(pred = c(T, F, T, F), truth = c(T, T, F, F)), cats = list(pred = c(F, T), truth = c(F, T)))
binaryMeasure = function(tab, strict = TRUE) {
	if (strict && !all(names(dimnames(tab)) == c('pred', 'truth'))) stop('dimnames != pred, truth');
	if (strict && !all(unique(unlist(dimnames(tab))) == c('FALSE', 'TRUE'))) stop('values != FALSE, TRUE');
	r = list(
		tp = sum(tab[, 'TRUE']) / sum(tab),
		tn = sum(tab[, 'FALSE']) / sum(tab),
		sensitivity = tab['TRUE', 'TRUE'] / sum(tab[, 'TRUE']),
		specificity = tab['FALSE', 'FALSE'] / sum(tab[, 'FALSE']),
		ppv = tab['TRUE', 'TRUE'] / sum(tab['TRUE', ]),
		npv = tab['FALSE', 'FALSE'] / sum(tab['FALSE', ])
	);
	return(r);
}

binaryMeasures = function(prob, labels) {
	cutoffs = sort(unique(prob));
	measures = lapply(cutoffs, function(co) {
		tab = Table(list(pred = prob >= co, truth = labels), cats = list(pred = c(F, T), truth = c(F, T)));
		bm = c(list(cutoff = co), binaryMeasure(tab));
		bm
	});
	r = apply(do.call(rbind, measures), 2, unlist);
	r
}

cvROC = function(f1, data,
	cv_fold = 10, cv_repeats = 1, cv_train = cv_train_glmnet, cv_test = cv_test_glmnet) {
	Library('pROC');
	Library('AUC');

	# <p> data preparation
	mm = model_matrix_from_formula(f1, data);
	d0 = data[mm$indeces, ];
	d0resp = d0[[formula.response(f1)]];

	# <p> crossvalidation
	pred = crossvalidate(cv_train, cv_test, data = d0, cv_fold = cv_fold, cv_repeats = cv_repeats, f1 = f1);

	# <p> results
	# <i><N> only first repeat headed at the moment
	roc = AUC::roc(pred[[1]], as.factor(d0resp));
	auc = AUC::auc(roc);
	rocProc = pROC::roc(as.factor(d0resp), pred[[1]]);
	aucProc = pROC::auc(rocProc);
	P = cor.test(d0resp, pred[[1]])

	r = list(roc = roc, auc = auc, rocProc = rocProc, aucProc = aucProc, aucCi = ci(rocProc), P = P,
		data = Df(response = d0resp, prediction = pred[[1]]));
	return(r);
}

plotRocs = function(data, interval = 0.2, breaks = seq(0, 1, interval)) {
	d0 = do.call(rbind, nlapply(data, function(n) {
		d = if (all( c('tpr', 'fpr') %in% names(data[[n]]))) data[[n]] else	# roc already computed
			with(data[[n]], AUC::roc(prediction, as.factor(response)));
		Df(fpr = 1 - d$fpr, tpr = d$tpr, type = n)
	}));
	p = ggplot(data = d0) +
		#geom_line(aes(x = fpr, y = tpr)) +
		geom_step(aes(x = fpr, y = tpr, col = type), lwd = 1) +
		geom_segment(aes(x = 0, y = 1, xend = 1, yend = 0)) +	#diagonal
		scale_x_reverse(name = "Specificity", limits = c(1,0), breaks = breaks,  expand = c(0.001,0.001)) + 
		scale_y_continuous(name = "Sensitivity", limits = c(0,1), breaks = breaks, expand = c(0.001, 0.001))+
		labs(color = 'ROC type') +
		coord_equal() + theme_bw();
	p
}


#
#	<p> multiple testing
#
tailStrength = function(ps, M = length(ps)) {
	N = length(ps);
	# standardize p-values to 0
	psS = 1 - sort(ps) * (N + 1)/(1:N);
	r = - sum(psS[1:M])/M;	# return negative as small values are deemed relevant
	r
}

#
#	<p> power calculations
#

findBinomN = function(pTrue, pLower, level = 0.95, Nmax = 1e3, lower = T) {
	Library('binom');
	fBinom = if (lower)
		function(N)binom.logit(N*pTrue, N, conf.level = level)$lower else
		function(N)binom.logit(N*pTrue, N, conf.level = level)$upper
	inverse(fBinom, c(1, Nmax))(pLower);
}

#
#	<p> pseudo counts
#

# implement pseudo-count, needed as observations c(0, 0, X) cause diverging estimates
mulitnomCoeff = function(x, log = TRUE) {
	N = sum(x);
	muco = lgamma(N + 1) - sum(sapply(x + 1, lgamma));
	if (!log) muco = exp(muco);
	return(muco);
}

dmultinomPseudo = function(x, prob, log = FALSE, pseudo = .1, factors = 1:1e2, F = NULL) {
	# find multiplication factor
	if (is.null(F)) {
		pseudoF = t(t_(pseudo) %*% factors);
		pseudoR = abs(pseudoF - round(pseudoF));
		F = factors[apply(pseudoR, 2, which.min)];
	}
	# standardize probabilities
	logP = log(v2freq(prob));
	# raw likelihood contributions
	pRaw = sum(round((x + pseudo) * F) * logP / F);
	# add multinomial coefficient
	pCooked = pRaw + mulitnomCoeff(x);
	if (!log) pCooked = exp(pCooked);
	return(pCooked);
}

#
#	<p> random numbers
#

generateSeed = function(preSeed = Sys.time()) {
	ints = hex2ints(md5sumString(as.integer(preSeed)));
	r = 0
	for (i in 1:length(ints)) r = bitwXor(r, ints[i]);
	return(r);
}

# run expression with fixed seed but leave the RNG stream unchanged
#	(bracket expression with save/restore of seed)
#	as.integer(Sys.time())
fixSeedForExpr = function(seed, expr, envir = parent.frame()) {
	if (!exists('.Random.seed')) runif(1);	# vivify RNG
	currentSeed = .Random.seed;
	on.exit(.Random.seed <<- currentSeed);
	set.seed(seed);
	r = eval(expr, envir = envir);
	#.Random.seed <<- currentSeed;
	return(r);
}

#
#	<p> general distributions
#

lchoosemult = function(x) {
	N = sum(x);
	K = length(x);
	lmult = dmultinom(x, prob = rep(1, K), log = T) + N*log(K);
	return(lmult);
}

#
#	<p> sampling
#

# individual probabilities, sample single event
# P: N x (K - 1) matrix with class probabilities
# size == 1 only supported case
rmultinomial = function(P, size = 1) {
	Ps = t2r(apply(P, 1, cumsum));
	R = runif(nrow(P));	# case of size == 1
	y = apply(apply(Ps, 1, function(P)R > P), 1, function(i)max(c(0, which(i))));
	return(y);
}

rbinomial = function(P, Nbinom = 1) {
	# binomial case for Nbinom >= 1
	P = t_(sapply(P, function(p)cumsum(dbinom(0:(Nbinom - 1), Nbinom, p))));
	return(rmultinomial(P));
}

#
#	<p> special data transformation
#

# partial long -> wide transformation by putting baseline value into separate column

dataSplitBaseline = function(dataN, valueColumn = 'RATEHEALTHTODAY',
	timeColumn = 'TIMINGASSDR1', baselineValue = 'Baseline', idColumn = 'STUDYCODE', suffix = 'bl') {

	valueColBl = paste0(valueColumn, suffix);
	dataBL0 = by(dataN, dataN[[ idColumn ]], function(d0) {
		Ibaseline = which(d0[[ timeColumn ]] == baselineValue & !is.na(d0[[ timeColumn ]]));
		if (length(Ibaseline) > 1)
			warning(Sprintf('Id %{id}s with %{N}d baseline values. Chosing first.', id = d0[[ idColumn ]][1], N = length(Ibaseline)));
		d1 = if (length(Ibaseline) >= 1) d0[- Ibaseline, , drop = F] else d0;
		val = if (length(Ibaseline) >= 1) d0[[ valueColumn ]][ Ibaseline[1] ] else NA;
		if (nrow(d1) == 0) {
			return(NULL);
		}
		d = Df(baselineVar.. = val, d1, headerMap = list(baselineVar.. = valueColBl));
		return(d);
	});
	dataBL = Df_(do.call(rbind, dataBL0));
	return(dataBL);
}
#
#	<p> Rrubin.R
#Wed Aug 30 13:45:03 CEST 2017

regressionEffectsLme = function(fit) {
	qhat =  fit$coefficients$fixed;
	ui <- vcov(fit)
	if (ncol(ui) != length(qhat)) stop("Different number of parameters: class lme")
	u = array(ui, dim = c(1, dim(ui)));
	r = list(qhat = qhat, u = u);
	r	
}
regressionEffectsLmerMod = function(fit) {
	qhat = lme4::fixef(fit);
	ui = vcov(fit);
	u = array(ui, dim = c(1, dim(ui)));
	r = list(qhat = qhat, u = u);
	r	
}
regressionEffectsCoxph = function(fit) {
	qhat =  coefficients(fit);
	ui <- vcov(fit)
	if (ncol(ui) != length(qhat)) stop("Different number of parameters: class coxph")
	u = array(ui, dim = c(1, dim(ui)));
	r = list(qhat = qhat, u = u);
	r	
}

# regressionEffectsMer = function(fit) {
# 			qhat = lme4::fixef(fit)
# 			ui <- as.matrix(vcov(fit))
# 			if (ncol(ui) != ncol(qhat)) 
# 				stop("Different number of parameters: class mer, fixef(fit): ", 
# 					ncol(qhat), ", as.matrix(vcov(fit)): ", ncol(ui))
# 			u[i, , ] <- array(ui, dim = c(1, dim(ui)))
# 		}
# 		else if (class(fit)[1] == "lmerMod") {
# 			qhat[i, ] <- lme4::fixef(fit)
# 			ui <- vcov(fit)
# 			if (ncol(ui) != ncol(qhat)) 
# 				stop("Different number of parameters: class lmerMod, fixed(fit): ", 
# 					ncol(qhat), ", vcov(fit): ", ncol(ui))
# 			u[i, , ] <- array(ui, dim = c(1, dim(ui)))
# 		}
# 		else if (class(fit)[1] == "polr") {
# 			qhat[i, ] <- c(coef(fit), fit$zeta)
# 			ui <- vcov(fit)
# 			if (ncol(ui) != ncol(qhat)) 
# 				stop("Different number of parameters: class polr, c(coef(fit, fit$zeta): ", 
# 					ncol(qhat), ", vcov(fit): ", ncol(ui))
# 			u[i, , ] <- array(ui, dim = c(1, dim(ui)))
# 		}
# 		else if (class(fit)[1] == "survreg") {
# 			qhat[i, ] <- coef(fit)
# 			ui <- vcov(fit)
# 			parnames <- dimnames(ui)[[1]]
# 			select <- !(parnames %in% "Log(scale)")
# 			ui <- ui[select, select]
# 			if (ncol(ui) != ncol(qhat)) 
# 				stop("Different number of parameters: class survreg, coef(fit): ", 
# 					ncol(qhat), ", vcov(fit): ", ncol(ui))
# 			u[i, , ] <- array(ui, dim = c(1, dim(ui)))
# 		}
# 		else {
# 			qhat[i, ] <- coef(fit)
# 			ui <- vcov(fit)
# 			ui <- expandvcov(qhat[i, ], ui)
# 			if (ncol(ui) != ncol(qhat)) 
# 				stop("Different number of parameters: coef(fit): ", 
# 					ncol(qhat), ", vcov(fit): ", ncol(ui))
# 			u[i, , ] <- array(ui, dim = c(1, dim(ui)))


pool.rubin = function (models, method = "smallsample")  {
	m = length(models);

	rL = lapply(models, function(fit)callDelegate('regressionEffects', class(fit), list(fit = fit)));

	# <p> raw input for Rubin's rule
	qhat = do.call(rbind, list.kp(rL, 'qhat'));
	uRaw = list.kp(rL, 'u');
	#u = array(unlist(uRaw), dim = c(length(rL), dim(rL[[1]]$u)[-1]))
	u = aperm(do.call(abind, lapply(uRaw, aperm, perm = c(2, 3, 1))), perm = c(3, 1, 2));
	dimnames(u)[2:3] = rep.list(dimnames(qhat)[[2]], 2);

	# <p> Rubin's rule
	names = dimnames(qhat)[[2]];
	qbar <- apply(qhat, 2, mean)
    ubar <- apply(u, c(2, 3), mean)
    e <- qhat - matrix(qbar, nrow = m, ncol = ncol(qhat), byrow = TRUE)
    b <- (t(e) %*% e)/(m - 1)
    t <- ubar + (1 + 1/m) * b
    r <- (1 + 1/m) * diag(b/ubar)
    lambda <- (1 + 1/m) * diag(b/t)
    #dfcom <- df.residual(models[[1]])
    dfcom <- ncol(qhat);
	#df <- mice:::mice.df(m, lambda, dfcom, method)
    df <- mice:::barnard.rubin(m, b, t, dfcom)
    fmi <- (r + 2/(df + 3))/(r + 1)
    names(r) <- names(df) <- names(fmi) <- names(lambda) <- names
    fit <- list(N = m, qhat = qhat, u = u, qbar = qbar, 
        ubar = ubar, b = b, t = t, r = r, dfcom = dfcom, df = df, 
        fmi = fmi, lambda = lambda)
    return(fit)
}

summary.rubin = function(fit) {
    x <- fit
    table <- array(x$qbar, dim = c(length(x$qbar), 10))
    dimnames(table) <- list(labels(x$qbar), c("est", "se", "t", 
        "df", "Pr(>|t|)", "lo 95", "hi 95", "nmis", "fmi", "lambda"))
    table[, 2] <- sqrt(diag(x$t))
    table[, 3] <- table[, 1]/table[, 2]
    table[, 4] <- x$df
    table[, 5] <- if (all(x$df > 0)) 
        2 * (1 - pt(abs(table[, 3]), x$df))
    else NA
    table[, 6] <- table[, 1] - qt(0.975, x$df) * table[, 2]
    table[, 7] <- table[, 1] + qt(0.975, x$df) * table[, 2]
    if (is.null(x$nmis) | is.null(names(x$qbar))) 
        table[, 8] <- NA
    else table[, 8] <- x$nmis[names(x$qbar)]
    table[, 9] <- x$fmi
    table[, 10] <- x$lambda
    return(table)
}

statMI = function(data, stat, ..., MIcolumn = 'Imputation_', skip0 = TRUE) {
	if (skip0) data = data[data[[MIcolumn]] != 0, , drop = F];
	rs = by(data, data[[MIcolumn]],
		function(data)stat(..., data = data)
	);
	rRubin = pool.rubin(rs);
	r = list(rubin = rRubin, r = rs);
	r
}

MIcompare = function (fit1, fit0, data = NULL, method = "Wald") {
	LLlogistic <- function(formula, data, coefs) {
		logistic <- function(mu) exp(mu)/(1 + exp(mu))
		Xb <- model.matrix(formula, data) %*% coefs
		y <- model.frame(formula, data)[1][, 1]
		p <- logistic(Xb)
		y <- (y - min(y))/(max(y) - min(y))
		term1 <- term2 <- rep(0, length(y))
		term1[y != 0] <- y[y != 0] * log(y[y != 0]/p[y != 0])
		term2[y == 0] <- (1 - y[y == 0]) * log((1 - y[y == 0])/(1 - 
			p[y == 0]))
		return(-(2 * sum(term1 + term2)))
	}
	call <- match.call()
	meth <- match.arg(tolower(method), c("wald", "likelihood"))
# 	if (!is.mira(fit1) | !is.mira(fit0)) 
# 		stop("fit1 and fit0 should both have class 'mira'.\n")
	m1 <- length(fit1$r)
	m0 <- length(fit0$r)
	if (m1 != m0) 
		stop("Number of imputations differs between fit1 and fit0.\n")
	if (m1 < 2) 
		stop("At least two imputations are needed for pooling.\n")
	m <- m1
	est1 <- fit1$rubin;	# pool.rubin(fit1)
	est0 <- fit0$rubin;	# pool.rubin(fit0)
	dimQ1 <- length(est1$qbar)
	dimQ2 <- dimQ1 - length(est0$qbar)
	formula1 <- formula(fit1$r[[1]])
	formula0 <- formula(fit0$r[[1]])
	vars1 = names(est1$qbar)
	vars0 = names(est0$qbar)
	if (is.null(vars1) | is.null(vars0)) 
		stop("coefficients do not have names")
	if (dimQ2 < 1) 
		stop("The larger model should be specified first and must be strictly larger than the smaller model.\n")
	if (!setequal(vars0, intersect(vars0, vars1))) 
		stop("The smaller model should be fully contained in the larger model. \n")
	if (meth == "wald") {
		Q <- diag(rep(1, dimQ1), ncol = dimQ1)
		where_new_vars = which(!(vars1 %in% vars0))
		Q <- Q[where_new_vars, , drop = FALSE]
		qbar <- Q %*% est1$qbar
		Ubar <- Q %*% est1$ubar %*% (t(Q))
		Bm <- Q %*% est1$b %*% (t(Q))
		rm <- (1 + 1/m) * sum(diag(Bm %*% (solve(Ubar))))/dimQ2
		Dm <- (t(qbar)) %*% (solve(Ubar)) %*% qbar/(dimQ2 * (1 + 
			rm))
	}
	if (meth == "likelihood") {
		if (is.null(data)) 
			stop("For method=likelihood the imputed data set (a mids object) should be included.\n")
		devM <- devL <- 0
		for (i in (1:m)) {
			devL <- devL + LLlogistic(formula1, complete(data, 
				i), est1$qbar) - LLlogistic(formula0, complete(data, 
				i), est0$qbar)
			devM <- devM + LLlogistic(formula1, complete(data, 
				i), est1$qhat[i, ]) - LLlogistic(formula0, complete(data, 
				i), est0$qhat[i, ])
		}
		devL <- devL/m
		devM <- devM/m
		rm <- ((m + 1)/(dimQ2 * (m - 1))) * (devM - devL)
		Dm <- devL/(dimQ2 * (1 + rm))
	}
	v <- dimQ2 * (m - 1)
	if (v > 4) 
		w <- 4 + (v - 4) * ((1 + (1 - 2/v) * (1/rm))^2)
	else w <- v * (1 + 1/dimQ2) * ((1 + 1/rm)^2)/2
	statistic <- list(call = call, call11 = fit1$call, call12 = fit1$call1, 
		call01 = fit0$call, call02 = fit0$call1, method = method, 
		nmis = fit1$nmis, m = m, qhat1 = est1$qhat, qhat0 = est0$qhat, 
		qbar1 = est1$qbar, qbar0 = est0$qbar, ubar1 = est1$ubar, 
		ubar0 = est0$ubar, Dm = Dm, rm = rm, df1 = dimQ2, df2 = w, 
		pvalue = 1 - pf(Dm, dimQ2, w))
	return(statistic)
}


#
#	Rpatches.R
#Fri Nov 20 17:18:37 CET 2009

# geepack patch

anovageePrim2 = function (m1, m2, ...)
{
    mm1 <- model.matrix(m1)
    mm2 <- model.matrix(m2)
    P1 <- mm1 %*% solve(t(mm1) %*% mm1) %*% t(mm1)
    P2 <- mm2 %*% solve(t(mm2) %*% mm2) %*% t(mm2)
    e2 <- mm2 - P1 %*% mm2
    e1 <- mm1 - P2 %*% mm1
    m2inm1 <- all(apply(e2, 2, var) < 1e-10)
    m1inm2 <- all(apply(e1, 2, var) < 1e-10)
    if (!any(c(m2inm1, m1inm2)))
        cat("Models not nested\n")
    else if (all(c(m2inm1, m1inm2)))
        cat("Models are identical\n")
    else {
        if (m1inm2) {
            tmp <- m1
            m1 <- m2
            m2 <- tmp
        }
        mm1 <- model.matrix(m1)
        mm2 <- model.matrix(m2)
        mf1 <- paste(paste(formula(m1))[c(2, 1, 3)], collapse = " ")
        mf2 <- paste(paste(formula(m2))[c(2, 1, 3)], collapse = " ")
        mm <- cbind(mm2, mm1)
        qmm <- qr(mm)
        qmmq <- qr.Q(qmm)
        nymm1 <- as.data.frame(qmmq[, 1:qmm$rank])
        colnames(nymm1) <- paste("parm", 1:ncol(nymm1), sep = ".")
        nymm2 <- nymm1[, 1:ncol(mm2), drop = FALSE]
        formula1 <- formula(paste(formula(m1)[[2]], formula(m1)[[1]],
            paste(c("-1", colnames(nymm1)), collapse = "+"),
            collapse = ""))
        m1call <- m1$call
        nymm1[, paste(formula(m1)[[2]])] <- m1$y
        nymm1[, paste(m1call$id)] <- m1$id
        m1call$offset <- m1$offset
        m1call$weights <- m1$weights
        m1call$formula <- formula1
        m1call$data <- nymm1
        m1ny <- eval(m1call)
        beta <- coef(m1ny)
        vbeta <- summary(m1ny)$cov.unscaled
        df <- dim(mm1)[2] - dim(mm2)[2]
        rbeta <- rep(1, length(beta))
        rbeta[1:df] <- 0
        beta0 <- rev(rbeta)
        zeroidx <- beta0 == 0
        X2 <- t(beta[zeroidx]) %*% solve(vbeta[zeroidx, zeroidx,
            drop = FALSE]) %*% beta[zeroidx]
        topnote <- paste("Model 1", mf1, "\nModel 2", mf2)
        title <- "Analysis of 'Wald statistic' Table\n"
        table <- data.frame(Df = df, X2 = X2, p = 1 - pchisq(X2,
            df))
        dimnames(table) <- list("1", c("Df", "X2", "P(>|Chi|)"))
        val <- structure(table, heading = c(title, topnote),
            class = c("anova", "data.frame"))
        return(val)
    }
}
#
#	Rdataset.R
#Tue Sep 28 14:53:47 2010

#	a dataset is a list with two data.frames
#	data: contains "data"
#	meta: contains meta information about "data"

# meta data frame
#	name	string/re to describe variable
#	type	(admin|var|unknown)
#	fullType	(admin:cluster|id|idM|idF)
#	index		index of column

metaData = function(d, metaTemplate, ignore.case = T) {
	ns = names(d);
	dm = listOfLists2data.frame(lapply(1:length(ns), function(i) {
		n = ns[i];
		m = sapply(metaTemplate, function(mt)(length(grep(mt$name, n, ignore.case = ignore.case)) > 0));
		r = metaTemplate[m];
		r = if (length(r) != 1) list(name = n, type = 'unknown', fullType = 'unknown') else
			merge.lists(r[[1]], list(name = n))[c('name', 'type', 'fullType')];
		r = c(r, list(index = i));
		r
	}), idColumn = NULL);
	dm
}

transformData = function(d, metaTemplate, ..., ignore.case = T) {
	ns = names(d);
	for (n in ns) {
		m = sapply(metaTemplate, function(mt)(length(grep(mt$name, n, ignore.case = ignore.case)) > 0));
		if (sum(m) == 1) {
			mt = metaTemplate[m][[1]];
			if (!is.null(mt$transf)) d[[n]] = mt$transf(d[[n]]);
		}
	}
	d
}

columnsOfType = function(d, type)d$meta$name[d$meta$fullType == type];
#
#	Rsimulation.R
#Mon 07 Jan 2008 06:56:12 PM CET 

#
#	<§> setup
#

#library(MASS);
#source(sprintf("%s/Rgeneric.R", Sys.getenv("MYRLIB")), chdir=TRUE);
#library(ggplot2);	#firstUpper

#
#	<§> implementation
#

#
#	<p> helper methods
#

parameterCombinationsTwins = function(specification, parameters, twins) {
	pars = strsplit(twins, ".", fixed = T)[[1]];
	N = length(pars);
	M = length(parameters[[pars[1]]]);	# assume equal length here
	df = data.frame(matrix(1:M, ncol = N, nrow = M));
	names(df) = pars;
	df
}

parameterCombinations = function(specification, parameters) {
	# <p> initialization
	parCnts = lapply(parameters, length);

	# <p> handle constraints (<A> must not overlap)
	if (!is.null(specification$constraints)) {
		parsC = lapply(names(specification$constraints), function(c) {
			fn = get(con("parameterCombinations", firstUpper(specification$constraints[[c]]$type)));
			cs = fn(specification, parameters, c);
			cs
		})
		names(parsC) = names(specification$constraints);
	} else parsC = list();

	# <p> add remaining parameters
	parsF = if (!is.null(specification$constraints)) {
		parameters[-unlist(sapply(names(specification$constraints), function(p) {
			pars = strsplit(p, ".", fixed = T)[[1]];
			idcs = which.indeces(pars, parameters);
			idcs
		}))]
	} else parameters;
	parsF = lapply(parsF, function(p)1:length(p));
	parsA = c(parsC, parsF);

	# <p> construct natural joint: unconstraint combinations
	df = data.frame(..dummy = 1);
	for (i in 1:length(parsA)) {
		df = merge(df, parsA[i]);
	}
	df = df[, -1];

	# <p> cleanup (names of df)
	ns = unlist(lapply(parsC, function(p)names(p)));
	ns = c(ns, names(parsF));
	names(df) = ns;

	df
}

#	gIndex: global index for reference purposes
#		lists are interpolated with arrays such that the name of the array
#		becomes embedded as list element
collapseParameters = function(collapsingGroups, parameters, indeces, gIndex) {
	iNs = names(indeces);
	pars = lapply(collapsingGroups, function(g) {
#		p = unlist.n(sapply(g$names, function(nm){
#			as.list(parameters[[nm]][indeces[[nm]]])
#		}), firstDef(g$collapse, 0));
		p = unlist.n(lapply(g, function(nm){
			po = parameters[[nm]][[indeces[[nm]]]];	# parameter object
			if (!is.list(po)) {
				po = list(po);
				names(po) = nm;
			}
			po
		}), 1);
		p
	});
	#if (is.list(pars$system)) pars$system$globalIndex = gIndex;
	pars
}

#
#	<p> generic methods
#

parameterIteration = function(s, order = NULL, reverse = F) {
	o = firstDef(order, 1:dim(s@combinations)[1], .dfInterpolate = F);
	#order.df(s@combinations, names(s@parameters), reverse);
	ps = lapply(o, function(i) {
		p = collapseParameters(s@specification$collapse, s@parameters, as.list(s@combinations[i, ]), i);
		p
	});
	i = list(parameters = ps, order = o);
	i
}

# i is given in canonical ordering of parameters
simulationFile = function(s, i) {
	spec = s@specification;
	pars = parameterIteration(s);	# canonical ordering
	digits = ceiling(log10(length(pars$order)));	# digits needed for enumeration
	filename = sprintf("%s/%s-%0*d.RData", spec$resultsDir, spec$name, digits, i);
	filename
}

#	needs: spec$cluster(hosts, type), spec$resultsFile|spec$name, spec$simulationFunction
runIterationCluster = function(s, order = NULL, reverse = F) {
	# <p> initialization
	spec = merge.lists(list(doSave = T, delaySave = F, local = F), s@specification);
	simulationPars = parameterIteration(s, order = order, reverse = reverse);

	# <p> initialize
	if (!is.null(spec$init)) {	eval(parse(text = spec$init)); }
	f = get(spec$simulationFunction);

	# <p> iteration function
	clf = function(i, simulationPars, ...){
		p = simulationPars$parameters[[i]];
		t0 = sum(proc.time()[3]);
		sim = try(f(p, ...));
		t1 = sum(proc.time()[3]) - t0;
		if (class(sim) != "try-error" & spec$doSave & !spec$delaySave) {
			save(sim, file = simulationFile(s, simulationPars$order[i]));
		}
		r = list(
			time = t1,
			parameters = p,
			result = ifelse(spec$delaySave, sim, class(sim) != "try-error")
		);
		r
	};

	if (!spec$local) {
		# <p> make cluster
		library("snow");
		c = spec$cluster;
		hosts = if (is.null(c$hosts)) rep("localhost", 8) else c$hosts;	#<A> cave vectors
		cl = makeCluster(hosts, type = firstDef(c$type, "SOCK"));
		clusterSetupRNG(cl);
	
		# <p> cluster intitalizations
		if (!is.null(c$source)) {
			textSource = sprintf("clusterEvalQ(cl, { %s })",
				paste(c(sapply(c$source, function(s)sprintf("source(\"%s\")", s)), ""), collapse = "; ")
			);
			eval(parse(text = textSource));
		}
		clusterExport(cl, spec$simulationFunction);
	}

	# <p> iterate
	textExec = sprintf(
		"%s 1:length(simulationPars$parameters), clf, simulationPars = simulationPars, %s%s;",
			ifelse(spec$local, "lapply(", "clusterApplyLB(cl,"), paste(spec$args, collapse = ", "), ")"
	);
	print(textExec);
	simulations = eval(parse(text = textExec));
	#print(simulations);

	# <p> finish up
	if (!spec$local) stopCluster(cl)

	if (spec$delaySave) for (i in 1:length(simulations)) {
		sim = simulations[[i]];
		if (class(sim) != "try-error" & spec$doSave) save(sim, file = simulationFile(s, i, pars$order[i]));
	}
	simulationPars
}

runIterationPlain = function(s, order = NULL, reverse = F) {
	# <p> initialization
	spec = s@specification;
	pars = parameterIteration(s, order = order, reverse = reverse);

	f = get(spec$simulationFunction);
	# <p> iterate
	simulations = lapply(1:length(pars$parameters), function(i){
		p = pars$parameters[[i]];
		t0 = sum(proc.time()[1:2]);
		sim = try(f(p));
		t1 = sum(proc.time()[1:2]) - t0;
		if (class(sim) != "try-error" & spec$doSave & !spec$delaySave) {
			save(sim, file = simulationFile(s, pars$order[i]));
		}
		r = list(
			time = t1,
			parameters = p,
			result = ifelse(spec$delaySave, sim, class(sim) != "try-error"));
		r
	});

	if (spec$delaySave) for (i in 1:length(simulations)) {
		sim = simulations[[i]];
		if (class(sim) != "try-error" & spec$doSave) save(sim, file = simulationFile(s, i, pars$order[i]));
	}
	pars
}

summarizeIteration = function(s, order = NULL, reverse = F) {
	# <p> initialization
	spec = s@specification;
	pars = parameterIteration(s, order = order, reverse = reverse);
print(pars);
	f = if (is.null(spec$summaryFunctionSingle)) NULL else get(spec$summaryFunctionSingle);

	simulations = lapply(1:length(pars$order), function(i) {
		parIndex = pars$order[i];
		file = simulationFile(s, parIndex);
		sim = if (file.exists(file)) { get(load(file)[1]) } else NULL;
		# <%><N> interpolate old simulations
		#if (length(sim) == 1) sim = sim[[1]];
		r = if (is.null(f)){ NA } else f(s, sim, pars$parameters[[parIndex]]);
		r
	});

	r = NULL;
	if (!is.null(spec$summaryFunction)) {
		summary = get(spec$summaryFunction);
		r = summary(s, simulations, pars$order, pars);
	}
	r
}

runIteration = function(s, order = NULL, reverse = F) {
	spec = s@specification;
	methodName = sprintf("runIteration%s", firstUpper(firstDef(spec$iterationMethod, "plain")));
	method = get(methodName);
	Log(sprintf('Rsimulation: %s', methodName), 2);
	method(s, order, reverse);
}

#
#	<p> class
#

#	specification contains restrictions on parameter combinations, grouping
#	restrictions:
#		twins:	pair parameters as listed (e.g. model simulation, estimation)
#	grouping: build final parameters by merging sublists
#		conventional group:
#			system: parameters other than involved in statistical concepts
#			model: specification of the model
#			parameters: model parameters

setClass("Rsimulation",
	representation(specification = "list", parameters = "list", combinations = "data.frame",
		mode = "character"),
	prototype(specification = list(), parameters = list(), combinations = data.frame(), mode = NULL)
);

setMethod("initialize", "Rsimulation", function(.Object, simulationName, mode = NULL) {
	s = get(simulationName);
	specification = merge.lists(list(doSave = T, delaySave = F), s$specification);
	specification$name = simulationName;
	parameters = s$parameters;

	if (specification$needsMode & is.null(mode)) {
		stop(con("Need simulation mode [",
			paste(names(specification$mode), collapse = ", "), "]"));
	}
	if (!is.null(mode)) {
		specification = merge.lists(specification, specification$mode[[mode]]);
	}
	.Object@mode = mode;
	.Object@specification = specification;
	.Object@parameters = parameters;
	.Object@combinations = parameterCombinations(specification, parameters);
	.Object
});


#
#	RpropertyList.R
#Fri Jan  7 17:40:12 2011

# wrap string for property list
ws = function(s) {
	s = if (length(grep('^([_/\\a-zA-Z0-9.]+)$', s)) > 0) { s } else {
		s = gsub('([\\"])', '\\\\\\1', s);
		sprintf('"%s"', s);
	}
	s
}

# can a string be condensed into a single line
condense = function(s, ident, o) {
	if (nchar(s) + ident * o$tabWidth - nchar(grep("\t", s)) < o$screenWidth) {
		s = gsub("\n", ' ', s);
		s = gsub("\t", '', s);
	}
	s
}

stringFromPropertyI = function(obj, ident, o) {
	str = '';
	inS = join(rep("\t", ident), '');
	in1S = join(rep("\t", ident + 1), '');

	if ( class(obj) == 'function' ) {
		str = sprintf('%s%s', str, ws(join(deparse(obj), "\n")))
	} else if ( class(obj) != 'list' & length(obj) == 1 & !(o$kp %in% o$forceVectors)) {
		# <i> data support
		str = sprintf('%s%s', str, ws(obj));
	} else if (class(obj) == 'list' && !is.null(names(obj))) {
		hash = sprintf("{\n%s%s;\n%s}", in1S, paste(sapply(names(obj), function(k) {
			o = merge.lists(o, list(kp = sprintf('%s.%s', o$kp, k)));
			r = sprintf('%s = %s', ws(k), stringFromPropertyI(obj[[k]], ident+1, o))
			r
		}), collapse = sprintf(";\n%s", in1S)), inS);
		if (!o$noFormatting) hash = condense(hash, ident, o);
		str = sprintf('%s%s', str, hash);
	} else { # vector or anonymous list
		obj = as.list(obj);
		array = sprintf("(\n%s%s\n%s)", in1S, if (length(obj) < 1) '' else paste(
			sapply(1:length(obj), function(i) {
			e = obj[[i]];
			o = merge.lists(o, list(kp = sprintf('%s.[%d]', o$kp, i)));
			stringFromPropertyI(e, ident+1, o)
		}), collapse = sprintf(",\n%s", in1S)), inS);
		if (!o$noFormatting) array = condense(array, ident, o);
		str = sprintf('%s%s', str, array);
	}
	str
}

stringFromPropertyDefaults = list(screenWidth = 80, tabWidth = 4, noFormatting = F, kp = '');
stringFromProperty = function(obj, o = list()) {
	o = merge.lists(stringFromPropertyDefaults, o);
	s = stringFromPropertyI(obj, 0, o);
	if (o$noFormatting) {
		s = gsub("[\n\t]", '', s);
	}
	s
}

# tokens: character vector of tokens
# ti: current token cursor (token index)
propertyFromStringRaw = function(tokens, ti = 1) {
	if (length(tokens) < 1) stop("propertyFromString: out of tokens");
	pl = if (tokens[ti] == '(') {	# we have an array here 	# ')' (bracket)
		a = NULL;
		repeat {
			ti = ti + 1;	# advance to next token
			if (ti > length(tokens) || tokens[ti] == ')') break;	# <A> empty list
			r = propertyFromStringRaw(tokens, ti);	# sub propertyList
			if (is.list(r$pl)) r$pl = list(r$pl);	# <A> concatanating of lists
			a = c(a, r$pl);
			ti = r$ti + 1;
			if (ti > length(tokens) || tokens[ti] == ')') break;	# <A> returning to list end
			if (tokens[ti] != ',') stop("propertyFromString: expected ',' or ')'");
		}
		if (ti > length(tokens) || tokens[ti] != ')') stop("propertyFromString: no array termination");
		a
	} else if (tokens[ti] == '{') {
		dict = list();
		repeat {
			ti = ti + 1;	# advance to next token
			if (ti > length(tokens) || tokens[ti] == '}') break;
			key = tokens[ti];
			if (tokens[ti + 1] != '=') stop("propertyFromString: expected '='");
			r = propertyFromStringRaw(tokens, ti + 2);
			dict[[key]] = r$pl;
			ti = r$ti + 1;
			if (tokens[ti] != ';') stop("propertyFromString: expected ';'");
		}
		if (ti > length(tokens) || tokens[ti] != '}') stop("propertyFromString: no dict termination");;
		dict
	#} elsif ($token =~ /^<(.*)>$/so) {		# we encountered data
	# <N> data not supported
	} else {	# string
		s = tokens[ti];
		if (substr(s, 1, 1) == '"') s = substr(s, 2, nchar(s) - 1);
		s
	}
	r = list(pl = pl, ti = ti);
	r
}

plStringRE = '(?:(?:[_\\/\\-a-zA-Z0-9.]+)|(?:\"(?:(?:\\\\.)*(?:[^"\\\\]+(?:\\\\.)*)*)\"))';
plCommentRE = '(?:/\\*(?:.*?)\\*/)';

propertyFromString = function(plistString, o = list()) {
	plistString = gsub(plCommentRE, '', plistString, perl = T);
	tokens = fetchRegexpr(sprintf('%s|[(]|[)]|[{]|[}]|[=]|[,]|[;]|<.*?>', plStringRE), plistString);
	pl = propertyFromStringRaw(tokens);
	pl$pl
}

#	pattern for extended plist properties
# #------------------------------------------------------------------------------------------------------------
# #PROPERTY_TAG
plExtDefaults = list( REstart = '^#[-]{80,}', REname = '^#(.*)' );
splitExtendedPlist = function(s, c = list()) {
	c = merge.lists(plExtDefaults, c);
	lines = splitString("\\n", s);
	m = Regexpr(c$REstart, lines);

	# <p> separate items
	start = which(sapply(m, length) > 0);
	startA = c(start, length(lines));
	plist = join(lines[Seq(1, start[1] - 1)], "\n");

	plistE = unlist.n(eilapply(start, function(st, i) {
		n = Regexpr(c$REname, lines[st + 1], captures  = T);
		SetNames(list(join(lines[Seq(startA[i] + 2, startA[i + 1] - 1)], "\n")), n);
	}), 1);
	return(list(propertyList = plist, interpolation = plistE));
}


propertyListTraverseRaw = function(plist, fString = identity, fData = NULL, ..., logLevel = 7) {
	Log("Plist traversal enter...", logLevel);
	plistI = if ( is.character(plist) && length(plist) == 1 ) {
		LogS(logLevel, "Plist traversal reached String [%{plist}s]");
		if (RegexprM('[\\x00-\\x08\\x80-\\x9f\\x7f-\\xff]', plist) && notE(fData)) fData(plist, ...) else
		if (notE(fString)) fString(plist, ...) else plist
	} else if (is.character(plist)) {
		LogS(logLevel, "Plist traversal reached array [1..%{N}d]", length(plist));
		sapply(plist, propertyListTraverseRaw, fString = fString, fData = fData, ...)
	} else if (is.list(plist)) {
		LogS(logLevel, "Plist traversal reached dictionary {%{ks}s}", ks = join(names(plist), ','));
		ns = sapply(names(plist), propertyListTraverseRaw, fString = fString, fData = fData, ...);
		values = lapply(plist, propertyListTraverseRaw, fString = fString, fData = fData, ...);
		SetNames(values, ns)
	} else stopS("Unknown Plist type: %{t}s", t = class(plist));
	return(plistI);
}

# c: fString => function handling strings, fArray, fDict, c: context
propertyListTraverse = function(plist, fString = identity, ...)propertyListTraverseRaw(plist, fString, ...);

plistInterpolateString = function(s, i, logLevel = 7) {
	LogS(logLevel, "Plist interpolation: %{s}s --> '%{d}s'", d = if (notE(i[[s]])) i[[s]] else s);
 	return(if (notE(i[[s]])) i[[s]] else s);
}
plistInterpolate = function(plist, i) {
	return(propertyListTraverse(plist, fString = plistInterpolateString, i = i));
}

propertyFromStringExt = function(s, c) {
	plRaw = splitExtendedPlist(s);
	return(plistInterpolate(propertyFromString(plRaw$propertyList), plRaw$interpolation));
}

#
#	Rlinux.R
#Tue May  8 18:05:44 2012

#
#	<p> RsowReap.R
#Wed May  7 18:16:23 CEST 2014

# <p> Design
#	These classes are meant to implement several Sow/Reap patterns
#	Standard Pattern
#	r = Reap(expression, returnResult = T);
#	print(r$result);
#	print(r$yield);
#
#	AutoPrint sowed values, reap later
#	SowerAddReaper(auto_reaper = printRepeaper, logLevel = 4);
#	{ Sow(my_tag = 4, logLevel = 3); }
#	r = Reap();
#
#	for (i in 1:10) {
#		Sow(my_number = i); 
#		Sow(my_greeting = 'hello world');
#	}
#	# prints list of list w/ each entry beting list(my_number = i, my_greeting = ..)
#	print(Reap(stacked = T));
#
#	Sow to different categories
#	SowerSetCatcher(default = StackingSowCatcherClass);
#	SowerSetCatcher(exclusions = SowCatcherClass);
#	Sow(1);
#	Sow(individuals = 1:10, sow_field = 'exclusions');
#	Collect(union, sow_field = 'exclusions');	# do not remove

packageDefinition = list(
	name = 'sowreap',
	files = c('Rmeta.R', 'Rdata.R'),
	#instFiles = list(Rscripts = 'Dev/pkg-minimal.R'),
	testing = list(
		doInstall = FALSE,
		tests = c('RtestsPackages/sowreap/sowreap.R')
	),
	description = list(
		title = 'Asynchroneous return with the Sow/Reap pattern',
		# version to be documented in news section
		#version = '0.1-0',
		author = 'Stefan B\uf6hringer <r-packages@s-boehringer.org>',
		description = 'Complex workflows benefit from decoupling of generating results and returning them, i.e., values can returned anywhere without leaving a function. This paradigm is implemented using the function pair Sow and Reap.',
		depends = c(),
		suggests = c(),	# c('testme'),
		news = "0.1-0	Initial release",
		license = 'LGPL-2'
		#, vignettes = "vignettes/vignette-sowreap.Rmd"
	),
	git = list(
		readme = '## Installation\n```{r}\nlibrary(devtools);\ninstall_github("sboehringer/sowreap")\n```\n',
		push = F,
		pushOnNewVersion = T,
		remote = 'https://github.com/sboehringer/sowreap.git'
	)
);

#__PACKAGE_DOC__
# This package allows you to use the Sow/Reap pattern for asynchroneous function returns. Use cases include larger software workflows, complicated, recursive algorithms, and reporting.
# The basic idea is that an new function, `Sow`, can be called at any point to deposit a return value.
# All sown values are later collected by the `Reap` function and return as a single list.
# @seealso {Sow()} for basic examples
#__PACKAGE_DOC_END__


ReaperAbstractClass = setRefClass('ReaperAbstract',
	fields = list(),
	methods = list(
	#
	#	<p> methods
	#
	initialize = function(...) {
		.self$initFields(...);
		.self
	},
	reap = function(...) { }
	#
	#	</p> methods
	#
	)
);
#ReaperAbstractClass$accessors(names(ReaperAbstractClass$fields()));

SowCatcherClass = setRefClass('SowCatcher', contains = 'ReaperAbstract',
	fields = list(
		auto_reapers = 'list',
		seeds = 'list'
	),
	methods = list(
	#
	#	<p> methods
	#
	initialize = function(...) {
		auto_reapers <<- list();
		seeds <<- list();
		.self$initFields(...);
		.self
	},
	sow_raw = function(seed) {
		for (r in c(.self, auto_reapers)) r$reap(seed);
	},
	sow = function(...) {
		.self$sow_raw(list(...)[1]);
	},
	reap = function(seed) {
		seeds <<- c(seeds, seed);
	},
	last_seed = function() {
		seeds[length(seeds)];
	},
	seed_count = function()length(seeds),
	Seeds = function(fields = NULL) {
		if (is.null(fields)) seeds else seeds[which.indeces(fields, names(seeds))]
	},
	set_seed_at = function(seed, pos) {
		seeds[pos] <<- seed;
		names(seeds)[pos] <<- names(seed);
		NULL
	},
	push_reaper = function(r) {
		auto_reapers <<- c(auto_reapers, r);
		NULL
	},
	register = function(ensemble, field)NULL,
	# <p> end a global SowReap session
	conclude = function()NULL
	#
	#	</p> methods
	#
	)
);
SowCatcherClass$accessors(names(SowCatcherClass$fields()));

SowCatcherPersistentClass = setRefClass('SowCatcherPersistent', contains = 'SowCatcher',
	fields = list(
		path = 'character',
		splitRe = 'character',
		cursor = 'integer'
	),
	methods = list(
	#
	#	<p> methods
	#
	initialize = function(...) {
		splitRe <<- '';
		callSuper(...);
		cursor <<- 1L;
		.self
	},
	seed_path_name = function(n, i = length(seeds) + 1) {
		key = if (splitRe != '') splitString(splitRe, n) else n;
		key[1] = Sprintf('%{i}03d_%{k}s', k = key[1]);
		seedPath = Sprintf('%{path}s/%{keyComponents}s.RData', keyComponents = join(key, '/'));
	},
	seed_path = function(seed, i = length(seeds) + 1) .self$seed_path_name(names(seed), i),
	seed_save = function(seed, i = length(seeds) + 1) {
		seedPath = .self$seed_path(seed, i);
		s = seed[[1]];
		Save(s, file = seedPath);
	},
	set_seed_at = function(seed, i) {
		.self$seed_save(seed, i);
		if (names(seeds)[i] != names(seed))
			Logs('SowCatcherPersistent: Warning: seed key %{k2}s does not match seed slot %{k1}s',
				k1 = names(seeds)[i], k2 = names(seeds), logLevel = 3);
	},
	reap_raw = function(seed) {
		.self$seed_save(seed);
		seeds <<- c(seeds, listKeyValue(names(seed), NA));
		save(seeds, file = .self$seed_path_name('__seed_names', 0));
		NULL
	},
	reap = function(seed) {
		if (cursor > .self$seed_count()) {
			.self$reap_raw(seed);
			.self$setCursor(cursor + 1L);
			return(NULL);
		}
		seed_nm = names(seed);

		# <p> locate previous position
		ns = names(.self$getSeeds());
		occs = which(seed_nm == ns[Seq(1, cursor - 1, neg = T)]);
		if (length(occs) == 0) {
			Logs('SowCatcherPersistent: adding seed %{seed_nm}s of class %{cl}s not seen before.',
				cl = class(seed[[1]]), 3);
			.self$reap_raw(seed);
			return(NULL);
		}
		new_cursor = cursor + min(occs) - 1L;
		Logs('SowCatcherPersistent: Skipping to cursor %{new_cursor}s.', 5);
		.self$set_seed_at(seed, new_cursor);
		.self$setCursor(new_cursor + 1L);
	},
	Seeds = function(fields = NULL) {
		idcs = if (is.null(fields)) Seq(1, length(seeds)) else which.indeces(fields, names(seeds));
		r = lapply(idcs, function(i)get(load(.self$seed_path(seeds[i], i))[1]));
		names(r) = names(seeds)[idcs];
		r
	},
	register = function(ensemble, field, doReset = F) {
		# <N> if path was not specified yet, try to query from ensemble, should exit on NULL
		if (!length(.self$getPath())) {
			.self$setPath(ensemble$getPath());
			# <p> subpath for this field
			path <<- Sprintf('%{path}s/%{field}s');
		}
		# <p> keep track of seeds
		seedsPath = .self$seed_path_name('__seed_names', 0);
		if (file.exists(seedsPath)) seeds <<- get(load(seedsPath)[1]);
		if (doReset) {
			unlink(sapply(Seq(1, length(seeds)), function(i).self$seed_path(seeds[i], i)));
			if (file.exists(seedsPath)) unlink(seedsPath);
			seeds <<- list();
		}
		NULL
	}
	#
	#	</p> methods
	#
	)
);
SowCatcherPersistentClass$accessors(names(SowCatcherPersistentClass$fields()));


SowCatcherStackClass = setRefClass('SowCatcherStack',
	fields = list(
		sowCatchers = 'list',
		sowCatcherClass = 'character'
	),
	methods = list(
	#
	#	<p> methods
	#
	initialize = function(...) {
		sowCatchers <<- list();
		sowCatcherClass <<- 'SowCatcher';
		.self$initFields(...);
		.self
	},
	push = function(sowCatcher = getRefClass(.self$sowCatcherClass)$new(), ...) {
		sowCatchers[[length(sowCatchers) + 1]] <<- sowCatcher;
	},
	pop = function() {
		currentCatcher = sowCatchers[[length(sowCatchers)]];
		sowCatchers <<- sowCatchers[-length(sowCatchers)];
		currentCatcher
	},
	sowCatcher = function() {
		if (!length(sowCatchers)) .self$push();	# autovivify
		sowCatchers[[length(sowCatchers)]]
	},
	reap = function(fields = NULL) {
		r = lapply(sowCatchers, function(sc)sc$Seeds(fields))
	},
	register = function(ensemble, sow_field, ...)
		lapply(sowCatchers, function(sc)sc$register(ensemble, sow_field, ...)),
	conclude = function()lapply(rev(sowCatchers), function(sc)sc$conclude())
	#
	#	</p> methods
	#
	)
);
SowCatcherStackClass$accessors(names(SowCatcherStackClass$fields()));

SowCatcherEnsembleClass = setRefClass('SowCatcherEnsemble',
	fields = list(
		sowers = 'list',
		sowCatcherClass = 'character'
	),
	methods = list(
	#
	#	<p> methods
	#
	initialize = function(...) {
		sowers <<- list();
		sowCatcherClass <<- 'SowCatcher';
		.self$initFields(...);
		.self
	},
	push = function(sowCatcher = SowCatcherStackClass$new(), sow_field = 'default', ...) {
		# <b> default argument mechanism does not work
		#if (is.null(sowCatcher)) sowCatcher = getRefClass('SowCatcher')$new();
		if (is.null(sowers[[sow_field]])) sowers[[sow_field]] <<- SowCatcherStackClass$new();
		sowers[[sow_field]]$push(sowCatcher)
		sowCatcher$register(.self, sow_field, ...);
	},
	pop = function(sow_field = 'default')sowers[[sow_field]]$pop(),
	sowCatcher = function(sow_field = 'default')sowers[[sow_field]]$sowCatcher(),
	reap = function(sow_field = 'default', fields = NULL) sowers[[sow_field]]$reap(fields),
	conclude = function() sapply(sowers, function(sower)sower$conclude())
	#
	#	</p> methods
	#
	)
);
SowCatcherEnsembleClass$accessors(names(SowCatcherEnsembleClass$fields()));

SowCatcherEnsemblePersistentClass = setRefClass('SowCatcherEnsemblePersistent',
	contains = 'SowCatcherEnsemble',
	fields = list(
		path = 'character'
	),
	methods = list(
	#
	#	<p> methods
	#
	initialize = function(...) {
		callSuper(...)
		.self
	},
	push = function(sowCatcher = SowCatcherStackClass$new(), sow_field = 'default', ...) {
		r = callSuper(sowCatcher, sow_field, ...);
		.self$freeze();
		r
	},
	pop = function(sow_field = 'default') {
		r = callSuper(sow_field);
		.self$freeze();
		r
	},
	freeze_path = function()Sprintf('%{path}s/000_ensemble.RData'),
	freeze = function() {
		Save(.self, file = freeze_path());
		NULL
	},
	thaw = function() {
		e = get(load(freeze_path())[1]);
		# SowCatchers have to recover their own state
		lapply(names(e$sowers), function(n)e$sowers[[n]]$register(e, n));
		e
	}
	#
	#	</p> methods
	#
	)
);
SowCatcherEnsemblePersistentClass$accessors(names(SowCatcherEnsemblePersistentClass$fields()));

if (!exists('SowReap_env__')) SowReap_env__ = new.env();
SowReap_env__ = new.env();

SowReapInit = function(ensembleClass = 'SowCatcherEnsemble', ...) {
	ensemble = getRefClass(ensembleClass)$new(...);
	assign('sowEnsemble', ensemble, envir = SowReap_env__);
	ensemble
}
SowReapConclude = function() {
	sowReapEnsemble()$conclude();
}
sowReapEnsemble = function() {
	if (!exists('sowEnsemble', envir = SowReap_env__)) SowReapInit();
	ensemble = get('sowEnsemble', envir = SowReap_env__);
	ensemble
}

SowReapCreateField = function(sow_field, sowCatcherClass = 'SowCatcher', ...) {
	e = sowReapEnsemble();
	for (sf in sow_field) {
		catcher = getRefClass(sowCatcherClass)$new();
		e$push(catcher, sow_field = sf, ...);
	}
	NULL
}
SowReapReapField = function(sow_field) {
	e = sowReapEnsemble();
	e$pop(sow_field)$getSeeds();
}

#' Asynchroneously return a value
#' 
#' @param ... value(s) to be returned
#' @param sow_field tag to indicate a pool of values into which the value(s) are to be stored, defaults to 'default'
#'
#' @details This function stores values into the package environment, so that they can later be
#'  retrieved by the \code{Reap} function. A ReferenceClass is used to implement the storage logic.
#'  Nested calls of Reap/Sow are possible as a stack is internally maintained.
#' @author Stefan Böhringer, \email{r-packages@@s-boehringer.org}
#' @seealso package-Reap
#' @keywords Sow Reap
#' @examples
#' 
#' packageDefinition = list(
#' 	name = 'pkg-minimal',
#' 	files = c(),
#'	instFiles = list(),
#' 	description = list(
#' 		title = 'Minimal R-package created with `package`',
#' 		# version to be documented in news section
#' 		#version = '0.1-0',
#' 		author = 'Stefan Böhringer <r-packages@s-boehringer.org>',
#' 		description = 'This appears in the package-documentaion, the markdown of the git-repository and in the package details.',
#' 		depends = c(),
#' 		suggests = c(),
#' 		license = 'LGPL',
#' 		news = "0.1-0	Initial release"
#' 	),
#' 	git = list(
#' 		readme = '## Installation\n```{r}\nlibrary(devtools);\ninstall_github("user/pkg-minimal")\n```\n',
#' 		push = FALSE,
#' 		pushOnNewVersion = FALSE
#' 	)
#' );
#'
#'
#' @export Sow
Sow = function(..., sow_field = 'default') {
	catcher = sowReapEnsemble()$sowCatcher(sow_field = sow_field);
	catcher$sow(...)
}

Reap = function(expr, sow_field = 'default', fields = NULL, envir = parent.frame(), auto_unlist = T,
	vivify = F, returnResult = F) {
	e = sowReapEnsemble();
	r = if (missing(expr)) {
		r = e$reap(sow_field, fields = fields);
		if (vivify) {
			r = lapply(r, function(e) {
				tbVivified = setdiff(fields, names(e));
				e = c(e, unlist.n(lapply(tbVivified, function(n)List(NULL, names_ = n)), 1));
				e
			});
		}
		if (auto_unlist && length(r) == 1) r = r[[1]];
		r
	} else {
		catcher = getRefClass(e$getSowCatcherClass())$new();
		e$push(catcher, sow_field = sow_field);
			rExpr = eval(expr, envir = envir);
		e$pop(sow_field)$Seeds(fields);
	}
	if (returnResult) r = list(result = rExpr, yield = r);
	r
}

ReapFromDisk = function(path, sow_field = 'default', fields = NULL, auto_unlist = T,
	ensembleClass = 'SowCatcherEnsemblePersistent', vivify = F) {
	e = getRefClass(ensembleClass)$new(path = path);
	e = e$thaw();

	r = e$reap(sow_field, fields = fields);
	if (vivify) {
		r = lapply(r, function(e) {
			tbVivified = setdiff(fields, names(e));
			e = c(e, lapply(tbVivified, function(n)List(NULL, names_ = n)));
			e
		});
	}
	if (auto_unlist && length(r) == 1) r = r[[1]];
	r
	
}
#
#	RknitrExt.R
#

KableRaw = function(df, ...) {
  join(kable(df, ...), "\n");
}

Kable = function(o, ..., Names = names(o)) {
  KableRaw(Df_(o, names = Names), ...);
}

KableTable = function(o, ..., freqs = T) {
	name = if (class(o) == 'data.frame') names(o) else join(as.character(substitute(o)));
	tCounts = Kable(table(o, ...), Names = c(name, 'Count'));
	r = if (freqs) Con(
		tCounts, Kable(table.freq(o, ...), Names = c(name, 'Freq'))
	, Sep_ = "\n\n") else tCounts;
  r
}

KableList = function(l, freqs = F) {
	d = Df_(t(list2df(l)));
	d1 = Df(name = names(l), d, names = c('name', 'value'));
	row.names(d1) = NULL
	Kable(d1, Names = names(d1));
}

KableCfs = function(o)cat(Kable(coefficients(summary(o))));
CatKableDf = function(t_, output = NULL, formats = c('csv', 'xls'), lf = TRUE, names = NULL) {
	if (notE(output)) capture.output( writeTable(t_, path = paste(output, formats, sep = '.')) );
	cat(Kable(Df_(t_, names = names)))
	if (lf) cat("\n\n")
}
CatKable = function(t_, names = NULL)CatKableDf(Df(table(t_), names = names))

CatKableDfTot = function(t_, output = NULL, formats = c('csv', 'xls'), lf = TRUE, names = NULL, nameColumn = 1) {
	t0 = Df_(t_);
	t1 = rbind(t0, sapply(t0, \(.) if (is.numeric(.)) sum(.) else NA));
	if (notE(nameColumn)) {
		t1[[nameColumn]] = as.character(t1[[nameColumn]]);
		t1[nrow(t1), nameColumn] = 'Total'
	} else row.names(t1)[nrow(t1)] = 'Total';
	CatKableDf(t1, output, formats, lf, names);
}


#
#	<p> ad-hoc reporting
#

knit_element_verbatim = function(e)e$verbatim
knit_element_table = function(e) {
	Sprintf("```{r echo = FALSE}\n  df = %{df}s;\n```\n```{r results = 'asis'}\n  cat(Kable(df))\n```\n",
		df = Deparse(e$table))
}
knit_element_raw = function(e) {
	Sprintf("```{r echo = FALSE}\n  r = %{r}s;\n```\n```{r}\n  cat(r)\n```\n",
		r = Deparse(e$raw))
}
knit_element_listtable = function(e) {
	Sprintf("```{r echo = FALSE}\n  l = %{list}s;\n```\n```{r results = 'asis'}\n  cat(KableList(l))\n```\n",
		list = Deparse(e$listtable))
}
knit_element_code = function(e) {
	Sprintf("```\n%{e}s\n```\n", e = join(e$code, "\n"))
}
knit_element_header = function(e)with(e, Sprintf('# %{header}s\n'))
knit_element_subheader = function(e)with(e, Sprintf('## %{subheader}s\n'))
knitPlot = function(e, ..., envir = parent.frame()) {
	pp = tempfile(fileext = '.png');
	plot_save(e, ..., envir = envir, plot_path = pp);
	return(list(path = pp));
}
knit_element_plot = function(e) {
	if (is.list(e$plot) && notE(e$plot$path)) return(Sprintf("![](%{pp}s)\n\n", pp = e$plot$path));
	if (is.character(e$plot) && exists(e$plot))
		return(with(e, Sprintf("```{r echo = FALSE}\n  plot(%{plot}s);\n```\n")));
	return(Sprintf("```{r echo = FALSE}\n  %{plot}s\n```\n",
		plot = ifelse(is.character(e$plot), e$plot, Deparse(e$plot))));
}
knit_element = function(e) {
	if (!length(e)) return('');
	n = names(e);
	fcandidates = paste('knit_element_', n, sep = '');
	f = fcandidates[which(sapply(fcandidates, exists))][1];
	if (!exists(f)) stopS('Knit element "%{n}s" not known [${f}s].');
	LogS(4, 'Knitting element %{n}s [%{f}s]');
	return(get(f)(e));
}
Knit_raw = function(l, output, output_type = 'html_document') {
	Library('knitr');

	md = join(sapply(l, knit_element), '\n\n');
	Log(md, 4);
	writeFile(Sprintf('%{output}s.Rmd'), md);
	o = knit(text = Trimws(md), output = Sprintf('%{output}s.md'));
	rmarkdown::render(Sprintf('%{output}s.Rmd'), output_type)
}
Knit = Vectorize(Knit_raw, 'output_type', SIMPLIFY = FALSE);

# if (1) {
# 	Knit(list(
# 		list(header = 'Polygenic risk score (PRS) analysis'),
# 		list(verbatim = 'SNPs were selected according to P-value up to a maximal P-value. Minimum distance between SNPs was respected.'),
# 		list(listtable = list(`Max P-value` = 0.01, `Min SNP distance` = 1e5, Nsnps = length(snps), covariates = join(covs, ', '))),
# 		list(subheader = 'PRS on training data'),
# 		list(verbatim = 'ROC curve'),
# 		list(plot = quote(plot(rocToxPrimData, main = 'ROC curve training data')))
# 	)
# 	, output = 'results/prs/prs-toxPrim');
# }

#
#	<p> block unfolding
#

parseUnfoldOptions = function(optionsString) {
	re = '(?<key>[a-zA-Z0-9_-]+):?\\s*(?<value>[^,]*)';
	kv = Regexpr(re, optionsString, captures = T, concatMatches = F);
	return(listKeyValue(kv[[1]]$key, kv[[1]]$value));
}
# element is in which sets of the list
which.in = function(e, l)which(sapply(l, function(s)(e %in% s)))

unfoldBlocks = function(code) {
	lines = splitString("\\n", code);
	blks = lapply(lines, function(line)parseUnfoldOptions(Regexpr(
		#'<!--\\s+%MDMETA\\s+(?<key>[a-zA-Z0-9_-]+):?\\s*(?<value>.*?)\\s*-->',
		'<!--\\s+%MDMETA\\s+(.*?)\\s*-->',
		line, captures = T, concatMatches = F))
	);
	keys = lapply(blks, names)
	metaLines = which(sapply(keys, function(k)length(k) >= 1 && k != ''));
	# line ranges
	#idcs = expandBlocks(rbind(which(keys == 'begin'), which(keys == 'end')));
	idcs = expandBlocks(rbind(which.in('begin', keys), which.in('end', keys)));
	r = lapply(idcs, function(i) {
		j = intersect(metaLines, i)
		return(list(
			meta = merge.lists(blks[j], listOfLists = T),
			template = setdiff(i, metaLines),
			range = c(min(i), max(i))
		));
	});
	return(r);
}

vRange = function(v, from, to) {
	if (to < from) return(NULL);
	return(v[from:to]);
}

knitInit = "```{r echo = FALSE, results = 'hide'}
	knitr::opts_knit$set(progress = FALSE, verbose = FALSE)
	options(knitr.duplicate.label = 'allow')
```
";
metaTemplates = list(default = "```{r results = 'asis', echo = FALSE}
	iterator__ = %{iterate}s;
	dummy__ = lapply(iterator__, function(%{varName}s) {
		rmd2knit = knit_expand(text = %{template}s);
		cat(knit(text = rmd2knit))
	})
```
",
	with = "```{r results = 'asis', echo = FALSE}
	iterator__ = %{iterate}s;
	dummy__ = lapply(iterator__, function(%{varName}s) with(%{varName}s, {
		rmd2knit = knit_expand(text = %{template}s);
		cat(knit(text = rmd2knit))
	}))
```
");

metaMDprefix = c('')
metaTemplatesExpandRaw = function(code, templates) {
	lines = splitString("\\n", code);
	rA = lapply(seq_along(templates), function(i) {
		tl = templates[[i]];
		templateRaw = qs(join(c(lines[tl$template], metaMDprefix), "\n"));
		template = gsub("\n", "\\\\n", templateRaw);
		varName = tl$meta$var;
		metaTemplate = metaTemplates[[firstDef(tl$meta$template, 'default')]];
		s = Sprintf(metaTemplate, tl$meta, template = template);
		prefix = vRange(lines, if (i > 1) templates[[i - 1]]$range[2] + 1 else 1, tl$range[1] - 1);
		return(c(prefix, s));
	});
	postfix = vRange(lines, last(templates)[[1]]$range[2] + 1, length(lines));
	return(join(c(unlist(rA), postfix), "\n"));
}
knitUnfolded = function(code, prefix, output_format = "html_document",
	params = NULL, envir = parent.frame(), output_file) {
	lines = splitString("\\n", code);
	header = max(which(lines == '---'));
	code = join(c(lines[1:header], knitInit, lines[(header + 1): length(lines)]), "\n");
	o = paste0(splitPath(prefix)$fullbase, '_expanded.Rmd');
	writeFile(o, code);
	rmarkdown::render(o, output_file = output_file, output_format = output_format,
		params = params, envir = envir);
}

knitUnfoldedExpand = function(Rmd) {
	blks = unfoldBlocks(Rmd);
	codeExpanded = metaTemplatesExpandRaw(Rmd, blks);
	return(codeExpanded);
}

renderUnfold = knitUnfold = function(input, output_file = NULL, text, output_format = "html_document",
	params = NULL, envir = parent.frame(), prefix = input) {
	codeExpanded = knitUnfoldedExpand(if (missing(text)) readFile(input) else text);
	knitUnfolded(codeExpanded, prefix = prefix,
		output_format = output_format, params = params, envir = envir, output_file = output_file);
}

Render = function(markdown, ...) {
	LogS(5, 'Knitr render in : %{wd}s', wd = getwd());
	rmarkdown::render(markdown, ...)
}

markdownRenderIn = function(markdown, out = .fn('md'), files = NULL, from = '.', absolute = TRUE,
	unfold = FALSE, output_format = "html_document") {
	Dir.create(out);
	files2link = c(files);
	if (absolute) {
		files2link = list.kpu(lapply(files2link, splitPath), 'absolute');
	} else {
		rel = relativePath(Sprintf('%{out}s/'), from);
		files2link = pathSimplify(paste(rel, files2link, sep = '/'));
	}
	File.symlink(files2link, out, replace = FALSE);	# assume out tb directory
	File.copy(markdown, out, overwrite = TRUE, symbolicLinkIfLocal = FALSE);	# render follows symlink <A>
	if (unfold) {
		exprInDir(renderUnfold(splitPath(markdown)$file, output_format = output_format), out);
	} else {
		exprInDir(Render(splitPath(markdown)$file, output_format = output_format), out);
	}
}
#
#	RparallelTools.R
#Fri Jul 26 09:13:16 2013

#
#	<p> interface functions
#

Env.new = function(hash = T, parent = parent.frame(), size = 29L, content = list()) {
	e = new.env(hash = hash, parent = parent, size = size);
	nlapply(content, function(n) {
		assign(n, content[[n]], envir = e);
		NULL
	});
	e
}

#' Create a placeholder for an object to be loaded later
#'
#' @param path File system path to the file containing a saved R data structure
#'
delayed_load = function(path) {
	new('ParallelizeDelayedLoad', path)
}

delayed_load_dummy = function(path) get(load(path)[1])
# might exist from reconstructions on remote machines
if (!exists('parallelize_env')) parallelize_env <<- new.env();
Lapply = lapply;
Sapply = sapply;
Apply = apply;
P__ = parallelize = function(.f, ..., Lapply_config = NULL, envir__ = NULL).f(...);
P_ = parallelize_call = function(.call, Lapply_config = NULL, envir__ = parent.frame(n = 2)) 
	base:::eval(.call, envir = envir__);
#
#	Rrepl.R
#Mon Sep 24 12:41:57 2018

Require('rjson');
replMagick = "REPL_PERL5_OVER";

ReplExternalClass = setRefClass('ReplExternal',
	fields = list(
		command = 'character',
		expectJSON = 'logical',
		magicIn = 'character',
		magicOut = 'character',
		connectionIn = 'ANY',
		connectionOut = 'ANY'
	),
	methods = list(
	#
	#	<p> methods
	#
	initialize = function(command = 'repl_perl5 --json --no-echo', expectJSON = TRUE,
		magicIn = replMagick, magicOut = replMagick, ...) {
		# initialize super class
		callSuper(.self,
			command = command, expectJSON = expectJSON, magicIn = magicIn, magicOut = magicOut, ...);
		.self
	},
	close = function() {
		gclose = get('close', envir = .GlobalEnv);
		gclose(connectionIn);
		gclose(connectionOut);
	},

	#
	#	functional methods
	#

	start = function() {
		# <p> input, named pipe
		connectionInPath = tempfile('fifo');
		System(Sprintf('mkfifo %{connectionInPath}q'), 4);
		connectionIn <<- fifo(fifoIn, 'r', blocking = T);
		# <p> output, start expternal REPLer
		commandPipe = Sprintf('%{command}s > %{connectionInPath}q');
		Log(commandPipe, 4);
		connectionOut <<- pipe(cmdPipe, 'w');
		.self
	},

	repl = function(code) {
		# <p> write code + magick
		writeLines(c(code, replMagick), connectionOut);
		flush(connectionOut);

		# <p> read, wait for expectation
		ls = c();
		repeat {
			l = readLines(connectionIn, n = 1);
			if (l == replMagick) break;
			ls = c(ls, l);
		}
		r = con(ls);
		return(if (expectJSON) fromJSON(r) else r);
	}

	#
	#	</p> methods
	#
	)
);
ReplExternalClass$accessors(names(ReplExternalClass$fields()));
#
#	Rpackage.R
#Thu Oct 17 16:52:03 CEST 2019

packageDefinition = list(
	name = 'package',
	files = c('Rmeta.R', 'Rdata.R', 'Rsystem.R', 'Rfunctions.R', 'RpropertyList.R'),
	instFiles = list(Rscripts = 'Dev/pkg-minimal.R'),
	testing = list(
		doInstall = TRUE,
		tests = c('RtestsPackages/package/package.R')
	),
	description = list(
		title = 'Create packages from R-code directly',
		# version to be documented in news section
		#version = '0.1-0',
		author = 'Stefan B\uf6hringer <r-packages@s-boehringer.org>',
		description = 'This package simplifies package generation by automating the use of `devtools` and `roxygen`. It also makes the development workflow more efficient by allowing ad-hoc development of packages. Use `?"package-package"` for a tutorial or visit the project wiki (belonging to the source repository).',
		depends = c('roxygen2', 'devtools', 'methods'),
		suggests = c('testme', 'jsonlite', 'yaml', 'knitr'),
		enhances = c(),
		systemrequirements = c(),
		news = "0.11-4	noGit option for checkPackage\n0.11-3	Bug fix Windows support\n0.11-2	Bug fix\n0.11-1	Removed shell constructs in system calls\n0.11-0	Windows support\n0.10-0	Support for enhances, SystemRequirements in NAMESPACE\n0.9-0	Finished vignette. Clean test. RC1\n0.8-1	Bug fix automatic dependency addition.\n0.8-0	Vignette building finished. Project vignette in progress.\n0.7-0	Vignette building. Started vignette for package `package`\n0.6-0	Clean CRAN check\n0.5-1	Resolved documentation\n0.5-0	Error free CRAN check. Warnings left.\n0.4-4	bugfix NAMESPACE generation\n0.4-3	`createPackage` fully documented.\n0.4-2	More documentation\n0.4-1	Bug fix NEWS file\n0.4-0	Self-contained example\n0.3-1	bug fix for missing files\n0.3-0	Beta, self-contained\n0.2-0	Alpha version\n0.1-0	Initial release",
		license = 'LGPL-2',
		vignettes = "vignettes/vignette-package.Rmd"
	),
	git = list(
		readme = '## Installation\n```{r}\nlibrary(devtools);\ninstall_github("sboehringer/package")\n```\n',
		push = F,
		pushOnNewVersion = T,
		remote = 'https://github.com/sboehringer/package.git'
	)
);
# Imports
#' @import methods
#' @import roxygen2
#' @import devtools
#' @importFrom "stats" "as.formula" "median" "model.matrix" "na.omit" "runif" "setNames" "optimize" "sd"
#' @importFrom "utils" "read.table" "recover" "write.table"
globalVariables(c('valueMapperStandard', 'read_yaml', 'read_json'))

#__PACKAGE_DOC__
# This package allows you to create a full-fledged R-package from a single R-file reducing the added work-load for maintaining a package to a minimum. Depending on the project, collections of files can be used and configuration can be seperated into a stand-alone configuration file, providing full flexibility. It can also handle git interaction. This package is created with itself and you can look at the single R-file `Rpackage.R` for a self-referring example.
# The package contains a self-contained package example defined in the single R-file \code{pkg-minimal.R} which can be inspected and installed as in the example below.
# @examples
# \dontrun{
#  file.show(system.file('Rscripts/pkg-minimal.R', package = 'package'))
#  createPackage(system.file('Rscripts/pkg-minimal.R', package = 'package'))
# }
# @seealso {createPackage()} for starting the main workflow
#__PACKAGE_DOC_END__

packageDocPrefix = "# This is package `%{name}s`\n#\n# %{title}s\n#\n# @details\n# %{description}s\n";
#packageDocPrefix = "# This is package `%{name}s`\n#\n# @details\n# %{description}s\n#\n";
packageReadmeTemplate = "# R-package `%{PACKAGE_NAME}s`, version %{VERSION}s\n%{README}s\n# Description\n%{DESCRIPTION_MD}s";

packageDescTemplate = "Package: %{PACKAGE_NAME}s\nType: %{TYPE}s\nTitle: %{TITLE}s\nVersion: %{VERSION}s\nDate: %{DATE}s\nAuthor: %{AUTHOR}s\nMaintainer: %{MAINTAINER}s\nDescription: %{DESCRIPTION}s\nLicense: %{LICENSE}s\nEncoding: %{ENCODING}s\nDepends: %{DEPENDS}s\nCollate: %{COLLATE}s\nSuggests: %{SUGGESTS}s\nEnhances: %{ENHANCES}s\nSystemRequirements: %{SYSTEMREQUIREMENTS}s\nLazyData: true\n%{ADDITIONS}s\n";

packageInterpolationDict = function(o, debug = F) {
	additions = '';
	if (length(o$description$vignettes) > 0) {
		o$description$suggests = unique(c(o$description$suggests, c('knitr', 'rmarkdown')));
		additions = paste0(additions,  "VignetteBuilder: knitr\n");
	}
	d = o$description;
	vars = list(
		PACKAGE_NAME = o$name,
		TYPE = firstDef(d$type, 'Package'),
		TITLE = d$title,
		VERSION = o$version,	# determined in probeDefinition
		DATE = firstDef(d$date, format(Sys.time(), "%Y-%m-%d")),
		AUTHOR = firstDef(d$author, 'anonymous'),
		MAINTAINER = firstDef(d$maintainer, d$author, 'anonymous'),
		DESCRIPTION = firstDef(d$description, ''),
		LICENSE = firstDef(d$license, 'LGPL-2'),
		ENCODING = firstDef(d$encoding, 'UTF-8'),
		COLLATE = circumfix(join(sapply(o$files, function(f)splitPath(f)$file), "\n    "), pre = "\n    "),
		DEPENDS = circumfix(join(d$depends, ",\n    "), pre = "\n    "),
		SUGGESTS = circumfix(join(d$suggests, ",\n    "), pre = "\n    "),
		ENHANCES = circumfix(join(d$enhances, ",\n    "), pre = "\n    "),
		SYSTEMREQUIREMENTS = circumfix(join(d$systemrequirements, ",\n    "), pre = "\n    "),
		README = firstDef(o$git$readme, ''),
		ADDITIONS = additions
	);
	# <!> code w/ {}'s
	vars$DESCRIPTION_MD = gsub("\\\\code[{](.*?)[}]", "`\\1`", vars$DESCRIPTION, perl = T);
	if (debug) print(vars);
	return(vars);
}
packageInterpolateVars = function(o, templ, debug = F) {
	if (debug) cat(templ);
	vars = packageInterpolationDict(o, debug);
	return(with(vars, Sprintf(templ)));
}
packageDescription = function(o, debug = F) {
	templ = firstDef(o$descriptionTemplate, packageDescTemplate);
	if (debug) cat(templ);
	return(packageInterpolateVars(o, templ, debug));
}

gitOptionsDefault = list(doPull = F, doPush = F);
gitActions = function(o, packagesDir, debug, gitOptions = gitOptionsDefault) {
	gitOptions = merge.lists(gitOptionsDefault, gitOptions);
	i = packageInterpolationDict(o, debug);
	pdir = Sprintf('%{packagesDir}s/%{name}s', o);
	if (gitOptions$doPull) System('git pull', 2, wd = pdir);

	readme = packageInterpolateVars(o, firstDef(o$git$readme, packageReadmeTemplate));
	writeFile(Sprintf('%{pdir}s/README.md'), readme);
	# initialize git
	if (!file.exists(with(o, Sprintf('%{packagesDir}s/%{name}s/.git')))) {
		System('git init', 2, wd = pdir);
	}
	System('git add --all', 2, wd = pdir);
	System(with(i, Sprintf('git commit -a -m "Commit for onward development of version %{VERSION}s"')), 2, wd = pdir);
	tags = System('git tag', 2, wd = pdir, return.output = T)$output;
	# tag new version
	newVersion = F;
	if (length(Regexpr(Sprintf('\\Q%{VERSION}s\\E', i), tags)[[1]]) == 0) {
		System(with(i, Sprintf('git tag %{VERSION}s')), wd = pdir);
		newVersion = T;
	}
	# remote
	if (notE(o$git$remote)) {
		remotes = System('git remote -v', 2, wd = pdir, return.output = T)$output;
		if (remotes == '' && o$git$remote != '')
			System(Sprintf('git remote add origin %{remote}s', o$git), 2, wd = pdir);
		if (o$git$pushOnNewVersion && newVersion || gitOptions$doPush) {
			System('git push -u origin master', 2, wd = pdir);
			System(Sprintf('git push origin %{VERSION}s', i), 2, wd = pdir);
		}
	}
}

installTests = function(o, packageDir, loadTestMe = FALSE, asCran = FALSE) {
	#if (loadTestMe) requireNamespace('testme');	# <i> trick shiny not to install testme
	if (is.null(rget('installPackageTests', NULL))) {
		Log('Package testme not available, skipping tests creation', 1);
		return(NULL);
	}
	#testme::installPackageTests(packageDir, o$testing$tests, createReference = TRUE, asCran);
	installPackageTests(packageDir, o$testing$tests, createReference = TRUE, asCran);

}

# <N> values are interpolated by Sprintf, '%' -> '%%'
vignetteDefaultKeys = list(
	title = 'How to use %{packageName}s',
	date = '"`r Sys.Date()`"',
	vignette =  ">\n  %%\\VignetteIndexEntry{%{file}s}\n  %%\\%{Vignette}sEngine{knitr::rmarkdown}\n  %%\\%{Vignette}sEncoding{UTF-8}",
	output = "rmarkdown::html_vignette"
	#output = "\n  html_document:\n    keep_md: TRUE"
	#output = "\n  rmarkdown::html_vignette:\n    keep_md: TRUE"
);

popcharif = function(s, char = "\n") {
	N = nchar(s);
	if (N == 0) return(s);
	if (substr(s, N, N) == char) return(substr(s, 1, N -1));
	return(s);
}

installVignette = function(name, path, o, packageDir, noGit = F) {
	name = splitPath(name)$base;	# normalize, allow path as name
	v = readFile(path);
	cat(v);
	m0 = unlist(Regexpr("(?six)^---\\n+
	((?:
		(?:\\S*)[\\t ]*:[\\t ]*
		(?:[^\\n]+\\n (?:[ \\t]+[^\\n]+\\n)* ) \\n*
	)+)---\\n(.*)", v, captures = T, concatMatches = F));
	dictR0 = Regexpr("(?six)
		(?<key>[a-z]\\S*)[\\t ]*:[\\t ]*
		(?<value> [^\\n]+\\n (?:[ \\t]+[^\\n]+\\n)* ) \\n*
	", m0[1], captures = T, concatMatches = F, global = T);
	dictR1 = list.transpose(dictR0[[1]]);
	dictR2 = listKeyValue(list.kp(dictR1, 'key'), sapply(list.kp(dictR1, 'value'), popcharif));
	dict = merge.lists(vignetteDefaultKeys, list(
		author = o$description$author,
		packageName = o$name,
		file = Sprintf('%{name}s.Rmd')
	), dictR2);
	meta = Sprintf(join(nelapply(dict, function(n, e)Sprintf('%{n}s: %{e}s')), "\n"),
		c(dict, list(Vignette = 'Vignette')));	#<!> hack to avoid warning
	vignette = m0[2];
	Rmd = Sprintf('---\n%{meta}s\n---\n%{vignette}s');
	writeFile(Sprintf('%{packageDir}s/vignettes/%{name}s.Rmd'), Rmd, mkpath = T);
	if (!noGit) {
		ignore = Sprintf('%{packageDir}s/vignettes/.gitignore');
		if (!file.exists(ignore)) writeFile(ignore, "vignettes/*.html\nvignettes/*.R\n");
	}
}

build_vignettes_to_md = function(o, packageDir) {
	nelapply(o$description$vignettes, function(name, p){
		name = splitPath(name)$base;
		knitr::knit(
			Sprintf('%{packageDir}s/vignettes/%{name}s.Rmd'),
			Sprintf('%{packageDir}s/doc/%{name}s.md')
		);
	});
}

installVignettes = function(o, packageDir, keep_md = TRUE) {
	vignettes = o$description$vignettes;
	if (!is.list(vignettes)) o$description$vignettes = listKeyValue(vignettes, vignettes);
	nelapply(o$description$vignettes, installVignette, o = o, packageDir = packageDir);
	# keep_md = T does not seem to work, even in conjunction with a markdown parameter -> workaround
	build_vignettes(packageDir, keep_md = FALSE);
	if (keep_md) build_vignettes_to_md(o, packageDir);
}

#doInstall = FALSE, debug = F, gitOptions = list(), noGit = F, lib = NULL, tarDir = packagesDir)
createPackageWithConfig = function(o, packagesDir = '~/src/Rpackages',
	doInstall = FALSE, debug = F, gitOptions = list(), noGit = F, lib = NULL, tarDir = tempdir(),
	asCran = FALSE) {
	if (debug) print(o);

	i = packageInterpolationDict(o, debug);
	pdir = Sprintf('%{packagesDir}s/%{name}s', o);
	packageDir = Sprintf('%{packagesDir}s/%{name}s', o);

	# <p> folders
	Dir.create(Sprintf('%{pdir}s'));
	Dir.create(Sprintf('%{pdir}s/R'));

	# <p> copy R files
	#src = Sprintf('%{dir}s/%{files}s', dir = o$dir, files = o$files, sprintf_cartesian = T);
	#src = Sprintf('%{dir}s/%{files}s', o, sprintf_cartesian = T);
	src = sapply(o$files, function(f)if (splitPath(f)$isAbsolute) f else Sprintf('%{dir}s/%{f}s', o));
	dest = Sprintf('%{packageDir}s/R/');
	LogS(2, 'Copying files: %{f}s -> %{dest}s', f = join(src, ', '));
	File.copy(src, dest, symbolicLinkIfLocal = F, overwrite = T);
	# remove surplus files <N>
	surplus = setdiff(list.files(dest), sapply(src, \(.)splitPath(.)$file));
	LogS(2, "Removing surplus files [%{dest}s]: %{f}s", f = join(src, ', '));
	#File.remove(paste0(dest, surplus));

	# <p> copy files for inst sub-folder
	nelapply(o$instFiles, function(n, files) {
		dest = Sprintf('%{packageDir}s/inst/%{n}s');
		#dest = Sprintf('%{packageDir}s/%{n}s');
		Dir.create(dest, recursive = T);
		LogS(2, 'Copying files: %{f}s -> %{dest}s', f = join(src, ', '));
		File.copy(files, dest, symbolicLinkIfLocal = F, overwrite = T);
	});

	# <p> extract package documentation
	doc = sapply(src, function(f)
		Regexpr('(?s)(?<=#__PACKAGE_DOC__\\n).*?(?=#__PACKAGE_DOC_END__\\n)', readFile(f))
	);
	# conflicts
	Ndoc = length(which(sapply(doc, nchar) > 0));
	if (Ndoc > 1) stop('More than one package documentation files/sections found');
	if (Ndoc == 1) {
		if (any(sapply(src, function(f)splitPath(f)$file) == Sprintf('%{name}s.R', o)))
			stop(Sprintf('%{name}s.R should contain package documentation which is also given elsewhere. %{name}s.R will be overwritten so that it cannot be used as an input file.', o));
		# substitute in fields from configuration [description]
		#doc0 = paste0(packageDocPrefix, join(doc, ''), "\n\"_PACKAGE\"\n");
		doc0 = paste0(packageDocPrefix, unlist(doc), "\n\"_PACKAGE\"\n");
		doc1 = Sprintf(doc0, o$description, name = o$name);
		doc2 = gsub("(^#)|((?<=\n)#)", "#'", doc1, perl = T);
		#print(doc2);
		writeFile(Sprintf('%{pdir}s/R/%{name}s.R', o), doc2);
		o$files = c(Sprintf('%{name}s.R', o), o$files);	# documentation file to list of files
	}

	# <p> update NEWS file
	writeFile(with(o, Sprintf('%{pdir}s/NEWS')), firstDef(o$description$news, '0.1-0	Initial release'));
	writeFile(with(o, Sprintf('%{pdir}s/DESCRIPTION')), packageDescription(o));

	# <p> testing
	if (notE(o$testing) && o$testing$doInstall && !nif(o$testing$prevent))
		installTests(o, packageDir, asCran = asCran);
	# <p> roxigen2
	#Library(c('roxygen2', 'devtools'));
	#document(packageDir, roclets = c('namespace', 'rd'));
	#document(packageDir, roclets = c('rd'));
	#roxygenize(packageDir, roclets = c('rd'), clean = TRUE);
	roxygenize(packageDir, clean = TRUE);

	# <p> vignettes
	if (notE(o$description$vignettes)) installVignettes(o, packageDir);

	# <p> git
	if (!noGit && notE(o$git)) gitActions(o, packagesDir, debug, gitOptions);

	# <p> install
	if (doInstall) Install_local(Sprintf('%{packageDir}s'), upgrade = 'never', lib = lib, tarDir = tarDir);

	return(o);
}

packageDefFromPath = function(path) {
	myEnv = new.env();
	# <N> instead of Source: avoid RCurl dependency
	SourceLocal(path, local = myEnv);
	def = get('packageDefinition', envir = myEnv);
	# add definition file as first file <!> (no forward declarations)
	# <!> 30.9.2021 changed to last position to allow referencing
	def$files = unique(c(def$files, splitPath(path)$file));
	return(def);
}

probeDefinition = function(desc, dir = NULL) {
	path = Sprintf('%{pre}s%{file}s', pre = circumfix(dir, '/'), file = desc[1]);
	sp = splitPath(path);
	o = switch(sp$ext,
		# <!> assume unique is stable
		R = packageDefFromPath(desc[1]),
		plist = propertyFromStringExt(readFile(path)),
		json = ({ requireNamespace('jsonlite'); read_json(path) }),
		yaml = ({ requireNamespace('yaml'); read_yaml(path) })
	);
	o$dir = firstDef(dir, sp$dir);
	o$version = firstDef(o$description$version, Regexpr('\\S+', o$description$news)[[1]], '0.1-0');
	return(o);
}

packageTarOSOptions = list(Linux = '--overwrite', Windows = c());
checkPackage = function(packageDesc, packagesDir, asCran = TRUE, copyCranTarball = TRUE, clean = TRUE, noGit = FALSE)
	with (packageDesc, {
	checkDir = packageDir = Sprintf("%{packagesDir}s/%{name}s");
	packagesDirPrev = packagesDir;
	if (!noGit && file.exists(Sprintf("%{packageDir}s/.git"))) {
		packagesDir = tempdir();
		checkDir = Sprintf('%{packagesDir}s/%{name}s');
		# <i> move if exists
		if (file.exists(checkDir) && clean) {
			file.rename(checkDir, Sprintf('%{checkDir}s-%{i}d', i = as.integer(runif(1)*1e6)))
		}
		dir.create(checkDir, FALSE);
		#SystemS('cd %{packageDir}q ; git archive --format tar HEAD | ( cd %{checkDir}q ; tar xf - --exclude inst/doc --overwrite )', 2);
		#SystemS('cd %{packageDir}q ; git archive --format tar HEAD | ( cd %{checkDir}q ; tar xf - --overwrite )', 2);
		#SystemS('cd %{packageDir}q ; git archive --format tar HEAD | ( cd %{checkDir}q ; tar xf - --overwrite )', 2);
		# 		cmdGit = JoinCmds(Sprintf(c('cd %{packageDir}q', 'git archive --format tar HEAD')));
		# 		cmdTar = JoinCmds(Sprintf(c('cd %{checkDir}q', 'tar xf - --overwrite')));
		# 		SystemS('%{cmdGit}s | ( %{cmdTar}s )', 2);
		fArchive = tempfile();
		SystemS('git archive --format tar HEAD -o %{fArchive}q', 2, wd = packageDir);
		#SystemS('tar xf %{fArchive}q --overwrite', 2, wd = checkDir);
		untar(fArchive, exdir = checkDir, extras = packageTarOSOptions[[ Sys.info()['sysname'] ]]);
	}
	cran = if (asCran) '--as-cran' else '';
	# 	SystemS(JoinCmds(c(
	# 		'cd %{packagesDir}q',
	# 		'R CMD build %{name}q',
	# 		'R CMD check %{cran}s %{name}q_%{version}s.tar.gz'
	# 	)), 2);
	SystemS('R CMD build %{name}q', 2, wd = packagesDir);
	SystemS('R CMD check %{cran}s %{name}q_%{version}s.tar.gz', 2, wd = packagesDir);
	if (asCran && copyCranTarball) {
		from = Sprintf('%{packagesDir}s/%{name}q_%{version}s.tar.gz');
		to = Sprintf('%{packagesDirPrev}s/%{name}q_%{version}s-cran.tar.gz');
		File.copy(from, to, overwrite = T, symbolicLinkIfLocal = F, logLevel = 2);
	}
})

#' Create package from vector of source files and single configuration
#' 
#' This function creates a package dir, runs documentation generation using roxygen and optionally installs the package. It can also update via git and manage version numbers. In a minimal configuration, a single file is sufficient to create a fully documented R package.
#' 
#' @alias createPackageWithConfig
#' @param packageDesc path to the configuration file (R, extended plist format, json, yaml).
#'	If an R-file is provided, it is sourced in a seperate environment and needs to define the variable
#'	`packageDefinition`. This variable has to be a list which is further specified in the details below.
#'	If a propertyList/JSON/YAML-file is provided, they have to parse into a list corresponding to
#'	`packageDefinition`. Functions \code{propertyFromStringExt}, \code{read_json}, and \code{read_yaml} are
#'	used for parsing, coming from packages \code{package}, \code{jsonlite}, and \code{yaml}.
#' @param packagesDir folder in which the folder structure of the package is written
#' @param doInstall flag to indicate whether the package should also be installed
#' @param doCheck whether to run R CMD check --as-cran on the package
#' @param dir directory to search for package definition
#' @param gitOptions list with options for git interaction
#' @param noGit boolean to suppress git calls, overwrites other options
#' @param lib Path to package library as forwarded to \code{install.packages}
#' @param asCran boolean to indicate whether check should be including --as-cran
#'
#' @details This function creates a valid R package folder with DESCRIPTION, LICENSE and NEWS files.
#'	All R-files listed are copied to this directory and documentation is created by  
#'	running the \code{devtools::document} function on this folder.
#' The package is specified through a list coming from an R script or configuration file.
#'	The following elements control package generation. In this list \code{key1-key2} indicates a
#'	sublist-element, i.e. it stand for \code{packageDefinition[[key1]][[key2]]}.
#'	
#'	\itemize{
#'		\item{name: }{The name of the pacakge}
#'		\item{files: }{R-files to be put into the package. If an R-file is used for the configuration,
#'			it will automatically be include. This allows package definition through a single R-file.}
#'		\item{instFiles: }{Other files to be installed. This is a list, the names of which specify
#'			sub-folders in the \code{inst} sub-directory of the package. Values are character vectors
#'			specifying files to be put there.}
#'		\item{description: }{A sub-list with elements specifying the \code{DESCRIPTION} file of the package.}
#'		\item{description-title: }{The title of the help page 'package-myPackage' (myPackage is the name
#'			of the package) and the title fiedl in DESCRIPTION.}
#'		\item{description-author: }{The \code{Author} field in DESCRIPTION.
#'		\item{description-description: } The \code{Description} field in DESCRIPTION. The text is re-used
#'			for the package documentation as displayed by ?'package-myPackage' and is prepended to the 
#'			seperate package documentation. It is also inserted as a Description section of a Markdown file
#'			if git is used. This string should therefore not make use of roxygen/markdown markup.}
#'		\item{description-depends: }{A character vector with package dependencies.}
#'		\item{description-suggests: }{A character vector with package suggestions.}
#'		\item{description-news: }{A character vector with a single element containing the \code{NEWS}
#'			file of the package. It is assumed that a line starting with a non white space character
#'			indicates a new version. The version identifier is taken to be all characters until the
#'			first white space. Once a new version is detected, git actions might be triggered (see below).}
#'		\item{git: }{A sub-list specifying git behavior. If you do not
#'			want to use git, omit this entry from \code{packageDefinition}. If present, a git repository
#'			is created and maintained in the target folder. Default \code{git} settings for the package can be 
#'			overwritten using the  \code{gitOptions} argument.}
#'		\item{git-push: }{Logical, whether to push each commit, defaults to \code{FALSE}.}
#'		\item{git-pushOnNewVersion: }{Logical, whether to push each when a new release is created
#'			(see item \code{description-news}. Defaults to \code{TRUE}.
#'			A push is automatically performed, once a new release is created, irrespective of this setting.
#'			To suppress a push in these cases, push has to set to \code{FALSE} in the. }
#'		\item{git-readme: }{Character vector with single entry with the content of a readme file that is put
#'			as \code{README.md} into the package main folder. The description given under
#'			\code{description-description} is appended to this text.}
#'	}
#' @author Stefan Böhringer, \email{r-packages@@s-boehringer.org}
#' @seealso package-package
#' @keywords create package createPackage
#' @examples
#' 
#' packageDefinition = list(
#' 	name = 'pkg-minimal',
#' 	files = c(),
#'	instFiles = list(),
#' 	description = list(
#' 		title = 'Minimal R-package created with `package`',
#' 		# version to be documented in news section
#' 		#version = '0.1-0',
#' 		author = 'Stefan Böhringer <r-packages@s-boehringer.org>',
#' 		description = 'This appears in the package-documentaion, the markdown of the git-repository and in the package details.',
#' 		depends = c(),
#' 		suggests = c(),
#' 		license = 'LGPL',
#' 		news = "0.1-0	Initial release"
#' 	),
#' 	git = list(
#' 		readme = '## Installation\n```{r}\nlibrary(devtools);\ninstall_github("user/pkg-minimal")\n```\n',
#' 		push = FALSE,
#' 		pushOnNewVersion = FALSE
#' 	)
#' );
#'
#'
#' @export createPackage
createPackage = function(packageDesc, packagesDir = '~/src/Rpackages',
	dir = NULL, doInstall = FALSE, doCheck = T, gitOptions = list(), noGit = F, lib = NULL, asCran = FALSE) {
	packageDef = probeDefinition(packageDesc, dir);
	#print(packageDef);
	r = createPackageWithConfig(packageDef, packagesDir, doInstall,
		gitOptions = gitOptions, noGit = noGit,
		lib = lib, asCran = asCran
	);
	if (doCheck) checkPackage(packageDef, packagesDir, asCran, noGit = noGit);
	return(r);
}

#
#	<p> local development
#
SourcePackage = function(path) {
	def = packageDefFromPath(path);
	Source(def$files, locations = splitPath(path)$dir);
}

LibraryPackage = function(path, suggests = TRUE) {
	def = packageDefFromPath(path);
	Library(def$description$depends);
	if (suggests) Library(def$description$suggests);
}

SetupPackage = function(path, suggests = TRUE) {
	LibraryPackage(path, suggests = suggests);
	SourcePackage(path);
}
#
#	Rshiny.R
# Tue Mar 23 12:02:30 CET 2021

# 	installWorkflow(list(title = 'Initial Data Analysis - PREPARE'),
# 	markdown = 'md/PREPARE-IDA.html',
# 	files = c('RgenericAllRaw.R'),
# 	destination = 'shiny')


# parameters
#	title
#	markdown: markdown workflow
#	files: copy/symlink files
#	destination folder
#	maxRequestSize

Mb = 1024^2;
installWorkflowDefaults = list(
	title = 'Generic Workflow',
	files = 'RgenericAllRaw.R',
	destination = 'shiny',
	maxRequestSize = 50 * Mb,
	port = 7780L,
	host = '127.0.0.1',
	standardFiles = c('RgenericAllRaw.R', 'shiny/app.R', 'shiny/ui.R', 'shiny/runApp')
);

installWorkflow = function(parameters, markdown, files, destination,
	standardFilesDir = '~/src/Rprivate') {
	path = Sprintf('%{destination}s/workflow');
	parameters = merge.lists(installWorkflowDefaults, parameters);
	if (missing(destination)) destination = paramters$destination;
	
	dir.create(path, recursive = TRUE, showWarnings = FALSE);
	installFiles = c(markdown, files);
	if ('shinyGenericWorkflow.R' %in% installFiles)
		stop('shinyGenericWorkflow.R not allowed as a file to install');
	File.copy(installFiles, path, overwrite = TRUE, symbolicLinkIfLocal = F);
	File.copy(paste(standardFilesDir, parameters$standardFiles, sep =  '/'), destination, overwrite = T);
	print(installFiles);
	parsFull = c(parameters, list(workflow = markdown, files = files));
	dput(parsFull, Sprintf('%{destination}s/parameters.R'));

	return(parsFull);
}

assignListIntoEnv = function(l = list(), envir = parent.frame()) {
	nelapply(l, function(n, e)assign(n, e, env = envir));
	return(env);
}
