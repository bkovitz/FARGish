# Modules to install:
# matplotlib
# pyrsistent

ifeq ($(shell hostname),samosa)  # Ben's MacBook Air M1
PY = python3.10
BROWSER = open
else
PY = PYTHONHASHSEED=0 python3.10
BROWSER = google-chrome
endif

MYPY = mypy --python-version 3.10 #--version
MYPY_FLAGS = --show-error-codes --allow-redefinition

#SRCFILES = Addrs.py Canvas.py Funcs.py Indenting.py Log.py Model.py \
#	Painters.py Soup.py Subst.py Types.py util.py
SRCFILES = Indenting.py Log.py Model.py util.py

# Plot.py 
# Log.py 
# Numbo1.py 

# Type 'make' to run unit tests and then run current program in interactive
# mode.
ut_and_go: ut current

# Convenience target for "do whatever I'm currently working on".
test: ut at
mtest: mypy test

# Static typecheck. Requires that 'mypy' be installed.
mypy:
	which mypy
	@echo -e \\n\\n\\n\\n\\n
	$(MYPY) $(MYPY_FLAGS) $(SRCFILES) test*.py # atest*.py

mypya:
	$(MYPY) $(MYPY_FLAGS) a.py

# For type-checking just one file (specified here).
CURRENT = run.py
CURRENT_ARGS = --asteps=100 --lla=6 --llr=6 --au=0 --rngseed=6360465893744549081 #--rngseed=3572413201058895229  #1118343772539054194
#CURRENT_ARGS = ' m    ' --llr=4 --rngseed=2376745550637150490
#CURRENT_ARGS = 'm    ' --llr=4 --au=0 --rngseed=5801632031525413091 #--rngseed=2376745550637150490

R = $(CURRENT) $(CURRENT_ARGS)
r:   
	$(PY) $R
m:
	#$(MYPY) $(MYPY_FLAGS) --pdb --show-traceback $(CURRENT)
	$(MYPY) $(MYPY_FLAGS) $(SRCFILES)
mr: m r

i: m  # 'interactive' way to type-check and run current file of interest
	$(PY) -i $R
mi: i

# NEXT Why does ((1, 3, same), WorkingSoup, (1, 2, 'j')) get run?
# There is no painter (1, 3, same), so the source should not match anything.
# It happens at t=1 of regeneration, so this should be relatively easy to debug.
# ALSO Why is (3, 5, succ) even getting allowed as a DetPainter at t=1
# when cell 3 is blank?
bad:
	$(PY) run.py --asteps=100 --lla=6 --llr=6 --au=0 --rngseed=6360465893744549081 


# Run all unit tests.
ut:
	$(PY) -m unittest -v test*.py

# Convenience target for single unit test current being worked on
u:
	#$(PY) -m unittest -v testPainters.TestPainters.test_make_between_painter
	$(PY) -m unittest -v testPainter

# Acceptance tests
at:
	$(PY) -m unittest -v atest*.py

# Convenience target for single acceptance test current being worked on
a:
	$(PY) -m unittest -v atestExperiments.ATestExperiments.test_sequential_salt

mut: mypy ut
mu: mypy u
mat: mypy at
ma: mypy a
mt: mypy test
test: ut at

# line counts
lc: clean
	@echo 'LIVE: '
	@wc -l $(SRCFILES)
	@echo -e '\nTEST: '
	@wc -l test*.py atest*.py
#@wc -l `ls -1 *.py | grep -Ev '^a?test'`

# test coverage
# 'make ct bc' to generate coverage data and see it in a browser
ct coverage:
	coverage run -m unittest

bc browse-coverage:
	coverage html && $(BROWSER) htmlcov/index.html

tags:
	ctags *.py  # fargcyto.js  # force-directed*.js

clean:
	rm -f *.gen.py

.PHONY: ut tags clean test current ut_and_go lc
