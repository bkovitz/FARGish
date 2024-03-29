# Modules to install:
# matplotlib
# pyrsistent

ifeq ($(shell hostname),samosa)  # Ben's MacBook Air M1
PY = python3.10
BROWSER = open
MYPYPATH = /opt/homebrew/lib/python3.10
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
#	which mypy
#	which python3.10
#	echo $$PATH
#	echo $$MYPYPATH
#	echo $$BROWSER
#	echo $$PY
mypy:
	@echo -e \\n\\n\\n\\n\\n
	$(MYPY) $(MYPY_FLAGS) $(SRCFILES) test*.py # atest*.py

mypya:
	$(MYPY) $(MYPY_FLAGS) a.py

# For type-checking just one file (specified here).
CURRENT = spike3.py #Model.py #run.py
CURRENT_ARGS = --asteps=100 --lla=6 --llr=6 --au=0 --rngseed=6360465893744549081 #--rngseed=3572413201058895229  #1118343772539054194
#CURRENT_ARGS = ' m    ' --llr=4 --rngseed=2376745550637150490
#CURRENT_ARGS = 'm    ' --llr=4 --au=0 --rngseed=5801632031525413091 #--rngseed=2376745550637150490

R = $(CURRENT) $(CURRENT_ARGS)
r:   
	$(PY) $R
m:
	#$(MYPY) $(MYPY_FLAGS) --pdb --show-traceback $(CURRENT)
	#$(MYPY) $(MYPY_FLAGS) $(SRCFILES)
	$(MYPY) $(MYPY_FLAGS) $(CURRENT)
mr: m r

i: m  # 'interactive' way to type-check and run current file of interest
	$(PY) -i $R
mi: i

bad:
	$(PY) run.py --asteps=100 --lla=6 --llr=6 --au=0 --rngseed=6360465893744549081 


# Run all unit tests.
ut:
	$(PY) -m unittest -v test*.py

# Convenience target for single unit test current being worked on
u:
	$(PY) -m unittest -v testModel.TestArgumentRelationDetector.test_detect_arg_relation_in_succ
	#$(PY) -m unittest -v testModel.TestPainterCluster

# Acceptance tests
at:
	$(PY) -m unittest -v atest*.py

# Convenience target for single acceptance test current being worked on
a:
	$(PY) -m unittest -v atestExperiments.ATestExperiments.test_sequential_salt

mypy3:
	$(MYPY) $(MYPY_FLAGS) atestModel3.py Model3.py
	
at3:
	$(PY) -m unittest -v atestModel3

ut3:
	$(PY) -m unittest -v testModel3

mut3: mypy3 ut3
mat3: mypy3 at3

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
	ctags *.py

clean:
	rm -f *.gen.py

.PHONY: ut tags clean test current ut_and_go lc
