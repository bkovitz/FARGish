ifeq ($(shell hostname),samosa)  # Ben's MacBook Air M1
PY = python3.10
BROWSER = open
else
PY = PYTHONHASHSEED=0 python3.10
BROWSER = google-chrome
endif

MYPY = mypy --python-version 3.10 #--version
MYPY_FLAGS = --show-error-codes --allow-redefinition

SRCFILES = Canvas.py Funcs.py Model.py Subst.py Soup.py Types.py

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

# For type-checking just one file (specified here).
CURRENT = Model.py

m:
	$(MYPY) $(MYPY_FLAGS) --pdb --show-traceback $(CURRENT)
i: m   # 'interactive' way to type-check and run current file of interest
	$(PY) -i $(CURRENT)
mi: i

# Run all unit tests.
ut:
	$(PY) -m unittest -v test*.py

# Convenience target for single unit test current being worked on
u:
	#$(PY) -m unittest -v testModel.TestModel.test_rel_indirect_painter
	$(PY) -m unittest -v testLog

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
