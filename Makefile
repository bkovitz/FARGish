PY = python3.7

# Convenience target for "do whatever I'm currently working on".
current: ut
	$(PY) -i numbo5.py

ut:
	$(PY) -m unittest -v

lut:
	$(PY) -m unittest -v `cat LIVETEST`

# Convenience target for single test current being worked on
u:
	$(PY) -m unittest -v testView
	#$(PY) -m unittest -v test_slipnet
	#$(PY) -m unittest -v testBrute.TestBrute.test_brute

tags:
	ctags *.py fargcyto.js  # force-directed*.js

clean:
	rm *.gen.py

.PHONY: ut tags clean current
