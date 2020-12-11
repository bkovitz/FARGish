# numbo3bfuncs.py -- External functions for numbo3b.farg

def backtrack(g, nodeid):
    # Find nodeid's builder.
    # If already tagged Failed, abort. Or does this even matter?
    # Find all the nodes built by the builder.
    # Tag them all Failed.
    # Move any Avail tags to the builder's inputs.
