# log.py -- Functions and classes for logging behavior of a FARG model

from contextlib import contextmanager

from util import as_iter


logging = set()  # LoggingObjects now active, i.e. logging
logging_objects = set()  # All LoggingObjects, regardless of whether active

suppressing = False  # Is logging currently being suppressed?

# TODO Make it so you don't have to call this as a function; just:
#  with SuppressLogging:
@contextmanager
def SuppressLogging():
    global suppressing
    old_suppressing = suppressing
    try:
        suppressing = True
        yield True
    finally:
        suppressing = old_suppressing

class LoggingObject:

    def __init__(self):
        logging_objects.add(self)

    def __call__(self, s):
        if self:
            print(s)

    def start_logging(self):
        logging.add(self)

    def is_logging(self):
        global suppressing
        return (not suppressing) and (self in logging)

    def stop_logging(self):
        logging.discard(self)

    def __bool__(self):
        return self.is_logging()

ShowActiveNodes = LoggingObject()
ShowActiveNodesCollected = LoggingObject()
ShowActionList = LoggingObject()
ShowActionsChosen = LoggingObject()
ShowActionsPerformed = LoggingObject()
ShowPrimitives = LoggingObject()
ShowResponseList = LoggingObject()
ShowResponseResults = LoggingObject()
ShowOperandCandidates = LoggingObject()
ShowAnnotations = LoggingObject()
ShowIsMatch = LoggingObject()
ShowResults = LoggingObject()

logging.add(ShowResults)

def start_logging(os):
    for o in as_iter(os):
        logging.add(o)

def stop_all_logging():
    logging.clear()

def log_all():
    logging.update(logging_objects)

#OAOO Should this or LoggingObject.is_logging() be authoritative?
def is_logging(os):
    return any(o in logging for o in as_iter(os))
