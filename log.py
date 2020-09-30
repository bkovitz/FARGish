# log.py -- Functions and classes for logging behavior of a FARG model

from util import as_iter


logging = set()  # LoggingObjects now active, i.e. logging
logging_objects = set()  # All LoggingObjects, regardless of whether active

class LoggingObject:

    def __init__(self):
        logging_objects.add(self)

    def __call__(self, s):
        if self:
            print(s)

    def start_logging(self):
        logging.add(self)

    def is_logging(self):
        return self in logging

    def stop_logging(self):
        logging.discard(self)

    def __bool__(self):
        return self in logging

ShowActiveNodes = LoggingObject()
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
