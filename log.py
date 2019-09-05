# logging.py -- Functions and classes for logging behavior of a FARG model

from util import as_iter


logging = set()

class LoggingObject:

    def start_logging(self):
        logging.add(self)

    def is_logging(self):
        return self in logging

    def stop_logging(self):
        logging.discard(self)

ShowResponseList = LoggingObject()
ShowResponseResults = LoggingObject()


def start_logging(os):
    for o in as_iter(os):
        logging.add(o)

#OAOO Should this or LoggingObject.is_logging() be authoritative?
def is_logging(os):
    return any(o in logging for o in as_iter(os))
