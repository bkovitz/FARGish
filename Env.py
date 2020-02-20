# Env.py -- A class that holds the environment (scope) in FARGish

from abc import ABC, abstractmethod

from exc import FARGishCompilerException
from util import NiceRepr


class EnvKeyError(LookupError):
    pass

class Env(NiceRepr):

    def __init__(self, items=None):
        '''Calls .add_to_env(self) on each item in items.'''
        self.stack = [{}]
        if items is not None:
            for item in items:
                item.add_to_env(self)

    def add(self, name, o):
        try:
            old_o = self.stack[-1][name]
            if old_o != o:
                raise FARGishCompilerException(
                    f'{name} is already defined as {old_o}'
                )
                #TODO Better error message
                #TODO Allow adding new info to old object
        except KeyError:
            self.stack[-1][name] = o

    #TODO UT
    def get_or_add(self, name, o):
        '''If name is already defined in innermost scope, returns the object
        that it is defined as. Otherwise calls o.add_to_env(self) and returns
        o.
        
        This makes it easy for calling code to define a name once and
        accumulate more information in the same object from 'definitions'
        that come later.'''
        try:
            return self.stack[-1][name]
        except KeyError:
            o.add_to_env(self)
            return o

    def push(self):
        self.stack.append({})

    def pop(self):
        self.stack.pop()

    def get(self, name):
        '''Returns the value of name in current scope, or None if undefined.'''
        for d in reversed(self.stack):
            try:
                return d[name]
            except KeyError:
                continue
        return None

    #TODO UT
    def __getitem__(self, name):
        '''Returns the value of name in current scope or raises KeyError.'''
        for d in reversed(self.stack):
            try:
                return d[name]
            except KeyError:
                continue
        raise EnvKeyError(name)

class EnvItem(ABC, NiceRepr):

    @abstractmethod
    def add_to_env(self, env):
        '''env is an Env object. add_to_env should update env as appropriate
        for any names that this EnvItem enters into scope.'''