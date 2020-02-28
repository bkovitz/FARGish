from sys import stdout
from contextlib import AbstractContextManager

class Indenting(AbstractContextManager):
    '''A wrapper for a file object that adds automatic indentation of lines.
    The wrapper is a context manager: every entrance into a new context
    increases the indent level by 1, and every exit decreases it by 1.

    The Indenting object has a .wrote_any member, which is True iff at least
    one character has been written to the object since it was created.
    It's OK to reset it to False from the outside.'''

    def __init__(self, file, prefix = ' ' * 4):
        self.file = file
        self.at_start_of_line = True
        self.prefix = prefix
        self.level = 0
        self.wrote_any = False

    def __getattr__(self, name):
        if name == 'write':
            return self.write_with_indent
        else:
            return getattr(self.file, name)

    # __getattr__ doesn't intercept __iter__
    def __iter__(self, *args, **kwargs):
        return self.file.__iter__(*args, **kwargs)

    def write_with_indent(self, s):
        if not s:
            return 0
        self.wrote_any = True
        if s == '\n':
            self.at_start_of_line = True
            return self.file.write(s)
        num_written = 0
        for line in s.splitlines(keepends=True):
            if self.at_start_of_line:
                num_written += self.file.write(self.prefix * self.level)
            num_written += self.file.write(line)
            self.at_start_of_line = True

        self.at_start_of_line = s[-1] == '\n'
        return num_written

    def __enter__(self):
        self.level += 1
        return self

    def __exit__(self, *args, **kwargs):
        self.level -= 1
        return None

def indent(f):
    '''A function to make 'with' statements more readable: with indent(f): ...
    
    f must be an Indenting object.'''
    if not isinstance(f, Indenting):
        raise ValueError('Object must be an instance of Indenting.')
    return f

def run():
    f = Indenting(stdout)
    print('Blah', file=f)
    with indent(f):
        print('Indented\nGah', file=f)
        with indent(f):
            print('Indented twice', file=f)
        print('Back to once', file=f)
    print('Back to nothing', file=f)
