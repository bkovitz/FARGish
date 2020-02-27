from sys import stdout
from contextlib import AbstractContextManager

class Indenting(AbstractContextManager):
    '''A wrapper for a file object that adds automatic indentation of lines.
    The wrapper is a context manager: every entrance into a new context
    increases the indent level by 1, and every exit decreases it by 1.'''

    def __init__(self, file, prefix = ' ' * 4):
        self.file = file
        self.at_start_of_line = True
        self.prefix = prefix
        self.level = 0

    def __getattr__(self, name):
        if name == 'write':
            return self.write_with_indent
        else:
            return getattr(self.file, name)

    def write_with_indent(self, s):
        if not s:
            return 0
        if s == '\n':
            self.at_start_of_line = True
            return self.file.write(s)
        num_written = 0
        lines = s.splitlines()
        for n, line in enumerate(lines):
            if self.at_start_of_line:
                num_written += self.file.write(self.prefix * self.level)
            num_written += self.file.write(line)
            if n + 1 < len(lines):
                num_written += self.file.write('\n')
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
