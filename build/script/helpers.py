import os
from os import path
import types
from subprocess import Popen, PIPE, check_call

def run_command(args):
    # interpret a string as a whitespace-separated list of executable+arguments
    if isinstance(args, types.StringTypes):
        args = args.split()
    t = Popen(args, stdout=PIPE)
    stdout = t.communicate()[0]
    if t.returncode != 0:
        raise OSError("Call %s failed" % args)
    return stdout

class GitWrapper(object):
    def __init__(self):
        self.shallow = False
        self.supports_nofetch = True
        v = run_command('git --version')
        prefix = 'git version '
        def error():
            sys.stderr.write('Cannot parse "git --version" output, '
                             'assuming new version')
        if not v.startswith(prefix):
            error()
            return
        v = v[len(prefix):]
        try:
            v = [int(n) for n in v.split('.')[:3]]
        except:
            error()
            return
        if v < [1, 6, 2]:
            self.supports_nofetch = False

    def submodule_clone(self, name):
        if self.shallow:
            # Use a depth greater than 1 for shallow clones to reduce the
            # chance that required submodule versions are not fetched when
            # all branch heads in the corresponding repo have moved ahead.
            shallow_args = ['--depth', '100']
        else:
            shallow_args = []
        if self.supports_nofetch:
            nofetch_args = ['--no-fetch']
        else:
            nofetch_args = []
        if path.exists(path.join(name, '.git')):
            # If the submodule already exists just try to update it
            check_call('git submodule sync'.split()+[name])
            check_call('git submodule update'.split()+[name])
        else:
            # Do things manually instead of using "git submodule update --init",
            # because git's sucky submodule support has no way of giving
            # options to the "git clone" command that would be needed for
            # shallow clones.
            repo_addr = run_command('git config --get submodule.%s.url' % name)
            # old git versions fail to clone over an empty directory
            try:
                os.rmdir(name)
            except:
                # Don't fail if it already doesn't exist - having other
                # failure cases continue and fail later is OK.
                pass
            check_call('git clone'.split() + shallow_args + [repo_addr, name])
            check_call('git submodule update'.split() + nofetch_args + [name])

    def get_config(self):
        output = run_command('git config --null --list')
        result = {}
        for line in output.split(chr(0)):
            if not line:
                continue
            name, value = line.split('\n', 1)
            result[name] = value
        return result

    def get_submodules(self):
        output = run_command('git ls-files --stage')
        result = []
        for line in output.splitlines():
            mode, sha, stage, path = line.split(None, 3)
            if mode != '160000':
                continue
            result.append(path)
        return result

    def foreach_submodule(self, func, recurse=True):
        for module in self.get_submodules():
            if path.exists(path.join(module, '.git')):
                os.chdir(module)
                func()
                if recurse:
                    self.foreach_submodule(func)
                os.chdir('..')

    def foreach_module(self, func):
        func()
        self.foreach_submodule(func)

def parse_configfile(filename):
    if not path.exists(filename):
        return []
    args = []
    f = open(filename)
    for line in f:
        line = line.strip()
        if not line or line.startswith('#'):
            continue
        args.append(line)
    f.close()
    return args
