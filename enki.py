import sys
import os
import ninja_syntax as ninja

def dict_merge(a: dict, b: dict, path=[]):
    for key in b:
        if key in a:
            if isinstance(a[key], dict) and isinstance(b[key], dict):
                dict_merge(a[key], b[key], path + [str(key)])
            else:
                a[key] = b[key]
        else:
            a[key] = b[key]
    return a

class Object:
    def __init__(self, rule : str, source : str, out : str):
        self.rule   = rule
        self.source = source
        self.out    = out

        self.deps  : list[str] = []
        self.flags : list[str] = []

class Target:
    def __init__(self, name : str, type : str, src_dir : str, target_os : str):
        self.name      = name
        self.src_dir   = src_dir
        self.type      = type
        self.target_os = target_os

        self.flags : dict[list[str]] = dict()
        self.flags["c"] = []

        self.libs      = []
        self.dylibs    = []
        self.deps      = []
        self.generated = []
        self.objects   = []
        self.variables : dict[str, str] = dict()

        self.lib_paths    : list[str] = []
        self.public_flags : dict[list[str]] = dict()

        self.out = ""
        self.ext = ""
        if self.type == "exe":
            self.rule = "link"

            if self.target_os == "win32": self.ext = ".exe"
            self.out = npath_join("$builddir", self.name + self.ext)
        elif self.type == "lib":
            self.rule = "ar"

            if self.target_os == "win32": self.ext = ".lib"
            else: self.ext = ".a"

            self.out = npath_join("$builddir", self.name + self.ext)

        self.gen_dir = npath_join(src_dir, "gen")


    def generate(self, n : ninja.Writer, parent) -> str:
        variables = dict_merge(parent.variables, self.variables)
        variables["gendir"] = self.gen_dir.replace("\\", "/")

        if self.gen_dir:
            gen_dir = resolve_variables(self.gen_dir, variables)
            if not os.path.exists(gen_dir): os.makedirs(gen_dir)

            n.variable("gendir", gen_dir)

        n.variable("objdir", npath_join(parent.obj_dir, self.name))
        n.newline()

        if self.dylibs: self.flags["link"].append("-Wl,-rpath,'$$ORIGIN'")

        t_flags = dict()
        for k, v in self.flags.items():
            if not v: continue

            t_flags[k] = []
            if k in parent.flags: t_flags[k].extend(parent.flags[k])
            t_flags[k].extend(v)

        for k, v in t_flags.items(): vars(n, k+"flags", v)
        n.newline()

        ogen_dep = []
        if self.generated: ogen_dep.append("$objdir/%s.stamp" % self.name)

        flibs   : list[str] = []

        for lib in self.dylibs:
            dylib = lib
            if self.target_os == "linux":
                if not dylib.endswith(".so"): dylib = dylib + ".so"
                if not dylib.startswith("lib"): dylib = "lib" + dylib
            elif self.target_os == "win32":
                if not dylib.endswith(".dll"): dylib = dylib + ".dll"

            flibs.append(f_lib(lib))

            # for path in self.lib_paths:
            #     fpath = resolve_variables(path, parent.variables)
            #     fpath = os.path.join(fpath, dylib)
            #
            #     if os.path.exists(fpath):
            #         lpath = os.path.join(path, dylib)
            #         if os.path.islink(fpath):
            #             rpath = os.path.realpath(fpath)
            #             print("dylib: {}, fpath: {}, lpath: {}, rpath: {}".format(dylib, fpath, lpath, rpath))
            #
            #         copy(self, lpath, "$builddir/{}".format(dylib))
            #         break

        objects : list[str] = []
        if self.objects:
            for obj in self.objects:

                order_only = []
                if obj.rule == "cxx" or obj.rule == "cc":
                    order_only.extend(ogen_dep)

                source_name = os.path.splitext(obj.source)[0]
                n.build(obj.out, obj.rule, src(obj.source, self.src_dir),
                        implicit = obj.deps,
                        order_only = order_only)

                o_flags = []
                if obj.flags:
                    if obj.rule in t_flags: o_flags.extend(t_flags[obj.rule])
                    o_flags.extend(obj.flags)
                vars(n, "flags", o_flags, indent=1)

                objects.append(obj.out)

            for d in self.deps:
                if d.type != "exe": objects.append(d.out)
            n.newline()

        for lib in self.libs:
            if lib.endswith(".a"):
                rlib = resolve_variables(lib, variables)
                if os.path.exists(rlib):
                    objects.append(rlib)
                else:
                    found = False
                    for path in self.lib_paths:
                        path = npath_join(path, rlib)
                        path = resolve_variables(path, variables)

                        if os.path.exists(path):
                            objects.append(path)
                            found = True
                            break

                    if not found:
                        print("unable to find library: %s" % lib)
                        exit(1)
            else:
                flibs.append(f_lib(lib))

        generated : list[str] = []
        if self.generated:
            for gen in self.generated:
                source_name = os.path.splitext(gen.source)[0]
                n.build(gen.out, gen.rule, src(gen.source, self.src_dir), implicit = gen.deps)
                vars(n, "flags", gen.flags, indent=1)

                generated.append(gen.out)
            n.newline()

            sgenerated = " ".join(generated)
            n.build("$objdir/%s.stamp" % self.name, "touch", order_only = generated)
            n.build("gen.%s" % self.name, "phony", "$objdir/%s.stamp" % self.name)
            n.newline()

        n.newline()
        n.build(self.out, self.rule, objects, order_only = generated)
        vars(n, "libs", flibs, 1)

        if self.ext:
            n.newline()
            n.build(npath_join("$builddir", self.name), "phony", self.out)

        print("wrote %s." % os.path.basename(n.output.name))

class Ninja:
    def __init__(self, name : str, root : str, args : str, target_os : str):
        self.root      = root
        self.build_dir = npath_join(root, name).replace("\\", "/")

        self.host_os = sys.platform
        self.target_os = target_os

        self.variables : dict[str, str] = dict()
        self.flags     : dict[str, list[str]] = dict()
        self.targets   : list[Target] = []
        self.test_targets : list[Target] = []
        self.rules     : dict[str, str] = dict()
        self.default   : Target = None

        if not os.path.exists(self.build_dir): os.makedirs(self.build_dir)

        path = os.path.join(self.build_dir, "build.ninja")
        self.writer    = ninja.Writer(open(path, "w"))


        self.obj_dir = npath_join(self.build_dir, "obj")
        if not os.path.exists(self.obj_dir): os.makedirs(self.obj_dir)

        self.variables["root"]     = self.root.replace("\\", "/")
        self.variables["builddir"] = self.build_dir.replace("\\", "/")
        self.variables["objdir"]   = self.obj_dir.replace("\\", "/")
        self.variables["gendir"]   = npath_join(self.build_dir, "gen")
        self.variables["configure_args"] = " ".join(args)

        self.flags["c"] = []

        for k, v in self.variables.items(): self.writer.variable(k, v)
        self.writer.newline()


        # re-generate ninja rule and target
        enki_dir = os.path.dirname(os.path.realpath(__file__))
        self.rule("configure",
             command="%s $root/configure.py $configure_args" % sys.executable,
             description = "regenerate ninja",
             generator=True)

        self.writer.build("build.ninja", "configure",
                 implicit = [
                     "$root/configure.py",
                     os.path.join(enki_dir, "ninja_syntax.py"),
                     os.path.join(enki_dir, "enki.py"),
                 ])
        self.writer.newline()

        # generic run utility
        self.rule("run", "$in $flags",
                  description = "RUN $in",
                  pool = "console")

        # file util rules
        if self.host_os == "linux":
            self.rule("copy", "cp $in $out", description = "COPY $out")
            self.rule("symlink", "ln -s $in $out", description = "SYMLINK $out -> $in")

        # generate-header utility
        if self.host_os == "win32":
            self.rule("meta", "$builddir/meta.exe $flags $in -o $out -- $cflags",
                       description = "META $in",
                       restat = True)
        elif self.host_os == "linux":
            self.rule("meta", "$builddir/meta $flags $in -o $out -- $cflags",
                       description = "META $in",
                       restat = True)

        meta = self.executable("meta", "$root/tools/enki")
        if self.host_os == "win32": define(meta, "_CRT_SECURE_NO_WARNINGS");
        lib_path(meta, "$builddir")
        include_path(meta, "$root/external/LLVM/include")
        cxx(meta, "meta.cpp")

        if self.target_os == "win32":
            lib_path(meta, "$root/external/LLVM/lib/win64")
            lib(meta, "libclang")
        elif self.target_os == "linux":
            copy(meta, "$root/external/LLVM/lib/linux/libclang.so.17.0.2", "$builddir/libclang.so.17.0.2")
            symlink(meta, "$builddir/libclang.so.17.0.2", "$builddir/libclang.so.17")
            symlink(meta, "$builddir/libclang.so.17.0.2", "$builddir/libclang.so")
            dylib(meta, "clang")

    def rule(self, name : str, command : str, **kwargs):
        if name not in self.flags: self.flags[name] = []
        self.rules[name] = command

        self.writer.rule(name, command, **kwargs)

    def executable(self, name : str, src_dir : str = "") -> Target:
        t = Target(name, "exe", src_dir, self.target_os)
        for r, c in self.rules.items(): t.flags[r] = []

        self.targets.append(t)
        return t

    def library(self, name : str, src_dir : str = "", flags: dict[str,list[str]] = None) -> Target:
        ext = ".a"
        if self.target_os == "win32": ext = ".lib"

        t = Target(name, "lib", src_dir, self.target_os)
        self.targets.append(t)

        if flags:
            for k, v in flags.items(): t.flags[k] = v
        return t

    def test(self, parent : Target, src_dir : str, include_header : str = None) -> Target:
        t = Target("test.%s" % parent.name, "exe", src_dir, self.target_os)
        for r, c in self.rules.items(): t.flags[r] = []
        dep(t, parent)

        if include_header:
            t.flags["c"].append("-include " + include_header)

        self.test_targets.append(t)
        return t

    def generate(self):
        json = open(os.path.join(self.build_dir, "compile_commands.json"), "w")
        json.write("[\n")

        for t in self.targets:
            src_dir = resolve_variables(t.src_dir, self.variables)
            src_dir = src_dir.replace("\\", "/")

            for o in t.objects:
                source = src(o.source, t.src_dir)
                source = resolve_variables(source, self.variables)
                source = relpath(source, src_dir)

                command = resolve_variables(self.rules[o.rule], self.variables)
                command = command.replace("$in", source)
                command = command.replace("$out", o.out)

                for k, flags in t.flags.items():
                    sflags = " ".join(flags)
                    command = command.replace("$%sflags" % k, sflags)

                oflags = " ".join(o.flags)
                command = command.replace("$flags", oflags)

                for rule in self.flags.keys():
                    if rule not in t.flags:
                        command = command.replace(rule, "")

                command = resolve_variables(command, self.variables)
                command = command.replace('\\', '/')
                command = command.replace('"', '\\"')

                json.write('\t{\n');
                json.write('\t\t\"file": "%s",\n' % source)
                json.write('\t\t\"directory": "%s",\n' % src_dir)
                json.write('\t\t\"command": "%s",\n' % command)
                json.write('\t},\n');
        json.write("]\n")

        self.writer.newline()

        for k, v in self.flags.items(): vars(self.writer, k+"flags", v)
        self.writer.newline()

        gen_targets : list[str] = []
        for t in self.targets:
            filename = t.name + ".ninja"
            path = os.path.join(self.build_dir, filename)

            t.generate(ninja.Writer(open(path, "w")), self)
            self.writer.subninja(npath_join("$builddir", filename))

            if t.generated: gen_targets.append("gen.%s" % t.name)

        for t in self.test_targets:
            filename = t.name + ".ninja"
            path = os.path.join(self.build_dir, filename)
            self.writer.subninja(npath_join("$builddir", filename))

            w = ninja.Writer(open(path, "w"))
            t.generate(w, self)

            w.newline()
            w.build("$builddir/%s.stamp" % t.name, "run", "$builddir/{}{}".format(t.name, t.ext))

        if self.targets: self.writer.newline()
        for t in self.targets: self.writer.build(t.name, "phony", t.out)

        test_targets : list[str] = []
        for t in self.test_targets:
            self.writer.build(t.name, "phony", npath_join("$builddir", "%s.stamp" % t.name))
            test_targets.append(t.name)

        if gen_targets or test_targets: self.writer.newline()
        if gen_targets: self.writer.build("gen.all", "phony", gen_targets)
        if test_targets: self.writer.build("test.all", "phony", test_targets)


        if self.default:
            self.writer.newline()
            self.writer.default(self.default.name)
        print("wrote %s." % os.path.basename(self.writer.output.name))


def npath_join(*args) -> str: return "/".join(args)
def normpath(p : str) -> str: return os.path.normpath(p).replace("\\", "/")
def relpath(path : str, root : str = None) -> str: return os.path.relpath(path, root).replace("\\", "/")

def resolve_variables(s : str, variables : dict[str, str]) -> str:
    while True:
        cont = False

        for k, v in variables.items():
            replaced = s.replace("$"+k, v)
            if replaced != s: cont = True
            s = replaced

        if not cont: break

    return s

def src(source : str, src_dir : str):
    if not source.startswith("$"):
        return npath_join(src_dir, source)

    return source


def shell_escape(s : str) -> str:
    # This isn't complete, but it's just enough to make NINJA_PYTHON work.
    if sys.platform == "win32":
      return s
    if '"' in s:
        return "'%s'" % s.replace("'", "\\'")
    return s

def f_inc(path : str) -> str: return "-I"+normpath(path)
def f_lib_path(path : str) -> str: return "-L"+normpath(path)
def f_lib(path : str) -> str: return "-l"+normpath(path)
def f_lib(path : str) -> str: return "-l"+normpath(path)
def f_define(var : str) -> str: return "-D"+var

def rule(n, name, command, description = None, depfile = None):
    r = n.rule(name, command, description = description, depfile = depfile)
    n.newline()
    return r

def vars(n : ninja.Writer, name : str, flags : list[str], indent=0):
    if not flags: return
    n.variable(name, " ".join(shell_escape(flag) for flag in flags), indent)


def include_path(t : Target, paths : list[str], public = False) -> [str]:
    if type(paths) is not list: return include_path(t, [paths], public)

    for path in paths:
        flag = f_inc(src(path, t.src_dir))

        if "c" not in t.flags: t.flags["c"] = []
        t.flags["c"].append(flag)

        if public:
            if "c" not in t.public_flags: t.public_flags["c"] = []
            t.public_flags["c"].append(flag)

    return paths

def lib_path(t : Target, paths : list[str], public = False) -> [str]:
    if type(paths) is not list: return lib_path(t, [paths], public)

    for path in paths:
        t.lib_paths.append(path)
        flag = f_lib_path(path)

        if "link" not in t.flags: t.flags["link"] = []
        t.flags["link"].append(flag)

        if public:
            if "link" not in t.public_flags: t.public_flags["link"] = []
            t.public_flags["link"].append(flag)

    return paths

def define(t : Target, vars : list[str], public = False) -> [str]:
    if type(vars) is not list: return define(t, [vars], public)

    for var in vars:
        d = f_define(var)
        if sys.platform == "win32" and "\"" in d:
            d = '"%s"' % d.replace('"', '\\"')

        if "c" not in t.flags: t.flags["c"] = []
        t.flags["c"].append(d)

        if public:
            if "c" not in t.public_flags: t.public_flags["c"] = []
            t.public_flags["c"].append(d)

    return vars

def cxx(t : Target, sources : list[str], deps : list[str] = None, flags : list[str] = None) -> list[Object]:
    if type(sources) is not list: return cxx(t, [sources], deps, flags)

    objs : list[Object] = []
    for source in sources:
        source_name = os.path.splitext(source)[0]
        if source_name.startswith("$"): source_name = os.path.basename(source)
        out = npath_join("$objdir", normpath(source_name+".o"))

        o = Object("cxx", source, out)
        if deps:  o.deps.extend(deps)
        if flags: o.flags.extend(flags)
        objs.append(o)

    t.objects.extend(objs)
    return objs

def cc(t : Target, sources : list[str], deps : list[str] = None, flags : list[str] = None) -> list[Object]:
    if type(sources) is not list: return cc(t, [sources], deps, flags)

    objs : list[Object] = []
    for source in sources:
        source_name = os.path.splitext(source)[0]
        if source_name.startswith("$"): source_name = os.path.basename(source)
        out = npath_join("$objdir", normpath(source_name+".o"))

        o = Object("cc", source, out)
        if deps:  o.deps.extend(deps)
        if flags: o.flags.extend(flags)
        objs.append(o)

    t.objects.extend(objs)
    return objs

def lib(t : Target, libs : [str]) -> [str]:
    if type(libs) is not list: return lib(t, [libs])

    t.libs.extend(libs)
    return libs

def dylib(t : Target, name : str):
    t.dylibs.append(name)

def copy(t : Target, src : str, dst : str):
    obj = Object("copy", src,  dst)
    t.objects.append(obj)

def symlink(t : Target, src : str, dst : str):
    obj = Object("symlink", src,  dst)
    t.objects.append(obj)

def dep(t : Target, deps : list[Target]):
    if type(deps) is not list: return dep(t, [deps])

    for d in deps:
        t.deps.append(d)

        for k, v in d.public_flags.items():
            if k not in t.flags: t.flags[k] = []
            t.flags[k].extend(v)

        for lib in d.libs:
            if lib not in t.libs: t.libs.append(lib)


def meta(t : Target, sources : list[str], flags : list[str] = None):
    if type(sources) is not list: return meta(t, [sources], flags)

    for source in sources:
        source_name = os.path.splitext(source)[0]
        out = npath_join("$gendir", normpath(source_name+".h"))

        o = Object("meta", source, out)
        o.deps.append("$builddir/meta")
        if flags: o.flags.extend(flags)

        t.generated.append(o)
