import sys
import os
import json
import subprocess
from pathlib import Path
from typing import Dict, List, Optional, Any

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

class Rule:
    def __init__(self, command, description=None, depfile=None, generator=False, pool=None, restat=False, rspfile=None, rspfile_content=None, deps=None):
        self.command         = command
        self.description     = description
        self.depfile         = depfile
        self.generator       = generator
        self.pool            = pool
        self.restat          = restat
        self.rspfile         = rspfile
        self.rspfile_content = rspfile_content
        self.deps            = deps

class Object:
    def __init__(self, rule : str, source : str, out : str):
        self.rule   = rule
        self.source = source
        self.out    = out

        self.deps      : list[str]     = []
        self.flags     : list[str]      = []
        self.variables : dict[str, str] = dict()

class Target:
    def __init__(self, name : str, type : str, src_dir : str, target_os : str):
        self.name      = name
        self.src_dir   = src_dir
        self.type      = type
        self.target_os = target_os

        self._flags : dict[list[str]] = dict()
        self._flags["c"] = []

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

        self.gen_dir = npath_join(src_dir, "generated")

    def generate(self, n : ninja.Writer, parent) -> str:
        variables = dict_merge(parent.variables, self.variables)
        variables["gendir"] = self.gen_dir.replace("\\", "/")

        if self.gen_dir:
            gen_dir = resolve_variables(self.gen_dir, variables)
            if not os.path.exists(gen_dir): os.makedirs(gen_dir)

            n.variable("gendir", gen_dir)

        for k,v in self.variables.items():
            n.variable(k, v)

        n.variable("objdir", npath_join(parent.obj_dir, self.name))
        n.newline()

        if self.dylibs: self._flags["link"].append("-Wl,-rpath,'$$ORIGIN'")

        t_flags = dict()
        for k, v in self._flags.items():
            if not v: continue

            t_flags[k] = []
            if k in parent._flags: t_flags[k].extend(parent._flags[k])
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

                for k,v in obj.variables:
                    n.variable(k, v)

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
                        path = resolve_variables(path, variables)
                        path = npath_join(path, rlib)

                        if os.path.exists(path):
                            objects.append(path)
                            found = True
                            break

                    if not found:
                        print("unable to find library: %s" % lib)
                        exit(1)
            else:
                flibs.append(f_lib(lib))

        implicit_deps : list[str] = [ npath_join("$builddir", "compile_commands.json") ]
        order_deps : list[str] = []

        if self.generated:
            generated : list[str] = []
            for gen in self.generated:
                source_name = os.path.splitext(gen.source)[0]
                n.build(gen.out, gen.rule, src(gen.source, self.src_dir), implicit = gen.deps)
                vars(n, "flags", gen.flags, indent=1)

                for k,v in gen.variables.items():
                    n.variable(k, v, 1)

                generated.append(gen.out)
            n.newline()

            sgenerated = " ".join(generated)
            n.build("$objdir/%s.stamp" % self.name, "touch", order_only = generated)
            n.build("gen.%s" % self.name, "phony", "$objdir/%s.stamp" % self.name)
            n.newline()

            order_deps.extend(generated)

        n.newline()
        n.build(self.out, self.rule, objects, implicit = implicit_deps, order_only = order_deps)
        vars(n, "libs", flibs, 1)

        if self.ext:
            n.newline()
            n.build(npath_join("$builddir", self.name), "phony", self.out)

        print("wrote %s." % os.path.basename(n.output.name))

class CMake:
    def __init__(self, name : str, src_dir : str, target_os : str, opts : list[str] = None):
        self.name      : str          = name
        self.src_dir   : str          = src_dir
        self.opts      : list[str]    = opts
        self.target_os : str          = target_os
        self.targets   : list[Target] = []

    def generate(self, parent):
        self.build_dir = npath_join(parent.build_dir, self.name)
        self.src_dir = resolve_variables(self.src_dir, parent.variables)

        self.api_dir = npath_join(self.build_dir, ".cmake", "api", "v1")
        self.query_dir = npath_join(self.api_dir, "query")
        self.reply_dir = npath_join(self.api_dir, "reply")
        self.codemodel_query_dir = npath_join(self.query_dir, "codemodel-v2")

        print("Generating CMake submodule: {}".format(self.src_dir))
        os.makedirs(self.codemodel_query_dir, exist_ok=True)

        query_file = npath_join(self.codemodel_query_dir, "query.json")
        if not os.path.exists(query_file):
            with open(query_file, 'w') as f:
                json.dump({}, f)

        args = ["cmake", "-B", str(self.build_dir), "-S", str(self.src_dir), "-GNinja"]
        args.extend(["-DCMAKE_CXX_COMPILER=clang++", "-DCMAKE_C_COMPILER=clang"])

        if self.opts: args.extend(self.opts)
        subprocess.run(args)

        codemodel_file = self.find_codemodel_file()
        if not codemodel_file:
            print("Codemodel file not found")
            return {}

        with open(codemodel_file, 'r') as f:
            codemodel = json.load(f)

        self.targets_info = {}
        for config in codemodel.get("configurations", []):
            targets = config.get("targets", [])

            for target_ref in targets:
                target_file_path = npath_join(self.reply_dir, target_ref["jsonFile"])

                with open(target_file_path, 'r') as f:
                    target_data = json.load(f)

                name = target_data["name"]
                targets_info = self.process_target(target_data)
                self.targets_info[name] = targets_info

        if parent.verbose: self.print_all_targets_summary()

    def find_codemodel_file(self) -> Optional[str]:
        if not os.path.exists(self.reply_dir):
            return None

        for filename in os.listdir(self.reply_dir):
            if filename.startswith("codemodel-v2"):
                return npath_join(self.reply_dir, filename)
        return None

    def process_target(self, target_data: Dict[str, Any]) -> Dict[str, Any]:
        result = {
            "name": target_data["name"],
            "type": target_data.get("type", ""),
            "is_library": "LIBRARY" in target_data.get("type", ""),
            "artifacts": [],
            "include_directories": [],
            "link_libraries": [],
            "compile_definitions": [],
            "compile_options": [],
        }

        for artifact in target_data.get("artifacts", []):
            result["artifacts"].append({
                "path": artifact.get("path", ""),
                "output_name": os.path.basename(artifact.get("path", "")),
            })

        for compile_group in target_data.get("compileGroups", []):
            for include in compile_group.get("includes", []):
                include_path = include.get("path", "")
                if include_path and include_path not in result["include_directories"]:
                    result["include_directories"].append(include_path)

            for define in compile_group.get("defines", []):
                define_name = define.get("define", "")
                if define_name and define_name not in result["compile_definitions"]:
                    result["compile_definitions"].append(define_name)

            for option in compile_group.get("compileCommandFragments", []):
                fragment = option.get("fragment", "")
                if fragment and fragment not in result["compile_options"]:
                    result["compile_options"].append(fragment)

        for link_lib in target_data.get("link", {}).get("libraries", []):
            lib_name = link_lib.get("name", "")
            if lib_name:
                result["link_libraries"].append(lib_name)

        return result

    def print_all_targets_summary(self):
        print("\nAll Targets Summary:")
        print("=" * 80)

        target_types = set()
        for info in self.targets_info.values():
            target_types.add(info["type"])

        for target_type in sorted(target_types):
            print(f"\nType: {target_type}")
            print("-" * 40)

            for name, info in self.targets_info.items():
                if info["type"] != target_type: continue

                if info["is_library"]:
                    print(f"Target: {name}")

                    # Print artifacts (output files)
                    print("  Output Files:")
                    for artifact in info["artifacts"]:
                        print(f"    - {artifact['output_name']} (Path: {artifact['path']})")

                    # Print include directories
                    print("  Include Directories:")
                    for include_dir in info["include_directories"]:
                        print(f"    - {include_dir}")

                    # Print link libraries
                    print("  Link Libraries:")
                    for lib in info["link_libraries"]:
                        print(f"    - {lib}")

                    print("-" * 80)
                else:
                    artifacts_str = ", ".join([a["output_name"] for a in info["artifacts"]])
                    print(f"  {name}: {artifacts_str}")



    def lib(self, target_name : str) -> Target:
        for name, info in self.targets_info.items():
            if name == target_name and info["type"] == "STATIC_LIBRARY":
                #print("found lib in CMake submodule: {}, artifacts: {}".format(name, info["artifacts"]))
                target = Target(target_name, "lib", self.src_dir, self.target_os)
                target.out = npath_join("$builddir", self.name, info["artifacts"][0]["path"])
                self.targets.append(target)
                return target

        return None

class Ninja:
    def __init__(self, name : str, root : str, args : str, target_os : str):
        self.verbose = False

        self.root      = root
        self.build_dir = npath_join(root, name).replace("\\", "/")

        self.host_os   = sys.platform
        self.target_os = target_os
        self.compiler  = "clang"

        self.variables    : dict[str, str] = dict()
        self._flags       : dict[str, list[str]] = dict()
        self.targets      : list[Target] = []
        self.cmakes       : list[CMake] = []
        self.test_targets : list[Target] = []
        self.rules        : dict[str, Rule] = dict()
        self.default      : Target = None

        if not os.path.exists(self.build_dir): os.makedirs(self.build_dir)

        self.obj_dir = npath_join(self.build_dir, "obj")
        if not os.path.exists(self.obj_dir): os.makedirs(self.obj_dir)

        self.variables["root"]     = self.root.replace("\\", "/")
        self.variables["builddir"] = self.build_dir.replace("\\", "/")
        self.variables["objdir"]   = self.obj_dir.replace("\\", "/")
        self.variables["gendir"]   = npath_join(self.build_dir, "generated")
        self.variables["configure_args"] = " ".join(args)

        self._flags["c"] = []



    def rule(self, name : str, command : str, **kwargs):
        if name not in self._flags: self._flags[name] = []
        self.rules[name] = Rule(command, **kwargs)

    def flags(self, rule : str, opts : list[str]):
        if not opts: return self._flags
        if type(opts) is not list: return self.flags(rule, [opts])
        if not rule in self._flags: self._flags[rule] = []
        self._flags[rule].extend(opts)
        return self._flags

    def executable(self, name : str, src_dir : str = "") -> Target:
        t = Target(name, "exe", src_dir, self.target_os)
        for rule, info in self.rules.items(): t._flags[rule] = []

        self.targets.append(t)
        return t

    def library(self, name : str, src_dir : str = "", flags: dict[str,list[str]] = None) -> Target:
        ext = ".a"
        if self.target_os == "win32": ext = ".lib"

        t = Target(name, "lib", src_dir, self.target_os)
        self.targets.append(t)

        if flags:
            for k, v in flags.items(): t._flags[k] = v
        return t

    def cmake(self, name : str, src_dir : str = "", opts : list[str] = None) -> CMake:
        t = CMake(name, src_dir, self.target_os, opts)
        t.generate(self)
        self.cmakes.append(t)
        return t

    def test(self, parent : Target, src_dir : str, include_header : str = None) -> Target:
        t = Target("test.%s" % parent.name, "exe", src_dir, self.target_os)
        for rule, info in self.rules.items(): t._flags[rule] = []
        dep(t, parent)

        if include_header:
            t._flags["c"].append("-include " + include_header)

        self.test_targets.append(t)
        return t

    def generate(self):
        root_dir = os.path.realpath(self.root)
        writer = ninja.Writer(open(os.path.join(self.build_dir, "build.ninja"), "w"))

        # root variables
        for k, v in self.variables.items(): writer.variable(k, v)

        # re-generate ninja rule and target
        writer.newline()
        enki_dir = os.path.dirname(os.path.realpath(__file__))
        enki_dir = enki_dir.replace(root_dir, "$root")

        writer.rule("configure",
             command="%s $root/configure.py $configure_args" % sys.executable,
             description = "regenerate ninja",
             generator=True)

        writer.build("build.ninja", "configure",
                 implicit = [
                     "$root/configure.py",
                     os.path.join(enki_dir, "ninja_syntax.py"),
                     os.path.join(enki_dir, "enki.py"),
                 ])

        # root flags
        if self._flags: writer.newline()
        for k, v in self._flags.items(): vars(writer, k+"flags", v)

        # toolchain
        writer.newline()
        toolchain = "toolchain.{}.{}.ninja".format(self.compiler, self.host_os);
        writer.include(npath_join(enki_dir, toolchain))

        # user rules
        if self.rules: writer.newline()
        for rule, info in self.rules.items():
            writer.rule(rule, info.command, info.description, info.depfile, info.generator, info.pool, info.restat, info.rspfile, info.rspfile_content, info.deps)

        # generic run utility
        writer.newline()
        writer.rule("run", "$in $flags",
                  description = "RUN $in",
                  pool = "console")

        # external project rules
        writer.rule("ninja", "ninja -C $dir $target",
                  pool = "console",
                  restat = True)
        writer.rule("cmake", "cmake -GNinja $opts -S $in -B $dst",
                  pool = "console",
                  restat = True)

        # file util rules
        if self.host_os == "linux":
            writer.rule("copy", "cp $in $out", description = "COPY $out")
            writer.rule("symlink", "ln -s $in $out", description = "SYMLINK $out -> $in")

        # generate-header utility
        if self.host_os == "win32":
            writer.rule("meta", "$builddir/meta.exe $flags $in -o $out -- $cflags",
                       description = "META $in",
                       restat = True)
        elif self.host_os == "linux":
            writer.rule("meta", "$builddir/meta $flags $in -o $out -- $cflags",
                       description = "META $in",
                       restat = True)

        # built-in meta target
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

        # cmake submodules
        writer.newline()
        for cmake in self.cmakes:
            deps = npath_join("$builddir", cmake.name, "build.ninja")
            for t in cmake.targets:
                writer.build(t.out, "ninja", implicit = deps)
                writer.variable("target", t.name, 1)
                writer.variable("dir", npath_join("$builddir", cmake.name), 1)

        # compile commands database
        writer.newline()
        writer.build(npath_join("$builddir", "compile_commands.json"), "ninja")
        writer.variable("dir", "$builddir", 1)
        writer.variable("target", "-t compdb > compile_commands.json", 1)

        # targets
        writer.newline()
        gen_targets : list[str] = []
        for t in self.targets:
            filename = t.name + ".ninja"
            path = os.path.join(self.build_dir, filename)

            t.generate(ninja.Writer(open(path, "w")), self)
            writer.subninja(npath_join("$builddir", filename))

            if t.generated: gen_targets.append("gen.%s" % t.name)

        # tests
        writer.newline()
        for t in self.test_targets:
            filename = t.name + ".ninja"
            path = os.path.join(self.build_dir, filename)
            writer.subninja(npath_join("$builddir", filename))

            w = ninja.Writer(open(path, "w"))
            t.generate(w, self)

            w.newline()
            w.build("$builddir/%s.stamp" % t.name, "run", "$builddir/{}{}".format(t.name, t.ext))

        if self.targets: writer.newline()
        for t in self.targets: writer.build(t.name, "phony", t.out)

        test_targets : list[str] = []
        for t in self.test_targets:
            writer.build(t.name, "phony", npath_join("$builddir", "%s.stamp" % t.name))
            test_targets.append(t.name)

        if gen_targets or test_targets: writer.newline()
        if gen_targets: writer.build("gen.all", "phony", gen_targets)
        if test_targets: writer.build("test.all", "phony", test_targets)


        if self.default:
            writer.newline()
            writer.default(self.default.name)
        print("wrote %s." % os.path.basename(writer.output.name))


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

        if "c" not in t._flags: t._flags["c"] = []
        t._flags["c"].append(flag)

        if public:
            if "c" not in t.public_flags: t.public_flags["c"] = []
            t.public_flags["c"].append(flag)

    return paths

def lib_path(t : Target, paths : list[str], public = False) -> [str]:
    if type(paths) is not list: return lib_path(t, [paths], public)

    for path in paths:
        t.lib_paths.append(path)
        flag = f_lib_path(path)

        if "link" not in t._flags: t._flags["link"] = []
        t._flags["link"].append(flag)

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

        if "c" not in t._flags: t._flags["c"] = []
        t._flags["c"].append(d)

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
            if k not in t._flags: t._flags[k] = []
            t._flags[k].extend(v)

        for lib in d.libs:
            if lib not in t.libs: t.libs.append(lib)


def meta(t : Target, sources : list[str], flags : list[str] = None):
    if type(sources) is not list: return meta(t, [sources], flags)

    for source in sources:
        source_name = os.path.splitext(source)[0]
        out = npath_join("$gendir", normpath(source_name+".h"))
        objfile = npath_join("$objdir", normpath(source_name+".o"))
        depfile = objfile+".d"

        o = Object("meta", source, out)
        o.deps.append("$builddir/meta")
        #o.variables["depfile"] = depfile

        if flags: o.flags.extend(flags)

        t.generated.append(o)
