# -*- mode:python; coding:utf-8; -*-

#  Calculation of Pi using quadrature.
#
#  Copyright © 2008–2013 Russel Winder

import os
import platform
import re
import subprocess
import sys

osName, _, _, _, platformVersion, _ = platform.uname()
platformVersion = re.sub('i.86', 'ix86', platformVersion)
extraLibName = os.environ['HOME'] + '/lib.' + osName + '.' + platformVersion

from executablesupport import compileTargets, addCompileTarget, runTargets, addRunTarget, executables, createHelp

##  NB We must avoid amending the globally shared environment with language or specific program dependent
##  changes to keys used by other languages or other programs.  By specifying key/value pairs explicitly in
##  the Program calls, a new temporary clone environment is created a so everything proceeds as expected in
##  the C and C++ case below.  A problem arises with the D tool though.  See below.

#  C  ################################################################################

ccFlags = ['-O3', '-Wall', '-Wextra']

cEnvironment = Environment(tools=['gcc', 'gnulink'])

microsecondTimeC = cEnvironment.Object('Timing/microsecondTime.c', CFLAGS=ccFlags)

def cRule(globPattern, compiler='gcc', cpppath=[], cflags=ccFlags, linkflags=[], libpath=[], libs=[]):
    for item in Glob(globPattern):
        executables.append(
            addCompileTarget(
                cEnvironment.Program(
                    os.path.splitext(item.name)[0], [item.name, microsecondTimeC],
                    CC=compiler, CPPPATH=['Timing'] + cpppath, CFLAGS=cflags + ['-std=c99'], LINKFLAGS=linkflags + ['-std=c99'], LIBPATH=libpath, LIBS=libs)))

cRule('pi_c_sequential*.c')
cRule('pi_c_pthread*.c', libs=['pthread'])
cRule('pi_c_mpi*.c', compiler='mpicc')  #  This execution target runs things sequentially.  Use the command "mpirun -np N pi_c_mpi" to run the code on N processors.
cRule('pi_c_openmp*.c', cflags=ccFlags + ['-fopenmp'], libs=['gomp'])  #  Assumes gcc is 4.2.0 or greater since that is when gomp was included.

#  C++  ##############################################################################

cppEnvironment = Environment(tools=['g++', 'gnulink'])

def cppRule(globPattern, compiler='g++', cpppath=[], cxxflags=ccFlags, linkflags=[], libpath=[], libs=[]):
    for item in Glob(globPattern):
        executables.append(
            addCompileTarget(
                cppEnvironment.Program(
                    os.path.splitext(item.name)[0], [item.name, microsecondTimeC],
                    CXX=compiler, CPPPATH=['Timing'] + cpppath, CXXFLAGS=cxxflags + ['-std=c++0x'], LINKFLAGS=linkflags + ['-std=c++0x'], LIBPATH=libpath, LIBS=libs)))

cppRule('pi_cpp_sequential*.cpp')
cppRule('pi_cpp_pthread*.cpp', libs=['pthread'])
cppRule('pi_cpp_mpi*.cpp', compiler='mpic++')  #  This MPI execution target runs things sequentially.  Use the command "mpirun -np N pi_c_mpi" to run the code on N processors.
cppRule('pi_cpp_openmp*.cpp', cxxflags=ccFlags + ['-fopenmp'], libs=['gomp'])  #  Assumes gcc is 4.2.0 or greater since that is when gomp was included.

cppRule('pi_cpp_cppcsp2.cpp', cpppath=[os.environ['HOME'] + '/include'], libpath=[extraLibName], libs=['cppcsp2', 'pthread'])

#  As from 2010-03-04 15:56+00:00, the Boost.MPI library is not in Lucid.  As is documented in the bug tracker
#  on Launchpad (https://bugs.launchpad.net/ubuntu/+source/boost-defaults/+bug/531973 and
#  https://bugs.launchpad.net/ubuntu/+source/boost1.42/+bug/582420), Boost.MPI has been ejected from
#  Ubuntu!!!!!!!!  It's still in Debian though :-))
#
#  If BOOST_HOME is set then use that otherwise use whatever is installed, if it is!
#
#  NB The MPI execution targets runs things sequentially.  Use the command "mpirun -np N pi_cpp_boostMPI" to
#  run the code on N processors.
try:
    boostHome = os.environ['BOOST_HOME']
    boostInclude = boostHome + '/include'
    boostLib = boostHome + '/lib'
    cppRule('pi_cpp_boostThread*.cpp', cpppath=[boostInclude], libpath=[boostLib], libs=['boost_thread'])
    cppRule('pi_cpp_boostMPI*.cpp', compiler='mpic++', cpppath=[boostInclude], libpath=[boostLib], libs=['boost_mpi', 'boost_serialization'])
except KeyError:
    if not os.path.isfile('/usr/lib/libboost_mpi.so'):
        print '\nWarning:  Cannot find a Boost.MPI.\n'
    else:
        cppRule('pi_cpp_boostThread*.cpp', libs=['boost_thread'])
        cppRule('pi_cpp_boostMPI*.cpp', compiler='mpic++', libs=['boost_mpi'])

#  Use Anthony Williams' Just::Thread library as an implementation of C++0x threads and things.  Anthony's
#  Ubuntu debs seems to work fine on Debian, which is good -- albeit lucky.  In order to use the standard
#  naming in the source code we have to augment the include path.  Must also inform GCC that we are using
#  the next standard.
#cppRule('pi_cpp_justThread*.cpp', cpppath=['/usr/include/justthread'], libs=['justthread', 'rt'])
# Use the pre-release Just::Thread Pro as it has the actor and dataflow support.
cppRule('pi_cpp_justThread*.cpp', cpppath=[extraLibName + '/JustThreadPro/include'], linkflags=['--static'], libpath=[extraLibName + '/JustThreadPro/libs'], libs=['justthread', 'pthread', 'rt'])

#  TBB 2.2 is packaged in Ubuntu Lucid and Debian Squeeze.  Debian Wheezy appears to package TBB 4 though it
#  still has the SO number 2.  Deal with the situation of TBB_HOME being defined for a custom variant of
#  TBB.  TBB only provides dynamic libraries, there are no static libraries, so we have to get into the hassle of
#  specifying a LD_LIBRARY_PATH to run the constructed executable if the TBB libraries are not in the
#  standard path :-( "LD_LIBRARY_PATH=$TBB_HOME pi_cpp_tbb . . . "
try:
    tbbHome = os.environ['TBB_HOME']
    cppRule('pi_cpp_tbb*.cpp', cpppath=[tbbHome + '/include'], libpath=[tbbHome], libs=['tbb'])
except KeyError:
    cppRule('pi_cpp_tbb*.cpp', libs=['tbb'])

#  Fortran  ##########################################################################

fortranFlags = ['-O3', '-std=f2008', '-ffree-form', '-pedantic', '-Wall']

fortranEnvironment = Environment(tools=['gfortran', 'gnulink'], FORTRANFLAGS=fortranFlags)

fortranOutput = fortranEnvironment.Object('output_f.f')

def fortranRule(globPattern, compiler='gfortran', fortranflags=fortranFlags, linkflags=[], libpath=[], libs=[]):
    for item in Glob(globPattern):
        executables.append(
            addCompileTarget(
                fortranEnvironment.Program(
                    os.path.splitext(item.name)[0], [item.name, fortranOutput],
                    FORTRAN=compiler, FORTRANFLAGS=fortranflags, LINKFLAGS=linkflags, LIBPATH=libpath, LIBS=libs)))

fortranRule('pi_fortran_sequential*.f')
fortranRule('pi_fortran_openmp*.f', fortranflags=fortranFlags + ['-fopenmp'], libs=['gomp'])
fortranRule('pi_fortran_mpi*.f', compiler='mpif90')

#  D  ################################################################################

##  DMD 2.059 worked, DMD 2.060 has issues – various problems with the thread.  GDC on Debian as at
## 2012-09-06 realizes D 2.056 and so barfs on some constructs introduced after that version.  LDC compiled
## from master/HEAD as at 2012-11--11 is D 2.060+changes and works fine. Except for
## pi_d_threadsGlobalState_array_declarative.d. See http://d.puremagic.com/issues/show_bug.cgi?id=8774.  It
## appears that pi_d_threadsGlobalState_array_declarative.d should never have worked under 2.059 as it was:
## maps are iterable but lazy, the array needs to be instantiated for the algorithm to work as required.
##
##  pi_d_threadsGlobalState_array_iterative.d and pi_d_threadsGlobalState_threadGroup.d fail on DMD 2.060
##  but worked under 2.059, and work under LDC2.


dEnvironment = {
    'dmd': Environment(tools=['link', 'dmd'], # Why is the order crucial here?
                       DFLAGS=['-O', '-release'],
                       #DC='gdmd'
                       ),
    'gdc':  Environment(tools=['link', 'gdc'], # Why is the order crucial here?
                        DFLAGS=['-O3'],
                        ),
    'ldc': Environment(tools=['link', 'ldc'],
                       ENV = os.environ,
                       DFLAGS=['-O', '-release'],
                       ),
#}['dmd']
#}['gdc']
}['ldc']

dOutput = dEnvironment.Object('output_d.d')

for item in Glob('pi_d_*.d'):
    root = os.path.splitext(item.name)[0]
    executables.append(addCompileTarget(dEnvironment.Program([item, dOutput])))

#  Chapel  ###########################################################################

chapelEnvironment = Environment(tools=['chapel'], ENV=os.environ, CHPLFLAGS=['-O', '--fast'])

for item in Glob('pi_chapel_*.chpl'):
    executables.append(addCompileTarget(chapelEnvironment.ChapelProgram(item.name.replace('.chpl', ''), [item, 'output.chpl'])))

#  Haskell  ##########################################################################

#  Haskell jobs run in a single thread by default (which is what happens here).  Run the executable with
#  "+RTS -Nx" to run with x OS threads.

haskellEnvironment = Environment(tools=['haskell'])

for item in Glob('pi_haskell_*.hs'):
    root = os.path.splitext(item.name)[0]
    options = ['-rtsopts']
    variant = root.split('_')[2]
    if variant == 'threads':
        options.append('-threaded')
    elif variant == 'parMap' or variant == 'parallel':
        options.append('-threaded')
        options.append('-fdph-par')
    if True:
        executables.append(addCompileTarget(haskellEnvironment.Command(root, item.name, 'ghc --make -O -o $TARGET' + ''.join([' ' + x for x in options]) + ' $SOURCE')))
        SideEffect([root + ext for ext in ['.hi', '.o', '.hp', '.ps', '.aux']],  root)
    else:
        executables.append(addCompileTarget(haskellEnvironment.HaskellProgram(item, HSLINKFLAGS=['--make'])))

#  OCaml  ############################################################################

ocamlEnvironment = Environment(tools=[])  #, ENV=os.environ)

for item in Glob('pi_ocaml_*.ml'):
    root = os.path.splitext(item.name)[0]
    extraOptions = ''
    variant = root.split('_')[2]
    if variant == 'threads':
        extraOptions = '-thread unix.cmxa threads.cmxa'
    if variant == 'mpi':
        extraOptions = '-I ' + os.environ['OCAMLMPI_HOME'] + ' mpi.cmxa unix.cmxa'  #  These programs get run as sequential ones from SCons.  Use the command "mpirun -np N . . . " to run the code on N processors.
    executables.append(addCompileTarget(ocamlEnvironment.Command(root, item.name, 'ocamlopt -o $TARGET {} $SOURCE'.format(extraOptions))))
    SideEffect([root + '.' + extension for extension in ['cmi', 'cmx', 'o']], root)

#  occam  ############################################################################

#  As far as is known there is no occam implementation packaged in Debian Squeeze or Ubuntu Lucid.  We
#  therefore use KRoC which will be installed somewhere known to the shell.  It doesn't seem obvious how to
#  get a successful static link, so need to ensure LD_LIBRARY_PATH is correctly set to run the resulting
#  executable.

occamEnvironment = Environment(tools=[], ENV=os.environ)

for item in Glob('pi_occam*.occ'):
    root = os.path.splitext(item.name)[0]
    executables.append(addCompileTarget(occamEnvironment.Command(root, item.name, 'kroc -o $TARGET $SOURCE')))

#  Clay  ############################################################################

clayEnvironment = Environment(tools=[], ENV=os.environ)

for item in Glob('pi_clay_*.clay'):
    root = os.path.splitext(item.name)[0]
    executables.append(addCompileTarget(clayEnvironment.Command(root, item.name, 'clay -o $TARGET $SOURCE')))

## ###################################################################################
##  All the native compiled executables are processed the same way.
## ###################################################################################

for item in executables:
    addRunTarget(Command('run_' + item[0].name, item, './' + item[0].name))

#  C#  ###############################################################################

cSharpEnvironment = Environment(tools=['csharp'])

for item in Glob('Pi_CS_*.cs'):
    compiledFile = cSharpEnvironment.CLIProgram(item)
    compiledFileName = compiledFile[0].name
    compileTargets.append(compiledFileName)
    addRunTarget(cSharpEnvironment.Command('run_' + compiledFileName.replace('.exe', ''), compiledFile, 'mono ' + compiledFileName))

#  Java  #############################################################################

#  Include the user environment in the Java environment so that when Java code is executed it does so in the
#  users environment. This is needed to get all the Unicode characters to print anything other than ?. To
#  ensure we do not get the warnings/errors about "unmappable character for encoding ASCII", forcibly set
#  the compilation encoding to the UTF-8 that we know the file is encoded as.

javaEnvironment = Environment(tools=['javac'], JAVACFLAGS=['-source', '7', '-encoding', 'utf-8'], ENV=os.environ)

classpathEntries = {
    'JCSP': (os.environ['HOME'] + '/lib/Java/jcsp.jar',),
    'FunctionalJava': (os.environ['HOME'] + '/lib/Java/functionaljava.jar',),
    'GPars': (os.environ['HOME'] + '/lib/Java/gpars.jar', os.environ['HOME'] + '/lib/Java/groovy-all.jar'),
    'ParallelArray': (os.environ['HOME'] + '/lib/Java/jsr166y.jar', os.environ['HOME'] + '/lib/Java/extra166y.jar'),
    }

javaOutputClass = javaEnvironment.Java(target='.', source='JOutput.java')

for item in Glob('Pi_Java_*.java'):
    className = os.path.splitext(item.name)[0]
    compileTargets.append(className + '.class')
    variant = className.split('_')[2]
    javaCommand = 'java'
    if variant == 'DataRush5':
        dataRushHome = os.environ['HOME'] + '/lib/Java/' + variant
        compiledClass = javaEnvironment.Java(target='.', source=item, JAVACLASSPATH=[f.path for f in Glob(dataRushHome + '/lib/*.jar')])
        javaCommand = 'JAVA_HOME=' + os.environ['JAVA_HOME'] + ' ' + dataRushHome + '/bin/dr -cp . '
    elif variant in classpathEntries.keys():
        compiledClass = javaEnvironment.Java(target='.', source=item, JAVACLASSPATH=classpathEntries[variant])
        javaCommand += ' -cp .:' + ':'.join(classpathEntries[variant])
    else:
        compiledClass = javaEnvironment.Java(target='.', source=item)
    Depends(compiledClass, javaOutputClass)
    addRunTarget(javaEnvironment.Command('run_' + className, compiledClass, javaCommand + ' ' + className))

##  NB The Java builder does not track all the classes generated by the compilation so not all class files
##  get automatically removed during clean.  The same applies to the Scala compilation.

Clean('.', Glob('*.class'))

##  Some of the Closure and Groovy versions use a Java class.  This class has to be compiled, so do it with
##  the Java environment.  The source code is UTF-8 encoded no matter what the environment of build.

processSliceClasses = javaEnvironment.Java('.', ['ProcessSlice.java'])
processSliceJCSPClasses = javaEnvironment.Java('.', ['ProcessSlice_JCSP.java'], JAVACLASSPATH=classpathEntries['JCSP'])

Alias('compileJavaProcessSlicesForGroovy', [processSliceClasses, processSliceJCSPClasses])

#  Ceylon  ###########################################################################

ceylonEnvironment = Environment(
    ENV = os.environ,
    JAVA_HOME=os.path.join(extraLibName, 'JDK7')
    )

ceylonEnvironment.Command('run_pi_ceylon_sequential', 'pi_ceylon_sequential.ceylon', ['ceylon compile --src . $SOURCE',
                          'ceylon run --run main default'])
Clean('.', 'modules')

#  X10  ##############################################################################

####  With X10 2.0.[23] things compile using the Java backend but the sequential code seems to cause all
####  cores to go to 90% for what appears to be forever.  Making use instead of the C++ backend which has
####  had much more work done on it allows the code to run. The parallel code doesn't work as yet due to
####  some class cast problem.

####  With X10 2.1.0 everything seems to be working as it should be once the various Rail -> Array changes
####  and use of DistArray has been sorted.

####  For some forgotten reason 2.1.1 failed to work as required and was ignored.

####  2.1.2 seems to work sort of but the old runx10 command that was used to launch the C++ target
####  versions has been removed, the generated executables are native executables that are MPI aware.  So
####  they run on 1 core unless the mpirun command is used.  One downside though the Java backend version
####  appears to show no scaling at all.  And the MPI execution is bizarre, and shows no scaling at all.

x10Environment = Environment(
    tools=['x10'],
    ENV=os.environ,
)

for item in Glob('Pi_X10_*.x10'):
    x10ClassName = os.path.splitext(item.name)[0]
    #
    #  Java backend bits.
    #
    x10ClassFile = x10Environment.X10Classes(item)
    SideEffect(item.name.replace('.x10', '$$Main.class'), x10ClassFile)
    compileTargets.append(x10ClassFile[0].name)
    addRunTarget(x10Environment.Command('run_' + x10ClassName + '_Java', x10ClassFile, 'x10 ' + x10ClassName))
    #
    # C++ backend bits.
    #
    x10Executable = x10Environment.X10Program(item, X10FLAGS=['-O'])
    SideEffect('xxx_main_xxx.cc', x10Executable)
    compileTargets.append(x10Executable[0].name)
    addRunTarget(x10Environment.Command('run_' + x10ClassName + '_Cpp', x10Executable, x10ClassName))

#  Groovy  ###########################################################################

#  Most of the dependencies are handled with @Grab annotations in the Groovy source code.  However for the
#  mixed Groovy/Java there needs to be more attention to the classpath for both comiplation and execution.

groovyEnvironment = Environment(tools=['javac'], ENV=os.environ)

dependsOnProcessSlice = []
dependsOnProcessSlice_JCSP = []
for item in Glob('pi_groovy*.groovy'):
    root = os.path.splitext(item.name)[0]
    runTarget = 'run_' + root
    bits = root.split('_')
    runCommand = './$SOURCE'
    if bits[1] == 'groovyjava':
        if bits[3] == 'CSP':
            dependsOnProcessSlice_JCSP.append(runTarget)
            runCommand = 'groovy -cp .:' + groovyEnvironment['ENV']['HOME'] + '/lib/Java/jcsp.jar $SOURCE'
        else:
            dependsOnProcessSlice.append(runTarget)
    elif bits[1] == 'GroovyJava':
        dependsOnProcessSlice.append(runTarget)
    addRunTarget(groovyEnvironment.Command('run_' + root, item.name, runCommand))

Depends(dependsOnProcessSlice_JCSP, processSliceJCSPClasses)
Depends(dependsOnProcessSlice, processSliceClasses)

#  Fortress  #########################################################################

fortressEnvironment = Environment(tools=['latex'], ENV=os.environ)

for item in Glob('*.fss'):
    fortressCodeRoot = os.path.splitext(item.name)[0]
    addRunTarget(fortressEnvironment.Command('run_' + fortressCodeRoot, item.name, 'fortress $SOURCE'))
    #  For the moment LaTeX doesn't work correctly on Mac OS X:-(((
    if osName != 'Darwin':
        pdfDocument = fortressEnvironment.PDF(fortressCodeRoot + '_document.ltx')
        Depends(pdfDocument, fortressEnvironment.Command(fortressCodeRoot + '.tex', item.name, 'fortify $SOURCE'))
        addCompileTarget(Alias('typesetFortress', pdfDocument))

####################################################################################
####################################################################################

addCompileTarget(Alias('compile', compileTargets))

addRunTarget(Alias('run', runTargets))

Default('compile')

Help(createHelp())
