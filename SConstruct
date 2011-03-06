# -*- mode:python; coding:utf-8; -*-

#  Calculation of Pi using quadrature.
#
#  Copyright © 2008-11 Russel Winder 

import os
import platform
import re
import sys

osName , _ , _ , _ , platformVersion , _ = platform.uname ( )
platformVersion = re.sub ( 'i.86' , 'ix86' , platformVersion )
extraLibName = os.environ['HOME'] + '/lib.' + osName + '.' + platformVersion

compileTargets = [ ]
def addCompileTarget ( target ) :
    global compileTargets
    compileTargets.append ( target[0].name )
    return target

runTargets = [ ]
def addRunTarget ( target ) :
    global runTargets
    runTargets.append ( target[0].name )
    return target

##  When compilation produces an executable we can generate the run target in an identical way.  Executables
##  accumlates a list of the executables that will be run targets.

executables = [ ]

##  NB We must avoid amending the globally shared environment with language or specific program dependent
##  changes to keys used by other languages or other programs.  By specifying key/value pairs explicitly in
##  the Program calls, a new temporary clone environment is created a so everything proceeds as expected in
##  the C and C++ case below.  A problem arises with the D tool though.  See below.

#  C  ################################################################################

ccFlags = [ '-O3' , '-Wall' , '-Wextra' ]

cEnvironment = Environment ( tools = [ 'gcc' , 'gnulink' ] )

microsecondTimeC = cEnvironment.Object ( '../Timing/microsecondTime.c' , CFLAGS = ccFlags )

def cRule ( globPattern , compiler = 'gcc' , cpppath = [ ] , cflags = ccFlags , linkflags = [ ] , libpath = [ ] , libs = [ ] ) :
    for item in Glob ( globPattern ) :
        executables.append (
            addCompileTarget (
                cEnvironment.Program (
                    os.path.splitext ( item.name ) [0] , [ item.name , microsecondTimeC ] ,
                    CC = compiler , CPPPATH = [ '../Timing' ] + cpppath , CFLAGS = cflags + [ '-std=c99' ] , LINKFLAGS= linkflags , LIBPATH = libpath , LIBS = libs )
                )
            )

cRule ( 'pi_c_sequential*.c' )
cRule ( 'pi_c_pthread*.c' , libs = [ 'pthread' ] )
cRule ( 'pi_c_mpi*.c' , compiler = 'mpicc' )  #  This execution target runs things sequentially.  Use the command "mpirun -np N pi_c_mpi" to run the code on N processors.
cRule ( 'pi_c_openmp*.c' , cflags = ccFlags + [ '-fopenmp' ] , libs = [ 'gomp' ] )  #  Assumes gcc is 4.2.0 or greater since that is when gomp was included.

#  C++  ##############################################################################

cppEnvironment = Environment ( tools = [ 'g++' , 'gnulink' ] )

def cppRule ( globPattern , compiler = 'g++' , cpppath = [ ] , cxxflags = ccFlags , linkflags = [ ] , libpath = [ ] , libs = [ ] ) :
    for item in Glob ( globPattern ) :
        executables.append (
            addCompileTarget (
                cppEnvironment.Program (
                    os.path.splitext ( item.name ) [0] , [ item.name , microsecondTimeC ] ,
                    CXX = compiler , CPPPATH = [ '../Timing' ] + cpppath , CXXFLAGS = cxxflags , LINKFLAGS= linkflags , LIBPATH = libpath , LIBS = libs )
                )
            )

cppRule ( 'pi_cpp_sequential*.cpp' )
cppRule ( 'pi_cpp_pthread*.cpp' , libs = [ 'pthread' ] )
cppRule ( 'pi_cpp_mpi*.cpp' , compiler = 'mpic++' )  #  This MPI execution target runs things sequentially.  Use the command "mpirun -np N pi_c_mpi" to run the code on N processors.
cppRule ( 'pi_cpp_openmp*.cpp' , cxxflags = ccFlags + [ '-fopenmp' ] , libs = [ 'gomp' ] ) #  Assumes gcc is 4.2.0 or greater since that is when gomp was included.

cppRule ( 'pi_cpp_cppcsp2.cpp' , cpppath = [ os.environ['HOME'] +'/include' ] , libpath = [ extraLibName ] , libs = [ 'cppcsp2' , 'pthread' ] )

#  As from 2010-03-04 15:56+00:00, the Boost MPI library is not in Lucid.  As is document in the bug tracker
#  on Launchpad (https://bugs.launchpad.net/ubuntu/+source/boost-defaults/+bug/531973 and
#  https://bugs.launchpad.net/ubuntu/+source/boost1.42/+bug/582420), Boost.MPI has been ejected from
#  Ubuntu!!!!!!!!
#
#  NB The MPI execution targets runs things sequentially.  Use the command "mpirun -np N pi_cpp_boostMPI" to
#  run the code on N processors.
if os.environ['BOOST_HOME'] :
    boostHome = os.environ['BOOST_HOME']
    boostInclude = boostHome + '/include'
    boostLib = boostHome + '/lib'
    cppRule ( 'pi_cpp_boostThread*.cpp' , cpppath = [ boostInclude ] , libpath = [ boostLib ] , libs = [ 'boost_thread' ] ) 
    cppRule ( 'pi_cpp_boostMPI*.cpp' , compiler = 'mpic++' , cpppath = [ boostInclude ] , libpath = [ boostLib ] , libs = [ 'boost_mpi' , 'boost_serialization' ] )
elif not os.path.isfile ( '/usr/lib/libboost_mpi.so' ) :
    print '\nWarning:  Cannot find a Boost.MPI.\n'
else :
    cppRule ( 'pi_cpp_boostThread*.cpp' , libs = [ 'boost_thread' ] ) 
    cppRule ( 'pi_cpp_boostMPI*.cpp' , compiler = 'mpic++' , libs = [ 'boost_mpi' ] )

#  Use Anthony Williams' Just::Thread library as an implementation of C++0x threads and things.  Anthony's
#  Ubuntu Lucid deb seems to work fine on Debian Squeeze, which is good.  In order to use the standard
#  naming in the source code we have to augment the include path.  Must also inform GCC that we are using
#  the next standard.
cppRule ( 'pi_cpp_justThread*.cpp' , cpppath = [ '/usr/include/justthread' ] , cxxflags = ccFlags + [ '-std=c++0x' ] , linkflags = [ '-std=c++0x' ] , libs = [ 'justthread' , 'rt' ] )

#  TBB 2.2 is packaged in Ubuntu Lucid and Debian Squeeze, but TBB 3 is now out and compiled up in some
#  location known to the shell.
if not os.path.isfile ( '/usr/lib/libtbb.so.3' ) :
    #  Intel's Threading Building Blocks (TBB) only provides dynamic libraries, there are no static
    #  libraries, so we have to get into the hassle of specifying a LD_LIBRARY_PATH to run the constructed
    #  executable if the TBB libraries are not in the standard path :-( "LD_LIBRARY_PATH=$TBB_HOME pi_cpp_tbb . . . "
    tbbHome = os.environ['TBB_HOME']
    cppRule (  'pi_cpp_tbb*.cpp' , cpppath = [ tbbHome + '/include' ] , libpath = [ tbbHome ] , libs = [ 'tbb' ] )
else :
    cppRule (  'pi_cpp_tbb*.cpp' , libs = [ 'tbb' ] )

#  Fortran  ##########################################################################

fortranFlags = [ '-O3' , '-std=f2003' , '-ffree-form' , '-pedantic' , '-Wall' ]

fortranEnvironment = Environment ( tools = [ 'gfortran' , 'gnulink' ] )

def fortranRule ( globPattern , compiler = 'gfortran' , fortranflags = fortranFlags , linkflags = [ ] , libpath = [ ] , libs = [ ] ) :
    for item in Glob ( globPattern ) :
        executables.append (
            addCompileTarget (
                fortranEnvironment.Program (
                    os.path.splitext ( item.name ) [0] , item.name ,
                    FORTRAN = compiler , FORTRANFLAGS = fortranflags , LINKFLAGS= linkflags , LIBPATH = libpath , LIBS = libs )
                )
            )

fortranRule ( 'pi_fortran_sequential*.f' )
fortranRule ( 'pi_fortran_openmp*.f' , fortranflags = fortranFlags + [ '-fopenmp' ] , libs = [ 'gomp' ] )
fortranRule ( 'pi_fortran_mpi*.f' , compiler = 'mpif90' )

#  D  ################################################################################

##  NB Until 2011-02-18 (1.066, 2.051) the D compiler was a 32-bit application that generated 32-bit code --
##  so on 64-bit platforms special care was needed.  As of 2011-02-18 (1.067, 2.052) the D compiler is able
##  to generate 64-bit code on a Linux machine.

######
##
##  NB The D tool amends the 'LIBS' key in the environment used.  To avoid this polluting link phases for
##  other languages, ensure to use a distinct clone for all the D compilation and linking.  As this is done
##  for all the languages anyway there is no problem :-)
##
######

#  The dmd tool fails to set up the environment correctly to do linking on Ubuntu and Debian unless there is
#  a compiler tool specified in order to determine the linker AND the dmd tool is included after the link
#  and compiler tools. Also the dmd compiler is not in the bootstrap path on RW's set up, so amend the path
#  to ensure it is found.

dEnvironment = Environment ( tools = [ 'gcc' , 'gnulink' , 'dmd_new' ] , ENV = os.environ , DFLAGS = [ '-O' , '-release' , '-inline' ] )

for item in Glob ( 'pi_d2_*.d' ) :
    root = os.path.splitext ( item.name ) [0]
    environment = dEnvironment
    includesParallelism = False
    with open ( item.name , 'r' ) as code :
        for line in code :
            if 'std.parallelism' in line :
                includesParallelism = True
                break
    if includesParallelism :
        environment = dEnvironment.Clone ( )
        environment.Append (
            DPATH =  [ os.environ['HOME'] + '/lib/D' ] ,
            LIBPATH = [ extraLibName ] ,
            LIBS = [ 'parallelism' ] )
    executables.append ( addCompileTarget ( environment.Program ( item.name ) ) )

#  Chapel  ###########################################################################

chapelEnvironment = Environment ( tools = [ 'chapel' ] , ENV = os.environ , CHPLFLAGS = [ '-O' , '--fast' ] )

for item in Glob ( 'pi_chapel_*.chpl' ) :
    executables.append ( addCompileTarget ( chapelEnvironment.ChapelProgram ( item ) ) )

#  Haskell  ##########################################################################

#  Haskell jobs run in a single thread by default (which is what happens here).  Run the executable with
#  "+RTS -Nx" to run with x OS threads.

haskellEnvironment = Environment ( tools = [ 'haskell' ] )

for item in Glob ( 'pi_haskell_*.hs' ) :
    root = os.path.splitext ( item.name ) [0]
    options = [ ]
    variant = root.split ( '_' )[2]
    if variant == 'threads' :
        options.append ( '-threaded' )
    elif variant == 'parMap' or variant == 'parallel' :
        options.append ( '-threaded' )
        options.append ( '-fdph-par' )
    if True :
        executables.append ( addCompileTarget ( haskellEnvironment.Command ( root , item.name , 'ghc --make -O -o $TARGET' + ''.join ( [ ' ' + x for x in options ] ) + ' $SOURCE' ) ) )
        SideEffect ( [  root + ext for ext in [ '.hi' , '.o' , '.hp' , '.ps' , '.aux' ] ] ,  root )
    else :
        executables.append ( addCompileTarget ( haskellEnvironment.HaskellProgram ( item , HSLINKFLAGS = [ '--make' ] ) ) )

#  OCaml  ############################################################################

ocamlEnvironment = Environment ( tools = [ ] ) # , ENV = os.environ )

for item in Glob ( 'pi_ocaml_*.ml' ) :
    root = os.path.splitext ( item.name ) [0]
    extraOptions = ''
    variant = root.split ( '_' )[2]
    if variant == 'threads' : extraOptions = '-thread unix.cmxa threads.cmxa'
    if variant == 'mpi' : extraOptions = '-I ' + os.environ['OCAMLMPI_HOME'] + ' mpi.cmxa unix.cmxa' #  These programs get run as sequential ones from SCons.  Use the command "mpirun -np N . . . " to run the code on N processors.
    executables.append ( addCompileTarget ( ocamlEnvironment.Command ( root , item.name , 'ocamlopt -o $TARGET %s $SOURCE' % ( extraOptions ) ) ) )
    SideEffect ( [ root + '.' + extension for extension in [ 'cmi' , 'cmx' , 'o' ] ] , root )

#  Go  ###############################################################################

goEnvironment = Environment ( tools = [ 'go' ] )
goEnvironment.GoTarget ( os.environ['GOOS'] , os.environ['GOARCH'] ) 

for item in Glob ( 'pi_go_*.go' ) :
    root = os.path.splitext ( item.name ) [0]
    executable = goEnvironment.GoProgram ( root , item )
    executables.append ( addCompileTarget ( executable ) )

#  occam  ############################################################################

#  As far as is known there is no occam implementation packaged in Debian Squeeze or Ubuntu Lucid.  We
#  therefore use KRoC which will be installed somewhere known to the shell.  It doesn't seem obvious how to
#  get a successful static link, so need to ensure LD_LIBRARY_PATH is correctly set to run the resulting
#  executable.

occamEnvironment = Environment ( tools = [ ] , ENV = os.environ )

for item in Glob ( 'pi_occam*.occ' ) :
    root = os.path.splitext ( item.name ) [0]
    executables.append ( addCompileTarget ( occamEnvironment.Command ( root , item.name , 'kroc -o $TARGET $SOURCE' ) ) )
    
#  Clay  ############################################################################

clayEnvironment = Environment ( tools = [ ] , ENV = os.environ )

for item in Glob ( 'pi_clay_*.clay' ) :
    root = os.path.splitext ( item.name ) [0]
    executables.append ( addCompileTarget ( clayEnvironment.Command ( root , item.name , 'clay -o $TARGET $SOURCE' ) ) )

## ###################################################################################
##  All the native compiled executables are processed the same way.
## ###################################################################################

for item in executables :
    addRunTarget ( Command ( 'run_' + item[0].name , item , './' + item[0].name ) )

#  C#  ###############################################################################

cSharpEnvironment = Environment ( tools = [ 'csharp' ] )

for item in Glob ( 'Pi_CS_*.cs' ) :
    compiledFile = cSharpEnvironment.CLIProgram ( item )
    compiledFileName = compiledFile[0].name
    compileTargets.append ( compiledFileName )
    addRunTarget ( cSharpEnvironment.Command ( 'run_' + compiledFileName.replace ( '.exe' , '' ) , compiledFile , 'mono ' + compiledFileName ) )

#  Java  #############################################################################

#  The following environment does not include the user shell environment and so assumes the file encoding is
#  ASCII.  We know the encoding is UTF-8 so say so.  This avoids all the warnings/errors about "unmappable
#  character for encoding ASCII".

javaEnvironment = Environment ( tools = [ 'javac' ] , JAVACFLAGS = [ '-source' , '6' , '-encoding' , 'utf-8' ] )

classpathEntries = {
    'JCSP' : ( os.environ['HOME'] + '/lib/Java/jcsp.jar' , ) ,
    'ForkJoinBasic' : ( os.environ['HOME'] + '/lib/Java/jsr166y.jar' , ) ,
    'ForkJoinCollection' : ( os.environ['HOME'] + '/lib/Java/jsr166y.jar' , ) ,
    'ParallelArray' : ( os.environ['HOME'] + '/lib/Java/jsr166y.jar' , os.environ['HOME'] + '/lib/Java/extra166y.jar' ) ,
    'FunctionalJava' : ( os.environ['HOME'] + '/lib/Java/functionaljava.jar' , ) ,
    'GPars' : ( os.environ['HOME'] + '/lib/Java/gpars.jar' , os.environ['HOME'] + '/lib/Java/groovy-all.jar' ) ,
    }

for item in Glob ( 'Pi_Java_*.java' ) :
    className = os.path.splitext ( item.name ) [0]
    compileTargets.append ( className + '.class' )
    variant = className.split ( '_' )[2]
    javaCommand = 'java'
    classpath = [ ]
    if variant in [ 'DataRush4' , 'DataRush5' ] :
        dataRushHome = os.environ['HOME'] + '/lib/Java/' + variant
        compiledBits = javaEnvironment.Java ( target = '.' , source = item , JAVACLASSPATH = [ f.path for f in Glob ( dataRushHome + '/lib/*.jar' ) ] )
        javaCommand = 'JAVA_HOME=' + os.environ['JAVA_HOME'] + ' ' + dataRushHome + '/bin/dr -cp . '
    elif variant in classpathEntries.keys ( ) :
        compiledBits = javaEnvironment.Java ( target = '.' , source = item , JAVACLASSPATH = classpathEntries[variant] )
        javaCommand += ' -cp .:' + ':'.join ( classpathEntries[variant] )
    else :
        compiledBits = javaEnvironment.Java ( target = '.' , source = item )
    addRunTarget ( javaEnvironment.Command ( 'run_' + className , compiledBits , javaCommand + ' ' + className ) )

##  NB The Java builder does not track all the classes generated by the compilation so not all class files
##  get automatically removed during clean.  The same applies to the Scala compilation.

Clean ( '.' , Glob ( '*.class' ) )

##  Some of the Closure and Groovy versions use a Java class.  This class has to be compiled, so do it with
##  the Java environment.  The source code is UTF-8 encoded no matter what the environment of build.

processSliceClasses =  javaEnvironment.Java ( '.' , [ 'ProcessSlice.java' ] )
processSliceJCSPClasses = javaEnvironment.Java ( '.' , [ 'ProcessSlice_JCSP.java' ] , JAVACLASSPATH = [ classpathEntries['JCSP'] ] )

Alias ( 'compileJavaProcessSlicesForGroovy' , [ processSliceClasses , processSliceJCSPClasses ] )

#  Scala  #############################################################################

scalaEnvironment = Environment ( tools = [ ] , ENV = os.environ )

for item in Glob ( 'Pi_Scala_*.scala' ) :
    if item.name == 'Pi_Scala_FunctionalJava_ParMap.scala' : continue #  Hack because there is a compilation error.
    if item.name == 'Pi_Scala_Scalaz_ParMap.scala' : continue #  Hack because there is a compilation error.
    className = os.path.splitext ( item.name ) [0]
    compiledFileName = className + '.class'
    variant = className.split ( '_' )[2]
    extraStuff = ''
    if variant == 'FunctionalJava' :
        extraStuff = '-cp .:%s/lib/Java/functionaljava.jar' % ( os.environ['HOME'] , )
    elif variant == 'Scalaz' :
        extraStuff = '-cp .:%s/lib/Java/scalaz-core.jar' % ( os.environ['HOME'] , )
    compileTargets.append ( compiledFileName )
    addRunTarget ( scalaEnvironment.Command ( 'run_' + className , scalaEnvironment.Command ( compiledFileName , item.name , ( ( 'scalac %s -optimise ' + item.name ) % ( extraStuff , ) ) ) , ( ( 'scala %s ' + className ) % ( extraStuff , ) ) ) )

##  As the clean rule for removing all the class files has already been set since the Java builder does not
##  track all the classes generated by the compilation so not all class files get automatically removed
##  during clean, it doesn't have to be done again for Scala.

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

x10Environment = Environment ( tools = [ 'x10' ] , ENV = os.environ )

for item in Glob ( 'Pi_X10_*.x10' ) :
    x10ClassName = os.path.splitext ( item.name ) [0]
    #
    #  Java backend bits.
    #
    x10ClassFile = x10Environment.X10Classes ( item )
    compileTargets.append ( x10ClassFile[0].name )
    addRunTarget ( x10Environment.Command ( 'run_' + x10ClassName + '_Java' , x10ClassFile , 'x10 ' + x10ClassName ) )
    #
    # C++ backend bits.
    #
    x10Executable = x10Environment.X10Program ( item , X10FLAGS = [ '-O' ] )
    SideEffect ( 'xxx_main_xxx.cc' , x10Executable )
    compileTargets.append ( x10Executable[0].name )
    addRunTarget ( x10Environment.Command ( 'run_' + x10ClassName + '_Cpp' , x10Executable , x10ClassName ) )

#  Clojure  ##########################################################################

clojureEnvironment = Environment ( tools = [ 'javac' ] )

for item in Glob ( '*.clj' ) :
    addRunTarget ( clojureEnvironment.Command ( 'run_' + os.path.splitext ( item.name ) [0] , item.name , 'java -cp .:%s/lib/Java/clojure/clojure.jar clojure.main $SOURCE' % ( os.environ['HOME'] ) ) )

Depends ( 'run_pi_clojure_processSlice' , processSliceClasses )

#  Groovy  ###########################################################################

groovyEnvironment = Environment ( tools = [ 'javac' ] , ENV = os.environ )

dependsOnProcessSlice = [ ]
dependsOnProcessSlice_JCSP = [ ]
for item in Glob ( '*.groovy' ) :
    root = os.path.splitext ( item.name )[0]
    runTarget = 'run_' + root
    bits = root.split ( '_' )
    if bits[1] == 'groovyjava' :
        if bits[3] == 'CSP' : dependsOnProcessSlice_JCSP.append ( runTarget )
        else : dependsOnProcessSlice.append ( runTarget )
    elif bits[1] == 'GroovyJava' : dependsOnProcessSlice.append ( runTarget )
    if len ( bits ) > 3 and bits[3] == 'CSP' :
        jcspJarPath =  groovyEnvironment['ENV']['HOME'] + '/lib/Java/jcsp.jar'
        addRunTarget ( groovyEnvironment.Command ( runTarget , item.name , 'groovy -cp .:%s ./$SOURCE' % jcspJarPath ) )
    else :    
        addRunTarget ( groovyEnvironment.Command ( 'run_' + root , item.name , './$SOURCE' ) )

Depends ( dependsOnProcessSlice_JCSP , processSliceJCSPClasses )
Depends ( dependsOnProcessSlice , processSliceClasses )

#  Python  ###########################################################################

#  Some of the Python code relies on extensions written in various languages.  Because we have both Python 3
#  and Python 2 versions this means that two targets are specifying the same extension, which leads SCons to
#  deduce a "warning: Two different environments were specified for target . . . "  So we have to protect
#  against trying to issue two SharedLibrary calls for the same extensions.  This leads to apparent
#  inefficiency in handling the libraries due to wanting to use node references for dependency handling.
#
#  Cython and Pyrex need access to the Python headers when compiling the generated C and so are Python
#  version dependent.
#
#  This is all grossly over-complicated and needs a complete rethink.

#  At some point it was believed that these options were needed for building the Pyrex and Cython
#  extensions, but they are not.  Leave them here as reminders.
#
#  -Bsymbolic-functions is a Posix ld option only relevant for ELF shared libraries.  -O1 is a Posix ld
#  option for optimization level and only relevant to ELF shared libraries.

#cythonPyrexCcFlags =  ccFlags + [ '-fno-strict-aliasing' , '-DNDEBUG' , '-fwrapv' , '-Wstrict-prototypes' ]
#cythonPyrexLinkFlags =  [ '-Wl,-O1' ,  '-Wl,-Bsymbolic-functions' ]

extensionRoot = 'processSlice'
extensionsData = {
    'c' : {
        'CPPPATH' : [ ] ,
        'CFLAGS' : ccFlags + [ '-std=c99' ] ,
        'LINKFLAGS' : [ ]
        } ,
    'cpp' : {
        'CPPPATH' : [ ] ,
        'CFLAGS' : ccFlags ,
        'LINKFLAGS' : [ ]
        } ,
    'pyrex_py2' : {
        'CPPPATH' : [ '/usr/include/python2.6' ] ,
        'CFLAGS' : ccFlags ,
        'LINKFLAGS' : [ ] ,
        'COMMAND' : 'pyrexc'
        } ,
    'pyrex_py3' : {
        'CPPPATH' : [ '/usr/include/python3.1' ] ,
        'CFLAGS' : ccFlags ,
        'LINKFLAGS' :  [ ] ,
        'COMMAND' : 'pyrexc'
        } ,
    'cython_py2' : {
        'CPPPATH' : [ '/usr/include/python2.6' ] ,
        'CFLAGS' : ccFlags ,
        'LINKFLAGS' : [ ] ,
        'COMMAND' : 'cython'
        } ,
    'cython_py3' : {
        'CPPPATH' : [ '/usr/include/python3.1' ] ,
        'CFLAGS' : ccFlags ,
        'LINKFLAGS' : [ ] ,
        'COMMAND' : 'cython'
        } ,
    }

#  A number of the Python codes use the same extensions.  The extension only needs to be constructed once.
#  Keep a dictionary of items made,

extensionsSharedLibraries = { }

pythonEnvironment = Environment ( )

for item in Glob ( 'pi_python*.py' ) :
    root = os.path.splitext ( item.name ) [0]
    target = 'run_' + root
    bits = root.split ( '_' )
    if len ( bits ) > 5 and bits[4] == 'extension' :
        extension = bits[5]
        assert item.name.find ( extension ) != -1
        if extension in [ 'cython' , 'pyrex' ] :
            majorVersion = bits[1][-1:]
            ##
            ##  Cython 0.11.2 and Pyrex 0.9.8.5 generate C that cannot be compiled against Python 3.1
            ##
            if majorVersion == '3' : continue
            extension += '_py' + majorVersion
        extensionName =  '%s_%s' % ( extensionRoot , extension )
        if extensionName not in extensionsSharedLibraries.keys ( ) :
            extensionsSharedLibraries[extensionName] = pythonEnvironment.SharedLibrary ( extensionName ,
                                        pythonEnvironment.Command ( '%s_%s.c' % ( extensionRoot , extension ) , '%s_%s.pyx' % ( extensionRoot , extension ) ,
                                          extensionsData[extension]['COMMAND'] + ' $SOURCE' ) if extension.split ( '_' )[0] in [ 'pyrex' , 'cython' ] else  '%s_%s.%s' % ( extensionRoot , extension , extension ) ,
                                        CPPPATH = extensionsData[extension]['CPPPATH'] ,
                                        CFLAGS = extensionsData[extension]['CFLAGS'] , CXXFLAGS = extensionsData[extension]['CFLAGS'] ,
                                        SHLIBPREFIX = '' , LINKFLAGS = extensionsData[extension]['LINKFLAGS'] )
        addRunTarget ( pythonEnvironment.Command ( target , [ item.name , extensionsSharedLibraries[extensionName] ] , 'LD_LIBRARY_PATH=. ./$SOURCE' ) )
    else :
        addRunTarget ( pythonEnvironment.Command ( target , item.name , './$SOURCE' ) )

compilePythonExtensions = addCompileTarget ( Alias ( 'compilePythonExtensions' , extensionsSharedLibraries.values ( ) ) )

#  Ruby  #############################################################################

rubyEnvironment = Environment ( )

for item in Glob ( '*.rb' ) :
    addRunTarget ( rubyEnvironment.Command ( 'run_' + os.path.splitext ( item.name ) [0] , item.name , './$SOURCE' ) )

#  Fortress  #########################################################################

fortressEnvironment = Environment ( tools = [ 'latex' ] , ENV = os.environ )

for item in Glob ( '*.fss' ) :
    fortressCodeRoot = os.path.splitext ( item.name ) [0]
    addRunTarget ( fortressEnvironment.Command ( 'run_' + fortressCodeRoot , item.name , 'fortress $SOURCE' ) )
    #  For the moment LaTeX doesn't work correctly on Mac OS X :-(((
    if osName != 'Darwin' :
        pdfDocument = fortressEnvironment.PDF ( fortressCodeRoot + '_document.ltx' )
        Depends ( pdfDocument , fortressEnvironment.Command ( fortressCodeRoot + '.tex' , item.name , 'fortify $SOURCE' ) )
        addCompileTarget ( Alias ( 'typesetFortress' , pdfDocument ) )

#  Erlang  ###########################################################################

erlangEnvironment = Environment ( tools = [ 'erlang' ] )

#  The trailing slash on the OUTPUT is critical for the way the erlang tool works :-(

microsecondTimeErlang = erlangEnvironment.Erlang ( '../Timing/microsecondTime.erl' ,  OUTPUT = './' )

for item in Glob ( 'pi_erlang_*.erl' ) :
    root = os.path.splitext ( item.name ) [0]
    addRunTarget ( erlangEnvironment.Command ( 'run_' + root , [ addCompileTarget ( erlangEnvironment.Erlang ( item ) ) , microsecondTimeErlang ] , '$ERL -noshell -s %s -smp' % ( root ) ) )
    #  Executing an Erlang program can result in a crash dump file so let SCons know this.
    SideEffect (  'erl_crash.dump' ,  'run_' + root )

####################################################################################
####################################################################################

addCompileTarget ( Alias ( 'compile' , compileTargets + compilePythonExtensions ) )

addRunTarget ( Alias ( 'run' , runTargets ) )

Default ( 'compile' )

helpString = 'Compile targets are:\n'
compileTargets.sort ( )
for target in compileTargets :
    helpString += '\t' + target + '\n'
helpString += '\nRun targets are:\n'
runTargets.sort ( )
for target in runTargets :
    helpString += '\t' + target + '\n'
helpString += '\nDefault is to achieve all compile targets.\n'
Help ( helpString )
