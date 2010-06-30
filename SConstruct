# -*- mode:python; coding:utf-8; -*-

#  Calculation of Pi using quadrature.
#
#  Copyright © 2008-10 Russel Winder 

import os
import sys

environment = Environment (
    tools = [ 'default' , 'csharp' , 'erlang' , 'haskell' ] ,
    ENV = os.environ ,
    PATH = os.environ['PATH']
    )

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

#  C  ################################################################################

ccFlags = [ '-O3' , '-Wall' ]

microsecondTimeC = Object ( '../Timing/microsecondTime.c' , CFLAGS = ccFlags )

def cRule ( globPattern , compiler = 'gcc' , cpppath = [ ] , cflags = ccFlags , linkflags = [ ] , libpath = [ ] , libs = [ ] ) :
    for item in Glob ( globPattern ) :
        executables.append (
            addCompileTarget (
                environment.Program (
                    os.path.splitext ( item.name ) [0] , [ item.name , microsecondTimeC ] ,
                    CC = compiler , CPPPATH = [ '../Timing' ] + cpppath , CFLAGS = cflags + [ '-std=c99' ] , LINKFLAGS= linkflags , LIBPATH = libpath , LIBS = libs )
                )
            )

cRule ( 'pi_c_sequential*.c' )
cRule ( 'pi_c_pthread*.c' , libs = [ 'pthread' ] )
cRule ( 'pi_c_mpi*.c' , compiler = 'mpicc' )  #  This execution target runs things sequentially.  Use the command "mpirun -np N pi_c_mpi" to run the code on N processors.
cRule ( 'pi_c_openmp*.c' , cflags = ccFlags + [ '-fopenmp' ] , libs = [ 'gomp' ] )  #  Assumes gcc is 4.2.0 or greater since that is when gomp was included.

#  C++  ##############################################################################

def cppRule ( globPattern , compiler = 'g++' , cpppath = [ ] , cxxflags = ccFlags , linkflags = [ ] , libpath = [ ] , libs = [ ] ) :
    for item in Glob ( globPattern ) :
        executables.append (
            addCompileTarget (
                environment.Program (
                    os.path.splitext ( item.name ) [0] , [ item.name , microsecondTimeC ] ,
                    CXX = compiler , CPPPATH = [ '../Timing' ] + cpppath , CXXFLAGS = cxxflags , LINKFLAGS= linkflags , LIBPATH = libpath , LIBS = libs )
                )
            )

cppRule ( 'pi_cpp_sequential*.cpp' )
cppRule ( 'pi_cpp_pthread*.cpp' , libs = [ 'pthread' ] )
cppRule ( 'pi_cpp_mpi*.cpp' , compiler = 'mpic++' )  #  This MPI execution target runs things sequentially.  Use the command "mpirun -np N pi_c_mpi" to run the code on N processors.
cppRule ( 'pi_cpp_openmp*.cpp' , cxxflags = ccFlags + [ '-fopenmp' ] , libs = [ 'gomp' ] ) #  Assumes gcc is 4.2.0 or greater since that is when gomp was included.

#  As from 2010-03-04 15:56+00:00, the Boost MPI library is not in Lucid.  As is document in the bug tracker
#  on Launchpad (https://bugs.launchpad.net/ubuntu/+source/boost-defaults/+bug/531973 and
#  https://bugs.launchpad.net/ubuntu/+source/boost1.42/+bug/582420), Boost.MPI has been ejected from
#  Ubuntu!!!!!!!!
#
#  NB The MPI execution targets runs things sequentially.  Use the command "mpirun -np N pi_cpp_boostMPI" to
#  run the code on N processors.
if not os.path.isfile ( '/usr/lib/libboost_mpi.so' ) :
    boostHome = os.environ['BOOST_HOME']
    boostInclude = boostHome + '/include'
    boostLib = boostHome + '/lib'
    cppRule ( 'pi_cpp_boostThread*.cpp' , cpppath = [ boostInclude ] , libpath = [ boostLib ] , libs = [ 'boost_thread' ] ) 
    cppRule ( 'pi_cpp_boostMPI*.cpp' , compiler = 'mpic++' , cpppath = [ boostInclude ] , libpath = [ boostLib ] , libs = [ 'boost_mpi' , 'boost_serialization' ] )
else :
    cppRule ( 'pi_cpp_boostThread*.cpp' , libs = [ 'boost_thread' ] ) 
    cppRule ( 'pi_cpp_boostMPI*.cpp' , compiler = 'mpic++' , libs = [ 'boost_mpi' ] )

#  Use Anthony Williams' Just::Thread library as an implementation of C++0x threads and things.  Anthony's
#  Ubuntu Lucid deb seems to work fine on Debian Squeeze, which is good.  In order to use the standard
#  naming in the source code we have to augment the include path.  Must also inform GCC that we are using
#  the next standard.
cppRule ( 'pi_cpp_justThread*.cpp' , cpppath = [ '/usr/include/justthread' ] , cxxflags = ccFlags + [ '-std=c++0x' ] , linkflags = [ '-std=c++0x' ] , libs = [ 'justthread' , 'rt' ] )

#  TBB 2.2 is present packaged in Ubuntu Lucid and Debian Squeeze, but TBB 3 is now out and compiled up in
#  some location known to the shell.
if not os.path.isfile ( '/usr/lib/libtbb.so.3' ) :
    #  Intel's Threading Building Blocks (TBB) only provides dynamic libraries, there are no static
    #  libraries, so we have to get into the hassle of specifying a LD_LIBRARY_PATH to run the constructed
    #  executable since the location is not in the standard path :-( "LD_LIBRARY_PATH=$TBB_HOME pi_cpp_tbb
    #  . . . "
    tbbHome = os.environ['TBB_HOME']
    cppRule (  'pi_cpp_tbb*.cpp' , cpppath = [ tbbHome + '/include' ] , libpath = [ tbbHome ] , libs = [ 'tbb' ] )
else :
    cppRule (  'pi_cpp_tbb*.cpp' , libs = [ 'tbb' ] )

#  Fortran  ##########################################################################

fortranFlags = [ '-O3' , '-std=f2003' , '-ffree-form' , '-pedantic' , '-Wall' ]

def fortranRule ( globPattern , compiler = 'gfortran' , fortranflags = fortranFlags , linkflags = [ ] , libpath = [ ] , libs = [ ] ) :
    for item in Glob ( globPattern ) :
        executables.append (
            addCompileTarget (
                environment.Program (
                    os.path.splitext ( item.name ) [0] , item.name ,
                    FORTRAN = compiler , FORTRANFLAGS = fortranflags , LINKFLAGS= linkflags , LIBPATH = libpath , LIBS = libs )
                )
            )

fortranRule ( 'pi_fortran_sequential*.f' )
fortranRule ( 'pi_fortran_openmp*.f' , fortranflags = fortranFlags + [ '-fopenmp' ] , libs = [ 'gomp' ] )
fortranRule ( 'pi_fortran_mpi*.f' , compiler = 'mpif90' )

#  D  ################################################################################

##  NB As at 2010-06-21 the D compiler (2.047) is a 32-bit application that generates 32-bit code.  So on
##  64-bit platforms special care is needed.

##  As at 2010-04-24 using D 2.043 the D threads examples does not compile due to a problem that causes an
##  assertion fail in src/phobos/std/traits.d

for item in Glob ( 'pi_d2_*.d' ) :
    continue # Temporary hack because the D tool amends LIBS which ruins later C linking.
    if item.name != 'pi_d2_sequential.d' : continue # Temporary hack as the threads stuff won't compile.
    root = os.path.splitext ( item.name ) [0]
    #  As at 2010-06-23, the standard D tool in SCons assumes D v1.0, but we use an amended version (that
    #  has yet to be merged in :-( so it correctly finds libphobos2.
    executables.append ( addCompileTarget ( environment.Program ( item.name , DFLAGS = [ '-O' ] ) ) )

#  Chapel  ###########################################################################

for item in Glob ( 'pi_chapel_*.chpl' ) :
    executables.append ( addCompileTarget ( environment.Command ( os.path.splitext ( item.name ) [0] , item.name , 'chpl -o $TARGET -O --fast $SOURCE' ) ) )

#  Haskell  ##########################################################################

#  Haskell jobs run in a single thread by default (which is what happens here).  Run the executable with
#  "+RTS -Nx" to run with x OS threads.

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
        executables.append ( addCompileTarget ( environment.Command ( root , item.name , 'ghc --make -O -o $TARGET' + ''.join ( [ ' ' + x for x in options ] ) + ' $SOURCE' ) ) )
        SideEffect ( [  root + ext for ext in [ '.hi' , '.o' , '.hp' , '.ps' , '.aux' ] ] ,  root )
    else :
        executables.append ( addCompileTarget ( environment.HaskellProgram ( item , HSLINKFLAGS = [ '--make' ] ) ) )

#  OCaml  ############################################################################

for item in Glob ( 'pi_ocaml_*.ml' ) :
    root = os.path.splitext ( item.name ) [0]
    extraOptions = ''
    variant = root.split ( '_' )[2]
    if variant == 'threads' : extraOptions = '-thread unix.cmxa threads.cmxa'
    if variant == 'mpi' : extraOptions = '-I ' + os.environ['OCAMLMPI_HOME'] + ' mpi.cmxa unix.cmxa' #  These programs get run as sequential ones from SCons.  Use the command "mpirun -np N . . . " to run the code on N processors.
    executables.append ( addCompileTarget ( environment.Command ( root , item.name , 'ocamlopt -o $TARGET %s $SOURCE' % ( extraOptions ) ) ) )
    SideEffect ( [ root + '.' + extension for extension in [ 'cmi' , 'cmx' , 'o' ] ] , root )

#  Go  ###############################################################################

for item in Glob ( 'pi_go_*.go' ) :
    root = os.path.splitext ( item.name ) [0]
    if os.uname ( ) [4] == 'x86_64' : goVariant = '6'
    else : goVariant = '8'
    executables.append ( addCompileTarget ( environment.Command ( root , environment.Command ( root + '.' + goVariant , root + '.go' , goVariant + 'g -o $TARGET $SOURCE' ) , goVariant + 'l -o $TARGET $SOURCE' ) ) )

## #################################################################################
##  All the native compiled executables are processed the same way.
## #################################################################################

for item in executables :
    addRunTarget ( environment.Command ( 'run_' + item[0].name , item , './' + item[0].name ) )

#  Java  #############################################################################

for item in Glob ( 'Pi_Java_*.java' ) :
    className = os.path.splitext ( item.name ) [0]
    compileTargets.append ( className + '.class' )
    variant = className.split ( '_' )[2]
    if variant == 'DataRush' :
        dataRushHome = os.environ['HOME'] + '/lib/Java/datarush'
        compiledBits = environment.Java ( target = '.' , source = item , JAVACLASSPATH = [ f.path for f in Glob ( dataRushHome + '/lib/*.jar' ) ] )
        javaCommand = dataRushHome + '/bin/dr '
    elif variant == 'JCSP' :
        jcspJarPath = os.environ['HOME'] + '/lib/Java/jcsp.jar'
        compiledBits = environment.Java ( target = '.' , source = item , JAVACLASSPATH = [ jcspJarPath ] )
        javaCommand = 'java -cp .:' + jcspJarPath
    elif variant == 'ForkJoinBasic' or variant == 'ForkJoinCollection' :
        jsr166yJarPath = os.environ['HOME'] + '/lib/Java/jsr166y.jar'
        compiledBits = environment.Java ( target = '.' , source = item , JAVACLASSPATH = [ jsr166yJarPath ] )
        javaCommand = 'java -cp .:' + jsr166yJarPath
    elif variant == 'ParallelArray' :
        jsr166yJarPath = os.environ['HOME'] + '/lib/Java/jsr166y.jar'
        extra166yJarPath = os.environ['HOME'] + '/lib/Java/extra166y.jar'
        compiledBits = environment.Java ( target = '.' , source = item , JAVACLASSPATH = [ jsr166yJarPath , extra166yJarPath ] )
        javaCommand = 'java -cp .:' + jsr166yJarPath + ':' + extra166yJarPath
    elif variant == 'FunctionalJava' :
        jarPath = os.environ['HOME'] + '/lib/Java/functionaljava.jar'
        compiledBits = environment.Java ( target = '.' , source = item , JAVACLASSPATH = [ jarPath ] )
        javaCommand = 'java -cp .:' + jarPath
    else :
        compiledBits = environment.Java ( target = '.' , source = item )
        javaCommand = 'java'
    addRunTarget ( environment.Command ( 'run_' + className , compiledBits , javaCommand + ' ' + className ) )

for item in Glob ( 'Pi_Scala_*.scala' ) :
    className = os.path.splitext ( item.name ) [0]
    compiledFileName = className + '.class'
    variant = className.split ( '_' )[2]
    extraStuff = ''
    if variant == 'FunctionalJava' :
        extraStuff = '-cp .:%s/lib/Java/functionaljava.jar' % ( os.environ['HOME'] , )
    elif variant == 'Scalaz' :
        extraStuff = '-cp .:%s/lib/Java/scalaz-core.jar' % ( os.environ['HOME'] , )
    compileTargets.append ( compiledFileName )
    addRunTarget ( environment.Command ( 'run_' + className , environment.Command ( compiledFileName , item.name , ( ( 'scalac %s -optimise ' + item.name ) % ( extraStuff , ) ) ) , ( ( 'scala %s ' + className ) % ( extraStuff , ) ) ) )

##  NB The Java builder does not track all the classes generated by the compilation so not all class files
##  get automatically removed during clean.  The same applies to the Scala compilation.

Clean ( '.' , Glob ( '*.class' ) )

#  X10  ##############################################################################

####  With X10 2.0.[23] things compile using the Java backend but the sequential code seems to cause all
####  cores to go to 90% for what appears to be forever.  Making use instead of the C++ backend which has
####  had much more work done on it allows the code to run. The parallel code doesn't work as yet due to
####  some class cast problem.

for item in Glob ( 'Pi_X10_*.x10' ) :
    className = os.path.splitext ( item.name ) [0]
    #
    #  Java backend bits.
    #
    javaClassFileName = className + '.class'
    compileTargets.append ( javaClassFileName )
    addRunTarget ( environment.Command ( 'run_' + className + '_Java' , environment.Command ( javaClassFileName , item.name , 'x10c -O ' + item.name ) , 'x10 ' + className ) )
    SideEffect ( [ className + '.java' ] , item.name )
    #
    # C++ backend bits.
    #
    cppClassFileName = className + '.x10_cpp'
    compileTargets.append ( cppClassFileName )
    addRunTarget ( environment.Command ( 'run_' + className + '_Cpp' , environment.Command ( cppClassFileName , item.name , 'x10c++ -O -o ' + cppClassFileName + ' ' + item.name ) , 'runx10 ' + cppClassFileName ) )
    SideEffect ( [ className + '.' + extension for extension in [ 'cc' , 'h' , 'inc' ] ] , item.name )

#  Clojure  ##########################################################################

for item in Glob ( '*.clj' ) :
    addRunTarget ( environment.Command ( 'run_' + os.path.splitext ( item.name ) [0] , item.name , 'java -cp .:%s/lib/Java/clojure/clojure.jar clojure.main $SOURCE' % ( os.environ['HOME'] ) ) )
    Depends ( 'run_pi_clojure_processSlice' , environment.Java ( '.' , 'ProcessSlice.java' ) )

#  C#  ###############################################################################

for item in Glob ( 'Pi_CS_*.cs' ) :
    compiledFile = environment.CLIProgram ( item )
    compiledFileName = compiledFile[0].name
    compileTargets.append ( compiledFileName )
    addRunTarget ( environment.Command ( 'run_' + compiledFileName.replace ( '.exe' , '' ) , compiledFile , 'mono ' + compiledFileName ) )

#  Groovy  ###########################################################################

dependsOnProcessSlice = [ ]
dependsOnProcessSlice_JCSP = [ ]
for item in Glob ( '*.groovy' ) :
    root = os.path.splitext ( item.name )[0]
    runTarget = 'run_' + root
    bits = root.split ( '_' )
    if bits[1] == 'groovyjava' :
        if bits[3] == 'CSP' : dependsOnProcessSlice_JCSP.append ( runTarget )
        else : dependsOnProcessSlice.append ( runTarget )
    if len ( bits ) > 3 and bits[3] == 'CSP' :
        jcspJarPath =  environment['ENV']['HOME'] + '/lib/Java/jcsp.jar'
        addRunTarget ( environment.Command ( runTarget , item.name , 'groovy -cp .:%s ./$SOURCE' % jcspJarPath ) )
    else :    
        addRunTarget ( environment.Command ( 'run_' + root , item.name , './$SOURCE' ) )
        
Depends ( dependsOnProcessSlice_JCSP , environment.Java ( '.' , [ 'ProcessSlice_JCSP.java' ] , JAVACLASSPATH = [ jcspJarPath ] ) )
Depends ( dependsOnProcessSlice , environment.Java ( '.' , [ 'ProcessSlice.java' ] ) )

#  Python  ###########################################################################

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
    'pyrex' : {
        'CPPPATH' : [ '/usr/include/python2.6' ] ,
        'CFLAGS' : ccFlags + [ '-fno-strict-aliasing' , '-DNDEBUG' , '-fwrapv' , '-Wstrict-prototypes' ] ,
        'LINKFLAGS' : [ '-Wl,-O1' ,  '-Wl,-Bsymbolic-functions' ] ,
        'COMMAND' : 'pyrexc'
        } ,
    'cython' : {
        'CPPPATH' : [ '/usr/include/python2.6' ] ,
        'CFLAGS' : ccFlags + [ '-fno-strict-aliasing' , '-DNDEBUG' , '-fwrapv' , '-Wstrict-prototypes' ] ,
        'LINKFLAGS' : [ '-Wl,-O1' ,  '-Wl,-Bsymbolic-functions' ] ,
        'COMMAND' : 'cython'
        } ,
    }

sharedLibraries = [ ]

for item in Glob ( 'pi_python*.py' ) :
    root = os.path.splitext ( item.name ) [0]
    target = 'run_' + root
    bits = root.split ( '_' )
    if len ( bits ) > 5 and bits[4] == 'extension' :
        extension = bits[5]
        assert item.name.find ( extension ) != -1
        sharedLibrary =  environment.SharedLibrary ( '%s_%s' % ( extensionRoot , extension ) ,
            environment.Command ( '%s_%s.c' % ( extensionRoot , extension ) , '%s_%s.pyx' % ( extensionRoot , extension ) ,
                extensionsData[extension]['COMMAND'] + ' $SOURCE' ) if extension in [ 'pyrex' , 'cython' ] else  '%s_%s.%s' % ( extensionRoot , extension , extension ) ,
            CPPPATH = extensionsData[extension]['CPPPATH'] ,
            CFLAGS = extensionsData[extension]['CFLAGS'] , CXXFLAGS = extensionsData[extension]['CFLAGS'] ,
            SHLIBPREFIX = '' , LINKFLAGS = extensionsData[extension]['LINKFLAGS'] )
        sharedLibraries.append ( sharedLibrary )
        addRunTarget ( environment.Command ( target , [ item.name , sharedLibrary ] , 'LD_LIBRARY_PATH=. ./$SOURCE' ) )
    else :
        addRunTarget ( environment.Command ( target , item.name , './$SOURCE' ) )

compilePythonExtensions = Alias ( 'compilePythonExtensions' , sharedLibraries )

#  Ruby  #############################################################################

for item in Glob ( '*.rb' ) :
    addRunTarget ( environment.Command ( 'run_' + os.path.splitext ( item.name ) [0] , item.name , './$SOURCE' ) )

#  Fortress  #########################################################################

for item in Glob ( '*.fss' ) :
    fortressCodeRoot = os.path.splitext ( item.name ) [0]
    addRunTarget ( environment.Command ( 'run_' + fortressCodeRoot , item.name , 'fortress $SOURCE' ) )
    pdfDocument = environment.PDF ( fortressCodeRoot + '_document.ltx' )
    Depends ( pdfDocument , environment.Command ( fortressCodeRoot + '.tex' , item.name , 'fortify $SOURCE' ) )
    addCompileTarget ( Alias ( 'typesetFortress' , pdfDocument ) )

#  Erlang  ###########################################################################

microsecondTimeErlang = environment.Erlang ( '../Timing/microsecondTime.erl' ,  OUTPUT = '.' )
for item in Glob ( 'pi_erlang_*.erl' ) :
    root = os.path.splitext ( item.name ) [0]
    addRunTarget ( environment.Command ( 'run_' + root , [ addCompileTarget ( environment.Erlang ( item ) ) , microsecondTimeErlang ] , '$ERL -noshell -s %s -smp' % ( root ) ) )
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
