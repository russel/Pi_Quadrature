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
                    CC = compiler , CPPPATH = [ '../Timing' ] + cpppath , CFLAGS = cflags , LINKFLAGS= linkflags , LIBPATH = libpath , LIBS = libs )
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
cppRule ( 'pi_cpp_openmp*.cpp' , cxxflags = ccFlags + [ '-fopenmp' ] , libs = [ 'gomp' ] )  #  Assumes gcc
#  is 4.2.0 or greater since that is when gomp was included.  Ubuntu 9.10 Karmic Koala has Boost 1.38 as
#  default but 1.40 is available -- use 1.40 in preference to 1.38.  Ubuntu 10.4 Lucind Lynx has Boost 1.40
#  as default but 1.41 is available and this is the one to use in preference.
cppRule ( 'pi_cpp_boostThread*.cpp' , libs = [ 'boost_thread' ] ) 
#  As at 2010-03-04 15:56+00:00, the Boost MPI library is not in Lucid -- neither the 1.40 or 1.41 are there.
####cppRule ( 'pi_cpp_boostMPI*.cpp' , compiler = 'mpic++' , libs = [ 'boost_mpi' ] ) # This MPI execution target runs things sequentially.  Use the command "mpirun -np N pi_cpp_boostMPI" to run the code on N processors.

#  Using Anthony Williams' Just::Thread library as an implementation of C++0x threads and things.  Use the
#  standard deb file.
cppRule ( 'pi_cpp_justThread*.cpp' , cpppath = [ '/usr/include/justthread' ] , cxxflags = ccFlags + [ '-std=c++0x' ] , linkflags = [ '-std=c++0x' ] , libs = [ 'justthread' , 'rt' ] )

#  Intel's Threading Building Blocks (TBB) is provided only with dynamic libraries, there are no static
#  libraries, so we have to get into the hassle of specifying a LD_LIBRARY_PATH since the location is not in
#  the standard path :-( "LD_LIBRARY_PATH=/opt/TBB pi_cpp_tbb . . . "
tbbPath = '/opt/TBB'
cppRule (  'pi_cpp_tbb*.cpp' , cpppath = [ tbbPath + '/include' ] , libpath = [ tbbPath ] , libs = [ 'tbb' ] )

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

##  As at 2010-03-02 using D 2.040 the D threads examples does not compile due to a problem that causes an
##  assertion fail in src/phobos/std/traits.d

#for item in Glob ( 'pi_d2_*.d' ) :
for item in Glob ( 'pi_d2_sequential.d' ) :
    root = os.path.splitext ( item.name ) [0]
    #  As at 2010-03-03, the standard D tool assumes D v1.0, but we are using an amended version so it correctly finds libphobos2.
    #  NB as at 2010-03-03 the D systems is only a 32-bit system which generates 32-bit systems.
    executables.append ( addCompileTarget ( environment.Program ( item.name , DFLAGS = [ '-O' ] ) ) )

#  Chapel  ###########################################################################

for item in Glob ( 'pi_chapel_*.chpl' ) :
    executables.append ( addCompileTarget ( environment.Command ( os.path.splitext ( item.name ) [0] , item.name , 'chpl -o $TARGET -O --fast $SOURCE' ) ) )

#  Haskell  ##########################################################################

##  As at 2010-02-03 using GHC 6.10.4 only the sequential Haskell code compiles :-((

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
    if variant == 'mpi' : extraOptions = '-I /opt/OCamlMPI mpi.cmxa unix.cmxa' #  These programs get run as sequential ones from SCons.  Use the command "mpirun -np N . . . " to run the code on N processors.
    executables.append ( addCompileTarget ( environment.Command ( root , item.name , 'ocamlopt -o $TARGET %s $SOURCE' % ( extraOptions ) ) ) )
    SideEffect ( [ root + '.' + extension for extension in [ 'cmi' , 'cmx' , 'o' ] ] , root )

#  Go  ###############################################################################

for item in Glob ( 'pi_go_*.go' ) :
    root = os.path.splitext ( item.name ) [0]
    goVariant = '6'
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

####  The X10 compiler takes an infinite amount of time to compile even trivial programs, so take it out of
####  the equation.

for item in Glob ( 'Pi_X10_*.x10' ) :
    className = os.path.splitext ( item.name ) [0]
    compiledFileName = className + '.class'
    compileTargets.append ( compiledFileName )
    addRunTarget ( environment.Command ( 'run_' + className , environment.Command ( compiledFileName , item.name , 'x10c ' + item.name ) , 'x10 ' + className ) )
    SideEffect ( [ className + '.java' ] , item.name )

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

for item in Glob ( '*.groovy' ) :
    if item.name == 'pi_groovyjava_gpars_CSP.groovy' :
        jcspJarPath =  environment['ENV']['HOME'] + '/lib/Java/jcsp.jar'
        addRunTarget ( environment.Command ( 'run_' + os.path.splitext ( item.name ) [0] , item.name , 'groovy -cp .:%s ./$SOURCE' % jcspJarPath ) )
        Depends ( 'run_pi_groovyjava_gpars_CSP' , environment.Java ( '.' , [ 'ProcessSlice_JCSP.java' ] , JAVACLASSPATH = [ jcspJarPath ] ) )
    else :    
        addRunTarget ( environment.Command ( 'run_' + os.path.splitext ( item.name ) [0] , item.name , './$SOURCE' ) )
        Depends ( [ 'run_pi_groovyjava_gpars_actorScript' , 'run_pi_groovyjava_gpars_gparsPool' , 'run_pi_groovyjava_gpars_parallelEnhancer' ] , environment.Java ( '.' , [ 'ProcessSlice.java' ] ) )

#  Python  ###########################################################################

for item in Glob ( 'pi_python*.py' ) :
    addRunTarget ( environment.Command ( 'run_' + os.path.splitext ( item.name ) [0] , item.name , './$SOURCE' ) )

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

addCompileTarget ( Alias ( 'compile' , compileTargets ) )

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
