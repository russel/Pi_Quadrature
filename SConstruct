# -*- mode:python; coding:utf-8; -*-

#  Calculation of π using quadrature.
#
#  Copyright © 2008–2013 Russel Winder

import os
import subprocess
import sys


from executablesupport import compileTargets, addCompileTarget, runTargets, addRunTarget, executables, createHelp


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
