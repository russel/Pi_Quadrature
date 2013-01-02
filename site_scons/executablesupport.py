# -*- mode:python; coding:utf-8; -*-

#  Some bits and pieces for supporting the SCons build of various native code language versions of the
#  calculation of Pi using quadrature.
#
#  Copyright © 2013  Russel Winder

compileTargets = []

def addCompileTarget(target):
    global compileTargets
    compileTargets.append(target[0].name)
    return target

runTargets = []

def addRunTarget(target):
    global runTargets
    runTargets.append(target[0].name)
    return target

executables = []

def createHelp():
    helpString = 'Compile targets are:\n'
    compileTargets.sort()
    for target in compileTargets:
        helpString += '\t' + target + '\n'
    helpString += '\nRun targets are:\n'
    runTargets.sort()
    for target in runTargets:
        helpString += '\t' + target + '\n'
    helpString += '\nDefault is to achieve all compile targets.\n'
    return helpString
