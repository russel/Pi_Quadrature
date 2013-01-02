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
