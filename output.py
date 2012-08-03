# -*- coding:utf-8; -*-

'''
A module providing an output function for all the variants of the "Π by Quadrature" problem.
'''

# This function must work in both Python 2 and Python 3.

from multiprocessing import cpu_count
from os.path import basename

def out ( name , pi , n , elapseTime , *args ) :
    outputString = '================ {}'.format ( basename ( name ) )
    pCount = len ( args )
    if pCount > 1 :
        raise ValueError ( 'Too many parameters to out function.' )
    if pCount > 0 :
        outputString += ', task count = {}'.format ( args[0] )
    outputString += '\n\tπ = {:.18f}'.format ( pi )
    outputString += '\n\titeration count = {}'.format ( n )
    outputString += '\n\telapse time = {}'.format ( elapseTime )
    outputString += '\n\tprocessor count = {}'.format ( cpu_count ( ) )
    print ( outputString )
