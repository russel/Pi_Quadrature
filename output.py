# -*- coding:utf-8; -*-

'''
A module providing an output function for all the variants of the "Π by Quadrature" problem.
'''

# This function must work in both Python 2 and Python 3.

def out ( banner , pi , n , elapseTime , *args ) :
    print ( '================ {}'.format ( banner ) )
    print ( '\tΠ = {:.18f}'.format ( pi ) )
    print ( '\titeration count = {}'.format ( n ) )
    print ( '\telapse time = {}'.format ( elapseTime ) )
    if len ( args ) > 0 :
        assert len ( args ) == 1
        print ( '\ttask count = {}'.format ( args[0] ) ) 
