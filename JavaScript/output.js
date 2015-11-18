/*
 *  Output functions for the JavaScript codes.
 *
 *  Copyright © 2014, 2015  Russel Winder
 */

'use strict';

function out(banner, π, n, δt) {
  print('==================== ' + banner)
  print('\tπ = ' + π)
  print('\titeration count = ' + n)
  print('\telapse time = ' + δt)
}
