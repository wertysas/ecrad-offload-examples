program hoisted_scheme_program

use hoisted_scheme_layer_module, only: hoisted_scheme_layer

implicit none

integer, parameter :: ngptot = 128
integer, parameter :: nproma = 16
integer, parameter :: nflevg = 32


call hoisted_scheme_layer(ngptot, nflevg, nproma)


end program
