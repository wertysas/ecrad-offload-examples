program field_wrapper_scheme_program

use field_wrapper_scheme_layer_module, only: field_wrapper_scheme_layer

implicit none

integer, parameter :: ngptot = 128
integer, parameter :: nproma = 16
integer, parameter :: nflevg = 32


call field_wrapper_scheme_layer(ngptot, nflevg, nproma)


end program
