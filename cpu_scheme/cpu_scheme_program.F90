program hoisted_scheme_program

use cpu_scheme_layer_module, only: cpu_scheme_layer

implicit none

integer, parameter :: ngptot = 128
integer, parameter :: nproma = 16
integer, parameter :: nflevg = 32


call cpu_scheme_layer(ngptot, nflevg, nproma)


end program
