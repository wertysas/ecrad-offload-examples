
add_executable(hoisted_ii_scheme
    hoisted_ii_scheme_layer_mod.F90
    hoisted_ii_scheme_mod.F90
    hoisted_ii_scheme_program.F90
    ../radiation/radiation_ii_types.F90     # radiation types
    ../field_api/field_data_module.F90      # field wrapper of radiation types
)

target_link_libraries(hoisted_ii_scheme
    field_api_dp
    parkind_dp
    fiat
    OpenACC::OpenACC_Fortran
    OpenMP::OpenMP_Fortran
    )

