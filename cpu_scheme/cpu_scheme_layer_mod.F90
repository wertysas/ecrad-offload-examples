module cpu_scheme_layer_module


  use field_module, only: field_2rb, field_3rb
  use field_factory_module
  use parkind1, only: jprb
  use radiation_types_module, only: single_level_type, flux_type
  use cpu_scheme_module, only: cpu_scheme
  use field_data_module, only: field_data

  implicit none

  contains

  subroutine cpu_scheme_layer(klon, klev, nproma)
    integer, intent(in) :: klon, klev, nproma

    type(field_data)          :: fields
    real(kind=jprb), pointer  :: cos_sza_gpu(:,:), flux_sw_gpu(:,:,:), flux_lw_gpu(:,:,:)
    integer                   :: jkglo, kidia, kfdia, ibl, jlon, jlev
    logical                   :: okay = .true.


    ! initialise field data and copy fields to device
    call fields%init(nproma, klon, klev)

    do jkglo = 1,klon, nproma
      kidia=1
      kfdia=min(nproma, klon-jkglo+1)
      ibl=(jkglo-1)/nproma + 1
      call fields%update_view(ibl)
      call cpu_scheme(kidia, kfdia, nproma, klev, fields%single_level_cos_sza, fields%flux_sw, fields%flux_lw, okay)
    end do


    ! verify that data was corectly transferred to device
    if ( .not. okay) print *, "ERROR wrong fields values on device"
    ! verify that fields have been updated
    do jkglo = 1,klon, nproma
      kidia=1
      kfdia=min(nproma, klon-jkglo+1)
      ibl=(jkglo-1)/nproma + 1
      call fields%update_view(ibl)
      do jlev=1,klev
        do jlon=1,nproma
          if (fields%flux_sw(jlon,jlev) /= 10*jlon+jlev) print *, "ERROR wrong sw flux value after kernel: ", fields%flux_sw(jlon,jlev)
          if (fields%flux_lw(jlon,jlev) /= 100*jlon+jlev) print *, "ERROR wrong lw flux value after kernel: ", fields%flux_lw(jlon,jlev)
        end do
      end do
      do jlon=kidia,kfdia
        if (fields%single_level_cos_sza(jlon) /= jlon) print *, "ERROR wrong cos_sza value after kernel: ", fields%single_level_cos_sza(jlon)
      end do
    end do

    ! delete and clean up field data
    call fields%final()

  end subroutine cpu_scheme_layer

end module cpu_scheme_layer_module
