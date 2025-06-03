module hoisted_scheme_layer_module


  use field_module, only: field_2rb, field_3rb
  use field_factory_module
  use parkind1, only: jprb
  use radiation_types_module, only: single_level_type, flux_type
  use hoisted_scheme_module, only: hoisted_scheme
  use field_data_module, only: field_data

  implicit none

  contains

  subroutine hoisted_scheme_layer(klon, klev, nproma)
    integer, intent(in) :: klon, klev, nproma

    type(field_data)            :: fields
    real(kind=jprb), pointer    :: cos_sza_gpu(:,:), flux_sw_gpu(:,:,:), flux_lw_gpu(:,:,:)
    integer                     :: jkglo, kidia, kfdia, ibl, jlon, jlev
    logical                     :: okay = .true.
    type(single_level_type)     :: level
    type(flux_type)             :: fluxes

    ! initialise field data
    call fields%init(nproma, klon, klev)
    
    ! copy radiation types to device
    !$acc enter data copyin(level, fluxes)

    ! copy field data to device and set access pointers to device pointers
    call fields%f_single_level_cos_sza%get_device_data_rdwr(level%ii_cos_sza)
    call fields%f_flux_sw%get_device_data_rdwr(fluxes%ii_sw)
    call fields%f_flux_lw%get_device_data_rdwr(fluxes%ii_lw)

    ! open acc structured data region
    !$acc data copy(okay) &
    !$acc & present(level, fluxes) &
    !$acc & attach(level%ii_cos_sza, fluxes%ii_sw, fluxes%ii_lw)

    ! call kernel with structs
    !$acc parallel loop gang vector_length(nproma)
    do jkglo = 1,klon, nproma
      kidia=1
      kfdia=min(nproma, klon-jkglo+1)
      ibl=(jkglo-1)/nproma + 1
      call hoisted_scheme(ibl, kidia, kfdia, nproma, klev, level, fluxes, okay)
    end do

    !$acc end data

    ! copy fields back to host
    call fields%f_single_level_cos_sza%sync_host_rdwr()
    call fields%f_flux_sw%sync_host_rdwr()
    call fields%f_flux_lw%sync_host_rdwr()

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

  end subroutine hoisted_scheme_layer

end module hoisted_scheme_layer_module
