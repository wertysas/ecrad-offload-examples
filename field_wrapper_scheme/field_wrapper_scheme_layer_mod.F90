module field_wrapper_scheme_layer_module

  use parkind1, only: jprb
  use radiation_types_module, only: single_level_type, flux_type
  use radiation_field_type_module, only: single_level_field_type, flux_field_type
  use cpu_scheme_module, only: cpu_scheme

  implicit none

  contains

  subroutine field_wrapper_scheme_layer(klon, klev, nproma)
    integer, intent(in) :: klon, klev, nproma

    type(field_data)              :: fields
    real(kind=jprb), pointer      :: cos_sza_gpu(:,:), flux_sw_gpu(:,:,:), flux_lw_gpu(:,:,:)
    integer                       :: jkglo, kidia, kfdia, ibl, jlon, jlev
    logical                       :: okay = .true.
    type(single_level_type)       :: single_level
    type(single_level_field_type) :: single_level_wrapper
    type(flux_type)               :: flux
    type(flux_field_type)         :: flux_wrapper

    ! initialise field data (also allocates gpu pointer storage)
    call single_level_wrapper%init(nproma, klev, on_gpu=.true.)
    call single_level_wrapper%attach()
    call flux_wrapper%init(nproma, klon, klev, on_gpu=.true.)
    call flux_wrapper%attach()
  

    ! copy radiation types to device
    !$acc enter data copyin(single_level, flux)

    ! open acc structured data region
    !$acc data copy(okay) &
    !$acc & present(single_level_wrapper, single_level, flux_wrapper, flux) &

    !$acc parallel loop gang vector_length(nproma)
    do jkglo = 1,klon, nproma
      kidia=1
      kfdia=min(nproma, klon-jkglo+1)
      ibl=(jkglo-1)/nproma + 1

      ! Associate ptrs in derived types to Field API device buffers
      call single_level_field_associate_pointer(single_level_wrapper, single_level, ibl)
      call flux_field_associate_pointer(flux_wrapper, flux, ibl)

      call cpu_scheme(ibl, kidia, kfdia, nproma, klev, single_level, flux, okay)
    end do

    !$acc end data
    
    ! detach device wrappers
    call single_level_wrapper%detach()
    call flux_wrapper%detach()

    ! copy field data back to host
    call single_level_wrapper%f_single_level_cos_sza%sync_host_rdwr()
    call flux%f_flux_sw%sync_host_rdwr()
    call flux%f_flux_lw%sync_host_rdwr()

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

  end subroutine field_wrapper_scheme_layer

end module field_wrapper_scheme_layer_module
