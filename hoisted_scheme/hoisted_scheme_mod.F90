module hoisted_scheme_module

  use parkind1, only: jprb
  use radiation_types_module, only: single_level_type, flux_type
  use field_data_module, only: field_type

  implicit none

  contains

  ! hoisted kernel
  subroutine hoisted_scheme(kidia, kfdia, klon, klev, cos_sza, sw, lw, okay)
    integer, intent(in)                     :: kidia    ! start column to process
    integer, intent(in)                     :: kfdia    ! end column to process
    integer, intent(in)                     :: klon     ! number of columns (nproma)
    integer, intent(in)                     :: klev     ! number of levels
    real(kind=jprb), target,  intent(inout) :: cos_sza(:)   ! (klon)
    real(kind=jprb), target,  intent(inout) :: sw(:,:)      ! (klon,nlev)
    real(kind=jprb), target,  intent(inout) :: lw(:,:)      ! (klon,nlev)
    logical , intent(inout)                 :: okay

    type(single_level_type)  :: level
    type(flux_type)          :: fluxes
    integer             :: jlev, jlon
    !$acc routine seq

    ! associate type member pointers to arrays
    level%cos_sza => cos_sza(:)
    fluxes%sw => sw(:,:)
    fluxes%lw => lw(:,:)


    do jlev=1,klev
      do jlon=kidia,kfdia
        if (fluxes%sw(jlon,jlev) /= 1) okay = .false.
        fluxes%sw(jlon,jlev) = 10*jlon+jlev
        if (fluxes%lw(jlon,jlev) /= 2) okay = .false.
        fluxes%lw(jlon,jlev) = 100*jlon + jlev
      end do
    end do
    do jlon = kidia, kfdia
      if (level%cos_sza(jlon) /= 0.) okay = .false.
      level%cos_sza(jlon) = jlon
    end do

  end subroutine hoisted_scheme

end module hoisted_scheme_module
