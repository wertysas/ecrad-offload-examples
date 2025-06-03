module hoisted_scheme_module

  use parkind1, only: jprb
  use radiation_types_module, only: single_level_type, flux_type
  use field_data_module, only: field_type

  implicit none

  contains

  ! hoisted kernel
  subroutine hoisted_scheme(ibl, kidia, kfdia, klon, klev, level, fluxes, okay)
    integer, intent(in)                       :: ibl      ! ibl injected block index
    integer, intent(in)                       :: kidia    ! start column to process
    integer, intent(in)                       :: kfdia    ! end column to process
    integer, intent(in)                       :: klon     ! number of columns (nproma)
    integer, intent(in)                       :: klev     ! number of levels
    type(single_level_type), intent(inout)   :: level
    type(flux_type),         intent(inout)   :: fluxes
    logical , intent(inout)                   :: okay
    integer             :: jlev, jlon
    !$acc routine seq


    do jlev=1,klev
      do jlon=kidia,kfdia
        if (fluxes%ii_sw(jlon,jlev,ibl) /= 1) okay = .false.
        fluxes%ii_sw(jlon,jlev,ibl) = 10*jlon+jlev
        if (fluxes%ii_lw(jlon,jlev,ibl) /= 2) okay = .false.
        fluxes%ii_lw(jlon,jlev,ibl) = 100*jlon + jlev
      end do
    end do
    do jlon = kidia, kfdia
      if (level%ii_cos_sza(jlon,ibl) /= 0.) okay = .false.
      level%ii_cos_sza(jlon,ibl) = jlon
    end do

  end subroutine hoisted_scheme

end module hoisted_scheme_module
