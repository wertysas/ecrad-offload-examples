! (C) Copyright 2025- ECMWF.
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.


module radiation_types_module

  use parkind1, only: jprb

  implicit none

  ! reduced version of ecRAD single_level type
  type single_level_type
    real(jprb), pointer, dimension(:) :: cos_sza ! (ncol) Cosine of solar zenith angle
    real(jprb) :: solar_irradiance = 1366.0_jprb ! W m-2
    
    ! extra field only used for index injection variant
    real(jprb), pointer, contiguous, dimension(:,:) :: ii_cos_sza ! (ncol, npgot/nproma) Cosine of solar zenith angle

  contains
    procedure :: allocate   => allocate_single_level
    procedure :: deallocate => deallocate_single_level
  end type single_level_type

  ! reduced version of ecRAD flux type
  type flux_type
    ! dimensions (ncol,nlev+1).
    real(jprb), pointer, dimension(:,:) :: lw, sw

    ! extra fields only used for index injection variants
    real(jprb), pointer, contiguous, dimension(:,:,:) :: ii_lw, ii_sw

  contains
    procedure :: allocate   => allocate_flux_type
    procedure :: deallocate => deallocate_flux_type
  end type flux_type

contains

  subroutine allocate_single_level(this, ncol)
    class(single_level_type), intent(inout) :: this
    integer, intent(in)                     :: ncol

    allocate(this%cos_sza(ncol))
  end subroutine allocate_single_level

  subroutine deallocate_single_level(this)
    class(single_level_type), intent(inout) :: this

    deallocate(this%cos_sza)
  end subroutine deallocate_single_level

  subroutine allocate_flux_type(this, istartcol, iendcol, nlev)
    class(flux_type), intent(inout) :: this
    integer, intent(in)             :: istartcol, iendcol, nlev

    allocate(this%lw(istartcol:iendcol,nlev+1))
    allocate(this%sw(istartcol:iendcol,nlev+1))
  end subroutine allocate_flux_type

  subroutine deallocate_flux_type(this)
    class(flux_type), intent(inout) :: this

    deallocate(this%lw)
    deallocate(this%sw)
  end subroutine deallocate_flux_type

end module radiation_types_module
