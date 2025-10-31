! (C) Copyright 2025- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.


module radiation_field_type_module

  use field_module, only: field_2rb, field_3rb
  use field_factory_module
  use radiation_types_module, only: single_level_type, flux_type
  use parkind1, only: jprb

  implicit none

  type single_level_field_type
      ! view access pointers
      real(kind=jprb), pointer, contiguous :: single_level_cos_sza(:)
      ! field holding dat
      class(field_2rb), pointer :: f_single_level_cos_sza
      logical :: on_gpu = .false.
    contains
      procedure :: init => single_level_field_init
      procedure :: final => single_level_field_final
      procedure :: update_view => single_level_field_update_view
      procedure :: associate_pointers => single_level_field_associate_pointers
      procedure :: attach => single_level_field_attach
      procedure :: detach => single_level_field_detach
    end type single_level_field_type


  type flux_field_type
      ! view access pointers
      real(kind=jprb), pointer, contiguous :: flux_sw(:,:)
      real(kind=jprb), pointer, contiguous :: flux_lw(:,:)
        ! flux type fields
      class(field_3rb), pointer :: f_flux_sw, f_flux_lw
    contains
      procedure :: init => flux_field_init
      procedure :: final => flux_field_final
      procedure :: update_view => flux_field_update_view
      procedure :: attach => flux_field_attach
      procedure :: detach => flux_field_detach
    end type flux_field_type

  contains


    subroutine single_level_field_init(self, i, k, on_gpu)
      class(single_level_field_type)    :: self
      integer, intent(in) :: i
      integer, intent(in) :: k
      logical, optional, intent(in) :: on_gpu

      if (present(on_gpu)) then
        self%on_gpu = on_gpu
      else
        self%on_gpu = .false.
      end if

      call field_new(self%f_single_level_cos_sza, ubounds=[i,k], persistent=.TRUE., init_value=0._jprb)

      if (self%on_gpu) then
        call self%f_single_level_cos_sza%sync_device_wronly()
      end if
    end subroutine single_level_field_init

    subroutine single_level_field_final(self)
      class(single_level_field_type)    :: self
      call field_delete(self%f_single_level_cos_sza)
      nullify(self%f_single_level_cos_sza)
    end subroutine single_level_field_final

    subroutine single_level_field_update_view(self, k)
      class(single_level_field_type)   :: self
      integer, intent(in) :: k

      if ( associated(self%f_single_level_cos_sza) ) self%single_level_cos_sza => self%f_single_level_cos_sza%get_view(k)
    end subroutine single_level_field_update_view

    subroutine single_level_field_associate_pointers(self, single_level, k)
    !$acc routine seq
      class(single_level_field_type)    :: self
      class(single_level_type)          :: single_level
      integer, intent(in)               :: k

      if ( associated(self%f_single_level_cos_sza) ) then
        single_level%cos_sza => self%f_single_level_cos_sza%devptr(:,k)
      end if
    end subroutine single_level_field_update_view

    subroutine single_level_field_attach(self)
      class(single_level_field_type)    :: self

      !$acc enter data copyin(self)
      !$acc enter data attach(self%f_single_level_cos_sza)
      !$acc enter data attach(self%f_single_level_cos_sza%devptr)

    end subroutine single_level_field_attach()

    subroutine single_level_field_detach(self)
      class(single_level_field_type)    :: self

      !$acc exit data detach(self%f_single_level_cos_sza%devptr)
      !$acc exit data detach(self%f_single_level_cos_sza)
      !$acc exit data delete(self)

    end subroutine single_field_detach()


    subroutine flux_field_init(self, i, j, k)
      class(flux_field_type)    :: self
      integer, intent(in) :: i
      integer, intent(in) :: j
      integer, intent(in) :: k

      call field_new(self%f_flux_sw, ubounds=[i,j,k], persistent=.TRUE., init_value=1._jprb)
      call field_new(self%f_flux_lw, ubounds=[i,j,k], persistent=.TRUE., init_value=2._jprb)
    end subroutine flux_field_init

    subroutine flux_field_final(self)
      class(flux_field_type)    :: self
      call field_delete(self%f_single_level_cos_sza)
      nullify(self%f_single_level_cos_sza)
      call field_delete(self%f_flux_sw)
      nullify(self%f_flux_sw)
      call field_delete(self%f_flux_lw)
      nullify(self%f_flux_lw)
    end subroutine flux_field_final


    subroutine flux_field_update_view(self, k)
      class(flux_field_type)   :: self
      integer, intent(in) :: k

      if ( associated(self%f_flux_sw) ) self%flux_sw => self%f_flux_sw%get_view(k)
      if ( associated(self%f_flux_lw) ) self%flux_lw => self%f_flux_lw%get_view(k)

    end subroutine flux_field_update_view

    subroutine flux_field_associate_pointers(self, flux, k)
    !$acc routine seq
      class(flux_field_type)          :: self
      class(flux_type), intent(inout) :: flux
      integer, intent(in)             :: k

      if ( associated(self%f_flux_sw) ) then
        flux%sw => self%f_flux_sw%devptr(:,:,k)
      end if
      if ( associated(self%f_flux_lw) ) then
        flux%lw => self%f_flux_lw%devptr(:,:,k)
      end if
    end subroutine flux_field_associate_pointers

    subroutine flux_field_attach(self)
      class(flux_field_type)   :: self
      
      !$acc enter data create(self)

      if ( associated(self%f_flux_sw) ) then
        !$acc enter data attach(f%f_flux_sw)
        !$acc enter data attach(f%f_flux_sw%devptr)
      end if
      if ( associated(self%f_flux_lw) ) then
        !$acc enter data_attach(self%f_flux_lw)
        !$acc enter data attach(f%f_flux_sw%devptr)
      end if

    end subroutine flux_field_attach

    subroutine flux_field_detach(self)
      class(flux_field_type)   :: self
      

      if ( associated(self%f_flux_sw) ) then
        !$acc exit data detach(f%f_flux_sw%devptr)
        !$acc exit data detach(f%f_flux_sw)
      end if
      if ( associated(self%f_flux_lw) ) then
        !$acc exit data detach(f%f_flux_sw%devptr)
        !$acc exit data_detach(self%f_flux_lw)
      end if

      !$acc exit data delete(self)

    end subroutine flux_field_attach



end module radiation_field_type_module
