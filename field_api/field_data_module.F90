! (C) Copyright 2025- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.


module field_data_module

  use field_module, only: field_2rb, field_3rb
  use field_factory_module
  use parkind1, only: jprb

  implicit none

  type field_data
      ! view access pointers
      real(kind=jprb), pointer, contiguous :: single_level_cos_sza(:)
        ! flux type fields
      real(kind=jprb), pointer, contiguous :: flux_sw(:,:)
      real(kind=jprb), pointer, contiguous :: flux_lw(:,:)

      ! underlying fields handling allocations
      ! single level type fields
      class(field_2rb), pointer :: f_single_level_cos_sza
        ! flux type fields
      class(field_3rb), pointer :: f_flux_sw, f_flux_lw

    contains
      procedure :: init => field_data_init
      procedure :: update_view => field_data_update_view
      procedure :: final => field_data_final
    end type field_data

  contains

    subroutine field_data_init(self, i, j, k)
      class(field_data)    :: self
      integer, intent(in) :: i
      integer, intent(in) :: j
      integer, intent(in) :: k

      call field_new(self%f_single_level_cos_sza, ubounds=[i,j], persistent=.TRUE., init_value=0._jprb)
      call field_new(self%f_flux_sw, ubounds=[i,j,k], persistent=.TRUE., init_value=1._jprb)
      call field_new(self%f_flux_lw, ubounds=[i,j,k], persistent=.TRUE., init_value=2._jprb)
    end subroutine field_data_init

    subroutine field_data_update_view(self, k)
      class(field_data)   :: self
      integer, intent(in) :: k

      if ( associated(self%f_single_level_cos_sza) ) self%single_level_cos_sza => self%f_single_level_cos_sza%get_view(k)
      if ( associated(self%f_flux_sw) ) self%flux_sw => self%f_flux_sw%get_view(k)
      if ( associated(self%f_flux_lw) ) self%flux_lw => self%f_flux_lw%get_view(k)

    end subroutine field_data_update_view

    subroutine field_data_final(self)
      class(field_data)    :: self
      call field_delete(self%f_single_level_cos_sza)
      nullify(self%f_single_level_cos_sza)
      call field_delete(self%f_flux_sw)
      nullify(self%f_flux_sw)
      call field_delete(self%f_flux_lw)
      nullify(self%f_flux_lw)
    end subroutine field_data_final

end module field_data_module
