# (C) Copyright 2025- ECMWF.
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

cmake_minimum_required(VERSION 3.20.2)
project(gpu_offload_tests VERSION 1.0 LANGUAGES Fortran)

# include(CTest)
find_package(OpenACC COMPONENTS Fortran REQUIRED)


# temp work around?
find_package(fiat REQUIRED)
find_package(field_api REQUIRED)
find_package(OpenMP COMPONENTS Fortran REQUIRED)

add_subdirectory(hoisted_scheme)
add_subdirectory(hoisted_ii_scheme)
