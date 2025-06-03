# ecRAD GPU offload examples
A small toy example showing a suggested GPU offload strategy for ecRAD.

This repository contains 3 small programs written to highlight a possible refactor of the
IFS layer of ecRAD for GPU offload.

* [cpu_scheme](cpu_scheme) A cpu variant where field_api is used to store the data and per-block
view pointers are passed to the ``scheme``.

* [hoisted_ii_scheme](hoisted_ii_scheme) A GPU variant that uses hoisting
(i.e. allocations made by ecRAD derived types are moved up from the ``radiation_scheme`` to
the ``radiation_scheme_layer``) and the derived types are passed to the ``radiation_scheme``
instead of arrays/pointers. Loki can be used to generate this code from the cpu code in
the [cpu_scheme](cpu_scheme).

* [hoisted_scheme](hoisted_scheme) A variant that implements a "naive hoisting" where pointers
are passed and each gpu thread must retrieve the correct pointer **(not performant and not  a
reasonable implementation of a GPU variant)**.


## Build Instructions

### Requirements
The examples require the following libraries to be built:
* [field_api]()
* [fiat]()
* OpenMP
* OpenACC

All examples have been tested on ECMWF's ATOS supercomputer with the NVHPC compiler version 24.5
on the GPU nodes with NVIDIA A100 GPUs.

### CMake Build Instructions


Configure the build
```
cmake -B [build-dir] -S [ecrad-offload-examples] -Dfield_api_DIR=[field-api-build-dir] -Dfiat_ROOT=[fiat-build-dir]
```

```
cmake --build [build-dir]
```

### Running on Atos
The following srun invocation can be used to run the compiled binaries on ATOS GPU nodes with
output recording OpenACC data transfers and kernel launches.


**cpu_scheme:**
```
NVCOMPILER_ACC_NOTIFY=8 srun --nodes=1  --gpus-per-task=1 -q dg cpu_scheme/cpu_scheme 
```


**hoisted_ii_scheme:**
```
NVCOMPILER_ACC_NOTIFY=8 srun --nodes=1  --gpus-per-task=1 -q dg hoisted_ii_scheme/hoisted_ii_scheme 
```


**hoisted_scheme:**
```
NVCOMPILER_ACC_NOTIFY=8 srun --nodes=1  --gpus-per-task=1 -q dg hoisted_scheme/hoisted_scheme 
```
