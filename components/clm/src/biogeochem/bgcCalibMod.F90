module bgcCalibMod

use fileutils      , only : getfil
use shr_kind_mod   , only : r8 => shr_kind_r8
use decompMod      , only : bounds_type
use shr_log_mod    , only : errMsg => shr_log_errMsg
use ncdio_pio
implicit none

  private
  public :: initCalib
  type, public :: calb_type
  real(r8), pointer :: km_den_no3_calg(:)
  real(r8), pointer :: km_nit_nh4_calg(:)
  real(r8), pointer :: km_decomp_nh4_calg(:)
  real(r8), pointer :: km_decomp_no3_calg(:)
  real(r8), pointer :: km_decomp_p_calg(:)
  real(r8), pointer :: vmax_ptase_calg(:)
  real(r8), pointer :: vmax_nfix_calg(:)
  real(r8), pointer :: km_plant_nh4_calg(:)
  real(r8), pointer :: km_plant_no3_calg(:)
  real(r8), pointer :: km_plant_p_calg(:)
  end type calb_type

  type(calb_type), public :: calb_inst
contains

  subroutine initCalib(bounds)
  use clm_varctl     , only : fsurdat
  use clm_varcon     , only : grlnd
  use abortutils     , only : endrun
  implicit none
  ! !ARGUMENTS:
  type(bounds_type)        , intent(in)    :: bounds
  real(r8),  pointer :: data_in(:)
  type(file_desc_t)     :: ncid              ! netcdf id
  character(len=256)    :: locfn             ! local filename
  logical               :: readvar
  character(len=48) :: tstring

  call AllocCalib(bounds)

  call getfil (fsurdat, locfn, 0)
  call ncd_pio_openfile (ncid, locfn, 0)

  data_in=> calb_inst%km_den_no3_calg; tstring='km_den_no3'
  call ncd_io(ncid=ncid, varname=trim(tstring), flag='read', data=data_in, dim1name=grlnd, readvar=readvar)
  if ( .not. readvar ) call endrun(msg=' ERROR: error in reading in '//trim(tstring)//errMsg(__FILE__, __LINE__))

  data_in=> calb_inst%km_nit_nh4_calg; tstring='km_nit_nh4'
  call ncd_io(ncid=ncid, varname=trim(tstring), flag='read', data=data_in, dim1name=grlnd, readvar=readvar)
  if ( .not. readvar ) call endrun(msg=' ERROR: error in reading in '//trim(tstring)//errMsg(__FILE__, __LINE__))

  data_in=> calb_inst%km_decomp_nh4_calg; tstring='km_decomp_nh4'
  call ncd_io(ncid=ncid, varname=trim(tstring), flag='read', data=data_in, dim1name=grlnd, readvar=readvar)
  if ( .not. readvar ) call endrun(msg=' ERROR: error in reading in '//trim(tstring)//errMsg(__FILE__, __LINE__))

  data_in=> calb_inst%km_decomp_no3_calg; tstring='km_decomp_no3'
  call ncd_io(ncid=ncid, varname=trim(tstring), flag='read', data=data_in, dim1name=grlnd, readvar=readvar)
  if ( .not. readvar)  call endrun(msg=' ERROR: error in reading in '//trim(tstring)//errMsg(__FILE__, __LINE__))

  data_in=> calb_inst%km_decomp_p_calg; tstring='km_decomp_p'
  call ncd_io(ncid=ncid, varname=trim(tstring), flag='read', data=data_in, dim1name=grlnd, readvar=readvar)
  if ( .not. readvar ) call endrun(msg=' ERROR: error in reading in '//trim(tstring)//errMsg(__FILE__, __LINE__))

  data_in=> calb_inst%vmax_ptase_calg; tstring='vmax_ptase'
  call ncd_io(ncid=ncid, varname=trim(tstring), flag='read', data=data_in, dim1name=grlnd, readvar=readvar)
  if ( .not. readvar)  call endrun(msg=' ERROR: error in reading in '//trim(tstring)//errMsg(__FILE__, __LINE__))

  data_in=> calb_inst%vmax_nfix_calg; tstring='vmax_nfix'
  call ncd_io(ncid=ncid, varname=trim(tstring), flag='read', data=data_in, dim1name=grlnd, readvar=readvar)
  if ( .not. readvar ) call endrun(msg=' ERROR: error in reading in '//trim(tstring)//errMsg(__FILE__, __LINE__))

  data_in=> calb_inst%km_plant_nh4_calg; tstring='km_plant_nh4'
  call ncd_io(ncid=ncid, varname=trim(tstring), flag='read', data=data_in, dim1name=grlnd, readvar=readvar)
  if ( .not. readvar ) call endrun(msg=' ERROR: error in reading in '//trim(tstring)//errMsg(__FILE__, __LINE__))

  data_in=> calb_inst%km_plant_no3_calg; tstring='km_plant_no3'
  call ncd_io(ncid=ncid, varname=trim(tstring), flag='read', data=data_in, dim1name=grlnd, readvar=readvar)
  if ( .not. readvar ) call endrun(msg=' ERROR: error in reading in '//trim(tstring)//errMsg(__FILE__, __LINE__))

  data_in=> calb_inst%km_plant_p_calg; tstring='km_plant_p'
  call ncd_io(ncid=ncid, varname=trim(tstring), flag='read', data=data_in, dim1name=grlnd, readvar=readvar)
  if ( .not. readvar ) call endrun(msg=' ERROR: error in reading in '//trim(tstring)//errMsg(__FILE__, __LINE__))

  call ncd_pio_closefile(ncid)

  end subroutine initCalib

!-------------------------------------------------------------------------------

  subroutine AllocCalib(bounds)
  implicit none
  ! !ARGUMENTS:
  type(bounds_type)        , intent(in)    :: bounds
  allocate(calb_inst%km_den_no3_calg(bounds%begg:bounds%endg))
  allocate(calb_inst%km_nit_nh4_calg(bounds%begg:bounds%endg))
  allocate(calb_inst%km_decomp_nh4_calg(bounds%begg:bounds%endg))
  allocate(calb_inst%km_decomp_no3_calg(bounds%begg:bounds%endg))
  allocate(calb_inst%km_decomp_p_calg(bounds%begg:bounds%endg))
  allocate(calb_inst%vmax_ptase_calg(bounds%begg:bounds%endg))
  allocate(calb_inst%vmax_nfix_calg(bounds%begg:bounds%endg))
  allocate(calb_inst%km_plant_nh4_calg(bounds%begg:bounds%endg))
  allocate(calb_inst%km_plant_no3_calg(bounds%begg:bounds%endg))
  allocate(calb_inst%km_plant_p_calg(bounds%begg:bounds%endg))

  end subroutine AllocCalib
end module bgcCalibMod
