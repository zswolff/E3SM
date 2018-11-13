module bgcCalibMod

use fileutils      , only : getfil
use ncdio_pio
implicit none

  private
  public :: initCalib

  real(r8), pointer :: vmax_plant_p_calg(:)
contains

  subroutine initCalib(bounds)
  use clm_varctl     , only : fsurdat
  use clm_varcon     , only : grlnd
  use abortutils     , only : endrun
  implicit none
  real(r8),  pointer :: data_in(:)
  type(file_desc_t)     :: ncid              ! netcdf id
  character(len=256)    :: locfn             ! local filename
  logical               :: readvar

  call getfil (fsurdat, locfn, 0)
  call ncd_pio_openfile (ncid, locfn, 0)


  data_in=> vmax_plant_p_calg
  call ncd_io(ncid=ncid, varname='vmax_plant_p', flag='read', data=data_in, dim1name=grlnd, readvar=readvar)
  if ( .not. readv ) call endrun(msg=' ERROR: error in reading in vmax_plant_p'//errMsg(__FILE__, __LINE__))

  call ncd_pio_closefile(ncid)

  end subroutine initCalib

!-------------------------------------------------------------------------------

  subroutine AllocCalib(bounds)
  implicit none
  allocate(vmax_plant_p_calg(bounds%begg:bounds%endg))

  end subroutine AllocCalib
end module bgcCalibMod
