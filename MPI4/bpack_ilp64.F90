#ifdef MPI4
! Application integers stay unchanged; only the numerical-library ABI is ILP64.
module bpack_ilp64
   use iso_fortran_env, only: int64
   implicit none
   private :: int64, bpack_int

   interface blacs_gridinfo
      module procedure bpack_blacs_gridinfo
   end interface
   interface blacs_gridmap
      module procedure bpack_blacs_gridmap
   end interface
   interface blacs_gridexit
      module procedure bpack_blacs_gridexit
   end interface
   interface blacs_exit
      module procedure bpack_blacs_exit
   end interface
   interface sys2blacs_handle
      module procedure bpack_sys2blacs_handle
   end interface
   interface descinit
      module procedure bpack_descinit
   end interface
   interface descset
      module procedure bpack_descset
   end interface
   interface numroc
      module procedure bpack_numroc
   end interface
   interface pb_topget
      module procedure bpack_pb_topget
   end interface
   interface pb_topset
      module procedure bpack_pb_topset
   end interface

#include "s_lapack_interfaces.fi"
#include "d_lapack_interfaces.fi"
#include "c_lapack_interfaces.fi"
#include "z_lapack_interfaces.fi"
#include "s_scalapack_interfaces.fi"
#include "d_scalapack_interfaces.fi"
#include "c_scalapack_interfaces.fi"
#include "z_scalapack_interfaces.fi"

contains

   impure elemental integer function bpack_int(value) result(narrow)
      integer(int64), intent(in) :: value
      if (value > int(huge(narrow), int64) .or. &
          value < -int(huge(narrow), int64) - 1_int64) then
         error stop 'ILP64 numerical-library result exceeds ButterflyPACK integer range'
      endif
      narrow = int(value)
   end function bpack_int

   subroutine bpack_blacs_gridinfo(ctxt, nprow, npcol, myrow, mycol)
      integer, intent(in) :: ctxt
      integer, intent(out) :: nprow, npcol, myrow, mycol
      integer(int64) :: nprow8, npcol8, myrow8, mycol8
      external :: blacs_gridinfo
      call blacs_gridinfo(int(ctxt,int64), nprow8, npcol8, myrow8, mycol8)
      nprow = bpack_int(nprow8)
      npcol = bpack_int(npcol8)
      myrow = bpack_int(myrow8)
      mycol = bpack_int(mycol8)
   end subroutine bpack_blacs_gridinfo

   subroutine bpack_blacs_gridmap(ctxt, pmap, ldu, nprow, npcol)
      integer, intent(inout) :: ctxt
      integer, intent(in) :: ldu, nprow, npcol, pmap(ldu,*)
      integer(int64) :: ctxt8
      external :: blacs_gridmap
      ctxt8 = int(ctxt,int64)
      call blacs_gridmap(ctxt8, int(pmap(1:ldu,1:npcol),int64), &
                        int(ldu,int64), int(nprow,int64), int(npcol,int64))
      ctxt = bpack_int(ctxt8)
   end subroutine bpack_blacs_gridmap

   subroutine bpack_blacs_gridexit(ctxt)
      integer, intent(in) :: ctxt
      external :: blacs_gridexit
      call blacs_gridexit(int(ctxt,int64))
   end subroutine bpack_blacs_gridexit

   subroutine bpack_blacs_exit(flag)
      integer, intent(in) :: flag
      external :: blacs_exit
      call blacs_exit(int(flag,int64))
   end subroutine bpack_blacs_exit

   integer function bpack_sys2blacs_handle(comm) result(ctxt)
      integer, intent(in) :: comm
      integer(int64), external :: sys2blacs_handle
      ctxt = bpack_int(sys2blacs_handle(int(comm,int64)))
   end function bpack_sys2blacs_handle

   subroutine bpack_descinit(desc, m, n, mb, nb, irsrc, icsrc, ctxt, lld, info)
      integer, intent(out) :: desc(9), info
      integer, intent(in) :: m, n, mb, nb, irsrc, icsrc, ctxt, lld
      integer(int64) :: desc8(9), info8
      external :: descinit
      call descinit(desc8, int(m,int64), int(n,int64), int(mb,int64), &
                    int(nb,int64), int(irsrc,int64), int(icsrc,int64), &
                    int(ctxt,int64), int(lld,int64), info8)
      info = bpack_int(info8)
      desc = bpack_int(desc8)
   end subroutine bpack_descinit

   subroutine bpack_descset(desc, m, n, mb, nb, irsrc, icsrc, ctxt, lld)
      integer, intent(out) :: desc(9)
      integer, intent(in) :: m, n, mb, nb, irsrc, icsrc, ctxt, lld
      integer(int64) :: desc8(9)
      external :: descset
      call descset(desc8, int(m,int64), int(n,int64), int(mb,int64), &
                   int(nb,int64), int(irsrc,int64), int(icsrc,int64), &
                   int(ctxt,int64), int(lld,int64))
      desc = bpack_int(desc8)
   end subroutine bpack_descset

   integer function bpack_numroc(n, nb, iproc, isrcproc, nprocs) result(nlocal)
      integer, intent(in) :: n, nb, iproc, isrcproc, nprocs
      integer(int64), external :: numroc
      nlocal = bpack_int(numroc(int(n,int64), int(nb,int64), int(iproc,int64), &
                              int(isrcproc,int64), int(nprocs,int64)))
   end function bpack_numroc

   subroutine bpack_pb_topget(ctxt, operation, scope, topology)
      integer, intent(in) :: ctxt
      character(*), intent(in) :: operation, scope
      character(*), intent(out) :: topology
      external :: pb_topget
      call pb_topget(int(ctxt,int64), operation, scope, topology)
   end subroutine bpack_pb_topget

   subroutine bpack_pb_topset(ctxt, operation, scope, topology)
      integer, intent(in) :: ctxt
      character(*), intent(in) :: operation, scope, topology
      external :: pb_topset
      call pb_topset(int(ctxt,int64), operation, scope, topology)
   end subroutine bpack_pb_topset

#include "s_lapack_wrappers.fi"
#include "d_lapack_wrappers.fi"
#include "c_lapack_wrappers.fi"
#include "z_lapack_wrappers.fi"
#include "s_scalapack_wrappers.fi"
#include "d_scalapack_wrappers.fi"
#include "c_scalapack_wrappers.fi"
#include "z_scalapack_wrappers.fi"

end module bpack_ilp64
#endif
