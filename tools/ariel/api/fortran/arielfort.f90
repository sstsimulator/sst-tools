!-----------------------------------------------------------------------
!
! MODULE: arielfort
!> @brief
!> This module defines the Ariel library interface for fortran
!> These calls are replaced by the Ariel pintool
!
!-----------------------------------------------------------------------
MODULE arielfort
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: ariel_enable, ariel_output_stats
  CONTAINS
  SUBROUTINE ariel_enable
    WRITE(*,*) 'ARIEL: enabling simulation control'
  END SUBROUTINE ariel_enable
  SUBROUTINE ariel_output_stats
! WRITE(*,*) 'ARIEL: printing statistics'
  END SUBROUTINE ariel_output_stats
END MODULE arielfort
