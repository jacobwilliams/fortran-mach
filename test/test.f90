

program test

    use mach_routines
    use iso_fortran_env

    implicit none

    integer :: i !! counter

    do i = 1, 16
        write(output_unit,'(A,I2,A,I16)') 'i1mach(',i,') = ', i1mach(i)
    end do
    write(*,*) ''
    do i = 1, 5
        write (output_unit, '(1P,A,I2,A,E27.8E3,1X,Z32)') 'r1mach(',i,') = ', r1mach(i), r1mach(i)
    end do
    write(*,*) ''
    do i = 1, 5
        write (output_unit, '(1P,A,I2,A,E27.17E4,1X,Z32)') 'd1mach(',i,') = ', d1mach(i), d1mach(i)
    end do

end program test