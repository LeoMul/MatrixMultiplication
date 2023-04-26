program matmul_my 
    use my_library_2
    integer , parameter :: n = 3 , m = 2 , o = 3
    
    real*8 :: x_matrix(n,m),y_matrix(m,o),resmatrix(n,o)
    x_matrix = 1.0d0 
    y_matrix = 2.0d0

    resmatrix = my_matmul_serial(x_matrix,y_matrix)

    call display_matrix(resmatrix)    



end program matmul_my 

module my_library_2
    implicit none 
    contains 

    function my_matmul_serial(x_matrix,y_matrix) result(res_matrix)
        real * 8 , intent(in) :: x_matrix(:,:), y_matrix(:,:) 
        real * 8 , allocatable :: res_matrix(:,:)
        real * 8 :: sum
        integer :: ii,jj,kk 
        integer :: n,m,o,r 
        n = size(x_matrix,1)
        m = size(x_matrix,2)
        o = size(y_matrix,1)
        r = size(y_matrix,2) 

        print*,"beginning", n,m,o,r

        if (m .ne. o ) then 
            allocate(res_matrix(1,1))
            res_matrix = 0.0d0
            print*, "These two matrices cannot be multiplied, check dimensions. Returning zero scalar."
        else 
            print*,"allocating"
            allocate(res_matrix(n,r))
            res_matrix = 0.0d0
            !call display_matrix(res_matrix)
            do  ii = 1,n
                do jj = 1,r 
                    sum = 0.0d0
                    do kk = 1,o 
                        sum = sum + x_matrix(ii,kk) * y_matrix(kk,jj)
                    end do 
                    res_matrix(ii,jj) = sum
                    !print*,ii,jj 
                end do 
            end do 


            end if 
    end function


    subroutine display_matrix(matrix) 
        real * 8 ,intent(in) :: matrix(:,:)
        integer :: n,m ,i,j

        m = size(matrix,1)
        n = size(matrix,2)
        do i = 1,m 
            do j = 1,n 
                write(*,FMT = "(10F5.1)", advance = "no") matrix(i,j)
                
            end do
            print*, ""
        end do 
    end subroutine

end module