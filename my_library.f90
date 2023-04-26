module my_library 
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
                write(*,FMT = "(10F10.2)", advance = "no") matrix(i,j)
                write (*,FMT = "(A1)",advance = "no") " "
            end do
            print*, ""
        end do 
    end subroutine

    function create_identity(n) result(identity)
        integer :: n 
        real * 8 , allocatable :: identity(:,:)
        integer :: ii
        allocate(identity(n,n))
        identity = 0.0d0
        do ii = 1,n 
            identity(ii,ii) = 1.0d0
        end do 
    end function

    function create_example(n,m) result(example)
        integer :: n,m 
        real * 8 , allocatable :: example(:,:)
        integer :: ii,jj
        allocate(example(n,m))
        do ii = 1,n 
            do jj = 1,m 
                example(ii,jj) = real((ii-1)*n + jj)
            end do 
        end do 

    end function


end module