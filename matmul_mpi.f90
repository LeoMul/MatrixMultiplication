program matmul_my 
    use my_library
    use mpi
    
    implicit none   

    integer , parameter :: n = 4 , m = 2 , o = 2
    real*8 :: x_matrix(n,m),y_matrix(m,o),resmatrix(n,o),sum

    real * 8, allocatable :: my_block(:,:)
    !mpi stuff
    integer :: ierr 
    integer :: rank ,root
    integer :: nprocs 
    integer :: cols_per_proc
    integer :: my_starting_point,my_ending_point

    integer :: ii,jj,kk 
    
    real * 8 :: t1,t2 

    root = 0 
    call MPI_INIT(ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD,nprocs,ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)

    if (rank .eq. root) then 
        PRINT*, "Currently: each core has a copy of the two matrices and calcualtes & 
        & n/numprocs cols of the resultant matrix. "
    end if 

    call cpu_time(t1)

    
     
    x_matrix = create_example(n,m)
    y_matrix = create_example(m,n)

    
    cols_per_proc = o / nprocs 

    if (rank .eq. root) then 
        print*, "///////////X MATRIX///////////////"
        call display_matrix(x_matrix)
        print*, "///////////Y MATRIX///////////////"
        call display_matrix(y_matrix)
        print *, "setting " , cols_per_proc , "cols per processsor"
    end if 

    my_starting_point = 1 + rank * cols_per_proc
    my_ending_point = my_starting_point + cols_per_proc
    !print*,rank,my_starting_point
    allocate(my_block(cols_per_proc*n,1))
    my_block = 0.0d0 

    do  ii = 1,n
        do jj = 1,cols_per_proc 
            sum = 0.0d0
            do kk = 1,m 
                sum = sum + x_matrix(ii,kk) * y_matrix(kk,jj + rank * cols_per_proc)
            end do 
            my_block((jj-1)*n+ii,1) = sum
        end do 
    end do

    call MPI_GATHER(my_block,cols_per_proc*n,MPI_DOUBLE_PRECISION & 
    ,resmatrix,cols_per_proc*n,MPI_DOUBLE_PRECISION,root,MPI_COMM_WORLD,ierr)
    
    if (rank .eq. root ) then 
        print*, "///////////Z MATRIX///////////////"
        call display_matrix(resmatrix)
        call cpu_time(t2)
        print*, "Time taken: ", t2-t1
    end if 
    call MPI_FINALIZE(ierr)


end program matmul_my 

