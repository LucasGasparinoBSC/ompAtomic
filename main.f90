program main
    use meshOps
    use elemOps
    implicit none
    integer(8)              :: nelem, nnode, npoin
    integer(8)              :: ielem, inode, ipoin
    integer(8), allocatable :: table(:,:)
    real(4)   , allocatable :: u(:), R(:), R_ACC(:), R_OMP(:), N(:,:)

    nelem = 10
    nnode = 64
    npoin = (nelem+1) + nelem*(nnode-2)

    allocate(table(nelem,nnode))
    call genMesh(nelem, nnode, table)

    allocate(u(npoin), R(npoin), R_ACC(npoin), R_OMP(npoin))
    allocate(N(nnode,nnode))
    call inicond(npoin, nnode, u, N)

    call cpuOps(nelem, nnode, npoin, table, N, u, R)
    call accOps(nelem, nnode, npoin, table, N, u, R_ACC)
    call ompOps(nelem, nnode, npoin, table, N, u, R_OMP)

    do ipoin = 1,npoin
        print*, ipoin, R(ipoin), R_ACC(ipoin), R_OMP(ipoin)
    end do

end program main