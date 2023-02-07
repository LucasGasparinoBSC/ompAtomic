module elemOps
    implicit none
    contains
        subroutine inicond(npoin,nnode,u,N)
            implicit none
            integer(8), intent(in)  :: npoin, nnode
            real(4)   , intent(out) :: u(npoin), N(nnode,nnode)
            integer(8)              :: ipoin
            !$acc parallel loop
            do ipoin = 1,npoin
                u(ipoin) = real(ipoin,4)/real(npoin,4)
                !u(ipoin) = 1.0
            end do
            !$acc end parallel loop

            !$acc kernels
            N(:,:) = 1.0
            !$acc end kernels
        end subroutine inicond
        subroutine cpuOps(nelem,nnode,npoin,table,N,u,R)
            implicit none
            integer(8), intent(in)  :: nelem, nnode, npoin, table(nelem,nnode)
            real(4)   , intent(in)  :: N(nnode,nnode), u(npoin)
            real(4)   , intent(out) :: R(npoin)
            integer(8)              :: ielem, igaus, inode
            real(4)                 :: aux, Re(nnode)
            R(:) = 0.0
            do ielem = 1,nelem
                Re(:) = 0.0
                do igaus = 1,nnode
                    aux = dot_product(N(igaus,:),u(table(ielem,:)))
                    do inode = 1,nnode
                        Re(inode) = Re(inode) + aux
                    end do
                end do
                do inode = 1,nnode
                    R(table(ielem,inode)) = R(table(ielem,inode)) + Re(inode)
                end do
            end do
        end subroutine cpuOps
        subroutine accOps(nelem,nnode,npoin,table,N,u,R)
            implicit none
            integer(8), intent(in)  :: nelem, nnode, npoin, table(nelem,nnode)
            real(4)   , intent(in)  :: N(nnode,nnode), u(npoin)
            real(4)   , intent(out) :: R(npoin)
            integer(8)              :: ielem, igaus, inode
            real(4)                 :: aux, Re(nnode)
            !$acc kernels
            R(:) = 0.0
            !$acc end kernels
            !$acc parallel loop gang private(Re)
            do ielem = 1,nelem
                Re(:) = 0.0
                !$acc loop seq
                do igaus = 1,nnode
                    aux = dot_product(N(igaus,:),u(table(ielem,:)))
                    !$acc loop vector
                    do inode = 1,nnode
                        Re(inode) = Re(inode) + aux
                    end do
                end do
                !$acc loop vector
                do inode = 1,nnode
                    !$acc atomic update
                    R(table(ielem,inode)) = R(table(ielem,inode)) + Re(inode)
                    !$acc end atomic
                end do
            end do
            !$acc end parallel loop
        end subroutine accOps
        subroutine ompOps(nelem,nnode,npoin,table,N,u,R)
            implicit none
            integer(8), intent(in)  :: nelem, nnode, npoin, table(nelem,nnode)
            real(4)   , intent(in)  :: N(nnode,nnode), u(npoin)
            real(4)   , intent(out) :: R(npoin)
            integer(8)              :: ielem, igaus, inode, ipoin
            real(4)                 :: aux, Re(nnode)
            !$omp target teams distribute parallel do
            do ipoin = 1,npoin
                R(ipoin) = 0.0
            end do
            !$omp end target teams distribute parallel do
            !$omp target teams distribute parallel do private(Re,aux)
            do ielem = 1,nelem
                Re(:) = 0.0
                do igaus = 1,nnode
                    aux = dot_product(N(igaus,:),u(table(ielem,:)))
                    do inode = 1,nnode
                        Re(inode) = Re(inode) + aux
                    end do
                end do
                do inode = 1,nnode
                    R(table(ielem,inode)) = R(table(ielem,inode)) + Re(inode)
                end do
            end do
            !$omp end target teams distribute parallel do
        end subroutine ompOps
end module elemOps