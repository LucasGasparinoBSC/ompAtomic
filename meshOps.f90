module meshOps
    implicit none
    contains
        subroutine genMesh(nelem,nnode,table)
            implicit none
            integer(8), intent(in)  :: nelem, nnode
            integer(8), intent(out) :: table(nelem,nnode)
            integer(8) :: ielem, inode, ipoin
            do ielem = 1,nelem
                do inode = 1,2
                    table(ielem,inode) = (ielem-1) + inode
                end do
            end do
            if (nnode .gt. 2) then
                ipoin = (nelem+1)
                do ielem = 1,nelem
                    do inode = 3,nnode
                        ipoin = ipoin + 1
                        table(ielem,inode) = ipoin
                    end do
                end do
            end if
        end subroutine genMesh
end module meshOps