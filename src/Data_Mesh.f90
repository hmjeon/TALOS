!
! ---------------------------------------------------------------------------------------
!
!                                   Data_Mesh
!
!                                                                    Updated : 2017/04/29
!
! Comments: This module is to define data sturcutre for element mesh, which
!           is corresponding to basepair.
!
! Script written by Hyungmin Jun (hyungminjun@outlook.com)
! Copyright Hyungmin Jun, 2017. All rights reserved.
!
! ---------------------------------------------------------------------------------------
!
module Data_Mesh

! ---------------------------------------------------------------------------------------

    ! Node(base pair) data type structure
    type :: NodeType
        integer :: id       ! Node ID
        integer :: bp       ! Base pair ID
        integer :: up       ! Upward ID
        integer :: dn       ! Downward ID
        integer :: sec      ! Section ID
        integer :: iniL     ! Initial line ID
        integer :: croL     ! Cross-section line ID

        ! Nodal connectivity at vertex
        ! -1 - no-connection, 1 - neighbor, 2 - self, 3 - modified neighbor, 4 - modified self
        integer :: conn

        ! Ghost node : -1 : normal node, 1 : ghost node to be deleted
        integer :: ghost

        double precision :: pos(3)      ! Position vector
        double precision :: ori(3, 3)   ! Orientation vector
    end type NodeType

    ! ElementType structure
    type :: EleType
        integer :: cn(2)    ! Connectivity
    end type EleType

! ---------------------------------------------------------------------------------------

    ! MeshType data structure
    type :: MeshType
        integer :: n_node = 0   ! The number of nodes
        integer :: n_ele  = 0   ! The number of elements
        integer :: n_mitered    ! The number of mitered nodes

        type(NodeType), allocatable :: node(:)   ! Node array
        type(EleType),  allocatable :: ele(:)    ! Element array
    end type

! ---------------------------------------------------------------------------------------

end module Data_Mesh