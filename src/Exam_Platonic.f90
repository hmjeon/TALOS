!
! =============================================================================
!
! Module - Exam_Platonic
! Last Updated : 04/10/2018, by Hyungmin Jun (hyungminjun@outlook.com)
!
! =============================================================================
!
! This is part of PERDIX-6P, which allows scientists to build and solve
! the sequence design of complex DNAnanostructures.
! Copyright 2018 Hyungmin Jun. All rights reserved.
!
! License - GPL version 3
! PERDIX-6P is free software: you can redistribute it and/or modify it under
! the terms of the GNU General Public License as published by the Free Software
! Foundation, either version 3 of the License, or any later version.
! PERDIX-6P is distributed in the hope that it will be useful, but WITHOUT
! ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
! FOR A PARTICULAR PURPOSE. See the GNU General Public License
! for more details.
! You should have received a copy of the GNU General Public License along with
! this program. If not, see <http://www.gnu.org/licenses/>.
!
! -----------------------------------------------------------------------------
!
module Exam_Platonic

    use Data_Prob
    use Data_Geom

    use Mani

    implicit none

    public Exam_Platonic_Tetrahedron        ! V=4,  E=6,  F=4
    public Exam_Platonic_Cube               ! V=8,  E=12, F=6
    public Exam_Platonic_Octahedron         ! V=6,  E=12, F=8
    public Exam_Platonic_Dodecahedron       ! V=20, E=30, F=12
    public Exam_Platonic_Icosahedron        ! V=12, E=30, F=20

contains

! -----------------------------------------------------------------------------

! Example of Tetrahedron
subroutine Exam_Platonic_Tetrahedron(prob, geom)
    type(ProbType), intent(inout) :: prob
    type(GeomType), intent(inout) :: geom

    prob.name_prob = "01_Tet"
    call Mani_Set_Problem(prob, [52, 152, 219], "xy")

    ! Preset parameters
    if(para_preset == "on") then
        if(para_vertex_design == "flat") then
            para_junc_ang        = "min"    ! Junctional gap
            para_unpaired_scaf   = "off"    ! Unpaired scaffold nucleotides
            para_n_base_tn       = 7        ! The number of nucleotides

            ! Folding conditions
            para_const_edge_mesh = "on"     ! Constant edge length

            ! 42bp
            !para_set_seq_scaf    = 0        ! Scaffold sequence, 0 - M13mp18, 1 - import sequence, 2 - random
            !para_set_start_scaf  = 4141     ! Starting nucleotide position of scaffold strand
        else if(para_vertex_design == "mitered") then
            para_junc_ang        = "opt"    ! Junctional gap
            para_unpaired_scaf   = "on"     ! Unpaired scaffold nucleotides
            para_n_base_tn       = -1       ! The number of nucleotides

            ! 42bp
            !para_set_seq_scaf    = 1        ! Scaffold sequence, 0 - M13mp18, 1 - import sequence, 2 - random
            !para_set_start_scaf  = 1

            ! More unpaired nt when less 60 degree
            !para_un_depend_angle = "on"
        end if
    end if

    ! The number of points and faces
    geom.n_iniP = 4
    geom.n_face = 4

    allocate(geom.iniP(geom.n_iniP))
    allocate(geom.face(geom.n_face))

    ! Set point position vectors
    geom.iniP(1).pos(1:3) = [  0.00000d0,  0.00000d0,  6.12374d0 ]
    geom.iniP(2).pos(1:3) = [  5.77351d0,  0.00000d0, -2.04125d0 ]
    geom.iniP(3).pos(1:3) = [ -2.88676d0,  5.00000d0, -2.04125d0 ]
    geom.iniP(4).pos(1:3) = [ -2.88676d0, -5.00000d0, -2.04125d0 ]

    ! Set face connnectivity
    geom.face(1).n_poi = 3; allocate(geom.face(1).poi(3)); geom.face(1).poi(1:3) = [ 1, 2, 3 ]
    geom.face(2).n_poi = 3; allocate(geom.face(2).poi(3)); geom.face(2).poi(1:3) = [ 1, 3, 4 ]
    geom.face(3).n_poi = 3; allocate(geom.face(3).poi(3)); geom.face(3).poi(1:3) = [ 1, 4, 2 ]
    geom.face(4).n_poi = 3; allocate(geom.face(4).poi(3)); geom.face(4).poi(1:3) = [ 2, 4, 3 ]

    ! Sakul's tetraheron geometric information
    ! Set point position vectors
    !geom.iniP(1).pos(1:3) = [  0.000000d0,  0.000000d0,  0.612372d0 ]
    !geom.iniP(2).pos(1:3) = [ -0.288675d0, -0.500000d0, -0.204124d0 ]
    !geom.iniP(3).pos(1:3) = [ -0.288675d0,  0.500000d0, -0.204124d0 ]
    !geom.iniP(4).pos(1:3) = [  0.577350d0,  0.000000d0, -0.204124d0 ]

    ! Set face connnectivity
    !geom.face(1).n_poi = 3; allocate(geom.face(1).poi(3)); geom.face(1).poi(1:3) = [ 1, 3, 2 ]
    !geom.face(2).n_poi = 3; allocate(geom.face(2).poi(3)); geom.face(2).poi(1:3) = [ 1, 2, 4 ]
    !geom.face(3).n_poi = 3; allocate(geom.face(3).poi(3)); geom.face(3).poi(1:3) = [ 1, 4, 3 ]
    !geom.face(4).n_poi = 3; allocate(geom.face(4).poi(3)); geom.face(4).poi(1:3) = [ 2, 3, 4 ]
end subroutine Exam_Platonic_Tetrahedron

! -----------------------------------------------------------------------------

! Example of Cube
subroutine Exam_Platonic_Cube(prob, geom)
    type(ProbType), intent(inout) :: prob
    type(GeomType), intent(inout) :: geom

    prob.name_prob = "02_Cube"
    call Mani_Set_Problem(prob, [52, 152, 219], "xyz")

    ! Preset parameters
    if(para_preset == "on") then
        if(para_vertex_design == "flat") then
            para_junc_ang        = "min"    ! Junctional gap
            para_unpaired_scaf   = "off"    ! Unpaired scaffold nucleotides
            para_n_base_tn       = 7        ! The number of nucleotides
        else if(para_vertex_design == "mitered") then
            para_junc_ang        = "opt"    ! Junctional gap
            para_unpaired_scaf   = "on"     ! Unpaired scaffold nucleotides
            para_n_base_tn       = -1       ! The number of nucleotides
        end if
    end if

    ! The number of points and faces
    geom.n_iniP = 8
    geom.n_face = 6

    allocate(geom.iniP(geom.n_iniP))
    allocate(geom.face(geom.n_face))

    ! Set point position vectors
    geom.iniP(1).pos(1:3) = [ -1.0d0, -1.0d0, -1.0d0 ]; geom.iniP(2).pos(1:3) = [  1.0d0, -1.0d0, -1.0d0 ]
    geom.iniP(3).pos(1:3) = [  1.0d0,  1.0d0, -1.0d0 ]; geom.iniP(4).pos(1:3) = [ -1.0d0,  1.0d0, -1.0d0 ]
    geom.iniP(5).pos(1:3) = [ -1.0d0, -1.0d0,  1.0d0 ]; geom.iniP(6).pos(1:3) = [  1.0d0, -1.0d0,  1.0d0 ]
    geom.iniP(7).pos(1:3) = [  1.0d0,  1.0d0,  1.0d0 ]; geom.iniP(8).pos(1:3) = [ -1.0d0,  1.0d0,  1.0d0 ]

    ! Set face connnectivity
    geom.face(1).n_poi = 4; allocate(geom.face(1).poi(4)); geom.face(1).poi(1:4) = [ 1, 4, 3, 2 ]
    geom.face(2).n_poi = 4; allocate(geom.face(2).poi(4)); geom.face(2).poi(1:4) = [ 5, 6, 7, 8 ]
    geom.face(3).n_poi = 4; allocate(geom.face(3).poi(4)); geom.face(3).poi(1:4) = [ 2, 3, 7, 6 ]
    geom.face(4).n_poi = 4; allocate(geom.face(4).poi(4)); geom.face(4).poi(1:4) = [ 1, 5, 8, 4 ]
    geom.face(5).n_poi = 4; allocate(geom.face(5).poi(4)); geom.face(5).poi(1:4) = [ 1, 2, 6, 5 ]
    geom.face(6).n_poi = 4; allocate(geom.face(6).poi(4)); geom.face(6).poi(1:4) = [ 3, 4, 8, 7 ]

    ! Set point position vectors
    !point(1).pos(1:3) = [  0.00000d0,  0.00000d0,  8.66030d0 ]; iniP(2).pos(1:3) = [  8.16497d0,  0.00000d0,  2.88675d0 ]
    !point(3).pos(1:3) = [ -4.08248d0,  7.07107d0,  2.88675d0 ]; iniP(4).pos(1:3) = [ -4.08248d0, -7.07107d0,  2.88675d0 ]
    !point(5).pos(1:3) = [  4.08248d0,  7.07107d0, -2.88675d0 ]; iniP(6).pos(1:3) = [  4.08248d0, -7.07107d0, -2.88675d0 ]
    !point(7).pos(1:3) = [ -8.16497d0,  0.00000d0, -2.88675d0 ]; iniP(8).pos(1:3) = [  0.00000d0,  0.00000d0, -8.66030d0 ]

    ! Set face connnectivity
    !face(1).n_poi = 4; allocate(face(1).poi(4)); face(1).poi(1:4) = [ 1, 2, 5, 3 ]
    !face(2).n_poi = 4; allocate(face(2).poi(4)); face(2).poi(1:4) = [ 1, 3, 7, 4 ]
    !face(3).n_poi = 4; allocate(face(3).poi(4)); face(3).poi(1:4) = [ 1, 4, 6, 2 ]
    !face(4).n_poi = 4; allocate(face(4).poi(4)); face(4).poi(1:4) = [ 2, 6, 8, 5 ]
    !face(5).n_poi = 4; allocate(face(5).poi(4)); face(5).poi(1:4) = [ 3, 5, 8, 7 ]
    !face(6).n_poi = 4; allocate(face(6).poi(4)); face(6).poi(1:4) = [ 4, 7, 8, 6 ]
end subroutine Exam_Platonic_Cube

! -----------------------------------------------------------------------------

! Example of Octahedron
subroutine Exam_Platonic_Octahedron(prob, geom)
    type(ProbType), intent(inout) :: prob
    type(GeomType), intent(inout) :: geom

    prob.name_prob = "03_Octa"
    call Mani_Set_Problem(prob, [52, 152, 219], "xy")

    ! Preset parameters
    if(para_preset == "on") then
        if(para_vertex_design == "flat") then
            para_junc_ang        = "min"    ! Junctional gap
            para_unpaired_scaf   = "off"    ! Unpaired scaffold nucleotides
            para_n_base_tn       = 7        ! The number of nucleotides

            ! Folding conditions
            para_const_edge_mesh = "on"     ! Constant edge length
        else if(para_vertex_design == "mitered") then
            para_junc_ang        = "opt"    ! Junctional gap
            para_unpaired_scaf   = "on"     ! Unpaired scaffold nucleotides
            para_n_base_tn       = -1       ! The number of nucleotides

            ! The mimimum gap between staple crossover and vertex boundary
            para_gap_xover_bound_stap = 8

            ! [new, old], Mitered method
            para_mitered_method = "old"
        end if
    end if

    ! The number of points and faces
    geom.n_iniP = 6
    geom.n_face = 8

    allocate(geom.iniP(geom.n_iniP))
    allocate(geom.face(geom.n_face))

    ! Set point position vectors
    geom.iniP(1).pos(1:3) = [  0.00000d0,  0.00000d0,  7.07107d0 ]
    geom.iniP(2).pos(1:3) = [  7.07107d0,  0.00000d0,  0.00000d0 ]
    geom.iniP(3).pos(1:3) = [  0.00000d0,  7.07107d0,  0.00000d0 ]
    geom.iniP(4).pos(1:3) = [ -7.07107d0,  0.00000d0,  0.00000d0 ]
    geom.iniP(5).pos(1:3) = [  0.00000d0, -7.07107d0,  0.00000d0 ]
    geom.iniP(6).pos(1:3) = [  0.00000d0,  0.00000d0, -7.07107d0 ]

    ! Set face connnectivity
    geom.face(1).n_poi = 3; allocate(geom.face(1).poi(3)); geom.face(1).poi(1:3) = [ 1, 2, 3 ]
    geom.face(2).n_poi = 3; allocate(geom.face(2).poi(3)); geom.face(2).poi(1:3) = [ 1, 3, 4 ]
    geom.face(3).n_poi = 3; allocate(geom.face(3).poi(3)); geom.face(3).poi(1:3) = [ 1, 4, 5 ]
    geom.face(4).n_poi = 3; allocate(geom.face(4).poi(3)); geom.face(4).poi(1:3) = [ 1, 5, 2 ]
    geom.face(5).n_poi = 3; allocate(geom.face(5).poi(3)); geom.face(5).poi(1:3) = [ 2, 5, 6 ]
    geom.face(6).n_poi = 3; allocate(geom.face(6).poi(3)); geom.face(6).poi(1:3) = [ 2, 6, 3 ]
    geom.face(7).n_poi = 3; allocate(geom.face(7).poi(3)); geom.face(7).poi(1:3) = [ 3, 6, 4 ]
    geom.face(8).n_poi = 3; allocate(geom.face(8).poi(3)); geom.face(8).poi(1:3) = [ 4, 6, 5 ]
end subroutine Exam_Platonic_Octahedron

! -----------------------------------------------------------------------------

! Example of Dodecahedron
subroutine Exam_Platonic_Dodecahedron(prob, geom)
    type(ProbType), intent(inout) :: prob
    type(GeomType), intent(inout) :: geom

    prob.name_prob = "04_Dodeca"
    call Mani_Set_Problem(prob, [52, 152, 219], "xy")

    ! Preset parameters
    if(para_preset == "on") then
        if(para_vertex_design == "flat") then
            para_junc_ang        = "min"    ! Junctional gap
            para_unpaired_scaf   = "off"    ! Unpaired scaffold nucleotides
            para_n_base_tn       = 7        ! The number of nucleotides
        else if(para_vertex_design == "mitered") then
            para_junc_ang        = "opt"    ! Junctional gap
            para_unpaired_scaf   = "on"     ! Unpaired scaffold nucleotides
            para_n_base_tn       = -1       ! The number of nucleotides
        end if
    end if

    ! The number of points and faces
    geom.n_iniP = 20
    geom.n_face = 12

    allocate(geom.iniP(geom.n_iniP))
    allocate(geom.face(geom.n_face))

    ! Set point position vectors
    geom.iniP( 1).pos(1:3) = [   0.00000d0,   0.00000d0,  14.01264d0 ]; geom.iniP( 2).pos(1:3) = [   9.34173d0,   0.00000d0,  10.44437d0 ]
    geom.iniP( 3).pos(1:3) = [  -4.67086d0,   8.09018d0,  10.44437d0 ]; geom.iniP( 4).pos(1:3) = [  -4.67086d0,  -8.09018d0,  10.44437d0 ]
    geom.iniP( 5).pos(1:3) = [  10.44437d0,   8.09018d0,   4.67086d0 ]; geom.iniP( 6).pos(1:3) = [  10.44437d0,  -8.09018d0,   4.67086d0 ]
    geom.iniP( 7).pos(1:3) = [ -12.22848d0,   5.00000d0,   4.67086d0 ]; geom.iniP( 8).pos(1:3) = [   1.78411d0,  13.09018d0,   4.67086d0 ]
    geom.iniP( 9).pos(1:3) = [   1.78411d0, -13.09018d0,   4.67086d0 ]; geom.iniP(10).pos(1:3) = [ -12.22848d0,  -5.00000d0,   4.67086d0 ]
    geom.iniP(11).pos(1:3) = [  12.22848d0,   5.00000d0,  -4.67086d0 ]; geom.iniP(12).pos(1:3) = [  12.22848d0,  -5.00000d0,  -4.67086d0 ]
    geom.iniP(13).pos(1:3) = [ -10.44437d0,   8.09018d0,  -4.67086d0 ]; geom.iniP(14).pos(1:3) = [  -1.78411d0,  13.09018d0,  -4.67086d0 ]
    geom.iniP(15).pos(1:3) = [  -1.78411d0, -13.09018d0,  -4.67086d0 ]; geom.iniP(16).pos(1:3) = [ -10.44437d0,  -8.09018d0,  -4.67086d0 ]
    geom.iniP(17).pos(1:3) = [   4.67086d0,   8.09018d0, -10.44437d0 ]; geom.iniP(18).pos(1:3) = [   4.67086d0,  -8.09018d0, -10.44437d0 ]
    geom.iniP(19).pos(1:3) = [  -9.34173d0,   0.00000d0, -10.44437d0 ]; geom.iniP(20).pos(1:3) = [   0.00000d0,   0.00000d0, -14.01264d0 ]

    ! Set face connnectivity
    geom.face(1).n_poi  = 5; allocate(geom.face(1).poi(5));  geom.face( 1).poi(1:5) = [  1,  2,  5,  8,  3 ]
    geom.face(2).n_poi  = 5; allocate(geom.face(2).poi(5));  geom.face( 2).poi(1:5) = [  1,  3,  7, 10,  4 ]
    geom.face(3).n_poi  = 5; allocate(geom.face(3).poi(5));  geom.face( 3).poi(1:5) = [  1,  4,  9,  6,  2 ]
    geom.face(4).n_poi  = 5; allocate(geom.face(4).poi(5));  geom.face( 4).poi(1:5) = [  2,  6, 12, 11,  5 ]
    geom.face(5).n_poi  = 5; allocate(geom.face(5).poi(5));  geom.face( 5).poi(1:5) = [  3,  8, 14, 13,  7 ]
    geom.face(6).n_poi  = 5; allocate(geom.face(6).poi(5));  geom.face( 6).poi(1:5) = [  4, 10, 16, 15,  9 ]
    geom.face(7).n_poi  = 5; allocate(geom.face(7).poi(5));  geom.face( 7).poi(1:5) = [  5, 11, 17, 14,  8 ]
    geom.face(8).n_poi  = 5; allocate(geom.face(8).poi(5));  geom.face( 8).poi(1:5) = [  6,  9, 15, 18, 12 ]
    geom.face(9).n_poi  = 5; allocate(geom.face(9).poi(5));  geom.face( 9).poi(1:5) = [  7, 13, 19, 16, 10 ]
    geom.face(10).n_poi = 5; allocate(geom.face(10).poi(5)); geom.face(10).poi(1:5) = [ 11, 12, 18, 20, 17 ]
    geom.face(11).n_poi = 5; allocate(geom.face(11).poi(5)); geom.face(11).poi(1:5) = [ 13, 14, 17, 20, 19 ]
    geom.face(12).n_poi = 5; allocate(geom.face(12).poi(5)); geom.face(12).poi(1:5) = [ 15, 16, 19, 20, 18 ]
end subroutine Exam_Platonic_Dodecahedron

! -----------------------------------------------------------------------------

! Example of Icosahedron
subroutine Exam_Platonic_Icosahedron(prob, geom)
    type(ProbType), intent(inout) :: prob
    type(GeomType), intent(inout) :: geom

    prob.name_prob = "05_Icosa"
    call Mani_Set_Problem(prob, [52, 152, 219], "xy")

    ! Preset parameters
    if(para_preset == "on") then
        if(para_vertex_design == "flat") then
            para_junc_ang        = "min"    ! Junctional gap
            para_unpaired_scaf   = "off"    ! Unpaired scaffold nucleotides
            para_n_base_tn       = 7        ! The number of nucleotides
        else if(para_vertex_design == "mitered") then
            para_junc_ang        = "opt"    ! Junctional gap
            para_unpaired_scaf   = "on"     ! Unpaired scaffold nucleotides
            para_n_base_tn       = -1       ! The number of nucleotides
        end if
    end if

    ! The number of points and faces
    geom.n_iniP = 12
    geom.n_face = 20

    allocate(geom.iniP(geom.n_iniP))
    allocate(geom.face(geom.n_face))

    ! Set point position vectors
    geom.iniP( 1).pos(1:3) = [  0.00000d0,  0.00000d0,  9.51058d0 ]; geom.iniP( 2).pos(1:3) = [  8.50650d0,  0.00000d0,  4.25326d0 ]
    geom.iniP( 3).pos(1:3) = [  2.62866d0,  8.09018d0,  4.25326d0 ]; geom.iniP( 4).pos(1:3) = [ -6.88192d0,  5.00001d0,  4.25326d0 ]
    geom.iniP( 5).pos(1:3) = [ -6.88192d0, -5.00001d0,  4.25326d0 ]; geom.iniP( 6).pos(1:3) = [  2.62866d0, -8.09018d0,  4.25326d0 ]
    geom.iniP( 7).pos(1:3) = [  6.88192d0,  5.00001d0, -4.25326d0 ]; geom.iniP( 8).pos(1:3) = [  6.88192d0, -5.00001d0, -4.25326d0 ]
    geom.iniP( 9).pos(1:3) = [ -2.62866d0,  8.09018d0, -4.25326d0 ]; geom.iniP(10).pos(1:3) = [ -8.50650d0,  0.00000d0, -4.25326d0 ]
    geom.iniP(11).pos(1:3) = [ -2.62866d0, -8.09018d0, -4.25326d0 ]; geom.iniP(12).pos(1:3) = [  0.00000d0,  0.00000d0, -9.51058d0 ]

    ! Set face connnectivity
    geom.face(1).n_poi  = 3; allocate(geom.face(1).poi(3));  geom.face( 1).poi(1:3) = [  1,  2,  3 ]
    geom.face(2).n_poi  = 3; allocate(geom.face(2).poi(3));  geom.face( 2).poi(1:3) = [  1,  3,  4 ]
    geom.face(3).n_poi  = 3; allocate(geom.face(3).poi(3));  geom.face( 3).poi(1:3) = [  1,  4,  5 ]
    geom.face(4).n_poi  = 3; allocate(geom.face(4).poi(3));  geom.face( 4).poi(1:3) = [  1,  5,  6 ]
    geom.face(5).n_poi  = 3; allocate(geom.face(5).poi(3));  geom.face( 5).poi(1:3) = [  1,  6,  2 ]
    geom.face(6).n_poi  = 3; allocate(geom.face(6).poi(3));  geom.face( 6).poi(1:3) = [  2,  6,  8 ]
    geom.face(7).n_poi  = 3; allocate(geom.face(7).poi(3));  geom.face( 7).poi(1:3) = [  2,  8,  7 ]
    geom.face(8).n_poi  = 3; allocate(geom.face(8).poi(3));  geom.face( 8).poi(1:3) = [  2,  7,  3 ]
    geom.face(9).n_poi  = 3; allocate(geom.face(9).poi(3));  geom.face( 9).poi(1:3) = [  3,  7,  9 ]
    geom.face(10).n_poi = 3; allocate(geom.face(10).poi(3)); geom.face(10).poi(1:3) = [  3,  9,  4 ]
    geom.face(11).n_poi = 3; allocate(geom.face(11).poi(3)); geom.face(11).poi(1:3) = [  4,  9, 10 ]
    geom.face(12).n_poi = 3; allocate(geom.face(12).poi(3)); geom.face(12).poi(1:3) = [  4, 10,  5 ]
    geom.face(13).n_poi = 3; allocate(geom.face(13).poi(3)); geom.face(13).poi(1:3) = [  5, 10, 11 ]
    geom.face(14).n_poi = 3; allocate(geom.face(14).poi(3)); geom.face(14).poi(1:3) = [  5, 11,  6 ]
    geom.face(15).n_poi = 3; allocate(geom.face(15).poi(3)); geom.face(15).poi(1:3) = [  6, 11,  8 ]
    geom.face(16).n_poi = 3; allocate(geom.face(16).poi(3)); geom.face(16).poi(1:3) = [  7,  8, 12 ]
    geom.face(17).n_poi = 3; allocate(geom.face(17).poi(3)); geom.face(17).poi(1:3) = [  7, 12,  9 ]
    geom.face(18).n_poi = 3; allocate(geom.face(18).poi(3)); geom.face(18).poi(1:3) = [  8, 11, 12 ]
    geom.face(19).n_poi = 3; allocate(geom.face(19).poi(3)); geom.face(19).poi(1:3) = [  9, 12, 10 ]
    geom.face(20).n_poi = 3; allocate(geom.face(20).poi(3)); geom.face(20).poi(1:3) = [ 10, 12, 11 ]
end subroutine Exam_Platonic_Icosahedron

! -----------------------------------------------------------------------------

end module Exam_Platonic