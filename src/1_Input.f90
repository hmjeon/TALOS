!
! =============================================================================
!
! Module - Input
! Last Updated : 07/28/2018, by Hyungmin Jun (hyungminjun@outlook.com)
!
! =============================================================================
!
! This is part of TALOS, which allows scientists to build and solve
! the sequence design of complex DNAnanostructures.
! Copyright 2018 Hyungmin Jun. All rights reserved.
!
! License - GPL version 3
! TALOS is free software: you can redistribute it and/or modify it under
! the terms of the GNU General Public License as published by the Free Software
! Foundation, either version 3 of the License, or any later version.
! TALOS is distributed in the hope that it will be useful, but WITHOUT
! ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
! FOR A PARTICULAR PURPOSE. See the GNU General Public License
! for more details.
! You should have received a copy of the GNU General Public License along with
! this program. If not, see <http://www.gnu.org/licenses/>.
!
! -----------------------------------------------------------------------------
!
module Input

    use Importer

    use Exam_Platonic
    use Exam_Archi
    use Exam_Catalan
    use Exam_Johnson
    use Exam_Miscel

    use Section

    use Para
    use Math
    use List

    implicit none

    public  Input_Initialize

    private Input_Print_Parameters
    private Input_Read_Parameter
    private Input_Set_Command
    private Input_Print_Problem
    private Input_Print_Section
    private Input_Print_Num_BP_Edge
    private Input_Print_Vertex_Design
    private Input_Set_Problem
    private Input_Set_Vertex_Design
    private Input_Select_Problem
    private Input_Select_File
    private Input_Set_Section
    private Input_Find_Max_Min_Section
    private Input_Set_Section_Connectivity
    private Input_Set_Num_BP_Edge
    private Input_Convert_Face_To_Line
    private Input_Scale_Init_Geometry
    private Input_Set_Path
    private Input_Set_Workplace
    private Input_Write_GEO_File
    private Input_Write_PLY_File
    private Input_Chimera_Init_Geometry
    private Input_Tecplot_Init_Geometry
    private Input_Generate_Schlegel_Diagram

contains

! -----------------------------------------------------------------------------

! Initialize parameters and inputs
subroutine Input_Initialize(prob, geom)
    type(ProbType), intent(inout) :: prob
    type(GeomType), intent(inout) :: geom

    integer :: arg, i, j, n_section, n_vertex, n_edge_len, len_char, ppos
    character(10) :: c_sec, c_edge_len, c_vertex
    character(100) :: c_prob
    logical :: results

    ! Read parameters from env.dat
    call Input_Read_Parameter

    ! Set command environment
    call Input_Set_Command

    if(iargc() == 0) then

        ! --------------------------------------------------
        ! Running from Win32 console interface menu
        ! --------------------------------------------------
        ! Print pre-defined problems
        call Input_Print_Problem
        read(*, *), c_prob

        ppos = scan(trim(c_prob), ".", BACK = .true.)
        if(ppos > 0) then
            prob.sel_prob  = 0
            len_char       = len_trim(c_prob)
            prob.name_file = trim(adjustl(c_prob(1:ppos-1)))
            prob.type_file = trim(adjustl(c_prob(ppos+1:len_char)))
        else
            read(c_prob, *), prob.sel_prob

            ! The negative value terminate the program
            if(prob.sel_prob <= 0) stop
        end if

        ! Clean the screen
        results = SYSTEMQQ("cls")

        ! Print vertex design options
        call Input_Print_Vertex_Design()
        read(*, *), prob.sel_vertex

        ! The negative value terminate the program
        if(prob.sel_vertex < 1 .or. prob.sel_vertex > 2) stop

        ! Print pre-defined cross-sections
        call Input_Print_Section
        read(*, *) prob.sel_sec

        ! The negative value terminate the program
        if(prob.sel_sec < 1 .or. prob.sel_sec > 2) stop

        ! Print pre-defined edge length(bps)
        call Input_Print_Num_BP_Edge(prob)
        read(*, *) prob.sel_bp_edge

        ! The negative value terminate the program
        if(prob.sel_bp_edge < 1) stop
    else

        ! --------------------------------------------------
        ! Running from a command shell with options
        ! --------------------------------------------------
        arg = 1; call getarg(arg, c_prob)       ! 1st argument, problem
        arg = 2; call getarg(arg, c_vertex)     ! 2nd argument, vertex design
        arg = 3; call getarg(arg, c_sec)        ! 3rd argument, section
        arg = 4; call getarg(arg, c_edge_len)   ! 4th argument, edge length

        ppos = scan(trim(c_prob), ".", BACK = .true.)
        if(ppos > 0) then
            prob.sel_prob  = 0
            len_char       = len_trim(c_prob)
            prob.name_file = trim(adjustl(c_prob(1:ppos-1)))
            prob.type_file = trim(adjustl(c_prob(ppos+1:len_char)))
        else
            read(c_prob, *), prob.sel_prob

            ! The negative value terminate the program
            if(prob.sel_prob <= 0) stop
        end if

        read(c_sec,      *), n_section
        read(c_edge_len, *), n_edge_len
        read(c_vertex,   *), n_vertex

        ! Set inputs for geometry, section, edge length, vertex design, stap-break rule
        ! Zero means that it will take arbitrary inputs
        prob.sel_sec     = n_section
        prob.sel_bp_edge = n_edge_len
        prob.sel_vertex  = n_vertex
    end if

    ! --------------------------------------------------
    ! Set problem, cross-section, edge length and vertex design
    ! --------------------------------------------------
    ! Set vertex design
    call Input_Set_Vertex_Design(prob)

    ! Set cross-section
    call Input_Set_Section(prob, geom)

    ! Set the minimum edge length
    call Input_Set_Num_BP_Edge(prob, geom)

    ! Set problem
    call Input_Set_Problem(prob, geom)

    ! --------------------------------------------------
    ! Prepair geometry - line generation and scaling
    ! --------------------------------------------------
    ! Convert surface to line connectivity
    call Input_Convert_Face_To_Line(geom)

    ! Set geometric scale with initial minimum length
    call Input_Scale_Init_Geometry(geom)

    ! --------------------------------------------------
    ! Set environment and write initial geometry
    ! --------------------------------------------------
    ! Set working and Chimera path
    call Input_Set_Path(prob)

    ! Remove previous working directory and make new one
    call Input_Set_Workplace(prob)

    ! Write *.geo file
    call Input_Write_GEO_File(prob, geom)

    ! Write *.ply file
    call Input_Write_PLY_File(prob, geom)

    ! Write initial geometry
    call Input_Chimera_Init_Geometry(prob, geom)

    ! Write initial geometry for Tecplot
    call Input_Tecplot_Init_Geometry(prob, geom)

    ! Generate Schlegel diagram
    call Input_Generate_Schlegel_Diagram(prob, geom)

    ! Print progress
    call Input_Print_Parameters(prob, geom)
end subroutine Input_Initialize

! -----------------------------------------------------------------------------

! Print progress
subroutine Input_Print_Parameters(prob, geom)
    type(ProbType), intent(inout) :: prob
    type(GeomType), intent(inout) :: geom

    integer :: i

    ! Open output progress file (unit 11 is used for global output file)
    open(unit=11, file=trim(prob.path_work)//"/"//"TXT_TALOS.txt", form="formatted")

    do i = 0, 11, 11
        write(i, "(a )"), "   +--------------------------------------------------------------------+"
        write(i, "(a )"), "   |          1. Inputs - geometry, cross-section, edge length          |"
        write(i, "(a )"), "   +--------------------------------------------------------------------+"
        write(i, "(a )")
        call Space(i, 6)
        write(i, "(a)"), "1.1. Geometry"
        call Space(i, 11)
        write(i, "(a)"), "* Geometric name                    : "//trim(prob.name_prob)
        call Space(i, 11)
        write(i, "(a)"), "* Geometric file type               : "//trim(prob.type_file)
        call Space(i, 11)
        write(i, "(a)"), "* The number of faces               : "//trim(adjustl(Int2Str(geom.n_face)))
        call Space(i, 11)
        write(i, "(a)"), "* The number of points              : "//trim(adjustl(Int2Str(geom.n_iniP)))
        call Space(i, 11)
        write(i, "(a)"), "* The number of edges               : "//trim(adjustl(Int2Str(geom.n_iniL)))
        write(i, "(a)")

        call Space(i, 6)
        write(i, "(a)"), "1.2. Cross-section information"
        call Space(i, 11)
        write(i, "(a)"), "* The number of duplexes            : "//trim(adjustl(Int2Str(geom.n_sec)))
        call Space(i, 11)
        write(i, "(a)"), "* The number of rows                : "//trim(adjustl(Int2Str(geom.sec.maxR-geom.sec.minR+1)))
        call Space(i, 11)
        write(i, "(a)"), "* The number of columns             : "//trim(adjustl(Int2Str(geom.sec.maxC-geom.sec.minC+1)))
        call Space(i, 11)
        write(i, "(a)"), "* Reference row                     : "//trim(adjustl(Int2Str(geom.sec.ref_row)))
        call Space(i, 11)
        write(i, "(a)"), "* Reference min/max column          : "&
            //trim(adjustl(Int2Str(geom.sec.ref_minC)))//" / "//trim(adjustl(Int2Str(geom.sec.ref_maxC)))
        write(i, "(a)")

        call Space(i, 6)
        write(i, "(a)"), "1.3. Edge length"
        call Space(i, 11)
        write(i, "(a)"), "* The minimum edge length           : "//trim(adjustl(Int2Str(prob.n_bp_edge)))
        write(i, "(a)")
        call Space(i, 6)

        write(i, "(a)"), "1.4. Design parameters"
        call Space(i, 11)
        write(i, "(a)"), "* Junction modification             : "//trim(para_junc_ang)
        call Space(i, 11)
        write(i, "(a)"), "* Vertex design                     : "//trim(para_vertex_design)//" vertex"
        call Space(i, 11)
        write(i, "(a)"), "* Vertex modification               : "//trim(para_vertex_modify)
        call Space(i, 11)
        write(i, "(a)"), "* Sticky-end for self connection    : "//trim(para_sticky_self)
        call Space(i, 11)
        write(i, "(a)"), "* Unpaired scffold nucleotides      : "//trim(para_unpaired_scaf)
        call Space(i, 11)
        write(i, "(a$)"), "* The number of bases in Tn         : "
        if(para_n_base_tn == -1) then
            write(i, "(a)"), "depending on distance"
        else
            write(i, "(a)"), trim(adjustl(Int2Str(para_n_base_tn)))
        end if
        call Space(i, 11)
        write(i, "(a)"), "* Distance btw phosphate groups     : "//trim(adjustl(Dble2Str(para_dist_pp)))
        call Space(i, 11)
        write(i, "(a)"), "* Starting base pair ID             : "//trim(adjustl(Int2Str(para_start_bp_ID)))
        call Space(i, 11)
        write(i, "(a)"), "* Axial rise distance [nm]          : "//trim(adjustl(Dble2Str(para_dist_bp)))
        call Space(i, 11)
        write(i, "(a)"), "* Radius of helix [nm]              : "//trim(adjustl(Dble2Str(para_rad_helix)))
        call Space(i, 11)
        write(i, "(a)"), "* The Gap between helixes           : "//trim(adjustl(Dble2Str(para_gap_helix)))
        call Space(i, 11)
        write(i, "(a)"), "* Angle of minor groove             : "//trim(adjustl(Dble2Str(para_ang_minor)))
        call Space(i, 11)
        write(i, "(a)"), "* Angle correction factor           : "//trim(adjustl(Dble2Str(para_ang_correct)))
        call Space(i, 11)
        write(i, "(a)"), "* Gap between two scaf xovers       : "//trim(adjustl(Int2Str(para_gap_xover_two_scaf)))
        call Space(i, 11)
        write(i, "(a)"), "* Gap between xover(stap) and bound : "//trim(adjustl(Int2Str(para_gap_xover_bound_stap)))
        call Space(i, 11)
        write(i, "(a)"), "* Gap between stap and scaf xovers  : "//trim(adjustl(Int2Str(para_gap_xover_two)))
        call Space(i, 11)
        write(i, "(a)"), "* Gap between xover and first nick  : "//trim(adjustl(Int2Str(para_gap_xover_nick1)))
        call Space(i, 11)
        write(i, "(a)"), "* Gap between xover and nick        : "//trim(adjustl(Int2Str(para_gap_xover_nick)))
        call Space(i, 11)
        write(i, "(a)"), "* Staple cutting method             : "//trim(para_cut_stap_method)
        call Space(i, 11)
        write(i, "(a)"), "* Non-circular stap by single xover : "//trim(para_set_stap_sxover)
        call Space(i, 11)
        write(i, "(a$)"), "* Minimum # of bases in scaf strand : "
        if(para_max_cut_scaf == 0) then
            write(i, "(a)"), "infinite"
        else
            write(i, "(a)"), trim(adjustl(Int2Str(para_max_cut_scaf)))
        end if
        call Space(i, 11)
        write(i, "(a)"), "* Minimum # of bases in stap strand : "//trim(adjustl(Int2Str(para_min_cut_stap)))
        call Space(i, 11)
        write(i, "(a)"), "* Midium # of bases in stap strand  : "//trim(adjustl(Int2Str(para_mid_cut_stap)))
        call Space(i, 11)
        write(i, "(a)"), "* Maximum # of bases in stap strand : "//trim(adjustl(Int2Str(para_max_cut_stap)))
        call Space(i, 11)
        write(i, "(a$)"), "* Scaffold sequence                 : "
        if(para_set_seq_scaf == 0) then
            write(i, "(a)"), "M13mp18 sequence"
        else if(para_set_seq_scaf == 1) then
            write(i, "(a)"), "user-defined sequence"
        else if(para_set_seq_scaf == 2) then
            write(i, "(a)"), "random sequence"
        end if
        write(i, "(a)")
    end do
end subroutine Input_Print_Parameters

! -----------------------------------------------------------------------------

! Read parameters from external txt file, env.txt
subroutine Input_Read_Parameter
    character(200) :: ctemp
    integer :: i

    ! Open file
    open(unit=1, file="env.txt", form="formatted")

    read(1, *), ctemp, para_platform

    ! If the mode is not dev
    if(para_platform /= "dev") then
        read(1, *), ctemp, para_cut_stap_method
        read(1, *), ctemp, para_min_cut_stap
        read(1, *), ctemp, para_max_cut_stap
        read(1, *), ctemp, para_set_seq_scaf
    end if

    close(unit=1)
end subroutine Input_Read_Parameter

! -----------------------------------------------------------------------------

! Set command environment
subroutine Input_Set_Command
    logical :: results

    ! Set command environments
    results = SYSTEMQQ('title TALOS')                       ! cmd title
    results = SYSTEMQQ('mode con: cols=135 lines=6000')     ! cmd size
    results = SYSTEMQQ('color')                             ! convert color, 02, f0, f1, f2
    results = SYSTEMQQ('date /t')                           ! display time
end subroutine Input_Set_Command

! -----------------------------------------------------------------------------

! Print pre-defined problems
subroutine Input_Print_Problem
    write(0, "(a)")
    write(0, "(a)"), "       +=====================================================================================+"
    write(0, "(a)"), "       |                                                                                     |"
    write(0, "(a)"), "       |        TALOS by Hyungmin Jun (hyungminjun@outlook.com), MIT, Bathe Lab, 2018        |"
    write(0, "(a)"), "       |                                                                                     |"
    write(0, "(a)"), "       +=====================================================================================+"
    write(0, "(a)")
    write(0, "(a)"), "   A. First input - Pre-defined 3D target geometries"
    write(0, "(a)"), "   ================================================="
    write(0, "(a)")
    write(0, "(a)"), "      [ Platonic solids ]"
    write(0, "(a)"), "        *1. Tetrahedron,    *2. Cube,    *3. Octahedron,    4. Dodecahedron,    5. Icosahedron"
    write(0, "(a)")
    write(0, "(a)"), "      [ Archimedean solids ]"
    write(0, "(a)"), "         6. Cubeocta,                    7. Icosidodeca,               8. Rhombicubocta"
    write(0, "(a)"), "         9. Snub Cube,                  10. Truncated Cube,           11. Truncated Cubocta"
    write(0, "(a)"), "        12. Truncated Dodeca,           13. Truncated Icosa,          14. Truncated Octa"
    write(0, "(a)"), "       *15. Truncated Tetra"
    write(0, "(a)")
    write(0, "(a)"), "      [ Johnson solids ]"
    write(0, "(a)"), "        16. Gyroelongated Penta Pyramid,              *17. Triangular Bipyramid"
    write(0, "(a)"), "       *18. Penta Bipyramid,                           19. Gyroelongated Square Bipyramid"
    write(0, "(a)"), "        20. Square Gyrobicupola,                       21. Penta Orthocupolarotunda"
    write(0, "(a)"), "        22. Penta Orthobirotunda,                      23. Elongated Penta Gyrobicupola"
    write(0, "(a)"), "        24  Elongated Penta Gyrobirotunda,             25. Gyroelongated Square Bicupola"
    write(0, "(a)")
    write(0, "(a)"), "      [ Catalan solids ]"
    write(0, "(a)"), "        26. Rhombic Dodeca,         27. Rhombic Triaconta,       28. Deltoidal Icositetra"
    write(0, "(a)"), "        29. Penta Icositetra,       30. Triakis Octa,            31. Disdyakis Dodeca"
    write(0, "(a)"), "        32. Triakis Icosa,          33. Pentakis Dodecahe,       34. Tetrakis Hexa"
    write(0, "(a)"), "        35. Triakis Tetra"
    write(0, "(a)")
    write(0, "(a)"), "      [ Miscellaneous polyhedra ]"
    write(0, "(a)"), "       *36. Twisted Tri Prism,      37. Heptagonal Bipyramid,    38. Enneagonal Trapezo"
    write(0, "(a)"), "        39. Small Stell Dodeca,    *40. Rhombic Hexeconta"
    write(0, "(a)")
    write(0, "(a)"), "   Select the number or type geometry file (*.ply) [Enter] : "
    write(0, "(a)")
end subroutine Input_Print_Problem

! -----------------------------------------------------------------------------

! Print vertex design options
subroutine Input_Print_Vertex_Design
    write(0, "(a)")
    write(0, "(a)"), "   B. Second input - Vertex design"
    write(0, "(a)"), "   ==============================="
    write(0, "(a)")
    write(0, "(a)"), "   1. Flat vertex"
    write(0, "(a)"), "   2. Mitered vertex"
    write(0, "(a)")
    write(0, "(a)"), "   Select the number [Enter] : "
end subroutine Input_Print_Vertex_Design

! -----------------------------------------------------------------------------

! Print pre-defined cross-sections
subroutine Input_Print_Section
    write(0, "(a)")
    write(0, "(a)"), "   C. Third input - Vertex connection"
    write(0, "(a)"), "   =================================="
    write(0, "(a)")
    write(0, "(a)"), "                [sec ID]                  [sec ID] "
    write(0, "(a)"), "      1.  @ @      4 3          2.  @-@      5 4   "
    write(0, "(a)"), "         @   @    5   2           =@   @=  =0   3= "
    write(0, "(a)"), "         =@ @=    =0 1=             @-@      1 2   "
    write(0, "(a)"), "                                                   "
    write(0, "(a)"), "      [ Inner connection ]      [ Middle connection ] "
    write(0, "(a)")
    write(0, "(a)")
    write(0, "(a)"), "   Select the number [Enter] : "
end subroutine Input_Print_Section

! -----------------------------------------------------------------------------

! Print pre-defined the number of base pair on edges
subroutine Input_Print_Num_BP_Edge(prob)
    type(ProbType), intent(in) :: prob

    write(0, "(a)")
    write(0, "(a)"), "   D. Fourth input - Pre-defined minimum edge length"
    write(0, "(a)"), "   ================================================="
    write(0, "(a)")
    write(0, "(a)"), "      1.  42 bp =  4 turn * 21 bp/turn ->  42 bp * 0.34 nm/bp = 14.28 nm"
    write(0, "(a)"), "      2.  63 bp =  6 turn * 21 bp/turn ->  63 bp * 0.34 nm/bp = 21.42 nm"
    write(0, "(a)"), "      3.  84 bp =  8 turn * 21 bp/turn ->  84 bp * 0.34 nm/bp = 28.56 nm"
    write(0, "(a)"), "      4. 105 bp = 10 turn * 21 bp/turn -> 105 bp * 0.34 nm/bp = 35.70 nm"
    write(0, "(a)"), "      5. 126 bp = 12 turn * 21 bp/turn -> 126 bp * 0.34 nm/bp = 42.84 nm"
    write(0, "(a)")
    write(0, "(a)"), "   Select the number [Enter] : "
end subroutine Input_Print_Num_BP_Edge

! -----------------------------------------------------------------------------

! Set problem to be solved
subroutine Input_Set_Problem(prob, geom)
    type(ProbType), intent(inout) :: prob
    type(GeomType), intent(inout) :: geom

    ! Preset parameters
    if(para_vertex_design == "flat") then
        para_junc_ang        = "ave"    ! Junctional gap
        para_unpaired_scaf   = "off"    ! Unpaired scaffold nucleotides
        para_n_base_tn       = 7        ! The number of nucleotides
    else if(para_vertex_design == "mitered") then
        para_junc_ang        = "opt"    ! Junctional gap
        para_unpaired_scaf   = "on"     ! Unpaired scaffold nucleotides
        para_n_base_tn       = -1       ! The number of nucleotides
    end if

    ! Set problem
    if(prob.sel_prob == 0) call Input_Select_File(prob, geom)
    if(prob.sel_prob /= 0) call Input_Select_Problem(prob, geom)
end subroutine Input_Set_Problem

! -----------------------------------------------------------------------------

! Set vertex design
subroutine Input_Set_Vertex_Design(prob)
    type(ProbType), intent(inout) :: prob

    if(prob.sel_vertex == 1) then
        para_vertex_design = "flat"
    else
        para_vertex_design = "mitered"
    end if

    print *, para_vertex_design
end subroutine Input_Set_Vertex_Design

! -----------------------------------------------------------------------------

! File selector
subroutine Input_Select_File(prob, geom)
    type(ProbType), intent(inout) :: prob
    type(GeomType), intent(inout) :: geom

    integer :: i, len_char

    if(para_fig_view == "PRESET" .or. para_fig_view == "preset") para_fig_view = "xy"

    ! Select file type
    if(prob.type_file == "ply") then

        ! Import ply format
        call Importer_PLY(prob, geom)
    else if(prob.type_file == "stl") then

        ! Import stl format -> ply format
        call Importer_STL(prob)
        call Importer_PLY(prob, geom)

    else if(prob.type_file == "wrl") then

        ! Import wrl format -> ply format
        call Importer_WRL(prob)
        call Importer_PLY(prob, geom)
    else if(prob.type_file == "geo") then

        ! Import geo format
        call Importer_GEO(prob, geom)
    else

        write(0, "(a$)"), "Error - Not defined file type : "
        write(0, "(a )"), "Input_Select_File"
        stop
    end if

    ! Problem specified preset parameters
    if(para_vertex_design == "flat" .and. para_preset == "on") then
        para_junc_ang        = "max"    ! Junction gap modification for different arm angle
        para_const_edge_mesh = "off"    ! Constant edge length from polyhedra mesh
        para_unpaired_scaf   = "off"    ! Unpaired scaffold nucleotides
        para_n_base_tn       = 7
    end if

    prob.name_prob = prob.name_file
    call Mani_Set_Problem(prob, [52, 152, 219], "xy")

    ! Print filename and type
    !do i = 0, 11, 11
    call Space(0, 11)
    write(0, "(a)"), "* File name : "//trim(prob.name_file)//"."//trim(prob.type_file)
    write(0, "(a)")
    !end do
end subroutine Input_Select_File

! -----------------------------------------------------------------------------

! Select the pre-defined example from user input
subroutine Input_Select_Problem(prob, geom)
    type(ProbType), intent(inout) :: prob
    type(GeomType), intent(inout) :: geom

    ! Set file type as primitive
    prob.type_file = "primitive"

    ! Select problem
    select case (prob.sel_prob)

        ! Pre-defined geometries: Platonic solids
        case (1);  call Exam_Platonic_Tetrahedron  (prob, geom)
        case (2);  call Exam_Platonic_Cube         (prob, geom)
        case (3);  call Exam_Platonic_Octahedron   (prob, geom)
        case (4);  call Exam_Platonic_Dodecahedron (prob, geom)
        case (5);  call Exam_Platonic_Icosahedron  (prob, geom)

        ! Pre-defined geometries: Archimedean solids
        case (6);  call Exam_Archi_Cubeoctahedron          (prob, geom)
        case (7);  call Exam_Archi_Icosidodecahedron       (prob, geom)
        case (8);  call Exam_Archi_Rhombicuboctahedron     (prob, geom)
        case (9);  call Exam_Archi_Snub_Cube               (prob, geom)
        case (10); call Exam_Archi_Truncated_Cube          (prob, geom)
        case (11); call Exam_Archi_Truncated_Cuboctahedron (prob, geom)
        case (12); call Exam_Archi_Truncated_Dodecahedron  (prob, geom)
        case (13); call Exam_Archi_Truncated_Icosahedron   (prob, geom)
        case (14); call Exam_Archi_Truncated_Octahedron    (prob, geom)
        case (15); call Exam_Archi_Truncated_Tetrahedron   (prob, geom)

        ! Pre-defined geometries: Johnson solids
        case (16); call Exam_Johnson_Gyroelongated_Pentagonal_Pyramid_J11   (prob, geom)
        case (17); call Exam_Johnson_Triangular_Bipyramid_J12               (prob, geom)
        case (18); call Exam_Johnson_Pentagonal_Bipyramid_J13               (prob, geom)
        case (19); call Exam_Johnson_Gyroelongated_Square_Bipyramid_J17     (prob, geom)
        case (20); call Exam_Johnson_Square_Gyrobicupola_J29                (prob, geom)
        case (21); call Exam_Johnson_Pentagonal_Orthocupolarotunda_J32      (prob, geom)
        case (22); call Exam_Johnson_Pentagonal_Orthobirotunda_J34          (prob, geom)
        case (23); call Exam_Johnson_Elongated_Pentagonal_Gyrobicupola_J39  (prob, geom)
        case (24); call Exam_Johnson_Elongated_Pentagonal_Gyrobirotunda_J43 (prob, geom)
        case (25); call Exam_Johnson_Gyroelongated_Square_Bicupola_J45      (prob, geom)

        ! Pre-defined geometries: Catalan solids
        case (26); call Exam_Catalan_Rhombic_Dodecahedron        (prob, geom)
        case (27); call Exam_Catalan_Rhombic_Triacontahedron     (prob, geom)
        case (28); call Exam_Catalan_Deltoidal_Icositetrahedron  (prob, geom)
        case (29); call Exam_Catalan_Pentagonal_Icositetrahedron (prob, geom)
        case (30); call Exam_Catalan_Triakis_Octahedron          (prob, geom)
        case (31); call Exam_Catalan_Disdyakis_Dodecahedron      (prob, geom)
        case (32); call Exam_Catalan_Triakis_Icosahedron         (prob, geom)
        case (33); call Exam_Catalan_Pentakis_Dodecahedron       (prob, geom)
        case (34); call Exam_Catalan_Tetrakis_Hexahedron         (prob, geom)
        case (35); call Exam_Catalan_Triakis_Tetrahedron         (prob, geom)

        ! Pre-defined geometries: Miscellaneous polyhedra
        case (36); call Exam_Miscel_Twisted_Triangular_Prism (prob, geom)
        case (37); call Exam_Miscel_Heptagonal_Bipyramid     (prob, geom)
        case (38); call Exam_Miscel_Enneagonal_Trapezohedron (prob, geom)
        case (39); call Exam_Miscel_Small_Stell_Dodecahedron (prob, geom)
        case (40); call Exam_Miscel_Rhombic_Hexecontahedron  (prob, geom)

        case (41); call Exam_Miscel_Goldberg_Dk5dgD          (prob, geom)
        case (42); call Exam_Miscel_Double_Helix             (prob, geom)
        case (43); call Exam_Miscel_Nested_Cube              (prob, geom)
        case (44); call Exam_Miscel_Nested_Octahedron        (prob, geom)
        case (45); call Exam_Miscel_Torus                    (prob, geom)
        case (46); call Exam_Miscel_Double_Torus             (prob, geom)

        case default
            write(0, "(a$)"), "Error - Not defined problem : "
            write(0, "(a )"), "Input_Select_Problem"
            stop
    end select
end subroutine Input_Select_Problem

! -----------------------------------------------------------------------------

! Set cross-section based on square or honeycomb lattices
subroutine Input_Set_Section(prob, geom)
    type(ProbType), intent(in)    :: prob
    type(GeomType), intent(inout) :: geom

    integer :: bp_id

    ! Cross-section definition on local coordinate - t3-t2
    !        t2
    !        ¡è
    !        |
    !     ---|-------¡æ t3
    !        |
    ! The number of columns should be even
    if(prob.sel_sec == 1) then

        if(para_start_bp_ID == -1) para_start_bp_ID = 13 + 1
        bp_id = mod(para_start_bp_ID, 21)

        ! Starting BP - 3, 4 / 13, 14       | caDNAno   02    (CW)
        !      ¡Ü¡Ü        04 03              |         03  01
        !     ¡Ü  ¡Ü      05   02             |         04  00
        !      ¡Ü¡Ü       .00 01.   <--- ref  |           05
        geom.sec.dir      = 90
        geom.n_sec        = 6
        geom.sec.ref_row  = 1
        geom.sec.ref_minC = 1
        geom.sec.ref_maxC = 2

        call Mani_Allocate_SecType(geom.sec, geom.n_sec)
        call Mani_Init_SecType    (geom.sec, geom.n_sec)

        geom.sec.id(1) = 0; geom.sec.posR(1) = 1; geom.sec.posC(1) = 1
        geom.sec.id(2) = 1; geom.sec.posR(2) = 1; geom.sec.posC(2) = 2
        geom.sec.id(3) = 2; geom.sec.posR(3) = 2; geom.sec.posC(3) = 2
        geom.sec.id(4) = 3; geom.sec.posR(4) = 3; geom.sec.posC(4) = 2
        geom.sec.id(5) = 4; geom.sec.posR(5) = 3; geom.sec.posC(5) = 1
        geom.sec.id(6) = 5; geom.sec.posR(6) = 2; geom.sec.posC(6) = 1

        ! Increase or decrease edge length except for reference row and column section
        para_vertex_modify = "mod2"

    else if(prob.sel_sec == 2) then

        if(para_start_bp_ID == -1) para_start_bp_ID = 11 + 1
        bp_id = mod(para_start_bp_ID, 21)

        ! Starting BP - 1 / 11              | caDNAno   02    (CW)
        !      ¡Ü¡Ü        05=04              |         03  01
        !     ¡Ü  ¡Ü     .00   03.  <--- ref  |         04  00
        !      ¡Ü¡Ü        01=02              |           05
        geom.sec.dir      = 150
        geom.n_sec        = 6
        geom.sec.ref_row  = 2
        geom.sec.ref_minC = 1
        geom.sec.ref_maxC = 2

        call Mani_Allocate_SecType(geom.sec, geom.n_sec)
        call Mani_Init_SecType    (geom.sec, geom.n_sec)

        geom.sec.id(1) = 0; geom.sec.posR(1) = 2; geom.sec.posC(1) = 1
        geom.sec.id(2) = 1; geom.sec.posR(2) = 1; geom.sec.posC(2) = 1
        geom.sec.id(3) = 2; geom.sec.posR(3) = 1; geom.sec.posC(3) = 2
        geom.sec.id(4) = 3; geom.sec.posR(4) = 2; geom.sec.posC(4) = 2
        geom.sec.id(5) = 4; geom.sec.posR(5) = 3; geom.sec.posC(5) = 2
        geom.sec.id(6) = 5; geom.sec.posR(6) = 3; geom.sec.posC(6) = 1

        ! Decrease edge length at the bottom
        if(para_vertex_modify == "mod") para_vertex_modify = "mod1"
    else

        write(0, "(a$)"), "Error - Not defined cross-section : "
        write(0, "(a )"), "Input_Set_Section"
        stop
    end if

    !! Set section connectivity in the defined initial section
    call Input_Set_Section_Connectivity(prob, geom)

    ! Find maximum and minimum sectional row and column
    call Input_Find_Max_Min_Section(geom)
end subroutine Input_Set_Section

! -----------------------------------------------------------------------------

! Find maximum and minimum sectional row and column
subroutine Input_Find_Max_Min_Section(geom)
    type(GeomType), intent(inout) :: geom

    integer :: i

    ! sec.maxR ~ minC were initialized from Mani_Init_SecType
    do i = 1, geom.n_sec
        ! Find max and min of row
        if(geom.sec.posR(i) > geom.sec.maxR) geom.sec.maxR = geom.sec.posR(i)
        if(geom.sec.posR(i) < geom.sec.minR) geom.sec.minR = geom.sec.posR(i)

        ! Find max and min of col
        if(geom.sec.posC(i) > geom.sec.maxC) geom.sec.maxC = geom.sec.posC(i)
        if(geom.sec.posC(i) < geom.sec.minC) geom.sec.minC = geom.sec.posC(i)
    end do

    ! Find the size of columns and rows
    geom.sec.n_row = geom.sec.maxR - geom.sec.minR + 1
    geom.sec.n_col = geom.sec.maxC - geom.sec.minC + 1

    ! Check even number
    if(mod(geom.sec.n_col, 2) /= 0) then
        write(0, "(a$)"), "Error - the column number should be even : "
        write(0, "(a )"), "Input_Find_Max_Min_Section"
        stop
    end if
end subroutine Input_Find_Max_Min_Section

! -----------------------------------------------------------------------------

! Set section connectivity in the defined initial section
subroutine Input_Set_Section_Connectivity(prob, geom)
    type(ProbType), intent(in)    :: prob
    type(GeomType), intent(inout) :: geom

    integer :: sec_cur, sec_com, row_cur, row_com
    integer :: i, j, count, id_bp
    logical :: b_connect

    ! If self connection route turns on, find connection to link each other
    ! Section connection was initialized as -1
    ! Loop for current section
    do i = 1, geom.n_sec
        sec_cur = geom.sec.id(i)
        row_cur = geom.sec.posR(i)

        ! Loop for comparing section
        do j = 1, geom.n_sec
            sec_com = geom.sec.id(j)
            row_com = geom.sec.posR(j)

            ! If section numbers are the same
            if(sec_cur == sec_com) cycle

            ! Determine the section connection for scaffold strand
            id_bp     = 1
            b_connect = Section_Connection_Scaf(geom, sec_cur, sec_com, id_bp)

            ! Set section connectivity
            if( (para_vertex_design == "flat"    .and. b_connect == .true.) .or. &
                (para_vertex_design == "mitered" .and. para_vertex_modify == "mod1" .and. &
                 row_cur < geom.sec.ref_row .and. row_cur == row_com ) ) then
                geom.sec.conn(i) = sec_com
                exit
            end if
        end do
    end do

    ! For alternative 6-helice bundle
    if(prob.sel_sec == 1 .and. para_vertex_design == "flat" .and. para_vertex_modify == "mod2") then
        geom.sec.conn(1) = -1       ! Sec 0 -> neighbor
        geom.sec.conn(2) = -1       ! Sec 1 -> neighbor
        geom.sec.conn(3) = 3        ! Sec 2 -> 3
        geom.sec.conn(4) = 2        ! Sec 3 -> 2
        geom.sec.conn(5) = 5        ! Sec 4 -> 5
        geom.sec.conn(6) = 4        ! Sec 5 -> 4
    end if

    ! Print information for self connection route
    count = 0
    write(0, "(a)"), "   --------------------------------------------------"
    write(0, "(a)"), "     Sectional connection to determine route method  "
    write(0, "(a)"), "   --------------------------------------------------"
    do i = 1, geom.n_sec

        write(0, "(i10, a$)"), geom.sec.id(i), " th section  ->"

        if(geom.sec.conn(i) /= -1) then
            write(0, "(i7, a)"), geom.sec.conn(i), "    th section"
        else
            write(0, "(a)"), "  Neighbor connection"
            count = count + 1
        end if
    end do
    write(0, "(a)"), "   --------------------------------------------------"
    write(0, "(a)")

    ! Check neighboring connection
    ! There should be at least two neighboring connection
    ! Also, these number should be even
    if(mod(count, 2) /= 0 .or. count == 0) then
        write(0, "(a$)"), "Error - The neighboring connection should be "
        write(0, "(a$)"), "even and larger than 1 : "
        write(0, "(a )"), "Input_Set_Section_Connectivity"
        stop
    end if
end subroutine Input_Set_Section_Connectivity

! -----------------------------------------------------------------------------

! Set edge-length of the target DNA structure
subroutine Input_Set_Num_BP_Edge(prob, geom)
    type(ProbType), intent(inout) :: prob
    type(GeomType), intent(in)    :: geom

    if(prob.sel_bp_edge == 1) prob.n_bp_edge = 42   ! 21 bp/turn * 2 turn
    if(prob.sel_bp_edge == 2) prob.n_bp_edge = 63   ! 21 bp/turn * 3 turn
    if(prob.sel_bp_edge == 3) prob.n_bp_edge = 84   ! 21 bp/turn * 4 turn
    if(prob.sel_bp_edge == 4) prob.n_bp_edge = 105  ! 21 bp/turn * 5 turn
    if(prob.sel_bp_edge == 5) prob.n_bp_edge = 126  ! 21 bp/turn * 6 turn

    if(prob.sel_bp_edge > 5 .and. prob.sel_bp_edge <= 41) then
        write(0, "(a)"), "==================================================="
        write(0, "(a)"), ""
        write(0, "(a)"), "Error: The minimum edge length should be over 42-bp"
        write(0, "(a)"), ""
        write(0, "(a)"), "==================================================="
        stop
    end if

    if(prob.sel_bp_edge >= 21) then
        prob.n_bp_edge = prob.sel_bp_edge
    end if
end subroutine Input_Set_Num_BP_Edge

! -----------------------------------------------------------------------------

! Convert surface to line connectivity
subroutine Input_Convert_Face_To_Line(geom)
    type(GeomType), intent(inout) :: geom
    
    integer :: i, j, k, f_zero, flag
    integer :: point_1, point_2, point_1_com, point_2_com

    ! Mesh data structure
    type :: MeshType
        integer :: cn(100)   ! Maximum connectivity
    end type MeshType

    type(MeshType), allocatable, dimension (:) :: Basepair_con  ! 1st: # of meshes, 2nd: points
    type(ListConn), pointer :: straight_con
    type(ListConn), pointer :: ptr, ptr1

    allocate(Basepair_con(geom.n_face))

    ! Nullify the linked list for the junction data
    nullify(straight_con)
    nullify(ptr)
    nullify(ptr1)

    ! Initialze variable
    f_zero      = 0
    geom.n_iniL = 0

    ! Read mesh
    do i = 1, geom.n_face

        ! Read the number of vectices in the mesh and connectivity
        Basepair_con(i).cn(1:geom.face(i).n_poi) = geom.face(i).poi(1:geom.face(i).n_poi)

        ! If there is zero value in the connectivity
        do j = 1, geom.face(i).n_poi
            if(Basepair_con(i).cn(j) == 0) f_zero = 1
        end do

        do j = 1, geom.face(i).n_poi

            flag = 1    ! Flag 1 means that there is no connectivity in the existing array

            if(j == geom.face(i).n_poi) then
                point_1 = Basepair_con(i).cn(j)
                point_2 = Basepair_con(i).cn(1)
            else
                point_1 = Basepair_con(i).cn(j)
                point_2 = Basepair_con(i).cn(j+1)
            end if

            if(i == 1) then     ! First connectivities are always saved
                flag = 1
            else
                allocate(ptr1)
                ptr1 => straight_con

                do k = 1, geom.n_iniL

                    point_1_com = ptr1%point(1)
                    point_2_com = ptr1%point(2)

                    ! Check where there is the same connectivity in the existing array
                    if((point_1_com == point_1 .and. point_2_com == point_2) ) then
                        flag = 0
                        exit
                    else if((point_1_com == point_2 .and. point_2_com == point_1) ) then
                        flag = 0
                        exit
                    end if

                    ! Pointer to move next list
                    ptr1 => ptr1%next
                end do
            end if

            ! Set connectivity adding this into list
            if(flag == 1) then

                allocate(ptr)
                geom.n_iniL  = geom.n_iniL + 1
                ptr%point(1) = point_1
                ptr%point(2) = point_2
                straight_con => List_Insert_Conn(straight_con, ptr)

                if(i == 1 .and. geom.face(i).n_poi == 2) exit
            end if

        end do
    end do

    ! Allocate stragiht line data structure
    allocate(geom.iniL(geom.n_iniL))

    ! Initialize line data type
    call Mani_Init_LineType(geom.iniL, geom.n_iniL)

    ! Set straight line information
    ptr1 => straight_con
    do i = 1, geom.n_iniL

        ! If there is zero in the connectivity, add one
        if(f_zero == 1) then
            geom.iniL(geom.n_iniL+1-i).poi(1:2) = ptr1%point(1:2) + 1
        else
            geom.iniL(geom.n_iniL+1-i).poi(1:2) = ptr1%point(1:2)
        end if

        ! Pointer to move next linked list
        ptr1 => ptr1%next
    end do

    if(f_zero == 1) then
        do i = 1, geom.n_face
            do j = 1, geom.face(i).n_poi
                geom.face(i).poi(j) = geom.face(i).poi(j) + 1
            end do
        end do
    end if

    ! Copy from poi to iniP that always save initial points
    do i = 1, geom.n_iniL
        geom.iniL(i).iniP(1:2) = geom.iniL(i).poi(1:2)
    end do

    ! Delete linked list allocated
    call List_Delete_Conn(straight_con)
    !call List_Delete_Conn(ptr)
    !call List_Delete_Conn(ptr1)

    ! Deallocate mesh connectivity data
    deallocate(Basepair_con)
end subroutine Input_Convert_Face_To_Line

! -----------------------------------------------------------------------------

! Set geometric scale, initial minimum length of edge is set up 20 nm
subroutine Input_Scale_Init_Geometry(geom)
    type(GeomType), intent(inout) :: geom

    double precision :: pos_c(3), length, min_len
    integer :: i, poi_1, poi_2

    ! Find center position of the structure
    pos_c(1:3) = 0.0d0
    do i = 1, geom.n_iniP
        pos_c(1:3) = pos_c + geom.iniP(i).pos
    end do
    pos_c(1:3) = pos_c / dble(geom.n_iniP)

    ! Move whole geometry to center position
    do i = 1, geom.n_iniP
        geom.iniP(i).pos(1:3) = geom.iniP(i).pos(1:3) - pos_c(1:3)
    end do

    ! Find minimum length of line
    poi_1 = geom.iniL(1).poi(1)
    poi_2 = geom.iniL(1).poi(2)
    min_len = Norm(geom.iniP(poi_1).pos - geom.iniP(poi_2).pos)

    do i = 2, geom.n_iniL

        poi_1  = geom.iniL(i).poi(1)
        poi_2  = geom.iniL(i).poi(2)
        length = Norm(geom.iniP(poi_1).pos - geom.iniP(poi_2).pos)

        ! If short edge exists
        if(length < min_len) then
            min_len = length
        end if
    end do

    ! Recalucate edge length
    do i = 1, geom.n_iniP
        geom.iniP(i).pos(1:3) = geom.iniP(i).pos / min_len * para_init_scale
    end do

    ! Find edge that has minimum length 
    !do i = 1, geom.n_iniL
    !    poi_1  = geom.iniL(i).poi(1)
    !    poi_2  = geom.iniL(i).poi(2)
    !    length = Norm(geom.iniP(poi_1).pos - geom.iniP(poi_2).pos)
    !    write(0, "(i, f15.5)"), i, length
    !end do
end subroutine Input_Scale_Init_Geometry

! -----------------------------------------------------------------------------

! Set working and Chimera path
subroutine Input_Set_Path(prob)
    type(ProbType), intent(inout) :: prob

    ! Set working directory
    if(para_platform == "dev") then
        prob.path_work = "output"
    else if(para_platform == "win") then
        prob.path_work = "output\"//trim(prob.name_file)
    else
        prob.path_work = "output/"//trim(prob.name_file)
    end if
end subroutine Input_Set_Path

! -----------------------------------------------------------------------------

! Remove previous working directory and make new one
subroutine Input_Set_Workplace(prob)
    type(ProbType), intent(in) :: prob

    logical :: results

    ! Remove the directory and files
    if(para_platform == "dev") then
        results = systemqq("rd "//trim(prob.path_work)//' /s /q')   ! Windows
    else if(para_platform == "win") then
        results = systemqq("rd "//trim(prob.path_work)//' /s /q')   ! Windows
    else
        results = systemqq("rm -r "//trim(prob.path_work))          ! Linux & Mac
    end if

    ! Make new working directory
    if(para_platform == "dev") then
        results = systemqq("md "//trim(prob.path_work))         ! Dev
    else if(para_platform == "win") then
        results = systemqq("md "//trim(prob.path_work))         ! Windows
    else
        results = systemqq("mkdir -p "//trim(prob.path_work))   ! Linux & Mac
    end if

    ! Directory for Tecplot output
    if(para_output_Tecplot == "on") then
        if(para_platform == "dev" )then
            results = systemqq("md "//trim(prob.path_work)//"\tecplot")         ! Dev
        else if(para_platform == "win") then
            results = systemqq("md "//trim(prob.path_work)//"\tecplot")         ! Windows
        else
            results = systemqq("mkdir -p "//trim(prob.path_work)//"/tecplot")   ! Linux & Mac
        end if
    end if

    write(0, "(a)"), "  ...Removed the existing working directory"
    write(0, "(a)")
end subroutine Input_Set_Workplace

! -----------------------------------------------------------------------------

! Write geo file file
subroutine Input_Write_GEO_File(prob, geom)
    type(ProbType), intent(in) :: prob
    type(GeomType), intent(in) :: geom

    character(200) :: path
    integer :: i, j

    ! Exception
    if(para_write_101 == .false.) return

    path = trim(prob.path_work)//"/"//trim(prob.name_file)
    open(unit=101, file=trim(path)//".geo", form="formatted")

    ! Write points
    write(101, "(i7)"), geom.n_iniP
    do i = 1, geom.n_iniP
        write(101,"(i7, 3f10.4)"), i, geom.iniP(i).pos(1:3)
    end do
    write(101, "(a)")

    ! Write faces
    write(101, "(i7)"), geom.n_face
    do i = 1, geom.n_face
        write(101,"(i7, i16$)"), geom.face(i).n_poi, geom.face(i).poi(1)
        do j = 2, geom.face(i).n_poi - 1
            write(101,"(i7$)"), geom.face(i).poi(j)
        end do
        write(101,"(i7)"), geom.face(i).poi(geom.face(i).n_poi)
    end do
    write(101, "(a)")

    ! Write edges
    write(101, "(i7)"), geom.n_iniL
    do i = 1, geom.n_iniL
        write(101, "(3i8)"), 2, geom.iniL(i).poi(1:2)
    end do
    write(101, "(a)")

    ! Write geometric for input module
    write(101, "(a)")
    write(101, "(a)"), "--------------------------------------------------"
    write(101, "(a)")
    call Space(101, 4)
    write(101, "(a)") "! Allocate point and face structure"
    call Space(101, 4)
    write(101, "(a, i4)") "geom.n_iniP = ", geom.n_iniP
    call Space(101, 4)
    write(101, "(a, i4)") "geom.n_face = ", geom.n_face
    write(101, "(a)")
    call Space(101, 4)
    write(101, "(a)") "allocate(geom.iniP(geom.n_iniP))"
    call Space(101, 4)
    write(101, "(a)") "allocate(geom.face(geom.n_face))"
    write(101, "(a)")

    ! For points
    call Space(101, 4)
    write(101, "(a)") "! Set point position vectors"
    do i = 1, geom.n_iniP
        call Space(101, 4)
        write(101, "(a, i7, a$)"), "geom.iniP(", i, ").pos(1:3) = [ "
        write(101, "(f10.4, a$)"), geom.iniP(i).pos(1), "d0, "
        write(101, "(f10.4, a$)"), geom.iniP(i).pos(2), "d0, "
        write(101, "(f10.4, a )"), geom.iniP(i).pos(3), "d0 ]"
    end do
    write(101, "(a)")

    ! For faces
    call Space(101, 4)
    write(101, "(a)") "! Set point position vectors"
    do i = 1, geom.n_face
        call Space(101, 4)
        write(101, "(a, i7$)"), "geom.face(",            i
        write(101, "(a, i7$)"), ").n_poi = ",            geom.face(i).n_poi
        write(101, "(a, i7$)"), "; allocate(geom.face(", i
        write(101, "(a, i7$)"), ").poi(",                geom.face(i).n_poi
        write(101, "(a, i7$)"), ")); geom.face(",        i
        write(101, "(a, i7$)"), ").poi(1:",              geom.face(i).n_poi
        write(101, "(a$    )"), ") = ["

        do j = 1, geom.face(i).n_poi - 1
            write(101, "(i7, a$)"), geom.face(i).poi(j), ", "
        end do
        write(101, "(i7, a)"), geom.face(i).poi(geom.face(i).n_poi), " ]"
    end do

    write(101, "(a)")
    write(101, "(a)"), "--------------------------------------------------"
    write(101, "(a)")

    ! For edges
    write(101, "(i7)"), geom.n_iniL
    do i = 1, geom.n_iniL
        write(101, "(a, i7, a$)"), "line(", i, ", 1:2) = [ "
        write(101, "(i7, a$   )"), geom.iniL(i).poi(1), ", "
        write(101, "(i7, a    )"), geom.iniL(i).poi(2), " ]"
    end do
    write(101, "(a)")

    close(unit=101)
end subroutine Input_Write_GEO_File

! -----------------------------------------------------------------------------

! Write ply file file
subroutine Input_Write_PLY_File(prob, geom)
    type(ProbType), intent(in) :: prob
    type(GeomType), intent(in) :: geom

    character(200) :: path
    integer :: i, j

    path = trim(prob.path_work)//"/"//trim(prob.name_file)
    open(unit=101, file=trim(path)//"_00_PLY.ply", form="formatted")

    write(101, "(a)"), "ply"
    write(101, "(a)"), "format ascii 1.0"
    write(101, "(a)"), "element vertex "//trim(adjustl(Int2Str(geom.n_iniP)))
    write(101, "(a)"), "property float32 x"
    write(101, "(a)"), "property float32 y"
    write(101, "(a)"), "property float32 z"
    write(101, "(a)"), "element face "//trim(adjustl(Int2Str(geom.n_face)))
    write(101, "(a)"), "property list uint8 int32 vertex_indices"
    write(101, "(a)"), "end_header"

    ! Write points
    do i = 1, geom.n_iniP
        write(101, "(3f10.4)"), geom.iniP(i).pos(1:3)
    end do

    ! Write faces
    do i = 1, geom.n_face
        write(101, "(i7$)"), geom.face(i).n_poi
        do j = 1, geom.face(i).n_poi
            write(101, "(i7$)"), geom.face(i).poi(j) - 1
        end do
        write(101, "(a)")
    end do

    close(unit=101)
end subroutine Input_Write_PLY_File

! -----------------------------------------------------------------------------

! Write initial geometry for Chimera
subroutine Input_Chimera_Init_Geometry(prob, geom)
    type(ProbType), intent(in) :: prob
    type(GeomType), intent(in) :: geom

    double precision :: length, pos_1(3), pos_2(3), pos_c(3)
    integer :: i, j
    logical :: f_info, f_axis
    character(200) :: path

    if(para_write_102 == .false.) return

    ! Set option
    f_axis = para_chimera_axis
    f_info = para_chimera_102_info

    path = trim(prob.path_work)//"/"//trim(prob.name_file)
    open(unit=102, file=trim(path)//"_01_target_geometry.bild", form="formatted")

    ! Write initial points
    write(102, "(a)"), ".color red"
    do i = 1, geom.n_iniP
        write(102, "(a$    )"), ".sphere "
        write(102, "(3f9.2$)"), geom.iniP(i).pos(1:3)
        write(102, "(1f9.2 )"), 0.75d0
    end do

    ! Write initial edges
    write(102, "(a)"), ".color dark green"
    do i = 1, geom.n_iniL

        pos_1(1:3) = geom.iniP(geom.iniL(i).poi(1)).pos(1:3)
        pos_2(1:3) = geom.iniP(geom.iniL(i).poi(2)).pos(1:3)

        write(102,"(a$    )"), ".cylinder "
        write(102,"(3f9.2$)"), pos_1(1:3)
        write(102,"(3f9.2$)"), pos_2(1:3)
        write(102,"(1f9.2 )"), 0.3d0
    end do

    ! Information on initial geometry
    if(f_info == .true.) then

        ! For points
        do i = 1, geom.n_iniP
            write(102, "(a$   )"), ".cmov "
            write(102, "(3f9.2)"), geom.iniP(i).pos + 1.0d0
            write(102, "(a    )"), ".color red"
            write(102, "(a    )"), ".font Helvetica 12 bold"
            write(102, "(i    )"), i
        end do

        ! For edges
        do i = 1, geom.n_iniL
            pos_1(1:3) = geom.iniP(geom.iniL(i).poi(1)).pos(1:3)
            pos_2(1:3) = geom.iniP(geom.iniL(i).poi(2)).pos(1:3)
            pos_c(1:3) = (pos_1(1:3) + pos_2(1:3)) / 2.0d0
            length     = Norm(pos_2 - pos_1)

            write(102, "(a$   )"), ".cmov "
            write(102, "(3f9.2)"), pos_c(1:3) + 0.5d0
            write(102, "(a    )"), ".color dark green"
            write(102, "(a    )"), ".font Helvetica 12 bold"
            write(102, "(i,    a$)"), i, "("
            write(102, "(f5.1, a )"), length, ")"
        end do

        ! For faces
        do i = 1, geom.n_face

            ! Find center position in mesh
            pos_c(1:3) = 0.0d0
            do j = 1, geom.face(i).n_poi
                pos_c(1:3) = pos_c + geom.iniP(geom.face(i).poi(j)).pos
            end do
            pos_c(1:3) = pos_c / dble(geom.face(i).n_poi)

            ! Write face number
            write(102, "(a$   )"), ".cmov "
            write(102, "(3f9.2)"), pos_c(1:3) + 1.0d0
            write(102, "(a    )"), ".color black"
            write(102, "(a    )"), ".font Helvetica 12 bold"
            write(102, "(i7   )"), i
        end do
    end if

    ! Write global axis
    if(f_axis == .true.) then
        write(102, "(a)"), ".translate 0.0 0.0 0.0"
        write(102, "(a)"), ".scale 0.5"
        write(102, "(a)"), ".color grey"
        write(102, "(a)"), ".sphere 0 0 0 0.5"      ! Center
        write(102, "(a)"), ".color red"             ! x-axis
        write(102, "(a)"), ".arrow 0 0 0 4 0 0 "
        write(102, "(a)"), ".color blue"            ! y-axis
        write(102, "(a)"), ".arrow 0 0 0 0 4 0 "
        write(102, "(a)"), ".color yellow"          ! z-axis
        write(102, "(a)"), ".arrow 0 0 0 0 0 4 "
    end if
    close(unit=102)

    ! --------------------------------------------------
    ! Write the file for Tecplot
    ! --------------------------------------------------
    if(para_output_Tecplot == "off") return

    path = trim(prob.path_work)//"/tecplot/"//trim(prob.name_file)
    open(unit=102, file=trim(path)//"_01_target_geometry.dat", form="formatted")

    write(102, "(a )"), 'TITLE = "'//trim(prob.name_file)//'"'
    write(102, "(a )"), 'VARIABLES = "X", "Y", "Z", "weight"'
    write(102, "(a$)"), 'ZONE F = FEPOINT'
    write(102, "(a$)"), ', N='//trim(adjustl(Int2Str(geom.n_iniP)))
    write(102, "(a$)"), ', E='//trim(adjustl(Int2Str(geom.n_iniL)))
    write(102, "(a )"), ', ET=LINESEG'

    ! Write points
    do i = 1, geom.n_iniP
        write(102, "(3f10.4$)"), geom.iniP(i).pos(1:3)
        write(102, "(1f10.4 )"), 1.0d0
    end do

    ! Write edges
    do i = 1, geom.n_iniL
        write(102, "(1i7$)"), geom.iniL(i).poi(1)
        write(102, "(1i7 )"), geom.iniL(i).poi(2)
    end do

    close(unit=102)
end subroutine Input_Chimera_Init_Geometry

! -----------------------------------------------------------------------------

! Write initial geometry for Tecplot
subroutine Input_Tecplot_Init_Geometry(prob, geom)
    type(ProbType), intent(in) :: prob
    type(GeomType), intent(in) :: geom

    double precision :: length, pos_1(3), pos_2(3), pos_c(3)
    logical :: f_info, f_axis
    integer :: i, j, nline
    character(200) :: path

    if(para_write_103 == .false.) return

    ! Open file
    path = trim(prob.path_work)//"/"//trim(prob.name_file)
    open(unit=102, file=trim(path)//"_init_geo_face.dat", form="formatted")

    ! Find the number of lines
    nline = 0
    do i = 1, geom.n_face
        nline = nline + geom.face(i).n_poi
    end do

    ! For Tecplot output
    write(102, "(a)"), 'VARIABLES = "X", "Y", "Z"'
    write(102, "(a)"), 'ZONE T    = "'//trim(prob.name_file)//'"'
    write(102, "(a)"), 'ZONETYPE  = FEPOLYGON'
    write(102, "(a)"), 'NODES     = '//trim(adjustl(Int2Str(geom.n_iniP)))
    write(102, "(a)"), 'ELEMENTS  = '//trim(adjustl(Int2Str(geom.n_face)))
    write(102, "(a)"), 'FACES     = '//trim(adjustl(Int2Str(nline)))
    write(102, "(a)"), 'NumConnectedBoundaryFaces   = 0'
    write(102, "(a)"), 'TotalNumBoundaryConnections = 0'
    write(102, "(a)")

    ! Write x-direction position
    do i = 1, geom.n_iniP
        write(102, "(f8.2$)"), geom.iniP(i).pos(1)
    end do
    write(102, "(a)")

    ! Write y-direction position
    do i = 1, geom.n_iniP
        write(102, "(f8.2$)"), geom.iniP(i).pos(2)
    end do
    write(102, "(a)")

    ! Write z-direction position
    do i = 1, geom.n_iniP
        write(102, "(f8.2$)"), geom.iniP(i).pos(3)
    end do
    write(102, "(a)"); write(102, "(a)")

    ! Write line connectivity
    write(102, "(a)"), "# Face Nodes List"
    do i = 1, geom.n_face
        do j = 1, geom.face(i).n_poi
            if(j == geom.face(i).n_poi) then
                write(102, "(2i8)"), geom.face(i).poi(j), geom.face(i).poi(1)
            else
                write(102, "(2i8)"), geom.face(i).poi(j), geom.face(i).poi(j+1)
            end if
        end do
    end do
    write(102, "(a)"); write(102, "(a)")

    ! # Face Left Elements (In this case, they all point to the single element in this zone)
    write(102, "(a)"), "# Face Left Elements"
    do i = 1, geom.n_face
        do j = 1, geom.face(i).n_poi
            write(102, "(i7$)"), i
        end do
    end do
    write(102, "(a)"); write(102, "(a)")

    ! # Face Right elements (0 means no boundary, -n means use the nth boundary connection)
    write(102, "(a)"), "# Face Right elements"
    do i = 1, geom.n_face
        do j = 1, geom.face(i).n_poi
            write(102, "(i7$)"), 0
        end do
    end do
    write(102, "(a)"); write(102, "(a)")

    close(unit=102)
end subroutine Input_Tecplot_Init_Geometry

! -----------------------------------------------------------------------------

! Generate Schlegel diagram
subroutine Input_Generate_Schlegel_Diagram(prob, geom)
    type(ProbType), intent(in) :: prob
    type(GeomType), intent(in) :: geom

    double precision, allocatable :: pos_xy(:,:), pos_nei(:,:)
    integer, allocatable :: face(:), vert_row(:), vert_col(:), nei(:)

    double precision :: angle, pos_mid(2)
    integer :: i, j, k, n_vert, n_rept, n_nei, max_n_vert, max_face, nbr
    logical :: b_continue

    ! Number of vertices and iterations to calculate pos_xy
    n_vert = geom.n_iniP
    n_rept = 10*n_vert

    ! Choose face that has the maximum number of vertices
    max_n_vert = geom.face(1).n_poi
    max_face   = 1
    do i = 2, geom.n_face
        if(geom.face(i).n_poi > max_n_vert) then
            max_n_vert = geom.face(i).n_poi
            max_face   = i
        end if
    end do

    ! Identify vertices associated with biggest face
    allocate(face(max_n_vert))
    do i = 1, max_n_vert
        face(i) = geom.face(max_face).poi(i)
    end do

    ! Initialize pos_xy
    allocate(pos_xy(n_vert, 2))
    pos_xy(1:n_vert, 1:2) = 0.0d0

    ! Set big face on unit circle
    angle = 2.0d0*pi / dble(max_n_vert)

    ! For each vertex in big face
    do i = 1, max_n_vert
        pos_xy(face(i), 1) = dcos(angle*dble(i-1))
        pos_xy(face(i), 2) = dsin(angle*dble(i-1))
    end do

    ! Calculate positions of other vertices through iterative process
    do i = 1, n_rept
        do j = 1, n_vert

            b_continue = .true.
            do k = 1, max_n_vert
                if(face(k) == j) then
                    b_continue = .false.
                    exit
                end if
            end do

            if(b_continue == .true.) then

                ! Find j th vert
                allocate(vert_row(geom.n_iniL))
                allocate(vert_col(geom.n_iniL))

                n_nei = 0
                do k = 1, geom.n_iniL
                    if(geom.iniL(k).poi(1) == j) then
                        n_nei           = n_nei + 1
                        vert_row(n_nei) = k
                        vert_col(n_nei) = 1
                    else if(geom.iniL(k).poi(2) == j) then
                        n_nei           = n_nei + 1
                        vert_row(n_nei) = k
                        vert_col(n_nei) = 2
                    end if
                end do

                ! Find neighbors
                allocate(nei(n_nei))
                nei(1:n_nei) = 0

                do k = 1, n_nei
                    nbr    = 3 - vert_col(k)
                    nei(k) = geom.iniL(vert_row(k)).poi(nbr)
                end do

                ! Get neighbor position
                allocate(pos_nei(n_nei,2))
                pos_nei(:, 1:2) = pos_xy(nei, 1:2)

                ! Find mid position
                pos_mid(1:2) = 0.0d0
                do k = 1, n_nei
                    pos_mid(1:2) = pos_mid(1:2) + pos_nei(k, 1:2)
                end do
                pos_mid(1:2) = pos_mid(1:2)/dble(n_nei)

                ! Store as new pos_xy for jth vertex
                pos_xy(j, 1:2) = pos_mid(1:2)

                ! Deallocate memory
                deallocate(vert_row, vert_col, pos_nei, nei)
            end if
        end do
    end do

    ! Write Schlegel diagram
    call Input_Chimera_Schlegel_Diagram(prob, geom, pos_xy)

    ! Deallocate memory
    deallocate(face)
    deallocate(pos_xy)
end subroutine Input_Generate_Schlegel_Diagram

! -----------------------------------------------------------------------------

! Write Schlegel diagram
subroutine Input_Chimera_Schlegel_Diagram(prob, geom, pos_xy)
    type(ProbType),   intent(in) :: prob
    type(GeomType),   intent(in) :: geom
    double precision, intent(in) :: pos_xy(:,:)

    double precision :: pos_1(3), pos_2(3)
    integer :: i, j
    logical :: f_axis
    character(200) :: path

    if(para_write_104 == .false.) return

    ! Set option
    f_axis = para_chimera_axis

    path = trim(prob.path_work)//"/"//trim(prob.name_file)
    open(unit=104, file=trim(path)//"_schlegel.bild", form="formatted")

    ! Write initial points
    write(104, "(a)"), ".color red"
    do i = 1, geom.n_iniP
        write(104, "(a$    )"), ".sphere "
        write(104, "(3f9.2$)"), pos_xy(i,1:2) * 30.0d0, 0.0d0
        write(104, "(1f9.2 )"), 0.75d0
    end do

    ! Write initial edges
    write(104, "(a)"), ".color dark green"
    do i = 1, geom.n_iniL
        pos_1(3)   = 0.0d0
        pos_2(3)   = 0.0d0
        pos_1(1:2) = pos_xy(geom.iniL(i).poi(1),1:2) * 30.0d0
        pos_2(1:2) = pos_xy(geom.iniL(i).poi(2),1:2) * 30.0d0

        write(104,"(a$    )"), ".cylinder "
        write(104,"(3f9.2$)"), pos_1(1:3)
        write(104,"(3f9.2$)"), pos_2(1:3)
        write(104,"(1f9.2 )"), 0.3d0
    end do

    ! Write global axis
    if(f_axis == .true.) then
        write(104, "(a)"), ".translate 0.0 0.0 0.0"
        write(104, "(a)"), ".scale 0.5"
        write(104, "(a)"), ".color grey"
        write(104, "(a)"), ".sphere 0 0 0 0.5"      ! Center
        write(104, "(a)"), ".color red"             ! x-axis
        write(104, "(a)"), ".arrow 0 0 0 4 0 0 "
        write(104, "(a)"), ".color blue"            ! y-axis
        write(104, "(a)"), ".arrow 0 0 0 0 4 0 "
        write(104, "(a)"), ".color yellow"          ! z-axis
        write(104, "(a)"), ".arrow 0 0 0 0 0 4 "
    end if
    close(unit=104)

    ! ---------------------------------------------
    ! Write the file for Tecplot
    ! ---------------------------------------------
    if(para_output_Tecplot == "off") return

    path = trim(prob.path_work)//"/tecplot/"//trim(prob.name_file)
    open(unit=104, file=trim(path)//"_schlegel.dat", form="formatted")

    write(104, "(a )"), 'TITLE = "'//trim(prob.name_file)//'"'
    write(104, "(a )"), 'VARIABLES = "X", "Y", "Z", "weight"'
    write(104, "(a$)"), 'ZONE F = FEPOINT'
    write(104, "(a$)"), ', N='//trim(adjustl(Int2Str(geom.n_iniP)))
    write(104, "(a$)"), ', E='//trim(adjustl(Int2Str(geom.n_iniL)))
    write(104, "(a )"), ', ET=LINESEG'

    ! Write vertices
    do i = 1, geom.n_iniP
        write(104, "(3f10.4$)"), pos_xy(i, 1:2), 0.0d0
        write(104, "(1f10.4 )"), 1.0d0
    end do

    ! Write edges
    do i = 1, geom.n_iniL
        write(104, "(1i7$)"), geom.iniL(i).poi(1)
        write(104, "(1i7 )"), geom.iniL(i).poi(2)
    end do

    close(unit=104)
end subroutine Input_Chimera_Schlegel_Diagram

! -----------------------------------------------------------------------------

end module Input